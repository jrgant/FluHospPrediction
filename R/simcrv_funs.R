#' Simulate a hospitalization curve


#' @param print.plot Logical. Print a plot of the simulated curve. Default: FALSE
#' @param print.samples Logical. Print stochastically sampled parameters as the function runs. Default: FALSE
#' @param print.eq Logical. Print values calculated using the modified formula from Brooks et al (see Details). Default: FALSE 
#' @param verbose Logical. Equivalent to setting both \code{print.samples} and \code{print.eq} to TRUE. Default: FALSE.
#' @param peakdist Observed peak hospitalizations (height and week) from FluSurv-NET.
#' @param hstdat Observed hospitalization curves from FluSurv-NET.
#' @param fitseas Trend filter fit objects for each observed hospitalization curve.
#' @param predfits Trend filter predictions based on fits to observed hospitalization curves.
#' @param nu.min Numeric. Minimum for random uniform draw governing simulated curve shifting.
#' @param nu.max Numeric. Maximum for random uniform draw governing simulated curve shifting.

#' @return Simulated curve depicting weekly hospitalization rates during a 
#'    hypothetical flu season. Returns a list containing two nested lists, one 
#'    storing sampled values from random draws, and one storing the results of
#'    feeding these values into the curve generating function.

#' @details Methods adapted from: 
#' 
#'    Brooks LC, Farrow DC, Hyun S, Tibshirani RJ, Rosenfeld R. Flexible Modeling 
#'    of Epidemics with an Empirical Bayes Framework. PLoS Comput Biol. 
#'    2015 Aug;11(8):e1004382. 
#'    Available from: http://dx.doi.org/10.1371/journal.pcbi.1004382

#' @import data.table
#' @export simcrv

simcrv <- function(
                   print.plot = FALSE,
                   print.samples = FALSE,
                   print.eq = FALSE,
                   verbose = FALSE,
                   peakdist = dist_peaks,
                   hstdat = ed,
                   predfits = tf_pred,
                   fitseas = tf_seas,
                   nu.min = 0.75,
                   nu.max = 1.25,
                   lamb_val = 25) {

  # sample shape (f)
  s <- sample(unique(hstdat$season), 1)
  max_j <- peakdist[, pkhosp[season == s]]

  # one season had peak hospitalizations occur in two separate weeks
  # to account for this, we sample one of the peak weeks at random for that
  # season
  pkw <- peakdist[, pkweek[season == s]]
  argmax_j <- ifelse(
    length(pkw) > 1,
    pkw[sample(x = c(1, 2), size = 1)],
    pkw
  )

  # sample noise (sigma)
  sigma <- predfits[[s]]$tau

  # peak height (theta)
  theta <- runif(1, min(peakdist$pkhosp), max(peakdist$pkhosp))

  # peak week
  mu <- runif(1, min(peakdist$pkweek), max(peakdist$pkweek))

  # pacing (nu)
  nu <- runif(1, nu.min, nu.max)

  slist <- list(
    "season" = s,
    "maxj" = max_j,
    "argmax" = argmax_j,
    "sigma" = sigma,
    "theta" = theta,
    "mu" = mu,
    "nu" = nu
  )

  if (print.samples | verbose) print(slist)

  # Calculate curve equation
  t1 <- theta / max_j
  arg_f <- (1:31 - mu) / nu + argmax_j

  f <- predict(fitseas[[s]],
    x.new = arg_f,
    lambda = fitseas[[s]]$lambda[lamb_val]
  )

  err <- rnorm(n = length(f), 0, sd = sqrt(sigma))
  fi <- t1 * f + err

  #  transformation to set the lower function bound to 0
  fi_tf <- sapply(fi, function(x) 0.5 * (abs(x) + x))

  eqlist <- list(
    "term1" = t1,
    "arg_f" = arg_f,
    "predictions" = f,
    "error" = err,
    "fi_pluserr" = fi,
    "fi_tf" = fi_tf
  )

  if (print.eq | verbose) print(eqlist)


  if (print.plot) {
    plot(fi_tf,
      type = "l", col = "red",
      main = substitute(
        paste(
          sigma, " = ", sig, ", ",
          theta, " = ", the, ", ",
          mu, " = ", mus, ", ",
          nu, " = ", nus
        ),
        list(
          sig = round(sigma, 2),
          the = round(theta, 2),
          mus = round(mu, 2),
          nus = round(nu, 2)
        )
      ),
      col.main = "navy",
      font.main = 2
    )
  }

  return(list(
    sample = slist,
    eq = eqlist
  ))
}


#' Simulate multiple hospitalization curves

#' @param nreps Numeric. Number of hypothetical hospitalization curves to generate.
#' @param seed Numeric. Set random number generator seed.
#' @param gimme Character. Can take three values: NULL, "everything", or "hc". NULL returns
#'    a data.frame containing the simulated hospitalization curves, labeled with a 
#'    run id; "everything" returns both the full results of \code{simcrv()} and the
#'    labeled simulations; "hc" returns only the results of \code{simcrv()}. Default:
#'    NULL
#' @param check Logical. Print preview of data.table containing labeled hypothetical
#'    hospitalization curves.
#' @param nrow Numeric. If \code{check = TRUE}, choose number of rows to preview.
#'    Because functions use data.table, \code{nrow} applies at both the head and the 
#'    tail of the dataset. In other words, setting \code{nrow} to 10 would print 10
#'    rows from the beginning of the dataset and 10 rows from the end of it.
#' @param sim_args List. A list of arguments to pass to \code{simcrv} via \code{do.call}.
#'    See \code{?do.call} for more information.
#'    
#'    
#' @return A set of simulated hospitalization curves numbering \code{nreps}.
#'
#' @seealso \code{\link{simcrv}}
#'
#' @import data.table
#' @export simdist

simdist <- function(nreps,
                    seed = 1971,
                    gimme = NULL,
                    check = FALSE,
                    nrow = 10,
                    sim_args = list()) {
  set.seed(seed)

  hc <- replicate(nreps,
    do.call("simcrv",
      args = sim_args
    ),
    simplify = FALSE
  )

  outhc <-
    sapply(hc, function(x) {
      data.frame(week = x$eq$arg_f, prediction = x$eq$fi_tf)
    },
    simplify = FALSE
    ) %>%
    rbindlist

  setDT(outhc)
  outhc[, cid := rep(1:nreps, each = 31)]

  if (check) print(outhc, topn = nrow)

  if (!is.null(gimme)) {
    if (gimme == "everything") {
      return(list(
        hc = hc,
        outhc = outhc
      ))
    } else if (gimme == "hc") {
      hc
    }
  } else {
    outhc
  }
}
