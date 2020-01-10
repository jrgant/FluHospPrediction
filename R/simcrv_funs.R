#' Function to generate a curve

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
                   mu.max = 1.25,
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


#' Function to generate multiple curves

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
    dplyr::bind_rows()

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
