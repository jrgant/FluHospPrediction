#' Absolute error loss function
#'

#' @param pred Predicted outcomes from a fit model or ensemble learner.
#' @param observed Observed outcomes.

#' @description The function calculates the \eqn{L_1} absolute error loss as 
#'              \eqn{L(O, \bar{Q}) = \| Y - \bar{Q}(A,W) \|}.

#' @references
#'
#' Polley EC, Rose S, van der Laan MJ. Super Learning. In: van der Laan MJ,
#'  Rose S, eds. Targeted Learning: Causal Inference for Observational and
#'  Experimental Data. New York, NY: Springer New York; 2011:43–66.
#'  https://doi.org/10.1007/978-1-4419-9782-1_3

#' @export loss_absolute_error

loss_absolute_error <- function(pred, observed) {
  out <- abs(pred - observed)
  return(out)
}
