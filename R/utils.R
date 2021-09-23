#' Apply a logit transformation to an input
#'
#' \code{logit} applies the logit transformation to convert a vector of values
#' between 0 and 1, to values between -Inf and Inf. Used to convert a
#' probability from a logistic regression model onto the linear predictor scale.
#'
#' @param p Numeric vector of probabilities (i.e. values between 0 and 1) that
#'   will be transformed
#'
#' @return A numeric vector, with values between -Inf and Inf
#' @export
#'
#' @author Glen Martin, \email{glen.martin@@manchester.ac.uk}
#'
#' @seealso \code{\link{inv_logit}}
#'
#' @examples
#' logit(0.5)
#' logit(c(0.1, 0.2, 0.3))
#' plot(logit(seq(from=0.01, to=0.99, by=0.01)), type='l', ylab = "LP")
#' \dontrun{ logit(2) }
logit <- function(p) {
  if( any(p < 0 | p > 1) ) stop('p not between 0 and 1')
  log(p / (1 - p))
}


#' Apply the inverse logit function to an input
#'
#' \code{inv_logit} applies the inverse-logit transformation (expit/ logistic
#' function) to convert a vector of values between Inf and Inf, to values
#' between 0 and 1. Used to convert the linear predictor of a logistic
#' regression model into a probability.
#'
#' @param x Numeric vector with values between -Inf and Inf
#'
#' @return Numeric vector of probabilities (i.e. values between 0 and 1)
#' @export
#'
#' @author Glen Martin, \email{glen.martin@@manchester.ac.uk}
#'
#' @seealso \code{\link{logit}}
#'
#' @examples
#' inv_logit(-2)
#' inv_logit(c(-2,-1,0,1,2))
#' plot(inv_logit(seq(from=-10, to=10, by=1)), type='l', ylab = "Prob")
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
