#' Validate Predicted Probabilities
#'
#' This function is included for situations where one has a vector of predicted
#' probabilities from a model and a vector of observed binary outcomes that we
#' wish to validate the predictions against. See \code{\link{pred_validate}} for
#' the main validation function of this package.
#'
#' @param binary_outcome vector of binary outcomes (coded as 1 if outcome
#'   happened, and 0 otherwise). Must be of same length as Prob
#' @param Prob vector of predicted probabilities. Must be of same length of
#'   binary_outcome.
#' @param cal_plot indicate if a flexible calibration plot should be produced
#'   (TRUE) or not (FALSE).
#' @param level the confidence level required for all performance metrics.
#'   Defaults at 95%. Must be a value between 0 and 1.
#' @param ... further plotting arguments for the calibration plot. See Details
#'   below.
#'
#' @return An object of class "\code{predvalidate}", which is a list containing
#'   relevant calibration and discrimination measures. See
#'   \code{\link{pred_validate}} for details. This will include
#'   observed:expected ratio, calibration-intercept, calibration slope, area
#'   under the ROC curve, R-squared, and Brier Score. Optionally, a
#'   flexible calibration plot is also produced, along with a box-plot and
#'   violin plot of the predicted risk distribution.
#'
#' @export
#'
#' @examples
#' # simulate some data for purposes of example illustration
#' set.seed(1234)
#' x1 <- rnorm(2000)
#' LP <- -2 + (0.5*x1)
#' PR <- 1/(1+exp(-LP))
#' y <- rbinom(2000, 1, PR)
#'
#' #fit hypothetical model to the simulated data
#' mod <- glm(y[1:1000] ~ x1[1:1000], family = binomial(link="logit"))
#'
#' #obtain the predicted risks from the model
#' pred_risk <- predict(mod, type = "response",
#'                       newdata = data.frame("x1" = x1[1001:2000]))
#'
#' #Use pred_val_probs to validate the predicted risks against the
#' #observed outcomes
#' summary(pred_val_probs(binary_outcome = y[1001:2000],
#'                         Prob = pred_risk,
#'                         cal_plot = FALSE))
#' @seealso \code{\link{pred_input_info}}, \code{\link{pred_validate}}
pred_val_probs <- function(binary_outcome,
                           Prob,
                           cal_plot = TRUE,
                           level = 0.95,
                           ...) {

  #Check length of binary_outcome and Prob agree
  if (length(binary_outcome) != length(Prob)) {
    stop("length of binary_outcome and length of Prob are not the same",
         call. = FALSE)
  }

  #ensure level is specified correctly
  if (!is.numeric(level)) {
    stop("level specified incorrectly; must be a value between 0 and 1",
         call. = FALSE)
  }
  if (level > 1 |
      level < 0 |
      is.na(level) |
      is.null(level)) {
    stop("level specified incorrectly; must be a value between 0 and 1",
         call. = FALSE)
  }

  #call internal predictive performance function
  LP <- predRupdate::logit(Prob)
  performance <- validate_logistic(ObservedOutcome = binary_outcome,
                                   Prob = Prob,
                                   LP = LP,
                                   level = level,
                                   cal_plot = cal_plot,
                                   ...)
  performance$M <- 1
  #set class as predvalidate to drawn upon S3 methods of that object class
  class(performance) <- c("predvalidate_logistic", "predvalidate")
  performance
}
