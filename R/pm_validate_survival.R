#' Validate an existing prediction model based on survival/time-to-event models,
#' to calculate the predictive performance of an existing prediction model,
#' against a new dataset.
#'
#' @param x an object of class "pminfo", or a data.frame. If a data.frame is
#'   supplied, \code{LinearPredictor} and \code{Outcome} should also be supplied
#'   as described below.
#' @param ... other arguments
#'
#' @return TO ADD
#'
#' @export
#'
#' @examples #TO ADD
#'
#' @seealso \code{\link{pm_input_info}}
#'
#' @rdname pm_validate_survival
pm_validate_survival <- function(x, ...) {
  UseMethod("pm_validate")
}

#' @rdname pm_validate_survival
#' @param LinearPredictor character variable specifying the column name that
#'   stores the linear predictor of the model to be validated
#' @param Outcome character variable specifying the column name that
#'   stores the observed outcomes within the validation dataset
#' @export
pm_validate_survival.data.frame <- function(x, LinearPredictor, Outcome, ...) {
  NULL
}

#' @rdname pm_validate_survival
#' @export
pm_validate_survival.pminfo <- function(x, ...) {
  if (x$model_type == "logistic") {
    stop("Validation of models of type='logistic' should call pm_validate_logistic() instead of pm_validate_survival()")
  } else {
    NULL
  }
}
