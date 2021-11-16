#' Validate an existing prediction model based on logistic regression, to
#' calculate the predictive performance of an existing prediction model, against
#' a new dataset.
#'
#' @param x an object of class "pminfo", or a data.frame. If a data.frame is
#'   supplied, \code{LinearPredictor} and \code{Outcome} should also be supplied
#'   as described below.
#' @param ... other arguments - not used
#'
#' @return TO ADD
#'
#' @export
#'
#' @examples #TO ADD
#'
#' @seealso \code{\link{pm_input_info}}
#'
#' @rdname pm_validate_logistic
pm_validate_logistic <- function(x, ...) {
  UseMethod("pm_validate_logistic")
}


#' @rdname pm_validate_logistic
#' @param LinearPredictor character variable specifying the column name that
#'   stores the linear predictor of the model to be validated
#' @param Outcome character variable specifying the column name that
#'   stores the observed outcomes within the validation dataset
#' @export
pm_validate_logistic.data.frame <- function(x,
                                            LinearPredictor,
                                            Outcome, ...) {
  ### Test inputs
  if(missing(LinearPredictor) | missing(Outcome)) {
    stop("When x is a data.frame, 'LinearPredictor' and 'Outcome' must be supplied")
  }
  if(LinearPredictor %in% names(x) == FALSE) {
    stop("Specified 'LinearPredictor' is not found in x")
  }else if(Outcome %in% names(x) == FALSE) {
    stop("Specified 'Outcome' is not found in x")
  }

  ### VALIDATION OF THE EXISTING MODEL
  performance <- logistic_performance(ObservedOutcome = x[,Outcome],
                                      LinearPredictor = x[,LinearPredictor])
  class(performance) <- "pmperformance"
  performance
}


#' @rdname pm_validate_logistic
#' @export
pm_validate_logistic.pminfo <- function(x, ...) {
  ### Test inputs
  if (x$model_type == "survival") {
    stop("Validation of models of type='survival' should call pm_validate_survival() instead of pm_validate_logistic()")
  }

  if (is.null(predictions$Outcomes)) {
    stop("Observed outcomes must be supplied in newdata to validate the existing model")
  }

  ### USE EXISTING INFO ABOUT PREDICTION MODEL TO MAKE PREDICTIONS IN NEWDATA
  predictions <- pmupdate::pm_predict(x)

  ### VALIDATION OF THE EXISTING MODEL
  performance <- logistic_performance(ObservedOutcome = predictions$Outcomes,
                                      LinearPredictor = predictions$LinearPredictor)
  class(performance) <- "pmperformance"
  performance
}


#' @export
print.pmperformance <- function(x, ...) {
  results <- matrix(NA, ncol = 4, nrow = 5)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Calibration-in-the-large",
                         "Calibration Slope",
                         "AUC",
                         "Cox-Snell R-squared",
                         "Brier Score")
  results[1,] <- c(round(x$CITL, 4),
                   round(x$CITL_SE, 4),
                   round(x$CITL_Lower, 4),
                   round(x$CITL_Upper, 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round(x$CalSlope_Lower, 4),
                   round(x$CalSlope_Upper, 4))
  results[3,] <- c(round(x$AUC, 4),
                   round(x$AUC_SE, 4),
                   round(x$AUC_Lower, 4),
                   round(x$AUC_Upper, 4))
  results[4,] <- c(round(x$R2, 4),
                   NA,
                   NA,
                   NA)
  results[5,] <- c(round(x$BrierScore, 4),
                   NA,
                   NA,
                   NA)

  print(results)
}
