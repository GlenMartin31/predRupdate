#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "pminfo"
#' @param ... further arguments passed to other methods
#'
#' @return TO ADD
#'
#' @export
#'
#' @examples #TO ADD
#'
#' @seealso \code{\link{pm_input_info}}
pm_validate <- function(x, ...) {
  UseMethod("pm_validate")
}


#' @export
pm_validate.default <- function(x, ...) {
  stop("'x' is not of class 'pminfo'",
       call. = FALSE)
}



#' @export
pm_validate.pminfo_logistic <- function(x, ...){

  #Check outcomes were inputted into pminfo object
  if (is.null(x$Outcomes)) {
    stop("Observed outcomes must be supplied in newdata to validate the existing model. Recall pm_input_info() with outcome specified.",
         call. = FALSE)
  }

  ### USE EXISTING INFO ABOUT PREDICTION MODEL TO MAKE PREDICTIONS IN NEWDATA
  predictions <- pmupdate::pm_predict(x)

  ### VALIDATION OF THE EXISTING MODEL
  performance <- pmupdate::validate_probabilities(ObservedOutcome = predictions$Outcomes,
                                                  Logit = predictions$LinearPredictor)

  class(performance) <- c("pmperformance_logistic", "pmperformance")
  performance
}


#' @export
pm_validate.pminfo_survival <- function(x, ...){
  stop("Models of type='survival' are not currently supported")
}



#' @export
print.pmperformance_logistic <- function(x, ...) {
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
