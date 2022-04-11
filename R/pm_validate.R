#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "pminfo"
#' @param ... further arguments passed to other methods. See
#'   \code{\link{validate_probabilities}} and
#'   INSERT_SURVIVAL_VALIDATION_FUNCTION for more details of arguments that can
#'   be passed
#'
#' @details TO ADD
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
                                                  LP = predictions$LinearPredictor,
                                                  ...)

  class(performance) <- c("pmvalidate_logistic", "pmvalidate")
  performance
}


#' @export
pm_validate.pminfo_survival <- function(x, ...){
  stop("Models of type='survival' are not currently supported")
}



#' @export
print.pmvalidate_logistic <- function(x, ...) {
  cat("Calibration Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 2)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Calibration-in-the-large",
                         "Calibration Slope")
  results[1,] <- c(round(x$CITL, 4),
                   round(x$CITL_SE, 4),
                   round(x$CITL_Lower, 4),
                   round(x$CITL_Upper, 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round(x$CalSlope_Lower, 4),
                   round(x$CalSlope_Upper, 4))
  print(results)
  cat("\n Also examine the calibration plot, if produced. \n")
  cat("\nDiscrimination Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 1)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("AUC")
  results[1,] <- c(round(x$AUC, 4),
                   round(x$AUC_SE, 4),
                   round(x$AUC_Lower, 4),
                   round(x$AUC_Upper, 4))
  print(results)
  cat("\n")
  cat("\nOverall Performance Measures \n",
      "================================= \n", sep = "")
  cat("Cox-Snell R-squared: ", round(x$R2_CoxSnell, 4), "\n", sep = "")
  cat("Nagelkerke R-squared: ", round(x$R2_Nagelkerke, 4), "\n", sep = "")
  cat("Brier Score: ", round(x$BrierScore, 4), "\n", sep = "")

  cat("\n Also examine the histogram of predicted risks. \n")
}
