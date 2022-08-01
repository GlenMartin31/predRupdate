#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "predinfo"
#' @param ... further arguments passed to other methods. See
#'   \code{\link{validate_probabilities}} for more details of arguments that can
#'   be passed
#'
#' @details This function takes an existing prediction model formatted according
#'   to \code{\link{pred_input_info}}, and calculates measures of predictive
#'   performance on new data (e.g., within an external validation study). The
#'   information about the existing prediction model should first be inputted by
#'   calling \code{\link{pred_input_info}}, before passing the resulting object
#'   to \code{pred_validate}.
#'
#'   In the case of validating a logistic regression model, \code{pred_validate}
#'   internally calls \code{\link{validate_probabilities}}. See this function
#'   for details on the performance metrics that are estimated.
#'
#'   In the case of validating a survival prediction model,...
#'
#' @return A list of performance metrics, estimated by applying the existing
#'   prediction model to the newdata.
#'
#' @export
#'
#' @examples
#' # Example 1 - logistic regression example
#' existing_cpm_info <- pred_input_info(model_type = "logistic",
#'                                      model_info = SYNPM$Existing_models[1,],
#'                                      newdata = SYNPM$ValidationData,
#'                                      binary_outcome = "Y")
#'
#' pred_validate(existing_cpm_info)
#'
#' # Example 2 - survival example
#' #TO ADD
#'
#' @seealso \code{\link{pred_input_info}} \code{\link{validate_probabilities}}
pred_validate <- function(x, ...) {
  UseMethod("pred_validate")
}


#' @export
pred_validate.default <- function(x, ...) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}



#' @export
pred_validate.predinfo_logistic <- function(x, ...){

  #Check outcomes were inputted into predinfo object
  if (is.null(x$Outcomes)) {
    stop("Observed outcomes must be supplied in newdata to validate the existing model. Recall pred_input_info() with outcome specified.",
         call. = FALSE)
  }

  ### USE EXISTING INFO ABOUT PREDICTION MODEL TO MAKE PREDICTIONS IN NEWDATA
  predictions <- predRupdate::pred_predict(x)

  if (x$M == 1) {
    ### VALIDATION OF THE EXISTING MODEL
    performance <- predRupdate::validate_probabilities(ObservedOutcome = predictions$Outcomes,
                                                       LP = predictions$LinearPredictor,
                                                       ...)
  } else{
    performance <- vector(mode = "list", length = x$M)
    names(performance) <- paste("Model_",1:x$M, sep = "")
    for (m in 1:x$M) {
      performance[[paste("Model_",m, sep = "")]] <-
        predRupdate::validate_probabilities(ObservedOutcome = predictions[[m]]$Outcomes,
                                            LP = predictions[[m]]$LinearPredictor,
                                            ...)
    }
  }
  performance
}


#' @export
pred_validate.predinfo_survival <- function(x, ...){
  stop("Models of type='survival' are not currently supported")
}



#' @export
print.predvalidate_logistic <- function(x, ...) {
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
                   round((x$CITL - (stats::qnorm(0.975)*x$CITL_SE)), 4),
                   round((x$CITL + (stats::qnorm(0.975)*x$CITL_SE)), 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round((x$CalSlope - (stats::qnorm(0.975)*x$CalSlope_SE)), 4),
                   round((x$CalSlope + (stats::qnorm(0.975)*x$CalSlope_SE)), 4))
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
                   round((x$AUC - (stats::qnorm(0.975)*x$AUC_SE)), 4),
                   round((x$AUC + (stats::qnorm(0.975)*x$AUC_SE)), 4))
  print(results)
  cat("\n")
  cat("\nOverall Performance Measures \n",
      "================================= \n", sep = "")
  cat("Cox-Snell R-squared: ", round(x$R2_CoxSnell, 4), "\n", sep = "")
  cat("Nagelkerke R-squared: ", round(x$R2_Nagelkerke, 4), "\n", sep = "")
  cat("Brier Score: ", round(x$BrierScore, 4), "\n", sep = "")

  cat("\n Also examine the histogram of predicted risks. \n")
}
