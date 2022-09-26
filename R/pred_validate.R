#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "predinfo"
#' @param newdata data.frame upon which the prediction model should be validated
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{newdata} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise.
#' @param survival_time Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as \code{NULL} otherwise.
#' @param time_horizon for survival models, an integer giving the time horizon
#'   (post baseline/time of prediction) at which a prediction is required.
#'   Currently, this must match a time in x$baselinehazard.
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
#' #Example 1 - logistic regression existing model, with outcome specified; uses
#' #            an example dataset within the package
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_models[1,])
#' pred_validate(x = model1,
#'               newdata = SYNPM$ValidationData,
#'               binary_outcome = "Y")
#'
#' \dontrun{
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package. Also shows use of pre-processing to handle
#' #             categorical variables - need converting prior to call
#' SMART_dummaryvars <- dummyvars(SMART)
#' model2 <- pred_input_info(model_type = "survival",
#'                           model_info = data.frame("SEX_M" = 0.53,
#'                                                   "AGE" = -0.05,
#'                                                   "SYSTBP" = -0.0055,
#'                                                   "BMIO" = 0.0325,
#'                                                   "CARDIAC" = -0.126,
#'                                                   "DIABETES" = -0.461),
#'                            baselinehazard = data.frame("t" = 1:5,
#'                                                        "h" = c(0.12, 0.20,
#'                                                                0.26, 0.33,
#'                                                                0.38)))
#' pred_validate(x = model2,
#'               newdata = SMART_dummaryvars,
#'               survival_time = "TEVENT",
#'               event_indicator = "EVENT",
#'               time_horizon = 2)
#' }
#'
#' #Example 3 - multiple existing models
#' model3 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_models)
#' pred_validate(x = model3,
#'               newdata = SYNPM$ValidationData,
#'               binary_outcome = "Y")
#'
#' @seealso \code{\link{pred_input_info}} \code{\link{validate_probabilities}}
pred_validate <- function(x,
                          newdata,
                          binary_outcome = NULL,
                          survival_time = NULL,
                          event_indicator = NULL,
                          time_horizon = NULL, ...) {
  UseMethod("pred_validate")
}


#' @export
pred_validate.default <- function(x,
                                  newdata,
                                  binary_outcome = NULL,
                                  survival_time = NULL,
                                  event_indicator = NULL,
                                  time_horizon = NULL, ...) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}



#' @export
pred_validate.predinfo_logistic <- function(x,
                                            newdata,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #Make predictions within newdata using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
                                           binary_outcome = binary_outcome)

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
pred_validate.predinfo_survival <- function(x,
                                            newdata,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(survival_time) |
      is.null(event_indicator)) {
    stop("survival_time and event_indicator must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #Make predictions within newdata using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator,
                                           time_horizon = time_horizon)

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
