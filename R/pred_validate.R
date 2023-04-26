#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "\code{predinfo}" produced by calling
#'   \code{\link{pred_input_info}}.
#' @param new_data data.frame upon which the prediction model should be
#'   evaluated.
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{new_data} that represents the observed outcomes. Only relevant for
#'   \code{x$model_type}="logistic"; leave as \code{NULL} otherwise.
#' @param survival_time Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival times. Only relevant
#'   for \code{x$model_type}="survival"; leave as \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{x$model_type}="survival";
#'   leave as \code{NULL} otherwise.
#' @param time_horizon for survival models, an integer giving the time horizon
#'   (post baseline) at which a prediction is required. Currently, this must
#'   match a time in x$cum_hazard.
#' @param cal_plot indicate if a flexible calibration plot should be produced
#'   (TRUE) or not (FALSE).
#' @param ... further plotting arguments for the calibration plot. See Details
#'   below.
#'
#' @details This function takes an existing prediction model formatted according
#'   to \code{\link{pred_input_info}}, and calculates measures of predictive
#'   performance on new data (e.g., within an external validation study). The
#'   information about the existing prediction model should first be inputted by
#'   calling \code{\link{pred_input_info}}, before passing the resulting object
#'   to \code{pred_validate}.
#'
#'   \code{new_data} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction model
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{new_data}).
#'
#'   Any factor variables within \code{new_data} must be converted to dummy
#'   (0/1) variables before calling this function. \code{\link{dummy_vars}} can
#'   help with this. See \code{\link{pred_predict}} for examples.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{new_data} (use
#'   \code{binary_outcome} if \code{x$model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{x$model_type} =
#'   "survival").
#'
#'   In the case of validating a logistic regression model, this function
#'   assesses the predictive performance of the predicted risks against an
#'   observed binary outcome. Various metrics of calibration (agreement between
#'   the observed risk and the predicted risks, across the full risk range) and
#'   discrimination (ability of the model to distinguish between those who
#'   develop the outcome and those who do not) are calculated. For calibration,
#'   calibration-in-the-large (CITL) and calibration slopes are estimated. CITL
#'   is estimated by fitting a logistic regression model to the observed binary
#'   outcomes, with the linear predictor of the model as an offset. For
#'   calibration slope, a logistic regression model is fit to the observed
#'   binary outcome with the linear predictor from the model as the only
#'   covariate. For discrimination, the function estimates the area under the
#'   receiver operating characteristic curve (AUC). Various other metrics are
#'   also calculated to assess overall accuracy (Brier score, Cox-Snell R2).
#'
#'   In the case of validating a survival prediction model, this function
#'   assesses the predictive performance of the linear predictor and
#'   (optionally) the predicted event probabilities at a fixed time horizon
#'   against an observed time-to-event outcome. Various metrics of calibration
#'   and discrimination are calculated. For calibration, the
#'   observed-to-expected ratio at the specified \code{time_horizon} (if
#'   predicted risks are available through specification of \code{x$cum_hazard})
#'   and calibration slope are produced. For discrimination, Harrell's
#'   C-statistic is calculated.
#'
#'   For both model types, a flexible calibration plot is produced (for survival
#'   models, the cumulative baseline hazard must be available in the
#'   \code{predinfo} object, \code{x$cum_hazard}). Specify parameter
#'   \code{cal_plot} to indicate whether a calibration plot should be produced
#'   (TRUE), or not (FALSE). The calibration plot is produced by regressing the
#'   observed outcomes against a cubic spline of the logit of predicted risks
#'   (for a logistic model) or the complementary log-log of the predicted risks
#'   (for a survival model). A histogram of the predicted risk distribution is
#'   displayed on the top x-axis. Users can specify parameters to modify the
#'   calibration plot. Specifically, one can specify: \code{xlab}, \code{ylab},
#'   \code{xlim}, and \code{ylim} to change plotting characteristics for the
#'   calibration plot.
#'
#' @return \code{\link{pred_validate}} returns an object of class
#'   "\code{predvalidate}", with child classes per \code{model_type}. This is a
#'   list of performance metrics, estimated by applying the existing prediction
#'   model to the new_data. An object of class "\code{predvalidate}" is a list
#'   containing relevant calibration and discrimination measures. For logistic
#'   regression models, this will include calibration-intercept, calibration
#'   slope, area under the ROC curve, R-squared, and Brier Score. For survival
#'   models, this will include observed:expected ratio (if \code{cum_hazard} is
#'   provided to \code{x}), calibration slope, and Harrell's C-statistic.
#'   Optionally, a flexible calibration plot is also produced, along with a
#'   histogram of the predicted risk distribution.
#'
#' @export
#'
#' @examples
#' #Example 1 - multiple existing model, with outcome specified; uses
#' #            an example dataset within the package
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_logistic_models)
#' pred_validate(x = model1,
#'              new_data = SYNPM$ValidationData,
#'              binary_outcome = "Y",
#'              cal_plot = FALSE)
#'
#' @seealso \code{\link{pred_input_info}}
pred_validate <- function(x,
                          new_data,
                          binary_outcome = NULL,
                          survival_time = NULL,
                          event_indicator = NULL,
                          time_horizon = NULL,
                          cal_plot = TRUE,
                          ...) {
  UseMethod("pred_validate")
}


#' @export
pred_validate.default <- function(x,
                                  new_data,
                                  binary_outcome = NULL,
                                  survival_time = NULL,
                                  event_indicator = NULL,
                                  time_horizon = NULL,
                                  cal_plot = TRUE, ...) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}



#' @export
pred_validate.predinfo_logistic <- function(x,
                                            new_data,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL,
                                            cal_plot = TRUE, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator,
                                           time_horizon = time_horizon)

  if (x$M == 1) {
    ### VALIDATION OF THE EXISTING MODEL
    performance <- validate_logistic(ObservedOutcome = predictions$Outcomes,
                                     Prob = predictions$PredictedRisk,
                                     LP = predictions$LinearPredictor,
                                     cal_plot = cal_plot,
                                     ...)
  } else{
    performance <- vector(mode = "list", length = x$M)
    for (m in 1:x$M) {
      performance[[m]] <-
        validate_logistic(ObservedOutcome = predictions[[m]]$Outcomes,
                          Prob = predictions[[m]]$PredictedRisk,
                          LP = predictions[[m]]$LinearPredictor,
                          cal_plot = cal_plot,
                          ...)
    }
  }
  performance$M <- x$M
  class(performance) <- c("predvalidate_logistic", "predvalidate")
  performance
}



#' @export
pred_validate.predinfo_survival <- function(x,
                                            new_data,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL,
                                            cal_plot = TRUE, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(survival_time) |
      is.null(event_indicator)) {
    stop("survival_time and event_indicator must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #ensure that a time_horizon is supplied - needed for validating time-to-event models:
  if (is.null(time_horizon)) {
    stop("time_horizon must be supplied to validate time-to-event models")
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator,
                                           time_horizon = time_horizon)

  if (x$M == 1) {
    ### VALIDATION OF THE EXISTING MODEL
    performance <- validate_survival(ObservedOutcome = predictions$Outcomes,
                                     Prob = predictions$PredictedRisk,
                                     LP = predictions$LinearPredictor,
                                     time_horizon = predictions$TimeHorizon,
                                     cal_plot = cal_plot,
                                     ...)
  } else{
    performance <- vector(mode = "list", length = x$M)
    for (m in 1:x$M) {
      performance[[m]] <-
        validate_survival(ObservedOutcome = predictions[[m]]$Outcomes,
                          Prob = predictions[[m]]$PredictedRisk,
                          LP = predictions[[m]]$LinearPredictor,
                          time_horizon = predictions[[m]]$TimeHorizon,
                          cal_plot = cal_plot,
                          ...)
    }
  }
  performance$M <- x$M
  class(performance) <- c("predvalidate_survival", "predvalidate")
  performance
}



#' @export
summary.predvalidate_logistic <- function(object, ...) {
  if(object$M == 1){
    predvalidatesummary.fnc(object = object,
                            model_type = "logistic")
  } else{
    for(m in 1:object$M) {
      cat(paste("\nPerformance Results for Model", m, "\n", sep = " "))
      cat("================================= \n")
      predvalidatesummary.fnc(object = object[[m]],
                              model_type = "logistic")
    }
  }
}



#' @export
summary.predvalidate_survival <- function(object, ...) {
  if(object$M == 1){
    predvalidatesummary.fnc(object = object,
                            model_type = "survival")
  } else{
    for(m in 1:object$M) {
      cat(paste("\nPerformance Results for Model", m, "\n", sep = " "))
      cat("================================= \n")
      predvalidatesummary.fnc(object = object[[m]],
                              model_type = "survival")
    }
  }
}



predvalidatesummary.fnc <- function(object, model_type) {
  if(model_type == "logistic") {

    cat("Calibration Measures \n",
        "--------------------------------- \n", sep = "")
    results <- matrix(NA, ncol = 4, nrow = 2)
    colnames(results) <- c("Estimate",
                           "Std. Err",
                           "Lower 95% Confidence Interval",
                           "Upper 95% Confidence Interval")
    rownames(results) <- c("Calibration-in-the-large",
                           "Calibration Slope")
    results[1,] <- c(round(object$CITL, 4),
                     round(object$CITL_SE, 4),
                     round((object$CITL - (stats::qnorm(0.975)*object$CITL_SE)), 4),
                     round((object$CITL + (stats::qnorm(0.975)*object$CITL_SE)), 4))
    results[2,] <- c(round(object$CalSlope, 4),
                     round(object$CalSlope_SE, 4),
                     round((object$CalSlope - (stats::qnorm(0.975)*object$CalSlope_SE)), 4),
                     round((object$CalSlope + (stats::qnorm(0.975)*object$CalSlope_SE)), 4))
    print(results)
    cat("\n Also examine the calibration plot, if produced. \n")
    cat("\nDiscrimination Measures \n",
        "--------------------------------- \n", sep = "")
    results <- matrix(NA, ncol = 4, nrow = 1)
    colnames(results) <- c("Estimate",
                           "Std. Err",
                           "Lower 95% Confidence Interval",
                           "Upper 95% Confidence Interval")
    rownames(results) <- c("AUC")
    results[1,] <- c(round(object$AUC, 4),
                     round(object$AUC_SE, 4),
                     round((object$AUC - (stats::qnorm(0.975)*object$AUC_SE)), 4),
                     round((object$AUC + (stats::qnorm(0.975)*object$AUC_SE)), 4))
    print(results)
    cat("\n")
    cat("\nOverall Performance Measures \n",
        "--------------------------------- \n", sep = "")
    cat("Cox-Snell R-squared: ", round(object$R2_CoxSnell, 4), "\n", sep = "")
    cat("Nagelkerke R-squared: ", round(object$R2_Nagelkerke, 4), "\n", sep = "")
    cat("Brier Score: ", round(object$BrierScore, 4), "\n", sep = "")

    cat("\n Also examine the histogram of predicted risks. \n")

  } else if(model_type == "survival"){

    cat("Calibration Measures \n",
        "--------------------------------- \n", sep = "")
    results <- matrix(NA, ncol = 4, nrow = 2)
    colnames(results) <- c("Estimate",
                           "Std. Err",
                           "Lower 95% Confidence Interval",
                           "Upper 95% Confidence Interval")
    rownames(results) <- c("Observed:Expected Ratio", "Calibration Slope")
    results[1,] <- c(round(object$OE_ratio, 4),
                     round(object$OE_ratio_SE, 4),
                     round((object$OE_ratio * exp(-stats::qnorm(0.975) * object$OE_ratio_SE)), 4),
                     round((object$OE_ratio * exp(stats::qnorm(0.975) * object$OE_ratio_SE)), 4),
                     round((object$CITL + (stats::qnorm(0.975)*object$CITL_SE)), 4))
    results[2,] <- c(round(object$CalSlope, 4),
                     round(object$CalSlope_SE, 4),
                     round((object$CalSlope - (stats::qnorm(0.975)*object$CalSlope_SE)), 4),
                     round((object$CalSlope + (stats::qnorm(0.975)*object$CalSlope_SE)), 4))
    print(results)
    cat("\n Also examine the calibration plot, if produced. \n")
    cat("\nDiscrimination Measures \n",
        "--------------------------------- \n", sep = "")
    results <- matrix(NA, ncol = 4, nrow = 1)
    colnames(results) <- c("Estimate",
                           "Std. Err",
                           "Lower 95% Confidence Interval",
                           "Upper 95% Confidence Interval")
    rownames(results) <- c("Harrell C")
    results[1,] <- c(round(object$harrell_C, 4),
                     round(object$harrell_C_SE, 4),
                     round((object$harrell_C - (stats::qnorm(0.975)*object$harrell_C_SE)), 4),
                     round((object$harrell_C + (stats::qnorm(0.975)*object$harrell_C_SE)), 4))
    print(results)

    cat("\n Also examine the histogram of predicted risks. \n")

  }
}
