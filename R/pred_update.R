#' Perform Model Updating on an Existing Prediction Model
#'
#' This function takes an existing (previously developed) prediction model and
#' applies various model updating methods to tailor/adapt it to a new dataset.
#' Various levels of updating are possible, ranging from model re-calibration to
#' model revision.
#'
#' @param x an object of class "\code{predinfo}" produced by calling
#'   \code{\link{pred_input_info}} containing information on exactly one
#'   existing prediction model.
#' @param update_type character variable specifying the level of updating that
#'   is required
#' @param newdata data.frame upon which the prediction models should be
#'   aggregated
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
#'
#' @details  The aim of this function is to take an existing (previously
#'   estimated) prediction model, and apply various model discrete model
#'   updating methods (see Su et al. 2018) to tailor the model to a new dataset.
#'
#'   The type of updating method is selected with the \code{update_type}
#'   parameter, with options: "intercept_update", "recalibration" and
#'   "refit". "intercept_update" corrects the overall
#'   calibration-in-the-large of the model, through altering the model intercept
#'   (or baseline hazard) to suit the new dataset. This is achieved by fitting a
#'   logistic model (if the existing model is of type logistic) or time-to-event
#'   model (if the existing model if of type survival) to the new dataset, with
#'   the linear predictor as the only covariate, with the coefficient fixed at
#'   unity (i.e. as an offset). "recalibration" corrects the
#'   calibration-in-the-large and any under/over-fitting, by fitting a logistic
#'   model (if the existing model is of type logistic) or time-to-event model
#'   (if the existing model if of type survival) to the new dataset, with the
#'   linear predictor as the only covariate. Finally, "refit" takes the original
#'   model structure and re-estimates all coefficients; this has the effect as
#'   re-developing the original model in the new dat.
#'
#'   \code{newdata} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction model
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{newdata}).
#'
#'   Any factor variables within \code{newdata} must be converted to dummy (0/1)
#'   variables before calling this function. \code{\link{dummy_vars}} can help
#'   with this.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{newdata} (use
#'   \code{binary_outcome} if \code{x$model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{x$model_type} =
#'   "survival").
#'
#' @return A object of class "predinfo" with subclass"\code{predUpdate}". This
#'   is the same as that detailed in \code{\link{pred_input_info}}, with the
#'   added element containing the estimates of the model updating.
#'
#' @examples
#' #Example 1 - logistic regression existing model, updated using recalibration:
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_logistic_models[1,])
#' recalibrated_model1 <- pred_update(x = model1,
#'                                    update_type = "recalibration",
#'                                    newdata = SYNPM$ValidationData,
#'                                    binary_outcome = "Y")
#' summary(recalibrated_model1)
#' recalibrated_model1$model_update_results #explore the model updating parameter estimates
#' #one could then validate this as follows (but this should be adjusted for
#' #in-sample optimism):
#' pred_validate(recalibrated_model1, newdata = SYNPM$ValidationData, binary_outcome = "Y")
#'
#' #Example 2 - update time-to-event model by updating the baseline hazard in new dataset
#' model2 <- pred_input_info(model_type = "survival",
#'                           model_info = SYNPM$Existing_TTE_models[1,],
#'                           baselinehazard = SYNPM$TTE_mod1_baseline)
#' recalibrated_model2 <- pred_update(x = model2,
#'                                    update_type = "intercept_update",
#'                                    newdata = SYNPM$ValidationData,
#'                                    survival_time = "ETime",
#'                                    event_indicator = "Status")
#' summary(recalibrated_model2)
#' #one could then validate this as follows (but this should be adjusted for
#' #in-sample optimism):
#' pred_validate(recalibrated_model2,
#'               newdata = SYNPM$ValidationData,
#'               event_indicator = "Status",
#'               survival_time = "ETime",
#'               time_horizon = 5)
#'
#' @references Su TL, Jaki T, Hickey GL, Buchan I, Sperrin M. A review of
#'   statistical updating methods for clinical prediction models. \emph{Stat Methods
#'   Med Res}. 2018 Jan;27(1):185-197. doi: 10.1177/0962280215626466.
#'
#' @export
pred_update <- function(x,
                        update_type = c("intercept_update", "recalibration", "refit"),
                        newdata,
                        binary_outcome = NULL,
                        survival_time = NULL,
                        event_indicator = NULL) {
  UseMethod("pred_update")
}

#' @export
pred_update.default <- function(x,
                                update_type = c("intercept_update", "recalibration", "refit"),
                                newdata,
                                binary_outcome = NULL,
                                survival_time = NULL,
                                event_indicator = NULL) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}

#' @export
pred_update.predinfo_logistic <- function(x,
                                          update_type = c("intercept_update", "recalibration", "refit"),
                                          newdata,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL) {

  #Check outcomes were inputted (needed to update the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to update the existing model(s)",
         call. = FALSE)
  }
  #check only updating one model
  if (x$M != 1) {
    stop("M > 1,'pred_update' currently only supports updating a single model",
         call. = FALSE)
  }

  update_type <- match.arg(update_type)

  #Make predictions within newdata using the existing prediction model
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator)

  if(update_type == "intercept_update") {
    #Run model update
    fit <- stats::glm(predictions$Outcomes ~ offset(predictions$LinearPredictor),
                      family = stats::binomial(link = "logit"))
    param <- data.frame(cbind(fit$coefficients, sqrt(diag(stats::vcov(fit)))))
    names(param) <- c("Estimate", "Std. Error")

    #Update old coefficients
    coef_table <- x$coefs
    coef_table["Intercept"] <- coef_table["Intercept"] + param["(Intercept)","Estimate"]

  } else if(update_type == "recalibration") {
    #Run model update
    fit <- stats::glm(predictions$Outcomes ~ predictions$LinearPredictor,
                      family = stats::binomial(link = "logit"))
    param <- data.frame(cbind(fit$coefficients, sqrt(diag(stats::vcov(fit)))))
    rownames(param)[2] <- c("Slope")
    names(param) <- c("Estimate", "Std. Error")

    #Update old coefficients
    coef_table <- x$coefs
    coef_table <- coef_table * param["Slope","Estimate"]
    coef_table["Intercept"] <- coef_table["Intercept"] + param["(Intercept)","Estimate"]

  } else if(update_type == "refit") {
    #Run model update
    formula_text <- paste0(binary_outcome, x$formula[1], x$formula[2])
    fit <- stats::glm(eval(parse(text=formula_text)), data = newdata,
                      family = stats::binomial(link = "logit"))
    param <- data.frame(cbind(fit$coefficients, sqrt(diag(stats::vcov(fit)))))
    names(param) <- c("Estimate", "Std. Error")

    #Update old coefficients
    coef_table <- data.frame(t(param$Estimate))
    names(coef_table) <- names(stats::coef(fit))
    names(coef_table)[which(names(coef_table) == "(Intercept)")] <- "Intercept"

  }

  #Return results and set S3 class
  update_results <- list("M" = 1,
                         "model_type" = x$model_type,
                         "coefs" = data.frame(as.list(coef_table)),
                         "coef_names" = names(coef_table),
                         "formula" = stats::as.formula(paste("~",
                                                             paste(names(coef_table)[which(!is.na(coef_table))][-1],
                                                             collapse = "+"),
                                                             sep="")),
                         "model_info" = coef_table,
                         "model_update_results" = param)
  class(update_results) <- c("predUpdate", "predinfo_logistic", "predinfo")
  update_results
}

#' @export
pred_update.predinfo_survival <- function(x,
                                          update_type = c("intercept_update", "recalibration", "refit"),
                                          newdata,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL) {
  #Check outcomes were inputted (needed to update the model)
  if (is.null(survival_time) |
      is.null(event_indicator)) {
    stop("survival_time and event_indicator must be supplied to update the existing model(s)",
         call. = FALSE)
  }
  #check only updating one model
  if (x$M != 1) {
    stop("M > 1,'pred_update' currently only supports updating a single model",
         call. = FALSE)
  }

  update_type <- match.arg(update_type)

  #Make predictions within newdata using the existing prediction model
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator)

  if(update_type == "intercept_update") {
    #Run model update
    fit <- survival::coxph(predictions$Outcomes ~ offset(predictions$LinearPredictor))

    #obtain new baseline cumulative hazard
    BH <- survival::basehaz(fit)
    #undo the scaling of the offset term in the baseline hazard (above BH is for scaled LP):
    BH$hazard <- BH$hazard / exp(mean(predictions$LinearPredictor))

    #keep original coefficients
    coef_table <- x$coefs
    param <- NA

  } else if(update_type == "recalibration") {
    #Run model update
    fit <- survival::coxph(predictions$Outcomes ~ predictions$LinearPredictor)
    param <- data.frame(cbind(fit$coefficients, sqrt(diag(stats::vcov(fit)))))
    rownames(param)[1] <- c("Slope")
    names(param) <- c("Estimate", "Std. Error")

    #Update old coefficients
    coef_table <- x$coefs
    coef_table <- coef_table * param["Slope","Estimate"]

    #obtain new baseline cumulative hazard
    BH <- survival::basehaz(fit, centered = FALSE)

  } else if(update_type == "refit") {
    #Run model update
    DM <- data.frame("outcome" = predictions$Outcomes,
                     newdata[,x$coef_names])
    fit <- survival::coxph(outcome ~ ., data = DM)
    param <- data.frame(cbind(fit$coefficients, sqrt(diag(stats::vcov(fit)))))
    names(param) <- c("Estimate", "Std. Error")

    #Update old coefficients
    coef_table <- data.frame(t(param$Estimate))
    names(coef_table) <- names(stats::coef(fit))

    #obtain new baseline cumulative hazard
    BH <- survival::basehaz(fit, centered = FALSE)

  }

  #Return results and set S3 class
  update_results <- list("M" = 1,
                         "model_type" = x$model_type,
                         "coefs" = data.frame(as.list(coef_table)),
                         "coef_names" = names(coef_table),
                         "formula" = stats::as.formula(paste("~",
                                                             paste(names(coef_table)[which(!is.na(coef_table))],
                                                                   collapse = "+"),
                                                             sep="")),
                         "baselinehazard" = data.frame("time" = BH$time,
                                                       "hazard" = BH$hazard),
                         "model_info" = coef_table,
                         "model_update_results" = param)
  class(update_results) <- c("predUpdate", "predinfo_survival", "predinfo")
  update_results
}
