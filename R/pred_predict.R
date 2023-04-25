#' Make predictions from an existing prediction model
#'
#' Use an existing prediction model to estimate predicted risks of the outcome
#' for each observation in a new dataset.
#'
#' @param x an object of class "\code{predinfo}" produced by calling
#'   \code{\link{pred_input_info}}.
#' @param new_data data.frame upon which predictions are obtained using the
#'   prediction model.
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{new_data} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise. Leave as
#'   \code{NULL} if \code{new_data} does not contain any outcomes.
#' @param survival_time Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as \code{NULL} otherwise. Leave as
#'   \code{NULL} if \code{new_data} does not contain any survival outcomes.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as \code{NULL} otherwise. Leave as \code{NULL} if \code{new_data}
#'   does not contain any survival outcomes.
#' @param time_horizon for survival models, an integer giving the time horizon
#'   (post baseline) at which a prediction is required (i.e.
#'   the t at which P(T<t) should be estimated). Currently, this must match a
#'   time in x$cum_hazard. If left as NULL, no predicted risks will be returned,
#'   just the linear predictor.
#'
#' @details This function takes the relevant information about the existing
#'   prediction model (as supplied by calling \code{\link{pred_input_info}}),
#'   and returns the linear predictor and predicted risks for each
#'   individual/observation in \code{new_data}.
#'
#'   If the existing prediction model is based on logistic regression (i.e., if
#'   x$model_type == "logistic"), the predicted risks will be the predicted
#'   probability of the binary outcome conditional on the predictor variables in
#'   the new data (i.e., P(Y=1 | X)). If the existing prediction model is
#'   based on a time-to-event/survival model (i.e., if x$model_type ==
#'   "survival"), the predicted risks can only be calculated if a baseline
#'   cumulative hazard is provided; in this case, the predicted risks will be one
#'   minus the survival probability (i.e., 1 - S(T>time horizon
#'   | X)).
#'
#'   \code{new_data} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction model
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{new_data}).
#'   Any factor variables within \code{new_data} must be converted to dummy
#'   (0/1) variables before calling this function. \code{\link{dummy_vars}} can
#'   help with this. See examples.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{new_data} (use
#'   \code{binary_outcome} if \code{x$model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{x$model_type} =
#'   "survival").
#'
#' @return \code{\link{pred_predict}} returns a list containing the following
#'   components: \itemize{ \item{LinearPredictor = the linear predictor for each
#'   observation in the new data (i.e., the linear combination of the models
#'   predictor variables and their corresponding coefficients)}
#'   \item{PredictedRisk = the predicted risk for each observation in the new
#'   data} \item{TimeHorizon = for survival models, an integer giving the time
#'   horizon at which a prediction is made} \item{Outcomes = vector of
#'   outcomes/endpoints (if available).} }
#'
#' @examples
#' #Example 1 - logistic regression existing model - shows handling of factor variables
#' coefs_table <- data.frame("Intercept" = -3.4,
#'                           "Sex_M" = 0.306,
#'                           "Smoking_Status" = 0.628)
#' existing_Logistic_Model <- pred_input_info(model_type = "logistic",
#'                                            model_info = coefs_table)
#' new_df <- data.frame("Sex" = as.factor(c("M", "F", "M", "M", "F", "F", "M")),
#'                      "Smoking_Status" = c(1, 0, 0, 1, 1, 0, 1))
#' #new_df has a factor variable, so needs indicator variables creating before pred_predict:
#' new_df_indicators <- dummy_vars(new_df)
#' pred_predict(x = existing_Logistic_Model,
#'              new_data = new_df_indicators)
#'
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package. Multiple existing models
#' model2 <- pred_input_info(model_type = "survival",
#'                           model_info = SYNPM$Existing_TTE_models,
#'                           cum_hazard = list(SYNPM$TTE_mod1_baseline,
#'                                                 SYNPM$TTE_mod2_baseline,
#'                                                 SYNPM$TTE_mod3_baseline))
#' pred_predict(x = model2,
#'              new_data = SYNPM$ValidationData[1:10,],
#'              survival_time = "ETime",
#'              event_indicator = "Status",
#'              time_horizon = 5)
#'
#' @seealso \code{\link{pred_input_info}}
#'
#' @export
pred_predict <- function(x,
                         new_data,
                         binary_outcome = NULL,
                         survival_time = NULL,
                         event_indicator = NULL,
                         time_horizon = NULL) {
  UseMethod("pred_predict")
}


#' @export
pred_predict.default <- function(x,
                                 new_data,
                                 binary_outcome = NULL,
                                 survival_time = NULL,
                                 event_indicator = NULL,
                                 time_horizon = NULL) {
  stop("'x' is not of class 'predinfo'; please see pred_input_info()",
       call. = FALSE)
}


#' @export
pred_predict.predinfo_logistic <- function(x,
                                           new_data,
                                           binary_outcome = NULL,
                                           survival_time = NULL,
                                           event_indicator = NULL,
                                           time_horizon = NULL){
  #Map the pminfo object to the supplied new dataset:
  mapped_data <- map_newdata(x = x,
                             new_data = new_data,
                             binary_outcome = binary_outcome,
                             survival_time = survival_time,
                             event_indicator = event_indicator)
  if (x$M == 1) {
    #Gather information from the predinfo blueprint:
    existingcoefs <- as.numeric(mapped_data$modelinfo$coefs)
    DM <- stats::model.matrix(mapped_data$modelinfo$formula,
                              mapped_data$PredictionData)
    #Double-check dimensions:
    if (ncol(DM) != length(existingcoefs)) {
      stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
           call. = FALSE)
    }
    #Calculate the linear predictor
    LP <- as.numeric(DM %*% existingcoefs)
    #Map to predicted risks
    PR <- predRupdate::inv_logit(LP)
    #return results
    out <- list("LinearPredictor" = LP,
                "PredictedRisk" = PR,
                "Outcomes" = mapped_data$Outcomes)

  } else{

    out <- vector(mode = "list", length = x$M)
    for(m in 1:x$M) {
      #Gather information from the predinfo blueprint:
      existingcoefs <- as.numeric(mapped_data$modelinfo$coefs[[m]])
      DM <- stats::model.matrix(mapped_data$modelinfo$formula[[m]],
                                mapped_data$PredictionData)
      #Double-check dimensions:
      if (ncol(DM) != length(existingcoefs)) {
        stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
             call. = FALSE)
      }
      #Calculate the linear predictor
      LP <- as.numeric(DM %*% existingcoefs)
      #Map to predicted risks
      PR <- predRupdate::inv_logit(LP)
      #return results
      out[[m]] <- list("LinearPredictor" = LP,
                       "PredictedRisk" = PR,
                       "Outcomes" = mapped_data$Outcomes)
    }
  }
  out
}


#' @export
pred_predict.predinfo_survival <- function(x,
                                           new_data,
                                           binary_outcome = NULL,
                                           survival_time = NULL,
                                           event_indicator = NULL,
                                           time_horizon = NULL){
  #Map the pminfo object to the supplied new dataset:
  mapped_data <- map_newdata(x = x,
                             new_data = new_data,
                             binary_outcome = binary_outcome,
                             survival_time = survival_time,
                             event_indicator = event_indicator)
  if (x$M == 1) {
    #Gather information from the predinfo blueprint:
    existingcoefs <- as.numeric(mapped_data$modelinfo$coefs)
    DM <- stats::model.matrix(mapped_data$modelinfo$formula,
                              mapped_data$PredictionData)
    DM <- DM[,-which(colnames(DM) == "(Intercept)"), drop = FALSE]
    #Double-check dimensions:
    if (ncol(DM) != length(existingcoefs)) {
      stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
           call. = FALSE)
    }

    #Calculate the linear predictor
    LP <- as.numeric(DM %*% existingcoefs)

    #check if the baseline cumulative hazard is supplied to calculating predicted risks
    if(is.null(x$cum_hazard)) {
      PR <- NULL
    } else{
      #check validity of time_horizon
      if(!is.null(time_horizon)){
        if(length(time_horizon) > 1){
          stop("only one time_horizon can be specified",
               call. = FALSE)
        }
        if(!(time_horizon %in% x$cum_hazard[,1])){
          stop("time_horizon is not available in cum_hazard",
               call. = FALSE)
        }
        #extract baseline hazard value for required time_horizon:
        bh <- x$cum_hazard[x$cum_hazard[,1] == time_horizon,2]

        #Map to predicted risks
        PR <- 1-(exp(-bh)^exp(LP))
      } else{
        PR <- NULL
      }
    }

    #return results
    out <- list("LinearPredictor" = LP,
                "PredictedRisk" = PR,
                "TimeHorizon" = time_horizon,
                "Outcomes" = mapped_data$Outcomes)
  } else{

    out <- vector(mode = "list", length = x$M)
    for(m in 1:x$M) {
      #Gather information from the predinfo blueprint:
      existingcoefs <- as.numeric(mapped_data$modelinfo$coefs[[m]])
      DM <- stats::model.matrix(mapped_data$modelinfo$formula[[m]],
                                mapped_data$PredictionData)
      DM <- DM[,-which(colnames(DM) == "(Intercept)")]
      #Double-check dimensions:
      if (ncol(DM) != length(existingcoefs)) {
        stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
             call. = FALSE)
      }

      #Calculate the linear predictor
      LP <- as.numeric(DM %*% existingcoefs)


      #check if the baseline cumulative hazard is supplied to calculating predicted risks
      if(is.null(x$cum_hazard[[m]])) {
        PR <- NULL
      } else{
        if(!is.null(time_horizon)){
          #check validity of time_horizon
          if(length(time_horizon) > 1){
            stop("only one time_horizon can be specified",
                 call. = FALSE)
          }
          if(!(time_horizon %in% x$cum_hazard[[m]][,1])){
            stop("time_horizon is not available in cum_hazard",
                 call. = FALSE)
          }

          #extract baseline hazard value for required time_horizon:
          bh <- x$cum_hazard[[m]][x$cum_hazard[[m]][,1] == time_horizon,2]
          #Map to predicted risks
          PR <- 1-exp(-bh)^exp(LP)
        } else {
          PR <- NULL
        }
      }

      #return results
      out[[m]] <- list("LinearPredictor" = LP,
                       "PredictedRisk" = PR,
                       "TimeHorizon" = time_horizon,
                       "Outcomes" = mapped_data$Outcomes)
    }
  }
  out
}
