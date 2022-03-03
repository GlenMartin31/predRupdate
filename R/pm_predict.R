#' Use an existing prediction model to estimate predicted risks in new data,
#' given published coefficients
#'
#' @param x an object of class "\code{pminfo}" produced by
#'   \code{\link{pm_input_info}}. This is a 'blueprint' description of the
#'   existing prediction model, and the newdata on which predictions should be
#'   made.
#'
#' @param time_horizon for survival models, an integer giving the time horizon at
#'   which a prediction is required.
#'   Currently, must match a time in x$baselinehazard.
#'
#' @details This function takes the relevant information about the existing
#'   prediction model (as supplied by called \code{\link{pm_input_info}}), and
#'   returns the predicted risks for each individual/observation in
#'   \code{newdata}. See \code{\link{pm_input_info}}) for more details.
#'
#' @return Linear predictor and predicted risks for each observation in
#'   \code{newdata}, based on the specified information about the existing
#'   prediction model
#'
#' @seealso \code{\link{pm_input_info}}
#'
#' @export
pm_predict <- function(x, time_horizon = NULL) {
  UseMethod("pm_predict")
}


#' @export
pm_predict.default <- function(x, time_horizon = NULL) {
  stop("'x' is not of class 'pminfo'; please see pm_input_info()",
       call. = FALSE)
}


#' @export
pm_predict.pminfo_logistic <- function(x, time_horizon = NULL){
  #Gather information from the pminfo blueprint:
  existingcoefs <- x$coefs
  DM <- x$PredictionData

  #Double-check dimensions:
  if (ncol(DM) != length(existingcoefs)) {
    stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
         call. = FALSE)
  }


  #Calculate the linear predictor
  LP <- as.numeric(DM %*% existingcoefs)
  #Map to predicted risks
  PR <- pmupdate::inv_logit(LP)

  #return results
  out <- list("LinearPredictor" = LP,
              "PredictedRisk" = PR,
              "Outcomes" = x$Outcomes)
  out
}


#' @export
pm_predict.pminfo_survival <- function(x, time_horizon = NULL){
  #Gather information from the pminfo blueprint:
  existingcoefs <- x$coefs
  DM <- x$PredictionData

  #Double-check dimensions:
  if (ncol(DM) != length(existingcoefs)) {
    stop("Existing coefficients of the model and new data to make predictions on are non-conformable",
         call. = FALSE)
  }

  #check validity of time_horizon
  if(is.null(time_horizon)){
    stop("time_horizon must be specified to make a prediction",
         call. = FALSE)
  }

  if(length(time_horizon > 1)){
    stop("only one time_horizon can be specified",
         call. = FALSE)
  }

  if(!(time_horizon %in% x$baselinehazard[,1])){
    stop("time_horizon is not available in baselinehazard",
         call. = FALSE)
  }

  #extract baseline hazard value for required time_horizon:
  bh <- x$baselinehazard[x$baselinehazard[,1] == time_horizon,2]

  #Calculate the linear predictor
  LP <- as.numeric(DM %*% existingcoefs)
  #Map to predicted risks
  PR <- 1-exp(-bh)^exp(LP)

  #return results
  out <- list("LinearPredictor" = LP,
              "PredictedRisk" = PR,
              "TimeHorizon" = time_horizon,
              "Outcomes" = x$Outcomes)
  out
}
