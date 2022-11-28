#' Map newdata to a predinfo object
#'
#' This function takes a predinfo object and applies (maps) a newdata to this
#' object to check there is consistency between the two. This function is not
#' usually called directly, but rather within  other functions within the
#' package, such as \code{pred_predict}, \code{pred_validate} and
#' \code{pred_stacked_regression}.
#'
#' @param x an object of class "predinfo"
#' @param newdata data.frame upon which the prediction model should be applied
#'   (for subsequent validation/model updating/model aggregation).
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{newdata} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise. Leave as
#'   \code{NULL} if \code{newdata} does not contain any outcomes.
#' @param survival_time Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as \code{NULL} otherwise. Leave as
#'   \code{NULL} if \code{newdata} does not contain any survival outcomes.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as \code{NULL} otherwise. Leave as \code{NULL} if \code{newdata} does
#'   not contain any survival outcomes.
#'
#' @details This function maps a new dataset onto a pred_info object. The new
#'   dataset might be a validation dataset (to test the performance of the
#'   existing prediction model) and/or it might be the dataset on which one
#'   wishes to apply model updating methods to revise the model. In any case,
#'   this should be specified in \code{newdata} as a data.frame. Each row should
#'   be an observation (e.g. patient) and each variable/column should be a
#'   predictor variable. The predictor variables need to include (as a minimum)
#'   all of the predictor variables that are included in the existing prediction
#'   model (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{newdata}).
#'
#'   Any factor variables within \code{newdata} must be converted to dummy (0/1)
#'   variables before calling this function. \code{\link{dummyvars}} can help
#'   with this - see examples below.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{newdata}, if relevant
#'   (use \code{binary_outcome} if \code{model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{model_type} =
#'   "survival"). For example, if validating an existing model, then these
#'   inputs specify the columns of \code{newdata} that will be used for
#'   assessing predictive performance of the predictions in the validation
#'   dataset. If \code{newdata} does not contain outcomes, then leave these
#'   inputs to the default of \code{NULL}.
#'
#' @return Returns a list of the predinfo object, the newdata, and outcomes.
#'
#' @noRd
map_newdata <- function(x,
                        newdata,
                        binary_outcome = NULL,
                        survival_time = NULL,
                        event_indicator = NULL) {
  UseMethod("map_newdata")
}

#' @export
map_newdata.default <- function(x,
                                newdata,
                                binary_outcome = NULL,
                                survival_time = NULL,
                                event_indicator = NULL) {
  stop("'x' is not of class 'predinfo'")
}

#' @export
map_newdata.predinfo_logistic <- function(x,
                                          newdata,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL){

  ########################## INPUT CHECKING #############################
  # Check that supplied 'newdata' is a data.frame
  if (inherits(newdata, "data.frame") == FALSE) {
    stop("'newdata' should be a data.frame")
  }

  if (any(sapply(newdata, function(x) is.factor(x)))) {
    stop("'newdata' contains factor variables - convert to dummy/indicator variables first \n dummayvar() can help with this")
  }

  if (!is.null(survival_time)) {
    stop("'survival_time' should be set to NULL if model_type=logistic")
  }
  if (!is.null(event_indicator)) {
    stop("'event_indicator' should be set to NULL if model_type=logistic")
  }
  if(!is.null(binary_outcome)) {
    if(!is.character(binary_outcome)) {
      stop("'binary_outcome' should be supplied as a character variable")
    }
    if(length(binary_outcome) != 1) {
      stop("length of 'binary_outcome' should be one")
    }
    if(binary_outcome %in% names(newdata) == FALSE) {
      stop("'binary_outcome' not found in 'newdata'")
    }
  }

  # Check that all predictor variables specified in the pminfo object are also
  # included in the 'newdata':
  if (x$M == 1) {
    predictor_variables <- x$coef_names[-which(x$coef_names=="Intercept")]
  } else{
    predictor_variables <- unique(unlist(x$coef_names))
    predictor_variables <- predictor_variables[-which(predictor_variables=="Intercept")]
  }
  if (all(predictor_variables %in% names(newdata)) == FALSE) {
    stop("newdata does not contain some of the predictor variables for the model(s) specified within the pminfo object")
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(newdata[,c(binary_outcome,
                                          predictor_variables), drop = FALSE]) == FALSE)) {
    newdata <- newdata[stats::complete.cases(newdata[,c(binary_outcome,
                                                        predictor_variables), drop = FALSE])
                       , , drop = FALSE]
    warning(paste("Some rows of newdata have been removed due to missing data in either the outcome variable and/or predictor variables.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data",
                  sep = ''))
  }

  ################# EXTRACT OUTCOMES FROM NEWDATA ######################
  if (is.null(binary_outcome)) {
    Outcomes <- NULL
  }else {
    Outcomes <- newdata[,binary_outcome]
  }

  ######################### RETURN RESULTS #############################
  list(modelinfo = x,
       PredictionData = newdata,
       Outcomes = Outcomes)
}


#' @export
map_newdata.predinfo_survival <- function(x,
                                          newdata,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL){

  ########################## INPUT CHECKING #############################
  # Check that supplied 'newdata' is a data.frame
  if (inherits(newdata, "data.frame") == FALSE) {
    stop("'newdata' should be a data.frame")
  }

  if (any(sapply(newdata, function(x) is.factor(x)))) {
    stop("'newdata' contains factor variables - convert to dummy/indicator variables first \n dummayvar() can help with this")
  }

  if (!is.null(binary_outcome)) {
    stop("'binary_outcome' should be set to NULL if model_type=survival")
  }
  if ((!is.null(survival_time) & is.null(event_indicator)) |
      (is.null(survival_time) & !is.null(event_indicator))) {
    stop("'survival_time' and 'event_indicator' should either both be NULL or both have values supplied for model_type == 'survival'")
  }
  if (!is.null(survival_time) & !is.null(event_indicator)) {
    if(!is.character(survival_time) |
       !is.character(event_indicator)) {
      stop("'survival_time' and 'event_indicator' should be supplied as a character variable")
    }
    if(length(survival_time) != 1 |
       length(event_indicator) != 1) {
      stop("length of 'survival_time' and 'event_indicator' should be one")
    }
    if(survival_time %in% names(newdata) == FALSE |
       event_indicator %in% names(newdata) == FALSE) {
      stop("'survival_time' and/or 'event_indicator' not found in 'newdata'")
    }
  }

  # Check that all predictor variables specified in the pminfo object are also
  # included in the 'newdata':
  if (x$M == 1) {
    predictor_variables <- x$coef_names
  } else{
    predictor_variables <- unique(unlist(x$coef_names))
  }
  if (all(predictor_variables %in% names(newdata)) == FALSE) {
    stop("newdata does not contain some of the predictor variables for the model(s) specified within the pminfo object")
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(newdata[,c(survival_time,
                                          event_indicator,
                                          predictor_variables), drop = FALSE]) == FALSE)) {
    newdata <- newdata[stats::complete.cases(newdata[,c(survival_time,
                                                        event_indicator,
                                                        predictor_variables), drop = FALSE])
                       , , drop = FALSE]
    warning(paste("Some rows of newdata have been removed due to missing data in either the outcome variable and/or predictor variables.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data",
                  sep = ''))
  }

  ################# EXTRACT OUTCOMES FROM NEWDATA ######################
  if (!is.null(survival_time) & !is.null(event_indicator)) {
    Outcomes <- survival::Surv(newdata[,survival_time],
                               newdata[,event_indicator])
  } else{
    Outcomes <- NULL
  }

  ######################### RETURN RESULTS #############################
  list(modelinfo = x,
       PredictionData = newdata,
       Outcomes = Outcomes)
}

