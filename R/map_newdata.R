#' Map new data to a predinfo object
#'
#' This function takes a \code{predinfo} object and applies (maps) a new data to
#' this object to check there is consistency between the two. This function is
#' not usually called directly, but rather within  other functions within the
#' package, such as \code{pred_predict}.
#'
#' @param x an object of class "predinfo".
#' @param new_data data.frame upon which the prediction model should be applied
#'   (for subsequent validation/model updating/model aggregation).
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{new_data} that represents the observed binary outcomes (should be
#'   coded 0 and 1 for non-event and event, respectively). Only relevant for
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
#'
#' @details This function maps a new dataset onto a \code{pred_info} object. The
#'   new dataset might be a validation dataset (to test the performance of the
#'   existing prediction model) and/or it might be the dataset on which one
#'   wishes to apply model updating methods to revise the model. In any case,
#'   this should be specified in \code{new_data} as a data.frame. Each row
#'   should be an observation (e.g. patient) and each variable/column should be
#'   a predictor variable. The predictor variables need to include (as a
#'   minimum) all of the predictor variables that are included in the existing
#'   prediction model (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{new_data}).
#'
#'   Any factor variables within \code{new_data} must be converted to dummy
#'   (0/1) variables before calling this function. \code{\link{dummy_vars}} can
#'   help with this.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{new_data}, if relevant
#'   (use \code{binary_outcome} if \code{model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{model_type} =
#'   "survival"). For example, if validating an existing model, then these
#'   inputs specify the columns of \code{new_data} that will be used for
#'   assessing predictive performance of the predictions in the validation
#'   dataset. If \code{new_data} does not contain outcomes, then leave these
#'   inputs to the default of \code{NULL}.
#'
#' @return Returns a list of the predinfo object, the new_data, and outcomes.
#'
#' @examples
#' #as above, this function is not usually called directly, but an example of
#' #such use is:
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_logistic_models[1,])
#' map_newdata(x = model1,
#'             new_data = SYNPM$ValidationData[1:10,],
#'             binary_outcome = "Y")
#'
#' @export
map_newdata <- function(x,
                        new_data,
                        binary_outcome = NULL,
                        survival_time = NULL,
                        event_indicator = NULL) {
  UseMethod("map_newdata")
}

#' @export
map_newdata.default <- function(x,
                                new_data,
                                binary_outcome = NULL,
                                survival_time = NULL,
                                event_indicator = NULL) {
  stop("'x' is not of class 'predinfo'")
}

#' @export
map_newdata.predinfo_logistic <- function(x,
                                          new_data,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL){

  ########################## INPUT CHECKING #############################
  # double-check x object
  pred_input_info_input_checks(model_type = x$model_type,
                               model_info = x$model_info,
                               cum_hazard = NULL)

  # Check that supplied 'new_data' is a data.frame
  if (inherits(new_data, "data.frame") == FALSE) {
    stop("'new_data' should be a data.frame")
  }

  if (any(sapply(new_data, function(x) is.factor(x)))) {
    stop("'new_data' contains factor variables - convert to dummy/indicator variables first \n dummy_vars() can help with this")
  }

  if (any(sapply(new_data, function(x) is.character(x)))) {
    warning("'new_data' contains character variables - should these be indicator variables (see dummy_vars())?")
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
    if(binary_outcome %in% names(new_data) == FALSE) {
      stop("'binary_outcome' not found in 'new_data'")
    }
    if(all(unique(new_data[[binary_outcome]]) %in% c(0,1)) == FALSE){
      stop("The 'binary_outcome' column of 'new_data' should only contain 0 and 1s")
    }
  }

  # Check that all predictor variables specified in the pminfo object are also
  # included in the 'new_data':
  if (x$M == 1) {
    predictor_variables <- x$coef_names[-which(x$coef_names=="Intercept")]
  } else{
    predictor_variables <- unique(unlist(x$coef_names))
    predictor_variables <- predictor_variables[-which(predictor_variables=="Intercept")]
  }
  if (all(predictor_variables %in% names(new_data)) == FALSE) {
    stop("new_data does not contain some of the predictor variables for the model(s) specified within the pminfo object")
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(new_data[,c(binary_outcome,
                                          predictor_variables), drop = FALSE]) == FALSE)) {
    new_data <- new_data[stats::complete.cases(new_data[,c(binary_outcome,
                                                        predictor_variables), drop = FALSE])
                       , , drop = FALSE]
    warning(paste("Some rows of new_data have been removed due to missing data in either the outcome variable and/or predictor variables.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data",
                  sep = ''))
  }

  ################# EXTRACT OUTCOMES FROM NEWDATA ######################
  if (is.null(binary_outcome)) {
    Outcomes <- NULL
  }else {
    Outcomes <- new_data[[binary_outcome]]
  }

  ######################### RETURN RESULTS #############################
  list(modelinfo = x,
       PredictionData = new_data,
       Outcomes = Outcomes)
}


#' @export
map_newdata.predinfo_survival <- function(x,
                                          new_data,
                                          binary_outcome = NULL,
                                          survival_time = NULL,
                                          event_indicator = NULL){

  ########################## INPUT CHECKING #############################
  # double-check x object
  pred_input_info_input_checks(model_type = x$model_type,
                               model_info = x$model_info,
                               cum_hazard = x$cum_hazard)

  # Check that supplied 'new_data' is a data.frame
  if (inherits(new_data, "data.frame") == FALSE) {
    stop("'new_data' should be a data.frame")
  }

  if (any(sapply(new_data, function(x) is.factor(x)))) {
    stop("'new_data' contains factor variables - convert to dummy/indicator variables first \n dummayvar() can help with this")
  }

  if (any(sapply(new_data, function(x) is.character(x)))) {
    warning("'new_data' contains character variables - should these be indicator variables (see dummy_vars())?")
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
    if(survival_time %in% names(new_data) == FALSE |
       event_indicator %in% names(new_data) == FALSE) {
      stop("'survival_time' and/or 'event_indicator' not found in 'new_data'")
    }
    if(all(unique(new_data[[event_indicator]]) %in% c(0,1)) == FALSE){
      stop("The 'event_indicator' column of 'new_data' should only contain 0 and 1s")
    }
  }

  # Check that all predictor variables specified in the pminfo object are also
  # included in the 'new_data':
  if (x$M == 1) {
    predictor_variables <- x$coef_names
  } else{
    predictor_variables <- unique(unlist(x$coef_names))
  }
  if (all(predictor_variables %in% names(new_data)) == FALSE) {
    stop("new_data does not contain some of the predictor variables for the model(s) specified within the pminfo object")
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(new_data[,c(survival_time,
                                          event_indicator,
                                          predictor_variables), drop = FALSE]) == FALSE)) {
    new_data <- new_data[stats::complete.cases(new_data[,c(survival_time,
                                                        event_indicator,
                                                        predictor_variables), drop = FALSE])
                       , , drop = FALSE]
    warning(paste("Some rows of new_data have been removed due to missing data in either the outcome variable and/or predictor variables.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data",
                  sep = ''))
  }

  ################# EXTRACT OUTCOMES FROM NEWDATA ######################
  if (!is.null(survival_time) & !is.null(event_indicator)) {
    Outcomes <- survival::Surv(new_data[[survival_time]],
                               new_data[[event_indicator]])
  } else{
    Outcomes <- NULL
  }

  ######################### RETURN RESULTS #############################
  list(modelinfo = x,
       PredictionData = new_data,
       Outcomes = Outcomes)
}

