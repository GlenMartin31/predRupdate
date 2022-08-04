#' Input information about an existing prediction model
#'
#' Input relevant information about one or multiple xisting prediction model
#' (i.e. the functional form and published coefficients), and a new dataset, to
#' create a standardised 'blueprint' for further evaluation.
#'
#' @param model_type specifies the type of model that the existing prediction
#'   model is based on; possible options are: \itemize{ \item {\code{"logistic"}
#'   indicates that the existing model was based on a logistic regression model
#'   (default)} \item {\code{"survival"} indicates that the existing model was
#'   based on a survival regression model} } If multiple models are being
#'   entered (i.e., if \code{M}>1), then note that all such model need to be of
#'   the same type.
#' @param model_info a data.frame that contains the coefficients of the existing
#'   prediction model(s). Each column should be a predictor variable (with the
#'   name being the name of said predictor variable as specified in
#'   \code{newdata}), with the values being the coefficients, taken exactly as
#'   published from the existing prediction model(s). Multiple existing
#'   prediction models should be specified by entering multiple rows. If a
#'   predictor variable is not present in a given model then enter that cell of
#'   the data.frame as NA. See examples below.
#' @param newdata  data.frame upon which the prediction model should be applied
#'   (for subsequent validation/model updating/model aggregation).
#' @param baselinehazard A data.frame with two columns: (1) time, and (2)
#'   estimated baseline hazard at that time. Only relevant if \code{model_type}
#'   is "survival"; leave as NULL otherwise.
#' @param pre_processing a list where each element is a function that describes
#'   transformations to apply to columns of \code{newdata}. See "Details".
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
#' @details This function will structure the relevant information about one or
#'   more existing prediction model and a new dataset (on which one wishes to
#'   make predictions/ apply model updating/ apply model aggregation) into a
#'   standardised format, such that it can be used with other functions in the
#'   package.
#'
#'   The new dataset might be a validation dataset (to test the performance of
#'   the existing prediction model) and/or it might be the dataset on which one
#'   wishes to apply model updating methods to revise the model. In any case,
#'   this should be specified in \code{newdata} as a data.frame. Each row should
#'   be an observation (e.g. patient) and each variable/column should be a
#'   predictor variable. The predictor variables need to include (as a minimum)
#'   all of the predictor variables that are included in the existing prediction
#'   model (as specified in \code{formula} and \code{existingcoefs}).
#'
#'   Sometimes, it is necessary to transform some variables in the dataset prior
#'   to applying the model (e.g., if the existing model includes non-linear
#'   variable transformations, such as squared terms).
#'   \code{\link{pred_input_info}} provides mechanisms for applying such
#'   transformations by specifying \code{pre_processing}. \code{pre_processing}
#'   should be a list where each element is a function that applies the desired
#'   transformations/ pre-processing steps. Each function (list element) should
#'   have one input (the new data) and return either a single transformed
#'   variable (vector) or a data.frame/ list of multiple transformed variables.
#'
#'   The existing prediction model(s) will have a functional form (i.e. the
#'   linear predictor of the model); this will be taken as being a linear
#'   combination of the variables specified by the columns of \code{model_info}.
#'   As such, each column name (variable) of \code{model_info} must have a
#'   corresponding name (variable) in \code{newdata} (after applying any
#'   \code{pre_processing} steps to \code{newdata}).
#'
#'   Additionally, each of the predictor variables of the existing prediction
#'   model(s) will have a published coefficient (e.g. log-odds-ratio or
#'   log-hazard-ratio), which should each be given as the values in
#'   \code{model_info}. If entering information about multiple existing
#'   prediction models, then \code{model_info} will contain multiple rows (one
#'   per existing model). Here, if a given model does not contain a predictor
#'   variable that is included in another model, then set as NA; see examples of
#'   this below.
#'
#'   In the case of \code{model_type} = "logistic", then \code{model_info} must
#'   contain a column named as "Intercept", which gives the intercept
#'   coefficient of each of the existing logistic regression models (taken
#'   exactly as previously published). If \code{model_type} = "survival", then
#'   \code{baselinehazard} should be provided and no "Intercept" column is
#'   needed in \code{model_info}.
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
#' @return \code{\link{pred_input_info}} returns an object of class "predinfo",
#'   with child classes per model_type. This is a standardised format, such that
#'   it can be used with other functions in the package. An object of class
#'   "predinfo" is a list containing at least the following components:
#'   \itemize{ \item{model_type = this is the type of analytical model that the
#'   existing prediction model is based upon ("logistic" or "survival")}
#'   \item{coefs = this is the list of (previously estimated) coefficients for
#'   each predictor variable} \item{coef_names = gives the names of each
#'   predictor variable} \item{PredictionData = this is the data formed by
#'   any subsequent predictions/model updating/ model aggregation will be
#'   based on this data.} \item{Outcomes = vector of outcomes/endpoints (if
#'   specified in the input).} }
#'
#' @examples
#' #Example 1 - logistic regression existing model, with outcome specified; uses
#' #            an example dataset within the package
#' pred_input_info(model_type = "logistic",
#'                 model_info = SYNPM$Existing_models[1,],
#'                 newdata = SYNPM$ValidationData,
#'                  binary_outcome = "Y")
#'
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package. Also shows use of pre-processing to handle
#' #             categorical variables
#' pred_input_info(model_type = "survival",
#'                 model_info = data.frame("SEXM" = 0.53,
#'                                         "AGE" = -0.05,
#'                                         "SYSTBP" = -0.0055,
#'                                         "BMIO" = 0.0325,
#'                                         "CARDIAC" = -0.126,
#'                                         "DIABETES" = -0.461),
#'                  newdata = SMART,
#'                  baselinehazard = data.frame("t" = 1:5,
#'                                              "h" = c(0.12, 0.20, 0.26, 0.33, 0.38)),
#'                  survival_time = "TEVENT",
#'                  event_indicator = "EVENT",
#'                  pre_processing = list(function(x) predRupdate::dummyvars(x)))
#'
#'
#' #Example 3 - Input information about multiple models
#' pred_input_info(model_type = "logistic",
#'                 model_info = SYNPM$Existing_models,
#'                 newdata = SYNPM$ValidationData,
#'                  binary_outcome = "Y")
#'
#' #Example 4 - showing use of 'pre_processing' - the following are all valid ways
#' #            of specifying elements of 'pre_processing'
#' pred_input_info(model_type = "logistic",
#'                 model_info = data.frame("Intercept" = -5,
#'                                         "Age" = 0.05,
#'                                         "Age_squared" = 0.0005,
#'                                         "BMI_logged" = 0.006),
#'                 newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                      "BMI" = rnorm(100, 25, 0.5)),
#'                 pre_processing = list("Age_squared" = function(df) df$Age^2,
#'                                       "BMI_logged" = function(df) log(df$BMI)))
#' pred_input_info(model_type = "logistic",
#'                 model_info = data.frame("Intercept" = -5,
#'                                         "Age" = 0.05,
#'                                         "Age_squared" = 0.0005,
#'                                         "BMI_logged" = 0.006),
#'                 newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                      "BMI" = rnorm(100, 25, 0.5)),
#'                 pre_processing = list(function(df) {
#'                   Age_squared <- df$Age^2
#'                   BMI_logged <- log(df$BMI)
#'                   return(list("Age_squared" = Age_squared,
#'                               "BMI_logged" = BMI_logged))
#'                 }))
#' pred_input_info(model_type = "logistic",
#'                 model_info = data.frame("Intercept" = -5,
#'                                         "Age" = 0.05,
#'                                         "Age_squared" = 0.0005,
#'                                         "BMI_logged" = 0.006),
#'                 newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                      "BMI" = rnorm(100, 25, 0.5)),
#'                 pre_processing = list(function(df) {
#'                   df$Age_squared <- df$Age^2
#'                   df$BMI_logged <- log(df$BMI)
#'                   return(df)
#'                 }))
#'
#' @export
pred_input_info <- function(model_type = c("logistic", "survival"),
                            model_info,
                            newdata,
                            baselinehazard = NULL,
                            pre_processing = NULL,
                            binary_outcome = NULL,
                            survival_time = NULL,
                            event_indicator = NULL) {

  ########################## INPUT CHECKING #############################
  model_type <- match.arg(model_type)

  pred_input_info_input_checks(model_type = model_type,
                               model_info = model_info,
                               newdata = newdata,
                               baselinehazard = baselinehazard,
                               pre_processing = pre_processing,
                               binary_outcome = binary_outcome,
                               survival_time = survival_time,
                               event_indicator = event_indicator)

  M <- nrow(model_info) #number of existing models that info has been entered about

  ########### APPLY PRE-PROCESSING TO NEWDATA AS NEEDED ##################
  if (!is.null(pre_processing)) {
    newdata <- apply_pre_processing(newdata = newdata,
                                    pre_processing = pre_processing)
  }
  #now any pre-processing is complete, check that all predictor variables
  # specified in 'model_info' are also included in the 'newdata':
  if (model_type == "logistic") {
    predictor_variables <- names(model_info)[-which(names(model_info)=="Intercept")]
  } else {
    predictor_variables <- names(model_info)
  }
  if (all(predictor_variables %in% names(newdata)) == FALSE) {
      stop("Ensure that all predictor variables specified in 'model_info' are also in 'newdata'",
           call. = FALSE)
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(newdata[,c(binary_outcome,
                                          survival_time,
                                          event_indicator,
                                          predictor_variables), drop = FALSE]) == FALSE)) {
    newdata <- newdata[stats::complete.cases(newdata[,c(binary_outcome,
                                                        survival_time,
                                                        event_indicator,
                                                        predictor_variables), drop = FALSE])
                       , , drop = FALSE]
    warning(paste("Some rows of newdata have been removed due to missing data in either the outcome variables and/or variables specified in 'formula'.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data in newdata prior to calling pred_input_info()",
                  sep = ''),
            call. = FALSE)
  }

  ############### EXTRACT INFORMATION BY MODEL TYPE ######################
  if (model_type == "logistic") {

    ##### Extract the Outcomes from newdata if specified by user:
    if (is.null(binary_outcome)) {
      Outcomes <- NULL
    }else {
      Outcomes <- newdata[,binary_outcome]
    }

    if (M > 1){
      existingcoefs <- vector(mode = "list", length = M)
      form <- vector(mode = "list", length = M)
      vars <- vector(mode = "list", length = M)
      for (m in 1:M) {
        vars[[m]] <- names(model_info[,which(!is.na(model_info[m,]))])
        form[[m]] <- stats::as.formula(paste("~",
                                             paste(vars[[m]][-which(vars[[m]]=="Intercept")],
                                                   collapse = "+"),
                                             sep = ''))
        existingcoefs[[m]] <- model_info[m, vars[[m]]]
      }
      info_vals <- list(M = M,
                        model_type = model_type,
                        coefs = existingcoefs,
                        coef_names = lapply(existingcoefs, names),
                        formula = form,
                        PredictionData = newdata,
                        Outcomes = Outcomes)
    } else {
      vars <- names(model_info[1,which(!is.na(model_info[1,]))])
      form <- stats::as.formula(paste("~",
                                      paste(vars[-which(vars=="Intercept")],
                                            collapse = "+"),
                                      sep = ''))
      existingcoefs <- model_info[1, vars]
      info_vals <- list(M = M,
                        model_type = model_type,
                        coefs = existingcoefs,
                        coef_names = names(existingcoefs),
                        formula = form,
                        PredictionData = newdata,
                        Outcomes = Outcomes)
    }
    #Set the S3 class
    class(info_vals) <- c("predinfo_logistic", "predinfo")

  } else if (model_type == "survival") {

    ##### Extract the Outcomes from newdata if specified by user:
    if (!is.null(survival_time) & !is.null(event_indicator)) {
      Outcomes <- survival::Surv(newdata[,survival_time],
                                   newdata[,event_indicator])
    } else{
        Outcomes <- NULL
    }

    if (M > 1){
      existingcoefs <- vector(mode = "list", length = M)
      form <- vector(mode = "list", length = M)
      vars <- vector(mode = "list", length = M)
      for (m in 1:M) {
        vars[[m]] <- names(model_info[,which(!is.na(model_info[m,]))])
        form[[m]] <- stats::as.formula(paste("~",
                                             paste(vars[[m]], collapse = "+"),
                                             sep = ''))
        existingcoefs[[m]] <- model_info[m, vars[[m]]]
      }
      info_vals <- list(M = M,
                        model_type = model_type,
                        coefs = existingcoefs,
                        coef_names = lapply(existingcoefs, names),
                        formula = form,
                        baselinehazard = baselinehazard,
                        PredictionData = newdata,
                        Outcomes = Outcomes)
    } else {
      vars <- names(model_info[1,which(!is.na(model_info[1,]))])
      form <- stats::as.formula(paste("~",
                                      paste(vars, collapse = "+"),
                                      sep = ''))
      existingcoefs <- model_info[1, vars]
      info_vals <- list(M = M,
                        model_type = model_type,
                        coefs = existingcoefs,
                        coef_names = names(existingcoefs),
                        formula = form,
                        baselinehazard = baselinehazard,
                        PredictionData = newdata,
                        Outcomes = Outcomes)
    }

    #Set the S3 class
    class(info_vals) <- c("predinfo_survival", "predinfo")
  }

  info_vals
}


#' @export
print.predinfo <- function(x, ...) {

  if (is.list(x$formula) & is.list(x$coefs) & is.list(x$coef_names)) {
    M <- length(x$formula)
  } else{
    M <- 1
  }

  cat(paste("Information entered about", M, "existing models",
            paste("of type '", x$model_type, "' \n \n", sep = ""), sep = " "))

  if (M > 1) {
    cat("Coefficients: \n")
    print(lapply(x$coefs, function(X) paste(X, collapse = ", ")))
  } else{
    cat("Coefficients: \n")
    print(paste(x$coefs, collapse = ", "))
  }

  if (M > 1) {
    cat("Predictors: \n")
    print(lapply(x$coef_names, function(X) paste(X, collapse = ", ")))
    cat("\n \n")
  } else{
    cat("Predictors: \n")
    print(paste(x$coef_names, collapse = ", "))
    cat("\n \n")
  }

  if (nrow(x$PredictionData) < 6) {
    cat("Data on which predictions will be made has dimension",
        nrow(x$PredictionData), "by", ncol(x$PredictionData), ":\n")
    print(x$PredictionData)
  } else{
    cat("Data on which predictions will be made has dimension",
        nrow(x$PredictionData), "by", ncol(x$PredictionData), ":\n")
    print(utils::head(x$PredictionData))
    cat("...plus",  nrow(x$PredictionData)-6, "other rows \n")
  }
}



# Internal functions for pred_input_info() ---------------------------------------

pred_input_info_input_checks <- function(model_type,
                                         model_info,
                                         newdata,
                                         baselinehazard,
                                         pre_processing,
                                         binary_outcome,
                                         survival_time,
                                         event_indicator) {

  #check that model_info is provided as a data.frame
  if (inherits(model_info, "data.frame") == FALSE) {
    stop("'model_info' should be a data.frame", call. = FALSE)
  }
  #Check that each 'cell' of model_info data.frame is a number
  if (all(sapply(model_info, is.numeric)) == FALSE) {
    stop("All columns of 'model_info' should be numeric", call. = FALSE)
  }

  # Check that supplied 'newdata' is a data.frame
  if (inherits(newdata, "data.frame") == FALSE) {
    stop("'newdata' should be a data.frame", call. = FALSE)
  }

  # Check that any supplied outcome variables are contained in 'newdata' and that
  # they are specified correctly
  if (model_type == "logistic") {

    #check that 'model_info' contains an intercept column for logistic models
    if ("Intercept" %in% names(model_info) == FALSE) {
      stop("When model_type=logistic, then 'model_info' should contain a column named 'Intercept'",
           call. = FALSE)
    }

    if (!is.null(survival_time)) {
      stop("'survival_time' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }
    if (!is.null(event_indicator)) {
      stop("'event_indicator' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }
    if (!is.null(baselinehazard)) {
      stop("'baselinehazard' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }
    if(!is.null(binary_outcome)) {
      if(!is.character(binary_outcome)) {
        stop("'binary_outcome' should be supplied as a character variable",
             call. = FALSE)
      }
      if(length(binary_outcome) != 1) {
        stop("length of 'binary_outcome' should be one",
             call. = FALSE)
      }
      if(binary_outcome %in% names(newdata) == FALSE) {
        stop("'binary_outcome' not found in 'newdata'",
             call. = FALSE)
      }
    }

  } else if (model_type == "survival") {

    if (!is.null(binary_outcome)) {
      stop("'binary_outcome' should be set to NULL if model_type=survival",
           call. = FALSE)
    }
    if ((!is.null(survival_time) & is.null(event_indicator)) |
        (is.null(survival_time) & !is.null(event_indicator))) {
      stop("'survival_time' and 'event_indicator' should either both be NULL or both have values supplied for model_type == 'survival'",
           call. = FALSE)
    }
    if (!is.null(survival_time) & !is.null(event_indicator)) {
      if(!is.character(survival_time) |
         !is.character(event_indicator)) {
        stop("'survival_time' and 'event_indicator' should be supplied as a character variable",
             call. = FALSE)
      }
      if(length(survival_time) != 1 |
         length(event_indicator) != 1) {
        stop("length of 'survival_time' and 'event_indicator' should be one",
             call. = FALSE)
      }
      if(survival_time %in% names(newdata) == FALSE |
         event_indicator %in% names(newdata) == FALSE) {
        stop("'survival_time' and/or 'event_indicator' not found in 'newdata'",
             call. = FALSE)
      }
    }
    #check baseline hazard specification
    if (is.null(baselinehazard)) {
      stop("'baselinehazard' should be provided if model_type=survival",
           call. = FALSE)
    }
    if (nrow(model_info) > 1) {
      if ((inherits(baselinehazard, "list") == FALSE) |
         (length(baselinehazard) != nrow(model_info) )) {
        stop("If multiple existing models entered, and model_type = survival, then 'baselinehazard' should be supplied as list of length equal to number of models",
             call. = FALSE)
      }
      if (any(sapply(baselinehazard, function(X) (inherits(X, "data.frame") == FALSE) | ncol(X) !=2))) {
        stop("each supplied baseline hazard should be a data.frame of two columns",
             call. = FALSE)
      }
      if (any(sapply(baselinehazard, function(X) sum(duplicated(X[,1])) > 0))) {
        stop("all baseline hazard times must be unique",
             call. = FALSE)
      }
      if (any(sapply(baselinehazard, function(X) min(X[,1]) <= 0))) {
        stop("all baseline hazard times must be positive",
             call. = FALSE)
      }
      if (any(sapply(baselinehazard, function(X) min(X[,2]) < 0))) {
        stop("all baseline hazards must be nonnegative",
             call. = FALSE)
      }

    } else {
      if (inherits(baselinehazard, "data.frame") == FALSE |
          ncol(baselinehazard) !=2) {
        stop("baseline hazard should be a data.frame of two columns",
             call. = FALSE)
      }
      if(sum(duplicated(baselinehazard[,1])) > 0){
        stop("all baseline hazard times must be unique",
             call. = FALSE)
      }
      if(min(baselinehazard[,1]) <= 0){
        stop("all baseline hazard times must be positive",
             call. = FALSE)
      }
      if(min(baselinehazard[,2]) < 0){
        stop("all baseline hazards must be nonnegative",
             call. = FALSE)
      }
    }

  }

}


set_existing_coefs <- function(existingcoefs,
                               formula,
                               newdata,
                               model_type) {

  ##Define the design matrix given the provided functional form of the existing
  #model:
  DM <- stats::model.matrix(formula, newdata)
  if(model_type == "survival") {
    #for survival model, remove the intercept column that is created in DM:
    DM <- DM[ ,-which(colnames(DM) == "(Intercept)"), drop=FALSE]
  }

  #Check that each column of the design matrix produced by user-supplied
  #formula is included as an element of 'existingcoefs', and vice versa:
  if (all(names(existingcoefs) %in% colnames(DM)) == FALSE |
      all(colnames(DM) %in% names(existingcoefs)) == FALSE) {
    stop(paste("existingcoefs and design matrix are inconsistent. Variables in existingcoefs are: \n",
               paste(names(existingcoefs), collapse = ", "),
               "\n
               While variables in the design matrix (produced by applying 'formula' to 'newdata' arguments) are: \n",
               paste(colnames(DM), collapse = " ,"),
               "\n
               Check for inconsistencies in 'existingcoefs', 'formula' and 'newdata'.
               Pay particular attention to any factor variables in 'newdata', and compare the result of applying stats::model.matrix(formula, newdata) to produce the design matrix with names of 'existingcoefs'.",
               sep = ''),
         call. = FALSE)
  }
  #Ensure that order of 'existingcoefs' matches the design matrix that is
  #produced by 'formula':
  existingcoefs <- existingcoefs[colnames(DM)]
  existingcoefs
}






