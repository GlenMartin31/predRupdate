#' Input information about an existing prediction model
#'
#' Input relevant information about an existing prediction model (i.e. the
#' functional form and published coefficients), and a new dataset, to create a
#' standardised 'blueprint' for further evaluation.
#'
#' @param model_type specifies the type of model that the existing prediction
#'   model is based on; possible options are: \itemize{ \item {\code{"logistic"}
#'   indicates that the existing model was based on a logistic regression model
#'   (default)} \item {\code{"survival"} indicates that the existing model was
#'   based on a survival regression model} }
#' @param existingcoefs a named vector of coefficients, taken exactly as
#'   published from the existing prediction model. Names much match variables in
#'   \code{newdata} (after any \code{pre_processing}). There should be one
#'   coefficient for each predictor variable in the model. If \code{model_type}
#'   is "logistic", then an intercept should be provided. See "Details".
#' @param formula an object of class "\code{\link[stats]{formula}}" (or a
#'   character string that can be coerced to that class). This specifies the
#'   functional form of the existing prediction model. See "Details".
#' @param newdata  data.frame upon which the prediction model should be applied
#'   (for subsequent validation/model updating/model aggregation). Variable
#'   names must match those in \code{existingcoefs} and \code{formula} (after
#'   any \code{pre_processing}). See "Details".
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
#' @details This function will structure the relevant information about an
#'   existing prediction model and a new dataset (on which one wishes to make
#'   predictions/ apply model updating/ apply model aggregation) into a
#'   standardised format, such that it can be used with other functions in the
#'   package.
#'
#'   The new dataset might be a validation dataset (to test the performance of
#'   the existing prediction model) and/or it might be the dataset on which one
#'   wishes to apply model updating methods to 'tweak' the model. In any case,
#'   this should be specified in \code{newdata} as a data.frame. Each row should
#'   be an observation (e.g. patient) and each variable/column should be a
#'   predictor variable. The predictor variables need to include (as a minimum)
#'   all of the predictor variables that are included in the existing prediction
#'   model (as specified in \code{formula} and \code{existingcoefs}).
#'
#'   Sometimes, it is necessary to transform some variables in the dataset prior
#'   to applying the model (e.g., if the existing model includes splines, or
#'   non-linear variable transformations, such as squared terms).
#'   \code{\link{pred_input_info}} provides mechanisms for applying such
#'   transformations by specifying \code{pre_processing}. \code{pre_processing}
#'   should be a list where each element is a function that applies the desired
#'   transformations/ pre-processing steps. Each function (list element) should
#'   have one input (the new data) and return either a single transformed
#'   variable (vector) or a data.frame/ list of multiple transformed variables.
#'   In the case of returning a vector, the list element can be named such that
#'   the name of the list element becomes the new variable name; if returning a
#'   data.frame/list, then each new variable should be named internally within
#'   the function. See "Examples" below.
#'
#'   The existing prediction model will have a functional form (i.e. the linear
#'   predictor of the model); this should be specified in \code{formula}. For
#'   example, if the existing prediction model included "age" and "BMI" as the
#'   predictor variables, then \code{formula} would be entered as "\code{~ age +
#'   BMI}". Each variable name in \code{formula} must have a corresponding name
#'   in \code{newdata} (after applying any \code{pre_processing} steps).
#'   Additionally, each predictor variable specified in \code{formula} must have
#'   a corresponding named element in \code{existingcoefs}. Only the right hand
#'   side of the formula is needed.
#'
#'   Each of the predictor variables included in the existing prediction model
#'   will have a published coefficient (e.g. log-odds-ratio or
#'   log-hazard-ratio), which should each be specified in \code{existingcoefs}.
#'   \code{existingcoefs} should be provided as a named numeric vector, where
#'   each name directly matches those in \code{formula} and in \code{newdata}
#'   (after applying any \code{pre_processing} steps). The values are the
#'   corresponding coefficient estimates taken \strong{exactly} as published by
#'   the existing model. Note, these coefficients should not normally be
#'   re-evaluated in the current data, especially if the \code{newdata} is being
#'   used for validation of the existing prediction model. In the case of
#'   \code{model_type} = "logistic", the intercept of the existing prediction
#'   model must be named as "(Intercept)". If \code{model_type} = "survival",
#'   then \code{baselinehazard} should be provided.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{newdata}, if relevant
#'   (use \code{binary_outcome} if \code{model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{model_type} =
#'   "survival"). For example, if validating an existing model, then these
#'   inputs specify the columns of \code{newdata} that will be used for
#'   assessing predictive performance of the predictions in the validation
#'   dataset. If \code{newdata} does not contain outcomes, then leave
#'   these inputs to the default of \code{NULL}.
#'
#' @return \code{\link{pred_input_info}} returns an object of class "predinfo", with
#'   child classes per model_type. This is a standardised format, such that it
#'   can be used with other functions in the package. An object of class
#'   "predinfo" is a list containing at least the following components:
#'   \itemize{
#'      \item{model_type = this is the type of analytical model that the
#'      existing prediction model is based upon ("logistic" or "survival")}
#'      \item{coefs = this is the list of (previously estimated) coefficients
#'      for each predictor variable}
#'      \item{coef_names = gives the names of each predictor variable}
#'      \item{PredictionData = this is the design matrix formed by mapping the
#'      specified \code{pre_processing} steps (if relevant) and functional form
#'      of the existing prediction model specified in \code{formula} onto
#'      \code{newdata}; any subsequent predictions/model updating/
#'      model aggregation will be based on this data.}
#'      \item{Outcomes = vector of outcomes/endpoints (if specified in the
#'      input).}
#'      }
#'
#' @examples
#' #Example 1 - logistic regression existing model, with outcome specified, and
#' #            handling of categorical variable in 'pre_processing'; uses
#' #            package dataset
#' existing_cpm_info <- pred_input_info(model_type = "logistic",
#'                                      existingcoefs = c("(Intercept)" = -3.0893961710923,
#'                                                        "Age" = 0.0230955938292795,
#'                                                        "SexM" = 0.263578567485447,
#'                                                        "Smoking_Status" = 0.689825139075564,
#'                                                        "Diabetes" = 0.387810349702088,
#'                                                        "CKD" = 0.56129156010678),
#'                                      formula = formula(SYNPM$Existing_models$Formula[2]),
#'                                      newdata = SYNPM$ValidationData,
#'                                      pre_processing = list(function(df) {dummyvars(df)}),
#'                                      binary_outcome = "Y")
#'
#' #Example 2 - survival model example; uses package dataset
#' pred_input_info(model_type = "survival",
#'                 existingcoefs = c("SEX" = 0.53,
#'                                   "AGE" = -0.05,
#'                                   "SYSTBP" = -0.0055,
#'                                   "BMIO" = 0.0325,
#'                                   "CARDIAC" = -0.126,
#'                                   "DIABETES" = -0.461),
#'                 formula = ~ SEX + AGE + SYSTBP + BMIO + CARDIAC + DIABETES,
#'                 newdata = SMART,
#'                 baselinehazard = data.frame("t" = 1:5,
#'                                             "h" = c(0.12, 0.20, 0.26, 0.33, 0.38)),
#'                 survival_time = "TEVENT",
#'                 event_indicator = "EVENT")
#'
#'
#' #Example 3 - example of incorrect specification; here, the intercept in
#' #            'existingcoefs' is incorrectly named; will return an error
#' \dontrun{
#' pred_input_info(model_type = "logistic",
#'                 existingcoefs = c("Intercept" = -2, "X" = 0.5),
#'                 formula = ~X,
#'                 newdata = data.frame("X" = rnorm(100)),
#'                 pre_processing = NULL)
#'          }
#'
#'
#' #Example 4 - example of incorrect specification; here, the existing
#' #            prediction model is specified as having a functional form of
#' #            X+Z (in 'formula') with corresponding 'existingcoefs', but Z does
#' #            not exist in 'newdata'; will return an error
#' \dontrun{
#' pred_input_info(model_type = "logistic",
#'                 existingcoefs = c("(Intercept)" = -2, "X" = 0.5, "Z" = 0.9),
#'                 formula = ~X + Z,
#'                 newdata = data.frame("X" = rnorm(100)),
#'                 pre_processing = NULL)
#'          }
#'
#'
#' #Example 5 - showing use of 'pre_processing' - the following are all valid ways
#' #            of specifying elements of 'pre_processing'
#' pred_input_info(model_type = "logistic",
#'                 existingcoefs = c("(Intercept)" = -5,
#'                                   "Age" = 0.05,
#'                                   "Age_squared" = 0.0005,
#'                                   "BMI_logged" = 0.006),
#'                 formula = ~Age + Age_squared + BMI_logged,
#'                 newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                      "BMI" = rnorm(100, 25, 0.5)),
#'                 pre_processing = list("Age_squared" = function(df) df$Age^2,
#'                                       "BMI_logged" = function(df) log(df$BMI)))
#' pred_input_info(model_type = "logistic",
#'                 existingcoefs = c("(Intercept)" = -5,
#'                                   "Age" = 0.05,
#'                                   "Age_squared" = 0.0005,
#'                                   "BMI_logged" = 0.006),
#'                 formula = ~Age + Age_squared + BMI_logged,
#'                 newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                      "BMI" = rnorm(100, 25, 0.5)),
#'                 pre_processing = list(function(df) {
#'                   Age_squared <- df$Age^2
#'                   BMI_logged <- log(df$BMI)
#'                   return(list("Age_squared" = Age_squared,
#'                               "BMI_logged" = BMI_logged))
#'                 }))
#' pred_input_info(model_type = "logistic",
#'                 existingcoefs = c("(Intercept)" = -5,
#'                                   "Age" = 0.05,
#'                                   "Age_squared" = 0.0005,
#'                                   "BMI_logged" = 0.006),
#'                 formula = ~Age + Age_squared + BMI_logged,
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
                            existingcoefs,
                            formula,
                            newdata,
                            baselinehazard = NULL,
                            pre_processing = NULL,
                            binary_outcome = NULL,
                            survival_time = NULL,
                            event_indicator = NULL) {

  model_type <- match.arg(model_type)
  formula <- stats::as.formula(formula)

  ########################## INPUT CHECKING #############################
  pred_input_info_input_checks(model_type = model_type,
                               existingcoefs = existingcoefs,
                               formula = formula,
                               newdata = newdata,
                               baselinehazard = baselinehazard,
                               pre_processing = pre_processing,
                               binary_outcome = binary_outcome,
                               survival_time = survival_time,
                               event_indicator = event_indicator)


  ########### APPLY PRE-PROCESSING TO NEWDATA AS NEEDED ##################
  if (!is.null(pre_processing)) {
    newdata <- apply_pre_processing(newdata = newdata,
                                    pre_processing = pre_processing)
  }
  #now any pre-processing is complete, check that all predictor variables
  # specified in 'formula' are also included in the 'newdata':
  if (all(all.vars(formula) %in% names(newdata)) == FALSE) {
    stop("Ensure that all predictor variables specified in 'formula' are also in 'newdata'",
         call. = FALSE)
  }


  ##################### HANDLE MISSING DATA ############################
  # Perform complete case analysis across relevant columns:
  if(any(stats::complete.cases(newdata[,c(binary_outcome,
                                          survival_time,
                                          event_indicator,
                                          all.vars(formula)), drop = FALSE]) == FALSE)) {
    newdata <- newdata[stats::complete.cases(newdata[,c(binary_outcome,
                                                        survival_time,
                                                        event_indicator,
                                                        all.vars(formula)), drop = FALSE])
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

    ##Define the design matrix given the provided functional form of the existing
    #model:
    DM <- stats::model.matrix(formula, newdata)

    #Standardise the supplied existing coefficients according to the DM, running
    #checks to ensure that each column of DM has a specified existing coefficient
    #and vice versa:
    existingcoefs <- set_existing_coefs(existingcoefs = existingcoefs,
                                        DM = DM)

    #Set the S3 class  and return the 'blueprint'
    info_vals <- list(model_type = model_type,
                      coefs = as.numeric(existingcoefs),
                      coef_names = names(existingcoefs),
                      PredictionData = DM,
                      Outcomes = Outcomes)
    class(info_vals) <- c("predinfo_logistic", "predinfo")

  } else if (model_type == "survival") {

    ##### Extract the Outcomes from newdata if specified by user:
    if (!is.null(survival_time) & !is.null(event_indicator)) {
      Outcomes <- survival::Surv(newdata[,survival_time],
                                   newdata[,event_indicator])
    }else{
        Outcomes <- NULL
    }

    ##Define the design matrix given the provided functional form of the existing
    #model:
    DM <- stats::model.matrix(formula, newdata)
    #for survival model, remove the intercept column that is created in DM:
    DM <- DM[ ,-which(colnames(DM) == "(Intercept)"), drop=FALSE]

    #Standardise the supplied existing coefficients according to the DM, running
    #checks to ensure that each column of DM has a specified existing coefficient
    #and vice versa:
    existingcoefs <- set_existing_coefs(existingcoefs = existingcoefs,
                                        DM = DM)

    #Set the S3 class  and return the 'blueprint'
    info_vals <- list(model_type = model_type,
                      coefs = as.numeric(existingcoefs),
                      coef_names = names(existingcoefs),
                      baselinehazard = baselinehazard,
                      PredictionData = DM,
                      Outcomes = Outcomes)
    class(info_vals) <- c("predinfo_survival", "predinfo")
  }

  info_vals
}


#' @export
print.predinfo <- function(x, ...) {
  cat("Existing Prediction Model of type '", x$model_type, "' \n \n", sep = "")
  cat("Coefficients =", paste(x$coefs, collapse = ", "), "\n")
  cat("Predictors =", paste(x$coef_names, collapse = ", "), "\n \n")
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

  if (x$model_type == "survival") {
    if (nrow(x$baselinehazard) < 6) {
      cat("\n Baseline hazard data has dimension",
          nrow(x$baselinehazard), "by", ncol(x$baselinehazard), ":\n")
      print(x$baselinehazard)
    } else{
      cat("\n Baseline hazard data has dimension",
          nrow(x$baselinehazard), "by", ncol(x$baselinehazard), ":\n")
      print(utils::head(x$baselinehazard))
      cat("...plus",  nrow(x$baselinehazard)-6, "other rows \n")
    }
  }

  if (!is.null(x$Outcomes)) {
    cat("Outcomes are avaliable for model validation: \n")
    print(utils::head(x$Outcomes))
    cat("...plus",  length(x$Outcomes)-6, "other rows \n")
  }
}




# Internal functions for pred_input_info() ---------------------------------------

pred_input_info_input_checks <- function(model_type,
                                         existingcoefs,
                                         formula,
                                         newdata,
                                         baselinehazard,
                                         pre_processing,
                                         binary_outcome,
                                         survival_time,
                                         event_indicator) {
  # Check that 'existingcoefs' is supplied as a named numeric vector
  if (!is.vector(existingcoefs) |
      !is.numeric(existingcoefs) |
      is.null(names(existingcoefs))) {
    stop("'existingcoefs' should be a named numeric vector", call. = FALSE)
  }

  # Check that supplied 'newdata' is a data.frame
  if (!is.data.frame(newdata)) {
    stop("'newdata' should be a data.frame", call. = FALSE)
  }

  # Check that any supplied outcome variables are contained in 'newdata' and that
  # they are specified correctly
  if (model_type == "logistic") {

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
    if(!is.null(binary_outcome)) { #if binary_outcome is supplied, check in newdata
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
    if (!is.null(survival_time) & !is.null(event_indicator)) {
      #if survival_time & event_indicator are supplied, check in newdata
      if(survival_time %in% names(newdata) == FALSE |
         event_indicator %in% names(newdata) == FALSE) {
        stop("'survival_time' and/or 'event_indicator' not found in 'newdata'",
             call. = FALSE)
      }
    }
    if ((!is.null(survival_time) & is.null(event_indicator)) |
        (is.null(survival_time) & !is.null(event_indicator))) {
      stop("'survival_time' and 'event_indicator' should either both be NULL or both have values supplied for model_type == 'survival'",
           call. = FALSE)
    }
    #check baseline hazard specification
    if (is.null(baselinehazard)) {
      stop("'baselinehazard' should be provided if model_type=survival",
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


set_existing_coefs <- function(existingcoefs,
                               DM) {
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






