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
#'   published from the existing prediction model. Names much match those in
#'   \code{newdata}. See "Details".
#' @param baselinehazard A data.frame with two columns: (1) time, and (2)
#'   estimated baseline hazard at that time. Only relevant if \code{model_type}
#'   is "survival"; must be NULL otherwise.
#' @param formula an object of class "\code{\link[stats]{formula}}" (or a
#'   character string that can be coerced to that class). This specifies the
#'   functional form description of the existing prediction model. The details
#'   of the model specification are given under "Details".
#' @param newdata  data.frame which will be used to make predictions on using
#'   the existing prediction model. Variable names must match those in
#'   \code{existingcoefs} and \code{formula}, after applying any pre-processing
#'   steps (using \code{pre_processing}) where needed. See "Details".
#' @param pre_processing a list where each element is a function that describes
#'   transformations to apply to columns of \code{newdata}. See "Details".
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{newdata} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as default \code{NULL} otherwise. Leave
#'   as \code{NULL} if \code{newdata} does not contain any outcomes.
#' @param survival_time Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as default \code{NULL} otherwise.
#'   Leave as \code{NULL} if \code{newdata} does not contain any survival
#'   outcomes.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as default \code{NULL} otherwise. Leave as \code{NULL} if
#'   \code{newdata} does not contain any survival outcomes.
#'
#' @details This function will structure relevant information about an existing
#'   prediction model, and a new dataset (on which one wishes to make
#'   predictions using the existing model), into a standardised format, such
#'   that it can be used with other functions in the package.
#'
#'   \code{newdata} should be a data.frame, where each row is an observation
#'   (e.g. patient) on which predictions will be made. Each variable/column of
#'   \code{newdata} should include (as a minimum) all of the predictor variables
#'   that are included in the existing prediction model. The names of these
#'   variables/columns should match those provided in \code{formula} and
#'   \code{existingcoefs}.
#'
#'   Variable transformations/ pre-processing-steps to apply to \code{newdata}
#'   (e.g. interaction terms, non-linear terms or splines) can be created within
#'   \code{\link{pm_input_info}} by specifying \code{pre_processing}.
#'   \code{pre_processing} should be a list of functions that apply the desired
#'   transformations/ pre-processing step  - the functions should have one input
#'   (\code{newdata}) and each return a single transformed variable. Usually,
#'   each element of \code{pre_processing} will be named with the corresponding
#'   function applying a transformation to a single variable in \code{newdata}
#'   and returning a vector of length equal to the number of rows of
#'   \code{newdata}; here, the name of the list element becomes the
#'   new/transformed variable name in \code{newdata}. Alternatively,
#'   \code{pre_processing} can take a function that applies multiple
#'   transformations/ pre-processing-steps to \code{newdata} and returns a
#'   data.frame or list giving the set of transformed variable outputs. For
#'   example, if the \code{newdata} contains factor/categorical variables, then
#'   "dummy" variables of these factor/categorical variables can be created by
#'   specifying the function \code{\link{dummyvars}} as a list element. See
#'   "Examples" below.
#'
#'   \code{formula} describes the functional form of the existing prediction
#'   model. For example, if the existing prediction model included "age" and
#'   "BMI" as predictor variables, then \code{formula} would be entered as
#'   \code{~ age + BMI}. Each variable name in \code{formula} must have a
#'   corresponding name in \code{newdata} (after applying any
#'   \code{pre_processing} steps). Additionally, each predictor variable
#'   specified in \code{formula} must have a corresponding named element in
#'   \code{existingcoefs}. Only the right hand side of the formula is needed.
#'
#'   \code{existingcoefs} should be provided as a named numeric vector, where
#'   each name directly matches those in \code{formula} and in \code{newdata}
#'   (after applying any \code{pre_processing} steps). The values are the
#'   corresponding coefficient estimates taken \strong{exactly} as published by
#'   the existing model. These coefficients should not be re-evaluated in the
#'   current data, if the \code{newdata} is being used for validation of the
#'   existing prediction model. In the case of model_type = "logistic", the
#'   intercept of the existing prediction model must be named as "(Intercept)".
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcomes in \code{newdata} if this is relevant. For
#'   example, if validating the existing model, then these specify the columns
#'   in \code{newdata} that will be used for assessing predictive performance of
#'   the predictions in the validation dataset. If the \code{newdata} does not
#'   contain outcomes (e.g. if simply applying/implementing the existing model
#'   in \code{newdata}), then leave these to the default of \code{NULL}. If
#'   \code{model_type} is "logistic" then both \code{survival_time} and
#'   \code{event_indicator} should be set to NULL; likewise, if
#'   \code{model_type} is "survival", then \code{binary_outcome} should be set
#'   to NULL.
#'
#'
#' @return \code{\link{pm_input_info}} returns an object of class "pminfo", with
#'   child classes per model_type. This is a standardised format, such that it
#'   can be used with other functions in the package. An object of class
#'   "pminfo" is a list containing at least the following components:
#'   \itemize{\item{model_type = this is the type of analytical model that the
#'   existing prediction model is based upon: either "logistic" or "survival"}
#'   \item{coefs = this is the list of (previously estimated) coefficients for
#'   each predictor included in the existing prediction model} \item{coef_names
#'   = gives the names of each predictor variable, with corresponding
#'   coefficient specified in coefs} \item{PredictionData = this is the design
#'   matrix formed by mapping the specified \code{pre_processing} steps and
#'   functional form of the existing prediction model specified in
#'   \code{formula} onto \code{newdata}; any predictions will be upon such
#'   data.}} More items might be required depending on the inputs to the
#'   function.
#'
#' @export
#'
#' @examples
#' #Example 1 - basic usage
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("(Intercept)" = -2, "X" = 0.5),
#'               baselinehazard = NULL,
#'               formula = ~X,
#'               newdata = data.frame("X" = rnorm(100)),
#'               pre_processing = NULL)
#'
#'
#' #Example 2 - here, the intercept in existingcoefs is incorrectly named; will
#' # return error
#' \dontrun{
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("Intercept" = -2, "X" = 0.5),
#'               formula = ~X,
#'               newdata = data.frame("X" = rnorm(100)),
#'               pre_processing = NULL)
#'          }
#'
#'
#' #Example 3 - here, the existing prediction model is specified as having a
#' # functional form of X+Z (in 'formula') with corresponding existingcoefs, but
#' # Z does not exist in newdata (even after any pre_processing); will
#' # return an error
#' \dontrun{
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("(Intercept)" = -2, "X" = 0.5, "Z" = 0.9),
#'               formula = ~X + Z,
#'               newdata = data.frame("X" = rnorm(100)),
#'               pre_processing = NULL)
#'          }
#'
#'
#' #Example 4 - shows how to handle categorical variables - can either call
#' #pmupdate::dummyvars() within the pre_processing input, or incorporate
#' #user-defined functions to handle the categorical variables
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("X" = 0.5,
#'                                 "X_Squared" = 0.005,
#'                                 "(Intercept)" = -2,
#'                                 "Colourgreen" = 0.5,
#'                                 "Colourred" = 0.95,
#'                                 "Colourwhite" = 2,
#'                                 "SexMale" = 0.6),
#'               formula = ~X + X_Squared + Colourgreen + Colourred + Colourwhite + SexMale,
#'               newdata = data.frame("X" = rnorm(500),
#'                                    "Colour" = factor(sample(c("red",
#'                                                               "azure",
#'                                                               "green",
#'                                                               "white"),
#'                                                               500,
#'                                                               replace = TRUE)),
#'                                    "Sex" = factor(sample(c("Male",
#'                                                            "Female"),
#'                                                            500,
#'                                                            replace = TRUE))),
#'               pre_processing = list("X_Squared" = function(df){df$X^2},
#'                                     function(df) {dummyvars(df)}))
#' ###....alternatively:
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("X" = 0.5,
#'                                 "X_Squared" = 0.005,
#'                                 "(Intercept)" = -2,
#'                                 "Colour_green" = 0.5,
#'                                 "Colour_red" = 0.95,
#'                                 "Colour_white" = 2),
#'               formula = ~X + X_Squared + Colour_green + Colour_red + Colour_white,
#'               newdata = data.frame("X" = rnorm(500),
#'                                    "Colour" = factor(sample(c("red",
#'                                                               "azure",
#'                                                               "green",
#'                                                               "white"),
#'                                                               500,
#'                                                               replace = TRUE)),
#'                                    "Sex" = factor(sample(c("Male",
#'                                                            "Female"),
#'                                                            500,
#'                                                            replace = TRUE))),
#'               pre_processing = list("X_Squared" = function(df){df$X^2},
#'                                     "Colour_green" = function(df) {
#'                                     ifelse(df$Colour == "Green", 1, 0)
#'                                     },
#'                                     "Colour_white" = function(df) {
#'                                     ifelse(df$Colour == "White", 1, 0)
#'                                     },
#'                                     "Colour_red" = function(df) {
#'                                     ifelse(df$Colour == "Red", 1, 0)
#'                                     }
#'                                     ))
#'
#'
#' #Example 5 - showing use of pre_processing - the following are all valid ways
#' #            of specifying elements of pre_processing
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("(Intercept)" = -5,
#'                                 "Age" = 0.05,
#'                                 "Age_squared" = 0.0005,
#'                                 "BMI_logged" = 0.006),
#'               formula = ~Age + Age_squared + BMI_logged,
#'               newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                    "BMI" = rnorm(100, 25, 0.5)),
#'               pre_processing = list("Age_squared" = function(df) df$Age^2,
#'                                     "BMI_logged" = function(df) log(df$BMI)))
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("(Intercept)" = -5,
#'                                 "Age" = 0.05,
#'                                 "Age_squared" = 0.0005,
#'                                 "BMI_logged" = 0.006),
#'               formula = ~Age + Age_squared + BMI_logged,
#'               newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                    "BMI" = rnorm(100, 25, 0.5)),
#'               pre_processing = list(function(df) {
#'                 Age_squared <- df$Age^2
#'                 BMI_logged <- log(df$BMI)
#'                 return(list("Age_squared" = Age_squared,
#'                             "BMI_logged" = BMI_logged))
#'               }))
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = c("(Intercept)" = -5,
#'                                 "Age" = 0.05,
#'                                 "Age_squared" = 0.0005,
#'                                 "BMI_logged" = 0.006),
#'               formula = ~Age + Age_squared + BMI_logged,
#'               newdata = data.frame("Age" = rnorm(100, 50, 0.5),
#'                                    "BMI" = rnorm(100, 25, 0.5)),
#'               pre_processing = list(function(df) {
#'                 df$Age_squared <- df$Age^2
#'                 df$BMI_logged <- log(df$BMI)
#'                 return(df)
#'               }))
#'
#'
#' #Example 6 - showing specification of outcome columns in newdata; uses built in dataset
#' existing_coefs <- as.numeric(unlist(strsplit(
#' SYNPM$Existing_models$Coeffs[1], split = "\\|")))
#' names(existing_coefs) <- gsub(' ', '', unlist(strsplit(
#' SYNPM$Existing_models$Variables[1], split = "\\|")))
#' pm_input_info(model_type = "logistic",
#'               existingcoefs = existing_coefs,
#'               formula = formula(SYNPM$Existing_models$Formula[1]),
#'               newdata = SYNPM$ValidationData,
#'               pre_processing = list(function(df) {dummyvars(df)}),
#'               binary_outcome = "Y")
#'
#'
#' #Example 7 - survival model example
#' pm_input_info(model_type = "survival",
#'               existingcoefs = c("SEX" = 0.53,
#'                                 "AGE" = -0.05,
#'                                 "SYSTBP" = -0.0055,
#'                                 "BMIO" = 0.0325,
#'                                 "CARDIAC" = -0.126,
#'                                 "DIABETES" = -0.461),
#'               formula = ~ SEX + AGE + SYSTBP + BMIO + CARDIAC + DIABETES,
#'               newdata = SMART,
#'               baselinehazard = data.frame("t" = 1:5,
#'                                           "h" = c(0.12, 0.20, 0.26, 0.33, 0.38)),
#'               survival_time = "TEVENT",
#'               event_indicator = "EVENT")

pm_input_info <- function(model_type = c("logistic", "survival"),
                          existingcoefs,
                          baselinehazard = NULL,
                          formula,
                          newdata,
                          pre_processing = NULL,
                          binary_outcome = NULL,
                          survival_time = NULL,
                          event_indicator = NULL) {

  ########################## INPUT CHECKING #############################
  model_type <- match.arg(model_type)

  #Check that 'existingcoefs' is supplied as a named numeric vector
  if (!is.vector(existingcoefs) |
      !is.numeric(existingcoefs) |
      is.null(names(existingcoefs))) {
    stop("'existingcoefs' should be a named numeric vector", call. = FALSE)
  }

  #Convert formula to formula class if needed
  formula <- stats::as.formula(formula)

  #Check that supplied 'newdata' is a data.frame
  if (!is.data.frame(newdata)) {
    stop("'newdata' should be a data.frame", call. = FALSE)
  }


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


  ############### EXTRACT INFORMATION BY MODEL TYPE ######################
  if (model_type == "logistic") {

    #Check any specification of outcome variable names
    if (!is.null(survival_time)) {
      stop("'survival_time' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }else if (!is.null(event_indicator)) {
      stop("'event_indicator' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }
    if (!is.null(baselinehazard)) {
      stop("'baselinehazard' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }

    # Remove rows of newdata that have missing data in relevant columns:
    if(any(stats::complete.cases(newdata[,c(binary_outcome,
                                            all.vars(formula)), drop = FALSE]) == FALSE)) {
      newdata <- newdata[stats::complete.cases(newdata[,c(binary_outcome,
                                                          all.vars(formula)), drop = FALSE])
                         , , drop = FALSE]
      warning(paste("Some rows of newdata have been removed due to missing data in either the outcome variables and/or variables specified in 'formula'.  \n",
                    "Complete case may not be appropriate - consider alternative methods of handling missing data in newdata prior to calling pm_input_info()",
                    sep = ''),
              call. = FALSE)
    }

    info_vals <- pm_input_info_logistic(model_type = model_type,
                                        existingcoefs = existingcoefs,
                                        formula = formula,
                                        newdata = newdata,
                                        binary_outcome = binary_outcome)

  } else if (model_type == "survival") {

    #Check any specification of outcome variable names
    if (!is.null(binary_outcome)) {
      stop("'binary_outcome' should be set to NULL if model_type=survival",
           call. = FALSE)
    }

    #check baseline hazard specification
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

    # Remove rows of newdata that have missing data in relevant columns:
    if(any(stats::complete.cases(newdata[,c(survival_time,
                                            event_indicator,
                                            all.vars(formula)), drop = FALSE]) == FALSE)) {
      newdata <- newdata[stats::complete.cases(newdata[,c(survival_time,
                                                          event_indicator,
                                                          all.vars(formula)), drop = FALSE])
                         , , drop = FALSE]
      warning(paste("Some rows of newdata have been removed due to missing data in either the outcome variables and/or variables specified in 'formula'.  \n",
                    "Complete case may not be appropriate - consider alternative methods of handling missing data in newdata prior to calling pm_input_info()",
                    sep = ''),
              call. = FALSE)
    }

    info_vals <- pm_input_info_survival(model_type = model_type,
                                        existingcoefs = existingcoefs,
                                        baselinehazard = baselinehazard,
                                        formula = formula,
                                        newdata = newdata,
                                        survival_time = survival_time,
                                        event_indicator = event_indicator)
  }

  info_vals
}


#' @export
print.pminfo <- function(x, ...) {
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




# Internals for pm_input_info() -------------------------------------------

pm_input_info_logistic <- function(model_type,
                                   existingcoefs,
                                   formula,
                                   newdata,
                                   binary_outcome) {

  ##Extract the Outcomes from newdata if specified by user:
  if (is.null(binary_outcome)) {
    Outcomes <- NULL
  }else {
    if(binary_outcome %in% names(newdata)) {
      Outcomes <- newdata[,binary_outcome]
    }else{
      stop("'binary_outcome' not found in 'newdata'",
           call. = FALSE)
    }
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
  class(info_vals) <- c("pminfo_logistic", "pminfo")
  info_vals
}



pm_input_info_survival <- function(model_type,
                                   existingcoefs,
                                   baselinehazard,
                                   formula,
                                   newdata,
                                   survival_time,
                                   event_indicator) {

  ##Extract the Outcomes from newdata if specified by user:
  if (!is.null(survival_time) & !is.null(event_indicator)) {
    if(survival_time %in% names(newdata) &
       event_indicator %in% names(newdata)) {
      Outcomes <- survival::Surv(newdata[,survival_time],
                                 newdata[,event_indicator])
    }else{
      stop("'survival_time' and/or 'event_indicator' not found in 'newdata'",
           call. = FALSE)
    }
  }else if (is.null(survival_time) & is.null(event_indicator)) {
    Outcomes <- NULL
  }else {
    stop("'survival_time' and 'event_indicator' should either both be NULL or both have values supplied for model_type == 'survival'",
         call. = FALSE)
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
  class(info_vals) <- c("pminfo_survival", "pminfo")
  info_vals
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






