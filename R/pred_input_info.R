#' Input information about an existing prediction model
#'
#' Input coefficient information about one or multiple existing prediction
#' model(s), for use in other functions in the package.
#'
#' @param model_type specifies the type of model that the existing prediction
#'   model is based on; possible options are: \itemize{ \item {\code{"logistic"}
#'   indicates that the existing model was based on a logistic regression model
#'   (default)} \item {\code{"survival"} indicates that the existing model was
#'   based on a survival regression model} } If multiple models are being
#'   entered, then all models need to be of the same type - otherwise call
#'   function multiple times for each type of model.
#' @param model_info a data.frame that contains the coefficients of the existing
#'   prediction model(s). Each column should be a predictor variable (with the
#'   name of the column being the name of the predictor variable), with the
#'   values being the coefficients, taken exactly as published from the existing
#'   prediction model(s). Multiple existing prediction models should be
#'   specified by entering multiple rows. If a predictor variable is not present
#'   in a given model then enter that cell of the data.frame as NA. See
#'   examples.
#' @param cum_hazard A data.frame with two columns: (1) time, and (2) estimated
#'   cumulative baseline hazard at that time. The first column (time) should be
#'   named 'time' and the second (cumulative baseline hazard) should be named
#'   'hazard'. Only relevant if \code{model_type} is "survival"; leave as NULL
#'   otherwise. If multiple existing models entered, and model_type = survival,
#'   then \code{cum_hazard} should be supplied as list of length equal to number
#'   of models.
#'
#' @details This function will structure the relevant information about one or
#'   more existing prediction model(s) into a standardised format, such that it
#'   can be used within other functions in the package.
#'
#'   First, the existing prediction model(s) will have a functional form (i.e.
#'   the linear predictor of the model); this will be taken as being a linear
#'   combination of the variables specified by the columns of \code{model_info}.
#'
#'   Second, each of the predictor variables of the existing prediction model(s)
#'   will have a published coefficient (e.g. log-odds-ratio or
#'   log-hazard-ratio), which should each be given as the values of
#'   \code{model_info}. If entering information about multiple existing
#'   prediction models, then \code{model_info} will contain multiple rows (one
#'   per existing model). Here, if a given model does not contain a predictor
#'   variable that is included in another model, then set as NA; see examples of
#'   this below.
#'
#'   In the case of \code{model_type} = "logistic", then \code{model_info} must
#'   contain a column named as "Intercept", which gives the intercept
#'   coefficient of each of the existing logistic regression models (taken
#'   exactly as previously published); this should be the first column of
#'   \code{model_info}.
#'
#'   If \code{model_type} = "survival", then the baseline cumulative hazard of
#'   the model(s) can be specified in \code{cum_hazard}. If the baseline
#'   cumulative hazard of the existing survival model is not available, then
#'   leave as NULL; this will limit any validation metrics that can be
#'   calculated.
#'
#'   Note, the column names of \code{model_info} should match columns in any new
#'   data that the existing model(s) will be applied to (i.e. any new data that
#'   will be provided to other functions within the package should have
#'   corresponding predictor variables entered through \code{model_info}). See
#'   \code{\link{pred_predict}}, \code{\link{pred_validate}},
#'   \code{\link{pred_update}} and \code{\link{pred_stacked_regression}} for
#'   more information.
#'
#' @return \code{\link{pred_input_info}} returns an object of class
#'   "\code{predinfo}", with child classes per \code{model_type}. This is a
#'   standardised format, such that it can be used with other functions in the
#'   package. An object of class "\code{predinfo}" is a list containing the
#'   following components:
#'   \itemize{ \item{M = the number of existing models that information has been
#'   entered about} \item{model_type = this is the type of model that
#'   the existing prediction model is based upon ("logistic" or "survival")}
#'   \item{coefs = this is the set of (previously estimated) coefficients for
#'   each predictor variable} \item{coef_names = gives the names of each
#'   predictor variable} \item{formula = this is the functional form of the
#'   model's linear predictor} \item{cum_hazard = if supplied, this is the
#'   cumulative baseline hazard of the existing model(s)}}
#'
#' @examples
#' #Example 1 - logistic regression existing model
#' # create a data.frame of the model coefficients, with columns being variables
#' coefs_table <- data.frame("Intercept" = -3.4,
#'                           "SexM" = 0.306,
#'                           "Smoking_Status" = 0.628,
#'                           "Diabetes" = 0.499,
#'                           "CKD" = 0.538)
#' #pass this into pred_input_info()
#' Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
#'                                            model_info = coefs_table)
#' summary(Existing_Logistic_Model)
#'
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package.
#' pred_input_info(model_type = "survival",
#'                 model_info = SYNPM$Existing_TTE_models[2,],
#'                 cum_hazard = SYNPM$TTE_mod2_baseline)
#'
#' #Example 3 - Input information about multiple models
#' summary(pred_input_info(model_type = "logistic",
#'                         model_info = SYNPM$Existing_logistic_models))
#'
#' @export
pred_input_info <- function(model_type = c("logistic", "survival"),
                            model_info,
                            cum_hazard = NULL) {

  ########################## INPUT CHECKING #############################
  model_type <- match.arg(model_type)

  pred_input_info_input_checks(model_type = model_type,
                               model_info = model_info,
                               cum_hazard = cum_hazard)

  M <- nrow(model_info) #number of existing models


  ############### EXTRACT INFORMATION BY MODEL TYPE ######################
  if (model_type == "logistic") {
    if (M > 1){
      existingcoefs <- vector(mode = "list", length = M)
      form <- vector(mode = "list", length = M)
      vars <- vector(mode = "list", length = M)
      for (m in 1:M) {
        vars[[m]] <- names(model_info)[which(!is.na(model_info[m,]))]
        if(all(vars[[m]]=="Intercept")){
          form[[m]] <- stats::as.formula("~1")
        } else{
          form[[m]] <- stats::as.formula(paste("~",
                                               paste(vars[[m]][-which(vars[[m]]=="Intercept")],
                                                     collapse = "+"),
                                               sep = ''))
        }
        existingcoefs[[m]] <- model_info[m, vars[[m]], drop = FALSE]
      }

    } else {
      vars <- names(model_info)[which(!is.na(model_info[1,]))]
      if(all(vars=="Intercept")){
        form <- stats::as.formula("~1")
      } else{
        form <- stats::as.formula(paste("~",
                                        paste(vars[-which(vars=="Intercept")],
                                              collapse = "+"),
                                        sep = ''))
      }
      existingcoefs <- model_info[1, vars, drop = FALSE]

    }
    info_vals <- list(M = M,
                      model_type = model_type,
                      coefs = existingcoefs,
                      coef_names = vars,
                      formula = form,
                      model_info = model_info)
    #Set the S3 class
    class(info_vals) <- c("predinfo_logistic", "predinfo")

  } else if (model_type == "survival") {
    if (M > 1){
      existingcoefs <- vector(mode = "list", length = M)
      form <- vector(mode = "list", length = M)
      vars <- vector(mode = "list", length = M)
      for (m in 1:M) {
        vars[[m]] <- names(model_info)[which(!is.na(model_info[m,]))]
        form[[m]] <- stats::as.formula(paste("~",
                                             paste(vars[[m]], collapse = "+"),
                                             sep = ''))
        existingcoefs[[m]] <- model_info[m, vars[[m]], drop = FALSE]
      }

    } else {
      vars <- names(model_info)[which(!is.na(model_info[1,]))]
      form <- stats::as.formula(paste("~",
                                      paste(vars, collapse = "+"),
                                      sep = ''))
      existingcoefs <- model_info[1, vars, drop = FALSE]

    }
    info_vals <- list(M = M,
                      model_type = model_type,
                      coefs = existingcoefs,
                      coef_names = vars,
                      formula = form,
                      cum_hazard = cum_hazard,
                      model_info = model_info)
    #Set the S3 class
    class(info_vals) <- c("predinfo_survival", "predinfo")
  }

  info_vals
}


#' @export
summary.predinfo <- function(object, ...) {

  cat(paste("Information about", object$M, "existing model(s)",
            paste("of type '", object$model_type, "' \n", sep = ""), sep = " "))

  cat("\nModel Coefficients \n",
      "================================= \n", sep = "")
  print(object$coefs)

  cat("\nModel Functional Form \n",
      "================================= \n", sep = "")
  if(object$M == 1){
    cat(as.character(object$formula)[2])
  } else{
    for(m in 1:object$M){
      cat(paste("Model ", m, ": ", as.character(object$formula[[m]])[2], "\n", sep  = ""))
    }
  }
}


#' @export
print.predinfo <- function(x, ...){

  if(x$M == 1){
    cat("\n Formula: \n")
    print(x$formula)
    cat("\n Coefficients: \n")
    print(x$coefs)
  } else {
    for(m in 1:x$M){
      cat(paste("\n Model", m, "Formula: \n", sep = " "))
      print(x$formula[[m]])
      cat(paste("\n Model", m, "Coefficients: \n", sep = " "))
      print(x$coefs[[m]])
    }
  }
}
