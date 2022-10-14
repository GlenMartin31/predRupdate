#' Input information about an existing prediction model
#'
#' Input relevant information about one or multiple existing prediction model
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
#' @param baselinehazard A data.frame with two columns: (1) time, and (2)
#'   estimated baseline hazard at that time. Only relevant if \code{model_type}
#'   is "survival"; leave as NULL otherwise. If multiple existing models
#'   entered, and model_type = survival, then \code{baselinehazard} should be
#'   supplied as list of length equal to number of models.
#'
#' @details This function will structure the relevant information (detailed
#'   below) about one or more existing prediction model into a standardised
#'   format, such that it can be used with other functions in the package.
#'
#'   First, the existing prediction model(s) will have a functional form (i.e.
#'   the linear predictor of the model); unless otherwise stated by
#'   \code{formula}, this will be taken as being a linear combination of the
#'   variables specified by the columns of \code{model_info}.
#'
#'   Second, each of the predictor variables of the existing prediction model(s)
#'   will have a published coefficient (e.g. log-odds-ratio or
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
#' @return \code{\link{pred_input_info}} returns an object of class "predinfo",
#'   with child classes per model_type. This is a standardised format, such that
#'   it can be used with other functions in the package. An object of class
#'   "predinfo" is a list containing at least the following components:
#'   \itemize{ \item{model_type = this is the type of analytical model that the
#'   existing prediction model is based upon ("logistic" or "survival")}
#'   \item{coefs = this is the list of (previously estimated) coefficients for
#'   each predictor variable} \item{coef_names = gives the names of each
#'   predictor variable} \item{PredictionData = this is the data formed by any
#'   subsequent predictions/model updating/ model aggregation will be based on
#'   this data.} \item{Outcomes = vector of outcomes/endpoints (if specified in
#'   the input).} }
#'
#' @examples
#' #Example 1 - logistic regression existing model; uses
#' #            an example dataset within the package
#' pred_input_info(model_type = "logistic",
#'                 model_info = SYNPM$Existing_models[1,])
#'
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package.
#' pred_input_info(model_type = "survival",
#'                 model_info = data.frame("SEX_M" = 0.53,
#'                                         "AGE" = -0.05,
#'                                         "SYSTBP" = -0.0055,
#'                                         "BMIO" = 0.0325,
#'                                         "CARDIAC" = -0.126,
#'                                         "DIABETES" = -0.461),
#'                  baselinehazard = data.frame("t" = 1:5,
#'                                              "h" = c(0.12, 0.20, 0.26, 0.33, 0.38)))
#'
#' #Example 3 - Input information about multiple models
#' pred_input_info(model_type = "logistic",
#'                 model_info = SYNPM$Existing_models)
#'
#' @export
pred_input_info <- function(model_type = c("logistic", "survival"),
                            model_info,
                            baselinehazard = NULL) {

  ########################## INPUT CHECKING #############################
  model_type <- match.arg(model_type)

  pred_input_info_input_checks(model_type = model_type,
                               model_info = model_info,
                               baselinehazard = baselinehazard)

  M <- nrow(model_info) #number of existing models


  ############### EXTRACT INFORMATION BY MODEL TYPE ######################
  if (model_type == "logistic") {
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
                        formula = form)
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
                        formula = form)
    }
    #Set the S3 class
    class(info_vals) <- c("predinfo_logistic", "predinfo")

  } else if (model_type == "survival") {
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
                        baselinehazard = baselinehazard)
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
                        baselinehazard = baselinehazard)
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

  cat(paste("Information about", M, "existing model(s)",
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
}



# Internal functions for pred_input_info() ---------------------------------------

pred_input_info_input_checks <- function(model_type,
                                         model_info,
                                         baselinehazard) {

  #check that model_info is provided as a data.frame
  if (inherits(model_info, "data.frame") == FALSE) {
    stop("'model_info' should be a data.frame", call. = FALSE)
  }
  #Check that each 'cell' of model_info data.frame is a number
  if (all(sapply(model_info, is.numeric)) == FALSE) {
    stop("All columns of 'model_info' should be numeric", call. = FALSE)
  }

  if (model_type == "logistic") {
    #check that 'model_info' contains an intercept column for logistic models
    if ("Intercept" %in% names(model_info) == FALSE) {
      stop("When model_type=logistic, then 'model_info' should contain a column named 'Intercept'",
           call. = FALSE)
    }

    if (!is.null(baselinehazard)) {
      stop("'baselinehazard' should be set to NULL if model_type=logistic",
           call. = FALSE)
    }

  } else if (model_type == "survival") {
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
