#' Apply an existing prediction model to a dataset, given published coefficients
#'
#' @param formula an object of class "\code{\link[stats]{formula}}" (or a
#'   character string that can be coerced to that class). This is a symbolic
#'   description of the existing prediction model. The details of the model
#'   specification are given under "Details".
#' @param type specifies the type of model that the existing prediction model is
#'   based on; possible options are:
#'   \itemize{
#'        \item {\code{"logistic"} indicates that the existing model was based
#'        on a logistic regression model}
#'        \item {\code{"survival"} indicates that the existing model was based
#'        on a survival regression model}
#'    }
#'    \strong{Currently, only \code{"logistic"} is supported.}
#' @param existingcoefs a named vector of coefficients, taken exactly as
#'   published from the existing prediction model. Names much match those in
#'   \code{newdata}. See "Details".
#' @param newdata a data frame in which to look for variables with which to
#'   apply the existing prediction model to (i.e. predict risk). Variable names
#'   must match those in \code{existingcoefs}. \strong{Currently assumes all
#'   data pre-processing has been done before calling function}.
#'
#' @details The \code{formula} argument must be specified in the form
#'    \code{outcome ~ predictors}, where \code{outcome} is the name of the
#'    column in \code{newdata} that stores the binary outcome values (0 or 1),
#'    and \code{predictors} is the functional form of the existing prediction
#'    model, where each predictor is separated with a \code{+} symbol.
#'
#' @return Linear predictor and predicted risks for each observation in
#'    \code{newdata}, based on the specified information about the existing
#'    prediction model
#' @export
apply_pm <- function(formula,
                     type = c("logistic", "survival"),
                     existingcoefs,
                     newdata) {

  type <- match.arg(type)
  if (type == "survival") {
    stop("Models of type='survival' are not currently supported")
  }
  print(type)

  #Convert formula argument to object of class formula if needed
  if (inherits(formula, "formula") == FALSE) {
    formula <- stats::as.formula(formula)
  }

  #define a design matrix given the provided functional form of the existing
  #prediction model
  DM <- stats::model.matrix(formula, newdata)

  #Test if the names given in DM match
  if (all(names(existingcoefs) %in% colnames(DM)) == FALSE) {
    print(paste("Variable ",
                names(existingcoefs)[which(names(existingcoefs) %in%
                                             colnames(DM) == FALSE)],
                " in existingcoefs is not found in functional form of newdata"))
    stop("Names in existingcoefs do not match those created by the
         functional form of formula.")
  }
  #Ensure that order of existingcoefs matches the design matrix
  existingcoefs <- existingcoefs[colnames(DM)]
  existingcoefs
}
