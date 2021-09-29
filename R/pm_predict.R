#' Use an existing prediction model to estimate predicted risks in new data,
#' given published coefficients
#'
#' @param x an object of class "\code{pminfo}" produced by
#'   \code{\link{pm_input_info}}. This is a 'blueprint' description of the
#'   existing prediction model.
#' @param ... Not used.
#'
#' @details This function takes the relevant information about the existing
#'   prediction model (as supplied by called \code{\link{pm_input_info}}), and
#'   returns the predicted risks for each individual/observation in
#'   \code{newdata}. See \code{\link{pm_input_info}}) for more details.
#'
#' @return Linear predictor and predicted risks for each observation in
#'   \code{newdata}, based on the specified information about the existing
#'   prediction model
#' @export
pm_predict <- function(x, ...) {
  UseMethod("pm_predict")
}


#' @export
pm_predict.default <- function(x, ...) {
  stop("'x' is not of class 'pminfo'; please see pm_input_info()",
       call. = FALSE)
}


#' @export
pm_predict.pminfo <- function(x, ...){
  if (x$model_type == "survival") {
    stop("Models of type='survival' are not currently supported")

  } else {
    predictions <- pm_predict_logistic(existingcoefs = x$coefs,
                                       DM = x$PredictionData)

    #return results
    x$LinearPredictor <- predictions$LP
    x$PredictedRisk <- predictions$PR
    x
  }
}
