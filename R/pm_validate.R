#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object used to select a method - should be of class "pminfo" or
#'   "data.frame". If a data.frame this should include the linear predictor of
#'   the existing prediction model and the observed outcomes to validate this
#'   model against.
#' @param ... further arguments passed to other methods
#'
#' @return TO ADD
#'
#' @export
#'
#' @examples #TO ADD
#'
#' @seealso \code{\link{pm_input_info}}
pm_validate <- function(x, ...) {
  UseMethod("pm_validate")
}


#' @export
pm_validate.default <- function(x, ...) {
  stop("'x' is not of class 'pminfo' or 'data.frame'",
       call. = FALSE)
}


#' @rdname pm_validate
#' @param model_type specifies the type of model that the existing prediction
#'   model is based on; possible options are: \itemize{ \item {\code{"logistic"}
#'   indicates that the existing model was based on a logistic regression model
#'   (default)} \item {\code{"survival"} indicates that the existing model was
#'   based on a survival regression model} }
#' @param LinearPredictor character variable specifying the column of \code{x}
#'   that stores the linear predictor of the model to be validated
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{x} that represents the observed binary outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as default \code{NULL} otherwise.
#' @param survival_time Character variable giving the name of the column in
#'   \code{x} that represents the observed survival times. Only relevant for
#'   \code{model_type}="survival"; leave as default \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{x} that represents the observed survival indicator (1 for event, 0
#'   for censoring). Only relevant for \code{model_type}="survival"; leave as
#'   default \code{NULL} otherwise.
#'
#' @export
pm_validate.data.frame <- function(x,
                                   model_type = c("logistic", "survival"),
                                   LinearPredictor = NULL,
                                   binary_outcome = NULL,
                                   survival_time = NULL,
                                   event_indicator = NULL,
                                   ...) {
  model_type <- match.arg(model_type)

  if (model_type == "logistic") {
    ### Test inputs
    if(is.null(LinearPredictor) | is.null(binary_outcome)) {
      stop("When x is a data.frame, 'LinearPredictor' and 'binary_outcome' must be supplied for model_type = logistic",
           call. = FALSE)
    }
    if(LinearPredictor %in% names(x) == FALSE) {
      stop("Specified 'LinearPredictor' is not found in x")
    }else if(binary_outcome %in% names(x) == FALSE) {
      stop("Specified 'binary_outcome' is not found in x")
    }

    ### VALIDATION OF THE EXISTING MODEL
    performance <- logistic_performance(ObservedOutcome = x[,binary_outcome],
                                        LinearPredictor = x[,LinearPredictor])
    class(performance) <- c("pmperformance_logistic", "pmperformance")
    performance
  }

  else if (model_type == "survival") {
    stop("Models of type='survival' are not currently supported")
  }

}


#' @export
pm_validate.pminfo_logistic <- function(x, ...){

  #Check outcomes were inputted into pminfo object
  if (is.null(x$Outcomes)) {
    stop("Observed outcomes must be supplied in newdata to validate the existing model. Recall pm_input_info() with outcome specified.",
         call. = FALSE)
  }

  ### USE EXISTING INFO ABOUT PREDICTION MODEL TO MAKE PREDICTIONS IN NEWDATA
  predictions <- pmupdate::pm_predict(x)

  ### VALIDATION OF THE EXISTING MODEL
  performance <- logistic_performance(ObservedOutcome = predictions$Outcomes,
                                      LinearPredictor = predictions$LinearPredictor)

  class(performance) <- c("pmperformance_logistic", "pmperformance")
  performance
}


#' @export
pm_validate.pminfo_survival <- function(x, ...){
  stop("Models of type='survival' are not currently supported")
}



#' @export
print.pmperformance_logistic <- function(x, ...) {
  results <- matrix(NA, ncol = 4, nrow = 5)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Calibration-in-the-large",
                         "Calibration Slope",
                         "AUC",
                         "Cox-Snell R-squared",
                         "Brier Score")
  results[1,] <- c(round(x$CITL, 4),
                   round(x$CITL_SE, 4),
                   round(x$CITL_Lower, 4),
                   round(x$CITL_Upper, 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round(x$CalSlope_Lower, 4),
                   round(x$CalSlope_Upper, 4))
  results[3,] <- c(round(x$AUC, 4),
                   round(x$AUC_SE, 4),
                   round(x$AUC_Lower, 4),
                   round(x$AUC_Upper, 4))
  results[4,] <- c(round(x$R2, 4),
                   NA,
                   NA,
                   NA)
  results[5,] <- c(round(x$BrierScore, 4),
                   NA,
                   NA,
                   NA)

  print(results)
}
