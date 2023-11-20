#' SYNthetic Prediction Models (SYNPM) and Validation dataset
#'
#' A list containing: (1) information on some (synthetic) existing prediction
#' models (representing those available/published, which we want to validate in
#' another independent dataset); and (2) a synthetic dataset that we wish to
#' validate/update the models on.
#'
#' @format A list with six elements.
#'   \enumerate{
#'   \item{The first element is a
#'   data frame with the information about three existing binary (logistic
#'   regression) models for a binary outcome at one year}
#'   \item{The second element is a data frame with the information about three
#'   existing time-to-event (Cox) models for the time-to-event outcome}
#'   \item{The third, fourth and fifth elements are the cumulative baseline
#'   hazard information for the three time-to-event model}
#'   \item{The sixth element is the (synthetic) validation dataset on which we want to
#'   validate the existing models. The dataset has 20000 rows and 8 variables:
#'   \describe{ \item{Age}{The age of the individual at baseline} \item{SexM}{The
#'   sex of the individual (1 = male; 0 = female)}
#'   \item{Smoking_Status}{Indicates whether the individual was or is a smoker
#'   (1=previous/ current smoker, 0=non-smoker)} \item{Diabetes}{Indicates
#'   whether the individual has diabetes (1=diabetic, 0=not diabetic)}
#'   \item{Creatinine}{The Creatinine value for the individual (mg/dL)}
#'   \item{ETime}{The time from baseline until either the event or censoring}
#'   \item{Status}{Indicator of whether the patient experienced the event or was censored at ETime}
#'   \item{Y}{Binary indicator of whether the individual experienced the event by 1 time-unit} } }
#'   }
#' @source Simulated Data; see \url{https://github.com/GlenMartin31/predRupdate}
"SYNPM"
