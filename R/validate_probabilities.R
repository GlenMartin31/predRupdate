#' Validate predicted probabilities against binary outcome
#'
#' This function is used to validate predicted probabilities (usually from an
#' existing/ previously developed logistic regression model) against binary
#' outcomes. It takes a vector of predicted risks (or the log odds - the linear
#' predictor) and a vector of binary observed probabilities (not used to develop
#' the model that generates the predictions). From which, the function
#' calculates metrics of calibration, discrimination and overall accuracy.
#'
#' @param ObservedOutcome a vector of N binary observations, denoting if the
#'   outcome was observed (1) or not observed (0) for each individual in the
#'   validation dataset
#' @param Prob a vector of N predicted probabilities from the model. Specify
#'   either \code{Prob} or \code{Logit}.
#' @param Logit a vector of N observations where each is the calculated linear
#'   predictor (log-odds) from the existing model that is being evaluated.
#'   Specify either \code{Prob} or \code{Logit}.
#'
#' @details TO ADD
#'
#' @return Returns a list of the performance metrics and associated 95%
#'   confidence intervals, where appropriate.
#'
#' @seealso \code{\link{pm_validate}}
#'
#' @export
validate_probabilities <- function(ObservedOutcome,
                                   Prob,
                                   Logit) {

  if (missing(Prob)) {
    Prob <- pmupdate::inv_logit(Logit)
  } else if (missing(Logit)) {
    Logit <- pmupdate::logit(Prob)
  } else{
    stop("one of 'Prob' or 'Logit' must be specified",
         call. = FALSE)
  }

  #Estimate calibration intercept (i.e. calibration-in-the-large)
  CITL_mod <- stats::glm(ObservedOutcome ~ stats::offset(Logit),
                         family = stats::binomial(link = "logit"))
  CITL <- as.numeric(stats::coef(CITL_mod)[1])
  CITLSE <- sqrt(stats::vcov(CITL_mod)[1,1])


  #Estimate calibration slope
  CalSlope_mod <- stats::glm(ObservedOutcome ~ Logit,
                             family = stats::binomial(link = "logit"))
  CalSlope <- as.numeric(stats::coef(CalSlope_mod)[2])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[2,2])


  #Discrimination
  roc_curve <- pROC::roc(response = ObservedOutcome,
                         predictor = Prob,
                         direction = "<",
                         levels = c(0,1),
                         ci = TRUE)
  AUC <- as.numeric(roc_curve$auc)
  AUCSE <- sqrt(pROC::var(roc_curve))

  #R2_coxsnell
  LR <- -2 * (as.numeric(stats::logLik(stats::glm(ObservedOutcome ~ 1,
                                                  family = stats::binomial(link = "logit")))) -
                as.numeric(stats::logLik(CITL_mod)))
  R2_coxsnell <- 1 - exp(-LR/length(ObservedOutcome))


  #Brier Score
  BrierScore <- 1/length(ObservedOutcome) *
    (sum((Prob - ObservedOutcome)^2))


  #Return results
  out <- list("CITL" = CITL,
              "CITL_SE" = CITLSE,
              "CITL_Lower" = CITL - (stats::qnorm(0.975)*CITLSE),
              "CITL_Upper" = CITL + (stats::qnorm(0.975)*CITLSE),

              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "CalSlope_Lower" = CalSlope - (stats::qnorm(0.975)*CalSlopeSE),
              "CalSlope_Upper" = CalSlope + (stats::qnorm(0.975)*CalSlopeSE),

              "AUC" = AUC,
              "AUC_SE" = AUCSE,
              "AUC_Lower" = AUC - (stats::qnorm(0.975)*AUCSE),
              "AUC_Upper" = AUC + (stats::qnorm(0.975)*AUCSE),

              "R2" = R2_coxsnell,
              "BrierScore" = BrierScore)
  out
}
