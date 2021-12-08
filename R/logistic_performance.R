#' Calculate predictive performance of a logistic regression model
#'
#' @param ObservedOutcome a vector of N binary observations, denoting if the
#'   outcome was observed (1) or not observed (0) for each individual in the
#'   validation dataset
#' @param LinearPredictor a vector of N observations where each is the
#'   calculated linear predictor from the existing model that is being evaluated
#' @noRd
logistic_performance <- function(ObservedOutcome,
                                 LinearPredictor){

  #Estimate calibration intercept (i.e. calibration-in-the-large)
  CITL_mod <- stats::glm(ObservedOutcome ~ stats::offset(LinearPredictor),
                         family = stats::binomial(link = "logit"))
  CITL <- as.numeric(stats::coef(CITL_mod)[1])
  CITLSE <- sqrt(stats::vcov(CITL_mod)[1,1])


  #Estimate calibration slope
  CalSlope_mod <- stats::glm(ObservedOutcome ~ LinearPredictor,
                             family = stats::binomial(link = "logit"))
  CalSlope <- as.numeric(stats::coef(CalSlope_mod)[2])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[2,2])


  #Discrimination
  PredictedRisk <- pmupdate::inv_logit(LinearPredictor)
  roc_curve <- pROC::roc(response = ObservedOutcome,
                         predictor = PredictedRisk,
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
    (sum((PredictedRisk - ObservedOutcome)^2))


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
}
