# Internal functions for pred_validate.predinfo_logistic() ---------------------
validate_logistic <- function(ObservedOutcome,
                              Prob,
                              LP,
                              cal_plot,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1)) {

  # Test for 0 and 1 probabilities
  n_inf <- sum(is.infinite(LP))
  if (n_inf > 0) {
    id <- which(is.infinite(LP))
    ObservedOutcome <- ObservedOutcome[-id]
    LP <- LP[-id]
    Prob <- Prob[-id]
    warning(paste(n_inf,
                  'observations deleted due to predicted risks being 0 and 1'))
  }


  #Estimate calibration intercept (i.e. calibration-in-the-large)
  CITL_mod <- stats::glm(ObservedOutcome ~ 1,
                         family = stats::binomial(link = "logit"),
                         offset = LP)
  CITL <- as.numeric(stats::coef(CITL_mod)[1])
  CITLSE <- sqrt(stats::vcov(CITL_mod)[1,1])


  #Estimate calibration slope
  CalSlope_mod <- stats::glm(ObservedOutcome ~ LP,
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


  #R-squared metrics
  R2_mod <- stats::glm(ObservedOutcome ~ -1,
                       family = stats::binomial(link = "logit"),
                       offset = LP)
  E <- sum(ObservedOutcome) #number of events in the validation data
  N <- length(ObservedOutcome) #number of observations in the validation data
  L_Null <- (E*log(E/N)) + ((N-E)*log(1 - (E/N)))
  LR <- -2 * (L_Null - as.numeric(stats::logLik(R2_mod)))
  MaxR2 <- 1 - exp((2*L_Null) / length(ObservedOutcome))
  R2_coxsnell <- 1 - exp(-LR / length(ObservedOutcome))
  R2_Nagelkerke <- R2_coxsnell / MaxR2


  #Brier Score
  BrierScore <- 1/N * (sum((Prob - ObservedOutcome)^2))

  # If not creating a calibration plot, then at least produce histogram of
  # predicted risks; otherwise this is embedded into the calibration plot
  if (cal_plot == FALSE){
    plot_df <- data.frame("Prob" = Prob)
    print(ggplot2::ggplot(plot_df,
                          ggplot2::aes(x = .data$Prob)) +
            ggplot2::geom_histogram(bins = 30,
                                    colour = "black") +
            ggplot2::ggtitle("Histogram of the Probability Distribution") +
            ggplot2::xlab(xlab) +
            ggplot2::theme_bw(base_size = 12))

  } else{

    if(length(unique(Prob)) <= 10) { #allows handling of intercept-only models
      stop("Very low unique predicted risks - calplot not possible; call again with cal_plot = FALSE")
    } else{
      flex_calplot(model_type = "logistic",
                   ObservedOutcome = ObservedOutcome,
                   Prob = Prob,
                   LP = LP,
                   xlim = xlim,
                   ylim = ylim,
                   xlab = xlab,
                   ylab = ylab)
    }

  }

  #Return results
  out <- list("CITL" = CITL,
              "CITL_SE" = CITLSE,
              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "AUC" = AUC,
              "AUC_SE" = AUCSE,
              "R2_CoxSnell" = R2_coxsnell,
              "R2_Nagelkerke" = R2_Nagelkerke,
              "BrierScore" = BrierScore)
  out
}
