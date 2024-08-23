# Internal functions for pred_validate.predinfo_logistic() ---------------------
validate_logistic <- function(ObservedOutcome,
                              Prob,
                              LP,
                              cal_plot,
                              level = 0.95,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1),
                              pred_rug = FALSE,
                              cal_plot_n_sample = NULL) {

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

  z_val <- stats::qnorm(1 - (1-level)/2)

  #Estimate observed:expected ratio
  log_OE_ratio <- log(sum(ObservedOutcome)) - log(mean(Prob)*length(Prob))
  log_OE_ratio_SE <- sqrt(((1 - mean(ObservedOutcome)) / sum(ObservedOutcome)))
  OE_ratio <- exp(log_OE_ratio)
  OE_ratio_lower <- exp(log_OE_ratio - (z_val * log_OE_ratio_SE))
  OE_ratio_upper <- exp(log_OE_ratio + (z_val * log_OE_ratio_SE))


  #Estimate calibration intercept
  CITL_mod <- stats::glm(ObservedOutcome ~ 1,
                         family = stats::binomial(link = "logit"),
                         offset = LP)
  CalInt <- as.numeric(stats::coef(CITL_mod)[1])
  CalIntSE <- sqrt(stats::vcov(CITL_mod)[1,1])
  CalInt_lower <- CalInt - (z_val*CalIntSE)
  CalInt_upper <- CalInt + (z_val*CalIntSE)


  #Estimate calibration slope
  CalSlope_mod <- stats::glm(ObservedOutcome ~ LP,
                             family = stats::binomial(link = "logit"))
  CalSlope <- as.numeric(stats::coef(CalSlope_mod)[2])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[2,2])
  CalSlope_lower <- CalSlope - (z_val*CalSlopeSE)
  CalSlope_upper <- CalSlope + (z_val*CalSlopeSE)


  #Discrimination
  roc_curve <- pROC::roc(response = ObservedOutcome,
                         predictor = Prob,
                         direction = "<",
                         levels = c(0,1),
                         ci = TRUE)
  AUC <- as.numeric(roc_curve$auc)
  AUCSE <- sqrt(pROC::var(roc_curve))
  AUC_lower <- AUC - (z_val*AUCSE)
  AUC_upper <- AUC + (z_val*AUCSE)


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
  Brier_var <- (N^(-2)) * (sum(((1 - (2*Prob))^2) * (Prob*(1-Prob))))
  #Spiegelhalter, D. J. 1986 https://doi.org/10.1002/sim.4780050506
  BrierSE <- sqrt(Brier_var)
  Brier_lower <- BrierScore - (z_val*BrierSE)
  Brier_upper <- BrierScore + (z_val*BrierSE)

  #Distribution of predicted risks:
  plot_df <- data.frame("Prob" = Prob,
                        "Outcome" = factor(ifelse(ObservedOutcome == 1,
                                                  "Event",
                                                  "No Event")))
  PR_dist <- ggplot2::ggplot(plot_df,
                             ggplot2::aes(x = .data$Outcome,
                                          y = .data$Prob)) +
    ggplot2::geom_violin(position = ggplot2::position_dodge(width = .75),
                         linewidth = 1) +
    ggplot2::geom_boxplot(width = 0.1,
                          outlier.shape = NA) +
    ggplot2::ylab(xlab) +
    ggplot2::theme_bw(base_size = 12)

  # Create flexible calibration plot:
  if (cal_plot == TRUE){
    if(length(unique(Prob)) <= 10) { #allows handling of intercept-only models
      stop("Very low unique predicted risks - calplot not possible; call again with cal_plot = FALSE")
    } else{
      flex_calibrationplot <- flex_calplot(model_type = "logistic",
                                           ObservedOutcome = ObservedOutcome,
                                           Prob = Prob,
                                           LP = LP,
                                           xlim = xlim,
                                           ylim = ylim,
                                           xlab = xlab,
                                           ylab = ylab,
                                           pred_rug = pred_rug,
                                           cal_plot_n_sample = cal_plot_n_sample)
    }
  } else {
    flex_calibrationplot <- NULL
  }

  #Return results
  out <- list("level" = level,
              "OE_ratio" = OE_ratio,
              "OE_ratio_lower" = OE_ratio_lower,
              "OE_ratio_upper" = OE_ratio_upper,
              "CalInt" = CalInt,
              "CalInt_SE" = CalIntSE,
              "CalInt_lower" = CalInt_lower,
              "CalInt_upper" = CalInt_upper,
              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "CalSlope_lower" = CalSlope_lower,
              "CalSlope_upper" = CalSlope_upper,
              "AUC" = AUC,
              "AUC_SE" = AUCSE,
              "AUC_lower" = AUC_lower,
              "AUC_upper" = AUC_upper,
              "R2_CoxSnell" = R2_coxsnell,
              "R2_Nagelkerke" = R2_Nagelkerke,
              "BrierScore" = BrierScore,
              "Brier_lower" = Brier_lower,
              "Brier_upper" = Brier_upper,
              "PR_dist" = PR_dist,
              "flex_calibrationplot" = flex_calibrationplot)
  return(out)
}
