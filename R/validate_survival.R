# Internal functions for pred_validate.predinfo_survival() ---------------------
validate_survival <- function(ObservedOutcome,
                              Prob,
                              LP,
                              cal_plot,
                              time_horizon,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1),
                              pred_rug = TRUE) {

  # Test if max observed survival time in validation data is less than
  # time_horizon that performance metrics as requested for:
  if(max(ObservedOutcome[,1]) < time_horizon) {
    stop("Maximum observed survival time in validation data is less than time_horizon",
         call. = FALSE)
  }

  # Test for 0 and 1 probabilities
  n_inf <- sum(Prob == 0 | Prob == 1)
  if (n_inf > 0) {
    id <- which(Prob == 0 | Prob == 1)
    ObservedOutcome <- ObservedOutcome[-id]
    LP <- LP[-id]
    Prob <- Prob[-id]
    warning(paste(n_inf,
                  'observations deleted due to predicted risks being 0 and 1'))
  }

  #Test Discrimination
  harrell_C <- survival::concordance(ObservedOutcome ~ LP,
                                     reverse = TRUE)
  harrell_C_est <- harrell_C$concordance
  harrell_C_SE <- sqrt(harrell_C$var)

  #Estimate calibration slope
  CalSlope_mod <- survival::coxph(ObservedOutcome ~ LP)
  CalSlope <- as.numeric(CalSlope_mod$coefficients[1])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[1,1])

  # Check if predicted risks are available for stronger calibration assessments
  if (is.null(Prob)) {
    warning("Predicted risks are not available for some models: limited calibration metrics returned and no calibration plot produced",
            call. = FALSE)

    OE_ratio <- NA
    OE_ratio_SE <- NA
    PR_dist <- NULL
    flex_calibrationplot <- NULL

  } else{
    #Estimate calibration-in-the-large: observed-expected ratio
    KM_observed <- summary(survival::survfit(ObservedOutcome ~ 1),
                           times = time_horizon)
    OE_ratio <- (1 - KM_observed$surv) / mean(Prob)
    OE_ratio_SE <- sqrt(1 / KM_observed$n.event)

    #Distribution of predicted risks:
    plot_df <- data.frame("Prob" = Prob,
                          "Outcome" = factor(ifelse(ObservedOutcome[,2] == 1 &
                                                      ObservedOutcome[,1] <= time_horizon,
                                                    paste("Event prior to time",
                                                          time_horizon, sep = " "),
                                                    paste("No Event/Censored prior to time",
                                                          time_horizon, sep = " "))))
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
        flex_calibrationplot <- flex_calplot(model_type = "survival",
                                             ObservedOutcome = ObservedOutcome,
                                             Prob = Prob,
                                             LP = LP,
                                             xlim = xlim,
                                             ylim = ylim,
                                             xlab = xlab,
                                             ylab = ylab,
                                             pred_rug = pred_rug,
                                             time_horizon = time_horizon)
      }
    } else {
      flex_calibrationplot <- NULL
    }
  }

  #Return results
  out <- list("OE_ratio" = OE_ratio,
              "OE_ratio_SE" = OE_ratio_SE,
              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "harrell_C" = harrell_C_est,
              "harrell_C_SE" = harrell_C_SE,
              "PR_dist" = PR_dist,
              "flex_calibrationplot" = flex_calibrationplot)
  return(out)
}
