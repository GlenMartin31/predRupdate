#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "predinfo"
#' @param new_data data.frame upon which the prediction model should be validated
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{new_data} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise.
#' @param survival_time Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{new_data} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as \code{NULL} otherwise.
#' @param time_horizon for survival models, an integer giving the time horizon
#'   (post baseline/time of prediction) at which a prediction is required.
#'   Currently, this must match a time in x$cum_hazard.
#' @param CalPlot indicate if a flexible calibration plot should be produced
#'   (TRUE) or not (FALSE)
#' @param ... further plotting arguments for the calibration plot. See Details
#'   below.
#'
#' @details This function takes an existing prediction model formatted according
#'   to \code{\link{pred_input_info}}, and calculates measures of predictive
#'   performance on new data (e.g., within an external validation study). The
#'   information about the existing prediction model should first be inputted by
#'   calling \code{\link{pred_input_info}}, before passing the resulting object
#'   to \code{pred_validate}.
#'
#'   \code{new_data} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction model
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{new_data}).
#'
#'   Any factor variables within \code{new_data} must be converted to dummy (0/1)
#'   variables before calling this function. \code{\link{dummy_vars}} can help
#'   with this - see examples below.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{new_data} (use
#'   \code{binary_outcome} if \code{x$model_type} = "logistic", or use
#'   \code{survival_time} and \code{event_indicator} if \code{x$model_type} =
#'   "survival").
#'
#'   In the case of validating a logistic regression model, this function
#'   assesses the predictive performance of the predicted risks against an
#'   observed binary outcome. Various metrics of calibration (agreement between
#'   the observed risk and the predicted risks, across the full risk range) and
#'   discrimination (ability of the model to distinguish between those who
#'   develop the outcome and those who do not) are calculated. For calibration,
#'   a flexible calibration plot is produced. Calibration-in-the-large (CITL)
#'   and calibration slopes are also estimated. CITL is estimated by fitting a
#'   logistic regression model to the observed binary outcomes, with the linear
#'   predictor of the model as an offset. For calibration slope, a logistic
#'   regression model is fit to the observed binary outcome with the linear
#'   predictor from the model as the only covariate. For discrimination, the
#'   function estimates the area under the receiver operating characteristic
#'   curve (AUC). Various other metrics are also calculated to assess overall
#'   accuracy (Brier score, Cox-Snell R2). Specify parameter \code{CalPlot} to
#'   indicate whether a calibration plot should be produced (TRUE), or not
#'   (FALSE). Can also specify parameters \code{xlab}, \code{ylab},
#'   \code{xlim},and \code{ylim} to change plotting characteristics for the
#'   calibration plot.
#'
#'   In the case of validating a survival prediction model, this function
#'   assesses the predictive performance of the predicted event probabilities
#'   (at a fixed time horizon) against an observed time-to-event outcome.
#'   Various metrics of calibration and discrimination are calculated. For
#'   calibration, a flexible calibration plot, observed-to-expected ratio and
#'   calibration slope are produced (all at the specified \code{time_horizon}).
#'   For discrimination, Harrell's C-statistic is calculated. Specify parameter
#'   \code{CalPlot} to indicate whether a calibration plot should be produced
#'   (TRUE), or not (FALSE). Can also specify parameters \code{xlab},
#'   \code{ylab}, \code{xlim},and \code{ylim} to change plotting characteristics
#'   for the calibration plot.
#'
#' @return A list of performance metrics, estimated by applying the existing
#'   prediction model to the new_data.
#'
#' @export
#'
#' @examples
#' #Example 1 - logistic regression existing model, with outcome specified; uses
#' #            an example dataset within the package
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_logistic_models[1,])
#' pred_validate(x = model1,
#'              new_data = SYNPM$ValidationData,
#'              binary_outcome = "Y")
#'
#' #Example 2 - multiple existing model, with outcome specified; uses
#' #            an example dataset within the package
#' model2 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_logistic_models)
#' pred_validate(x = model2,
#'              new_data = SYNPM$ValidationData,
#'              binary_outcome = "Y")
#'
#' #Example 3 - survival model example; uses an example dataset within the
#' #             package.
#' model3 <- pred_input_info(model_type = "survival",
#'                           model_info = SYNPM$Existing_TTE_models[2,],
#'                           cum_hazard = SYNPM$TTE_mod2_baseline)
#' pred_validate(x = model3,
#'              new_data = SYNPM$ValidationData,
#'             survival_time = "ETime",
#'             event_indicator = "Status",
#'             time_horizon = 5)
#'
#' @seealso \code{\link{pred_input_info}}
pred_validate <- function(x,
                          new_data,
                          binary_outcome = NULL,
                          survival_time = NULL,
                          event_indicator = NULL,
                          time_horizon = NULL,
                          CalPlot = TRUE,
                          ...) {
  UseMethod("pred_validate")
}


#' @export
pred_validate.default <- function(x,
                                  new_data,
                                  binary_outcome = NULL,
                                  survival_time = NULL,
                                  event_indicator = NULL,
                                  time_horizon = NULL,
                                  CalPlot = TRUE, ...) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}



#' @export
pred_validate.predinfo_logistic <- function(x,
                                            new_data,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL,
                                            CalPlot = TRUE, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator,
                                           time_horizon = time_horizon)

  if (x$M == 1) {
    ### VALIDATION OF THE EXISTING MODEL
    performance <- validate_logistic(ObservedOutcome = predictions$Outcomes,
                                          Prob = predictions$PredictedRisk,
                                          LP = predictions$LinearPredictor,
                                          ...)
  } else{
    performance <- vector(mode = "list", length = x$M)
    names(performance) <- paste("Model_",1:x$M, sep = "")
    for (m in 1:x$M) {
      performance[[paste("Model_",m, sep = "")]] <-
        validate_logistic(ObservedOutcome = predictions[[m]]$Outcomes,
                               Prob = predictions[[m]]$PredictedRisk,
                               LP = predictions[[m]]$LinearPredictor,
                               ...)
    }
  }
  performance
}



#' @export
pred_validate.predinfo_survival <- function(x,
                                            new_data,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(survival_time) |
      is.null(event_indicator)) {
    stop("survival_time and event_indicator must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #ensure that a time_horizon is supplied - needed for validating time-to-event models:
  if (is.null(time_horizon)) {
    stop("time_horizon must be supplied to validate time-to-event models")
  }

  #Make predictions within new_data using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           new_data = new_data,
                                           binary_outcome = binary_outcome,
                                           survival_time = survival_time,
                                           event_indicator = event_indicator,
                                           time_horizon = time_horizon)

  if (x$M == 1) {
    ### VALIDATION OF THE EXISTING MODEL
    performance <- validate_survival(ObservedOutcome = predictions$Outcomes,
                                     Prob = predictions$PredictedRisk,
                                     LP = predictions$LinearPredictor,
                                     time_horizon = predictions$TimeHorizon,
                                     ...)
  } else{
    performance <- vector(mode = "list", length = x$M)
    names(performance) <- paste("Model_",1:x$M, sep = "")
    for (m in 1:x$M) {
      performance[[paste("Model_",m, sep = "")]] <-
        validate_survival(ObservedOutcome = predictions[[m]]$Outcomes,
                          Prob = predictions[[m]]$PredictedRisk,
                          LP = predictions[[m]]$LinearPredictor,
                          time_horizon = predictions[[m]]$TimeHorizon,
                          ...)
    }
  }
  performance
}



#' @export
print.predvalidate_logistic <- function(x, ...) {
  cat("Calibration Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 2)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Calibration-in-the-large",
                         "Calibration Slope")
  results[1,] <- c(round(x$CITL, 4),
                   round(x$CITL_SE, 4),
                   round((x$CITL - (stats::qnorm(0.975)*x$CITL_SE)), 4),
                   round((x$CITL + (stats::qnorm(0.975)*x$CITL_SE)), 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round((x$CalSlope - (stats::qnorm(0.975)*x$CalSlope_SE)), 4),
                   round((x$CalSlope + (stats::qnorm(0.975)*x$CalSlope_SE)), 4))
  print(results)
  cat("\n Also examine the calibration plot, if produced. \n")
  cat("\nDiscrimination Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 1)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("AUC")
  results[1,] <- c(round(x$AUC, 4),
                   round(x$AUC_SE, 4),
                   round((x$AUC - (stats::qnorm(0.975)*x$AUC_SE)), 4),
                   round((x$AUC + (stats::qnorm(0.975)*x$AUC_SE)), 4))
  print(results)
  cat("\n")
  cat("\nOverall Performance Measures \n",
      "================================= \n", sep = "")
  cat("Cox-Snell R-squared: ", round(x$R2_CoxSnell, 4), "\n", sep = "")
  cat("Nagelkerke R-squared: ", round(x$R2_Nagelkerke, 4), "\n", sep = "")
  cat("Brier Score: ", round(x$BrierScore, 4), "\n", sep = "")

  cat("\n Also examine the histogram of predicted risks. \n")
}



#' @export
print.predvalidate_survival <- function(x, ...) {
  cat("Calibration Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 2)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Observed:Expected Ratio", "Calibration Slope")
  results[1,] <- c(round(x$OE_ratio, 4),
                   round(x$OE_ratio_SE, 4),
                   round((x$OE_ratio * exp(-stats::qnorm(0.975) * x$OE_ratio_SE)), 4),
                   round((x$OE_ratio * exp(stats::qnorm(0.975) * x$OE_ratio_SE)), 4),
                   round((x$CITL + (stats::qnorm(0.975)*x$CITL_SE)), 4))
  results[2,] <- c(round(x$CalSlope, 4),
                   round(x$CalSlope_SE, 4),
                   round((x$CalSlope - (stats::qnorm(0.975)*x$CalSlope_SE)), 4),
                   round((x$CalSlope + (stats::qnorm(0.975)*x$CalSlope_SE)), 4))
  print(results)
  cat("\n Also examine the calibration plot, if produced. \n")
  cat("\nDiscrimination Measures \n",
      "================================= \n", sep = "")
  results <- matrix(NA, ncol = 4, nrow = 1)
  colnames(results) <- c("Estimate",
                         "Std. Err",
                         "Lower 95% Confidence Interval",
                         "Upper 95% Confidence Interval")
  rownames(results) <- c("Harrell C")
  results[1,] <- c(round(x$harrell_C, 4),
                   round(x$harrell_C_SE, 4),
                   round((x$harrell_C - (stats::qnorm(0.975)*x$harrell_C_SE)), 4),
                   round((x$harrell_C + (stats::qnorm(0.975)*x$harrell_C_SE)), 4))
  print(results)
  # cat("\n")
  # cat("\nOverall Performance Measures \n",
  #     "================================= \n", sep = "")
  # cat("Cox-Snell R-squared: ", round(x$R2_CoxSnell, 4), "\n", sep = "")
  # cat("Nagelkerke R-squared: ", round(x$R2_Nagelkerke, 4), "\n", sep = "")
  # cat("Brier Score: ", round(x$BrierScore, 4), "\n", sep = "")

  cat("\n Also examine the histogram of predicted risks. \n")
}


# Internal functions for pred_validate.predinfo_logistic() ---------------------
validate_logistic <- function(ObservedOutcome,
                              Prob,
                              LP,
                              CalPlot = TRUE,
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
  if (CalPlot == FALSE){
    graphics::hist(Prob, breaks = seq(xlim[1], xlim[2],
                                      length.out = 20),
                   xlab = xlab,
                   main = "Histogram of the Probability Distribution")
  } else{
    # otherwise produce calibration plot
    flex_calplot(model_type = "logistic",
                 ObservedOutcome = ObservedOutcome,
                 Prob = Prob,
                 LP = LP,
                 xlim = xlim,
                 ylim = ylim,
                 xlab = xlab,
                 ylab = ylab)
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
  class(out) <- c("predvalidate_logistic", "predvalidate")
  out
}





# Internal functions for pred_validate.predinfo_survival() ---------------------
validate_survival <- function(ObservedOutcome,
                              Prob,
                              LP,
                              CalPlot = TRUE,
                              time_horizon,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1)) {

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


  #Estimate calibration-in-the-large: observed-expected ratio
  KM_observed <- summary(survival::survfit(ObservedOutcome ~ 1),
                         times = time_horizon)
  OE_ratio <- (1 - KM_observed$surv) / mean(Prob)
  OE_ratio_SE <- sqrt(1 / KM_observed$n.event)

  #Estimate calibration slope
  CalSlope_mod <- survival::coxph(ObservedOutcome ~ LP)
  CalSlope <- as.numeric(CalSlope_mod$coefficients[1])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[1,1])


  # If not creating a calibration plot, then at least produce histogram of
  # predicted risks; otherwise this is embedded into the calibration plot
  if (CalPlot == FALSE){
    graphics::hist(Prob, breaks = seq(xlim[1], xlim[2],
                                      length.out = 20),
                   xlab = xlab,
                   main = "Histogram of the Probability Distribution")
  } else{
    # otherwise produce calibration plot
    flex_calplot(model_type = "survival",
                 ObservedOutcome = ObservedOutcome,
                 Prob = Prob,
                 LP = LP,
                 xlim = xlim,
                 ylim = ylim,
                 xlab = xlab,
                 ylab = ylab,
                 time_horizon = time_horizon)
  }


  #Return results
  out <- list("OE_ratio" = OE_ratio,
              "OE_ratio_SE" = OE_ratio_SE,
              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "harrell_C" = harrell_C_est,
              "harrell_C_SE" = harrell_C_SE)
  class(out) <- c("predvalidate_survival", "predvalidate")
  out
}



# Internal functions for creating flexible calibration plots ---------------------
flex_calplot <- function(model_type = c("logistic", "survival"),
                         ObservedOutcome,
                         Prob,
                         LP,
                         xlim,
                         ylim,
                         xlab,
                         ylab,
                         time_horizon = NULL) {

  model_type <- as.character(match.arg(model_type))

  ## set graphical parameters
  graphics::layout(matrix(c(1,2), ncol=1),
                   widths=c(1),
                   heights=c(1/7, 6/7))
  pardefault_mar <- graphics::par("mar") #save default plotting margin values
  pardefault_oma <- graphics::par("oma") #save default outer margin values
  graphics::par(mar=c(4, 4, 1, 1),
                oma=rep(0.5, 4)) # plot parameters

  #return to default plotting parameters post function call:
  on.exit(graphics::layout(1), add = TRUE)
  on.exit(graphics::par(mar = pardefault_mar,
                        oma = pardefault_oma),
          add = TRUE,
          after = TRUE)

  #test supplied xlims to ensure not cutting-off Prob range
  if(xlim[1] > min(Prob)){
    xlim[1] <- min(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }
  if(xlim[2] < max(Prob)){
    xlim[2] <- max(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }

  ## Produce histogram of predicted risks to show the distribution
  xhist <- graphics::hist(Prob, breaks = seq(xlim[1], xlim[2],
                                             length.out = 20),
                          plot=FALSE)
  graphics::par(mar=c(0, 4, 0, 0))
  graphics::barplot(xhist$density, axes=FALSE,
                    ylim=c(0, max(xhist$density)),
                    space=0)

  ## Produce calibration plot
  graphics::par(mar=c(4, 4, 0, 0))
  plot(0.5, 0.5,
       xlim = xlim,
       ylim = ylim,
       type = "n",
       xlab = xlab,
       ylab = ylab)
  graphics::clip(xlim[1],xlim[2],ylim[1],ylim[2])
  graphics::abline(0,1)
  if(model_type == "logistic") {
    spline_model <- stats::glm(ObservedOutcome ~ splines::ns(LP, df = 3),
                               family = stats::binomial(link = "logit"))
    spline_preds <- stats::predict(spline_model, type = "response", se = T)
    plot_df <- data.frame("p" = Prob,
                          "o" = spline_preds$fit)

    graphics::lines(x = plot_df$p[order(plot_df$p)],
                    y = plot_df$o[order(plot_df$p)])
  } else {
    cloglog <- log(-log(1 - Prob))
    plot_df <- data.frame(ObservedOutcome,
                          Prob,
                          LP,
                          cloglog)
    vcal <- survival::coxph(ObservedOutcome ~ splines::ns(cloglog, df = 3),
                            data = plot_df)
    bh <- survival::basehaz(vcal)
    plot_df$observed_risk <- 1 - (exp(-bh[(max(which(bh[,2] <= time_horizon))),1])^(exp(stats::predict(vcal, type = "lp"))))


    graphics::lines(x = plot_df$Prob[order(plot_df$Prob)],
                    y = plot_df$observed_risk[order(plot_df$Prob)])
  }
}
