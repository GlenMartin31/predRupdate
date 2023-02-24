#' Validate an existing prediction
#'
#' Validate an existing prediction model, to calculate the predictive
#' performance against a new (validation) dataset.
#'
#' @param x an object of class "predinfo"
#' @param newdata data.frame upon which the prediction model should be validated
#' @param binary_outcome Character variable giving the name of the column in
#'   \code{newdata} that represents the observed outcomes. Only relevant for
#'   \code{model_type}="logistic"; leave as \code{NULL} otherwise.
#' @param survival_time Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival times. Only relevant
#'   for \code{model_type}="survival"; leave as \code{NULL} otherwise.
#' @param event_indicator Character variable giving the name of the column in
#'   \code{newdata} that represents the observed survival indicator (1 for
#'   event, 0 for censoring). Only relevant for \code{model_type}="survival";
#'   leave as \code{NULL} otherwise.
#' @param time_horizon for survival models, an integer giving the time horizon
#'   (post baseline/time of prediction) at which a prediction is required.
#'   Currently, this must match a time in x$baselinehazard.
#' @param ... further arguments passed to other methods. See Details below.
#'
#' @details This function takes an existing prediction model formatted according
#'   to \code{\link{pred_input_info}}, and calculates measures of predictive
#'   performance on new data (e.g., within an external validation study). The
#'   information about the existing prediction model should first be inputted by
#'   calling \code{\link{pred_input_info}}, before passing the resulting object
#'   to \code{pred_validate}.
#'
#'   \code{newdata} should be a data.frame, where each row should be an
#'   observation (e.g. patient) and each variable/column should be a predictor
#'   variable. The predictor variables need to include (as a minimum) all of the
#'   predictor variables that are included in the existing prediction model
#'   (i.e., each of the variable names supplied to
#'   \code{\link{pred_input_info}}, through the \code{model_info} parameter,
#'   must match the name of a variables in \code{newdata}).
#'
#'   Any factor variables within \code{newdata} must be converted to dummy (0/1)
#'   variables before calling this function. \code{\link{dummyvars}} can help
#'   with this - see examples below.
#'
#'   \code{binary_outcome}, \code{survival_time} and \code{event_indicator} are
#'   used to specify the outcome variable(s) within \code{newdata} (use
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
#'   a calibration plot is produced, using either flexible methods or the
#'   binned/grouped approach. Calibration-in-the-large (CITL) and calibration
#'   slopes are also estimated. For CITL, we estimate the intercept by fitting a
#'   logistic regression model to the observed binary outcomes, with the linear
#'   predictor of the model as an offset. For calibration slope, a logistic
#'   regression model is fit to the observed binary outcome with the linear
#'   predictor from the model as the only covariate. For discrimination, we
#'   estimate the area under the receiver operating characteristic curve (AUC).
#'   Various other metrics are also calculated to assess overall accuracy (Brier
#'   score, Cox-Snell R2). Specify parameter \code{CalPlot} to indicate whether
#'   a calibration plot should be produced, and the method for doing so; set to
#'   "smooth" (default) if a flexible (smooth) calibration plot should be
#'   produced using natural cubic splines, set to "grouped" if a grouped/binned
#'   calibration plot should be produced, and set to "none" if no calibration
#'   plot should be produced. If set to grouped then specification of parameter
#'   \code{groups} specifies the number of groups to produce. Can also specify
#'   parameters \code{xlab}, \code{ylab}, \code{xlim},and \code{ylim} to change
#'   plotting characteristics for the calibration plot.
#'
#'   In the case of validating a survival prediction model,...
#'
#' @return A list of performance metrics, estimated by applying the existing
#'   prediction model to the newdata.
#'
#' @export
#'
#' @examples
#' #Example 1 - logistic regression existing model, with outcome specified; uses
#' #            an example dataset within the package
#' model1 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_models[1,])
#' pred_validate(x = model1,
#'               newdata = SYNPM$ValidationData,
#'               binary_outcome = "Y")
#'
#' #Example 2 - survival model example; uses an example dataset within the
#' #             package. Also shows use of pre-processing to handle
#' #             categorical variables - need converting prior to call
#' SMART_dummaryvars <- dummyvars(SMART$SMART_dataset)
#' model2 <- pred_input_info(model_type = "survival",
#'                           model_info = SMART$Existing_models[1,],
#'                           baselinehazard = SMART$Framingham_Male_baseline)
#' pred_validate(x = model2,
#'              newdata = SMART_dummaryvars,
#'             survival_time = "TEVENT",
#'             event_indicator = "EVENT",
#'             time_horizon = 10)
#'
#' #Example 3 - multiple existing models
#' model3 <- pred_input_info(model_type = "logistic",
#'                           model_info = SYNPM$Existing_models)
#' pred_validate(x = model3,
#'               newdata = SYNPM$ValidationData,
#'               binary_outcome = "Y")
#'
#' @seealso \code{\link{pred_input_info}}
pred_validate <- function(x,
                          newdata,
                          binary_outcome = NULL,
                          survival_time = NULL,
                          event_indicator = NULL,
                          time_horizon = NULL, ...) {
  UseMethod("pred_validate")
}


#' @export
pred_validate.default <- function(x,
                                  newdata,
                                  binary_outcome = NULL,
                                  survival_time = NULL,
                                  event_indicator = NULL,
                                  time_horizon = NULL, ...) {
  stop("'x' is not of class 'predinfo'",
       call. = FALSE)
}



#' @export
pred_validate.predinfo_logistic <- function(x,
                                            newdata,
                                            binary_outcome = NULL,
                                            survival_time = NULL,
                                            event_indicator = NULL,
                                            time_horizon = NULL, ...){

  #Check outcomes were inputted (needed to validate the model)
  if (is.null(binary_outcome)) {
    stop("binary_outcome must be supplied to validate the existing model(s)",
         call. = FALSE)
  }

  #Make predictions within newdata using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
                                           binary_outcome = binary_outcome)

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
                                            newdata,
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

  #Make predictions within newdata using the existing prediction model(s)
  predictions <- predRupdate::pred_predict(x = x,
                                           newdata = newdata,
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
  cat("\n")
  cat("\nOverall Performance Measures \n",
      "================================= \n", sep = "")
  # cat("Cox-Snell R-squared: ", round(x$R2_CoxSnell, 4), "\n", sep = "")
  # cat("Nagelkerke R-squared: ", round(x$R2_Nagelkerke, 4), "\n", sep = "")
  # cat("Brier Score: ", round(x$BrierScore, 4), "\n", sep = "")

  cat("\n Also examine the histogram of predicted risks. \n")
}


# Internal functions for pred_validate.predinfo_logistic() ---------------------
validate_logistic <- function(ObservedOutcome,
                                   Prob,
                                   LP,
                                   CalPlot = c("smooth",
                                               "grouped",
                                               "none"),
                                   groups = NULL,
                                   xlab = "Predicted Probability",
                                   ylab = "Observed Probability",
                                   xlim = c(0,1),
                                   ylim = c(0,1)) {

  CalPlot <- match.arg(CalPlot)
  if (CalPlot == "grouped" & is.null(groups)) {
    stop("If CalPlot is set to 'grouped' then argument 'groups' must be specified",
         call. = FALSE)
  }
  if (!is.null(groups)) {
    if (length(groups) != 1 | !is.numeric(groups)) {
      stop("Argument 'groups' must be either NULL or a numeric value of length 1",
           calls. = FALSE)
    }
  }

  if (length(Prob) != length(LP)) {
    stop("Lengths of Prob and LP are different",
         call. = FALSE)
  }
  if (length(Prob) != length(ObservedOutcome)) {
    stop("Lengths of Prob and ObservedOutcome are different",
         call. = FALSE)
  } else if (length(LP) != length(ObservedOutcome)) {
    stop("Lengths of LP and ObservedOutcome are different",
         call. = FALSE)
  }

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

  # Remove any missing data in Prob, LP or ObservedOutcome
  if(any(is.na(Prob)) |
     any(is.na(LP)) |
     any(is.na(ObservedOutcome))) {

    ind_miss <- c(which(is.na(Prob)),
                  which(is.na(LP)),
                  which(is.na(ObservedOutcome)))
    ind_miss <- sort(unique(ind_miss))

    Prob <- Prob[-ind_miss]
    LP <- LP[-ind_miss]
    ObservedOutcome <- ObservedOutcome[-ind_miss]

    warning(paste("Some values of Prob/LP/ObservedOutcome have been removed due to missing data.  \n",
                  "Complete case may not be appropriate - consider alternative methods of handling missing data.",
                  sep = ''))
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
  if (CalPlot == "none"){
    graphics::hist(Prob, breaks = seq(xlim[1], xlim[2],
                                      length.out = 20),
                   xlab = xlab,
                   main = "Histogram of the Probability Distribution")
  }
  # otherwise produce calibration plot
  if (CalPlot != "none") {
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
    if (CalPlot == "smooth") {
      spline_model <- stats::glm(ObservedOutcome ~ splines::ns(LP, df = 3),
                                 family = stats::binomial(link = "logit"))
      spline_preds <- stats::predict(spline_model, type = "response", se = T)
      plot_df <- data.frame("p" = Prob,
                            "o" = spline_preds$fit)

      graphics::lines(x = plot_df$p[order(plot_df$p)],
                      y = plot_df$o[order(plot_df$p)])
      rm(plot_df, spline_model, spline_preds)
    } else if (CalPlot == "grouped") {
      plot_df <- data.frame("p" = Prob,
                            "y" = ObservedOutcome)
      plot_df <- plot_df[order(plot_df$p), ]
      plot_df$grouping <- cut(plot_df$p, groups)

      plot_df_split <- split(plot_df, plot_df$grouping)
      calibration_results <- lapply(plot_df_split,
                                    function(x) {
                                      data.frame('observed' = mean(x$y),
                                                 'expected' = mean(x$p))
                                    })
      calibration_results <- as.data.frame(do.call(rbind, calibration_results))
      smoother <- stats::loess(calibration_results$observed ~ calibration_results$expected)

      graphics::points(x = calibration_results$expected,
                       y = calibration_results$observed)
      graphics::lines(x = calibration_results$expected,
                      y = stats::predict(smoother, type = 'fitted'))

      rm(plot_df, plot_df_split, calibration_results, smoother)
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
  class(out) <- c("predvalidate_logistic", "predvalidate")
  out
}





# Internal functions for pred_validate.predinfo_survival() ---------------------
validate_survival <- function(ObservedOutcome,
                              Prob,
                              LP,
                              time_horizon,
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
  cloglog <- log(-log(1 - Prob))
  CalSlope_mod <- survival::coxph(ObservedOutcome ~ cloglog)
  CalSlope <- as.numeric(CalSlope_mod$coefficients[1])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[1,1])

  # Flexible calibration plot
  val.df <- data.frame(ObservedOutcome,
                       LP,
                       Prob,
                       cloglog)
  vcal <- survival::coxph(ObservedOutcome ~ splines::ns(cloglog, df = 3),
                          data = val.df)
  bh <- survival::basehaz(vcal)
  val.df$observed_risk <- 1 - (exp(-bh[(max(which(bh[,2] <= time_horizon))),1])^(exp(stats::predict(vcal, type = "lp", newdata = val.df))))
  plot(0.5, 0.5,
       xlim = c(0,1),
       ylim = c(0,1),
       type = "n",
       xlab = "Predicted Probability",
       ylab = "Observed Probability")
  graphics::clip(xlim[1],xlim[2],ylim[1],ylim[2])
  graphics::abline(0,1)
  graphics::lines(x = val.df$Prob[order(val.df$Prob)],
                  y = val.df$observed_risk[order(val.df$Prob)])

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

