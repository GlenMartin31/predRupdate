#' Validate predicted probabilities against binary outcome
#'
#' This function is used to validate predicted probabilities (usually from an
#' existing/ previously developed logistic regression model) against binary
#' outcomes. It takes a vector of predicted risks (or the linear predictor) and
#' a vector of binary observed outcomes, from which the function calculates
#' metrics of calibration, discrimination and overall accuracy.
#'
#' @param ObservedOutcome a vector of N binary observations, denoting if the
#'   outcome was observed (1) or not observed (0) for each individual in the
#'   validation dataset
#' @param Prob a vector of N predicted probabilities from the model. Specify
#'   either \code{Prob} or \code{LP}.
#' @param LP a vector of N observations where each is the calculated linear
#'   predictor (log-odds) from the existing model that is being evaluated.
#'   Specify either \code{Prob} or \code{LP}.
#' @param CalPlot Indicates whether a calibration plot should be produced, and
#'   the method for doing so. Set to smooth (default) if a flexible (smooth)
#'   calibration plot should be produced using natural cubic splines, set to
#'   grouped if a grouped/binned calibration plot should be produced, and set to
#'   none if no calibration plot should be produced. If set to grouped then
#'   \code{groups} specifies the number of groups to produce.
#' @param groups Specifies the number of groups to use for a grouped/binned
#'   calibration plot. Leave as default (NULL) if either no calibration plot or
#'   a flexible calibration plot is selected.
#' @param xlab,ylab,xlim,ylim Specifies plotting characteristics for the
#'   calibration plot, if relevant.
#'
#' @details This function assesses the predictive performance of predicted risks
#'   against an observed binary outcome. Various metrics of calibration
#'   (agreement between the observed risk and the predicted risks, across the
#'   full risk range) and discrimination (ability of the model to distinguish
#'   between those who develop the outcome and those who do not). For
#'   calibration, a calibration plot is produced, using either flexible methods
#'   or the binned/grouped approach. Calibration-in-the-large (CITL) and
#'   calibration slopes are also estimated. For CITL, we estimate the intercept
#'   by fitting a logistic regression model to the observed binary outcomes,
#'   with the linear predictor of the model as an offset. For calibration slope,
#'   a logistic regression model is fit to the observed binary outcome with the
#'   linear predictor from the model as the only covariate. For discrimination,
#'   we estimate the area under the receiver operating characteristic curve
#'   (AUC). Various other metrics are also calculated to assess overall accuracy
#'   (Brier score, Cox-Snell R2).
#'
#' @return Returns a list of the performance metrics and associated 95%
#'   confidence intervals, where appropriate.
#'
#' @seealso \code{\link{pred_validate}}, \code{\link{pred_predict}},
#'   \code{\link{pred_input_info}}
#'
#' @export
validate_probabilities <- function(ObservedOutcome,
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

  if (missing(Prob)) {
    Prob <- predRupdate::inv_logit(LP)
  } else if (missing(LP)) {
    LP <- predRupdate::logit(Prob)
  } else{
    stop("one of 'Prob' or 'LP' must be specified",
         call. = FALSE)
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
  class(out) <- c("predvalidate_logistic")
  out
}
