# Internal function for creating flexible calibration plots ---------------------
flex_calplot <- function(model_type = c("logistic", "survival"),
                         ObservedOutcome,
                         Prob,
                         LP,
                         xlim,
                         ylim,
                         xlab,
                         ylab,
                         pred_rug,
                         cal_plot_n_sample,
                         time_horizon = NULL) {

  model_type <- as.character(match.arg(model_type))

  #test supplied xlims to ensure not cutting-off Prob range
  if(length(xlim) != 2 | length(ylim) !=2) {
    stop("xlim or ylim should be numeric vectors of length 2")
  }
  if(!is.numeric(xlim)) {
    stop("xlim or ylim should be numeric vectors of length 2")
  }
  if(!is.numeric(ylim)) {
    stop("xlim or ylim should be numeric vectors of length 2")
  }
  if(xlim[1] > min(Prob)){
    xlim[1] <- min(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }
  if(xlim[2] < max(Prob)){
    xlim[2] <- max(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }
  if(!is.null(cal_plot_n_sample)) {
    if(length(cal_plot_n_sample) > 1) {
      stop("cal_plot_n_sample should be a single numeric value")
    }
    if(!is.numeric(cal_plot_n_sample)) {
      stop("cal_plot_n_sample should be a single numeric value")
    }
    if(cal_plot_n_sample > length(ObservedOutcome)){
      stop("cal_plot_n_sample should be less than the size of the data")
    }
  }

  if(model_type == "logistic") {
    spline_model <- stats::glm(ObservedOutcome ~ splines::ns(LP, df = 3),
                               family = stats::binomial(link = "logit"))
    spline_preds <- stats::predict(spline_model, type = "response", se = T)
    plot_df <- data.frame("ObservedOutcome" = ObservedOutcome,
                          "p" = Prob,
                          "o" = spline_preds$fit)

    if(!is.null(cal_plot_n_sample)) {
      IDs <- seq(from = 1,
                 to = nrow(plot_df),
                 by = 1)
      ID_samples <- sort(sample(IDs, size = cal_plot_n_sample, replace = FALSE))
      plot_df <- plot_df[ID_samples, ]
      warning("calibraion plot rendered over a sub-set of observations as per cal_plot_n_sample value")
    }

    calplot <- ggplot2::ggplot(plot_df,
                               ggplot2::aes(x = .data$p,
                                            y = .data$o)) +
      ggplot2::geom_line(ggplot2::aes(linetype = "Calibration Curve",
                                      colour = "Calibration Curve")) +
      ggplot2::xlim(xlim) +
      ggplot2::ylim(ylim) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                        linetype = "Reference",
                                        colour = "Reference")) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(color  = "Guide name", linetype = "Guide name") +
      ggplot2::scale_linetype_manual(values = c("Reference" = "dashed",
                                                "Calibration Curve" = "solid"),
                                     breaks = c("Reference",
                                                "Calibration Curve")) +
      ggplot2::scale_colour_manual(values = c("black",
                                              "blue"),
                                   breaks = c("Reference",
                                              "Calibration Curve")) +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     legend.position = "top")

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

    if(!is.null(cal_plot_n_sample)) {
      IDs <- seq(from = 1,
                 to = nrow(plot_df),
                 by = 1)
      ID_samples <- sort(sample(IDs, size = cal_plot_n_sample, replace = FALSE))
      plot_df <- plot_df[ID_samples, ]
      warning("calibraion plot rendered over a sub-set of observations as per cal_plot_n_sample value")
    }

    calplot <- ggplot2::ggplot(plot_df,
                               ggplot2::aes(x = .data$Prob,
                                            y = .data$observed_risk)) +
      ggplot2::geom_line(ggplot2::aes(linetype = "Calibration Curve",
                                      colour = "Calibration Curve")) +
      ggplot2::xlim(xlim) +
      ggplot2::ylim(ylim) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1,
                                        linetype = "Reference",
                                        colour = "Reference")) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::labs(color  = "Guide name", linetype = "Guide name") +
      ggplot2::scale_linetype_manual(values = c("dashed",
                                                "solid"),
                                     breaks = c("Reference",
                                                "Calibration Curve")) +
      ggplot2::scale_colour_manual(values = c("black",
                                              "blue"),
                                   breaks = c("Reference",
                                              "Calibration Curve")) +
      ggplot2::theme(legend.title=ggplot2::element_blank(),
                     legend.position = "top")
  }

  if(pred_rug == TRUE){
    calplot <- calplot +
      ggplot2::geom_rug(sides="b", alpha = 0.2)
  }

  return(calplot)
}
