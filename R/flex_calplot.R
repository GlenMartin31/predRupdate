# Internal function for creating flexible calibration plots ---------------------
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

  #test supplied xlims to ensure not cutting-off Prob range
  if(xlim[1] > min(Prob)){
    xlim[1] <- min(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }
  if(xlim[2] < max(Prob)){
    xlim[2] <- max(Prob)
    warning("Altering xlim range: specified range inconsistent with predicted risk range")
  }

  if(model_type == "logistic") {
    spline_model <- stats::glm(ObservedOutcome ~ splines::ns(LP, df = 3),
                               family = stats::binomial(link = "logit"))
    spline_preds <- stats::predict(spline_model, type = "response", se = T)
    plot_df <- data.frame("p" = Prob,
                          "o" = spline_preds$fit)

    print(ggExtra::ggMarginal(ggplot2::ggplot(plot_df,
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
                                                                  colour = "Reference"),
                                                     show.legend = FALSE) +
                                ggplot2::geom_point(alpha = 0) +
                                ggplot2::coord_fixed() +
                                ggplot2::theme_bw(base_size = 12) +
                                ggplot2::labs(color  = "Guide name", linetype = "Guide name") +
                                ggplot2::scale_linetype_manual(values = c("dashed",
                                                                          "solid"),
                                                               breaks = c("Reference",
                                                                          "Calibration Curve"),
                                                               labels = c("Reference",
                                                                          "Calibration Curve")) +
                                ggplot2::scale_colour_manual(values = c("black",
                                                                        "blue"),
                                                             breaks = c("Reference",
                                                                        "Calibration Curve")) +
                                ggplot2::theme(legend.title=ggplot2::element_blank()),
                              type = "histogram",
                              margins = "x"))

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

    print(ggExtra::ggMarginal(ggplot2::ggplot(plot_df,
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
                                                                  colour = "Reference"),
                                                     show.legend = FALSE) +
                                ggplot2::geom_point(alpha = 0) +
                                ggplot2::coord_fixed() +
                                ggplot2::theme_bw(base_size = 12) +
                                ggplot2::labs(color  = "Guide name", linetype = "Guide name") +
                                ggplot2::scale_linetype_manual(values = c("dashed",
                                                                          "solid"),
                                                               breaks = c("Reference",
                                                                          "Calibration Curve"),
                                                               labels = c("Reference",
                                                                          "Calibration Curve")) +
                                ggplot2::scale_colour_manual(values = c("black",
                                                                        "blue"),
                                                             breaks = c("Reference",
                                                                        "Calibration Curve")) +
                                ggplot2::theme(legend.title=ggplot2::element_blank()),
                              type = "histogram",
                              margins = "x"))
  }

}
