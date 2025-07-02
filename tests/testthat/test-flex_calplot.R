test_that("flex_calplot works", {
  # simulate some data for purposes of example illustration
  set.seed(1234)
  x1 <- rnorm(2000)
  LP <- -2 + (0.5*x1)
  PR <- 1/(1+exp(-LP))
  y <- rbinom(2000, 1, PR)

  #fit hypothetical model to the simulated data
  mod <- glm(y[1:1000] ~ x1[1:1000], family = binomial(link="logit"))
  # #obtain the predicted risks from the model
  pred_risk <- predict(mod, type = "response",
                       newdata = data.frame("x1" = x1[1001:2000]))
  pred_LP <- predict(mod, type = "link",
                     newdata = data.frame("x1" = x1[1001:2000]))

  expect_true(ggplot2::is_ggplot(flex_calplot(model_type = "logistic", "survival",
                                              ObservedOutcome = y[1001:2000],
                                              Prob = pred_risk,
                                              LP = pred_LP,
                                              xlab = "Predicted Probability",
                                              ylab = "Observed Probability",
                                              xlim = c(0,1),
                                              ylim = c(0,1),
                                              pred_rug = FALSE,
                                              cal_plot_n_sample = NULL)))

  expect_true(ggplot2::is_ggplot(flex_calplot(model_type = "logistic", "survival",
                                              ObservedOutcome = y[1001:2000],
                                              Prob = pred_risk,
                                              LP = pred_LP,
                                              xlab = "Predicted Probability",
                                              ylab = "Observed Probability",
                                              xlim = c(0,1),
                                              ylim = c(0,1),
                                              pred_rug = TRUE,
                                              cal_plot_n_sample = NULL)))

  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = c(0,1,3),
                            ylim = c(0,1),
                            pred_rug = TRUE,
                            cal_plot_n_sample = NULL))
  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = "hello",
                            ylim = c(0,1),
                            pred_rug = TRUE,
                            cal_plot_n_sample = NULL))
  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = c(0,1),
                            ylim = "hello",
                            pred_rug = TRUE,
                            cal_plot_n_sample = NULL))

  expect_warning(flex_calplot(model_type = "logistic", "survival",
                              ObservedOutcome = y[1001:2000],
                              Prob = pred_risk,
                              LP = pred_LP,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(min(pred_risk)+0.0001,1),
                              ylim = c(0,1),
                              pred_rug = TRUE,
                              cal_plot_n_sample = NULL))
  expect_warning(flex_calplot(model_type = "logistic", "survival",
                              ObservedOutcome = y[1001:2000],
                              Prob = pred_risk,
                              LP = pred_LP,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,max(pred_risk)-0.0001),
                              ylim = c(0,1),
                              pred_rug = TRUE,
                              cal_plot_n_sample = NULL))

  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = c(0,1),
                            ylim = c(0,1),
                            pred_rug = TRUE,
                            cal_plot_n_sample = c(100,1)))
  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = c(0,1),
                            ylim = c(0,1),
                            pred_rug = TRUE,
                            cal_plot_n_sample = "hello"))
  expect_error(flex_calplot(model_type = "logistic", "survival",
                            ObservedOutcome = y[1001:2000],
                            Prob = pred_risk,
                            LP = pred_LP,
                            xlab = "Predicted Probability",
                            ylab = "Observed Probability",
                            xlim = c(0,1),
                            ylim = c(0,1),
                            pred_rug = TRUE,
                            cal_plot_n_sample = 4000))
  expect_warning(flex_calplot(model_type = "logistic", "survival",
                              ObservedOutcome = y[1001:2000],
                              Prob = pred_risk,
                              LP = pred_LP,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1),
                              pred_rug = TRUE,
                              cal_plot_n_sample = 100))

})
