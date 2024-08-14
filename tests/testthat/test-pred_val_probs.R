test_that("pred_val_probs returns expected outputs", {
  set.seed(1234)
  x1 <- rnorm(2000)
  LP <- -2 + (0.5*x1)
  PR <- 1/(1+exp(-LP))
  y <- rbinom(2000, 1, PR)

  #fit hypothetical model to the simulated data
  mod <- glm(y[1:1000] ~ x1[1:1000], family = binomial(link="logit"))

  #obtain the predicted risks from the model
  pred_risk <- predict(mod, type = "response",
                       newdata = data.frame("x1" = x1[1001:2000]))

  #Use pred_val_probs to validate the predicted risks against the
  #observed outcomes
  test_val <- pred_val_probs(binary_outcome = y[1001:2000],
                             Prob = pred_risk,
                             cal_plot = FALSE)

  expect_equal(class(test_val),
               c("predvalidate_logistic", "predvalidate"))
  expect_equal(names(test_val),
               c("level",
                 "OE_ratio", "OE_ratio_lower", "OE_ratio_upper",
                 "CalInt", "CalInt_SE", "CalInt_lower", "CalInt_upper",
                 "CalSlope", "CalSlope_SE", "CalSlope_lower", "CalSlope_upper",
                 "AUC", "AUC_SE", "AUC_lower", "AUC_upper",
                 "R2_CoxSnell", "R2_Nagelkerke",
                 "BrierScore", "Brier_lower", "Brier_upper",
                 "PR_dist", "flex_calibrationplot", "M"))

  expect_error(pred_val_probs(binary_outcome = y[1001:1100],
                              Prob = pred_risk))

  expect_error(pred_val_probs(binary_outcome = y[1001:2000],
                              Prob = pred_risk,
                              level = "hello"))
  expect_error(pred_val_probs(binary_outcome = y[1001:2000],
                              Prob = pred_risk,
                              level = 95))
})
