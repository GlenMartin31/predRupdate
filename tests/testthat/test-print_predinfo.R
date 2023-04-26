test_that("expected output form of pred_input_info", {
  predinfo_test1 <- pred_input_info(model_type = "logistic",
                                    model_info = data.frame("Intercept" = -2,
                                                            "Age" = 5,
                                                            "Age_squared" = 0.05,
                                                            "Age_logged" = 0.06))

  expect_output(print(predinfo_test1))
})
