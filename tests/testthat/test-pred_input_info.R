test_that("pred_input_info() outputs required info for model implementation", {

  predinfo_test1 <- pred_input_info(model_type = "logistic",
                                    model_info = data.frame("Intercept" = -2,
                                                            "Age" = 5,
                                                            "Age_squared" = 0.05,
                                                            "Age_logged" = 0.06))
  expect_equal(predinfo_test1$M, 1)
  expect_equal(predinfo_test1$model_type, "logistic")
  expect_equal(as.numeric(predinfo_test1$coefs), c(-2.00, 5.00, 0.05, 0.06))
  expect_equal(predinfo_test1$coef_names,
               c("Intercept", "Age", "Age_squared", "Age_logged"))
  expect_s3_class(predinfo_test1$formula, class = "formula")
  expect_s3_class(predinfo_test1, class = "predinfo")
  expect_s3_class(predinfo_test1, class = "predinfo_logistic")


  predinfo_test2 <- pred_input_info(model_type = "logistic",
                                    model_info = data.frame("Intercept" = c(-2, -3),
                                                            "Age" = c(5, 3),
                                                            "Age_squared" = c(0.05,NA),
                                                            "Age_logged" = c(0.06, 0.07)))
  expect_equal(predinfo_test2$M, 2)
  expect_equal(predinfo_test2$model_type, "logistic")
  expect_type(predinfo_test2$coefs, type = "list")
  expect_equal(as.numeric(predinfo_test2$coefs[[1]]), c(-2, 5, 0.05, 0.06))
  expect_equal(as.numeric(predinfo_test2$coefs[[2]]), c(-3, 3, 0.07))
  expect_type(predinfo_test2$coef_names, type = "list")
  expect_equal(predinfo_test2$coef_names[[1]],
               c("Intercept", "Age", "Age_squared", "Age_logged"))
  expect_equal(predinfo_test2$coef_names[[2]],
               c("Intercept", "Age", "Age_logged"))
  expect_type(predinfo_test2$formula, type = "list")
  expect_s3_class(predinfo_test2$formula[[1]], class = "formula")
  expect_s3_class(predinfo_test2$formula[[2]], class = "formula")
  expect_s3_class(predinfo_test1, class = "predinfo")
  expect_s3_class(predinfo_test1, class = "predinfo_logistic")
})


test_that("pred_input_info() returns errors when input not correct", {

  expect_error(pred_input_info(model_type = "logistic",
                               model_info = data.frame("Int" = -2,
                                                       "Age" = 5,
                                                       "Age_squared" = 0.05,
                                                       "Age_logged" = 0.06))
  ) #incorrect specification of intercept in model_info

  expect_error(pred_input_info(model_type = "logistic",
                               model_info = data.frame("Age" = 5,
                                                       "Age_squared" = 0.05,
                                                       "Age_logged" = 0.06))
  ) #no specification of intercept in model_info

})
