test_that("pm_input_info() outputs required info for model implementation", {

  pminfo_test1 <- pm_input_info(model_type = "logistic",
                                existingcoefs = c("(Intercept)" = -2,
                                                  "Age" = 5,
                                                  "Age_squared" = 0.05,
                                                  "Age_logged" = 0.06),
                                formula = ~Age + Age_squared + Age_logged,
                                newdata = data.frame("Age" = c(25,
                                                               27,
                                                               33,
                                                               21)),
                                pre_processing = list(
                                  "Age_squared" = function(df) df$Age^2,
                                  "Age_logged" = function(df) log(df$Age)
                                  )
                                )

  expect_equal(pminfo_test1$model_type, "logistic")
  expect_equal(pminfo_test1$coefs, c(-2.00, 5.00, 0.05, 0.06))
  expect_equal(pminfo_test1$coef_names,
               c("(Intercept)", "Age", "Age_squared", "Age_logged"))
  expect_equal(nrow(pminfo_test1$PredictionData), 4)
  expect_equal(ncol(pminfo_test1$PredictionData), 4)
  expect_s3_class(pminfo_test1, class = "pminfo")
  expect_snapshot(pminfo_test1)
})


test_that("pm_input_info() returns errors when input not correct", {

  expect_error(pm_input_info(model_type = "logistic",
                             existingcoefs = c("(Intercept)" = -2,
                                               "Age" = 5,
                                               "Age_squared" = 0.05,
                                               "Age_logged" = 0.06),
                             formula = ~Age + Age_logged,
                             newdata = data.frame("Age" = c(25,
                                                            27,
                                                            33,
                                                            21)),
                             pre_processing = list(
                               "Age_squared" = function(df) df$Age^2,
                               "Age_logged" = function(df) log(df$Age)
                               )
                             )
               )

  expect_error(pm_input_info(model_type = "logistic",
                             existingcoefs = c("(Intercept)" = -2,
                                               "Age" = 5,
                                               "Age_squared" = 0.05,
                                               "Age_logged" = 0.06),
                             formula = ~Age + Age_squared + Age_logged,
                             newdata = data.frame("Age" = c(25,
                                                            27,
                                                            33,
                                                            21)),
                             pre_processing = list(
                               "Age_squared" = function(df) df$Age^2
                               )
                             )
               )

  expect_error(pm_input_info(model_type = "logistic",
                             existingcoefs = c("(Intercept)" = -2,
                                               "Age" = 5,
                                               "Age_squared" = 0.05,
                                               "Age_logged" = 0.06),
                             formula = ~Age + Age_squared + Age_logged,
                             newdata = data.frame("Age" = c(25,
                                                            27,
                                                            33,
                                                            21)),
                             pre_processing = list(
                               "Age_squared" = function(df) 3,
                               "Age_logged" = function(df) log(df$A)
                               )
                             )
               )

  expect_error(pm_input_info(model_type = "logistic",
                             existingcoefs = c("(Intercept)" = -2,
                                               "Age" = 5),
                             formula = ~1,
                             newdata = data.frame("Age" = c(25,
                                                            27,
                                                            33,
                                                            21))
                             )
               )
})
