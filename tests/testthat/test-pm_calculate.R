test_that("check predicted risks from pm_calculate for logistic", {
  set.seed(1234)
  testdf <- data.frame("X" = rnorm(5),
                       "Y" = rbinom(5, 1, 0.2))
  existingcoefs <- c("(Intercept)" = -2, "X" = 0.5)
  LP_true <- existingcoefs["(Intercept)"] + (existingcoefs["X"]*testdf$X)
  PR_true <- 1 / (1 + exp(-LP_true))

  testdf <- pm_calculate(formula = "Y~X", type = "l",
                         existingcoefs = existingcoefs,
                         newdata = testdf, add = TRUE)

  expect_identical(testdf$PredictedRisk, PR_true)
  expect_identical(testdf$LinearPredictor, LP_true)
})

test_that("check error returned with incorrect coefficient names", {
  set.seed(1234)
  testdf <- data.frame("X" = rnorm(5),
                       "Y" = rbinom(5, 1, 0.2))

  expect_error(pm_calculate(formula = "Y~X", type = "l",
                            existingcoefs = c("(Intercept)" = -2, "Z" = 0.5),
                            newdata = testdf, add = TRUE),
               "Variable Z in existingcoefs is not found in functional form of newdata. \n Check that elements in formula are also included in existingcoefs")
  expect_error(pm_calculate(formula = "Y~X", type = "l",
                            existingcoefs = c("Intercept" = -2, "X" = 0.5),
                            newdata = testdf, add = TRUE),
               "Variable Intercept in existingcoefs is not found in functional form of newdata. \n Check that elements in formula are also included in existingcoefs")
})
