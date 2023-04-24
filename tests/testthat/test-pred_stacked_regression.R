test_that("pred_stacked_regression has expected output", {
  LogisticModels <- pred_input_info(model_type = "logistic",
                                    model_info = SYNPM$Existing_logistic_models)
  expect_error(pred_stacked_regression(x = data.frame("test"),
                                       new_data = SYNPM$ValidationData,
                                       binary_outcome = "Y"))
  expect_error(pred_stacked_regression(x = data.frame("test"),
                                       new_data = SYNPM$ValidationData))
  expect_error(pred_stacked_regression(x = pred_input_info(model_type = "logistic",
                                                           model_info = SYNPM$Existing_logistic_models[1,]),
                                       new_data = SYNPM$ValidationData,
                                       binary_outcome = "Y"))

  SR <- pred_stacked_regression(x = LogisticModels,
                                new_data = SYNPM$ValidationData,
                                binary_outcome = "Y",
                                positivity_constraint = FALSE)
  expect_type(SR, type = "list")
  expect_equal(names(SR), c("M", "model_type", "coefs", "coef_names", "formula", "model_info", "Stacked_Regression_Weights"))
  expect_s3_class(SR, "predSR")
  expect_s3_class(SR, "predinfo_logistic")
  expect_s3_class(SR, "predinfo")

  expect_snapshot(summary(SR))

  SR <- pred_stacked_regression(x = LogisticModels,
                                new_data = SYNPM$ValidationData,
                                binary_outcome = "Y",
                                positivity_constraint = TRUE)
  expect_type(SR, type = "list")
  expect_equal(names(SR), c("M", "model_type", "coefs", "coef_names", "formula", "model_info", "Stacked_Regression_Weights"))
  expect_s3_class(SR, "predSR")
  expect_s3_class(SR, "predinfo_logistic")
  expect_s3_class(SR, "predinfo")




  TTModels <- pred_input_info(model_type = "survival",
                              model_info = SYNPM$Existing_TTE_models,
                              cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                SYNPM$TTE_mod2_baseline,
                                                SYNPM$TTE_mod3_baseline))
  expect_error(pred_stacked_regression(x = TTModels,
                                       new_data = SYNPM$ValidationData))

  SR <- pred_stacked_regression(x = TTModels,
                                new_data = SYNPM$ValidationData,
                                survival_time = "ETime",
                                event_indicator = "Status")

  expect_type(SR, type = "list")
  expect_equal(names(SR), c("M", "model_type", "coefs", "coef_names", "formula", "cum_hazard", "model_info", "Stacked_Regression_Weights"))
  expect_s3_class(SR, "predSR")
  expect_s3_class(SR, "predinfo_survival")
  expect_s3_class(SR, "predinfo")
})
