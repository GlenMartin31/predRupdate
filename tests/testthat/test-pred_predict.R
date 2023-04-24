test_that("output of pred_predict for logistic model is correct", {
  coefs_table <- data.frame("Intercept" = -3.4,
                            "Sex_M" = 0.306,
                            "Smoking_Status" = 0.628)
  existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                             model_info = coefs_table)
  new_df <- data.frame("Sex" = as.factor(c("M", "F", "M", "M", "F", "F", "M")),
                       "Smoking_Status" = c(1, 0, 0, 1, 1, 0, 1))

  expect_error(pred_predict(x = existing_Logistic_Model,
                            new_data = new_df))

  #new_df has a factor variable, so needs indicator variables creating before pred_predict:
  new_df_indicators <- dummy_vars(new_df)
  predout <- pred_predict(x = existing_Logistic_Model,
                          new_data = new_df_indicators)

  expect_type(predout, type = "list")
  expect_equal(length(predout), 3)
  expect_equal(names(predout), c("LinearPredictor", "PredictedRisk", "Outcomes"))
})


test_that("output of pred_predict for survival model is correct", {
  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[1,],
                            cum_hazard = SYNPM$TTE_mod1_baseline)
  predout <- pred_predict(x = model2,
                          new_data = SYNPM$ValidationData,
                          survival_time = "ETime",
                          event_indicator = "Status",
                          time_horizon = 5)

  expect_type(predout, type = "list")
  expect_equal(length(predout), 4)
  expect_equal(names(predout), c("LinearPredictor", "PredictedRisk", "TimeHorizon", "Outcomes"))
})


test_that("output of pred_predict for multiple logistic model passing", {
  model2 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models)
  predout <- pred_predict(x = model2,
                          new_data = SYNPM$ValidationData,
                          binary_outcome = "Y")

  expect_type(predout, type = "list")
  expect_equal(length(predout), model2$M)
  expect_equal(names(predout[[1]]), c("LinearPredictor", "PredictedRisk", "Outcomes"))
  expect_equal(names(predout[[2]]), c("LinearPredictor", "PredictedRisk", "Outcomes"))
})


test_that("output of pred_predict for multiple survival model passing", {
  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[1:2,],
                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                              SYNPM$TTE_mod2_baseline))
  predout <- pred_predict(x = model2,
                          new_data = SYNPM$ValidationData,
                          survival_time = "ETime",
                          event_indicator = "Status",
                          time_horizon = 5)

  expect_type(predout, type = "list")
  expect_equal(length(predout), model2$M)
  expect_equal(names(predout[[1]]), c("LinearPredictor", "PredictedRisk", "TimeHorizon", "Outcomes"))
  expect_equal(names(predout[[2]]), c("LinearPredictor", "PredictedRisk", "TimeHorizon", "Outcomes"))
})


test_that("output of pred_predict for survival models with no cum_hazard", {
  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models,
                            cum_hazard = list(NULL,
                                              NULL,
                                              NULL))
  predout <- pred_predict(x = model2,
                          new_data = SYNPM$ValidationData,
                          survival_time = "ETime",
                          event_indicator = "Status",
                          time_horizon = 5)

  expect_type(predout, type = "list")
  expect_equal(length(predout), model2$M)
  expect_equal(names(predout[[1]]$PredictedRisk), NULL)


  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[2,],
                            cum_hazard = SYNPM$TTE_mod2_baseline)
  predout <- pred_predict(x = model2,
                          new_data = SYNPM$ValidationData,
                          survival_time = "ETime",
                          event_indicator = "Status",
                          time_horizon = 5)

  expect_type(predout, type = "list")
  expect_equal(length(predout), 4)
  expect_equal(names(predout$PredictedRisk), NULL)
})



test_that("error messages of pred_predict are as expected", {
  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models,
                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                              SYNPM$TTE_mod2_baseline,
                                              SYNPM$TTE_mod3_baseline))
  expect_error(pred_predict(x = model2,
                            new_data = SYNPM$ValidationData,
                            survival_time = "ETime",
                            event_indicator = "Status",
                            time_horizon = c(3, 4, 5))) #not allowed multiple time_horizons

  expect_error(pred_predict(x = model2,
                            new_data = SYNPM$ValidationData,
                            survival_time = "Time", #outcome name wrong
                            event_indicator = "Status",
                            time_horizon = 5))

  expect_error(pred_predict(x = data.frame("test"), #not predinfo object
                            new_data = SYNPM$ValidationData,
                            survival_time = "ETime",
                            event_indicator = "Status",
                            time_horizon = 5))
})


