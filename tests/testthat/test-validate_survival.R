test_that("survival validations work without baseline hazard", {
  coefs_table <- data.frame("Age" = 0.007,
                            "SexM" = 0.225,
                            "Smoking_Status" = 0.685,
                            "Diabetes" = 0.425,
                            "Creatinine" = 0.587)

  #pass this into pred_input_info()
  Existing_TTE_Model <- pred_input_info(model_type = "survival",
                                        model_info = coefs_table,
                                        cum_hazard = NULL) #leave as NULL if the baseline not available

  expect_warning(pred_validate(x = Existing_TTE_Model,
                               new_data = SYNPM$ValidationData,
                               survival_time = "ETime",
                               event_indicator = "Status",
                               time_horizon = 5))

  validation_results <- suppressWarnings(pred_validate(x = Existing_TTE_Model,
                                      new_data = SYNPM$ValidationData,
                                      survival_time = "ETime",
                                      event_indicator = "Status",
                                      time_horizon = 5))
  expect_equal(validation_results$OE_ratio, NA)
  expect_equal(validation_results$OE_ratio_SE, NA)
})

test_that("survival validations through error if all observed survival times
          are less than time_horizon", {
  coefs_table <- data.frame("Age" = 0.007,
                            "SexM" = 0.225,
                            "Smoking_Status" = 0.685,
                            "Diabetes" = 0.425,
                            "Creatinine" = 0.587)

  Existing_TTE_Model <- pred_input_info(model_type = "survival",
                                        model_info = coefs_table,
                                        cum_hazard = NULL) #leave as NULL if the baseline not available

  expect_error(pred_validate(x = Existing_TTE_Model,
                             new_data = SYNPM$ValidationData[which(SYNPM$ValidationData$ETime<4),],
                             survival_time = "ETime",
                             event_indicator = "Status",
                             time_horizon = 5))


})

test_that("catch for too few unique predicted risks", {
  expect_error(pred_validate(pred_input_info(model_type = "survival",
                                             model_info = data.frame("SexM" = 0.012),
                                             cum_hazard = SYNPM$TTE_mod1_baseline),
                             new_data = SYNPM$ValidationData,
                             survival_time = "ETime",
                             event_indicator = "Status",
                             time_horizon = 5))
})
