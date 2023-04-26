test_that("various error messages for pred_info_info internal checks", {

  #check model_info data.frame
  expect_error(pred_input_info_input_checks(model_type = "logistic",
                                            model_info = "SYNPM$Existing_logistic_models[1,]",
                                            cum_hazard = NULL))

  #check all cells of model_info are numeric
  expect_error(pred_input_info_input_checks(model_type = "logistic",
                                            model_info = data.frame("Intercept" = c("1"),
                                                                    "SexM" = c(0.3)),
                                            cum_hazard = NULL))
  expect_error(pred_input_info_input_checks(model_type = "logistic",
                                            model_info = data.frame("Intercept" = c(1,2,3),
                                                                    "SexM" = c("0.3", "0.2", "0.1")),
                                            cum_hazard = NULL))

  #check error if cum_hazard is not null for logistic
  expect_error(pred_input_info_input_checks(model_type = "logistic",
                                            model_info = SYNPM$Existing_logistic_models[1,],
                                            cum_hazard = SYNPM$TTE_mod1_baseline))

  #check error of cum_hazard being passed as list for multiple survival models
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = (SYNPM$TTE_mod1_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                              SYNPM$TTE_mod2_baseline)))


  #check error for incorrect specification of cum_hazard
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame(SYNPM$TTE_mod1_baseline,
                                                                    "addedcolumn" = NA)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame(SYNPM$TTE_mod1_baseline$hazard,
                                                                    SYNPM$TTE_mod1_baseline$time)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame("time" = c(1,1,2,3), #non-unique times
                                                                    "hazard" = c(0.001, 0.001, 0.002, 0.003))))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame("time" = c(-1,1,2,3), #negative times
                                                                    "hazard" = c(0, 0.001, 0.002, 0.003))))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame("time" = c(0, 1,2,3), #zero time
                                                                    "hazard" = c(0, 0.001, 0.002, 0.003))))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models[1,],
                                            cum_hazard = data.frame("time" = c(1,2,3), #negative hazard
                                                                    "hazard" = c(-0.001, 0.002, 0.003))))


  #check error for incorrect specification of cum_hazard - multiple models
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(data.frame(SYNPM$TTE_mod1_baseline,
                                                                         "addedcolumn" = NA),
                                                              SYNPM$TTE_mod2_baseline,
                                                              SYNPM$TTE_mod3_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(data.frame(SYNPM$TTE_mod1_baseline$hazard,
                                                                         SYNPM$TTE_mod1_baseline$time),
                                                              SYNPM$TTE_mod2_baseline,
                                                              SYNPM$TTE_mod3_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                              data.frame("time" = c(1,1,2,3), #non-unique times
                                                                         "hazard" = c(0.001, 0.001, 0.002, 0.003)),
                                                              SYNPM$TTE_mod3_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                              data.frame("time" = c(-1,1,2,3), #negative times
                                                                         "hazard" = c(0.001, 0.001, 0.002, 0.003)),
                                                              SYNPM$TTE_mod3_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                              data.frame("time" = c(0,1,2,3), #zero time
                                                                         "hazard" = c(0.001, 0.001, 0.002, 0.003)),
                                                              SYNPM$TTE_mod3_baseline)))
  expect_error(pred_input_info_input_checks(model_type = "survival",
                                            model_info = SYNPM$Existing_TTE_models,
                                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                                              data.frame("time" = c(1,2,3), #negative hazard
                                                                         "hazard" = c(-0.001, 0.002, 0.003)),
                                                              SYNPM$TTE_mod3_baseline)))
})
