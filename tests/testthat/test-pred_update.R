test_that("pred_update() has correct outputs", {
  model1 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models[1,])

  expect_error(pred_update(x = "test non S£ calss",
                           update_type = "recalibration",
                           new_data = SYNPM$ValidationData,
                           binary_outcome = "Y"))

  expect_error(pred_update(x = model1,
                           update_type = "recalibration",
                           new_data = SYNPM$ValidationData))
  expect_error(pred_update(x = pred_input_info(model_type = "logistic",
                                               model_info = SYNPM$Existing_logistic_models),
                           update_type = "recalibration",
                           new_data = SYNPM$ValidationData,
                           binary_outcome = "Y"))

  intupdate_model1 <- pred_update(x = model1,
                                  update_type = "intercept_update",
                                  new_data = SYNPM$ValidationData,
                                  binary_outcome = "Y")
  recalibrated_model1 <- pred_update(x = model1,
                                     update_type = "recalibration",
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Y")
  refit_model1 <- pred_update(x = model1,
                              update_type = "refit",
                              new_data = SYNPM$ValidationData,
                              binary_outcome = "Y")

  expect_output(summary(refit_model1))

  expect_s3_class(intupdate_model1, c("predUpdate", "predinfo_logistic", "predinfo"))
  expect_s3_class(recalibrated_model1, c("predUpdate", "predinfo_logistic", "predinfo"))
  expect_s3_class(refit_model1, c("predUpdate", "predinfo_logistic", "predinfo"))

  expect_type(intupdate_model1, type = "list")
  expect_equal(names(intupdate_model1), c("M", "model_type", "coefs", "coef_names", "formula", "model_info", "model_update_results", "update_type"))
  expect_equal(intupdate_model1$update_type, "intercept_update")

  expect_type(recalibrated_model1, type = "list")
  expect_equal(names(recalibrated_model1), c("M", "model_type", "coefs", "coef_names", "formula", "model_info", "model_update_results", "update_type"))
  expect_equal(recalibrated_model1$update_type, "recalibration")

  expect_type(refit_model1, type = "list")
  expect_equal(names(refit_model1), c("M", "model_type", "coefs", "coef_names", "formula", "model_info", "model_update_results", "update_type"))
  expect_equal(refit_model1$update_type, "refit")



  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[1,],
                            cum_hazard = SYNPM$TTE_mod1_baseline)
  intupdate_model2 <- pred_update(x = model2,
                                  update_type = "intercept_update",
                                  new_data = SYNPM$ValidationData,
                                  survival_time = "ETime",
                                  event_indicator = "Status")
  recalibrated_model2 <- pred_update(x = model2,
                                     update_type = "recalibration",
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "ETime",
                                     event_indicator = "Status")
  refit_model2 <- pred_update(x = model2,
                              update_type = "refit",
                              new_data = SYNPM$ValidationData,
                              survival_time = "ETime",
                              event_indicator = "Status")

  expect_s3_class(intupdate_model2, c("predUpdate", "predinfo_survival", "predinfo"))
  expect_s3_class(recalibrated_model2, c("predUpdate", "predinfo_survival", "predinfo"))
  expect_s3_class(refit_model2, c("predUpdate", "predinfo_survival", "predinfo"))

  expect_type(intupdate_model2, type = "list")
  expect_equal(names(intupdate_model2), c("M", "model_type", "coefs", "coef_names", "formula", "cum_hazard", "model_info", "model_update_results", "update_type"))
  expect_equal(intupdate_model2$update_type, "intercept_update")

  expect_type(recalibrated_model2, type = "list")
  expect_equal(names(recalibrated_model2), c("M", "model_type", "coefs", "coef_names", "formula", "cum_hazard", "model_info", "model_update_results", "update_type"))
  expect_equal(recalibrated_model2$update_type, "recalibration")

  expect_type(refit_model2, type = "list")
  expect_equal(names(refit_model2), c("M", "model_type", "coefs", "coef_names", "formula", "cum_hazard", "model_info", "model_update_results", "update_type"))
  expect_equal(refit_model2$update_type, "refit")

})
