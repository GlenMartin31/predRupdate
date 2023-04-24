test_that("output of pred_validate is as expected - single models", {

  expect_error(pred_validate(x = "test S3 class",
                             new_data = SYNPM$ValidationData,
                             binary_outcome = "Y"))

  model1 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models[1,])
  expect_error(pred_validate(x = model1,
                             new_data = SYNPM$ValidationData)) #no outcome given

  val_results <- pred_validate(x = model1,
                               new_data = SYNPM$ValidationData,
                               binary_outcome = "Y")
  expect_s3_class(val_results, c("predvalidate_logistic", "predvalidate"))
  expect_type(val_results, type = "list")
  expect_equal(names(val_results),
               c("CITL", "CITL_SE", "CalSlope", "CalSlope_SE", "AUC",
                 "AUC_SE", "R2_CoxSnell", "R2_Nagelkerke", "BrierScore"))

  expect_snapshot(print(val_results))

})



test_that("output of pred_validate is as expected - multiple models", {

  model2 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models)
  val_results <- pred_validate(x = model2,
                               new_data = SYNPM$ValidationData,
                               binary_outcome = "Y",
                               cal_plot = FALSE)

  expect_type(val_results, type = "list")
  expect_equal(length(val_results), model2$M)

  expect_s3_class(val_results[[1]], c("predvalidate_logistic", "predvalidate"))
  expect_type(val_results[[1]], type = "list")
  expect_equal(names(val_results[[1]]),
               c("CITL", "CITL_SE", "CalSlope", "CalSlope_SE", "AUC",
                 "AUC_SE", "R2_CoxSnell", "R2_Nagelkerke", "BrierScore"))
})
