test_that("mapping of coefs post SR works", {
  test_info <- pred_input_info(model_type = "logistic",
                               model_info = SYNPM$Existing_models,
                               newdata = SYNPM$ValidationData,
                               binary_outcome = "Y")

  test_SR <- pred_stacked_regression(test_info)

  SR_dat <- do.call(cbind.data.frame, lapply(predRupdate::pred_predict(test_info),
                                             function(X) X[names(X)=="LinearPredictor"]))
  names(SR_dat) <- c(paste("LP", 1:test_info$M, sep = ""))

  expect_identical(round(as.numeric(predict(test_SR$Stacked_Regression_Model,
                                            newdata = SR_dat,
                                            type = "link")), 12)
                   ,
                   round(as.numeric(test_SR$Aggregate_Coefs["Intercept"] +
                                      (test_info$PredictionData[,"Age"] * test_SR$Aggregate_Coefs["Age"]) +
                                      (test_info$PredictionData[,"SexM"] * test_SR$Aggregate_Coefs["SexM"]) +
                                      (test_info$PredictionData[,"Diabetes"] * test_SR$Aggregate_Coefs["Diabetes"]) +
                                      (test_info$PredictionData[,"Smoking_Status"] * test_SR$Aggregate_Coefs["Smoking_Status"]) +
                                      (test_info$PredictionData[,"CKD"] * test_SR$Aggregate_Coefs["CKD"])),
                         12))
})
