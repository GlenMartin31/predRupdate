test_that("pred_input_info() and pred_predict() lead to correct predicted
          risk calculations for logistic model", {
            # Use package functions to input an existing model and make predictions
            test_info <- pred_input_info(model_type = "logistic",
                                         existingcoefs = c("(Intercept)" = -3.0893961710923,
                                                           "Age" = 0.0230955938292795,
                                                           "SexM" = 0.263578567485447,
                                                           "Smoking_Status" = 0.689825139075564,
                                                           "Diabetes" = 0.387810349702088,
                                                           "CKD" = 0.56129156010678),
                                         formula = formula(SYNPM$Existing_models$Formula[2]),
                                         newdata = SYNPM$ValidationData,
                                         pre_processing = list(function(df) {dummyvars(df)}),
                                         binary_outcome = "Y")
            test_predictions <- pred_predict(test_info)

            # Perform same task manually:
            test_mm <- model.matrix(~ Age + Sex + Smoking_Status + Diabetes + CKD,
                                    SYNPM$ValidationData)
            input_coefs <- c("(Intercept)" = -3.0893961710923,
                             "Age" = 0.0230955938292795,
                             "SexM" = 0.263578567485447,
                             "Smoking_Status" = 0.689825139075564,
                             "Diabetes" = 0.387810349702088,
                             "CKD" = 0.56129156010678)
            manual_LP <- as.vector(test_mm %*% input_coefs)
            manual_PR <- exp(manual_LP) / (1 + exp(manual_LP))

            # Compare the outputs
            expect_true(identical(test_mm,
                                  test_info$PredictionData))


            expect_true(identical(manual_LP,
                                  test_predictions$LinearPredictor))

            expect_true(identical(manual_PR,
                                  test_predictions$PredictedRisk))
          })
