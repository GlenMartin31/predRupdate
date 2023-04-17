test_that("pred_input_info() and pred_predict() lead to correct predicted
          risk calculations for logistic model", {
            # Use package functions to input an existing model and make predictions
            test_info <- pred_input_info(model_type = "logistic",
                                         model_info = data.frame("Intercept" = -3.0893961710923,
                                                                 "Age" = 0.0230955938292795,
                                                                 "SexM" = 0.263578567485447,
                                                                 "Smoking_Status" = 0.689825139075564,
                                                                 "Diabetes" = 0.387810349702088,
                                                                 "Creatine" = 0.56129156010678))
            test_predictions <- pred_predict(x = test_info,
                                             new_data = SYNPM$ValidationData,
                                             binary_outcome = "Y")

            # Perform same task manually:
            test_mm <- model.matrix(~ Age + SexM + Smoking_Status + Diabetes + Creatine,
                                    SYNPM$ValidationData)
            input_coefs <- c("Intercept" = -3.0893961710923,
                             "Age" = 0.0230955938292795,
                             "SexM" = 0.263578567485447,
                             "Smoking_Status" = 0.689825139075564,
                             "Diabetes" = 0.387810349702088,
                             "Creatine" = 0.56129156010678)
            manual_LP <- as.vector(test_mm %*% input_coefs)
            manual_PR <- 1 / (1 + exp(-manual_LP))

            # Compare the outputs
            expect_true(identical(manual_LP,
                                  test_predictions$LinearPredictor))

            expect_true(identical(manual_PR,
                                  test_predictions$PredictedRisk))
          })
