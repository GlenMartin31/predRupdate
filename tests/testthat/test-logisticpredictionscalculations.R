test_that("pm_input_info() and pm_predict() lead to correct predicted
          risk calculations for logistic model", {
            # Use package functions to input an existing model and make predictions
            test_info <- pm_input_info(model_type = "logistic",
                                       existingcoefs = c("(Intercept)" = -6.6252554,
                                                         "Age" = 0.1136751,
                                                         "SexM" = 0.2304344,
                                                         "Smoking_Status" = 0.6873743,
                                                         "Diabetes" = 0.4306382),
                                       formula = formula(SYNPM$Existing_models$Formula[1]),
                                       newdata = SYNPM$ValidationData,
                                       pre_processing = list(function(df) {dummyvars(df)}),
                                       binary_outcome = "Y")
            test_predictions <- pm_predict(test_info)

            # Perform same task manually:
            test_mm <- model.matrix(~Age+Sex+Smoking_Status+Diabetes,
                                    SYNPM$ValidationData)
            input_coefs <- c("(Intercept)" = -6.6252554,
                             "Age" = 0.1136751,
                             "SexM" = 0.2304344,
                             "Smoking_Status" = 0.6873743,
                             "Diabetes" = 0.4306382)
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



