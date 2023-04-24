test_that("test error messages on checks of map_newdata(),
          logistic input", {

            expect_error(map_newdata(x = data.frame("test S3"),
                                     new_data = SYNPM$ValidationData[1:10,],
                                     binary_outcome = "Y"))

            model1 <- pred_input_info(model_type = "logistic",
                                      model_info = SYNPM$Existing_logistic_models[1,])

            #newdata is not a data.frame
            expect_error(map_newdata(x = model1,
                                     new_data = list(SYNPM$ValidationData),
                                     binary_outcome = "Y"))

            #newdata contains factor variables
            expect_error(map_newdata(x = model1,
                                     new_data = data.frame(SYNPM$ValidationData,
                                                           "X" = factor(sample(c("a", "c", "d"),
                                                                               size = nrow(SYNPM$ValidationData),
                                                                               replace = TRUE))),
                                     binary_outcome = "Y"))

            #newdata contains character var
            expect_warning(map_newdata(x = model1,
                                       new_data = data.frame(SYNPM$ValidationData,
                                                             "X" = (sample(c("a", "c", "d"),
                                                                                 size = nrow(SYNPM$ValidationData),
                                                                                 replace = TRUE))),
                                       binary_outcome = "Y"))

            #specify survival_time for logistic input
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Y",
                                     survival_time = "ETime"))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Y",
                                     event_indicator = "Status"))

            #incorrect specification of binary outcome variable
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = factor("Y")))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = 3))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = c("Y", "Age")))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Outcome"))

            #not all predictor variables in newdata:
            coefs_table <- data.frame("Intercept" = -3.4,
                                      "Sex_M" = 0.306,
                                      "Test_Error" = 0.628)
            existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                                       model_info = coefs_table)
            expect_error(map_newdata(x = existing_Logistic_Model,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Y"))

            #intercept not first column:
            coefs_table <- data.frame( "SexM" = 0.306,
                                       "Intercept" = -3.4)
            expect_error(pred_input_info(model_type = "logistic",
                                         model_info = coefs_table))


})


test_that("test error messages on checks of map_newdata(),
          survival input", {
            expect_error(map_newdata(x = data.frame("test S3"),
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "ETime",
                                     event_indicator = "Status"))

            model1 <- pred_input_info(model_type = "survival",
                                      model_info = SYNPM$Existing_TTE_models[1,])

            #newdata is not a data.frame
            expect_error(map_newdata(x = model1,
                                     new_data = list(SYNPM$ValidationData),
                                     survival_time = "ETime",
                                     event_indicator = "Status"))

            #newdata contains factor variables
            expect_error(map_newdata(x = model1,
                                     new_data = data.frame(SYNPM$ValidationData,
                                                           "X" = factor(sample(c("a", "c", "d"),
                                                                               size = nrow(SYNPM$ValidationData),
                                                                               replace = TRUE))),
                                     survival_time = "ETime",
                                     event_indicator = "Status"))

            #newdata contains character var
            expect_warning(map_newdata(x = model1,
                                       new_data = data.frame(SYNPM$ValidationData,
                                                             "X" = (sample(c("a", "c", "d"),
                                                                           size = nrow(SYNPM$ValidationData),
                                                                           replace = TRUE))),
                                       survival_time = "ETime",
                                       event_indicator = "Status"))

            #specify binary_outcome for survival input
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     binary_outcome = "Y",
                                     survival_time = "ETime",
                                     event_indicator = "Status"))

            #incorrect specification of time-to-event outcome variables
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "ETime",#should be both NULL or supplied
                                     event_indicator = NULL))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = NULL, #should be both NULL or supplied
                                     event_indicator = "Status"))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = factor("ETime"),#should be character var
                                     event_indicator = "Status"))
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "ETime",
                                     event_indicator = c("Test", "Status"))) #should be length 1
            expect_error(map_newdata(x = model1,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "T", #not found in newdata
                                     event_indicator = "Status"))

            #not all predictor variables in newdata:
            coefs_table <- data.frame("Intercept" = -3.4,
                                      "Sex_M" = 0.306,
                                      "Test_Error" = 0.628)
            existing_surv_Model <- pred_input_info(model_type = "survival",
                                                   model_info = coefs_table)
            expect_error(map_newdata(x = existing_surv_Model,
                                     new_data = SYNPM$ValidationData,
                                     survival_time = "ETime",
                                     event_indicator = "Status"))

          })


test_that("test output format of map_newdata()", {
  model1 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[1,])
  mapped_data <- map_newdata(x = model1,
                             new_data = SYNPM$ValidationData,
                             survival_time = "ETime",
                             event_indicator = "Status")
  expect_type(mapped_data, type = "list")
  expect_equal(length(mapped_data), 3)
  expect_equal(names(mapped_data), c("modelinfo", "PredictionData", "Outcomes"))


  model2 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models[1,])
  mapped_data <- map_newdata(x = model2,
                             new_data = SYNPM$ValidationData,
                             binary_outcome = "Y")
  expect_type(mapped_data, type = "list")
  expect_equal(length(mapped_data), 3)
  expect_equal(names(mapped_data), c("modelinfo", "PredictionData", "Outcomes"))
})
