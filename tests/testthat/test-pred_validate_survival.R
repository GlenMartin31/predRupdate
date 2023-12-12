test_that("output of pred_validate is as expected - single models", {

  model1 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models[2,],
                            cum_hazard = SYNPM$TTE_mod2_baseline)
  val_results<- pred_validate(x = model1,
                              new_data = SYNPM$ValidationData,
                              survival_time = "ETime",
                              event_indicator = "Status",
                              time_horizon = 5)
  expect_s3_class(val_results, c("predvalidate_survival", "predvalidate"))
  expect_type(val_results, type = "list")
  expect_equal(names(val_results),
               c("OE_ratio", "OE_ratio_SE", "CalSlope", "CalSlope_SE", "harrell_C",
                 "harrell_C_SE", "PR_dist", "flex_calibrationplot", "M"))

  expect_no_error(print(val_results))
  expect_no_error(plot(val_results))
  expect_snapshot(summary(val_results))
})

test_that("output of pred_validate is as expected - multiple models", {

  model2 <- pred_input_info(model_type = "survival",
                            model_info = SYNPM$Existing_TTE_models,
                            cum_hazard = list(SYNPM$TTE_mod1_baseline,
                                              SYNPM$TTE_mod2_baseline,
                                              SYNPM$TTE_mod3_baseline))
  val_results<- pred_validate(x = model2,
                              new_data = SYNPM$ValidationData,
                              survival_time = "ETime",
                              event_indicator = "Status",
                              time_horizon = 5,
                              cal_plot = FALSE)

  expect_no_error(print(val_results))
  expect_no_error(summary(val_results))
  expect_no_error(plot(val_results))

  expect_type(val_results, type = "list")
  expect_equal(length(val_results), model2$M + 1)

  expect_s3_class(val_results, c("predvalidate_survival", "predvalidate"))

  for(m in 1:model2$M) {
    expect_type(val_results[[m]], type = "list")
    expect_equal(names(val_results[[m]]),
                 c("OE_ratio", "OE_ratio_SE", "CalSlope", "CalSlope_SE", "harrell_C",
                   "harrell_C_SE", "PR_dist", "flex_calibrationplot"))
  }
})
