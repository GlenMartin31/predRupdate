test_that("apply_pre_processing() returns dataframe of correct dimention", {

  new_df <- synthetic_df_pmupdate$ValidationData

  #Test across different ways of specifying pre_processing: approach 1
  pre_proc_function <- function(df) {
    Age_Squared <- df$Age^2
    df_dummyvars <- dummyvars(df)
    return(list("Age_Squared" = Age_Squared,
                df_dummyvars))
  }
  pre_proc_result <- apply_pre_processing(newdata = new_df,
                                          pre_processing = list(function(df) {pre_proc_function(df)}))
  expect_s3_class(pre_proc_result, "data.frame")
  expect_equal(nrow(pre_proc_result), nrow(new_df))
  expect_equal(ncol(pre_proc_result), (ncol(new_df) + length(dummyvars(new_df)) + 1))


  #Test across different ways of specifying pre_processing: approach 2
  pre_proc_result <- apply_pre_processing(newdata = new_df,
                                          pre_processing = list("Age_squared" = function(df) df$Age^2,
                                                                function(df) dummyvars(df)))
  expect_s3_class(pre_proc_result, "data.frame")
  expect_equal(nrow(pre_proc_result), nrow(new_df))
  expect_equal(ncol(pre_proc_result), (ncol(new_df) + length(dummyvars(new_df)) + 1))


  #Test across different ways of specifying pre_processing: approach 3
  pre_proc_result <- apply_pre_processing(newdata = new_df,
                                          pre_processing = list(function(df) {
                                            df$Age_squared <- df$Age^2
                                            df <- cbind(df, dummyvars(df))
                                            return(df)
                                          }))
  expect_s3_class(pre_proc_result, "data.frame")
  expect_equal(nrow(pre_proc_result), nrow(new_df))
  expect_equal(ncol(pre_proc_result), (ncol(new_df) + length(dummyvars(new_df)) + 1))
})
