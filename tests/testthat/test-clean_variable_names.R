test_that("calls to cleaning variable function are triggered correctly", {
  pattern <- paste(c("\\[", "\\]",
                     "\\{", "\\}",
                     "\\(", "\\)",
                     "[^_^:[:^punct:]]",
                     "\\s+",
                     "\\h+"), collapse = "|")

  expect_equal(clean_variable_names(string_vector = " helloworld",
                                    pattern = pattern),
               "helloworld")

  expect_equal(clean_variable_names(string_vector = "helloworld ",
                                    pattern = pattern),
               "helloworld")

  expect_equal(clean_variable_names(string_vector = " helloworld ",
                                    pattern = pattern),
               "helloworld")

  expect_equal(clean_variable_names(string_vector = "hello world",
                                    pattern = pattern),
               "hello_world")

  expect_equal(clean_variable_names(string_vector = "hello(world",
                                    pattern = pattern),
               "hello_world")

  expect_equal(clean_variable_names(string_vector = "hello (world)",
                                    pattern = pattern),
               "hello__world_")

  expect_equal(clean_variable_names(string_vector = "hello:world",
                                    pattern = pattern),
               "hello:world")
})
