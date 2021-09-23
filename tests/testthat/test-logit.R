test_that("logit() and inv_logit apply the correct transformations", {
  expect_equal(inv_logit(logit(0.2)), 0.2)
  expect_equal(inv_logit(0), 0.5)
  expect_equal(logit(0.5), 0)
  expect_error(logit(1.01), "p not between 0 and 1")
  expect_error(logit(-0.01), "p not between 0 and 1")
})
