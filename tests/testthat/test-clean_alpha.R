test_that("clean alpha", {
  expect_equal(clean_alpha('ABC-123def'), 'ABC-def')
})
