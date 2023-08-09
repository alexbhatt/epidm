test_that("Clean alpha numeric", {
  expect_equal(clean_alpha_numeric("123-A(*^%$£BC's"), "123-ABC's")
})
