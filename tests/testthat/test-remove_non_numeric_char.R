test_that("remove non numeric char", {
  expect_equal(remove_non_numeric_char("A1-_*"),"1")
  expect_equal(remove_non_numeric_char(NA),NA_character_)
})
