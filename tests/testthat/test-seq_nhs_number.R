test_that("seq nhs number", {
  expect_equal(seq_nhs_number(stringr::str_flatten(as.character(replicate(10,1)))),
               NA_character_)
  expect_equal(seq_nhs_number(stringr::str_flatten(as.character(replicate(10,5)))),
               NA_character_)
  expect_equal(seq_nhs_number(stringr::str_flatten(as.character(replicate(5,12)))),
               stringr::str_flatten(as.character(replicate(5,12))))
  expect_equal(seq_nhs_number(NA_integer_),
               NA_character_)
})
