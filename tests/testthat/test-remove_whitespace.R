test_that("remove whitespace", {
  expect_equal(epidm::remove_whitespace("\t\n   "), "")
  test <- readr::read_delim(readr::readr_example("whitespace-sample.txt"), show_col_types = FALSE)
  test_1 <- dplyr::mutate(test, phone = gsub("-", " \t\n", phone))
  expect_equal(dplyr::mutate(test, phone = epidm::remove_whitespace(phone)),
               test
               )
})
