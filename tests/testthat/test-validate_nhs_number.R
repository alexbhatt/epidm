testthat::test_that("Test valid NHS", {
  t <- rep(c("test", NA), 10)
  nhs_numbers <- nhsnumbergenerator::generate_nhs_number(n = 100)
  expect_equal(valid_nhs(c(nhs_numbers,t), TRUE), c(nhs_numbers, rep(NA_character_,20)))
  expect_equal(valid_nhs(c(nhs_numbers,t), FALSE), c(rep(1, 100), rep(0, 20)))
  nhs_numbers <- data.frame(test = nhs_numbers)
  # test output
  expect_equal(
    dplyr::mutate(nhs_numbers, test = valid_nhs(test, TRUE)), nhs_numbers)
  # invalid values
  expect_equal(valid_nhs(NA_character_, TRUE), NA_character_)
  expect_equal(valid_nhs(NA_character_, FALSE), 0)
  test_2 <- data.frame(test = c(NA, "test", ",", "", " ", "\t", NA_character_,
                                NA_complex_, NA_integer_, NA_real_, 12345, 64578,
                                5555555555, 9999999999))
  test_2 <- dplyr::union_all(nhs_numbers,test_2)
  test_3 <- dplyr::mutate(dplyr::union_all(nhs_numbers, data.frame(test = c(NA, NA, NA, NA, NA, NA, NA,
                                NA, NA, NA, NA, NA,NA, NA))),
                                test = as.character(test)
  )
   expect_equal(dplyr::mutate(test_2, test = valid_nhs(test, TRUE)), test_3)
   expect_equal(dplyr::mutate(test_2, test = valid_nhs(test, FALSE)),
                          dplyr::mutate(test_3, test = dplyr::case_when(is.na(test) ~ 0, TRUE ~ 1))
   )
   # valid_nhs
   expect_equal(dplyr::mutate(test_3, test2 = valid_nhs(test, FALSE)),
                dplyr::mutate(test_3, test2 = as.integer(epidm::valid_nhs(test)))
   )
})
