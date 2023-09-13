test_that("Test range NHS number", {
  nhsnumber <- epidm:::gen_test_nhs(n = 1)
  expect_equal(range_nhs_number(nhsnumber), as.double(nhsnumber))
  expect_equal(range_nhs_number(stringr::str_flatten(as.character(replicate(10,9)))),
               NA_integer_)
  expect_equal(range_nhs_number("1"),
               NA_integer_)
  test_2 <- data.frame(NA)
  colnames(test_2)[1] <- "test"
  nhs_numbers <- data.frame(epidm:::gen_test_nhs(n = 10))
  colnames(nhs_numbers)[1] <- "test"
  nhs_numbers <- dplyr::union(nhs_numbers,test_2)
  expect_equal(dplyr::mutate(nhs_numbers,
                       test = range_nhs_number(test)),
                dplyr::mutate(nhs_numbers, test = as.double(test)))
  expect_equal(range_nhs_number(NA),
               NA)
})
