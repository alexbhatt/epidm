test_that("Test NHS Number checksum", {
  nhsnumber <- epidm:::gen_test_nhs(n=1)
  nhsnumber_100 <- epidm:::gen_test_nhs(n=100)
  nhsnumber_2 <- c(nhsnumber_100, rep(c("test", "test123", NA), 5))
  expect_equal(checksum_nhs_number(nhsnumber), nhsnumber)
  expect_equal(checksum_nhs_number(nhsnumber_2), c(nhsnumber_100, rep(NA_character_, 15)))
  invalid_nhs_number <- 1234567890 # Not a real nhs number pls do not use real nhs numbers in testing, code or documentation
  expect_equal(checksum_nhs_number(invalid_nhs_number), NA_character_)
  expect_equal(checksum_nhs_number(stringr::str_flatten(replicate(5,12))), NA_character_)
  expect_equal(checksum_nhs_number(stringr::str_flatten(replicate(5,8))), NA_character_)
  expect_equal(checksum_nhs_number(NA), NA_character_)
})
