testthat::test_that("combine parquet files", {
  # Test to check combine parquet files combined only what is intended and only parquet files
  t <- tempdir(check = TRUE)
  t <- paste0(t, "\\")
  # Files to not be combined
  arrow::write_parquet(iris, paste0(t, "othertest.parquet"))
  write.table("test", paste0(t,"temptest.txt"))
  write.csv(data.frame(t = "test"), paste0(t,"temptest.csv"))
  write.table("test", paste0(t,"test.txt"))
  write.table("test", paste0(t,"temp.txt"))
  # Files to be combined
  arrow::write_parquet(iris, paste0(t, "temp.parquet"))
  arrow::write_parquet(iris, paste0(t, "1_temp.parquet"))
  arrow::write_parquet(iris, paste0(t, "2_temp.parquet"))
  # Getting expected result
  expect_1 <- list.files(t, pattern = "temp.parquet", full.names = TRUE)
  expect_1 <- expect_1[order(file.info(expect_1)$ctime, grep("[0-9]", expect_1))]
  # Test combine parquet files will not remove non parquet files
  res <- combine_parquet_files(
    path = t, pattern = "temp", backup_name = "test", remove_old = TRUE)
  expect_2 <- rbind(iris,iris,iris)
  act <- arrow::read_parquet(paste0(t, "test.parquet"))

  # Test what is in the list output is correct
  testthat::expect_equal(res, expect_1)
  # Test what is combined is correct
  testthat::expect_equal(act,expect_2)

  # Check everything else is still there using TRUE/ FALSE
  files_exists_expect_bool <- (file.exists(paste0(t, "temp.txt")) &
  file.exists(paste0(t, "test.txt")) &
  file.exists(paste0(t, "temptest.csv")) &
  file.exists(paste0(t, "temptest.txt")) &
  file.exists(paste0(t, "othertest.parquet")))

  testthat::expect_true(files_exists_expect_bool)
})
