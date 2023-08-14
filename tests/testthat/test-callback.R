test_that("Callback Test", {
  x <- iris
  x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  t <- tempdir(check = TRUE)
  t1 <- paste0(t, "\\")
  options(
    "con" = con,
    "schema" = NULL,
    "table_name" = "test",
    "backup_filepath" = t1,
    "pattern" = "test_backup_string",
    "backup_name" = "backup_test",
    "write_parquet" = TRUE
  )

  expect_true(epidm::callback(x,1))
  colnames(x) <- gsub("[^[:alnum:]]", "_", colnames(x))
  expect_equal(dplyr::as_tibble(DBI::dbReadTable(con, "test")), dplyr::as_tibble(x))
  DBI::dbDisconnect(con)
  pos <- 1
  expect_equal(arrow::read_parquet(glue::glue(getOption("backup_filepath"),
                                          "{pos}_",
                                          getOption("backup_name"),
                                          getOption("pattern"),
                                          ".parquet")
  ),
  x)
})
