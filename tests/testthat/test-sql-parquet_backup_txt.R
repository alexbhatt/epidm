testthat::test_that("parquet-backup-txt", {
 test_df <- readr::read_delim(readr::readr_example("whitespace-sample.txt"))
 t <- tempdir(check = TRUE)
 t1 <- paste0(t, "\\")
 arrow::write_parquet(test_df, paste0(t1, "test_txt.parquet"))
 con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
 test_output <- sql_upload_txt_chunked(readr::readr_example("whitespace-sample.txt"),
                     con = con,
                     schema = NULL,
                     table_name = "test",
                     write_parquet = TRUE,
                     backup_filepath = t1,
                     backup_name = "test_output_txt",
                     pattern = "temp_file_parq",
                     file_remove = TRUE,
                     chunk_size = 1,
                     delim = " ")
 DBI::dbDisconnect(con)
 testthat::expect_equal(arrow::read_parquet(paste0(t1, "test_output_txt.parquet")),
                        arrow::read_parquet(paste0(t1, "test_txt.parquet"))
 )
 unlink(t, recursive = T, force = T)
 testthat::expect_true(test_output)
})
