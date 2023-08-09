testthat::test_that("parquet-backup-csv", {
    test_df <- readr::read_csv(readr::readr_example("mtcars.csv"))
    t <- tempdir(check = TRUE)
    t1 <- paste0(t, "\\")
    arrow::write_parquet(test_df, paste0(t1, "test_csv.parquet"))
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    sql_upload_from_csv(readr::readr_example("mtcars.csv"),
                        con = con,
                        schema = NULL,
                        table_name = "test",
                        write_parquet = TRUE,
                        backup_filepath = t1,
                        backup_name = "test_output_csv",
                        pattern = "temp_file_parq",
                        file_remove = FALSE,
                        chunk_size = 5)
    DBI::dbDisconnect(con)
    testthat::expect_equal(arrow::read_parquet(paste0(t1, "test_output_csv.parquet")),
                           arrow::read_parquet(paste0(t1, "test_csv.parquet"))
                           )
    unlink(t, recursive = T, force = T)
})
