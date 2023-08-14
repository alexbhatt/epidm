testthat::test_that("sql-upload test txt", {
    test_df <- readr::read_delim(readr::readr_example("whitespace-sample.txt"))
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(con, name = DBI::SQL("example_sample"), value = test_df,
                      overwrite = TRUE)
    expected <- DBI::dbReadTable(con, "example_sample")
    test_output <- epidm::sql_upload_txt_chunked(input_filename = readr::readr_example("whitespace-sample.txt"),
                        con = con,
                        schema = NULL,
                        table_name = "test_sample",
                        chunk_size = 1,
                        delim = " "
    )
    test <- DBI::dbReadTable(con, "test_sample")
    DBI::dbDisconnect(con)
    testthat::expect_equal(test, expected)
    testthat::expect_true(test_output)
})
