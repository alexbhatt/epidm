testthat::test_that("sql-upload test txt", {
    # Test internal options are not retained
    options(table_name = "test internal options not retained")
    test_df <- readr::read_delim(readr::readr_example("whitespace-sample.txt"))
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(con, name = DBI::SQL("example_sample"), value = test_df,
                      overwrite = TRUE)
    expected <- DBI::dbReadTable(con, "example_sample")
    test_output <- suppressWarnings(
      epidm::sql_upload_txt_chunked(input_filename = readr::readr_example("whitespace-sample.txt"),
                        con = con,
                        schema = NULL,
                        table_name = "test_sample",
                        chunk_size = 1,
                        delim = " "
      )
    )
    test <- DBI::dbReadTable(con, "test_sample")
    DBI::dbDisconnect(con)
    # Test SQL output is as expected
    testthat::expect_equal(test, expected)
    # Test return is TRUE (invisibly returned)
    testthat::expect_true(test_output)
    # Test internal options are not retained
    testthat::expect_equal(getOption("table_name"), "test internal options not retained")
})
