testthat::test_that("sql-upload test csv", {
    # Test internal options are not retained
    options(table_name = "test internal options not retained")
    test_df <- readr::read_csv(readr::readr_example("mtcars.csv"))
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbWriteTable(con, name = DBI::SQL("example_mtcars"), value = test_df,
                      overwrite = TRUE)
    expected <- DBI::dbReadTable(con, "example_mtcars")
    test_output <- suppressWarnings(
      sql_upload_csv_chunked(input_filename = readr::readr_example("mtcars.csv"),
                        con = con,
                        schema = NULL,
                        table_name = "test_mtcars",
                        chunk_size = 5
      )
    )
    test <- DBI::dbReadTable(con, "test_mtcars")
    DBI::dbDisconnect(con)
    # Test SQL output is as expected
    testthat::expect_equal(test, expected)
    # Test return is TRUE (invisibly returned)
    testthat::expect_true(test_output)
    # Test internal options are not retained
    testthat::expect_equal(getOption("table_name"), "test internal options not retained")
})
