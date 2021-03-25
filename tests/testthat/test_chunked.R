library(rawutil)


# CONSTANTS ---------------------------------------------------------------

# columns to read from inst/extdata/dummy_ukb_data_.csv
selected_cols <- c("chr", "chr_rpt")


# GENERATE DUMMY DATA -----------------------------------------------------

# make dummy df and save to tempdir
test_df <- make_dummy_df()
test_df_file <- tempfile(pattern = "test_df",
                         fileext = ".csv")
write.csv(test_df,
          file = test_df_file,
          row.names = FALSE)

# TESTS -------------------------------------------------------------------

# fread_chunked() ---------------------------------------------------------

test_that("fread_chunked() reads all rows correctly", {
  # read test dataframe
  test_df_chunked <- fread_chunked(
    test_df_file,
    callback = function(df) df,
    chunk_size = 3,
    return_chunks = TRUE,
    colClasses = c("character", "integer", "factor", "logical", "character"),
    data.table = FALSE
  )

  # should be identical to test_df
  expect_equal(test_df_chunked, test_df)
})

# process_df_chunked() ----------------------------------------------------

test_that("process_df_chunked() returns the input df unaltered if callback function does not alter input df", {
  # process df in chunks, without actually making any changes
  test_df_chunked <- process_df_chunked(test_df,
                                        callback = function(df) df,
                                        chunk_size = 3)

  # output df should equal input df
  expect_equal(test_df_chunked, test_df)
})



