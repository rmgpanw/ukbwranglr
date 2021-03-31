

# SETUP -------------------------------------------------------------------
# get path for raw dummy_ukb data
dummy_ukb_data_filepath <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")

# get ukb data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# make data dictionary
data_dict <- make_data_dict(ukb_pheno = dummy_ukb_data_filepath,
               delim = ",",
               ukb_data_dict = ukb_data_dict)

# read dummy data with `read_pheno()`
dummy_ukb_read_pheno <-  read_pheno(path = dummy_ukb_data_filepath,
                                    delim = ",",
                                    data_dict = data_dict,
                                    ukb_data_dict = ukb_data_dict,
                                    ukb_codings = ukb_codings,
                                    clean_dates = FALSE,
                                    clean_selected_continuous_and_integers = FALSE,
                                    data.table = FALSE)

# TESTS -------------------------------------------------------------------


# `make_data_dict()` ------------------------------------------------------

# TODO make better tests
test_that("`make_data_dict()` works", {
  expect_equal(names(data_dict)[1:5],
               c("descriptive_colnames", "Field_FieldID", "colheaders_raw", "colheaders_processed", "FieldID"))
})

# `read_pheno()` ----------------------------------------------------------

test_that("`read_pheno()` correctly reads a file", {
  expect_equal(names(dummy_ukb_read_pheno),
               data_dict$descriptive_colnames)

  expect_equal(dummy_ukb_read_pheno$year_of_birth_f34_0_0[1:6],
               c(1952, 1946, 1951, 1956, NA, 1948))

  # convert from factor to character for simpler testing
  expect_equal(as.character(dummy_ukb_read_pheno$ethnic_background_f21000_0_0[1:6]),
               c(NA, "Caribbean", "Asian or Asian British", NA, "Prefer not to answer", "Asian or Asian British"))
})

# `format_ukb_df_header()` ------------------------------------------------

test_that(
  "`format_ukb_df_header()` reformats .dta style raw ukb column names", {
    raw_headers <- c("n_eid", "n_3_0_0", "ts_40000_0_0")

    expect_equal(format_ukb_df_header(raw_headers),
                 c("f.eid", "f.3.0.0", "f.40000.0.0"))
  }
)

test_that(
  "`format_ukb_df_header()` reformats .txt style raw ukb column names", {
    raw_headers <- c("eid", "20002-0.0")

    expect_equal(format_ukb_df_header(raw_headers),
                 c("f.eid", "f.20002.0.0"))
  }
)

test_that(
  "`format_ukb_df_header()` does NOT reformat .tab (R) style raw ukb column names", {
    raw_headers <- c("f.eid", "f.20002.0.0")

    expect_equal(format_ukb_df_header(raw_headers),
                 raw_headers)
  }
)
