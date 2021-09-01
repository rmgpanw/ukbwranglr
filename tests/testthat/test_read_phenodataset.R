

# SETUP -------------------------------------------------------------------
# get path for raw dummy_ukb data
dummy_ukb_data_filepath <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")

# get ukb data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# make data dictionary
data_dict <- make_data_dict(ukb_main = dummy_ukb_data_filepath,
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

test_that("`make_data_dict()` works", {
  expect_equal(names(data_dict)[1:4],
               c("descriptive_colnames", "colheaders_raw", "colheaders_processed", "FieldID"))
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


# `mutate_descriptive_columns()` ------------------------------------------

test_that(
  "`mutate_descriptive_columns()` works", {
    expect_equal(
      data_dict$descriptive_colnames,
      c(
        "eid",
        "sex_f31_0_0",
        "year_of_birth_f34_0_0",
        "month_of_birth_f52_0_0",
        "ethnic_background_f21000_0_0",
        "non_cancer_illness_code_self_reported_f20002_0_0",
        "body_mass_index_bmi_f21001_0_0"
      )
    )
  }
)

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

# `indicate_coltype_in_data_dict()` ---------------------------------------

test_that(
  "`indicate_coltype_in_data_dict()` returns the expected values", {
    data_dict_coltypes <- indicate_coltype_in_data_dict(data_dict = dplyr::bind_rows(data_dict,
                                                                                     data.frame(ValueType = c("something_else",
                                                                                                              "Date"))),
                                  ukb_codings = ukb_codings) %>%
      dplyr::group_by(.data[["ValueType"]]) %>%
      dplyr::slice(1L) %>%
      dplyr::select(tidyselect::all_of(c(
        "ValueType",
        "col_types_readr",
        "col_types_fread"
      )))

    expect_equal(data_dict_coltypes$ValueType,
                 c("Categorical multiple", "Categorical single", "Continuous", "Date", "Integer", "something_else"))

    expect_equal(data_dict_coltypes$col_types_readr,
                 c("i", "i", "d", "D", "i", "c"))

    expect_equal(data_dict_coltypes$col_types_fread,
                 c("integer", "integer", "double", "Date", "integer", "character"))
  }
)
