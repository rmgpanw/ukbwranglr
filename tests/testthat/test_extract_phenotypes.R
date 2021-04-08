
# SETUP -------------------------------------------------------------------

# GENERATE DUMMY DATA -----------------------------------------------------

# TODO make function to generate dummy clinical events df. Write tests for
# extract_first_diagnostic_code_record (redo to have arg specifying
# earliest/latest date)
# Look at TODO email re ukb_explore

# Functions ---------------------------------------------------------------


# Make dummy data ---------------------------------------------------------

# get ukb data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# get dummy ukb data (contains all diagnoses columns) and load single eid for testing
dummy_ukb_data_path <- download_dummy_ukb_data_to_tempdir()
dummy_ukb_data_dict <- make_data_dict(ukb_pheno = dummy_ukb_data_path,
                                      delim = ",",
                                      ukb_data_dict = ukb_data_dict)
dummy_ukb_data_3eid <- read_pheno(path = dummy_ukb_data_path,
                             delim = ",",
                             data_dict = dummy_ukb_data_dict,
                             ukb_data_dict = ukb_data_dict,
                             ukb_codings = ukb_codings,
                             clean_dates = FALSE,
                             clean_selected_continuous_and_integers = FALSE,
                             nrows = 3)

dummy_ukb_data_1eid <- dummy_ukb_data_3eid[1, ]
dummy_ukb_data_2eid <- dummy_ukb_data_3eid[c(1, 2), ]

# test all `get_XXX_diagnoses()` functions at once (by default, should include all of these)
dummy_ukb_data_all_diagnoses <-
  get_all_diagnostic_codes_multi(ukb_pheno = dummy_ukb_data_1eid,
                                 data_dict = dummy_ukb_data_dict,
                                 ukb_codings = ukb_codings)

# dummy clinical_events_df
dummy_clinical_events <- make_dummy_clinical_events_df(eids = c(1, 2, 3),
                                                       n_rows = c(200, 200, 200))

# dummy_clinical_events_df as sqlite db
# Create an ephemeral in-memory RSQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# copy dummy_clinical_events_df to db
dplyr::copy_to(con,
        dummy_clinical_events %>%
          dplyr::mutate(date = as.character(date)),
        "dummy_clinical_events",
        overwrite = TRUE)

# get tbl_sql object
dummy_clinical_events_db <- dplyr::tbl(con, "dummy_clinical_events")

# should match output from `generate_self_reported_diabetes_codes_df()`
dummy_clinical_codes_df <- tibble::tribble(
  ~ disease, ~ description, ~ category, ~ code_type, ~ code, ~ phenotype_source,
  "Diabetes", "diabetes", "Diabetes unspecified", "icd10", "D", "test",
  "Diabetes", "gestational diabetes", "Gestational diabetes", "icd10", "E", "test",
  "Diabetes", "type 1 diabetes", "Type 1 diabetes", "read3", "H", "test",
  "Diabetes", "type 1 diabetes", "Type 1 diabetes", "read3", "G", "test",
  "Diabetes", "type 2 diabetes", "Type 2 diabetes", "data_coding_6", "D", "test",
)

# Process dummy data (and record expected results) ------------------------

# ***`extract_single_diagnostic_code_record_basis()` -------------------------

# Note: I manually checked this to produce 'expected' below
min_dates <- extract_single_diagnostic_code_record_basis(df = dummy_clinical_events,
                                                         codes = c("A", "B"),
                                                         mapping_function = extract_first_or_last_record_mapper,
                                                         min_max = "min")

min_dates_db <- extract_single_diagnostic_code_record_basis(df = dummy_clinical_events_db,
                                                         codes = c("A", "B"),
                                                         mapping_function = extract_first_or_last_record_mapper,
                                                         min_max = "min")

# min_dates: expected result (- nested `data` col)
expected <- tibble::tribble(
  ~ eid, ~ source, ~ code, ~ date,
  1, "f20001", "A", as.Date("2000-01-03"),
  2, "f20001", "B", as.Date("2000-01-02"),
  3, "gpc_r2", "B", as.Date("2000-01-02"),
)

# TESTS -------------------------------------------------------------------


# `extract_first_or_last_clinical_event_multi()` --------------------------

test_that(
  "`extract_first_or_last_clinical_event_multi()` returns the expected column names", {
    result <- extract_first_or_last_clinical_event_multi(df = dummy_clinical_events,
                                               clinical_codes_df = dummy_clinical_codes_df,
                                               min_max = "min",
                                               prefix = "testy_")

    expect_equal(
      names(result),
      c(
        "eid",
        "testy_diabetes_unspecified_test_min_date",
        "testy_diabetes_unspecified_test_indicator",
        "testy_gestational_diabetes_test_min_date",
        "testy_gestational_diabetes_test_indicator",
        "testy_type_1_diabetes_test_min_date",
        "testy_type_1_diabetes_test_indicator",
        "testy_type_2_diabetes_test_min_date",
        "testy_type_2_diabetes_test_indicator"
        )
    )
  }
)

# `mutate_age_at_event_cols()` --------------------------------------------

test_that(
  "`mutate_age_at_event_cols()` creates the expected age-at-event columns", {
    dummy_ukb_pheno <- tibble::tribble(
      ~ eid, ~ dob, ~ event_date,
      1, as.Date("2000-01-01"), as.Date("2010-01-01")
    )

    result <- mutate_age_at_event_cols(dummy_ukb_pheno,
                                       dob_col = "dob",
                                       date_col_regex = "_date$",
                                       date_col_regex_replacement = "_age")

    expect_equal(
      names(result[4]),
      "event_age"
    )

    expect_equal(
      as.integer(result$event_age),
      10
    )
  }
)

# `extract_single_diagnostic_code_record_basis()` -------------------------

test_that(
  "`extract_single_diagnostic_code_record_basis()` extracts the correct earliest and latest dates - all sources and class(df) = dataframe",
  {
    # expectations
    expect_equal(min_dates$eid, expected$eid)
    expect_equal(min_dates$source, expected$source)
    expect_equal(min_dates$code, expected$code)
    expect_equal(min_dates$date, expected$date)
  }
)

test_that(
  "`extract_single_diagnostic_code_record_basis()` extracts the correct earliest and latest dates - all sources and class(df) = tbl_sql",
  {
    # expectations
    expect_equal(min_dates_db$eid, expected$eid)
    expect_equal(min_dates_db$source, expected$source)
    expect_equal(min_dates_db$code, expected$code)
    expect_equal(min_dates_db$date, expected$date)
  }
)


# `filter_clinical_events_for_codes()` ------------------------------------

test_that(
  "`filter_clinical_events_for_codes()` (helper function for `extract_single_diagnostic_code_record_basis()`) returns the expected number of rows",
  {
    expect_equal(
      nrow(filter_clinical_events_for_codes(df = dummy_clinical_events,
                                       codes = list("read2" = "A"))),
      4
    )

    expect_equal(
      nrow(filter_clinical_events_for_codes(df = dummy_clinical_events_db,
                                            codes = list("read2" = "A"))),
      4
    )
  }
)

# `filter_clinical_events_for_list_of_codes()` ----------------------------

test_that(
  "`filter_clinical_events_for_list_of_codes()` (helper function for `extract_single_diagnostic_code_record_basis()` returns the expected 'sources' for each data coding type",
  {
    # read2
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                     df_class = "df",
                                                     codes = list("read2" = "A"))$source)),
      expected = sort(get_sources_for_code_type("read2")))

    # read3
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                           df_class = "df",
                                                           codes = list("read3" = "A"))$source)),
      expected = sort(get_sources_for_code_type("read3")))

    # icd9
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                           df_class = "df",
                                                           codes = list("icd9" = "A"))$source)),
      expected = sort(get_sources_for_code_type("icd9")))

    # icd10
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                           df_class = "df",
                                                           codes = list("icd10" = "A"))$source)),
      expected = sort(get_sources_for_code_type("icd10")))

    # data_coding_6
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                           df_class = "df",
                                                           codes = list("data_coding_6" = "A"))$source)),
      expected = sort(get_sources_for_code_type("data_coding_6")))

    # data_coding_3
    expect_equal(
      sort(unique(filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                           df_class = "df",
                                                           codes = list("data_coding_3" = "A"))$source)),
      expected = sort(get_sources_for_code_type("data_coding_3")))
  }
)

test_that(
  "`filter_clinical_events_for_list_of_codes()` (helper function for `extract_single_diagnostic_code_record_basis()` returns the expected codes",
  {
    # result
    result <- filter_clinical_events_for_list_of_codes(dummy_clinical_events,
                                                       df_class = "df",
                                                       list(read2 = "A",
                                                            icd10 = "B"))

    result_db <- filter_clinical_events_for_list_of_codes(dummy_clinical_events_db,
                                                       df_class = "tbl_sql",
                                                       list(read2 = "A",
                                                            icd10 = "B"))

    # expected - ***AMEND IF `ukbwranglr:::clinical_events_sources` CHANGES***
    # the filter by source bits
    expected_result <- dummy_clinical_events %>%
      dplyr::filter(
        (.data[["source"]] %in% c("gpc_r2") & .data[["code"]] %in% "A") |
        (.data[["source"]] %in% c("f40001", "f40002", "f20002_icd10", "f40006", "f41270") & .data[["code"]] %in% "B")
        )

    # eid 1
    expect_equal(
      result[result[["eid"]] == "1", ],
      expected_result[expected_result[["eid"]] == "1", ]
      )

    expect_equal(
      result_db[result_db[["eid"]] == "1", ],
      expected_result[expected_result[["eid"]] == "1", ]
    )

    # eid 2
    expect_equal(
      result[result[["eid"]] == "2", ],
      expected_result[expected_result[["eid"]] == "2", ]
    )

    expect_equal(
      result_db[result_db[["eid"]] == "2", ],
      expected_result[expected_result[["eid"]] == "2", ]
    )

    # eid 3
    expect_equal(
      result[result[["eid"]] == "3", ],
      expected_result[expected_result[["eid"]] == "3", ]
    )

    expect_equal(
      result_db[result_db[["eid"]] == "3", ],
      expected_result[expected_result[["eid"]] == "3", ]
    )
  }
)

# `get_diagnoses_set_index_code_date_cols()` ------------------------------

test_that("`get_diagnoses_set_index_code_date_cols()` removes special dates (data coding 13, field IDs 20006 and 20008: interpolated year of diagnosis for self-reported cancer/non-cancer illnesses)", {

  self_report_non_cancer_diagnoses_unstandardised <- tibble::tribble(
    ~ eid, ~ f20002, ~ f20002_value, ~ instance_array, ~ f20008, ~ f20008_value,
    "1", "noncancer_illness_code_selfreported_f20002_0_0", "1665", "0_0", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_0", 1998.1,
    "1", "noncancer_illness_code_selfreported_f20002_0_1", "1532", "0_1", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_1", -1,
    "1", "noncancer_illness_code_selfreported_f20002_0_2", "1552", "0_2", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_2", -3,
  )

  self_report_non_cancer_diagnoses_standardised <-
    get_diagnoses_set_index_code_date_cols(
      get_clinical_events_df = self_report_non_cancer_diagnoses_unstandardised,
      index_col = "f20002",
      code_col = "f20002_value",
      date_col = "f20008_value",
      remove_special_dates = TRUE
    )

  expect_equal(self_report_non_cancer_diagnoses_standardised$date,
               c(1998.1, NA, NA))
})


# `get_all_diagnostic_codes_multi()` --------------------------------------

test_that(
  "output from `get_all_diagnostic_codes_multi()` contains all expected source types", {
    # output from `get_all_diagnostic_codes_multi()` won't include primary care data
    expected_sources <- subset(ukbwranglr:::clinical_events_sources$source,
                               !ukbwranglr:::clinical_events_sources$source %in% c("gpc_r2",
                                                                                   "gpc_r3"))

    expect_equal(sort(unique(dummy_ukb_data_all_diagnoses$source)),
                 sort(expected_sources))
  }
)

# `field_id_pivot_longer_multi()` -----------------------------------------

test_that("`field_id_pivot_longer_multi()` correctly matches diagnoses with dates by instance", {
  result <- field_id_pivot_longer_multi(
    # field_ids for HES data (icd9)
    field_ids = c("41271", "41281"),
    ukb_pheno = dummy_ukb_data_2eid,
    data_dict = dummy_ukb_data_dict,
    ukb_codings = ukb_codings)

  expect_true(is.data.frame(result))
})

# TODO make a better test - use mockr package:
# test_that("`field_id_pivot_longer_multi()` correctly raises an error if instance/array do not match between field ids", {
#   result <- field_id_pivot_longer_multi(
#     # field_ids for HES data (icd9)
#     field_ids = c("41271", "41281"),
#     ukb_pheno = dummy_ukb_data_2eid,
#     data_dict = dummy_ukb_data_dict,
#     ukb_codings = ukb_codings)
#
# })
