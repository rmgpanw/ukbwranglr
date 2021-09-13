
# Note that the dummy data formats dates as DD/MM/YYYY, whereas UKB formats these as YYYY-MM-DD

# SETUP -------------------------------------------------------------------

# Make dummy data ---------------------------------------------------------

# get ukb data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# This is now replaced with `dummy_ukb_main_clinical_events` below

# get dummy ukb data (contains all diagnoses columns) and load single eid for testing
# dummy_ukb_data_path <- download_dummy_ukb_data_to_tempdir()
# dummy_ukb_data_dict <- make_data_dict(ukb_main = dummy_ukb_data_path,
#                                       delim = ",",
#                                       ukb_data_dict = ukb_data_dict)
# dummy_ukb_data_2eid <- read_ukb(path = dummy_ukb_data_path,
#                              delim = ",",
#                              data_dict = dummy_ukb_data_dict %>%
#                                dplyr::filter(FieldID == "eid" |
#                                                (instance %in% c("0", "2") &
#                                                   array %in% c("0", "3") &
#                                                   FieldID %in% as.character(purrr::flatten(CLINICAL_EVENTS_FIELD_IDS)))),
#                              ukb_data_dict = ukb_data_dict,
#                              ukb_codings = ukb_codings,
#                              nrows = 2)

# manually create dummy data for 2 eids (copied from `dummy_ukb_data_2eid` above and appended
# operations fields, reformatted dates)
dummy_ukb_main_clinical_events <- data.table::data.table(
  eid = c(1, 2),
  cancer_code_self_reported_f20001_0_0 = c(1048, 1046),
  cancer_code_self_reported_f20001_0_3 = c(1005, 1003),
  cancer_code_self_reported_f20001_2_0 = c(1045, 1028),
  cancer_code_self_reported_f20001_2_3 = c(1017, 1039),
  non_cancer_illness_code_self_reported_f20002_0_0 = c(1665, 1383),
  non_cancer_illness_code_self_reported_f20002_0_3 = c(1223, 1352),
  non_cancer_illness_code_self_reported_f20002_2_0 = c(1514, 1447),
  non_cancer_illness_code_self_reported_f20002_2_3 = c(NA, 1165),
  interpolated_year_when_cancer_first_diagnosed_f20006_0_0 = c(2012.8173, 2016.0638),
  interpolated_year_when_cancer_first_diagnosed_f20006_0_3 = c(2007.0874, 2023.1635),
  interpolated_year_when_cancer_first_diagnosed_f20006_2_0 = c(2023.2047, 2024.0358),
  interpolated_year_when_cancer_first_diagnosed_f20006_2_3 = c(2014.7373, 2013.2044),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_0_0 = c(1998.9782, 2011.0121),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_0_3 = c(2003.1527, 2020.502),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_2_0 = c(2011.2636, 1981.1627),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_2_3 = c(2018.786, 1983.0059),
  diagnoses_icd10_f41270_0_0 = c('X715', 'T630'),
  diagnoses_icd10_f41270_0_3 = c('D550', 'M0087'),
  diagnoses_icd9_f41271_0_0 = c('E89115', 'E8326'),
  diagnoses_icd9_f41271_0_3 = c(NA, '75513'),
  date_of_first_in_patient_diagnosis_icd10_f41280_0_0 = c('1955-11-12', '1939-02-16'),
  date_of_first_in_patient_diagnosis_icd10_f41280_0_3 = c('1910-02-19', '1965-08-08'),
  date_of_first_in_patient_diagnosis_icd9_f41281_0_0 = c('1917-10-08', '1955-02-11'),
  date_of_first_in_patient_diagnosis_icd9_f41281_0_3 = c('1969-11-23', '1956-09-12'),
  underlying_primary_cause_of_death_icd10_f40001_0_0 = c('X095', 'A162'),
  contributory_secondary_causes_of_death_icd10_f40002_0_0 = c('P912', 'V374'),
  contributory_secondary_causes_of_death_icd10_f40002_0_3 = c('X715', NA),
  date_of_cancer_diagnosis_f40005_0_0 = c('1956-11-24', '1910-10-04'),
  date_of_cancer_diagnosis_f40005_2_0 = c('1962-09-04', NA),
  type_of_cancer_icd10_f40006_0_0 = c('M4815', NA),
  type_of_cancer_icd10_f40006_2_0 = c('C850', 'W192'),
  type_of_cancer_icd9_f40013_0_0 = c('27134', '9626'),
  type_of_cancer_icd9_f40013_2_0 = c('2042', 'E90200'),
  operative_procedures_opcs4_f41272_0_0 = c('A01', 'A023'),
  operative_procedures_opcs4_f41272_0_3 = c('A018', 'A02'),
  date_of_first_operative_procedure_opcs4_f41282_0_0 = c('1956-11-24', '1910-10-04'),
  date_of_first_operative_procedure_opcs4_f41282_0_3 = c('1969-11-23', '1956-09-12'),
  operative_procedures_opcs3_f41273_0_0 = c('001', '0011'),
  operative_procedures_opcs3_f41273_0_3 = c('0081', '0071'),
  date_of_first_operative_procedure_opcs3_f41283_0_0 = c('1969-11-23', '1956-09-12'),
  date_of_first_operative_procedure_opcs3_f41283_0_3 = c('1955-11-12', '1939-02-16'),
  operation_code_f20004_0_0 = c(1102, 1105),
  operation_code_f20004_0_3 = c(1108, 1109),
  interpolated_year_when_operation_took_place_f20010_0_0 = c(2012.8173, 2016.0638),
  interpolated_year_when_operation_took_place_f20010_0_3 = c(2008.2342, NA)
)

# tidy clinical events
dummy_ukb_main_clinical_events_tidy <- tidy_clinical_events(ukb_main = dummy_ukb_main_clinical_events,
                                                     ukb_data_dict = ukb_data_dict,
                                                     ukb_codings = ukb_codings,
                                                     clinical_events = c(
                                                       "death_icd10",
                                                       "self_report_non_cancer",
                                                       "self_report_non_cancer_icd10",
                                                       "self_report_cancer",
                                                       "self_report_operation",
                                                       "cancer_register_icd9",
                                                       "cancer_register_icd10",
                                                       "summary_hes_icd9",
                                                       "summary_hes_icd10",
                                                       "summary_hes_opcs3",
                                                       "summary_hes_opcs4"
                                                     ),
                                                     strict = FALSE,
                                                     .details_only = FALSE)

# dummy clinical_events_df
dummy_clinical_events <- make_dummy_clinical_events_df(eids = c(1, 2, 3),
                                                       n_rows = c(200, 200, 200))

# dummy_clinical_events_df as sqlite db
# Create an ephemeral in-memory RSQLite database
con <- DBI::dbConnect(RSQLite::SQLite(),
                      ":memory:")

# copy dummy_clinical_events_df to db
dplyr::copy_to(con,
        dummy_clinical_events %>%
          dplyr::mutate(date = as.character(date)),
        "dummy_clinical_events",
        overwrite = TRUE)

# get tbl_dbi object
dummy_clinical_events_db <- dplyr::tbl(con, "dummy_clinical_events")

# should match output from `example_clinical_codes()`
dummy_clinical_codes_df <- tibble::tribble(
  ~ disease, ~ description, ~ category, ~ code_type, ~ code, ~ author,
  "Diabetes", "diabetes", "Diabetes unspecified", "icd10", "D", "test",
  "Diabetes", "gestational diabetes", "Gestational diabetes", "icd10", "E", "test",
  "Diabetes", "type 1 diabetes", "Type 1 diabetes", "read3", "H", "test",
  "Diabetes", "type 1 diabetes", "Type 1 diabetes", "read3", "G", "test",
  "Diabetes", "type 2 diabetes", "Type 2 diabetes", "data_coding_6", "D", "test",
)

# Process dummy tidy clinical events data (and record expected results) ------------------------

# Note: this is manually checked to produce 'expected' below. ***will change
# whenever CLINICAL_EVENTS_SOURCES$source changes***
min_dates <- extract_single_diagnostic_code_record_basis(clinical_events = dummy_clinical_events,
                                                         codes = c("A", "B"),
                                                         mapping_function = extract_first_or_last_record_mapper,
                                                         min_max = "min")

min_dates_db <- extract_single_diagnostic_code_record_basis(clinical_events = dummy_clinical_events_db,
                                                         codes = c("A", "B"),
                                                         mapping_function = extract_first_or_last_record_mapper,
                                                         min_max = "min")

# min_dates: expected result (- nested `data` col)
expected <- tibble::tribble(
  ~ eid, ~ source, ~ code, ~ date,
  1, "f41271", "B", "2000-01-03",
  2, "f20002", "A", "2000-01-13",
  3, "f20002", "B", "2000-01-01",
)

# TESTS -------------------------------------------------------------------

# `tidy_clinical_events()`  -----------------------------------------

test_that(
  "output from `tidy_clinical_events()` contains all expected source types", {
    # won't contain gp read codes
    expect_equal(sort(unique(dplyr::bind_rows(dummy_ukb_main_clinical_events_tidy)$source)),
                 sort(subset(CLINICAL_EVENTS_SOURCES$source, !CLINICAL_EVENTS_SOURCES$source %in% c("gpc_r2",
                                                                                                    "gpc_r3"))))
  }
)

test_that(
  "`tidy_clinical_events()` raises warning message if any clinical event types are missing from ukb_main",
  {
    expect_warning(
      tidy_clinical_events(ukb_main = dummy_ukb_main_clinical_events %>%
                             dplyr::select(-tidyselect::contains("operation")),
                           ukb_data_dict = ukb_data_dict,
                           ukb_codings = ukb_codings,
                           clinical_events = c(
                             "death_icd10",
                             # "self_report_non_cancer",
                             # "self_report_non_cancer_icd10",
                             # "self_report_cancer",
                             "self_report_operation"
                             # "cancer_register_icd9",
                             # "cancer_register_icd10",
                             # "summary_hes_icd9",
                             # "summary_hes_icd10",
                             # "summary_hes_opcs3",
                             # "summary_hes_opcs4"
                           ),
                           strict = FALSE,
                           .details_only = FALSE),
      ": self_report_operation. Use")
  }
)

test_that(
  "`tidy_clinical_events()` raises error if any clinical event types are missing from ukb_main and strict = TRUE",
  {
    expect_error(
      tidy_clinical_events(ukb_main = dummy_ukb_main_clinical_events %>%
                             dplyr::select(-tidyselect::contains("operation")),
                           ukb_data_dict = ukb_data_dict,
                           ukb_codings = ukb_codings,
                           clinical_events = c(
                             "death_icd10",
                             # "self_report_non_cancer",
                             # "self_report_non_cancer_icd10",
                             # "self_report_cancer",
                             "self_report_operation"
                             # "cancer_register_icd9",
                             # "cancer_register_icd10",
                             # "summary_hes_icd9",
                             # "summary_hes_icd10",
                             # "summary_hes_opcs3",
                             # "summary_hes_opcs4"
                           ),
                           strict = TRUE,
                           .details_only = FALSE),
      ": self_report_operation. Use")
  }
)

# note: use `rawutil::print_df_as_call_to_tibble` to generate expected tibbles
# after manually checking they are correct

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'death_icd10'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$death_icd10,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2, 1),
                        source = c('f40001', 'f40001', 'f40002', 'f40002', 'f40002'),
                        index = c('0_0', '0_0', '0_0', '0_0', '0_3'),
                        code = c('X095', 'A162', 'P912', 'V374', 'X715'),
                        date = as.character(c(NA, NA, NA, NA, NA)),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'self_report_non_cancer'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$self_report_non_cancer,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2, 1, 2, 2),
                        source = c('f20002', 'f20002', 'f20002', 'f20002', 'f20002', 'f20002', 'f20002'),
                        index = c('0_0', '0_0', '0_3', '0_3', '2_0', '2_0', '2_3'),
                        code = c('1665', '1383', '1223', '1352', '1514', '1447', '1165'),
                        date = c('1998-12-24', '2011-01-05', '2003-02-25', '2020-07-02', '2011-04-07', '1981-03-01', '1983-01-03'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'self_report_non_cancer_icd10'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$self_report_non_cancer_icd10,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2, 1, 2, 2),
                        source = c('f20002_icd10', 'f20002_icd10', 'f20002_icd10', 'f20002_icd10', 'f20002_icd10', 'f20002_icd10', 'f20002_icd10'),
                        index = c('0_0', '0_0', '0_3', '0_3', '2_0', '2_0', '2_3'),
                        code = c('N95', 'M33', 'E11', 'N84', 'N30', 'D61', 'K85'),
                        date = c('1998-12-24', '2011-01-05', '2003-02-25', '2020-07-02', '2011-04-07', '1981-03-01', '1983-01-03'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'self_report_cancer'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$self_report_cancer,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2, 1, 2, 1, 2),
                        source = c('f20001', 'f20001', 'f20001', 'f20001', 'f20001', 'f20001', 'f20001', 'f20001'),
                        index = c('0_0', '0_0', '0_3', '0_3', '2_0', '2_0', '2_3', '2_3'),
                        code = c('1048', '1046', '1005', '1003', '1045', '1028', '1017', '1039'),
                        date = c('2012-10-26', '2016-01-24', '2007-02-01', '2023-03-01', '2023-03-16', '2024-01-14', '2014-09-27', '2013-03-16'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'self_report_operation'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$self_report_operation,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2),
                        source = c('f20004', 'f20004', 'f20004', 'f20004'),
                        index = c('0_0', '0_0', '0_3', '0_3'),
                        code = c('1102', '1105', '1108', '1109'),
                        date = c('2012-10-26', '2016-01-24', '2008-03-26', NA),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'cancer_register_icd9'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$cancer_register_icd9,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2),
                        source = c('f40013', 'f40013', 'f40013', 'f40013'),
                        index = c('0_0', '0_0', '2_0', '2_0'),
                        code = c('27134', '9626', '2042', 'E90200'),
                        date = c('1956-11-24', '1910-10-04', '1962-09-04', NA),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'cancer_register_icd10'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$cancer_register_icd10,
                      tibble::tibble(
                        eid = c(1, 1, 2),
                        source = c('f40006', 'f40006', 'f40006'),
                        index = c('0_0', '2_0', '2_0'),
                        code = c('M4815', 'C850', 'W192'),
                        date = c('1956-11-24', '1962-09-04', NA),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'summary_hes_icd9'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$summary_hes_icd9,
                      tibble::tibble(
                        eid = c(1, 2, 2),
                        source = c('f41271', 'f41271', 'f41271'),
                        index = c('0_0', '0_0', '0_3'),
                        code = c('E89115', 'E8326', '75513'),
                        date = c('1917-10-08', '1955-02-11', '1956-09-12'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'summary_hes_icd10'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$summary_hes_icd10,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2),
                        source = c('f41270', 'f41270', 'f41270', 'f41270'),
                        index = c('0_0', '0_0', '0_3', '0_3'),
                        code = c('X715', 'T630', 'D550', 'M0087'),
                        date = c('1955-11-12', '1939-02-16', '1910-02-19', '1965-08-08'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'summary_hes_opcs3'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$summary_hes_opcs3,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2),
                        source = c('f41273', 'f41273', 'f41273', 'f41273'),
                        index = c('0_0', '0_0', '0_3', '0_3'),
                        code = c('001', '0011', '0081', '0071'),
                        date = c('1969-11-23', '1956-09-12', '1955-11-12', '1939-02-16'),
                      ))
  }
)

test_that(
  "`Expected results returned`tidy_clinical_events` returns the expected results for 'summary_hes_opcs4'", {
    expect_equivalent(dummy_ukb_main_clinical_events_tidy$summary_hes_opcs4,
                      tibble::tibble(
                        eid = c(1, 2, 1, 2),
                        source = c('f41272', 'f41272', 'f41272', 'f41272'),
                        index = c('0_0', '0_0', '0_3', '0_3'),
                        code = c('A01', 'A023', 'A018', 'A02'),
                        date = c('1956-11-24', '1910-10-04', '1969-11-23', '1956-09-12'),
                      ))
  }
)

# `extract_first_or_last_clinical_event_multi_single_disease()` --------------------------

test_that(
  "`extract_first_or_last_clinical_event_multi_single_disease()` returns the expected column names", {
    result <-
      extract_first_or_last_clinical_event_multi_single_disease(
        disease = "Diabetes",
        clinical_events = dummy_clinical_events,
        clinical_codes = dummy_clinical_codes_df,
        min_max = "min",
        prefix = "testy_"
      )

    expect_equal(
      names(result),
      c(
        "eid",
        "testy_DIABETES_TEST_min_date",
        "testy_DIABETES_TEST_indicator",
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

# `extract_phenotypes()` --------------------------

test_that(
  "`extract_phenotypes()` returns the expected column names", {
    result <- extract_phenotypes(clinical_events = dummy_clinical_events,
                                               clinical_codes = dummy_clinical_codes_df,
                                               min_max = "min",
                                               prefix = "testy_")

    expect_equal(
      names(result[[1]]),
      c(
        "eid",
        "testy_DIABETES_TEST_min_date",
        "testy_DIABETES_TEST_indicator",
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

# This works when run locally but not when running tests with r-cmd-check

# test_that(
#   "`extract_first_or_last_clinical_event_multi()` works with tbl object", {
#     result <- extract_first_or_last_clinical_event_multi(clinical_events = dummy_clinical_events_db,
#                                                          clinical_codes = dummy_clinical_codes_df,
#                                                          min_max = "min",
#                                                          prefix = "testy_")
#
#     expect_equal(
#       names(result[[1]]),
#       c(
#         "eid",
#         "testy_DIABETES_TEST_min_date",
#         "testy_DIABETES_TEST_indicator",
#         "testy_diabetes_unspecified_test_min_date",
#         "testy_diabetes_unspecified_test_indicator",
#         "testy_gestational_diabetes_test_min_date",
#         "testy_gestational_diabetes_test_indicator",
#         "testy_type_1_diabetes_test_min_date",
#         "testy_type_1_diabetes_test_indicator",
#         "testy_type_2_diabetes_test_min_date",
#         "testy_type_2_diabetes_test_indicator"
#       )
#     )
#   }
# )

test_that(
  # need to rebuild package to include any changes when running this test
  "`extract_phenotypes()` works with parallel processing, data frame clinical events", {
    result <- extract_phenotypes(clinical_events = dummy_clinical_events,
                                                         clinical_codes = dummy_clinical_codes_df,
                                                         min_max = "min",
                                                         prefix = "testy_",
                                                         workers = 2)

    expect_equal(
      names(result[[1]]),
      c(
        "eid",
        "testy_DIABETES_TEST_min_date",
        "testy_DIABETES_TEST_indicator",
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

# Test not working - cannot test with a dummy database that is stored in tempdir
# or memory (cannot share this between multiple sessions running in parallel)

# test_that(
#   "`extract_first_or_last_clinical_event_multi()` works with parallel processing, tbl clinical events", {
#     result <- extract_first_or_last_clinical_event_multi(clinical_events = dummy_clinical_events_db,
#                                                          clinical_codes = dummy_clinical_codes_df,
#                                                          min_max = "min",
#                                                          prefix = "testy_",
#                                                          workers = 2)
#
#     expect_equal(
#       names(result[[1]]),
#       c(
#         "eid",
#         "testy_DIABETES_TEST_min_date",
#         "testy_DIABETES_TEST_indicator",
#         "testy_diabetes_unspecified_test_min_date",
#         "testy_diabetes_unspecified_test_indicator",
#         "testy_gestational_diabetes_test_min_date",
#         "testy_gestational_diabetes_test_indicator",
#         "testy_type_1_diabetes_test_min_date",
#         "testy_type_1_diabetes_test_indicator",
#         "testy_type_2_diabetes_test_min_date",
#         "testy_type_2_diabetes_test_indicator"
#       )
#     )
#   }
# )

# `mutate_age_at_event_cols()` --------------------------------------------

test_that(
  "`mutate_age_at_event_cols()` creates the expected age-at-event columns", {
    dummy_ukb_pheno <- tibble::tribble(
      ~ eid, ~ dob, ~ event_date,
      1, "2000-01-01", "2010-01-01"
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
  "`extract_single_diagnostic_code_record_basis()` extracts the correct earliest and latest dates - all sources and class(clinical_events) = dataframe",
  {
    # expectations
    expect_equal(min_dates$eid, expected$eid)
    expect_equal(min_dates$source, expected$source)
    expect_equal(min_dates$code, expected$code)
    expect_equal(min_dates$date, expected$date)
  }
)

test_that(
  "`extract_single_diagnostic_code_record_basis()` extracts the correct earliest and latest dates - all sources and class(clinical_events) = tbl_dbi",
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
      nrow(filter_clinical_events_for_codes(clinical_events = dummy_clinical_events,
                                       codes = list("read2" = "A"))),
      7
    )

    expect_equal(
      nrow(filter_clinical_events_for_codes(clinical_events = dummy_clinical_events_db,
                                            codes = list("read2" = "A"))),
      7
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
                                                       df_class = "tbl_dbi",
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

# `make_self_report_special_decimal_dates_na()` ------------------------------

test_that("`make_self_report_special_decimal_dates_na()` removes special dates from 'date' column (data coding 13, used for field IDs 20006 and 20008: interpolated year of diagnosis for self-reported cancer/non-cancer illnesses)", {

  self_report_non_cancer_diagnoses_unstandardised <- tibble::tribble(
    ~ eid, ~ f20002, ~ f20002_value, ~ instance_array, ~ f20008, ~ date,
    "1", "noncancer_illness_code_selfreported_f20002_0_0", "1665", "0_0", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_0", 1998.1,
    "1", "noncancer_illness_code_selfreported_f20002_0_1", "1532", "0_1", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_1", -1,
    "1", "noncancer_illness_code_selfreported_f20002_0_2", "1552", "0_2", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_2", -3,
  )

  expect_equal(
    make_self_report_special_decimal_dates_na(self_report_non_cancer_diagnoses_unstandardised)$date,
    c(1998.1, NA, NA))
})

# CLOSE DBI CONNECTION ----------------------------------------------------

DBI::dbDisconnect(con)
