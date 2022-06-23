
# SETUP -------------------------------------------------------------------

dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")

dummy_ukb_data_raw_path <- get_ukb_dummy("dummy_ukb_main.tsv",
  path_only = TRUE
)

dummy_data_dict <- make_data_dict(dummy_ukb_data_raw_path,
  ukb_data_dict = dummy_ukb_data_dict
)

dummy_ukb_main <- read_ukb(dummy_ukb_data_raw_path,
  data_dict = dummy_data_dict,
  ukb_data_dict = dummy_ukb_data_dict,
  ukb_codings = dummy_ukb_codings
)

# tidy dummy clinical events
dummy_clinical_events_list <-
  tidy_clinical_events(
    ukb_main = dummy_ukb_main,
    ukb_data_dict = dummy_ukb_data_dict,
    ukb_codings = dummy_ukb_codings,
    clinical_events = names(CLINICAL_EVENTS_FIELD_IDS),
    strict = TRUE,
    .details_only = FALSE
  )

# add some dummy GP data
dummy_gp_clinical_events_tidy <- get_ukb_dummy("dummy_gp_clinical.txt") %>%
  tidy_gp_clinical() %>%
  .$clinical_events

# combine all
dummy_clinical_events <- dummy_clinical_events_list %>%
  dplyr::bind_rows() %>%
  dplyr::bind_rows(dummy_gp_clinical_events_tidy)

# dummy_clinical_events as sqlite db
# Create an ephemeral in-memory RSQLite database
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  ":memory:"
)

# copy dummy_clinical_events_df to db
dplyr::copy_to(con,
  dummy_clinical_events %>%
    dplyr::mutate(date = as.character(date)),
  "dummy_clinical_events",
  overwrite = TRUE
)

# get tbl_dbi object
dummy_clinical_events_db <- dplyr::tbl(con, "dummy_clinical_events")


# Make dummy clinical codes
dummy_clinical_codes <- tibble::tribble(
  ~disease, ~description, ~category, ~code_type, ~code, ~author,
  "Disease1", "disease a", "A", "icd10", "W192", "test",
  "Disease1", "disease a", "A", "icd10", "X095", "test",
  "Disease1", "disease b", "B", "icd9", "27134", "test",
  "Disease1", "disease b", "B", "read3", "X40J5", "test",
  "Disease1", "disease a", "A", "read2", "C108.", "test",
  "Disease2", "disease c", "C", "data_coding_6", "1665", "test",
  "Disease2", "disease c", "C", "data_coding_3", "1045", "test",
  "Disease2", "disease d", "D", "data_coding_5", "1108", "test",
  "Disease2", "disease d", "D", "opcs3", "001", "test",
  "Disease2", "disease d", "D", "opcs4", "A01", "test"
)

stopifnot(validate_clinical_codes(dummy_clinical_codes))

# TESTS -------------------------------------------------------------------

result2 <- extract_phenotypes2(clinical_events = dummy_clinical_events_db,
                    clinical_codes = dummy_clinical_codes)

# `tidy_clinical_events_basis()`  -----------------------------------------

test_that("`tidy_clinical_events_basis()` removes empty string values", {
  # create dummy data containing empty strings
  dummy_ukb_main_clinical_events_empty_strings <-
    dummy_ukb_main %>%
    dplyr::select(
      eid,
      tidyselect::contains("opcs4")
    ) %>%
    head(2)

  dummy_ukb_main_clinical_events_empty_strings$operative_procedures_opcs4_f41272_0_0 <-
    c("", "  ")
  dummy_ukb_main_clinical_events_empty_strings$operative_procedures_opcs4_f41272_0_3[1] <-
    NA_character_

  expect_warning(
    tidy_clinical_events_basis(
      ukb_main = dummy_ukb_main_clinical_events_empty_strings,
      data_dict = make_data_dict(
        dummy_ukb_main_clinical_events_empty_strings,
        ukb_data_dict = dummy_ukb_data_dict
      ),
      ukb_codings = dummy_ukb_codings,
      data_dict_colname_col = "colheaders_raw",
      code_col_field_id = "41272",
      date_col_field_id = "41282"
    ),
    regexp = "Detected 2 empty code values"
  )

  expect_equal(
    suppressWarnings(
      tidy_clinical_events_basis(
        ukb_main = dummy_ukb_main_clinical_events_empty_strings,
        data_dict = make_data_dict(
          dummy_ukb_main_clinical_events_empty_strings,
          ukb_data_dict = dummy_ukb_data_dict
        ),
        ukb_codings = dummy_ukb_codings,
        data_dict_colname_col = "colheaders_raw",
        code_col_field_id = "41272",
        date_col_field_id = "41282"
      )
    ) %>%
      # need to remove label attributes
      dplyr::mutate(dplyr::across(
        tidyselect::everything(),
        ~ {
          attributes(.x) <- NULL
          .x
        }
      )),
    expected = data.table::data.table(
      eid = c(2),
      source = c("f41272"),
      index = c("0_3"),
      code = c("A02"),
      date = c("1956-09-12")
    )
  )
})

# `tidy_clinical_events()`  -----------------------------------------

test_that(
  "output from `tidy_clinical_events()` contains all expected source types",
  {
    # won't contain gp read codes
    expect_equal(
      sort(unique(dplyr::bind_rows(dummy_clinical_events_list)$source)),
      sort(subset(CLINICAL_EVENTS_SOURCES$source, !stringr::str_detect(CLINICAL_EVENTS_SOURCES$source,
        pattern = "^gp"
      )))
    )
  }
)

test_that(
  "`tidy_clinical_events()` raises warning message if any clinical event types are missing from ukb_main",
  {
    expect_warning(
      tidy_clinical_events(
        ukb_main = dummy_ukb_main %>%
          dplyr::select(-tidyselect::contains("operation")),
        ukb_data_dict = dummy_ukb_data_dict,
        ukb_codings = dummy_ukb_codings,
        clinical_events = c(
          "primary_death_icd10",
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
        .details_only = FALSE
      ),
      ": self_report_operation. Use"
    )
  }
)

test_that(
  "`tidy_clinical_events()` raises error if any clinical event types are missing from ukb_main and strict = TRUE",
  {
    expect_error(
      tidy_clinical_events(
        ukb_main = dummy_ukb_main %>%
          dplyr::select(-tidyselect::contains("operation")),
        ukb_data_dict = dummy_ukb_data_dict,
        ukb_codings = dummy_ukb_codings,
        clinical_events = c(
          "primary_death_icd10",
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
        .details_only = FALSE
      ),
      ": self_report_operation. Use"
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'primary_death_icd10'",
  {
    expect_equivalent(
      dummy_clinical_events_list$primary_death_icd10,
      tibble::tibble(
        eid = c(1, 2, 1, 2),
        source = c("f40001", "f40001", "f40001", "f40001"),
        index = c("0_0", "0_0", "1_0", "1_0"),
        code = c("X095", "A162", "X095", "A162"),
        date = c("1917-10-08", "1955-02-11", "1910-02-19", "1965-08-08"),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'secondary_death_icd10'",
  {
    expect_equivalent(
      dummy_clinical_events_list$secondary_death_icd10,
      tibble::tibble(
        eid = c(1, 2, 1),
        source = c("f40002", "f40002", "f40002"),
        index = c("0_0", "0_0", "1_3"),
        code = c("W192", "V374", "X715"),
        date = c("1917-10-08", "1955-02-11", "1910-02-19")
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'self_report_medication'",
  {
    expect_equivalent(
      dummy_clinical_events_list$self_report_medication,
      tibble::tibble(
        eid = c(1, 2, 1, 2, 1, 2),
        source = c("f20003", "f20003", "f20003", "f20003", "f20003", "f20003"),
        index = c("0_0", "0_0", "2_0", "2_0", "2_3", "2_3"),
        code = c("1140861958", "1141146234", "1141146188", "1141184722", "1141184722", "1140861958"),
        date = c("1955-02-11", "1965-08-08", "1910-02-19", "1915-03-18", "1910-02-19", "1915-03-18")
      )
    )
  }
)

test_that("`tidy_clinical_events` returns the expected results for 'self_report_non_cancer'", {
  expect_equivalent(
    dummy_clinical_events_list$self_report_non_cancer,
    data.table::data.table(
      eid = c(1L, 2L, 3L, 4L, 1L, 2L, 1L, 2L, 2L),
      source = c(
        "f20002",
        "f20002",
        "f20002",
        "f20002",
        "f20002",
        "f20002",
        "f20002",
        "f20002",
        "f20002"
      ),
      index = c(
        "0_0", "0_0", "0_0", "0_0", "0_3",
        "0_3", "2_0", "2_0", "2_3"
      ),
      code = c(
        "1665",
        "1383",
        "1665",
        "1383",
        "1223",
        "1352",
        "1514",
        "1447",
        "1165"
      ),
      date = c(
        "1998-12-24",
        "2011-01-05",
        NA,
        NA,
        "2003-02-25",
        "2020-07-02",
        "2011-04-07",
        "1981-03-01",
        "1983-01-03"
      )
    )
  )
})

test_that("`tidy_clinical_events` removes special date codings for 'self_report_non_cancer'", {
  expect_equivalent(
    dummy_clinical_events_list$self_report_non_cancer$date[3:4],
    c(NA_character_, NA_character_)
  )
})

test_that(
  "`tidy_clinical_events` returns the expected results for 'self_report_non_cancer_icd10'",
  {
    expect_equivalent(
      dummy_clinical_events_list$self_report_non_cancer_icd10,
      data.table::data.table(
        eid = c(1L, 2L, 3L, 4L, 1L, 2L, 1L, 2L, 2L),
        source = c(
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10",
          "f20002_icd10"
        ),
        index = c(
          "0_0", "0_0", "0_0", "0_0", "0_3",
          "0_3", "2_0", "2_0", "2_3"
        ),
        code = c(
          "N95", "M33", "N95", "M33", "E11",
          "N84", "N30", "D61", "K85"
        ),
        date = c(
          "1998-12-24",
          "2011-01-05",
          NA,
          NA,
          "2003-02-25",
          "2020-07-02",
          "2011-04-07",
          "1981-03-01",
          "1983-01-03"
        )
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'self_report_cancer'",
  {
    expect_equivalent(
      dummy_clinical_events_list$self_report_cancer,
      tibble::tibble(
        eid = c(1, 2, 1, 2, 1, 2, 1, 2),
        source = c("f20001", "f20001", "f20001", "f20001", "f20001", "f20001", "f20001", "f20001"),
        index = c("0_0", "0_0", "0_3", "0_3", "2_0", "2_0", "2_3", "2_3"),
        code = c("1048", "1046", "1005", "1003", "1045", "1028", "1017", "1039"),
        date = c("2012-10-26", "2016-01-24", "2007-02-01", "2023-03-01", "2023-03-16", "2024-01-14", "2014-09-27", "2013-03-16"),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'self_report_operation'",
  {
    expect_equivalent(
      dummy_clinical_events_list$self_report_operation,
      tibble::tibble(
        eid = c(1, 2, 1, 2),
        source = c("f20004", "f20004", "f20004", "f20004"),
        index = c("0_0", "0_0", "0_3", "0_3"),
        code = c("1102", "1105", "1108", "1109"),
        date = c("2012-10-26", "2016-01-24", "2008-03-26", NA),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'cancer_register_icd9'",
  {
    expect_equivalent(
      dummy_clinical_events_list$cancer_register_icd9,
      tibble::tibble(
        eid = c(1, 2, 1, 2),
        source = c("f40013", "f40013", "f40013", "f40013"),
        index = c("0_0", "0_0", "2_0", "2_0"),
        code = c("27134", "9626", "2042", "E90200"),
        date = c("1956-11-24", "1910-10-04", "1962-09-04", NA),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'cancer_register_icd10'",
  {
    expect_equivalent(
      dummy_clinical_events_list$cancer_register_icd10,
      tibble::tibble(
        eid = c(1, 1, 2),
        source = c("f40006", "f40006", "f40006"),
        index = c("0_0", "2_0", "2_0"),
        code = c("M4815", "C850", "W192"),
        date = c("1956-11-24", "1962-09-04", NA),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'summary_hes_icd9'",
  {
    expect_equivalent(
      dummy_clinical_events_list$summary_hes_icd9,
      tibble::tibble(
        eid = c(1, 2, 2),
        source = c("f41271", "f41271", "f41271"),
        index = c("0_0", "0_0", "0_3"),
        code = c("E89115", "E8326", "75513"),
        date = c("1917-10-08", "1955-02-11", "1956-09-12"),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'summary_hes_icd10'",
  {
    expect_equivalent(
      dummy_clinical_events_list$summary_hes_icd10,
      tibble::tibble(
        eid = c(1, 2, 1, 2),
        source = c("f41270", "f41270", "f41270", "f41270"),
        index = c("0_0", "0_0", "0_3", "0_3"),
        code = c("X715", "E11", "E10", "M0087"),
        date = c("1955-11-12", "1939-02-16", "1910-02-19", "1965-08-08"),
      )
    )
  }
)

test_that(
  "`tidy_clinical_events` returns the expected results for 'summary_hes_opcs3'",
  {
    expect_equivalent(
      dummy_clinical_events_list$summary_hes_opcs3,
      tibble::tibble(
        eid = c(1, 2, 1, 2),
        source = c("f41273", "f41273", "f41273", "f41273"),
        index = c("0_0", "0_0", "0_3", "0_3"),
        code = c("001", "0011", "0081", "0071"),
        date = c("1969-11-23", "1956-09-12", "1955-11-12", "1939-02-16"),
      )
    )
  }
)

test_that("`tidy_clinical_events` returns the expected results for 'summary_hes_opcs4'", {
  expect_equivalent(
    dummy_clinical_events_list$summary_hes_opcs4,
    data.table::data.table(
      eid = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L),
      source = c(
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272",
        "f41272"
      ),
      index = c(
        "0_0",
        "0_0",
        "0_0",
        "0_0",
        "0_0",
        "0_0",
        "0_0",
        "0_0",
        "0_3",
        "0_3"
      ),
      code = c(
        "A01",
        "A023",
        "H01",
        "H011",
        "H022",
        "H013",
        "H018",
        "H019",
        "A018",
        "A02"
      ),
      date = c(
        "1956-11-24",
        "1910-10-04",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        "1969-11-23",
        "1956-09-12"
      )
    )
  )
})

# `extract_phenotypes_single_disease()` --------------------------

test_that(
  "`extract_phenotypes_single_disease()` returns the expected list names and column names",
  {
    result <-
      extract_phenotypes_single_disease(
        disease = "Disease1",
        clinical_events = dummy_clinical_events,
        clinical_codes = dummy_clinical_codes,
        min_max = "min",
        colnames_prefix = "test_",
        labels_prefix = "Test! ",
        data_sources = NULL,
        keep_all = FALSE
      )

    expect_equal(
      sort(names(result)),
      sort(c(
        "test_a_test",
        "test_b_test",
        "test_DISEASE1_TEST"
      ))
    )

    expect_equal(
      sort(names(result[["test_a_test"]])),
      sort(c(
        "eid",
        "test_a_test_min_date",
        "test_a_test_indicator"
      ))
    )
  }
)

# `extract_phenotypes()` --------------------------

test_that("`extract_phenotypes()` returns the expected column names", {
  result <-
    extract_phenotypes(
      clinical_events = dummy_clinical_events,
      clinical_codes = dummy_clinical_codes,
      min_max = "min",
      colnames_prefix = "test_",
      labels_prefix = "Test! "
    )

  expect_equal(
    sort(names(result[["Disease2"]])),
    sort(c(
      "test_c_test",
      "test_d_test",
      "test_DISEASE2_TEST"
    ))
  )

  # test variable labels are prefixed
  expect_equal(
    attributes(result[["Disease2"]]$test_c_test$test_c_test_indicator)$label,
    "Test! C"
  )

  expect_equal(
    attributes(result[["Disease2"]]$test_c_test$test_c_test_min_date)$label,
    "Test! C date (min)"
  )
})

# Note can test dummy_clinical_events_db locally but not with R CMD check
test_that("`extract_phenotypes()` returns the expected column names with tbl_dbi object", {
  result <-
    extract_phenotypes(
      clinical_events = dummy_clinical_events_db,
      clinical_codes = dummy_clinical_codes,
      min_max = "min",
      colnames_prefix = "test_",
      labels_prefix = "Test! "
    )

  expect_equal(
    sort(names(result[["Disease2"]])),
    sort(c(
      "test_c_test",
      "test_d_test",
      "test_DISEASE2_TEST"
    ))
  )

  # test variable labels are prefixed
  expect_equal(
    attributes(result[["Disease2"]]$test_c_test$test_c_test_indicator)$label,
    "Test! C"
  )

  expect_equal(
    attributes(result[["Disease2"]]$test_c_test$test_c_test_min_date)$label,
    "Test! C date (min)"
  )
})

test_that( # need to rebuild package to include any changes when running this test
  "`extract_phenotypes()` works with parallel processing, data frame clinical events",
  {
    result <-
      extract_phenotypes(
        clinical_events = dummy_clinical_events,
        clinical_codes = dummy_clinical_codes,
        min_max = "min",
        colnames_prefix = "test_",
        workers = 2
      )

    expect_equal(
      sort(names(result[["Disease2"]])),
      sort(c(
        "test_c_test",
        "test_d_test",
        "test_DISEASE2_TEST"
      ))
    )
  }
)

test_that("`extract_phenotypes()` returns expected results", {
  result <- extract_phenotypes(
    clinical_events = dummy_clinical_events,
    clinical_codes = dummy_clinical_codes,
    min_max = "min",
    colnames_prefix = "test_"
  )

  expect_equivalent(
    result$Disease1$test_a_test,
    tibble::tibble(
      eid = c(1, 2),
      test_a_test_min_date = c("1910-02-19", NA),
      test_a_test_indicator = c("Yes", "Yes")
    )
  )

  expect_equivalent(
    result$Disease1$test_b_test,
    tibble::tibble(
      eid = c(1, 2),
      test_b_test_min_date = c("1956-11-24", "1990-10-04"),
      test_b_test_indicator = c("Yes", "Yes")
    )
  )

  expect_equivalent(
    result$Disease1$test_DISEASE1_TEST,
    tibble::tibble(
      eid = c(1, 2),
      test_DISEASE1_TEST_min_date = c("1910-02-19", "1990-10-04"),
      test_DISEASE1_TEST_indicator = c("Yes", "Yes")
    )
  )

  expect_equivalent(
    result$Disease2$test_c_test,
    tibble::tribble(
      ~eid, ~test_c_test_min_date, ~test_c_test_indicator,
      1L, "1998-12-24", "Yes",
      3L, NA, "Yes"
    )
  )

  expect_equivalent(
    result$Disease2$test_d_test,
    tibble::tibble(
      eid = 1,
      test_d_test_min_date = "1956-11-24",
      test_d_test_indicator = "Yes"
    )
  )

  expect_equivalent(
    result$Disease2$test_DISEASE2_TEST,
    tibble::tribble(
      ~eid, ~test_DISEASE2_TEST_min_date, ~test_DISEASE2_TEST_indicator,
      1L, "1956-11-24", "Yes",
      3L, NA, "Yes"
    )
  )
})

test_that(
  "`extract_phenotypes()` returns expected results when certain `data_sources` are specified",
  {
    result <-
      suppressWarnings(
        # some warnings generated when no codes are found for a disease
        extract_phenotypes(
          clinical_events = dummy_clinical_events,
          clinical_codes = dummy_clinical_codes,
          min_max = "min",
          colnames_prefix = "cancer_icd10_",
          data_sources = c("f40006")
        )
      )

    expect_equal(
      names(result$Disease1$cancer_icd10_a_test),
      c("eid", "cancer_icd10_a_test_min_date", "cancer_icd10_a_test_indicator")
    )

    expect_equivalent(
      result$Disease1$cancer_icd10_a_test,
      tibble::tibble(
        eid = 2,
        cancer_icd10_a_test_min_date = as.character(NA),
        cancer_icd10_a_test_indicator = "Yes"
      )
    )

    expect_equivalent(
      result$Disease1$cancer_icd10_a_test,
      result$Disease1$cancer_icd10_DISEASE1_TEST
    )
  }
)

test_that(
  "`extract_phenotypes()` assigns `NULL` to disease categories where no codes are identified for any eids",
  {
    result <-
      suppressWarnings(
        # some warnings generated when no codes are found for a disease
        extract_phenotypes(
          clinical_events = dummy_clinical_events,
          clinical_codes = dummy_clinical_codes,
          min_max = "min",
          colnames_prefix = "test_",
          data_sources = c("f40006")
        )
      )

    expect_null(result$Disease1$test_b_test)
  }
)

# `filter_clinical_events()` ------------------------------------

test_that("`filter_clinical_events()` returns the expected number of rows", {
  expect_equal(
    nrow(
      filter_clinical_events(
        clinical_events = dummy_clinical_events,
        clinical_codes_list = list(
          "read2" = "C108.",
          "read3" = "C108."
        )
      )
    ),
    2
  )

  expect_equal(
    nrow(
      filter_clinical_events(
        clinical_events = dummy_clinical_events_db,
        clinical_codes_list = list("read3" = "C108.")
      )
    ),
    0
  )
})

test_that("`filter_clinical_events()` returns the expected 'sources' for each data coding type", {
  # read2
  expect_equal(sort(unique(
    filter_clinical_events(dummy_clinical_events,
      clinical_codes_list = list("read2" = "C108.")
    )$source
  )),
  expected = "gpc1_r2"
  )

  # read3
  expect_equal(sort(unique(
    filter_clinical_events(dummy_clinical_events,
      clinical_codes_list = list("read3" = "X40J4")
    )$source
  )),
  expected = "gpc3_r3"
  )

  # icd9
  expect_equal(sort(unique(
    filter_clinical_events(dummy_clinical_events,
      clinical_codes_list = list("icd9" = "27134")
    )$source
  )),
  expected = "f40013"
  )

  # icd10
  expect_equal(sort(unique(
    filter_clinical_events(dummy_clinical_events,
      clinical_codes_list = list("icd10" = "W192")
    )$source
  )),
  expected = c("f40002", "f40006")
  )

  # data_coding_6
  expect_equal(sort(unique(
    filter_clinical_events(
      dummy_clinical_events,
      clinical_codes_list = list("data_coding_6" = "1665")
    )$source
  )),
  expected = sort(get_sources_for_code_type("data_coding_6"))
  )

  # data_coding_3
  expect_equal(sort(unique(
    filter_clinical_events(
      dummy_clinical_events,
      clinical_codes_list = list("data_coding_3" = "1045")
    )$source
  )),
  expected = sort(get_sources_for_code_type("data_coding_3"))
  )

  # data_coding_5
  expect_equal(sort(unique(
    filter_clinical_events(
      dummy_clinical_events,
      clinical_codes_list = list("data_coding_5" = "1108")
    )$source
  )),
  expected = sort(get_sources_for_code_type("data_coding_5"))
  )

  # data_coding_4
  expect_equal(sort(unique(
    filter_clinical_events(
      dummy_clinical_events,
      clinical_codes_list = list("data_coding_4" = "1140861958")
    )$source
  )),
  expected = sort(get_sources_for_code_type("data_coding_4"))
  )
})

test_that(
  "`filter_clinical_events()` returns the expected results",
  {
    # icd10
    expect_equivalent(
      tibble::as_tibble(filter_clinical_events(dummy_clinical_events,
        clinical_codes_list = list(
          "icd10" = "W192",
          "read2" = "C108.",
          "read3" = "X40J4"
        )
      )),
      expected = tibble::tribble(
         ~eid,   ~source, ~index,   ~code,        ~date,
           1L,  "f40002",  "0_0",  "W192", "1917-10-08",
           2L,  "f40006",  "2_0",  "W192",           NA,
           1L, "gpc1_r2",    "7", "C108.", "1990-10-01",
           1L, "gpc3_r3",    "9", "X40J4", "1990-10-03",
           1L, "gpc1_r2",   "11", "C108.", "1990-10-03"
         )
    )
  }
)

# `make_self_report_special_decimal_dates_na()` ------------------------------

test_that("`make_self_report_special_decimal_dates_na()` removes special dates from 'date' column (data coding 13, used for field IDs 20006 and 20008: interpolated year of diagnosis for self-reported cancer/non-cancer illnesses)", {
  self_report_non_cancer_diagnoses_unstandardised <- tibble::tribble(
    ~eid, ~f20002, ~f20002_value, ~instance_array, ~f20008, ~date,
    "1", "noncancer_illness_code_selfreported_f20002_0_0", "1665", "0_0", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_0", 1998.1,
    "1", "noncancer_illness_code_selfreported_f20002_0_1", "1532", "0_1", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_1", -1,
    "1", "noncancer_illness_code_selfreported_f20002_0_2", "1552", "0_2", "interpolated_year_when_noncancer_illness_first_diagnosed_f20008_0_2", -3,
  )

  expect_equal(
    make_self_report_special_decimal_dates_na(self_report_non_cancer_diagnoses_unstandardised)$date,
    c(1998.1, NA, NA)
  )
})

# `mutate_age_at_event_cols()` --------------------------------------------

test_that(
  "`mutate_age_at_event_cols()` creates expected age-at-event columns",
  {
    dummy_ukb <- tibble::tribble(
      ~eid, ~dob, ~event_date,
      1, "2000-01-01", "2010-01-01"
    )

    attributes(dummy_ukb$event_date)$label <- "Event date (min)"

    result <- mutate_age_at_event_cols(dummy_ukb,
      dob_col = "dob",
      date_col_regex = "_date$",
      date_col_regex_replacement = "_age"
    )

    expect_equal(
      names(result[4]),
      "event_age"
    )

    expect_equal(as.numeric(result$event_age),
      10,
      tolerance = 1
    )

    expect_equal(
      attributes(result$event_age)$label,
      "Event age"
    )
  }
)

# CLOSE DBI CONNECTION ----------------------------------------------------

DBI::dbDisconnect(con)
