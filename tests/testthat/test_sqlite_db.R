
# SETUP -------------------------------------------------------------------
dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
dummy_gp_clinical <- get_ukb_dummy("dummy_gp_clinical.txt")
dummy_gp_scripts <- get_ukb_dummy("dummy_gp_scripts.txt")

# file paths
dummy_ukb_main_path <- get_ukb_dummy("dummy_ukb_main.tsv",
  path_only = TRUE
)
dummy_gp_clinical_path <- get_ukb_dummy("dummy_gp_clinical.txt",
  path_only = TRUE
)
dummy_gp_scripts_path <- get_ukb_dummy("dummy_gp_scripts.txt",
  path_only = TRUE
)
dummy_ukb_db_path <- file.path(tempdir(), "ukb.db")

# tidy gp_clinical
dummy_gp_clinical_tidy <-
  tidy_gp_data_db(
    gp_df = dummy_gp_clinical,
    gp_df_type = "gp_clinical",
    pos = 2
  )

# tidy gp_scripts
dummy_gp_scripts_tidy <-
  tidy_gp_data_db(
    gp_df = dummy_gp_scripts,
    gp_df_type = "gp_scripts",
    pos = 4
  )

# make clinical events db
suppressWarnings(make_clinical_events_db(
  ukb_main_path = dummy_ukb_main_path,
  ukb_main_delim = "\t",
  gp_clinical_path = dummy_gp_clinical_path,
  gp_scripts_path = dummy_gp_scripts_path,
  ukb_db_path = dummy_ukb_db_path,
  ukb_data_dict = dummy_ukb_data_dict,
  ukb_codings = dummy_ukb_codings,
  overwrite = TRUE,
  chunk_size = 2
))

con <- DBI::dbConnect(RSQLite::SQLite(), dummy_ukb_db_path)
ukbdb <- db_tables_to_list(con)

# TESTS -------------------------------------------------------------------

# `tidy_gp_data_db()` --------------------------------------------

test_that(
  "`tidy_gp_data_db()` formats dates correctly (and returns expected column names) for 'gp_clinical'",
  {
    # check colnames
    expect_equal(
      names(dummy_gp_clinical),
      c("eid", "data_provider", "event_dt", "read_2", "read_3", "value1", "value2", "value3")
    )

    expect_equal(
      names(dummy_gp_clinical_tidy),
      c("clinical_events", "gp_clinical_values")
    )

    expect_equal(
      names(dummy_gp_clinical_tidy$clinical_events),
      c("eid", "source", "index", "code", "date")
    )

    expect_equal(
      names(dummy_gp_clinical_tidy$gp_clinical_values),
      c("index", "value1", "value2", "value3")
    )

    # check special dates exist in dummy_gp_clinical
    expect_equal(
      dummy_gp_clinical$event_dt,
      c(
        "03/03/1903",
        "01/01/1901",
        "07/07/2037",
        "07/07/2037",
        "01/02/1999",
        "01/02/1999",
        "01/10/1990",
        "02/10/1990",
        "03/10/1990",
        "04/10/1990",
        "03/10/1990",
        "04/10/1990"
      )
    )

    # check special dates are *not* removed in dummy_gp_clinical_tidy and
    # remaining dates are otherwise in the desired format
    expect_equal(
      dummy_gp_clinical_tidy$clinical_events$date,
      c(
        "1903-03-03",
        "1901-01-01",
        "2037-07-07",
        "2037-07-07",
        "1999-02-01",
        "1999-02-01",
        "1990-10-01",
        "1990-10-02",
        "1990-10-03",
        "1990-10-04",
        "1990-10-03",
        "1990-10-04"
      )
    )

    # check values in `source` col
    expect_equal(
      sort(
        unique(dummy_gp_clinical_tidy$clinical_events$source)
      ),
      sort(
        c(
          "gpc1_r2",
          "gpc2_r2",
          "gpc3_r3",
          "gpc4_r2"
        )
      )
    )

    # check `index` col
    expect_equal(
      dummy_gp_clinical_tidy$clinical_events$index,
      as.character(2:13)
    )
  }
)

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column for 'gp_clinical'", {
  expect_true(all(dummy_gp_clinical_tidy$clinical_events$source %in% CLINICAL_EVENTS_SOURCES$source))
})

test_that(
  "`tidy_gp_data_db()` formats dates correctly (and returns expected column names) for 'gp_scripts'",
  {
    # check colnames
    expect_equal(
      names(dummy_gp_scripts),
      c(
        "eid",
        "data_provider",
        "issue_date",
        "read_2",
        "bnf_code",
        "dmd_code",
        "drug_name",
        "quantity"
      )
    )

    expect_equal(
      names(dummy_gp_scripts_tidy),
      c("clinical_events", "gp_scripts_names_and_quantities")
    )

    expect_equal(
      names(dummy_gp_scripts_tidy$clinical_events),
      c("eid", "source", "index", "code", "date")
    )

    expect_equal(
      names(dummy_gp_scripts_tidy$gp_scripts_names_and_quantities),
      c("index", "drug_name", "quantity")
    )

    # check special dates exist in dummy_gp_scripts
    expect_equal(
      dummy_gp_scripts$issue_date,
      c(
        "03/03/1903",
        "01/01/1901",
        "07/07/2037",
        "07/07/2037",
        "01/02/1999",
        "01/02/1999"
      )
    )

    # check special dates are *not* removed in dummy_gp_scripts_tidy and
    # remaining dates are otherwise in the desired format
    expect_equivalent(
      dummy_gp_scripts_tidy$clinical_events,
      tibble::tribble(
         ~eid,    ~source, ~index,            ~code,        ~date,
           1L,  "gps1_r2",    "4",         "bxi300", "1903-03-03",
           1L, "gps1_dmd",    "4",              "1", "1903-03-03",
           1L,  "gps4_r2",    "5",           "bxi3", "1901-01-01",
           1L, "gps3_bnf",    "6", "02.02.01.00.00", "2037-07-07",
           3L,  "gps1_r2",    "7",         "bd3j00", "2037-07-07",
           3L, "gps1_dmd",    "7",              "1", "2037-07-07",
           4L,  "gps2_r2",    "8",           "bd3j", "1999-02-01",
           4L, "gps2_bnf",    "8",       "02020100", "1999-02-01",
           8L, "gps1_dmd",    "9",              "1", "1999-02-01"
         )

    )
  }
)

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column for 'gp_scripts'", {
  expect_true(all(dummy_gp_scripts_tidy$clinical_events$source %in% CLINICAL_EVENTS_SOURCES$source))
})

# `make_clinical_events_db()` ---------------------------------------------

test_that("`make_clinical_events_db()` works", {
  expect_equal(
    names(ukbdb),
    c("clinical_events", "gp_clinical_values", "gp_scripts_names_and_quantities")
  )

  # gp_clinical clinical_events
  gp_clinical_events <- ukbdb$clinical_events %>%
    dplyr::filter(source %in% c(
      "gpc1_r2",
      "gpc2_r2",
      "gpc4_r2",
      "gpc3_r3"
    )) %>%
    dplyr::collect() %>%
    dplyr::arrange(as.numeric(index))

  expect_equivalent(
    gp_clinical_events,
    dummy_gp_clinical_tidy$clinical_events %>%
      dplyr::mutate(index = as.character(dplyr::row_number()))
  )

  expect_equivalent(
    ukbdb$gp_clinical_values %>%
      dplyr::collect(),
    tibble::tribble(
      ~index, ~value1, ~value2, ~value3,
      "1", "1", "2", "3",
      "2", "1", "2", "3",
      "3", "1", "2", "3",
      "4", "1", "2", "3",
      "5", "1", "2", "3",
      "6", "1", "2", "3",
      "7", NA, NA, NA,
      "8", NA, NA, NA,
      "9", NA, NA, NA,
      "10", NA, NA, NA,
      "11", NA, NA, NA,
      "12", NA, NA, NA
    )
  )

  # gp_scripts clinical_events
  gp_scripts_events <- ukbdb$clinical_events %>%
    dplyr::filter(source %in% c(
      "gps1_r2",
      "gps1_dmd",
      "gps2_r2",
      "gps2_bnf",
      "gps3_bnf",
      "gps4_r2"
    )) %>%
    dplyr::collect() %>%
    dplyr::arrange(as.numeric(index))

  expect_equivalent(
    gp_scripts_events,
    tibble::tribble(
       ~eid,    ~source, ~index,            ~code,        ~date,
         1L,  "gps1_r2",    "1",         "bxi300", "1903-03-03",
         1L, "gps1_dmd",    "1",              "1", "1903-03-03",
         1L,  "gps4_r2",    "2",           "bxi3", "1901-01-01",
         1L, "gps3_bnf",    "3", "02.02.01.00.00", "2037-07-07",
         3L,  "gps1_r2",    "4",         "bd3j00", "2037-07-07",
         3L, "gps1_dmd",    "4",              "1", "2037-07-07",
         4L,  "gps2_r2",    "5",           "bd3j", "1999-02-01",
         4L, "gps2_bnf",    "5",       "02020100", "1999-02-01",
         8L, "gps1_dmd",    "6",              "1", "1999-02-01"
       )

  )

  expect_equivalent(
    ukbdb$gp_scripts_names_and_quantities %>%
      dplyr::collect(),
    tibble::tribble(
      ~index, ~drug_name, ~quantity,
      "1", "drug2", "50",
      "2", NA, NA,
      "3", "drug2", "30",
      "4", "drug2", "30",
      "5", "drug2", "30",
      "6", "2", "30"
    )
  )
})

test_that("`make_clinical_events_db()` raises an error if table already exist", {
  expect_error(
    make_clinical_events_db(
      ukb_main_path = dummy_ukb_main_path,
      ukb_main_delim = "\t",
      gp_clinical_path = dummy_gp_clinical_path,
      gp_scripts_path = dummy_gp_scripts_path,
      ukb_db_path = dummy_ukb_db_path,
      ukb_data_dict = ukb_data_dict,
      ukb_codings = ukb_codings,
      overwrite = FALSE
    ),
    "Error! The following table"
  )
})

