
# SETUP -------------------------------------------------------------------
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# file paths
dummy_ukb_main_clinical_events_path <- file.path(tempdir(), "DUMMY_UKB_MAIN_CLINICAL_EVENTS.tsv")
dummy_gp_clinical_path <- file.path(tempdir(), "dummy_gp_clinical_path.tsv")
dummy_gp_scripts_path <- file.path(tempdir(), "dummy_gp_scripts_path.tsv")
dummy_ukb_db_path <- file.path(tempdir(), "ukb.db")

# write dummy clinical events data to file
readr::write_tsv(DUMMY_UKB_MAIN_CLINICAL_EVENTS,
                 file = dummy_ukb_main_clinical_events_path)

# make dummy gp_clinical data
dummy_gp_clinical <- tibble::tibble(
  eid = c(1, 1, 1, 3, 4, 8),
  data_provider = c('1', '4', '3', '1', '2', '1'),
  event_dt = c(
    '03/03/1903',
    '01/01/1901',
    '07/07/2037',
    '07/07/2037',
    '01/02/1999',
    '01/02/1999'
  ),
  read_2 = c('C', 'A', 'E', 'E', 'J', NA),
  read_3 = c(NA, NA, NA, NA, NA, 'G'),
  value1 = c('1', '1', '1', '1', '1', '1'),
  value2 = c('2', '2', '2', '2', '2', '2'),
  value3 = c('3', '3', '3', '3', '3', '3')
)

# write dummy gp_clinical data to file
readr::write_tsv(dummy_gp_clinical,
                 file = dummy_gp_clinical_path)

# make dummy gp_scripts data
dummy_gp_scripts <- tibble::tibble(
  eid = c(1, 1, 1, 3, 4, 8),
  data_provider = c('1', '4', '3', '1', '2', '1'),
  issue_date = c(
    '03/03/1903',
    '01/01/1901',
    '07/07/2037',
    '07/07/2037',
    '01/02/1999',
    '01/02/1999'
  ),
  read_2 = c('bxi300', 'bxi3', NA, 'bd3j00', 'bd3j', NA),
  bnf_code = c(NA, NA, "02.02.01.00.00", NA, "02020100", NA),
  dmd_code = c('1', NA, NA, '1', NA, '1'),
  drug_name = c('drug2', NA, 'drug2', 'drug2', 'drug2', '2'),
  quantity = c('50', NA, '30', '30', '30', '30')
)

# write dummy gp_scripts data to file
readr::write_tsv(dummy_gp_scripts,
                 file = dummy_gp_scripts_path)

# tidy gp_clinical
dummy_gp_clinical_special_dates_rm <-
  tidy_gp_data_db(
    gp_df = dummy_gp_clinical,
    gp_df_type = "gp_clinical",
    remove_special_dates = TRUE,
    pos = 2
  )

# tidy gp_scripts
dummy_gp_scripts_special_dates_rm <-
  tidy_gp_data_db(
    gp_df = dummy_gp_scripts,
    gp_df_type = "gp_scripts",
    remove_special_dates = TRUE,
    pos = 4
  )

# make clinical events db
suppressWarnings(make_clinical_events_db(
  ukb_main_path = dummy_ukb_main_clinical_events_path,
  ukb_main_delim = "\t",
  gp_clinical_path = dummy_gp_clinical_path,
  gp_scripts_path = dummy_gp_scripts_path,
  ukb_db_path = dummy_ukb_db_path,
  ukb_data_dict = ukb_data_dict,
  ukb_codings = ukb_codings,
  overwrite = TRUE,
  chunk_size = 2
))

con <- DBI::dbConnect(RSQLite::SQLite(), dummy_ukb_db_path)
ukbdb <- db_tables_to_list(con)

# TESTS -------------------------------------------------------------------

# `tidy_gp_data_db()` --------------------------------------------

test_that(
  "`tidy_gp_data_db()` formats dates correctly (and returns expected column names) for 'gp_clinical'", {
    # check colnames
    expect_equal(names(dummy_gp_clinical),
                 c("eid", "data_provider", "event_dt", "read_2", "read_3", "value1", "value2", "value3"))

    expect_equal(names(dummy_gp_clinical_special_dates_rm),
                 c("clinical_events", "gp_clinical_values"))

    expect_equal(names(dummy_gp_clinical_special_dates_rm$clinical_events),
                 c("eid", "source", "index", "code", "date"))

    expect_equal(names(dummy_gp_clinical_special_dates_rm$gp_clinical_values),
                 c("index", "value1", "value2", "value3"))

    # check special dates exist in dummy_gp_clinical
    expect_equal(dummy_gp_clinical$event_dt,
                 c("03/03/1903",
                   "01/01/1901",
                   "07/07/2037",
                   "07/07/2037",
                   "01/02/1999",
                   "01/02/1999"))

    # check special dates are removed in dummy_gp_clinical_special_dates_rm and
    # remaining dates are otherwise in the desired format
    expect_equal(dummy_gp_clinical_special_dates_rm$clinical_events$date,
                 c(NA, NA, NA, NA, "1999-02-01", "1999-02-01"))

    # check values in `source` col
    expect_equal(dummy_gp_clinical_special_dates_rm$clinical_events$source,
                 c('gpc1_r2',
                   'gpc4_r2',
                   'gpc3_r2',
                   'gpc1_r2',
                   'gpc2_r2',
                   'gpc1_r3'))

    # check `index` col
    expect_equal(dummy_gp_clinical_special_dates_rm$clinical_events$index,
                 c('2',
                   '3',
                   '4',
                   '5',
                   '6',
                   '7'))
    }
)

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column for 'gp_clinical'", {
  expect_true(all(dummy_gp_clinical_special_dates_rm$clinical_events$source %in% CLINICAL_EVENTS_SOURCES$source))
})

test_that(
  "`tidy_gp_data_db()` formats dates correctly (and returns expected column names) for 'gp_scripts'", {
    # check colnames
    expect_equal(names(dummy_gp_scripts),
                 c("eid", "data_provider", "issue_date", "read_2", "bnf_code", "dmd_code", "drug_name", "quantity"))

    expect_equal(names(dummy_gp_scripts_special_dates_rm),
                 c("clinical_events", "gp_scripts_names_and_quantities"))

    expect_equal(names(dummy_gp_scripts_special_dates_rm$clinical_events),
                 c("eid", "source", "index", "code", "date"))

    expect_equal(names(dummy_gp_scripts_special_dates_rm$gp_scripts_names_and_quantities),
                 c("index", "drug_name", "quantity"))

    # check special dates exist in dummy_gp_clinical
    expect_equal(dummy_gp_scripts$issue_date,
                 c("03/03/1903",
                   "01/01/1901",
                   "07/07/2037",
                   "07/07/2037",
                   "01/02/1999",
                   "01/02/1999"))

    # check special dates are removed in dummy_gp_clinical_special_dates_rm and
    # remaining dates are otherwise in the desired format
    expect_equal(dummy_gp_clinical_special_dates_rm$clinical_events$date,
                 c(NA, NA, NA, NA, "1999-02-01", "1999-02-01"))

    # check values in `source` col
    expect_equal(dummy_gp_scripts_special_dates_rm$clinical_events$source,
                 c('gps1_r2',
                   'gps1_dmd',
                   'gps4_r2',
                   'gps3_bnf',
                   'gps1_r2',
                   'gps1_dmd',
                   'gps2_r2',
                   'gps2_bnf',
                   'gps1_dmd'))

    # check `index` col
    expect_equal(dummy_gp_scripts_special_dates_rm$clinical_events$index,
                 c('4',
                   '4',
                   '5',
                   '6',
                   '7',
                   '7',
                   '8',
                   '8',
                   '9'))
  }
)

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column for 'gp_scripts'", {
  expect_true(all(dummy_gp_scripts_special_dates_rm$clinical_events$source %in% CLINICAL_EVENTS_SOURCES$source))
})

# `make_clinical_events_db()` ---------------------------------------------

test_that("`make_clinical_events_db()` works", {
    expect_equal(names(ukbdb),
               c("clinical_events", "gp_clinical_values", "gp_scripts_names_and_quantities"))

  # gp_clinical clinical_events
  gp_clinical_events <- ukbdb$clinical_events %>%
    dplyr::filter(source %in% c(
      'gpc1_r2',
      'gpc2_r2',
      'gpc3_r2',
      'gpc4_r2',
      'gpc1_r3',
      'gpc2_r3',
      'gpc3_r3',
      'gpc4_r3'
    )) %>%
    dplyr::collect() %>%
    dplyr::arrange(index)

  expect_equal(
    gp_clinical_events$eid,
    c(1, 1, 1, 3, 4, 8)
  )

  expect_equal(
    gp_clinical_events$source,
    c('gpc1_r2', 'gpc4_r2', 'gpc3_r2', 'gpc1_r2', 'gpc2_r2', 'gpc1_r3')
  )

  expect_equal(
    gp_clinical_events$index,
    c('1', '2', '3', '4', '5', '6')
  )

  expect_equal(
    gp_clinical_events$code,
    c('C', 'A', 'E', 'E', 'J', 'G'),
  )

  expect_equal(
    gp_clinical_events$date,
    c(NA, NA, NA, NA, '1999-02-01', '1999-02-01'),
  )

  # expect_equivalent(
  #   gp_clinical_events,
  #   tibble::tibble(
  #     eid = c(1, 1, 1, 3, 4, 8),
  #     source = c('gpc1_r2', 'gpc4_r2', 'gpc3_r2', 'gpc1_r2', 'gpc2_r2', 'gpc1_r3'),
  #     index = c('1', '2', '3', '4', '5', '6'),
  #     code = c('C', 'A', 'E', 'E', 'J', 'G'),
  #     date = c(NA, NA, NA, NA, '1999-02-01', '1999-02-01'),
  #   )
  # )

  expect_equivalent(
    ukbdb$gp_clinical_values %>%
      dplyr::collect(),
    tibble::tibble(
      index = c('1', '2', '3', '4', '5', '6'),
      value1 = c('1', '1', '1', '1', '1', '1'),
      value2 = c('2', '2', '2', '2', '2', '2'),
      value3 = c('3', '3', '3', '3', '3', '3')
    )
  )

  # gp_scripts clinical_events
  gp_scripts_events <- ukbdb$clinical_events %>%
    dplyr::filter(source %in% c(
      'gps1_r2',
      'gps1_dmd',
      'gps2_r2',
      'gps2_bnf',
      'gps3_bnf',
      'gps4_r2'
    )) %>%
    dplyr::collect() %>%
    dplyr::arrange(index,
                   eid,
                   source,
                   code,
                   date)

  expect_equivalent(
    gp_scripts_events,
    tibble::tibble(
      eid = c(1, 1, 1, 1, 3, 3, 4, 4, 8),
      source = c('gps1_dmd', 'gps1_r2', 'gps4_r2', 'gps3_bnf', 'gps1_dmd', 'gps1_r2', 'gps2_bnf', 'gps2_r2', 'gps1_dmd'),
      index = c('1', '1', '2', '3', '4', '4', '5', '5', '6'),
      code = c('1', 'bxi300', 'bxi3', '02.02.01.00.00', '1', 'bd3j00', '02020100', 'bd3j', '1'),
      date = c(NA, NA, NA, NA, NA, NA, '1999-02-01', '1999-02-01', '1999-02-01'),
    )
  )

  expect_equivalent(
    ukbdb$gp_scripts_names_and_quantities %>%
      dplyr::collect(),
    tibble::tibble(
      index = as.character(1:nrow(dummy_gp_scripts)),
      drug_name = dummy_gp_scripts$drug_name,
      quantity = dummy_gp_scripts$quantity
    )
  )
})

test_that("`make_clinical_events_db()` raises an error if table already exist", {
  expect_error(
    make_clinical_events_db(
      ukb_main_path = dummy_ukb_main_clinical_events_path,
      ukb_main_delim = "\t",
      gp_clinical_path = dummy_gp_clinical_path,
      gp_scripts_path = dummy_gp_scripts_path,
      ukb_db_path = dummy_ukb_db_path,
      ukb_data_dict = ukb_data_dict,
      ukb_codings = ukb_codings,
      overwrite = FALSE
    ),
    "Error! The following table")
})

