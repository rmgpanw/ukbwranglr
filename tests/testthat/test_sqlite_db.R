
# SETUP -------------------------------------------------------------------
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# file paths
dummy_ukb_main_clinical_events_path <- file.path(tempdir(), "DUMMY_UKB_MAIN_CLINICAL_EVENTS.tsv")
dummy_gp_clinical_path <- file.path(tempdir(), "dummy_gp_clinical_path.tsv")

# write dummy clinical events data to file
readr::write_tsv(DUMMY_UKB_MAIN_CLINICAL_EVENTS,
                 file = dummy_ukb_main_clinical_events_path)

# make dummy gp_clinical data
dummy_gp_clinical <- tibble::tibble(
  eid = c(1, 1, 1, 3, 4, 8),
  data_provider = c('1', '4', '3', '1', '2', '1'),
  event_dt = c('03/03/1903', '01/01/1901', '07/07/2037', '07/07/2037', '01/02/1999', '01/02/1999'),
  read_2 = c('C', 'A', 'E', 'E', 'J', NA),
  read_3 = c(NA, NA, NA, NA, NA, 'G'),
  value1 = c('1', '1', '1', '1', '1', '1'),
  value2 = c('2', '2', '2', '2', '2', '2'),
  value3 = c('3', '3', '3', '3', '3', '3')
)

# write dummy gp_clinical data to file
readr::write_tsv(dummy_gp_clinical,
                 file = dummy_gp_clinical_path)

# TESTS -------------------------------------------------------------------

# `tidy_gp_clinical_db()` --------------------------------------------

# assign `pos` - `tidy_gp_clinical_db()` looks for this in the parent
# environment, which would normally be that of `file_to_sqlite_db()`
test_that(
  "`tidy_gp_clinical_db()` formats dates correctly (and returns expected column names)", {
    dummy_gp_clinical_special_dates_rm <- tidy_gp_clinical_db(gp_clinical = dummy_gp_clinical,
                                                                    remove_special_dates = TRUE,
                                                              pos = 2)

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

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column", {
  expect_true(all(dummy_gp_clinical_special_dates_rm$clinical_events$source %in% CLINICAL_EVENTS_SOURCES$source))

  expect_equal(dummy_gp_clinical_special_dates_rm$clinical_events$source,
               c("gpc1_r2",
                 "gpc4_r2",
                 "gpc3_r2",
                 "gpc1_r2",
                 "gpc2_r2",
                 "gpc1_r3"))
})

# `make_clinical_events_db()` ---------------------------------------------

test_that("`make_clinical_events_db()` works", {
  dummy_ukb_db_path <- file.path(tempdir(), "ukb.db")

  suppressWarnings(make_clinical_events_db(
    ukb_main_path = dummy_ukb_main_clinical_events_path,
    ukb_main_delim = "\t",
    gp_clinical_path = dummy_gp_clinical_path,
    ukb_db_path = dummy_ukb_db_path,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    overwrite = TRUE,
    strict = TRUE,
    chunk_size = 2
  ))

  con <- DBI::dbConnect(RSQLite::SQLite(), dummy_ukb_db_path)
  ukbdb <- db_list_tables(con)

  expect_equal(names(ukbdb),
               c("clinical_events", "gp_clinical_values"))

  expect_error(
    make_clinical_events_db(
    ukb_main_path = dummy_ukb_main_clinical_events_path,
    ukb_main_delim = "\t",
    gp_clinical_path = dummy_gp_clinical_path,
    ukb_db_path = dummy_ukb_db_path,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    overwrite = FALSE,
    strict = TRUE
  ),
  "Error! The following table")

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

  expect_equivalent(
    gp_clinical_events,
    tibble::tibble(
      eid = c(1, 1, 1, 3, 4, 8),
      source = c('gpc1_r2', 'gpc4_r2', 'gpc3_r2', 'gpc1_r2', 'gpc2_r2', 'gpc1_r3'),
      index = c('1', '2', '3', '4', '5', '6'),
      code = c('C', 'A', 'E', 'E', 'J', 'G'),
      date = c(NA, NA, NA, NA, '1999-02-01', '1999-02-01'),
    )
  )

  expect_equivalent(
    ukbdb$gp_clinical_values %>%
      dplyr::collect(),
    tibble::tibble(
      as.character(1:nrow(dummy_gp_clinical)),
      dummy_gp_clinical$value1,
      dummy_gp_clinical$value2,
      dummy_gp_clinical$value3
    )
  )
})
