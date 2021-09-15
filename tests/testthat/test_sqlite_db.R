
# SETUP -------------------------------------------------------------------

# file paths
dummy_ukb_main_clinical_events_path <- file.path(tempdir(), "DUMMY_UKB_MAIN_CLINICAL_EVENTS.tsv")
dummy_gp_clinical_path <- file.path(tempdir(), "dummy_gp_clinical_path.tsv")

# write dummy clinical events data to file
readr::write_tsv(DUMMY_UKB_MAIN_CLINICAL_EVENTS,
                 file = dummy_ukb_main_clinical_events_path)

# make dummy gp_clinical data
dummy_gp_clinical <-
  dplyr::bind_rows(
    make_dummy_gp_clinical_df_single_eid(eid = 1,
                                         n = 5,
                                         coding = "read_2"),
    make_dummy_gp_clinical_df_single_eid(eid = 1,
                                         n = 1,
                                         coding = "read_3")
  )

# write dummy gp_clinical data to file
readr::write_tsv(dummy_gp_clinical,
                 file = dummy_gp_clinical_path)

# TESTS -------------------------------------------------------------------

# `tidy_gp_clinical()` --------------------------------------------

# assign `pos` - `tidy_gp_clinical()` looks for this in the parent
# environment, which would normally be that of `file_to_sqlite_db()`
pos <- 2
test_that(
  "`tidy_gp_clinical()` formats dates correctly (and returns expected column names)", {
    dummy_gp_clinical_special_dates_rm <- tidy_gp_clinical(gp_clinical = dummy_gp_clinical,
                                                                    remove_special_dates = TRUE)

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
               c(rep("gpc_r2", 5), "gpc_r3"))
})

# `make_clinical_events_db()` ---------------------------------------------

make_clinical_events_db(ukb_main_path = dummy_ukb_main_clinical_events_path,
                        ukb_main_delim = "\t",
                        gp_clinical_path = dummy_gp_clinical_path,
                        ukb_db_dir = tempdir(),
                        overwrite = TRUE,
                        strict = TRUE)
