
# NOTES -------------------------------------------------------------------


# SETUP -------------------------------------------------------------------


# Make dummy data ---------------------------------------------------------

dummy_clinical_events <- make_dummy_clinical_events_df(eids = 1,
                                                       n_rows = 5)

dummy_gp_clinical <-
  dplyr::bind_rows(
    make_dummy_gp_clinical_df_single_eid(eid = 1,
                                         n = 5,
                                         coding = "read_2"),
    make_dummy_gp_clinical_df_single_eid(eid = 1,
                                         n = 1,
                                         coding = "read_3")
  )

dummy_gp_clinical_special_dates_rm <- gp_clinical_to_sqlite_db(df = dummy_gp_clinical,
                                                               remove_special_dates = TRUE)

# TESTS -------------------------------------------------------------------


# `main_dataset_diagnoses_to_sqlite_db()` ---------------------------------

test_that(
  "`main_dataset_diagnoses_to_sqlite_db()` creates a sqlite table with the correct date formats",
  {
    # create ephemeral in-memory database
    con <- main_dataset_diagnoses_to_sqlite_db(df = dummy_clinical_events,
                                        table = "dummy_clinical_events",
                                        db_path = ":memory:",
                                        overwrite = TRUE,
                                        append = FALSE)

    # get table from database and convert date col to date type
    dummy_clinical_events_db <- dplyr::tbl(con, "dummy_clinical_events") %>%
      dplyr::collect()

    # disconnect from DB
    DBI::dbDisconnect(conn = con)

    # convert date from chracter to date format
    dummy_clinical_events_db$date <- as.Date(dummy_clinical_events_db$date)

    # expected result
    expect_equal(dummy_clinical_events_db,
                 dummy_clinical_events)
  }
)

# `gp_clinical_to_sqlite_db()` --------------------------------------------

test_that(
  "`gp_clinical_to_sqlite_db()` formats dates correctly (and returns expected column names)", {
    # check colnames
    expect_equal(names(dummy_gp_clinical),
                 c("eid", "data_provider", "event_dt", "read_2", "read_3", "value_1", "value_2", "value_3"))

    expect_equal(names(dummy_gp_clinical_special_dates_rm),
                 c("eid", "source", "code", "date"))

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
    expect_equal(dummy_gp_clinical_special_dates_rm$date,
                 c(NA, NA, NA, NA, "1999-02-01", "1999-02-01"))
    }
)

test_that("`gp_clinical_to_sqlite_db() returns the expected values in 'source' column (should match those in `ukbwranglr:::clinical_events_sources$source`)", {
  expect_true(all(dummy_gp_clinical_special_dates_rm$source %in% ukbwranglr:::clinical_events_sources$source))

  expect_equal(dummy_gp_clinical_special_dates_rm$source,
               c(rep("gpc_r2", 5), "gpc_r3"))
})
