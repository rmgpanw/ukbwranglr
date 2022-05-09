
# SETUP -------------------------------------------------------------------
dummy_gp_clinical <- get_ukb_dummy("dummy_gp_clinical.txt")
dummy_gp_clinical_events_tidy <- tidy_gp_clinical(dummy_gp_clinical)$clinical_events

# TESTS -------------------------------------------------------------------

# eid is type integer -------------------------------------------

test_that("'eid' column is type integer", {
  expect_true(is.integer(dummy_gp_clinical$eid))
})

# `dummy_gp_clinical.txt` -------------------------------------------------

test_that("Tidied `dummy_gp_clinical.txt` has valid data sources", {
  expect_true(all(dummy_gp_clinical_events_tidy$source %in% CLINICAL_EVENTS_SOURCES$source))
})

