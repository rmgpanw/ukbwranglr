
# SETUP -------------------------------------------------------------------

ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

dummy_ukb_data_raw <- tibble::tibble(
  eid = c(1, 2, 3, 4, 5, 6, 7, 8),
  `31-0.0` = c(0, 0, 1, 0, NA, 1, NA, NA),
  `34-0.0` = c(1952, 1946, 1951, 1956, NA, 1948, NA, NA),
  `52-0.0` = c(8, 3, 4, 9, 4, 2, NA, NA),
  `21000-0.0` = c(-1, -3, 1, 1001, 1002, 1003, NA, NA),
  `21000-1.0` = c(2, 2001, 2002, 2003, 2004, 3, 3001, 5),
  `21000-2.0` = c(3003, 3004, -1, 4001, 4002, 4003, 5, 6),
  `20002-0.0` = c(1665, 1383, 1197, 1441, 1429, 1513, NA, NA),
  `21001-0.0` = c(20.1115, 30.1536, 22.8495, 23.4904, 29.2752, 28.2567, NA, NA),
  `21001-1.0` = c(20.864, 20.2309, 26.7929, 25.6826, 19.7576, 30.286, NA, NA),
  `21001-2.0` = c(NA, 27.4936, 27.6286, 37.2294, 14.6641, 27.3534, NA, NA),
)

dummy_ukb_data_raw_path <- file.path(tempdir(), "dummy_ukb_data_raw.csv")

readr::write_csv(dummy_ukb_data_raw, file = dummy_ukb_data_raw_path)

dummy_ukb_data_dict <- make_data_dict(dummy_ukb_data_raw_path,
                                      delim = ",",
                                      ukb_data_dict = ukb_data_dict)
dummy_ukb_data <- read_ukb(dummy_ukb_data_raw_path,
                           delim =  ",",
                           data_dict = dummy_ukb_data_dict,
                           ukb_data_dict = ukb_data_dict,
                           ukb_codings = ukb_codings)

# TESTS -------------------------------------------------------------------

# `derive_dob` ------------------------------------------------------------

test_that("`derive_dob()` returns the expected dates", {
  # warnings generated for any missing yob/mob values in the dummy data - can suppress these
  result <- derive_dob(ukb_main = dummy_ukb_data,
                       ukb_data_dict = ukb_data_dict)

  expect_equal(as.character(result$dob),
               c("1952-08-01", "1946-03-01", "1951-04-01", "1956-09-01", NA, "1948-02-01", NA, NA))

  expect_equal(attributes(result$dob)$label,
               "Date of birth (estimated)")
})

test_that(
  "`derive_dob()` returns the same dates when yob/mob are either type integer or type factor",
  {
    expect_equal(suppressWarnings(
      derive_dob(ukb_main = dummy_ukb_data,
                 ukb_data_dict = ukb_data_dict)
    )$dob,
    suppressWarnings(
      derive_dob(
        ukb_main = dummy_ukb_data %>%
          haven::as_factor(),
        ukb_data_dict = ukb_data_dict
      )
    )$dob)
  }
)

# `derive_ethnic_background_simplified` -----------------------------------

test_that(
  "`derive_ethnic_background_simplified()` returns expected results", {
    ethnic_background_simplified <- derive_ethnic_background_simplified(ukb_main = haven::as_factor(dummy_ukb_data),
                                        ukb_data_dict = ukb_data_dict)

    expect_equal(as.character(ethnic_background_simplified$ethnic_background_simplified),
                 c("Mixed",
                   "Mixed",
                   "White",
                   "White",
                   "White",
                   "White",
                   "Asian or Asian British",
                   "Chinese"))
  }
)

test_that(
  "`derive_ethnic_background_simplified()` `.drop` argument works as expected", {
    ethnic_background_simplified <- derive_ethnic_background_simplified(ukb_main = haven::as_factor(dummy_ukb_data),
                                                                        ukb_data_dict = ukb_data_dict,
                                                                        .drop = TRUE)

    expect_true(all(!c("ethnic_background_f21000_0_0",
                       "ethnic_background_f21000_1_0",
                       "ethnic_background_f21000_2_0") %in% names(ethnic_background_simplified)))

    expect_true("ethnic_background_simplified" %in% names(ethnic_background_simplified))
  }
)
