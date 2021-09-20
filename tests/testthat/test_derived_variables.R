
# SETUP -------------------------------------------------------------------

ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

dummy_ukb_data_raw <- tibble::tibble(
  eid = c(1, 2, 3, 4, 5, 6),
  `31-0.0` = c(0, 0, 1, 0, NA, 1),
  `34-0.0` = c(1952, 1946, 1951, 1956, NA, 1948),
  `52-0.0` = c(8, 3, 4, 9, 4, 2),
  `21000-0.0` = c(NA, 4001, 3, NA, -3, 3),
  `20002-0.0` = c(1665, 1383, 1197, 1441, 1429, 1513),
  `21001-0.0` = c(20.1115, 30.1536, 22.8495, 23.4904, 29.2752, 28.2567),
  `21001-1.0` = c(20.864, 20.2309, 26.7929, 25.6826, 19.7576, 30.286),
  `21001-2.0` = c(NA, 27.4936, 27.6286, 37.2294, 14.6641, 27.3534),
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

# `mutate_dob` ------------------------------------------------------------

test_that("`derive_dob()` returns the expected dates", {
  # warnings generated for any missing yob/mob values in the dummy data - can suppress these
  result <- derive_dob(ukb_main = dummy_ukb_data,
                       ukb_data_dict = ukb_data_dict)

  expect_equal(result$dob_derived,
               c("1952-08-01", "1946-03-01", "1951-04-01", "1956-09-01", NA, "1948-02-01"))
})

test_that(
  "`derive_dob()` returns the same dates when yob/mob are either type integer or type factor",
  {
    expect_equal(suppressWarnings(
      derive_dob(ukb_main = dummy_ukb_data,
                 ukb_data_dict = ukb_data_dict)
    )$dob_derived,
    suppressWarnings(
      derive_dob(
        ukb_main = dummy_ukb_data %>%
          haven::as_factor(),
        ukb_data_dict = ukb_data_dict
      )
    )$dob_derived)
  }
)
