
# SETUP -------------------------------------------------------------------

ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()
dummy_ukb_data_path <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")
dummy_ukb_data_dict <- make_data_dict(dummy_ukb_data_path, delim = ",", ukb_data_dict = ukb_data_dict)
dummy_ukb_data <- read_ukb(dummy_ukb_data_path,
                           delim =  ",",
                           data_dict = dummy_ukb_data_dict,
                           ukb_data_dict = ukb_data_dict,
                           ukb_codings = ukb_codings)


# TESTS -------------------------------------------------------------------

# `mutate_dob` ------------------------------------------------------------

test_that("`derive_dob()` returns the expected dates", {
  # warnings generated for any missing yob/mob values in the dummy data - can suppress these
  result <- head(
    derive_dob(ukb_main = dummy_ukb_data,
               ukb_data_dict = ukb_data_dict)
  )
  expect_equal(result$dob_derived,
               c("1952-08-01", "1946-03-01", "1951-04-01", "1956-09-01", NA, "1948-02-01"))
})

test_that(
  "`derive_dob()` returns the same dates when yob/mob are either type integer or type factor",
  {
    expect_equal(head(suppressWarnings(
      derive_dob(ukb_main = dummy_ukb_data,
                 ukb_data_dict = ukb_data_dict)
    ))$dob_derived,
    head(suppressWarnings(
      derive_dob(
        ukb_main = dummy_ukb_data %>%
          haven::as_factor(),
        ukb_data_dict = ukb_data_dict
      )
    ))$dob_derived)
  }
)
