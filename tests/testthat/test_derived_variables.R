
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

# TESTS -------------------------------------------------------------------

# `derive_dob` ------------------------------------------------------------

test_that("`derive_dob()` returns the expected dates", {
  # warnings generated for any missing yob/mob values in the dummy data - can suppress these
  result <- derive_dob(
    ukb_main = dummy_ukb_main,
    ukb_data_dict = dummy_ukb_data_dict
  )

  expect_equal(
    as.character(result$dob),
    c(
      "1952-08-01",
      "1946-03-01",
      "1951-04-01",
      "1956-09-01",
      NA,
      "1948-02-01",
      "1949-12-01",
      "1956-10-01",
      "1962-04-01",
      "1953-02-01"
    )
  )

  expect_equal(
    attributes(result$dob)$label,
    "Date of birth (estimated)"
  )
})

test_that(
  "`derive_dob()` returns the same dates when yob/mob are either type integer or type factor",
  {
    expect_equal(
      derive_dob(
        ukb_main = dummy_ukb_main %>%
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(
              c(
                "year_of_birth_f34_0_0",
                "month_of_birth_f52_0_0"
              )
            ),
            as.factor
          )),
        ukb_data_dict = dummy_ukb_data_dict
      )$dob,
      derive_dob(
        ukb_main = dummy_ukb_main %>%
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(
              c(
                "year_of_birth_f34_0_0",
                "month_of_birth_f52_0_0"
              )
            ),
            as.integer
          )),
        ukb_data_dict = dummy_ukb_data_dict
      )$dob
    )
  }
)

# `derive_ethnic_background_simplified` -----------------------------------

test_that("`derive_ethnic_background_simplified()` returns expected results", {
  ethnic_background_simplified <-
    derive_ethnic_background_simplified(
      ukb_main = dummy_ukb_main,
      ukb_data_dict = dummy_ukb_data_dict
    )

  expect_equal(
    as.character(
      ethnic_background_simplified$ethnic_background_simplified
    ),
    c(
      "Mixed",
      "Mixed",
      "White",
      "White",
      "White",
      "White",
      "Asian or Asian British",
      "Chinese",
      "Black or Black British",
      "Black or Black British"
    )
  )
})

test_that("`derive_ethnic_background_simplified()` `.drop` argument works as expected", {
  ethnic_background_simplified <-
    derive_ethnic_background_simplified(
      ukb_main = dummy_ukb_main,
      ukb_data_dict = dummy_ukb_data_dict,
      .drop = TRUE
    )

  expect_true(all(
    !c(
      "ethnic_background_f21000_0_0",
      "ethnic_background_f21000_1_0",
      "ethnic_background_f21000_2_0"
    ) %in% names(ethnic_background_simplified)
  ))

  expect_true("ethnic_background_simplified" %in% names(ethnic_background_simplified))
})
