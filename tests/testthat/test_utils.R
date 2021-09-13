
# CONSTANTS ---------------------------------------------------------------

ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()
dummy_ukb_data_path <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")
dummy_ukb_data_dict <- make_data_dict(dummy_ukb_data_path, delim = ",", ukb_data_dict = ukb_data_dict)
dummy_ukb_data <- read_pheno(dummy_ukb_data_path, delim =  ",", data_dict = dummy_ukb_data_dict, ukb_data_dict = ukb_data_dict, ukb_codings = ukb_codings)

# named vector
dict <- letters[6:10]
names(dict) <- letters[1:5]

bad_dict <- dict
names(bad_dict) <- rep("A", 5)

# test_df
test_df <- make_dummy_df()

# TESTS -------------------------------------------------------------------

# assert_integer_ge_n() -----------------------------------------------------------

test_that("assert_integer_ge_n (helper function) raises an error if chunk_size is not an integer or is < 1", {
  expect_error(
    assert_integer_ge_n(0, "chunk_size", n = 1),
    regexp = "Error! chunk_size must be an integer that is greater than 1")

  expect_error(
    assert_integer_ge_n(1.3, "chunk_size", n = 2),
    regexp = "Error! chunk_size must be an integer that is greater than 2")
})

# rename_cols -------------------------------------------------------------

# setup
df <- data.frame(x = 1:3,
                 y = 4:6)
df_renamed1 <- rename_cols(df, "x", "x2")

df_renamed2 <- rename_cols(df, c("y", "x"), c("y2", "x2"))

df_renamed3 <- rename_cols(df, c("y", "x"), c("x2", "y2"))

df_duplicate_colnames <- df
names(df_duplicate_colnames) <- c("x", "x")

# tests
test_that("`rename_cols()` correctly renames columns", {

  expect_equal(names(df_renamed1),
               c("x2", "y"))

  expect_equal(names(df_renamed2),
               c("x2", "y2"))

  expect_equal(names(df_renamed3),
               c("y2", "x2"))
})

test_that("`rename_cols()` raises appropriate errors for invalid arguments", {
  expect_error(rename_cols(df, "z", "x2"),
               "Error! `old_colnames` contains values that are not present in `names\\(df\\)`")

  expect_error(rename_cols(df, c("x", "y"), c("x", "x")),
               "Error! `old_colnames` and `new_colnames` must contain unique values only and be of the same length")

  expect_error(rename_cols(df, c("x", "y"), "x"),
               "Error! `old_colnames` and `new_colnames` must contain unique values only and be of the same length")

  expect_error(rename_cols(df_duplicate_colnames, "x", "y"),
               "Error! Some column names in `df` are duplicated")
})


# `assert_all_df_cols_are_type_character()` -------------------------------

test_that("`assert_all_df_cols_are_type_character()` raises error appropriately", {
  acceptable_df <- data.frame(x = "a", y = "b")
  not_acceptable_df <- data.frame(x = "a", y = 1)

  expect_error(assert_all_df_cols_are_type_character(not_acceptable_df, "not_acceptable_df"),
               "Error! All columns in not_acceptable_df should be of type character")

  expect_true(assert_all_df_cols_are_type_character(acceptable_df, "acceptable_df"))
})

# `mutate_dob` ------------------------------------------------------------

test_that("`mutate_dob()` returns the expected dates", {
  # warnings generated for any missing yob/mob values - can suppress these
  result <- head(
    suppressWarnings(mutate_dob(ukb_pheno = dummy_ukb_data, data_dict = dummy_ukb_data_dict))
    )
  expect_equal(result,
               as.Date(c("1952-08-01", "1946-03-01", "1951-04-01", "1956-09-01", NA, "1948-02-01")))
})

# `revalue_vector()` -----------------------------------------------------------

test_that("`revalue_vector()` returns expected values", {
  expect_equal(revalue_vector(x = test_df$chr,
                              dict = dict,
                              suppress_warnings = TRUE),
               c("f", "g", "h", "i", "j", NA))

  expect_equal(revalue_vector(x = test_df$chr_rpt,
                              dict = dict),
               c(rep("f", 3), rep("h", 3)))
})

test_that("`revalue_vector()` raises warning if the col to be relabelled contains values that are not present in `dict`", {
  expect_warning(revalue_vector(test_df$chr,
                                dict = dict,
                                suppress_warnings = FALSE),
                 "The column to be relabelled contains values that are not present in \\`dict\\`. Number of values = 1"
  )
})

test_that("`revalue_vector()` raises error with non-unique 'keys'", {
  expect_error(revalue_vector(test_df$chr,
                              dict = bad_dict),
               "contains non-unique values")
})

test_that("`revalue_vector()` replaces values missing from `dict` with default values appropriately",
          {
            expect_equal(
              # use default_value of "foo". `NA` value should be set to "foo"
              revalue_vector(
                test_df$chr,
                dict = dict,
                default_value = "foo",
                suppress_warnings = TRUE
              )[6],
              "foo"
            )
          })

test_that("`revalue_vector()` raises error if df[[colname]] is not of class numeric/character/factor", {
  # error should state that this column is of type logical
  expect_error(revalue_vector(test_df$log,
                              dict = dict,
                              default_value = "foo"),
               "logical")
})


# `remove_special_characters_and_make_lower_case` -------------------------

test_that("`remove_special_characters_and_make_lower_case()` returns expected results", {
  expect_equal(
    remove_special_characters_and_make_lower_case("Fraction acceleration <= 10 milli-gravities"),
               "fraction_acceleration_less_or_equal_10_milli_gravities")

  expect_equal(
    remove_special_characters_and_make_lower_case("Wear duration during 23:00 - 23:59"),
  "wear_duration_during_2300_to_2359")

  expect_equal(
    remove_special_characters_and_make_lower_case("Unique minutes of wear in a 24 hour cycle (scattered over multiple days)"),
    "unique_minutes_of_wear_in_a_24_hour_cycle_scattered_over_multiple_days"
  )

  expect_equal(
    remove_special_characters_and_make_lower_case("Readings exceeding +/-8 gravities before calibration"),
    "readings_exceeding_plus_or_minus_8_gravities_before_calibration",
  )

  expect_equal(
    remove_special_characters_and_make_lower_case("Fraction acceleration <= 10 milli-gravities"),
    "fraction_acceleration_less_or_equal_10_milli_gravities"
  )

  expect_equal(
    remove_special_characters_and_make_lower_case("Number of days/week walked 10+ minutes"),
    "number_of_days_week_walked_10_plus_minutes"
  )

  expect_equal(
    remove_special_characters_and_make_lower_case("FI1 : numeric addition test"),
    "fi1_numeric_addition_test"
  )
})

# `validate_clinical_codes()` ---------------------------------------------

test_that(
  "`validate_clinical_codes`() returns expected error messages", {

  }
)
