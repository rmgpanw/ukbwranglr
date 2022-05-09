
# SETUP ---------------------------------------------------------------

dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
dummy_ukb_data_raw_path <- get_ukb_dummy("dummy_ukb_main.tsv",
                                         path_only = TRUE)
dummy_data_dict <- make_data_dict(dummy_ukb_data_raw_path,
                                  ukb_data_dict = dummy_ukb_data_dict)

# make dummy data
dummy_ukb_main <- read_ukb(
  dummy_ukb_data_raw_path,
  data_dict = dummy_data_dict,
  ukb_data_dict = dummy_ukb_data_dict,
  ukb_codings = dummy_ukb_codings
)

# named vector
dict <- letters[6:10]
names(dict) <- letters[1:5]

bad_dict <- dict
names(bad_dict) <- rep("A", 5)

# test_df
test_df <- data.frame(chr = c(letters[1:5], NA),
                      int = c(1:5, NA),
                      fac = as.factor(c(letters[1:5], NA)),
                      log = c(rep(TRUE, 5), FALSE),
                      chr_rpt = c(rep("a", 3), rep("c", 3)))

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

test_that("`rename_cols()` raises expected errors for invalid arguments", {
  expect_error(rename_cols(df, "z", "x2"),
               "`old_colnames` contains values that are not present in `names\\(df\\)`")

  expect_error(rename_cols(df, c("x", "y"), c("x", "x")),
               "`old_colnames` and `new_colnames` must contain unique values only and be of the same length")

  expect_error(rename_cols(df, c("x", "y"), "x"),
               "`old_colnames` and `new_colnames` must contain unique values only and be of the same length")

  expect_error(rename_cols(df_duplicate_colnames, "x", "y"),
               "Some column names in `df` are duplicated")
})


# `assert_all_df_cols_are_type_character()` -------------------------------

test_that("`assert_all_df_cols_are_type_character()` raises error appropriately", {
  acceptable_df <- data.frame(x = "a", y = "b")
  not_acceptable_df <- data.frame(x = "a", y = 1)

  expect_error(assert_all_df_cols_are_type_character(not_acceptable_df, "not_acceptable_df"),
               "Error! All columns in not_acceptable_df should be of type character")

  expect_true(assert_all_df_cols_are_type_character(acceptable_df, "acceptable_df"))
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
  "wear_duration_during_2300_2359")

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

  expect_equal(
    remove_special_characters_and_make_lower_case("Enthesopathies & synovial disorders"),
    "enthesopathies_and_synovial_disorders"
  )
})

# `validate_clinical_codes()` ---------------------------------------------

test_that(
  "`validate_clinical_codes()` returns TRUE for `example_clinical_codes()`", {
    expect_true(validate_clinical_codes(example_clinical_codes()))
  }
)

test_that(
  "`validate_clinical_codes()` returns error if expected column names are not present", {
    expect_error(
      suppressWarnings(validate_clinical_codes(iris)), "Error! clinical_codes should have the following column headers:")
  }
)

test_that(
  "`validate_clinical_codes()` returns error if not all columns are type character", {
    expect_error(
        example_clinical_codes() %>%
          dplyr::mutate("disease" = as.factor(.data[["disease"]])) %>%
          validate_clinical_codes(),
        "Error! All columns in clinical_codes should be of type character"
        )
  }
)

test_that(
  "`validate_clinical_codes()` returns error missing values are present", {
    expect_error(
        example_clinical_codes() %>%
          dplyr::mutate("code_type" = ifelse(.data[["code_type"]] == "icd10",
                                             NA,
                                             .data[["code_type"]])) %>%
          validate_clinical_codes(),
        "Error! There should be no missing values in clinical_codes"
    )
  }
)

test_that(
  "`validate_clinical_codes()` returns error missing if unrecognised code_types are present", {
    expect_error(
      example_clinical_codes() %>%
        dplyr::mutate("code_type" = ifelse(.data[["code_type"]] == "icd10",
                                           "unrecognised_code_type",
                                           .data[["code_type"]])) %>%
        validate_clinical_codes(),
      "Error! code_type column in clinical_codes contains unrecognised code types"
    )
  }
)

test_that("`validate_clinical_codes()` returns error by default with overlapping disease categories",
          {
            expect_error(
              rbind(example_clinical_codes(),
                    example_clinical_codes()) %>%
                validate_clinical_codes(),
              "Error! Overlapping disease categories detected"
            )
          })

test_that("`validate_clinical_codes()` optionally raises a warning with overlapping disease categories",
          {
            expect_warning(
              rbind(example_clinical_codes(),
                    example_clinical_codes()) %>%
                validate_clinical_codes(allow_overlapping_categories = TRUE),
              "Warning! Overlapping disease categories detected"
            )
          })

test_that(
  "`validate_clinical_codes()` passes with no overlapping disease categories",
  {
    expect_true(
      example_clinical_codes() %>%
        dplyr::mutate("author" = "ukbwr2") %>%
        rbind(example_clinical_codes()) %>%
        validate_clinical_codes()
    )
  }
)


# `summarise_first_non_na` ----------------------------------------------------------

test_that("`summarise_first_non_na()` returns expected results", {
  df <- tibble::tribble(
    ~x, ~y, ~z,
    1, NA, 2,
    NA, 2, 3,
    NA, NA, 3,
    4, NA, 1
  )

  expect_equal(summarise_first_non_na(df,
                         columns = c("x", "y", "z"),
                         new_col = "new_col")$new_col,
               c(1, 2, 3, 4))

  expect_equal(summarise_first_non_na(df,
                                      columns = c("x", "z", "y"),
                                      new_col = "new_col")$new_col,
               c(1, 3, 3, 4))

  expect_equal(summarise_first_non_na(df,
                                      columns = c("z", "y", "x"),
                                      new_col = "new_col")$new_col,
               c(2, 3, 3, 1))
})

