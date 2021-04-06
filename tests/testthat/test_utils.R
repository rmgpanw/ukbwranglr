
# CONSTANTS ---------------------------------------------------------------

dummy_ukb_data_path <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")
dummy_ukb_data_dict <-
dummy_ukb_data <-

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

# test_that("`mutate_dob()` returns the expected dates", {
#   mutate_dob()
# })

