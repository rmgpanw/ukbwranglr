

# CONSTANTS ---------------------------------------------------------------



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
