
# CONSTANTS ---------------------------------------------------------------

ukb_code_mappings <- get_ukb_code_mappings()

# TESTS -------------------------------------------------------------------

# `get_child_codes()` -----------------------------------------------------

test_that("`get_child_codes()` returns the expected nuber of results", {
  # return - codes only
  expect_equal(length(
    get_child_codes(
      codes = c("C10E."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      codes_only = TRUE,
    )
  ),
  expected = 27)

  # return codes and descriptions as a data frame
  expect_equal(nrow(
    get_child_codes(
      codes = c("C10E."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      codes_only = FALSE,
      preferred_description_only = FALSE
    )
  ),
  expected = 73)

  expect_equal(nrow(
    get_child_codes(
      codes = c("C10E."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      codes_only = FALSE,
      preferred_description_only = TRUE
    )
  ),
  expected = 27)
})

# `lookup_codes()` --------------------------------------------------------

test_that("`lookup_codes()` returns the expected number of results", {
  expect_equal(nrow(
    lookup_codes(
      codes = c("C10E.", "C108."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      preferred_description_only = FALSE
    )
  ),
  expected = 7)

  expect_equal(nrow(
    lookup_codes(
      codes = c("C10E.", "C108."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      preferred_description_only = TRUE
    )
  ),
  expected = 2)
})

# `map_codes()` -----------------------------------------------------------

test_that(
  "`map_codes()` raises warning if any of the supplied codes are not present in the coding system being mapped from", {
    expect_warning(
      map_codes(codes = c("C10E.", "foo", "bar"),
                from = "read2",
                to = "read3",
                ukb_code_mappings = ukb_code_mappings,
                quiet = FALSE),
      regexp = "Warning! The following codes were not found for read2 (the coding system being mapped from): 'foo', 'bar'",
      fixed = TRUE
    )
  }
)

test_that(
  "`map_codes()` returns the expected codes", {
    # codes only
    expect_equal(
      map_codes(codes = c("C10E."),
                from = "read2",
                to = "read3",
                ukb_code_mappings = ukb_code_mappings,
                quiet = FALSE,
                codes_only = TRUE),
      "X40J4"
    )

    # codes and ALL descriptions
    expect_equal(
      nrow(map_codes(codes = c("C10E."),
                     from = "read2",
                     to = "read3",
                     ukb_code_mappings = ukb_code_mappings,
                     quiet = FALSE,
                     codes_only = FALSE,
                     preferred_description_only = FALSE)),
      4
    )

    # codes and preferred descriptions only (this additionally filters for only unique codes)
    expect_equal(
      map_codes(codes = c("C10E.", "C108."),
                     from = "read2",
                     to = "read3",
                     ukb_code_mappings = ukb_code_mappings,
                     quiet = FALSE,
                     codes_only = FALSE,
                     preferred_description_only = TRUE)$READV3_CODE,
      "X40J4"
    )
  }
)

# `get_from_to_mapping_sheet()` -------------------------------------------

test_that(
  "`get_from_to_mapping_sheet()` returns the correct mapping sheet for various 'from'/'to' combinations", {
    expect_equal(get_from_to_mapping_sheet(from = "read2", "read3"),
                 "read_v2_read_ctv3")

    expect_equal(get_from_to_mapping_sheet(from = "read3", "read2"),
                 "read_ctv3_read_v2")

    expect_equal(get_from_to_mapping_sheet(from = "read2_drugs", "bnf"),
                 "read_v2_drugs_bnf")
  }
)

# `warning_if_codes_not_found()` ------------------------------------------

test_that("`warning_if_codes_not_found()` produces a waring message appropriately", {
  # should raise a warning
  expect_warning(
    warning_if_codes_not_found(codes = "foo",
                               code_type = "imaginary_coding_system",
                               search_col = c("A", "B", "C")),
    regexp = "Warning! The following codes were not found for imaginary_coding_system"
  )

  # should return NULL
  expect_null(
    warning_if_codes_not_found(codes = "A",
                             code_type = "imaginary_coding_system",
                             search_col = c("A", "B", "C"))
    )


})
