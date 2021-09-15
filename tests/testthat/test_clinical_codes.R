
# CONSTANTS ---------------------------------------------------------------

ukb_code_mappings <- get_ukb_code_mappings()


# TESTS -------------------------------------------------------------------


# `ukb_code_mappings` -----------------------------------------------------

test_that("`ukb_code_mappings` sheet 'icd10_lkp' has no rows with values in both the 'MODIFER-4' and 'MODIFER-5' columns", {
  # relevant to `lookup_codes()` when `standardise_output` is `TRUE`. Some
  # ICD-10 codes have a description modifier in one of these 2 columns (e.g.
  # `E10` for T1DM (MODIFER-4) and `S27` for traumatic pneumothorax
  # (MODIFER-5)). `lookup_codes()` creates a description column by pasting
  # together the 'DESCRIPTION' column with *only* one of these. Therefore only
  # one of these columns should contain a description.
  expect_true(
    sum(!is.na(ukb_code_mappings$icd10_lkp$MODIFIER_4) & !is.na(ukb_code_mappings$icd10_lkp$MODIFIER_5)) == 0
  )
})

# `get_child_codes()` -----------------------------------------------------

test_that("`get_child_codes()` returns the expected nuber of results", {
  # return - codes only
  expect_equal(length(
    get_child_codes(
      codes = c("C10E."),
      code_type = "read2",
      ukb_code_mappings = ukb_code_mappings,
      codes_only = TRUE,
      standardise_output = FALSE
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

test_that(
  "`lookup_codes()` returns the expected columns when `standardise_output` is `TRUE`",
  {
    result <- lookup_codes(
      codes = c("E10", "E10.0"),
      code_type = "icd10",
      ukb_code_mappings = ukb_code_mappings,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )

    expect_equal(names(result), c("code", "description", "code_type"))

    expect_equal(result$description,
                 c("Type 1 diabetes mellitus",
                   "Type 1 diabetes mellitus With coma"))
  }
  )

# `map_codes()` -----------------------------------------------------------

test_that(
  "`map_codes()` raises warning if any of the supplied codes are not present in the coding system being mapped from", {
    expect_warning(
      map_codes(codes = c("C10E.", "foo", "bar"),
                from = "read2",
                to = "read3",
                ukb_code_mappings = ukb_code_mappings,
                quiet = FALSE),
      regexp = "Warning! The following codes were not found for read2: 'foo', 'bar'",
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
                codes_only = TRUE,
                standardise_output = FALSE),
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
                     preferred_description_only = FALSE,
                     standardise_output = FALSE)),
      4
    )

    # codes and preferred descriptions only - should raise an error as can miss
    # codes e.g. try searching for "D4104", will only return the secondary
    # description for its Read3 equivalent (which is also "D4104")
    expect_error(
      map_codes(
        codes = c("C10E.", "C108."),
        from = "read2",
        to = "read3",
        ukb_code_mappings = ukb_code_mappings,
        quiet = FALSE,
        codes_only = FALSE,
        preferred_description_only = TRUE,
        standardise_output = FALSE
      ),
      regexp = "Error! `preferred_description_only` cannot be `TRUE` unless `standardise_output` is also `TRUE`"
    )
  }
)

test_that(
  "`map_codes` returns the expected output when `standardise_output` is `TRUE`",
  {
    expect_equal(
      map_codes(
        codes = c("C10E.", "C108."),
        from = "read2",
        to = "read3",
        ukb_code_mappings = ukb_code_mappings,
        quiet = FALSE,
        codes_only = FALSE,
        preferred_description_only = TRUE,
        standardise_output = TRUE
      )$code,
      "X40J4"
    )
  }
)

test_that("`map_codes()` works as expected for mapping icd10 to icd9 codes (these need reformatting first)", {
  expect_equal(
    suppressWarnings(map_codes(
    codes = "D75.1",
    from = "icd10",
    to = "icd9",
    ukb_code_mappings = ukb_code_mappings,
    quiet = FALSE,
    codes_only = FALSE,
    preferred_description_only = TRUE,
    standardise_output = TRUE
  )$code),
  "2890")
})

test_that("`map_codes()` works when mapping icd9 to icd10", {
  expect_equal(
    map_codes(
      codes = "0020",
      from = "icd9",
      to = "icd10",
      ukb_code_mappings = ukb_code_mappings,
      quiet = FALSE,
      codes_only = FALSE,
      preferred_description_only = TRUE,
      standardise_output = TRUE
    )$code,
    expected = "A01.0"
  )
})

# `reformat_standardised_codelist()` --------------------------------------

test_that("`reformat_standardised_codelist()` returns the expected output format",
          {
            expect_equal(
              lookup_codes(
                codes = c("C10E.", "C108."),
                code_type = "read2",
                ukb_code_mappings = ukb_code_mappings,
                preferred_description_only = TRUE
              ) %>%
                reformat_standardised_codelist(
                  code_type = "read2",
                  disease = "T1DM",
                  disease_category = "T1DM GP diagnosis",
                  author = "test"
                ) %>%
                names(),
              c(
                'disease',
                'description',
                'category',
                'code_type',
                'code',
                'author'
              )
            )
          })

test_that("`reformat_standardised_codelist()` raises error with invalid args", {
  expect_error(
    reformat_standardised_codelist(
      standardised_codelist = data.frame(
        code = "C10E.",
        description = "T1DM",
        code_type = "invalid_code"
      ),
      code_type = "read2",
      disease = "T1DM",
      disease_category = "T1DM GP diagnosis",
      author = "test"
    ),
    regexp = "contains unrecognised code types"
  )

  expect_error(
    reformat_standardised_codelist(
      standardised_codelist = data.frame(
        code = "C10E.",
        description = "T1DM",
        A_TYPE_OF_CODE = "read2"
      ),
      code_type = "read2",
      disease = "T1DM",
      disease_category = "T1DM GP diagnosis",
      author = "test"
    ),
    regexp = "must be a data frame with the following headings: 'code', 'description', 'code_type'"
  )
})

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

# `reformat_icd10_codes()` ------------------------------------------------
test_that("`reformat_icd10_codes()` returns the expected values for ICD10_CODE to ALT_CODE", {
  expect_equal(
    # warning raised because "I714" not present in ICD10_CODE col of icd10_lkp
    # sheet
    reformat_icd10_codes(
        icd10_codes = c("D75.1",
                        "H40", # will be the same for ICD10_CODE and ALT_CODE
                        "H40.1",
                        "I714", # not in ICD10_CODE col
                        "M00.0"), # multiple associated ALT_CODEs
        ukb_code_mappings = ukb_code_mappings,
        input_icd10_format = "ICD10_CODE",
        output_icd10_format = "ALT_CODE"
      ),
    c("D751", "H40", "H401", "I714", "M000")
  )
})

test_that("`reformat_icd10_codes()` returns the expected values for ALT_CODE to ICD10_CODE", {
  expect_equal(
    # warning raised because "I714" not present in ICD10_CODE col of icd10_lkp
    # sheet
    reformat_icd10_codes(
        icd10_codes = c("D751",
                        "H40", # will be the same for ICD10_CODE and ALT_CODE
                        "H401",
                        "I714", # not in ICD10_CODE col
                        "M000", # multiple associated ALT_CODEs - all map to "M00.0"
                        "M0001",
                        "M0002"),
        ukb_code_mappings = ukb_code_mappings,
        input_icd10_format = "ALT_CODE",
        output_icd10_format = "ICD10_CODE"
      ),
    c("D75.1", "H40", "H40.1", "I71.4", "M00.0", "M00.0", "M00.0")
  )
})

