
# CONSTANTS ---------------------------------------------------------------


# TESTS -------------------------------------------------------------------


# Diagnoses FieldIDs -------------------------------------------------------

DIAGNOSES_FIELD_IDS <- c(

  # Death data ICD10
  "40001",
  "40002",

  # NOT IN AK DATASET
  # # HES ICD9
  "41271",
  "41281",

  # # HES ICD10
  "41270",
  "41280",

  # self-report non-cancer
  "20002",
  "20008",

  # self-report cancer
  "20001",
  "20006",

  # cancer ICD9
  "40013",
  "40005",

  # cancer ICD10
  "40006",
  "40005"
)

# UKB code mappings constants ---------------------------------------------

# `ukb_code_mappings_sheet_names` and `ukb_code_mappings_code_type --------

test_that("`ukb_code_mappings_sheet_names` and `ukb_code_mappings_code_types` contain only unique values", {
  expect_true(length(ukbwranglr:::ukb_code_mappings_sheet_names) == length(unique(ukbwranglr:::ukb_code_mappings_sheet_names)))

  expect_true(length(ukbwranglr:::ukb_code_mappings_code_types) == length(unique(ukbwranglr:::ukb_code_mappings_code_types)))
})


# `clinical_code_mappings_map` --------------------------------------------

test_that("`clinical_code_mappings_map` has no spelling mistakes", {
  expect_equal(
    object = sort(unique(c(
      ukbwranglr:::clinical_code_mappings_map$from,
      ukbwranglr:::clinical_code_mappings_map$to))),
    expected = sort(
      # minus 'dmd' - there is no code mapping sheet for this
      ukb_code_mappings_code_types[-which(ukb_code_mappings_code_types == "dmd")]
      )
  )

  expect_true(
    all(ukbwranglr:::clinical_code_mappings_map$mapping_sheet %in% ukbwranglr:::ukb_code_mappings_sheet_names)
  )
})

test_that("`clinical_code_mappings_map` has only unique to_from mapping combinations", {
  expect_true(
    length(
      paste(ukbwranglr:::clinical_code_mappings_map$from, ukbwranglr:::clinical_code_mappings_map$to, sep = "_")
    ) == nrow(ukbwranglr:::clinical_code_mappings_map)
  )
})

test_that("`clinical_code_mappings_map` has only unique values in 'mapping_sheet' column", {
  expect_true(
    length(unique(ukbwranglr:::clinical_code_mappings_map$mapping_sheet)) == nrow(ukbwranglr:::clinical_code_mappings_map)
  )
})

test_that(
  "`clinical_code_mappings_map`: for each 'mapping_sheet', the 'from_col' and 'to_col' values are actually column names in that 'mapping sheet'",
  {
    # check colnames for each mapping sheet
    lambda <- function() {
    result <- NULL
    for (sheet in ukbwranglr:::clinical_code_mappings_map$mapping_sheet) {
      if (all(
        c(
          get_value_for_mapping_sheet(mapping_sheet = sheet, value = "from_col"),
          get_value_for_mapping_sheet(mapping_sheet = sheet, value = "to_col")
        ) %in% colnames_for_ukb_code_mappings_sheet_names[[sheet]]
      )) {
        result <- TRUE
      } else {
        result <- FALSE
        break()
      }
    }
    return(result)
    }

    expect_true(object = lambda())
  }
)

# `code_type_to_lkp_sheet_map_df` --------------------------------------------

test_that("`code_type_to_lkp_sheet_map_df` has only unique values", {
  expect_true(length(ukbwranglr:::code_type_to_lkp_sheet_map_df$code) == length(unique(ukbwranglr:::code_type_to_lkp_sheet_map_df$code)))

  expect_true(length(ukbwranglr:::code_type_to_lkp_sheet_map_df$lkp_sheet) == length(unique(ukbwranglr:::code_type_to_lkp_sheet_map_df$lkp_sheet)))
})

test_that("`code_type_to_lkp_sheet_map_df` only contains values in `ukb_code_mappings_sheet_names` and `ukb_code_mappings_code_types`", {
  expect_true(
    all(ukbwranglr:::code_type_to_lkp_sheet_map_df$code %in% ukbwranglr:::ukb_code_mappings_code_types)
  )

  expect_true(
    all(ukbwranglr:::code_type_to_lkp_sheet_map_df$lkp_sheet %in% ukbwranglr:::ukb_code_mappings_sheet_names)
  )
})

# `clinical_events_sources` -----------------------------------------------

test_that("`clinical_events_sources$data_coding` contains the following:
          'icd10',
          'data_coding_6',
          'data_coding_3',
          'icd9',
          'read2',
          'read3'.
          IF NOT: AMEND `extract_single_diagnostic_code_record_basis()` FILTER STATEMENT and tests for `filter_clinical_events_for_list_of_codes()`", {
  expect_equal(unique(sort(ukbwranglr:::clinical_events_sources$data_coding)),
               sort(c(
                 'icd10',
                 'data_coding_6',
                 'data_coding_3',
                 'icd9',
                 'read2',
                 'read3',
                 'opcs3',
                 'opcs4'
               )))

})
