library(rawutil)


# CONSTANTS ---------------------------------------------------------------


# TESTS -------------------------------------------------------------------


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

# `code_type_to_lkp_sheet_map` --------------------------------------------

test_that("`code_type_to_lkp_sheet_map` has only unique values", {
  expect_true(length(names(ukbwranglr:::code_type_to_lkp_sheet_map)) == length(unique(names(ukbwranglr:::code_type_to_lkp_sheet_map))))

  expect_true(length(ukbwranglr:::code_type_to_lkp_sheet_map) == length(unique(ukbwranglr:::code_type_to_lkp_sheet_map)))
})

test_that("`code_type_to_lkp_sheet_map` only contains values in `ukb_code_mappings_sheet_names` and `ukb_code_mappings_code_types`", {
  expect_true(
    all(names(ukbwranglr:::code_type_to_lkp_sheet_map) %in% ukbwranglr:::ukb_code_mappings_code_types)
  )

  expect_true(
    all(ukbwranglr:::code_type_to_lkp_sheet_map %in% ukbwranglr:::ukb_code_mappings_sheet_names)
  )
})
