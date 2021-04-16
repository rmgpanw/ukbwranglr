
# OVERVIEW ----------------------------------------------------------------

# Functions to map between different clinical codes e.g. between Read2 and
# Read3, or Read3 and ICD-10. These rely on the code mapping file provided by
# UK Biobank (resource 592: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)

# TODO - make these work when `ukb_code_mappings` is supplied as a SQLite db
# file containing resource 592(?)

# TODO - validate `ukb_code_mappings` arg

# EXPORTED FUNCTIONS ------------------------------------------------------

# Exploring and mapping clinical codes ------------------------------------

#' Get child codes for a clinical code type
#'
#' Uses the code mapping file provided by UK Biobank (resource 592:
#' https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)
#'
#' This is case \emph{sensitive} (important for read codes especially).
#'
#' @param codes character. A vector of codes to get the child codes for
#' @param code_type character. The type of clinical code system to be searched.
#'   Must be one of \code{read2}, \code{read3}, \code{icd9}, \code{icd10},
#'   \code{bnf}, \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#' @param ukb_code_mappings the \code{ukb_code_mappings} list returned by
#'   \code{\link{get_ukb_code_mappings}}.
#' @param codes_only bool. If \code{TRUE} (default), return a character vector
#'   of \emph{unique} codes. If \code{FALSE}, return a data frame of all results
#'   including code descriptions (useful for manual validation).
#' @param quiet bool. Warning message if any of \code{codes} are not found for
#'   the supplied \code{code_type}.
#' @param preferred_description_only bool. Return only preferred descriptions
#'   for clinical codes with synonyms. Default value is \code{TRUE}.
#'
#' @export
#' @family Clinical code lookups and mappings
get_child_codes <- function(codes,
                            code_type,
                            ukb_code_mappings = get_ukb_code_mappings(),
                            codes_only = TRUE,
                            preferred_description_only = TRUE,
                            quiet = FALSE) {
  # validate args
  match.arg(arg = code_type,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)

  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

  # check all sheets are present
  assertthat::assert_that(
    all(ukbwranglr:::ukb_code_mappings_sheet_names %in% names(ukb_code_mappings))
  )

  # determine relevant lookup sheet
  lkp_sheet <- get_lookup_sheet(code_type = code_type)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                       column = "code_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                       column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_sheet)
  }

  # add "^" at start and ".*" at end
  codes <- paste0("^", codes, ".*")

  # combine into single string, separated by "|"
  codes <- stringr::str_c(codes, sep = "", collapse = "|")

  # get children (filter for codes which match ANY of those in `codes` arg)
  result <- ukb_code_mappings[[lkp_sheet]] %>%
    dplyr::collect() %>%
    dplyr::filter(stringr::str_detect(.data[[code_col]],
                                      pattern = stringr::regex(codes,
                                                               ignore_case = FALSE)))

  # filter for preferred code descriptions only if requested
  if (preferred_description_only & !is.na(preferred_description_col)) {
    result <- result %>%
      dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
  }

  # return result
  if (rlang::is_empty(result)) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  } else {
    # TODO remove this?
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    # if (quiet == FALSE) {
    #   warning_if_codes_not_found(codes = codes,
    #                              code_type = code_type,
    #                              search_col = ukb_code_mappings[[lkp_sheet]][[code_col]])
    # }

    # return either unique codes only, or df including code descriptions
    if (codes_only) {
      return(
        unique(result[[code_col]])
      )
    } else {
      return(result)
    }
  }
}

#' Look up descriptions for clinical codes
#'
#' Returns a data frame including descriptions for the codes of interest
#'
#' @param codes character. Vector of codes to lookup
#' @param standardise_output bool. If \code{TRUE} (default), outputs a data
#'   frame with columns named 'code', 'description' and 'code_type'. Otherwise
#'   returns a data frame with all columns for the relevant lookup sheet from
#'   (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UK Biobank
#'   resource 592}).
#' @inheritParams get_child_codes
#'
#' @return data frame
#' @export
#' @family Clinical code lookups and mappings
lookup_codes <- function(codes,
                         code_type,
                         ukb_code_mappings = get_ukb_code_mappings(),
                         preferred_description_only = TRUE,
                         standardise_output = TRUE,
                         quiet = FALSE) {
  # validate args
  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  match.arg(arg = code_type,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)

  # check all sheets are present
  assertthat::assert_that(
    all(ukbwranglr:::ukb_code_mappings_sheet_names %in% names(ukb_code_mappings))
  )

  # determine relevant lookup sheet
  lkp_sheet <- get_lookup_sheet(code_type = code_type)

  # determine code column for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                       column = "code_col")

  # determine description column for lookup sheet
  description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                             column = "description_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                                        column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_sheet)
  }

  # lookup - filter lookup sheet for codes
  result <- ukb_code_mappings[[lkp_sheet]] %>%
    dplyr::filter(.data[[code_col]] %in% codes) %>%
    dplyr::collect()

  # filter for preferred code descriptions only if requested
  if (!is.null(preferred_description_only)) {
    if (preferred_description_only &
        !is.na(preferred_description_col)) {
      result <- result %>%
        dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
    }
  }

  # standardise output if requested
  if (standardise_output) {
    names(result)[which(names(result) == code_col)] <- "code"
    names(result)[which(names(result) == description_col)] <- "description"

    # ICD-10 has a modifier column e.g. "E10" = "Type 1 diabetes mellitus",
    # whereas "E10.0" = "Type 1 diabetes mellitus with coma". "With coma" is
    # contained in the modifier columns "MODIFIER-4". See 'S27' for an example
    # code where additional description is contained in the "MODIFER-5" column.
    # The returned "description" column from `standardise_output == TRUE`
    # therefore combines the 'DESCRIPTION' column with one of these 2 columns
    # (whichever is not NA).
    if (lkp_sheet == "icd10_lkp") {
      result$description <- dplyr::case_when(
        !is.na(result$MODIFIER_4) ~ paste(result$description, result$MODIFIER_4),
        !is.na(result$MODIFIER_5) ~ paste(result$description, result$MODIFIER_4),
        TRUE ~ result$description
      )
    }

    # return code, description and code_type cols only
    result <- result[c("code", "description")]
    result[["code_type"]] <- code_type
  }

  # return result
  if (rlang::is_empty(result)) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  } else {
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(codes = codes,
                                 code_type = code_type,
                                 search_col = ukb_code_mappings[[lkp_sheet]] %>%
                                   dplyr::collect() %>%
                                   .[[code_col]])
    }

    # return either unique codes only, or df including code descriptions
    return(result)
  }
}


#' Search for codes that match a description
#'
#' Returns a data frame with clinical codes that match the supplied regular
#' expression. Ignores case by default.
#'
#' @param reg_expr a regular expression to search for
#' @inheritParams stringr::regex
#' @inheritParams get_child_codes
#'
#' @return data frame by default, or a character vector of codes if
#'   \code{codes_only} is \code{TRUE}.
#' @export
search_codes_by_description <- function(reg_expr,
                                      code_type,
                                      ukb_code_mappings = get_ukb_code_mappings(),
                                      ignore_case = TRUE,
                                      codes_only = FALSE,
                                      preferred_description_only = TRUE) {
  # validate args
  match.arg(arg = code_type,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)

  # check all sheets are present
  assertthat::assert_that(
    all(ukbwranglr:::ukb_code_mappings_sheet_names %in% names(ukb_code_mappings))
  )

  # determine relevant lookup sheet
  lkp_sheet <- get_lookup_sheet(code_type = code_type)

  # determine code and description columns for lookup sheet
  code_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                              column = "code_col")

  description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                       column = "description_col")

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_col_for_lookup_sheet(lookup_sheet = lkp_sheet,
                                                        column = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_preferred_description_code_for_lookup_sheet(lookup_sheet = lkp_sheet)
  }

  # search for codes
  result <- ukb_code_mappings[[lkp_sheet]] %>%
    dplyr::collect() %>%
    dplyr::filter(stringr::str_detect(
      string = .data[[description_col]],
      pattern = stringr::regex(pattern = reg_expr,
                               ignore_case = ignore_case)
    ))

  # filter for preferred code descriptions only if requested
  if (!is.null(preferred_description_only)) {
    if (preferred_description_only &
        !is.na(preferred_description_col)) {
      result <- result %>%
        dplyr::filter(.data[[preferred_description_col]] == preferred_description_code)
    }
  }

  # return result
  if (rlang::is_empty(result)) {
    message("No matching codes found. Returning `NULL`")
    return(NULL)
  } else {
    if (codes_only) {
      return(
        unique(result[[code_col]])
      )
    } else {
      return(result)
    }
  }
}


#' Map clinical codes from one coding system to another
#'
#' Uses the code mapping file provided by UK Biobank
#' (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592}).
#'
#' The values for arguments \code{from} and \code{to} must be one of
#' \code{read2}, \code{read3}, \code{icd9}, \code{icd10}, \code{bnf},
#' \code{dmd}, \code{read2_drugs} or \code{opcs4}.
#'
#' @param codes character. A character vector of codes to be mapped.
#' @param from character (scalar). Coding system that \code{codes} belong to.
#' @param to character (scalar). Coding system to map \code{codes} to.
#' @param quiet bool. Warning message if any of \code{codes} are not found for
#'   the code type being mapped from.
#' @param preferred_description_only bool. Return only preferred descriptions
#'   for clinical codes with synonyms. Can only be \code{TRUE} if
#'   \code{standardise_output} is also \code{TRUE}. Default value is
#'   \code{NULL}.
#' @inheritParams get_child_codes
#' @inheritParams lookup_codes
#'
#' @export
#' @family Clinical code lookups and mappings
map_codes <- function(codes,
                      from,
                      to,
                      ukb_code_mappings = get_ukb_code_mappings(),
                      codes_only = FALSE,
                      standardise_output = TRUE,
                      quiet = FALSE,
                      preferred_description_only = NULL) {
  # validate args
  # check all sheets are present
  assertthat::assert_that(
    all(ukbwranglr:::ukb_code_mappings_sheet_names %in% names(ukb_code_mappings))
  )

  match.arg(arg = from,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)
            # choices = ukbwranglr:::clinical_code_mappings_map$from)

  match.arg(arg = to,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)
            # choices = ukbwranglr:::clinical_code_mappings_map$to)

  assertthat::assert_that(
    is.character(codes),
    msg = "Error! `codes` must be a character vector"
  )

  assertthat::assert_that(!from == to,
                          msg = "Error! `from` and `to` args cannot be the same")

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

  assertthat::assert_that(!(codes_only & standardise_output),
                          msg = "Error! `codes_only` and `standardise_output` cannot both be `TRUE`")

  if (!is.null(preferred_description_only)) {
    assertthat::assert_that(!(
      preferred_description_only == TRUE & standardise_output == FALSE
    ),
    msg = "Error! `preferred_description_only` cannot be `TRUE` unless `standardise_output` is also `TRUE`")
  }

  # get appropriate mapping sheet
  swap_mapping_cols <- FALSE
  mapping_sheet <- get_from_to_mapping_sheet(from = from, to = to)

  # if above returns `character(0)`, try to map the other way
  if (rlang::is_empty(mapping_sheet)) {
    swap_mapping_cols <- TRUE
    mapping_sheet <- get_from_to_mapping_sheet(from = to, to = from)
  }

  # if still returns `character(0)`, error
  if (rlang::is_empty(mapping_sheet)) {
    stop("Error! Invalid (or unavailable) code mapping request")
  } else if (swap_mapping_cols) {
    warning("Warning! No mapping sheet available for this request. Attempting to map anyway using: ", mapping_sheet)
  }

  # get from_col and to_col column names for mapping sheet
  # swap if appropriate
  if (swap_mapping_cols) {
    from_col <-
      get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                  value = "to_col")
    to_col <-
      get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                  value = "from_col")
  } else {
    from_col <-
      get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                  value = "from_col")
    to_col <-
      get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                  value = "to_col")
  }

  # determine relevant column indicating whether code description is preferred
  # (for code types with synonymous code descriptions like read 2 and read 3)
  preferred_description_col <- get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                                        value = "preferred_synonym_col")

  # get preferred code, if appropriate
  if (!is.na(preferred_description_col)) {
    preferred_description_code <- get_value_for_mapping_sheet(mapping_sheet = mapping_sheet,
                                                              value = "preferred_code")
  }

  # do mapping
  # reformat codes if mapping from icd10 and NOT swapping mapping cols
  if (from == "icd10" & swap_mapping_cols) {
    codes <- reformat_icd10_codes(icd10_codes = codes,
                                  ukb_code_mappings = ukb_code_mappings)
  }

  result <- ukb_code_mappings[[mapping_sheet]] %>%
    dplyr::filter(.data[[from_col]] %in% codes) %>%
    dplyr::collect()

  # return result
  if (nrow(result) == 0) {
    message("\nNo codes found after mapping. Returning `NULL`")
    return(NULL)
  } else {
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(codes = codes,
                                 code_type = from,
                                 search_col = ukb_code_mappings[[mapping_sheet]] %>%
                                   dplyr::collect() %>%
                                   .[[from_col]])
    }

    # return either unique codes only, or df including descriptions
    if (codes_only) {
      result <- unique(result[[to_col]])

      # TO DELETE - this doesn't work. Try map_codes(c("C10E.", "C108."), from
      # =" read2", to = "icd10") to see why...

      # # reformat codes if output is icd10 codes
      # if (to == "icd10") {
      #   result <- reformat_icd10_codes(icd10_codes = result,
      #                        ukb_code_mappings = ukb_code_mappings,
      #                        input_icd10_format = "ALT_CODE",
      #                        output_icd10_format = "ICD10_CODE")
      # }

      return(result)
    } else if (standardise_output) {
      # Note, not all mapping sheets in UKB resource 592 contain descriptions
      # (e.g. 'read_v2_icd9'). Therefore need to use `lookup_codes` if
      # `standardise_output` is `TRUE`
      return(
        lookup_codes(
          codes = unique(result[[to_col]]),
          code_type = to,
          ukb_code_mappings = ukb_code_mappings,
          preferred_description_only = preferred_description_only,
          quiet = quiet
        )
      )
      } else {
      return(result)
    }
  }
}

# Clinical code lists -----------------------------------------------------

#' Reformat a dataframe of clinical codes to work with
#' \code{\link{extract_first_or_last_clinical_event_multi}}
#'
#' A utility function that helps reformat the output from \code{\link{map_codes}}
#' or \code{\link{lookup_codes}} to work with
#' \code{\link{extract_first_or_last_clinical_event_multi}}. See also output
#' from \code{\link{generate_self_reported_diabetes_codes_df}} for an example of
#' the format that this function will output.
#'
#' @param standardised_codelist a data frame with column names "code",
#'   "description", "code_type".
#' @param code_type character (scalar). The clinical code type e.g. "read2"
#' @param disease character (scalar), e.g. "Secondary polycythaemia"
#' @param disease_category character (scalar). The subcategory of \code{disease}
#'   that these codes belong to e.g. "Diagnosis of Secondary polycythaemia".
#' @param phenotype_source character (scalar), e.g. "caliber".
#'
#' @return A data frame with the following column names: 'disease',
#'   'description', 'category', 'code_type', 'code' and 'phenotype_source'.
#'
#' @export
reformat_standardised_codelist <- function(standardised_codelist,
                                           code_type,
                                           disease,
                                           disease_category,
                                           phenotype_source) {
  # validate args
  assertthat::assert_that(any(
    class(standardised_codelist) %in% c("data.frame", "data.table", "tbl_df")
  ))
  assertthat::is.string(code_type)
  assertthat::is.string(disease)
  assertthat::is.string(disease_category)
  assertthat::is.string(phenotype_source)

  match.arg(code_type,
            choices = ukbwranglr:::clinical_events_sources$data_coding)

  assertthat::assert_that(all(
    names(standardised_codelist) == c("code", "description", "code_type")
  ),
  msg = "Error! `standardised_codelist` must be a data frame with the following headings: 'code', 'description', 'code_type'")

  assertthat::assert_that(
    all(
      standardised_codelist$code_type %in% unique(ukbwranglr:::clinical_events_sources$data_coding)
    ),
    msg = paste0(
      "Error! `standardised_codelist$code_type` contains unrecognised code types. Recognised code types: ",
      stringr::str_c(
        unique(ukbwranglr:::clinical_events_sources$data_coding),
        sep = "",
        collapse = ", "
      )
    )
  )

  # reformat to work with `extract_first_or_last_clinical_event_multi()`
  standardised_codelist <- standardised_codelist %>%
    dplyr::mutate(
      "code_type" = code_type,
      "disease" = disease,
      "category" = disease_category,
      "phenotype_source" = phenotype_source,
    ) %>%
    dplyr::select(.data[["disease"]],
                  .data[["description"]],
                  .data[["category"]],
                  .data[["code_type"]],
                  .data[["code"]],
                  .data[["phenotype_source"]])

  return(standardised_codelist)
}

#' Generate a data frame of clinical codes for self-reported diabetes in the UK
#' Biobank
#'
#' Data frames of this format may be used with
#' \code{\link{extract_first_or_last_clinical_event_multi}} to facilitate
#' identification of clinical events (e.g. date of diabetes diagnosis).
#'
#' @return Data frame
#' @export
#'
#' @examples
#' generate_self_reported_diabetes_codes_df()
generate_self_reported_diabetes_codes_df <- function() {
  tibble::tribble(
    ~ disease, ~ description, ~ category, ~ code_type, ~ code, ~ phenotype_source,
    "Diabetes", "diabetes", "Diabetes unspecified", "data_coding_6", "1220", "ukbwr",
    "Diabetes", "gestational diabetes", "Gestational diabetes", "data_coding_6", "1221", "ukbwr",
    "Diabetes", "type 1 diabetes", "Type 1 diabetes", "data_coding_6", "1222", "ukbwr",
    "Diabetes", "type 2 diabetes", "Type 2 diabetes", "data_coding_6", "1223", "ukbwr",
  )
}


# Utilities ---------------------------------------------------------------

#' Reformat ICD-10 codes
#'
#' The lookup sheet in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UKB resource 592}
#' for ICD-10 ("icd10_lkp") has a column called "ALT_CODE", which is an
#' alternative format for ICD-10 codes. This is the format used in the mapping
#' sheets for this resource, as well as in
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270}{Field ID 41270}.
#' This function converts from one format to the other.
#'
#' @param icd10_codes character vector of ICD-10 codes
#' @param input_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @param output_icd10_format character. Must be either "ICD10_CODE" or
#'   "ALT_CODE".
#' @inheritParams get_child_codes
#'
#' @return character vector of ICD-10 codes, reformatted as specified by
#'   \code{output_icd10_format}.
#' @export
#' @family Clinical code lookups and mappings
reformat_icd10_codes <- function(icd10_codes,
                                 ukb_code_mappings,
                                 input_icd10_format = "ICD10_CODE",
                                 output_icd10_format = "ALT_CODE") {
  # validate args
  match.arg(arg = input_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))

  match.arg(arg = output_icd10_format,
            choices = c("ICD10_CODE", "ALT_CODE"))


  assertthat::assert_that(input_icd10_format != output_icd10_format,
                          msg = "Error for `reformat_icd10_codes()`! Input and output icd10 formats cannot be the same")

  # reformat icd10 codes
  result <- ukb_code_mappings$icd10_lkp %>%
    dplyr::filter(.data[[input_icd10_format]] %in% icd10_codes) %>%
    dplyr::collect() %>%
    .[[output_icd10_format]] %>%
    unique()

  if (rlang::is_empty(result)) {
    warning("Warning! `reformat_icd10_codes()` found no icd10 code matches. Returning NULL")
    return(NULL)
  } else(
    return(result)
  )
}

# PRIVATE FUNCTIONS -------------------------------------------------------


#' Helper function for \code{\link{map_codes}}
#'
#' Returns name of the appropriate mapping sheet from the UKB code mappings
#' excel file (resource 592) for mapping from one clinical coding system to
#' another.
#'
#' @param from character
#' @param to character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_from_to_mapping_sheet <- function(from, to) {
  ukbwranglr:::clinical_code_mappings_map[
    (
      ukbwranglr:::clinical_code_mappings_map[["from"]] == from &
        ukbwranglr:::clinical_code_mappings_map[["to"]] == to
    ),
  ][["mapping_sheet"]]
}

#' Helper function for \code{\link{map_codes}}
#'
#' Returns the requested value for a 'mapping_sheet' in
#' \code{ukbwranglr:::clinical_code_mappings_map}.
#'
#' @param mapping_sheet character
#' @param value character. column name from
#'   \code{ukbwranglr:::clinical_code_mappings_map} (apart from
#'   "mapping_sheet").
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_value_for_mapping_sheet <- function(mapping_sheet,
                                        value) {

  # validate args
  match.arg(
    arg = mapping_sheet,
    choices = ukbwranglr:::clinical_code_mappings_map[["mapping_sheet"]]
  )

  match.arg(
    arg = value,
    choices = subset(
      names(ukbwranglr:::clinical_code_mappings_map),
      subset = names(ukbwranglr:::clinical_code_mappings_map) != "mapping_sheet"
    )
  )

  # return specified `value`
  ukbwranglr:::clinical_code_mappings_map[
    ukbwranglr:::clinical_code_mappings_map[["mapping_sheet"]] == mapping_sheet,
  ][[value]]
}

#' Get name of lookup sheet for a clinical code system
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{get_child_codes}}
#'
#' @param code_type character
#'
#' @return character (scalar)
#' @noRd
#' @family Clinical code lookups and mappings
get_lookup_sheet <- function(code_type) {
  # validate args
  match.arg(code_type,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)

  # get lookup sheet
  ukbwranglr:::code_type_to_lkp_sheet_map_df %>%
    dplyr::filter(.data[["code"]] == code_type) %>%
    .$lkp_sheet
}

#' Get name of code, description or preferred synonym column for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{get_child_codes}}
#'
#' @param lookup_sheet character
#' @param column character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_col_for_lookup_sheet <- function(lookup_sheet,
                                     column) {

  # validate args
  match.arg(arg = lookup_sheet,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$lkp_sheet)

  match.arg(arg = column,
            choices = c("code_col", "description_col", "preferred_synonym_col"))

  # get column name for lookup sheet
  ukbwranglr:::code_type_to_lkp_sheet_map_df %>%
    dplyr::filter(.data[["lkp_sheet"]] == lookup_sheet) %>%
    .[[column]]
}

#' Get preferred description code for a lookup sheet
#'
#' Helper function for \code{\link{lookup_codes}} and \code{\link{get_child_codes}}
#'
#' @param lookup_sheet character
#'
#' @return character (scalar)
#'
#' @family Clinical code lookups and mappings
#' @noRd
get_preferred_description_code_for_lookup_sheet <- function(lookup_sheet) {
  # validate args
  match.arg(arg = lookup_sheet,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$lkp_sheet)

  # get preferred description code for lookup sheet
  ukbwranglr:::code_type_to_lkp_sheet_map_df %>%
    dplyr::filter(.data[["lkp_sheet"]] == lookup_sheet) %>%
    .[["preferred_code"]]
}

#' Helper function - generate warning message
#'
#' Raises a warning if searched codes do not exist for a clinical code system.
#'
#' @param codes character vector. The codes being searched for.
#' @param code_type character. Type of code
#' @param search_col character vector. The column of codes (e.g. read2, ICD etc)
#'   being searched in.
#'
#' @return informative warning message, or nothing
#' @noRd
#' @family Clinical code lookups and mappings
warning_if_codes_not_found <-
  function(codes, code_type, search_col) {
    if (any(!codes %in% search_col)) {
      missing_codes <- unique(subset(codes, !codes %in% search_col))

      warning(
        "Warning! The following codes were not found for ",
        code_type,
        ": '",
        stringr::str_c(missing_codes, sep = "", collapse = "', '"),
        "'"
      )
    }
  }
