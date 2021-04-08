
# OVERVIEW ----------------------------------------------------------------

# Functions to map between different clinical codes e.g. between Read2 and
# Read3, or Read3 and ICD-10. These rely on the code mapping file provided by
# UK Biobank (resource 592: https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)

# EXPORTED FUNCTIONS ------------------------------------------------------

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

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

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
#' @inheritParams get_child_codes
#'
#' @return data frame
#' @export
#' @family Clinical code lookups and mappings
lookup_codes <- function(codes,
                         code_type,
                         ukb_code_mappings = get_ukb_code_mappings(),
                         preferred_description_only = TRUE,
                         quiet = FALSE) {
  # validate args
  match.arg(arg = code_type,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)

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

  # lookup - filter lookup sheet for codes
  result <- ukb_code_mappings[[lkp_sheet]] %>%
    dplyr::filter(.data[[code_col]] %in% codes)

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
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(codes = codes,
                                 code_type = code_type,
                                 search_col = ukb_code_mappings[[lkp_sheet]][[code_col]])
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
    dplyr::filter(stringr::str_detect(
      string = .data[[description_col]],
      pattern = stringr::regex(pattern = reg_expr,
                               ignore_case = ignore_case)
    ))

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
#' @param quiet bool. Warning message if any of `codes` are not found for the
#'   code type being mapped from.
#' @inheritParams get_child_codes
#'
#' @export
#' @family Clinical code lookups and mappings
map_codes <- function(codes,
                      from,
                      to,
                      ukb_code_mappings = get_ukb_code_mappings(),
                      codes_only = TRUE,
                      preferred_description_only = TRUE,
                      quiet = FALSE) {
  # validate args
  match.arg(arg = from,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)
            # choices = ukbwranglr:::clinical_code_mappings_map$from)

  match.arg(arg = to,
            choices = ukbwranglr:::code_type_to_lkp_sheet_map_df$code)
            # choices = ukbwranglr:::clinical_code_mappings_map$to)

  assertthat::assert_that(!from == to,
                          msg = "Error! `from` and `to` args cannot be the same")

  assertthat::assert_that(is.logical(codes_only),
                          msg = "`code_only` must be either 'TRUE' or 'FALSE'")

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
  # reformat codes if mapping from icd10
  if (from == "icd10") {
    codes <- reformat_icd10_codes(icd10_codes = codes,
                                  ukb_code_mappings = ukb_code_mappings)
  }

  result <- ukb_code_mappings[[mapping_sheet]] %>%
    dplyr::filter(.data[[from_col]] %in% codes)

  # filter for preferred code descriptions only if requested
  if (preferred_description_only & !is.na(preferred_description_col)) {
    result <- result %>%
      dplyr::filter(.data[[preferred_description_col]] == preferred_description_code) %>%
      # Note, will get duplicate codes without this step
      dplyr::distinct(.data[[to_col]], .keep_all = TRUE)
  }

  # return result
  if (rlang::is_empty(result)) {
    message("No codes found after mapping. Returning `NULL`")
    return(NULL)
  } else {
    # warning if any `codes` not present in `from_col`
    # TODO - warning if duplicates found in `codes`
    if (quiet == FALSE) {
      warning_if_codes_not_found(codes = codes,
                                 code_type = from,
                                 search_col = ukb_code_mappings[[mapping_sheet]][[from_col]])
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
    } else {
      return(result)
    }
  }
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
        " (the coding system being mapped from): '",
        stringr::str_c(missing_codes, sep = "", collapse = "', '"),
        "'"
      )
    }
  }


#' Helper function for \code{\link{map_codes}} - reformat ICD-10 codes
#'
#' The lookup sheet for ICD-10 ("icd10_lkp") has a column called "ALT_CODE",
#' which is an alternative format for ICD-10 codes. This is the format used in
#' the mapping sheets. This function converts from one format to the other.
#'
#' @param icd10_codes character vector of ICD-10 codes
#' @param input_icd10_format character. Must be either "ICD10_CODE" or "ALT_CODE".
#' @inheritParams get_child_codes
#'
#' @return character vector of ICD-10 codes, reformatted as per the "ALT_CODE"
#'   column in the "icd10_lkp" sheet in
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UKB resource
#'   592}.
#' @noRd
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
    .[[output_icd10_format]] %>%
    unique()

  if (rlang::is_empty(result)) {
    warning("Warning! `reformat_icd10_codes()` found no icd10 code matches. Returning NULL")
    return(NULL)
  } else(
    return(result)
  )
}
