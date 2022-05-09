
# EXPORTED FUNCTIONS ------------------------------------------------------


# Summarise ---------------------------------------------------------------

# Download data dictionary/codings ----------------------------------------

#' Download UKB data dictionary directly from UKB website
#'
#' Downloads the UK Biobank data dictionary from the
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UK
#' Biobank website} and reads into R with all columns as character type.
#'
#' @param path Path where file will be downloaded to. If a file already exists
#'   at this path, then this will be read into R without re-downloading.
#'
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{ get_ukb_data_dict() }
get_ukb_data_dict <- function(path = file.path(tempdir(), "Data_Dictionary_Showcase.tsv")) {
  download_file(download_url = "https://biobank.ctsu.ox.ac.uk/~bbdatan/Data_Dictionary_Showcase.tsv",
                path = path)
  fread_tsv_as_character(path)
}

#' Download UKB codings file directly from UKB website
#'
#' Downloads the UK Biobank codings list from the
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UK
#' Biobank website} and reads into R with all columns as character type.
#'
#' @param path Path where file will be downloaded to. If a file already exists
#'   at this path, then this will be read into R without re-downloading.
#'
#' @return A data frame.
#' @export
#' @examples
#' \dontrun{ get_ukb_codings() }
get_ukb_codings <- function(path = file.path(tempdir(), "Codings.tsv")) {
  download_file(download_url = "https://biobank.ctsu.ox.ac.uk/~bbdatan/Codings.tsv",
                path = path)
  fread_tsv_as_character(path)
}

# Validation helpers ------------------------------------------------------

#' Validate a clinical codes data frame
#'
#' Checks whether a data frame of clinical codes is correctly formatted for use
#' with \code{\link{extract_phenotypes}}.
#'
#' Checks that:
#'
#' \itemize{ \item Expected column names are present \item All columns are of
#' type character \item No missing values are present in any column \item No
#' disease categories overlap with each other i.e. each disease (for each
#' \code{author}) contains a unique set of clinical codes. Overlapping disease
#' categories may optionally be permitted by setting
#' \code{allow_overlapping_categories} to \code{TRUE}}
#'
#' Note that currently this does not check whether the clinical codes themselves
#' are valid (i.e. whether a clinical code exists for a given coding system).
#'
#' @param clinical_codes A data frame. See \code{\link{example_clinical_codes}}
#'   for an example.
#' @param allow_overlapping_categories If \code{TRUE}, will pass with a warning
#'   if any codes are duplicated between disease categories. If \code{FALSE}, an
#'   error will be raised. Default value is \code{FALSE}.
#'
#' @return Returns invisibly \code{TRUE} if all checks pass.
#' @export
#'
#' @examples
#' validate_clinical_codes(example_clinical_codes())
validate_clinical_codes <- function(clinical_codes,
                                    allow_overlapping_categories = FALSE) {
  standard_message <- ". Use example_clinical_codes() to see a valid clinical_codes example"

  # check is a data frame
  assertthat::assert_that(is.data.frame(clinical_codes),
                          msg = paste0("Error! clinical_codes must be a data frame",
                                       standard_message))

  # check for expected column names
  assertthat::assert_that(all(names(clinical_codes) == names(example_clinical_codes())),
                          msg = paste0("Error! clinical_codes should have the following column headers: ",
                                       stringr::str_c(names(example_clinical_codes()),
                                                      sep = "",
                                                      collapse = ", "),
                                       standard_message))

  # check all cols are type character
  assertthat::assert_that(
    all(
      (clinical_codes %>%
          purrr::map_chr(class)) == "character"),
    msg = paste0("Error! All columns in clinical_codes should be of type character. ",
                 standard_message)
  )

  # check there are no NA values
  cols_with_NA <- clinical_codes %>%
    purrr::map( ~ ifelse(sum(is.na(.x)) > 0,
                         yes = TRUE,
                         no = FALSE)) %>%
    purrr::keep(~ .x) %>%
    names()

  assertthat::assert_that(rlang::is_empty(cols_with_NA),
                          msg = paste0("Error! There should be no missing values in clinical_codes. The following columns contain NA's: ",
                                       stringr::str_c(cols_with_NA,
                                                      sep = "",
                                                      collapse = ", "),
                                       standard_message))

  # check all values in code_type are recognised
  unique_clinical_code_types <- unique(clinical_codes$code_type)
  unrecognised_code_types <- subset(unique_clinical_code_types,
                                    !unique_clinical_code_types %in% unique(CLINICAL_EVENTS_SOURCES$data_coding))

  assertthat::assert_that(rlang::is_empty(unrecognised_code_types),
                          msg = paste0("Error! code_type column in clinical_codes contains unrecognised code types: ",
                                       stringr::str_c(unrecognised_code_types,
                                                      sep = "",
                                                      collapse = ", "),
                                       standard_message))

  # check for overlapping disease categories - raise either warning or error if any detected
  overlapping_disease_categories <- identify_overlapping_disease_categories(clinical_codes)

  if (!is.null(overlapping_disease_categories)) {
    overlapping_disease_categories
  }

  if (allow_overlapping_categories) {
    if (!is.null(overlapping_disease_categories)) {
      warning(paste0("Warning! Overlapping disease categories detected. The following ",
             length(overlapping_disease_categories$disease_author),
             " disease(s) (author) contain overlapping disease categories: ",
             overlapping_disease_categories$disease_author_string))
    }
  } else {
  assertthat::assert_that(is.null(overlapping_disease_categories),
                          msg = paste0("Error! Overlapping disease categories detected. Each disease/author in clinical_codes should have a unique set of clinical codes/code_type. The following ",
                                       length(overlapping_disease_categories$disease_author),
                                       " disease(s) (author) contain overlapping disease categories: ",
                                       overlapping_disease_categories$disease_author_string,
                                       ". Set `allow_overlapping_categories` to `TRUE` to ignore these."))
  }

  invisible(TRUE)
}

# Miscellaneous -----------------------------------------------------------


#' Make a named list of \code{tbl_dbi} objects from a \code{DBIConnection}
#' object
#'
#' A convenience function that returns a list of \code{tbl_dbi}
#' objects from a \code{DBIConnection} object, named as per the database table
#' names.
#'
#' See the \href{https://dbplyr.tidyverse.org/articles/dbplyr.html}{introduction
#' to dbplyr} vignette for getting started with databases and
#' \code{\link[dplyr]{dplyr}}.
#'
#' @param conn A DBIConnection object, as returned by
#'   \code{\link[DBI]{dbConnect}}.
#'
#' @return A named list of \code{tbl_dbi} objects
#' @export
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
#' dplyr::copy_to(con, head(iris), "iris_head")
#' dplyr::copy_to(con, head(mtcars), "mtcars_head")
#' db_tables <- db_tables_to_list(con)
#' db_tables$iris_head
#' db_tables$mtcars_head
db_tables_to_list <- function(conn) {
  result <- DBI::dbListTables(conn) %>%
    purrr::set_names() %>%
    purrr::map(~ dplyr::tbl(conn, .x))

  if (rlang::is_empty(result)) {
    stop("Error! No tables identified in database")
  } else {
    return(result)
  }
}

#' Details for UK Biobank clinical events data
#'
#' Returns a data frame with details of clinical events data that is currently
#' processed by \code{\link{ukbwranglr}} functions.
#'
#' @return A data frame
#' @export
#'
#' @seealso \code{\link{tidy_clinical_events}}
#' @examples
#' clinical_events_sources()
clinical_events_sources <- function() {
  CLINICAL_EVENTS_SOURCES
}

# PRIVATE FUNCTIONS -------------------------------------------------------

# Download file -----------------------------------------------------------

#' Download a file
#'
#' First checks if the file already exists at the download path. If so, the file
#' path is returned invisibly without re-downloading.
#'
#' @param download_url Character
#' @param path Character
#'
#' @return File path to downloaded file
#' @noRd
download_file <- function(download_url,
                          path = tempfile()) {

  if (file.exists(path)) {
    invisible(path)
  } else {
    utils::download.file(url = download_url,
                         destfile = path,
                         mode = "wb")
    invisible(path)
  }
}

# fread - tsv as character ------------------------------------------------

#' Read a tsv file with all columns as type character
#'
#' Wrapper around \code{\link[data.table]{fread}}
#'
#' @param ... additional arguments are passed on to \code{\link[data.table]{fread}}
#' @noRd
fread_tsv_as_character <- purrr::partial(data.table::fread,
                                         colClasses = c('character'),
                                         sep = "\t",
                                         quote = " ",
                                         na.strings = c("", "NA"))


# Data dictionary/codings helpers -----------------------------------------

#' Get descriptive colnames associated with one or more FieldIDs
#'
#' Returns all descriptive column names matching one or more FieldIDs.
#'
#' Use this in functions which manipulate a UKB phenotype dataset processed by
#' \code{\link{read_ukb}}. Should the convention for descriptive column names
#' change then so will these functions, however changes would only need to be
#' updated at the start of each function.
#'
#' @param field_ids A character vector of UK Biobank Field IDs.
#' @param data_dict A data dictionary generated by \code{\link{make_data_dict}}
#' @param scalar_output If \code{TRUE}, error raised if more than one colname is
#'   returned. Default value is \code{FALSE}.
#' @param error_if_missing If \code{TRUE}, raise error if any values in
#'   \code{filter_value} are missing. Default is \code{TRUE}.
#' @param colname_col The column in \code{data_dict} containing the column names
#'   to return. By default this is "descriptive_colnames".
#'
#' @noRd
#' @return A character vector of column names
get_colnames_for_fieldids <- function(field_ids,
                                      data_dict,
                                      colname_col = "descriptive_colnames",
                                      scalar_output = FALSE,
                                      error_if_missing = TRUE) {

  col_names <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = field_ids,
                                return_col = colname_col,
                                error_if_missing = error_if_missing)

  if (scalar_output == TRUE) {
    assertthat::is.scalar(col_names)
  }

  return(col_names)
}


#' Generic helper function values from a data dictionary
#'
#' Filters a UK Biobank data dictionary on one column and returns the values
#' from another column (e.g. filter data dictionary for a FieldID, and return
#' the corresponding descriptive column name(s))
#'
#' General helper function. Raises an error if produces and empty vector
#'
#' @param data_dict a data dictionary generated by \code{\link{make_data_dict}}
#' @param filter_col character. The column in \code{data_dict} to filter on.
#' @param filter_value character (single or multiple). Filter for values in
#'   \code{filter_col} that match one of these.
#' @param return_col character. The column in \code{data_dict} to return after
#'   filtering.
#' @param error_if_missing bool. Raise error if any values in
#'   \code{filter_value} are missing. Default is \code{TRUE}.
#'
#' @return A vector.
#' @noRd
#' @family data dictionary helper functions
filter_data_dict <- function(data_dict,
                             filter_col,
                             filter_value,
                             return_col,
                             error_if_missing = TRUE) {
  # filter
  result <- data_dict %>%
    dplyr::filter(.data[[filter_col]] %in% filter_value) %>%
    .[[return_col]]

  # missing filter_values
  missing_filter_values <- subset(filter_value, !filter_value %in% data_dict[[filter_col]])

  # Error if produces an empty vector
  if (rlang::is_empty(result)) {
    stop(
      paste0(
        "Error! Data dictionary does not contain any of the required values in column ",
        filter_col,
        ": ",
        stringr::str_c(filter_value, sep = "", collapse = ", ")
      )
    )
  } else if (any(!filter_value %in% data_dict[[filter_col]]) &
             (error_if_missing == TRUE)) {
    stop(
      paste0(
        "Error! The following values are not present in the data dictionary (under column ",
        filter_col,
        "): ",
        stringr::str_c(missing_filter_values, sep = "", collapse = ", ")
      )
    )
  } else {
    if (length(missing_filter_values) > 0) {
      warning(
        paste0(
          "Warning! The following values are not present in the data dictionary (under column ",
          filter_col,
          "): ",
          stringr::str_c(
            missing_filter_values,
            sep = "",
            collapse = ", "
          )
        )
      )
    }
    return(result)
  }
}

#' Get UKB codings for one or more FieldIDs
#'
#' General helper function - filters
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{ukb_codings}
#' for the codings associated with one or more FieldIDs.
#'
#' @param field_ids Character. One or more UKB FieldIDs
#' @param ukb_data_dict UKB data dictionary
#' @param ukb_codings UKB Codings dictionary
#'
#' @noRd
#' @return Dataframe.
extract_codings_for_fieldids <- function(field_ids,
                                         ukb_data_dict,
                                         ukb_codings) {
  ukb_codings %>%
    dplyr::filter(.data[["Coding"]] == (
      ukb_data_dict %>%
        dplyr::filter(.data[["FieldID"]] %in% field_ids) %>%
        .$Coding %>%
        utils::head(n = 1)
    ))
}

# Testing/assertion helpers -----------------------------------------------

#' Assert number is an integer that is greater than or equal to 1
#'
#' Helper function for \code{\link{fread_chunked}} and
#' \code{\link{process_df_chunked}}.
#'
#' @param x An integer >= 1. Raises an error if this condition is not met
#' @param arg_name character. The argument name for x. This is used to generate
#'   an informative error message.
#'
#' @seealso \code{\link{fread_chunked}}, \code{\link{process_df_chunked}}
#' @noRd
assert_integer_ge_1 <- function(x, arg_name) {
  # custom error message
  error_message <- paste("Error!", arg_name, "must be an integer that is greater than 0")

  # assertion
  assertthat::assert_that(x >= 1,
                          rlang::is_integerish(x),
                          msg = error_message)
}

#' Assert number is an integer that is greater than or equal to n
#'
#' Helper function for \code{\link{fread_chunked}},
#' \code{\link{process_df_chunked}}, and some other functions.
#'
#' @param x An integer >= n. Raises an error if this condition is not met
#' @param arg_name character. The argument name for x. This is used to generate
#'   an informative error message.
#' @param n integer.
#'
#' @seealso \code{\link{fread_chunked}}, \code{\link{process_df_chunked}}
#' @noRd
assert_integer_ge_n <- function(x,
                                arg_name,
                                n) {
  # custom error message
  error_message <- paste("Error!",
                         arg_name,
                         "must be an integer that is greater than",
                         n)

  # assertion
  assertthat::assert_that(x >= n,
                          rlang::is_integerish(x),
                          msg = error_message)
}


#' Helper function - assert that all columns in a data frame are of type
#' character
#'
#' @param df a data frame
#' @param arg_name character. For generating informative messages
#'
#' @return Returns \code{TRUE} if passes.
#' @noRd
assert_all_df_cols_are_type_character <- function(df, arg_name) {
  assertthat::assert_that(
    all(df %>% purrr::map_lgl(~ is.character(.x))),
    msg = paste("Error! All columns in", arg_name, "should be of type character")
  )
}


# Validation helpers ------------------------------------------------------

identify_overlapping_disease_categories <- function(clinical_codes) {
  # helper function for validate_clinical_codes()

  stopifnot(is.data.frame(clinical_codes))

  # make df of all disease/author and code/code_type combinations
  overlapping_disease_categories <- data.frame(disease_author = paste(clinical_codes$disease,
                                                                      clinical_codes$author,
                                                                      sep = "_DISEASE_AUTHOR_"),
                                               code_code_type = paste(clinical_codes$code,
                                                                      clinical_codes$code_type,
                                                                      sep = "_DISEASE_AUTHOR_"))

  # identify disease/author combinations with non-unique codes
  overlapping_disease_categories <- overlapping_disease_categories %>%
    dplyr::group_by(.data[["disease_author"]],
                    .data[["code_code_type"]]) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(.data[["n"]] > 1) %>%
    dplyr::ungroup()

  # if any are identified, filter clinical_codes for the relevant rows
  if (nrow(overlapping_disease_categories) > 0) {

    overlapping_disease_categories <- overlapping_disease_categories %>%
      tidyr::separate("disease_author",
                      into = c("disease", "author"),
                      sep = "_DISEASE_AUTHOR_",
                      remove = FALSE) %>%
      tidyr::separate("code_code_type",
                      into = c("code", "code_type"),
                      sep = "_DISEASE_AUTHOR_")

    clinical_codes <- clinical_codes %>%
      dplyr::semi_join(overlapping_disease_categories,
                       by = c("disease", "code_type", "code", "author"))

    # return list containing the subset of clinical_codes with duplicated codes
    # and unique disease_author combinations (both as a vector and a single
    # string for convenience)
    return(list(clinical_codes = clinical_codes,
                disease_author = unique(overlapping_disease_categories$disease_author),
                disease_author_string = unique(overlapping_disease_categories$disease_author) %>%
                  stringr::str_replace("_DISEASE_AUTHOR_",
                                       " (") %>%
                  paste0(")") %>%
                  stringr::str_c(sep = "",
                                 collapse = ", ")))
  } else {
    invisible(NULL)
  }
}

# Miscellaneous helpers ---------------------------------------------------

check_all_vector_values_unique <- function(x,
                                           x_name) {
  assertthat::assert_that(length(x) == length(unique(x)),
                          msg = paste0("Error! ",
                                       x_name,
                                       " contains non-unique values"))
}

#' Check required columns are present
#'
#' Combines supplied character vectors into a single vector, then checks whether
#' these are all present in `names(df)`. Raises an error if not.
#'
#' @param df Dataframe
#' @param ... Character vector(s)
#' @noRd
check_required_cols_exist <- function(df,
                                      ...) {
  # combine input colnames into single character vector
  required_cols <- list(...) %>% purrr::reduce(c)

  # error if not all required cols present in df
  if (!all(required_cols %in% names(df))) {
    # invisible(required_cols) # TODO: is there a way to return a character
    # vector of required columns?
    # make print-friendly version of required cols
    # paste(required_cols, collapse = "\n\n")

    stop(paste0("Required columns not present in data. Required cols: ",
                stringr::str_c(required_cols,
                               sep = "",
                               collapse = ", ")))
  }
}

#' Rename columns in a dataframe
#'
#' Unlike \code{\link[dplyr]{rename}} and \code{\link[dplyr]{select}}, columns
#' can be renamed without using
#' \href{https://adv-r.hadley.nz/metaprogramming.html}{non-standard evaluation}.
#'
#' \code{old_colnames} and \code{new_colnames} must be character type, of the
#' same length and contain only unique values. All values in \code{old_colnames}
#' should also be present in \code{names(df)}.
#'
#' @param df data frame
#' @param old_colnames character vector of old column names to replace
#' @param new_colnames character vector of new column names
#'
#' @return data frame
#' @noRd
rename_cols <- function(df, old_colnames, new_colnames) {
  # validate args
  # colnames(df) must be unique
  assertthat::assert_that(length(names(df)) == length(unique(names(df))),
                          msg = "Some column names in `df` are duplicated")

  # old_colnames and new_colnames must be type character
  assertthat::is.string(old_colnames)
  assertthat::is.string(new_colnames)

  # all old_colnames must be in colnames(df)
  assertthat::assert_that(all(old_colnames %in% names(df)),
                          msg = "`old_colnames` contains values that are not present in `names(df)`")

  # old_colnames and new_colnames must be unique, same length and type character
  assertthat::assert_that((length(old_colnames) == length(unique(old_colnames))) &
                            (length(new_colnames) == length(unique(new_colnames))) &
                               (length(new_colnames) == length(old_colnames)),
                          msg = "`old_colnames` and `new_colnames` must contain unique values only and be of the same length")

  # get indices for old colnames
  old_colname_indices <- old_colnames %>%
    purrr::set_names() %>% # set names more for debugging
    purrr::map(~ which(names(df) == .x))

  # replace with new colnames
  for (i in seq_along(old_colname_indices)) {
    names(df)[old_colname_indices[[i]]] <- new_colnames[i]
  }

  # return result
  return(df)
}

#' Helper function - format string to a valid format for data frame colnames
#'
#' Removes special characters and converts all letters to lower case in
#' character vector.
#'
#' @param string character vector
#'
#' @noRd
remove_special_characters_and_make_lower_case <- function(string) {
  # Replace special characters

  # make '+/-' '_plus_or_minus_
  string <- stringr::str_replace_all(string, "\\+/-", "_plus_or_minus_")

  # make '+' 'plus'
  string <- stringr::str_replace_all(string, "\\+", "_plus_")

  # make '<=' 'le'
  string <- stringr::str_replace_all(string, "<=", "_less_or_equal_")

  # make '&' 'and
  string <- stringr::str_replace_all(string, "&", "_and_")

  # characters to be replaced with "_"
  to_underscore <- c("-",
                     "\\s",
                     "/",
                     "\\.")

  for (char in to_underscore) {
    string <- stringr::str_replace_all(string, char, "_")
  }

  # characters to replace with "" (i.e. to remove)
  to_remove <- c("\\(",
                 "\\)",
                 "\\-",
                 ",",
                 ":",
                 "`",
                 "#",
                 "\\[",
                 "\\]",
                 "_+$",
                 "'")

  for (char in to_remove) {
    string <- stringr::str_replace_all(string, char, "")
  }

  # make lower case
  string <- tolower(string)

  # add 'x' at start if first character is a digit
  string <- ifelse(stringr::str_detect(string, "^[:digit:]+"),
                   yes = paste0("x", string),
                   no = string)

  # remove any repeated '_' (this should go last)
  string <- stringr::str_replace_all(string, "__+", "_")

  # return result
  return(string)
}

#' Revalue values in a vector
#'
#' Similar idea to
#' \href{https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.replace.html}{pandas.replace
#' method} using a dictionary for the \code{value} argument. \strong{Note:} only
#' works with vectors that are of type numeric (including integer) or character.
#'
#' @param x character. Name of column to be relabelled
#' @param dict a named vector. \code{names(dict)} are the 'keys' i.e. the
#'   existing values in \code{df[[colname]]} to be replaced. These should be
#'   unique. An error is raised if non-unique values are found in
#'   \code{names(dict)}
#' @param default_value default value to use for values in \code{df[[colname]]}
#'   that are not present in \code{names(dict)}. By default this is \code{NULL},
#'   meaning that values not present in \code{names(dict)} will remain
#'   unchanged.
#' @param suppress_warnings bool. A warning is raised if the column to be
#'   relabelled contains values not present in \code{dict}. This message is
#'   silenced if \code{suppress_warnings} is \code{TRUE}. Default value is
#'   \code{FALSE}.
#' @param strict bool. If \code{TRUE}, raise error if if dict does not include
#'   all values in x
#'
#' @return A relabelled vector.
#' @noRd
revalue_vector <-
  function(x,
           dict,
           default_value = NULL,
           suppress_warnings = FALSE,
           strict = FALSE) {

    # raise an error if column is not character/numeric/integer
    assertthat::assert_that(all(class(x) %in% c("numeric", "integer", "character", "ordered", "haven_labelled", "vctrs_vctr")),
                            msg = paste("Error! Selected column must be of type numeric/integer/character/haven_labelled. x is type:", class(x)))

    # `dict` is a named vector - check the names (keys) are unique
    if (length(unique(names(dict))) != length(dict)) {
      stop("names(dict) contains non-unique values")
    }

    # if default_value specified (i.e. default_value is not NULL), check length == 1
    if (!is.null(default_value)) {
      assertthat::are_equal(length(default_value), 1)
    }

    # get any values not incldued in dict
    vals_missing_from_dict <-
      subset(x,!(x %in% names(dict)))

    # strict - error if dict does not include all values in x
    if (strict) {
      if (!rlang::is_empty(vals_missing_from_dict)) {
        stop(
          paste0(
            "The column to be relabelled contains values that are not present in `dict`. Number of values = ",
            length(vals_missing_from_dict)
          )
        )
      }
    }

    # warning message if dict does not include all values in x
    if (!suppress_warnings) {
      if (!rlang::is_empty(vals_missing_from_dict)) {
        warning(
          paste0(
            "The column to be relabelled contains values that are not present in `dict`. Number of values = ",
            length(vals_missing_from_dict)
          )
        )
      }
    }

    # replace values
    if (is.null(default_value)) {
      # if old value is not in `dict`, then keep unchanged

      x <-  ifelse(
        test = (x %in% names(dict)),
        yes = dict[x],
        no = x
      )
    } else {
      # if old value is not in `dict`, then change to default_value
      x <-  ifelse(
        test = (x %in% names(dict)),
        yes = dict[x],
        no = default_value
      )
    }

    return(x)
  }

extract_file_ext <- function(x) {
  stringr::str_extract(x, "\\.[:alpha:]+$") %>%
    stringr::str_remove_all("\\.")
}

extract_fid_instance_array_from_descriptive_or_processed_colheaders <-
  function(colheaders) {
    ukb_descriptive_format_indices <-
      stringr::str_which(string = colheaders,
                         pattern = "^.*_*f[:digit:]+(_[:digit:]+){0,2}$")
    colheaders[ukb_descriptive_format_indices] <-
      stringr::str_extract(string = colheaders[ukb_descriptive_format_indices],
                           pattern = "f[:digit:]+(_[:digit:]+)*$")

    return(colheaders)
  }

#' Convert descriptive/processed colnames to a dataframe of
#' FieldID/instance/array combinations
#'
#' @param x a character vector of descriptive or processed colnames generated by
#'   \code{\link{make_data_dict}}. Also works for derived variables which may be
#'   missing instance or array values
#'
#' @return a dataframe of extracted FieldID/instance/array values and all their
#'   possible combinations
#' @noRd
colname_to_field_inst_array_df <- function(x) {
  # make x into a df
  x <- data.frame(colnames = x)

  # extract fieldid_instance_array (removing "_f")
  x$fieldid_instance_array <-
    extract_fid_instance_array_from_descriptive_or_processed_colheaders(x$colnames) %>%
    stringr::str_remove("^f")

  # make description column (descriptive colname minus fieldid_instance_array)
  x$description <- stringr::str_remove(x$colnames,
                                       paste0("_?f", x$fieldid_instance_array))

  # separate into field_id, instance and array
  x <- x %>%
    tidyr::separate(col = "fieldid_instance_array",
                    into = c("fieldid", "instance", "array"),
                    sep = "_",
                    remove = FALSE,
                    fill = "right")

  # mutate cols with all possible combinations
  final_col_order <- c("colnames",
                       "description",
                       "fieldid_instance_array",
                       "fieldid",
                       "instance",
                       "array",
                       "fieldid_instance",
                       "instance_array")

  x <- x %>%
    tidyr::unite(
      col = "fieldid_instance",
      tidyselect::all_of(c("fieldid", "instance")),
      sep = "_",
      remove = FALSE,
      na.rm = TRUE
    ) %>%
    tidyr::unite(
      col = "instance_array",
      tidyselect::all_of(c("instance", "array")),
      sep = "_",
      remove = FALSE,
      na.rm = TRUE
    ) %>%
    # set empty values to 'NA'
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                                ~ ifelse(.x == "",
                                         yes = NA,
                                         no = .x))) %>%
    # rearrange
    dplyr::select(tidyselect::all_of(final_col_order))

    # check expected number of cols present (to update if function is amended in
    # future)
    assertthat::are_equal(length(names(x)),
                          length(final_col_order))

  return(x)
}


#' Create an empty list to be populated with diagnostic codes
#'
#' Returns a named list, where names are for different clinical coding systems.
#' This is to be populated with clinical codes and used with
#' \code{\link{extract_first_or_last_clinical_event}} and its related functions.
#'
#' @return A named list
#'
#' @seealso \code{\link{extract_first_or_last_clinical_event}}
#' @noRd
#' @examples
#' make_empty_clinical_codes_list()
make_empty_clinical_codes_list <- function() {
  CLINICAL_EVENTS_SOURCES$data_coding %>%
    unique() %>%
    purrr::set_names() %>%
    purrr::map( ~ NULL)
}

check_if_all_list_items_are_null <- function(a_list) {
  # returns TRUE if all items are NULL, otherwise returns FALSE
  a_list %>%
    purrr::map_lgl(is.null) %>%
    purrr::keep(~ !.x) %>%
    rlang::is_empty()
}

#' Display time taken message
#'
#' Helper function for displaying time taken messages within other functions. Use
#' \code{\link[base]{proc.time}} at start of function and supply this as the
#' `start_time` parameter to this function.
#'
#' @param start_time The start time.
#' @noRd
#' @return A message stating time taken since start time
time_taken_message <- function(start_time) {
  # get time taken
  time_taken <- proc.time() - start_time

  # display message
  message("Time taken: ",
          (time_taken[3] %/% 60),
          " minutes, ",
          (round(time_taken[3] %% 60)),
          " seconds.")
}

summarise_first_non_na <- function(df,
                                   columns,
                                   new_col) {
  # appends a new col composed of the first non-NA value across `columns`
  # (determined by the order of the `columns` vector)
  if (any(purrr::map_lgl(dplyr::select(df, tidyselect::all_of(columns)), is.factor))) {
    stop("Error! `columns` cannot be type factor")
  }

  df[[new_col]] <- NA

  for (i in columns) {
    df[[new_col]] <- ifelse(is.na(df[[new_col]]),
                              yes = df[[i]],
                              no = df[[new_col]])
  }

  return(df)
}
