
# EXPORTED FUNCTIONS ------------------------------------------------------


# Summarise ---------------------------------------------------------------

#' Quickly summarise a dataframe
#'
#' A tidyverse-friendly summary function that summarises a dataframe by column type
#'
#' Works with \code{dplyr::group_by()} and the pipe. See the
#' \code{\link[skimr]{skim}} documentation for more details. Adapts the
#' \code{skimr::skim()} function to include proportion counts for factor
#' variables
#'
#' @inheritParams skimr::skim
#'
#' @export
my_skim <- skimr::skim_with(
  # factor - a long anonymous function that converts a prop table to a single string
  factor = skimr::sfl(pct = function(x) {
    # make a prop table in %
    pct_table <- prop.table(table(x)) * 100

    # round % to 1dp
    pct_table <- round(pct_table, 1)

    # zip the table names and values together
    combined_vector <- vector(mode = 'character', length = 0L)
    for (i in 1:length(pct_table)) {
      combined_vector <- c(combined_vector, paste0(names(pct_table)[i], ":"))
      combined_vector <- c(combined_vector, paste0(as.character(pct_table)[i], "%,"))
    }

    # see result
    combined_vector

    # glue character vector into single string
    combined_vector <- stringr::str_c(combined_vector, collapse = " ")

    # return result
    return(combined_vector)
  }),

  # logical - returns proportion = TRUE
  logical = skimr::sfl(pct_TRUE = function(x) {sum(x == TRUE, na.rm=TRUE) / length(x) * 100})
)

# Mappings - data dictionary/codings --------------------------------------


#' Get descriptive colnames associated with one or more FieldIDs
#'
#' Returns all descriptive column names matching one or more FieldIDs.
#'
#' Use this in functions which manipulate a UKB phenotype dataset processed by
#' \code{\link{read_pheno}}. Should the convention for descriptive column names
#' change then so will these functions, however changes would only need to be
#' updated at the start of each function.
#'
#' @param field_ids character vector of UK Biobank Field IDs.
#' @param data_dict a data dictionary generated by \code{\link{make_data_dict}}
#' @param scalar_output bool. If \code{TRUE}, error raised if more than one
#'   colname is returned. Default value is \code{FALSE}.
#' @param error_if_missing bool. Raise error if any values in
#'   \code{filter_value} are missing. Default is \code{TRUE}.
#'
#' @export
#'
#' @return A character vector of descriptive colnames
#'
#' @family data dictionary helper functions
get_colnames_for_fieldids <- function(field_ids,
                                     data_dict,
                                     scalar_output = FALSE,
                                     error_if_missing = TRUE) {

  col_names <- filter_data_dict(data_dict = data_dict,
                   filter_col = "FieldID",
                   filter_value = field_ids,
                   return_col = "descriptive_colnames",
                   error_if_missing = error_if_missing)

  if (scalar_output == TRUE) {
    assertthat::is.scalar(col_names)
  }

  return(col_names)
}


#' Get UKB codings for one or more FieldIDs
#'
#' General helper function - filters
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{`ukb_codings`}
#' for the codings associated with one or more FieldIDs.
#'
#' @param field_ids Character. One or more UKB FieldIDs
#' @param ukb_data_dict UKB data dictionary
#' @param ukb_codings UKB Codings dictionary
#'
#' @return Dataframe.
#'
#' @export
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


# Download data dictionary/codings ----------------------------------------



#' Get UKB data dictionary
#'
#' Downloads the UK Biobank data dictionary from
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
#' github repo.
#'
#' @export
get_ukb_data_dict <- function() {
  # file destination in tempdir
  ukb_data_dict_rds <- file.path(tmpdir = tempdir(),
                                     "ukb_data_dict.rds")


  # download from ukbwranglr_resources if not already downloaded
  if(!file.exists(ukb_data_dict_rds)) {
    utils::download.file(url = "https://github.com/rmgpanw/ukbwranglr_resources/raw/main/_targets/objects/UKB_DATA_DICT",
                         destfile = ukb_data_dict_rds,
                         mode = "wb")
  }

  # load
  readRDS(ukb_data_dict_rds)
}

#' Get UKB codings file
#'
#' Downloads the UK Biobank codings dictionary from
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
#' github repo.
#'
#' @export
get_ukb_codings <- function() {
  # file destination in tempdir
  ukb_codings_rds <- file.path(tmpdir = tempdir(),
                                 "ukb_codings")


  # download from ukbwranglr_resources if not already downloaded
  if(!file.exists(ukb_codings_rds)) {
    utils::download.file(url = "https://github.com/rmgpanw/ukbwranglr_resources/raw/main/_targets/objects/UKB_CODINGS",
                         destfile = ukb_codings_rds,
                         mode = "wb")
  }

  # load
  readRDS(ukb_codings_rds)
}

#' Download UKB data dictionary directly from UKB website
#'
#' Downloads the UK Biobank data dictionary from the
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UK
#' Biobank website} and reads into R with all columns as character type.
#'
#' @export
get_ukb_data_dict_direct <- function() {
  fread_tsv_as_character("https://biobank.ctsu.ox.ac.uk/~bbdatan/Data_Dictionary_Showcase.tsv")
}

#' Download UKB codings file directly from UKB website
#'
#' Downloads the UK Biobank codings list from the
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UK
#' Biobank website} and reads into R with all columns as character type.
#'
#' @export
get_ukb_codings_direct <- function() {
  fread_tsv_as_character("https://biobank.ctsu.ox.ac.uk/~bbdatan/Codings.tsv")
}


# Download ukb clinical code mappings file -----------------------------------------------

#' Get UK Biobank clinical code mappings file
#'
#' Downloads the UK Biobank code mappings file
#' (\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592})
#' from the
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{ukbwranglr_resources}
#' github repo. The raw file is a large excel spreadsheet. This has been saved
#' in \code{.rds} format in
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{ukbwranglr_resources}
#' as a named list of data frames, one for each sheet in the original file.
#'
#' This function downloads the \code{ukb_code_mappings.rds} file from
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{ukbwranglr_resources}
#' to a temporary directory before loading and returning the result.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @return A named list.
#' @export
#' @seealso \code{\link{get_ukb_code_mappings_direct}}
get_ukb_code_mappings <- function() {
  # file destination in tempdir
  ukb_code_mappings_rds <- file.path(tmpdir = tempdir(),
                                       "ukb_code_mappings.rds")


  # download from ukbwranglr_resources if not already downloaded
  if(!file.exists(ukb_code_mappings_rds)) {
    utils::download.file(url = "https://github.com/rmgpanw/ukbwranglr_resources/raw/main/_targets/objects/UKB_CODE_MAPPINGS",
                  destfile = ukb_code_mappings_rds,
                  mode = "wb")
  }

  # load
  readRDS(ukb_code_mappings_rds)
}

#' Get UK Biobank clinical code mappings file directly from UKB website
#'
#' Downloads the UK Biobank code mappings file (\code{all_lkps_maps_v2.xlsx},
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{resource 592})
#' directly from the UKB website to a temporary directory at
#' \code{\link[base]{tempdir}}. This is then read into R as a named list of data
#' frames, one for each sheet in the original file.
#'
#' \strong{Note:} This is a large object (>450 MB)
#'
#' @return A named list.
#' @export
#' @seealso \code{\link{get_ukb_code_mappings}}
get_ukb_code_mappings_direct <- function() {
  # name of resource 592 excel file
  primarycare_codings <- "all_lkps_maps_v2.xlsx"

  # filepaths in tempdir
  primarycare_codings_zip_filepath <- tempfile()
  primarycare_codings_excel_filepath <- file.path(tempdir(), primarycare_codings)

  # download primary care codings file to tempdir
  message("Downloading primarycare_codings.zip (UKB resource 592) to tempdir")
  utils::download.file("https://biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip",
                primarycare_codings_zip_filepath,
                mode = "wb")

  # extract excel file only from zip
  message("Extracting all_lkps_maps_v2.xlsx from zip file to tempdir")
  utils::unzip(primarycare_codings_zip_filepath,
               files = primarycare_codings,
               exdir = tempdir())

  # reading all sheets into named list
  message("Reading sheets from all_lkps_maps_v2.xlsx to a named list")
  primarycare_codings_excel_filepath %>%
    readxl::excel_sheets() %>%
    purrr::discard(~ .x %in% c("Description", "Contents")) %>% # first 2 sheets not needed
    purrr::set_names() %>%
    purrr::map(readxl::read_excel,
        path = primarycare_codings_excel_filepath,
        col_types = "text")
}


# Download sqlite db containing ukb data dictionary/codings/code mappings --------

#' Download essential UKB resources as a SQLite database file
#'
#' Downloads a file called \code{ukb.db} from the
#' \href{https://github.com/rmgpanw/ukbwranglr_resources}{ukbwranglr_resources}
#' repo to the specified directory. This is an SQLite database containing the
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UKB
#' data dictionary and codings files}, UKB
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=592}{resource 592}
#' (lookup tables and code mappings for various clinical coding systems) and
#' clinical code lists from the
#' \href{https://github.com/spiros/chronological-map-phenotypes}{CALIBER repo}.
#'
#' @param directory_path character. The directory to which
#'
#' @return NULL
#' @export
get_ukb_db <- function(directory_path) {
  # check ukb.db does not already exist in `directory_path`
  if (file.exists(file.path(directory_path, "ukb.db"))) {
    stop(paste0("Error! A file called `ukb.db` already exists in ",
                directory_path))
  }

  # zipped file download destination in tempdir
  ukb_db_esssentials_zip <- file.path(tmpdir = tempdir(),
                                     "ukb_db_esssentials_zip")


  # download from ukbwranglr_resources if not already downloaded
  if(!file.exists(ukb_db_esssentials_zip)) {
    utils::download.file(url = "https://github.com/rmgpanw/ukbwranglr_resources/raw/main/ukb.db.zip",
                         destfile = ukb_db_esssentials_zip,
                         mode = "wb")
  }

  # unzip
  ukb_db_esssentials_unzipped = utils::unzip(ukb_db_esssentials_zip,
                                             exdir = directory_path)

  message(paste0("Download complete. Use `con <- DBI::dbConnect(RSQLite::SQLite(), dbname = '",
  directory_path,
  "/ukb.db')` to connect"))
}


# Download dummy ukb data -------------------------------------------------

#' Download dummy UK Biobank data
#'
#' A dummy UKB data file is downloaded to \code{\link[base]{tempdir}}. This will
#' be removed at the end of the current R session. The path to this file is
#' returned as a string.
#'
#' @return The path to the downloaded file.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # download dummy data to tempdir() and get filepath
#' # dummy_ukb_data_path <- download_dummy_ukb_data_to_tempdir()
#'
#' # make data dictionary make_data_dict(dummy_ukb_data_path, delim = ",",
#' # ukb_data_dict = get_ukb_data_dict())
#' }
download_dummy_ukb_data_to_tempdir <- function() {
  # file path in tempdir
  dummy_ukb_data_tempdir_path <- tempfile(pattern = "dummy_ukb_data",
                                          tmpdir = tempdir(),
                                          fileext = ".csv")

  # download dummy data from ukbwranglr
  utils::download.file("https://raw.githubusercontent.com/rmgpanw/ukbwranglr_resources/main/dummy_ukb_data/dummy_ukb_data.csv",
                destfile = dummy_ukb_data_tempdir_path)

  # return file path
  message("Downloaded dummy UKB data to `tempdir()`. Returning path to file")
  return(dummy_ukb_data_tempdir_path)
}


# Miscellaneous -----------------------------------------------------------

#' Estimate participant date of birth
#'
#' Estimates dates of birth from year and month of birth (Field IDs
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=34}{34} and
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=52}{52} respectively) as
#' the first date of the month.
#'
#' @inheritParams field_id_pivot_longer
#'
#' @return a vector of estimated dates of birth
#' @export
mutate_dob <- function(ukb_pheno, data_dict) {
  # select only the 2 required Field IDs
  required_cols <- get_colnames_for_fieldids(
    field_ids = c("34", "52"),
    data_dict = data_dict,
    scalar_output = FALSE
  )

  ukb_pheno <- ukb_pheno %>%
    dplyr::select(tidyselect::all_of(required_cols))

  # rename
  names(ukb_pheno) <- c('yob', 'mob')

  # vcheck that 'mob' column is a factor
  assertthat::assert_that(
    is.factor(ukb_pheno$mob),
    msg = "Error! The month of birth column (Field ID 52) must be a factor"
  )

  # estimate dob
  ukb_pheno %>%
    dplyr::mutate("dob" = paste(.data[["yob"]],
                                as.integer(.data[["mob"]]), # need to extract integer value e.g. 'January' == 1
                                01, # first day of month
                                sep = '-')) %>%
    .$dob %>%
    # convert to date format
    lubridate::ymd()
}

#' Create an empty list to be populated with diagnostic codes
#'
#' Returns a named list, where names are for different clinical coding systems.
#' This is to be populated with clinical codes and used with
#' \code{\link{extract_first_or_last_clinical_event}} and its related functions.
#'
#' @return A named list
#' @export
#'
#' @seealso \code{\link{extract_first_or_last_clinical_event}}
#' @examples
#' make_empty_clinical_codes_list()
make_empty_clinical_codes_list <- function() {
  ukbwranglr:::clinical_events_sources$data_coding %>%
    unique() %>%
    purrr::set_names() %>%
    purrr::map( ~ NULL)
}

#' Display time taken message
#'
#' Helper function for displaying time taken messages within other functions. Use
#' \code{\link[base]{proc.time}} at start of function and supply this as the
#' `start_time` parameter to this function.
#'
#' @param start_time The start time.
#' @export
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

# PRIVATE FUNCTIONS -------------------------------------------------------

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
        "Error! The data dictionary does not contain any of the required values in column ",
        filter_col,
        ": ",
        stringr::str_c(filter_value, sep = "", collapse = ", ")
      )
    )
  } else if (any(!filter_value %in% data_dict[[filter_col]]) &
             error_if_missing) {
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



#' Helper function for \code{\link{recode_ukbcol}}
#'
#' Generates a `mapping_df` for \code{\link{recode_ukbcol}}
#'
#' @inheritParams recode_ukbcol
#' @noRd
#' @family recode UKB values
recode_column_coding_meaning_value_mapping_df <- function(field_id,
                                                           ukb_data_dict,
                                                           ukb_codings,
                                                           mapping_direction = "meaning_code") {
  # check user supplied a valid `mapping_direction` value - error if not
  if (!(mapping_direction %in% c("meaning_code", "code_meaning"))) {
    stop("Argument `mapping_direction` must be either 'meaning_code' or 'code_meaning'")
  }

  # get coding/meaning for the specified field_id
  ukb_codings <- extract_codings_for_fieldids(
    field_id = field_id,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings
  )

  # label according to mapping direction
  if (mapping_direction == "meaning_code") {
    ukb_codings <- rename_cols(df = ukb_codings,
                               old_colnames = c("Meaning", "Value"),
                               new_colnames = c("old_vals", "new_vals"))
    ukb_codings <- ukb_codings %>%
      dplyr::select(-.data[["Coding"]])
  } else if (mapping_direction == "code_meaning") {
    ukb_codings <- rename_cols(df = ukb_codings,
                               old_colnames = c("Meaning", "Value"),
                               new_colnames = c("new_vals", "old_vals"))
    ukb_codings <- ukb_codings %>%
      dplyr::select(-.data[["Coding"]])
  }

  return(ukb_codings)
}


#' Recode values in a UK Biobank dataframe for a single column
#'
#' Recodes values in a specified column from descriptive label to UK Biobank
#' coding (default) or vice versa
#'
#' @section Under the hood:
#'
#' The UK Biobank codings for a given FieldID (\code{field_id}) are extracted into
#'   a \code{mapping_df}, which is formatted for use with
#'   \code{\link{recode_column}}. This then recodes the specified column
#'   (\code{col_to_recode}) in \code{df} from either meaning to raw UK Biobank codings
#'   (default) or vice versa.
#'
#' @seealso Columns "Value" and "Meaning" in the
#'   \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{UKB
#'    codings dictionary}
#'
#' @param df A dataframe
#' @param col_to_recode Name of column in \code{df} to be recoded
#' @param field_id Character. A UK Biobank FieldID
#' @inheritParams read_pheno
#' @param mapping_direction Character. Either "meaning_code" (default) or
#'   "code_meaning"
#'
#' @noRd
#' @family recode UKB values
recode_ukbcol <- function(df,
                          col_to_recode,
                          field_id,
                          ukb_data_dict,
                          ukb_codings,
                          mapping_direction = "meaning_code") {
  # make mapping_df with old and new col values
  mapping_df <- recode_column_coding_meaning_value_mapping_df(field_id = field_id,
                                                               ukb_data_dict = ukb_data_dict,
                                                               ukb_codings = ukb_codings,
                                                               mapping_direction = "meaning_code")

  # remove coding values with multiple associated meanings for categorical fields
  # (see also misc_ukb_codings.Rmd)
  if (field_id %in% c("20001",
                      "20002",
                      "20004",
                      "40013",
                      "41203",
                      "41205",
                      "41271")) {
    mapping_df <- mapping_df %>%
      dplyr::filter(!(.data[["old_vals"]] %in% c("-1", "Chapter V")) &
                      !(.data[["new_vals"]] %in% c("-1", "Chapter V")))
  }

  # for fieldid 20002, 'myasthenia gravis' has 2  associated codes - remove one of these if
  # mapping from meaning back to codes. Also see:
  # https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=6
  if (field_id == "20002" & mapping_direction == "meaning_code") {
    mapping_df <- mapping_df %>%
      dplyr::filter(.data[["new_vals"]] != "1260")
  }

  # relabel
  dict <- mapping_df$new_vals
  names(dict) <- mapping_df$old_vals

  df[[col_to_recode]] <- revalue_vector(x = as.character(df[[col_to_recode]]),
                                        dict = dict,
                                        default_value = NULL,
                                        suppress_warnings = FALSE)

  # OLD - to delete
  # df <- recode_column(df = df,
  #                      col_to_recode = col_to_recode,
  #                      mapping_df = mapping_df)

  return(df)
}


#' Recode values in a dataframe column
#'
#' Returns the input dataframe with recoded values in the specified
#' \code{col_to_recode}
#'
#' Uses a \code{mapping_df} to recode the values for a selected column
#' (\code{col_to_recode}) in a dataframe (\code{df}). The \code{mapping_df} should only contain
#' 2 columns named "old_vals" (containing values in \code{df$col_to_recode} to be
#' recoded) and "new_vals" (replacement values). There should also be no duplicated values in either column.
#'
#' A warning is generated if \code{mapping_df$old_vals} does not contain all values
#' in \code{df}, or if the return value has more rows than the original \code{df} (i.e. a
#' mutating join has been performed)
#'
#' @section Under the hood:
#'
#'   Uses \code{\link[dplyr]{left_join}} to merge \code{df} and \code{mapping_df},
#'   retaining the "new_vals" in \code{mapping_df}.
#'
#' @param df a dataframe
#' @param col_to_recode Character - name of column in \code{df} to be recoded
#' @param mapping_df a dataframe with 2 columns named "old_vals" and "new_vals"
#' @noRd
#' @family recode UKB values
recode_column <- function(df, col_to_recode, mapping_df) {

  # check nrow for input df
  original_nrow <- nrow(df)

  # check mapping_df is valid
  # - must have 2 columns named "old_vals" and "new_vals".
  # - All values in both columns are unique
  # Error if any checks fail
  if (!(all(names(mapping_df) == c("old_vals", "new_vals")) |
        all(names(mapping_df) == c("new_vals", "old_vals")))) {
    stop("Invalid `mapping_df`: must be a dataframe with 2 columns named 'old_vals' and 'new_vals'")
  }

 if(
   length(unique(mapping_df$old_vals)) != nrow(mapping_df)
    # length(unique(mapping_df$new_vals)) != nrow(mapping_df)
   ) {
   stop("`mapping_df` must contain only unique values in 'old_vals' column")
 }

  # Warning if "old_vals" does not contain all values in df
  if (length(setdiff(unique(stats::na.omit(df[[col_to_recode]])), mapping_df$old_vals)) > 0) {
    warning("WARNING! `mapping_df` does not contain all unique values in `df[[col_to_Recode]]`.
            Some values will not have been recoded - is this intentional?")
  }

  # rename col_to_recode before joining with mapper
  names(df)[which(names(df) == col_to_recode)] <- "old_vals"

  # join with mapper and drop old column - TODO replace this with
  # a dictionary-like function (would be safer and faster?)

  # TO DELETE- looping is very slow
  # pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
  #                        total = nrow(df))
  # recode_helper <- function(x,
  #                           mapping_df) {
  #   # return the corresponding 'new_val' for an 'old_val'
  #   pb$tick()
  #   result <- mapping_df %>%
  #     dplyr::filter(old_vals == x) %>%
  #     .$new_vals
  #
  #   if (rlang::is_empty(result)) {
  #     return(NA)
  #   } else {
  #     return(result)
  #   }
  # }
  #
  # message("\nrecoding...\n")
  # df <- df %>%
  #   dplyr::mutate(old_vals = map_chr(old_vals,
  #                                    recode_helper,
  #                                    mapping_df))

  df <- df %>%
    dplyr::left_join(mapping_df,
              by = "old_vals")

  df$old_vals <- df$new_vals

  df <- df %>%
    dplyr::select(-.data[["new_vals"]])

  # rename to original colname
  names(df)[which(names(df) == "old_vals")] <- col_to_recode

  # # warning message if returns a result with more rows than the original input
  if (nrow(df) > original_nrow) {
    warning("WARNING! New dataframe has more rows than the original input. Was this intentional?")
    }

  return(df)
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


# Miscellaneous helpers ---------------------------------------------------

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

    stop("Required columns not present in data")
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
                          msg = "Error! Some column names in `df` are duplicated")

  # old_colnames and new_colnames must be type character
  assertthat::is.string(old_colnames)
  assertthat::is.string(new_colnames)

  # all old_colnames must be in colnames(df)
  assertthat::assert_that(all(old_colnames %in% names(df)),
                          msg = "Error! `old_colnames` contains values that are not present in `names(df)`")

  # old_colnames and new_colnames must be unique, same length and type character
  assertthat::assert_that((length(old_colnames) == length(unique(old_colnames))) &
                            (length(new_colnames) == length(unique(new_colnames))) &
                               (length(new_colnames) == length(old_colnames)),
                          msg = "Error! `old_colnames` and `new_colnames` must contain unique values only and be of the same length")

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
  # characters to be replaced with "_"
  to_underscore <- c(" - ",
                     " ",
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
                 ":")

  for (char in to_remove) {
    string <- stringr::str_replace_all(string, char, "")
  }

  # Make lower case
  string <- tolower(string)

  # Return result
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
#'
#' @return A relabelled vector.
#' @noRd
revalue_vector <-
  function(x,
           dict,
           default_value = NULL,
           suppress_warnings = FALSE) {

    # raise an error if column is not character/numeric/integer
    assertthat::assert_that(all(class(x) %in% c("numeric", "integer", "character", "ordered")),
                            msg = paste("Error! Selected column must be of type numeric/integer/character. x is type:", class(x)))

    # `dict` is a named vector - check the names (keys) are unique
    if (length(unique(names(dict))) != length(dict)) {
      stop("names(dict) contains non-unique values")
    }

    # if default_value specified (i.e. default_value is not NULL), check length == 1
    if (!is.null(default_value)) {
      assertthat::are_equal(length(default_value), 1)
    }

    # warning message if dict does not include all values in x
    if (!suppress_warnings) {
      vals_missing_from_dict <-
        subset(x,!(x %in% names(dict)))
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
