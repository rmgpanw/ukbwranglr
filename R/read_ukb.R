
# NOTES -------------------------------------------------------------------



# EXPORTED FUNCTIONS ------------------------------------------------------

#' Generate a UK Biobank data dictionary
#'
#' Creates a data dictionary from either a raw UK Biobank main dataset file or a
#' data frame if this has already been loaded into R.
#'
#' @param ukb_main Either the path to a UKB main dataset file (character string)
#'   or a data frame.
#' @param delim Delimiter for the UKB main dataset file. Default is "\\t".
#'   Ignored if the file name ends with \code{.dta} (i.e. is a \code{STATA}
#'   file) or if \code{ukb_main} is a data frame.
#' @param ukb_data_dict The UKB data dictionary (available online at the UK
#'   Biobank
#'   \href{https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide}{data
#'    showcase}. This should be a data frame where all columns are of type
#'   \code{character}. By default, this is downloaded from the
#'   \href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
#'    github repo.
#'
#' @return A data dictionary (data frame) specific to \code{ukb_main}. This
#'   includes columns with descriptive column names ("descriptive_colnames") and
#'   the current column names ("colheaders_raw").
#' @export
make_data_dict <- function(ukb_main,
                           delim = "\t",
                           ukb_data_dict = get_ukb_data_dict()) {

  # extract column names - ukb_main can be a file path or a dataframe already
  # loaded into R
  if (class(ukb_main)[1] == "character") {
    # read column names only
    # stata file
    if (stringr::str_detect(ukb_main, "\\.dta$")) {
      message("`ukb_main` appears to be a STATA file. Ignoring `delim` argument")
      colheaders_raw <-
        haven::read_dta(file = ukb_main, n_max = 0) %>%
        names()
    } else {
      # flat file
      colheaders_raw <- data.table::fread(
        ukb_main,
        colClasses = c('character'),
        na.strings = c("", "NA"),
        sep = delim,
        header = TRUE,
        data.table = TRUE,
        showProgress = TRUE,
        nrows = 0
      ) %>%
        names()
    }

  } else if ("data.frame" %in% class(ukb_main)) {
    colheaders_raw <- names(ukb_main)
  } else {
    stop("Error! `ukb_main` must be either a string specifying the filepath for a UKB main dataset, or a UKb dataset loaded into R as a data frame")
  }

  # check no duplicated column names
  check_all_vector_values_unique(colheaders_raw,
                                 "`names(ukb_main)`")

  # process header to this form: 'f.5912.0.0'
  colheaders <- format_ukb_df_header(colheaders_raw)

  # make mapping df
  # convert column names to a tibble and append 'mapping' columns
  data_dict <- dplyr::tibble(colheaders_raw = colheaders_raw,
                             colheaders_processed = colheaders) %>%
    # Make columns for FieldID, instance and array
    tidyr::separate(
      col = "colheaders_processed",
      sep = "\\.",
      into = c("temp", "FieldID", "instance", "array"),
      remove = FALSE,
      fill = "right" # 'eid' column will not separate so will raise an error without this option
    ) %>%

    # Remove column of "f"'s
    dplyr::select(-.data[["temp"]]) %>%

    # join with full ukb data dictionary
    dplyr::left_join(y = ukb_data_dict,
                     by = "FieldID")

  # mutate 'descriptive_colnames' column
  data_dict <- mutate_descriptive_columns(data_dict = data_dict)

  # make ValueType column for eid type 'Integer'
  data_dict$ValueType <- ifelse(data_dict$FieldID == "eid",
                                yes = "Integer",
                                no = data_dict$ValueType)

  ## return data_dict
  return(data_dict)
}

read_ukb_raw_basis <- function(path,
                         delim = "\t",
                         data_dict,
                         ukb_data_dict = get_ukb_data_dict(),
                         ukb_codings = get_ukb_codings(),
                         na.strings = c("", "NA"),
                         read_with = "fread",
                         callback = readr::DataFrameCallback$new(function(x, pos) x),
                         ...) {
  # low level function to read a UKB main dataset file specifying col types. Can
  # use either data.table or readr

  # validate args
  match.arg(read_with,
            choices = c("fread", "read_delim_chunked"))

  data_dict <- indicate_coltype_in_data_dict(data_dict = data_dict,
                                ukb_codings = ukb_codings)

  # make coltype args (for both `fread` and `readr`)
  coltypes_fread <- stats::setNames(
    data_dict$col_types_fread,
    data_dict$colheaders_raw
  )

  coltypes_readr <- stats::setNames(data_dict$col_types_readr,
                                    data_dict$colheaders_raw) %>%
    as.list() %>%
    do.call(readr::cols_only, .)

  # read data
  switch(read_with,
         fread = data.table::fread(path,
                                   select = coltypes_fread,
                                   na.strings = na.strings,
                                   sep = delim,
                                   ...),
         readr::read_delim_chunked(file = path,
                                   callback = callback,
                                   delim = delim,
                                   na = na.strings,
                                   ...))
}

label_ukb_raw <- function() {

}



# PRIVATE FUNCTIONS -------------------------------------------------------

#' Processes a ukb main dataset header to match the form 'f.5912.0.0'
#'
#' Helper function for \code{make_data_dict}
#'
#' @param colheaders A character vector. The first item should contain 'eid'
#'   i.e. the first column should be the eid column.
#'
#' @return A ukb main dataset header (character vector) of the form
#'   'f.5912.0.0'. Returns the header unaltered if already in this form
#'
#' @noRd
format_ukb_df_header <- function(colheaders) {

  assertthat::assert_that(stringr::str_detect(colheaders[1],
                                              "eid"),
                          msg = "Error! First column name should include 'eid'")

  colheaders[1] <- "f.eid"

  # Process colheaders not of the form 'f.5912.0.0'
  # ukb.txt format
  ukb_txt_format_indices <- stringr::str_which(string = colheaders,
                                               pattern = "[:digit:]+-[:digit:]+\\.[:digit:]+")

  colheaders[ukb_txt_format_indices] <- paste0("f.", gsub("-", ".", colheaders[ukb_txt_format_indices]))

  # ukb.dta format
  ukb_dta_format_indices <- stringr::str_which(string = colheaders,
                                               pattern = "[:alpha:]_[:digit:]+_[:digit:]+_[:digit:]+")

  colheaders[ukb_dta_format_indices] <- paste0("f.", gsub("_", ".", stringr::str_replace(colheaders[ukb_dta_format_indices],
                                                                                         pattern = "^[:alpha:]+_",
                                                                                         replacement = "")))

  return(colheaders)
}

#' Mutate a column of descriptive colnames
#'
#' Helper function for \code{make_data_dict()}
#'
#' @param data_dict A data dictionary formed by joining the column names
#'   from a raw ukb pheno file with the ukb data dictionary
#'   (https://biobank.ctsu.ox.ac.uk/~bbdatan/Data_Dictionary_Showcase.tsv)
#'
#' @noRd
mutate_descriptive_columns <- function(data_dict) {

  # Create vector of column names and Field_FieldID names from Field
  # descriptions/instance/array indices
  column_names <- paste(data_dict$Field,
                        paste0("f", data_dict$FieldID),
                        data_dict$instance,
                        data_dict$array)

  Field_FieldID_names <- paste(data_dict$Field,
                               data_dict$FieldID)

  # Replace the first with 'eid'
  column_names[1] <- 'eid'
  Field_FieldID_names[1] <- 'eid'

  # replace special characters and convert to lower case
  column_names <- remove_special_characters_and_make_lower_case(column_names)
  Field_FieldID_names <- remove_special_characters_and_make_lower_case(Field_FieldID_names)

  # mutate column with new, 'descriptive' column names and Field_FieldID_names
  data_dict[['descriptive_colnames']] <- column_names
  data_dict[['Field_FieldID']] <- Field_FieldID_names

  # Rearrange columns
  data_dict <- data_dict %>%
    dplyr::select(
      .data[["descriptive_colnames"]],
      tidyselect::everything()
    )

  return(data_dict)
}

indicate_coltype_in_data_dict <- function(data_dict,
                                          ukb_codings) {
  # helper for `read_ukb*`

  # codings in ukb_codings that can be read as integers
  ukb_codings_coercible_to_integer <- ukb_codings %>%
    dplyr::mutate("Value_coercible_to_integer" = dplyr::case_when(
      !is.na(suppressWarnings(as.integer(.data[["Value"]]))) ~ TRUE,
      is.na(suppressWarnings(as.integer(.data[["Value"]]))) ~ FALSE
    )) %>%
    dplyr::group_by(.data[["Coding"]]) %>%
    # identify codings where *all* values are coercible to integer
    dplyr::summarise("coercible_to_integer" = dplyr::case_when(
      mean(.data[["Value_coercible_to_integer"]]) == 1 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::filter(.data[["coercible_to_integer"]]) %>%
    dplyr::pull(.data[["Coding"]])

  # add column to data_dict indicating column type
  data_dict <- data_dict %>%
    dplyr::mutate(
      "col_types_readr" = dplyr::case_when(
        ((.data[["ValueType"]] == "Integer") |
           (.data[["Coding"]] %in% ukb_codings_coercible_to_integer) |
           (.data[["FieldID"]] == "eid")) ~ "i",
        .data[["ValueType"]] == "Continuous" ~ "d",
        .data[["ValueType"]] == "Date" ~ "D",
        # Default is type character
        TRUE ~ "c"
      )
    ) %>%
    dplyr::mutate(
      "col_types_fread" = dplyr::case_when(
        .data[["col_types_readr"]] == "i" ~ "integer",
        .data[["col_types_readr"]] == "d" ~ "double",
        .data[["col_types_readr"]] == "D" ~ "Date",
        .data[["col_types_readr"]] == "c" ~ "character"
      )
    )

  return(data_dict)
}
