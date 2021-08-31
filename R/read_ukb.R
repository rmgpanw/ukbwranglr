
# NOTES -------------------------------------------------------------------



# EXPORTED FUNCTIONS ------------------------------------------------------

#' Generate a UK Biobank data dictionary
#'
#' Creates a data dictionary from either a raw UK Biobank main dataset file or a
#' data frame if this has already loaded into R.
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
#'   includes a column of descriptive names.
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

  ## return data_dict
  return(data_dict)
}


# PRIVATE FUNCTIONS -------------------------------------------------------

#' Processes a ukb main dataset header to match the form 'f.5912.0.0'
#'
#' Helper function for \code{make_data_dict()}
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
      .data[["Field_FieldID"]],
      tidyselect::everything()
    )

  return(data_dict)
}
