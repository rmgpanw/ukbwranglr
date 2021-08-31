
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

  } else if (all(class(ukb_main) %in% c("data.table", "data.frame", "tbl_df", "tbl"))) {
    colheaders_raw <- names(ukb_main)
  } else {
    stop("Error! `ukb_main` must be either a string specifying the filepath for a UKB main dataset, or a UKb dataset loaded into R as a data frame")
  }

  # process header to this form: 'f.5912.0.0'
  colheaders <- format_ukb_df_header(colheaders_raw)

  # make mapping df
  # convert column names to a tibble and append 'mapping' columns
  data_dict <- dplyr::tibble(
    colheaders_raw = colheaders_raw,
    colheaders_processed = colheaders
  ) %>%
    # Make columns for FieldID, instance and array
    tidyr::separate(col = "colheaders_processed",
                    sep = "\\.",
                    into = c("temp", "FieldID", "instance", "array"),
                    remove= FALSE,
                    fill = "right" # 'eid' column will not separate so will raise an error without this option
    ) %>%

    # Remove column of "f"'s
    dplyr::select(-.data[["temp"]]) %>%

    # join with full ukb data dictionary
    dplyr::left_join(y = ukb_data_dict,
                     by = "FieldID")


  # mutate 'descriptive_colnames' column
  data_dict <- mutate_descriptive_columns(data_dict = data_dict)

  # mutate 'cont_int_to_na' column: indicates whether all special codings for a
  # continuous/integer variable can be cleaned to 'NA' (see also
  # `ukb_select_codings_to_na.Rmd`)
  data_dict$cont_int_to_na <- dplyr::case_when(
    # CONVERT TO NA
    data_dict$Coding %in% ukbwranglr:::cont_and_int_codings_to_na ~ TRUE,

    # DO *NOT* CONVERT TO NA
    data_dict$Coding %in% ukbwranglr:::cont_and_int_codings_NOT_to_na ~ FALSE,
    TRUE ~ FALSE
  )

  ## return data_dict
  return(data_dict)
}


# PRIVATE FUNCTIONS -------------------------------------------------------


