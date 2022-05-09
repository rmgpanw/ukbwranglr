
# NOTES -------------------------------------------------------------------



# EXPORTED FUNCTIONS ------------------------------------------------------

#' Generate a UK Biobank data dictionary
#'
#' Creates a data dictionary from either a raw UK Biobank main dataset file or a
#' data frame if this has already been loaded into R.
#'
#' @param ukb_main Either the path to a UK Biobank main dataset file (character
#'   string) or a data frame.
#' @param delim Delimiter for the UKB main dataset file. Default is "auto" (see
#'   [data.table::fread]). Ignored if the file name ends with \code{.dta} (i.e.
#'   is a \code{STATA} file) or if \code{ukb_main} is a data frame.
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
                           delim = "auto",
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

  } else if (is.data.frame(ukb_main)) {
    colheaders_raw <- names(ukb_main)
  } else {
    stop("Error! `ukb_main` must be either a string specifying the filepath for a UKB main dataset, or a UKb dataset loaded into R as a data frame")
  }

  # check no duplicated column names
  check_all_vector_values_unique(colheaders_raw,
                                 "`names(ukb_main)`")

  # process header to this form: 'f.5912.0.0'
  colheaders_processed <- format_ukb_df_header(colheaders_raw)

  # make mapping df
  # convert column names to a tibble and append 'mapping' columns
  data_dict <- dplyr::tibble(colheaders_raw = colheaders_raw,
                             colheaders_processed = colheaders_processed) %>%
    # Make columns for FieldID, instance and array
    tidyr::separate(
      col = "colheaders_processed",
      sep = "_",
      into = c("FieldID", "instance", "array"),
      remove = FALSE,
      extra = "drop",
      fill = "right" # 'eid' column will not separate so will raise an error without this option
    ) %>%

    # Remove "f"'s
    dplyr::mutate("FieldID" = stringr::str_remove(string = .data[["FieldID"]],
                                                  pattern = "^f")) %>%

    # join with full ukb data dictionary
    dplyr::left_join(y = ukb_data_dict,
                     by = "FieldID")

  # remove non-existent FieldIDs from FieldID, instance and array columns
  data_dict <- data_dict %>%
    dplyr::mutate(
      "unrecognised_fid" = dplyr::case_when(
        !(.data[["FieldID"]] %in% c("eid", ukb_data_dict$FieldID)) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("FieldID",
                                                     "instance",
                                                     "array")),
                                ~ ifelse(.data[["unrecognised_fid"]] == TRUE,
                                         yes = NA,
                                         no = .x))) %>%
    dplyr::select(-.data[["unrecognised_fid"]])

  # mutate 'descriptive_colnames' column
  data_dict <- mutate_descriptive_columns(data_dict = data_dict)

  # add ValueType column for eid - type 'Integer'
  data_dict$ValueType <- ifelse(data_dict$FieldID == "eid",
                                yes = "Integer",
                                no = data_dict$ValueType)

  # update 'Field' to 'Participant identifier' for 'eid'
  data_dict$Field <- ifelse(data_dict$FieldID == "eid",
                            yes = "Participant identifier ('eid')",
                            no = data_dict$Field)

  # return data_dict
  return(data_dict)
}


#' Read a UK Biobank main dataset file
#'
#' Reads a UK Biobank main dataset file into R using either
#' \code{\link[data.table]{fread}} or \code{\link[haven]{read_dta}}. Optionally
#' renames variables with descriptive names, add variable labels and label coded
#' values as factors.
#'
#' Note that \code{na.strings} is not recognised by
#' \code{\link[haven]{read_dta}}. Reading in a STATA file may therefore require
#' careful checking for empty strings that ned converting to \code{NA}.
#'
#' @param path The path to a UK Biobank main dataset file.
#' @param delim Delimiter for the UK Biobank main dataset file. Default is
#'   \code{\\t}
#' @param data_dict A data dictionary specific to the UKB main dataset file,
#'   generated by \code{\link{make_data_dict}}. To load only a selection of
#'   columns, supply a filtered copy of this data dictionary containing only the
#'   required variables. If \code{NULL} (default) then all fields will be read.
#' @param ukb_codings The UKB codings dictionary. This should be a data frame
#'   where all columns are of type \code{character}. By default, this is
#'   downloaded from the
#'   \href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
#'    github repo.
#' @param descriptive_colnames If \code{TRUE}, rename columns with longer
#'   descriptive names derived from the UK Biobank's data dictionary 'Field'
#'   column.
#' @param label If \code{TRUE}, apply variable labels and label coded
#'   values as factors.
#' @param max_n_labels Coded variables with associated value labels less than or
#'   equal to this threshold will be labelled as factors. If \code{NULL}, then
#'   all value labels will be applied. Default value is 30.
#' @param ... Additional parameters are passed on to either
#'   \code{\link[data.table]{fread}} or \code{\link[haven]{read_dta}}
#' @inheritParams make_data_dict
#' @inheritParams data.table::fread
#'
#' @return A UK Biobank phenotype dataset as a data table with human-readable
#'   variables labels and data values.
#' @export
read_ukb <- function(path,
                     delim = "auto",
                     data_dict = NULL,
                     ukb_data_dict = get_ukb_data_dict(),
                     ukb_codings = get_ukb_codings(),
                     descriptive_colnames = TRUE,
                     label = TRUE,
                     max_n_labels = 30,
                     na.strings = c("", "NA"),
                     nrows = Inf,
                     ...) {
  start_time <- proc.time()

  # validate args
  assertthat::assert_that(is.logical(descriptive_colnames),
                          msg = "Descriptive_colnames' must be either TRUE or FALSE")
  assertthat::assert_that(is.logical(label),
                          msg = "'label' must be either TRUE or FALSE")
  assert_integer_ge_1(max_n_labels,
                      "max_n_labels")

  # make data_dict if not supplied
  if (is.null(data_dict)) {
    message("Creating data dictionary")
    data_dict <- make_data_dict(ukb_main = path,
                                delim = delim,
                                ukb_data_dict = ukb_data_dict)
  }

  # read with haven or fread, dependent on file extension
  file_extension <- extract_file_ext(path)
  read_with <- switch(file_extension,
         dta = "read_dta",
         txt = "fread",
         tsv = "fread",
         tab = "fread",
         csv = "fread")

  assertthat::assert_that(!is.null(read_with),
                          msg = paste0("Unrecognised file extension: ", file_extension))

  N_STEPS <-  1 + descriptive_colnames + label
  STEP <- 1

  # read file
  message(paste0("STEP ", STEP, " of ", N_STEPS))
  message("Reading data into R")

  result <- read_ukb_raw_basis(
    path = path,
    delim = delim,
    data_dict = data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = na.strings,
    read_with = read_with,
    nrows = nrows,
    ...
  )

  # rename
  if (descriptive_colnames) {
    STEP <- STEP + 1
    message(paste0("STEP ", STEP, " of ", N_STEPS))
    message("Renaming with descriptive column names")

    result <-  rename_cols(
      df = result,
      old_colnames = data_dict[["colheaders_raw"]],
      new_colnames = data_dict[["descriptive_colnames"]]
    )
  }

  # label
  if (label) {
    STEP <- STEP + 1
    message(paste0("STEP ", STEP, " of ", N_STEPS))
    message("Applying variable and value labels")

    if (descriptive_colnames) {
      colnames_col <- "descriptive_colnames"
    } else {
      colnames_col <- "colheaders_raw"
    }

    result <- label_ukb_main(
      ukb_main = result,
      data_dict = data_dict,
      ukb_codings = ukb_codings,
      colnames_col = colnames_col,
      max_n_labels = max_n_labels
    )
  }

  time_taken_message(start_time)
  return(result)
}


#' Label a UK Biobank main dataset
#'
#' Applies variable labels and factor labels to coded values in a UK Biobank
#' main dataset.
#'
#' @param ukb_main A UK Biobank main dataset (data frame)
#' @param data_dict A data dictionary specific to the UKB main dataset file,
#'   generated by \code{\link{make_data_dict}}. This can be filtered for a
#'   subset of columns to be labelled.
#' @param colnames_col The name of the column in \code{data_dict} that contains
#'   column names matching those in \code{ukb_main}.
#' @inheritParams read_ukb
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' dummy_data <- dummy_main_dataset_clinical_events()
#'
#' label_ukb_main(
#'   ukb_main = dummy_data,
#'   data_dict = make_data_dict(dummy_data)
#' )
#' }
label_ukb_main <- function(ukb_main,
                           data_dict,
                           ukb_codings = get_ukb_codings(),
                           colnames_col = "descriptive_colnames",
                           max_n_labels = 30) {
  message("Labelling dataset")

  # check that ukb_main and data_dict match
  assertthat::assert_that(all(data_dict[[colnames_col]] %in% names(ukb_main)),
                          msg = "There are values in `data_dict[[colnames_col]]` that do not match `names(ukb_main)`. Check `data_dict`.")

  # warning if any fields in `data_dict` do not have a FieldID
  data_dict_fields_with_no_fid <- data_dict[is.na(data_dict$FieldID), ]$FieldID
  if (!rlang::is_empty(data_dict_fields_with_no_fid)) {
    warning(paste0("The following items in `data_dict` could not be matched to a FieldID: ",
                   stringr::str_c(data_dict_fields_with_no_fid,
                                  sep = "",
                                  collapse = ", ")))
    # remove these items from data_dict
    data_dict <- data_dict[!is.na(data_dict$FieldID), ]$FieldID
  }

  # identify codings to be converted to integer type
  data_dict <- indicate_coltype_in_data_dict(data_dict = data_dict,
                                             ukb_codings = ukb_codings)

  # filter ukb_codings for required codings only
  ukb_codings <- ukb_codings %>%
    dplyr::filter(.data[["Coding"]] %in% data_dict[["Coding"]])

  # remove duplicated codings e.g. coding 3 (for FID 20001, self-reported
  # cancers) has multiple meanings for value '-1'. These were identified
  # manually (see hashed code)

  # coding_values_with_multiple_meanings <- ukb_codings %>%
  #   dplyr::count(.data[["Coding"]],
  #                .data[["Value"]]) %>%
  #   dplyr::filter(.data[["n"]] > 1)

  ukb_codings <- ukb_codings %>%
    dplyr::mutate("to_remove" = dplyr::case_when(
      (.data[["Coding"]] == "1003") &
        (.data[["Value"]] == "-1") ~ TRUE,
      (.data[["Coding"]] == "1005") &
        (.data[["Value"]] == "-1") ~ TRUE,
      (.data[["Coding"]] == "1006") &
        (.data[["Value"]] %in% c("-1", "1440", "1443")) ~ TRUE,
      (.data[["Coding"]] == "2360") &
        (.data[["Value"]] == "-9.000e+006") ~ TRUE,
      (.data[["Coding"]] == "3") &
        (.data[["Value"]] == "-1") ~ TRUE,
      (.data[["Coding"]] == "5") &
        (.data[["Value"]] == "-1") ~ TRUE,
      (.data[["Coding"]] == "6") &
        (.data[["Value"]] == "-1") ~ TRUE,
      (.data[["Coding"]] == "87") &
        (.data[["Value"]] == "Chapter V") ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::filter(!.data[["to_remove"]]) %>%
    dplyr::select(-.data[["to_remove"]])

  # label categorical variables - set threshold for number of labels per code to
  # be applied (e.g. set threshold to 22 to label ethnic background, but not
  # label ICD codes)

  if (!is.null(max_n_labels)) {
    ukb_codings <- ukb_codings %>%
      dplyr::group_by(.data[["Coding"]]) %>%
      dplyr::mutate("n" = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data[["n"]] <= max_n_labels)
  }

  # set 'Coding' col in `data_dict` to 'NA' if excluded from `ukb_codings`
  data_dict$Coding <- ifelse(data_dict$Coding %in% ukb_codings$Coding,
                             yes = data_dict$Coding,
                             no = NA)

  # check that column types match expected coltype from data_dict
  expected_coltypes <- as.list(stats::setNames(data_dict$col_types_fread,
                          data_dict[[colnames_col]]))
  expected_coltypes <- expected_coltypes %>%
    purrr::map(~ ifelse(.x %in% c("integer", "double"),
                        yes = "numeric",
                        no = .x))

  ukb_main_cols_unexpected_type <- ukb_main %>%
    purrr::map(typeof) %>%
    .[data_dict[[colnames_col]]] %>%
    purrr::map(~ ifelse(.x %in% c("integer", "double"),
                        yes = "numeric",
                        no = .x)) %>%
    purrr::imap(~ ifelse(expected_coltypes[[.y]] == .x,
                         yes = .x,
                         no = NA)) %>%
    purrr::keep(is.na) %>%
    names()

  assertthat::assert_that(
    rlang::is_empty(ukb_main_cols_unexpected_type),
    msg = paste0(
      length(ukb_main_cols_unexpected_type),
      " column(s) are not of the expected type: ",
      stringr::str_c(ukb_main_cols_unexpected_type,
                     sep = "",
                     collapse = ", "),
      ". Try `ukbwranglr:::indicate_coltype_in_data_dict(data_dict = data_dict, ukb_codings = ukb_codings)` and check the values under column `col_types_fread`"
    )
  )

  # create column labels
  data_dict[["variable_label"]] <- ifelse(
    data_dict[[colnames_col]] == "eid",
    yes = "eid",
    no = paste0(data_dict$Field,
                " (",
                data_dict$colheaders_processed,
                ")")
  )

  # label
  label_df_by_coding(df = ukb_main,
                     data_dict = data_dict,
                     codings = ukb_codings,
                     data_dict_coding_col = "Coding",
                     codings_coding_col = "Coding",
                     data_dict_colname_col = colnames_col,
                     data_dict_variable_label_col = "variable_label",
                     codings_value_col = "Value",
                     codings_meaning_col = "Meaning",
                     data_dict_coltype_col = "col_types_fread")
}

# PRIVATE FUNCTIONS -------------------------------------------------------

#' Processes a ukb main dataset header to match the form 'f_5912_0_0'
#'
#' Helper function for \code{make_data_dict}
#'
#' @param colheaders A character vector. The first item should contain 'eid'
#'   i.e. the first column should be the eid column.
#' @param eid_first logical. If \code{TRUE} (default) then will raise error if
#'   first item in \code{colheaders} does not contain "eid"
#'
#' @return A ukb main dataset header (character vector) of the form
#'   'f_5912_0_0'. Returns the header unaltered if already in this form
#'
#' @noRd
format_ukb_df_header <- function(colheaders,
                                 eid_first = TRUE) {

  if (eid_first) {
    assertthat::assert_that(stringr::str_detect(colheaders[1],
                                                "eid"),
                            msg = "First column name should include 'eid'")

    colheaders[1] <- "feid"
  }

  # Process colheaders not of the form 'f_5912_0_0'

  # descriptive colheader format (or processed colheader format)
  colheaders <- extract_fid_instance_array_from_descriptive_or_processed_colheaders(colheaders)

  # ukb.txt format
  ukb_txt_format_indices <- stringr::str_which(string = colheaders,
                                               pattern = "^[:digit:]+-[:digit:]+\\.[:digit:]+$")

  colheaders[ukb_txt_format_indices] <- paste0("f", gsub("-",
                                                          "_",
                                                          colheaders[ukb_txt_format_indices]))
  colheaders[ukb_txt_format_indices] <- gsub("\\.",
                                             "_",
                                             colheaders[ukb_txt_format_indices])

  # r format (this converts to dta style; 'f_' is then converted to 'f' below)
  ukb_r_format_indices <- stringr::str_which(string = colheaders,
                                             pattern = "f\\.[:digit:]+\\.[:digit:]+\\.[:digit:]+$")
  colheaders[ukb_r_format_indices] <- gsub("\\.", "_", colheaders[ukb_r_format_indices])

  # ukb.dta format
  ukb_dta_format_indices <- stringr::str_which(string = colheaders,
                                               pattern = "^[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+$")

  colheaders[ukb_dta_format_indices] <- paste0("f", stringr::str_replace(colheaders[ukb_dta_format_indices],
                                                                                         pattern = "^[:alpha:]+_",
                                                                                         replacement = ""))

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
                               paste0("f",
                                      data_dict$FieldID))

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

  # check for duplicate names - set to 'colheaders raw' if this is unique
  duplicated_descriptive_colnames <- data_dict %>%
    dplyr::group_by(.data[["descriptive_colnames"]]) %>%
    dplyr::mutate("n" = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[["n"]] > 1)

  if (nrow(duplicated_descriptive_colnames) > 0) {

    assertthat::assert_that(length(unique(data_dict$colheaders_raw)) == nrow(data_dict),
                            msg = "Data dictionary contains non-unique values in `colhedaers_raw` column")

    data_dict <- data_dict %>%
      dplyr::mutate(
        "descriptive_colnames" = dplyr::case_when(
          .data[["descriptive_colnames"]] %in% duplicated_descriptive_colnames$descriptive_colnames ~ .data[["colheaders_raw"]],
          TRUE ~ .data[["descriptive_colnames"]]
        )
      )

    warning(
      paste0(
        "\nWarning. Unable to generate descriptive colnames for ",
        length(unique(
          duplicated_descriptive_colnames$colheaders_raw
        )),
        " columns.\n"
      )
    )
  }

  # remove "_na"(s) from end of column names
  data_dict$descriptive_colnames <- stringr::str_remove(data_dict$descriptive_colnames,
                                                         pattern = "[_na]+$")

  # if original colnames include Field_FieldID, then use these as descriptive
  # colnames (apart from for 'eid' col) - this step is designed for colnames generated by
  # `summarise_numerical_variables()`
  data_dict$descriptive_colnames <- ifelse((stringr::str_detect(string = data_dict$colheaders_raw,
                                                               pattern = data_dict$Field_FieldID)) &
                                             (data_dict$Field_FieldID != "eid"),
                                           yes = data_dict$colheaders_raw,
                                           no = data_dict$descriptive_colnames)

  # make `descriptive_colnames` = `colheaders_raw` if fields/instance/array are
  # all `NA`
  data_dict$descriptive_colnames <- ifelse((is.na(data_dict$FieldID)) &
                                             (is.na(data_dict$instance) &
                                                is.na(data_dict$array)),
                                           yes = data_dict$colheaders_raw,
                                           no = data_dict$descriptive_colnames)

  # remove Field_FieldID column
  data_dict$Field_FieldID <- NULL

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

  # add column to data_dict indicating column type (either 'ValueType` is
  # already 'integer', or the coded value is coercible to integer as identified
  # above)
  data_dict <- data_dict %>%
    dplyr::mutate("col_types_readr" = dplyr::case_when(((.data[["ValueType"]] == "Integer") |
                                                          (.data[["Coding"]] %in% ukb_codings_coercible_to_integer) |
                                                          (.data[["FieldID"]] == "eid")
    ) ~ "i",
    # Default is type character
    TRUE ~ "c")) %>%
    # ValueType 'Continuous' overrides the above
    dplyr::mutate(
      "col_types_readr" = dplyr::case_when(.data[["ValueType"]] == "Continuous" ~ "d",
                                           .data[["ValueType"]] == "Date" ~ "c",
                                           TRUE ~ .data[["col_types_readr"]])
    ) %>%
    dplyr::mutate(
      "col_types_fread" = dplyr::case_when(
        .data[["col_types_readr"]] == "i" ~ "integer",
        .data[["col_types_readr"]] == "d" ~ "double",
        .data[["col_types_readr"]] == "D" ~ "character",
        .data[["col_types_readr"]] == "c" ~ "character"
      )
    )

  return(data_dict)
}

read_ukb_raw_basis <- function(path,
                               delim = "auto",
                               data_dict,
                               ukb_data_dict = get_ukb_data_dict(),
                               ukb_codings = get_ukb_codings(),
                               na.strings = c("", "NA"),
                               read_with = "fread",
                               callback = readr::DataFrameCallback$new(function(x, pos) x),
                               chunk_size = 10000,
                               nrows = Inf,
                               ...) {
  # low level function to read a UKB main dataset file specifying col types. Can
  # use either data.table, readr (chunked) or haven

  # validate args
  match.arg(read_with,
            choices = c("fread", "read_delim_chunked", "read_dta"))

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
  switch(
    read_with,
    fread = data.table::fread(
      path,
      select = coltypes_fread,
      na.strings = na.strings,
      sep = delim,
      nrows = nrows,
      ...
    ),
    read_delim_chunked = readr::read_delim_chunked(
      file = path,
      callback = callback,
      delim = delim,
      na = na.strings,
      col_types = coltypes_readr,
      chunk_size = chunk_size,
      ...
    ),
    read_dta = haven::read_dta(
      file = path,
      col_select = tidyselect::all_of(data_dict$colheaders_raw),
      n_max = nrows,
      ...
    ) %>%
      update_column_classes(coltypes = coltypes_fread)
  )
}

#' Programmatically label variables/values in a data frame
#'
#' Low level helper function. Uses the \code{haven} package.
#'
#' @param df data frame
#' @param data_dict data dictionary
#' @param codings codings
#' @param data_dict_coding_col column name
#' @param data_dict_colname_col column name
#' @param data_dict_variable_label_col column name
#' @param data_dict_coltype_col column name
#' @param codings_coding_col column name
#' @param codings_value_col column name
#' @param codings_meaning_col column name
#'
#' @noRd
#' @return A labelled dataframe
label_df_by_coding <- function(df,
                               data_dict,
                               codings,
                               data_dict_coding_col = "coding",
                               codings_coding_col = "coding",
                               data_dict_colname_col = "colname",
                               data_dict_variable_label_col = "variable_label",
                               data_dict_coltype_col = "col_types",
                               codings_value_col = "value",
                               codings_meaning_col = "meaning") {
  # convert data_dict and codings to named lists
  data_dict_list <-
    split(data_dict, data_dict[[data_dict_coding_col]])
  codings_list <- split(codings, codings[[codings_coding_col]])

  # all codings to label
  all_codings <- names(codings_list)

  # integer/double codings - note that some continuous variables have coded
  # values (e.g. FID 20006, interpolated year when cancer first diagnosed)
  integer_codings <- data_dict %>%
    dplyr::filter(.data[[data_dict_coltype_col]] %in% c("integer", "double")) %>%
    dplyr::filter(!is.na(.data[[data_dict_coding_col]])) %>%
    dplyr::pull(.data[[data_dict_coding_col]]) %>%
    as.integer()

  non_coded_columns_to_label <- data_dict %>%
    dplyr::filter(is.na(.data[[data_dict_coding_col]])) %>%
    dplyr::pull(.data[[data_dict_colname_col]])

  # progress bar - one tick per column
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = nrow(data_dict))
  pb$tick(0)

  # loop through codings
  for (single_coding in all_codings) {

    # colnames using this coding
    columns_to_label <-
      data_dict_list[[single_coding]][[data_dict_colname_col]]

    # value labels for these columns
    if (single_coding %in% integer_codings) {
      value_labels <-
        sort(stats::setNames(object = as.integer(codings_list[[single_coding]][[codings_value_col]]),
                        nm = codings_list[[single_coding]][[codings_meaning_col]]))
    } else {
      value_labels <-
        stats::setNames(object = codings_list[[single_coding]][[codings_value_col]],
                        nm = codings_list[[single_coding]][[codings_meaning_col]])
    }

    # label variables and values
    for (column in columns_to_label) {
      # progress bar
      pb$tick(1)

      variable_label = data_dict[data_dict[[data_dict_colname_col]] == column, data_dict_variable_label_col][[1]]

      # checks
      assertthat::assert_that(!is.null(df[[column]]),
                              msg = paste0("Error while labelling columns: column ", column, " does not exist. Try checking data dictionary"))

      if (is.na(variable_label)) {
        variable_label <- NULL
      }

      # for debugging
      # print(column)

      df[[column]] <- factor(df[[column]],
                                      levels = value_labels,
                                      labels = names(value_labels))

      attributes(df[[column]])$label <- variable_label
    }
  }

  # label remaining variables (i.e. those without associated codings/value labels)
  for (column in non_coded_columns_to_label) {

    pb$tick(1)

    # for debugging
    # print(column)
    attributes(df[[column]])$label <- data_dict[data_dict[[data_dict_colname_col]] == column, data_dict_variable_label_col][[1]]
  }

  # error if nothing was labelled
  assertthat::assert_that(!rlang::is_empty(all_codings) | !rlang::is_empty(non_coded_columns_to_label),
                          msg = "No value or variable labels were applied")

  return(df)
}

update_column_classes <- function(df,
                                  coltypes) {
  # coltypes must be a named character vector with the following allowed values
  assertthat::assert_that(all(
    unique(coltypes) %in% c("character",
                            "Date",
                            "double",
                            "integer")
  ))

  # for each class, coerce all relevant columns to this class
  message("Updating column classes")

  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = length(unique(coltypes)))
  pb$tick(0)

  for (column_type in unique(coltypes)) {
    # progress bar
    pb$tick(1)

    columns_to_reclass <-
      names(subset(coltypes, coltypes == column_type))

    df <- switch(
      column_type,
      character = df %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(columns_to_reclass),
          as.character
        )),
      Date =  df %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(columns_to_reclass),
          as.Date
        )),
      double =  df %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(columns_to_reclass),
          as.double
        )),
      integer =  df %>%
        dplyr::mutate(dplyr::across(
          tidyselect::all_of(columns_to_reclass),
          as.integer
        ))
    )
  }

  return(df)
}


# Dev ---------------------------------------------------------------------

#' Read a UKB main dataset in chunks, process and write to a file
#'
#' @param in_path .
#' @param out_path .
#' @param data_dict .
#' @param in_delim .
#' @param ukb_data_dict .
#' @param ukb_codings .
#' @param max_n_labels .
#' @param descriptive_colnames .
#' @param label .
#' @param na.strings .
#' @param chunk_size .
#' @param ... additional arguments are passed to
#'   \code{read_ukb_chunked_callback_write_to_file}
#'
#' @return Returns \code{NULL} invisibly.
#' @noRd
#'
#' @examples
#' \dontrun{
#' }
read_ukb_chunked_to_file <- function(in_path,
                                     out_path,
                                     data_dict,
                                     in_delim = "auto",
                                     ukb_data_dict = get_ukb_data_dict(),
                                     ukb_codings = get_ukb_codings(),
                                     max_n_labels = 22,
                                     descriptive_colnames = FALSE,
                                     label = FALSE,
                                     na.strings = c("", "NA"),
                                     chunk_size = 10000,
                                     ...) {
  start_time <- proc.time()

  # validate in_path
  in_path_ext <- extract_file_ext(in_path)
  out_path_ext <- extract_file_ext(out_path)

  assertthat::assert_that(in_path_ext %in% c("txt", "tsv", "csv", "tab"),
                          msg = "`in_path` file extension must be one of 'txt', 'tsv', 'csv', 'tab'")

  assertthat::assert_that(out_path_ext %in% c("txt", "tsv", "csv", "dta", "rds"),
                          msg = "`in_path` file extension must be one of 'txt', 'tsv', 'csv', 'tab'")

  assertthat::assert_that(!((out_path_ext == "dta") &
                              (descriptive_colnames == TRUE)),
                          msg = "`descriptive_colnames` cannot be `TRUE` if writing to a STATA file")

  data_dict_full <- make_data_dict(in_path,
                                   delim = in_delim,
                                   ukb_data_dict = ukb_data_dict)

  # check that ukb_main and data_dict match
  assertthat::assert_that(all(data_dict$colheaders_raw %in% data_dict_full$colheaders_raw),
                          msg = "Values in `data_dict$colheaders_raw` do not match column names for file at `in_path`. Check `data_dict`.")



  # write result to csv/tsv/dta/rds
  read_ukb_raw_basis(
    path = in_path,
    delim = in_delim,
    data_dict = data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = na.strings,
    read_with = "read_delim_chunked",
    chunk_size = chunk_size,
    callback = readr::SideEffectChunkCallback$new(read_ukb_chunked_callback_write_to_file(out_path = out_path,
                                                                                          out_path_ext = out_path_ext,
                                                                                          max_n_labels = max_n_labels,
                                                                                          data_dict = data_dict,
                                                                                          ukb_codings = ukb_codings,
                                                                                          start_time = start_time,
                                                                                          descriptive_colnames = descriptive_colnames,
                                                                                          label = label,
                                                                                          ...)),
    ...
  )

  invisible(NULL)
}

read_ukb_chunked_callback_write_to_file <-
  function(out_path,
           out_path_ext,
           max_n_labels,
           data_dict,
           ukb_codings,
           start_time,
           descriptive_colnames = descriptive_colnames,
           label = label,
           ...) {
    # a function factory for `read_ukb_chunked_to_file()`
    progress_update <- function(start_time, pos) {
      time_taken <- proc.time() - start_time

      message(
        "Writing from line ",
        pos,
        ". Time taken: ",
        (time_taken[3] %/% 60),
        " minutes, ",
        (round(time_taken[3] %% 60)),
        " seconds"
      )
    }

    function(x, pos) {
      progress_update(start_time, pos)

      # rename columns - either descriptive or processed
      if (descriptive_colnames) {
        NEW_COLNAMES_COL <- "descriptive_colnames"

        x <-  rename_cols(
          df = x,
          old_colnames = data_dict[["colheaders_raw"]],
          new_colnames = data_dict[[NEW_COLNAMES_COL]]
        )
      } else if (!descriptive_colnames) {
        NEW_COLNAMES_COL <- "colheaders_processed"

        x <- rename_cols(
          df = x,
          old_colnames = data_dict[["colheaders_raw"]],
          new_colnames = data_dict[[NEW_COLNAMES_COL]]
        )
      }

      if (label) {
        x <- label_ukb_main(
          ukb_main = x,
          data_dict = data_dict,
          ukb_codings = ukb_codings,
          colnames_col = NEW_COLNAMES_COL,
          max_n_labels = max_n_labels
        )
      }


      switch(
        out_path_ext,
        dta = haven::write_dta(data = x,
                               path = out_path,
                               ...),
        txt = readr::write_delim(
          x = x,
          file = out_path,
          delim = "\t",
          ...
        ),
        tsv = readr::write_delim(
          x = x,
          file = out_path,
          delim = "\t",
          ...
        ),
        csv = readr::write_delim(
          x = x,
          file = out_path,
          delim = ",",
          ...
        ),
        rds = saveRDS(object = x,
                      file = out_path,
                      ...)
      )
    }
  }
