# NOTES -------------------------------------------------------------------

# Steps to append additional `clinical_events_sources` for
# `tidy_clinical_events()`:

# 1. Update `CLINICAL_EVENTS_FIELD_IDS` and `CLINICAL_EVENTS_SOURCES` in
# `metadata.R`

# 2. Update relevant details in `tidy_clinical_events()`

# 3. Add dummy data fields to `DUMMY_UKB_MAIN_CLINICAL_EVENTS` in `dummy_data.R`

# 4. Update end of `filter_clinical_events()` (from comment '***Update this
# section ...)

# 5. Add tests to `test_extract_phenotypes.R` under `tidy_clinical_events()`
# section

# 6. Run tests in `test_extract_phenotypes.R` and fix as necessary


# CONSTANTS ---------------------------------------------------------------

CLINICAL_EVENTS_COLHEADERS <- c(
  "eid",
  "source",
  "index",
  "code",
  "date"
)

CLINICAL_EVENTS_COLTYPES <- c(
  "numeric",
  "character",
  "character",
  "character",
  "character"
)

# EXPORTED FUNCTIONS ----------------------------------------------------


# Mutate indicator columns ------------------------------------------------

#' Mutate age at event columns
#'
#' Mutates numeric columns with the age at event (in years) for all columns that match the
#' regex supplied to \code{date_col_regex}.
#'
#' @param ukb_main a UKB main dataset (data frame) containing a column named
#'   "eid" and a date of birth column, as well as date columns matching the
#'   regex supplied to the \code{date_col_regex} argument.
#' @param dob_col character. The name of the date of birth column in
#'   \code{ukb_main}.
#' @param date_col_regex character. A regular expression that matches the date
#'   columns to be processed in \code{ukb_main}.
#' @param date_col_regex_replacement character. New "age-at-event" columns will
#'   have names matching those of the input date columns, replacing the regex
#'   supplied to \code{date_col_regex} with this string.
#'
#' @return A data frame with additional "age at event" columns (in years).
#' @export
#'
#' @examples
#' # dummy data
#' dummy_ukb_main <- tibble::tribble(
#'   ~eid, ~dob, ~event_date,
#'   1, "2000-01-01", "2010-01-01"
#' )
#'
#' # mutate age at event col
#' mutate_age_at_event_cols(
#'   dummy_ukb_main,
#'   dob_col = "dob",
#'   date_col_regex = "_date$",
#'   date_col_regex_replacement = "_age"
#' )
mutate_age_at_event_cols <- function(ukb_main,
                                     dob_col = "dob",
                                     date_col_regex = "_date$",
                                     date_col_regex_replacement = "_age") {
  # check eid and dob cols exist
  assertthat::assert_that(
    all(c("eid", dob_col) %in% names(ukb_main)),
    msg = paste0("Error! Either the 'eid' or '", dob_col, "' columns are absent from ukb_main")
  )

  # date columns to be processed
  date_cols_to_process <-
    subset(names(ukb_main), stringr::str_detect(names(ukb_main), date_col_regex))

  assertthat::assert_that(
    !rlang::is_empty(date_cols_to_process),
    msg = paste0("Error! No columns found matching, ", date_col_regex)
  )

  # loop through columns whose names match `date_col_regex`. These should all be
  # date columns. For each one, mutate an additional column: age at this event
  # date
  for (column in date_cols_to_process) {
    new_colname <-
      stringr::str_replace(column, pattern = date_col_regex, replacement = date_col_regex_replacement)
    message(paste0(
      "Calculating age at event for ",
      column,
      ". New colname: ",
      new_colname
    ))
    ukb_main[[new_colname]] <-
      lubridate::time_length(as.Date(ukb_main[[column]]) - as.Date(ukb_main[[dob_col]]), unit = "year")

    new_colname_label <- stringr::str_replace(attributes(ukb_main[[column]])$label,
      pattern = "date (.+)$",
      replacement = "age"
    )

    if (!rlang::is_empty(new_colname_label)) {
      attributes(ukb_main[[new_colname]])$label <- new_colname_label
    }
  }

  # return ukb_main with additional age at event cols
  return(ukb_main)
}

# Extract phenotypes from clinical events -------------------------------------------------------

#' Extract phenotypes from clinical events data
#'
#' Filters a clinical events table created by \code{\link{tidy_clinical_events}}
#' for a set clinical codes that represent one or more phenotypes.
#'
#' @param clinical_events A long format data frame created by
#'   \code{\link{tidy_clinical_events}}, \code{\link{tidy_gp_clinical}},
#'   \code{\link{tidy_gp_scripts}} or \code{\link{make_clinical_events_db}}.
#'   This can also be a \code{\link[dbplyr]{tbl_dbi}} object.
#' @param clinical_codes data frame. Must match the format as per
#'   \code{\link{example_clinical_codes}}.
#' @param verbose If `TRUE` (default), display progress messages.
#' @param source_filter Character vector of data sources to filter for
#'   (optional).
#'
#' @return A data frame.
#' @export
#' @family clinical events
#' @examples
#' library(magrittr)
#'
#' # dummy clinical events data frame
#' dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
#' dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
#'
#' dummy_clinical_events <- read_ukb(
#'   path = get_ukb_dummy("dummy_ukb_main.tsv", path_only = TRUE),
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   ukb_codings = dummy_ukb_codings
#' ) %>%
#'   tidy_clinical_events(
#'     ukb_data_dict = dummy_ukb_data_dict,
#'     ukb_codings = dummy_ukb_codings
#'   ) %>%
#'   dplyr::bind_rows()
#'
#' head(dummy_clinical_events)
#'
#' # dummy clinical code list
#' example_clinical_codes()
#'
#' # Filter for participants with matching clinical codes
#' extract_phenotypes(
#'   clinical_events = dummy_clinical_events,
#'   clinical_codes = example_clinical_codes()
#' )
extract_phenotypes <- function(clinical_events,
                               clinical_codes,
                               source_filter = NULL,
                               verbose = TRUE) {

  start_time <- proc.time()

  # validate args -----
  # clinical_events
  validate_clinical_events_and_check_type(clinical_events)

  # clinical_codes
  validate_clinical_codes(clinical_codes)

  # source_filter
  if (!is.null(source_filter)) {
    assertthat::assert_that(is.character(source_filter),
                            msg = "`source_filter` should be type character")

    assertthat::assert_that(assertthat::noNA(source_filter),
                            msg = "`source_filter` must not contain `NA` values")

    invalid_data_sources <-
      subset(
        source_filter,
        !source_filter %in% clinical_events_sources()$source
      )

    assertthat::assert_that(
      length(invalid_data_sources) == 0,
      msg = paste0(
        "Error! The following values in `source_filter` are not valid: ",
        stringr::str_c(invalid_data_sources,
                       sep = "",
                       collapse = ", ")
      )
    )
  } else {
    source_filter <- clinical_events_sources()$source
  }


  # filter `clinical_events` -------
  # filter `clinical_events` for codes in `clinical_codes` (regardless of code
  # type at this stage)

  if(verbose) {
    message("Filtering for requested clinical codes/sources")
  }

  clinical_events <- clinical_events %>%
    dplyr::filter((.data[["code"]] %in% local(unique(
      clinical_codes$code
    ))) &
      (.data[["source"]] %in% local(unique(source_filter)))) %>%
    dplyr::collect()

  # join with `clinical_codes` ---------
  # append code_type

  if (verbose) {
    message("Joining filtered events with clinical codelist")
  }

  source_to_code_type_map <- clinical_events_sources() %>%
    dplyr::select(.data[["source"]],
                  "code_type" = .data[["data_coding"]])

  # check that source-to-code_type mapping table has only unique values under `source` col
  stopifnot(
    dplyr::n_distinct(source_to_code_type_map$source) == nrow(source_to_code_type_map)
  )

  clinical_events <- clinical_events %>%
    dplyr::left_join(source_to_code_type_map,
                     by = "source")

  # perform filtering join with clinical codelist
  result <- clinical_events %>%
    dplyr::inner_join(clinical_codes %>%
                        dplyr::select(-.data[["description"]]),
                      by = c("code",
                             "code_type"))

  if (verbose) {
    time_taken_message(start_time)
  }

  return(result)
}

# Tidy clinical events -------------------------------------------------------


#' Details for UK Biobank clinical events data
#'
#' Returns a data frame with details of clinical events data that may be tidied
#' by [tidy_clinical_events()].
#'
#' @return A data frame
#' @export
#'
#' @family clinical events
#' @examples
#' clinical_events_sources()
clinical_events_sources <- function() {
  CLINICAL_EVENTS_SOURCES
}

#' Tidy clinical events data from a UK Biobank main dataset
#'
#' Data in a UK Biobank main dataset is stored in wide format i.e. a single row
#' of data per UK Biobank participant ('eid's). Clinical events may be
#' ascertained from numerous sources (e.g. self-reported medical conditions,
#' linked hospital records) with coded events and their associated dates
#' recorded across multiple columns. This function tidies this data into a
#' standardised long format table.
#'
#' A named list of data frames is returned, with the names corresponding to the
#' data sources specified by \code{clinical_events}. Each data frame has the
#' following columns:
#'
#' \itemize{ \item \code{eid} - participant identifier \item \code{source} - the
#' FieldID (prefixed by 'f') where clinical codes were extracted from. See
#' \code{\link{clinical_events_sources}} for further details. \item \code{index}
#' - the corresponding instance and array (e.g. '0-1' means instance 0 and array
#' 1) \item \code{code} - clinical code. The type of clinical codings system
#' used depends on \code{source}. \item \code{date} - associated date. Note that
#' in cases where participants self-reported a medical condition but recorded
#' the date as either 'Date uncertain or unknown' or 'Preferred not to answer'
#' (see \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding
#' 13}) then the date is set to \code{NA}.}
#'
#' @section Other notes:
#'
#'   Results may be combined into a single data frame using
#'   \code{\link[dplyr]{bind_rows}}.
#'
#' @param ukb_main A UK Biobank main dataset.
#' @param clinical_events_sources A character vector of clinical events sources
#'   to tidy. By default, all available options are included.
#' @param strict If \code{TRUE}, raise an error if required columns for any
#'   clinical events sources listed in \code{clinical_events} are not present in
#'   \code{ukb_main}. If \code{FALSE}, then a warning message will be displayed
#'   instead. Default value is \code{TRUE}.
#' @param .details_only If \code{TRUE}, return a list detailing required Field
#'   IDs
#' @inheritParams read_ukb
#'
#' @return A named list of clinical events data frames.
#' @export
#' @family clinical events
#'
#' @examples
#' # dummy UKB main dataset and metadata
#' dummy_ukb_main <- get_ukb_dummy("dummy_ukb_main.tsv")
#' dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
#' dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
#'
#' # tidy clinical events in a UK Biobank main dataset
#' clinical_events <- tidy_clinical_events(
#'   ukb_main = dummy_ukb_main,
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   ukb_codings = dummy_ukb_codings
#' )
#'
#' # returns a named list of data frames, one for each `clinical_events_source`
#' names(clinical_events)
#'
#' clinical_events$summary_hes_icd10
#'
#' # use .details_only = TRUE to return details of required Field IDs for
#' # specific clinical_events sources
#' tidy_clinical_events(.details_only = TRUE)
tidy_clinical_events <- function(ukb_main,
                                 ukb_data_dict = get_ukb_data_dict(),
                                 ukb_codings = get_ukb_codings(),
                                 clinical_events_sources = c(
                                   "primary_death_icd10",
                                   "secondary_death_icd10",
                                   "self_report_medication",
                                   "self_report_non_cancer",
                                   "self_report_non_cancer_icd10",
                                   "self_report_cancer",
                                   "self_report_operation",
                                   "cancer_register_icd9",
                                   "cancer_register_icd10",
                                   "summary_hes_icd9",
                                   "summary_hes_icd10",
                                   "summary_hes_opcs3",
                                   "summary_hes_opcs4"
                                 ),
                                 strict = TRUE,
                                 .details_only = FALSE) {

  # validate args
  assertthat::assert_that(length(clinical_events_sources) == length(unique(clinical_events_sources)),
    msg = "Error! `clinical_events_sources` contains duplicate values"
  )

  unrecognised_clinical_events <- subset(
    clinical_events_sources,
    !clinical_events_sources %in% names(CLINICAL_EVENTS_FIELD_IDS)
  )

  assertthat::assert_that(rlang::is_empty(unrecognised_clinical_events),
    msg = paste0(
      "Error! `clinical_events_sources` includes unrecognised values: ",
      stringr::str_c(unrecognised_clinical_events,
        sep = "",
        collapse = ", "
      ),
      ". Please choose from: ",
      stringr::str_c(names(CLINICAL_EVENTS_FIELD_IDS),
        sep = "",
        collapse = ", "
      )
    )
  )

  # if required field IDs requested
  if (.details_only) {
    return(
      list(required_field_ids = CLINICAL_EVENTS_FIELD_IDS)
    )
  }

  assertthat::assert_that(
    is.data.frame(ukb_main),
    msg = "Error! `ukb_main` must be a data frame"
  )

  if (!data.table::is.data.table(ukb_main)) {
    warning("`ukb_main` must be a data table, attempting to coerce...")
    ukb_main <- data.table::as.data.table(ukb_main)
  }

  assertthat::assert_that(
    "eid" %in% names(ukb_main),
    msg = "Error! `ukb_main` must contain a column called `eid`"
  )

  # make data dictionary
  data_dict <- make_data_dict(ukb_main,
    ukb_data_dict = ukb_data_dict
  )

  # check if required columns are present
  filter_data_dict_safely <- purrr::safely(filter_data_dict)

  clinical_events_with_missing_required_cols <-
    CLINICAL_EVENTS_FIELD_IDS[clinical_events_sources] %>%
    purrr::imap(
      ~ filter_data_dict_safely(
        data_dict = data_dict,
        filter_col = "FieldID",
        filter_value = .x,
        return_col = "colheaders_raw",
        error_if_missing = FALSE
      )
    ) %>%
    purrr::keep(.p = ~ !is.null(.x$error)) %>%
    names()

  if (!rlang::is_empty(clinical_events_with_missing_required_cols)) {
    missing_required_cols_message <-
      paste0(
        "Required FieldID columns are missing for the following in `clinical_events_sources`: ",
        stringr::str_c(
          clinical_events_with_missing_required_cols,
          sep = "",
          collapse = ", "
        ),
        ". Use `.details_only = TRUE` for a list of required Field IDs"
      )

    if (strict) {
      stop(paste0("Error! ", missing_required_cols_message, " or try setting `strict = FALSE`"))
    } else {
      if (all(clinical_events_sources %in% clinical_events_with_missing_required_cols)) {
        stop(
          "Required FieldID columns are missing for all requested `clinical_events_sources`. Use `.details_only = TRUE` for a list of required Field IDs"
        )
      } else {
        warning(paste0("Warning! ", missing_required_cols_message))
        # subset for available clinical events only
        clinical_events_sources <- subset(clinical_events_sources,
          subset = !(clinical_events_sources %in% clinical_events_with_missing_required_cols)
        )
      }
    }
  }

  # To use in loop below - determine how event_type should be processed
  death_cancer_summary_hes_self_report_meds <- c(
    "cancer_register_icd9",
    "cancer_register_icd10",
    "summary_hes_icd9",
    "summary_hes_icd10",
    "summary_hes_opcs3",
    "summary_hes_opcs4",
    "primary_death_icd10",
    "secondary_death_icd10",
    "self_report_medication"
  )

  self_report <- c(
    "self_report_non_cancer",
    "self_report_cancer",
    "self_report_operation"
  )
  self_report_non_cancer_icd10 <- "self_report_non_cancer_icd10"

  # (internal check that all options are covered)
  assertthat::are_equal(
    sort(c(
      death_cancer_summary_hes_self_report_meds,
      self_report,
      self_report_non_cancer_icd10
    )),
    sort(names(CLINICAL_EVENTS_FIELD_IDS))
  )

  # make empty list for results
  result <- vector(mode = "list", length = length(clinical_events_sources))
  names(result) <- clinical_events_sources

  # loop through clinical_events_sources
  for (event_type in clinical_events_sources) {
    message(paste0("Tidying clinical events for ", event_type))

    # determine how clinical events will be tidied
    tidying_option <- dplyr::case_when(
      event_type %in% self_report ~ "self_report",
      event_type == self_report_non_cancer_icd10 ~ "self_report_non_cancer_icd10",
      event_type %in% death_cancer_summary_hes_self_report_meds ~ "death_cancer_summary_hes_self_report_meds"
    )

    # tidy clinical events
    result[[event_type]] <- switch(tidying_option,
      self_report = tidy_clinical_events_basis(
        ukb_main = ukb_main,
        data_dict = data_dict,
        ukb_codings = ukb_codings,
        data_dict_colname_col = "colheaders_raw",
        code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
        date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"]
      ) %>%
        make_self_report_special_decimal_dates_na() %>%
        dplyr::mutate("date" = as.character(
          lubridate::as_date(
            lubridate::date_decimal(as.numeric(.data[["date"]]))
          )
        )),
      self_report_non_cancer_icd10 = tidy_clinical_events_basis(
        ukb_main = ukb_main,
        data_dict = data_dict,
        ukb_codings = ukb_codings,
        data_dict_colname_col = "colheaders_raw",
        code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
        date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"]
      ) %>%
        make_self_report_special_decimal_dates_na() %>%
        dplyr::mutate("date" = as.character(
          lubridate::as_date(
            lubridate::date_decimal(as.numeric(.data[["date"]]))
          )
        )) %>%
        recode_self_report_non_cancer_diagnoses_to_icd10(
          data_dict = data_dict,
          ukb_codings = ukb_codings
        ),
      death_cancer_summary_hes_self_report_meds = tidy_clinical_events_basis(
        ukb_main = ukb_main,
        data_dict = data_dict,
        ukb_codings = ukb_codings,
        data_dict_colname_col = "colheaders_raw",
        code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
        date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"]
      )
    )
  }

  # check for any dfs with no rows - set to `NULL`
  result <- result %>%
    purrr::imap(~ {
      if (nrow(.x) == 0) {
        message("No clinical events identified for ", .y)
        NULL
      } else {
        .x
      }
    })

  return(result)
}

# Clinical code lists -----------------------------------------------------

#' Generate an example data frame of clinical codes for diabetes
#'
#' Data frames of this format may be used with \code{\link{extract_phenotypes}}.
#' Use \code{\link{validate_clinical_codes}} to check whether a clinical codes
#' data frame meets all requirements.
#'
#' @return A data frame of selected clinical codes (non-exhaustive) for diabetes.
#' @export
#' @family clinical events
#' @seealso \code{\link{validate_clinical_codes}}
#'
#' @examples
#' example_clinical_codes()
example_clinical_codes <- function() {
  tibble::tribble(
    ~disease, ~description, ~category, ~code_type, ~code, ~author,
    "Diabetes", "diabetes", "Diabetes unspecified", "data_coding_6", "1220", "ukbwr",
    "Diabetes", "gestational diabetes", "Gestational diabetes", "data_coding_6", "1221", "ukbwr",
    "Diabetes", "type 1 diabetes", "Type 1 DM", "data_coding_6", "1222", "ukbwr",
    "Diabetes", "type 2 diabetes", "Type 2 DM", "data_coding_6", "1223", "ukbwr",
    "Diabetes", "Type 1 diabetes mellitus", "Type 1 DM", "icd10", "E10", "ukbwr",
    "Diabetes", "Type 2 diabetes mellitus", "Type 2 DM", "icd10", "E11", "ukbwr",
    "Diabetes", "Insulin dependent diabetes mellitus", "Type 1 DM", "read2", "C108.", "ukbwr",
    "Diabetes", "Non-insulin dependent diabetes mellitus", "Type 2 DM", "read2", "C109.", "ukbwr",
  )
}


# PRIVATE FUNCTIONS -------------------------------------------------------


# Tidy clinical events helpers ----------------------------------------------------

#' Melt diagnoses code and date columns in a UKB main dataset
#'
#' For most clinical events variables in the UKB main dataset there is a
#' diagnostic code variable and an associated date of event variable. There are
#' spread over several instances/arrays. This function melts these to long
#' format. Note that dates are handled differently for death data and
#' self-reported medications.
#'
#' @inheritParams read_ukb
#' @param data_dict_colname_col character. The column in \code{data_dict} with
#'   the column names for \code{ukb_main}
#' @param code_col_field_id character. The Field ID representing 'code'
#'   variables
#' @param date_col_field_id character. The Field ID representing 'date'
#'   variables that correspond to \code{code_col_field_id}.
#'
#' @noRd
#' @return A data table with 'eid', 'source' (Field ID for the code column),
#'   'index' (instance_array), 'code' and 'date' columns.
tidy_clinical_events_basis <- function(ukb_main,
                                       data_dict,
                                       data_dict_colname_col,
                                       ukb_codings,
                                       code_col_field_id,
                                       date_col_field_id) {
  start_time <- proc.time()

  assertthat::assert_that(
    is.character(code_col_field_id) & is.character(date_col_field_id),
    msg = "Error! Both `code_col_field_id` and `date_col_field_id` must be of type character"
  )

  assertthat::assert_that(
    # need `all()` as death data has multiple code cols
    all(code_col_field_id %in% data_dict$FieldID) &
      (date_col_field_id %in% data_dict$FieldID),
    msg = paste0(
      "Error! Both FieldID ",
      stringr::str_c(code_col_field_id, sep = "", collapse = ", "),
      " and FieldID ",
      date_col_field_id,
      " are required, one or both of these is not present in data_dict"
    )
  )

  # get lists of required columns
  code_cols <- filter_data_dict(
    data_dict = data_dict,
    filter_col = "FieldID",
    filter_value = code_col_field_id,
    return_col = data_dict_colname_col,
    error_if_missing = TRUE
  )

  date_cols <- filter_data_dict(
    data_dict = data_dict,
    filter_col = "FieldID",
    filter_value = date_col_field_id,
    return_col = data_dict_colname_col,
    error_if_missing = TRUE
  )

  # Note: `code_cols` and `date_cols` may be of different lengths e.g. try searching
  # the UKB data showcase for FieldIDs 40005 (date of cancer diagnosis) and
  # 40013 (type of cancer ICD-9). Some instance-arrays may also be omitted if
  # they contain no data, resulting in apparently 'missing' columns (e.g.
  # FieldID 40013 should have instance_arrays from 0-0 to 0-14, but may be
  # missing 12-0, perhaps because some ppts have left the study and there are
  # therefore no longer any data entries for these columns)

  code_cols_instance_arrays <- colname_to_field_inst_array_df(code_cols)
  date_cols_instance_arrays <- colname_to_field_inst_array_df(date_cols)

  # ...all code columns should have a corresponding date column though. An error
  # is therefore raised here if there are 'missing' date columns. The date
  # columns should match code columns by instance-array, with the exception of
  # secondary causes of death (FID 40002).

  # Primary and secondary causes of death (and dates) have the same number of
  # instances, but secondary cases have more arrays. For cases with more than
  # one date of death, the date should be the same for both instances (see
  # https://biobank.ndph.ox.ac.uk/ukb/ukb/docs/DeathLinkage.pdf for why this may
  # occasionally happen), however we match the date of instance to
  # primary/secondary causes of death instance anyway

  # similarly, there is no associated start date for self-reported medications.
  # These codes are therefore matched by instance with the date of attending the
  # UK Biobank assessment centre
  if (code_col_field_id %in% c("40002", "20003")) {
    JOIN_COL <- "instance"
    ARRANGE_COL <- "instance"
    INSTANCE_ARRAY_COL <- "instance_array.code"
  } else {
    JOIN_COL <- "instance_array"
    ARRANGE_COL <- "instance.code"
    INSTANCE_ARRAY_COL <- JOIN_COL
  }

  colnames_df <- code_cols_instance_arrays %>%
    dplyr::left_join(date_cols_instance_arrays,
      by = JOIN_COL,
      suffix = c(".code", ".date")
    ) %>%
    dplyr::arrange(as.numeric(.data[[ARRANGE_COL]])) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate("rowname" = as.integer(.data[["rowname"]]))

  assertthat::assert_that(!is.null(colnames_df$colnames.date),
    msg = "Error! in `tidy_clinical_events_basis`. Please raise an issue at https://github.com/rmgpanw/ukbwranglr/issues"
  )

  assertthat::assert_that(
    sum(is.na(colnames_df$colnames.date)) == 0,
    msg = paste0(
      "Error! Some date columns appear to be missing. Check that all columns for Field ID ",
      stringr::str_c(code_col_field_id,
        sep = "",
        collapse = ", "
      ),
      " have a corresponding column for Field ID ",
      date_col_field_id
    )
  )

  # update code_cols and date_cols
  code_cols <- colnames_df$colnames.code
  date_cols <- colnames_df$colnames.date

  # check these are actually present in ukb_main
  check_required_cols_exist(
    df = ukb_main,
    code_cols,
    date_cols
  )

  # melt cols
  ukb_main <- data.table::melt(
    ukb_main %>% dplyr::select(tidyselect::all_of(c("eid", code_cols, date_cols))),
    measure = list(code_cols, date_cols),
    value.name = c("code", "date")
  )

  # add instance_array col
  ukb_main$variable <- revalue_vector(
    x = as.integer(ukb_main$variable),
    dict = stats::setNames(
      object = colnames_df[[INSTANCE_ARRAY_COL]],
      nm = colnames_df$rowname
    ),
    default_value = NULL,
    suppress_warnings = FALSE,
    strict = TRUE
  )

  # check for empty values in code column. If present, convert to `NA` and raise warning
  n_empty_strings_code_col <- sum(stringr::str_detect(
    ukb_main$code,
    "^\\s*$"
  ),
  na.rm = TRUE
  )

  if (n_empty_strings_code_col > 0) {
    warning(paste0("Detected ", n_empty_strings_code_col, " empty code values (e.g. '', ' '), these will be removed"))
  }

  # remove `NA` and empty string code values
  ukb_main <- ukb_main[!(is.na(ukb_main$code) | (stringr::str_detect(
    ukb_main$code,
    "^\\s*$"
  )))]

  # make 'source' col
  ukb_main$source <- paste0("f", code_col_field_id)

  # rename 'variable' to 'index'
  names(ukb_main)[which(names(ukb_main) == "variable")] <- "index"

  # make code col type character (may be type integer for self-reported UKB
  # codes)
  ukb_main$code <- as.character(ukb_main$code)

  # rearrange cols
  ukb_main <- dplyr::select(ukb_main, tidyselect::all_of(CLINICAL_EVENTS_COLHEADERS))

  # return result
  time_taken_message(start_time)
  return(ukb_main)
}

recode_self_report_non_cancer_diagnoses_to_icd10 <-
  function(ukb_main,
           data_dict,
           ukb_codings) {
    # recode to ICD10
    mapping_df <- ukb_codings %>%
      dplyr::filter(.data[["Coding"]] == 609 &
        .data[["Value"]] != -1) %>%
      dplyr::select(tidyselect::all_of(c("Value", "Meaning")))

    dict <- mapping_df$Meaning
    names(dict) <- mapping_df$Value

    ukb_main$code <-
      suppressWarnings(revalue_vector(
        as.character(ukb_main$code),
        dict = dict,
        default_value = NA
      ))

    # remove rows with no code (not all self-reported conditions map to ICD10)
    ukb_main <- ukb_main %>%
      dplyr::filter(!is.na(.data[["code"]]))

    # relabel 'source' col to indicate these are icd10 codes
    ukb_main$source <- paste0(ukb_main$source, "_icd10")

    return(ukb_main)
  }

make_self_report_special_decimal_dates_na <- function(df) {
  # Helper function for get_self_report_xxx functions

  # special dates to remove
  # FID 20006 and 20008; https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20008
  # https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20006
  self_report_cancer_and_non_cancer_diagnoses_special_dates <- c(-3, -1)

  # remove special dates
  df$date <- ifelse(
    test = df$date %in% self_report_cancer_and_non_cancer_diagnoses_special_dates,
    yes = NA,
    no = df$date
  )

  return(df)
}

# Extract phenotypes helpers --------------------------------

validate_clinical_events_and_check_type <- function(clinical_events) {
  # check type of clinical_events: 'tbl_dbi' or 'df'
  if (suppressWarnings(all(class(clinical_events) == c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))) {
    clinical_events_type <- "tbl_dbi"
  } else if (is.data.frame(clinical_events)) {
    clinical_events_type <- "df"
  } else {
    clinical_events_type <- "unknown"
  }

  assertthat::assert_that(clinical_events_type %in% c("tbl_dbi", "df"),
    msg = "Error! clinical_events must either be a data frame or a tbl_dbi object"
  )

  # check colheaders
  assertthat::assert_that(
    all(CLINICAL_EVENTS_COLHEADERS == colnames(clinical_events)),
    msg = paste0(
      "Error! clinical_events does not have the expected column names. Expected column names: ",
      stringr::str_c(
        CLINICAL_EVENTS_COLHEADERS,
        sep = "",
        collapse = ", "
      )
    )
  )

  # check coltypes
  clinical_events_column_types <- clinical_events %>%
    utils::head(1) %>%
    dplyr::collect() %>%
    purrr::map_chr(class)

  clinical_events_column_types[1] <- ifelse(clinical_events_column_types[1] == "integer",
    yes = "numeric",
    no = clinical_events_column_types[1]
  )

  assertthat::assert_that(
    all(CLINICAL_EVENTS_COLTYPES == clinical_events_column_types),
    msg = paste0(
      "Error! columns in clinical_events are not of the expected types. Expected types: ",
      stringr::str_c(
        CLINICAL_EVENTS_COLTYPES,
        sep = "",
        collapse = ", "
      )
    )
  )

  # return clinical_events type ('tbl_dbi' or 'df') invisibly if all checks pass
  invisible(clinical_events_type)
}
