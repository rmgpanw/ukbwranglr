# NOTES -------------------------------------------------------------------

# Steps to append additional `clinical_events_sources` for `tidy_clinical_events()`:

# 1. Update `CLINICAL_EVENTS_FIELD_IDS` and `CLINICAL_EVENTS_SOURCES` in `DATASET.R`

# 2. Update relevant details in `tidy_clinical_events()`

# 3. Add dummy data fields to `DUMMY_UKB_MAIN_CLINICAL_EVENTS` in `DATASET.R`

# 4. Update end of `filter_clinical_events()` (from comment '***Update this section ...)

# 5. Add tests to `test_extract_phenotypes.R` under `tidy_clinical_events()` section

# 6. Run tests in `test_extract_phenotypes.R` and fix as necessary


# CONSTANTS ---------------------------------------------------------------

CLINICAL_EVENTS_COLHEADERS <- c("eid",
                                "source",
                                "index",
                                "code",
                                "date")

CLINICAL_EVENTS_COLTYPES <- c("numeric",
                              "character",
                              "character",
                              "character",
                              "character")

# EXPORTED FUNCTIONS ----------------------------------------------------


# Mutate indicator columns ------------------------------------------------

#' Mutate age at event columns
#'
#' Mutates numeric columns with the age at event for all columns that
#' match the regex supplied to \code{date_col_regex}.
#'
#' @param ukb_main a UKB main dataset (data frame) containing a column named "eid" and a date
#'   of birth column, as well as date columns matching the regex supplied to
#'   the \code{date_col_regex} argument.
#' @param dob_col character. The name of the date of birth column in
#'   \code{ukb_main}.
#' @param date_col_regex character. A regular expression that matches the date
#'   columns to be processed in \code{ukb_main}.
#' @param date_col_regex_replacement character. New "age-at-event" columns will
#'   have names matching those of the input date columns, replacing the regex
#'   supplied to \code{date_col_regex} with this string.
#'
#' @return A data frame with additional "age at event" columns
#' @export
#'
#' @examples
#' # dummy data
#' dummy_ukb_main <- tibble::tribble(
#' ~ eid, ~ dob, ~ event_date,
#' 1, "2000-01-01", "2010-01-01"
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
      lubridate::time_length(as.Date(haven::zap_labels(ukb_main[[column]])) - as.Date(haven::zap_labels(ukb_main[[dob_col]])), unit = 'year')

    new_colname_label <- stringr::str_replace(attributes(ukb_main[[column]])$label,
                                              pattern = "date (.+)$",
                                              replacement = "age")
    ukb_main[[new_colname]] <- haven::labelled(ukb_main[[new_colname]],
                                               labels = NULL,
                                               label = new_colname_label)
  }

  # return ukb_main with additional age at event cols
  return(ukb_main)
}

# Extract phenotypes from clinical events -------------------------------------------------------

#' Extract phenotypes from clinical events data
#'
#' Filters a clinical events table created by \code{\link{tidy_clinical_events}}
#' for a set or sets of specified clinical codes that represent one or more
#' phenotypes. By default, the \emph{earliest} date that any clinical code
#' appears in an individual participant's record is extracted. See also the
#' \href{https://rmgpanw.github.io/ukbwranglr/articles/ukb_clinical_events.html}{'Clinical
#' events'} vignette on the \code{ukbwranglr} package website.
#'
#' @param clinical_events A long format data frame created by
#'   \code{\link{tidy_clinical_events}}, \code{\link{tidy_gp_clinical}},
#'   \code{\link{tidy_gp_scripts}} or \code{\link{make_clinical_events_db}}.
#'   This can also be a \code{\link[dbplyr]{tbl_dbi}} object.
#' @param clinical_codes data frame. Must match the format as per
#'   \code{\link{example_clinical_codes}}.
#' @param data_sources A character vector of clinical events sources in
#'   \code{clinical_events} to extract phenotypes from. Use
#'   \code{\link{clinical_events_sources}} (\code{source column}) for a list of
#'   valid values.
#' @param min_max Choose either "min" or "max" to extract either the earliest or
#'   latest date that one of the codes appears in \code{clinical_codes} appears.
#' @param prefix Optionally add a prefix to column names.
#' @param workers The number of processes to run in parallel when
#'   \code{clinical_codes} includes multiple diseases. This is used to set the
#'   number of workers in \code{\link[future]{plan}} with \code{strategy =
#'   \link[future]{multisession}}, which is passed on to
#'   \code{\link[furrr]{future_map}}. If \code{NULL}, then no processing is
#'   performed sequentially. Default value is \code{NULL}.
#' @param keep_all If \code{TRUE}, keep all matching codes for each eid in a
#'   nested data frame column named 'data'.
#'
#' @return A named list of data frames, one for each disease. Each data frame
#'   has an "eid" column, and "event_min/max_indicator" and "event_min/max_date"
#'   columns for each phenotype in the 'category' column of
#'   \code{clinical_codes} for that disease. If \code{keep_all} is \code{TRUE},
#'   then there will also be additional nested data frame column called 'data'.
#' @export
#' @examples
#' \dontrun{
#' dummy_main_dataset_clinical_events() %>%
#'   tidy_clinical_events(clinical_events_sources = c("summary_hes_icd10")) %>%
#'   dplyr::bind_rows() %>%
#'   extract_phenotypes(
#'     clinical_codes = example_clinical_codes(),
#'     data_sources = "f41270",
#'     min_max = "min"
#'   )
#' }
extract_phenotypes <- function(
  clinical_events,
  clinical_codes,
  data_sources = NULL,
  min_max = "min",
  prefix = NULL,
  workers = NULL,
  keep_all = FALSE
) {
  start_time <- proc.time()

  # validate args
  clinical_events_type <- validate_clinical_events_and_check_type(clinical_events)
  validate_clinical_codes(clinical_codes)

  if (!is.null(data_sources)) {
    assertthat::assert_that(is.character(data_sources),
                            msg = "Error! `data_sources` should be either `NULL` or a character vector")
    assertthat::assert_that(length(data_sources) == length(unique(data_sources)),
                            msg = "Error! `data_sources` contains duplicated values")
    assertthat::assert_that(sum(is.na(data_sources)) == 0,
                            msg = "Error! `NA` values are present in `data_sources`")

    invalid_data_sources <-
      subset(data_sources,
             !data_sources %in% CLINICAL_EVENTS_SOURCES$source)
    if (!rlang::is_empty(invalid_data_sources)) {
      stop(
        paste0(
          "Error! The following values in `data_sources` are not valid: ",
          stringr::str_c(
            invalid_data_sources,
            sep = "",
            collapse = ", "
          )
        )
      )
    }
  }

  # set plan if using parallel processing
  if (!is.null(workers)) {
    assert_integer_ge_1(workers, "workers")
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential))
  }

  # make mapper function
  mapper_fn <- function(disease,
                        clinical_events,
                        clinical_codes,
                        data_sources,
                        min_max,
                        prefix,
                        db_path = NULL,
                        table_name = NULL,
                        keep_all) {
    message(paste0("\n***PROCESSING DISEASE ", counter, " OF ", n_diseases, "***"))
    time_taken_message(start_time)
    message("\n")

    counter <<- counter + 1

    # see note below re non-exportable objects and parallel processing
    if (!is.null(db_path) & !is.null(table_name)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
      on.exit(DBI::dbDisconnect(con))
      clinical_events <- dplyr::tbl(con, table_name)
    }

    extract_phenotypes_single_disease(
      disease = disease,
      clinical_events = clinical_events,
      clinical_codes = clinical_codes,
      data_sources = data_sources,
      min_max = min_max,
      prefix = prefix,
      keep_all = keep_all
    )
  }

  # loop through diseases in clinical_codes
  n_diseases <- length(unique(clinical_codes$disease))
  counter = 1

  # PARALLEL PROCESSING -----

  # clinical_events: tbl_dbi -----

  # need to extract the SQLite db path and table name. Non-exportable objects
  # (like db connections) won't work with parallel processing
  if ((clinical_events_type == "tbl_dbi") &
      !is.null(workers)) {

    # get db_path and table_name from clinical_events

    # check that no functions have been applied to the tbl_dbi object
    if (!is.null(clinical_events$op$name)) {
      stop("Error! `clinical_events` is a tbl_dbi object - it should not have any functions applied to it if using parallel processing (i.e. workers >= 1)")
    }

    db_path <- clinical_events$src$con@dbname
    table_name <- gsub("`", "", as.character(clinical_events$op$x))[1]

    result <- unique(clinical_codes$disease) %>%
      purrr::set_names() %>%
      furrr::future_map(~ mapper_fn(.x,
                                    db_path = db_path,
                                    table_name = table_name,
                                    clinical_events = clinical_events,
                                    clinical_codes = clinical_codes,
                                    data_sources = data_sources,
                                    min_max = min_max,
                                    prefix = prefix,
                                    keep_all = keep_all),
                        .progress = TRUE,
                        seed = TRUE)

    # clinical_events: df -----
  } else if ((clinical_events_type == "df") &
             !is.null(workers)) {

  result <- unique(clinical_codes$disease) %>%
    purrr::set_names() %>%
    furrr::future_map(~ mapper_fn(
      .x,
      clinical_events = clinical_events,
      clinical_codes = clinical_codes,
      data_sources = data_sources,
      min_max = min_max,
      prefix = prefix,
      keep_all = keep_all
    ),
    .progress = TRUE,
    seed = TRUE)

  # SEQUENTIAL PROCESSING -----
  } else if (is.null(workers)) {

      result <- unique(clinical_codes$disease) %>%
        purrr::set_names() %>%
        purrr::map(~ mapper_fn(
          .x,
          clinical_events = clinical_events,
          clinical_codes = clinical_codes,
          data_sources = data_sources,
          min_max = min_max,
          prefix = prefix,
          keep_all = keep_all
        ))

    } else {
    stop("Unexpected error! Please raise an issue at https://github.com/rmgpanw/ukbwranglr/issues")
  }

  # return result
  message("COMPLETE!")
  time_taken_message(start_time)
  return(result)
}


# Tidy clinical events -------------------------------------------------------

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
#' @param ukb_main A UK Biobank main dataset. Must be a
#'   \code{\link[data.table]{data.table}}
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
#'
#' @examples
#' # tidy clinical events in a UK Biobank main dataset
#' tidy_clinical_events(
#'   ukb_main = dummy_main_dataset_clinical_events(),
#'   ukb_data_dict = get_ukb_data_dict(),
#'   ukb_codings = get_ukb_codings()
#' )
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
                                 .details_only = FALSE
) {

  # validate args
  assertthat::assert_that(length(clinical_events_sources) == length(unique(clinical_events_sources)),
                          msg = "Error! `clinical_events_sources` contains duplicate values")

  unrecognised_clinical_events <- subset(clinical_events_sources,
                                         !clinical_events_sources %in% names(CLINICAL_EVENTS_FIELD_IDS))

  assertthat::assert_that(rlang::is_empty(unrecognised_clinical_events),
                          msg = paste0("Error! `clinical_events_sources` includes unrecognised values: ",
                                       stringr::str_c(unrecognised_clinical_events,
                                                      sep = "",
                                                      collapse = ", "),
                                       ". Please choose from: ",
                                       stringr::str_c(names(CLINICAL_EVENTS_FIELD_IDS),
                                                      sep = "",
                                                      collapse = ", ")))

  # if required field IDs requested
  if (.details_only) {
    return(
      list(required_field_ids = CLINICAL_EVENTS_FIELD_IDS)
    )
  }

  assertthat::assert_that(
    data.table::is.data.table(ukb_main),
    msg = "Error! `ukb_main` must be a data table (try `data.table::as.data.table(ukb_main)`)"
  )

  assertthat::assert_that(
    "eid" %in% names(ukb_main),
    msg = "Error! `ukb_main` must contain a column called `eid`"
  )

  # make data dictionary
  data_dict <- make_data_dict(ukb_main,
                              ukb_data_dict = ukb_data_dict)

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
                                  subset = !(clinical_events_sources %in% clinical_events_with_missing_required_cols))
      }
    }
  }

  # To use in loop below - determine how event_type should be processed
  death_cancer_summary_hes_self_report_meds <- c("cancer_register_icd9",
                              "cancer_register_icd10",
                              "summary_hes_icd9",
                              "summary_hes_icd10",
                              "summary_hes_opcs3",
                              "summary_hes_opcs4",
                              "primary_death_icd10",
                              "secondary_death_icd10",
                              "self_report_medication")

  self_report <- c("self_report_non_cancer",
                   "self_report_cancer",
                   "self_report_operation")
  self_report_non_cancer_icd10 <- "self_report_non_cancer_icd10"

  # (internal check that all options are covered)
  assertthat::are_equal(
    sort(c(
      death_cancer_summary_hes_self_report_meds,
      self_report,
      self_report_non_cancer_icd10
    )),
    sort(names(CLINICAL_EVENTS_FIELD_IDS)))

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
    result[[event_type]] <- switch(
      tidying_option,

      self_report = tidy_clinical_events_basis(ukb_main = ukb_main,
                                               data_dict = data_dict,
                                               ukb_codings = ukb_codings,
                                               data_dict_colname_col = "colheaders_raw",
                                               code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
                                               date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"]) %>%
        make_self_report_special_decimal_dates_na() %>%
        dplyr::mutate("date" = as.character(
          lubridate::as_date(
            lubridate::date_decimal(.data[["date"]])
            )
          )),

      self_report_non_cancer_icd10 = tidy_clinical_events_basis(ukb_main = ukb_main,
                                                                data_dict = data_dict,
                                                                ukb_codings = ukb_codings,
                                                                data_dict_colname_col = "colheaders_raw",
                                                                code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
                                                                date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"]) %>%
        make_self_report_special_decimal_dates_na() %>%
        dplyr::mutate("date" = as.character(
          lubridate::as_date(
            lubridate::date_decimal(.data[["date"]])
          )
        )) %>%
        recode_self_report_non_cancer_diagnoses_to_icd10(data_dict = data_dict,
                                                         ukb_codings = ukb_codings),

      death_cancer_summary_hes_self_report_meds = tidy_clinical_events_basis(ukb_main = ukb_main,
                                                          data_dict = data_dict,
                                                          ukb_codings = ukb_codings,
                                                          data_dict_colname_col = "colheaders_raw",
                                                          code_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["code_fid"],
                                                          date_col_field_id = CLINICAL_EVENTS_FIELD_IDS[[event_type]]["date_fid"])
    )
  }

  return(result)
}

# Clinical code lists -----------------------------------------------------

#' Generate an example data frame of clinical codes for diabetes
#'
#' Data frames of this format may be used with \code{\link{extract_phenotypes}}.
#' Use \code{\link{validate_clinical_codes}} to check whether a clinical codes
#' data frame meets all requirements.
#'
#' Note: this example is not an exhaustive list of codes for diabetes.
#'
#' @return A data frame of selected clinical codes for diabetes.
#' @export
#' @seealso \code{\link{validate_clinical_codes}}
#'
#' @examples
#' example_clinical_codes()
example_clinical_codes <- function() {
  tibble::tribble(
    ~ disease, ~ description, ~ category, ~ code_type, ~ code, ~ author,
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
  code_cols <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = code_col_field_id,
                                return_col = data_dict_colname_col,
                                error_if_missing = TRUE)

  date_cols <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = date_col_field_id,
                                return_col = data_dict_colname_col,
                                error_if_missing = TRUE)

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
                     suffix = c(".code", ".date")) %>%
    dplyr::arrange(as.numeric(.data[[ARRANGE_COL]])) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate("rowname" = as.integer(.data[["rowname"]]))

  assertthat::assert_that(!is.null(colnames_df$colnames.date),
                          msg = "Error! in `tidy_clinical_events_basis`. Please raise an issue at https://github.com/rmgpanw/ukbwranglr/issues")

  assertthat::assert_that(
    sum(is.na(colnames_df$colnames.date)) == 0,
    msg = paste0(
      "Error! Some date columns appear to be missing. Check that all columns for Field ID ",
      stringr::str_c(code_col_field_id,
                     sep = "",
                     collapse = ", "),
      " have a corresponding column for Field ID ",
      date_col_field_id
    )
  )

  # update code_cols and date_cols
  code_cols <- colnames_df$colnames.code
  date_cols <- colnames_df$colnames.date

  # check these are actually present in ukb_main
  check_required_cols_exist(df = ukb_main,
                            code_cols,
                            date_cols)

  # melt cols
  ukb_main <- data.table::melt(
    ukb_main %>% dplyr::select(tidyselect::all_of(c("eid", code_cols, date_cols))),
    measure = list(code_cols, date_cols),
    value.name = c("code", "date")
  )

  # add instance_array col
  ukb_main$variable <- revalue_vector(
    x = as.integer(ukb_main$variable),
    dict = stats::setNames(object = colnames_df[[INSTANCE_ARRAY_COL]],
                           nm = colnames_df$rowname),
    default_value = NULL,
    suppress_warnings = FALSE,
    strict = TRUE
  )

  ukb_main <- ukb_main[!is.na(ukb_main$code)]

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
  self_report_cancer_and_non_cancer_diagnoses_special_dates <- c(-3,-1)

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
                          msg = "Error! clinical_events must either be a data frame or a tbl_dbi object")

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
                                            no = clinical_events_column_types[1])

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

#' Helper function for \code{extract_phenotypes} - Get
#' the earliest record date for multiple phenotype categories within a single
#' disease
#'
#' Uses \code{\link{extract_first_or_last_clinical_event}}, extracting
#' either the first or last recorded clinical event for multiple phenotype
#' categories within a single disease
#'
#' @param disease scalar character. \code{clinical_codes} will be filtered
#'   for this value in the 'disease' column.
#' @param clinical_codes data frame. Must match the format as per
#'   \code{\link{example_clinical_codes}}.
#' @param prefix character. Optionally add a prefix to column names.
#' @inheritParams extract_phenotypes
#'
#' @return Data frame with an "eid" column, and "event_min/max_indicator" and
#'   "event_min/max_date" columns for each phenotype in the 'category' column of
#'   \code{clinical_codes}.
#'
#' @noRd
#' @family extract specific diagnostic codes functions
extract_phenotypes_single_disease <-
  function(disease,
           clinical_events,
           clinical_codes,
           data_sources,
           min_max,
           prefix,
           keep_all) {

    start_time <- proc.time()

    # validate args
    assertthat::is.string(disease)
    assertthat::assert_that(disease %in% clinical_codes$disease,
                            msg = "Error! disease not present in clinical_codes$disease")


    # general set up ------------------------------------------------------------------


    message(paste0("Extracting clinical events for disease: ", disease))

    # filter clinical_codes for single disease
    clinical_codes <- clinical_codes[clinical_codes$disease == disease, ]

    assertthat::assert_that(
      !(disease %in% clinical_codes$category)
      ,
      msg = paste0(
        "Error! ",
        disease,
        " is present in both the 'disease' and 'category' columns of clinical_code_df"
      )
    )

    # prepare clinical_codes so that overall summary columns (i.e. including all
    # codes across all categories for the disease) are also created in upper
    # case
    clinical_codes <- dplyr::bind_rows(clinical_codes,
                                          clinical_codes %>%
                                            dplyr::mutate("category" = disease))

    # clinical_codes$category: remove special characters and convert to lower
    # case. This will be used to label the columns with
    # `extract_first_or_last_clinical_event()`
    clinical_codes$phenotype_colname <-
      remove_special_characters_and_make_lower_case(clinical_codes$category)

    clinical_codes$phenotype_colname <-
      paste0(clinical_codes$phenotype_colname,
             "_",
             clinical_codes$author)

    clinical_codes$phenotype_colname <-
      ifelse(
        clinical_codes$category == clinical_codes$disease,
        toupper(clinical_codes$phenotype_colname),
        clinical_codes$phenotype_colname
      )

    # add prefix if specified
    if (!is.null(prefix)) {
      clinical_codes$phenotype_colname <-
        paste0(prefix, clinical_codes$phenotype_colname)
    }

    # make 2 named lists of code lists for each phenotype: 1 will contain code
    # lists, the other will contain the results from
    # `extract_first_or_last_clinical_event()`
    list_of_phenotype_codelists <- vector(mode = "list",
                                          length = length(unique(clinical_codes$phenotype_colname)))
    names(list_of_phenotype_codelists) <-
      sort(unique(clinical_codes$phenotype_colname))

    list_of_phenotype_results <- list_of_phenotype_codelists

    # populate `list_of_phenotype_codelists` with empty code lists
    for (phenotype in names(list_of_phenotype_codelists)) {
      list_of_phenotype_codelists[[phenotype]] <-
        make_empty_clinical_codes_list()
    }

    # now populate these code lists loop through phenotypes
    for (phenotype in names(list_of_phenotype_codelists)) {
      # filter codes clinical_events for phenotype category
      single_phenotype_df <- clinical_codes %>%
        dplyr::filter(.data[["phenotype_colname"]] == phenotype)

      # loop through code_types to populate code lists for phenotype category
      for (code_type in single_phenotype_df$code_type) {
        list_of_phenotype_codelists[[phenotype]][[code_type]] <-
          single_phenotype_df[single_phenotype_df$code_type == code_type, ]$code
      }
    }


    # preliminary filtering steps -----------------------------------------------

    # now filter `clinical_events` for all codes in `clinical_codes`. This is to speed
    # up the next step.

    message("Filtering clinical_events for all codes in clinical_codes")
    all_codes <- unique(clinical_codes$code)

    clinical_events <- clinical_events %>%
      dplyr::filter(.data[["code"]] %in% all_codes) %>%
      dplyr::collect()

    # filter for only certain sources if specified
    if (!is.null(data_sources)) {
      clinical_events <- clinical_events %>%
        dplyr::filter(.data[["source"]] %in% data_sources)
    }

    # exit early with warning if no rows in `clinical_events`
    if (nrow(clinical_events) == 0) {
      warning(
        "Warning! `clinical_events` does not contain any of the codes in `clinical_codes$code`. Returning `NULL`"
      )
      return(NULL)
    }


    # refined filtering steps -------------------------------------------------

    # get results: loop through phenotypes
    message("EXTRACTING EVENT DATES FOR PHENOTYPES")
    n_phenotypes <- length(names(list_of_phenotype_results))
    i <- 1
    # this is used in the loop to get the label for a `phenotype_colname`
    phenotype_colname_to_phenotype <- clinical_codes %>%
      dplyr::distinct(phenotype_colname,
                      .keep_all = TRUE) %>%
      split(.$phenotype_colname) %>%
      purrr::map_chr(~ .x[["category"]])

    for (phenotype_colname in names(list_of_phenotype_results)) {
      message(
        paste0(
          "Extracting event dates for ",
          phenotype_colname,
          " (Phenotype ",
          i,
          " of ",
          n_phenotypes,
          ")"
        )
      )

       result <- extract_clinical_events(
          clinical_events = clinical_events,
          clinical_codes_list = list_of_phenotype_codelists[[phenotype_colname]],
          phenotype_colname = phenotype_colname,
          phenotype = as.character(phenotype_colname_to_phenotype[phenotype_colname]),
          min_max = min_max,
          keep_all = keep_all
        )

       # if result is `NULL`, it must be assigned as a list with single square
       # brackets, otherwise it will remove this item will be completely removed
       # from the list. See this stackoverflow post:
       # https://stackoverflow.com/a/7945259
       if (is.null(result)) {
         list_of_phenotype_results[phenotype_colname] <- list(NULL)
       } else {
         list_of_phenotype_results[[phenotype_colname]] <- result
       }

      i <- i + 1
      time_taken_message(start_time)
    }

    if (check_if_all_list_items_are_null(list_of_phenotype_results)) {
      # note, it is possible to only pick this up at this stage if there are codes
      # that appear in more than one clinical coding system e.g. the OPCS4 code
      # 'L03' indicates patent ductus arteriosus, but there is also a ICD10 code
      # 'L03' which indicates cellulitis
      warning(
        "Warning! `clinical_events` does not contain any of the codes in `clinical_codes$code`. Returning `NULL`"
      )
      return(NULL)
    } else {
      return(list_of_phenotype_results)
    }
  }

#' Get the earliest or latest record date per eid for a set of clinical codes
#'
#' For a set of clinical codes, extract eids with at least one of these and
#' retrieve, if available, the earliest or latest recorded date.
#'
#' @section Under the hood:
#'
#'   Filters a long format data frame of clinical codes generated by
#'   \code{\link{tidy_clinical_events}}), filters this for the codes in
#'   \code{clinical_codes_list} then groups by eid. For each eid the earliest or
#'   latest date is then retrieved, as specified by \code{min_max}.
#'
#' @param min_max can be either "min" or "max". Extracts either the earliest or
#'   latest date that on of the specified \code{codes} appears.
#' @param phenotype_colname character. Column name for phenotype
#' @param phenotype character. Phenotype
#' @inheritParams extract_phenotypes_single_disease
#'
#' @return Data frame with columns "eid", "event_min/max_indicator" and
#'   "event_min/max_date". "Event" is replaced with the string supplied to the
#'   \code{phenotype_colname} argument. Retain a "data" column of nested data
#'   frames containing all clinical events if
#' @noRd
extract_clinical_events <- function(clinical_events,
                                    clinical_codes_list,
                                    min_max,
                                    phenotype_colname,
                                    phenotype,
                                    keep_all) {

  # extract earliest date
  clinical_events <- extract_single_record(
    clinical_events = clinical_events,
    clinical_codes_list = clinical_codes_list,
    mapping_function = purrr::partial(extract_first_or_last_record_mapper,
                                      min_max = min_max),
    keep_all = keep_all
  )

  # exit early returning `NULL` if clinical_events is empty
  if (nrow(clinical_events) == 0) {
    return(NULL)
  }

  # select just eid and date cols
  selected_cols <- c("eid", "date")

  if (keep_all) {
    selected_cols <- c(selected_cols, "data")
  }

  clinical_events <- clinical_events %>%
    dplyr::select(tidyselect::all_of(selected_cols))

  # create indicator column - in some cases there may a diagnosis, even if the
  # date is unknown
  indicator_colname <- paste0(phenotype_colname, "_indicator")

  clinical_events[[indicator_colname]] <- 1

  # rename date col to `phenotype_colname`
  date_colname <- paste0(phenotype_colname, "_", min_max, "_date")

  clinical_events <- clinical_events %>%
    rename_cols(old_colnames = "date",
                new_colnames = date_colname)

  # add labels
  indicator_labels <- stats::setNames(c(1, 0),
                               nm = c(phenotype, paste0("No ", phenotype)))

  clinical_events[[indicator_colname]] <- haven::labelled(
    x = clinical_events[[indicator_colname]],
    labels = indicator_labels,
    label = phenotype
  )

  clinical_events[[date_colname]] <- haven::labelled(
    x = clinical_events[[date_colname]],
    labels = NULL,
    label = paste0(phenotype, " date (", min_max, ")")
  )

  return(clinical_events)
}

#' Extract a single record from a data frame generated by
#' \code{tidy_clinical_events}
#'
#' Helper function for \code{extract_clinical_events}. For a set of diagnostic codes, filter for eids with at least
#' one of these and extract a single row per eid (e.g. the earliest date that
#' one of the diagnostic codes appears).
#'
#' @param clinical_events Either a dataframe or a \code{\link[dbplyr]{tbl_dbi}}
#'   object generated by \code{tidy_clinical_events}
#' @param codes Either a character vector or named list of diagnostic codes. If
#'   a list, then each named item should be a character vector of codes (use
#'   \code{\link{make_empty_clinical_codes_list}}).
#' @param mapping_function a function that will be applied to
#'   \code{clinical_events} nested by eid, extracting a single row per eid (e.g.
#'   the earliest date that one of the \code{codes} appears). This will be
#'   applied using \code{\link[purrr]{map_chr}} and should return the
#'   \code{source}, \code{code} and \code{date} columns as a single character,
#'   separated by "_SEP_".
#' @param ... additional args are passed on to \code{mapping_function}.
#'
#' @return A dataframe nested by eid, with \code{source}, \code{code} and
#'   \code{date} columns for row extracted by the \code{mapping_function}.
#'
#' @noRd
extract_single_record <- function(clinical_events,
                                  clinical_codes_list,
                                  mapping_function,
                                  ...) {

  # validate args
  assertthat::assert_that(is.function(mapping_function),
                          msg = "`mapping_function` must be a function")

  # filter clinical events (a df or tbl_dbi object) for codes (a character
  # vector, or a named list of codes)
  clinical_events <-
    filter_clinical_events(clinical_events = clinical_events,
                           clinical_codes_list = clinical_codes_list)

  # nest by eid and extract earliest date + code (or just a code if no dates
  # available)
  clinical_events <- clinical_events %>%
    dplyr::group_by(.data[["eid"]]) %>%
    tidyr::nest()

  clinical_events$result <- purrr::map_chr(clinical_events$data, mapping_function)

  clinical_events <- clinical_events %>%
    tidyr::separate(.data[["result"]],
                    into = c("source", "index", "code", "date"),
                    sep = "_SEP_") %>%
    suppressWarnings() # separate() raises a warning if there are rows with no dates - suppress these

  clinical_events <- clinical_events %>%
    dplyr::ungroup()

  return(clinical_events)
}

#' Helper function for \code{\link{extract_single_record}}
#'
#' Performs the code filtering step in
#' \code{\link{extract_single_record}}.
#' \code{clinical_events} can be either a data frame or a tbl_dbi object.
#' \code{codes} should be a named list of codes (names as per
#' \code{\link{make_empty_clinical_codes_list}}).
#'
#' @param clinical_codes_list A named list of clinical codes. All names must be
#'   present in \code{CLINICAL_EVENTS_SOURCES$data_coding}
#' @inheritParams extract_phenotypes
#' @return a data frame
#' @noRd
filter_clinical_events <- function(clinical_events,
                                   clinical_codes_list) {
  # validate args - clinical_events should have already been checked by any exported functions

  assertthat::assert_that(
    # this should also have already been checked by any exported functions
    (all(names(clinical_codes_list) %in% CLINICAL_EVENTS_SOURCES$data_coding)) &
      (length(names(clinical_codes_list)) == length(unique(names(clinical_codes_list)))),
    msg = "Error! Names for clinical_codes_list should be unique and all present in CLINICAL_EVENTS_SOURCES$data_coding"
    )

  # filter for selected codes

  # ***Update this section if new types of data_coding are added to
  # CLINICAL_EVENTS_SOURCES$data_coding***
  assertthat::assert_that(all(
    unique(sort(CLINICAL_EVENTS_SOURCES$data_coding)) == sort(c(
      'bnf',
      'dmd',
      'data_coding_3',
      'data_coding_4',
      'data_coding_6',
      'data_coding_5',
      'icd10',
      'icd9',
      'opcs3',
      'opcs4',
      'read2',
      'read3'
    ))),
    msg = "Error! check code for `CLINICAL_EVENTS_SOURCES$data_coding` and amend the filter statement in this function to address all possible code types")

  # filter Note: for tbl_dbi objects, cannot use
  # `get_sources_for_code_type("icd9")` or `clinical_codes_list$icd9` in the filter statement
  # below. It will silently return an error result

  data_coding_3_sources <- get_sources_for_code_type("data_coding_3")
  data_coding_4_sources <- get_sources_for_code_type("data_coding_4")
  data_coding_6_sources <- get_sources_for_code_type("data_coding_6")
  data_coding_5_sources <- get_sources_for_code_type("data_coding_5")
  icd10_sources <- get_sources_for_code_type("icd10")
  icd9_sources <- get_sources_for_code_type("icd9")
  opcs3_sources <- get_sources_for_code_type("opcs3")
  opcs4_sources <- get_sources_for_code_type("opcs4")
  read2_sources <- get_sources_for_code_type("read2")
  read3_sources <- get_sources_for_code_type("read3")
  bnf_sources <- get_sources_for_code_type("bnf")
  dmd_sources <- get_sources_for_code_type("dmd")

  data_coding_3_codes <- clinical_codes_list$data_coding_3
  data_coding_4_codes <- clinical_codes_list$data_coding_4
  data_coding_6_codes <- clinical_codes_list$data_coding_6
  data_coding_5_codes <- clinical_codes_list$data_coding_5
  icd10_codes <- clinical_codes_list$icd10
  icd9_codes <- clinical_codes_list$icd9
  opcs3_codes <- clinical_codes_list$opcs3
  opcs4_codes <- clinical_codes_list$opcs4
  read2_codes <- clinical_codes_list$read2
  read3_codes <- clinical_codes_list$read3
  bnf_codes <- clinical_codes_list$bnf
  dmd_codes <- clinical_codes_list$dmd

  clinical_events <- clinical_events %>%
    dplyr::filter(
      (.data[["source"]] %in% data_coding_3_sources &
         .data[["code"]] %in% data_coding_3_codes) |
        (.data[["source"]] %in% data_coding_6_sources &
           .data[["code"]] %in% data_coding_6_codes) |
        (.data[["source"]] %in% data_coding_4_sources &
           .data[["code"]] %in% data_coding_4_codes) |
        (.data[["source"]] %in% data_coding_5_sources &
           .data[["code"]] %in% data_coding_5_codes) |
        (.data[["source"]] %in% icd9_sources &
           .data[["code"]] %in% icd9_codes) |
        (.data[["source"]] %in% icd10_sources &
           .data[["code"]] %in% icd10_codes) |
        (.data[["source"]] %in% opcs3_sources &
           .data[["code"]] %in% opcs3_codes) |
        (.data[["source"]] %in% opcs4_sources &
           .data[["code"]] %in% opcs4_codes) |
        (.data[["source"]] %in% read2_sources &
           .data[["code"]] %in% read2_codes) |
        (.data[["source"]] %in% read3_sources &
           .data[["code"]] %in% read3_codes) |
        (.data[["source"]] %in% bnf_sources &
           .data[["code"]] %in% bnf_codes) |
        (.data[["source"]] %in% dmd_sources &
           .data[["code"]] %in% dmd_codes)
    ) %>%
    dplyr::collect()

  return(clinical_events)
}

# (mappers) ---------------------------------------------------------------

# Helper for `extract_single_record()`. A mapping function
# to extract earliest diagnostic code data for each eid - returns a single
# character. If no dates are available, will return the first row of the data frame
extract_first_or_last_record_mapper <- function(df,
                                                min_max = "min") {
  # validate args
  match.arg(arg = min_max,
            choices = c("min", "max"))

  # get row index for earliest/latest date. Note, `which.min` and `which.max` do
  # not work with dates as strings
  selected_date <- switch(min_max,
                          min = which.min(as.Date(df$date)),
                          max = which.max(as.Date(df$date)))

  # if there are no dates, return the first row of codes
  if (rlang::is_empty(selected_date)) {
    return(df[1, ] %>%
             dplyr::select(-date) %>%
             tidyr::unite("result", sep = "_SEP_", remove = TRUE) %>%
             .$result)
  }

  # otherwise, return first date
  df[selected_date,] %>%
    tidyr::unite("result", sep = "_SEP_", remove = TRUE) %>%
    .$result
}

# Mappings - data dictionary/codings/clinical events schema --------------------------------------

#' Helper function for \code{link{filter_clinical_events}}
#'
#' For a given clinical code type, returns the data sources that use this.
#'
#' @param code_type character vector of length 1.
#'
#' @return Character vector of data sources which use the clinical coding system
#'   specified by \code{code_type}.
#' @noRd
#' @family extract specific diagnostic codes functions
get_sources_for_code_type <- function(code_type) {
  match.arg(code_type,
            CLINICAL_EVENTS_SOURCES$data_coding)

  CLINICAL_EVENTS_SOURCES %>%
    dplyr::filter(.data[["data_coding"]] == code_type) %>%
    .$source
}
