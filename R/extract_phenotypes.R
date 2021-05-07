# NOTES -------------------------------------------------------------------

# Section 'Get all diagnostic codes' - functions to generate long format dataframes
# with standardised formats, listing all diagnostic codes for each eid. Separate
# functions created for each data source (i.e. self-report, HES, primary care
# etc)

# Section 'Extract specific diagnostic codes' - for a set of
# specific diagnostic codes, extract a list of eid's with these codes and the
# earliest recorded date. Depends on output from the 'get all diagnostic codes'
# functions

#TODO

# optimise field_id_pivot_longer_multi() - set an 'index' column and filter this for
#non-na values before performing inner_join() e.g. for self-reported non-cancer
#diagnoses, fieldid 20002 would be the index col: if there is no self-reported
#diagnosis, then the associated date and age at diagnosis columns will also be
#empty.

# For 'get all diagnostic codes' functions, each starts with the required
# fieldids. Recode the remainder of the functions to use this so any changes
# only need to be made at the start (maybe separate into 'code_fid' and 'date_fid' variables?)

# Add check - UKB dataset should have all the required columns for these
# functions i.e. all instances/arrays (as per the complete UKB data dictionary)
# for all required FIDs. Raise error if not

# Remove special values e.g. -3 and -1 for self-reported years of diagnosis
# (https://biobank.ndph.ox.ac.uk/showcase/field.cgi?id=20008); coding 13)

# EXPORTED FUNCTIONS ----------------------------------------------------


# Mutate indicator columns ------------------------------------------------

#' Mutate age at event columns
#'
#' Mutates numeric columns with the age at event for all columns that
#' match the regex supplied to \code{date_col_regex}.
#'
#' @param ukb_pheno a UK Biobank phenotype data frame containing "eid" and date
#'   of birth columns, as well as date columns matching the regex supplied to
#'   the \code{date_col_regex} argument.
#' @param dob_col character. The name of the date of birth column in
#'   \code{ukb_pheno}.
#' @param date_col_regex character. A regular expression that matches the date
#'   columns to be processed in \code{ukb_pheno}.
#' @param date_col_regex_replacement character. New "age-at-event" columns will
#'   have names matching those of the input date columns, replacing the regex
#'   supplied to \code{date_col_regex} with this string.
#'
#' @return A dataframe with additional "age-at-event" columns
#' @export
#'
#' @examples
#' # dummy data
#' dummy_ukb_pheno <- tibble::tribble(
#' ~ eid, ~ dob, ~ event_date,
#' 1, as.Date("2000-01-01"), as.Date("2010-01-01")
#' )
#'
#' # mutate age at event col
#' mutate_age_at_event_cols(
#'   dummy_ukb_pheno,
#'   dob_col = "dob",
#'   date_col_regex = "_date$",
#'   date_col_regex_replacement = "_age"
#' )
mutate_age_at_event_cols <- function(ukb_pheno,
                                     dob_col = "dob",
                                     date_col_regex = "_date$",
                                     date_col_regex_replacement = "_age") {
  # check eid and dob cols exist
  assertthat::assert_that(
    all(c("eid", dob_col) %in% names(ukb_pheno)),
    msg = paste0("Error! Either the 'eid' or '", dob_col, "' columns are absent from ukb_pheno")
  )

  # date columns to be processed
  date_cols_to_process <-
    subset(names(ukb_pheno), stringr::str_detect(names(ukb_pheno), date_col_regex))

  assertthat::assert_that(
    !rlang::is_empty(date_cols_to_process),
    msg = paste0("Error! No columns found matching, ", date_col_regex)
  )

  # check that `dob_col` and `date_cols_to_process` are all date columns
  assertthat::assert_that(
    all(
      c(date_cols_to_process, dob_col) %>%
        purrr::map_lgl(~ class(ukb_pheno[[.x]]) == "Date")
    ),
    msg = "Error! Either the date of birth column or some of the other selected columns are not of class date. Try `dplyr::select(ukb_pheno, tidyselect::matches(date_col_regex)` to see which columns are being selected."
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
    ukb_pheno[[new_colname]] <-
      lubridate::time_length(ukb_pheno[[column]] - ukb_pheno[[dob_col]], unit = 'year')
  }

  # return ukb_pheno with additional age at event cols
  return(ukb_pheno)
}

# Extract specific diagnostic codes -------------------------------------------------------

#' Get the earliest record date for multiple phenotype categories
#'
#' Builds on \code{\link{extract_first_or_last_clinical_event}}, extracting
#' either the first or last recorded clinical event for multiple phenotype
#' categories.
#'
#' @param clinical_codes_df data frame. Must match the format as per
#'   \code{\link{generate_self_reported_diabetes_codes_df}}.
#' @param prefix character. Optionally add a prefix to column names.
#' @param workers integer. The number of processes to run in parallel when
#'   \code{clinical_codes_df} includes multiple diseases. This is used to set
#'   the number of workers in \code{\link[future]{plan}} with \code{strategy =
#'   \link[future]{multisession}}, which is passed on to
#'   \code{\link[furrr]{future_map}}. Default value is \code{NULL}.
#' @inheritParams extract_single_diagnostic_code_record_basis
#' @inheritParams extract_first_or_last_clinical_event
#'
#' @return A named list of data frames, one for each disease. Each data frame
#'   has an "eid" column, and "event_min/max_indicator" and "event_min/max_date"
#'   columns for each phenotype in the 'category' column of
#'   \code{clinical_codes_df} for that disease.
#' @export
#'
#' @family extract specific diagnostic codes functions
extract_first_or_last_clinical_event_multi <- function(
  df,
  clinical_codes_df,
  min_max = "min",
  prefix = NULL,
  workers = NULL
) {
  start_time <- proc.time()

  # validate args
  assertthat::assert_that(
    (
      (suppressWarnings(all(class(df) == c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl")))) |
    ("data.frame" %in% class(df))
    ),
    msg = "Error! df must either be a data frame or a tbl object"
  )

  # set plan if using parallel processing
  if (!is.null(workers)) {
    assert_integer_ge_1(workers, "workers")
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential))
  }

  # make mapper function
  mapper_fn <- function(disease,
                        db_path = NULL,
                        table_name = NULL,
                        df,
                        clinical_codes_df,
                        min_max,
                        prefix) {
    message(paste0("\n***PROCESSING DISEASE ", counter, " OF ", n_diseases, "***"))
    time_taken_message(start_time)
    message("\n")

    counter <<- counter + 1

    # see note below re non-exportable objects and parallel processing
    if (!is.null(db_path) & !is.null(table_name)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
      df <- dplyr::tbl(con, table_name)
    }

    extract_first_or_last_clinical_event_multi_single_disease(
      disease = disease,
      df = df,
      clinical_codes_df = clinical_codes_df,
      min_max = min_max,
      prefix = prefix
    )
  }

  # loop through diseases in clinical_codes_df
  n_diseases <- length(unique(clinical_codes_df$disease))
  counter = 1

  # if using parallel processing (!is.null(workers)) and df is a tbl, need to
  # extract the SQLite db path and table name. Non-exportable objects (like db
  # connections) won't work with parallel processing
  if (suppressWarnings(all(class(df) == c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"))) &
      !is.null(workers)) {

    # get db_path and table_name from df

    # check that no functions have been applied to the tbl
    if (!is.null(df$op$name)) {
      stop("Error! `df` is a tbl object - it should not have any functions applied to it if using parallel processing (i.e. workers >= 1).")
    }

    db_path <- df$src$con@dbname
    table_name <- gsub("`", "", as.character(df$op$x))[1]

    result <- unique(clinical_codes_df$disease) %>%
      purrr::set_names() %>%
      furrr::future_map(~ mapper_fn(.x,
                                    db_path = db_path,
                                    table_name = table_name,
                                    df = df,
                                    clinical_codes_df = clinical_codes_df,
                                    min_max = min_max,
                                    prefix = prefix),
                        .progress = TRUE)

  } else if ("data.frame" %in% class(df) |
             is.null(workers)) {

  result <- unique(clinical_codes_df$disease) %>%
    purrr::set_names() %>%
    furrr::future_map(~ mapper_fn(
      .x,
      df = df,
      clinical_codes_df = clinical_codes_df,
      min_max = min_max,
      prefix = prefix
    ),
    .progress = TRUE)

  } else {
    stop("Error! Please raise an issue to fix this function")
  }

  # TO DELETE - combining all elements can increase the object size
  # significantly. Therefore, return as a named list instead. The user can then
  # combine if needed # combine result <- result %>% purrr::compact()
  #
  # if (!rlang::is_empty(result)) { result <- result %>%
  # purrr::reduce(dplyr::full_join, by = "eid") }

  # return result
  message("COMPLETE!")
  time_taken_message(start_time)
  return(result)
}

#' Get the earliest record date for a set of diagnostic codes
#'
#' For a set of diagnostic codes, extract individuals with at least one of these
#' and retrieve, if available, the earliest or latest recorded date.
#'
#' @section Under the hood:
#'
#'   Filters a long format dataframe of diagnostic codes generated by one of the
#'   'get all diagnostic codes' functions (e.g.
#'   \code{\link{get_hes_icd10_diagnoses}}), filters this for the specified
#'   \code{codes} then groups by eid. For each eid the earliest or latest date
#'   is then retrieved, as specified by \code{min_max}.
#'
#' @param min_max can be either "min" or "max". Extracts either the earliest or
#'   latest date that on of the specified \code{codes} appears.
#' @param phenotype_name character. Name
#' @inheritParams extract_single_diagnostic_code_record_basis
#'
#' @return Data frame with columns "eid", "event_min/max_indicator" and
#'   "event_min/max_date". "Event" is replaced with the string supplied to the
#'   \code{phenotype_name} argument.
#' @export
#' @family extract specific diagnostic codes functions
extract_first_or_last_clinical_event <- function(df,
                                                 codes,
                                                 min_max = "min",
                                                 phenotype_name = "event") {

  # extract earliest date
  df <- extract_single_diagnostic_code_record_basis(
    df = df,
    codes = codes,
    mapping_function = purrr::partial(extract_first_or_last_record_mapper,
                                      min_max = min_max)
  )

  # select just eid and date cols
  df <- df %>%
    dplyr::select(.data[["eid"]],
                  .data[["date"]]) %>%
    dplyr::ungroup()

  # create indicator column - in some cases there may a diagnosis, even if the
  # date is unknown
  df[[paste0(phenotype_name, "_indicator")]] <- TRUE

  # rename date col to `phenotype_name`
  df <- df %>%
    rename_cols(old_colnames = "date",
                new_colnames = paste0(phenotype_name, "_", min_max, "_date"))

  # create indicator column

  return(df)
}

# Get all diagnostic codes -------------------------------------------------------

#' Get death data ICD10 diagnoses
#'
#' Returns a long format dataframe with all death data ICD10 codes for each UK
#' Biobank participant.
#'
#' Reformats the data for FieldIDs 40001 and 40002 (death register
#' health-related outcomes - primary and secondary causes of death; see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100093}{category
#' 100093}).
#'
#' @section Under the hood:
#'
#'   Converts the data from FieldIDs 40001 and 40002 to long format separately
#'   before combining into a single dataframe.
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_death_data_icd10_diagnoses <- function(ukb_pheno,
                                   data_dict,
                                   ukb_codings) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100093 )

  # death data does not include dates. This function is a modified version of
  # `get_diagnosis_basis_fn()`
  start_time <- proc.time()
  code_col_field_id <- c("40001", "40002")

  # validate args
  assertthat::assert_that(
    "data.table" %in% class(ukb_pheno),
    msg = "Error! ukb_pheno must be a data table"
  )

  assertthat::assert_that(
    (all(code_col_field_id %in% data_dict$FieldID)),
    msg = paste0(
      "Error! FieldID ",
      code_col_field_id,
      "is not present in data_dict"
    )
  )

  # get lists of required columns
  code_cols <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = code_col_field_id,
                                return_col = "descriptive_colnames",
                                error_if_missing = TRUE)

  # melt cols
  ukb_pheno <- data.table::melt(
    ukb_pheno %>% dplyr::select(tidyselect::all_of(c("eid", code_cols))),
    measure = code_cols,
    value.name = "code"
  )

  ukb_pheno <- ukb_pheno[!is.na(ukb_pheno$code)]

  # relabel code col
  ukb_pheno <- recode_ukbcol(df = ukb_pheno,
                             col_to_recode = "code",
                             # both Field IDsuse the same coding
                             field_id = code_col_field_id[1],
                             ukb_data_dict = data_dict,
                             ukb_codings = ukb_codings,
                             mapping_direction = "meaning_code")

  # make 'source' col
  ukb_pheno$variable <- stringr::str_extract(ukb_pheno$variable,
                                             pattern = "f[:digit:]+_[:digit:]+_[:digit:]+$") %>%
    stringr::str_extract(pattern = "^f[:digit:]+")

  names(ukb_pheno)[which(names(ukb_pheno) == "variable")] <- "source"

  # make empty date col
  ukb_pheno$date <- as.Date(NA)

  # return result
  time_taken_message(start_time)
  return(ukb_pheno)
}


#' Get hospital inpatient ICD9 diagnoses
#'
#' Returns a long format dataframe with all summary hospital inpatient ICD9
#' codes and associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41271 and 41281 (hospital inpatient diagnoses
#' and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002}{category 2002}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Data frame
#' @export
#'
#' @family get all diagnostic codes
get_hes_icd9_diagnoses <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002 )
  get_diagnoses_basis_fn(ukb_pheno = ukb_pheno,
                         data_dict = data_dict,
                         ukb_codings = ukb_codings,
                         code_col_field_id = "41271",
                         date_col_field_id = "41281")
}


#' Get hospital inpatient ICD10 diagnoses
#'
#' Returns a long format dataframe with all summary hospital inpatient ICD10
#' codes and associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41270 and 41280 (hospital inpatient diagnoses
#' and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002}{category 2002}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_hes_icd10_diagnoses <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002 )

  get_diagnoses_basis_fn(
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings,
    code_col_field_id = "41270",
    date_col_field_id = "41280"
  )
}


#' Get self-reported non-cancer diagnoses
#'
#' Returns a long format dataframe with all self-reported non-cancer diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20002 and 20008 (self-reported non-cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074}). Coded using
#' \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6}{data coding 6}.
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_self_report_non_cancer_diagnoses <- function(ukb_pheno,
                                                 data_dict,
                                                 ukb_codings,
                                                 remove_special_dates = TRUE) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  result <- get_diagnoses_basis_fn(ukb_pheno = ukb_pheno,
                         data_dict = data_dict,
                         ukb_codings = ukb_codings,
                         code_col_field_id = "20002",
                         date_col_field_id = "20008")

  if (remove_special_dates) {
    result <- make_self_report_special_decimal_dates_na(result)
  }

  # convert date_col from decimal to date type
  result$date <- lubridate::as_date(lubridate::date_decimal(result$date))

  return(result)
}


#' Get self-reported non-cancer diagnoses (ICD10)
#'
#' Returns a long format dataframe with all self-reported non-cancer diagnoses
#' mapped to 3-character ICD10 codes with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20002 and 20009 (self-reported non-cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074}).
#'
#' \strong{Note: not all self-reported medical conditions map to ICD10 codes.}
#' (See \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=609}{data coding
#' 609})
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_self_report_non_cancer_diagnoses_icd10 <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings,
                                    remove_special_dates = TRUE) {

  result <-
    get_self_report_non_cancer_diagnoses(
      ukb_pheno = ukb_pheno,
      data_dict = data_dict,
      ukb_codings = ukb_codings,
      remove_special_dates = remove_special_dates
    )

  # ...now recode to ICD10
  mapping_df <- ukb_codings %>%
    dplyr::filter(.data[["Coding"]] == 609 & .data[["Value"]] != -1) %>%
    dplyr::select(tidyselect::all_of(c("Value", "Meaning")))

  dict <- mapping_df$Meaning
  names(dict) <- mapping_df$Value

  result$code <- suppressWarnings(revalue_vector(result$code,
                                                 dict = dict,
                                                 default_value = NA))

  # remove rows with no code (not all self-reported conditions map to ICD10)
  result <- result %>%
    dplyr::filter(!is.na(.data[["code"]]))

  # relabel 'source' col to indicate these are icd10 codes
  result$source <- paste0(result$source, "_icd10")

  return(result)
}


#' Get self-reported cancer diagnoses
#'
#' Returns a long format dataframe with all self-reported cancer diagnoses with
#' associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20001 and 20006 (self-reported cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category
#' 100074}). Coded using
#' \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=3}{data coding 3}.
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_self_report_cancer_diagnoses <- function(ukb_pheno,
                                             data_dict,
                                             ukb_codings,
                                             remove_special_dates = TRUE) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)

  result <- get_diagnoses_basis_fn(
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings,
    code_col_field_id = "20001",
    date_col_field_id = "20006"
  )

  if (remove_special_dates) {
    result <- make_self_report_special_decimal_dates_na(result)
  }

  # convert date_col from decimal to date type
  result$date <- lubridate::as_date(lubridate::date_decimal(result$date))

  return(result)
}


#' Get cancer register ICD9 diagnoses
#'
#' Returns a long format dataframe with all ICD9 cancer register diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 40013 and 40005 (cancer register diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092}{category 100092}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_cancer_register_icd9_diagnoses <- function(ukb_pheno,
                                             data_dict,
                                             ukb_codings) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092)

  get_diagnoses_basis_fn(
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings,
    code_col_field_id = "40013",
    date_col_field_id = "40005"
  )
}


#' Get cancer register ICD10 diagnoses
#'
#' Returns a long format dataframe with all ICD10 cancer register diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 40006 and 40005 (cancer register diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092}{category 100092}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Dataframe
#' @export
#'
#' @family get all diagnostic codes
get_cancer_register_icd10_diagnoses <- function(ukb_pheno,
                                               data_dict,
                                               ukb_codings) {

  # for required field_ids (see
  # https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)

  get_diagnoses_basis_fn(
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings,
    code_col_field_id = "40006",
    date_col_field_id = "40005"
  )
}

#' Get hospital inpatient OPCS4 operative procedures
#'
#' Returns a long format dataframe with all summary hospital inpatient OPCS4
#' codes and associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41272 and 41282 (hospital inpatient OPCS4
#' operative procedures and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2005}{category 2005}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Data frame
#' @export
#'
#' @family get all diagnostic codes
get_hes_opcs4_operations <- function(ukb_pheno,
                                     data_dict,
                                     ukb_codings) {
  # for required field_ids (see
  # https://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=2005 )

  get_diagnoses_basis_fn(ukb_pheno = ukb_pheno,
                         data_dict = data_dict,
                         ukb_codings = ukb_codings,
                         code_col_field_id = "41272",
                         date_col_field_id = "41282")
}

#' Get hospital inpatient OPCS3 operative procedures
#'
#' Returns a long format dataframe with all summary hospital inpatient OPCS3
#' codes and associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41273 and 41283 (hospital inpatient OPCS3
#' operative procedures and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2005}{category 2005}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams read_pheno
#'
#' @return Data frame
#' @export
#'
#' @family get all diagnostic codes
get_hes_opcs3_operations <- function(ukb_pheno,
                                     data_dict,
                                     ukb_codings) {
  # for required field_ids (see
  # https://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=2005 )

  get_diagnoses_basis_fn(ukb_pheno = ukb_pheno,
                         data_dict = data_dict,
                         ukb_codings = ukb_codings,
                         code_col_field_id = "41273",
                         date_col_field_id = "41283")
}

#' Get all diagnostic codes from multiple data sources
#'
#' Extract diagnostic codes from multiple sources
#'
#' Loops through a list of functions (\code{function_list}) from the 'get all
#' diagnostic codes' family and combines the results into a single data frame.
#'
#' @param function_list A list of \code{get_XXXX_diagnoses} functions.
#' @inheritParams get_self_report_non_cancer_diagnoses_icd10
#'
#' @return Dataframe
#' @export
#' @family get all diagnostic codes
get_all_diagnostic_codes_multi <- function(function_list = list(
  get_self_report_non_cancer_diagnoses,
  get_self_report_non_cancer_diagnoses_icd10,
  get_self_report_cancer_diagnoses,
  get_hes_icd9_diagnoses,
  get_hes_icd10_diagnoses,
  get_death_data_icd10_diagnoses,
  get_cancer_register_icd9_diagnoses,
  get_cancer_register_icd10_diagnoses
),
ukb_pheno,
data_dict,
ukb_codings) {

  start_time <- proc.time()

  # TODO - make safely version, and display informative error message re failed jobs
  result <- function_list %>%
    purrr::imap(~ {
      fn <- purrr::safely(.x)
      single_result <- fn(ukb_pheno = ukb_pheno,
                          data_dict = data_dict,
                          ukb_codings = ukb_codings)
      message("\nProcessed get diagnosis function ", .y, " of ", length(function_list))
      time_taken_message(start_time)
      return(single_result)
    })

  result <- result %>%
    purrr::transpose()

  errors <- result$error %>%
    purrr::compact() %>%
    purrr::map("message")

  result <- result$result %>%
    dplyr::bind_rows()

  # completion message
  message("Complete!")
  time_taken_message(start_time)

  # warning messages
  if (length(errors) > 0) {
    warning(paste0(length(errors),
                   " functions failed, generating the error messages above"))
    errors %>%
      purrr::walk( ~ message(.x))
  }

  # return result
  return(result)
}


# PRIVATE FUNCTIONS -------------------------------------------------------


# Get diagnoses helpers ---------------------------------------------------

#' Melt diagnoses code and date columns in a main UKB dataset
#'
#' For most clinical events variables in the main UKB dataset there is a
#' diagnostic code variable and an associated date of event variable. There are
#' spread over several instances/arrays. This function melts these to long
#' format.
#'
#' @inheritParams read_pheno
#' @inheritParams summarise_rowise
#' @param code_col_field_id character. The Field ID representing 'code'
#'   variables
#' @param date_col_field_id character. The Field ID representing 'date'
#'   variables that correspond to \code{code_col_field_id}.
#'
#' @noRd
#' @return A data table with 'eid', 'source' (Field ID for the code
#' column), 'code' and 'date' columns.
get_diagnoses_basis_fn <- function(ukb_pheno,
                                   data_dict,
                                   ukb_codings,
                                   code_col_field_id,
                                   date_col_field_id) {
  start_time <- proc.time()

  # validate args
  assertthat::assert_that(
    "data.table" %in% class(ukb_pheno),
    msg = "Error! ukb_pheno must be a data table"
  )

  assertthat::assert_that(
    is.character(code_col_field_id) & is.character(date_col_field_id),
    msg = "Error! Both `code_col_field_id` and `date_col_field_id` must be of type character"
  )

  assertthat::assert_that(
    (code_col_field_id %in% data_dict$FieldID) &
      (date_col_field_id %in% data_dict$FieldID),
    msg = paste0(
      "Error! Both FieldID ",
      code_col_field_id,
      " and FieldID ",
      date_col_field_id,
      " are required, one or both of these is not present in data_dict"
    )
  )

  # get lists of required columns
  code_cols <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = code_col_field_id,
                                return_col = "descriptive_colnames",
                                error_if_missing = TRUE)

  date_cols <- filter_data_dict(data_dict = data_dict,
                                filter_col = "FieldID",
                                filter_value = date_col_field_id,
                                return_col = "descriptive_colnames",
                                error_if_missing = TRUE)

  # `code_cols` and `date_cols` may be of different lengths e.g. try searching
  # the UKB data showcase for FieldIDs 40005 (date of cancer diagnosis) and
  # 40013 (type of cancer ICD-9). Some instance-arrays may also be omitted if
  # they contain no data, resulting in apparently 'missing' columns (e.g.
  # FieldID 40013 should have instance_arrays from 0-0 to 0-14, but may be
  # missing 12-0, perhaps because some ppts have lef the study and there are
  # therefore no longer any data entries for these columns)

  # ...all code columns should have a corresponding date column though. An error
  # is therefore raised here if there are 'missing' date columns.
  code_cols_instance_arrays <- colname_to_field_inst_array_df(code_cols)
  date_cols_instance_arrays <- colname_to_field_inst_array_df(date_cols)


  colnames_df <- code_cols_instance_arrays %>%
    dplyr::left_join(date_cols_instance_arrays,
                     by = "instance_array",
                     suffix = c(".code", ".date")) %>%
    dplyr::arrange(as.numeric(.data[["instance.code"]]))

  assertthat::assert_that(
    sum(is.na(colnames_df$descriptive_colnames.date)) == 0,
    msg = paste0("Error! Some date columns appear to be missing. Check that all columns for Field ID ",
                 code_col_field_id,
                 " have a corresponding column for Field ID ",
                 date_col_field_id)
  )

  # update code_cols and date_cols
  code_cols <- colnames_df$descriptive_colnames.code
  date_cols <- colnames_df$descriptive_colnames.date

  # check these are actually present in ukb_pheno
  check_required_cols_exist(df = ukb_pheno,
                            code_cols,
                            date_cols)

  # melt cols
  ukb_pheno <- data.table::melt(
    ukb_pheno %>% dplyr::select(tidyselect::all_of(c("eid", code_cols, date_cols))),
    measure = list(code_cols, date_cols),
    value.name = c("code", "date")
  )

  ukb_pheno <- ukb_pheno[!is.na(ukb_pheno$code)]

  # relabel code col
  ukb_pheno <- recode_ukbcol(df = ukb_pheno,
                             col_to_recode = "code",
                             field_id = code_col_field_id,
                             ukb_data_dict = data_dict,
                             ukb_codings = ukb_codings,
                             mapping_direction = "meaning_code")

  # make 'source' col
  ukb_pheno$variable <- paste0("f", code_col_field_id)
  names(ukb_pheno)[which(names(ukb_pheno) == "variable")] <- "source"

  # return result
  time_taken_message(start_time)
  return(ukb_pheno)
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

# Get diagnoses archived functions ----------------------------------------

#' Get death data ICD10 diagnoses
#'
#' Returns a long format dataframe with all death data ICD10 codes for each UK
#' Biobank participant.
#'
#' Reformats the data for FieldIDs 40001 and 40002 (death register
#' health-related outcomes - primary and secondary causes of death; see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100093}{category
#' 100093}).
#'
#' @section Under the hood:
#'
#'   Converts the data from FieldIDs 40001 and 40002 to long format separately
#'   before combining into a single dataframe.
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_death_data_icd10_diagnoses <- function(ukb_pheno,
                                           data_dict,
                                           ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100093 )
  death_data_field_ids <- c("40001", "40002")

  # initialise
  death_data_diagnoses <- vector(mode = "list", length = length(death_data_field_ids))
  names(death_data_diagnoses) <- paste0("f", death_data_field_ids)

  # get all diagnostic codes and dates (and standardise) - fieldid 40001
  death_data_diagnoses[["f40001"]] <- field_id_pivot_longer(
    field_id = "40001",
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # make date col: 'NA' (cannot ascertain dates of diagnoses from death certificates)
  # make date type - allows combining with output from other get_XXX functions
  death_data_diagnoses[["f40001"]]$date <- as.Date(NA)

  # remove rows with no codes
  death_data_diagnoses[["f40001"]] <- death_data_diagnoses[["f40001"]] %>%
    dplyr::filter(!is.na(.data[["f40001_value"]])) # remove rows with no codes

  death_data_diagnoses[["f40001"]] <- get_diagnoses_set_index_code_date_cols(get_clinical_events_df = death_data_diagnoses[["f40001"]],
                                                                             index_col = "f40001",
                                                                             code_col = "f40001_value",
                                                                             date_col = "date")

  # get all diagnostic codes and dates (and standardise) - fieldid 40002
  death_data_diagnoses[["f40002"]] <- field_id_pivot_longer(
    field_id = "40002",
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # make date col: 'NA' (cannot ascertain dates of diagnoses from death certificates)
  # make date type - allows combining with output from other get_XXX functions
  death_data_diagnoses[["f40002"]]$date <- as.Date(NA)

  # remove rows with no codes
  death_data_diagnoses[["f40002"]] <- death_data_diagnoses[["f40002"]] %>%
    dplyr::filter(!is.na(.data[["f40002_value"]])) # remove rows with no codes

  death_data_diagnoses[["f40002"]] <-
    get_diagnoses_set_index_code_date_cols(
      get_clinical_events_df = death_data_diagnoses[["f40002"]],
      index_col = "f40002",
      code_col = "f40002_value",
      date_col = "date"
    )

  # combine into single df
  death_data_diagnoses <- dplyr::bind_rows(death_data_diagnoses)

  # recode to ICD10 (NB at this stage, this column is already contains ICD10
  # codes but additionally includes descriptions e.g. "J95.1 Acute pulmonary
  # insufficiency following thoracic surgery")
  death_data_diagnoses <- recode_ukbcol(df = death_data_diagnoses,
                                        col_to_recode = "code",
                                        field_id = "40002",
                                        ukb_data_dict = data_dict,
                                        ukb_codings = ukb_codings,
                                        mapping_direction = "meaning_code")

  return(death_data_diagnoses)
}


#' Get hospital inpatient ICD9 diagnoses
#'
#' Returns a long format dataframe with all hospital inpatient ICD9 codes and
#' associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41271 and 41281 (hospital inpatient diagnoses
#' and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002}{category 2002}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Data frame
#' @noRd
#' @family get all diagnostic codes
OLD_get_hes_icd9_diagnoses <- function(ukb_pheno,
                                   data_dict,
                                   ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002 )
  hes_icd9_field_ids <- c("41271", "41281")

  # get all diagnostic codes and dates
  hes_icd9_diagnoses <- field_id_pivot_longer_multi(
    field_ids = hes_icd9_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  hes_icd9_diagnoses <- hes_icd9_diagnoses %>%
    dplyr::filter(!is.na(.data[["f41271_value"]]))

  # recode to ICD9
  hes_icd9_diagnoses <- recode_ukbcol(df = hes_icd9_diagnoses,
                                      col_to_recode = "f41271_value",
                                      field_id = "41271",
                                      ukb_data_dict = data_dict,
                                      ukb_codings = ukb_codings,
                                      mapping_direction = "meaning_code")

  # standardise
  hes_icd9_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = hes_icd9_diagnoses,
    index_col = "f41271",
    code_col = "f41271_value",
    date_col = "f41281_value")

  return(hes_icd9_diagnoses)
}


#' Get hospital inpatient ICD10 diagnoses
#'
#' Returns a long format dataframe with all hospital inpatient ICD10 codes and associated dates for
#' each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41270 and 41280 (hospital inpatient diagnoses
#' and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002}{category 2002}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_hes_icd10_diagnoses <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  hes_icd10_field_ids <- c("41270", "41280")

  # get all diagnostic codes and dates
  hes_icd10_diagnoses <- field_id_pivot_longer_multi(
    field_ids = hes_icd10_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  hes_icd10_diagnoses <- hes_icd10_diagnoses %>%
    dplyr::filter(!is.na(.data[["f41270_value"]]))

  # recode to ICD10
  hes_icd10_diagnoses <- recode_ukbcol(df = hes_icd10_diagnoses,
                                       col_to_recode = "f41270_value",
                                       field_id = "41270",
                                       ukb_data_dict = data_dict,
                                       ukb_codings = ukb_codings,
                                       mapping_direction = "meaning_code")

  # standardise
  hes_icd10_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = hes_icd10_diagnoses,
    index_col = "f41270",
    code_col = "f41270_value",
    date_col = "f41280_value")

  return(hes_icd10_diagnoses)
}


#' Get self-reported non-cancer diagnoses
#'
#' Returns a long format dataframe with all self-reported non-cancer diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20002 and 20008 (self-reported non-cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074}). Coded using
#' \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=6}{data coding 6}.
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_self_report_non_cancer_diagnoses <- function(ukb_pheno,
                                                 data_dict,
                                                 ukb_codings,
                                                 remove_special_dates = TRUE) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  self_report_non_cancer_field_ids <- c("20002", "20008")

  # get diagnostic codes and dates
  self_report_non_cancer_diagnoses <- field_id_pivot_longer_multi(
    field_ids = self_report_non_cancer_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  self_report_non_cancer_diagnoses <- self_report_non_cancer_diagnoses %>%
    dplyr::filter(!is.na(.data[["f20002_value"]]))

  # recode to ukb codes
  self_report_non_cancer_diagnoses <- suppressWarnings(recode_ukbcol(df = self_report_non_cancer_diagnoses,
                                                                     col_to_recode = "f20002_value",
                                                                     field_id = "20002",
                                                                     ukb_data_dict = data_dict,
                                                                     ukb_codings = ukb_codings,
                                                                     mapping_direction = "meaning_code"))

  # standardise
  self_report_non_cancer_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = self_report_non_cancer_diagnoses,
    index_col = "f20002",
    code_col = "f20002_value",
    date_col = "f20008_value",
    remove_special_dates = remove_special_dates)

  # convert date_col from decimal to date type
  self_report_non_cancer_diagnoses$date <- lubridate::as_date(lubridate::date_decimal(self_report_non_cancer_diagnoses$date))

  return(self_report_non_cancer_diagnoses)
}


#' Get self-reported non-cancer diagnoses (ICD10)
#'
#' Returns a long format dataframe with all self-reported non-cancer diagnoses
#' mapped to 3-character ICD10 codes with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20002 and 20009 (self-reported non-cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074}).
#'
#' \strong{Note: not all self-reported medical conditions map to ICD10 codes.}
#' (See \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=609}{data coding
#' 609})
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_self_report_non_cancer_diagnoses_icd10 <- function(ukb_pheno,
                                                       data_dict,
                                                       ukb_codings,
                                                       remove_special_dates = TRUE) {

  self_report_non_cancer_diagnoses_icd10 <-
    get_self_report_non_cancer_diagnoses(ukb_pheno = ukb_pheno,
                                         data_dict = data_dict,
                                         ukb_codings = ukb_codings,
                                         remove_special_dates = remove_special_dates)

  # ...now recode to ICD10
  mapping_df <- ukb_codings %>%
    dplyr::filter(.data[["Coding"]] == 609 & .data[["Value"]] != -1) %>%
    dplyr::select(tidyselect::all_of(c("Value", "Meaning")))

  dict <- mapping_df$Meaning
  names(dict) <- mapping_df$Value

  self_report_non_cancer_diagnoses_icd10$code <- revalue_vector(self_report_non_cancer_diagnoses_icd10$code,
                                                                dict = dict,
                                                                default_value = NA)

  # TO DELETE
  # mapping_df <- rename_cols(df = mapping_df,
  #             old_colnames = c("Value", "Meaning"),
  #             new_colnames = c("old_vals", "new_vals"))
  #
  # self_report_non_cancer_diagnoses_icd10 <- suppressWarnings(recode_column(df = self_report_non_cancer_diagnoses_icd10,
  #                                                                    col_to_recode = "code",
  #                                                                    mapping_df = mapping_df))

  # remove rows with no code (not all self-reported conditions map to ICD10)
  self_report_non_cancer_diagnoses_icd10 <- self_report_non_cancer_diagnoses_icd10 %>%
    dplyr::filter(!is.na(.data[["code"]]))

  # relabel 'source' col to indicate these are icd10 codes
  self_report_non_cancer_diagnoses_icd10$source <- paste0(self_report_non_cancer_diagnoses_icd10$source, "_icd10")

  return(self_report_non_cancer_diagnoses_icd10)
}


#' Get self-reported cancer diagnoses
#'
#' Returns a long format dataframe with all self-reported cancer diagnoses with
#' associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20001 and 20006 (self-reported cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category
#' 100074}). Coded using
#' \href{https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=3}{data coding 3}.
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{TRUE}. See
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_self_report_cancer_diagnoses <- function(ukb_pheno,
                                             data_dict,
                                             ukb_codings,
                                             remove_special_dates = TRUE) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  self_report_cancer_field_ids <- c("20001", "20006")

  # get diagnostic codes and dates
  self_report_cancer_diagnoses <- field_id_pivot_longer_multi(
    field_ids = self_report_cancer_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  self_report_cancer_diagnoses <- self_report_cancer_diagnoses %>%
    dplyr::filter(!is.na(.data[["f20001_value"]]))

  # recode to ukb codes
  self_report_cancer_diagnoses <- suppressWarnings(recode_ukbcol(df = self_report_cancer_diagnoses,
                                                                 col_to_recode = "f20001_value",
                                                                 field_id = "20001",
                                                                 ukb_data_dict = data_dict,
                                                                 ukb_codings = ukb_codings,
                                                                 mapping_direction = "meaning_code"))

  # standardise
  self_report_cancer_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = self_report_cancer_diagnoses,
    index_col = "f20001",
    code_col = "f20001_value",
    date_col = "f20006_value",
    remove_special_dates = remove_special_dates)

  # convert date_col from decimal to date type
  self_report_cancer_diagnoses$date <- lubridate::as_date(lubridate::date_decimal(self_report_cancer_diagnoses$date))

  return(self_report_cancer_diagnoses)
}


#' Get cancer register ICD9 diagnoses
#'
#' Returns a long format dataframe with all ICD9 cancer register diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 40013 and 40005 (cancer register diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092}{category 100092}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_cancer_register_icd9_diagnoses <- function(ukb_pheno,
                                               data_dict,
                                               ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092 )
  cancer_register_icd9_field_ids <- c("40013", "40005")

  # **TEMPFIX** - for fieldid 40013 (ICD9), there are only 14 instances, where as
  # fieldid 40006 (ICD10) and 40005 (date of cancer diagnosis) have 16
  # instances. field_id_pivot_longer_multi() raises an error if the number of
  # instances are not equal

  # Therefore, remove the 'extra' instance columns for 40005 before proceeding
  # check number of instances for FID 40013
  f40013_instances <- data_dict %>%
    dplyr::filter(.data[["FieldID"]] == "40013") %>%
    .$instance

  cols_to_remove <- data_dict %>%
    dplyr::filter(.data[["FieldID"]] == "40005") %>%
    dplyr::filter(!(.data[["instance"]] %in% f40013_instances)) %>%
    .$descriptive_colnames

  if (rlang::is_empty(cols_to_remove)) {
    stop("Required fields are not present in data_dict")
  }

  ukb_pheno <- ukb_pheno %>%
    dplyr::select(-tidyselect::all_of(cols_to_remove))

  data_dict <- data_dict %>%
    dplyr::filter(!(.data[["descriptive_colnames"]] %in% cols_to_remove))

  # get diagnostic codes and dates
  cancer_register_icd9_diagnoses <- field_id_pivot_longer_multi(
    field_ids = cancer_register_icd9_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  cancer_register_icd9_diagnoses <- cancer_register_icd9_diagnoses %>%
    dplyr::filter(!is.na(.data[["f40013_value"]]))

  # recode to ICD9
  cancer_register_icd9_diagnoses <- recode_ukbcol(df = cancer_register_icd9_diagnoses,
                                                  col_to_recode = "f40013_value",
                                                  field_id = "40013",
                                                  ukb_data_dict = data_dict,
                                                  ukb_codings = ukb_codings,
                                                  mapping_direction = "meaning_code")

  # standardise
  cancer_register_icd9_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = cancer_register_icd9_diagnoses,
    index_col = "f40013",
    code_col = "f40013_value",
    date_col = "f40005_value")

  return(cancer_register_icd9_diagnoses)
}


#' Get cancer register ICD10 diagnoses
#'
#' Returns a long format dataframe with all ICD10 cancer register diagnoses
#' with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 40006 and 40005 (cancer register diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=100092}{category 100092}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @noRd
#' @family get all diagnostic codes
OLD_get_cancer_register_icd10_diagnoses <- function(ukb_pheno,
                                                data_dict,
                                                ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  cancer_register_icd10_field_ids <- c("40006", "40005")

  # get diagnostic codes and dates
  cancer_register_icd10_diagnoses <- field_id_pivot_longer_multi(
    field_ids = cancer_register_icd10_field_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  cancer_register_icd10_diagnoses <- cancer_register_icd10_diagnoses %>%
    dplyr::filter(!is.na(.data[["f40006_value"]]))

  # recode to ICD9
  cancer_register_icd10_diagnoses <- recode_ukbcol(df = cancer_register_icd10_diagnoses,
                                                   col_to_recode = "f40006_value",
                                                   field_id = "40006",
                                                   ukb_data_dict = data_dict,
                                                   ukb_codings = ukb_codings,
                                                   mapping_direction = "meaning_code")

  # standardise
  cancer_register_icd10_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_clinical_events_df = cancer_register_icd10_diagnoses,
    index_col = "f40006",
    code_col = "f40006_value",
    date_col = "f40005_value")

  return(cancer_register_icd10_diagnoses)
}

#' Standardise a long format diagnoses dataframe
#'
#' Helper function
#'
#' Selects essential columns: eid, field_id, code and date recorded.
#'
#' @param get_clinical_events_df a dataframe created by a 'get_X_diagnoses' function
#' @param index_col Column indicating diagnosis source (e.g. HES, self-report)
#' @param code_col Column of codes
#' @param date_col Column of dates when code was recorded
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{FALSE}, as only the self-report fields contain
#'   such values.
#' @family get all diagnostic codes
#' @noRd
OLD_get_diagnoses_set_index_code_date_cols <- function(get_clinical_events_df,
                                                   index_col,
                                                   code_col,
                                                   date_col,
                                                   remove_special_dates = FALSE) {

  # create 'source' col indicating which FieldID the data (code) comes from
  get_clinical_events_df$source <- index_col

  # select required columns and rename
  get_clinical_events_df <- get_clinical_events_df %>%
    dplyr::select(.data[["eid"]],
           .data[["source"]],
           code = .data[[code_col]],
           date = .data[[date_col]],
           source_col = .data[[index_col]])

  # remove self-report special date values if requested
  if (remove_special_dates == TRUE) {

    # special dates to remove
    # FID 20006 and 20008; https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20008
    # https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20006
    self_report_cancer_and_non_cancer_diagnoses_special_dates <- c(
      -3,
      -1
    )

    # remove special dates
    get_clinical_events_df$date <- ifelse(
      test = get_clinical_events_df$date %in% self_report_cancer_and_non_cancer_diagnoses_special_dates,
      yes = NA,
      no = get_clinical_events_df$date
    )

  }

  return(get_clinical_events_df)
}


#' Convert column for a FieldID to long format
#'
#' Helper function. Selects the columns relating to a specified FieldID (plus
#' the \code{eid} column) and applies \code{\link[tidyr]{pivot_longer}}.
#'
#' @param field_id Character. A UK Biobank Field ID
#' @inheritParams read_pheno
#' @inheritParams summarise_rowise
#'
#' @return A long format data frame with 3 columns: \itemize{ \item eid \item
#'   column names (labelled as the specified \code{field_id}, prefixed by 'f')
#'   \item column values (labelled as he specified \code{field_id}, prefixed by
#'   'f' and suffixed by '_value') }
#'
#' @family extract disease outcomes helpers
field_id_pivot_longer <- function(ukb_pheno,
                                  field_id,
                                  data_dict,
                                  ukb_codings) {

  # check a single field_id has been supplied
  assertthat::is.string(field_id)

  # filter ukb_pheno for selected cols (eid + fieldid cols)
  required_cols <- get_colnames_for_fieldids(field_id = field_id,
                                             data_dict = data_dict)

  check_required_cols_exist(df = ukb_pheno,
                            required_cols)

  ukb_pheno <- ukb_pheno %>%
    dplyr::select(tidyselect::all_of(c("eid", required_cols)))

  # TO DELETE?
  # get codings for fieldid
  # field_id_codings <- ukb_codings %>%
  #   dplyr::filter(.data[["Coding"]] == (data_dict %>%
  #                              dplyr::filter(.data[["FieldID"]] == field_id) %>%
  #                              .$Coding %>%
  #                              utils::head(n = 1)))

  ukb_pheno <- ukb_pheno %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(required_cols),
      values_to = paste(paste0("f", field_id), "value", sep = "_")
    )

  ukb_pheno$instance_array <- stringr::str_extract(ukb_pheno$name,
                                                   pattern = "[:digit:]+_[:digit:]+$")

  # rename
  names(ukb_pheno)[which(names(ukb_pheno) == 'name')] <- paste0("f", field_id)

  return(ukb_pheno)
}


#' Apply pivot_longer across multiple sets of columns
#'
#' Helper function.
#'
#' This should only be applied to related FieldIDs which are each associated
#' with the same number of columns. For example, FieldIDs 20002, 20008, and
#' 20009 all pertain to self-reported non-cancer illnesses.
#'
#' Raises and error if the supplied FieldIDs are associated with differing
#' numbers of columns, or if the result has more rows than expected.
#'
#' @section Under the hood:
#'
#'   Maps \code{\link{field_id_pivot_longer}} to a set of FieldIDs, then
#'   recombines the results into a single dataframe using
#'   \code{\link[dplyr]{inner_join}}.
#'
#' @param field_ids Character vector of FieldIDs
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe.
#' @noRd
#' @family extract disease outcomes helpers
field_id_pivot_longer_multi <- function(field_ids,
                                        ukb_pheno,
                                        data_dict,
                                        ukb_codings) {

  # get required cols and check they exist
  required_cols <- field_ids %>%
    purrr::set_names(nm = paste0("f", .)) %>%
    purrr::map(get_colnames_for_fieldids,
              data_dict = data_dict)

  check_required_cols_exist(df = ukb_pheno,
                            purrr::flatten_chr(required_cols))

  # check field ids have matching instances/arrays (intersect should equal union)
  required_cols_inst_arrays <- required_cols %>%
    purrr::map(colname_to_field_inst_array_df) %>%
    purrr::map("instance_array")

  # TODO - write formal test for this (I've checked manually)
  assertthat::assert_that(
    length(purrr::reduce(required_cols_inst_arrays, union)) == length(purrr::reduce(required_cols_inst_arrays, intersect)),
    msg = paste0("Error! The supplied field IDs (",
                 stringr::str_c(field_ids, sep = "", collapse = ", "),
                 ") have differing instances/arrays. Try checking these manually in `data_dict`.")
  )

  # expected number of rows for each fieldid should be the same - this returns a
  # numerical vector, each number is the number of columns selected for a
  # fieldid
  expected_row_numbers <- field_ids %>%
    purrr::map(get_colnames_for_fieldids,
               data_dict = data_dict) %>%
    purrr::map_dbl(length) %>%
    purrr::map_dbl(~ .x * nrow(ukb_pheno))

  # check these are numeric and all the equal
  assertthat::assert_that(is.numeric(expected_row_numbers))

  if (length(unique(expected_row_numbers)) != 1) {
    stop("Selected FieldIDs have differing numbers of associated columns")
  }

  # pivot_longer and join results into a single dataframe
  result <- field_ids %>%
    purrr::map(field_id_pivot_longer,
               ukb_pheno = ukb_pheno,
               data_dict = data_dict,
               ukb_codings = ukb_codings)

  result <- result %>%
    purrr::reduce(dplyr::inner_join, by = c("eid", "instance_array"))

  # check that row number matches the expected value
  if (nrow(result) != (expected_row_numbers[1])) {
    stop("Result does not contain the expected number of rows. Aborting.")
  }

  return(result)
}

#' Standardise a long format diagnoses dataframe
#'
#' Helper function
#'
#' Selects essential columns: eid, field_id, code and date recorded.
#'
#' @param get_clinical_events_df a dataframe created by a 'get_X_diagnoses' function
#' @param index_col Column indicating diagnosis source (e.g. HES, self-report)
#' @param code_col Column of codes
#' @param date_col Column of dates when code was recorded
#' @param remove_special_dates Logical. Remove 'special' date values if
#'   requested. Default is \code{FALSE}, as only the self-report fields contain
#'   such values.
#' @family get all diagnostic codes
get_diagnoses_set_index_code_date_cols <- function(get_clinical_events_df,
                                                   index_col,
                                                   code_col,
                                                   date_col,
                                                   remove_special_dates = FALSE) {

  # create 'source' col indicating which FieldID the data (code) comes from
  get_clinical_events_df$source <- index_col

  # select required columns and rename
  get_clinical_events_df <- get_clinical_events_df %>%
    dplyr::select(.data[["eid"]],
                  .data[["source"]],
                  code = .data[[code_col]],
                  date = .data[[date_col]],
                  source_col = .data[[index_col]])

  # remove self-report special date values if requested
  if (remove_special_dates == TRUE) {

    # special dates to remove
    # FID 20006 and 20008; https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20008
    # https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20006
    self_report_cancer_and_non_cancer_diagnoses_special_dates <- c(
      -3,
      -1
    )

    # remove special dates
    get_clinical_events_df$date <- ifelse(
      test = get_clinical_events_df$date %in% self_report_cancer_and_non_cancer_diagnoses_special_dates,
      yes = NA,
      no = get_clinical_events_df$date
    )

  }

  return(get_clinical_events_df)
}

# Extract specific diagnostic code helpers --------------------------------

#' Helper function for \code{extract_first_or_last_clinical_event_multi()} - Get
#' the earliest record date for multiple phenotype categories within a single
#' disease
#'
#' Builds on \code{\link{extract_first_or_last_clinical_event}}, extracting
#' either the first or last recorded clinical event for multiple phenotype
#' categories within a single disease
#'
#' @param disease scalar character. \code{clinical_codes_df} will be filtered
#'   for this value in the 'disease' column.
#' @param clinical_codes_df data frame. Must match the format as per
#'   \code{\link{generate_self_reported_diabetes_codes_df}}.
#' @param prefix character. Optionally add a prefix to column names.
#' @inheritParams extract_single_diagnostic_code_record_basis
#' @inheritParams extract_first_or_last_clinical_event
#'
#' @return Data frame with an "eid" column, and "event_min/max_indicator" and
#'   "event_min/max_date" columns for each phenotype in the 'category' column of
#'   \code{clinical_codes_df}.
#'
#' @noRd
#' @family extract specific diagnostic codes functions
extract_first_or_last_clinical_event_multi_single_disease <-
  function(disease,
           df,
           clinical_codes_df,
           min_max,
           prefix) {

    start_time <- proc.time()

    # validate args
    assertthat::is.string(disease)

    message(paste0("Extracting clinical events for disease: ", disease))

    # filter clinical_codes_df for single disease
    clinical_codes_df <- clinical_codes_df[clinical_codes_df$disease == disease, ]

    assertthat::assert_that(
      !(disease %in% clinical_codes_df$category)
      ,
      msg = paste0(
        "Error! ",
        disease,
        " is present in both the 'disease' and 'category' columns of clinical_code_df"
      )
    )

    # prepare clinical_codes_df so that overall summary columns are also created in upper case
    clinical_codes_df <- dplyr::bind_rows(clinical_codes_df,
                                          clinical_codes_df %>%
                                            dplyr::mutate("category" = disease))

    # clinical_codes_df$category: remove special characters and convert to lower
    # case. This will be used to label the columns with
    # `extract_first_or_last_clinical_event()`
    clinical_codes_df$phenotype_name <-
      remove_special_characters_and_make_lower_case(clinical_codes_df$category)

    clinical_codes_df$phenotype_name <-
      paste0(clinical_codes_df$phenotype_name,
             "_",
             clinical_codes_df$phenotype_source)
    clinical_codes_df$phenotype_name <-
      ifelse(
        clinical_codes_df$category == clinical_codes_df$disease,
        toupper(clinical_codes_df$phenotype_name),
        clinical_codes_df$phenotype_name
      )

    # add prefix if specified
    if (!is.null(prefix)) {
      clinical_codes_df$phenotype_name <-
        paste0(prefix, clinical_codes_df$phenotype_name)
    }

    # make 2 named list of code lists for each phenotype: 1 will contain code
    # lists, the other will contain the results from
    # `extract_first_or_last_clinical_event()`
    list_of_phenotype_codelists <- vector(mode = "list",
                                          length = length(unique(clinical_codes_df$phenotype_name)))
    names(list_of_phenotype_codelists) <-
      sort(unique(clinical_codes_df$phenotype_name))

    list_of_phenotype_results <- list_of_phenotype_codelists

    # populate `list_of_phenotype_codelists` with empty code lists
    for (phenotype in names(list_of_phenotype_codelists)) {
      list_of_phenotype_codelists[[phenotype]] <-
        make_empty_clinical_codes_list()
    }

    # now populate these code lists loop through phenotypes
    for (phenotype in names(list_of_phenotype_codelists)) {
      # filter codes df for phenotype category
      single_phenotype_df <- clinical_codes_df %>%
        dplyr::filter(.data[["phenotype_name"]] == phenotype)

      # loop through code_types to populate code lists for phenotype category
      for (code_type in single_phenotype_df$code_type) {
        list_of_phenotype_codelists[[phenotype]][[code_type]] <-
          single_phenotype_df[single_phenotype_df$code_type == code_type, ]$code
      }
    }

    # now filter `df` for all codes in this `clinical_codes_df`. This is to speed
    # up the next step.

    # TODO ...may be problematic if returns too large a result though
    message("Filtering df for all codes in clinical_codes_df")
    df <- filter_clinical_events_for_codes(df,
                                           clinical_codes_df$code)

    # exit early with warning if no rows in `df`
    if (nrow(df) == 0) {
      warning(
        "Warning! `df` does not contain any of the codes in `clinical_codes_df$code`. Returning `NULL`"
      )
      return(NULL)
    }

    # get results: loop through phenotypes
    message("EXTRACTING EVENT DATES FOR PHENOTYPES")
    n_phenotypes <- length(names(list_of_phenotype_results))
    i <- 1
    for (phenotype in names(list_of_phenotype_results)) {
      message(
        paste0(
          "Extracting event dates for ",
          phenotype,
          " (Phenotype ",
          i,
          " of ",
          n_phenotypes,
          ")"
        )
      )
      list_of_phenotype_results[[phenotype]] <-
        extract_first_or_last_clinical_event(
          df = df,
          codes = list_of_phenotype_codelists[[phenotype]],
          phenotype_name = phenotype,
          min_max = min_max
        )
      i <- i + 1
      time_taken_message(start_time)
    }

    # combine
    list_of_phenotype_results %>%
      purrr::compact() %>%
      purrr::reduce(dplyr::full_join, by = "eid")
  }

#' Extract a single record from a dataframe generated by one of the
#' \code{get_XXX_diagnoses} functions (generic)
#'
#' Generic helper function. For a set of diagnostic codes, filter for eids with
#' at least one of these and extract a single row per eid (e.g. the earliest
#' date that one of the diagnostic codes appears).
#'
#' @param df Either a dataframe or a \code{\link[dplyr]{tbl}} object generated
#'   by one of the \code{get_XXX_diagnoses} functions
#' @param codes Either a character vector or named list of diagnostic codes. If
#'   a list, then each named item should be a character vector of codes (use
#'   \code{\link{make_empty_clinical_codes_list}}).
#' @param mapping_function a function that will be applied to \code{df} nested
#'   by eid, extracting a single row per eid (e.g. the earliest date that one of
#'   the \code{codes} appears). This will be applied using
#'   \code{\link[purrr]{map_chr}} and should return the \code{source},
#'   \code{code} and \code{date} columns as a single character, separated by
#'   "_SEP_".
#' @param ... additional args are passed on to \code{mapping_function}.
#'
#' @return A dataframe nested by eid, with \code{source}, \code{code} and
#'   \code{date} columns for row extracted by the \code{mapping_function}.
#'
#' @family extract specific diagnostic codes functions
extract_single_diagnostic_code_record_basis <- function(df,
                                                        codes,
                                                        mapping_function,
                                                        ...) {
  # validate args
  assertthat::assert_that("function" %in% class(mapping_function),
                          msg = "`mapping_function` must be a function")

  # filter clinical events (a df or tbl_sql object) for codes (a character
  # vector, or a named list of codes)
  df <- filter_clinical_events_for_codes(df = df,
                                         codes = codes)

  # check that date column is date type (not character)
  assertthat::assert_that(class(df$date) == "Date",
                          msg = "`date` column is not of class 'Date'")

  #nest by eid and extract earliest date + code (or just a code if no dates
  #available)
  df <- df %>%
    dplyr::group_by(.data[["eid"]]) %>%
    tidyr::nest()

  df$result <- purrr::map_chr(df$data, mapping_function)
    # dplyr::mutate(result = purrr::map_chr(data, mapping_function)) %>%
  df <- df %>%
    tidyr::separate(.data[["result"]],
                    into = c("source", "code", "date"),
                    sep = "_SEP_") %>%
    suppressWarnings() # separate() raises if there are rows with no dates - suppress these

  # convert date column back to date type
  df$date <- as.Date(df$date)

  return(df)
}

#' Helper function for \code{\link{extract_single_diagnostic_code_record_basis}}
#'
#' Performs the code filtering step in
#' \code{\link{extract_single_diagnostic_code_record_basis}}. \code{df} can be
#' either a data frame or a tbl_sql object. \code{codes} may be either a
#' character vector of codes or a named list of codes (format as per
#' \code{\link{make_empty_clinical_codes_list}}). For the latter, codes are
#' filtered by code type.
#'
#' @param df_class character. Either "df", or "tbl_sql".
#' @inheritParams  extract_single_diagnostic_code_record_basis
#'
#' @return a data frame
#' @noRd
filter_clinical_events_for_codes <- function(df,
                                             codes) {
  # validate args
  expected_colnames <-  c("eid", "source", "code", "date")
  assertthat::assert_that(all(expected_colnames %in% colnames(df)),
                          msg = "`df` does not contain the expected columns: eid, source, code, date")
  assertthat::assert_that(all(class(codes) %in% c("character", "list")),
                          msg = "`codes` must be either a character vector or list")

  if (class(codes) == "list") {
    # check names are valid and unique
    assertthat::assert_that(
      all(
        names(codes) %in% ukbwranglr:::clinical_events_sources$data_coding
      ) &
        length(unique(names(codes))) == length(names(codes)),
      msg = paste(
        "If `codes` is a list, `names(codes)` must be unique and be one of the following:",
        stringr::str_c(
          unique(ukbwranglr:::clinical_events_sources$data_coding),
          sep = "",
          collapse = ", "
        )
      )
    )
  }

  # filter for selected codes. Depends on:

  # 1: Whether `df` is a dataframe or tbl_sql object
  # 2: Whether `codes` is a character vector or list

  # TODO replace nested ifelse statements with S3
  # methods?

  # if `df` is a "tbl_sql" object
  if (all(class(df) %in% c(
    "tbl_SQLiteConnection",
    "tbl_dbi",
    "tbl_sql",
    "tbl_lazy",
    "tbl"
  ))) {
    if (class(codes) == "character") {
      df <- df %>%
        dplyr::filter(.data[["code"]] %in% codes) %>%
        dplyr::collect()
      # need to convert this from character to a date
      df$date <- as.Date(df$date)
    } else if (class(codes) == "list") {
      # filter for codes only within sources that use that code_type

      df <- filter_clinical_events_for_list_of_codes(df,
                                                     df_class = "tbl_sql",
                                                     codes = codes)
    }

  } else if (all(class(df) %in% c("data.table", "data.frame", "tbl_df", "tbl"))) {
    # if `df` is a data frame (or data table/tibble)
    if (class(codes) == "character") {
      df <- df %>%
        dplyr::filter(.data[["code"]] %in% codes)
    } else if (class(codes) == "list") {
      # filter for codes only within sources that use that code_type

      df <- filter_clinical_events_for_list_of_codes(df,
                                                     df_class = "df",
                                                     codes = codes)
    }

  } else {
    stop(paste(
      "Error! `class(df)` is not valid. Check `class(df)` matches one of: ",
      c(
        "tbl_SQLiteConnection",
        "tbl_dbi",
        "tbl_sql",
        "tbl_lazy",
        "tbl",
        "data.table",
        "data.frame",
        "tbl_df",
        "tbl"
      )
    ))
  }
  return(df)
}

#' Helper function for \code{\link{filter_clinical_events_for_codes}}
#'
#' Performs the code filtering step in
#' \code{\link{filter_clinical_events_for_codes}} when \code{codes}
#' is of class \code{list}.
#'
#' @param df_class character. Either "df", or "tbl_sql".
#' @inheritParams  extract_single_diagnostic_code_record_basis
#'
#' @return a data frame
#' @noRd
filter_clinical_events_for_list_of_codes <- function(df,
                                                     df_class,
                                                     codes) {
  # validate args
  match.arg(arg = df_class,
            choices = c("df", "tbl_sql"))

  if (df_class == "df") {
    assertthat::assert_that(
      all(class(df) %in% c("data.table", "data.frame", "tbl_df", "tbl")),
      msg = paste("Error! arg `df_class` == 'df' but `class(df)` is: ", stringr::str_c(class(df),
                                                                                       sep = "",
                                                                                       collapse = ", "))
    )
  } else if (df_class == "tbl_sql") {
    assertthat::assert_that(
      all(class(df) %in% c("tbl_SQLiteConnection",
                           "tbl_dbi",
                           "tbl_sql",
                           "tbl_lazy",
                           "tbl")),
      msg = paste("Error! arg `df_class` == 'tbl_sql' but `class(df)` is: ", stringr::str_c(class(df),
                                                                                            sep = "",
                                                                                            collapse = ", "))
    )
  }

  # ***A reminder to amend this section if new types of
  # data_coding are added to
  # ukbwranglr:::clinical_events_sources$data_coding***
  assertthat::assert_that(all(
    unique(sort(ukbwranglr:::clinical_events_sources$data_coding)) == sort(c(
      'icd10',
      'data_coding_6',
      'data_coding_3',
      'icd9',
      'read2',
      'read3',
      'opcs3',
      'opcs4'
    ))),
    msg = "Error! check code for `ukbwranglr:::clinical_events_sources$data_coding` and amend the filter statement in this function to address all possible code types")

  # filter Note: for dbplyr (tbl_sql objects), cannot use
  # `get_sources_for_code_type("icd9")` or `codes$icd9` in the filter statement
  # below. It will silently return an error result

  icd9_sources <- get_sources_for_code_type("icd9")
  icd10_sources <- get_sources_for_code_type("icd10")
  read2_sources <- get_sources_for_code_type("read2")
  read3_sources <- get_sources_for_code_type("read3")
  data_coding_6_sources <- get_sources_for_code_type("data_coding_6")
  data_coding_3_sources <- get_sources_for_code_type("data_coding_3")
  opcs3_sources <- get_sources_for_code_type("opcs3")
  opcs4_sources <- get_sources_for_code_type("opcs4")

  icd9_codes <- codes$icd9
  icd10_codes <- codes$icd10
  read2_codes <- codes$read2
  read3_codes <- codes$read3
  data_coding_6_codes <- codes$data_coding_6
  data_coding_3_codes <- codes$data_coding_3
  opcs3_codes <- codes$opcs3
  opcs4_codes <- codes$opcs4

  df <- df %>%
    dplyr::filter(
      (
        .data[["source"]] %in% icd9_sources &
          .data[["code"]] %in% icd9_codes
      ) |
        (
          .data[["source"]] %in% icd10_sources &
            .data[["code"]] %in% icd10_codes
        ) |
        (
          .data[["source"]] %in% read2_sources &
            .data[["code"]] %in% read2_codes
        ) |
        (
          .data[["source"]] %in% read3_sources &
            .data[["code"]] %in% read3_codes
        ) |
        (
          .data[["source"]] %in% data_coding_6_sources &
            .data[["code"]] %in% data_coding_6_codes
        ) |
        (
          .data[["source"]] %in% data_coding_3_sources &
            .data[["code"]] %in% data_coding_3_codes
        ) |
        (
          .data[["source"]] %in% opcs3_sources &
            .data[["code"]] %in% opcs3_codes
        ) |
        (
          .data[["source"]] %in% opcs4_sources &
            .data[["code"]] %in% opcs4_codes
        )
    )

  if (df_class == "tbl_sql") {
    df <- df %>%
      dplyr::collect() %>%
      # need to convert this from character to a date
      dplyr::mutate(date = as.Date(date))
  }

  return(df)
}

# (mappers) ---------------------------------------------------------------

# Helper for `extract_single_diagnostic_code_record_basis()`. A mapping function
# to extract earliest diagnostic code data for each eid - returns a single
# character. If no dates available, will return the first row of the data frame
extract_first_or_last_record_mapper <- function(df,
                                                min_max = "min") {
  # validate args
  match.arg(arg = min_max,
            choices = c("min", "max"))

  # get row index for earliest/latest date
  if (min_max == "min") {
    selected_date <- which.min(df$date)
  } else if (min_max == "max") {
    selected_date <- which.max(df$date)
  }

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

#' Helper function for \code{link{extract_single_diagnostic_code_record_basis}}
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
            ukbwranglr:::clinical_events_sources$data_coding)

  ukbwranglr:::clinical_events_sources %>%
    dplyr::filter(.data[["data_coding"]] == code_type) %>%
    .$source
}
