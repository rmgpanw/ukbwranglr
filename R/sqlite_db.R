# NOTES -------------------------------------------------------------------

#TODO

# EXPORTED FUNCTIONS ----------------------------------------------------

#' Write a file to a database
#'
#' Writes a file in chunks to a SQLite database, with or without pre-processing.
#'
#' This function is designed to be used with large files that may not fit into
#' memory on a personal computer. The file is written to a table in a SQLite
#' database in chunks with the option to apply a 'pre-processing' function to
#' each chunk (e.g. reshape the data).
#'
#' @section Under the hood:
#'
#'   Relies on the \href{https://db.rstudio.com/dbi/}{DBI} and
#'   \href{https://db.rstudio.com/databases/sqlite/}{RSQLite} packages to create
#'   and write to a SQLite database, and \code{\link[readr]{read_delim_chunked}}
#'   to read the input file in chunks.
#'
#' @param file Character. The path to the file to be written to \code{db_path}.
#' @param db_path Character. Path to a SQLite database. A new database will be
#'   created if this does not already exist. Default is "ukb.db".
#' @param table Character. Name of table to write to.
#' @param chunk_size The number of rows to include in each chunk. Default is
#'   10000.
#' @param col_types Default is for all columns to be type character.
#' @param verbose Print time taken after each chunk has been written to
#'   database.
#' @param append Append to table if already exists. Default is \code{FALSE}.
#' @param data_processing_function A function to be applied to each chunk before
#'   writing to database. Default is `NULL`.
#' @inheritParams readr::read_delim_chunked
#' @param ... additional parameters passed on to \code{data_processing_function}.
#'
#' @return Returns a database connection object invisibly.
#'
#' @export
#'
#' @family Generate a UKB database.
file_to_sqlite_db <- function(file,
                              table,
                              db_path = "ukb.db",
                              chunk_size = 10000,
                              delim = "\t",
                              col_types = readr::cols(.default = "c"),
                              # indexes = NULL,
                              verbose = TRUE,
                              append = FALSE,
                              data_processing_function = NULL,
                              quote = "\"",
                              escape_backslash = FALSE,
                              escape_double = TRUE,
                              col_names = TRUE,
                              locale = readr::default_locale(),
                              na = c("", "NA"),
                              quoted_na = TRUE,
                              comment = "",
                              trim_ws = FALSE,
                              skip = 0,
                              guess_max = min(1000, chunk_size),
                              progress = readr::show_progress(),
                              skip_empty_rows = TRUE,
                              ...) {

  start_time <- proc.time()

  # Create sqlite db connection
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Error message if table already exists and append == FALSE
  if (
    table %in% DBI::dbListTables(con) &
    append == FALSE) {
    stop("Error! Table already exists in database. Specify `append = TRUE` to append data. To overwrite, first delete table with `DBI::dbRemoveTable(conn, 'table_name')`")
  }

  f <- function(x,
                pos) {
    time_taken <- proc.time() - start_time

    if (verbose == TRUE) {
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

    if (!is.null(data_processing_function)) {
      x <- data_processing_function(x, ...)
    }

    DBI::dbWriteTable(
      conn = con,
      name = table,
      value = x,
      overwrite = FALSE, # ensure table is not inadvertently overwritten
      append = TRUE # needs to be TRUE, otherwise cannot write in chunks with read_delim_chunked()
      # field.types = field.types
    )
  }

  # Read file and write to table in chunks
  message("Writing file to table")
  readr::read_delim_chunked(
    file = file,
    callback = readr::DataFrameCallback$new(f),
    chunk_size = chunk_size,
    delim = delim,
    col_types = col_types,
    quote = quote,
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    col_names = col_names,
    locale = locale,
    na = na,
    quoted_na = quoted_na,
    comment = comment,
    trim_ws = trim_ws,
    skip = skip,
    guess_max = guess_max,
    progress = progress,
    skip_empty_rows = skip_empty_rows
  )

  # Completion message
  time_taken <- proc.time() - start_time
  message("Complete. Time taken: ",
          (time_taken[3] %/% 60),
          " minutes, ",
          (round(time_taken[3] %% 60)),
          " seconds")

  # return db connection object invisibly
  invisible(con)
}


#' Write diagnoses to a SQLite database
#'
#' Writes a long format dataframe generated by one of the 'get all diagnostic
#' codes' functions (see 'See Also' section at bottom for a list of these) to a
#' SQLite database. If no database exists at \code{db_path}, then a new one will
#' be created. Returns the database connection object.
#'
#' @inheritParams file_to_sqlite_db
#' @inheritParams extract_single_diagnostic_code_record_basis
#' @param overwrite Logical. If \code{TRUE}, will overwrite an existing table
#'   with the same name. Default is \code{FALSE}
#' @return Returns a database connection object invisibly.
#'
#' @export
#' @family get all diagnostic codes
main_dataset_diagnoses_to_sqlite_db <- function(df,
                                                table,
                                                db_path = "ukb.db",
                                                overwrite = FALSE,
                                                append = FALSE
) {
  start_time <- proc.time()
  # TODO - add checks that df is of the expected format

  # convert date col to character
  message("Converting dates to character type")
  df$date <- as.character(df$date)

  # create connection
  # Create sqlite db connection
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  message("Writing to database")
  # write to
  DBI::dbWriteTable(
    conn = con,
    name = table,
    value = df,
    overwrite = overwrite,
    append = append
  )

  message("Complete!")
  time_taken_message(start_time)
  invisible(con)
}

#' Pre-processing function - reformat UK Biobank primary care clinical events
#' data
#'
#' To be used with \code{\link{file_to_sqlite_db}}. Reformats the UK Biobank
#' primary care clinical events dataset to match the output format for
#' \code{\link{get_all_diagnostic_codes_multi}}.
#'
#' @section Under the hood:
#'
#'   The UK Biobank primary care data lists read codes in separate columns, one
#'   for Read2 and one for Read3. This function reshapes the data to long format
#'   so that all codes are in a single column. Note that the 3 free-text 'value'
#'   columns are dropped. By default, special date values are set to \code{NA}.
#'
#' @param df A dataframe
#' @param remove_special_dates Logical. Removes special date values if requested
#'   (see \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource
#'   591} for details). Default is \code{TRUE}.
#'
#' @export
#' @return data frame
#' @family Generate a UKB database.
gp_clinical_to_sqlite_db <- function(df, remove_special_dates = TRUE) {
  df <- df %>%
    dplyr::select(
      .data[["eid"]],
      .data[["event_dt"]],
      .data[["read_2"]],
      .data[["read_3"]]
    ) %>% # remove 3 'value' cols
    tidyr::pivot_longer(
      cols = c("read_2", "read_3"),
      names_to = "source",
      values_to = "code"
    ) %>%
    dplyr::filter(!is.na(.data[["code"]])) %>% # remove redundant rows (original data has no rows with a value in bot read2/3 cols)
    dplyr::select(.data[["eid"]],
                  .data[["source"]],
                  .data[["code"]],
                  .data[["event_dt"]])

  # relabel 'read_2' and 'read_3' to 'gpc_r2' and 'gpc_r3'
  df$source <- dplyr::case_when(
    df$source == "read_2" ~ "gpc_r2",
    df$source == "read_3" ~ "gpc_r3",
    TRUE ~ "gpc_unknown_coding"
  )

  # rename 'event_dt' to 'date'
  df <- rename_cols(df,
                    old_colnames = "event_dt",
                    new_colnames = "date")

  # reformat date (needs to be character when writing to db)
  df$date <- df$date %>%
    lubridate::dmy() %>%
    as.character()

  # remove special dates if requested (default is to remove)
  if (remove_special_dates == TRUE) {

    # primary care dates to remove
    # see https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591
    primary_care_special_dates_to_remove <- c("01/01/1901",
                                              "02/02/1902",
                                              "03/03/1903",
                                              "07/07/2037") %>%
      lubridate::dmy() %>%
      as.character()

    df$date <- ifelse(
      test = df$date %in% primary_care_special_dates_to_remove,
      yes = NA,
      no = df$date
    )
  }

  return(df)
}

#' Create a SQLite database with a \code{clinical_events} table
#'
#' Creates a SQLite database called \code{ukb.db} containing all tables from
#' \code{\link{get_ukb_db}}, plus an additional table called
#' \code{clinical_events}. This is a long format table combining all clinical
#' events as listed in \code{ukbwranglr:::CLINICAL_EVENTS_SOURCES}.
#'
#' @param ukb_pheno_path character. Path to the main UKB dataset file.
#' @param gp_clinical_path character. Path to the UKB primary care clinical
#'   events file (\code{gp_clinical.txt}).
#' @param ukb_db_dir character. Directory where \code{ukb.db} should be written
#'   to. An error is raised if a file called \code{ukb.db} already exists here.
#' @param allow_missing_fields logical. If TRUE, create database regardless of
#'   whether the main UKB dataset file is missing any required clinical events
#'   fields. Default is \code{FALSE}.
#'
#' @return NULL
#' @export
make_clinical_events_db <- function(ukb_pheno_path,
                                    gp_clinical_path,
                                    ukb_db_dir,
                                    allow_missing_fields = FALSE) {
  start_time <- proc.time()

  # get ukb.db
  message("Downloading ukb.db from ukbwranglr_resources")
  ukb_db_path <- get_ukb_db(ukb_db_dir)

  # collect ukb data dict and codings files from db
  message("Getting UKB data dict and codings")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ukb_db_path)
  ukb_db_tbl_list <- list_tbls_from_dbconn(con)

  ukb_data_dict <- ukb_db_tbl_list$ukb_data_dict %>% dplyr::collect()
  ukb_codings <- ukb_db_tbl_list$ukb_codings %>% dplyr::collect()

  DBI::dbDisconnect(con) # TODO amend `main_dataset_diagnoses_to_sqlite_db` to require con object as arg
  ukbwranglr:::time_taken_message(start_time)

  # make data dictionary and filter for required FieldIDs
  message("Creating data dictionary for UKB main dataset")
  data_dict <- make_data_dict(ukb_pheno_path,
                              delim = "\t",
                              ukb_data_dict = ukb_data_dict)
  ukbwranglr:::time_taken_message(start_time)

  # check that all required cols are present
  assertthat::assert_that("eid" %in% data_dict$FieldID,
                          msg = "Error! 'eid' column  is missing from the main UKB dataset")

  required_fields <- c(
    # self-reported, death certificate and HES diagnoses
    ukbwranglr:::DIAGNOSES_FIELD_IDS,
    # operations Field IDs
    # OPCS4
    "41272",
    "41282",
    # OPCS3
    "41273",
    "41283"
  )

  missing_fields <- subset(required_fields,
                           !(required_fields %in% data_dict$FieldID))

  if (!allow_missing_fields) {
    assertthat::assert_that(
      length(missing_fields) == 0,
      msg = paste0(
        "Error! Some required field IDs are missing from the main UKB dataset: ",
      stringr::str_c(missing_fields, sep = "", collapse = ", "))
    )
  } else if (allow_missing_fields) {
    if (length(missing_fields) > 0) {
      warning(
        paste0("Some required field IDs are missing from the main UKB dataset: ",
        stringr::str_c(missing_fields, sep = "", collapse = ", "))
      )
    }
  }

  # read selected diagnoses cols into R
  message("Reading diagnosis columns from UKB main dataset into R")
  ukb_pheno <- read_pheno(path = ukb_pheno_path,
                          data_dict = data_dict,
                          ukb_data_dict = ukb_data_dict,
                          ukb_codings = ukb_codings,
                          clean_dates = FALSE,
                          clean_selected_continuous_and_integers = FALSE)
  ukbwranglr:::time_taken_message(start_time)


  # create long format dataframe containing all diagnostic codes in main dataset

  message("Reshaping data on diagnosis into long format")
  list_of_get_diagnostic_codes_functions <-
    list(
      get_death_data_icd10_diagnoses,
      get_hes_icd9_diagnoses,
      get_hes_icd10_diagnoses,
      get_self_report_non_cancer_diagnoses,
      get_self_report_non_cancer_diagnoses_icd10,
      get_self_report_cancer_diagnoses,
      get_cancer_register_icd9_diagnoses,
      get_cancer_register_icd10_diagnoses,
      get_hes_opcs3_operations,
      get_hes_opcs4_operations
    )

  all_diagnostic_codes_in_main_ukb_dataset <-
    get_all_diagnostic_codes_multi(ukb_pheno = ukb_pheno,
                                   data_dict = data_dict,
                                   ukb_codings = ukb_codings,
                                   function_list = list_of_get_diagnostic_codes_functions)
  time_taken_message(start_time)

  # add diagnoses to database ---------------------------------------------------------------
  message("Writing long format diagnoses data from main UKB dataset to database")
  con <- main_dataset_diagnoses_to_sqlite_db(
    df = all_diagnostic_codes_in_main_ukb_dataset,
    db_path = ukb_db_path,
    table = "clinical_events"
  )

  time_taken_message(start_time)

  # now append (preprocessed) primary care data to 'clinical_events' table ---------------------------------------------------------------
  message("Appending UKB primary care clinical events data 'clinical events' table")
  file_to_sqlite_db(file = gp_clinical_path,
                    col_types = readr::cols(.default = "c"), # all cols as type character
                    db_path = ukb_db_path,
                    table = "clinical_events",
                    chunk_size = 500000,
                    delim = "\t",
                    append = TRUE, # set to TRUE if appending to an existing table
                    verbose = TRUE,
                    data_processing_function = gp_clinical_to_sqlite_db)
  time_taken_message(start_time)

  # set index on 'code'/'source'/'eid' columns for faster lookups
  message("Setting index on 'source' and 'code' columns in UKB database 'clinical_events' table")
  sql_index_source <- "CREATE INDEX idx_clinical_events_source ON clinical_events (source);"
  sql_index_code <- "CREATE INDEX idx_clinical_events_code ON clinical_events (code);"
  sql_index_eid <- "CREATE INDEX idx_clinical_events_eid ON clinical_events (eid);"
  DBI::dbSendQuery(con, statement = sql_index_source)
  DBI::dbSendQuery(con, statement = sql_index_code)
  DBI::dbSendQuery(con, statement = sql_index_eid)

  # close connection and completion message
  DBI::dbDisconnect(con)

  message("Success! UKB database setup complete")
  time_taken_message(start_time)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

