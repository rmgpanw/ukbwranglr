# NOTES -------------------------------------------------------------------

#TODO

# EXPORTED FUNCTIONS ----------------------------------------------------


#' Create a SQLite database with a \code{clinical_events} table
#'
#' Creates a SQLite database called \code{ukb.db} containing all tables from
#' \code{\link{get_ukb_db}}, plus an additional table called
#' \code{clinical_events}. This is a long format table combining all clinical
#' events as listed in \code{ukbwranglr:::CLINICAL_EVENTS_SOURCES}.
#'
#' @param ukb_main_path character. Path to the main UKB dataset file.
#' @param gp_clinical_path character. Path to the UKB primary care clinical
#'   events file (\code{gp_clinical.txt}).
#' @param ukb_db_dir character. Directory where \code{ukb.db} should be written
#'   to. An error is raised if a file called \code{ukb.db} already exists here.
#' @param strict logical. If TRUE, create database regardless of
#'   whether the main UKB dataset file is missing any required clinical events
#'   fields. Default is \code{FALSE}.
#'
#' @return NULL
#' @export
make_clinical_events_db <- function(ukb_main_path,
                                    ukb_main_delim,
                                    gp_clinical_path,
                                    ukb_db_dir,
                                    overwrite = FALSE,
                                    strict = FALSE) {
  start_time <- proc.time()

  # get ukb.db
  message("***DOWNLOADING UKB.DB FROM UKBWRANGLR_RESOURCES REPO***")
  ukb_db_path <- get_ukb_db(ukb_db_dir,
                            overwrite = overwrite)

  # collect ukb data dict and codings files from db
  message("***GETTING UKB DATA DICT AND CODINGS***")
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ukb_db_path)
  on.exit(DBI::dbDisconnect(con))
  ukb_db_tbl_list <- db_list_tables(con)

  ukb_data_dict <- ukb_db_tbl_list$ukb_data_dict %>%
    dplyr::collect()
  ukb_codings <- ukb_db_tbl_list$ukb_codings %>%
    dplyr::collect()

  ukbwranglr:::time_taken_message(start_time)

  # make data dictionary and filter for required FieldIDs
  message("***CREATING DATA DICTIONARY FOR UKB MAIN DATASET***")
  data_dict <- make_data_dict(ukb_main_path,
                              delim = ukb_main_delim,
                              ukb_data_dict = ukb_data_dict)
  ukbwranglr:::time_taken_message(start_time)

  # check that all required cols are present
  assertthat::assert_that("eid" %in% data_dict$FieldID,
                          msg = "Error! 'eid' column  is missing from the main UKB dataset")

  required_fields <- tidy_clinical_events(.details_only = TRUE) %>%
    purrr::pluck("required_field_ids") %>%
    purrr::flatten() %>%
    as.character() %>%
    unique()

  missing_fields <- subset(required_fields,
                           !(required_fields %in% data_dict$FieldID))

  if (!strict) {
    assertthat::assert_that(
      length(missing_fields) == 0,
      msg = paste0(
        "Error! Some required field IDs are missing from the main UKB dataset: ",
      stringr::str_c(missing_fields, sep = "", collapse = ", "))
    )
  } else if (strict) {
    if (length(missing_fields) > 0) {
      warning(
        paste0("Some required field IDs are missing from the main UKB dataset: ",
        stringr::str_c(missing_fields, sep = "", collapse = ", "))
      )

      data_dict <- data_dict %>%
        dplyr::filter(.data[["FieldID"]] %in% c("eid", required_fields))
    }
  }

  # read and tidy ukb_main clinical events ----------------------------------


  # read selected clinical events cols into R
  message("***READING DIAGNOSIS COLUMNS FROM UKB MAIN DATASET INTO R***")
  ukb_main <- read_ukb(
    path = ukb_main_path,
    delim = ukb_main_delim,
    data_dict = data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    descriptive_colnames = TRUE
  )
  ukbwranglr:::time_taken_message(start_time)

  # create long format data frame containing all clinical events codes in main dataset
  message("***TIDYING CLINICAL EVENTS DATA***")
  ukb_main <-
    tidy_clinical_events(
      ukb_main = ukb_main,
      ukb_data_dict = ukb_data_dict,
      ukb_codings = ukb_codings,
      strict = strict
    ) %>%
    dplyr::bind_rows()

  time_taken_message(start_time)

  # add ukb_main clinical events to database ---------------------------------------------------------------
  message("***WRITING CLINICAL EVENTS FROM MAIN UKB DATASET TO `clinical_events` TABLE IN DATABASE***")
  DBI::dbWriteTable(
    conn = con,
    name = "clinical_events",
    value = ukb_main,
    overwrite = overwrite,
    append = FALSE
  )

  time_taken_message(start_time)

  # now append (preprocessed) primary care data to 'clinical_events' table, adding value column to a separate `gp_clinical_values` table ---------------------------------------------------------------
  message("***APPENDING UKB PRIMARY CARE CLINICAL EVENTS DATA TO 'clinical_events' TABLE AND WRITING VALUE COLUMNS TO `gp_clinical_values` TABLE***")
  file_to_sqlite_db(file = gp_clinical_path,
                    col_types = list(
                      eid = "i",
                      data_provider = "c",
                      event_dt = "c",
                      read_2 = "c",
                      read_3 = "c",
                      value1 = "c",
                      value2 = "c",
                      value3 = "c"
                    ), # all cols as type character
                    db_path = ukb_db_path,
                    chunk_size = 500000,
                    delim = "\t",
                    append = TRUE, # set to `TRUE` as appending to an existing table
                    verbose = TRUE,
                    callback_function = tidy_gp_clinical)

  time_taken_message(start_time)

  # set index on 'code'/'source'/'eid' columns for faster lookups
  message("***SETTING INDEX ON `source`, `code` AND `eid` COLUMNS IN UKB DATABASE 'clinical_events' TABLE***")
  sql_index_source <- "CREATE INDEX idx_clinical_events_source ON clinical_events (source);"
  sql_index_code <- "CREATE INDEX idx_clinical_events_code ON clinical_events (code);"
  sql_index_eid <- "CREATE INDEX idx_clinical_events_eid ON clinical_events (eid);"
  DBI::dbSendQuery(con, statement = sql_index_source)
  DBI::dbSendQuery(con, statement = sql_index_code)
  DBI::dbSendQuery(con, statement = sql_index_eid)

  # completion message
  message("SUCCESS! UKB DATABASE SETUP COMPLETE")
  time_taken_message(start_time)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

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
#' @param chunk_size The number of rows to include in each chunk. Default is
#'   10000.
#' @param col_types Default is for all columns to be type character.
#' @param verbose Print time taken after each chunk has been written to
#'   database.
#' @param append Append to table if already exists. Default is \code{FALSE}.
#' @param callback_function A function to be applied to each chunk before
#'   writing to database. This must output a named list of data frames. Each
#'   item will be written to a table with the same name. It should also have a
#'   \code{.details_only} argument and return a character vector of table names
#'   if this is \code{TRUE}. This is used to check whether these tables already
#'   exist in the database before attempting to read \code{file}.
#' @inheritParams readr::read_delim_chunked
#' @param ... additional parameters passed on to \code{callback_function}.
#'
#' @return Returns \code{NULL} invisibly.
#'
#' @noRd
file_to_sqlite_db <- function(file,
                              db_path = "ukb.db",
                              chunk_size = 10000,
                              delim = "\t",
                              col_types = readr::cols(.default = "c"),
                              # indexes = NULL,
                              verbose = TRUE,
                              append = FALSE,
                              callback_function = function(x, ...) list("table" = x),
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
  on.exit(DBI::dbDisconnect(con))

  # Error message if table already exists and append == FALSE
  tables_to_write <- callback_function(.details_only = TRUE)
  table_already_present_in_db <- subset(tables_to_write,
                                        tables_to_write %in% DBI::dbListTables(con))

  if (!rlang::is_empty(table_already_present_in_db) &
      !append) {
    stop(
      paste0(
        "Error! The following table(s) already exists in database: ,",
        stringr::str_c(
          table_already_present_in_db,
          sep = "",
          collapse = ", "
        ),
        " Specify `append = TRUE` to append data. To overwrite, first delete table with `DBI::dbRemoveTable(conn, 'table_name')`"
      )
    )
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

    # callback_function must output a named list of data frames
    x <- callback_function(x, ...)

    # loop through items in x
    for (table_name in names(x)) {
      DBI::dbWriteTable(
        conn = con,
        name = table_name,
        value = x[[table_name]],
        overwrite = FALSE,
        # ensure table is not inadvertently overwritten
        append = TRUE # needs to be TRUE, otherwise cannot write in chunks with read_delim_chunked()
        # field.types = field.types
      )
    }
  }

  # Read file and write to table in chunks
  message("Writing file to table")
  readr::read_delim_chunked(
    file = file,
    callback = readr::SideEffectChunkCallback$new(f),
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

  # return NULL invisibly
  invisible(NULL)
}

#' Tidy UK Biobank primary care clinical events
#'
#' Reformats the UK Biobank primary care clinical events dataset to match the
#' output format for \code{\link{tidy_clinical_events}}.
#'
#' The UK Biobank primary care clinical events data lists read codes in separate
#' columns, one for Read2 and one for Read3. This function reshapes the data to
#' long format so that all codes are in a single column. The \code{index} column
#' values relate to row numbers in the original data.
#'
#' The primary care data also contains 3 'value' columns. The clinical
#' codes/dates in long format and 'value' columns are both returned in a list
#' under the names 'clinical_events' and 'gp_clinical_values' respectively.
#'
#' @section: Other notes
#'
#' By default, special date values (see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource 591} for
#' further details) are set to \code{NA}.
#'
#' @param gp_clinical A data frame
#' @param remove_special_dates Logical. Removes special date values if
#'   requested. Default value is \code{TRUE}.
#'
#' @noRd
#' @return A named list. Item 'clinical_events' contains the read codes with
#'   event dates, and item 'gp_clinical_values' contains the 'value' columns.
#' @seealso tidy_clinical_events
tidy_gp_clinical <- function(gp_clinical,
                                      remove_special_dates = TRUE,
                                      .details_only = FALSE) {

  # names of table to be returned
  output_table_names <- c("clinical_events",
                          "gp_clinical_values")

  if (.details_only) {
    return(output_table_names)
  }

  # validate args
  assertthat::assert_that(all(
    names(gp_clinical) == c(
      "eid",
      "data_provider",
      "event_dt",
      "read_2",
      "read_3",
      "value1",
      "value2",
      "value3"
    )
  ),
  msg = "Error! `gp_clinical` has unexpected column names")

  assertthat::assert_that(all(
    as.character(purrr::map_chr(dummy_gp_clinical, class)) == c(
      "numeric",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character"
    )
  ),
  msg = "Error! `gp_clinical` has one or more columns of invalid type. Column `eid` should be type 'numeric' and all other columns should be type 'character'")

  # add index col - 'pos' will be found in the `file_to_sqlite_db environment`
  index_col_end <- pos + nrow(gp_clinical) - 1
  gp_clinical$index <- as.character(pos:index_col_end)

  # tidy clinical codes/dates
  gp_clinical_codes <- gp_clinical %>%
    dplyr::select(
      .data[["eid"]],
      .data[["index"]],
      .data[["data_provider"]],
      .data[["event_dt"]],
      .data[["read_2"]],
      .data[["read_3"]]
    ) %>% # remove 3 'value' cols
    tidyr::pivot_longer(
      cols = c("read_2", "read_3"),
      names_to = "source",
      values_to = "code"
    ) %>%
    # remove redundant rows (original data has no rows with a value in bot read2/3 cols)
    dplyr::filter(!is.na(.data[["code"]]))

  # relabel 'read_2' and 'read_3' to 'gpc_r2' and 'gpc_r3'
  gp_clinical_codes$source <- dplyr::case_when(
    gp_clinical_codes$source == "read_2" ~ paste0("gpc",
                                                  gp_clinical_codes[["data_provider"]],
                                                  "_",
                                                  "r2"),
    gp_clinical_codes$source == "read_3" ~ paste0("gpc",
                                                  gp_clinical_codes[["data_provider"]],
                                                  "_",
                                                  "r3"),
    TRUE ~ "gpc_unknown_coding"
  )

  # rename 'event_dt' to 'date'
  gp_clinical_codes <- rename_cols(gp_clinical_codes,
                    old_colnames = "event_dt",
                    new_colnames = "date")

  # reformat date
  gp_clinical_codes$date <- gp_clinical_codes$date %>%
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

    gp_clinical_codes$date <- ifelse(
      test = gp_clinical_codes$date %in% primary_care_special_dates_to_remove,
      yes = NA,
      no = gp_clinical_codes$date
    )
  }

  result <- list(clinical_events = gp_clinical_codes[c("eid",
                                                       "source",
                                                       "index",
                                                       "code",
                                                       "date")],
                 gp_clinical_values = gp_clinical[c("index",
                                                    "value1",
                                                    "value2",
                                                    "value3")])

  # check that items in `result` match those returned when `.details_only = TRUE`
  assertthat::assert_that(all(names(result) == output_table_names))

  return(result)
}
