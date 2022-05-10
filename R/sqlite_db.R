# NOTES -------------------------------------------------------------------

#TODO

# EXPORTED FUNCTIONS ----------------------------------------------------


#' Create a SQLite database with a `clinical_events` table
#'
#' Adds tables named `clinical_events`, and optionally 'gp_clinical_values' and
#' 'gp_scripts_names_and_quantities' to a SQLite database file (the latter 2 are
#' only added if `gp_clinical_path` and/or `gp_scripts_path` respectively are
#' provided). This is a long format table combining all clinical events data
#' from a UK Biobank main dataset and the UK Biobank primary care clinical
#' events dataset. Use [`clinical_events_sources()`] to see a list of all
#' currently included clinical events sources.
#'
#' See the [introduction to
#' dbplyr](https://dbplyr.tidyverse.org/articles/dbplyr.html) vignette for
#' getting started with databases and [dplyr::dplyr].
#'
#' Indexes are set on the `source`, `code` and `eid` columns in the
#' `clinical_events` table for faster querying.
#'
#' @param ukb_main_path Path to the main UKB dataset file.
#' @param ukb_db_path Path to the SQLite database file. The file name must end
#'   with '.db'. If no file with this name exists then one will be created.
#' @param ukb_main_delim Delimiter for `ukb_main_path`. Default value is
#'   `"auto"`.
#' @param gp_clinical_path (Optional) path to the UKB primary care clinical
#'   events file (`gp_clinical.txt`).
#' @param gp_scripts_path (Optional) path to the UKB primary care prescriptions
#'   file (`gp_scripts.txt`).
#' @param overwrite If `TRUE`, then tables `clinical_events` and
#'   `gp_clinical_values` will be overwritten if they already exist in the
#'   database. Default value is `FALSE`.
#' @param chunk_size The number of rows to include in each chunk when processing
#'   primary care datasets.
#' @inheritParams tidy_clinical_events
#'
#' @return Returns `ukb_db_path` invisibly.
#' @export
#' @family clinical events
#' @examples
#' # dummy UKB data dictionary and codings
#' dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
#' dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
#'
#' # file paths to dummy UKB main and primary care datasets
#' dummy_ukb_main_path <- get_ukb_dummy(
#'   "dummy_ukb_main.tsv",
#'   path_only = TRUE
#' )
#'
#' dummy_gp_clinical_path <- get_ukb_dummy(
#'   "dummy_gp_clinical.txt",
#'   path_only = TRUE
#' )
#'
#' dummy_gp_scripts_path <- get_ukb_dummy(
#'   "dummy_gp_scripts.txt",
#'   path_only = TRUE
#' )
#'
#' # file path where SQLite database will be created
#' dummy_ukb_db_path <- file.path(tempdir(), "ukb.db")
#'
#' # build database
#' suppressWarnings(make_clinical_events_db(
#'   ukb_main_path = dummy_ukb_main_path,
#'   gp_clinical_path = dummy_gp_clinical_path,
#'   gp_scripts_path = dummy_gp_scripts_path,
#'   ukb_db_path = dummy_ukb_db_path,
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   ukb_codings = dummy_ukb_codings,
#' ))
#'
#' # connect to database
#' con <- DBI::dbConnect(
#'   RSQLite::SQLite(),
#'   dummy_ukb_db_path
#' )
#'
#' ukbdb <- db_tables_to_list(con)
#'
#' # table names
#' names(ukbdb)
#'
#' # view tables
#' ukbdb$clinical_events
#'
#' ukbdb$gp_clinical_values
#'
#' ukbdb$gp_scripts_names_and_quantities
#'
#' # close database connection
#' DBI::dbDisconnect(con)
make_clinical_events_db <- function(ukb_main_path,
                                    ukb_db_path,
                                    ukb_main_delim = "auto",
                                    gp_clinical_path = NULL,
                                    gp_scripts_path = NULL,
                                    ukb_data_dict = get_ukb_data_dict(),
                                    ukb_codings = get_ukb_codings(),
                                    overwrite = FALSE,
                                    chunk_size = 500000) {
  start_time <- proc.time()

  # validate args
  assertthat::assert_that(!rlang::is_missing(ukb_db_path),
                          msg = "Error! argument 'ukb_db_path' is missing, with no default")
  assertthat::assert_that(!rlang::is_missing(ukb_main_path),
                          msg = "Error! argument 'ukb_main_path' is missing, with no default")

  ukb_db_ext <- extract_file_ext(ukb_db_path)
  if (!(ukb_db_ext == "db") | is.na(ukb_db_ext)) {
    stop("Error! The file name for `ukb_db_path` must end with '.db'")
  }

  # connect to ukbdb
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ukb_db_path)
  on.exit(DBI::dbDisconnect(con))

  # Error message if table already exists and append == FALSE
  tables_to_write <- c(
    tidy_gp_data_db(gp_df_type = "gp_clinical",
                    .details_only = TRUE),
    tidy_gp_data_db(gp_df_type = "gp_scripts",
                    .details_only = TRUE),
    "clinical_events"
  )

  table_already_present_in_db <- subset(tables_to_write,
                                        tables_to_write %in% DBI::dbListTables(con))

  if (!rlang::is_empty(table_already_present_in_db) &
      !overwrite) {
    stop(
      paste0(
        "Error! The following table(s) already exists in database: ",
        stringr::str_c(
          table_already_present_in_db,
          sep = "",
          collapse = ", "
        ),
        " Specify `overwrite = TRUE` to overwrite these."
      )
    )
  }

  # Main dataset -----

  # make data dictionary and filter for required FieldIDs
  message("***CREATING DATA DICTIONARY FOR UKB MAIN DATASET***")
  data_dict <- make_data_dict(ukb_main_path,
                              delim = ukb_main_delim,
                              ukb_data_dict = ukb_data_dict)

  # check that eid col is present
  assertthat::assert_that("eid" %in% data_dict$FieldID,
                          msg = "Error! 'eid' column  is missing from the main UKB dataset")

  # check at least some clinical events fields are present
  available_clinical_events_fields <-
    tidy_clinical_events(.details_only = TRUE) %>%
    purrr::pluck("required_field_ids") %>%
    purrr::flatten() %>%
    as.character() %>%
    unique()

  # error if no clinical events fields present
  if (length(available_clinical_events_fields) == 0) {
    stop("No clinical events fields identified in main UKB dataset. Use `ukbwranglr::clinical_events_sources()` for a list of valid clinical events Field IDs.")
  }

  # warning if any missing clinical events fields
  missing_clinical_events_fields <-
    subset(available_clinical_events_fields, !(available_clinical_events_fields %in% data_dict$FieldID))

    if (length(missing_clinical_events_fields) > 0) {
      warning(
        paste0(
          "The following clinical events field IDs are missing from the main UKB dataset: ",
          stringr::str_c(missing_clinical_events_fields, sep = "", collapse = ", ")
        )
      )
    }

  # filter `data_dict` for 'eid' and clinical events fields only
  data_dict <- data_dict %>%
        dplyr::filter(.data[["FieldID"]] %in% c("eid", available_clinical_events_fields))

  # tidy ukb_main clinical events ----------------------------------

  # read selected clinical events cols into R
  message("***READING DIAGNOSIS COLUMNS FROM UKB MAIN DATASET INTO R***")
  ukb_main <- read_ukb(
    path = ukb_main_path,
    delim = ukb_main_delim,
    data_dict = data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    descriptive_colnames = TRUE,
    label = FALSE
  )

  # create long format data frame containing all clinical events codes in main dataset
  message("***TIDYING CLINICAL EVENTS DATA***")
  ukb_main <-
    tidy_clinical_events(
      ukb_main = ukb_main,
      ukb_data_dict = ukb_data_dict,
      ukb_codings = ukb_codings,
      strict = FALSE
    ) %>%
    dplyr::bind_rows()

  # add ukb_main clinical events to database ---------------------------------------------------------------
  message(
    "***WRITING CLINICAL EVENTS FROM MAIN UKB DATASET TO `clinical_events` TABLE IN DATABASE***"
  )
  DBI::dbWriteTable(
    conn = con,
    name = "clinical_events",
    value = ukb_main,
    overwrite = overwrite,
    append = FALSE
  )

  # append primary care data codes/dates to 'clinical_events' table, adding other columns to separate tables ---------------------------------------------------------------

  if (!is.null(gp_clinical_path)) {
    message(
      "***APPENDING UKB PRIMARY CARE CLINICAL EVENTS DATA TO 'clinical_events' TABLE AND WRITING VALUE COLUMNS TO `gp_clinical_values` TABLE***"
    )

    if (overwrite &
        ("gp_clinical_values" %in% DBI::dbListTables(con))) {
      DBI::dbRemoveTable(con, "gp_clinical_values")
    }

    file_to_sqlite_db(
      file = gp_clinical_path,
      col_types = list(
        eid = "i",
        data_provider = "c",
        event_dt = "c",
        read_2 = "c",
        read_3 = "c",
        value1 = "c",
        value2 = "c",
        value3 = "c"
      ),
      db_path = ukb_db_path,
      chunk_size = chunk_size,
      delim = "\t",
      append = TRUE,
      # set to `TRUE` as appending to an existing table
      verbose = TRUE,
      callback_function = purrr::partial(tidy_gp_data_db,
                                         gp_df_type = "gp_clinical")
    )
  }

  if (!is.null(gp_scripts_path)) {
    message(
      "***APPENDING UKB PRIMARY CARE PRESCRIPTION DATA TO 'clinical_events' TABLE AND WRITING DRUG NAME AND QUANTITY COLUMNS TO `gp_scripts_names_and_quantities` TABLE***"
    )

    if (overwrite &
        ("gp_scripts_names_and_quantities" %in% DBI::dbListTables(con))) {
      DBI::dbRemoveTable(con, "gp_scripts_names_and_quantities")
    }

    file_to_sqlite_db(
      file = gp_scripts_path,
      col_types = list(
        eid = "i",
        data_provider = "c",
        issue_date = "c",
        read_2 = "c",
        bnf_code = "c",
        dmd_code = "c",
        drug_name = "c",
        quantity = "c"
      ),
      db_path = ukb_db_path,
      chunk_size = chunk_size,
      delim = "\t",
      append = TRUE,
      # set to `TRUE` as appending to an existing table
      verbose = TRUE,
      callback_function = purrr::partial(tidy_gp_data_db,
                                         gp_df_type = "gp_scripts")
    )
  }

  # set sql indexes ---------------------------------------------------------


  # set index on 'code'/'source'/'eid' columns for faster lookups
  message(
    "***SETTING INDEX ON `source`, `code` AND `eid` COLUMNS IN UKB DATABASE 'clinical_events' TABLE***"
  )
  sql_index_source <-
    "CREATE INDEX idx_clinical_events_source ON clinical_events (source);"
  sql_index_code <-
    "CREATE INDEX idx_clinical_events_code ON clinical_events (code);"
  sql_index_eid <-
    "CREATE INDEX idx_clinical_events_eid ON clinical_events (eid);"
  DBI::dbSendQuery(con, statement = sql_index_source)
  DBI::dbSendQuery(con, statement = sql_index_code)
  DBI::dbSendQuery(con, statement = sql_index_eid)

  # completion message
  message("SUCCESS! UKB DATABASE SETUP COMPLETE")
  message(paste0("To connect to db: `con <- DBI::dbConnect(RSQLite::SQLite(), '", ukb_db_path, "')`, then `ukbdb <- ukbwranglr::db_tables_to_list(con)`"))
  time_taken_message(start_time)

  invisible(ukb_db_path)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

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
#' @section Other notes:
#'
#'   By default, special date values (see
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource 591} for
#'   further details) are set to \code{NA}.
#'
#' @param gp_clinical The UK Biobank primary care clinical events dataset
#' @param remove_special_dates Logical. Removes special date values if
#'   requested. Default value is \code{TRUE}.
#' @param .details_only logical. If \code{TRUE}, return a character vector of
#'   output table names only
#'
#' @keywords internal
#' @return A named list. Item 'clinical_events' contains the read codes with
#'   event dates, and item 'gp_clinical_values' contains the 'value' columns.
#' @seealso \code{\link{tidy_clinical_events}},
#'   \code{\link{make_clinical_events_db}}
tidy_gp_clinical <- function(gp_clinical,
                             remove_special_dates = TRUE,
                             .details_only = FALSE) {
  tidy_gp_data_db(
    gp_df = gp_clinical,
    gp_df_type = "gp_clinical",
    pos = NULL,
    remove_special_dates = remove_special_dates,
    .details_only = .details_only
  )
}

#' Tidy UK Biobank primary care prescriptions data
#'
#' Reformats the UK Biobank primary care prescriptions dataset to match the
#' output format for \code{\link{tidy_clinical_events}}.
#'
#' The UK Biobank primary care prescriptions data has multiple code columns
#' (Read, BNF and DMD codes). This function reshapes the data to long format so
#' that all codes are in a single column. The \code{index} column values relate
#' to row numbers in the original data.
#'
#' The primary care data also contains drug name and quantities columns. The
#' clinical codes/dates in long format and drug name/quantity columns are both
#' returned in a list under the names 'clinical_events' and
#' 'gp_scripts_names_and_quantities' respectively.
#'
#' @section Other notes:
#'
#'   By default, special date values (see
#'   \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource 591} for
#'   further details) are set to \code{NA}.
#'
#' @param gp_scripts The UK Biobank primary care prescriptions dataset
#' @param remove_special_dates Logical. Removes special date values if
#'   requested. Default value is \code{TRUE}.
#' @param .details_only logical. If \code{TRUE}, return a character vector of
#'   output table names only
#'
#' @keywords internal
#' @return A named list. Item 'clinical_events' contains the read codes with
#'   event dates, and item 'gp_scripts_names_and_quantities' contains the drug
#'   names/quantities columns.
#' @seealso \code{\link{tidy_clinical_events}},
#'   \code{\link{make_clinical_events_db}}
tidy_gp_scripts <- function(gp_scripts,
                            remove_special_dates = TRUE,
                            .details_only = FALSE) {
  tidy_gp_data_db(
    gp_df = gp_scripts,
    gp_df_type = "gp_scripts",
    pos = NULL,
    remove_special_dates = remove_special_dates,
    .details_only = .details_only
  )
}

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
#'   item will be written to a table with the same name. It should have a
#'   \code{.details_only} argument and return a character vector of table names
#'   if this is \code{TRUE}. This is used to check whether these tables already
#'   exist in the database before attempting to read \code{file}. It should also
#'   have a \code{pos} argument, which is used to add a column of row numbers.
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
                              callback_function = function(x, ...)
                                list("table" = x),
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
    x <- callback_function(x, pos, ...)

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

tidy_gp_data_db <- function(gp_df,
                            gp_df_type,
                            pos,
                            remove_special_dates = TRUE,
                            .details_only = FALSE) {
  # see documentation for `tidy_gp_clinical`/`tidy_gp_scripts` the `pos`
  # argument is required for use with `file_to_sqlite_db` - adds the row number
  # as an 'index' column

  # ***Note: `pos` must be an unnamed argument - when setting `pos = NULL`,
  # tests pass locally but not with github actions***

  match.arg(gp_df_type,
            choices = c("gp_clinical", "gp_scripts"))

  gp_df_details <- switch(
    gp_df_type,
    gp_clinical = list(
      output_table_names = c("clinical_events",
                             "gp_clinical_values"),
      input_col_names = c(
        "eid",
        "data_provider",
        "event_dt",
        "read_2",
        "read_3",
        "value1",
        "value2",
        "value3"
      ),
      clinical_events_cols = c(
        "eid",
        "index",
        "data_provider",
        "event_dt",
        "read_2",
        "read_3"
      ),
      code_cols = c("read_2",
                    "read_3"),
      date_col = "event_dt",
      other_cols = c("value1",
                     "value2",
                     "value3"),
      source_acronym = "gpc"
    ),
    gp_scripts = list(
      output_table_names = c("clinical_events",
                             "gp_scripts_names_and_quantities"),
      input_col_names = c(
        "eid",
        "data_provider",
        "issue_date",
        "read_2",
        "bnf_code",
        "dmd_code",
        "drug_name",
        "quantity"
      ),
      clinical_events_cols = c(
        "eid",
        "index",
        "data_provider",
        "issue_date",
        "read_2",
        "bnf_code",
        "dmd_code"
      ),
      code_cols = c("read_2",
                    "bnf_code",
                    "dmd_code"),
      date_col = "issue_date",
      other_cols = c("drug_name",
                     "quantity"),
      source_acronym = "gps"
    ),
  )

  if (.details_only) {
    # names of table to be returned
    return(gp_df_details$output_table_names)
  }

  # validate args
  assertthat::assert_that(
    all(names(gp_df) == gp_df_details$input_col_names),
    msg = paste0("Error! `",
                 gp_df_type,
                 "` has unexpected column names")
  )

  assertthat::assert_that(
    all(as.character(purrr::map_chr(gp_df[, 2:ncol(gp_df)], class)) == rep("character", ncol(gp_df) - 1)) &
      is.numeric(gp_df$eid),
    msg = paste0(
      "Error! `",
      gp_df_type,
      "` has one or more columns of invalid type. Column `eid` should be type 'integer' and all other columns should be type 'character'"
    )
  )

  # add index col - 'pos' is required for `file_to_sqlite_db environment`
  if (is.null(pos)) {
    pos <- 1
  }

  index_col_end <- pos + nrow(gp_df) - 1
  gp_df$index <- as.character(pos:index_col_end)

  # tidy clinical codes/dates
  gp_df_codes <- gp_df %>%
    dplyr::select(tidyselect::all_of(gp_df_details$clinical_events_cols)) %>%
    tidyr::pivot_longer(cols = gp_df_details$code_cols,
                        names_to = "source",
                        values_to = "code") %>%
    # remove redundant rows
    dplyr::filter(!is.na(.data[["code"]]))

  # relabel `source` col - combine 'gpc'/'gps' with data provider and code type
  if (gp_df_type == "gp_clinical") {
    gp_df_codes$source <- dplyr::case_when(
      gp_df_codes$source == "read_2" ~ paste0(gp_df_details$source_acronym,
                                              gp_df_codes[["data_provider"]],
                                              "_",
                                              "r2"),
      gp_df_codes$source == "read_3" ~ paste0(gp_df_details$source_acronym,
                                              gp_df_codes[["data_provider"]],
                                              "_",
                                              "r3"),
      TRUE ~ "gpc_unknown_coding"
    )
  } else if (gp_df_type == "gp_scripts") {
    gp_df_codes$source <- dplyr::case_when(
      gp_df_codes$source == "read_2" ~ paste0(gp_df_details$source_acronym,
                                              gp_df_codes[["data_provider"]],
                                              "_",
                                              "r2"),
      gp_df_codes$source == "bnf_code" ~ paste0(gp_df_details$source_acronym,
                                                gp_df_codes[["data_provider"]],
                                                "_",
                                                "bnf"),
      gp_df_codes$source == "dmd_code" ~ paste0(gp_df_details$source_acronym,
                                                gp_df_codes[["data_provider"]],
                                                "_",
                                                "dmd"),
      TRUE ~ "gpc_unknown_coding"
    )
  }

  # rename 'event_dt'/`issue_date` to 'date'
  gp_df_codes <- rename_cols(gp_df_codes,
                             old_colnames = gp_df_details$date_col,
                             new_colnames = "date")

  # reformat date
  gp_df_codes$date <- gp_df_codes$date %>%
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

    gp_df_codes$date <- ifelse(
      test = gp_df_codes$date %in% primary_care_special_dates_to_remove,
      yes = NA,
      no = gp_df_codes$date
    )
  }

  result <- list(
    events = gp_df_codes %>%
      dplyr::select(tidyselect::all_of(
        c("eid",
          "source",
          "index",
          "code",
          "date")
      )),
    other = gp_df %>%
      dplyr::select(tidyselect::all_of(c(
        "index",
        gp_df_details$other_cols
      )))
  )

  names(result) <- gp_df_details$output_table_names

  return(result)
}
