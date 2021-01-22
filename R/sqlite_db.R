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
#' @param file Character. The path to the file to be written to `db_path`.
#' @param db_path Character. Path to a SQLite database. A new database will be
#'   created if this does not already exist. Default is "ukb.db".
#' @param table Character. Name of table to write to.
#' @param chunk_size The number of rows to include in each chunk. Default is
#'   10000.
#' @param col_types Default is for all columns to be type character.
#' @param verbose Print time taken after each chunk has been written to
#'   database.
#' @param append Append to table if already exists. Default is `FALSE`.
#' @param data_processing_function A function to be applied to each chunk before
#'   writing to database. Default is `NULL`.
#' @inheritParams readr::read_delim_chunked
#' @param ... additional parameters passed on to `data_processing_function`.
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
    stop("Error! Table already exist in database. Specify `append = TRUE` to append data. To overwrite, first delete table with `DBI::dbRemoveTable(conn, 'table_name')`")
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
#'   columns are dropped.
#'
#' @param gp_clinical_df A dataframe
#'
#' @return
#' @export
#' @family Generate a UKB database.
gp_clinical_to_sqlite_db <- function(df) {
  df %>%
    dplyr::select(-value1,-value2,-value3) %>% # remove 3 'value' cols
    tidyr::pivot_longer(
      cols = c("read_2", "read_3"),
      names_to = "source",
      values_to = "code"
    ) %>%
    dplyr::filter(!is.na(code)) %>% # remove redundant rows (original data has no rows with a value in bot read2/3 cols)
    dplyr::select(eid,
                  source,
                  code,
                  date = event_dt) %>%
    dplyr::mutate(date = lubridate::dmy(date)) %>% # reformat date (needs to be character when writing to db)
    dplyr::mutate(date = as.character(date))
}
# PRIVATE FUNCTIONS -------------------------------------------------------
