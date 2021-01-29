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
#'   columns are dropped. By default, special date values are set to \code{NA}.
#'
#' @param gp_clinical_df A dataframe
#' @param remove_special_dates Logical. Removes special date values if requested
#'   (see \href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=591}{resource
#'   591} for details). Default is \code{TRUE}.
#'
#' @return
#' @export
#' @family Generate a UKB database.
gp_clinical_to_sqlite_db <- function(df, remove_special_dates = TRUE) {
  df <- df %>%
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

    df <- df %>%
      dplyr::mutate(date = ifelse(
        test = date %in% primary_care_special_dates_to_remove,
        yes = NA,
        no = date))
  }

  return(df)
}

#' Write diagnoses from the main UK Biobank dataset to a SQLite database
#'
#' If no database exists at \code{db_path}, then a new one will be created..
#'
#' @inheritParams file_to_sqlite_db
#' @inheritParams get_first_diagnostic_code_record_basis
#' @param overwrite Logical. If \code{TRUE}, will overwrite an existing table
#'   with the same name. Default is \code{FALSE}
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
  df <- df %>%
    dplyr::mutate(date = as.character(date))

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
}
# PRIVATE FUNCTIONS -------------------------------------------------------
