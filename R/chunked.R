# OVERVIEW ----------------------------------------------------------------

# Functions to read/process data in chunks. Useful for large datasets which may
# not fit into memory


# OTHER NOTES -------------------------------------------------------------

# Read in chunks:
# See this [stackoverflow post](https://stackoverflow.com/a/60085589)
# Get an error message if you try skipping too many lines (i.e. when end of file is reached)

# TODO --------------------------------------------------------------------

# add check for fread_chunked and process_df_chunked: callback function should
# return a df if returning result

# EXPORTED FUNCTIONS ------------------------------------------------------

#' Read and (optionally) process a file in chunks
#'
#' Works like \code{\link[readr]{read_delim_chunked}}, but is built on
#' \code{\link[data.table]{fread}}. An advantage is that it is simpler to
#' programmatically read a selection of columns.
#'
#' @param return_chunks bool. Save chunks and return as a single combined data
#'   frame when function completes. Default is \code{FALSE}.
#' @param progress bool. Display a progress message stating time taken after
#'   each chunk is processed.
#' @inheritParams data.table::fread
#' @inheritParams readr::read_delim_chunked
#' @param ... additional arguments passed on to \code{callback} function.
#'
#' @export
fread_chunked <- function(file,
                          callback,
                          chunk_size = 10000,
                          progress = TRUE,
                          return_chunks = FALSE,
                          cmd = NULL,
                          sep = "auto",
                          sep2 = "auto",
                          dec = ".",
                          quote = "\"",
                          na.strings = getOption("datatable.na.strings", "NA"),
                          stringsAsFactors = FALSE,
                          select = NULL,
                          drop = NULL,
                          colClasses = c("character"),
                          integer64 = getOption("datatable.integer64", "integer64"),
                          col.names,
                          check.names = FALSE,
                          encoding = "unknown",
                          strip.white = TRUE,
                          fill = FALSE,
                          blank.lines.skip = FALSE,
                          key = NULL,
                          index = NULL,
                          showProgress = getOption("datatable.showProgress", interactive()),
                          data.table = getOption("datatable.fread.datatable", TRUE),
                          # nThread = data.table::getDTthreads(verbose),
                          logical01 = getOption("datatable.logical01", FALSE),
                          keepLeadingZeros = getOption("datatable.keepLeadingZeros", FALSE),
                          yaml = FALSE,
                          tmpdir = tempdir(),
                          tz = "",
                          ...) {
  # start timer
  start_time <- proc.time()

  # stop if chunk_size is not an integer or less than 1
  assert_integer_ge_1(x = chunk_size,
                      arg_name = "chunk_size")

  # initialise 'data' - will contain combined chunks if return_chunks == TRUE
  if (return_chunks) {
    data <- NULL
  }

  # get column names
  column_names <- names(
    data.table::fread(
      file = file,
      nrow = 0,
      cmd = cmd,
      sep = sep,
      sep2 = sep2,
      dec = dec,
      quote = quote,
      header = TRUE,
      na.strings = na.strings,
      stringsAsFactors = stringsAsFactors,
      drop = drop,
      colClasses = colClasses,
      integer64 = integer64,
      check.names = check.names,
      encoding = encoding,
      strip.white = strip.white,
      fill = fill,
      blank.lines.skip = blank.lines.skip,
      key = key,
      index = index,
      showProgress = showProgress,
      data.table = data.table,
      # nThread = nThread,
      logical01 = logical01,
      keepLeadingZeros = keepLeadingZeros,
      yaml = yaml,
      tmpdir = tmpdir,
      tz = tz
    )
  )

  # infinite loop - start reading file from first line. stop when last line
  # reached (this raises an error, which is handled to exit the loop)
  message("Reading file in chunks")
  i = 0
  while (TRUE) {
    # read chunk - return NULL if error raised from reaching end of file
    chunk <- tryCatch(
      data.table::fread(
        file = file,
        nrow = chunk_size,
        skip = chunk_size * i,
        col.names = column_names,
        cmd = cmd,
        sep = sep,
        sep2 = sep2,
        dec = dec,
        quote = quote,
        header = TRUE,
        # must be TRUE
        na.strings = na.strings,
        stringsAsFactors = stringsAsFactors,
        drop = drop,
        colClasses = colClasses,
        integer64 = integer64,
        check.names = check.names,
        encoding = encoding,
        strip.white = strip.white,
        fill = fill,
        blank.lines.skip = blank.lines.skip,
        key = key,
        index = index,
        showProgress = showProgress,
        data.table = data.table,
        # nThread = nThread,
        logical01 = logical01,
        keepLeadingZeros = keepLeadingZeros,
        yaml = yaml,
        tmpdir = tmpdir,
        tz = tz
      ),

      # return NULL if end of file reached. If any other error message is raised,
      # then raise that error and abort function
      error = function(e) {
        if (grepl("^skip=[0-9]+ but the input only has [0-9]+ lines*$",
                  e$message)) {
          return(NULL)
        } else {
          stop(e)
        }
      }
    )

    # if chunk is not NULL, apply callback function and increment i. Continue
    # loop
    if (!is.null(chunk)) {
      chunk <- callback(chunk, ...)
      i <- i + 1

      # store chunks in list if return_chunks arg == TRUE
      if (return_chunks) {
        data <- dplyr::bind_rows(data, chunk)
      }

      # time taken message
      if (progress) {
        message("\nProcessed ", i * chunk_size, " rows")
        rawutil::time_taken_message(start_time)
      }
    } else if (is.null(chunk)) {
      # if
      break()
    } else {
      stop("Unexpected error while processing chunks")
    }
  }

  # completion message
  message("Complete!")
  rawutil::time_taken_message(start_time)

  # return result, if return_chunks == TRUE
  if (return_chunks) {
    return(data)
  }
}


#' Apply a function to a data frame in chunks
#'
#' May be helpful if applying \code{\link[dplyr]{pivot_longer}} to a large
#' dataframe where many of the values are \code{NA} and these are to be removed.
#'
#' @param df data frame
#' @inheritParams fread_chunked
#'
#' @return a dataframe
#' @export
process_df_chunked <- function(df,
                               callback,
                               chunk_size = 100,
                               progress = TRUE,
                               ...) {
  # start timer
  start_time <- proc.time()

  # stop if chunk_size is not an integer or less than 1
  assert_integer_ge_1(chunk_size,
                      "chunk_size")

  # initialise empty list to hold processed chunks
  total_n_rows <- nrow(df)
  n_chunks <- ceiling(total_n_rows / chunk_size) # (round up)
  result <- vector(mode = "list", length = n_chunks)

  # apply callback function to df in chunks
  message("Processing data frame in chunks")

  for (chunk in seq_along(result)) {
    # rows to process
    start_row <- ((chunk - 1) * chunk_size) + 1
    end_row <-  (start_row + chunk_size) - 1

    if (end_row > total_n_rows) {
      # for final chunk, set end_row to nrow(df)
      end_row <- total_n_rows
    }

    # subset df and process
    df_chunk <- df[start_row:end_row, ]
    df_chunk <- callback(df_chunk, ...)

    # check df_chunk is a df (or data table)
    assertthat::assert_that(any(class(df_chunk) %in% c("data.frame", "data.table")),
                            msg = "Error! callback function must return either a data frame or data table")

    # add processed to chunk to 'result'
    result[[chunk]] <- df_chunk

    # time taken message
    if (progress) {
      message("\nProcessed ", end_row, " rows")
      rawutil::time_taken_message(start_time)
    }
  }

  # combine results
  message("Concatenating chunks")
  result <- dplyr::bind_rows(result)

  # completion message
  message("Complete!")
  rawutil::time_taken_message(start_time)

  return(result)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

#' Assert number is an integer that is greater than or equal to 1
#'
#' Helper function for \code{\link{fread_chunked}} and
#' \code{\link{process_df_chunked}}.
#'
#' @param x An integer >= 1. Raises an error if this condition is not met
#' @param arg_name character. The argument name for x. This is used to generate
#'   an informative error message.
#'
#' @seealso \code{\link{fread_chunked}}, \code{\link{process_df_chunked}}
assert_integer_ge_1 <- function(x, arg_name) {
  # custom error message
  error_message <- paste("Error!", arg_name, "must be an integer that is greater than 0")

  # assertion
  assertthat::assert_that(x >= 1,
                          rlang::is_integerish(x),
                          msg = error_message)
}


