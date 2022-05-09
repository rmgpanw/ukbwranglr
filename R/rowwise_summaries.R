# NOTES -------------------------------------------------------------------

# EXPORTED FUNCTIONS ----------------------------------------------------

#' Summarise numerical variables
#'
#' Summarises numerical variables with repeated measurements either by field
#' (i.e. all available measurements) or by instance (i.e. for all measurements
#' at each assessment visit). Currently available summary options are mean,
#' minimum, maximum, sum and number of non-missing values.
#'
#' Note that when \code{summary_function = "sum"}, missing values are converted
#' to zero. Therefore if a set of values are \emph{all} missing then the sum
#' will summarised as \code{0}. See the documentation for
#' \code{\link[base]{rowSums}} for further details.
#'
#' @param ukb_main A UK Biobank main dataset data frame. Column names must match
#'   those under the \code{descriptive_colnames} column in \code{data_dict}.
#' @param data_dict a data dictionary specific to the UKB main dataset file,
#'   created by \code{\link{make_data_dict}}.
#' @inheritParams make_data_dict
#' @param summary_function The summary function to be applied. Options: "mean",
#'   "min", "max", "sum" or "n_values"
#' @param summarise_by Whether to summarise by "Field" or by "Instance".
#' @param .drop If \code{TRUE}, removes the original numerical variables from
#'   the result. Default value is \code{FALSE}.
#'
#' @return A data frame with new columns summarising numerical variables. The
#'   names for these new columns are prefixed by the value for
#'   \code{summary_function} and end with 'x', FieldID +/- instance being
#'   summarised e.g. if summarising FieldID 4080 instance 0, the new column
#'   would be named 'mean_systolic_blood_pressure_automated_reading_x4080_0'.
#' @export
#' @examples
#' library(magrittr)
#' # get dummy UKB data and data dictionary
#' dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
#' dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
#'
#' dummy_ukb_main <- read_ukb(
#'   path = get_ukb_dummy("dummy_ukb_main.tsv", path_only = TRUE),
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   ukb_codings = dummy_ukb_codings
#' ) %>%
#'   dplyr::select(eid, tidyselect::contains("systolic_blood_pressure")) %>%
#'   tibble::as_tibble()
#'
#' # summarise mean values by Field, keep original variables
#' summarise_numerical_variables(
#'   dummy_ukb_main,
#'   ukb_data_dict = dummy_ukb_data_dict
#' )
#'
#' # summarise mean values by Field, drop original variables
#' summarise_numerical_variables(
#'   dummy_ukb_main,
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   .drop = TRUE
#' )
#'
#' # summarise min values by instance, dropping original variables
#' summarise_numerical_variables(
#'   dummy_ukb_main,
#'   ukb_data_dict = dummy_ukb_data_dict,
#'   summary_function = "min",
#'   summarise_by = "Instance",
#'   .drop = TRUE
#' )
summarise_numerical_variables <- function(ukb_main,
                                          data_dict = NULL,
                                          ukb_data_dict = get_ukb_data_dict(),
                                          summary_function = "mean",
                                          summarise_by = "Field",
                                          .drop = FALSE) {
  start_time <- proc.time()

  dummy_ukb_data <- tibble::tibble(
    f.eid = c(1, 2, 3, 4, 5, 6),
    f.31.0.0 = c(0, 0, 1, 0, NA, 1),
    f.34.0.0 = c(1952, 1946, 1951, 1956, NA, 1948),
    f.52.0.0 = c(8, 3, 4, 9, 4, 2),
    f.21000.0.0 = c(NA, 4001, 3, NA, -3, 3),
    f.20002.0.0 = c(1665, 1383, 1197, 1441, 1429, 1513),
    f.21001.0.0 = c(20.1115, 30.1536, 22.8495, 23.4904, 29.2752, 28.2567),
    f.21001.1.0 = c(20.864, 20.2309, 26.7929, 25.6826, 19.7576, 30.286),
    f.21001.2.0 = c(NA, 27.4936, 27.6286, 37.2294, 14.6641, 27.3534),
  )

  # validate args
  match.arg(summary_function,
            choices = c("mean", "min", "max", "sum", "n_values"))

  match.arg(summarise_by,
            choices = c("Field", "Instance"))

  if (is.null(data_dict)) {
    data_dict <- make_data_dict(ukb_main,
                                ukb_data_dict = ukb_data_dict)
  } else if (!is.null(data_dict)) {
    assertthat::assert_that(all(data_dict$colheaders_raw %in% names(ukb_main)),
                            msg = "Error! `data_dict` does not match `ukb_main`. All values in `colheaders_raw` should be present in `names(ukb_main)`. Try making a new data dictionary with `make_data_dict()`?")
  }

  # rowwise summary functions
  function_list <- list(
    mean = rowMeans,
    min = pmin,
    max = pmax,
    sum = rowSums,
    n_values = function(x, na.rm) rowSums(!is.na(x), na.rm)
  )

  # filter for numerical variables with more than one instance and create names
  # for summary cols (depends on whether summarising by Field or by Instance)
  numerical_vars_to_summarise <- data_dict %>%
    dplyr::filter(.data[["ValueType"]] %in% c("Continuous", "Integer")) %>%
    dplyr::filter(.data[["Field"]] != "eid") %>%
    dplyr::mutate("Field" = tolower(.data[["Field"]]))

  if (summarise_by == "Field") {
    numerical_vars_to_summarise <- numerical_vars_to_summarise %>%
      dplyr::filter(as.numeric(.data[["Instances"]]) > 1) %>%
      dplyr::mutate(summary_colname = paste0(stringr::str_replace_all(stringr::str_to_title(summary_function),
                                                                   "_",
                                                                   " "),
                                             " ",
                                             .data[["Field"]],
                                             " (x",
                                             .data[["FieldID"]],
                                             ")"))
  } else if (summarise_by == "Instance") {
    numerical_vars_to_summarise <- numerical_vars_to_summarise %>%
      dplyr::filter(as.numeric(.data[["Array"]]) > 1) %>%
      dplyr::mutate(summary_colname = paste0(stringr::str_replace_all(stringr::str_to_title(summary_function),
                                                                   "_",
                                                                   " "),
                                             " ",
                                             .data[["Field"]],
                                             " (x",
                                             .data[["FieldID"]],
                                             " ",
                                             .data[["instance"]],
                                             ")"))
  }

  # exit if no variables to summarise
  assertthat::assert_that(nrow(numerical_vars_to_summarise) > 0,
                          msg = paste0("Error! No numerical variables to summarise by ",
                                       summarise_by,
                                       ". Check data dictionary - are there any numerical variables with more than one instance/array?"))

  # split by new summary col labels
  numerical_vars_to_summarise <- split(numerical_vars_to_summarise,
                                       numerical_vars_to_summarise$summary_colname)

  # number of summary cols
  message(paste0("Number of summary columns to make: ", length(names(
    numerical_vars_to_summarise
  ))))

  # progress bar - one tick per summary column
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                   total = length(names(numerical_vars_to_summarise)))
  pb$tick(0)

  for (new_col in names(numerical_vars_to_summarise)) {
    # make new summary colname and cols to summarise
    new_col_name <-
      remove_special_characters_and_make_lower_case(new_col)

    selected_cols <-
      numerical_vars_to_summarise[[new_col]][["colheaders_raw"]]

    # summarise - different approaches required for pmin/pmax vs rowMeans/rowSums
    if (summary_function %in% c("min", "max")) {
      if (data.table::is.data.table(ukb_main)) {
        ukb_main[[new_col_name]] <-
          do.call(function_list[[summary_function]], c(ukb_main[, ..selected_cols], list(na.rm = TRUE)))
      } else {
        ukb_main[[new_col_name]] <-
          do.call(function_list[[summary_function]], c(ukb_main[, selected_cols, drop = FALSE], list(na.rm = TRUE)))
      }
    } else if (summary_function %in% c("mean", "sum", "n_values")) {
      ukb_main <- ukb_main %>%
        dplyr::mutate(!!new_col_name := function_list[[summary_function]](dplyr::across(
          tidyselect::all_of(selected_cols)
        ), na.rm = TRUE))
    }

    attributes(ukb_main[[new_col_name]])$label <- new_col

    # warning if only one column was summarised (this column would equal the summary column if so)
    if (length(selected_cols) == 1) {
      warning(
        paste0(
          "Warning! Summary column '",
          new_col_name,
          "' was summarised from  only a single column: '",
          selected_cols,
          "'. Are there missing instances/arrays in the dataset for this FieldID?\n"
        )
      )
    }

    # remove summarised columns
    if (.drop) {
      ukb_main <- ukb_main %>%
        dplyr::select(-tidyselect::all_of(selected_cols))
    }

    pb$tick(1)
  }

  time_taken_message(start_time)
  return(ukb_main)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

# Dev ---------------------------------------------------------------------

#' Summarise a UK Biobank phenotype dataset row-wise
#'
#' Allows the user to summarise row-wise across a set of column "groups". e.g.
#' calculate standard deviation values for all systolic and diastolic blood
#' pressure readings per eid. Can also accept custom functions (e.g. a function
#' to count the number of non-NA values). Custom functions should include a
#' '...' parameter. \strong{Note: \code{ukb_main} must be a data table.}
#'
#' @param ukb_main a UK Biobank phenotype dataset. Must be a data table
#' @param functions a character vector of function names (e.g. \code{c("sd")})
#' @param data_dict a data dictionary for \code{ukb_main}. Can be a "filtered"
#'   version including only the columns to be summarised.
#' @param grouping_col the name of a column in \code{data_dict} that indicates
#'   column groups to summarise
#' @param prefix character. An optional prefix to add to the names of all newly
#'   created columns. Default is \code{NULL}.
#' @param ... arguments to passed on to summary functions listed in
#'   \code{functions}
#'
#' @noRd
#' @examples
#' \dontrun{
#' # make a custom function that counts the number of non-NA values.
#' # Note: include '...', otherwise an error is thrown by including na.rm = TRUE below
#'
#' # n_not_na <- function(x, ...) {
#' # sum(!is.na(x))
#' # }
#'
#' # summarise all numerical columns in UKB dataset by calculating mean/sd/n_not_na,
#' # including argument na.rm = TRUE
#'
#' # ukb_main_summarised <- summarise_rowwise(
#' # # UKB phenotype dataset as a datatable
#' # ukb_main = ukb_main,
#'
#' # functions = c("mean", "sd", "n_not_na"),
#'
#' # # data dictionary generated by make_data_dict(), filtered
#' # # for only numerical data
#' # data_dict = data_dict %>%
#' #    dplyr::filter(ValueType %in% c("Continuous", "Integer")),
#'
#' # # to summarise by FieldID
#' #      grouping_col = "Field_FieldID"
#'
#' # # additional args passed on to `functions`
#' #      na.rm = TRUE
#' #     )
#' }
summarise_rowwise <- function(ukb_main,
                             functions,
                             data_dict,
                             grouping_col = "Field_FieldID",
                             prefix = NULL,
                             ...) {

  start_time <- proc.time()

  # validate args
  assertthat::is.string(grouping_col)

  assertthat::assert_that(
    all(data_dict$descriptive_colnames %in% names(ukb_main)),
    msg = "Error! `data_dict$descriptive_colnames` includes values not present in `names(ukb_main)`. Please filter `data_dict` for only the columns in `ukb_main` to be summarised"
  )

  # ***STEP 1***
  # Create list of column groups with columns to summarise and new
  # colnames as nested dataframe

  # get groups of columns to summarise from data dictionary
  col_groups_to_summarise <- data_dict %>%
    dplyr::group_by(.data[[grouping_col]]) %>%
    tidyr::nest()

  # error if no col_groups_to_summarise
  assertthat::assert_that(
    !nrow(col_groups_to_summarise) == 0,
    msg = "Error! No columns identified to summarise from the argument values provided"
  )

  # rename grouping_col to facilitate join in code chunk below
  col_groups_to_summarise <- rename_cols(df = col_groups_to_summarise,
                                         old_colnames = c(grouping_col, "data"),
                                         new_colnames = c("group", "cols_to_summarise"))

  # new summary colnames: paste function names with grouping_col (e.g.
  # mean_blood_pressure...)
  new_colnames <- base::expand.grid(functions,
                                    col_groups_to_summarise$group,
                                    stringsAsFactors = FALSE)
  # expand.grid makes all possible combinations of functions/col_group_names,
  # now paste these together to create new colnames
  new_colnames$new_colnames <- paste(new_colnames$Var1,
                                     new_colnames$Var2,
                                     sep = "_")

  # add optional prefix to new column names
  if (!is.null(prefix)) {
    new_colnames$new_colnames <- paste0(prefix,
                                        new_colnames$new_colnames)
  }

  new_colnames <- new_colnames %>%
    # remove 'functions' column ('Var1')
    dplyr::select(-.data[["Var1"]]) %>%
    # group_by grouping_col and nest (and rename "data" as newcolnames for clarity)
    dplyr::group_by(.data[["Var2"]]) %>%
    tidyr::nest() %>%
    dplyr::rename("new_colnames" = .data[["data"]])


  # join with col_groups_to_summarise (prev step)
  col_groups_summary_dict <- col_groups_to_summarise %>%
    dplyr::left_join(new_colnames, by = c("group" = "Var2"))


  # ***STEP 2***
  # Summarise: Using dictionary of summary jobs
  # ('col_groups_summary_dict'), loop through each group of columns to be
  # summarised, applying the full set of summary functions

  # Set up progress bar TODO - not shwoing for some reason...
  # pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
  #                                  total = length(col_groups_summary_dict$group),
  #                                  force = TRUE)
  # pb$tick(0) TODO

  # loop
  for (col_group in seq_along(col_groups_summary_dict$group)) {

    # progress bar - indicates how many col_groups have ben summarised
    # pb$tick(1) TODO

    # time taken message
    if (col_group != 1) {
      time_taken <- proc.time() - start_time

      message("\tTime taken: ",
              (time_taken[3] %/% 60),
              " minutes, ",
              (round(time_taken[3] %% 60)),
              " seconds")
    }

    # number of col_group's processed message
    message("Processing ",
            col_group,
            " of ",
            length(col_groups_summary_dict$group))

    # mutate summary cols for col_group
    ukb_main[
      # i
      ,

      # j
      col_groups_summary_dict$new_colnames[[col_group]]$new_colnames := purrr::map(
        functions,
        dt_rowwise_fn,
        .SD,
        ...),

      # by
      .SDcols = col_groups_summary_dict$cols_to_summarise[[col_group]]$descriptive_colnames]
  }

  # time taken message
  time_taken <- proc.time() - start_time

  message(
    "Complete! Time taken: ",
    (time_taken[3] %/% 60),
    " minutes, ",
    (round(time_taken[3] %% 60)),
    " seconds"
  )

  # message - number of new columns created and their names
  new_colnames_vector <- tidyr::unnest(new_colnames,
                                       c(.data[["new_colnames"]]))$new_colnames
  message(paste0(
    "Appended, ", length(new_colnames_vector), " new columns: ", stringr::str_c(new_colnames_vector,
                                                                                sep = "",
                                                                                collapse = ", ")
  ))

  return(ukb_main)
}

#' Summarise a group of columns row-wise
#'
#' Helper function
#'
#' @param function_name A function name as a character
#' @param selected_cols Character vector of column names
#' @inheritParams rowwise_min_max_date
#' @noRd
rowwise_summary <- function(ukb_main,
                           function_name,
                           selected_cols,
                           new_colname) {

  ukb_main[, c(new_colname) := purrr::map(
    function_name,
    dt_rowwise_fn,
    .SD), .SDcols = selected_cols]

  return(ukb_main)
}

#' Helper function for rowwise_summary()
#'
#' @param fn a character vector of functions
#' @param cols a character vector of column names to summarise with functions in
#'   fn
#' @param ... allows additional arguments such as na.rm to passed on to
#'   functions listed in fn
#' @noRd
dt_rowwise_fn <- function(fn, cols, ...) {
  # Row-wise summary helper function:
  # applies a set of summary functions ('fn' =
  # character vector of function names) across selected columns ('cols' =
  # character vector of column names in a datatable) using apply(). '...' allows
  # additional arguments to be passed on to functions (e.g. 'na.rm = ')
  apply(cols, MARGIN = 1, fn, ...)
}

