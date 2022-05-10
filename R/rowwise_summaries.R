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
    choices = c("mean", "min", "max", "sum", "n_values")
  )

  match.arg(summarise_by,
    choices = c("Field", "Instance")
  )

  if (is.null(data_dict)) {
    data_dict <- make_data_dict(ukb_main,
      ukb_data_dict = ukb_data_dict
    )
  } else if (!is.null(data_dict)) {
    assertthat::assert_that(all(data_dict$colheaders_raw %in% names(ukb_main)),
      msg = "Error! `data_dict` does not match `ukb_main`. All values in `colheaders_raw` should be present in `names(ukb_main)`. Try making a new data dictionary with `make_data_dict()`?"
    )
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
      dplyr::mutate(summary_colname = paste0(
        stringr::str_replace_all(
          stringr::str_to_title(summary_function),
          "_",
          " "
        ),
        " ",
        .data[["Field"]],
        " (x",
        .data[["FieldID"]],
        ")"
      ))
  } else if (summarise_by == "Instance") {
    numerical_vars_to_summarise <- numerical_vars_to_summarise %>%
      dplyr::filter(as.numeric(.data[["Array"]]) > 1) %>%
      dplyr::mutate(summary_colname = paste0(
        stringr::str_replace_all(
          stringr::str_to_title(summary_function),
          "_",
          " "
        ),
        " ",
        .data[["Field"]],
        " (x",
        .data[["FieldID"]],
        " ",
        .data[["instance"]],
        ")"
      ))
  }

  # exit if no variables to summarise
  assertthat::assert_that(nrow(numerical_vars_to_summarise) > 0,
    msg = paste0(
      "Error! No numerical variables to summarise by ",
      summarise_by,
      ". Check data dictionary - are there any numerical variables with more than one instance/array?"
    )
  )

  # split by new summary col labels
  numerical_vars_to_summarise <- split(
    numerical_vars_to_summarise,
    numerical_vars_to_summarise$summary_colname
  )

  # number of summary cols
  message(paste0("Number of summary columns to make: ", length(names(
    numerical_vars_to_summarise
  ))))

  # progress bar - one tick per summary column
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent)",
    total = length(names(numerical_vars_to_summarise))
  )
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
