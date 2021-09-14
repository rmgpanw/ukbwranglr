# NOTES -------------------------------------------------------------------

# EXPORTED FUNCTIONS ----------------------------------------------------

#' Summarise numerical variables
#'
#' Summarises numerical variables with repeated measurements either by field
#' (i.e. all available measurements) or by instance (i.e. for all measurements
#' at each assessment visit). Currently available summary options are mean,
#' minimum, maximum and sum.
#'
#' @param summary_function The summary function to be applied. Options:
#'   "Mean", "Min", "Max" or "Sum"
#' @param summarise_by Whether to summarise by "Field" or by "Instance".
#' @param .drop If \code{TRUE}, removes the original numerical variables from
#'   the result. Default value is \code{FALSE}.
#' @inheritParams tidy_clinical_events
#' @inheritParams read_ukb
#'
#' @return A data frame
#' @export
#' @examples
#' # get dummy data
#' dummy_data_path <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")
#' read_ukb(dummy_data_path, delim = ",") %>%
#'   summarise_numerical_variables()
summarise_numerical_variables <- function(ukb_main,
                                          data_dict = NULL,
                                          summary_function = "Mean",
                                          summarise_by = "Field",
                                          .drop = FALSE) {
  start_time <- proc.time()

  # validate args
  match.arg(summary_function,
            choices = c("Mean", "Min", "Max", "Sum"))

  match.arg(summarise_by,
            choices = c("Field", "Instance"))

  if (is.null(data_dict)) {
    data_dict <- make_data_dict(ukb_main)
  }

  # rowwise summary functions
  function_list <- list(
    Mean = rowMeans,
    Min = pmin,
    Max = pmax,
    Sum = rowSums
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
      dplyr::mutate(summary_colname = paste0(summary_function,
                                             " ",
                                             .data[["Field"]]))
  } else if (summarise_by == "Instance") {
    numerical_vars_to_summarise <- numerical_vars_to_summarise %>%
      dplyr::filter(as.numeric(.data[["Array"]]) > 1) %>%
      dplyr::mutate(summary_colname = paste0(summary_function,
                                             " ",
                                             .data[["Field"]],
                                             " (",
                                             .data[["instance"]], ")"))
  }

  # exit if no variables to summarise
  assertthat::assert_that(nrow(numerical_vars_to_summarise) > 0,
                          msg = paste0("Error! No numerical variables to summarise by ",
                                       summarise_by,
                                       ". Check data dictionary"))

  # split by new summary col labels
  numerical_vars_to_summarise <- split(numerical_vars_to_summarise,
                                       numerical_vars_to_summarise$summary_colname)

  # number of summary cols
  message(paste0("Number of summary columns to make: ", length(names(
    numerical_vars_to_summarise
  ))))

  for (new_col in names(numerical_vars_to_summarise)) {
    # make new summary colname and cols to summarise
    new_col_name <-
      remove_special_characters_and_make_lower_case(new_col)

    selected_cols <-
      numerical_vars_to_summarise[[new_col]][["descriptive_colnames"]]

    # summarise - different approaches required for pmin/pmax vs rowMeans/rowSums
    if (summary_function %in% c("Min", "Max")) {
      if (data.table::is.data.table(ukb_main)) {
        ukb_main[[new_col_name]] <-
          do.call(function_list[[summary_function]], c(ukb_main[, ..selected_cols], list(na.rm = TRUE)))
      } else {
        ukb_main[[new_col_name]] <-
          do.call(function_list[[summary_function]], c(ukb_main[, selected_cols, drop = FALSE], list(na.rm = TRUE)))
      }
    } else if (summary_function %in% c("Mean", "Sum")) {
      ukb_main <- ukb_main %>%
        dplyr::mutate(!!new_col_name := function_list[[summary_function]](dplyr::across(
          tidyselect::all_of(selected_cols)
        ), na.rm = TRUE))
    }

    ukb_main[[new_col_name]] <-
      haven::labelled(x = ukb_main[[new_col_name]],
                      labels = NULL,
                      label = new_col)

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
#' '...' parameter. \strong{Note: \code{ukb_pheno} must be a data table.}
#'
#' @param ukb_pheno a UK Biobank phenotype dataset. Must be a data table
#' @param functions a character vector of function names (e.g. \code{c("sd")})
#' @param data_dict a data dictionary for \code{ukb_pheno}. Can be a "filtered"
#'   version including only the columns to be summarised.
#' @param grouping_col the name of a column in \code{data_dict} that indicates
#'   column groups to summarise
#' @param prefix character. An optional prefix to add to the names of all newly
#'   created columns. Default is \code{NULL}.
#' @param ... arguments to passed on to summary functions listed in
#'   \code{functions}
#'
#' @export
#'
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
#' # ukb_pheno_summarised <- summarise_rowise(
#' # # UKB phenotype dataset as a datatable
#' # ukb_pheno = ukb_pheno,
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
summarise_rowise <- function(ukb_pheno,
                             functions,
                             data_dict,
                             grouping_col = "Field_FieldID",
                             prefix = NULL,
                             ...) {

  # validate args
  assertthat::is.string(grouping_col)

  assertthat::assert_that(
    all(data_dict$descriptive_colnames %in% names(ukb_pheno)),
    msg = "Error! `data_dict$descriptive_colnames` includes values not present in `names(ukb_pheno)`. Please filter `data_dict` for only the columns in `ukb_pheno` to be summarised"
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
    ukb_pheno[
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

  return(ukb_pheno)
}


#' Get rowise min/max date from UKB dataset
#'
#' Wrapper around \code{pmin}/\code{pmax}. Removes 'nonsense' dates (see example
#' special coding on
#' \href{https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=819}{UK Biobank
#' website}).
#'
#' The special date coding-IDs are 1313, 272, 586 and 819. Note: these are also
#' used in the linked primary care dataset.
#'
#' @inheritParams tidy_clinical_events
#' @param selected_date_cols Character vector of column names
#' @param new_colname Name of new column to be created
#' @param min_max Character. Must be either "pmin" or "pmax"
#'
#' @return The UKB dataset supplied to argument \code{ukb_pheno} with an
#'   additional column containing the earliest/latest date across the
#'   \code{selected_date_cols}.
#' @export
rowise_min_max_date <- function(ukb_pheno,
                                selected_date_cols,
                                new_colname,
                                min_max = "pmin") {
  start_time <- proc.time()

  # check min_max arg is either pmin or pmax
  match.arg(min_max,
            choices = c('pmin', 'pmax'))

  # nonsense dates, as integer
  nonsense_dates <- lubridate::as_date(
    c(
      '1904-04-04',
      '1900-01-01',
      '1910-01-01',
      '1920-01-01',
      '1930-01-01',
      '1901-01-01',
      '1902-02-02',
      '1903-03-03',
      '2037-07-07'
    )
  )

  nonsense_dates_integer <- as.integer(nonsense_dates)

  # get subset of date cols + eid
  subset_cols <- c("eid", selected_date_cols)
  ukb_pheno_subset <- ukb_pheno[, subset_cols]

  # remove nonsense dates (set these to NA) - get a data.table warning message
  # when using this... ukb_pheno_subset <- ukb_pheno_subset %>%

  # dplyr::mutate(dplyr::across(tidyselect::all_of(selected_date_cols),
  # function(x) {replace(x, x %in% nonsense_dates, NA)}))

  # THIS MAKES DATES BECOME RANDOM NUMBERS - NEED TO BE IN INTEGER FORMAT FIRST?
  # ukb_pheno_subset[
  #   ,
  #   c(selected_date_cols) := lapply(.SD, function(x) {ifelse(x %in% nonsense_dates,
  #                                                            NA,
  #                                                            (x))}),
  #   .SDcols = selected_date_cols
  #   ]

  # convert selected_date_cols to integer form
  ukb_pheno_subset[
    ,
    c(selected_date_cols) := lapply(.SD, as.integer),
    .SDcols = selected_date_cols
  ]

  # set nonsense dates to NA
  ukb_pheno_subset[
    ,
    c(selected_date_cols) := lapply(.SD, function(x) {ifelse(x %in% nonsense_dates_integer,
                                                             NA,
                                                             (x))}),
    .SDcols = selected_date_cols
  ]

  # mutate min/max date rowise
  ukb_pheno_subset[
    # i
    ,

    # j
    new_col := do.call(min_max, c(.SD, list(na.rm = TRUE))),

    # by = columns to summarise
    .SDcols = selected_date_cols]

  # convert new_col back to date format
  ukb_pheno_subset[
    ,
    `:=`(new_col = lubridate::as_date(new_col))
  ]

  # Add new col to full dataset
  ukb_pheno[[new_colname]] <- ukb_pheno_subset$new_col

  return(ukb_pheno)
}


#' Summarise a group of columns row-wise
#'
#' @param function_name A function name as a character
#' @param selected_cols Character vector of column names
#' @inheritParams rowise_min_max_date
#'
#' @export
rowise_summary <- function(ukb_pheno,
                           function_name,
                           selected_cols,
                           new_colname) {

  ukb_pheno[, c(new_colname) := purrr::map(
    function_name,
    dt_rowwise_fn,
    .SD), .SDcols = selected_cols]

  return(ukb_pheno)
}

#' Helper function for summarise_rowise()
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

