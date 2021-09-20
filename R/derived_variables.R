

# NOTES -------------------------------------------------------------------

# Dervied variable function requirements:

# - Function name should start with 'derive_'

# - Accept a df as first argument, and return a df with appended columns

# - Have a `.drop` argument - if `TRUE`, remove required input FID cols from output

# - Have an argument `.details_only`. If `TRUE`, return a list containing items:

## - `required_fields`

## - `new_columns`, a named list of new columns with details for each:

### - `label`

### - `value_labels`

# EXPORTED FUNCTIONS ------------------------------------------------------


#' Derive an estimated participant date of birth
#'
#' Estimates dates of birth from year and month of birth (Field IDs
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=34}{34} and
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=52}{52} respectively) as
#' the first date of the month.
#'
#' @param ukb_main A UK Biobank main dataset.
#' @param .drop If \code{TRUE}, remove the required input columns from the
#'   result
#' @param .details_only If \code{TRUE}, return a list containing details of
#'   required input variables (Field IDs) and derived variables (new column
#'   name, label and values/value labels).
#' @inheritParams read_ukb
#'
#' @return A data frame with a column called \code{dob_derived} (type
#'   character).
#' @export
#' @examples
#' ukb_main <- data.frame(eid = 1, f34_0_0 = 1990, f52_0_0 = 1)
#'  # keep input year/month of birth columns
#'  derive_dob(ukb_main)
#'
#'  # remove input year/month of birth columns
#'  derive_dob(ukb_main, .drop = TRUE)
derive_dob <- function(ukb_main,
                       ukb_data_dict = get_ukb_data_dict(),
                       .drop = FALSE,
                       .details_only = FALSE) {
  list_of_details <- list(
    required_field_ids = c(yob = "34", mob = "52"),
    new_columns = list(dob_derived = list(
      label = "Date of birth",
      value_labels = NULL
    ))
  )

  # if required field IDs requested
  if (.details_only) {
    return(list_of_details)
  }

  data_dict <- make_data_dict(ukb_main,
                              ukb_data_dict = ukb_data_dict)

  # select only the 2 required Field IDs
  yob_col <- get_colnames_for_fieldids(
    field_ids = list_of_details$required_field_ids['yob'],
    data_dict = data_dict,
    scalar_output = TRUE,
    error_if_missing = TRUE,
    colname_col = "colheaders_raw"
  )

  mob_col <- get_colnames_for_fieldids(
    field_ids = list_of_details$required_field_ids['mob'],
    data_dict = data_dict,
    scalar_output = TRUE,
    error_if_missing = TRUE,
    colname_col = "colheaders_raw"
  )

  # check that 'mob' column is either a factor or numeric
  assertthat::assert_that(
    is.factor(ukb_main[[mob_col]]) | is.numeric(ukb_main[[mob_col]]),
    msg = "Error! The month of birth column (Field ID 52) must either be type numeric or type factor"
  )

  # estimate dob
  new_dob_col <- names(list_of_details$new_columns)

  ukb_main[[new_dob_col]] <- paste(ukb_main[[yob_col]],
                                as.integer(ukb_main[[mob_col]]), # need to extract if a factor integer value e.g. 'January' == 1
                                01, # first day of month
                                sep = '-')

  suppressWarnings(
    ukb_main[[new_dob_col]] <- as.character(lubridate::ymd(ukb_main$dob_derived))
  )

  # drop input cols if requested
  if (.drop) {
    ukb_main <- dplyr::select(ukb_main, -tidyselect::all_of(c(yob_col, mob_col)))
  }

  return(ukb_main)
}
