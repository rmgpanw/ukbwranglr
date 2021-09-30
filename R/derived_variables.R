

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
#' @return A data frame with a column called \code{dob} (type
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
    new_columns = list(dob = list(
      label = "Date of birth (estimated)",
      value_labels = NA,
      FieldID = NA,
      instance = NA,
      array = NA,
      ValueType = "Date"
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
    ukb_main[[new_dob_col]] <- as.character(lubridate::ymd(ukb_main$dob))
  )

  # label
  attributes(ukb_main[[new_dob_col]])$label <- list_of_details$new_columns$dob$label

  # drop input cols if requested
  if (.drop) {
    ukb_main <- dplyr::select(ukb_main, -tidyselect::all_of(c(yob_col, mob_col)))
  }

  return(ukb_main)
}


#' Derive simplified ethnic background
#'
#' Simplifies ethnic background in a UK Biobank main dataset to the main
#' categories for
#' \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=21000}{Field ID 21000}.
#'
#' Categories "Do not know" and "Prefer not to answer" are converted to
#' \code{NA}. A new column called \code{ethnic_background_simplified} of type
#' factor is appended to the input data frame. By default, "White" ethnicity is
#' set to the baseline level as this is the largest category. Levels can be
#' explicitly specified using the \code{ethnicity_levels} argument.
#'
#' @param ethnicity_levels The factor level order for the appended
#'   \code{ethnic_background_simplified} column. By default, the baseline level
#'   is set to "White" ethnicity.
#' @inheritParams derive_dob
#'
#' @return A data frame with a column called \code{ethnic_background_simplified}
#'   (type factor).
#' @export
#'
#' @examples
#' dummy_ukb_data <- data.frame(
#'   eid = c(1, 2, 3),
#'   f.21000.0.0 = factor(c("White", "White and Black Caribbean", NA)),
#'   f.21000.1.0 = factor(c("British", "Caribbean", NA)),
#'   f.21000.2.0 = factor(c("Irish", NA, "Any other Asian background"))
#' )
#'
#' derive_ethnic_background_simplified(
#'   ukb_main = dummy_ukb_data,
#'   ukb_data_dict = get_ukb_data_dict()
#' )
#'
derive_ethnic_background_simplified <- function(ukb_main,
                                                ukb_data_dict,
                                                ethnicity_levels = c(
                                                  "White",
                                                  "Mixed",
                                                  "Asian or Asian British",
                                                  "Black or Black British",
                                                  "Chinese",
                                                  "Other ethnic group"
                                                ),
                                                .drop = FALSE,
                                                .details_only = FALSE) {
  list_of_details <- list(
    required_field_ids = c(ethnic_background = "21000"),
    new_columns = list(ethnic_background_simplified = list(
      label = "Ethnic background (simplified)",
      value_labels = NA,
      FieldID = "21000",
      instance = NA,
      array = NA,
      ValueType = "Categorical single"
    ))
  )

  # if required field IDs only requested
  if (.details_only) {
    return(list_of_details)
  }

  # validate args
  assertthat::assert_that(!names(list_of_details$new_columns) %in% names(ukb_main),
                          msg = paste0("Error! `ukb_main` already has a column named ",
                                       names(list_of_details$new_columns)))

  # get required cols
  data_dict <- make_data_dict(ukb_main,
                              ukb_data_dict = ukb_data_dict)

  ethnic_background_cols <- get_colnames_for_fieldids(
    field_ids = list_of_details$required_field_ids['ethnic_background'],
    data_dict = data_dict,
    scalar_output = FALSE,
    error_if_missing = TRUE,
    colname_col = "colheaders_raw"
  )

  # sort - sets earliest record to 'baseline'
  ethnic_background_cols <- sort(ethnic_background_cols)

  # loop through input ethnicity cols to simplify
  params <- data.frame(old_ethnic_background_col = ethnic_background_cols)
  params$new_ethnic_background_col <- paste0(params$old_ethnic_background_col, "_simplified")

  for (i in 1:nrow(params)) {
    old_ethnic_background_col <- params[i, ][["old_ethnic_background_col"]]
    new_ethnic_background_col <- params[i, ][["new_ethnic_background_col"]]

    ukb_main <- derive_ethnic_background_simplified_single(ukb_main = ukb_main,
                                               old_ethnic_background_col = old_ethnic_background_col,
                                               new_ethnic_background_col = new_ethnic_background_col,
                                               ethnicity_levels = ethnicity_levels,
                                               .details_only = FALSE)
  }

  # create single summary col - take the first non-missing value
  ukb_main <- summarise_first_non_na(df = ukb_main,
                                     columns = params$new_ethnic_background_col,
                                     new_col = names(list_of_details$new_columns))

  # drop individual simplified ethnicity columns
  ukb_main <- ukb_main %>%
    dplyr::select(-tidyselect::all_of(params$new_ethnic_background_col))

  # reorder factor
  ukb_main[[names(list_of_details$new_columns)]] <-
    factor(ukb_main[[names(list_of_details$new_columns)]],
           levels = ethnicity_levels)

  # label
  attributes(ukb_main[[names(list_of_details$new_columns)]])$label <- list_of_details$new_columns$ethnic_background_simplified$label

  # drop original cols
  if (.drop) {
    ukb_main <- ukb_main %>%
      dplyr::select(-tidyselect::all_of(params$old_ethnic_background_col))
  }

  # return result
  return(ukb_main)
}



# PRIVATE FUNCTIONS -------------------------------------------------------

derive_ethnic_background_simplified_single <- function(ukb_main,
                                                       old_ethnic_background_col,
                                                       new_ethnic_background_col,
                                                       ethnicity_levels = c(
                                                         "White",
                                                         "Mixed",
                                                         "Asian or Asian British",
                                                         "Black or Black British",
                                                         "Chinese",
                                                         "Other ethnic group"
                                                       ),
                                                       .details_only = FALSE)
{
  all_ethnicity_categories <- list(
    White = c("White",
              "British",
              "Irish",
              "Any other white background"),
    Mixed = c(
      "Mixed",
      "White and Black Caribbean",
      "White and Black African",
      "White and Asian",
      "Any other mixed background"
    ),
    `Asian or Asian British` = c(
      "Asian or Asian British",
      "Indian",
      "Pakistani",
      "Bangladeshi",
      "Any other Asian background"
    ),
    `Black or Black British` = c(
      "Caribbean",
      "Black or Black British",
      "African",
      "Any other Black background"
    ),
    Chinese = c("Chinese"),
    `Other ethnic group` = c("Other ethnic group"),
    Do_not_know_Prefer_not_to_answer = c("Do not know",
                                         "Prefer not to answer")
  )

  if (.details_only) {
    return(all_ethnicity_categories)
  }

  all_ethnicity_categories_vector <- purrr::reduce(all_ethnicity_categories, c)

  # validate args
  assertthat::assert_that(is.factor(ukb_main[[old_ethnic_background_col]]),
                          msg = paste0("Error! ",
                                       old_ethnic_background_col,
                                       " must be a factor"))

  ethnic_background_col_unique_values <- unique(as.character(ukb_main[[old_ethnic_background_col]]))
  unrecognised_ethnicity_values <- subset(ethnic_background_col_unique_values,
                                          (!ethnic_background_col_unique_values %in% all_ethnicity_categories_vector) &
                                            (!is.na(ethnic_background_col_unique_values)))

  assertthat::assert_that(rlang::is_empty(unrecognised_ethnicity_values),
                          msg = paste0("Error! Column ",
                                       old_ethnic_background_col,
                                       " contains values that are not listed in UKB data coding 1001: ",
                                       stringr::str_c(unrecognised_ethnicity_values,
                                                      sep = "",
                                                      collapse = ", ")))

  assertthat::assert_that(length(unique(ethnicity_levels)) == length(ethnicity_levels),
                          msg = "Error! `ethnicity_levels` contains duplicate values")

  assertthat::assert_that(
    all(ethnicity_levels %in% c(
      "White",
      "Mixed",
      "Asian or Asian British",
      "Black or Black British",
      "Chinese",
      "Other ethnic group"
    )),
    msg = "Error! `ethnicity_levels` contains unrecognised values"
  )

  # simplify ethnicity
  ukb_main[[new_ethnic_background_col]] <-  dplyr::case_when(
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$White ~ "White",
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$Mixed ~ "Mixed",
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$`Asian or Asian British` ~ "Asian or Asian British",
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$`Black or Black British` ~ "Black or Black British",
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$Chinese ~ "Chinese",
    ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$`Other ethnic group` ~ "Other ethnic group",
    (ukb_main[[old_ethnic_background_col]] %in% all_ethnicity_categories$Do_not_know_Prefer_not_to_answer) |
      is.na(ukb_main[[old_ethnic_background_col]]) ~ as.character(NA),
    TRUE ~ "error"
  )

  return(ukb_main)
}

# DEV ---------------------------------------------------------------------

named_vector_to_string <- function(x) {
  stopifnot(!is.null(names(x)))
  x %>%
    as.list() %>%
    purrr::imap_chr( ~ paste(.y, "=", .x)) %>%
    stringr::str_c(sep = "", collapse = "; ")
}

derived_var_details_to_data_dict <- function(derived_var_details) {
  # `derived_var_details` should be a list created by one of the `derive_` functions
  derived_var_details$new_columns %>%
    # loop through new columns
  purrr::map( ~ {
    # convert value labels (codings) to a single string
    if (any(!is.na(derived_var_details$new_columns$dob_derived$value_labels))) {
      derived_var_details$new_columns$dob_derived$value_labels <- named_vector_to_string(derived_var_details$new_columns$dob_derived$value_labels)
    }

    # convert details to a data frame
    as.data.frame(derived_var_details$new_columns$dob_derived) %>%
      dplyr::rename(Field = label,
                    Coding = value_labels)
  }) %>%
    dplyr::bind_rows(.id = "colheaders_raw") %>%
    dplyr::mutate("Notes" = paste0("Required FieldIDs: ",
                                   toString(derived_var_details$required_field_ids)))
}

# DERIVED VARIABLES DATA DICTIONARY ---------------------------------------

