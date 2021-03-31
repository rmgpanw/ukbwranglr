#? Functions to extract summary values from a ukb phenotype file

# Notes -------------------------------------------------------------------

# - Should be used on a **cleaned** and **renamed** ukb_df (i.e. processed by ukb_parase() with arguments rename_header = TRUE, clean_dates = TRUE and clean_continuous = TRUE,)
# - Mutate DOB
# - Functions require mapping_df. Base field selections on FieldID's (fixed by UKB i.e. should not change over time) rather than my own made-up colnames

# Load dependencies --------------------------------------------------
# library(stringr)
# library(dplyr)
# library(lubridate)

# Functions ---------------------------------------------------------------
#' Mutate DOB column from a processed UKB df
#'
#' @param ukb_df a **cleaned** and **renamed** ukb phenotypes_file
#' @param ukb_mapping_df a ukb mapping file generated with ukb_mapping_generator()
#'
#' @return the ukb df with a new (DOB) column
#' @export
ukb_mutate_dob <- function(ukb_df, ukb_mapping_df) {
  # Mutates approximate DOB column from FieldID's 34 and 52 (year and month of birth)

  # PARAMETERS:
  ## ukb_df: a **cleaned** and **renamed** ukb phenotypes_file
  ## ukb_mapping_df: a ukb mapping file generated with ukb_mapping_generator()

  # MAIN BODY
  ## Find year/month of birth columns
  selected_cols <- ukb_mapping_df %>%
    dplyr::filter(.data[["FieldID"]] %in% c('34', '52')) %>%
    .$descriptive_colnames

  ## make ukb_df_temp with only these 2 fields and mutate dob
  ukb_df_temp <- ukb_df %>%
    dplyr::select(tidyr::contains(selected_cols)) # select fields

  names(ukb_df_temp) <- c('yob', 'mob') # rename

  ukb_df_temp <- ukb_df_temp %>%
    dplyr::mutate(dob = paste(yob,
                       as.integer(mob), # need to extract integer value e.g. 'January' == 1
                       01, # first day of month
                       sep = '-'))

  ukb_df_temp$dob <- lubridate::ymd(ukb_df_temp$dob) # convert to date format

  ## join dob back to ukb_df
  ukb_df$dob <- ukb_df_temp$dob

  ## return ukb_df
  return(ukb_df)
}

#' Mutate mean values across instances for numerical UKB fields
#'
#' @param ukb_df a **cleaned** and **renamed** ukb phenotypes_file
#' @param ukb_mapping_df a ukb mapping file generated with ukb_mapping_generator()
#'
#' @return the UKB df with additional columns with mean values across instances for numerical UKB fields
#' @export
ukb_mutate_numerical_means <- function(ukb_df,
                               ukb_mapping_df) {
  # Function to mutate mean values for numerical fields in a ukb phenotype file ***WITHOUT SPECIAL CODING VALUES***

  # Parameters:
  ## ukb_df: a **cleaned** and **renamed** ukb phenotypes_file
  ## ukb_mapping_df: a ukb mapping file generated with ukb_mapping_generator()

  # Functions (private) ---------------------------------------------------------------

  ## nest ukb_mapping_df by Field and mutate names for mean_cols
  nest_mapping_df_by_field <- function(.ukb_mapping_df) {
    .ukb_mapping_df %>%
    dplyr::filter(
      (.data[["ValueType"]] %in% c('Continuous', 'Integer')) &
        (
      #***IMPORTANT*** filters for either:
      ## 1. Cols without special coding values
      (is.na(.data[["Coding"]])) |
      ## 2. Cols with special coding values that have been 'cleaned' to 'NA' y `ukb_parse()`
          (!is.na(.data[["Coding"]]) & cont_int_to_na == TRUE)
      ## ...i.e. filters out any continuous/integer cols with remaining 'uncleaned' special values
      )) %>%
    dplyr::group_by(Field_FieldID) %>%
    tidyr::nest() %>%
    dplyr::mutate(new_colname = paste('mean', Field_FieldID, sep = '_'))
  }

  ## extract vector of descriptive_colnames matching a Field
  extract_descriptive_colnames_for_field <- function(.ukb_mapping_df_nested,
                                                     .field) {
    .ukb_mapping_df_nested[(.ukb_mapping_df_nested$Field_FieldID == .field), ][[2]][[1]][['descriptive_colnames']]
  }

  # mutate mean columns (his is a messy function...)
  mutate_mean_col <- function(.ukb_df, .mean_col_name, .selected_cols) {
    # assign temp name to new col
    .ukb_df <- .ukb_df %>%
    dplyr::mutate(new_col = rowMeans(dplyr::across(tidyselect::all_of(.selected_cols)),
                                     na.rm = TRUE))

    # rename (messy, but works...)
    names(.ukb_df)[length(names(.ukb_df))] <- .mean_col_name

    # return result
    return(.ukb_df)
  }

  # Main body of function ---------------------------------------------------
  ## nest_mapping_df by Field
  ukb_mapping_df_nested <- nest_mapping_df_by_field(.ukb_mapping_df = ukb_mapping_df)

  ## loop through Fields to mutate mean value columns
  for (Field_FieldID in ukb_mapping_df_nested$Field_FieldID) {
    ukb_df <- mutate_mean_col(.ukb_df = ukb_df,
                              .selected_cols = extract_descriptive_colnames_for_field(.ukb_mapping_df_nested = ukb_mapping_df_nested,
                                                                                      .field = Field_FieldID),
                              .mean_col_name = ukb_mapping_df_nested[(ukb_mapping_df_nested$Field_FieldID == Field_FieldID), ]$new_colname
    )
  }

  ## warning message
  cat('\n\nReminder: please ensure that "ukb_df" has been "cleaned" and renamed by ukb_parse().
  Also, any columns with special values that have not been converted to "NA" will not have been summarised by this function')

  ## return result
  return(ukb_df)
}

ukb_mutate_numerical_summaries <- function(.ukb_df) {
  # idea is to use rowwise() and pass a list of functions e.g. n(), min(), max():
  # ...however, will be very slow compared to rowMeans(). Unhash below to explore

  # # cols to summarise e.g. hba1c: FieldID == 30750
  # ukb_mapping_df %>%
  #   filter(grepl(pattern = 'hba1c', x = Field, ignore.case = TRUE))
  #
  # # vector of colnames
  # selected_cols_hba1c <- ukb_mapping_df %>%
  #   filter(grepl(pattern = 'hba1c', x = Field, ignore.case = TRUE)) %>%
  #   .$descriptive_colnames
  #
  # # cols to summarise e.g. bmi: FieldID == 21001
  # ukb_mapping_df %>%
  #   filter(grepl(pattern = 'bmi', x = Field, ignore.case = TRUE))
  #
  # # vector of colnames
  # selected_cols_bmi <- ukb_mapping_df %>%
  #   filter(grepl(pattern = 'bmi', x = Field, ignore.case = TRUE)) %>%
  #   .$descriptive_colnames
  #
  # ################################
  #
  # # mutate min, max, n, mean cols
  # ## fast
  # ukb_main_filter_parsed_CLEAN %>%
  #   mutate('mean_hba1c' = rowMeans(across(all_of(selected_cols_hba1c)), na.rm = TRUE)) %>%
  #   select(selected_cols, mean_hba1c)
  #
  # ## very slow
  # ukb_main_filter_parsed_CLEAN %>%
  #   rowwise() %>%
  #   mutate(mean_hba1c = 'mean'(c_across(cols = selected_cols_hba1c), na.rm = TRUE)) %>%
  #   select(selected_cols, mean_hba1c)
}

ukb_diabetes_summary <- function(ukb_df,
                                 ukb_mapping_df) {
  # Function to mutate indicator column for DM type (1/2/1+2/other) and earliest date of diagnosis
  # based on the the ['First occurrence'](http://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=1712) fields

  # Parameters:
  ## ukb_df: a **cleaned** and **renamed** ukb phenotypes_file
  ## ukb_mapping_df: a ukb mapping file generated with ukb_mapping_generator()

  # Functions (private) ---------------------------------------------------------------

}
