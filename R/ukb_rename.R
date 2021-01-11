# ?R function to rename columns of (subset of) UKB main datafile. Takes arguments for associated data dictionary and data object e.g. df_renamed <- ukb_rename(df, dict)
# version 1.0 14/09/2020


# Load libraries ----------------------------------------------------------
# library(stringr)


# Function ----------------------------------------------------------------
#' Rename columns of a ukb df
#'
#' @param ukb_df a UKB main dataset
#' @param ukb_df_dict a UKB main dataset specific to ukb_df created with `ukb_data_dictionary.R`
#'
#' @return a renamed ukb df
#' @export
ukb_rename <- function(ukb_df, ukb_df_dict) {
  # Parameters
  ## ukb_df: a UKB main dataset
  ## ukb_df_dict: a UKB main dataset specific to ukb_df created with `ukb_data_dictionary.R`

  # 1) make vector of column names
  ## start by combining 'FieldID', 'Field' and 'instance.array_indices' in data dictionary
  column_names <- paste(
    ukb_df_dict$Field,
    ukb_df_dict$FieldID,
    ukb_df_dict$instance.array_indices
  )

  ## add "eid" to start
  covariate_names <- c("eid", covariate_names)

  # 2) replace special characters
  ## characters to be replaced with "_"
  to_underscore <- c(" ")

  for (string in to_underscore) {
    column_names <- stringr::str_replace_all(column_names, string, "_")
  }

  ## characters to replace with "" (i.e. to remove)
  to_remove <- c(
    "\\(",
    "\\)",
    "\\-",
    ","
  )

  for (string in to_underscore) {
    column_names <- stringr::str_replace_all(column_names, string, "")
  }

  # 3) replace column names for ukb_df
  names(ukb_df) <- column_names

  # 4) return renamed ukb_df
  return(ukb_df)
}
