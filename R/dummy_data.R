
# Notes for updating dummy data

# Do not use MS Excel as date fields (and possibly others too) get
# auto-formatted. Try something like ModernCsv instead
# (https://www.moderncsv.com/).

# After adding any new dummy data fields, update
# `dummy_Data_Dictionary_Showcase.tsv` and `dummy_Codings.tsv` by un-hashing the
# following, then re-run tests. Note that only manually selected codings should
# be kept for ICD/OPCS4 etc to minimise file size. These are also used by the
# codemapper package, so codemapper tests should also be checked after any
# updates here.


# library(tidyverse)
#
# # 'large' codings that require manual selection - note that ICD10/9 should *not*
# # be manually added to `dummy_Codings.tsv`. These are instead included in
# # `dummy_all_lkps_maps_v3.xlsx` for codemapper package.
# large_codings <- c(
#   # ICD10
#   "19",
#
#   # ICD9
#   "87",
#
#   # OPCS4
#   "240",
#
#   # OPCS3
#   "259",
#
#   # Self-reported cancer, non-cancer, medication, operation
#   "3",
#   "6",
#   "4",
#   "5"
# )
#
# # get UKB data dict and codings
# ukb_data_dict <- get_ukb_data_dict()
# ukb_codings <- get_ukb_codings()
#
# # get dummy ukb_main and make data_dict
# dummy_ukb_main_data_dict <- get_ukb_dummy(file_name = "dummy_ukb_main.tsv") %>%
#   make_data_dict()
#
# # filter ukb_data_dict and ukb_codings for required fields/codings, and write to
# # tsv files
#
# dummy_data_dir <- "inst/extdata/dummy_tsv"
#
# # data dict
# dummy_Data_Dictionary_Showcase <- ukb_data_dict %>%
#   filter(FieldID %in% dummy_ukb_main_data_dict$FieldID)
#
# dummy_Data_Dictionary_Showcase %>%
#   write_tsv(file.path(dummy_data_dir, "dummy_Data_Dictionary_Showcase.tsv"))
#
# # codings
# dummy_Codings <- ukb_codings %>%
#   filter(Coding %in% dummy_ukb_main_data_dict$Coding) %>%
#
#   # do not include ICD/Read/OPCS4 codings
#   filter(!Coding %in% large_codings)
#
# # large codings to keep
# large_codings_to_keep <- get_ukb_dummy(file_name = "dummy_Codings.tsv") %>%
#   filter(Coding %in% large_codings)
#
# dummy_Codings %>%
#   bind_rows(large_codings_to_keep) %>%
#   write_tsv(file.path(dummy_data_dir, "dummydummy_Codings.tsv"))


# PUBLIC ------------------------------------------------------

#' Dummy UK Biobank data
#'
#' Either read a dummy UK Biobank dataset into R or return the file path only.
#'
#' The following dummy datasets are included with this package:
#'
#' * `dummy_Data_Dictionary_Showcase.tsv`: A subset of fields from the UK
#' Biobank data dictionary (full version available from the UK Biobank [data
#' showcase
#' website](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)).
#'
#' * `dummy_Codings.tsv`: A subset of UK Biobank data codings (full version
#' available from the UK Biobank [data showcase
#' website](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)).
#'
#' * `dummy_ukb_main.tsv`: A dummy main UK Biobank dataset. May be read into R
#' with [read_ukb()]. Tidy xlinical events fields with [tidy_clinical_events()].
#'
#' * `dummy_gp_clinical.txt`: A dummy UK Biobank primary care clinical event
#' records dataset.
#'
#' * `dummy_gp_scripts.txt`: A dummy UK Biobank primary care prescription
#' records dataset.
#'
#' @param file_name Name of dummy dataset file.
#' @param path_only If `TRUE`, return the file path to the dummy dataset file,
#'   otherwise if `FALSE` (default), read the dummy dataset into R.
#'
#' @return A data frame if `path_only` is `FALSE` (default) or a string if
#'   `path_only` is `TRUE`.
#' @export
#'
#' @examples
#' # available dummy datasets
#' dummy_datasets <- c(
#'   "dummy_Data_Dictionary_Showcase.tsv",
#'   "dummy_Codings.tsv",
#'   "dummy_ukb_main.tsv",
#'   "dummy_ukb_main_clinical_events.tsv",
#'   "dummy_gp_clinical.txt",
#'   "dummy_gp_scripts.txt"
#' )
#'
#' # file paths
#' dummy_datasets %>%
#'   purrr::map_chr(get_ukb_dummy, path_only = TRUE)
#'
#' # read into R
#' dummy_datasets %>%
#'   purrr::set_names() %>%
#'   purrr::map(get_ukb_dummy, path_only = FALSE) %>%
#'   purrr::map(tibble::as_tibble)
get_ukb_dummy <- function(file_name,
                          path_only = FALSE) {
  match.arg(file_name,
            choices = c(
              "dummy_Data_Dictionary_Showcase.tsv",
              "dummy_Codings.tsv",
              "dummy_ukb_main.tsv",
              "dummy_gp_clinical.txt",
              "dummy_gp_scripts.txt"
            ))

  file_path <- system.file(file.path("extdata", "dummy_tsv"),
                           file_name,
                           package = "ukbwranglr")

  if (path_only) {
    return(file_path)
  } else {
    return(fread_tsv_as_character(file_path))
  }
}
