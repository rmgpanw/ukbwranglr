
# Notes for updating dummy data

# Do not use MS Excel as date fields (and possibly others too) get
# auto-formatted. Try something like ModernCsv instead
# (https://www.moderncsv.com/).

# After adding any new dummy data fields, update
# `dummy_Data_Dictionary_Showcase.tsv` and `dummy_Codings.tsv` by running
# `gen_dummy_ukb_data_dict_and_codings.R` in `data-raw` dir. Note that only manually
# selected codings should be kept for ICD/OPCS4 etc to minimise file size. These
# are also used by the codemapper package, so codemapper tests should also be
# checked after any updates here.


# PUBLIC ------------------------------------------------------

#' Dummy UK Biobank data
#'
#' Either read a dummy UK Biobank dataset into R or return the file path only.
#'
#' The following dummy datasets are included with this package:
#'
#' - `dummy_Data_Dictionary_Showcase.tsv`: A subset of fields from the UK
#' Biobank data dictionary (full version available from the UK Biobank [data
#' showcase
#' website](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)).
#'
#' - `dummy_Codings.tsv`: A subset of UK Biobank data codings (full version
#' available from the UK Biobank [data showcase
#' website](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)).
#'
#' - `dummy_ukb_main.tsv`: A dummy main UK Biobank dataset. May be read into R
#' with [read_ukb()]. Tidy clinical events fields with [tidy_clinical_events()].
#'
#' - `dummy_gp_clinical.txt`: A dummy UK Biobank primary care clinical event
#' records dataset.
#'
#' - `dummy_gp_scripts.txt`: A dummy UK Biobank primary care prescription
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
#' library(magrittr)
#'
#' # available dummy datasets
#' dummy_datasets <- c(
#'   "dummy_Data_Dictionary_Showcase.tsv",
#'   "dummy_Codings.tsv",
#'   "dummy_ukb_main.tsv",
#'   "dummy_gp_clinical.txt",
#'   "dummy_gp_scripts.txt"
#' )
#'
#' # read dummy dataset into R
#' get_ukb_dummy("dummy_ukb_main.tsv")
#'
#' # get file path to dummy dataset
#' get_ukb_dummy("dummy_ukb_main.tsv", path_only = TRUE)
#'
#' # read all available dummy dataset into R
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

  file_path <- system.file("extdata",
                           file_name,
                           package = "ukbwranglr")

  if (path_only) {
    return(file_path)
  } else {
    result <- fread_tsv_as_character(file_path)

    # convert eid column to type integer
    if ("eid" %in% names(result)) {
      result$eid <- as.integer(result$eid)
    }

    return(result)
  }
}
