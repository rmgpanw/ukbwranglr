globalVariables(
  c(".", ".data", ".SD", "new_col", "all_lkps_maps_v2", "..selected_cols")
)

#' ukbwranglr: a package for UK Biobank data wrangling
#'
#' This package provides three categories of helpful functions:
#'
#' \enumerate{
#'
#' \item Reading UK Biobank data into R - \code{\link{make_data_dict}} and
#' \code{\link{read_ukb}}
#'
#' \item Summarising numerical variables -
#' \code{\link{summarise_numerical_variables}}
#'
#' \item Identifying phenotypic outcomes - \code{\link{tidy_clinical_events}},
#' \code{\link{make_clinical_events_db}} and \code{\link{extract_phenotypes}}
#'
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @docType package
#' @name ukbwranglr
NULL
