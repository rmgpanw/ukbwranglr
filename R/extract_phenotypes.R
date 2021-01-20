# NOTES -------------------------------------------------------------------

#TODO

# optimise field_id_pivot_longer_multi() - set an 'index' column and filter this for
#non-na values before performing inner_join() e.g. for self-reported non-cancer
#diagnoses, fieldid 20002 would be the index col: if there is no self-reported
#diagnosis, then the associated date and age at diagnosis columns will also be
#empty.

# EXPORTED FUNCTIONS ----------------------------------------------------

#' Apply pivot_longer across multiple sets of columns
#'
#' Helper function.
#'
#' This should only be applied to related FieldIDs which are each associated
#' with the same number of columns. For example, FieldIDs 20002, 20008, and
#' 20009 all pertain to self-reported non-cancer illnesses.
#'
#' Raises and error if the supplied FieldIDs are associated with differing
#' numbers of columns, or if the result has more rows than expected.
#'
#' @section Under the hood:
#'
#'   Maps \code{\link{field_id_pivot_longer}} to a set of FieldIDs, then
#'   recombines the results into a single dataframe using
#'   \code{\link[dplyr]{inner_join}}.
#'
#' @param field_ids Character vector of FieldIDs
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe.
#' @export
#'
#' @family extract disease outcomes helpers
field_id_pivot_longer_multi <- function(field_ids,
                                        ukb_pheno,
                                        data_dict,
                                        ukb_codings) {

  required_cols <- get_colnames_for_fieldids(field_ids = field_ids,
                                             data_dict = data_dict)

  check_required_cols_exist(df = ukb_pheno,
                            required_cols)

  # expected number of rows for each fieldid should be the same - this returns a
  # numerical vector, each number is the number of columns selected for a
  # fieldid
  expected_row_numbers <- field_ids %>%
    purrr::map(get_colnames_for_fieldids,
               data_dict = data_dict) %>%
    purrr::map_dbl(length) %>%
    purrr::map_dbl(~ .x * nrow(ukb_pheno))

  # check these are numeric and all the equal
  assertthat::assert_that(is.numeric(expected_row_numbers))

  if (length(unique(expected_row_numbers)) != 1) {
    stop("Selected FieldIDs have differing numbers of associated columns")
  }

  # pivot_longer and join results into a single dataframe
  result <- field_ids %>%
    purrr::map(field_id_pivot_longer,
               ukb_pheno = ukb_pheno,
               data_dict = data_dict,
               ukb_codings = ukb_codings) %>%
    purrr::reduce(dplyr::inner_join, by = c("eid", "instance_array"))

  # check that row number matches the expected value
  if (nrow(result) != (expected_row_numbers[1])) {
    stop("Result does not contain the expected number of rows. Aborting.")
  }

  return(result)
}


#' Get hospital inpatient ICD10 diagnoses
#'
#' Returns a long format dataframe with all hospital inpatient ICD10 codes and associated dates for
#' each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 41270 and 41280 (hospital inpatient diagnoses
#' and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002}{category 2002:
#' hospital inpatient summary diagnoses}).
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @export
#'
#' @family get diagnoses
get_hes_icd10_diagnoses <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  hes_icd10_fields_ids <- c("41270", "41280")

  # get diagnoses and dates
  hes_icd10_diagnoses <- field_id_pivot_longer_multi(
    field_ids = hes_icd10_fields_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  hes_icd10_diagnoses <- hes_icd10_diagnoses %>%
    filter(!is.na(f41270_value))

  # recode to ICD10
  hes_icd10_diagnoses <- recode_ukbcol(df = hes_icd10_diagnoses,
                col_to_recode = "f41270_value",
                field_id = "41270",
                ukb_data_dict = data_dict,
                ukb_codings = ukb_codings,
                mapping_direction = "meaning_code")

  # standardise
  hes_icd10_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_diagnoses_df = hes_icd10_diagnoses,
    index_col = "f41270",
    code_col = "f41270_value",
    date_col = "f41280_value",
    code_type = "ICD10")

  return(hes_icd10_diagnoses)
}


#' Get self-reported non-cancer diagnoses (ICD10)
#'
#' Returns a long format dataframe with all self-reported non-cancer diagnoses
#' mapped to 3-character ICD10 codes with associated dates for each UK Biobank participant.
#'
#' Reformats the data for FieldIDs 20002 and 20009 (self-reported non-cancer
#' diagnoses and dates, see
#' \href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074:
#' medical conditions - verbal interview}).
#'
#' \strong{Note: not all self-reported medical conditions map to ICD10 codes.}
#' (See \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=609}{data coding
#' 609})
#'
#' @inheritParams summarise_rowise
#' @inheritParams field_id_pivot_longer
#'
#' @return Dataframe
#' @export
#'
#' @family get diagnoses
get_self_report_non_cancer_diagnoses <- function(ukb_pheno,
                                    data_dict,
                                    ukb_codings) {

  # required field_ids (see https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=2002)
  self_report_non_cancer_fields_ids <- c("20002", "20008")

  # get diagnoses and dates
  self_report_non_cancer_diagnoses <- field_id_pivot_longer_multi(
    field_ids = self_report_non_cancer_fields_ids,
    ukb_pheno = ukb_pheno,
    data_dict = data_dict,
    ukb_codings = ukb_codings
  )

  # remove empty code rows
  self_report_non_cancer_diagnoses <- self_report_non_cancer_diagnoses %>%
    filter(!is.na(f20002_value))

  # recode to ukb codes
  self_report_non_cancer_diagnoses <- suppressWarnings(recode_ukbcol(df = self_report_non_cancer_diagnoses,
                                       col_to_recode = "f20002_value",
                                       field_id = "20002",
                                       ukb_data_dict = data_dict,
                                       ukb_codings = ukb_codings,
                                       mapping_direction = "meaning_code"))

  # ...now recode to ICD10
  mapping_df <- ukb_codings %>%
    dplyr::filter(Coding == 609 & Value != -1) %>%
    dplyr::select(old_vals = Value,
                  new_vals = Meaning)


  self_report_non_cancer_diagnoses <- suppressWarnings(recode_column(df = self_report_non_cancer_diagnoses,
                                                    col_to_recode = "f20002_value",
                                                    mapping_df = mapping_df))

  # standardise
  self_report_non_cancer_diagnoses <- get_diagnoses_set_index_code_date_cols(
    get_diagnoses_df = self_report_non_cancer_diagnoses,
    index_col = "f20002",
    code_col = "f20002_value",
    date_col = "f20008_value",
    code_type = "ICD10")

  # convert date_col from decimal to date type
  self_report_non_cancer_diagnoses <- self_report_non_cancer_diagnoses %>%
    dplyr::mutate(date = lubridate::date_decimal(date))

  return(self_report_non_cancer_diagnoses)
}

# PRIVATE FUNCTIONS -------------------------------------------------------

#' Convert column for a FieldID to long format
#'
#' Helper function. Selects the columns relating to a specified FieldID (plus the 'eid' column)
#' and applies \code{\link[tidyr]{pivot_longer}}.
#'
#' @param field_id Character. A UK Biobank Field ID
#' @inheritParams read_pheno
#' @inheritParams summarise_rowise
#'
#' @return A dataframe with 3 columns: \itemize{ \item eid \item column names
#'   (labelled as the specified `field_id`, prefixed by 'f') \item column values
#'   (labelled as he specified `field_id`, prefixed by 'f' and suffixed by
#'   '_value') }
#'
#' @family extract disease outcomes helpers
field_id_pivot_longer <- function(ukb_pheno,
                                 field_id,
                                 data_dict,
                                 ukb_codings) {

  # check a single field_id has been supplied
  assertthat::is.string(field_id)

  # filter ukb_pheno for selected cols (eid + fieldid cols)
  required_cols <- get_colnames_for_fieldids(field_id = field_id,
                                            data_dict = data_dict)

  check_required_cols_exist(df = ukb_pheno,
                            required_cols)

  ukb_pheno <- ukb_pheno %>%
    dplyr::select(eid,
           tidyselect::all_of(required_cols))

  # get codings for fieldid
  field_id_codings <- ukb_codings %>%
    dplyr::filter(Coding == (data_dict %>%
                        dplyr::filter(FieldID == field_id) %>%
                        .$Coding %>%
                        head(n = 1)))

  ukb_pheno <- ukb_pheno %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(required_cols),
      values_to = paste(paste0("f", field_id), "value", sep = "_")
    ) %>%
    dplyr::mutate(instance_array = colname_to_field_inst_array_df(name)$instance_array)

  # rename
  names(ukb_pheno)[which(names(ukb_pheno) == 'name')] <- paste0("f", field_id)

  return(ukb_pheno)
}

#' Standardise a long format diagnoses dataframe
#'
#' Helper function
#'
#' Selects essential columns: eid, field_id, code and date recorded. The code
#' column should be named as the code type (e.g. "ICD10")
#'
#' @param get_diagnoses_df
#' @param index_col Column indicating diagnosis source (e.g. HES, self-report)
#' @param code_col Column of codes
#' @param date_col Column of dates when code was recorded
#' @param code_type Character. Type of coding
#' @family get diagnoses
get_diagnoses_set_index_code_date_cols <- function(get_diagnoses_df,
                                                   index_col,
                                                   code_col,
                                                   date_col,
                                                   code_type) {
  # select required columns and rename
  get_diagnoses_df <- get_diagnoses_df %>%
    select(eid,
           .data[[index_col]],
           code = .data[[code_col]],
           date = .data[[date_col]])

  # rename code col with code type
  names(get_diagnoses_df)[which(names(get_diagnoses_df) == "code")] <- code_type

  return(get_diagnoses_df)
}
