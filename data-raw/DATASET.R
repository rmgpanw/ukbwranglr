library(magrittr)

# SETUP -------------------------------------------------------------------

ukb_code_mappings <- ukbwranglr::get_ukb_code_mappings()

# FIELD_ID_GROUPS ---------------------------------------------------------

CLINICAL_EVENTS_FIELD_IDS <- list(
  death_icd10 = c(primary_code_fid = "40001", secondary_code_fid = "40002"),
  self_report_non_cancer = c(code_fid = "20002", date_fid = "20008"),
  self_report_non_cancer_icd10 = c(code_fid = "20002", date_fid = "20008"),
  self_report_cancer = c(code_fid = "20001", date_fid = "20006"),
  self_report_operation = c(code_fid = "20004", date_fid = "20010"),
  cancer_register_icd9 = c(code_fid = "40013", date_fid = "40005"),
  cancer_register_icd10 = c(code_fid = "40006", date_fid = "40005"),
  summary_hes_icd9 = c(code_fid = "41271", date_fid = "41281"),
  summary_hes_icd10 = c(code_fid = "41270", date_fid = "41280"),
  summary_hes_opcs3 = c(code_fid = "41273", date_fid = "41283"),
  summary_hes_opcs4 = c(code_fid = "41272", date_fid = "41282")
)

# NONSENSE DATES ----------------------------------------------------------

# make list containing nonsense dates
nonsense_dates_categories <- c(
  "PRIMARY_CARE",
  "MAIN_DATASET"
)

NONSENSE_DATES <- vector(mode = "list", length = length(nonsense_dates_categories))
names(NONSENSE_DATES) <- nonsense_dates_categories

# populate list
NONSENSE_DATES$PRIMARY_CARE <- c("01/01/1901",
                                 "02/02/1902",
                                 "03/03/1903",
                                 "07/07/2037")

# from codings 1313, 272, 586 and 819
NONSENSE_DATES$MAIN_DATASET <- c(
  '1904-04-04',
  '1900-01-01',
  '1910-01-01',
  '1920-01-01',
  '1930-01-01',
  '1901-01-01',
  '1902-02-02',
  '1903-03-03',
  '2037-07-07')

# UKB CODE MAPPINGS -------------------------------------------------------

# used by functions in code_mappings.R

# NOTES: if editing this, always refer to `ukb_code_mappings_sheet_names` and
# `ukb_code_mappings_code_types` first - these should contain all possible code
# types and sheets from the UKB excel file (resource 592). Tests in
# test_data_raw_constants.R reply on the accuracy of these.

# names of excel spreadsheets in ukb_code_mappings (UKB resource 592) -----

# I generated this by manually copying `names(get_ukb_code_mappings())`
ukb_code_mappings_sheet_names <- c(
  "bnf_lkp",
  "dmd_lkp",
  "icd9_lkp",
  "icd10_lkp",
  "icd9_icd10",
  "read_v2_lkp",
  "read_v2_drugs_lkp",
  "read_v2_drugs_bnf",
  "read_v2_icd9",
  "read_v2_icd10",
  "read_v2_opcs4",
  "read_v2_read_ctv3",
  "read_ctv3_lkp",
  "read_ctv3_icd9",
  "read_ctv3_icd10",
  "read_ctv3_opcs4",
  "read_ctv3_read_v2"
)

assertthat::assert_that(
  all(sort(ukb_code_mappings_sheet_names) == sort(names(ukb_code_mappings))),
  msg = "`ukb_code_mappings_sheet_names` does not match the sheet names in resource 592 (fetched with `get_ukb_code_mappings`"
)


# colnames for each excel spreadsheet in resource 592 ---------------------
colnames_for_ukb_code_mappings_sheet_names <- ukb_code_mappings_sheet_names %>%
  purrr::set_names() %>%
  purrr::map(~ names(ukb_code_mappings[[.x]]))


# clinical codes types ----------------------------------------------------
# my labels for clinical codes types, used in the constants below
ukb_code_mappings_code_types <- c(
  "bnf",
  "dmd",
  "icd9",
  "icd10",
  "read2",
  "read2_drugs",
  "read3",
  "opcs4"
)

# clinical code system to lookup sheet map --------------------------------
# mappings note, BNF - 'description_col' is for chemical substances only (TODO
# amend this?)
code_type_to_lkp_sheet_map_df <- tibble::tribble(
  ~ code, ~ lkp_sheet, ~ code_col, ~ description_col, ~ preferred_synonym_col, ~ preferred_code,
  "bnf", "bnf_lkp", "BNF_Presentation_Code", "BNF_Chemical_Substance", NA, NA,
  "dmd", "dmd_lkp", "concept_id", "term", NA, NA,
  "icd9", "icd9_lkp", "ICD9", "DESCRIPTION_ICD9", NA, NA,
  "icd10", "icd10_lkp", "ICD10_CODE", "DESCRIPTION", NA, NA,
  "read2", "read_v2_lkp", "read_code", "term_description", "term_code", "00",
  "read2_drugs", "read_v2_drugs_lkp", "read_code", "term_description", NA, NA,
  "read3", "read_ctv3_lkp", "read_code", "term_description", "description_type", "P"
)

# clinical code mappings map ----------------------------------------------

# used by `map_codes()`
# 'from' and 'to' cols: possible mapping combinations
# 'mapping_sheet': the appropriate mapping sheet to use for a 'from'/'to' combination
# 'from_col' and 'to_col': the columns to use when mapping
# Note, `preferred_synonym_col` and `preferred_code` refer to `to_col`
clinical_code_mappings_map <- tibble::tribble(
  ~ from, ~ to, ~ mapping_sheet, ~ from_col, ~ to_col, ~ preferred_synonym_col, ~ preferred_code,
  "icd9", "icd10", "icd9_icd10", "ICD9", "ICD10", NA, NA,
  "read2_drugs", "bnf", "read_v2_drugs_bnf", "read_code", "bnf_code", NA, NA,
  "read2", "icd9", "read_v2_icd9", "read_code", "icd9_code", NA, NA,
  "read2", "icd10", "read_v2_icd10", "read_code", "icd10_code", NA, NA,
  "read2", "opcs4", "read_v2_opcs4", "read_code", "opcs_4.2_code", NA, NA,
  "read2", "read3", "read_v2_read_ctv3", "READV2_CODE", "READV3_CODE", "TERMV3_TYPE", "P",
  "read3", "icd9", "read_ctv3_icd9", "read_code", "icd9_code", NA, NA,
  "read3", "icd10", "read_ctv3_icd10", "read_code", "icd10_code", NA, NA,
  "read3", "opcs4", "read_ctv3_opcs4", "read_code", "opcs4_code", NA, NA,
  "read3", "read2", "read_ctv3_read_v2", "READV3_CODE", "READV2_CODE", "TERMV2_TYPE", "P"
)

# see `test_data_raw_DATASET.R` for tests (check nothing misspelled etc)


# CLINICAL EVENTS SCHEMA --------------------------------------------------

# This relates to the clinical events table generated by the `tidy_clinical_events`

# sources - describe possible values under the `source` column

# "f*" = fieldID with "f" prefix, numerical data_codings are UKB data-codings.
# description and category are copied from the UKB data showcase

# gpc_r2 and gpc_r3 are read2 and 3 codes from the gp_clinical table

CLINICAL_EVENTS_SOURCES <- tibble::tribble(
  ~ source, ~data_coding, ~ description, ~ category, ~ file,
  "f40001", "icd10", "Underlying (primary) cause of death", "Death register", "ukb_main",
  "f40002", "icd10", "Contributory (secondary) cause of death", "Death register", "ukb_main",
  "f20002", "data_coding_6", "Non-cancer illness code, self-reported", "Medical conditions", "ukb_main",
  "f20002_icd10", "icd10", "Non-cancer illness code, self-reported", "Medical conditions", "ukb_main",
  "f20001", "data_coding_3", "Cancer code, self-reported", "Medical conditions", "ukb_main",
  "f20004", "data_coding_5", "Operation code, self-reported", "Operations", "ukb_main",
  "f40013", "icd9", "Type of cancer: ICD9", "Cancer register", "ukb_main",
  "f40006", "icd10", "Type of cancer: ICD10", "Cancer register", "ukb_main",
  "f41270", "icd10", "Diagnoses - ICD10", "Summary Diagnoses - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f41271", "icd9", "Diagnoses - ICD9", "Summary Diagnoses - Hospital inpatient - Health-related outcomes", "ukb_main",
  "gpc_r2", "read2", "`read_2` column", "Primary care", "gp_clinical",
  "gpc_r3", "read3", "`read_3` column", "Primary care", "gp_clinical",
  "f41272", "opcs4", "Operative procedures - OPCS4", "Summary Operations - Hospital inpatient - Health-related outcomes", "ukb_main",
  "f41273", "opcs3", "Operative procedures - OPCS3", "Summary Operations - Hospital inpatient - Health-related outcomes", "ukb_main"
)

# SAVE AS R/sysdata.rda -------------------------------------------------------

usethis::use_data(
  # Field ID groups
  CLINICAL_EVENTS_FIELD_IDS,

  # TODO - review these special ukb codings
  cont_and_int_codings_to_na,
  cont_and_int_codings_NOT_to_na,

  # nonsense dates
  NONSENSE_DATES,

  # ukb_code_mappings
  ukb_code_mappings_sheet_names,
  ukb_code_mappings_code_types,
  colnames_for_ukb_code_mappings_sheet_names,

  code_type_to_lkp_sheet_map_df,
  clinical_code_mappings_map,

  # clinical events schema
  CLINICAL_EVENTS_SOURCES,

  internal = TRUE,
  overwrite = TRUE
)
