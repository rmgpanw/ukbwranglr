library(magrittr)

# SETUP -------------------------------------------------------------------

ukb_code_mappings <- ukbwranglr::get_ukb_code_mappings()

# FIELD_ID_GROUPS ---------------------------------------------------------

DIAGNOSES_FIELD_IDS <- c(

  # Death data ICD10
  "40001",
  "40002",

  # HES ICD9
  "41271",
  "41281",

  # HES ICD10
  "41270",
  "41280",

  # self-report non-cancer
  "20002",
  "20008",

  # self-report cancer
  "20001",
  "20006",

  # cancer ICD9
  "40013",
  "40005",

  # cancer ICD10
  "40006",
  "40005"
)



# UKB SPECIAL CODINGS -----------------------------------------------------------------

# Used by read_phenodataset.R
# codings that should, or should not, be converted to NA

cont_and_int_codings_to_na <- c(
  # Continuous
  '13',
  '909',
  '1317',
  # Integer
  '100291',
  '100586',
  '37',
  ## **NOTEre 37: also see notes under 'Other notes' in
  ## `ukb_select_codings_to_na.Rmd` re 'polymorphic data fields'
  '513',
  '485',
  '584',
  '100696',
  '170',
  '42',
  '525',
  '100584',
  '218'
)

cont_and_int_codings_NOT_to_na<- c(
  # Continuous
  '488',
  # Integer
  '100373', # -10 <- 'Less than one'
  '100329', # -10 <- 'Less than an hour a day'
  '528',
  '100290',
  '100306',
  '100567',
  '100569',
  '100353', # number of cigarettes previously smoked daily
  '487',
  '100298',
  '100300',
  '100307',
  '100355', # number of cigarettes currently smoked daily
  '100504',
  '100537',
  '100582',
  '100585',
  '100595',
  '100598',
  '530',
  '946',
  '957',
  '100698',
  '17',
  '1990',
  '402',
  '511',
  '517',
  '6361'
)



# NONSENSE DATES ----------------------------------------------------------

# make list containing nonsense dates
nonsense_dates_categories <- c(
  "PRIMARY_CARE"
)

NONSENSE_DATES <- vector(mode = "list", length = length(nonsense_dates_categories))
names(NONSENSE_DATES) <- nonsense_dates_categories

# populate list
NONSENSE_DATES$PRIMARY_CARE <- c("01/01/1901",
                                 "02/02/1902",
                                 "03/03/1903",
                                 "07/07/2037")

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

# This relates to the clinical events table generated by the `get_XXX_diagnoses` functions

# sources - describe possible values under the `source` column

# "f*" = fieldID with "f" prefix, numerical data_codings are UKB data-codings.
# description and category are copied from the UKB data showcase

# gpc_r2 and gpc_r3 are read2 and 3 codes from the gp_clinical table

clinical_events_sources <- tibble::tribble(
  ~ source, ~data_coding, ~ description, ~ category,
  "f40001", "icd10", "Underlying (primary) cause of death", "Death register",
  "f40002", "icd10", "Contributory (secondary) cause of death", "Death register",
  "f20002", "data_coding_6", "Non-cancer illness code, self-reported", "Medical conditions",
  "f20002_icd10", "icd10", "Non-cancer illness code, self-reported", "Medical conditions",
  "f20001", "data_coding_3", "Cancer code, self-reported", "Medical conditions",
  "f40013", "icd9", "Type of cancer: ICD9", "Cancer register",
  "f40006", "icd10", "Type of cancer: ICD10", "Cancer register",
  "f41270", "icd10", "Diagnoses - ICD10", "Summary Diagnoses - Hospital inpatient - Health-related outcomes",
  "f41271", "icd9", "Diagnoses - ICD9", "Summary Diagnoses - Hospital inpatient - Health-related outcomes",
  "gpc_r2", "read2", "gp_clinical table - `read_2` column", "Primary care",
  "gpc_r3", "read3", "gp_clinical table - `read_3` column", "Primary care"
)

# SAVE AS R/sysdata.rda -------------------------------------------------------

usethis::use_data(
  # Field ID groups
  DIAGNOSES_FIELD_IDS,

  # special ukb codings
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
  clinical_events_sources,

  internal = TRUE,
  overwrite = TRUE
)
