library(magrittr)

# SETUP -------------------------------------------------------------------

ukb_code_mappings <- ukbwranglr::get_ukb_code_mappings()

# FIELD_ID_GROUPS ---------------------------------------------------------

CLINICAL_EVENTS_FIELD_IDS <- list(
  # for death data, code_fid includes primary and secondary causes
  primary_death_icd10 = c(code_fid = "40001", date_fid = "40000"),
  secondary_death_icd10 = c(code_fid = "40002", date_fid = "40000"),
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
CLINICAL_CODE_MAPPINGS_MAP <- tibble::tribble(
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


# DUMMY CLINICAL EVENTS DATA ----------------------------------------------

# This is now replaced with `dummy_ukb_main_clinical_events` below

# get dummy ukb data (contains all diagnoses columns) and load single eid for testing
# dummy_ukb_data_path <- download_dummy_ukb_data_to_tempdir()
# dummy_ukb_data_dict <- make_data_dict(ukb_main = dummy_ukb_data_path,
#                                       delim = ",",
#                                       ukb_data_dict = ukb_data_dict)
# dummy_ukb_data_2eid <- read_ukb(path = dummy_ukb_data_path,
#                              delim = ",",
#                              data_dict = dummy_ukb_data_dict %>%
#                                dplyr::filter(FieldID == "eid" |
#                                                (instance %in% c("0", "2") &
#                                                   array %in% c("0", "3") &
#                                                   FieldID %in% as.character(purrr::flatten(CLINICAL_EVENTS_FIELD_IDS)))),
#                              ukb_data_dict = ukb_data_dict,
#                              ukb_codings = ukb_codings,
#                              nrows = 2)

# manually create dummy data for 2 eids (copied from `dummy_ukb_data_2eid`
# above, appended operations & date of death fields. Note also that the dummy
# data generated by tofu (https://github.com/spiros/tofu) formats dates as
# DD/MM/YYYY, whereas UKB formats these as YYYY-MM-DD)
DUMMY_UKB_MAIN_CLINICAL_EVENTS <- data.table::data.table(
  eid = c(1, 2),
  cancer_code_self_reported_f20001_0_0 = c(1048, 1046),
  cancer_code_self_reported_f20001_0_3 = c(1005, 1003),
  cancer_code_self_reported_f20001_2_0 = c(1045, 1028),
  cancer_code_self_reported_f20001_2_3 = c(1017, 1039),
  non_cancer_illness_code_self_reported_f20002_0_0 = c(1665, 1383),
  non_cancer_illness_code_self_reported_f20002_0_3 = c(1223, 1352),
  non_cancer_illness_code_self_reported_f20002_2_0 = c(1514, 1447),
  non_cancer_illness_code_self_reported_f20002_2_3 = c(NA, 1165),
  interpolated_year_when_cancer_first_diagnosed_f20006_0_0 = c(2012.8173, 2016.0638),
  interpolated_year_when_cancer_first_diagnosed_f20006_0_3 = c(2007.0874, 2023.1635),
  interpolated_year_when_cancer_first_diagnosed_f20006_2_0 = c(2023.2047, 2024.0358),
  interpolated_year_when_cancer_first_diagnosed_f20006_2_3 = c(2014.7373, 2013.2044),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_0_0 = c(1998.9782, 2011.0121),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_0_3 = c(2003.1527, 2020.502),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_2_0 = c(2011.2636, 1981.1627),
  interpolated_year_when_non_cancer_illness_first_diagnosed_f20008_2_3 = c(2018.786, 1983.0059),
  diagnoses_icd10_f41270_0_0 = c('X715', 'E11'),
  diagnoses_icd10_f41270_0_3 = c('E10', 'M0087'),
  diagnoses_icd9_f41271_0_0 = c('E89115', 'E8326'),
  diagnoses_icd9_f41271_0_3 = c(NA, '75513'),
  date_of_first_in_patient_diagnosis_icd10_f41280_0_0 = c('1955-11-12', '1939-02-16'),
  date_of_first_in_patient_diagnosis_icd10_f41280_0_3 = c('1910-02-19', '1965-08-08'),
  date_of_first_in_patient_diagnosis_icd9_f41281_0_0 = c('1917-10-08', '1955-02-11'),
  date_of_first_in_patient_diagnosis_icd9_f41281_0_3 = c('1969-11-23', '1956-09-12'),
  underlying_primary_cause_of_death_icd10_f40001_0_0 = c('X095', 'A162'),
  underlying_primary_cause_of_death_icd10_f40001_1_0 = c('X095', 'A162'),
  contributory_secondary_causes_of_death_icd10_f40002_0_0 = c('W192', 'V374'),
  contributory_secondary_causes_of_death_icd10_f40002_1_3 = c('X715', NA),
  date_of_death_f40000_0_0 = c('1917-10-08', '1955-02-11'),
  date_of_death_f40000_1_0 = c('1910-02-19', '1965-08-08'),
  date_of_cancer_diagnosis_f40005_0_0 = c('1956-11-24', '1910-10-04'),
  date_of_cancer_diagnosis_f40005_2_0 = c('1962-09-04', NA),
  type_of_cancer_icd10_f40006_0_0 = c('M4815', NA),
  type_of_cancer_icd10_f40006_2_0 = c('C850', 'W192'),
  type_of_cancer_icd9_f40013_0_0 = c('27134', '9626'),
  type_of_cancer_icd9_f40013_2_0 = c('2042', 'E90200'),
  operative_procedures_opcs4_f41272_0_0 = c('A01', 'A023'),
  operative_procedures_opcs4_f41272_0_3 = c('A018', 'A02'),
  date_of_first_operative_procedure_opcs4_f41282_0_0 = c('1956-11-24', '1910-10-04'),
  date_of_first_operative_procedure_opcs4_f41282_0_3 = c('1969-11-23', '1956-09-12'),
  operative_procedures_opcs3_f41273_0_0 = c('001', '0011'),
  operative_procedures_opcs3_f41273_0_3 = c('0081', '0071'),
  date_of_first_operative_procedure_opcs3_f41283_0_0 = c('1969-11-23', '1956-09-12'),
  date_of_first_operative_procedure_opcs3_f41283_0_3 = c('1955-11-12', '1939-02-16'),
  operation_code_f20004_0_0 = c(1102, 1105),
  operation_code_f20004_0_3 = c(1108, 1109),
  interpolated_year_when_operation_took_place_f20010_0_0 = c(2012.8173, 2016.0638),
  interpolated_year_when_operation_took_place_f20010_0_3 = c(2008.2342, NA)
)


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
  # cont_and_int_codings_to_na,
  # cont_and_int_codings_NOT_to_na,

  # nonsense dates
  NONSENSE_DATES,

  # ukb_code_mappings
  ukb_code_mappings_sheet_names,
  ukb_code_mappings_code_types,
  colnames_for_ukb_code_mappings_sheet_names,

  code_type_to_lkp_sheet_map_df,
  CLINICAL_CODE_MAPPINGS_MAP,

  # clinical events schema
  CLINICAL_EVENTS_SOURCES,

  # dummy clinical events data
  DUMMY_UKB_MAIN_CLINICAL_EVENTS,

  internal = TRUE,
  overwrite = TRUE
)
