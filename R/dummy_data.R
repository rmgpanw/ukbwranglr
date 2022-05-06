

# CONSTANTS ---------------------------------------------------------------

## DUMMY CLINICAL EVENTS DATA ----------------------------------------------

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

  treatment_medication_code_f20003_0_0 = c(1140861958, 1141146234),
  treatment_medication_code_f20003_2_0 = c(1141146188, 1141184722),
  treatment_medication_code_f20003_2_3 = c(1141184722, 1140861958),
  date_of_attending_assessment_centre_f53_0_0 = c('1955-02-11', '1965-08-08'),
  date_of_attending_assessment_centre_f53_2_0 = c('1910-02-19', '1915-03-18'),

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

# PUBLIC ------------------------------------------------------

#' Example clinical events data from a UK Biobank main dataset
#'
#' Example dummy clinical events data that can be tidied with \code{\link{tidy_clinical_events}}
#'
#' @return A data frame of dummy data
#' @export
#'
#' @seealso \code{\link{tidy_clinical_events}}
#'
#' @examples
#' dummy_main_dataset_clinical_events()
dummy_main_dataset_clinical_events <- function() {
  DUMMY_UKB_MAIN_CLINICAL_EVENTS
}

# PRIVATE -------------------------------------------------------

