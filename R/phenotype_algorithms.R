# NOTES -------------------------------------------------------------------

#TODO
#

# EXPORTED FUNCTIONS ----------------------------------------------------



#' Ascertain diabetes type from first occurrence fields
#'
#' Mutates an indicator column for diabetes type based on UKB first occurrence
#' fields
#'
#' Categories in indicator column:
#'
#' \itemize{
#'  \item "T1DM" - date recorded for T1DM (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130706}{FieldID 130706}) but no other type of diabetes
#'  \item "T2DM" - date recorded for T2DM (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130708}{FieldID 130708}) but no other type of diabetes
#'  \item "T1_and_T2DM" - dates recorded for both T1DM and T2DM
#'  \item "DM_not_T1/T2" - date for other DM type (FieldIDs \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130712}{130712}, \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130710}{130710}, \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130714}{130714} and \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=132202}{132202}), but no date for either T1 or T2
#'  \item "other_DM_and_T1/T2" - date for T1/T2DM AND other DM type
#'  \item "DM_unspecified" - date for DM unspecified only (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130714}{FieldID 130714})
#' }
#'
#' @inheritParams summarise_rowise
#'
#' @return The UK Biobank dataset (supplied to arg \code{ukb_pheno}) with an additional indicator column
#'   for diabetes type named "diabetes_type_first_occurrence"
#' @export
diabetes_type_first_occurrence <- function(
  ukb_pheno,
  data_dict
) {

  # Fields
  date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0 = get_colnames_for_fieldids("130706", data_dict, scalar_output = TRUE)
  date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0 = get_colnames_for_fieldids("130708", data_dict, scalar_output = TRUE)
  date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0 = get_colnames_for_fieldids("130712", data_dict, scalar_output = TRUE)
  date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0 = get_colnames_for_fieldids("130710", data_dict, scalar_output = TRUE)
  date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0 = get_colnames_for_fieldids("130714", data_dict, scalar_output = TRUE)
  date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0 = get_colnames_for_fieldids("132202", data_dict, scalar_output = TRUE)

  check_required_cols_exist(ukb_pheno,
                            date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0,
                            date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0,
                            date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0,
                            date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0,
                            date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0,
                            date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0)

  # Muate indicator column for diabetes subtype
  ukb_pheno <- ukb_pheno %>%
    dplyr::mutate(
      diabetes_type_first_occurrence = dplyr::case_when(

        # T1DM - no other first occurrence dates for T2DM/secondary diabetes etc
        !is.na(
          .data[[date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0]]
        ) &
          is.na(
            .data[[date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0]]
          ) &
          is.na(
            .data[[date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0]]
          ) &
          is.na(
            .data[[date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0]]
          ) &
          is.na(
            .data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]
          ) &
          is.na(
            .data[[date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0]]
          ) ~ 'T1DM',

        # T2DM - but no other first occurrence dates for T1DM/secondary diabetes etc
        !is.na(
          .data[[date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0]]
          ) &
          is.na(
            .data[[date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0]]
          ) &
          is.na(
            .data[[date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0]]
          ) &
          is.na(
            .data[[date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0]]
          ) &
          is.na(
            .data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]
          ) &
          is.na(
            .data[[date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0]]
          ) ~ 'T2DM',

        # T1_and_T2DM - dates for both T1 and T2, but no other DM type
        !is.na(
          .data[[date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0]]
          ) &
          !is.na(
            .data[[date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0]]
          ) & is.na(
            .data[[date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0]]
          ) &
          is.na(
            .data[[date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0]]
          ) &
          is.na(
            .data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]
          ) &
          is.na(
            .data[[date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0]]
          ) ~ 'T1_and_T2DM',

        # DM_not_T1/T2 - date for other DM, but not T1 or T2 or unspecified first occurrence dates
        is.na(
          .data[[date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0]]
        ) &
          is.na(
            .data[[date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0]]
          ) &
          is.na(
            .data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]
          ) &
          (
            !is.na(
              .data[[date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0]]
            ) |
              !is.na(
                .data[[date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0]]
              ) |
              !is.na(
                .data[[date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0]]
              )
          ) ~ 'DM_not_T1/T2',

        # other_DM_and_T1/T2 - T1 or T2 and one of the 'other DM's' (but not DM unspecified)
        (
          !is.na(
          .data[[date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0]]
        ) |
          !is.na(
            .data[[date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0]]
          )
        ) &
          is.na(
            .data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]
          ) &
          (
            !is.na(
              .data[[date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0]]
            ) |
              !is.na(
                .data[[date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0]]
              ) |
              !is.na(
                .data[[date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0]]
              )
        ) ~ 'other_DM_and_T1/T2',

        # unspecified DM
        !is.na(.data[[date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0]]) ~ 'DM_unspecified'
      )
    )
}

#' Get date of diabetes diagnosis based on first occurrence fields
#'
#' Wrapper around \code{\link{rowise_min_max_date}}. Extracts the earliest date
#' across FieldID's 130706, 130708, 130712, 130710, 130714, 132202. See
#' documentation for \code{\link{diabetes_type_first_occurrence}} also.
#'
#' @inheritParams summarise_rowise
#'
#' @return The input UK Biobank dataset with an additional column for date of
#'   diabetes diagnosis based on first occurrence fields
#' @export
diabetes_diagnosis_date_first_occurrence <- function(ukb_pheno,
                                                     data_dict) {
  # Fields
  date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0 = get_colnames_for_fieldids("130706", data_dict, scalar_output = TRUE)
  date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0 = get_colnames_for_fieldids("130708", data_dict, scalar_output = TRUE)
  date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0 = get_colnames_for_fieldids("130712", data_dict, scalar_output = TRUE)
  date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0 = get_colnames_for_fieldids("130710", data_dict, scalar_output = TRUE)
  date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0 = get_colnames_for_fieldids("130714", data_dict, scalar_output = TRUE)
  date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0 = get_colnames_for_fieldids("132202", data_dict, scalar_output = TRUE)

  check_required_cols_exist(ukb_pheno,
                            date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0,
                            date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0,
                            date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0,
                            date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0,
                            date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0,
                            date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0)

  # combine into single vector
  diabetes_first_occurrence_datecols = c(
    date_E10_first_reported_insulindependent_diabetes_mellitus_f130706_0_0,
    date_E11_first_reported_noninsulindependent_diabetes_mellitus_f130708_0_0,
    date_E13_first_reported_other_specified_diabetes_mellitus_f130712_0_0,
    date_E12_first_reported_malnutritionrelated_diabetes_mellitus_f130710_0_0,
    date_E14_first_reported_unspecified_diabetes_mellitus_f130714_0_0,
    date_O24_first_reported_diabetes_mellitus_in_pregnancy_f132202_0_0
  )

  # get minimum date
  rowise_min_max_date(
    ukb_pheno = ukb_pheno,
    selected_date_cols = diabetes_first_occurrence_datecols,
    new_colname = "diabetes_diagnosis_date_first_occurrence",
    min_max = "pmin"
  )
}

# PRIVATE FUNCTIONS -------------------------------------------------------

