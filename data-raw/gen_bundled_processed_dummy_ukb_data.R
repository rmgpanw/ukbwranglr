# GENERATE PROCESSED DUMMY UKB DATA FOR PACKAGE (lazy loaded)


# SETUP -------------------------------------------------------------------

library(ukbwranglr)

ukb_data_dict = get_ukb_data_dict()

ukb_codings = get_ukb_codings()

dummy_data_dict <- make_data_dict("data-raw/dummy_ukb_data.csv",
                                   delim = ",",
                                  ukb_data_dict = ukb_data_dict)

dummy_ukb_data <-
  ukbwranglr::read_pheno(
    "data-raw/dummy_ukb_data.csv",
    data_dict = dummy_data_dict,
    delim = ",",
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    clean_dates = FALSE,
    clean_selected_continuous_and_integers = FALSE
  )

dummy_diagnostic_codes_records <- get_all_diagnostic_codes_multi(ukb_pheno = dummy_ukb_data,
                                                                 data_dict = dummy_data_dict,
                                                                 ukb_codings = ukb_codings)

# TODO - this is still too large
dummy_ukb_data_diagnoses_cols <- dummy_ukb_data %>%
  dplyr::select(tidyselect::all_of(c(
    "eid",
    get_colnames_for_fieldids(
      field_ids = ukbwranglr:::DIAGNOSES_FIELD_IDS,
      data_dict = dummy_data_dict,
      scalar_output = FALSE
    )
  )))

# SAVE AS .RDATA ----------------------------------------------------------

usethis::use_data(
  # dummy_ukb_data,
  # dummy_data_dict,
  dummy_diagnostic_codes_records,
  overwrite = TRUE
)
