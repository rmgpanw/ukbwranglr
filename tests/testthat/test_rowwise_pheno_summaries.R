
# SETUP -------------------------------------------------------------------

# essential
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()
dummy_ukb_data_path <- download_dummy_ukb_data_to_tempdir()
dummy_ukb_data_dict <- make_data_dict(ukb_pheno = dummy_ukb_data_path,
                                      delim = ",",
                                      ukb_data_dict = ukb_data_dict)
dummy_ukb_data_3eid <- read_pheno(path = dummy_ukb_data_path,
                                  delim = ",",
                                  data_dict = dummy_ukb_data_dict,
                                  ukb_data_dict = ukb_data_dict,
                                  ukb_codings = ukb_codings,
                                  clean_dates = FALSE,
                                  clean_selected_continuous_and_integers = FALSE,
                                  nrows = 3)

# process dummy data with rowwise functions
dummy_ukb_data_3eid_bmi_rowmeans <-
  summarise_rowise_numerical_mean_min_max(ukb_pheno = dummy_ukb_data_3eid,
                                          data_dict = dummy_ukb_data_dict,
                                          mean_min_max = "rowMeans") %>%
  dplyr::select(tidyselect::contains("bmi")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(expected = mean(c(body_mass_index_bmi_f21001_0_0,
                                  body_mass_index_bmi_f21001_1_0,
                                  body_mass_index_bmi_f21001_2_0), na.rm = TRUE)) %>%
  dplyr::ungroup()

dummy_ukb_data_3eid_bmi_pmin <-
  summarise_rowise_numerical_mean_min_max(ukb_pheno = dummy_ukb_data_3eid,
                                          data_dict = dummy_ukb_data_dict,
                                          mean_min_max = "pmin") %>%
  dplyr::select(tidyselect::contains("bmi")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(expected = min(c(body_mass_index_bmi_f21001_0_0,
                                  body_mass_index_bmi_f21001_1_0,
                                  body_mass_index_bmi_f21001_2_0), na.rm = TRUE)) %>%
  dplyr::ungroup()

dummy_ukb_data_3eid_bmi_pmax <-
  summarise_rowise_numerical_mean_min_max(ukb_pheno = dummy_ukb_data_3eid,
                                          data_dict = dummy_ukb_data_dict,
                                          mean_min_max = "pmax") %>%
  dplyr::select(tidyselect::contains("bmi")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(expected = max(c(body_mass_index_bmi_f21001_0_0,
                                 body_mass_index_bmi_f21001_1_0,
                                 body_mass_index_bmi_f21001_2_0), na.rm = TRUE)) %>%
  dplyr::ungroup()

# TESTS -------------------------------------------------------------------


# `summarise_rowise_numerical_mean_min_max()` -----------------------------

test_that("`summarise_rowise_numerical_mean_min_max()` calculates the expected mean/min/max", {
  expect_equal(dummy_ukb_data_3eid_bmi_rowmeans$rowMeans_body_mass_index_bmi_21001,
               dummy_ukb_data_3eid_bmi_rowmeans$expected)

  expect_equal(dummy_ukb_data_3eid_bmi_pmin$pmin_body_mass_index_bmi_21001,
               dummy_ukb_data_3eid_bmi_pmin$expected)

  expect_equal(dummy_ukb_data_3eid_bmi_pmax$pmax_body_mass_index_bmi_21001,
               dummy_ukb_data_3eid_bmi_pmax$expected)
})
