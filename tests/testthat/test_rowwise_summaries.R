
# SETUP -------------------------------------------------------------------

dummy_ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
dummy_ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")

dummy_ukb_main_continuous <-
  dummy_ukb_main <- read_ukb(
    get_ukb_dummy("dummy_ukb_main.tsv",
      path_only = TRUE
    ),
    data_dict = NULL,
    ukb_data_dict = dummy_ukb_data_dict,
    ukb_codings = dummy_ukb_codings
  ) %>%
  dplyr::select(
    eid,
    tidyselect::contains("bmi"),
    tidyselect::contains("systolic_blood_pressure")
  ) %>%
  head(4)

dummy_data_dict <- make_data_dict(dummy_ukb_main_continuous,
  ukb_data_dict = dummy_ukb_data_dict
)

# TESTS -------------------------------------------------------------------

# `summarise_numerical_variables()` ----------------------------------------------------

test_that("`summarise_numerical_variables()` returns the expected results for mean, summarising by FieldID", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "mean"
  )

  expect_equal(
    as.numeric(result$mean_systolic_blood_pressure_automated_reading_x4080),
    c(138.1667, 142.8571, 130.3750, NaN),
    tolerance = 3
  )

  expect_equal(
    as.numeric(result$mean_body_mass_index_bmi_x21001),
    c(20.488, 25.959, 25.757, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for max, summarising by FieldID", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "max"
  )

  expect_equal(
    as.numeric(result$max_systolic_blood_pressure_automated_reading_x4080),
    c(159, 146, 162, NA)
  )

  expect_equal(
    as.numeric(result$max_body_mass_index_bmi_x21001),
    c(20.8640, 30.1536, 27.6286, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for min, summarising by FieldID", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "min"
  )

  expect_equal(
    as.numeric(result$min_systolic_blood_pressure_automated_reading_x4080),
    c(134, 129, 123, NA)
  )

  expect_equal(
    as.numeric(result$min_body_mass_index_bmi_x21001),
    c(20.1115, 20.2309, 22.8495, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for sum, summarising by FieldID", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "sum"
  )

  expect_equal(
    as.numeric(result$sum_systolic_blood_pressure_automated_reading_x4080),
    c(829, 1000, 1043, 0)
  )

  expect_equal(
    as.numeric(result$sum_body_mass_index_bmi_x21001),
    c(40.9755, 77.8781, 77.271, 0),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for n values, summarising by FieldID", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "n_values"
  )

  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_x4080),
    c(6, 7, 8, 0)
  )

  expect_equal(
    as.numeric(result$n_values_body_mass_index_bmi_x21001),
    c(2, 3, 3, 0)
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for n values, summarising by instance", {
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "n_values",
    summarise_by = "Instance"
  )

  # note: not summary cols created for bmi as each instance has only one array
  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_x4080_0),
    c(3, 3, 4, 0)
  )

  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_x4080_1),
    c(3, 4, 4, 0)
  )
})

test_that("`summarise_numerical_variables()` raises a warning if there are missing instances/arrays in data_dict", {
  expect_warning(
    summarise_numerical_variables(dummy_ukb_main_continuous,
      data_dict = dummy_data_dict %>%
        dplyr::filter(descriptive_colnames == "body_mass_index_bmi_f21001_0_0"),
      summary_function = "n_values",
      summarise_by = "Field"
    ),
    "Are there missing instances/arrays in the dataset for this FieldID?"
  )
})

test_that("`summarise_numerical_variables()` drops original summarised columns if requested", {
  expect_equal(
    summarise_numerical_variables(
      dummy_ukb_main_continuous,
      data_dict = dummy_data_dict,
      summary_function = "n_values",
      summarise_by = "Field",
      .drop = TRUE
    ) %>%
      ncol(),
    3
  )
})

test_that("`summarise_numerical_variables()` does not include previously generated summary cols", {

  # summarise once
  result <- summarise_numerical_variables(dummy_ukb_main_continuous,
    data_dict = dummy_data_dict,
    summary_function = "mean",
    summarise_by = "Field"
  )

  # summarise again - `n_values` should not include the `mean` summary column
  # generated above
  result <- summarise_numerical_variables(result,
    data_dict = dummy_data_dict,
    summary_function = "n_values",
    summarise_by = "Field"
  )

  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_x4080),
    c(6, 7, 8, 0)
  )

  expect_equal(
    as.numeric(result$n_values_body_mass_index_bmi_x21001),
    c(2, 3, 3, 0)
  )
})
