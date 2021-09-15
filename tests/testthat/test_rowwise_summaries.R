
# SETUP -------------------------------------------------------------------

# dummy data
DUMMY_UKB_MAIN_CONTINUOUS <- tibble::tibble(
  eid = c(1, 2, 3, 4),
  body_mass_index_bmi_f21001_0_0 = c(20.1115, 30.1536, 22.8495, NA),
  body_mass_index_bmi_f21001_1_0 = c(20.864, 20.2309, 26.7929, NA),
  body_mass_index_bmi_f21001_2_0 = c(NA, 27.4936, 27.6286, NA),
  systolic_blood_pressure_automated_reading_f4080_0_0 = c(NA, 146, 143, NA),
  systolic_blood_pressure_automated_reading_f4080_0_1 = c(134, 145, 123, NA),
  systolic_blood_pressure_automated_reading_f4080_0_2 = c(134, 145, 123, NA),
  systolic_blood_pressure_automated_reading_f4080_0_3 = c(134, NA, 123, NA),
  systolic_blood_pressure_automated_reading_f4080_1_0 = c(159, 129, 162, NA),
  systolic_blood_pressure_automated_reading_f4080_1_1 = c(134, 145, 123, NA),
  systolic_blood_pressure_automated_reading_f4080_1_2 = c(134, 145, 123, NA),
  systolic_blood_pressure_automated_reading_f4080_1_3 = c(NA, 145, 123, NA)
)

data_dict <- make_data_dict(DUMMY_UKB_MAIN_CONTINUOUS)

# TESTS -------------------------------------------------------------------

# `summarise_numerical_variables()` ----------------------------------------------------

test_that("`summarise_numerical_variables()` returns the expected results for mean, summarising by FieldID", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "mean")

  expect_equal(
    as.numeric(result$mean_systolic_blood_pressure_automated_reading),
    c(138.1667, 142.8571, 130.3750, NaN),
    tolerance = 3
  )

  expect_equal(
    as.numeric(result$mean_body_mass_index_bmi),
    c(20.488, 25.959, 25.757, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for max, summarising by FieldID", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "max")

  expect_equal(
    as.numeric(result$max_systolic_blood_pressure_automated_reading),
    c(159, 146, 162, NA)
  )

  expect_equal(
    as.numeric(result$max_body_mass_index_bmi),
    c(20.8640, 30.1536, 27.6286, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for min, summarising by FieldID", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "min")

  expect_equal(
    as.numeric(result$min_systolic_blood_pressure_automated_reading),
    c(134, 129, 123, NA)
  )

  expect_equal(
    as.numeric(result$min_body_mass_index_bmi),
    c(20.1115, 20.2309, 22.8495, NaN),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for sum, summarising by FieldID", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "sum")

  expect_equal(
    as.numeric(result$sum_systolic_blood_pressure_automated_reading),
    c(829, 1000, 1043, 0)
  )

  expect_equal(
    as.numeric(result$sum_body_mass_index_bmi),
    c(40.9755, 77.8781, 77.271, 0),
    tolerance = 3
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for n values, summarising by FieldID", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "n_values")

  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading),
    c(6, 7, 8, 0)
  )

  expect_equal(
    as.numeric(result$n_values_body_mass_index_bmi),
    c(2, 3, 3, 0)
  )
})

test_that("`summarise_numerical_variables()` returns the expected results for n values, summarising by instance", {
  result <- summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict,
                                          summary_function = "n_values",
                                          summarise_by = "Instance")

  # note: not summary cols created for bmi as each instance has only one array
  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_0),
    c(3, 3, 4, 0)
  )

  expect_equal(
    as.numeric(result$n_values_systolic_blood_pressure_automated_reading_1),
    c(3, 4, 4, 0)
  )

})

test_that("`summarise_numerical_variables()` raises a warning if there are missing instances/arrays in data_dict", {
  expect_warning(summarise_numerical_variables(DUMMY_UKB_MAIN_CONTINUOUS,
                                          data_dict = data_dict %>%
                                            dplyr::filter(descriptive_colnames == "body_mass_index_bmi_f21001_0_0"),
                                          summary_function = "n_values",
                                          summarise_by = "Field"),
                 "Are there missing instances/arrays in the dataset for this FieldID?")

})

test_that("`summarise_numerical_variables()` drops original summarised columns if requested",
          {
            expect_equal(
              summarise_numerical_variables(
                DUMMY_UKB_MAIN_CONTINUOUS,
                data_dict = data_dict,
                summary_function = "n_values",
                summarise_by = "Field",
                .drop = TRUE
              ) %>%
                ncol(),
              3
            )

          })
