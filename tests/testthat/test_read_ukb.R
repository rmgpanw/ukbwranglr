

# SETUP -------------------------------------------------------------------
# get path for raw dummy_ukb data
dummy_ukb_data_filepath <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")

# get ukb data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# make data dictionary
data_dict <- make_data_dict(ukb_main = dummy_ukb_data_filepath,
                            delim = ",",
                            ukb_data_dict = ukb_data_dict)

# make dummy stata file
dummy_ukb_stata_path <- file.path(tempdir(), "ukb_stata.dta")

read_ukb(dummy_ukb_data_filepath,
         delim = ",",
         data_dict = data_dict,
         labelled = FALSE,
         descriptive_colnames = FALSE) %>%
  rename_cols(old_colnames = data_dict$colheaders_raw,
              new_colnames = data_dict$colheaders_processed) %>%
  haven::write_dta(dummy_ukb_stata_path)

# df of potential UKB column name formats
example_colheaders_df <-
  data.frame(
    descriptive_ch = c(
      "eid",
      "verbal_interview_duration_f3_0_0",
      "date_of_death_f40000_0_0"
    ),
    dta_ch = c("n_eid", "n_3_0_0", "ts_40000_0_0"),
    txt_ch = c("eid", "3-0.0", "40000-0.0"),
    r_ch = c("f.eid", "f.3.0.0", "f.40000.0.0"),
    processed_ch = c("feid", "f3_0_0", "f40000_0_0"),
    processed_ch_derived = c("feid", "f3_0", "f40000"),
    descriptive_ch_derived = c("eid",
                               "n_values_systolic_blood_pressure_automated_reading_f4080_0",
                               "n_values_systolic_blood_pressure_automated_reading_f4080")
  )

# TESTS -------------------------------------------------------------------

# `make_data_dict()` ------------------------------------------------------

test_that("`make_data_dict()` works", {
  expect_equal(names(data_dict)[1:4],
               c("descriptive_colnames", "colheaders_raw", "colheaders_processed", "FieldID"))
})

# `read_ukb()` ------------------------------------------------------------

test_that("`read_ukb()` works with default settings", {
  ukb_main <- read_ukb(
    path = dummy_ukb_data_filepath,
    delim = ",",
    data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = c("", "NA")
  )

  expect_equal(names(ukb_main),
               data_dict$descriptive_colnames)

  expect_equal(attributes(ukb_main$sex_f31_0_0)$labels,
               c(Female = 0, Male = 1))

  expect_equal(attributes(ukb_main$sex_f31_0_0)$label,
               "Sex (f31_0_0)")
})

test_that("`read_ukb()` works with `labelled` and `descriptive_colnames` set to `FALSE`", {
  ukb_main <- read_ukb(
    path = dummy_ukb_data_filepath,
    delim = ",",
    data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = c("", "NA"),
    labelled = FALSE,
    descriptive_colnames = FALSE
  )

  expect_equal(names(ukb_main),
               data_dict$colheaders_raw)

  expect_null(attributes(ukb_main$sex_f31_0_0)$labels)

  expect_null(attributes(ukb_main$sex_f31_0_0)$label)
})

test_that("`read_ukb()` works with `labelled = TRUE` and `descriptive_colnames = FALSE`", {
  ukb_main <- read_ukb(
    path = dummy_ukb_data_filepath,
    delim = ",",
    data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = c("", "NA"),
    labelled = TRUE,
    descriptive_colnames = FALSE
  )

  expect_equal(names(ukb_main),
               data_dict$colheaders_raw)

  expect_equal(attributes(ukb_main$`31-0.0`)$labels,
              c(Female = 0, Male = 1))

  expect_equal(attributes(ukb_main$`31-0.0`)$label,
              "Sex (f31_0_0)")
})

test_that("`read_ukb()` works with `labelled = FALSE` and `descriptive_colnames = TRUE`", {
  ukb_main <- read_ukb(
    path = dummy_ukb_data_filepath,
    delim = ",",
    data_dict,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = c("", "NA"),
    labelled = FALSE,
    descriptive_colnames = TRUE
  )

  expect_equal(names(ukb_main),
               data_dict$descriptive_colnames)

  expect_null(attributes(ukb_main$sex_f31_0_0)$labels)

  expect_null(attributes(ukb_main$sex_f31_0_0)$label)
})

test_that("`read_ukb()` works with a stata file", {
  ukb_main <- read_ukb(
    path = dummy_ukb_stata_path,
    delim = ",",
    data_dict = NULL,
    ukb_data_dict = ukb_data_dict,
    ukb_codings = ukb_codings,
    na.strings = c("", "NA"),
    labelled = FALSE,
    descriptive_colnames = TRUE
  )

  expect_equal(names(ukb_main),
               data_dict$descriptive_colnames)

  expect_null(attributes(ukb_main$sex_f31_0_0)$labels)

  expect_null(attributes(ukb_main$sex_f31_0_0)$label)
})

# `mutate_descriptive_columns()` ------------------------------------------

test_that(
  "`mutate_descriptive_columns()` works", {
    expect_equal(
      data_dict$descriptive_colnames,
      c(
        "eid",
        "sex_f31_0_0",
        "year_of_birth_f34_0_0",
        "month_of_birth_f52_0_0",
        "ethnic_background_f21000_0_0",
        "non_cancer_illness_code_self_reported_f20002_0_0",
        "body_mass_index_bmi_f21001_0_0",
        "body_mass_index_bmi_f21001_1_0",
        "body_mass_index_bmi_f21001_2_0"
      )
    )
  }
)

test_that(
  "`mutate_descriptive_columns()` works for colnames generated by `summarise_numerical_variables()", {

    data_dict_for_mutate_descriptive_columns <- tibble::tibble(
      colheaders_raw = c('eid', 'systolic_blood_pressure_automated_reading_f4080_0_0', 'min_systolic_blood_pressure_automated_reading_f4080_0', 'mean_systolic_blood_pressure_automated_reading_f4080'),
      colheaders_processed = c('feid', 'f4080_0_0', 'f4080_0', 'f4080'),
      FieldID = c('eid', '4080', '4080', '4080'),
      instance = c(NA, '0', '0', NA),
      array = c(NA, '0', NA, NA),
      Path = c(NA, 'UK Biobank Assessment Centre > Physical measures > Blood pressure', 'UK Biobank Assessment Centre > Physical measures > Blood pressure', 'UK Biobank Assessment Centre > Physical measures > Blood pressure'),
      Category = c(NA, '100011', '100011', '100011'),
      Field = c(NA, 'Systolic blood pressure, automated reading', 'Systolic blood pressure, automated reading', 'Systolic blood pressure, automated reading'),
      Participants = c(NA, '475167', '475167', '475167'),
      Items = c(NA, '1053701', '1053701', '1053701'),
      Stability = c(NA, 'Complete', 'Complete', 'Complete'),
      ValueType = c(NA, 'Integer', 'Integer', 'Integer'),
      Units = c(NA, 'mmHg', 'mmHg', 'mmHg'),
      ItemType = c(NA, 'Data', 'Data', 'Data'),
      Strata = c(NA, 'Primary', 'Primary', 'Primary'),
      Sexed = c(NA, 'Unisex', 'Unisex', 'Unisex'),
      Instances = c(NA, '4', '4', '4'),
      Array = c(NA, '2', '2', '2'),
      Coding = c(NA, NA, NA, NA),
      Notes = c(NA, 'Blood pressure, automated reading, systolic. Two measures of blood pressure were taken a few moments apart.   Range returned by the Omron device is is 0-255', 'Blood pressure, automated reading, systolic. Two measures of blood pressure were taken a few moments apart.   Range returned by the Omron device is is 0-255', 'Blood pressure, automated reading, systolic. Two measures of blood pressure were taken a few moments apart.   Range returned by the Omron device is is 0-255'),
      Link = c(NA, 'http://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=4080', 'http://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=4080', 'http://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=4080'),
    )

    result <- mutate_descriptive_columns(data_dict_for_mutate_descriptive_columns)

     expect_equal(
       result$descriptive_colnames,
      c(
        "eid",
        "systolic_blood_pressure_automated_reading_f4080_0_0",
        "min_systolic_blood_pressure_automated_reading_f4080_0",
        "mean_systolic_blood_pressure_automated_reading_f4080"
      )
    )

     result <- make_data_dict(data.frame(eid = NA,
                                         systolic_blood_pressure_automated_reading_f4080_0_0 = NA,
                                         min_systolic_blood_pressure_automated_reading_f4080_0 = NA,
                                         mean_systolic_blood_pressure_automated_reading_f4080 = NA))

     expect_equal(
       result$descriptive_colnames,
       c(
         "eid",
         "systolic_blood_pressure_automated_reading_f4080_0_0",
         "min_systolic_blood_pressure_automated_reading_f4080_0",
         "mean_systolic_blood_pressure_automated_reading_f4080"
       )
     )
  }
)

# `format_ukb_df_header()` ------------------------------------------------

test_that(
  "`format_ukb_df_header()` reformats .dta style raw ukb column names", {
    expect_equal(format_ukb_df_header(example_colheaders_df$dta_ch),
                 example_colheaders_df$processed_ch)
  }
)

test_that(
  "`format_ukb_df_header()` reformats .txt style raw ukb column names", {
    expect_equal(format_ukb_df_header(example_colheaders_df$txt_ch),
                 example_colheaders_df$processed_ch)
  }
)

test_that(
  "`format_ukb_df_header()` reformats .tab (R) style raw ukb column names", {
    expect_equal(format_ukb_df_header(example_colheaders_df$r_ch),
                 example_colheaders_df$processed_ch)
  }
)

test_that(
  "`format_ukb_df_header()` reformats descriptive style ukb column names", {
    expect_equal(format_ukb_df_header(example_colheaders_df$descriptive_ch),
                 example_colheaders_df$processed_ch)
  }
)

test_that(
  "`format_ukb_df_header()` does NOT reformat ukb column names already processed by `format_ukb_df_header()`, including derived variables", {
    expect_equal(format_ukb_df_header(example_colheaders_df$processed_ch),
                 example_colheaders_df$processed_ch)

    expect_equal(format_ukb_df_header(example_colheaders_df$processed_ch_derived),
                 example_colheaders_df$processed_ch_derived)
  }
)

test_that(
  "`format_ukb_df_header()` does reformat dervied variable names from `summarise_numerical_variables()`", {
    expect_equal(format_ukb_df_header(example_colheaders_df$descriptive_ch_derived),
                 c('feid',
                   'f4080_0',
                   'f4080'))
  }
)

# `indicate_coltype_in_data_dict()` ---------------------------------------

test_that(
  "`indicate_coltype_in_data_dict()` returns the expected values", {
    data_dict_coltypes <- indicate_coltype_in_data_dict(data_dict = dplyr::bind_rows(data_dict,
                                                                                     data.frame(ValueType = c("something_else",
                                                                                                              "Date"))),
                                  ukb_codings = ukb_codings) %>%
      dplyr::group_by(.data[["ValueType"]]) %>%
      dplyr::slice(1L) %>%
      dplyr::select(tidyselect::all_of(c(
        "ValueType",
        "col_types_readr",
        "col_types_fread"
      )))

    expect_equal(data_dict_coltypes$ValueType,
                 c("Categorical multiple", "Categorical single", "Continuous", "Date", "Integer", "something_else"))

    expect_equal(data_dict_coltypes$col_types_readr,
                 c("i", "i", "d", "c", "i", "c"))

    expect_equal(data_dict_coltypes$col_types_fread,
                 c("integer", "integer", "double", "character", "integer", "character"))
  }
)

# `colname_to_field_inst_array_df()` --------------------------------------

test_that(
  "`colname_to_field_inst_array_df()` returns the expected values", {
    processed_ch_result <- colname_to_field_inst_array_df(example_colheaders_df$processed_ch)
    processed_ch_derived_result <- colname_to_field_inst_array_df(example_colheaders_df$processed_ch_derived)
    descriptive_ch_result <- colname_to_field_inst_array_df(example_colheaders_df$descriptive_ch)

    expect_equal(processed_ch_result$description,
                 c(NA, NA, NA))

    expect_equal(processed_ch_result$fieldid_instance_array,
                 c("eid", "3_0_0", "40000_0_0"))

    expect_equal(processed_ch_derived_result$instance,
                 c(NA, "0", NA))

    expect_equal(processed_ch_derived_result$array,
                 c(NA, NA, NA))

    expect_equal(descriptive_ch_result$description,
                 c("eid", "verbal_interview_duration", "date_of_death"))
  }
)


# Dev ---------------------------------------------------------------------


# `read_ukb_chunked_to_file()` --------------------------------------------

test_that("`read_ukb_chunked_to_file()` works", {
  dummy_data_dict_selected <- data_dict[c(1,3,5,6,7), ]

  outfile_list <- list(
    csv_file = tempfile(fileext = ".csv"),
    tsv_file = tempfile(fileext = ".tsv"),
    txt_file = tempfile(fileext = ".txt"),
    rds_file = tempfile(fileext = ".rds"),
    dta_file = tempfile(fileext = ".dta")
  )

  outfile_list %>%
    purrr::walk(
      ~
        read_ukb_chunked_to_file(
          in_path = dummy_ukb_data_filepath,
          out_path = .x,
          data_dict = dummy_data_dict_selected,
          in_delim = ",",
          ukb_data_dict = ukb_data_dict,
          ukb_codings = ukb_codings,
          max_n_labels = 22,
          chunk_size = 1,
          labelled = TRUE,
          descriptive_colnames = FALSE
        )
    )

  outfile_result <- list(
    csv_file = readr::read_csv(outfile_list$csv_file),
    tsv_file = readr::read_tsv(outfile_list$tsv_file),
    txt_file = readr::read_tsv(outfile_list$txt_file),
    rds_file = readRDS(outfile_list$rds_file),
    dta_file = haven::read_dta(outfile_list$dta_file)
  )

  EXPECTED_COLNAMES <- dummy_data_dict_selected$colheaders_processed

  expect_true(outfile_result %>%
                purrr::map_lgl(~ all(names(.x) == EXPECTED_COLNAMES)) %>%
                all())
})

