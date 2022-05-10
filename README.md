
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ukbwranglr

<!-- badges: start -->

[![R build
status](https://github.com/rmgpanw/ukbwranglr/workflows/R-CMD-check/badge.svg)](https://github.com/rmgpanw/ukbwranglr/actions)
[![Codecov test
coverage](https://codecov.io/gh/rmgpanw/ukbwranglr/branch/main/graph/badge.svg)](https://codecov.io/gh/rmgpanw/ukbwranglr?branch=main)
[![pkgdown](https://github.com/rmgpanw/ukbwranglr/workflows/pkgdown/badge.svg)](https://github.com/rmgpanw/ukbwranglr/actions)
[![Launch RStudio
Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/2528744)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

# Overview

The goal of ukbwranglr is to facilitate analysing UK Biobank data,
including:

1.  Reading a selection of UK Biobank variables into R.
2.  Summarising repeated continuous variable measurements.[^1]
3.  Extracting phenotypic outcomes of interest from clinical events
    data.[^2]

# Installation

You can install the development version of ukbwranglr with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/ukbwranglr")
```

# Basic workflow

The basic workflow is as follows:

1.  Create a data dictionary for your main UK Biobank dataset.
2.  Read selected variables into R.
3.  Summarise continuous variables.
4.  Tidy clinical events data and extract outcomes of interest.
5.  Analyse.

These steps are now illustrated with a dummy dataset included with
ukbwranglr:

``` r
library(ukbwranglr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyselect)
library(readr)
library(tibble)

# path to dummy data
ukb_main_path <- get_ukb_dummy("dummy_ukb_main.tsv",
                                     path_only = TRUE)

# raw data
read_tsv(ukb_main_path)
#> Rows: 10 Columns: 71
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (14): 41270-0.0, 41270-0.3, 41271-0.0, 40001-0.0, 40001-1.0, 40002-0.0,...
#> dbl  (43): eid, 31-0.0, 34-0.0, 52-0.0, 21000-0.0, 21000-1.0, 21000-2.0, 210...
#> date (14): 41280-0.0, 41280-0.3, 41281-0.0, 41281-0.3, 40000-0.0, 40000-1.0,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 10 × 71
#>      eid `31-0.0` `34-0.0` `52-0.0` `21000-0.0` `21000-1.0` `21000-2.0`
#>    <dbl>    <dbl>    <dbl>    <dbl>       <dbl>       <dbl>       <dbl>
#>  1     1        0     1952        8          -1           2        3003
#>  2     2        0     1946        3          -3        2001        3004
#>  3     3        1     1951        4           1        2002          -1
#>  4     4        0     1956        9        1001        2003        4001
#>  5     5       NA       NA        4        1002        2004        4002
#>  6     6        1     1948        2        1003           3        4003
#>  7     7        0     1949       12          NA        3001           5
#>  8     8        1     1956       10          NA           5          NA
#>  9     9        0     1962        4        4001          NA          NA
#> 10    10        1     1953        2        4001          NA          NA
#> # … with 64 more variables: `21001-0.0` <dbl>, `21001-1.0` <dbl>,
#> #   `21001-2.0` <dbl>, `4080-0.0` <dbl>, `4080-0.1` <dbl>, `4080-0.2` <dbl>,
#> #   `4080-0.3` <dbl>, `4080-1.0` <dbl>, `4080-1.1` <dbl>, `4080-1.2` <dbl>,
#> #   `4080-1.3` <dbl>, `20001-0.0` <dbl>, `20001-0.3` <dbl>, `20001-2.0` <dbl>,
#> #   `20001-2.3` <dbl>, `20002-0.0` <dbl>, `20002-0.3` <dbl>, `20002-2.0` <dbl>,
#> #   `20002-2.3` <dbl>, `20006-0.0` <dbl>, `20006-0.3` <dbl>, `20006-2.0` <dbl>,
#> #   `20006-2.3` <dbl>, `20008-0.0` <dbl>, `20008-0.3` <dbl>, …
```

## 1. Create data dictionary

First download a copy of the UK Biobank data dictionary and codings
files from the [UK Biobank data showcase
website](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide).
Dummy versions are used here:

``` r
# get required metadata
ukb_data_dict <- get_ukb_dummy("dummy_Data_Dictionary_Showcase.tsv")
ukb_codings <- get_ukb_dummy("dummy_Codings.tsv")
```

Then create a data dictionary using `make_data_dict()`:

``` r
# make data dictionary
data_dict <- make_data_dict(ukb_main_path, 
                            ukb_data_dict = ukb_data_dict)

data_dict
#> # A tibble: 71 × 22
#>    descriptive_col… colheaders_raw colheaders_proc… FieldID instance array Path 
#>    <chr>            <chr>          <chr>            <chr>   <chr>    <chr> <chr>
#>  1 eid              eid            feid             eid     <NA>     <NA>  <NA> 
#>  2 sex_f31_0_0      31-0.0         f31_0_0          31      0        0     Popu…
#>  3 year_of_birth_f… 34-0.0         f34_0_0          34      0        0     Popu…
#>  4 month_of_birth_… 52-0.0         f52_0_0          52      0        0     Popu…
#>  5 ethnic_backgrou… 21000-0.0      f21000_0_0       21000   0        0     Asse…
#>  6 ethnic_backgrou… 21000-1.0      f21000_1_0       21000   1        0     Asse…
#>  7 ethnic_backgrou… 21000-2.0      f21000_2_0       21000   2        0     Asse…
#>  8 body_mass_index… 21001-0.0      f21001_0_0       21001   0        0     Asse…
#>  9 body_mass_index… 21001-1.0      f21001_1_0       21001   1        0     Asse…
#> 10 body_mass_index… 21001-2.0      f21001_2_0       21001   2        0     Asse…
#> # … with 61 more rows, and 15 more variables: Category <chr>, Field <chr>,
#> #   Participants <chr>, Items <chr>, Stability <chr>, ValueType <chr>,
#> #   Units <chr>, ItemType <chr>, Strata <chr>, Sexed <chr>, Instances <chr>,
#> #   Array <chr>, Coding <chr>, Notes <chr>, Link <chr>
```

## 2. Read selected variables into R

Read a main UK Biobank dataset into R using `read_ukb()`:

``` r
read_ukb(path = ukb_main_path,
         ukb_data_dict = ukb_data_dict,
         ukb_codings = ukb_codings) %>% 
  # (convert to tibble for concise print method)
  as_tibble()
#> Creating data dictionary
#> STEP 1 of 3
#> Reading data into R
#> STEP 2 of 3
#> Renaming with descriptive column names
#> STEP 3 of 3
#> Applying variable and value labels
#> Labelling dataset
#> Time taken: 0 minutes, 0 seconds.
#> # A tibble: 10 × 71
#>      eid sex_f31_0_0 year_of_birth_f34_0_0 month_of_birth_f52_… ethnic_backgrou…
#>    <int> <fct>                       <int> <fct>                <fct>           
#>  1     1 Female                       1952 August               Do not know     
#>  2     2 Female                       1946 March                Prefer not to a…
#>  3     3 Male                         1951 April                White           
#>  4     4 Female                       1956 September            British         
#>  5     5 <NA>                           NA April                Irish           
#>  6     6 Male                         1948 February             Any other white…
#>  7     7 Female                       1949 December             <NA>            
#>  8     8 Male                         1956 October              <NA>            
#>  9     9 Female                       1962 April                Caribbean       
#> 10    10 Male                         1953 February             Caribbean       
#> # … with 66 more variables: ethnic_background_f21000_1_0 <fct>,
#> #   ethnic_background_f21000_2_0 <fct>, body_mass_index_bmi_f21001_0_0 <dbl>,
#> #   body_mass_index_bmi_f21001_1_0 <dbl>, body_mass_index_bmi_f21001_2_0 <dbl>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_0 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_1 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_2 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_3 <int>, …
```

A UK Biobank main dataset file is typically too large to fit into memory
on a personal computer. Often however, only a subset of the data is
required. To read a selection of variables, filter the data dictionary
created with `make_data_dict()` for a subset of variables and supply
this to `read_ukb()`:[^3]

``` r
# Filter data dictionary for sex, year of birth, body mass index, systolic blood pressure, self-reported non-cancer illness and summary hospital diagnoses (ICD10) fields
data_dict_selected <- data_dict %>%
  filter(FieldID %in% c(
    # Participant ID
    "eid",
    
    # Sex
    "31",
    
    # Year of birth
    "34",
    
    # Body mass index
    "21001",
    
    # Systolic blood pressure
    "4080",
    
    # Self-reported non-cancer medical conditions
    "20002",
    "20008",
    
    # Summary hospital diagnoses (ICD10)
    "41270",
    "41280"
  ))

# Read selected variables into R
ukb_main <- read_ukb(path = ukb_main_path,
         data_dict = data_dict_selected,
         ukb_data_dict = ukb_data_dict,
         ukb_codings = ukb_codings)
#> STEP 1 of 3
#> Reading data into R
#> STEP 2 of 3
#> Renaming with descriptive column names
#> STEP 3 of 3
#> Applying variable and value labels
#> Labelling dataset
#> Time taken: 0 minutes, 0 seconds.

# Convert to tibble for concise print method
as_tibble(ukb_main)
#> # A tibble: 10 × 26
#>      eid sex_f31_0_0 year_of_birth_f34_0_0 body_mass_index_bmi… body_mass_index…
#>    <int> <fct>                       <int>                <dbl>            <dbl>
#>  1     1 Female                       1952                 20.1             20.9
#>  2     2 Female                       1946                 30.2             20.2
#>  3     3 Male                         1951                 22.8             26.8
#>  4     4 Female                       1956                 NA               NA  
#>  5     5 <NA>                           NA                 29.3             19.8
#>  6     6 Male                         1948                 28.3             30.3
#>  7     7 Female                       1949                 NA               NA  
#>  8     8 Male                         1956                 NA               NA  
#>  9     9 Female                       1962                 25.4             21.9
#> 10    10 Male                         1953                 NA               25.2
#> # … with 21 more variables: body_mass_index_bmi_f21001_2_0 <dbl>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_0 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_1 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_2 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_0_3 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_1_0 <int>,
#> #   systolic_blood_pressure_automated_reading_f4080_1_1 <int>, …
```

## 3. Summarise continuous variables

Some variables such as body mass index and systolic blood pressure will
have been measured on more than one occasion. In these cases it may be
desirable to calculate a summary value (e.g. mean). Use
`summarise_numerical_variables()`:

``` r
# calculate the mean value across all repeated continuous variable measurements
ukb_main_numerical_vars_summarised <- summarise_numerical_variables(
  ukb_main = ukb_main,
  ukb_data_dict = ukb_data_dict,
  summary_function = "mean", 
  .drop = TRUE
) %>% 
  # reorder variables
  select(eid,
         sex_f31_0_0,
         year_of_birth_f34_0_0,
         mean_body_mass_index_bmi_x21001,
         mean_systolic_blood_pressure_automated_reading_x4080)
#> Number of summary columns to make: 3
#> Time taken: 0 minutes, 0 seconds.

as_tibble(ukb_main_numerical_vars_summarised)
#> # A tibble: 10 × 5
#>      eid sex_f31_0_0 year_of_birth_f34_0_0 mean_body_mass_inde… mean_systolic_b…
#>    <int> <fct>                       <int>                <dbl>            <dbl>
#>  1     1 Female                       1952                 20.5             138.
#>  2     2 Female                       1946                 26.0             143.
#>  3     3 Male                         1951                 25.8             130.
#>  4     4 Female                       1956                NaN               NaN 
#>  5     5 <NA>                           NA                 21.2             NaN 
#>  6     6 Male                         1948                 28.6             NaN 
#>  7     7 Female                       1949                NaN               NaN 
#>  8     8 Male                         1956                NaN               NaN 
#>  9     9 Female                       1962                 23.9             NaN 
#> 10    10 Male                         1953                 27.6             NaN
```

## 4. Tidy clinical events data and extract outcomes of interest

Use `tidy_clinical_events()` to reshape clinical events fields (such as
self-reported non-cancer medical conditions) to long format:

``` r
# tidy clinical events
clinical_events <- tidy_clinical_events(
  ukb_main = ukb_main, 
  ukb_data_dict = ukb_data_dict, 
  ukb_codings = ukb_codings,
  clinical_events_sources = c("self_report_non_cancer",
                              "summary_hes_icd10")
)
#> Tidying clinical events for self_report_non_cancer
#> Time taken: 0 minutes, 0 seconds.
#> Tidying clinical events for summary_hes_icd10
#> Time taken: 0 minutes, 0 seconds.

# returns a named list of data frames
clinical_events
#> $self_report_non_cancer
#>    eid source index code       date
#> 1:   1 f20002   0_0 1665 1998-12-24
#> 2:   2 f20002   0_0 1383 2011-01-05
#> 3:   3 f20002   0_0 1665       <NA>
#> 4:   4 f20002   0_0 1383       <NA>
#> 5:   1 f20002   0_3 1223 2003-02-25
#> 6:   2 f20002   0_3 1352 2020-07-02
#> 7:   1 f20002   2_0 1514 2011-04-07
#> 8:   2 f20002   2_0 1447 1981-03-01
#> 9:   2 f20002   2_3 1165 1983-01-03
#> 
#> $summary_hes_icd10
#>    eid source index  code       date
#> 1:   1 f41270   0_0  X715 1955-11-12
#> 2:   2 f41270   0_0   E11 1939-02-16
#> 3:   1 f41270   0_3   E10 1910-02-19
#> 4:   2 f41270   0_3 M0087 1965-08-08

# combine with dplyr
clinical_events <- dplyr::bind_rows(clinical_events)

clinical_events
#>     eid source index  code       date
#>  1:   1 f20002   0_0  1665 1998-12-24
#>  2:   2 f20002   0_0  1383 2011-01-05
#>  3:   3 f20002   0_0  1665       <NA>
#>  4:   4 f20002   0_0  1383       <NA>
#>  5:   1 f20002   0_3  1223 2003-02-25
#>  6:   2 f20002   0_3  1352 2020-07-02
#>  7:   1 f20002   2_0  1514 2011-04-07
#>  8:   2 f20002   2_0  1447 1981-03-01
#>  9:   2 f20002   2_3  1165 1983-01-03
#> 10:   1 f41270   0_0  X715 1955-11-12
#> 11:   2 f41270   0_0   E11 1939-02-16
#> 12:   1 f41270   0_3   E10 1910-02-19
#> 13:   2 f41270   0_3 M0087 1965-08-08
```

To identify participants with a condition of interest, first decide
which codes will capture this. For example, the following includes a
(non-exhaustive) list of clinical codes for diabetes:

``` r
example_clinical_codes()
#> # A tibble: 8 × 6
#>   disease  description                           category code_type code  author
#>   <chr>    <chr>                                 <chr>    <chr>     <chr> <chr> 
#> 1 Diabetes diabetes                              Diabete… data_cod… 1220  ukbwr 
#> 2 Diabetes gestational diabetes                  Gestati… data_cod… 1221  ukbwr 
#> 3 Diabetes type 1 diabetes                       Type 1 … data_cod… 1222  ukbwr 
#> 4 Diabetes type 2 diabetes                       Type 2 … data_cod… 1223  ukbwr 
#> 5 Diabetes Type 1 diabetes mellitus              Type 1 … icd10     E10   ukbwr 
#> 6 Diabetes Type 2 diabetes mellitus              Type 2 … icd10     E11   ukbwr 
#> 7 Diabetes Insulin dependent diabetes mellitus   Type 1 … read2     C108. ukbwr 
#> 8 Diabetes Non-insulin dependent diabetes melli… Type 2 … read2     C109. ukbwr
```

Supply this to `extract_phenotypes()` to filter for participants who
have any matching clinical codes in their records. By default, only the
earliest date is extracted:

``` r
# extract phenotypes
diabetes_cases <- extract_phenotypes(clinical_events = clinical_events,
                                     clinical_codes = example_clinical_codes())
#> 
#> ***PROCESSING DISEASE 1 OF 1***
#> Time taken: 0 minutes, 0 seconds.
#> 
#> Extracting clinical events for disease: Diabetes
#> Filtering clinical_events for all codes in clinical_codes
#> EXTRACTING EVENT DATES FOR PHENOTYPES
#> Extracting event dates for DIABETES_UKBWR (Phenotype 1 of 5)
#> Time taken: 0 minutes, 0 seconds.
#> Extracting event dates for diabetes_unspecified_ukbwr (Phenotype 2 of 5)
#> Time taken: 0 minutes, 1 seconds.
#> Extracting event dates for gestational_diabetes_ukbwr (Phenotype 3 of 5)
#> Time taken: 0 minutes, 1 seconds.
#> Extracting event dates for type_1_dm_ukbwr (Phenotype 4 of 5)
#> Time taken: 0 minutes, 2 seconds.
#> Extracting event dates for type_2_dm_ukbwr (Phenotype 5 of 5)
#> Time taken: 0 minutes, 2 seconds.
#> COMPLETE!
#> Time taken: 0 minutes, 2 seconds.

# returns a named list of data frames, one for each category in lower case, and one for the overall disease in capitals
diabetes_cases
#> $Diabetes
#> $Diabetes$DIABETES_UKBWR
#> # A tibble: 2 × 3
#>     eid DIABETES_UKBWR_min_date DIABETES_UKBWR_indicator
#>   <int> <chr>                   <chr>                   
#> 1     1 1910-02-19              Yes                     
#> 2     2 1939-02-16              Yes                     
#> 
#> $Diabetes$diabetes_unspecified_ukbwr
#> NULL
#> 
#> $Diabetes$gestational_diabetes_ukbwr
#> NULL
#> 
#> $Diabetes$type_1_dm_ukbwr
#> # A tibble: 1 × 3
#>     eid type_1_dm_ukbwr_min_date type_1_dm_ukbwr_indicator
#>   <int> <chr>                    <chr>                    
#> 1     1 1910-02-19               Yes                      
#> 
#> $Diabetes$type_2_dm_ukbwr
#> # A tibble: 2 × 3
#>     eid type_2_dm_ukbwr_min_date type_2_dm_ukbwr_indicator
#>   <int> <chr>                    <chr>                    
#> 1     1 2003-02-25               Yes                      
#> 2     2 1939-02-16               Yes
```

## 5. Analyse

Merge the output from steps 4 and 5:

``` r
ukb_main_processed <- dplyr::full_join(
  ukb_main_numerical_vars_summarised,
  diabetes_cases$Diabetes$DIABETES_UKBWR,
  by = "eid"
) %>% 
  mutate(DIABETES_UKBWR_indicator = ifelse(
    !is.na(DIABETES_UKBWR_indicator),
    yes = DIABETES_UKBWR_indicator,
    no = "No"
  ))

ukb_main_processed
#>     eid sex_f31_0_0 year_of_birth_f34_0_0 mean_body_mass_index_bmi_x21001
#>  1:   1      Female                  1952                        20.48775
#>  2:   2      Female                  1946                        25.95937
#>  3:   3        Male                  1951                        25.75700
#>  4:   4      Female                  1956                             NaN
#>  5:   5        <NA>                    NA                        21.23230
#>  6:   6        Male                  1948                        28.63203
#>  7:   7      Female                  1949                             NaN
#>  8:   8        Male                  1956                             NaN
#>  9:   9      Female                  1962                        23.94280
#> 10:  10        Male                  1953                        27.60310
#>     mean_systolic_blood_pressure_automated_reading_x4080
#>  1:                                             138.1667
#>  2:                                             142.8571
#>  3:                                             130.3750
#>  4:                                                  NaN
#>  5:                                                  NaN
#>  6:                                                  NaN
#>  7:                                                  NaN
#>  8:                                                  NaN
#>  9:                                                  NaN
#> 10:                                                  NaN
#>     DIABETES_UKBWR_min_date DIABETES_UKBWR_indicator
#>  1:              1910-02-19                      Yes
#>  2:              1939-02-16                      Yes
#>  3:                    <NA>                       No
#>  4:                    <NA>                       No
#>  5:                    <NA>                       No
#>  6:                    <NA>                       No
#>  7:                    <NA>                       No
#>  8:                    <NA>                       No
#>  9:                    <NA>                       No
#> 10:                    <NA>                       No
```

Describe:

``` r
ukb_main_processed %>%
  select(-eid) %>%
  group_by(DIABETES_UKBWR_indicator) %>%
  summarise(pct_female = sum(sex_f31_0_0 == "Female", na.rm = TRUE) / n(),
            across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
#> # A tibble: 2 × 5
#>   DIABETES_UKBWR_… pct_female year_of_birth_f… mean_body_mass_… mean_systolic_b…
#>   <chr>                 <dbl>            <dbl>            <dbl>            <dbl>
#> 1 No                    0.375            1954.             25.4             130.
#> 2 Yes                   1                1949              23.2             141.
```

# Learn more

See the vignettes on the ukbwranglr pkgdown website for further
information. You can try out the steps either locally by installing
ukbwranglr on your own machine, or online by clicking on the RStudio
Cloud badge on this page[^4] and navigating to the Rmd files under
‘vignettes’.

[^1]: For example, calculating a mean/minimum/maximum body mass index
    (BMI) from repeated BMI measurements.

[^2]: For example, identifying participants with a diagnosis of
    hypertension from linked primary and secondary health care records.

[^3]: It may be preferable at this stage to write the data dictionary to
    a csv file (using `readr::write_csv()`), manually filter in a text
    editor like Microsoft Excel, then reload into R (using
    `readr::read_csv()`).

[^4]: You will be asked to sign up for a free account if you do not have
    one already.
