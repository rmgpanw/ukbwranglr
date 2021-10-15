
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ukbwranglr

<!-- badges: start -->

[![R build
status](https://github.com/rmgpanw/ukbwranglr/workflows/R-CMD-check/badge.svg)](https://github.com/rmgpanw/ukbwranglr/actions)
[![Codecov test
coverage](https://codecov.io/gh/rmgpanw/ukbwranglr/branch/main/graph/badge.svg)](https://codecov.io/gh/rmgpanw/ukbwranglr?branch=main)
[![Launch RStudio
Cloud](https://img.shields.io/badge/RStudio-Cloud-blue)](https://rstudio.cloud/project/2528744)

<!-- badges: end -->

***–UNDER CONSTRUCTION–***

# Overview

The goal of `ukbwranglr` is to facilitate analysing UK Biobank data.
This includes:

1.  Reading selected UK Biobank variables into R
2.  Summarising continuous variables
3.  Identifying phenotypic outcomes from clinical events data
    (e.g. self-reported medical conditions and linked health records
    data)

# Reading a selection of variables into R

A UK Biobank main dataset file is typically too large to fit into memory
on a personal computer. Often however, only a subset of the data is
required.

*Raw data appearance:*

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ukbwranglr)

# path to dummy data
DUMMY_DATA_PATH <- system.file("extdata", "dummy_ukb_data.csv", package = "ukbwranglr")

# appearance of raw data
data.table::fread(DUMMY_DATA_PATH) %>% 
  select(eid, 
         `31-0.0`, 
         `34-0.0`,
         `21000-0.0`, 
         `20002-0.0`, 
         `21001-0.0`) %>% 
  head(n = 5)
#>    eid 31-0.0 34-0.0 21000-0.0 20002-0.0 21001-0.0
#> 1:   1      0   1952        NA      1665   20.1115
#> 2:   2      0   1946      4001      1383   30.1536
#> 3:   3      1   1951         3      1197   22.8495
#> 4:   4      0   1956        NA      1441   23.4904
#> 5:   5     NA     NA        -3      1429   29.2752
```

*Appearance when loaded using `ukbwranglr::read_ukb()`:*

``` r
data_dict <- ukbwranglr::make_data_dict(DUMMY_DATA_PATH, delim = ",")
ukbwranglr::read_ukb(DUMMY_DATA_PATH, data_dict, delim = ",") %>% 
  labelled::unlabelled() %>% 
  head()
#> STEP 1 of 3
#> Reading data into R
#> STEP 2 of 3
#> STEP 3 of 3
#> Labelling dataset
#> Time taken: 0 minutes, 5 seconds.
#>    eid sex_f31_0_0 year_of_birth_f34_0_0 month_of_birth_f52_0_0
#> 1:   1      Female                  1952                 August
#> 2:   2      Female                  1946                  March
#> 3:   3        Male                  1951                  April
#> 4:   4      Female                  1956              September
#> 5:   5        <NA>                    NA                  April
#> 6:   6        Male                  1948               February
#>    ethnic_background_f21000_0_0
#> 1:                         <NA>
#> 2:                    Caribbean
#> 3:       Asian or Asian British
#> 4:                         <NA>
#> 5:         Prefer not to answer
#> 6:       Asian or Asian British
#>    non_cancer_illness_code_self_reported_f20002_0_0
#> 1:                                             1665
#> 2:                                             1383
#> 3:                                             1197
#> 4:                                             1441
#> 5:                                             1429
#> 6:                                             1513
#>    body_mass_index_bmi_f21001_0_0 body_mass_index_bmi_f21001_1_0
#> 1:                        20.1115                        20.8640
#> 2:                        30.1536                        20.2309
#> 3:                        22.8495                        26.7929
#> 4:                        23.4904                        25.6826
#> 5:                        29.2752                        19.7576
#> 6:                        28.2567                        30.2860
#>    body_mass_index_bmi_f21001_2_0
#> 1:                             NA
#> 2:                        27.4936
#> 3:                        27.6286
#> 4:                        37.2294
#> 5:                        14.6641
#> 6:                        27.3534
```

`ukbwranglr` can also help to:

-   Identify participants with certain health conditions or taking
    certain medications (with dates)
-   Summarise biomarker measurements (e.g. extract the mean systolic
    blood pressure across repeated measurements)

Please see [‘getting
started’](https://rmgpanw.github.io/ukbwranglr/articles/ukbwranglr.html)
on the [package documentation
website](https://rmgpanw.github.io/ukbwranglr/index.html) for further
details.

## Installation

You can install the development version of `ukbwranglr` with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/ukbwranglr")
```

## Basic usage - reading selected columns from a UK Biobank dataset into R

**Steps:**

1.  Create a data dictionary specific to your UK Biobank file
2.  Filter this for your required variables
3.  Read your selected columns into R

This process is outlined below for dummy UK Biobank data generated using
the [tofu library](https://github.com/spiros/tofu) (*Spiros Denaxas.
(2020). spiros/tofu: Updated release for DOI (Version v1.1). Zenodo.
<http://doi.org/10.5281/zenodo.3634604>*).

#### 1. Create data dictionary

``` r
# load package
library(ukbwranglr)

# make data dictionary
data_dict <- make_data_dict("MY_UKB_FILE.tab")
```

#### 2. Filter data dictionary for required variables

``` r
# Filter for sex, year of birth, BMI and self-reported noncancer-illness fields
data_dict <- data_dict %>%
  dplyr::filter(FieldID %in% c("31",
                               "34",
                               "21001",
                               "20002"))
```

#### 3. Read selected columns into R

``` re
read_ukb(path = "MY_UKB_FILE.tab", 
                        pheno_data_dict = data_dict)
```
