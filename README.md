
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

1.  Reading a selection of variables
2.  Summarising continuous variables
3.  Identifying phenotypes from clinical events data (e.g. self-reported
    medical conditions and linked hospital data)

# Reading a selection of variables into R

A UK Biobank main dataset file is often too large to fit into memory on
a personal computer. However, often only a subset of the data is
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
ukbwranglr_data <- ukbwranglr::read_ukb(DUMMY_DATA_PATH, data_dict, delim = ",") 
#> STEP 1 of 3
#> Reading data into R
#> STEP 2 of 3
#> Renaming columns
#> STEP 3 of 3
#> Labelling dataset
#> Time taken: 0 minutes, 4 seconds.

# ukbwranglr_data_selected_cols <- ukbwranglr_data %>% 
#   select(eid, 
#          sex_f31_0_0, 
#          year_of_birth_f34_0_0, 
#          ethnic_background_f21000_0_0,
#          noncancer_illness_code_selfreported_f20002_0_0,
#          body_mass_index_bmi_f21001_0_0,
#          )
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
  dplyr::filter(FieldID %in% c(31,
                               34,
                               21001,
                               20002))
```

#### 3. Read selected columns into R

``` r
ukb_pheno <- read_pheno(path = "MY_UKB_FILE.tab", 
                        pheno_data_dict = data_dict)
```
