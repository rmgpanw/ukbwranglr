
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

The goal of `ukbwranglr` is to facilitate exploratory analyses with UK
Biobank phenotype data. A selection of columns from a raw UK Biobank
phenotype file can be loaded into R with human-readable labels:

*Raw data appearance:*

    #>      eid 31-0.0 34-0.0 21000-0.0 20002-0.0 21001-0.0
    #> 1: fake1      0   1952        NA      1665   20.1115
    #> 2: fake2      0   1946      4001      1383   30.1536
    #> 3: fake3      1   1951         3      1197   22.8495
    #> 4: fake4      0   1956        NA      1441   23.4904
    #> 5: fake5     NA     NA        -3      1429   29.2752

*Appearance when loaded using `ukbwranglr::read_pheno()`:*

    #>      eid sex_f31_0_0 year_of_birth_f34_0_0 ethnic_background_f21000_0_0
    #> 1: fake1      Female                  1952                         <NA>
    #> 2: fake2      Female                  1946                    Caribbean
    #> 3: fake3        Male                  1951       Asian or Asian British
    #> 4: fake4      Female                  1956                         <NA>
    #> 5: fake5        <NA>                    NA         Prefer not to answer
    #> 6: fake6        Male                  1948       Asian or Asian British
    #>    noncancer_illness_code_selfreported_f20002_0_0
    #> 1:                menopausal symptoms / menopause
    #> 2:                            dermatopolymyositis
    #> 3:        kidney stone/ureter stone/bladder stone
    #> 4:                                        malaria
    #> 5:                                     acromegaly
    #> 6:                                inguinal hernia
    #>    body_mass_index_bmi_f21001_0_0
    #> 1:                        20.1115
    #> 2:                        30.1536
    #> 3:                        22.8495
    #> 4:                        23.4904
    #> 5:                        29.2752
    #> 6:                        28.2567

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

You can install the development version of ukbwranglr from
[GitHub](https://github.com/rmgpanw/ukbwranglr/tree/dtable) with:

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
