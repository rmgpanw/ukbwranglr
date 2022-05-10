
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
[![DOI](https://zenodo.org/badge/308650225.svg)](https://zenodo.org/badge/latestdoi/308650225)
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

1.  Create a data dictionary for your main UK Biobank dataset with
    `make_data_dict()`.
2.  Read selected variables into R with `read_ukb()`.
3.  Summarise continuous variables with
    `summarise_numerical_variables()`.
4.  Tidy clinical events data and extract outcomes of interest with
    `tidy_clinical_events()` and `extract_phenotypes()`.
5.  Analyse.

Please see `vignette('ukbwranglr')` for further details.

[^1]: For example, calculating a mean/minimum/maximum body mass index
    (BMI) from repeated BMI measurements.

[^2]: For example, identifying participants with a diagnosis of
    hypertension from linked primary and secondary health care records.
