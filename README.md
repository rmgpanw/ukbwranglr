
  - [ukbwranglr
    <img src="test.png" align="right" width="100" />](#ukbwranglr)
  - [Overview](#overview)
      - [Installation](#installation)
      - [Basic usage - reading a UK Biobank dataset into
        R](#basic-usage---reading-a-uk-biobank-dataset-into-r)
          - [Create data dictionary](#create-data-dictionary)
          - [Filter for required variables
            (optional)](#filter-for-required-variables-optional)
          - [Read selected columns into
            R](#read-selected-columns-into-r)
      - [TODO](#todo)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ukbwranglr <img src="test.png" align="right" width="100" />

<!-- badges: start -->

<!-- badges: end -->

# Overview

The goal of `ukbwranglr` is to make getting started with UK Biobank data
easier. A raw UK Biobank phenotype file (or subset of) can be loaded
into R with human-readable labels without requiring any additional
dependencies. A basic usage example for this provided below.

Raw data appearance:

    #>      eid 31-0.0 34-0.0 21000-0.0 20002-0.0 21001-0.0
    #> 1: fake1      0   1969      3001      1466   32.0150
    #> 2: fake2      1   1955         1      1531        NA
    #> 3: fake3      0   1959      3001      1574   24.1923
    #> 4: fake4      0   1952      4001      1159   19.2004
    #> 5: fake5      1   1946        -1      1207        NA

Appearance when loaded using `ukbwranglr::read_pheno()`:

    #>      eid sex_f31_0_0 year_of_birth_f34_0_0 ethnic_background_f21000_0_0
    #> 1: fake1      Female                  1969                       Indian
    #> 2: fake2        Male                  1955                        White
    #> 3: fake3      Female                  1959                       Indian
    #> 4: fake4      Female                  1952                    Caribbean
    #> 5: fake5        Male                  1946                  Do not know
    #>    noncancer_illness_code_selfreported_f20002_0_0
    #> 1:                                           gout
    #> 2:                          post-natal depression
    #> 3:                                     diphtheria
    #> 4:                              bile duct disease
    #> 5:                  prostate problem (not cancer)
    #>    body_mass_index_bmi_f21001_0_0
    #> 1:                        32.0150
    #> 2:                             NA
    #> 3:                        24.1923
    #> 4:                        19.2004
    #> 5:                             NA

Also included are functions to help with pre-processing the data into an
analysable format. Please see [‘getting started’]() on the [package
documentation website](https://rmgpanw.github.io/ukbwranglr/index.html)
for further details.

## Installation

You can install the development version of ukbwranglr from
[GitHub](https://github.com/rmgpanw/ukbwranglr/tree/dtable) with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/ukbwranglr")
```

## Basic usage - reading a UK Biobank dataset into R

The basic work flow is to:

1.  Create a data dictionary specific to your UK Biobank file - this is
    generated by downloading the full UK Biobank data dictionary,
    available on their website
    [here](https://biobank.ctsu.ox.ac.uk/crystal/exinfo.cgi?src=accessing_data_guide)
2.  Filter this for the variables you require (optional)
3.  Read your selected columns into R

This process is outlined below. Details of how to do this using dummy UK
Biobank data (included with this package) are [here](TODO)

### Create data dictionary

``` r
# load package
library(ukbwranglr)

# create data dictionary
data_dict <- pheno_data_dict("PATH_TO_MY_UKB_FILE.tab")
```

### Filter for required variables (optional)

*Note: you could also write the data dictionary to a `.csv` file and
filter manually in excel*

``` r
# Filter for sex, year of birth, BMI and self-reported noncancer-illness fields
data_dict <- data_dict %>%
  dplyr::filter(FieldID %in% c(31,
                               34,
                               21001,
                               20002))
```

### Read selected columns into R

``` r
ukb_pheno <- read_pheno(path = "PATH_TO_MY_UKB_FILE.tab", 
                        ukb_data_dict = data_dict)
```

## TODO

  - Add globalvariables() to remove notes when runnning `check()` (see
    <https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/>)
  - Redo `read_pheno()` to use data.table syntax
