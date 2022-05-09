## code to prepare `n_labels_per_coding` dataset goes here - number of labels
## per data coding. Used by `label_ukb_main()`, which by default does not label
## codings with many labels e.g. ICD10, OPCS4

library(tidyverse)
library(ukbwranglr)

ukb_codings <- get_ukb_codings()

n_labels_per_coding <- ukb_codings %>%
  dplyr::count(.data[["Coding"]])

usethis::use_data(n_labels_per_coding,
                  overwrite = TRUE,
                  internal = TRUE)
