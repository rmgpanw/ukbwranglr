# After adding any new dummy data fields to `dummy_data.R`, update
# `dummy_Data_Dictionary_Showcase.tsv` and `dummy_Codings.tsv` by running
# `gen_dummy_ukb_data_dict_and_codings.R` in `data-raw` dir. Note that only manually
# selected codings should be kept for ICD/OPCS4 etc to minimise file size.

# These are also used by the codemapper package, so codemapper tests should also
# be checked after any updates here.

library(tidyverse)
library(ukbwranglr)

# 'large' codings that require manual selection, based on `dummy_ukb_main.tsv`,
# and codemapper package
large_codings <- c(
  # ICD10
  "19",

  # ICD9
  "87",

  # OPCS4
  "240",

  # OPCS3
  "259",

  # Self-reported cancer, non-cancer, medication, operation
  "3",
  "6",
  "4",
  "5"
)

# get UKB data dict and codings
ukb_data_dict <- get_ukb_data_dict()
ukb_codings <- get_ukb_codings()

# get dummy ukb_main and make data_dict
dummy_ukb_main_data_dict <- get_ukb_dummy(file_name = "dummy_ukb_main.tsv") %>%
  make_data_dict()

# tidy clinical events
dummy_ukb_main <- read_ukb(dummy_ukb_data_raw_path,
                           data_dict = dummy_data_dict,
                           ukb_data_dict = dummy_ukb_data_dict,
                           ukb_codings = dummy_ukb_codings)

# tidy dummy clinical events
dummy_clinical_events_list <-
  tidy_clinical_events(
    ukb_main = dummy_ukb_main,
    ukb_data_dict = dummy_ukb_data_dict,
    ukb_codings = dummy_ukb_codings,
    clinical_events = names(CLINICAL_EVENTS_FIELD_IDS),
    strict = TRUE,
    .details_only = FALSE
  )

get_selected_codings_only <- function(.x, .y) {
  # get code FID for each source
  code_fid <- CLINICAL_EVENTS_FIELD_IDS[[.y]]["code_fid"]

  # get associated data coding
  coding <- ukb_data_dict %>%
    filter(FieldID == !!code_fid) %>%
    pull(Coding)

  # filter ukb_codings for selected coding, and only values that are present
  # in dummy_clinical_events_list[[.x]]
  result <- ukb_codings %>%
    filter(Coding == !!coding) %>%
    filter(Value %in% .x$code)

  return(result)
}

selected_large_codings <- dummy_clinical_events_list %>%
  compact() %>%
  imap(~ get_selected_codings_only(.x = .x, .y = .y)) %>%
  bind_rows()

# filter ukb_data_dict and ukb_codings for required fields/codings, and write to
# tsv files
dummy_data_dir <- "inst/extdata/dummy_tsv"

# data dict
dummy_Data_Dictionary_Showcase <- ukb_data_dict %>%
  filter(FieldID %in% dummy_ukb_main_data_dict$FieldID)

dummy_Data_Dictionary_Showcase %>%
  write_tsv(file.path(dummy_data_dir, "dummy_Data_Dictionary_Showcase.tsv"))

# codings
dummy_Codings <- ukb_codings %>%
  # keep codings in dummy data dict, and 609 (map from self-reported non-cancer
  # illness to ICD10)
  filter(Coding %in% c("609",
                       dummy_ukb_main_data_dict$Coding)) %>%

  # do not include ICD/Read/OPCS4 codings
  filter(!Coding %in% large_codings) %>%

  # add in selected_large_codings
  bind_rows(selected_large_codings)

# write result
dummy_Codings %>%
  write_tsv(file.path(dummy_data_dir, "dummy_Codings.tsv"))
