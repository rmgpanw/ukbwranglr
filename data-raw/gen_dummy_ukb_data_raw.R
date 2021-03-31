# GENERATE RAW DUMMY UKB DATA FOR PACKAGE

# read selected columns from dummy_ukb_data.csv, downloaded from ukbwranglr_resources repo
# (https://github.com/rmgpanw/ukbwranglr_resources)
full_dummy_data_path <- download_dummy_ukb_data_to_tempdir()

dummy_data <- data.table::fread(
  file = full_dummy_data_path,
  colClasses = c('character'),
  sep = ",",
  quote = " ",
  na.strings = c("", "NA"),
  select = c(
    'eid',
    '31-0.0',
    '34-0.0',
    '21000-0.0',
    '20002-0.0',
    '21001-0.0'
  )
)

# write to csv file
readr::write_csv(dummy_data, file = "inst/extdata/dummy_ukb_data.csv")
