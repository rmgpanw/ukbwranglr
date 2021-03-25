# GENERATE RAW DUMMY UKB DATA FOR PACKAGE

# read selected columns from dummy_ukb_data.csv, downloaded manually from ukbwranglr_resources repo
# (https://github.com/rmgpanw/ukbwranglr_resources)
dummy_data <- data.table::fread(
  file = "data-raw/dummy_ukb_data.csv",
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
