
# NOTES -------------------------------------------------------------------

# Contains functions under development and some related constants


# CONSTANTS ---------------------------------------------------------------

# ukb special codings -----------------------------------------------------------------

# Used by read_ukb.R
# codings that should, or should not, be converted to NA

CONT_AND_INT_CODINGS_TO_NA <- c(
  # Continuous
  '13',
  '909',
  '1317',
  # Integer
  '100291',
  '100586',
  '37',
  ## **NOTEre 37: also see notes under 'Other notes' in
  ## `ukb_select_codings_to_na.Rmd` re 'polymorphic data fields'
  '513',
  '485',
  '584',
  '100696',
  '170',
  '42',
  '525',
  '100584',
  '218'
)

CONT_AND_INT_CODINGS_NOT_TO_NA<- c(
  # Continuous
  '488',
  # Integer
  '100373', # -10 <- 'Less than one'
  '100329', # -10 <- 'Less than an hour a day'
  '528',
  '100290',
  '100306',
  '100567',
  '100569',
  '100353', # number of cigarettes previously smoked daily
  '487',
  '100298',
  '100300',
  '100307',
  '100355', # number of cigarettes currently smoked daily
  '100504',
  '100537',
  '100582',
  '100585',
  '100595',
  '100598',
  '530',
  '946',
  '957',
  '100698',
  '17',
  '1990',
  '402',
  '511',
  '517',
  '6361'
)

# EXPORTED FUNCTIONS ------------------------------------------------------


# PRIVATE FUNCTIONS -------------------------------------------------------

#' Flag continuous/integer codings as NA
#'
#' Indicate which continuous/integer codings can be converted to \code{NA}
#' (decided through manual inspection)
#'
#' @param data_dict data frame created by \code{make_data_dict}
#'
#' @return data frame
#' @noRd
function(data_dict) {
  # mutate 'cont_int_to_na' column: indicates whether all special codings for a
  # continuous/integer variable can be cleaned to 'NA' (see also
  # `ukb_select_codings_to_na.Rmd`)
  data_dict$cont_int_to_na <- dplyr::case_when(
    # CONVERT TO NA
    data_dict$Coding %in% CONT_AND_INT_CODINGS_TO_NA ~ TRUE,

    # DO *NOT* CONVERT TO NA
    data_dict$Coding %in% CONT_AND_INT_CODINGS_NOT_TO_NA ~ FALSE,
    TRUE ~ FALSE
  )

  return(data_dict)
}
