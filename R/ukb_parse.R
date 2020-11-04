#? Functions to parse a raw UKB data file (replacing coded values with labelled factors converting dates to date format and removing nonsense dates/continuous values) create mapping df and rename columns descriptively

# To do -------------------------------------------------------------------
# - Tests: compare nabular dataframe gnerated from input vs output df. Should have same number of missing values
# - Could remove nesting steps and simply left_join the Codings cols to mapping_df?

# Load dependencies -------------------------------------------------------
# library(stringr)
# library(dplyr)
# library(progress)

# process_ukb_df_header() ------------------------------------------------------
# converts headers of the form 'f.5912.0.0' to 5912-0.0. Returns error message if headers not start with 'f.'

#' Process UKB header
#'
#' @param .ukb_df a raw ukb phenotypes dataframe
#' @param return_header_only if TRUE (default) will return the header as a vector, else returns the ukb_df renamed
#'
#' @return returns either the header as a vector or a renamed ukb df
#' @export
#' @import stringr
#'
#' @examples would convert headers of the form 'f.5912.0.0' to 5912-0.0. Returns error message if headers not start with 'f.'
process_ukb_df_header <- function(.ukb_df, return_header_only = TRUE) {
# extract header
header <- names(.ukb_df)

# only process if colnames start with 'f.'
  if (all(startsWith(header, prefix = 'f.'))) {
  # process
    header <- stringr::str_sub(header, start = 3L, end = -1L) %>% # remove 'f.'
      stringr::str_replace('\\.', '-') # replace the first '.' with '-'

    # return renamed .ukb_df or header only
    if (return_header_only == TRUE) {
      # return header
      return(header)
      } else {
        # replace .ukb_df header
        names(.ukb_df) <- header

        # return .ukb_df
        return(.ukb_df)
      }

# if colnames do NOT start with 'f.', then print message and return .ukb_df/header only
  } else {
    # print informative error message
    cat('No changes made to raw column names as they do not start with "f."')

    # return renamed .ukb_df or header only
    if (return_header_only == TRUE) {
      # return header
      return(header)
    } else {
      # return (unchanged) .ukb_df
      return(.ukb_df)
    }
  }
}

# ukb_mapping_generator() ----------------------------------------
# creates mapping df for FieldID / Codings / Column names (FieldID_instance) / descriptive_colnames in ukb_df
# also mutate a column called 'cont_int_to_na', indicating whether these fields will have special values converted to 'NA' using ukb_parse() in 'cleaning mode'
# Parameters:
## .ukb_df: a raw ukb phenotypes file
## .ukb_data_dict: the ukb data dictionary file

#' Title
#'
#' @param .ukb_df
#' @param .ukb_data_dict
#' @param reformat_header if TRUE, process header as for the .tab ukb data file
#'   made for R, where headers are of the form e.g. 'f.5912.0.0'. This would be
#'   converted to '5912-0.0'
#'
#' @return
#' @export
#' @import stringr
#' @import tidyr
#' @import dplyr
#'
#' @examples
ukb_mapping_generator <- function(.ukb_df, .ukb_data_dict, reformat_header = TRUE) {

  # Function to mutate descriptive_colnames column in
  ukb_rename_columns <- function(.mapping_df) {
    # create vector of column names and Field_FieldID names from Field descriptions/instance.array_indices
    column_names <- paste(.mapping_df$Field,
                          .mapping_df$FieldID,
                          .mapping_df$instance.array_indices)

    Field_FieldID_names <- paste(.mapping_df$Field,
                                 .mapping_df$FieldID)

    # replace the first with 'eid'
    column_names[1] <- 'eid'
    Field_FieldID_names[1] <- 'eid'

    # replace special characters
    ## characters to be replaced with "_"
    to_underscore <- c(" ",
                       "/")

    for (string in to_underscore) {
      column_names <- str_replace_all(column_names, string, "_")
      Field_FieldID_names <- str_replace_all(Field_FieldID_names, string, "_")
      }

    ## characters to replace with "" (i.e. to remove)
    to_remove <- c("\\(",
                   "\\)",
                   "\\-",
                   ",")

    for (string in to_remove) {
      column_names <- stringr::str_replace_all(column_names, string, "")
      Field_FieldID_names <- stringr::str_replace_all(Field_FieldID_names, string, "")
      }

    # mutate column with new, 'descriptive' column names and Field_FieldID_names
    .mapping_df[['descriptive_colnames']] <- column_names
    .mapping_df[['Field_FieldID']] <- Field_FieldID_names

    # return .ukb_df
    return(.mapping_df)
    }

  # main body of function

  # process header if of the form 'f.5912.0.0'
  if (reformat_header == TRUE) {
    names(.ukb_df) <- process_ukb_df_header(.ukb_df = .ukb_df)
  }

  ## make .ukb_df column names into a tibble and append 'mapping' columns
  mapping_df <- tibble(field_id_full = names(.ukb_df)) %>%
    # separate FieldID from full column names
    tidyr::separate(
      col = 'field_id_full',
      sep = "-",
      remove = FALSE,
      into = c('FieldID', 'instance.array_indices'),
      fill = 'right' # 'eid' column will not separate so will raise an error without this option
    ) %>%
    # append 'mapping' columns
    dplyr::left_join(y = .ukb_data_dict, by = 'FieldID')

  ## mutate 'descriptive_colnames' column
  mapping_df <- ukb_rename_columns(.mapping_df = mapping_df)

  ## mutate 'cont_int_to_na' column: indicates whether all special codings for a continuous/integer
    ## variable can be cleaned to 'NA' (see also `ukb_select_codings_to_na.Rmd`)
  mapping_df <- mapping_df %>%
    dplyr::mutate(cont_int_to_na  = case_when(

      # CONVERT TO NA -------------------------------
      Coding %in% c(
        # Continuous
        '13',
        '909',
        '1317',
        # Integer
        '100291',
        '100586',
        '37', # **NOTE: also see notes under 'Other notes' (below) re 'polymorphic data fields'
        '513',
        '485',
        '584',
        '100696',
        '170',
        '42',
        '525',
        '100584',
        '218'
      ) ~ TRUE,

      # DO *NOT* CONVERT TO NA  -------------------------------
      Coding %in% c(
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
      ) ~ FALSE,
      TRUE ~ FALSE
    ))

  ## return mapping_df
  return(mapping_df)
}

# ukb_parse() function ----------------------------------------------------
#' Parse a raw ukb df
#'
#' @param ukb_df a raw ukb phenotypes file, all columns must be of type 'string'
#' @param ukb_data_dict the ukb data dictionary file, all columns must be of
#'   type 'string'
#' @param ukb_codings the ukb codings file, all columns must be of type 'string'
#' @param rename_header if TRUE, rename columns with descriptive names
#' @param clean_dates if TRUE, convert 'nonsense' dates to 'NA'
#' @param clean_selected_continuous_and_integers if TRUE, convert special values
#'   to 'NA', see 'ukb_mapping_generator'
#' @param reformat_header if TRUE, process header as for the .tab ukb data file
#'   made for R, where headers are of the form e.g. 'f.5912.0.0'. This would be
#'   converted to '5912-0.0'
#'
#' @return
#' @export
#'
#' @import progress
#' @import dplyr
#' @import tidyr
#' @import tidyselect
#' @import purrr
#'
#' @examples
ukb_parse <- function(ukb_df,
                      ukb_data_dict,
                      ukb_codings,
                      rename_header = TRUE,
                      clean_dates = TRUE,
                      clean_selected_continuous_and_integers = TRUE,
                      reformat_header = FALSE) {
  # Parameters --------------------------------------------------------------
  ## ukb_df: a raw ukb phenotypes file
  ## ukb_data_dict: the ukb data dictionary file
  ## ukb_codings: the ukb codings file
  ## reformat_header: if TRUE, process header as for the .tab ukb data file made for R, where headers are of the form e.g. 'f.5912.0.0'. This would be converted to '5912-0.0'
  ## clean_dates: if TRUE, convert 'nonsense' dates to 'NA'
  ## clean_selected_continuous_and_integers: if TRUE, convert special values to 'NA', see 'ukb_mapping_generator'
  ### for list of which codings I have chosen where any special values get converted to 'NA'


  # Notes -------------------------------------------------------------------
  ## all columns should be of type 'character' for ukb_df, ukb_data_dict' and 'ukb_codings'

  print('Setting up...')

  # Functions ---------------------------------------------------------------

  ## filters ukb data dictionary for only fields in dataset. Requires a mapping_df generated by ukb_mapping_generator
  filter_data_dictionary <- function(.ukb_data_dict, .mapping_df) {
    .ukb_data_dict %>%
      dplyr::filter(FieldID %in% .mapping_df$FieldID)
  }

  ## filters ukb_codings for only codings in dataset
  filter_codings <- function(.ukb_codings, .mapping_df) {
    .ukb_codings %>%
      dplyr::filter(Coding %in% unique(.mapping_df$Coding))
  }

  ## extracts codings from a filtered ukb_data_dict nested by ValueType
  extract_codings <-
    function(.ukb_data_dict_filt_nested,
             .valuetype) {
      .ukb_data_dict_filt_nested[(.ukb_data_dict_filt_nested$ValueType == .valuetype), ]$data[[1]]$Coding
    }

  ## extracts Values/Meanings for a specified Coding from a filtered ukb_codings nested by 'Coding'
  extract_value_meaning <-
    function(.ukb_codings_filt_nested,
             .coding,
             .value_meaning) {
      # options: 'Value', 'Meaning')
      .ukb_codings_filt_nested[(.ukb_codings_filt_nested$Coding == .coding), ][[2]][[1]][[.value_meaning]]
    }

  # Main: setup --------------------------------------------------------------------

  # process header if of the form 'f.5912.0.0'
  if (reformat_header == TRUE) {
    names(ukb_df) <- process_ukb_df_header(.ukb_df = ukb_df)
  }

  # create mapping_df
  mapping_df <- ukb_mapping_generator(.ukb_df = ukb_df,
                                  .ukb_data_dict = ukb_data_dict)

  # filter ukb data dictionary file for fields in dataset and nest
  ukb_data_dict_filt_nested <-
    filter_data_dictionary(.ukb_data_dict = ukb_data_dict,
                           .mapping_df = mapping_df) %>%
    dplyr::group_by(ValueType) %>%
    tidyr::nest()

  # filterukb codings file for codings in dataset and nest
  ukb_codings_filt_nested <-
    filter_codings(.ukb_codings = ukb_codings,
                   .mapping_df = mapping_df) %>%
    dplyr::group_by(Coding) %>%
    tidyr::nest()

  # IMPORTANT: now reorder ukb_codings_filt_nested by Value
  ## ...otherwise, Fields like month of birth (ID = 52) will be correctly labelled BUT incorrectly levelled
  ## Notes on this:
  ### - Some codings (e.g. [Month of Birth](http://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=8)) should be ordered,
  ### whereas others are not (e.g. coding 3, ['Cancer'](http://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=3))
  ### - In `ukb_parse.R`, Values for Codings are 'character' type and therefore coercing to integer returns some NA's.
  ### - Selecting levels/labels in the order provided by `Codings.tsv` returns the wrong levels for these variables
  ### - ...in `ukb_parse.R` I therefore reorder these by integer value where possible (some cannot be converted to integer,
  ### in which case it's ok, they just do not get/need to be reordered), before assigning levels

  ukb_codings_filt_nested <- ukb_codings_filt_nested %>%
    dplyr::mutate(data = purrr::map(.x = data,
                                 .f = ~ suppressWarnings(.x %>% arrange(as.integer(
                                   Value
                                 )))))

  # Main: loop by coding ---------------------------------------------------------------
  # PARSES CATEGORICAL VARIABLES +/- CLEANS DATES/CONTINUOUS VARIABLES
  print('Processing categorical variables +/- cleaning dates/continuous variables...')

  # Set up progress bar
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                         total = length(unique(na.omit(mapping_df$Coding))))
  pb$tick(0)

  # loop through codings and parse columns depending on associated ValueType
  for (coding in unique(na.omit(mapping_df$Coding))) {

    # progress bar
    pb$tick(1)

    # categorical variables ---------------------------------------------------
    ## categorical
    if ((
      coding %in% extract_codings(
        .ukb_data_dict_filt_nested = ukb_data_dict_filt_nested,
        .valuetype = 'Categorical multiple'
      )
    ) |
    (
      coding %in% extract_codings(
        .ukb_data_dict_filt_nested = ukb_data_dict_filt_nested,
        .valuetype = 'Categorical single'
      )
    )) {
      ### define levels and labels
      .levels <-
        extract_value_meaning(
          .ukb_codings_filt_nested = ukb_codings_filt_nested,
          .coding = coding,
          .value_meaning = "Value"
        )

      .labels <-
        extract_value_meaning(
          .ukb_codings_filt_nested = ukb_codings_filt_nested,
          .coding = coding,
          .value_meaning = "Meaning"
        )

      ### identify Fields that use this coding
      selected_cols <- mapping_df %>%
        dplyr::filter(Coding == coding) %>%
        .$field_id_full

      ### now convert selected columns to labelled factors
      ukb_df <- ukb_df %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                      function(.x) {
                        ordered(.x, levels = .levels, labels = .labels)
                      }))

    # clean date variables ---------------------------------------------------
    ## Only execute this if clean_dates = TRUE
    } else if (
      ((coding %in% extract_codings(.ukb_data_dict_filt_nested = ukb_data_dict_filt_nested,
                                         .valuetype = 'Date')) &
      (clean_dates == TRUE))
      ) {

      ### identify Fields that use this coding
      selected_cols <- mapping_df %>%
        dplyr::filter(Coding == coding) %>%
        .$field_id_full

        #### vector of 'nonsense dates'
        nonsense_dates <- c(
          '1904-04-04',
          '1900-01-01',
          '1910-01-01',
          '1920-01-01',
          '1930-01-01',
          '1901-01-01',
          '1902-02-02',
          '1903-03-03',
          '2037-07-07'
        )

        #### create anonymous function to remove 'nonsense' dates
        special_values_to_na <- function(.x) {
          replace(.x, .x %in% nonsense_dates, NA)
        }

        #### remove 'nonsense' dates
        ukb_df <- ukb_df %>%
          dplyr::mutate(across(all_of(selected_cols),
                        special_values_to_na))

        # continuous variables ---------------------------------------------------
      ## continuous - note, only a few continuous FieldIDs have an associated coding, one of which
        ## should not be automatically set to NA
      ## Only execute this if clean_selected_continuous_and_integers = TRUE
    } else if (
      (clean_selected_continuous_and_integers == TRUE) &
      # (coding %in% extract_codings(
      #   .ukb_data_dict_filt_nested = ukb_data_dict_filt_nested,
      #   .valuetype = 'Continuous')
      # ) &
      (mapping_df %>%
       dplyr::filter(ValueType == 'Continuous' & Coding == coding) %>%
       dplyr::slice(1L) %>%
       .$cont_int_to_na %>% # 'isTRUE' will return 'FALSE' if this statement returns 'logical(0)' i.e. no rows returned, whereas '== TRUE' throws an error
       isTRUE)) {

      ### extract special coding values
      .values <-
        extract_value_meaning(
          .ukb_codings_filt_nested = ukb_codings_filt_nested,
          .coding = coding,
          .value_meaning = "Value"
        )

      ### create anonymous function to replace these special values
      special_values_to_na <- function(.x) {
        replace(.x, .x %in% .values, NA)
      }

      ### identify Fields that use this coding
      selected_cols <- mapping_df %>%
        dplyr::filter(Coding == coding) %>%
        .$field_id_full

      ### replace special coding values with 'NA' for selected columns
      ukb_df <- ukb_df %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                      special_values_to_na))

      # integer variables ---------------------------------------------------
      ## see ukb_mapping_generator() and ukb_select_codings_to_na.Rmd for details re which
      ## Codings I think can be automatically set to NA
      ## Only execute this if clean_selected_continuous_and_integers = TRUE
    } else if (
      (clean_selected_continuous_and_integers == TRUE) &
      # (coding %in% extract_codings(
      #   .ukb_data_dict_filt_nested = ukb_data_dict_filt_nested,
      #   .valuetype = 'Integer')
      # ) &
      (mapping_df %>%
       dplyr::filter(ValueType == 'Integer' & Coding == coding) %>%
       dplyr::slice(1L) %>%
       .$cont_int_to_na %>% # 'isTRUE' will return 'FALSE' if this statement returns 'logical(0)' i.e. no rows returned, whereas '== TRUE' throws an error
       isTRUE)) {

      ### extract special coding values
      .values <-
        extract_value_meaning(
          .ukb_codings_filt_nested = ukb_codings_filt_nested,
          .coding = coding,
          .value_meaning = "Value"
        )

      ### create anonymous function to replace these special values
      special_values_to_na <- function(.x) {
        replace(.x, .x %in% .values, NA)
      }

      ### identify Fields that use this coding
      selected_cols <- mapping_df %>%
        dplyr::filter(Coding == coding) %>%
        .$field_id_full

      ### replace special coding values with 'NA' for selected columns
      ukb_df <- ukb_df %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                      special_values_to_na))
    }
}

  # Main: convert dates/continuous/integer variables -------------------------------------------------
    # Dates -------------------------------------------------------------------
    print('Converting dates...')
    # extract column names
    selected_cols <- mapping_df %>%
      dplyr::filter(ValueType == 'Date') %>%
      .$field_id_full

    ### convert selected columns to date
    ukb_df <- ukb_df %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                    as.Date))

    # Continuous -------------------------------------------------------------------
    print('Converting continuous variables...')
    # extract column names
    selected_cols <- mapping_df %>%
      dplyr::filter(ValueType == 'Continuous') %>%
      .$field_id_full

    ### convert selected columns to numeric
    ukb_df <- ukb_df %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                    as.numeric))

    # Integer -------------------------------------------------------------------
    print('Converting integer variables...')
    # extract column names
    selected_cols <- mapping_df %>%
      dplyr::filter(ValueType == 'Integer') %>%
      .$field_id_full

    ### convert selected columns to integer
    ukb_df <- ukb_df %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(selected_cols),
                    as.integer))

  # Rename columns ----------------------------------------------------------
  print('Renaming columns...')
  if (rename_header == TRUE) {
    names(ukb_df) <- mapping_df$descriptive_colnames
  }

  print('...Complete!')

  # Message re 'uncleaned' cols if in 'cleaning mode' i.e. number of continuous/integer columns with special values NOT set to 'NA'
  if (clean_selected_continuous_and_integers == TRUE) {
    cat(
      '\n\nNumber of continuous/integer fields in this dataset that still have "uncleaned" special values:',
      mapping_df %>%
        dplyr::filter(
          ValueType %in% c('Continuous', 'Integer') &
            !is.na(Coding) &
            cont_int_to_na == FALSE
        ) %>%
        nrow()
    )
  }

  # Return output -----------------------------------------------------------
  return(ukb_df)
}
