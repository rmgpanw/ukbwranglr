# NOTES -------------------------------------------------------------------

#TODO
#

# EXPORTED FUNCTIONS ----------------------------------------------------


# PRIVATE FUNCTIONS -------------------------------------------------------

#' Convert column for a FieldID to long format
#'
#' Selects the columns relating to a specified FieldID (plus the 'eid' column)
#' and applies \code{\link[dplyr]{pivot_longer}}.
#'
#' @param field_id Character. A UK Biobank Field ID
#' @inheritParams read_pheno
#' @inheritParams summarise_rowise
#'
#' @return A dataframe with 3 columns: \itemize{ \item eid \item column names
#'   (labelled as the specified `field_id`, prefixed by 'f') \item column values
#'   (labelled as he specified `field_id`, prefixed by 'f' and suffixed by
#'   '_value') }
#'
field_id_pivot_longer <- function(ukb_pheno,
                                 field_id,
                                 data_dict,
                                 ukb_codings) {

  # filter ukb_pheno for selected cols (eid + fieldid cols)
  selected_cols <- get_colname(field_id = field_id,
                               ukb_mapping_df = data_dict)

  ukb_pheno <- ukb_pheno %>%
    select(eid,
           all_of(selected_cols))

  # get codings for fieldid
  field_id_codings <- ukb_codings %>%
    filter(Coding == (data_dict %>%
                        filter(FieldID == field_id) %>%
                        .$Coding %>%
                        head(n = 1)))

  ukb_pheno <- ukb_pheno %>%
    pivot_longer(
      cols = all_of(selected_cols),
      values_to = paste(paste0("f", field_id), "value", sep = "_")
    ) %>%
    mutate(instance_array = colname_to_field_inst_array_df(name)$instance_array)

  # rename
  names(ukb_pheno)[which(names(ukb_pheno) == 'name')] <- paste0("f", field_id)

  return(ukb_pheno)
}
