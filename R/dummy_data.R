
# EXPORTED FUNCTIONS ------------------------------------------------------

#' Example clinical events data from a UK Biobank main dataset
#'
#' Example dummy clinical events data that can be tidied with \code{\link{tidy_clinical_events}}
#'
#' @return A data frame of dummy data
#' @export
#'
#' @seealso \code{\link{tidy_clinical_events}}
#'
#' @examples
#' dummy_main_dataset_clinical_events()
dummy_main_dataset_clinical_events <- function() {
  DUMMY_UKB_MAIN_CLINICAL_EVENTS
}

# PRIVATE FUNCTIONS -------------------------------------------------------

