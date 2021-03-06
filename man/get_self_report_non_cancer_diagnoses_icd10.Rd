% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_phenotypes.R
\name{get_self_report_non_cancer_diagnoses_icd10}
\alias{get_self_report_non_cancer_diagnoses_icd10}
\title{Get self-reported non-cancer diagnoses (ICD10)}
\usage{
get_self_report_non_cancer_diagnoses_icd10(
  ukb_pheno,
  data_dict,
  ukb_codings,
  remove_special_dates = TRUE
)
}
\arguments{
\item{ukb_pheno}{a UK Biobank phenotype dataset. Must be a data table}

\item{data_dict}{a data dictionary for \code{ukb_pheno}. Can be a "filtered"
version including only the columns to be summarised.}

\item{ukb_codings}{The UKB codings dictionary. This should be a data frame
where all columns are of type \code{character}. By default, this is
downloaded from the
\href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
github repo.}

\item{remove_special_dates}{Logical. Remove 'special' date values if
requested. Default is \code{TRUE}. See
\href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=13}{data coding 13}.}
}
\value{
Dataframe
}
\description{
Returns a long format dataframe with all self-reported non-cancer diagnoses
mapped to 3-character ICD10 codes with associated dates for each UK Biobank participant.
}
\details{
Reformats the data for FieldIDs 20002 and 20009 (self-reported non-cancer
diagnoses and dates, see
\href{https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=100074}{category 100074}).

\strong{Note: not all self-reported medical conditions map to ICD10 codes.}
(See \href{https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=609}{data coding
609})
}
\seealso{
Other get all diagnostic codes: 
\code{\link{get_all_diagnostic_codes_multi}()},
\code{\link{get_cancer_register_icd10_diagnoses}()},
\code{\link{get_cancer_register_icd9_diagnoses}()},
\code{\link{get_death_data_icd10_diagnoses}()},
\code{\link{get_diagnoses_set_index_code_date_cols}()},
\code{\link{get_hes_icd10_diagnoses}()},
\code{\link{get_hes_icd9_diagnoses}()},
\code{\link{get_hes_opcs3_operations}()},
\code{\link{get_hes_opcs4_operations}()},
\code{\link{get_self_report_cancer_diagnoses}()},
\code{\link{get_self_report_non_cancer_diagnoses}()},
\code{\link{main_dataset_diagnoses_to_sqlite_db}()}
}
\concept{get all diagnostic codes}
