% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_phenotypes.R
\name{get_all_diagnostic_codes_multi}
\alias{get_all_diagnostic_codes_multi}
\title{Get all diagnostic codes from multiple data sources}
\usage{
get_all_diagnostic_codes_multi(
  function_list = list(get_self_report_non_cancer_diagnoses,
    get_self_report_non_cancer_diagnoses_icd10, get_self_report_cancer_diagnoses,
    get_hes_icd9_diagnoses, get_hes_icd10_diagnoses, get_death_data_icd10_diagnoses,
    get_cancer_register_icd9_diagnoses, get_cancer_register_icd10_diagnoses),
  ukb_pheno,
  data_dict,
  ukb_codings
)
}
\arguments{
\item{function_list}{A list of \code{get_XXXX_diagnoses} functions.}

\item{ukb_pheno}{a UK Biobank phenotype dataset. Must be a data table}

\item{data_dict}{a data dictionary for \code{ukb_pheno}. Can be a "filtered"
version including only the columns to be summarised.}

\item{ukb_codings}{The UKB codings dictionary. This should be a data frame
where all columns are of type \code{character}. By default, this is
downloaded from the
\href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
github repo.}
}
\value{
Dataframe
}
\description{
Extract diagnostic codes from multiple sources
}
\details{
Loops through a list of functions (\code{function_list}) from the 'get all
diagnostic codes' family and combines the results into a single data frame.
}
\seealso{
Other get all diagnostic codes: 
\code{\link{get_cancer_register_icd10_diagnoses}()},
\code{\link{get_cancer_register_icd9_diagnoses}()},
\code{\link{get_death_data_icd10_diagnoses}()},
\code{\link{get_diagnoses_set_index_code_date_cols}()},
\code{\link{get_hes_icd10_diagnoses}()},
\code{\link{get_hes_icd9_diagnoses}()},
\code{\link{get_hes_opcs3_operations}()},
\code{\link{get_hes_opcs4_operations}()},
\code{\link{get_self_report_cancer_diagnoses}()},
\code{\link{get_self_report_non_cancer_diagnoses_icd10}()},
\code{\link{get_self_report_non_cancer_diagnoses}()},
\code{\link{main_dataset_diagnoses_to_sqlite_db}()}
}
\concept{get all diagnostic codes}
