% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype_algorithms.R
\name{diabetes_type_first_occurrence}
\alias{diabetes_type_first_occurrence}
\title{Ascertain diabetes type from first occurrence fields}
\usage{
diabetes_type_first_occurrence(ukb_pheno, data_dict)
}
\arguments{
\item{ukb_pheno}{a UK Biobank phenotype dataset. Must be a data table}

\item{data_dict}{a data dictionary for \code{ukb_pheno}. Can be a "filtered"
version including only the columns to be summarised.}
}
\value{
The UK Biobank dataset (supplied to arg \code{ukb_pheno}) with an additional indicator column
  for diabetes type named "diabetes_type_first_occurrence"
}
\description{
Mutates an indicator column for diabetes type based on UKB first occurrence
fields
}
\details{
Categories in indicator column:

\itemize{
 \item "T1DM" - date recorded for T1DM (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130706}{FieldID 130706}) but no other type of diabetes
 \item "T2DM" - date recorded for T2DM (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130708}{FieldID 130708}) but no other type of diabetes
 \item "T1_and_T2DM" - dates recorded for both T1DM and T2DM
 \item "DM_not_T1/T2" - date for other DM type (FieldIDs \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130712}{130712}, \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130710}{130710}, \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130714}{130714} and \href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=132202}{132202}), but no date for either T1 or T2
 \item "other_DM_and_T1/T2" - date for T1/T2DM AND other DM type
 \item "DM_unspecified" - date for DM unspecified only (\href{https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=130714}{FieldID 130714})
}
}
