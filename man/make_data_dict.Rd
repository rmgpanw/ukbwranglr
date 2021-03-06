% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_phenodataset.R
\name{make_data_dict}
\alias{make_data_dict}
\title{Generate a data dictionary from a raw UK Biobank phenotype file}
\usage{
make_data_dict(ukb_pheno, delim = "\\t", ukb_data_dict = get_ukb_data_dict())
}
\arguments{
\item{ukb_pheno}{Either the path to a UKB phenotype file or the file already
loaded into R as a data frame.}

\item{delim}{Delimiter for the UKB phenotype file. Default is "\\t". Ignored
if the file name ends with \code{.dta} (i.e. is a \code{STATA} file).}

\item{ukb_data_dict}{The UKB data dictionary. This should be a data frame
where all columns are of type \code{character}. By default, this is
downloaded from the
\href{https://github.com/rmgpanw/ukbwranglr_resources}{\code{ukbwranglr_resources}}
 github repo.}
}
\value{
A data dictionary (data table) specific to the specified UKB
  phenotype. This includes a column of descriptive names. TODO: flesh this
  out
}
\description{
Generate a data dictionary from a raw UK Biobank phenotype file
}
