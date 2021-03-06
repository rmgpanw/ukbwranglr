% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinical_codes.R
\name{lookup_codes}
\alias{lookup_codes}
\title{Look up descriptions for clinical codes}
\usage{
lookup_codes(
  codes,
  code_type,
  ukb_code_mappings = get_ukb_code_mappings(),
  preferred_description_only = TRUE,
  standardise_output = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{codes}{character. Vector of codes to lookup}

\item{code_type}{character. The type of clinical code system to be searched.
Must be one of \code{read2}, \code{read3}, \code{icd9}, \code{icd10},
\code{bnf}, \code{dmd}, \code{read2_drugs} or \code{opcs4}.}

\item{ukb_code_mappings}{the \code{ukb_code_mappings} list returned by
\code{\link{get_ukb_code_mappings}}.}

\item{preferred_description_only}{bool. Return only preferred descriptions
for clinical codes with synonyms. Default value is \code{TRUE}.}

\item{standardise_output}{bool. If \code{TRUE} (default), outputs a data
frame with columns named 'code', 'description' and 'code_type'. Otherwise
returns a data frame with all columns for the relevant lookup sheet from
(\href{https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592}{UK Biobank
resource 592}).}

\item{quiet}{bool. Warning message if any of \code{codes} are not found for
the supplied \code{code_type}.}
}
\value{
data frame
}
\description{
Returns a data frame including descriptions for the codes of interest
}
\seealso{
Other Clinical code lookups and mappings: 
\code{\link{get_child_codes}()},
\code{\link{map_codes}()},
\code{\link{reformat_icd10_codes}()}
}
\concept{Clinical code lookups and mappings}
