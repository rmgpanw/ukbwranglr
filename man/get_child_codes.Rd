% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clinical_codes.R
\name{get_child_codes}
\alias{get_child_codes}
\title{Get child codes for a clinical code type}
\usage{
get_child_codes(
  codes,
  code_type,
  ukb_code_mappings = get_ukb_code_mappings(),
  codes_only = TRUE,
  preferred_description_only = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{codes}{character. A vector of codes to get the child codes for}

\item{code_type}{character. The type of clinical code system to be searched.
Must be one of \code{read2}, \code{read3}, \code{icd9}, \code{icd10},
\code{bnf}, \code{dmd}, \code{read2_drugs} or \code{opcs4}.}

\item{ukb_code_mappings}{the \code{ukb_code_mappings} list returned by
\code{\link{get_ukb_code_mappings}}.}

\item{codes_only}{bool. If \code{TRUE} (default), return a character vector
of \emph{unique} codes. If \code{FALSE}, return a data frame of all results
including code descriptions (useful for manual validation).}

\item{preferred_description_only}{bool. Return only preferred descriptions
for clinical codes with synonyms. Default value is \code{TRUE}.}

\item{quiet}{bool. Warning message if any of \code{codes} are not found for
the supplied \code{code_type}.}
}
\description{
Uses the code mapping file provided by UK Biobank (resource 592:
https://biobank.ndph.ox.ac.uk/ukb/refer.cgi?id=592)
}
\details{
This is case \emph{sensitive} (important for read codes especially).
}
\seealso{
Other Clinical code lookups and mappings: 
\code{\link{lookup_codes}()},
\code{\link{map_codes}()},
\code{\link{reformat_icd10_codes}()}
}
\concept{Clinical code lookups and mappings}
