% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extr_pubchem.R
\name{extr_casrn_from_cid}
\alias{extr_casrn_from_cid}
\title{Retrieve CASRN for PubChem CIDs}
\usage{
extr_casrn_from_cid(pubchem_ids, verbose = TRUE)
}
\arguments{
\item{pubchem_ids}{A numeric vector of PubChem CIDs. These are unique identifiers
for chemical compounds in the PubChem database.}

\item{verbose}{A logical value indicating whether to print detailed messages.
Default is TRUE.}
}
\value{
A data frame containing the CID, CASRN, and IUPAC name of the compound.
The returned data frame includes three columns:
\describe{
\item{CID}{The PubChem Compound Identifier.}
\item{casrn}{The corresponding CASRN of the compound.}
\item{iupac_name}{The IUPAC name of the compound.}
\item{query}{The pubchem_id queried.}
}
}
\description{
This function retrieves the CASRN for a given set of PubChem Compound Identifiers (CID).
It queries PubChem through the \code{webchem} package and extracts the CASRN from
the depositor-supplied synonyms.
}
\examples{
\donttest{
# Example with formaldehyde and aflatoxin
cids <- c(712, 14434) # CID for formaldehyde and aflatoxin B1
extr_casrn_from_cid(cids)
}
}
\seealso{
\href{https://pubchem.ncbi.nlm.nih.gov/}{PubChem}
}
