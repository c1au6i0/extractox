% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extr_ctd.R
\name{extr_ctd}
\alias{extr_ctd}
\title{Extract Data from the CTD API}
\usage{
extr_ctd(
  input_terms,
  category = "chem",
  report_type = "genes_curated",
  input_term_search_type = "directAssociations",
  action_types = NULL,
  ontology = NULL,
  verify_ssl = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{input_terms}{A character vector of input terms such as CAS numbers or IUPAC names.}

\item{category}{A string specifying the category of data to query. Valid options
are "all", "chem", "disease", "gene", "go", "pathway", "reference", and "taxon".
Default is "chem".}

\item{report_type}{A string specifying the type of report to return. Default is
"genes_curated". Valid options include:
\describe{
\item{"cgixns"}{Curated chemical-gene interactions. Requires at least one
\code{action_types} parameter.}
\item{"chems"}{All chemical associations.}
\item{"chems_curated"}{Curated chemical associations.}
\item{"chems_inferred"}{Inferred chemical associations.}
\item{"genes"}{All gene associations.}
\item{"genes_curated"}{Curated gene associations.}
\item{"genes_inferred"}{Inferred gene associations.}
\item{"diseases"}{All disease associations.}
\item{"diseases_curated"}{Curated disease associations.}
\item{"diseases_inferred"}{Inferred disease associations.}
\item{"pathways_curated"}{Curated pathway associations.}
\item{"pathways_inferred"}{Inferred pathway associations.}
\item{"pathways_enriched"}{Enriched pathway associations.}
\item{"phenotypes_curated"}{Curated phenotype associations.}
\item{"phenotypes_inferred"}{Inferred phenotype associations.}
\item{"go"}{All Gene Ontology (GO) associations. Requires at least one
\code{ontology} parameter.}
\item{"go_enriched"}{Enriched GO associations. Requires at least one
\code{ontology} parameter.}
}}

\item{input_term_search_type}{A string specifying the search method to use.
Options are "hierarchicalAssociations" or "directAssociations". Default is
"directAssociations".}

\item{action_types}{An optional character vector specifying one or more interaction
types for filtering results. Default is "ANY".
Other acceptable inputs are "abundance", "activity", "binding", "cotreatment",
"expression", "folding", "localization", "metabolic processing"...
See https://ctdbase.org/tools/batchQuery.go for a full list.}

\item{ontology}{An optional character vector specifying one or more ontologies
for filtering GO reports. Default NULL.}

\item{verify_ssl}{Boolean to control of SSL should be verified or not.}

\item{verbose}{A logical value indicating whether to print detailed messages.
Default is TRUE.}

\item{...}{Any other arguments to be supplied to \code{req_option} and thus to \code{libcurl}.}
}
\value{
A data frame containing the queried data in CSV format.
}
\description{
This function queries the Comparative Toxicogenomics Database API to retrieve data related to chemicals, diseases, genes, or other categories.
}
\examples{
\donttest{
input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")
dat <- extr_ctd(
  input_terms = input_terms,
  category = "chem",
  report_type = "genes_curated",
  input_term_search_type = "directAssociations",
  action_types = "ANY",
  ontology = c("go_bp", "go_cc")
)
str(dat)

# Get expresssion data
dat2 <- extr_ctd(
  input_terms = input_terms,
  report_type = "cgixns",
  category = "chem",
  action_types = "expression"
)

str(dat2)
}
}
\references{
\itemize{
\item Davis, A. P., Grondin, C. J., Johnson, R. J., Sciaky, D., McMorran, R.,
Wiegers, T. C., & Mattingly, C. J. (2019).
The Comparative Toxicogenomics Database: update 2019. Nucleic acids research,
47(D1), D948–D954. \doi{10.1093/nar/gky868}
}
}
\seealso{
\href{http://ctdbase.org}{Comparative Toxicogenomics Database}
}
