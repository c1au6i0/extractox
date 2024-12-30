#' Retrieve WHO IARC Monograph Information
#'
#' This function retrieves information regarding Monographs from the World Health Organization (WHO) International
#' Agency for Research on Cancer (IARC)  based on CAS Registry Number or Name of the chemical.
#'
#' @param search_type A character string specifying the type of search to perform. Valid options are "cas_rn" (CAS Registry Number)
#'   and "name" (name of the chemical). If `search_type` is "casrn", the function filters by the CAS Registry Number.
#'   If `search_type` is "name", the function performs a partial match search for the chemical name.
#' @param ids A character vector of IDs to search for.
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @return A data frame containing the relevant information from the WHO IARC, including Monograph `volume`, `volume_publication_year`,
#'    `evaluation_year`, and `additional_information` where the chemical was described.
#' @seealso \url{https://monographs.iarc.who.int/list-of-classifications/}
#' @export
#' @examples
#' {
#'   dat <- extr_monograph(search_type = "casrn", ids = c("105-74-8", "120-58-1"))
#'   str(dat)
#'
#'   # Example usage for name search
#'   dat2 <- extr_monograph(search_type = "name", ids = c("Aloe", "Schistosoma", "Styrene"))
#'   str(dat2)
#' }
extr_monograph <- function(ids, search_type = "casrn", verbose = TRUE) {
  if (missing(ids)) {
    cli::cli_abort("The argument {.field ids} is required.")
  }

  if (!search_type %in% c("casrn", "name")) {
    cli::cli_abort("The argument {.field search_type} needs to be either `casrn` or `name`.")
  }


  if (isTRUE(verbose)) {
    cli::cli_alert_info("Extracting WHO IARC monographs...\nLast updated: 2024-11-29 5:08pm (CET)")
  }

  col_names <-  c(names(who_iarc_monographs), "query")

  out <- search_and_match(dat = who_iarc_monographs,
                   ids = ids,
                   search_type = search_type,
                   col_names = col_names,
                   chemical_col = "name"
                    )

  ids_not_found <- out$query[is.na(out$chemical)]
  if (all(isTRUE(verbose), length(ids_not_found) != 0)) {
    cli::cli_warn("Chemical{?s} {.field {ids_not_found}} not found!")
  }


  if (all(isTRUE(verbose), all(nrow(out) == 0))) {
    cli::cli_alert_info("No info found.")
    return(NULL)
  }
  out

}
