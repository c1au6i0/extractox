#' Extract Data from EPA PPRTVs
#'
#' Extracts data for specified identifiers (CASRN or chemical names) from the EPA's
#' Provisional Peer-Reviewed Toxicity Values (PPRTVs) database. The function retrieves
#' and processes data, with options to use cached files or force a fresh download.
#'
#' @param ids Character vector of identifiers to search (e.g., CASRN or chemical names).
#' @param search_type Character string specifying the type of identifier: "casrn" or "name".
#'   Default is "casrn". If `search_type` is "name", the function performs a partial match search for the chemical name.
#' @param verbose Logical indicating whether to display progress messages. Default is TRUE.
#' @param force Logical indicating whether to force a fresh download of the database. Default is TRUE.
#' @return A data frame with extracted information matching the specified identifiers,
#'   or NULL if no matches are found.
#' @export
#' @examples
#' \donttest{
#' with_extr_sandbox({ # this is to write on tempdir as for CRAN policies
#'   # Extract data for a specific CASRN
#'   extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = TRUE)
#'
#'   # Extract data for a chemical name
#'   extr_pprtv(ids = "Acrolein", search_type = "name", verbose = TRUE, force = FALSE)
#'
#'   # Extract data for multiple identifiers
#'   extr_pprtv(
#'     ids = c("107-02-8", "79-10-7", "42576-02-3"),
#'     search_type = "casrn",
#'     verbose = TRUE,
#'     force = FALSE
#'   )
#' })
#' }
extr_pprtv <- function(ids, search_type = "casrn", verbose = TRUE, force = TRUE) {
  if (missing(ids)) {
    cli::cli_abort("The argument {.field ids} is required.")
  }

  if (!search_type %in% c("casrn", "name")) {
    cli::cli_abort("The argument {.field search_type} needs to be either `casrn` or `name`.")
  }

  file_name <- "epa_pprtvs" # Filename for caching

  full_path_cache_file <- fs::path(tools::R_user_dir("extractox", which = "cache"), file_name)
  cache_present <- fs::file_exists(full_path_cache_file)

  if (isTRUE(force) | !cache_present) {
    dat <- download_db(
      url = "https://cfpub.epa.gov/ncea/pprtv/atoz.cfm",
      url_query_param = list(excel = "yes"),
      file_name = file_name,
      verbose = verbose
    )

    path_cache <- save_to_cache(dat, file_name, verbose = verbose)
  } else {
    dat <- read_from_cache(file_name = file_name, verbose = verbose)

    if (isTRUE(verbose)) {
      date_download <- dat$date_downloaded[1]
      cli::cli_alert_info("Cache date {.strong {date_download}}.")
      cli::cli_alert_info("Set `force = TRUE` to force download from EPA.")
    }
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Extracting EPA PPRTVs.")
  }

  if (search_type == "casrn") {
    out <- dat[dat$casrn %in% ids, ]
  } else if (search_type == "name") {
    tot_pattern <- paste(ids, collapse = "|")
    out <- dat[grepl(tot_pattern, dat$chemical), ]
  }

  if (nrow(out) == 0) {
    cli::cli_alert_info("No info found.")
    return(NULL)
  }

  out
}
