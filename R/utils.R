#' Write Dataframes to Excel
#'
#' This function creates an Excel file with each dataframe in a list as a separate sheet.
#'
#' @param df_list A named list of dataframes to write to the Excel file.
#' @param filename The name of the Excel file to create.
#' @return No return value. The function prints a message indicating the completion of the Excel file writing.
#' @export
#' @examples
#' \donttest{
#' tox_dat <- extr_tox("50-00-0")
#' temp_file <- tempfile(fileext = ".xlsx")
#' write_dataframes_to_excel(tox_dat, filename = temp_file)
#' }
write_dataframes_to_excel <- function(df_list, filename) {
  if (isFALSE(requireNamespace("openxlsx", quietly = TRUE))) {
    cli::cli_abort(message = "{.pkg  openxlsx} not installed. Install it with: `install.packages('openxlsx')`")
  }

  wb <- openxlsx::createWorkbook()

  for (name in names(df_list)) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, sheet = name, df_list[[name]])
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  cli::cli_alert_info("Excell file written in {filename}...")
}

#' Verify SSL
#'
#' @param verify_ssl Boolean.
#' @param ... Any other arguments to be supplied to `req_option`
#' @keywords internal
#' @noRd
#' @return Named list.
set_ssl <- function(verify_ssl, ...) {
  libcurl_opt <- list(...)
  if (!verify_ssl) {
    libcurl_opt[["ssl_verifyhost"]] <- 0
    libcurl_opt[["ssl_verifypeer"]] <- 0
  }
  libcurl_opt
}

#' Check the status code of an HTTP response
#'
#' This function checks the status code of an HTTP response and provides
#' appropriate messages based on the status.
#'
#' @param resp An HTTP response object from the httr2 package.
#' @param verbose A logical value indicating whether to print detailed messages. Default is TRUE.
#' @keywords internal
#' @noRd
#' @return This function does not return a value. It is used for its side effects.
check_status_code <- function(resp, verbose = TRUE) {
  status_code <- httr2::resp_status(resp)
  if (!status_code %in% c(200L, 202L)) {
    cli::cli_abort("Request failed with status code: {status_code}")
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Request succeeded with status code: {status_code}")
    }
  }
}


#' Check Internet
#'
#' Wrapper around `pingr::is_online` to print message
#' a better message.
#'
#' @param verbose Boolean to display messages.
#' @keywords internal
#' @noRd
check_internet <- function(verbose = TRUE) {
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Checking Internet Connection...")
  }

  if (isFALSE(pingr::is_online())) {
    cli::cli_abort("It seems that you are not connected to internet!")
    out <- FALSE
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Internet connection OK...")
    }
    out <- TRUE
  }
  invisible(out)
}


#' Save an object to the package cache
#'
#' This function saves an R object to the cache directory of the `extractox` package
#' using the `.rds` file format. If a file with the same name already exists in the cache,
#' it will be overwritten.
#'
#' @param obj Any R object to be saved.
#' @param file_name A character string specifying the name of the file (without extension).
#' @param verbose A logical value indicating whether to print detailed messages. Default is FALSE.
#' @return Invisibly returns the full path of the saved file.
#' @details The cache directory is determined using [tools::R_user_dir()] with the `cache` subdirectory
#' for the `extractox` package. If the directory does not exist, it is created automatically.
#' The function will overwrite any existing file with the same name.
#' @noRd
save_to_cache <- function(obj, file_name, verbose = FALSE) {
  cache_dir <- tools::R_user_dir("extractox", which = "cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file_path <- file.path(cache_dir, paste0(file_name, ".rds"))

  if (all(file.exists(file_path), verbose)) {
    cli::cli_alert_info("Overwriting cache.")
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Saving data in the cache.")
    }
  }

  saveRDS(obj, file_path)

  invisible(file_path)
}

#' Read an object from the package cache
#'
#' This function reads an R object from the cache directory of the `extractox` package
#' using the `.rds` file format. If the file does not exist, it stops.
#'
#' @param file_name A character string specifying the name of the file (without extension).
#' @param verbose A logical value indicating whether to print detailed messages. Default is FALSE.
#' @return The R object read from the cache, or NULL if the file does not exist.
#' @details The cache directory is determined using [tools::R_user_dir()] with the `cache` subdirectory
#' for the `extractox` package. If the file does not exist, a message is printed if verbose is TRUE.
#' @noRd
read_from_cache <- function(file_name, verbose = FALSE) {
  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  file_path <- file.path(cache_dir, paste0(file_name, ".rds"))

  if (file.exists(file_path)) {
    out <- readRDS(file_path)
    if (verbose) {
      cli::cli_alert_success("Successfully load. {.file file_name} from cache.")
    }
  } else {
    cli::cli_abort("File not found in cache.")
  }
  out
}

#' Download and Process Data from a URL
#'
#' Downloads data from a specified URL, processes the response, and returns a cleaned
#' data frame. The function handles HTTP requests, saves temporary files, and extracts
#' table data from HTML content. Initially developed for EPA's PPRTVS data extraction
#' but designed to be generalizable for similar use cases.
#'
#' @param url Character string specifying the URL to download data from
#' @param url_query_param List of query parameters to be added to the URL
#' @param file_name Character string specifying the name for the downloaded file
#' @param file_ext Character string specifying the file extension. Default is "file".
#' @param verbose Logical indicating whether to display progress messages. Default is FALSE.
#' @return A data frame containing:
#'   * The processed table data from the HTML content
#'   * Clean column names (via janitor::clean_names)
#'   * An additional column 'date_downloaded' with the response timestamp
#' @keywords internal
#' @noRd
download_db <- function(url,
                        url_query_param,
                        file_name,
                        file_ext = "file",
                        verbose = TRUE) {
  check_internet(verbose = verbose)

  # Perform the request and get a response
  if (isTRUE(verbose)) {
    cli::cli_alert_info("Downloading data from {.url {url}}.")
  }

  req <- httr2::request(url) |>
    httr2::req_url_query(
      !!!url_query_param,
      multi = "explore"
    ) |>
    httr2::req_perform()

  dat_file <- tempfile(fileext = file_ext)

  req |>
    httr2::resp_body_raw() |>
    writeBin(dat_file)

  out <- dat_file |>
    rvest::read_html() |>
    rvest::html_nodes("table") |>
    rvest::html_table(fill = TRUE)

  out_cl <- out[[1]] |>
    janitor::clean_names()

  out_cl[, "date_downloaded"] <- httr2::resp_date(req)

  out_cl
}



#' Selection of assays of iris

#' @keywords internal
#' @noRd
ice_assays <- function() {
  ice_carc_woe <-
    c(
      "OPP Carcinogenicity",
      "Report on Carcinogens",
      "IRIS Carcinogenicity",
      "IARC Carcinogenicity",
      "IRIS Carcinogenicity"
    )


  ice_invivo_acute_tox <-
    c(
      "Rat Acute Oral Toxicity",
      "Rat Acute Inhalation Toxicity",
      "Rat Acute Dermal Toxicity"
    )


  ice_invivo_sensitization <- c("Human Maximization Test", "Human Repeat Insult Patch Test", "LLNA", "Guinea Pig Maximization/Buehler")

  ice_invivo_irritation <- c(
    "Draize Skin Irritation/Corrosion Test",
    "Draize Eye Irritation/Corrosion Test",
    "Four-hour Human Patch Test"
  )


  ice_invivo_endocrine <- c(
    "Hershberger-Agonist",
    "Hershberger-Antagonist",
    "Uterotrophic-Agonist"
  )

  ice_cancer <- c(
    "In Vitro Genotoxicity",
    "In Vivo Genotoxicity",
    "NTP Carcinogenicity",
    "Two year cancer bioassay"
  )

  ice_dart <- c(
    "Urinalysis", "Microscopic Pathology",
    "Gross Pathology", "Organ Weight", "In Life Observation", "Hematology",
    "Clinical Chemistry", "Cholinesterase", "in vivo", "Offspring Survival Late",
    "Developmental Malformation", "Developmental Landmark", "Reproductive Performance",
    "Offspring Survival Early"
  )

  list(
    ice_carc_woe = ice_carc_woe,
    ice_invivo_acute_tox = ice_invivo_acute_tox,
    ice_invivo_sensitization = ice_invivo_sensitization,
    ice_invivo_irritation = ice_invivo_irritation,
    ice_invivo_endocrine = ice_invivo_endocrine,
    ice_cancer = ice_cancer,
    ice_dart = ice_dart
  )
}


#' Run Code in a Temporary Sandbox Environment
#'
#' This function creates a temporary directory and sets it as `R_USER_CACHE_DIR`
#' before executing the provided code block. It is used for testing or running
#' code without affecting the user's default cache directory as required by CRAN for the examples .
#' This function is not  designed to be used by package users. Shamelessly "inspired" by
#' some @luciorq code.
#' @param code The code to be executed inside the sandbox. Should be an expression.
#' @return The result of the executed code.
#' @export
#' @examples
#' with_extr_sandbox(Sys.getenv("R_USER_CACHE_DIR"))
#' with_extr_sandbox(tools::R_user_dir("extractox", "cache"))
with_extr_sandbox <- function(code) {
  withr::with_envvar(
    new = c("R_USER_CACHE_DIR" = tempdir()),
    code = {
      eval(substitute(code), envir = parent.frame())
    }
  )
}
