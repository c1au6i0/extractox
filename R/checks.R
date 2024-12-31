#' Check for NA values in a specific column of a dataframe
#'
#' Checks for NA values in a specified column of a dataframe and optionally warns if any are found.
#'
#' @param dat A dataframe that contains the data.
#' @param col_to_check The name of the column to check for NA values.
#' @param verbose Logical indicating whether to show a warning if NAs are found. Default is TRUE.
#' @importFrom cli cli_warn
#' @keywords internal
#' @noRd
check_na_warn <- function(dat, col_to_check, verbose = TRUE) {
  ids_not_found <- dat$query[is.na(dat[[col_to_check]])]

  if (all(isTRUE(verbose), length(ids_not_found) != 0)) {
    cli::cli_warn("Chemical{?s} {.field {ids_not_found}} not found!")
  }

  invisible(ids_not_found)
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
