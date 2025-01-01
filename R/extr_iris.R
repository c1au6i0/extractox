#' Extract Data from EPA IRIS Database
#'
#' The `extr_iris` function sends a request to the EPA IRIS database to search
#' for information based on a specified keywords and cancer types. It retrieves
#' and parses the HTML content from the response.
#'
#' @param casrn A vector CASRN for the search.
#' @param verbose A logical value indicating whether to print detailed messages.
#'    Default is TRUE.
#' @return A data frame containing the extracted data.
#' @seealso \href{https://cfpub.epa.gov/ncea/iris/search/}{EPA IRIS database}
#' @export
#' @examples
#' \donttest{
#' extr_iris(casrn = c("1332-21-4", "50-00-0"))
#' }
extr_iris <- function(casrn = NULL, verbose = TRUE) {
  cancer_types <- c("non_cancer", "cancer")

  if (base::missing(casrn)) {
    cli::cli_abort("The argument {.field {casrn}} is required.")
  }

  # Check if online
  check_internet(verbose = verbose)

  if (length(casrn) > 1) {
    dat <- lapply(casrn, extr_iris_, cancer_types = cancer_types, verbose = verbose)
    out <- do.call(rbind, dat)
  } else {
    out <- extr_iris_(casrn = casrn, cancer_types = cancer_types, verbose = verbose)
  }

  out_cl <- out |>
    janitor::clean_names()

  check_na_warn(out_cl, col_to_check = "chemical_name", verbose = verbose)

  out_cl
}

#' @inherit extr_iris title description params return seealso
#' @param verify_ssl Boolean to control of SSL should be verified or not.
#' @param ... Any other arguments to be supplied to `req_option` and thus to
#'    `libcurl`.
#' @noRd
#' @keywords internal
extr_iris_ <- function(casrn = NULL,
                       cancer_types = c("non_cancer", "cancer"),
                       verify_ssl = FALSE,
                       verbose = TRUE,
                       ...) {
  # Check if online
  base_url <- "https://cfpub.epa.gov/ncea/iris/search/basic/"

  # Construct query parameters
  query_params <- list(
    keyword = casrn,
    cancer_or_no_cancer = cancer_types
  )

  libcurl_opt <- set_ssl(verify_ssl = verify_ssl, other_opt = ...)

  error_result <- NULL

  if (isTRUE(verbose)) {
    cli::cli_alert_info("Quering {.field {casrn}} to EPA IRIS database...\n")
  }
  resp <- tryCatch(
    {
      httr2::request(base_url = base_url) |>
        httr2::req_retry(max_tries = 2, backoff = ~3) |>
        httr2::req_url_query(!!!query_params, .multi = "explode") |>
        httr2::req_options(!!!libcurl_opt) |>
        httr2::req_perform()
    },
    error = function(e) {
      error_result <<- e
      NULL
    }
  )

  msg <- "Failed to perform the request: {conditionMessage(error_result)}"

  if (!is.null(error_result)) {
    if (grepl("unsafe legacy renegotiation disabled", conditionMessage(error_result))) {
      msg <- c(msg, "", cli::style_italic("!If you are using openssl, you might
                                          need to downgrade to curl v7.78.0, openssl v1.1.1!"))
    }
    cli::cli_abort(msg)
  }

  check_status_code(resp, verbose = verbose)

  # Parse the HTML content
  content <- httr2::resp_body_html(resp)

  dat <- tryCatch(
    {
      rvest::html_element(content, "#searchMain , th, td") |>
        rvest::html_table()
    },
    error = function(e) {
      cli::cli_abort("Failed to parse the HTML content: {conditionMessage(e)}")
    }
  )

  out <- dat[dat$CASRN %in% casrn, ]

  if (nrow(out) > 0) {
    out[, "query"] <- casrn
  } else {
    out[1, "query"] <- casrn
  }

  out
}
