#' Search and Match Data
#'
#' This function searches for matches in a dataframe based on a given list of ids and search type,
#' then combines the results into a single dataframe, making sure that NA rows are added for any missing ids.
#' The column `query` is a the end of the dataframe.
#'
#' @param dat The dataframe to be searched.
#' @param ids A vector of ids to search for.
#' @param search_type The type of search: "casrn" or "name".
#' @param col_names Column names to be used when creating a new dataframe in case of no matches.
#' @param chemical_col The name of the column in dat where chemical names are stored.
#' @return A dataframe with search results.
#' @internal
#'
#' @details This function is used in `extr_pprtv` and `extr_monograph`.
#'
#' @seealso
#' \code{\link{extr_pprtv}}, \code{\link{extr_monograph}}
search_and_match <- function(dat, ids, search_type, col_names, chemical_col = "chemical") {
  results <- lapply(ids, function(id) {
    if (search_type == "casrn") {
      match <- dat[dat$casrn == id, ]
    } else if (search_type == "name") {
      match <- dat[grepl(id, dat[[chemical_col]]), ]
    }

    if (nrow(match) == 0) {
      match <- data.frame(matrix(NA, nrow = 1, ncol = length(col_names)))
      names(match) <- col_names
    }

    match$query <- id
    match
  })

  out <- do.call(rbind, results)

  # Add NA rows for missing ids
  out <- merge(data.frame(query = ids, stringsAsFactors = FALSE), out,
               by = "query", all.x = TRUE)
  out <- out[, col_names]

  return(out)
}



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
