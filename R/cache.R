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
  # Sys.getenv("R_USER_CACHE_DIR")

  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file_path <- fs::path(cache_dir, file_name)

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
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  file_path <- fs::path(cache_dir, file_name)

  if (file.exists(file_path)) {
    out <- readRDS(file_path)
    if (verbose) {
      cli::cli_alert_success("Successfully load. {.file {file_name}} from cache.")
    }
  } else {
    cli::cli_abort("File not found in cache.")
  }
  out
}
