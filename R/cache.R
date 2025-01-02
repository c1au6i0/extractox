#' Save an object to the package cache
#'
#' This function saves an R object to the cache directory of the `extractox` package
#' using the `.rds` file format. If a file with the same name already exists in the cache,
#' it will be overwritten.
#'
#' @param dat Any R object to be saved.
#' @param file_name A character string specifying the name of the file (without extension).
#' @param verbose A logical value indicating whether to print detailed messages. Default is FALSE.
#' @return Invisibly returns the full path of the saved file.
#' @details The cache directory is determined using [tools::R_user_dir()] with the `cache` subdirectory
#' for the `extractox` package. If the directory does not exist, it is created automatically.
#' The function will overwrite any existing file with the same name.
#' @keywords internal
#' @noRd
save_to_cache <- function(dat, file_name, verbose = FALSE) {
  # Sys.getenv("R_USER_CACHE_DIR")

  if (base::missing(dat)) {
    cli::cli_abort("The argument {.field {dat}} is required.")
  }

  if (base::missing(file_name)) {
    cli::cli_abort("The argument {.field {file_name}} is required.")
  }

  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file_path <- file.path(cache_dir, file_name)

  if (all(file.exists(file_path), verbose)) {
    cli::cli_alert_info("Overwriting cache.")
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Saving data in the cache {.path {file_path}}.")
    }
  }

  saveRDS(dat, file_path)

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
#' @keywords internal
#' @noRd
read_from_cache <- function(file_name, verbose = FALSE) {
  if (base::missing(file_name)) {
    cli::cli_abort("The argument {.field {file_name}} is required.")
  }

  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  file_path <- file.path(cache_dir, file_name)

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


#' Run Code in a Temporary Sandbox Environment
#'
#' This function creates a temporary directory and sets it as `R_USER_CACHE_DIR`
#' before executing the provided code block. It is used for testing or running
#' code without affecting the user's default cache directory as required by CRAN for the examples .
#' This function is not  designed to be used by package users. Shamelessly "inspired" by
#' some @luciorq code.
#' @param code The code to be executed inside the sandbox. Should be an expression.
#' @param temp_dir A temporary directory created using `temdir()`.
#' @return The result of the executed code.
#' @export
#' @examples
#' with_extr_sandbox(Sys.getenv("R_USER_CACHE_DIR"))
#' with_extr_sandbox(tools::R_user_dir("extractox", "cache"))
with_extr_sandbox <- function(code, temp_dir = tempdir()) {
  if (base::missing(code)) {
    cli::cli_abort("The argument {.field {file_name}} is required.")
  }

  withr::with_envvar(
    new = c("R_USER_CACHE_DIR" = temp_dir),
    code = {
      eval(substitute(code), envir = parent.frame())
    }
  )
}
