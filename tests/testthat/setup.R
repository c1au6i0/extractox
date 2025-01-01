withr::local_envvar(
  .new = list(
    `R_USER_CACHE_DIR` = withr::local_tempdir(pattern = "tmp-cache")
  ),
  .local_envir = testthat::teardown_env()
)
