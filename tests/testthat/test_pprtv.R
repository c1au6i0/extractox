library(testthat)

temp_dir <- tempdir()

test_that("extr_pprtv casrn hit and not hit, verbose,  force = TRUE", {
  skip_on_cran()
  skip_if_offline()
  ids_search <- c("112-27-6", "98-86-2")

  expect_message(
    {
      with_extr_sandbox(
        temp_dir = temp_dir,
        out <- extr_pprtv(ids = ids_search, force = TRUE, verbose = TRUE)
      )
    },
    "Extracting EPA PPRTVs."
  )

  tmp_out <- file.path(temp_dir, "R", "extractox")
  cache_exist <- file.exists(file.path(tmp_out, "epa_pprtvs.rds"))

  expect_true(cache_exist)
  expect_equal(nrow(out), length(ids_search))
  expect_true("query" %in% names(out))
  expect_equal(out$query, ids_search)
})

test_that("Function to warn with  verbose = TRUE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("112-27-6", "bella", "ciao")
  expect_warning(
    {
      with_extr_sandbox(
        temp_dir = temp_dir,
        out <- extr_pprtv(
          ids = ids_search,
          force = FALSE, verbose = TRUE
        )
      )
    },
    "Chemicals .* not found!"
  )

  expect_equal(out$query, ids_search)
  expect_equal(nrow(out), length(ids_search))
  expect_true(is.na(out$casrn[[3]]))
})

test_that("Function verbose = FALSE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("112-27-6", "98-86-2")
  expect_silent({
    with_extr_sandbox(
      temp_dir = temp_dir,
      out <- extr_pprtv(
        ids = ids_search,
        force = FALSE, verbose = FALSE
      )
    )
  })
})

test_that("extr_pprtv na,es hit and not hit, verbose,  force = TRUE", {
  skip_on_cran()
  skip_if_offline()

  ids_search <- c("Ace", "Acetophenone")

  expect_message(
    {
      with_extr_sandbox(
        temp_dir = temp_dir,
        out <- extr_pprtv(
          ids = ids_search,
          search_type = "name",
          force = TRUE,
          verbose = TRUE
        )
      )
    },
    "Extracting EPA PPRTVs."
  )

  tmp_out <- file.path(temp_dir, "R", "extractox")
  cache_exist <- file.exists(file.path(tmp_out, "epa_pprtvs.rds"))

  expect_equal(nrow(out), 11)
})
