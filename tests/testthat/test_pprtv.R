library(testthat)

# extr_pprtv(c("112-27-6", "ciao"), search_type = "casrn", force = FALSE)

library(testthat)


temp_dir <- tempdir()

test_that("extr_pprtv casrn hit and not hit, verbose,  force = TRUE", {

  ids_to_search  = c("112-27-6", "98-86-2")

  expect_message({
  with_extr_sandbox(temp_dir = temp_dir,
  out <- extr_pprtv(ids = ids_to_search, force = TRUE, verbose = TRUE)
  )}, "Extracting EPA PPRTVs.")

  tmp_out <- fs::path(temp_dir, "R", "extractox")
  cache_exist <- fs::file_exists(fs::path(tmp_out, "epa_pprtvs.rds"))

  expect_true(cache_exist)
  expect_equal(nrow(out), length(ids_to_search))
  expect_true("query" %in% names(out))
  expect_equal(out$query, ids_to_search)
})

test_that("Function to warn with  verbose = TRUE", {

  ids_search = c("112-27-6", "bella", "ciao")
  expect_warning({
    with_extr_sandbox(temp_dir = temp_dir,
                      out <- extr_pprtv(ids = ids_search,
                                        force = FALSE, verbose = TRUE)
    )
  }, "Chemicals .* not found! ")

  expect_equal(out$query, ids_to_search)
  expect_equal(nrow(out), length(ids_to_search))
  expect_true(is.na(out$casrn[[2]]))
 })

test_that("Function verbose = FALSE", {

  ids_to_search  = c("112-27-6", "98-86-2")
  expect_silent({
    with_extr_sandbox(temp_dir = temp_dir,
                      out <- extr_pprtv(ids = ids_to_search,
                                        force = FALSE, verbose = FALSE)
    )
  })
})


test_that("extr_pprtv na,es hit and not hit, verbose,  force = TRUE", {

  ids_to_search  = c("Ace", "Acetophenone")

  expect_message({
    with_extr_sandbox(temp_dir = temp_dir,
                      out <- extr_pprtv(ids = ids_to_search,
                                        search_type = "name",
                                        force = TRUE,
                                        verbose = TRUE)
    )}, "Extracting EPA PPRTVs.")

  tmp_out <- fs::path(temp_dir, "R", "extractox")
  cache_exist <- fs::file_exists(fs::path(tmp_out, "epa_pprtvs.rds"))

  expect_true(cache_exist)
  expect_equal(nrow(out), length(ids_to_search))
  expect_true("query" %in% names(out))
  expect_equal(out$query, ids_to_search)
})
