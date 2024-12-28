library(testthat)

Sys.sleep(3)

# Test 1: Check if the function works with a valid chemical name input
test_that("Valid chemical name input", {
  skip_on_cran()

  dat <- extr_comptox(
    ids = c("Aspirin"),
    download_items = c("DTXCID", "CASRN")
  )

})

Sys.sleep(3)


# Test 2: Check if the function works with a valid CASRN input
test_that("Valid CASRN input", {
  skip_on_cran()
  result <- extr_comptox(
    ids = c("50-00-0"),
    download_items = c("DTXCID", "CASRN")
  )
  expect_snapshot(result[!names(result) %in% "comptox_cover_sheet"])
})


Sys.sleep(3)

test_that("Valid inputs", {
  skip_on_cran()
  dat <- extr_comptox(
    ids = c("50-00-0", "Aspirin", "DTXSID5020023"),
    download_items = c("DTXCID", "CASRN")
  )
  expect_snapshot(result[!names(result) %in% "comptox_cover_sheet"])
})

Sys.sleep(3)

test_that("Missing required parameter", {
  skip_on_cran()
  expect_error(
    extr_comptox(),
    "The argument ids is required."
  )
})


test_that("extr_comptox when download_items is set to one val", {
  expect_no_error(
    result <- extr_comptox(c("31-12-5", "80-05-7"), download_items = "TOXVAL_DETAILS")
  )
})
