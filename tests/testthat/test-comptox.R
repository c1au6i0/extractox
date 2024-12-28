library(testthat)

Sys.sleep(3)

# Test 1: Check if the function works with a valid chemical name input
test_that("Valid chemical name input", {
  skip_on_cran()

  expect_silent({
  dat <- extr_comptox(
    ids = c("Aspirin"),
    download_items = c("DTXCID", "CASRN"),
    verbose = FALSE)
  })
  expect_true(is.list(dat))
  expect_true(all(unlist(lapply(dat, is.data.frame))))
  expect_equal(nrow(dat$comptox_main_data), 1)
  expect_equal(nrow(dat$comptox_main_data), 1)
  expect_equal(nrow(dat$comptox_cover_sheet), 4)

})

Sys.sleep(3)

col_names <- c("comptox_cover_sheet", "comptox_main_data", "comptox_abstract_sifter",
  "comptox_synonym_identifier", "comptox_related_relationships",
  "comptox_toxcast_assays_ac50", "comptox_toxval_details", "comptox_chemical_properties"
)

test_that("Valid inputs", {
  skip_on_cran()

  ids <- c("50-00-0", "Aspirin", "DTXSID5020023")
  expect_message({
  dat <- extr_comptox(
    ids = ids)
  }, "Getting info from CompTox")

  expect_equal(names(dat), col_names)
  expect_equal(dat$comptox_main_data$input, ids)
  expect_equal(ncol(dat$comptox_main_data), 64)

})

Sys.sleep(3)

test_that("extr_comptox when download_items is set to one val", {
  expect_no_error(
    dat <- extr_comptox(c("50-00-0", "80-05-7"))
  )
})


test_that("extr_comptox warn for unknown ids", {
  expect_warning({
    dat <- extr_comptox(c("31-12-5", "bella", "ciao"))
  }, "Chemicals.*bella.*ciao.*not found!")
})
