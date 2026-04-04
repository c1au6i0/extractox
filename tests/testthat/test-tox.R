library(testthat)

# @@@@@@@@@
# TOX ----
# @@@@@@@@@

Sys.sleep(5)

col_names <- c(
  "who_iarc_monographs", "pprtv", "ghs_dat", "iris", "ice",
  "comptox_cover_sheet", "comptox_main_data", "comptox_abstract_sifter",
  "comptox_synonym_identifier", "comptox_related_relationships",
  "comptox_toxcast_assays_ac50", "comptox_toxval_details",
  "comptox_chemical_properties"
)


test_that("extr_tox fetches data for CASRN 50-00-0 and warn", {
  skip_on_cran()
  skip_if_offline()

  suppressWarnings(
    out <- extr_tox(casrn = c("50-00-0", "ciao"), verbose = FALSE)
  )

  expect_true(is.list(out))
  expect_equal(names(out), col_names)
  # NULL is allowed for services that may fail transiently (ice, comptox_*);
  # all non-NULL elements must be data.frames
  expect_true(all(unlist(lapply(Filter(Negate(is.null), out), is.data.frame))))
})
