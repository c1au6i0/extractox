library(testthat)


df_names <- create_na_df("ciao")

compounds <- c("Formaldehyde", "Aflatoxin B1", "bella", "ciao")

Sys.sleep(4)

#####################
# extr_chem_info ----
#####################

test_that("extr_chem_info fetches chem data", {

  skip_on_cran()
  expect_warning({

    dat <- extr_chem_info(compounds)

  }, "CID not retrieved")

  expect_true(is.data.frame(dat))
  expect_equal(nrow(dat), length(compounds))
  expect_equal(names(dat), names(df_names))
  expect_equal(dat$query, compounds)

})

Sys.sleep(4)

test_that("extr_chem_info fetches chem data", {

  skip_on_cran()
  expect_silent({

    dat <- extr_chem_info(compounds, verbose = FALSE)

  })

})

#################
# extr_fema  ----
#################

col_out <- c(
  "cid",
  "casrn",
  "IUPAC_name",
  "result",
  "source_name",
  "source_id",
  "other",
  "query"
)

Sys.sleep(4)

test_that("extr_pubchem_fema works correctly", {
  skip_on_cran()
  casrn_list <- c("1490-04-6", "50-00-0", "bella_ciao")

  expect_silent({
    dat <- extr_pubchem_fema(casrn_list, verbose = FALSE)
  })

  expect_equal(nrow(dat), length(casrn_list))
  expect_equal(names(dat), col_out)
  expect_equal(dat$query, casrn_list)
  expect_equal(dat$casrn , c("1490-04-6", "50-00-0", NA))
})

Sys.sleep(4)

test_that("extr_pubchem_fema produce CASRN warning", {
  skip_on_cran()
  expect_warning({
      dat <- extr_pubchem_fema("bella_ciao", verbose = TRUE)
    }, "CASRN .*not found")
})

Sys.sleep(4)

test_that("extr_pubchem_fema produce FEMA warning", {
  skip_on_cran()
  expect_warning({
    dat <- extr_pubchem_fema("50-00-0", verbose = TRUE)
  }, "FEMA .*not found")
})

Sys.sleep(4)

test_that("extr_pubchem_ghs works correctly", {
  skip_on_cran()
  casrn_list <- c("1490-04-6", "50-00-0", "bella_ciao")

  expect_silent({
    dat <- extr_pubchem_ghs(casrn_list, verbose = FALSE)
  })

  expect_equal(unique(dat$query), casrn_list)
  expect_equal(names(dat), col_out)
  expect_equal(unique(dat$casrn) , c("1490-04-6", "50-00-0", NA))
})

Sys.sleep(4)

test_that("extr_pubchem_ghs produce warning", {
  skip_on_cran()
  expect_warning({
    dat <- extr_pubchem_ghs("bella_ciao", verbose = TRUE)
  }, "not found")
})










