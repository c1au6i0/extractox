library(testthat)
set.seed(1)




input_terms <- c("50-00-0", "64-17-5", "methanal", "ethanol")

song <- "bella ciao bella ciao bella ciao ciao ciao"

# @@@@@@@@@@@@@
# extr_ctd ----
# @@@@@@@@@@@@@

Sys.sleep(3)

test_that("extr_ctd fetches valid expression data", {
  skip_on_cran()

  dat <- extr_ctd(
    input_terms = input_terms,
    report_type = "cgixns",
    category = "chem",
    action_types = "expression"
  )


  expect_true(is.data.frame(dat))
  expected_columns <- c(
    "x_input", "chemical_name", "chemical_id", "cas_rn",
    "gene_symbol", "gene_id", "organism", "organism_id",
    "interaction", "interaction_actions", "pubmed_ids"
  )

  expect_true(all(expected_columns %in% colnames(dat)))
  expect_gt(nrow(dat), 0)
})


Sys.sleep(3)

test_that("extr_ctd fetches other data", {
  skip_on_cran()
  dat <- extr_ctd(
      input_terms = input_terms,
      category = "chem",
      report_type = "genes_curated",
      input_term_search_type = "directAssociations",
      action_types = "ANY",
      ontology = c("go_bp", "go_cc")
    )
    expect_true(is.data.frame(dat))

    expected_columns <- c("x_input", "chemical_name", "chemical_id", "cas_rn", "gene_symbol",
      "gene_id", "organism", "organism_id", "pubmed_ids")

    expect_true(all(expected_columns %in% colnames(dat)))
    expect_gt(nrow(dat), 0)
})

Sys.sleep(3)

test_that("extr_ctd no results", {
  skip_on_cran()
  expect_warning({
  dat <- extr_ctd(
      input_terms = song,
      category = "chem",
      report_type = "genes_curated",
      input_term_search_type = "directAssociations",
      action_types = "ANY",
      ontology = c("go_bp", "go_cc")
  )},  "The chem .* was not found")

    expect_equal(nrow(dat), 1)
})

Sys.sleep(3)

test_that("extr_ctd no results with song (verbose = FALSE)", {
  skip_on_cran()

  # Expect no output and no error when verbose = FALSE
  expect_silent({
    dat <- extr_ctd(
      input_terms = song,
      category = "chem",
      report_type = "genes_curated",
      input_term_search_type = "directAssociations",
      action_types = "ANY",
      ontology = c("go_bp", "go_cc"),
      verbose = FALSE
    )
  })
})


# @@@@@@@@@@@@@@@@@@
# extr_tetramer ----
# @@@@@@@@@@@@@@@@@@

Sys.sleep(3)

test_that("extr_tetramer fetches tetramers data", {
  skip_on_cran()

  dat <- extr_tetramer(
      chem = c("50-00-0", "ethanol"),
      disease = "",
      gene = "",
      go = "",
      input_term_search_type = "directAssociations",
      qt_match_type = "equals"
    )
    expect_true(is.data.frame(dat))

    expected_columns <- c("query", "chemical", "chemical_id", "gene", "gene_id", "phenotype",
                          "phenotype_id", "disease", "disease_id")

    expect_true(all(expected_columns %in% colnames(dat)))
    expect_gt(nrow(dat), 0)
})

Sys.sleep(3)

test_that("extr_tetramer no results", {
  skip_on_cran()
  expect_warning({
    dat <- extr_tetramer(
      chem = song,
      disease = "",
      gene = "",
      go = "",
      input_term_search_type = "directAssociations",
      qt_match_type = "equals"
    )}, "The chem .* was not found")


    expect_true(is.data.frame(dat))

    expect_equal(nrow(dat), 1)
})

Sys.sleep(3)

test_that("extr_tetramer no results with song (verbose = FALSE)", {
  skip_on_cran()

  # Expect no output and no error when verbose = FALSE
  expect_silent({
    dat <- extr_tetramer(
      chem = song,
      disease = "",
      gene = "",
      go = "",
      input_term_search_type = "directAssociations",
      qt_match_type = "equals",
      verbose = FALSE
    )
  })
})

