temp_dir <- tempdir()


condathis::with_sandbox_dir({ # this is to write on tempdir as for CRAN policies

  with_extr_sandbox(temp_dir = temp_dir,
   # Extract data for a specific CASRN
  dat  <- extr_pprtv(ids = "107-02-8", search_type = "casrn", verbose = TRUE, force = TRUE)
  )
})
