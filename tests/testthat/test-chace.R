library(testthat)
library(fs)

# Check setup.R



# message(tools::R_user_dir("extractox", which = "cache"))

song <- c("bella", "ciao", "bella", "ciao", "ciao", "ciao")

test_that("Save to cache works", {


    expect_message({
      file_path <- save_to_cache(dat = song, file_name = "song", verbose =  TRUE)
    }, "Saving")


    lapply()

   exp_path  <- fs::path(Sys.getenv("R_USER_CACHE_DIR"), "song")
   message("Expected ", exp_path, "\n")

   file_path <- normalizePath(file_path)
   message("Real ", file_path)


#   expect_true(is.list(out))
#   expect_true(all(unlist(lapply(out, is.data.frame))))
#   expect_equal(nrow(out$comptox_main_data), 1)
#   expect_equal(nrow(out$comptox_main_data), 1)
#   expect_equal(nrow(out$comptox_cover_sheet), 4)
})


# tools::R_user_dir("extractox", "cache")
