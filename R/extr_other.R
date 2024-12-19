# # #
# dat <- request("https://cfpub.epa.gov/ncea/pprtv/atoz.cfm") |>
#   req_url_query(
#     excel = "yes",
#   ) |>
#   req_perform()
#
# xlsx_file <- tempfile(fileext = ".xlsx")
#
# dat |>
#   httr2::resp_body_raw() |>
#   writeBin(xlsx_file)
#
# #
# # read_html(xlsx_file) |>
# #   View()
# #
# # library(rvest)
# xx <- xlsx_file |>
#   read_html() |>
#   html_nodes("table") |>
#   html_table(fill = TRUE)
