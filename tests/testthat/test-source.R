#if (TRUE) skip("Some Important Requirement is not available")
if (file.exists("config.yml") == "FALSE") {
  testthat::skip("Skipping tests due to missing config.yml")
}


test_that("get_parameter works", {
  get_parameter(snr = "0001",
                ubb = FALSE,
                audience = "sus",
                stype = "gy",
                ganztag = FALSE)


  expect_s3_class(tmp.data, "data.frame")

  get_parameter(snr = "0001",
                ubb = TRUE,
                audience = "ubb",
                stype = "gy",
                ganztag = FALSE)

  expect_type(tmp.n, "character")
  expect_type(tmp.name, "character")

})



# test_that("plotGetData works", {
# This does not work yet
#   get_parameter(snr = "0001",
#                 ubb = FALSE,
#                 audience = "sus",
#                 stype = "gy",
#                 ganztag = FALSE)
#
#
#
#   plotGetData(data = tmp.data,
#               report = "rpt_sus_p1",
#               plotid = "A11",
#               audience = "sus")
#
#
#
# })


test_that("get_snr(list) works", {
  snr <- get_snr(expired = "yesterday")
  expect_s3_class(snr, "data.frame")

  snr <- get_snr(expired = "all")
  expect_s3_class(snr, "data.frame")

  snrlist <- get_snrlist(append = FALSE)
  expect_s3_class(snrlist, "data.frame")

})






