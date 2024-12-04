
if (file.exists("config.yml") == "FALSE") {
  testthat::skip("Skipping tests due to NO_TESTS environment variable")
}


test_that("get_sname() works", {
  sname <- get_sname(snr = "0001")
  expect_length(sname, 1)
  expect_type(sname, "character")
})


test_that("get_rmd() works", {
  rmd_code <- get_rmd(x_seq = 1:3)
  rmd_split <- strsplit(rmd_code, "(?=```\\{r)", perl = TRUE)
  expect_length(rmd_split[[1]], 18)
})


test_that("generate_rmd() works", {
  rmdtxt <- generate_rmd(x_seq = 1:3, ubb = FALSE, export = FALSE)
  rmd_split <- strsplit(rmdtxt, "(?=```\\{r)", perl = TRUE)
  expect_length(rmd_split[[1]], 21)

  rmdtxt <- generate_rmd(x_seq = 1:3, ubb = TRUE, export = FALSE)
  not_UBB <- grepl("Unterrichtsbeobachtung", rmdtxt)
  expect_true(not_UBB)
})


test_that("run_Parallel() works", {
  Sys.setenv(R_CONFIG_ACTIVE = "default")

  tmp.snr <- "0001"
  tmp.ubb <- FALSE
  #Report for: audience == sus, leh, elt, all, ubb, aus
  tmp.audience <- "sus"

  tmp.results <- "Lehrer"
  tmp.stype <- "gy"
  tmp.ganztag <- FALSE

  expect_no_error(run_Parallel(
    snr = tmp.snr,
    audience = tmp.audience,
    stype = tmp.stype,
    ubb = tmp.ubb,
    ganztag = tmp.ganztag,
    results = tmp.results
  ))

})






