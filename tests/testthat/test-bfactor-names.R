
context("Bayes factors - bfactor_* names")

testthat::test_that("default transf name", {
  expect_equal(
    round(
      bfactor_multinomial(x = austria_bl1, categories = c(1:9),  null.par = theta_benford(1)),
    3),
    c("BF" = 0.001))
})

testthat::test_that("level transf name", {
  expect_equal(
    round(
      bfactor_multinomial(x = austria_bl1, categories = c(1:9), null.par = theta_benford(1),  transf = "level"),
      3),
    c("BF" = 0.001))
})

testthat::test_that("log10 transfname", {
  expect_equal(
    round(
      bfactor_multinomial(x = austria_bl1, categories = c(1:9), null.par = theta_benford(1), transf = "log10"),
      3),
    c("log10(BF)" = -3.098))
})

testthat::test_that("log transf name", {
  expect_equal(
    round(
      bfactor_multinomial(x = austria_bl1, categories = c(1:9), null.par = theta_benford(1), transf = "log"),
    3),
    c("log(BF)" = -7.133))
})

testthat::test_that("BF strength of evidence", {
  expect_equal(
      bfactor_interpret(bfactor_multinomial(x = austria_bl1, categories = c(1:9), null.par = theta_benford(1),)),
    c("Evidence" = "Negative")
    )
  })






