
context("Bayes factors - Bin vs Multinom match")

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_binomial(
          x = austria_bl1,
          success = 1,
          null_par = theta_benford(1)[1],
          hyper_par = c(1, 1)
          )
        ),
      2),
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1 == 1, "aa", "bb"),
          categories = c("aa", "bb"),
          null_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1]),
          hyper_par = 1
          )
        ),
      2)
    )
})

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_binomial(
          x = austria_bl1,
          success = 1,
          null_par = theta_benford(1)[1],
          hyper_par = c(1, 1)
          )
        ),
      2),
    0.07)
})

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1 == 1, "bb", "aa"),
          categories = c("bb", "aa"),
          null_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1]))
        ),
      2),
    0.07)
})


testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1==1, "bb", "aa"),
          categories = c("aa", "bb"),
          null_par = c(1-theta_benford(1)[1], theta_benford(1)[1]))),
      2),
    0.07)
})

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1==1, "bb", "aa"),
          categories = c("bb", "aa"),
          null_par = c(theta_benford(1)[1], 1-theta_benford(1)[1]),
          hyper_par = 1)),
      2),
    0.07)
})

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1==1, "aa", "bb"),
          categories = c("aa", "bb"),
          null_par = c(theta_benford(1)[1], 1-theta_benford(1)[1]))),
      2),
    0.07)
})

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        bfactor_multinomial(
          x = ifelse(austria_bl1==1, "bb", "aa"),
          categories = c("bb", "aa"),
          null_par = c(theta_benford(1)[1], 1-theta_benford(1)[1]))
        ),
      2),
    0.07)
})







