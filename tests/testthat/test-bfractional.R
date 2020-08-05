
context("Fractional Bayes factors - Binomial")

testthat::test_that("Correspondence FBF vs Full BF test 1", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(1, 1)),
    bfractional_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(1, 1), frac = 0)
    )
})

testthat::test_that("Correspondence FBF vs Full BF test 3", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
    bfractional_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1]), frac = 0)
  )
})

testthat::test_that("Correspondence FBF vs Full BF test 2", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
    bfractional_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1]), frac = 0)
  )
})

context("Fractional Bayes factors - Multinomial")

testthat::test_that("Correspondence FBF vs Full BF test 1", {
  expect_equal(
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    bfractional_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, frac = 0)
  )
})

context("Only one obserbed level")

testthat::test_that("Bin-Haldane - sucess level not observed test 1", {
  expect_warning(
    bfractional_binomial(x = c(0,0,0), success = 1, null_par = .2)
  )
})

testthat::test_that("Bin-Haldane - sucess level not observed test 2", {
  expect_warning(
    bfractional_binomial(x = c(1,1,1), success = 0, null_par = .2)
  )
})

testthat::test_that("Bin-Haldane - sucess level not observed test 2", {
  expect_equal(
    round(bfractional_binomial(x = c(1,1,1), success = 1, null_par = .2), 2),
    0.27
  )
})














