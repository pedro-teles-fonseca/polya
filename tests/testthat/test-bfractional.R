
context("Fractional Bayes factors - Binomial")

test_that("Correspondence FBF vs Full BF test 1", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(1, 1)),
    Fbfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(1, 1), frac = 0)
    )
})

test_that("Correspondence FBF vs Full BF test 3", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
    Fbfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1]), frac = 0)
  )
})

test_that("Correspondence FBF vs Full BF test 2", {
  expect_equal(
    bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
    Fbfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1]), frac = 0)
  )
})

context("Only one observed level")

test_that("Bin-Haldane - sucess level not observed test 1", {
  expect_warning(
    Fbfactor_binomial(x = c(0,0,0), success = 1, null_par = .2)
  )
})

test_that("Bin-Haldane - sucess level not observed test 2", {
  expect_warning(
    Fbfactor_binomial(x = c(1,1,1), success = 0, null_par = .2)
  )
})

test_that("Bin-Haldane - sucess level not observed test 2", {
  expect_equal(
    round(Fbfactor_binomial(x = c(1,1,1), success = 1, null_par = .2), 2),
    0.27
  )
})

context("Fractional Bayes factors - Multinomial")

test_that("Correspondence consistent FBF vs FBF vs BF", {
  expect_equal(
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, b = 0)
  )
})

test_that("Correspondence consistent FBF vs FBF vs BF", {
  expect_equal(
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 0, robust = "minimal")
  )
})

test_that("Correspondence consistent FBF vs FBF vs BF", {
  expect_equal(
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 0, robust = "yes")
  )
})

test_that("Correspondence consistent FBF vs FBF vs BF", {
  expect_equal(
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = theta_benford(1)),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = theta_benford(1), m = 0, robust = "yes")
  )
})

test_that("correspondence consistent FBF vs FBF w/MTS", {
  expect_equal(
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, robust = "minimal"),
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 9, robust = "minimal")

  )
})

test_that("correspondence consistent FBF vs FBF w/MTS", {
  expect_equal(
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, b = 9/length(austria_bl1)),
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 9, robust = "minimal")

  )
})

test_that("correspondence consistent FBF vs FBF w/MTS", {
  expect_equal(
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, b = sqrt(9/length(austria_bl1))),
    Fbfactor_multinomial(austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 9, robust = "yes")

  )
})

test_that("", {
  expect_equal(
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, b = 0),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 0, robust = "yes")
  )
})

test_that("", {
  expect_equal(
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 0)
  )
})














