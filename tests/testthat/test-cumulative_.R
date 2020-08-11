
context("Cumulative BF - multinomial")

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bfactor_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
  )
})

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bfactor_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, b = 0)
  )
})

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bfactor_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1, m = 0, robust = "yes")
  )
})

test_that("2 samples", {
  expect_equal(
    cumulative_bfactor_multinomial(
      samples = list(austria_bl1, finland_bl1),
      categories = 1:9,
      null_par = theta_benford(1),
      prior = "dirichlet",
      hyper_par = 1),
    c(
      bfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), prior = "dirichlet", hyper_par = 1),
      bfactor_multinomial(x = finland_bl1, categories = 1:9, null_par = theta_benford(1), prior = "dirichlet", hyper_par = 1)
      )
  )
})

context("Cumulative FBF - multinomial")

test_that("1 samples", {
  expect_equal(
    cumulative_Fbfactor_multinomial(
      samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = 1),
    Fbfactor_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = 1)
  )
})

test_that("2 samples", {
  expect_equal(
    cumulative_Fbfactor_multinomial(samples = list(austria_bl1, finland_bl1), 1:9, theta_benford(1), 1),
    c(Fbfactor_multinomial(austria_bl1, 1:9, theta_benford(1), 1), Fbfactor_multinomial(finland_bl1, 1:9, theta_benford(1), 1))
  )
})











