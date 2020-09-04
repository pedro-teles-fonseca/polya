
context("Cumulative BF - multinomial")

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bf_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9)),
    bf_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9)),
  )
})

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bf_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9)),
    fbf_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9), frac = 0)
  )
})

test_that("1 sample test 1", {
  expect_equal(
    cumulative_bf_multinomial(samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9)),
    fbf_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9), m = 0, robust = "yes")
  )
})

test_that("2 samples", {
  expect_equal(
    cumulative_bf_multinomial(
      samples = list(austria_bl1, finland_bl1),
      categories = 1:9,
      null_par = theta_benford(1),
      prior = "dirichlet",
      hyper_par = rep(1, 9)),
    c(
      bf_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), prior = "dirichlet", hyper_par = rep(1, 9)),
      bf_multinomial(x = finland_bl1, categories = 1:9, null_par = theta_benford(1), prior = "dirichlet", hyper_par = rep(1, 9))
      )
  )
})

context("Cumulative FBF - multinomial")

test_that("1 samples", {
  expect_equal(
    cumulative_fbf_multinomial(
      samples = list(austria_bl1), categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9)),
    fbf_multinomial(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), hyper_par = rep(1, 9))
  )
})

test_that("2 samples", {
  expect_equal(
    cumulative_fbf_multinomial(samples = list(austria_bl1, finland_bl1), 1:9, theta_benford(1), rep(1, 9)),
    c(fbf_multinomial(austria_bl1, 1:9, theta_benford(1), 1), fbf_multinomial(finland_bl1, 1:9, theta_benford(1), rep(1, 9)))
  )
})











