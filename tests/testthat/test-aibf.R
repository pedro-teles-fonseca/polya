
context("aibf")

test_that("aibf helpers", {
  expect_equal(
    aibf_multinomial_mts_2(x = austria_bl1, categories = 1:9, null_par = theta_benford(1)),
    aibf_multinomial_mts(x = austria_bl1, categories = 1:9, null_par = theta_benford(1))
  )
})

test_that("aibf multinomial haldane w/ mts", {
  expect_equal(
    ibf(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), prior = "haldane", in_favour = "h1", method = "mts"),
    aibf_multinomial_mts(x = austria_bl1, categories = 1:9, null_par = theta_benford(1))
  )
})

test_that("aibf and gibf with mts", {
  expect_equal(
    ibf(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), prior = "haldane", type = "geometric", in_favour = "h1", method = "mts"),
    ibf(x = austria_bl1, categories = 1:9, null_par = theta_benford(1), prior = "haldane", type = "arithmetic", in_favour = "h1", method = "mts")
  )
})













