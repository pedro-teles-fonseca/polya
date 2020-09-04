
context("Beta function")

test_that("beta vs mbeta", {
  expect_equal(
    beta(.1, 15),
    mbeta(c(.1, 15)))
})

test_that("mbeta with zero", {
  expect_equal(
    mbeta(c(0, 15)),
    Inf)
})

test_that("mbeta with zero 2", {
  expect_equal(
    mbeta(c(1, 1, 1, 1, 0, 15)),
    Inf)
})

test_that("mbeta with negative input", {
  expect_error(
    mbeta(c(1, 1, 1, -1)),
    "Invalid argument: 'x' must be a numeric vector with only non-negative elements."
  )
})

context("LogBeta function")

test_that("lbeta vs lmbeta", {
  expect_equal(
    lbeta(.1,15),
    lmbeta(c(.1,15))
  )
})

test_that("lmbeta with zero", {
  expect_equal(
    lmbeta(c(0, 15)),
    Inf)
})

test_that("lmbeta with zero 2", {
  expect_equal(
    lmbeta(c(0, 15, 1, 1, 1)),
    Inf)
})

test_that("lmbeta with negative input", {
  expect_error(
    lmbeta(c(1, 1, 1, 1, -1)),
    "Invalid argument: 'x' must be a numeric vector with only non-negative elements."
  )
})



