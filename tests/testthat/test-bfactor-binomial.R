
library(digit.analysis)

context("Bayes factors - Binomial model")

# ------------------------------------------------
# data from Austria
# ------------------------------------------------

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        MoreArgs = list(
          x = austria.bl1,
          a = 1,
          b = 1,
          transf = "log10"
        )
      ),
      2
    ),
    c(
      "log10(BF)" = 0.07,
      "log10(BF)" = 1.29,
      "log10(BF)" = 1.32,
      "log10(BF)" = -0.62,
      "log10(BF)" = -1.78,
      "log10(BF)" = 1.55,
      "log10(BF)" = -3.02,
      "log10(BF)" = 1.25,
      "log10(BF)" = 1.48)
  )
})

testthat::test_that("Austria PP, unif prior", {
  expect_equal(
    as.numeric(round(sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        MoreArgs = list(
          x = austria.bl1,
          a = 1,
          b = 1)
      ),
      FUN = bfactor_to_prob),
      3
    )),
    c(0.542, 0.951, 0.954, 0.194, 0.016, 0.973, 0.001, 0.947, 0.968)
  )
})

testthat::test_that("Austria log10(BF), Dir prior c=1", {
  expect_equal(
    as.numeric(round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = theta_benford(1),
        b = 1 - theta_benford(1),
        MoreArgs = list(
          x = austria.bl1,
          transf = "log10")
      ),
      2)),
    c(1.27,  3.20,  3.45,  1.81,  0.79,  4.04, -0.94,  3.69,  4.12)
  )
})

testthat::test_that("Austria PP, Dir prior c=1", {
  expect_equal(
    as.numeric(round(sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = theta_benford(1),
        b = 1 - theta_benford(1),
        MoreArgs = list(
          x = austria.bl1)
      ),
      FUN = bfactor_to_prob),
      3
    )),
    c(0.949, 0.999, 1.000, 0.985, 0.861, 1.000, 0.104, 1.000, 1.000)
  )
})

testthat::test_that("Austria log10(BF), Dir prior c=22", {
  expect_equal(
    as.numeric(round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = 22 * theta_benford(1),
        b = 22 - 22*theta_benford(1),
        MoreArgs = list(
          x = austria.bl1,
          transf = "log10")
      ),
      2)),
    c(22.88, 39.27, 45.90, 47.33, 48.46, 53.47, 50.17, 55.21, 56.11)
  )
})

testthat::test_that("Austria PP, Dir prior c=22", {
  expect_equal(as.numeric(round(
    sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = 22 * theta_benford(1),
        b = 22 - 22 * theta_benford(1),
        MoreArgs = list(x = austria.bl1)
      ),
      FUN = bfactor_to_prob
    ),
    3
  )),
    rep(1, times = 9))
})

# ------------------------------------------------
# data from Belgium
# ------------------------------------------------

testthat::test_that("Belgium log10(BF), unif prior", {
  expect_equal(
    as.numeric(round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        MoreArgs = list(
          x = belgium.bl1,
          a = 1,
          b = 1,
          transf = "log10"
        )
      ),
      2
    )),
    c(-2.59, -1.55, -2.10,  1.41,  1.19,  1.38,  1.62,  1.35,  1.06)
  )
})

testthat::test_that("Belgium PP, unif prior", {
  expect_equal(
    as.numeric(round(sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        MoreArgs = list(
          x = belgium.bl1,
          a = 1,
          b = 1)
      ),
      FUN = bfactor_to_prob),
      3
    )),
    c(0.003, 0.028, 0.008, 0.963, 0.940, 0.960, 0.977, 0.957, 0.920)
  )
})

testthat::test_that("Belgium log10(BF), Dir prior c=1", {
  expect_equal(
    as.numeric(round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = theta_benford(1),
        b = 1 - theta_benford(1),
        MoreArgs = list(
          x = belgium.bl1,
          transf = "log10")
      ),
      2)),
    c(-1.43,  0.42,  0.18,  3.68,  3.52,  3.77,  4.13,  3.96,  3.48)
  )
})

testthat::test_that("Belgium PP, Dir prior c=1", {
  expect_equal(
    as.numeric(round(sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = theta_benford(1),
        b = 1 - theta_benford(1),
        MoreArgs = list(
          x = belgium.bl1)
      ),
      FUN = bfactor_to_prob),
      3
    )),
    c( 0.036, 0.723, 0.601, 1.000, 1.000, 1.000, 1.000, 1.000, 1.000)
  )
})

testthat::test_that("Belgium log10(BF), Dir prior c=22", {
  expect_equal(
    as.numeric(round(
      mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = 22 * theta_benford(1),
        b = 22 - 22*theta_benford(1),
        MoreArgs = list(
          x = belgium.bl1,
          transf = "log10")
      ),
      2)),
    c(20.35, 36.08, 42.02, 49.36, 51.42, 53.14, 54.49, 55.08, 55.49)
  )
})

testthat::test_that("Belgium PP, Dir prior c=22", {
  expect_equal(
    as.numeric(round(sapply(
      X = mapply(
        bfactor_binomial,
        null.par = theta_benford(1),
        sucess = 1:9,
        a = 22 * theta_benford(1),
        b = 22 - 22*theta_benford(1),
        MoreArgs = list(
          x = belgium.bl1)
      ),
      FUN = bfactor_to_prob),
      3
    )),
    rep(1, times = 9)  )
})
