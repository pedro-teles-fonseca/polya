
library(digit.analysis)

context("*_to_prob() and *_interpret()")

testthat::test_that("test bfactor_to_prob", {
  expect_equal(
    round(
      bfactor_to_prob(bfactor_multinomial(x = austria_bl1, categories = 1:9, null.par = theta_benford(1), transf = "log10")),
      3
    ),
    c("P(H0|X)" = 1.477)
    )
})

testthat::test_that("test bfactor_to_prob name", {
  expect_equal(
    round(
      bfactor_to_prob(bfactor_multinomial(x = austria_bl1, categories = 1:9, null.par = theta_benford(1), transf = "log10")),
      3
    ),
    c("P(H0|X)" = 1.477)
  )
})

testthat::test_that("test bfactor_interpret 1", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = 1
      ),
      2
    )
      , FUN = bfactor_interpret)),
    c(
      "Negative",
      "Negative",
      "Strong",
      "Negative",
      "Strong",
      "Substantial",
      "Negative",
      "Strong",
      "Negative",
      "Decisive",
      "Negative",
      "Substantial"
    )
  )
})

testthat::test_that("test bfactor_interpret 2", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = 1
      ),
      2
    )
      , FUN = bfactor_interpret)),
    c(
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive"
    )
  )
})

testthat::test_that("test bfactor_interpret 3", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = theta_benford(1)
      ),
      2
    )
      , FUN = bfactor_interpret)),
    c(
      "Strong",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Negative",
      "Decisive",
      "Negative",
      "Decisive"
    )
  )
})

testthat::test_that("test bfactor_interpret 4", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = theta_benford(2)
      ),
      2
    )
      , FUN = bfactor_interpret)),
    c(
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive"
    )
  )
})

testthat::test_that("test bfactor_log_interpret log", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = theta_benford(1),
        transf = "log"
      ),
      2
    )
      , FUN = bfactor_log_interpret)),
    c(
      "Strong",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Negative",
      "Decisive",
      "Negative",
      "Decisive"
    )
  )
})

testthat::test_that("test bfactor_log_interpret log 10", {
  expect_equal(
    unname(sapply(round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = theta_benford(2),
        transf = "log10"
      ),
      2
    )
      , FUN = bfactor_log_interpret,
      base = 10)),
    c(
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive",
      "Decisive"
    )
  )
})
