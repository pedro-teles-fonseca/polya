
context("Bayes factors - Multinomial model")

# ------------------------------------------------
# BL1 - Bayes factors and posterior probabilities
# ------------------------------------------------

testthat::test_that("log10(BF), uniform prior", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = 1,
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = -3.10,
      "log10(BF)" = -1.78,
      "log10(BF)" =  1.06,
      "log10(BF)" = -1.40,
      "log10(BF)" = 1.21,
      "log10(BF)" = 0.89,
      "log10(BF)" = -2.37,
      "log10(BF)" = 1.23,
      "log10(BF)" = -8.88,
      "log10(BF)" = 3.81,
      "log10(BF)" = -8.38,
      "log10(BF)" = 0.62
    )
  )
})

testthat::test_that("PPs, uniform prior", {
  expect_equal(
    round(as.numeric(
      mapply(
        FUN = bfactor_to_prob,
        bf = sapply(
          X = datalist_bl1,
          FUN = bfactor_multinomial,
          categories = 1:9,
          null.par = theta_benford(1),
          alpha = 1
        )
      )
    ), 3),
    c(
      0.001,
      0.016,
      0.919,
      0.039,
      0.942,
      0.885,
      0.004,
      0.945,
      0.000,
      1.000,
      0.000,
      0.808
    )
  )
})

testthat::test_that("log10(BF), centred Dir prior c=1", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = theta_benford(1),
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = 1.93,
      "log10(BF)" = 3.35,
      "log10(BF)" =  6.47,
      "log10(BF)" =  4.02,
      "log10(BF)" =  6.14,
      "log10(BF)" =  5.87,
      "log10(BF)" =  2.48,
      "log10(BF)" =  6.12,
      "log10(BF)" = -4.33,
      "log10(BF)" =  8.98,
      "log10(BF)" = -3.61,
      "log10(BF)" =  5.19
    )
  )
})

testthat::test_that("PPs, centred Dir prior c=1", {
  expect_equal(
    round(as.numeric(
      mapply(
        FUN = bfactor_to_prob,
        bf = sapply(
          X = datalist_bl1,
          FUN = bfactor_multinomial,
          categories = 1:9,
          null.par = theta_benford(1),
          alpha = theta_benford(1)
        )
      )
    ), 3),
    c(
      0.988,
      1.000,
      1.000,
      1.000,
      1.000,
      1.000,
      0.997,
      1.000,
      0.000,
      1.000,
      0.000,
      1.000
    )
  )
})

testthat::test_that("log10(BF), centred Dir prior c=22", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl1,
        FUN = bfactor_multinomial,
        categories = 1:9,
        null.par = theta_benford(1),
        alpha = 22 * theta_benford(1),
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = -4.92,
      "log10(BF)" = -3.59,
      "log10(BF)" =  -0.60,
      "log10(BF)" =  -2.93,
      "log10(BF)" =  -0.89,
      "log10(BF)" =  -1.14,
      "log10(BF)" =  -4.43,
      "log10(BF)" =  -0.91,
      "log10(BF)" = -10.95,
      "log10(BF)" =   1.86,
      "log10(BF)" = -10.29,
      "log10(BF)" =  -1.75
    )
  )
})

testthat::test_that("PPs, centred Dir prior c=22", {
  expect_equal(
    round(as.numeric(
      mapply(
        FUN = bfactor_to_prob,
        bf = sapply(
          X = datalist_bl1,
          FUN = bfactor_multinomial,
          categories = 1:9,
          null.par = theta_benford(1),
          alpha = 22 * theta_benford(1)
        )
      )
    ), 3),
    c(
      0.000,
      0.000,
      0.200,
      0.001,
      0.113,
      0.067,
      0.000,
      0.110,
      0.000,
      0.986,
      0.000,
      0.018
    )
  )

})

# ------------------------------------------------
# BL2 - Bayes factors and posterior probabilities
# ------------------------------------------------

testthat::test_that("log10(BF), uniform prior", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = 1,
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = 5.05,
      "log10(BF)" = 5.90,
      "log10(BF)" = 4.79,
      "log10(BF)" = 3.51,
      "log10(BF)" = 5.78,
      "log10(BF)" = 4.19,
      "log10(BF)" = 6.34,
      "log10(BF)" = 5.12,
      "log10(BF)" = 3.39,
      "log10(BF)" = 4.89,
      "log10(BF)" = 5.76,
      "log10(BF)" = 7.13
    )
  )
})

testthat::test_that("PPs, uniform prior", {
  expect_equal(round(as.numeric(
    mapply(
      FUN = bfactor_to_prob,
      bf = sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = 1
      )
    )
  ), 3),
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})

testthat::test_that("log10(BF), centred Dir prior c=1", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = theta_benford(2),
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = 11.27,
      "log10(BF)" = 12.20,
      "log10(BF)" = 11.11,
      "log10(BF)" =  9.74,
      "log10(BF)" = 12.03,
      "log10(BF)" = 10.44,
      "log10(BF)" = 12.64,
      "log10(BF)" = 11.39,
      "log10(BF)" =  9.61,
      "log10(BF)" = 11.13,
      "log10(BF)" = 11.99,
      "log10(BF)" = 13.39
    )
  )
})

testthat::test_that("PPs, centred Dir prior c=1", {
  expect_equal(round(as.numeric(
    mapply(
      FUN = bfactor_to_prob,
      bf = sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = theta_benford(2)
      )
    )
  ), 3),
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})

testthat::test_that("log10(BF), centred Dir prior c=12", {
  expect_equal(
    round(
      sapply(
        X = datalist_bl2,
        FUN = bfactor_multinomial,
        categories = 0:9,
        null.par = theta_benford(2),
        alpha = 12 * theta_benford(2),
        transf = "log10"
      ),
      2
    ),
    c(
      "log10(BF)" = 4.60,
      "log10(BF)" =  5.52,
      "log10(BF)" = 4.45,
      "log10(BF)" = 3.10,
      "log10(BF)" = 5.35,
      "log10(BF)" = 3.79,
      "log10(BF)" = 5.95,
      "log10(BF)" = 4.72,
      "log10(BF)" = 2.98,
      "log10(BF)" = 4.47,
      "log10(BF)" = 5.31,
      "log10(BF)" = 6.69
    )
  )
})

testthat::test_that("PPs, centred Dir prior c=12", {
  expect_equal(
    round(as.numeric(
      mapply(
        FUN = bfactor_to_prob,
        bf = sapply(
          X = datalist_bl2,
          FUN = bfactor_multinomial,
          categories = 0:9,
          null.par = theta_benford(2),
          alpha = 12 * theta_benford(2)
        )
      )
    ), 3),
    c(
      1.000,
      1.000,
      1.000,
      0.999,
      1.000,
      1.000,
      1.000,
      1.000,
      0.999,
      1.000,
      1.000,
      1.000
    )
  )

})

