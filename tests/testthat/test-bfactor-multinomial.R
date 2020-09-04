
context("Mu-Dir Bayes factors - basic checks")

test_that("test1", {
  expect_equal(
    round(
      bf_multinomial(x = austria_bl1, categories = c(1:9), null_par = theta_benford(1)),
      3),
    0.001)
})

test_that("log10 BF", {
  expect_equal(
    round(
      log10(
        bf_multinomial(x = austria_bl1, categories = c(1:9), null_par = theta_benford(1))
        ),
      3),
    -3.098)
})

test_that("log BF", {
  expect_equal(
    round(
      log(
        bf_multinomial(x = austria_bl1, categories = c(1:9), null_par = theta_benford(1))
        ),
      3),
    -7.133)
})

test_that("BF strength of evidence", {
  expect_equal(
    pcal::bfactor_interpret(
      bf_multinomial(x = austria_bl1, categories = c(1:9), null_par = theta_benford(1))
      ),
    "Negative"
  )
})

context("Mu-Dir BL1 - Bayes factors and posterior probabilities")

test_that("log10(BF), uniform prior", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = rep(1, 9)
          )
        ),
      2),
    c(-3.10, -1.78, 1.06, -1.40, 1.21,  0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62)
  )
})

test_that("PPs, uniform prior", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
          bf = sapply(
            X = datalist_bl1,
            FUN = bf_multinomial,
            categories = 1:9,
            null_par = theta_benford(1),
            hyper_par = rep(1, 9))
          )
        ),
      3),
    c(0.001, 0.016, 0.919, 0.039, 0.942, 0.885, 0.004, 0.945, 0.000, 1.000, 0.000, 0.808)
  )
})

test_that("log10(BF), centred Dir prior c=1", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = theta_benford(1)
          )
        ),
      2),
    c(1.93, 3.35, 6.47, 4.02, 6.14, 5.87, 2.48, 6.12, -4.33, 8.98, -3.61, 5.19)
  )
})

test_that("PPs, centred Dir prior c=1", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
          bf = sapply(
            X = datalist_bl1,
            FUN = bf_multinomial,
            categories = 1:9,
            null_par = theta_benford(1),
            hyper_par = theta_benford(1)
            )
          )
        ),
      3),
    c(0.988, 1.000, 1.000, 1.000, 1.000, 1.000, 0.997, 1.000, 0.000, 1.000, 0.000, 1.000)
  )
})

test_that("log10(BF), centred Dir prior c=22", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = 22 * theta_benford(1)
          )
        ),
      2),
    c(-4.92, -3.59, -0.60, -2.93, -0.89, -1.14, -4.43, -0.91, -10.95, 1.86, -10.29, -1.75)
  )
})

test_that("PPs, centred Dir prior c=22", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
          bf = sapply(
            X = datalist_bl1,
            FUN = bf_multinomial,
            categories = 1:9,
            null_par = theta_benford(1),
            hyper_par = 22 * theta_benford(1)
            )
          )
        ),
      3),
    c(0.000, 0.000, 0.200, 0.001, 0.113, 0.067, 0.000, 0.110, 0.000, 0.986, 0.000,0.018)
  )
})


context("Mu-Dir BL2 - Bayes factors and posterior probabilities")

test_that("log10(BF), uniform prior", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl2,
          FUN = bf_multinomial,
          categories = 0:9,
          null_par = theta_benford(2),
          hyper_par = rep(1, 10))
        ),
      2),
    c(5.05, 5.90, 4.79, 3.51, 5.78, 4.19, 6.34, 5.12, 3.39, 4.89, 5.76, 7.13)
  )
})

test_that("PPs, uniform prior", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
            bf = sapply(
            X = datalist_bl2,
            FUN = bf_multinomial,
            categories = 0:9,
            null_par = theta_benford(2),
            hyper_par = rep(1, 10)
            )
          )
        ),
      3),
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})

test_that("log10(BF), centred Dir prior c=1", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl2,
          FUN = bf_multinomial,
          categories = 0:9,
          null_par = theta_benford(2),
          hyper_par = theta_benford(2)
          )
        ),
      2),
    c(11.27, 12.20, 11.11, 9.74, 12.03, 10.44, 12.64, 11.39, 9.61, 11.13, 11.99, 13.39)
  )
})

test_that("PPs, centred Dir prior c=1", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
            bf = sapply(
              X = datalist_bl2,
              FUN = bf_multinomial,
              categories = 0:9,
              null_par = theta_benford(2),
              hyper_par = theta_benford(2)
              )
          )
        ),
      3),
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})

test_that("log10(BF), centred Dir prior c=12", {
  expect_equal(
    round(
      log10(
        sapply(
          X = datalist_bl2,
          FUN = bf_multinomial,
          categories = 0:9,
          null_par = theta_benford(2),
          hyper_par = 12 * theta_benford(2)
          )
        ),
      2),
    c(4.60, 5.52, 4.45, 3.10, 5.35, 3.79, 5.95, 4.72, 2.98, 4.47, 5.31, 6.69
    )
  )
})

test_that("PPs, centred Dir prior c=12", {
  expect_equal(
    round(
      as.numeric(
        mapply(
          FUN = pcal::bfactor_to_prob,
          bf = sapply(
            X = datalist_bl2,
            FUN = bf_multinomial,
            categories = 0:9,
            null_par = theta_benford(2),
            hyper_par = 12 * theta_benford(2)
            )
          )
        ),
      3),
    c(1.000, 1.000, 1.000, 0.999, 1.000, 1.000, 1.000, 1.000, 0.999, 1.000, 1.000, 1.000)
  )
})

context("Mu-Dir - test in_favor H1")

test_that("Mu-Dir - BF in_favor H1, test 1", {
  expect_equal(
    round(
       log10(
        sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = rep(1, 9),
          in_favour = "H0"
        )
      ),
      2),
    round(
      log10(
        1/sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = rep(1, 9),
          in_favour = "H1"
        )
      ),
      2)
  )
})

test_that("Mu-Dir - BF in_favor H1, test 2", {
  expect_equal(
    round(
      log10(
        1/sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = rep(1, 9),
          in_favour = "H0"
        )
      ),
      2),
    round(
      log10(
        sapply(
          X = datalist_bl1,
          FUN = bf_multinomial,
          categories = 1:9,
          null_par = theta_benford(1),
          hyper_par = rep(1, 9),
          in_favour = "H1"
        )
      ),
      2)
  )
})

