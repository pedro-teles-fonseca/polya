
context("Bin-Beta model - Bayes factors and posterior probabilities")

testthat::test_that("Austria log10(BF), unif prior", {
  expect_equal(
    round(
      log10(
        mapply(
          bfactor_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(x = austria_bl1, hyper_par = c(1, 1))
          )
        ),
      2),
    c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48))
  })

testthat::test_that("Austria PP, unif prior", {
  expect_equal(
    as.numeric(
      round(
        sapply(
          X = mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = austria_bl1, hyper_par = c(1, 1))
            ),
          FUN = pcal::bfactor_to_prob),
      3)
      ),
    c(0.542, 0.951, 0.954, 0.194, 0.016, 0.973, 0.001, 0.947, 0.968)
  )
})

testthat::test_that("Austria log10(BF), Dir prior c=1", {
  expect_equal(
    round(
      log10(
        c(
          bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
          bfactor_binomial(x = austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
          bfactor_binomial(x = austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
          bfactor_binomial(x = austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
          bfactor_binomial(x = austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
          bfactor_binomial(x = austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
          bfactor_binomial(x = austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
          bfactor_binomial(x = austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
          bfactor_binomial(x = austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9]))
          )
        ),
      2),
    c(0.21,  1.45,  1.40, -0.39, -1.51,  1.65, -3.40,  1.20,  1.60)
  )
})

testthat::test_that("Austria PP, Dir prior c=1", {
  expect_equal(
    round(
        pcal::bfactor_to_prob(
          c(
            bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
            bfactor_binomial(x = austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
            bfactor_binomial(x = austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
            bfactor_binomial(x = austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
            bfactor_binomial(x = austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
            bfactor_binomial(x = austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
            bfactor_binomial(x = austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
            bfactor_binomial(x = austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
            bfactor_binomial(x = austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9])))
          ),
        3),
    c(0.618, 0.966, 0.962, 0.290, 0.030, 0.978, 0.000, 0.941, 0.976)
  )
})

testthat::test_that("Austria log10(BF), Dir prior c=22", {
  expect_equal(
    round(
      log10(
        c(
          bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
          bfactor_binomial(x = austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
          bfactor_binomial(x = austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
          bfactor_binomial(x = austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
          bfactor_binomial(x = austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
          bfactor_binomial(x = austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
          bfactor_binomial(x = austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
          bfactor_binomial(x = austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
          bfactor_binomial(x = austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9]))
          )
        ),
      2),
    c(-0.52,  0.64,  0.56, -1.21, -2.32,  0.73, -4.10,  0.26,  0.63)
  )
})

testthat::test_that("Austria PP, Dir prior c=22", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bfactor_binomial(x = austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
          bfactor_binomial(x = austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
          bfactor_binomial(x = austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
          bfactor_binomial(x = austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
          bfactor_binomial(x = austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
          bfactor_binomial(x = austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
          bfactor_binomial(x = austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
          bfactor_binomial(x = austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
          bfactor_binomial(x = austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9])))
        ),
      3),
    c(0.232, 0.813, 0.782, 0.059, 0.005, 0.844, 0.000, 0.647, 0.812)
  )
})

testthat::test_that("Belgium log10(BF), unif prior", {
  expect_equal(
    as.numeric(
      round(
        log10(
          mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1))
            )
          ),
        2)
      ),
    c(-2.59, -1.55, -2.10,  1.41,  1.19,  1.38,  1.62,  1.35,  1.06)
  )
})

testthat::test_that("Belgium PP, unif prior", {
  expect_equal(
    as.numeric(
      round(
        sapply(
          X = mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1))
            ),
          FUN = pcal::bfactor_to_prob),
        3)
      ),
    c(0.003, 0.028, 0.008, 0.963, 0.940, 0.960, 0.977, 0.957, 0.920)
  )
})

testthat::test_that("Belgium log10(BF), Dir prior c=1", {
  expect_equal(
    round(
      log10(
        c(
          bfactor_binomial(x = belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
          bfactor_binomial(x = belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
          bfactor_binomial(x = belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
          bfactor_binomial(x = belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
          bfactor_binomial(x = belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
          bfactor_binomial(x = belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
          bfactor_binomial(x = belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
          bfactor_binomial(x = belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
          bfactor_binomial(x = belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9])))
        ),
      2),
    c(-2.49, -1.31, -1.84,  1.47,  1.20,  1.39,  1.69,  1.49,  0.97)
  )
})

testthat::test_that("Belgium PP, Dir prior c=1", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bfactor_binomial(x = belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
          bfactor_binomial(x = belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
          bfactor_binomial(x = belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
          bfactor_binomial(x = belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
          bfactor_binomial(x = belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
          bfactor_binomial(x = belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
          bfactor_binomial(x = belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
          bfactor_binomial(x = belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
          bfactor_binomial(x = belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9])))
        ),
      3),
    c(0.003, 0.047, 0.014, 0.967, 0.941, 0.961, 0.980, 0.969, 0.903)
  )
})

testthat::test_that("Belgium log10(BF), Dir prior c=22", {
  expect_equal(
   round(
     log10(
       c(
         bfactor_binomial(x = belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
         bfactor_binomial(x = belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
         bfactor_binomial(x = belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
         bfactor_binomial(x = belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
         bfactor_binomial(x = belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
         bfactor_binomial(x = belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
         bfactor_binomial(x = belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
         bfactor_binomial(x = belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
         bfactor_binomial(x = belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9])))
       ),
     2),
    c(-3.12, -2.04, -2.59,  0.60,  0.31,  0.47,  0.75,  0.54,  0.03)
  )
})

testthat::test_that("Belgium BF, Dir prior c=22", {
  expect_equal(
    round(
      c(
        bfactor_binomial(x = belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
        bfactor_binomial(x = belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
        bfactor_binomial(x = belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
        bfactor_binomial(x = belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
        bfactor_binomial(x = belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
        bfactor_binomial(x = belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
        bfactor_binomial(x = belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
        bfactor_binomial(x = belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
        bfactor_binomial(x = belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9]))
        ),
      2),
    c(0.00, 0.01, 0.00, 3.98, 2.07, 2.98, 5.67, 3.48, 1.06)
  )
})

testthat::test_that("Belgium PP, Dir prior c=22", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bfactor_binomial(x = belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
          bfactor_binomial(x = belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
          bfactor_binomial(x = belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
          bfactor_binomial(x = belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
          bfactor_binomial(x = belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
          bfactor_binomial(x = belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
          bfactor_binomial(x = belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
          bfactor_binomial(x = belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
          bfactor_binomial(x = belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9])))
        ),
      3),
    c(0.001, 0.009, 0.003, 0.799 ,0.674, 0.749, 0.850, 0.777, 0.515))
})

context("Bin-Beta - test in_favor H1")

testthat::test_that("Bin-Beta model - BF in_favor H1, test 1", {
  expect_equal(
    as.numeric(
      round(
          mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "H1")
          )
        ,
        2)
    ),
    as.numeric(
      round(
        1/mapply(
          bfactor_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "H0")
        )
        ,
        2)
    )
  )
})

testthat::test_that("Bin-Beta model - BF in_favor H1, test 2", {
  expect_equal(
    as.numeric(
      round(
        mapply(
          bfactor_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "Alternative")
        )
        ,
        2)
    ),
    as.numeric(
      round(
        1/mapply(
          bfactor_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "null")
        )
        ,
        2)
    )
  )
})

testthat::test_that("Bin-Beta model - BF in_favor H1, with log10", {
  expect_equal(
    as.numeric(
      round(
        log10(
          mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "H1")
            )
          ),
        2)
      ),
    as.numeric(
      round(
        log10(
          1/mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "H0")
            )
          ),
        2)
      )
    )
})

testthat::test_that("Bin-Beta BF in_favor H1, with log10", {
  expect_equal(
    as.numeric(
      round(
        log10(
          mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "alternative")
          )
        ),
        2)
    ),
    as.numeric(
      round(
        log10(
          1/mapply(
            bfactor_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(x = belgium_bl1, hyper_par = c(1, 1), in_favour = "Null")
          )
        ),
        2)
    )
  )
})

context("Bin-Beta Error messages")

testthat::test_that("H0 - H1 error messages", {
  expect_error(
    bfactor_binomial(x = belgium_bl1, success = 1, null_par = .5, in_favour = "h01")
  )}
)

testthat::test_that("H0 - H1 error messages", {
  expect_error(
    bfactor_binomial(x = belgium_bl1, success = 1, null_par = .5, in_favour = "h")
  )}
)

testthat::test_that("H0 - H1 error messages", {
  expect_error(
    bfactor_binomial(x = belgium_bl1, success = 1, null_par = .5, in_favour = "alternativee")
  )}
)

testthat::test_that("H0 - H1 error messages", {
  expect_error(
    bfactor_binomial(x = belgium_bl1, success = 1, null_par = .5, in_favour = "altern")
  )}
)

context("Bin-Beta - only one observed level")

testthat::test_that("Only successes observed", {
  expect_equal(
    bfactor_binomial(x = c(1,1,1,1), success = 1, null_par = .2, hyper_par = c(1, 1)),
    bfactor_binomial(x = c(0,0,0,0), success = 0, null_par = .2, hyper_par = c(1, 1))
  )
})

testthat::test_that("Sucess level not observed test 1", {
  expect_equal(
    suppressWarnings(bfactor_binomial(x = c(1,1,1,1), success = 0, null_par = .2, hyper_par = c(1, 1))),
    suppressWarnings(bfactor_binomial(x = c(0,0,0,0), success = 1, null_par = .2, hyper_par = c(1, 1)))
  )
})

testthat::test_that("Sucess level not observed test 2", {
  expect_warning(
    bfactor_binomial(x = c(1,1,1,1), success = 0, null_par = .2, hyper_par = c(1, 1))
  )
})

testthat::test_that("Sucess level not observed test 3", {
  expect_warning(
    bfactor_binomial(x = c(0,0,0,0), success = 1, null_par = .2, hyper_par = c(1, 1))
  )
})


