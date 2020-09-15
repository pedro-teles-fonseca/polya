
context("Bin-Beta model - Bayes factors and posterior probabilities")

test_that("Austria PP, unif prior", {
  expect_equal(
    as.numeric(
      round(
        sapply(
          X = mapply(
            bf_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(data = austria_bl1, hyper_par = c(1, 1))
            ),
          FUN = pcal::bfactor_to_prob),
      3)
      ),
    c(0.542, 0.951, 0.954, 0.194, 0.016, 0.973, 0.001, 0.947, 0.968)
  )
})

test_that("Austria PP, Dir prior c = 1", {
  expect_equal(
    round(
        pcal::bfactor_to_prob(
          c(
            bf_binomial(austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
            bf_binomial(austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
            bf_binomial(austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
            bf_binomial(austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
            bf_binomial(austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
            bf_binomial(austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
            bf_binomial(austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
            bf_binomial(austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
            bf_binomial(austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9])))
          ),
        3),
    c(0.618, 0.966, 0.962, 0.290, 0.030, 0.978, 0.000, 0.941, 0.976)
  )
})

test_that("Austria PP, Dir prior c = 22", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bf_binomial(austria_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
          bf_binomial(austria_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
          bf_binomial(austria_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
          bf_binomial(austria_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
          bf_binomial(austria_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
          bf_binomial(austria_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
          bf_binomial(austria_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
          bf_binomial(austria_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
          bf_binomial(austria_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9])))
        ),
      3),
    c(0.232, 0.813, 0.782, 0.059, 0.005, 0.844, 0.000, 0.647, 0.812)
  )
})

test_that("Belgium PP, unif prior", {
  expect_equal(
    as.numeric(
      round(
        sapply(
          X = mapply(
            bf_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(data = belgium_bl1, hyper_par = c(1, 1))
            ),
          FUN = pcal::bfactor_to_prob),
        3)
      ),
    c(0.003, 0.028, 0.008, 0.963, 0.940, 0.960, 0.977, 0.957, 0.920)
  )
})

test_that("Belgium PP, Dir prior c = 1", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bf_binomial(belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1])),
          bf_binomial(belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2])),
          bf_binomial(belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3])),
          bf_binomial(belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4])),
          bf_binomial(belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5])),
          bf_binomial(belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6])),
          bf_binomial(belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7])),
          bf_binomial(belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8])),
          bf_binomial(belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9])))
        ),
      3),
    c(0.003, 0.047, 0.014, 0.967, 0.941, 0.961, 0.980, 0.969, 0.903)
  )
})

test_that("Belgium PP, Dir prior c = 22", {
  expect_equal(
    round(
      pcal::bfactor_to_prob(
        c(
          bf_binomial(belgium_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22*theta_benford(1)[1], 22 - 22*theta_benford(1)[1])),
          bf_binomial(belgium_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22*theta_benford(1)[2], 22 - 22*theta_benford(1)[2])),
          bf_binomial(belgium_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22*theta_benford(1)[3], 22 - 22*theta_benford(1)[3])),
          bf_binomial(belgium_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22*theta_benford(1)[4], 22 - 22*theta_benford(1)[4])),
          bf_binomial(belgium_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22*theta_benford(1)[5], 22 - 22*theta_benford(1)[5])),
          bf_binomial(belgium_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22*theta_benford(1)[6], 22 - 22*theta_benford(1)[6])),
          bf_binomial(belgium_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22*theta_benford(1)[7], 22 - 22*theta_benford(1)[7])),
          bf_binomial(belgium_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22*theta_benford(1)[8], 22 - 22*theta_benford(1)[8])),
          bf_binomial(belgium_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22*theta_benford(1)[9], 22 - 22*theta_benford(1)[9])))
        ),
      3),
    c(0.001, 0.009, 0.003, 0.799 ,0.674, 0.749, 0.850, 0.777, 0.515))
})

context("Bin-Beta - test in_favor H1")

test_that("B-B model - BF in_favor H1, test 1", {
  expect_equal(
    as.numeric(
      round(
          mapply(
            bf_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(data = belgium_bl1, hyper_par = c(1, 1), in_favour = "H1")
          )
        ,
        2)
    ),
    as.numeric(
      round(
        1/mapply(
          bf_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(data = belgium_bl1, hyper_par = c(1, 1), in_favour = "H0")
        )
        ,
        2)
    )
  )
})

test_that("B-B model - BF in_favor H1, test 2", {
  expect_equal(
    as.numeric(
      round(
        mapply(
          bf_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(data = belgium_bl1, hyper_par = c(1, 1), in_favour = "Alternative")
        )
        ,
        2)
    ),
    as.numeric(
      round(
        1/mapply(
          bf_binomial,
          null_par = theta_benford(1),
          success = 1:9,
          MoreArgs = list(data = belgium_bl1, hyper_par = c(1, 1), in_favour = "null")
        )
        ,
        2)
    )
  )
})

test_that("B-B model - BF in_favor H1, test 3", {
  expect_equal(
    as.numeric(
      round(
        log10(
          mapply(
            bf_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(data =  belgium_bl1, hyper_par = c(1, 1), in_favour = "alternative")
          )
        ),
        2)
    ),
    as.numeric(
      round(
        log10(
          1/mapply(
            bf_binomial,
            null_par = theta_benford(1),
            success = 1:9,
            MoreArgs = list(data =  belgium_bl1, hyper_par = c(1, 1), in_favour = "Null")
          )
        ),
        2)
    )
  )
})

# context("Bin-Beta Error messages")
#
# test_that("H0 - H1 error messages", {
#   expect_error(
#     bf_binomial(data =  belgium_bl1, success = 1, null_par = .5, in_favour = "h01")
#   )}
# )
#
# test_that("H0 - H1 error messages", {
#   expect_error(
#     bf_binomial(data =  belgium_bl1, success = 1, null_par = .5, in_favour = "h")
#   )}
# )
#
# test_that("H0 - H1 error messages", {
#   expect_error(
#     bf_binomial(data =  belgium_bl1, success = 1, null_par = .5, in_favour = "alternativee")
#   )}
# )
#
# test_that("H0 - H1 error messages", {
#   expect_error(
#     bf_binomial(data =  belgium_bl1, success = 1, null_par = .5, in_favour = "altern")
#   )}
# )
#
# context("Bin-Beta - only one observed level")
#
# test_that("Only successes observed", {
#   expect_equal(
#     bf_binomial(data =  c(1,1,1,1), success = 1, null_par = .2, hyper_par = c(1, 1)),
#     bf_binomial(data =  c(0,0,0,0), success = 0, null_par = .2, hyper_par = c(1, 1))
#   )
# })
#
# test_that("Sucess level not observed test 1", {
#   expect_equal(
#     suppressWarnings(bf_binomial(data =  c(1,1,1,1), success = 0, null_par = .2, hyper_par = c(1, 1))),
#     suppressWarnings(bf_binomial(data =  c(0,0,0,0), success = 1, null_par = .2, hyper_par = c(1, 1)))
#   )
# })
#
# test_that("Sucess level not observed test 2", {
#   expect_warning(
#     bf_binomial(data =  c(1,1,1,1), success = 0, null_par = .2, hyper_par = c(1, 1))
#   )
# })
#
# test_that("Sucess level not observed test 3", {
#   expect_warning(
#     bf_binomial(data =  c(0,0,0,0), success = 1, null_par = .2, hyper_par = c(1, 1))
#   )
# })


