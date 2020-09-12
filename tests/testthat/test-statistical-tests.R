
context("Classical tests")

test_that("Chisq tests BL1", {
  expect_equal(
    round(
      c(
        chisq_test_multinomial(austria_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(belgium_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(finland_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(france_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(germany_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(greece_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(ireland_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(italy_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(luxembourg_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(netherlands_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(portugal_bl1, 1:9, theta_benford(1))[["p.value"]],
        chisq_test_multinomial(spain_bl1, 1:9, theta_benford(1))[["p.value"]]
      ),
      3
    ),
    c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.009, 0.000, 0.000
    )
  )
})


test_that("Chisq tests BL2", {
  expect_equal(
    round(
      c(
        chisq_test_multinomial(austria_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(belgium_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(finland_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(france_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(germany_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(greece_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(ireland_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(italy_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(luxembourg_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(netherlands_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(portugal_bl2, 0:9, theta_benford(2))[["p.value"]],
        chisq_test_multinomial(spain_bl2, 0:9, theta_benford(2))[["p.value"]]
      ),
      3
    ),
    c(0.082, 0.236, 0.068, 0.005, 0.175, 0.016, 0.387, 0.075, 0.007, 0.060, 0.189,0.855
    )
  )
})

test_that("Chisq categories order test 1", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, 0:9, theta_benford(2))[["p.value"]],
    0.8549962)
})

test_that("Chisq categories order test 2", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, 0:9, theta_benford(2))[["p.value"]],
    chisq_test_multinomial(spain_bl2, 9:0, rev(theta_benford(2)))[["p.value"]]
  )
})

test_that("Chisq categories order test 3", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, 0:9, theta_benford(2))[["p.value"]],
    chisq_test_multinomial(spain_bl2, c(1:4, 0, 6:9, 5), theta_benford(2)[c(2:5, 1, 7:10, 6)])[["p.value"]]
  )
})

test_that("Nigrini test austria_bl1", {
  expect_equal(
    round(
      c(
        nigrini_z_test(austria_bl1, success = 1, null_par = theta_benford(1)[1])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 2, null_par = theta_benford(1)[2])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 3, null_par = theta_benford(1)[3])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 4, null_par = theta_benford(1)[4])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 5, null_par = theta_benford(1)[5])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 6, null_par = theta_benford(1)[6])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 7, null_par = theta_benford(1)[7])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 8, null_par = theta_benford(1)[8])[["p.value"]],
        nigrini_z_test(austria_bl1, success = 9, null_par = theta_benford(1)[9])[["p.value"]]
        ),
      3),
    c(0.019, 0.493, 0.406, 0.001, 0.000, 0.740, 0.000, 0.191, 0.422)
  )
})

test_that("Nigrini test portugal_bl1", {
  expect_equal(
    round(
      c(
        nigrini_z_test(portugal_bl1, success = 1, null_par = theta_benford(1)[1])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 2, null_par = theta_benford(1)[2])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 3, null_par = theta_benford(1)[3])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 4, null_par = theta_benford(1)[4])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 5, null_par = theta_benford(1)[5])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 6, null_par = theta_benford(1)[6])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 7, null_par = theta_benford(1)[7])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 8, null_par = theta_benford(1)[8])[["p.value"]],
        nigrini_z_test(portugal_bl1, success = 9, null_par = theta_benford(1)[9])[["p.value"]]
        ),
      3),
    c(0.000, 0.000, 0.662, 0.000, 0.726, 0.848, 0.077, 0.591, 0.959)
  )
})

test_that("test.null.binomial test 1", {
  df1 <-   data.frame(rbind(
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[1], success = 1, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[2], success = 2, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[3], success = 3, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[4], success = 4, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[5], success = 5, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[6], success = 6, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[7], success = 7, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[8], success = 8, transf = "log10"),
    test.null.binomial(austria_bl1, null_par = theta_benford(1)[9], success = 9, transf = "log10")
  ), stringsAsFactors = FALSE)

  df1[[1]] <- as.character(df1[[1]])
  df1[[2]] <- as.numeric(df1[[2]])
  df1[[3]] <- as.character(df1[[3]])
  df1[[4]] <- as.numeric(df1[[4]])

  df2 <- data.frame(rbind(
    c("austria_bl1",     0.07,       "Weak",    0.542),
    c("austria_bl1",     1.29,       "Strong",        0.951),
    c("austria_bl1",     1.32,       "Strong",        0.954),
    c("austria_bl1",     -0.62,      "Negative",    0.194),
    c("austria_bl1",     -1.78,      "Negative",    0.016),
    c("austria_bl1",     1.55,       "Very Strong",        0.973),
    c("austria_bl1",     -3.02,      "Negative",    0.001),
    c("austria_bl1",     1.25,       "Strong",        0.947),
    c("austria_bl1",     1.48,       "Strong",        0.968)
  ), stringsAsFactors = FALSE)

  df2[[1]] <- as.character(df2[[1]])
  df2[[2]] <- as.numeric(df2[[2]])
  df2[[3]] <- as.character(df2[[3]])
  df2[[4]] <- as.numeric(df2[[4]])

  expect_equal(unname(df1), unname(df2))

  expect_equal(
    unname(bfactor_interpret(10^(df2$X2))),
    unname(df1[[3]]))

})

test_that("test.null.binomial test 2", {
  df1 <-   data.frame(rbind(
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[1], success = 1, hyper.par = c(22*theta_benford(1)[1], 22-22*theta_benford(1)[1]), transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[2], success = 2, hyper.par = c(22*theta_benford(1)[2], 22-22*theta_benford(1)[2]), transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[3], success = 3, hyper.par = c(22*theta_benford(1)[3], 22-22*theta_benford(1)[3]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[4], success = 4, hyper.par = c(22*theta_benford(1)[4], 22-22*theta_benford(1)[4]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[5], success = 5, hyper.par = c(22*theta_benford(1)[5], 22-22*theta_benford(1)[5]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[6], success = 6, hyper.par = c(22*theta_benford(1)[6], 22-22*theta_benford(1)[6]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[7], success = 7, hyper.par = c(22*theta_benford(1)[7], 22-22*theta_benford(1)[7]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[8], success = 8, hyper.par = c(22*theta_benford(1)[8], 22-22*theta_benford(1)[8]),transf = "log10"),
    test.null.binomial(portugal_bl1, null_par = theta_benford(1)[9], success = 9, hyper.par = c(22*theta_benford(1)[9], 22-22*theta_benford(1)[9]),transf = "log10")
  ), stringsAsFactors = FALSE)

  df1[[2]] <- as.numeric(df1[[2]])
  df1[[3]] <- as.character(df1[[3]])
  df1[[4]] <- as.numeric(df1[[4]])

  df2 <- data.frame(
    rbind(
      c(-10.22, "Negative", 0.000),
      c(-3.90, "Negative",  0.000),
      c(0.68, "Substantial", 0.828),
      c(-3.82,"Negative",  0.000),
      c(0.70, "Substantial", 0.834),
      c(0.75, "Substantial", 0.848),
      c(-0.11, "Negative", 0.435),
      c(0.70, "Substantial", 0.834),
      c(0.76, "Substantial", 0.853)
    ), stringsAsFactors = FALSE)

  df2[[1]] <- as.numeric(df2[[1]])
  df2[[2]] <- as.character(df2[[2]])
  df2[[3]] <- as.numeric(df2[[3]])

  expect_equal(
    (unname(df1[2:4])),
    (unname(df2)))
})

test_that("test.null.multinomial unif prior", {
  df1 <- data.frame(rbind(
    test.null.multinomial(datalist_bl1[[1]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[1]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[2]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[2]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[3]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[3]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[4]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[4]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[5]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[5]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[6]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[6]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[7]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[7]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[8]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[8]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[9]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[9]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10"),
    test.null.multinomial(datalist_bl1[[10]], null_par = theta_benford(1), categories = 1:9, hyper_par = rep(1, 9), transf = "log10"),
    test.null.multinomial(datalist_bl2[[10]], null_par = theta_benford(2), categories = 0:9, hyper_par = rep(1, 10), transf = "log10")
  ), row.names =  NULL)[, 2:5]

  df1[1] <- as.double(df1[[1]])
  df1[2] <- as.character(df1[[2]])
  df1[3] <- as.double(df1[[3]])
  df1[4] <- as.double(df1[[4]])

  df2 <- data.frame(
    rbind(
      c(-3.10, "Negative", 0.001, 0.000),
      c(5.05, "Decisive", 1.000, 0.082),
      c(-1.78, "Negative", 0.016, 0.000),
      c(5.90, "Decisive", 1.000, 0.236),
      c(1.06, "Strong", 0.919, 0.000),
      c(4.79, "Decisive", 1.000, 0.068),
      c(-1.40, "Negative", 0.039, 0.000),
      c(3.51, "Decisive", 1.000, 0.005),
      c(1.21, "Strong", 0.942, 0.000),
      c(5.78, "Decisive", 1.000, 0.175),
      c(0.89, "Substantial", 0.885, 0.000),
      c(4.19, "Decisive", 1.000, 0.016),
      c(-2.37, "Negative", 0.004, 0.000),
      c(6.34, "Decisive", 1.000, 0.387),
      c(1.23, "Strong", 0.945, 0.000),
      c(5.12, "Decisive", 1.000, 0.075),
      c(-8.88, "Negative", 0.000, 0.000),
      c(3.39, "Decisive", 1.000, 0.007),
      c(3.81, "Decisive", 1.000, 0.009),
      c(4.89, "Decisive", 1.000, 0.060)
    ), stringsAsFactors = FALSE, row.names =  NULL)

  df2[1] <- as.double(df2[[1]])
  df2[2] <- as.character(df2[[2]])
  df2[3] <- as.double(df2[[3]])
  df2[4] <- as.double(df2[[4]])

  expect_equal(
    unname(df1),
    unname(df2))
})

test_that("test.null.multinomial dir prior (c=1)", {
  df1 <- data.frame(rbind(
    test.null.multinomial(datalist_bl1[[1]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[1]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[2]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[2]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[3]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[3]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[4]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[4]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[5]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[5]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[6]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[6]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[7]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[7]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[8]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[8]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[9]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[9]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10"),
    test.null.multinomial(datalist_bl1[[10]], null_par = theta_benford(1), categories = 1:9, hyper_par=theta_benford(1), transf = "log10"),
    test.null.multinomial(datalist_bl2[[10]], null_par = theta_benford(2), categories = 0:9, hyper_par=theta_benford(2), transf = "log10")
  ), row.names =  NULL)[, 2:5]

  df1[1] <- as.double(df1[[1]])
  df1[2] <- as.character(df1[[2]])
  df1[3] <- as.double(df1[[3]])
  df1[4] <- as.double(df1[[4]])

  df2 <- data.frame(
    rbind(
      c(1.93, "Very Strong", 0.988,  0.000),
      c(11.27, "Decisive", 1.000,  0.082),
      c(3.35, "Decisive", 1.000,  0.000),
      c(12.20, "Decisive", 1.000,  0.236),
      c(6.47, "Decisive", 1.000,  0.000),
      c(11.11, "Decisive", 1.000,  0.068),
      c(4.02, "Decisive", 1.000,  0.000),
      c(9.74, "Decisive", 1.000,  0.005),
      c(6.14, "Decisive", 1.000,  0.000),
      c(12.03, "Decisive", 1.000,  0.175),
      c(5.87, "Decisive", 1.000,  0.000),
      c(10.44, "Decisive", 1.000,  0.016),
      c(2.48, "Decisive", 0.997,  0.000),
      c(12.64, "Decisive", 1.000,  0.387),
      c(6.12, "Decisive", 1.000,  0.000),
      c(11.39, "Decisive", 1.000,  0.075),
      c(-4.33, "Negative", 0.000,  0.000),
      c(9.61,"Decisive", 1.000,  0.007),
      c(8.98, "Decisive", 1.000,  0.009),
      c(11.13, "Decisive", 1.000,  0.060)
    ), stringsAsFactors = FALSE, row.names =  NULL)

  df2[1] <- as.double(df2[[1]])
  df2[2] <- as.character(df2[[2]])
  df2[3] <- as.double(df2[[3]])
  df2[4] <- as.double(df2[[4]])

  expect_equal(
    unname(df1),
    unname(df2))
})
