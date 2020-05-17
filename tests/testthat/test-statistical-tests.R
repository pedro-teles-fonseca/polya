
context("statistical tests")

testthat::test_that("Chisq tests BL1", {
  expect_equal(
round(
  c(
chisq_test_multinomial(austria_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(belgium_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(finland_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(france_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(germany_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(greece_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(ireland_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(italy_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(luxembourg_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(netherlands_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(portugal_bl1, theta_benford(1), 1:9)[["p.value"]],
chisq_test_multinomial(spain_bl1, theta_benford(1), 1:9)[["p.value"]]),
  3),
    c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.009, 0.000, 0.000)
)})

testthat::test_that("Chisc tests BL1", {
  expect_equal(
    round(
      c(
        chisq_test_multinomial(austria_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(belgium_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(finland_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(france_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(germany_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(greece_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(ireland_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(italy_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(luxembourg_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(netherlands_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(portugal_bl2, theta_benford(2), 0:9)[["p.value"]],
        chisq_test_multinomial(spain_bl2, theta_benford(2), 0:9)[["p.value"]]),
      3),
    c(0.082, 0.236, 0.068, 0.005, 0.175, 0.016, 0.387, 0.075, 0.007, 0.060, 0.189, 0.855)
  )})

testthat::test_that("Chisc categories order test 1", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, theta_benford(2), 0:9)[["p.value"]],
    0.8549962)
})

testthat::test_that("Chisc categories order test 2", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, theta_benford(2), 0:9)[["p.value"]],
    chisq_test_multinomial(spain_bl2, rev(theta_benford(2)), 9:0)[["p.value"]])
})

testthat::test_that("Chisc categories order test 3", {
  expect_equal(
    chisq_test_multinomial(spain_bl2, theta_benford(2), 0:9)[["p.value"]],
    chisq_test_multinomial(spain_bl2, theta_benford(2)[c(2:5, 1, 7:10, 6)], c(1:4, 0, 6:9, 5))[["p.value"]])
})

testthat::test_that("Nigrini test austria_bl1", {
  expect_equal(
    c(
      round(nigrini_z_test(austria_bl1, sucess = 1, null.par = theta_benford(1)[1])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 2, null.par = theta_benford(1)[2])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 3, null.par = theta_benford(1)[3])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 4, null.par = theta_benford(1)[4])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 5, null.par = theta_benford(1)[5])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 6, null.par = theta_benford(1)[6])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 7, null.par = theta_benford(1)[7])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 8, null.par = theta_benford(1)[8])[["p.value"]], 3),
      round(nigrini_z_test(austria_bl1, sucess = 9, null.par = theta_benford(1)[9])[["p.value"]], 3)
    ),
    c(0.019, 0.493, 0.406, 0.001, 0.000, 0.740, 0.000, 0.191, 0.422))
})

testthat::test_that("Nigrini test portugal_bl1", {
  expect_equal(
    c(
      round(nigrini_z_test(portugal_bl1, sucess = 1, null.par = theta_benford(1)[1])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 2, null.par = theta_benford(1)[2])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 3, null.par = theta_benford(1)[3])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 4, null.par = theta_benford(1)[4])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 5, null.par = theta_benford(1)[5])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 6, null.par = theta_benford(1)[6])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 7, null.par = theta_benford(1)[7])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 8, null.par = theta_benford(1)[8])[["p.value"]], 3),
      round(nigrini_z_test(portugal_bl1, sucess = 9, null.par = theta_benford(1)[9])[["p.value"]], 3)
    ),
    c(0.000, 0.000, 0.662, 0.000, 0.726, 0.848, 0.077, 0.591, 0.959))
})

aux_df_1 <-   data.frame(rbind(
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[1], sucess = 1, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[2], sucess = 2, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[3], sucess = 3, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[4], sucess = 4, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[6], sucess = 5, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[6], sucess = 6, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[7], sucess = 7, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[8], sucess = 8, transf = "log10"),
  test.null.binomial(austria_bl1, null.par = theta_benford(1)[9], sucess = 9, transf = "log10")
), stringsAsFactors = FALSE)

aux_df_1[[1]] <- as.character(aux_df_1[[1]])
aux_df_1[[2]] <- as.numeric(aux_df_1[[2]])
aux_df_1[[3]] <- as.character(aux_df_1[[3]])
aux_df_1[[4]] <- as.numeric(aux_df_1[[4]])

aux_df_2 <- data.frame(rbind(
  c("austria_bl1",     0.07,       "Negative",    0.06757283),
  c("austria_bl1",     1.29,       "Weak",        0.56318035),
  c("austria_bl1",     1.32,       "Weak",        0.56871103),
  c("austria_bl1",     -0.62,      "Negative",    -1.62305340),
  c("austria_bl1",     -4.28,      "Negative",    1.30469250),
  c("austria_bl1",     1.55,       "Weak",        0.60794138),
  c("austria_bl1",     -3.02,      "Negative",    1.49439135),
  c("austria_bl1",     1.25,       "Weak",        0.55531677),
  c("austria_bl1",     1.48,       "Weak",        0.59639593)
), stringsAsFactors = FALSE)
names(aux_df_2) <- names(aux_df_1)

aux_df_2[[1]] <- as.character(aux_df_2[[1]])
aux_df_2[[2]] <- as.numeric(aux_df_2[[2]])
aux_df_2[[3]] <- as.character(aux_df_2[[3]])
aux_df_2[[4]] <- as.numeric(aux_df_2[[4]])


testthat::test_that("test.null.binomial test 1", {
  expect_equal(aux_df_1[, 1:3], aux_df_2[, 1:3])})





