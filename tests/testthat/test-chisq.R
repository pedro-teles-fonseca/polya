
library(digit.analysis)

context("Frequentist tests")

testthat::test_that("Chisq tests BL1", {
  expect_equal(
round(
  c(
test_chisq_multinomial(austria_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(belgium_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(finland_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(france_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(germany_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(greece_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(ireland_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(italy_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(luxembourg_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(netherlands_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(portugal_bl1, theta_benford(1), 1:9)[["p_value"]],
test_chisq_multinomial(spain_bl1, theta_benford(1), 1:9)[["p_value"]]),
  3),
    c(0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.009, 0.000, 0.000)
)})

testthat::test_that("Chisc tests BL1", {
  expect_equal(
    round(
      c(
        test_chisq_multinomial(austria_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(belgium_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(finland_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(france_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(germany_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(greece_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(ireland_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(italy_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(luxembourg_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(netherlands_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(portugal_bl2, theta_benford(2), 0:9)[["p_value"]],
        test_chisq_multinomial(spain_bl2, theta_benford(2), 0:9)[["p_value"]]),
      3),
    c(0.082, 0.236, 0.068, 0.005, 0.175, 0.016, 0.387, 0.075, 0.007, 0.060, 0.189, 0.855)
  )})


