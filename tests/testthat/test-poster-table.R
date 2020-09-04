
context("Generate poster tables")

test_that("Luxembourg BL1", {
  expect_equal(
    cbind(

      round(pcal::bfactor_to_prob(mapply(bf_binomial, success = 1:9, null_par = theta_benford(1), MoreArgs = list(x = luxembourg_bl1))), 3),

      c(
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(theta_benford(1)[1], 1 - theta_benford(1)[1]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(theta_benford(1)[2], 1 - theta_benford(1)[2]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(theta_benford(1)[3], 1 - theta_benford(1)[3]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(theta_benford(1)[4], 1 - theta_benford(1)[4]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(theta_benford(1)[5], 1 - theta_benford(1)[5]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(theta_benford(1)[6], 1 - theta_benford(1)[6]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(theta_benford(1)[7], 1 - theta_benford(1)[7]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(theta_benford(1)[8], 1 - theta_benford(1)[8]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(theta_benford(1)[9], 1 - theta_benford(1)[9]))), 3)),

      c(
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 1, null_par = theta_benford(1)[1], hyper_par = c(22 * theta_benford(1)[1], 22 - 22 * theta_benford(1)[1]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 2, null_par = theta_benford(1)[2], hyper_par = c(22 * theta_benford(1)[2], 22 - 22 * theta_benford(1)[2]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 3, null_par = theta_benford(1)[3], hyper_par = c(22 * theta_benford(1)[3], 22 - 22 * theta_benford(1)[3]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 4, null_par = theta_benford(1)[4], hyper_par = c(22 * theta_benford(1)[4], 22 - 22 * theta_benford(1)[4]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 5, null_par = theta_benford(1)[5], hyper_par = c(22 * theta_benford(1)[5], 22 - 22 * theta_benford(1)[5]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 6, null_par = theta_benford(1)[6], hyper_par = c(22 * theta_benford(1)[6], 22 - 22 * theta_benford(1)[6]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 7, null_par = theta_benford(1)[7], hyper_par = c(22 * theta_benford(1)[7], 22 - 22 * theta_benford(1)[7]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 8, null_par = theta_benford(1)[8], hyper_par = c(22 * theta_benford(1)[8], 22 - 22 * theta_benford(1)[8]))), 3),
        round(pcal::bfactor_to_prob(bf_binomial(luxembourg_bl1, success = 9, null_par = theta_benford(1)[9], hyper_par = c(22 * theta_benford(1)[9], 22 - 22 * theta_benford(1)[9]))), 3)),

      c(
        round(nigrini_z_test(luxembourg_bl1, success = 1, theta_benford(1)[1])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 2, theta_benford(1)[2])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 3, theta_benford(1)[3])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 4, theta_benford(1)[4])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 5, theta_benford(1)[5])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 6, theta_benford(1)[6])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 7, theta_benford(1)[7])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 8, theta_benford(1)[8])$p.value, 3),
        round(nigrini_z_test(luxembourg_bl1, success = 9, theta_benford(1)[9])$p.value, 3))
    ),
    rbind(
      c(0.000, 0.000, 0.000, 0.000),
      c(0.046, 0.048, 0.010, 0.001),
      c(0.921, 0.930, 0.662, 0.187),
      c(0.966, 0.974, 0.834, 0.663),
      c(0.000, 0.000, 0.000, 0.000),
      c(0.966, 0.974, 0.818, 0.494),
      c(0.955, 0.954, 0.708, 0.264),
      c(0.894, 0.873, 0.450, 0.086),
      c(0.977, 0.978, 0.830, 0.690)
    )
  )})










