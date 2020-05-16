# Multinomial-Dirichlet model - parameter estimation

mu_dir_estimate <- function( # parameter estimation - Dirichlet prior and Multinomial likelihood
  data,
  hyper.par = list(rep(1, times = 9), rep(1, times = 10)),
  stat = "mode",
  par.round = 3){

  hyper.par_bl1 <- hyper.par[[1]]
  hyper.par_bl2 <- hyper.par[[2]]

  x_bl1 <- as.numeric(table(msdigit(data)))
  x_bl2 <- as.numeric(table(smsdigit(data)))

  posterior_mean_bl1 <- (hyper.par_bl1 + x_bl1)/sum(hyper.par_bl1 + x_bl1)
  posterior_mode_bl1 <- (hyper.par_bl1 + x_bl1 - 1)/(sum(hyper.par_bl1 + x_bl1) - 9)

  posterior_mean_bl2 <- (hyper.par_bl2 + x_bl2)/sum(hyper.par_bl2 + x_bl2)
  posterior_mode_bl2 <- (hyper.par_bl2 + x_bl2 - 1)/(sum(hyper.par_bl2 + x_bl2) - 10)

  if(stat == "mode"){

    results_bl1 <- round(t(rbind(1:9, theta_benford(1), posterior_mode_bl1)), par.round)
    colnames(results_bl1) <- c("Digit", "Benford", "Estimate")

    results_bl2 <- round(t(rbind(0:9, theta_benford(2), posterior_mode_bl2)), par.round)
    colnames(results_bl2) <- c("Digit", "Benford", "Estimate")

  } else if(stat == "mean"){

    results_bl1 <- round(t(rbind(1:9, theta_benford(1), posterior_mean_bl1)), par.round)
    colnames(results_bl1) <- c("Digit","Benford", "Estimate")

    results_bl2 <- round(t(rbind(0:9, theta_benford(2), posterior_mean_bl2)), par.round)
    colnames(results_bl2) <- c("Digit","Benford", "Estimate")

  } else {

    stop('"Stat" argument must be either "mode" or "mean"')

  }

  results <- list(
    bl1 = results_bl1,
    bl2 = results_bl2
  )

  return(results)

}

# mu_dir_estimate(Portugal)
# mu_dir_estimate(Portugal, stat = "mode")
# mu_dir_estimate(Portugal, stat = "mean")
# mu_dir_estimate(Portugal, hyper.par = list(rep(1/9, 9), rep(1/10, 10)))
# mu_dir_estimate(Portugal, hyper.par = list(theta_benford(1), theta_benford(2)))
# mu_dir_estimate(Portugal, hyper.par = list(22*theta_benford(1), 12 * theta_benford(2)))

# Binomial-Beta model - parameter estimation

bin_beta_estimate <- function(

  data,
  a = list(rep(1, times = 9), rep(1, times = 10)),
  b = list(rep(1, times = 9), rep(1, times = 10)),
  stat = "mode",
  cred = 0.05,
  par.round = 3){

  a_bl1 <- a[[1]]
  b_bl1 <- b[[1]]

  a_bl2 <- a[[2]]
  b_bl2 <- b[[2]]

  n_bl1 <- length(!is.na(msdigit(data)))
  n_bl2 <- length(!is.na(smsdigit(data)))

  x_bl1 <- as.numeric(table(msdigit(data)))
  x_bl2 <- as.numeric(table(smsdigit(data)))

  posterior_mean_bl1 <- (a_bl1 + x_bl1)/(a_bl1 + x_bl1 + b_bl1 + n_bl1 - x_bl1)
  posterior_mode_bl1 <- (a_bl1 + x_bl1-1)/(a_bl1 + x_bl1 + b_bl1 + n_bl1 - x_bl1 - 2)

  posterior_mean_bl2 <- (a_bl2 + x_bl2)/(a_bl2 + x_bl2 + b_bl2 + n_bl2 - x_bl2)
  posterior_mode_bl2 <- (a_bl2 + x_bl2-1)/(a_bl2 + x_bl2 + b_bl2 + n_bl2 - x_bl2 - 2)

  q <- cred/2

  ci_bl1_left <- qbeta(q, a_bl1 + x_bl1, b_bl1 + n_bl1 - x_bl1, lower.tail = TRUE)
  ci_bl1_right <- qbeta(q, a_bl1 + x_bl1, b_bl1 + n_bl1 - x_bl1, lower.tail = FALSE)

  ci_bl2_left <- qbeta(q, a_bl2 + x_bl2, b_bl2 + n_bl2 - x_bl2, lower.tail = TRUE)
  ci_bl2_right <- qbeta(q, a_bl2 + x_bl2, b_bl2 + n_bl2 - x_bl2, lower.tail = FALSE)

  if(stat == "mode"){

    results_bl1 <- round(t(rbind(1:9, theta_benford(1), posterior_mode_bl1, ci_bl1_left, ci_bl1_right)), par.round)
    colnames(results_bl1) <- c("Digit", "Benford", "Estimate", paste0("q", as.character(q)), paste0("q", as.character(round(1-q,3))))

    results_bl2 <- round(t(rbind(0:9, theta_benford(2), posterior_mode_bl2, ci_bl2_left, ci_bl2_right)), par.round)
    colnames(results_bl2) <- c("Digit", "Benford", "Estimate", paste0("q", as.character(q)), paste0("q", as.character(round(1-q,3))))

  } else if(stat == "mean"){

    results_bl1 <- round(t(rbind(1:9, theta_benford(1), posterior_mean_bl1, ci_bl1_left, ci_bl1_right)), par.round)
    colnames(results_bl1) <- c("Digit", "Benford", "Estimate", paste0("q", as.character(q)), paste0("q", as.character(round(1-q,3))))

    results_bl2 <- round(t(rbind(0:9, theta_benford(2), posterior_mean_bl2, ci_bl2_left, ci_bl2_right)), par.round)
    colnames(results_bl2) <- c("Digit", "Benford", "Estimate", paste0("q", as.character(q)), paste0("q", as.character(round(1-q,3))))

  } else {

    stop('"Stat" argument must be either "mode" or "mean"')

  }

  results <- list(
    bl1 = results_bl1,
    bl2 = results_bl2
  )

  return(results)

}

# bin_beta_estimate(Portugal,
#   stat = "mean",
#   a = list(rep(1, 9), rep(1, 10)),
#   b = list(rep(8, 9), rep(9, 10)),
#   cred = 0.05)
#
#


