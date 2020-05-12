
source("R/auxiliary-functions.R")

# Bayes factor
# ------------------------------------------
bayes.factor <- function(x, null.par, hyper.par, model = "multinomial") {

  if (model == "multinomial"){
    exp(sum(x * log(null.par)) + sum(lgamma(hyper.par)) + lgamma(sum(hyper.par + x)) - lgamma(sum(hyper.par)) - sum(lgamma(hyper.par + x)))
    } else if (model == "Binomial") {

    } else {

  }

}

# Testing Multinomial and Binomial point null hypothesese
# ------------------------------------------
test.point.null <- function(x,
  null.par,
  hyper.par = rep.int(1, length(unique(x))),
  model = "multinomial",
  pi_null = 0.5,
  log_10 = TRUE,
  bf.round = 2,
  probs.round = 3) {

  p_value <- chisq.test(table(x), p = null.par)$p.value
  bf <- bayes.factor(table(x), null.par, hyper.par)

  results <- data.frame(
    "sample" = deparse(substitute(x)),
    "BayesFactor" = if(isTRUE(log_10)){round(log10(bf), bf.round)} else {round(bf, bf.round)},
    "Evidence" = ifelse(bf < 1, "Negative",
                  ifelse(bf < 3.2, "Weak",
                    ifelse(bf < 10, "Substantial",
                      ifelse(bf < 100, "Strong", "Decisive")))),
    "P(H0|X)" = round(((1 + ((1 - pi_null) / pi_null) * (1 / bf)))^(-1), probs.round),
    "LB.P(H0|X)" = round(pcal(p_value), probs.round)
    )

    if(isTRUE(log_10)){names(results) <- gsub("BayesFactor(X)", "log10(BayesFactor(x))", names(results))}

    results
}


  test.point.null(x = msdigit(Austria), null.par = theta_benford(1))
  test.point.null(x = smsdigit(Austria), null.par = theta_benford(2))

rbind(
  test.point.null(x = msdigit(Portugal), null.par = theta_benford(1)),
  test.point.null(x = smsdigit(Portugal), null.par = theta_benford(2))
)



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

mu_dir_estimate(Portugal)
mu_dir_estimate(Portugal, stat = "mode")
mu_dir_estimate(Portugal, stat = "mean")
mu_dir_estimate(Portugal, hyper.par = list(rep(1/9, 9), rep(1/10, 10)))
mu_dir_estimate(Portugal, hyper.par = list(theta_benford(1), theta_benford(2)))
mu_dir_estimate(Portugal, hyper.par = list(22*theta_benford(1), 12 * theta_benford(2)))






