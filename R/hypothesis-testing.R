
# Testing Multinomial and Binomial point null hypotheses
# ------------------------------------------
test.point.null <- function(
  data,
  null.par,
  pi_null = 0.5,
  model = "multinomial",
  hyper.par = if(model == "multinomial") {rep.int(1, length(null.par))} else {c(1, 1)},
  log.10 = TRUE,
  bf.round = 2,
  probs.round = 3){

  bf <- bayes.factor(data = data, null.par = null.par, model = model, hyper.par = hyper.par)

  # data <- as.vector(data[!is.na(data)])

  if(model == "multinomial"){
    p_value <- chisq.test(table(data), p = null.par)$p.value
  } else if (model == "binomial"){
    n <- length(data)
    obs_prop <- as.numeric(rev(table(data))[1])/n
    z_stat <- (abs(obs_prop-null.par)-(1/(2*n)))/sqrt((null.par*(1-null.par))/n)
    p_value <- 2* (1-pnorm(abs(z_stat), 0, 1))
  }

  results <- data.frame(
    "sample" = deparse(substitute(data)),
    "BayesFactor" = if(isTRUE(log.10)){round(log10(bf), bf.round)} else {round(bf, bf.round)},
    "Evidence" = ifelse(bf < 1, "Negative",
      ifelse(bf < 3.2, "Weak",
        ifelse(bf < 10, "Substantial",
          ifelse(bf < 100, "Strong", "Decisive")))),
    "P(H0|X)" = round(((1 + ((1 - pi_null) / pi_null) * (1 / bf)))^(-1), probs.round),
    "LB.P(H0|X)" = round(pcal(p_value), probs.round)
  )

  if(isTRUE(log.10)){names(results) <- gsub("BayesFactor(X)", "log10(BayesFactor(x))", names(results))}

  results
}

# Testing many binomial point null hypotheses
# ------------------------------------------
test.binomial.hypotheses <- function( # Function to compute Binomial-Beta model results (BL1 or BL2)
  data,
  null.par,
  pi_null = rep.int(0.5, length(null.par)),
  hyper.par = list(rep(1, length(null.par)), rep(1, length(null.par))),
  log10 = TRUE,
  bf.round = 2,
  probs.round = 3){

    x <- as.numeric(table(msdigit(data)))
    n <- length(msdigit(data))
    obs_prop <- as.numeric(rev(table(data))[1])/n
    z_stats <- (abs(obs_prop-null.par)-(1/(2*n)))/sqrt((null.par*(1-null.par))/n)
    p_values <- 2 * (1-pnorm(abs(z_stats), 0, 1))

    digits <- 1:9

    bf <- function(x, theta_null, a, b){
      exp(
        x * log(theta_null) + (n - x) * log(1-theta_null) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
          (lgamma(a + b) + lgamma(n + a - x) + lgamma(x + a))
      )
    }

    bf <- mapply(bf, x, null.par, hyper.par[[1]], hyper.par[[2]])


  PostProb.H0 <- mapply(FUN = posterior.prob, pi_null = pi_null, bf = bf)

  result <- data.frame(
    "Digit" = digits,
    "BayesFactors" = round(if(log10 == FALSE){bf} else{log10(bf)}, bf.round),
    "Evidence" = ifelse(bf < 1, "Negative",
      ifelse(bf < 3.2, "Weak",
        ifelse(bf < 10, "Substantial",
          ifelse(bf < 100, "Strong",
            "Decisive")))
    ),
    "PostProb.H0" = round(PostProb.H0, probs.round),
    "LB.PostProb.H0" = round(pcal(p_values), probs.round),
    "P.value" = p_values
  )

  if(log10 == TRUE){names(result) <- gsub("BayesFactors", "log10(BayesFactors)", names(result))}
  return(result)

}

# test.binomial.hypotheses(Portugal.bl1, theta_benford(1))
# bayes.factor(Portugal.bl1, theta_benford, model = "binomial")







