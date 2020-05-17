
#' Bayes Factors, Strength of Evidence, Posterior probabilities and Chi-Squared test for Binomial Proportions

#' @examples
#'
#' test.null.binomial(austria_bl1, success = 1, null_par = theta_benford(1)[1])

test.null.binomial <- function(
  x,
  success,
  null_par,
  hyper.par = c(1, 1),
  pi_null = 0.5,
  transf = "level",
  bf_round = 2,
  probs_round = 3){

  bf <- unname(bfactor_binomial(x, success, null_par, hyper.par))
  bf_transf <- if(transf == "log"){log(bf)} else if (transf == "log10") {log10(bf)} else {bf}
  p <- nigrini_z_test(x, success, null_par)[["p.value"]]

  results <- data.frame(
    deparse(substitute(x)),
    round(bf_transf, bf_round),
    bfactor_interpret(bf),
    round(bfactor_to_prob(bf), probs_round),
    row.names = NULL
  )

  names(results) <- c(
    "Data",
    "BF" = if(isTRUE(transf == "log")){"log(BF)"} else if(isTRUE(transf == "log10")){"log10(BF)"} else {"BF"},
    "Evidence",
    "P(H0|X)"
  )

  results
}

#' @examples
#'
#' test.null.multinomial(austria_bl1, null_par = theta_benford(1), categories = 1:9, hyper_par=1, transf = "log10")

test.null.multinomial <- function(
  x,
  categories,
  null_par,
  hyper_par = 1,
  pi_null = 0.5,
  transf = "level",
  bf_round = 2,
  probs_round = 3){

  bf <- bfactor_multinomial(x, categories, null_par, hyper_par)
  bf_transf <- if(transf == "log"){log(bf)} else if (transf == "log10") {log10(bf)} else {bf}
  chisq <- chisq_test_multinomial(x, categories, null_par)

  results <- data.frame(
    deparse(substitute(x)),
    round(bf_transf, bf_round),
    bfactor_interpret(bf),
    round(bfactor_to_prob(bf), probs_round),
    row.names = NULL,
    round(chisq[["p.value"]], probs_round)
  )

  names(results) <- c(
    "Data",
    "BF" = if(isTRUE(transf == "log")){"log(BF)"} else if(isTRUE(transf == "log10")){"log10(BF)"} else {"BF"},
    "Evidence",
    "P(H0|X)",
    "p_value"
  )
  results
}



# Testing many binomial point null hypotheses
# ------------------------------------------
test.binomial.hypotheses <- function( # Function to compute Binomial-Beta model results (BL1 or BL2)
  data,
  null_par,
  success = unique(data),
  pi_null = .5,
  a = 1,
  b = 1,
  log10 = TRUE,
  bf_round = 2,
  probs_round = 3){

  data <- data[!is.na(data)]
  x <- as.numeric(table(data))
  n <- length(data)

    obs_prop <- as.numeric(table(data)[1])/n
    z_stats <- (abs(obs_prop-null_par)-(1/(2*n)))/sqrt((null_par*(1-null_par))/n)
    p_values <- 2 * (1-pnorm(abs(z_stats), 0, 1))

    digits <- 1:9

    bf <- mapply(FUN = bfactor_binomial, null_par = null_par, success = success,  a = a, b = b,
      MoreArgs = list(data = data))
    pp <- mapply(FUN = bfactor_to_prob, pi_null = pi_null, bf = bf)

  result <- data.frame(
    "Digit" = digits,
    "BayesFactors" = round(if(log10 == FALSE){bf} else{log10(bf)}, bf_round),
    "Evidence" = ifelse(bf < 1, "Negative",
      ifelse(bf < 3.2, "Weak",
        ifelse(bf < 10, "Substantial",
          ifelse(bf < 100, "Strong",
            "Decisive")))
    ),
    "PostProb.H0" = round(pp, probs_round),
    "P.value" = p_values
  )

  if(log10 == TRUE){names(result) <- gsub("BayesFactors", "log10(BayesFactors)", names(result))}
  return(result)

  }


# test.binomial.hypotheses(Austria.bl1, theta_benford(1))

