
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
    pcal::bfactor_interpret(bf),
    round(pcal::bfactor_to_prob(bf), probs_round),
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
    pcal::bfactor_interpret(bf),
    round(pcal::bfactor_to_prob(bf), probs_round),
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
test.binomial.hypotheses <- function(
  x,
  success,
  null_par,
  hyper_par = rep.int(1, length(null_par)),
  pi_null = .5,
  transf = "level",
  bf_round = 2,
  probs_round = 3){

  bfs <- mapply(FUN = bfactor_binomial, success, null_par, hyper_par, MoreArgs = list(x = x))
  bfs_transf <- if(transf == "log"){log(bfs)} else if (transf == "log10") {log10(bfs)} else {bfs}
  evidence <- sapply(FUN = pcal::bfactor_interpret, X =  bfs)
  pp <- mapply(FUN = pcal::bfactor_to_prob, pi_null = pi_null, bf = bfs)

  estimate <- vector(mode = "double", length = length(null_par))
  pvalue <- vector(mode = "double", length = length(null_par))

  for(par in seq_along(null_par)){
    tst <- nigrini_z_test(x, success[par], null_par[par])
    estimate[par] <- tst[["estimate"]]
    pvalue[par] <- tst[["p.value"]]
  }


  result <- data.frame(
    "BayesFactors" = round(bfs_transf, bf_round),
    "Evidence" = evidence,
    "PostProbH0" = round(pp, probs_round),
    "ParEstimate" = round(estimate, probs_round),
    "P.value" = round(pvalue, probs_round)
  )

  if(log10 == TRUE){names(result) <- gsub("BayesFactors", "log10(BayesFactors)", names(result))}
  result

  }

#test.binomial.hypotheses(austria_bl1, 1:9, theta_benford(1), transf = "log10")

