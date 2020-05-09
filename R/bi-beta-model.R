
rm(list = ls())

source("read-data.R")
source("auxiliary-functions.R")

# Binomial-Beta model - model selection

bi_beta_model <- function( # Function to compute Binomial-Beta model results (BL1 or BL2)
  data, 
  a = rep(1, 9),
  b = rep(1, 9),
  log10 = TRUE,
  pi_0 = 0.5, 
  bl = 1, 
  bf.round = 2, 
  probs.round = 3){ 
  
  if(bl == 1){
    theta_null <- theta_benford(1)
    x <- as.numeric(table(msdigit(data)))
    n <- length(!is.na(msdigit(data)))
    digits <- 1:9
    p_values <- round(bl1_analysis(data)$z_test$results$p_value, probs.round)
  } else {
    theta_null <- theta_benford(2)
    x <- as.numeric(table(smsdigit(data)))
    n <- length(!is.na(smsdigit(data)))
    digits <- 0:9
    p_values <- round(bl2_analysis(data)$z_test$results$p_value, probs.round)
  }
  
  bf <- function(x, theta_null, a, b){
    exp(
      x * log(theta_null) + (n - x) * log(1-theta_null) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
        (lgamma(a + b) + lgamma(n + a - x) + lgamma(x + a))
    )
  }
  
  bfs <- mapply(bf, x, theta_null, a, b)
  
  result <- data.frame(
    "Digit" = digits,
    "BayesFactors" = round(if(log10 == FALSE){bfs} else{log10(bfs)}, bf.round),
    "Evidence" = ifelse(bfs < 1, "Negative",
                        ifelse(bfs < 3.2, "Weak",
                               ifelse(bfs < 10, "Substantial",
                                      ifelse(bfs < 100, "Strong",
                                             "Decisive")))
    ),
    "PostProb.H0" = round(((1 + ((1 - pi_0) / pi_0) * (1 / bfs)))^(-1), probs.round),
    "LB.PostProb.H0" = round(pcal(p_values), probs.round),
    "P.value" = p_values
  )
  
  if(log10 == TRUE){names(result) <- gsub("BayesFactors", "log10(BayesFactors)", names(result))}
  return(result)
  
}

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

bin_beta_estimate(Portugal,
                  stat = "mean",
                  a = list(rep(1, 9), rep(1, 10)),
                  b = list(rep(8, 9), rep(9, 10)),
                  cred = 0.05)

                  
                  

