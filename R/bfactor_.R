
#' Bayes Factors for Binomial point null hypotheses
#'
#'
#' Computes Bayes factors in favour of a point null hypothesis in the context of a Binomial statistical model with a Beta prior.
#' @param x Atomic vector of the type integer, double or character.
#' @param success Number or character indicating which level of \code{x} corresponds to a success.
#' @param null_par The parameter of the Binomial distribution under the null hipothesis.
#' @param a, b Shape parameters of the Beta prior. Both must be greater than zero

bfactor_binomial <- function(
  x,
  success,
  null_par = 0.5,
  prior = "beta",
  hyper_par = c(1, 1),
  in_favour = "H0") {

  check_args_binomial(x, success, null_par, prior, hyper_par, in_favour)

  x <- x[!is.na(x)]
  n <- length(x)

  if(any_success(x, success)){
    s <- as.numeric(table(x == success)["TRUE"])
    } else{
    s <- 0
  }

  a <- hyper_par[1]
  b <- hyper_par[2]

  if(prior == "haldane"){
    bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) - lbeta(s, n - s))
  } else {
    bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a, b) - lbeta(a + s, b + n - s))
  }

  if(tolower(in_favour) %in% c("h1", "alternative")){
    1/bf
  } else{
    bf
  }
}

bfactor_multinomial <- function(
  x,
  categories = sort(unique(x)),
  null_par = 1 / length(categories),
  prior = "dirichlet",
  hyper_par = rep(1, length(categories)),
  in_favour = "H0") {

  check_args_multinomial(x, categories, null_par, prior, hyper_par, in_favour)

  categories <- factor(categories, levels = categories)
  counts <- table(factor(x[!is.na(x)], levels = categories))

  if(prior == "haldane"){
    bf <- exp(sum(counts * log(null_par))  - lmbeta(counts))
  } else {
    bf <- exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts))
  }

  if(tolower(in_favour) %in% c("h1", "alternative")){
    1/bf
  } else{
    bf
  }
}







