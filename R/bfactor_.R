
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
  hyper_par = 1,
  in_favour = "H0") {

  check_args_binomial(x, success, null_par, prior, hyper_par, in_favour)

  x <- x[!is.na(x)]
  n <- length(x)

  if(any_success(x, success)){
    s <- as.numeric(table(x == success)["TRUE"])
    } else{
    s <- 0
  }

  if(length(hyper_par == 1)){
    hyper_par <- rep.int(hyper_par, 2)
    }

  a <- hyper_par[1]
  b <- hyper_par[2]

  bf <- switch(
    tolower(prior),
    "beta" = exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a, b) - lbeta(a + s, b + n - s)),
    "haldane" = exp(s * log(null_par) + (n - s) * log(1 - null_par) - lbeta(s, n - s))
  )

  switch(
    tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1/bf)
}

bfactor_multinomial <- function(
  x,
  categories,
  null_par = 1 / length(categories),
  prior = "dirichlet",
  hyper_par = 1,
  in_favour = "H0") {

  check_args_multinomial(
    x,
    categories,
    null_par,
    prior,
    hyper_par,
    in_favour)

  if (length(null_par) < length(hyper_par)) {
    stop("Invalid model specification: more hyper-parameters than null parameter values.")
  } else if (length(hyper_par) == 1){
    hyper_par <- rep.int(hyper_par, length(null_par))
  } else if(length(null_par) > length(hyper_par)) {
    stop("Invalid specification: more null parameter values than hyper parameters.")
  }

  categories <- factor(categories, levels = categories)
  counts <- table(factor(x[!is.na(x)], levels = categories))

  bf <- switch(
    prior,
    "dirichlet" = exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts)),
    "haldane" = exp(sum(counts * log(null_par))  - lmbeta(counts))
    )

  switch(
    tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1/bf)
}







