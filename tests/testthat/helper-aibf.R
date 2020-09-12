
aibf_multinomial_mts <- function(x, categories, null_par) {

  x <- x[!is.na(x)]
  n <- length(x)
  k <- length(categories)
  counts <- table(x)

  exp(lgamma(k) - lgamma(n) + sum(lgamma(counts)) + sum((1-counts) * log(null_par)))

}

aibf_multinomial_mts_2 <- function(x, categories, null_par) {

  x <- x[!is.na(x)]
  n <- length(x)
  k <- length(categories)
  counts <- table(x)

  exp(sum((1-counts) * log(null_par)) + lmbeta(counts) - lmbeta(rep(1, k)))

}






