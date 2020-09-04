
pbf_binomial <- function(
  x,
  success,
  null_par = 0.5,
  hyper_par = c(1, 1),
  in_favour = "H0") {

  x <- x[!is.na(x)]
  n <- length(x)

  if (success %in% unique(x)) {
    s <- as.numeric(table(x == success)["TRUE"])
  } else{
    warning("Level corresponding to 'success' not observed in the data 'x'.")
    s <- 0
  }

  a <- hyper_par[1]
  b <- hyper_par[2]

  bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a + s, b + n - s) - lbeta(a + 2 * s, b + 2 * (n - s)))

  if (tolower(in_favour) %in% c("null", "h0")) {
    bf
  } else {
    1 / bf
  }
}

pbf_multinomial <- function(
  x,
  categories,
  null_par = 1 / length(categories),
  hyper_par = rep(1, length(categories)),
  in_favour = "H0"){

  x <- x[!is.na(x)]

  categories <- factor(categories, levels = categories)
  counts <- table(factor(x, levels = categories))

  bf <- exp(sum(counts * log(null_par)) + lmbeta(hyper_par + counts) - lmbeta(hyper_par + 2 * counts))

  if (tolower(in_favour) %in% c("null", "h0")) {
    bf
  } else {
    1 / bf
  }
}








