
bf_binomial <- function(
  x,
  success,
  null_par = 0.5,
  prior = "beta",
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

  if (prior == "haldane") {
    bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) - lbeta(s, n - s))
  } else {
    bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a, b) - lbeta(a + s, b + n - s))
  }

  switch(
    tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1/bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use 'Null' or 'Alternative'."))
}

bf_multinomial <- function(
  x,
  categories = sort(unique(x)),
  null_par = 1 / length(categories),
  prior = "dirichlet",
  hyper_par = rep(1, length(categories)),
  in_favour = "H0") {

  categories <- factor(categories, levels = categories)
  counts <- table(factor(x[!is.na(x)], levels = categories))

  if (prior == "haldane") {
    bf <- exp(sum(counts * log(null_par))  - lmbeta(counts))
  } else {
    bf <- exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts))
  }

  switch(
    tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1/bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use 'Null' or 'Alternative'."))
}







