

bf_binomial <- function(x,
                        success,
                        null_par = 0.5,
                        prior = "beta",
                        hyper_par = c(1, 1),
                        in_favour = "H0") {

  x <- x[!is.na(x)]
  n <- length(x)

  if (any_success(x, success)) {
    s <- as.numeric(table(x == success)["TRUE"])
  } else{
    s <- 0
  }

  a <- hyper_par[1]
  b <- hyper_par[2]

  if (prior == "haldane") {
    bf <-
      exp(s * log(null_par) + (n - s) * log(1 - null_par) - lbeta(s, n - s))
  } else {
    bf <-
      exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a, b) - lbeta(a + s, b + n - s))
  }

  if (tolower(in_favour) %in% c("h1", "alternative")) {
    1 / bf
  } else{
    bf
  }
}

bf_multinomial <- function(x,
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
    bf <-
      exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts))
  }

  if (tolower(in_favour) %in% c("h1", "alternative")) {
    1 / bf
  } else{
    bf
  }
}







