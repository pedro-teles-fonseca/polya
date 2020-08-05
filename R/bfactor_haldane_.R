
bfactor_haldane_binomial <- function(
  x,
  success,
  null_par,
  in_favour = "H0") {

  x <- x[!is.na(x)]
  n <- length(x)
  s <- as.numeric(table(x == success)["TRUE"])

  bfactor <- exp(s * log(null_par) + (n - s) * log(1 - null_par) + lgamma(n) - lgamma(s) - lgamma(n-s))

  if(tolower(in_favour) %in% c("null", "h0")){
    bfactor
  } else {
    1/bfactor
  }
}

bfactor_haldane_multinomial <- function(
  x,
  categories,
  null_par,
  hyper_par = 1,
  in_favour = "H0") {

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts))

  if(tolower(in_favour) %in% c("null", "h0")){
    bfactor
  } else {
    1/bfactor
  }
}




