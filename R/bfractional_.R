
bfractional_binomial <- function(
  x,
  success,
  null_par,
  hyper_par = c(1, 1),
  frac = 2/length(x[!is.na(x)]),
  in_favour = "H0") {

  if(isFALSE(success %in% unique(x))){
    warning("Level corresponding to 'success' not observed in the data 'x'.")
    sucess_is_observed <- FALSE
  } else{
    sucess_is_observed <- TRUE
  }

  if(length(hyper_par == 1)){hyper_par <- rep.int(hyper_par, 2)}

  x <- x[!is.na(x)]
  n <- length(x)

  if(sucess_is_observed){
    s <- as.numeric(table(x == success)["TRUE"])
    } else{
    s <- 0
    }

  a <- hyper_par[1]
  b <- hyper_par[2]

  bf <- exp(s * (1 - frac) * log(null_par) + (n - s) * (1 - frac) * log(1 - null_par) + lmbeta(c(a + s * frac, b + (n - s) * frac)) - lmbeta(c(a + s, b + n - s)))

  if(tolower(in_favour) %in% c("null", "h0")){
    bf
  } else {
    1/bf
  }
}

bfractional_multinomial <- function(
  x,
  categories,
  null_par,
  hyper_par = 1,
  frac = length(categories)/length(x[!is.na(x)]),
  in_favour = "H0") {

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- exp((1 - frac) * sum(counts * log(null_par)) + lmbeta(hyper_par + frac * counts) - lmbeta(hyper_par + counts))

  if(tolower(in_favour) %in% c("null", "h0")){
    bfactor
  } else {
    1/bfactor
  }
}






