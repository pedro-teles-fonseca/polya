
fbf_binomial <- function(
  x,
  success,
  null_par = 0.5,
  hyper_par = c(1, 1),
  frac = 0.1,
  robust = "no",
  in_favour = "H0") {

  x <- x[!is.na(x)]
  n <- length(x)

  if (success %in% unique(x)) {
    s <- as.numeric(table(x == success)["TRUE"])
  } else{
    warning("Level corresponding to 'success' not observed in the data 'x'.")
    s <- 0
  }

  if(isFALSE(tolower(robust) == "no")) {
    if (tolower(robust) == "minimal") {
      frac <- 2 / n
    } else if (tolower(robust) == "intermediate") {
      frac <- (2 * log(n)) / (n * log(2))
    } else if (tolower(robust) == "yes") {
      frac <- sqrt(2 / n)
    } else {
      stop("Invalid argument: Admissible values of 'robust': 'no', 'minimal', 'intermediate' or 'yes'.")
    }
  }

  a <- hyper_par[1]
  b <- hyper_par[2]

  bf <- exp(s * (1 - frac) * log(null_par) + (n - s) * (1 - frac) * log(1 - null_par) + lmbeta(c(a + s * frac, b + (n - s) * frac)) - lmbeta(c(a + s, b + n - s)))

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use 'Null' or 'Alternative'."))
}

fbf_multinomial <- function(
  x,
  categories = sort(unique(x)),
  null_par = 1 / length(categories),
  hyper_par = rep(1, length(categories)),
  frac = 0.1,
  m = length(categories),
  robust = "no",
  in_favour = "H0") {

  x <- x[!is.na(x)]
  n <- length(x)

  categories <- factor(categories, levels = categories)
  counts <- table(factor(x, levels = categories))

  if(isFALSE(tolower(robust) == "no")) {
    if (tolower(robust) == "minimal") {
      frac <- m / n
    } else if (tolower(robust) == "intermediate") {
      frac <- (m * log(n)) / (n * log(m))
    } else if (tolower(robust) == "yes") {
      frac <- sqrt(m / n)
    } else {
      stop("Invalid argument: Admissible values of 'robust': 'no', 'minimal', 'intermediate' or 'yes'.")
    }
  }

  bf <- exp((1 - frac) * sum(counts * log(null_par)) + lmbeta(hyper_par + frac * counts) - lmbeta(hyper_par + counts))

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use 'Null' or 'Alternative'."))
}



