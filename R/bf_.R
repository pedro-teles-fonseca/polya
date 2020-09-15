
#' @export

bf_binomial <- function(
  # falta stop para o caso de na Haldane prior não se observarem todas 
  # as categorias
  data,
  success,
  null_par = 0.5,
  hyper_par = c(1, 1),
  haldane = FALSE,
  in_favour = "H0") {

  if (isFALSE(length(success) == 1)) {
    stop("Invalid argument: `success` must be a vector of length 1'.")
  }
  if (isFALSE(length(null_par) == 1) || isFALSE(is.numeric(null_par))) {
    "Invalid argument: `null_par` must be a numeric vector of length 1."
  }
  if (null_par >= 1 || null_par <= 0) {
    "Invalid argument: `null_par` must be in the (0, 1) interval."
  }
  if (isFALSE(length(hyper_par) == 2) || isFALSE(is.numeric(hyper_par))) {
    "Invalid argument: `hyper_par` must be a numeric vector of length 2. "
  }
  if (any(hyper_par <= 0)) {
    "Invalid argument: all elements of `hyper_par` must be greater than zero. "
  }
  if (isFALSE(haldane %in% c(TRUE, FALSE))) {
    stop("Invalid argument: `haldane` must be either `TRUE` or `FALSE`.")
  }

  data <- data[!is.na(data)]
  n <- length(data)

  if (success %in% unique(data)) {
    s <- as.numeric(table(data == success)["TRUE"])
  } else{
    warning("Level corresponding to `success` not observed in `data`.")
    s <- 0
  }

  a <- hyper_par[1]
  b <- hyper_par[2]

  if (haldane) {
    bf <- exp(s * log(null_par) + (n - s) * log(1 - null_par) - lbeta(s, n - s))
  } else {
    bf <- exp(
      s * log(null_par) +
      (n - s) * log(1 - null_par) + lbeta(a, b) -
      lbeta(a + s, b + n - s)
      )
  }

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: `in_favour` must be either 'H0' or 'H1'."))
}

#' @export

bf_multinomial <- function(
  data,
  categories = sort(unique(data)),
  null_par = rep(1 / length(categories), length(categories)),
  hyper_par = rep(1, length(categories)),
  haldane = FALSE,
  in_favour = "H0") {

  if(any(null_par >= 1) || any(null_par <= 0)) {
    "Invalid argument: all elements of `null_par` must be in the (0, 1) interval."
  }
  if(isFALSE(dplyr::near(sum(null_par), 1))) {
    "Invalid argument: `null_par` must sum to 1."
  }
  if (isFALSE(is.numeric(hyper_par))) {
    "Invalid argument: `hyper_par` must be a numeric vector. "
  }
  if (any(hyper_par <= 0)) {
    "Invalid argument: all elements of `hyper_par` must be greater than zero. "
  }
  if (isFALSE(haldane %in% c(TRUE, FALSE))) {
    stop("Invalid argument: `haldane` must be either `TRUE` or `FALSE`.")
  }

  categories <- factor(categories, levels = categories)
  counts <- table(factor(data[!is.na(data)], levels = categories))

  if (haldane && any(counts == 0)) {
    stop("At least one observation of each `category` is needed
          in order to compute Bayes factors using the Haldane prior.")
  }

  if (haldane) {
    bf <- exp(sum(counts * log(null_par))  - lmbeta(counts))
  } else {
    bf <- exp(
      sum(counts * log(null_par)) +
          lmbeta(hyper_par) - lmbeta(hyper_par + counts)
      )
  }

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: `in_favour` must be either 'H0' or 'H1'."))
}