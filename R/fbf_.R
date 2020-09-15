
#' @export

fbf_binomial <- function(
  # falta garantir que no caso da Haldane prior frac não pode ser zero
  # falta stop para o caso de na Haldane prior não se observarem todas 
  # as categorias

  data,
  success,
  null_par,
  hyper_par = c(1, 1),
  haldane = FALSE,
  frac = 0.1,
  robust = "no",
  in_favour = "H0") {

  data <- data[!is.na(data)]
  n <- length(data)

  if (success %in% unique(data)) {
    s <- as.numeric(table(data == success)["TRUE"])
  } else{
    warning("Level corresponding to 'success' not observed in `data`.")
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
      stop("Invalid argument: Admissible values of 'robust': 'no', 'minimal',
            'intermediate' or 'yes'.")
    }
  }

  if (haldane) {
      a <- 0
      b <- 0
  } else {
      a <- hyper_par[1]
      b <- hyper_par[2]
  }

  bf <- exp(
    s * (1 - frac) * log(null_par) +
    (n - s) * (1 - frac) * log(1 - null_par) +
    lmbeta(c(a + s * frac, b + (n - s) * frac)) -
    lmbeta(c(a + s, b + n - s))
    )

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. "))
}

#' @export

fbf_multinomial <- function( 
  # falta garantir que no caso da Haldane prior frac não pode ser zero

  data,
  categories = sort(unique(data)),
  null_par = 1 / length(categories),
  hyper_par = rep(1, length(categories)),
  haldane = FALSE,
  frac = 0.1,
  m = length(categories),
  robust = "no",
  in_favour = "H0") {

  data <- data[!is.na(data)]
  n <- length(data)

  categories <- factor(categories, levels = categories)
  counts <- table(factor(data, levels = categories))

  if (haldane && any(counts == 0)) {
    stop("At least one observation of each `category` is needed
          in order to compute Bayes factors using the Haldane prior.")
  }

  if(isFALSE(tolower(robust) == "no")) {
    if (tolower(robust) == "minimal") {
      frac <- m / n
    } else if (tolower(robust) == "intermediate") {
      frac <- (m * log(n)) / (n * log(m))
    } else if (tolower(robust) == "yes") {
      frac <- sqrt(m / n)
    } else {
      stop("Invalid argument: Admissible values of 'robust': 'no', 'minimal',
            'intermediate' or 'yes'.")
    }
  }

  if (haldane) {
    hyper_par  <-  0
  }

  bf <- exp(
          (1 - frac) * sum(counts * log(null_par)) +
          lmbeta(hyper_par + frac * counts) -
          lmbeta(hyper_par + counts)
        )

  switch(tolower(in_favour),
    "h0" = , "null" = bf,
    "h1" = , "alternative" = 1 / bf,
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'."))
}