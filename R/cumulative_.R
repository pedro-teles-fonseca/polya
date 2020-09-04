
cumulative_bf_multinomial <- function(
  samples,
  categories,
  null_par = 1/length(categories),
  prior = "dirichlet",
  hyper_par = 1,
  in_favour = "H0") {

  sapply(
    X = samples,
    FUN = bf_multinomial,
    categories = categories,
    null_par = null_par,
    prior = prior,
    hyper_par = hyper_par,
    in_favour = in_favour
  )

}

cumulative_fbf_multinomial <- function(
  samples,
  categories,
  null_par = 1/length(categories),
  hyper_par,
  frac = 0.1,
  m = length(categories),
  robust = NULL,
  in_favour = "H0"){

  sapply(
    X = samples,
    FUN = fbf_multinomial,
    categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    frac = frac,
    m = m,
    robust = robust,
    in_favour = in_favour)

}

cumulative_ibf_multnomial <- function(
  samples,
  categories,
  null_par,
  prior = "haldane",
  hyper_par = 1,
  type = "arithmetic",
  method = "smts",
  k = 2,
  in_favour = "H1"){

  sapply(
    X = samples,
    FUN = ibf,
    categories = categories,
    null_par = null_par,
    prior =  prior,
    hyper_par = hyper_par,
    type = type,
    method = method,
    k = k,
    in_favour = in_favour)
}





