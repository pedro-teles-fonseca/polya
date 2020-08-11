
cumulative_bfactor_multinomial <- function(
  samples,
  categories,
  null_par = 1/length(categories),
  prior = "dirichlet",
  hyper_par = 1,
  in_favour = "H0") {

  sapply(
    X = samples,
    FUN = bfactor_multinomial,
    categories = categories,
    null_par = null_par,
    prior = prior,
    hyper_par = hyper_par,
    in_favour = in_favour
  )

}

cumulative_Fbfactor_multinomial <- function(
  samples,
  categories,
  null_par = 1/length(categories),
  hyper_par,
  b = 0.1,
  m = length(categories),
  robust = NULL,
  in_favour = "H0"){

  sapply(
    X = samples,
    FUN = Fbfactor_multinomial, categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    b = b,
    m = m,
    robust = robust,
    in_favour = in_favour)

}









