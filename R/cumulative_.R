
cumulative_BF_multinomial <- function(
  samples,
  categories,
  null_par,
  hyper_par = 1,
  in_favour = "H0") {

  sapply(
    X = samples,
    FUN = BF_multinomial,
    categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    in_favour = in_favour
  )

}

cumulative_FBF_multinomial <- function(
  samples,
  categories,
  null_par,
  hyper_par,
  b = 0.1,
  m = length(categories),
  robust = NULL,
  in_favour = "H0"){

  sapply(
    X = samples,
    FUN = FBF_multinomial, categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    b = b,
    m = m,
    robust = robust,
    in_favour = in_favour)

}









