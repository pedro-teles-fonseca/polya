
bfactor_fractional <- function(
  x,
  categories,
  null_par,
  hyper_par = 1,
  transf = "level",
  b = ) {

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- prod(null_par ^ x) * lbeta_multi(hyper_par + b * x) / lbeta_multi(hyper_par + x)

  if(transf == "log"){
    log(bfactor)
  } else if(transf == "log10"){
    log10(bfactor)
  } else {
    bfactor
  }
}


