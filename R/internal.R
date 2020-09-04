
na_rm <- function(x) {
  x[!is.na(x)]
}

getmode <- function(v) {
  uniqv <- unique(v)

  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mantissa <- function(x) {

  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

msdigit <- function(x) {

  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

smsdigit <- function(x) {

  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

theta_benford <- function(d) {

  if (d == 1){
    log10(1 + 1/1:9)
  } else if (d == 2){
    sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
  } else {
    stop("Invalid argument: 'd' must be either 1 or 2.")
  }
}

any_success <- function(x, success){

  if(isFALSE(success %in% unique(x))){
    FALSE
  } else{
    TRUE
  }
}

check_args_binomial <- function(
  x,
  success,
  null_par,
  prior,
  hyper_par,
  in_favour){

  inspector::inspect_data_cat_as_dichotomous(x, success)
  inspector::inspect_par_bernoulli(null_par)
  inspector::inspect_character_match(prior, c("Haldane", "haldane", "Beta", "beta"))
  inspector::inspect_par_beta(hyper_par)
  inspector::inspect_character_match(in_favour, c("H0", "H1", "h0", "h1", "null", "alternative"))
}

check_args_multinomial <- function(
  x,
  categories,
  null_par,
  prior,
  hyper_par,
  in_favour){

  inspector::inspect_data_categorical(x)
  inspector::inspect_categories(categories)
  inspector::inspect_par_multinomial(null_par)
  inspector::inspect_character_match(prior, c("Haldane", "haldane", "Dirichlet", "dirichlet"))
  inspector::inspect_character_match(in_favour, c("H0", "H1", "h0", "h1", "null", "alternative"))

  if(tolower(prior) == "dirichlet"){
    inspector::inspect_par_dirichlet(hyper_par)
    }
  if (length(null_par) != length(hyper_par)) {
    stop("null_par and hyper_par must have the same length.")
    }
}















