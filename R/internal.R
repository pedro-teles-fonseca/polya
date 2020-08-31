
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















