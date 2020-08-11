
check_binomial_data <- function(x, success){

  if(any(!is.atomic(x), !is.vector(x), is.null(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop("Invalid argument: 'x' must be of type 'integer', 'double' or 'character'.")
  }
  if(any(is.na(success), is.null(success))){
    stop("Invalid argument: 'success'  cant be missing nor null")
  }
  if(isFALSE(length(success) == 1)){
    stop("Invalid argument: 'success' must be of length 1")
  }
  if(isFALSE(success %in% unique(x))){
    warning("Level corresponding to 'success' not observed in the data 'x'.")
  }
}

any_success <- function(x, success){

  if(isFALSE(success %in% unique(x))){
    FALSE
  } else{
    TRUE
  }
}

check_binomial_null_par <- function(null_par){

  if(isFALSE(length(null_par) == 1)){
    stop("Invalid argument: 'null_par' must be of length 1")
  }
  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }
}

check_beta_prior <- function(prior){

  if(is.null(prior)){
    stop("Invalid argument: 'prior' is NULL.")
  }
  if(any(is.na(prior), is.nan(prior))){
    stop("Invalid argument: 'prior' is NA or NaN.")
  }
  if(any(!is.atomic(prior), !is.vector(prior), !is.character(prior))){
    stop("Invalid argument: 'prior' must be an atomic vector of type character.")
  }
  if(isFALSE(length(prior) == 1)){
    stop("Invalid argument: 'prior' must be of length 1.")
  }
  if(isFALSE(tolower(prior) %in% c("beta", "haldane"))){
    stop("Invalid argument: 'prior' must be either 'beta' or 'haldane'")
  }
}

check_beta_hyper_par <- function(hyper_par){

  if(any(hyper_par <= 0, is.na(hyper_par), is.null(hyper_par))){
    stop("Invalid argument: elements of 'hyper_par' must be greather than 0.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(isFALSE(length(hyper_par) %in% c(1, 2)))){
    stop("Invalid argument: 'hyper_par' must be of length 1 or 2.")
  }
}

check_in_favour <- function(in_favour){

  if(is.null(in_favour)){
    stop("Invalid argument: 'in_favour' is NULL.")
  }
  if(any(is.na(in_favour), is.nan(in_favour))){
    stop("Invalid argument: 'in_favour' is NA or NaN.")
  }
  if(any(!is.atomic(in_favour), !is.vector(in_favour), !is.character(in_favour))){
    stop("Invalid argument: 'in_favour' must be an atomic vector of type character.")
  }
  if(isFALSE(length(in_favour) == 1)){
    stop("Invalid argument: 'in_favour' must be of length 1.")
  }
  if(isFALSE(tolower(in_favour) %in% c("h0", "h1", "null", "alternative"))){
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use either 'null' or 'alternative'.")
  }

}

check_multinomial_data <- function(x){

  if(any(!is.atomic(x), !is.vector(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }

}

check_multinomial_null_par <- function(null_par){

  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }
  if(isFALSE(all.equal(sum(null_par), 1))) {
    stop("Invalid argument: sum(null_par) must be 1.")
  }
}

check_categories <- function(categories){

  if(any(is.na(categories), is.null(categories))){
    stop("Invalid argument: 'categories' must be a vector of the same lenght as 'x'")
  }
  if(isFALSE(typeof(categories) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(categories) must be 'integer', 'double' or 'character'.")
  }
}

check_dirichlet_prior <- function(prior){

  if(is.null(prior)){
    stop("Invalid argument: 'prior' is NULL.")
  }
  if(any(is.na(prior), is.nan(prior))){
    stop("Invalid argument: 'prior' is NA or NaN.")
  }
  if(any(!is.atomic(prior), !is.vector(prior), !is.character(prior))){
    stop("Invalid argument: 'prior' must be an atomic vector of type character.")
  }
  if(isFALSE(length(prior) == 1)){
    stop("Invalid argument: 'prior' must be of length 1.")
  }
  if(isFALSE(tolower(prior) %in% c("dirichlet", "haldane"))){
    stop("Invalid argument: 'prior' must be either 'dirichlet' or 'haldane'")
  }
}

check_dirichlet_hyper_par <- function(hyper_par, prior){

  if(is.null(hyper_par)){
    stop("Invalid argument: 'hyper_par' is NULL.")
  }
  if(any(is.na(hyper_par), is.nan(hyper_par))){
    stop("Invalid argument: 'hyper_par' is NA or NaN.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(!is.numeric(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be a numeric vector.")
  }
  if(any(hyper_par < 0)){
    stop("Invalid argument: dirichlet 'hyper_par' must be positive.")
  }
  if(tolower(prior) %in% c("dirichlet") && all(hyper_par == 0)){
    stop("Invalid argument: dirichlet 'hyper_par' must be positive. Use 'prior' = 'haldane'")
  }

}

check_args_binomial <- function(
  x,
  success,
  null_par,
  prior,
  hyper_par,
  in_favour){

  check_binomial_data(x, success)
  check_binomial_null_par(null_par)
  check_beta_prior(prior)
  check_beta_hyper_par(hyper_par)
  check_in_favour(in_favour)
}

check_args_multinomial <- function(
  x,
  categories,
  null_par,
  prior,
  hyper_par,
  in_favour){

  check_multinomial_data(x)
  check_categories(categories)
  check_multinomial_null_par(null_par)
  check_dirichlet_prior(prior)
  check_dirichlet_hyper_par(hyper_par, prior)
  check_in_favour(in_favour)
  }











