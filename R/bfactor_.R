
#' Bayes Factors for Binomial point null hypotheses
#'
#' Computes Bayes factors in favour of a point null hypothesis in the context of a Binomial statistical model with a Beta prior.
#' @param x Atomic vector of the type integer, double or character.
#' @param success Number or character indicating which level of \code{x} corresponds to a success.
#' @param null_par The parameter of the Binomial distribution under the null hipothesis.
#' @param a, b Shape parameters of the Beta prior. Both must be greater than zero
#'
#' @examples
#'
#' bfactor_binomial(austria_bl1, 1, theta_benford(1)[1])
#'
#' @export

bfactor_binomial <- function(
  x,
  success,
  null_par,
  hyper_par = c(1, 1),
  in_favour = "H0") {

  if(any(!is.atomic(x), !is.vector(x), is.null(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop("Invalid argument: 'x' must be of type 'integer', 'double' or 'character'.")
  }
  if(any(is.na(success), is.null(success))){
    stop("Invalid argument: 'success'  cant be missing nor null")
  }
  if(isFALSE(success %in% unique(x))){
    stop("Invalid argument: 'success %in% unique(x)' must be TRUE.")
  }
  if(any(isFALSE(length(null_par) == 1), isFALSE(length(success) == 1))){
    stop("Invalid argument: 'null_par' and 'success' must be of length 1")
  }
  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }
  if(any(hyper_par <= 0, is.na(hyper_par), is.null(hyper_par))){
    stop("Invalid argument: elements of 'hyper_par' must be greather than 0.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(isFALSE(length(hyper_par) %in% c(1, 2)))){
    stop("Invalid argument: 'hyper_par' must be of length 1 or 2.")
  }
  if(isFALSE(tolower(in_favour) %in% c("h0", "h1", "null", "alternative"))){
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1' Alternatively, you can use either 'null' or 'alternative'.")
  }

  if(length(hyper_par == 1)){hyper_par <- rep.int(hyper_par, 2)}

  x <- x[!is.na(x)]
  n <- length(x)
  s <- as.numeric(table(x == success)["TRUE"])
  a <- hyper_par[1]
  b <- hyper_par[2]

  bfactor <- exp(s * log(null_par) + (n - s) * log(1 - null_par) + lbeta(a, b) - lbeta(s + a, n + b - s))

  if(tolower(in_favour) %in% c("null", "h0")){
    bfactor
  } else {
    1/bfactor
  }
}

#' Bayes Factors for Multinomial point null hypotheses
#'
#' Computes Bayes factors in favour of a point null hypothesis in the context of a Multinomial statistical model with a Dirichlet prior.
#'
#' @export

bfactor_multinomial <- function(
  x,
  categories,
  null_par,
  hyper_par = 1,
  in_favour = "H0") {

  if(any(!is.atomic(x), !is.vector(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }
  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }
  if(any(is.na(categories), is.null(categories))){
    stop("Invalid argument: 'categories' must be a vector of the same lenght as 'x'")
  }
  if(isFALSE(typeof(categories) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(categories) must be 'integer', 'double' or 'character'.")
  }
  if(any(sort(unique(x)) != sort(unique(categories)))){
      stop("Invalid argument: the unique values of 'x' and 'categories' must be the same.")
  }
  if(any(!is.atomic(hyper_par), !is.vector(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be an atomic vector.")
  }
  if(any(!is.numeric(hyper_par), hyper_par < 0, is.na(hyper_par), is.null(hyper_par))){
    stop("Invalid argument: 'hyper_par' must be non-negative.")
  }
  if (length(null_par) < length(hyper_par)) {
      stop("Invalid model specification: more hyper-parameters than null parameter values.")
    } else if (length(hyper_par) == 1){
          hyper_par <- rep.int(hyper_par, length(null_par))
    } else if(length(null_par) > length(hyper_par)) {
      stop("Invalid specification: more null parameter values than hyper parameters.")
    }
  if(isFALSE(tolower(in_favour) %in% c("h0", "h1", "null", "alternative"))){
    stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1' Alternatively, you can use either 'null' or 'alternative'.")
  }

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- exp(sum(counts * log(null_par)) + lmbeta(hyper_par) - lmbeta(hyper_par + counts))

  if(tolower(in_favour) %in% c("null", "h0")){
    bfactor
  } else {
    1/bfactor
  }
}




