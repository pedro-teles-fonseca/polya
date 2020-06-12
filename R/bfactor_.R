
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
  transf = "level") {

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
  if(isFALSE(transf %in% c("level", "log", "log10"))){
    stop("Invalid argument: 'transf' must be either 'level', 'log' or 'log10'.")
  }

  if(length(hyper_par == 1)){hyper_par <- rep.int(hyper_par, 2)}

  x <- x[!is.na(x)]
  successes <- as.numeric(table(x == success)["TRUE"])
  n <- length(x)
  a <- hyper_par[1]
  b <- hyper_par[2]

  log.bfactor <- successes * log(null_par) + (n - successes) * log(1 - null_par) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
      (lgamma(a + b) + lgamma(n + a - successes) + lgamma(successes + a))

  bfactor <- exp(log.bfactor)

  if(transf == "log"){
    log.bfactor
  } else if(transf == "log10"){
    log10(bfactor)
  } else {
    bfactor
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
  transf = "level") {

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
  if(isFALSE(transf %in% c("level", "log", "log10"))){
    stop("Invalid argument: 'transf' must be either 'level', 'log' or 'log10'.")
  }
  if (length(null_par) < length(hyper_par)) {
      stop("Invalid model specification: more hyper-parameters than null parameter values.")
    } else if (length(hyper_par) == 1){
          hyper_par <- rep.int(hyper_par, length(null_par))
    } else if(length(null_par) > length(hyper_par)) {
      stop(
          "Invalid specification: more null parameter values than hyper parameters."
        )
    }

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- function(counts, null_par, hyper_par) {

      log.bfactor <- sum(counts * log(null_par)) + sum(lgamma(hyper_par)) + lgamma(sum(hyper_par + counts)) -
        lgamma(sum(hyper_par)) - sum(lgamma(hyper_par + counts))

      exp(log.bfactor)
    }

    if(transf == "log"){
      log(bfactor(counts, null_par, hyper_par))
    } else if(transf == "log10"){
      log10(bfactor(counts, null_par, hyper_par))
    } else {
      bfactor(counts, null_par, hyper_par)
    }
}

#' @export

bfactor_interpret <- function(bf) {

  bf <- unname(bf)

  ifelse(bf < 1, "Negative",
         ifelse(bf < 3.2, "Weak",
                ifelse(bf < 10, "Substantial",
                       ifelse(bf < 100, "Strong",
                              "Decisive")
      )
    )
  )
}

bfactor_log_interpret <- function(l_bf, base = exp(1)) {

  l_bf <- unname(l_bf)

  ifelse(l_bf < log(1, base = base), "Negative",
    ifelse(l_bf < log(3.2, base = base), "Weak",
      ifelse(l_bf < log(10, base = base), "Substantial",
        ifelse(l_bf < log(100, base = base), "Strong",
          "Decisive")
      )
    )
  )
}


#' Use Bayes factors to Update Prior Probabilities to Posterior Probabilities
#'
#' @param bf a Bayes factor
#' @param pi_null The prior probability of the null hypothesis.


#' @export

bfactor_to_prob <- function(bf, pi_null = .5) {

  if(any(!is.atomic(bf), !is.vector(bf) && !is.matrix(bf))){
    stop("Invalid argument: 'x' must be an atomic vector or matrix.")
  }
  if(isFALSE(typeof(bf) %in% c("double", "integer"))){
    stop("Invalid argument: typeof(bf) must be 'integer', 'double'.")
  }
  if(isFALSE(length(pi_null) == 1)){
    stop("Error: 'pi_null' must be of length 1. ")
  }
  if(any(pi_null < 0, pi_null > 1, isFALSE(typeof(pi_null) %in% c("double", "integer")), is.null(pi_null), is.na(pi_null))){
    stop("Error: 'pi_null' must be between zero and one.")
  }

  ((1 + ((1 - pi_null) / pi_null) * (1 / bf))) ^ (-1)

}


