
#' Bayes Factors for Binomial point null hypotheses
#'
#' Computes Bayes factors in favour of a point null hypothesis in the context of a Binomial statistical model with a Beta prior.
#' @param x Atomic vector of the type integer, double or character.
#' @param sucess Number or character indicating which level of \code{x} corresponds to a sucess.
#' @param null.par The parameter of the Binomial distribution under the null hipothesis.
#' @param a, b Shape parameters of the Beta prior. Both must be greater than zero
#'
#' @export

bfactor_binomial <- function(
  x,
  sucess,
  null.par,
  a = 1,
  b = 1,
  transf = "level") {

  if(any(!is.atomic(x), !is.vector(x), is.null(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop("Invalid argument: 'x' must be of type 'integer', 'double' or 'character'.")
  }
  if(any(is.na(sucess), is.null(sucess))){
    stop("Invalid argument: 'sucess'  cant be missing nor null")
  }
  if(isFALSE(sucess %in% unique(x))){
    stop("Invalid argument: 'sucess %in% unique(x)' must be TRUE.")
  }
  if(any(isFALSE(length(null.par) == 1), isFALSE(length(sucess) == 1))){
    stop("Invalid argument: 'null.par' and 'sucess' must be of length 1")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
    stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
  }
  if(any(!is.atomic(a), !is.vector(a), !is.atomic(b), !is.vector(b))){
    stop("Invalid argument: 'a' and 'b' must be atomic vectors.")
  }
  if(any(isFALSE(length(a) == 1), isFALSE(length(b) == 1))){
    stop("Invalid argument: 'a' and 'b' must be of length 1.")
    }
  if(any(a <= 0, is.na(a), is.null(a), b <= 0, is.na(b), is.null(b))){
    stop("Invalid argument: 'a' and 'b' must be greather than 0.")
  }
  if(isFALSE(transf %in% c("level", "log", "log10"))){
    stop("Invalid argument: 'transf' must be either 'level', 'log' or 'log10'.")
  }

  x <- x[!is.na(x)]
  n <- length(x)
  sucesses <- as.numeric(table(x == sucess)["TRUE"])

  log.bfactor <- sucesses * log(null.par) + (n - sucesses) * log(1 - null.par) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
      (lgamma(a + b) + lgamma(n + a - sucesses) + lgamma(sucesses + a))

  bfactor <- exp(log.bfactor)

  if(transf == "log"){
    c("log(BF)" = log.bfactor)
  } else if(transf == "log10"){
    c("log10(BF)" = log10(bfactor))
  } else {
    c("BF" = bfactor)
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
  null.par,
  alpha = 1,
  transf = "level") {

  if(any(!is.atomic(x), !is.vector(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
    stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
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
  if(any(!is.atomic(alpha), !is.vector(alpha))){
    stop("Invalid argument: 'alpha' must be an atomic vector.")
  }
  if(any(!is.numeric(alpha), alpha < 0, is.na(alpha), is.null(alpha))){
    stop("Invalid argument: 'alpha' must be non-negative.")
  }
  if(isFALSE(transf %in% c("level", "log", "log10"))){
    stop("Invalid argument: 'transf' must be either 'level', 'log' or 'log10'.")
  }
  if (length(null.par) < length(alpha)) {
      stop("Invalid model specification: more hyper-parameters than null parameter values.")
    } else if (length(alpha) == 1){
          alpha <- rep.int(alpha, length(null.par))
    } else if(length(null.par) > length(alpha)) {
      stop(
          "Invalid specification: more null parameter values than hyper parameters."
        )
    }

  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  bfactor <- function(counts, null.par, alpha) {

      log.bfactor <- sum(counts * log(null.par)) + sum(lgamma(alpha)) + lgamma(sum(alpha + counts)) -
        lgamma(sum(alpha)) - sum(lgamma(alpha + counts))

      exp(log.bfactor)
    }

    if(transf == "log"){
      c("log(BF)" =  log(bfactor(counts, null.par, alpha)))
    } else if(transf == "log10"){
      c("log10(BF)" =  log10(bfactor(counts, null.par, alpha)))
    } else {
      c("BF" =  bfactor(counts, null.par, alpha))
    }
}

# ----------------------------------------------------------------------------------------
# Function to interpret the evidence provided by the data in favour of the null hypothesis
# ----------------------------------------------------------------------------------------

#' @export

bfactor_interpret <- function(bf) {

  bf <- unname(bf)

  evidence <- ifelse(bf < 1, "Negative",
                ifelse(bf < 3.2, "Weak",
                  ifelse(bf < 10, "Substantial",
                    ifelse(bf < 100, "Strong",
                      "Decisive")
      )
    )
  )
  c("Evidence" = evidence)
}

# ----------------------------------------------------------------------------------------
# Function to interpret the evidence provided by the data in favour of the null hypothesis
# ----------------------------------------------------------------------------------------

bfactor_log_interpret <- function(l_bf, base = exp(1)) {

  l_bf <- unname(l_bf)

  evidence <- ifelse(l_bf < log(1, base = base), "Negative",
    ifelse(l_bf < log(3.2, base = base), "Weak",
      ifelse(l_bf < log(10, base = base), "Substantial",
        ifelse(l_bf < log(100, base = base), "Strong",
          "Decisive")
      )
    )
  )
  c("Evidence" = evidence)
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

  c("P(H0|X)" =  ((1 + ((1 - pi_null) / pi_null) * (1 / unname(bf)))) ^ (-1))

}


bfactor_to_prob(.5, pi_null = .1)
.5/1.5


