
#' @title Multivariate Beta Function
#'
#' @description \loadmathjax Underflow-robust implementation of the multivariate Beta function.
#'
#' @param x A numeric vector of non-negative values.
#'
#' @details Consider the numeric vector \mjeqn{x = (x_1, ..., x_k)}{x = (x_1, ..., x_k)} with \mjeqn{x_i > 0 \, \forall i \in \lbrace 1, \dots, k \rbrace}{x_i > 0 for all i in \{1, ..., k \}}. The multivariate Beta function is defined as:
#'
#' \mjdeqn{B(x) = \prod_{i=1}^{k} \Gamma (x_i) \Big/ \Gamma \left(\sum_{i=1}^{k} x_i \right)}{B(x) = product_{i=1}^{k} Gamma(x_i) / Gamma(sum_{i=1}^{k} x_i)}
#'
#' where \mjeqn{\Gamma}{Gamma} is the Gamma function. To avoid underflow, `mbeta` implements an alternative representation of the Beta function. Taking the natural logarithm of both sides in the latter equation and then exponentiating we obtain:
#'
#' \mjdeqn{\operatorname{B}(x) = \operatorname{exp} \,  \left\lbrace \sum_{i=1}^{k}{log \, \Gamma(x_i)} - log \, \Gamma \left(\sum_{i = 1}^{k}x_i\right)\right\rbrace}{B(x) = exp(sum_\{i=1\}^\{k\} logGamma(x_i) - logGamma(sum_\{i = 1\}^\{k\} x_i))}
#'
#' With this formula we can take advantage of the \code{\link[base]{lgamma}} function, which is itself underflow-robust. Note that \mjeqn{\operatorname{B}(x)}{B(x)} is only defined in \mjeqn{\Re}{R} if all elements of `x` are greater than zero and is infinite if any element of `x` is equal to zero \insertCite{becker1972}{polya}.
#'
#' @return If all elements of `x` are greater than zero then `mbeta` returns a numeric vector of \code{\link[base]{length}} 1. If any element of `x` is equal to zero then `mbeta` returns `inf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[polya]{lmbeta}} for an underflow-robust implementation of the natural logarithm of the Beta function.
#'
#' @examples
#' mbeta(c(1, 1)) # same as beta(1,1)
#' mbeta(c(1, 1, 1))
#' mbeta(c(1, 2, 4))
#' mbeta(rep(0.5, 10))
#' mbeta(1:5)
#'
#' @export

mbeta <- function(x) {
  if (any(x < 0)) {
    stop("Invalid argument: 'x' must be a numeric vector with only non-negative elements.")
  }
  if (any(x == 0)) {
    return(Inf)
  }
  exp(sum(lgamma(x)) - lgamma(sum(x)))
}

#' @title Natural Logarithm of the Multivariate Beta Function
#'
#' @description \loadmathjax Underflow-robust implementation of the natural logarithm of the multivariate Beta function.
#'
#' @inheritParams mbeta
#'
#' @details Consider the numeric vector \mjeqn{x = (x_1, ..., x_k)}{x = (x_1, ..., x_k)} with \mjeqn{x_i > 0 \, \forall i \in \lbrace 1, \dots, k \rbrace}{x_i > 0 for all i in \{1, ..., k \}}. The multivariate Beta function is defined as:
#'
#' \mjdeqn{\operatorname{B}(x) = \prod_{i=1}^{k} \Gamma (x_i) \Big/ \Gamma \left(\sum_{i=1}^{k} x_i \right)}{B(x) = product_{i=1}^{k} Gamma(x_i) / Gamma(sum_{i=1}^{k} x_i)}
#'
#' where \mjeqn{\Gamma}{Gamma} is the Gamma function. Taking the natural logarithm of both sides in the latter equation we obtain:
#'
#' \mjdeqn{\operatorname{log} \operatorname{B}(x) = \sum_{i=1}^{k}{log \, \Gamma(x_i)} - log \, \Gamma \left(\sum_{i = 1}^{k}x_i\right)}{log B(x) = sum_{i=1}^{k}logGamma(x_i) - logGamma(sum_{i = 1}^{k} x_i)}
#'
#' With this formula we can take advantage of the underflow-robust \code{\link[base]{lgamma}} function. Note that \mjeqn{\operatorname{B}(x)}{B(x)} is only defined in \mjeqn{\Re}{R} if all elements of `x` are greater than zero and is infinite if any element of `x` is equal to zero \insertCite{becker1972}{polya}.
#'
#' @return If all elements of `x` are greater than zero then `lmbeta` returns a numeric vector of \code{\link[base]{length}} 1. If any element of `x` is equal to zero then `lmbeta` returns `inf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[polya]{lmbeta}} for an underflow-robust implementation of the natural logarithm of the Beta function.
#'
#' @examples
#' lmbeta(c(1, 1)) # same as lbeta(1,1)
#' lmbeta(c(1, 1, 1))
#' lmbeta(c(1, 2, 4))
#' lmbeta(rep(0.5, 10))
#' lmbeta(1:5)
#'
#' @export

lmbeta <- function(x) {
  if (any(x < 0)) {
    stop("Invalid argument: 'x' must be a numeric vector with only non-negative elements.")
  }
  if (any(x == 0)) {
    return(Inf)
  }
  sum(lgamma(x)) - lgamma(sum(x))
}



