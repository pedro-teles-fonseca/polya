
posterior.prob <- function(pi_null, bf) {
  ((1 + ((1 - pi_null) / pi_null) * (1 / bf))) ^ (-1)
}
