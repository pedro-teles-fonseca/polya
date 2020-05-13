## functions for the "Dirichlet function", the multidimensional ##
# generalization of the beta distribution: it's the Bayesian canonical ##
# distribution for the parameter estimates of a multinomial ## distribution.
#
# "pdirichlet" and "qdirichlet" (distribution function and quantiles) would
# be more difficult because you'd first have to decide how to define the
# distribution function for a multivariate distribution ... I'm sure this
# could be done but I don't know how


logD <- function(a) {
  sum(lgamma(a)) - lgamma(sum(a))
}
######################################################
ddirichlet <- function(x, alpha)
  ## probability density for the Dirichlet function, where x=vector of
  ## probabilities
  ## and (alpha-1)=vector of observed samples of each type
  ## ddirichlet(c(p,1-p),c(x1,x2)) == dbeta(p,x1,x2)
{
  s <- sum((alpha - 1) * log(x))
  exp(sum(s) - logD(alpha))
}
######################################################
rdirichlet <- function(n, a)
  ## pick n random deviates from the Dirichlet function with shape
  ## parameters a
{
  l <- length(a)

  x <- matrix(rgamma(l * n, a), ncol = l, byrow = TRUE)

  sm <- x %*% rep(1, l)

  x / as.vector(sm)

}
