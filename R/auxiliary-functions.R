

# Function to turn Bayes factors into posterior probabilities of the NULL

bfactor.to.prob <- function(bf, pi_null = .5) {
  ((1 + ((1 - pi_null) / pi_null) * (1 / bf))) ^ (-1)
}

# Function to compute the mode of a vector.

getmode <- function(v) {
  uniqv <- unique(v)

  uniqv[which.max(tabulate(match(v, uniqv)))]

}
