
bfactor.to.prob <- function(pi_null, bf) {
  ((1 + ((1 - pi_null) / pi_null) * (1 / bf))) ^ (-1)
}

# Function to compte the mode of a vector.

getmode <- function(v) {

  uniqv <- unique(v)

  uniqv[which.max(tabulate(match(v, uniqv)))]

}
