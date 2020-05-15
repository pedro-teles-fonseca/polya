
# Function to compute the mode of a vector.

getmode <- function(v) {
  uniqv <- unique(v)

  uniqv[which.max(tabulate(match(v, uniqv)))]

}
