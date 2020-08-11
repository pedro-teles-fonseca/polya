

prob_mts <- function(theta, k = 9){

  prod(theta) * factorial(k)

}

# prob_mts(c(.9, .05, .05), 3)
# prob_mts(rep(.1, 10), 10)
# prob_mts(theta_benford(1), 9)
# prob_mts(theta_benford(2), 10)
#
# prob_mts(c(.1,.9,0), 3)
#
#
