
theta_benford <- function(d) {

  if (d == 1) {
    log10(1 + 1/1:9)
  } else if (d == 2) {
    sapply(0:9, function(x) {sum(log10(1+1/(10*(1:9)+x)))})
  } else {
    stop("Invalid argument: 'd' must be either 1 or 2.")
  }
}

theta_bl1 <- theta_benford(1)
theta_bl2 <- theta_benford(2)
