
# Bayes factor
# ------------------------------------------
bayes.factor <- function(
  data,
  null.par,
  model = "multinomial",
  hyper.par = if (model == "multinomial") {
    rep.int(1, length(null.par))
  } else {
    c(1, 1)
  }) {
  if (any(null.par > 1) ||
      any(null.par < 0) ||
      any(is.na(null.par)) || is.null(null.par)) {
    stop("Invalid null parameter value: all 'null.par' must be such that 0 < null.par < 1.")
  }
  if (any(hyper.par < 0) ||
      any(is.na(hyper.par)) || is.null(hyper.par)) {
    stop("Invalid hyper parameter: 'hyper.par' must be non-negative.")
  }
  if (isFALSE(model %in% c("multinomial", "binomial")) ||
      is.null(model)) {
    stop("Invalid statistical model. Model must be either 'multinomial' or 'binomial'.")
  }
  if (model == "multinomial") {
    if (length(null.par) == 1) {
      stop("One parameter model. Use 'model = 'binomial'.")
    } else {
      if (length(null.par) < length(hyper.par)) {
        stop(
          "Invalid model specification: more hyper-parameters than null parameter values."
        )
      } else if (length(null.par) > length(hyper.par)) {
        stop(
          "Invalid model specification: more null parameter values than hyper parameters."
        )
      }
    }
  }

  data <- data[!is.na(data)]

  if (model == "multinomial") {
    counts <- as.numeric(table(data))
    exp(sum(counts * log(null.par)) + sum(lgamma(hyper.par)) + lgamma(sum(hyper.par + counts)) -
        lgamma(sum(hyper.par)) - sum(lgamma(hyper.par + counts)))

  } else if (model == "binomial") {
    n <- length(data)
    x <- as.numeric(rev(table(data))[1])
    a <- hyper.par[[1]]
    b <- hyper.par[[2]]

    exp(
      x * log(null.par) + (n - x) * log(1 - null.par) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
        (lgamma(a + b) + lgamma(n + a - x) + lgamma(x + a))
    )

  }
}
