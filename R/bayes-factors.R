
# Bayes factor
# ------------------------------------------
bfactor.multinomial <- function(
  data,
  null.par,
  alpha = rep.int(1, length(null.par))) {

  if(any(!is.atomic(data), !is.vector(data))){
    stop("Invalid argument: 'data' must be an atomic vector.")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
        stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
  }
  if(any(!is.atomic(alpha), !is.vector(alpha))){
    stop("Invalid argument: 'alpha' must be an atomic vector.")
  }
  if(any(!is.numeric(alpha), any(alpha < 0), any(is.na(alpha)), is.null(alpha))){
    stop("Invalid argument: 'alpha' must be non-negative.")
  }
  # if (model == "multinomial") {
  #   if (length(null.par) == 1) {
  #     stop("One parameter model. Use 'model = 'binomial'.")
  #   } else {
  #     if (length(null.par) < length(alpha)) {
  #       stop(
  #         "Invalid model specification: more hyper-parameters than null parameter values."
  #       )
  #       } else if (length(alpha) == 1){
  #         alpha <- rep.int(alpha, length(null.par))
  #     } else if(length(null.par) > length(alpha)) {
  #       stop(
  #         "Invalid model specification: more null parameter values than hyper parameters."
  #       )
  #     }
  #   }
  # }

  data <- data[!is.na(data)]
  x <- table(data)

    bfactor <- function(x, null.par, alpha) {

      log.bfactor <- sum(x * log(null.par)) + sum(lgamma(alpha)) + lgamma(sum(alpha + x)) -
        lgamma(sum(alpha)) - sum(lgamma(alpha + x))

      exp(log.bfactor)
    }

    bfactor(x, null.par, alpha)
}



# Bayes factor
# ------------------------------------------
bfactor <- function(
  data,
  null.par,
  model = "multinomial",
  alpha = rep.int(1, length(null.par)),
  a = 1,
  b = 1) {

  if(any(!is.atomic(data), !is.vector(data))){
    stop("Invalid argument: 'data' must be an atomic vector.")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
    stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
  }
  if (any(isFALSE(model %in% c("multinomial", "binomial")),  is.null(model))) {
    stop("Invalid argument: 'model' must be either 'multinomial' or 'binomial'.")
  }
  if(any(!is.atomic(alpha), !is.vector(alpha))){
    stop("Invalid argument: 'alpha' must be an atomic vector.")
  }
  if(any(!is.numeric(alpha), any(alpha < 0), any(is.na(alpha)), is.null(alpha))){
    stop("Invalid argument: 'alpha' must be non-negative.")
  }
  if(any(!is.atomic(a), !is.atomic(b), !is.vector(a), !is.vector(b))){
    stop("Invalid argument: 'alpha' must be an atomic vector.")
  }
  if(any(a <= 0, b <= 0, is.na(a), is.na(b), is.null(a), is.null(b))){
    stop("Invalid argument: all values of 'a' and 'b' must be greather than 0.")
  }

  if (model == "multinomial") {
    if (length(null.par) == 1) {
      stop("One parameter model. Use 'model = 'binomial'.")
    } else {
      if (length(null.par) < length(alpha)) {
        stop(
          "Invalid model specification: more hyper-parameters than null parameter values."
        )
      } else if (length(alpha) == 1){
        alpha <- rep.int(alpha, length(null.par))
      } else if(length(null.par) > length(alpha)) {
        stop(
          "Invalid model specification: more null parameter values than hyper parameters."
        )
      }
    }
  }

  data <- data[!is.na(data)]

  if (model == "multinomial") {

    x <- table(data)

    bfactor <- function(x, null.par, alpha) {

      log.bfactor <- sum(x * log(null.par)) + sum(lgamma(alpha)) + lgamma(sum(alpha + x)) -
        lgamma(sum(alpha)) - sum(lgamma(alpha + x))

      exp(log.bfactor)
    }

    bfactor(x, null.par, alpha)

  } else if (model == "binomial") {

    x <- rev(table(data))[1]
    n <- length(data)

    bfactor <- function(x, null.par, a, b, n)  {

      log.bfactor <- x * log(null.par) + (n - x) * log(1 - null.par) + lgamma(a) + lgamma(b) + lgamma(n + a + b) -
        (lgamma(a + b) + lgamma(n + a - x) + lgamma(x + a))

      exp(log.bfactor)
    }

    c("Bayes factor in favour of H0" = bfactor(x, null.par, a, b, n))
  }
}



