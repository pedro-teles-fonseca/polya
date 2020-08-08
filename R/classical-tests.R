
#' Nigrini's Z-test for Binomial Proportions with Continuity Correction
#'

nigrini_z_test <- function(x, success, null_par){

  if(any(!is.atomic(x), !is.vector(x), is.null(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop("Invalid argument: 'x' must be of type 'integer', 'double' or 'character'.")
  }
  if(any(is.na(success), is.null(success))){
    stop("Invalid argument: 'success'  cant be missing nor null")
  }
  if(isFALSE(success %in% unique(x))){
    stop("Invalid argument: 'success %in% unique(x)' must be TRUE.")
  }
  if(any(isFALSE(length(null_par) == 1), isFALSE(length(success) == 1))){
    stop("Invalid argument: 'null_par' and 'success' must be of length 1")
  }
  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }

  data.name <- deparse(substitute(x))
  x <- x[!is.na(x)]
  n <- length(x)
  successes <- as.numeric(table(x == success)["TRUE"])

  obs_prop <- successes/n

  z_stat <- (abs(obs_prop-null_par)-(1/(2*n)))/sqrt((null_par*(1-null_par))/n)
  p_value <- 2* (1-pnorm(abs(z_stat), 0, 1))

  results <- list(
    null.value = c("proportion" = null_par),
    method = "Nigrini's Z-test for Binomial proportions with continuity correction",
    alternative = "two.sided",
    estimate = c("observed_proportion" = obs_prop),
    data.name = data.name,
    statistic = c("z-statistic" = z_stat),
    p.value = p_value,
    sample.size = c("n" = n)
  )

  class(results) <- "htest"
  results

}

#' Pearson's Chi-Squared Test for Multinomial Proportions
#'

chisq_test_multinomial <- function(x, categories, null_par, simulate.p.value = FALSE){

  if(any(!is.atomic(x), !is.vector(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }
  if (any(!is.atomic(null_par), !is.vector(null_par))){
    stop("Invalid argument: 'null_par' must be an atomic vector.")
  }
  if(any(!is.numeric(null_par), null_par >= 1, null_par <= 0, is.na(null_par), is.null(null_par))) {
    stop("Invalid argument: all 'null_par' values must be between 0 and 1.")
  }
  if(any(is.na(categories), is.null(categories))){
    stop("Invalid argument: missing 'categories'")
  }
  if(isFALSE(typeof(categories) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(categories) must be 'integer', 'double' or 'character'.")
  }

  data.name <- deparse(substitute(x))

  x <- factor(x[!is.na(x)], levels = categories)
  counts <- table(x)

  tst <- chisq.test(x = counts,  p = null_par, simulate.p.value = simulate.p.value)

  results <- list(
    method = "Pearson's Chi-Squared Test for Multinomial Proportions",
    alternative = "two.sided",
    data.name = data.name,
    statistic = c("Chi-squared" = unname(tst$statistic)),
    p.value =  tst$p.value,
    sample.size = c("n" = length(x))
  )

  class(results) <- "htest"
  results

}
