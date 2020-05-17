
#' Nigrini's Z-test for Binomial Proportions with Continuity Correction
#'

nigrini_z_test <- function(x, sucess, null.par){

  if(any(!is.atomic(x), !is.vector(x), is.null(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("integer", "double", "character"))){
    stop("Invalid argument: 'x' must be of type 'integer', 'double' or 'character'.")
  }
  if(any(is.na(sucess), is.null(sucess))){
    stop("Invalid argument: 'sucess'  cant be missing nor null")
  }
  if(isFALSE(sucess %in% unique(x))){
    stop("Invalid argument: 'sucess %in% unique(x)' must be TRUE.")
  }
  if(any(isFALSE(length(null.par) == 1), isFALSE(length(sucess) == 1))){
    stop("Invalid argument: 'null.par' and 'sucess' must be of length 1")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
    stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
  }

  data.name <- deparse(substitute(x))
  x <- x[!is.na(x)]
  n <- length(x)
  sucesses <- as.numeric(table(x == sucess)["TRUE"])

  obs_prop <- sucesses/n

  z_stat <- (abs(obs_prop-null.par)-(1/(2*n)))/sqrt((null.par*(1-null.par))/n)
  p_value <- 2* (1-pnorm(abs(z_stat), 0, 1))

  results <- list(
    null.value = c("proportion" = null.par),
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

chisq_test_multinomial <- function(x, null.par, categories){

  if(any(!is.atomic(x), !is.vector(x))){
    stop("Invalid argument: 'x' must be an atomic vector.")
  }
  if(isFALSE(typeof(x) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(x) must be 'integer', 'double' or 'character'.")
  }
  if (any(!is.atomic(null.par), !is.vector(null.par))){
    stop("Invalid argument: 'null.par' must be an atomic vector.")
  }
  if(any(!is.numeric(null.par), null.par >= 1, null.par <= 0, is.na(null.par), is.null(null.par))) {
    stop("Invalid argument: all 'null.par' values must be between 0 and 1.")
  }
  if(any(is.na(categories), is.null(categories))){
    stop("Invalid argument: 'categories' must be a vector of the same lenght as 'x'")
  }
  if(isFALSE(typeof(categories) %in% c("double", "integer", "character"))){
    stop("Invalid argument: typeof(categories) must be 'integer', 'double' or 'character'.")
  }
  if(any(sort(unique(x)) != sort(unique(categories)))){
    stop("Invalid argument: the unique values of 'x' and 'categories' must be the same.")
  }

  data.name <- deparse(substitute(x))
  x <- x[!is.na(x)]
  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  tst <- chisq.test(counts,  p = null.par)

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
