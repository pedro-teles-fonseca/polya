
#' @export

ibf <- function(
  x,
  categories = sort(unique(x)),
  null_par = rep(),
  prior = "dirichlet",
  hyper_par = rep(1, length(categories)),
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  x <- x[!is.na(x)]
  ncat <- length(categories)

  if (length(unique(x)) < ncat ) {
    stop("The data 'x' must containg at least one observation of each category. ")
  }
  if (length(unique(x)) > ncat ) {
    stop("More levels in the data 'x' than in 'categories'.")
  }

  n <- length(x)
  l <- k * n

  b10 <- bf_multinomial(x = x, categories = categories, null_par = null_par, prior = prior, hyper_par = hyper_par, in_favour = "H1")

  if(method == "smts") {

    x_l <- list()

    for (i in seq_len(l)) {

      x_l[[i]] <- vector()

      while (length(unique(x_l[[i]])) < ncat) {
        x_l[[i]] <- c(x_l[[i]], sample(x, size = 1, replace = FALSE))
      }
    }

    b01 <- mapply(FUN = bf_multinomial, x = x_l,
                  MoreArgs = list(categories = categories, prior = prior, in_favour = "H0"))
  }

  if(method == "mts") {

    b01 <- bf_multinomial(x = categories, categories = categories, null_par = null_par, prior = prior, hyper_par = hyper_par, in_favour = "H0")

  }

  ibf <- switch (type,
    "arithmetic" = , "a" = b10 * mean(b01),
    "geometric" = , "g" = b10 * exp(mean(log(b01)))
  )

  switch(tolower(in_favour),
         "h0" = , "null" = 1/ibf,
         "h1" = , "alternative" = ibf,
         stop("Invalid argument: 'in_favour' must be either 'H0' or 'H1'. Alternatively, you can use 'Null' or 'Alternative'."))
}

#' @export

aibf <- function(
  x,
  categories,
  null_par,
  prior = "dirichlet",
  hyper_par = rep(1, length(categories)),
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  ibf(
    x = x,
    categories = categories,
    null_par = null_par,
    prior = prior,
    hyper_par = hyper_par,
    type = "arithmetic",
    method = method,
    k = k,
    in_favour = in_favour

  )
}

#' @export

gibf <- function(
  x,
  categories,
  null_par,
  prior = "dirichlet",
  hyper_par = rep(1, length(categories)),
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  ibf(
    x = x,
    categories = categories,
    null_par = null_par,
    prior = prior,
    hyper_par = hyper_par,
    type = "arithmetic",
    method = method,
    k = k,
    in_favour = in_favour

  )
}




