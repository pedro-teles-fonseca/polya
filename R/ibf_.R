
#' @export

ibf <- function(
  data,
  categories = sort(unique(data)),
  null_par = rep(),
  hyper_par = rep(1, length(categories)),
  haldane = FALSE,
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  data <- data[!is.na(data)]
  ncat <- length(categories)

  if (length(unique(data)) < ncat ) {
    stop("`data` must containg at least one observation of each category. ")
  }
  if (length(unique(data)) > ncat ) {
    stop("More levels in `data` than in 'categories'.")
  }

  n <- length(data)
  l <- k * n

  b10 <- bf_multinomial(data = data, categories = categories, null_par = null_par, hyper_par = hyper_par, haldane = haldane, in_favour = "H1")

  if(method == "smts") {

    x_l <- list()

    for (i in seq_len(l)) {

      x_l[[i]] <- vector()

      while (length(unique(x_l[[i]])) < ncat) {
        x_l[[i]] <- c(x_l[[i]], sample(data, size = 1, replace = FALSE))
      }
    }

    b01 <- mapply(FUN = bf_multinomial, data = x_l,
                  MoreArgs = list(categories = categories, haldane = haldane, in_favour = "H0"))
  }

  if(method == "mts") {

    b01 <- bf_multinomial(data = categories, categories = categories, null_par = null_par, hyper_par = hyper_par, haldane = haldane, in_favour = "H0")

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
  data,
  categories,
  null_par,
  hyper_par = rep(1, length(categories)),
  haldane = FALSE,
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  ibf(
    data = data,
    categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    haldane = haldane,
    type = "arithmetic",
    method = method,
    k = k,
    in_favour = in_favour

  )
}

#' @export

gibf <- function(
  data,
  categories,
  null_par,
  hyper_par = rep(1, length(categories)),
  haldane = FALSE,
  type = "arithmetic",
  method = "mts",
  k = 2,
  in_favour = "H1") {

  ibf(
    data = data,
    categories = categories,
    null_par = null_par,
    hyper_par = hyper_par,
    haldane = haldane,
    type = "arithmetic",
    method = method,
    k = k,
    in_favour = in_favour

  )
}




