
ibf <- function(
  x,
  categories,
  null_par,
  prior = "haldane",
  hyper_par = rep(1, length(categories)),
  type = "arithmetic",
  method = "smts",
  k = 2,
  in_favour = "H1") {

  data <- x[!is.na(x)]
  n <- length(data)
  l <- k * n

  b10 <- bf_multinomial(x = data, categories = categories, null_par = null_par, prior = prior, hyper_par = hyper_par, in_favour = "H1")

  if(method == "smts"){

    x_l <- list()

    for (i in seq_len(l)){

      x_l[[i]] <- vector(mode = "numeric")

      while (length(table(x_l[[i]])) < length(categories)) {
        x_l[[i]] <- c(x_l[[i]], sample(data, size = 1, replace = FALSE))
      }
    }

    b01 <- mapply(FUN = bf_multinomial, x = x_l,
                  MoreArgs = list(categories = categories, prior = prior, in_favour = "H0"))
  }

  if(method == "mts"){

    b01 <- bf_multinomial(x = categories, categories = categories, null_par = null_par, prior = prior, hyper_par = hyper_par, in_favour = "H0")

  }

  ibf <- switch (type,
    "arithmetic" = , "a" = b10 * mean(b01),
    "geometric" = , "g" = b10 * exp(mean(log(b01)))
  )

  switch(
    tolower(in_favour),
    "h0" = , "null" = 1/ibf,
    "h1" = , "alternative" = ibf)
}








