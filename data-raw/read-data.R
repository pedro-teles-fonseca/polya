
library(usethis)
library(digit.analysis)

data <- read.delim("data-raw/data.txt")
source("R/auxiliary-functions.R")

datalist <- list()

for (c in 1:ncol(data)) {
  assign(names(data)[c], data[, c])
  assign(paste0(names(data)[c], ".bl1"), msdigit(data[, c]))
  assign(paste0(names(data)[c], ".bl2"), smsdigit(data[, c]))
  datalist[[c]] <- data[, c]
}

rm(c, datalist)

usethis::use_data(
  data,
  Austria,
  Belgium,
  Finland,
  France,
  Germany,
  Greece,
  Ireland,
  Italy,
  Luxembourg,
  Netherlands,
  Portugal,
  Spain,
  Austria.bl1,
  Belgium.bl1,
  Finland.bl1,
  France.bl1,
  Germany.bl1,
  Greece.bl1,
  Ireland.bl1,
  Italy.bl1,
  Luxembourg.bl1,
  Netherlands.bl1,
  Portugal.bl1,
  Spain.bl1,
  Austria.bl2,
  Belgium.bl2,
  Finland.bl2,
  France.bl2,
  Germany.bl2,
  Greece.bl2,
  Ireland.bl2,
  Italy.bl2,
  Luxembourg.bl2,
  Netherlands.bl2,
  Portugal.bl2,
  Spain.bl2,
  overwrite = TRUE)
