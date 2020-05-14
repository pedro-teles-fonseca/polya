
library(usethis)
library(digit.analysis)

data <- read.delim("data-raw/data.txt")

for (c in 1:ncol(data)) {
  assign(paste0(tolower(names(data)[c]), ".bl1"), msdigit(data[, c]))
  assign(paste0(tolower(names(data)[c]), ".bl2"), smsdigit(data[, c]))
}

datalist.bl1 <- list(
  austria.bl1,
  belgium.bl1,
  finland.bl1,
  france.bl1,
  germany.bl1,
  greece.bl1,
  ireland.bl1,
  italy.bl1,
  luxembourg.bl1,
  netherlands.bl1,
  portugal.bl1,
  spain.bl1
)

datalist.bl2 <- list(
  austria.bl2,
  belgium.bl2,
  finland.bl2,
  france.bl2,
  germany.bl2,
  greece.bl2,
  ireland.bl2,
  italy.bl2,
  luxembourg.bl2,
  netherlands.bl2,
  portugal.bl2,
  spain.bl2
)
usethis::use_data(
  austria.bl1,
  belgium.bl1,
  finland.bl1,
  france.bl1,
  germany.bl1,
  greece.bl1,
  ireland.bl1,
  italy.bl1,
  luxembourg.bl1,
  netherlands.bl1,
  portugal.bl1,
  spain.bl1,
  austria.bl2,
  belgium.bl2,
  finland.bl2,
  france.bl2,
  germany.bl2,
  greece.bl2,
  ireland.bl2,
  italy.bl2,
  luxembourg.bl2,
  netherlands.bl2,
  portugal.bl2,
  spain.bl2,
  datalist.bl1,
  datalist.bl2,
  overwrite = TRUE)
