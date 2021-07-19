library(tidyverse)
library(scholar)
library(quantmod)
# remotes::install_github("ikashnitsky/sjrdata")
library(sjrdata)


# goal: having the MAG journals together with APC and IF data
# steps:
# - start with mag journals
# - add doaj data
# - generate IF data: only current one possible from scholar?
# -- get data from sjr instead


currency_date <- as.Date("2021-07-18")

res <- getFX("EUR/USD", from = currency_date, to = currency_date,
             auto.assign = FALSE)
res

# create all currendy pairs
# look them up
# join them back
