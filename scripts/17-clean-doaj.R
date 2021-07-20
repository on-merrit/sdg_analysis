library(tidyverse)
library(quantmod)

# load datasets -----

doaj <- read_csv("data/external/journalcsv__doaj_20210719_0635_utf8.csv")

doaj_selected <- doaj %>%
  select(journal_title = `Journal title`, alt_title = `Alternative title`,
         pissn = `Journal ISSN (print version)`,
         eissn = `Journal EISSN (online version)`,
         language = `Languages in which the journal accepts manuscripts`,
         publisher = Publisher, country_of_pub = `Country of publisher`,
         APC, APC_amount = `APC amount`,
         waiver = `Journal waiver policy (for developing country authors etc)`,
         date_added_to_doaj = `Added on Date`) %>%
  mutate(across(c(APC, waiver), .fns = ~if_else(.x == "Yes", TRUE, FALSE)))

# goal: having the MAG journals together with APC and IF data
# steps:
# - start with mag journals
# - add doaj data
# - generate IF data: only current one possible from scholar?
# -- get data from sjr instead


# convert currencies to USD ----
# if amount is given as USD, take that. Otherwise take the first
with_currency <- doaj_selected %>%
  mutate(has_dollars = str_detect(APC_amount, "USD"),
         amount = case_when(
           has_dollars ~ str_extract(APC_amount, "\\d+(?=\\sUSD)"),
           TRUE ~ str_extract(APC_amount, "\\d+")
         ),
         currency = case_when(
           has_dollars ~ "USD",
           TRUE ~ str_extract(APC_amount, "(?<=\\d{1,10}\\s)[A-Z]+")
         )) %>%
  mutate(amount = as.numeric(amount))

# check whether it worked
with_currency %>%
  select(APC_amount, amount, currency) %>%
  filter(!is.na(APC_amount)) %>%
  summarise(across(.fns = ~any(is.na(.x))))


# get conversion factors
currencies_to_convert <- with_currency %>%
  filter(!is.na(currency), currency != "USD") %>%
  pull(currency) %>%
  unique()

get_conversion_factor <- function(currency) {
  currency_date <- as.Date("2021-07-19")

  res <- getFX(paste0(currency, "/USD"),
               from = currency_date, to = currency_date,
               auto.assign = FALSE)

  Sys.sleep(.5)
  as.numeric(res)
}

# does it work?
get_conversion_factor("EUR")

conversions <- tibble(currency = currencies_to_convert) %>%
  # head(2) %>%
  mutate(conversion_factor = map_dbl(currency, get_conversion_factor))

# convert APC values for all journals
doaj_converted_all <- with_currency %>%
  left_join(conversions) %>%
  mutate(APC_in_dollar = case_when(
    has_dollars ~ amount,
    TRUE ~ amount * conversion_factor
  ))

# remove columns which were used for conversion
doaj_converted <- doaj_converted_all %>%
  select(-APC_amount, -has_dollars, -amount, -currency, -conversion_factor)

# data might have some issues:
# - conversion might be incorrect du to outdated information in DOAJ, but
# current exchange rate
# - information in DOAJ might be wrong or misleading. E.g. "The Korean Journal
#  of Gastroenterology" has an APC of 10.000KRW **per page**, but DOAJ only
#  lists 10.000KRW as the APC.
# - only taking the USD value might also be misleading, the journal "الاكاديمي"
# charges 125,000 iraqi dinars, which is as of today 85,75USD, but for non-iraqi
# researchers it charges 150 USD. On top of that, the values are swapped in DOAJ
# - Tsaqafah does not have APCs any more


# df used to identify the journal and find the website
doaj_apc <- doaj %>%
  select(`Journal title`, `APC information URL`, `APC amount`,
         `Journal ISSN (print version)`)

# fix a few coding error from DOAJ
doaj_converted <- doaj_converted %>%
  mutate(APC_in_dollar = case_when(
    pissn == "1819-5229"~ 150,
    journal_title == "Tsaqafah" ~ 0,
    pissn == "1979-7788" ~ 0,
    TRUE ~ APC_in_dollar),
         APC = case_when(APC_in_dollar == 0 ~ FALSE,
                         TRUE ~ APC))


doaj_converted %>%
  write_csv("data/processed/doaj_cleaned.csv")
