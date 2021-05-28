library(tidyverse)


affil_cols <- cols(
  affiliationid = col_double(), #big ints cannot be directly represented in R
  rank = col_double(),
  normalizedname = col_character(),
  displayname = col_character(),
  gridid = col_character(),
  officialpage = col_character(),
  wikipage = col_character(),
  papercount = col_double(),
  citationcount = col_double(),
  latitude = col_double(),
  longitude = col_double(),
  createddate = col_date(format = "")
)

affils <- read_tsv("data/raw/Affiliations.txt", col_types = affil_cols, col_names = names(affil_cols$cols))

affil_country_code <- read_csv(
  "data/external/affiliations_countrycode.csv",
  col_types = "ddc"
) %>%
  select(-X1)


affils_w_country <- affils %>%
  left_join(affil_country_code)

write_csv(affils_w_country, "data/processed/affiliations_with_country.csv")

affils_w_country %>%
  select(displayname, wikipage, country, latitude, longitude) %>%
  View()

affils_w_country %>%
  count(country)
