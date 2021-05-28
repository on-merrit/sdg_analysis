library(tidyverse)

wbd <- read_csv("data/external/WDIData.csv")
wb_countries <- read_csv("data/external/WDICountry.csv")

# indicators of interest
# GDP (constant 2010 US$) -> NY.GDP.MKTP.KD
# GDP per capita (constant 2010 US$) -> NY.GDP.PCAP.KD
# Individuals using the Internet (% of population) -> IT.NET.USER.ZS
# Research and development expenditure (% of GDP) -> GB.XPD.RSDV.GD.ZS
# Researchers in R&D (per million people) -> SP.POP.SCIE.RD.P6
#
# alternatively could also use current US$ values:
# GDP per capita (current US$) -> NY.GDP.PCAP.CD

indicators <- c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD", "IT.NET.USER.ZS",
                "GB.XPD.RSDV.GD.ZS", "SP.POP.SCIE.RD.P6")

wbd_long <- wbd %>%
  filter(`Indicator Code` %in% indicators) %>%
  pivot_longer(`1960`:`2020`, names_to = "year") %>%
  select(-X66) %>%
  set_names(names(.) %>% str_to_lower() %>% str_replace("\\s", "_")) %>%
  drop_na(value)


wbd_long %>%
  filter(country_name == "Austria") %>% View()

wbd_long %>%
  write_csv("data/processed/world_bank_indicators.csv")

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/processed/sdg_mag.db")
copy_to(con, wbd_long, "world_bank_indicators",
        temporary = FALSE,
        indexes = list(
          "country_code", "indicator_code", "year"
        ))
DBI::dbDisconnect(con)
