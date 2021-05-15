library(tidyverse)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/processed/sdg_mag.db")

DBI::dbListTables(con)

affils <- tbl(con, "affils_w_country")
papers <- tbl(con, "papers")
author_paper_affiliations <- tbl(con, "author_paper_affil")
leiden <- readxl::read_excel("data/external/CWTS Leiden Ranking 2020.xlsx",
                             sheet = "Results")

leiden_small <- leiden %>%
  filter(Field == "All sciences", Period == "2015–2018",
         Frac_counting == 0) %>%
  select(University, Country, impact_P)


author_paper_affiliations %>%
  distinct(affiliationid) %>%
  filter(!is.na(affiliationid)) %>%
  left_join(affils) %>%
  select(affiliationid, normalizedname, displayname, country) %>%
  collect() -> sdg_affils

joined_affils <- sdg_affils %>%
  left_join(leiden_small, by = c("displayname" = "University"))

nrow(leiden_small)

joined_affils %>%
  summarise(matched_affils = sum(!is.na(Country)),
            unmatched = nrow(leiden_small) - matched_affils)


# todo here would be to manually draw up a table (two cols) to match these
# 250 institutions

copy_to(con, leiden, "leiden_ranking",
        temporary = FALSE,
        indexes = list(
          "University", "Field", "Country", "Period", "Frac_counting"
        )
)


# it could be that there are actually some that did not appear in our set. so
# maybe we need to match less

# possible after importing leiden into sqlite
leiden <- tbl(con, "leiden_ranking")
affils <- tbl(con, "affils_w_country")

leiden_small <- leiden %>%
  filter(Field == "All sciences", Period == "2015–2018",
         Frac_counting == 0) %>%
  select(University, Country, impact_P)
leiden_small

all_matched <- affils %>%
  distinct(affiliationid, normalizedname, displayname, country_code = country) %>%
  filter(!is.na(affiliationid)) %>%
  left_join(leiden_small, by = c("displayname" = "University"))

all_matched %>%
  summarise(matched_affils = sum(!is.na(Country)))
# nope, this is the same number.
# but this also means, that our sdg set covers at least all institutions in the
# leiden ranking

DBI::dbDisconnect(con)

