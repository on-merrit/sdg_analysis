library(tidyverse)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/processed/sdg_mag.db")

DBI::dbListTables(con)


affils <- tbl(con, "affils_w_country")
author_metadata <- tbl(con, "author_metadata")
author_paper_affiliations <- tbl(con, "author_paper_affil")
papers <- tbl(con, "papers")
funded_projects <- tbl(con, "funded_projects")
wb_indicators <- tbl(con, "world_bank_indicators")
leiden <- tbl(con, "leiden_ranking")


papers %>%
  filter(fos_displayname == "Climate change") %>%
  left_join(author_paper_affiliations) %>%
  head(5000) %>%
  collect() -> df


df <- author_metadata %>%
  head(200) %>%
  collect()



funded_projects %>%
  count(funder_name, funder_shortname, sort = TRUE)

funded_projects %>%
  left_join(papers) %>%
  select(doi, funder_shortname, funder_name, is_oa, oa_status) %>%
  count(funder_shortname, funder_name, is_oa) %>%
  collect() -> funded_oa



affils %>%
  left_join(wb_indicators, by = c("country" = "country_code")) %>%
  filter(country == "AUT") %>%
  collect() -> at_affils




DBI::dbDisconnect(con)




