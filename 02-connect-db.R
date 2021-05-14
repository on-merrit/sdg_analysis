library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/processed/sdg_mag.db")

DBI::dbListTables(con)


affils <- tbl(con, "affils_w_country")
author_metadata <- tbl(con, "author_metadata")
author_paper_affiliations <- tbl(con, "author_paper_affil")
papers <- tbl(con, "papers")
funded_projects <- tbl(con, "funded_projects")

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


# # setting up tables
# DBI::dbExecute(
#   con,
#   "CREATE TABLE author_metadata (
#     authorid bigint,
#     author_normalizedname varchar,
#     author_displayname varchar,
#     lastknownaffiliationid bigint,
#     papercount int,
#     year_first_paper int,
#     n_citations int,
#     n_citations_norm float,
#     total_co_authors int,
#     mean_co_authors float
#   );")
#
#
#
# DBI::dbRemoveTable(con, "author_metadata")

DBI::dbDisconnect(con)


# steps for import
# double click on sqlite exe
# .open sdg_mag.db
# .mode csv
# .import filename tablename
# potentially create indices: https://www.sqlitetutorial.net/sqlite-index/
# .quit
