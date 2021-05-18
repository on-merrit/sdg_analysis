Sys.setenv(SPARK_HOME="/usr/hdp/current/spark2-client")
library(sparklyr)

config <- spark_config()
config$spark.executor.cores <- 10
config$spark.executor.instances <- 5
config$spark.executor.memory <- "30G"
sc <- spark_connect(master = "yarn-client", config = config)

library(tidyverse)
library(arrow)

papers <- spark_read_csv(sc, "/user/tklebel/sdg/data/sdg_papers_collated.csv",
                         name = "papers")
affils <- spark_read_csv(sc,
                         "/user/tklebel/sdg/data/affiliations_with_country.csv",
                         name = "affils")
author_metadata <- spark_read_csv(sc,
                                  "/user/tklebel/sdg/data/sdg_author_data.csv",
                                  name = "author_metadata")
author_paper_affiliations <- spark_read_csv(
  sc,
  "/user/tklebel/sdg/data/sdg_author_paper_affil.csv",
  name = "author_paper_affiliations"
)
funded_projects <- spark_read_csv(
  sc,
  "/user/tklebel/sdg/data/openaire_funders_injoin_w_sdg.csv",
  name = "funded_projects"
)
wb_indicators <- spark_read_csv(
  sc,
  "/user/tklebel/sdg/data/world_bank_indicators.csv",
  name = "wb_indicators")
leiden <- spark_read_csv(sc,
                         "/user/tklebel/sdg/data/leiden_ranking.csv",
                         name = "leiden")


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




