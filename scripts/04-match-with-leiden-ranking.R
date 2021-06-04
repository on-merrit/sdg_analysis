Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
source(here::here("R/helpers.R"))

message("Connecting to spark...")

config <- spark_config()
config$spark.executor.cores <- 2
config$spark.executor.instances <- 1
config$spark.executor.memory <- "6G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "leiden")
message("Connection to Spark successful!")

message("Reading the datasets...")
affils <- spark_read_csv(sc,
                         "/user/tklebel/sdg/data/affiliations_with_country.csv",
                         name = "affils")

leiden <- read_csv("data/external/leiden_ranking.csv")


leiden_small <- leiden %>%
  filter(Field == "All sciences", Period == "2015–2018",
         Frac_counting == 0) %>%
  select(University, Country, impact_P)

leiden_small



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



