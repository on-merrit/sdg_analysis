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

affils <- spark_read_csv(sc,
                         "/user/tklebel/sdg/data/affiliations_with_country.csv",
                         name = "affils")

# trying to collect the leiden ranking from spark fails due to a JAva heap space
# error
# leiden <- spark_read_csv(sc,
#                          "/user/tklebel/sdg/data/leiden_ranking.csv",
#                           name = "leiden")

affils_local <- collect(affils)
leiden_local <- read_csv("data/external/leiden_ranking.csv")





leiden_small <- leiden_local %>%
  filter(Field == "All sciences", Period == "2015–2018",
         Frac_counting == 0) %>%
  select(University, Country)

leiden_small_for_match <- leiden_small %>%
  mutate(university_normalized = str_to_lower(University) %>%
           stringi::stri_trans_general(id = "latin-ascii"))


mag_affils_for_match <- affils_local %>%
  select(affiliationid, normalizedname, displayname) %>%
  mutate(university_normalized = str_to_lower(displayname) %>%
           stringi::stri_trans_general(id = "latin-ascii"))


joined_affils <- mag_affils_for_match %>%
  left_join(leiden_small_for_match, by = "university_normalized")

write_csv(joined_affils, "data/processed/leiden_matched.csv")

unmatched_leiden <- leiden_small_for_match %>%
  anti_join(joined_affils)

unmatched_leiden %>%
  write_csv("data/processed/leiden_unmatched.csv")


joined_affils %>%
  write_csv("data/processed/all_affils.csv")

spark_disconnect(sc)

