# setup -----
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


# explore duplicates ------
papers %>%
  distinct(paperid) %>%
  count()

# todo: fix look into this discrepancy
duplicated_papers <- papers %>%
  count(paperid, fos_displayname) %>%
  filter(n > 1) %>%
  collect()
# questions: how do some have an NA paperid?
# why are there duplicates?
papers %>%
  filter(paperid == 2803628678) %>%
  collect() %>%
  View()
# one reason for "duplication" is being relevant to multiple fos (virus being
# part of Medicine, mainly)
# but to some part there are real duplicates here
papers %>%
  filter(paperid == 295256) %>%
  collect() %>%
  distinct()



# aggregate oa_status
oa_status <- papers %>%
  filter(!is.na(is_oa)) %>%
  select(paperid, fos_displayname, year, is_oa, oa_status) %>%
  distinct()


oa_per_year <- oa_status %>%
  count(fos_displayname, year, is_oa) %>%
  collect()

oa_status_per_year <- oa_status %>%
  count(fos_displayname, year, oa_status) %>%
  collect()

oa_per_year %>%
  mutate(oa_share = n/sum(n)) %>%
  filter(is_oa == 1) %>%
  ggplot(aes(as.factor(year), oa_share, colour = fos_displayname,
             group = fos_displayname)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by Field of Study",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")


oa_status_per_year %>%
  filter(oa_status %in% c("bronze", "closed", "gold", "green", "hybrid")) %>%
  mutate(oa_share = n/sum(n)) %>%
  ggplot(aes(as.factor(year), oa_share, colour = oa_status,
             group = oa_status)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(fos_displayname)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by Field of Study",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")

# swapping facets with oclours
oa_status_per_year %>%
  filter(oa_status %in% c("bronze", "closed", "gold", "green", "hybrid")) %>%
  mutate(oa_share = n/sum(n)) %>%
  ggplot(aes(as.factor(year), oa_share, colour = fos_displayname,
             group = fos_displayname)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(oa_status)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by Field of Study",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")

# the rise in oa is thus mainly due to rise in gold OA, slightly rise in hybrid
# unpaywall might always prefer gold over green

## OA per funder ------
paper_oa_flag <- papers %>%
  select(paperid, is_oa, oa_status, year, fos_displayname)

oa_per_funder <- funded_projects %>%
  left_join(paper_oa_flag) %>%
  collect()

oa_per_funder

oa_per_funder %>%
  count(funder_name, sort = TRUE) %>% View()

oa_per_funder_aggregated <- oa_per_funder %>%
  select(doi, funder_name, year, is_oa) %>%
  distinct() %>% # remove duplicate rows since many papers are funded by multiple projects
  group_by(year, funder_name) %>%
  count(is_oa) %>%
  mutate(is_oa = recode(is_oa, `0` = "no", `1` = "yes", .missing = "unknown")) %>%
  filter(is_oa != "unknown") %>%
  mutate(oa_share = n/sum(n),
         total_papers = sum(n))

# why do so many have no year?
# the issue was that I only joined the climate papers

# -> obvs the results are now heavily influenced by medicine

text_labels <- oa_per_funder_aggregated %>%
  group_by(funder_name) %>%
  summarise(nn_papers = sum(total_papers)) %>%
  mutate(label = glue::glue("n = {format(nn_papers, big.mark = '.', decimal.mark = ',')}"))


oa_per_funder_aggregated %>%
  filter(is_oa == "yes") %>%
  ggplot(aes(lubridate::ymd(year, truncated = 2L), oa_share,
             group = funder_name)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x = lubridate::ymd("2017-01-01"),
                y = .4,
                label = label),
            data = text_labels) +
  facet_wrap(vars(str_wrap(funder_name, 50))) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by funder",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")

# share of OA going down for CA institutes of health


## OA per country
