# setup -----
Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)

config <- spark_config()
config$spark.executor.cores <- 15
config$spark.executor.instances <- 5
config$spark.executor.memory <- "50G"
sc <- spark_connect(master = "yarn-client", config = config, app_name = "SDG_OA")

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





# aggregate oa_status -----
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
  group_by(fos_displayname, year) %>%
  mutate(oa_share = n/sum(n)) %>%
  filter(is_oa) %>%
  ggplot(aes(as.factor(year), oa_share, colour = fos_displayname,
             group = fos_displayname)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by Field of Study",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")
ggsave("plots/oa_by_fos.png")


# swapping facets with oclours
oa_status_per_year %>%
  group_by(fos_displayname, year) %>%
  filter(!is.na(oa_status)) %>%
  mutate(oa_share = n/sum(n)) %>%
  ggplot(aes(lubridate::ymd(year, truncated = 2L), oa_share,
             colour = fos_displayname,
             group = fos_displayname)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(oa_status)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by Field of Study",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")
ggsave("plots/oa_type_by_fos.png")

# the rise in oa is thus mainly due to rise in gold OA, slightly rise in hybrid
# unpaywall might always prefer gold over green

## OA per funder ------
paper_oa_flag <- papers %>%
  select(paperid, is_oa, oa_status, year, fos_displayname)

oa_per_funder <- funded_projects %>%
  left_join(paper_oa_flag)

oa_per_funder

oa_per_funder %>%
  count(funder_name, sort = TRUE)

oa_per_funder_aggregated <- oa_per_funder %>%
  select(doi, funder_name, year, is_oa) %>%
  distinct() %>% # remove duplicate rows since many papers are funded by multiple projects
  group_by(year, funder_name) %>%
  count(is_oa) %>%
  filter(!is.na(is_oa)) %>%
  mutate(oa_share = n/sum(n),
         total_papers = sum(n)) %>%
  collect()

text_labels <- oa_per_funder_aggregated %>%
  group_by(funder_name) %>%
  summarise(nn_papers = sum(total_papers)) %>%
  mutate(label = glue::glue("n = {format(nn_papers, big.mark = '.', decimal.mark = ',')}"))


oa_per_funder_aggregated %>%
  filter(is_oa) %>%
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
ggsave("plots/oa_by_funder.png", width = 16, height = 9)
# share of OA going down for CA institutes of health


## OA per country -----
oa_per_affiliation <- paper_oa_flag %>%
  select(-fos_displayname) %>%
  left_join(author_paper_affiliations) %>%
  right_join(affils)

oa_per_affiliation_selected <- oa_per_affiliation %>%
  group_by(paperid) %>%
  mutate(frac_count = 1 / max(authorsequencenumber, na.rm = TRUE)) %>%
  select(paperid, authorid, is_oa, oa_status, year, country, frac_count) %>%
  filter(!is.na(is_oa))


oa_per_country <- oa_per_affiliation_selected %>%
  group_by(country, is_oa) %>%
  summarise(sum_frac_oa = sum(frac_count)) %>%
  mutate(prop_oa = sum_frac_oa/sum(sum_frac_oa)) %>%
  collect()

order_df <- oa_per_country %>%
  filter(is_oa) %>%
  arrange(desc(prop_oa)) %>%
  mutate(order = seq_along(prop_oa))

oa_per_country %>%
  filter(sum_frac_oa > 1000) %>%
  left_join(order_df) %>%
  ggplot(aes(prop_oa, fct_reorder(country, order, max, na.rm = TRUE), fill = is_oa)) +
  geom_col()



## OA per gdp, % spent on research

wb_local <- wb_indicators %>%
  filter(year == 2018) %>%
  collect()


oa_with_r_d_gdp <- oa_per_country %>%
  left_join(wb_local, by = c("country" = "country_code")) %>%
  select(-indicator_name) %>%
  filter(indicator_code %in% c("GB.XPD.RSDV.GD.ZS", "NY.GDP.MKTP.KD")) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  drop_na() %>%
  filter(is_oa)

oa_with_r_d_gdp %>%
  ggplot(aes(GB.XPD.RSDV.GD.ZS, prop_oa, colour = NY.GDP.MKTP.KD)) +
  geom_point(aes(size = NY.GDP.MKTP.KD), show.legend = FALSE) +
  scale_colour_viridis_c(trans = "log", option = "D", end = .9) +
  ggrepel::geom_text_repel(aes(label = country_name), show.legend = FALSE) +
  labs(x = "Expenditure towards research", y = "OA share") +
  scale_y_continuous(labels = scales::percent)
ggsave("plots/oa_country_rd.png", width = 13, height = 9)


oa_with_gdp_per_cap <- oa_per_country %>%
  left_join(wb_local, by = c("country" = "country_code")) %>%
  select(-indicator_name) %>%
  filter(indicator_code %in% c("GB.XPD.RSDV.GD.ZS", "NY.GDP.PCAP.KD")) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  drop_na() %>%
  filter(is_oa)


oa_with_gdp_per_cap %>%
  ggplot(aes(NY.GDP.PCAP.KD, prop_oa)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10()
ggsave("plots/oa_per_gdp_p_cap.png", width = 12, height = 8)



## OA per academic age ------
author_w_year <- author_paper_affiliations %>%
  left_join(author_metadata) %>%
  select(paperid, authorid, authorsequencenumber, year_first_paper)

papers_w_age <- papers %>%
  # only take papers with OA status for now
  filter(!is.na(oa_status)) %>%
  left_join(author_w_year) %>%
  mutate(current_age = year - year_first_paper)


age_dist <- papers_w_age %>%
  filter(year == 2018) %>%
  distinct(authorid, current_age) %>%
  count(current_age) %>%
  collect()


age_dist %>%
  ggplot(aes(current_age, n)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x,
                                                 big.mark = ".",
                                                 decimal.mark = ",")) +
  labs(y = "Number of researchers", title = "Current age of researchers in 2018",
       caption = "Only taking authors on papers that have an OA status into account")
ggsave("plots/age_overview.png")

age_dist %>%
  View()

age_trunc <- papers_w_age %>%
  filter(current_age <= 70 | current_age > 0)

age_trunc


## exit ----
spark_disconnect(sc)
