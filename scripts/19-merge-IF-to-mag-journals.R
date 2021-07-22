library(tidyverse)
# remotes::install_github("ikashnitsky/sjrdata")
library(sjrdata)
library(eulerr)

journals <- read_csv("data/processed/mag_journals_w_doaj.csv")

journals_with_issn <- journals %>%
  filter(!is.na(issn))

sjr <- sjr_journals %>%
  mutate(issn = if_else(str_detect(issn, "^-$"), NA_character_, issn))

sjr_issn <- sjr %>%
  filter(!is.na(issn)) %>%
  distinct(issn, title) %>%
  separate(issn, c("issn1", "issn2"), remove = FALSE) %>%
  mutate(across(matches("issn\\d"),
                # adding something within a string
                # https://stackoverflow.com/a/13863762/3149349
                .fns = str_replace, "^(.{4})(.+)$", "\\1-\\2")) %>%
  rename(issn_sjr = issn)


#
# step1 <- journals_with_issn %>%
#   left_join(sjr_issn, by = c("issn" = "issn1"))
#
# step2 <- step1 %>%
#   filter(is.na(title)) %>%
#   select(-c(title:issn2)) %>%
#   left_join(sjr_issn, by = c("issn" = "issn2"))
#
# full_joined <- step1 %>%
#   filter(!is.na(title)) %>%
#   bind_rows(step2)
#
# full_joined %>%
#   count(journalid, sort = TRUE) %>%
#   slice(1:10) %>%
#   left_join(full_joined) %>%
#   View()


# need to solve these duplicates
# probably: exlude all journals from sjr which have multiple titles under the
# same issn

sjr_issn_dupes_checked <- sjr_issn %>%
  group_by(issn1) %>%
  mutate(has_duplicated_titles_issn1 = case_when(is.na(issn1) ~ FALSE,
                                                 length(unique(title)) > 1 ~ TRUE,
                                                 TRUE ~ FALSE)) %>%
  group_by(issn2) %>%
  mutate(has_duplicated_titles_issn2 = case_when(is.na(issn2) ~ FALSE,
                                                 length(unique(title)) > 1 ~ TRUE,
                                                 TRUE ~ FALSE)) %>%
  ungroup() %>%
  mutate(has_duplicated_titles = has_duplicated_titles_issn1 | has_duplicated_titles_issn2)


sjr_issn_dupes <- sjr_issn_dupes_checked %>%
  filter(has_duplicated_titles)


dupes_if <- sjr_issn_dupes %>%
  left_join(sjr, by = c("issn_sjr" = "issn", "title" = "title")) %>%
  group_by(issn_sjr) %>%
  summarise(mean_if = mean(cites_doc_2years))

dupes_if %>%
  summarise(across(mean_if, .fns = list(min = min, mean = mean, med = median, max = max)))

dupes_if %>%
  ggplot(aes(mean_if)) +
  geom_histogram()
# very highly ranking journals are rare with these duplicates, so this is fine

sjr_issn_dedup <- sjr_issn_dupes_checked %>%
  filter(!has_duplicated_titles) %>%
  select(-starts_with("has_dupl"))

# join again
step1 <- journals_with_issn %>%
  left_join(sjr_issn_dedup, by = c("issn" = "issn1"))

step2 <- step1 %>%
  filter(is.na(title)) %>%
  select(-c(title:issn2)) %>%
  left_join(sjr_issn_dedup, by = c("issn" = "issn2"))

full_joined <- step1 %>%
  filter(!is.na(title)) %>%
  bind_rows(step2)

full_joined %>%
  count(journalid, sort = TRUE) %>%
  slice(1:10) %>%
  left_join(full_joined) %>%
  View()


# remove the journal of Rhumatology, since it is still duplicated
# this anyways only existed until 2004, so it will not be in our data
full_joined <- full_joined %>%
  filter(!str_detect(issn_sjr, "02497581, 00354929") | is.na(issn_sjr))

# merge sjr data
sjr_joined_w_data <- full_joined %>%
  select(issn_sjr, title) %>%
  left_join(sjr, by = c("issn_sjr" = "issn", "title" = "title"))

# keep only the most recent year
sjr_joined_w_data_distinct <- sjr_joined_w_data %>%
  group_by(issn_sjr, title) %>%
  mutate(sjr_most_recent_year = max(as.numeric(year))) %>%
  filter(year == sjr_most_recent_year) %>%
  select(-year) %>%
  rename(sjr_rank = rank, publisher_sjr = publisher) %>%
  # somewho some journals are complete duplicates, so remove them
  distinct()

full_joined_with_sjr_data <- full_joined %>%
  select(-issn2, -issn1) %>%
  left_join(sjr_joined_w_data_distinct, by = c("issn_sjr", "title"))


all_journals_joined <- journals %>%
  filter(is.na(issn)) %>%
  bind_rows(full_joined_with_sjr_data)

# check if we have the same number of journals
identical(nrow(journals),
          nrow(all_journals_joined))


all_journals_joined %>%
  arrange(journalid) %>%
  write_csv("data/processed/mag_journals_w_doaj_w_sjr.csv")

# do a venn diagram at the end (look for code from nick fraser):
#   all mag journals
#   all doaj
#   all sjr
#   overlaps
#   have to create a file like this: https://github.com/nicholasmfraser/biorxiv/blob/3/data/analysis/matching_overlap.csv
#   means: need to look at all doaj, all sjr, all mag
#   but: didnt attempt overlap between doaj and sjr - don't think I can do a
#   proper venn diagram here


all_journals_joined %>%
  summarise(mag_doaj_sjr = sum(!is.na(APC) & !is.na(title)),
            mag_doaj = sum(!is.na(APC)),
            mag_sjr = sum(!is.na(title)),
            mag = n()) %>%
  pivot_longer(everything()) %>%
  mutate(prop = value / max(value))
#> # A tibble: 4 x 3
#> name           value   prop
#> <chr>          <int>  <dbl>
#> 1 mag_doaj_sjr  3383 0.0690
#> 2 mag_doaj      6192 0.126
#> 3 mag_sjr      22220 0.453
#> 4 mag          49010 1
