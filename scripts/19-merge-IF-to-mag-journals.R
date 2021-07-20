library(tidyverse)
# remotes::install_github("ikashnitsky/sjrdata")
library(sjrdata)

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



step1 <- journals_with_issn %>%
  left_join(sjr_issn, by = c("issn" = "issn1"))

step2 <- step1 %>%
  filter(is.na(title)) %>%
  select(-c(title:issn2)) %>%
  left_join(sjr_issn, by = c("issn" = "issn2"))

full_joined <- step1 %>%
  filter(!is.na(title)) %>%
  bind_rows(step2)

full_joined %>%
  count(journalid, sort = TRUE) %>%
  slice(1:10) %>%
  left_join(full_joined) %>%
  View()


# need to solve these duplicates
# probably: exlude all journals from sjr which have multiple titles under the
# same issn

# do a venn diagram at the end (look for code from nick fraser):
#   all mag journals
#   all doaj
#   all sjr
#   overlaps
