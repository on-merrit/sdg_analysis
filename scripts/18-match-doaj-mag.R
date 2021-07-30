library(tidyverse)

journals <- read_tsv("data/external/mag_2021_journals.txt",
                     col_types = cols(
                       journalid = col_double(),
                       rank = col_double(),
                       normalizedname = col_character(),
                       displayname = col_character(),
                       issn = col_character(),
                       publisher = col_character(),
                       webpage = col_character(),
                       papercount = col_double(),
                       paperfamilycount = col_double(),
                       citationcount = col_double(),
                       createddate = col_date(format = "")
                     ),
                     col_names = c(
                       "journalid", "rank", "normalizedname", "displayname",
                       "issn", "publisher", "webpage", "papercount",
                       "paperfamilycount", "citationcount", "createddate"
                     ))

doaj <- read_csv("data/processed/doaj_cleaned.csv")



# merge doaj journals to mag journals -----
# steps:
# - first join be eissn, then by issn, then by name
# - for name normalization: lowercase, remove all non-word characters except
# spaces and "&"


doaj_for_matching <- doaj %>%
  select(-publisher)

journals_with_issn <- journals %>%
  filter(!is.na(issn))

joined <- journals_with_issn %>%
  left_join(doaj_for_matching, by = c("issn" = "eissn"))

joined %>%
  summarise(n = n(),
            n_doaj = sum(!is.na(APC)))
# we matched 2935 journals here

mag_issn_remaining <- joined %>%
  filter(is.na(APC)) %>%
  select(-c(journal_title:APC_in_dollar))

doaj_remaining <- joined %>%
  select(journal_title) %>%
  drop_na() %>%
  anti_join(doaj_for_matching, .)

joined2 <- mag_issn_remaining %>%
  left_join(doaj_remaining, by = c("issn" = "pissn"))

joined2 %>%
  summarise(n = n(),
            n_doaj = sum(!is.na(APC)))
# joined 3527 in this step

joined_by_issn <- joined %>%
  filter(!is.na(APC)) %>%
  bind_rows(joined2) %>%
  filter(!is.na(APC)) %>%
  select(-eissn)

joined_by_issn %>%
  distinct(journalid) %>%
  nrow() %>%
  all.equal(nrow(joined_by_issn))
# we didnt create duplicates here, which is good!


# prepare for name joining
all_journals_midstep <- journals %>%
  full_join(joined_by_issn)

unjoined_doaj <- all_journals_midstep %>%
  select(journal_title) %>%
  drop_na() %>%
  anti_join(doaj_for_matching, .)


all_journals_midstep %>%
  summarise(n_joined = sum(!is.na(APC)))
# in sum already joined 6192

still_unjoined <- all_journals_midstep %>%
  filter(is.na(APC)) %>%
  select(-c(journal_title:APC_in_dollar))

name_joined <- still_unjoined %>%
  left_join(unjoined_doaj, by = c("displayname" = "journal_title"))

# joining and inspecting one journal that is duplicated
join_complete <- joined_by_issn %>%
  bind_rows(name_joined)

dupes <- join_complete %>%
  count(journalid, sort = TRUE) %>%
  filter(n > 1) %>%
  left_join(join_complete)
# are any of the dupes in the issn set?

joined_by_issn %>%
  select(journalid) %>%
  inner_join(dupes) %>%
  nrow()
# nope, none of them are in the issn set

# we simply delete the DOAJ information for these duplicates, since we cannot be
# sure what is going on here

dupes_fixed <- dupes %>%
  select(journalid:createddate) %>%
  distinct()

mag_doaj_final <- join_complete %>%
  anti_join(dupes) %>%
  bind_rows(dupes_fixed)


# notes on why matched number from DOAJ might be low:
# - issues with eissn vs print issn - inconsistent use (but tried to mitigate)
# - Mirror journals:  e.g. Materials Letters: X. MAG has Materials Letters,
#   which is the paywall version, but the
#   OA subjournal it does not have as a recognized entity (it does have it by
#   name, but then again, name matching does not work easily)
# - DOAJ journals tend to be young (inclusion into DAOJ might be important for
#   them), but MAG often does not have younger entities.

write_csv(mag_doaj_final, "data/processed/mag_journals_w_doaj.csv")


