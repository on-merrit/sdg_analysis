library(tidyverse)
# remotes::install_github("ikashnitsky/sjrdata")
library(sjrdata)


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
