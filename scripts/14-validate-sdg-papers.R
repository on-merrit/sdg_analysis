library(tidyverse)

sdg_labels_local <- vroom::vroom("../sdg_labels.csv", delim = ",")

# sample a set of papers
set.seed(6749782)
validation_sample <- sdg_labels_local %>%
  group_by(SDG_label) %>%
  slice_sample(n = 33) %>%
  filter(!is.na(paperid))

# add one from SDG 3
validation_sample <- sdg_labels_local %>%
  filter(SDG_label == "SDG_3") %>%
  slice_sample(n = 1) %>%
  bind_rows(validation_sample, .)

# check if we have duplicates
n_distinct(validation_sample$paperid)
# nope

validation_sample %>%
  mutate(applies_to_SDG = "",
         link = paste0("https://academic.microsoft.com/paper/", paperid)) %>%
  write_csv("data/processed/papers_to_validate.csv")
