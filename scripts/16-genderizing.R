library(tidyverse)
library(genderizeR)

## Genderizing ------
uniqe_names <- read_csv("data/processed/unique_names.csv")

nr <- nrow(uniqe_names)
n <- round(nr/50) # 50 for actually running it, 50000 for debugging

# https://stackoverflow.com/a/37145960/3149349
splits <- uniqe_names %>%
  mutate(split_seq = rep(1:ceiling(nr/n), each = n, length.out = nr))

count(splits, split_seq)


run_genderizing <- function(df, dir, apikey = NULL, total_groups) {
  res <- df %>%
    pull(first_name) %>%
    findGivenNames(textPrepare = FALSE, apikey = apikey, progress = FALSE)

  group <- pull(df, split_seq) %>% unique()

  path <- paste0(dir, "genderized_names-", group, ".csv")

  write_csv(res, file = path)

  log_string <- paste0(Sys.time(), " --- Finished genderizing group ", group,
                       "/", total_groups, ".")
  write_lines(log_string, "genderizing_log", append = TRUE)
}

splits %>%
  split(.$split_seq) %>%
  #head(2) %>%
  walk(run_genderizing, "data/processed/genderized_names/",
       total_groups = round(nr/n), apikey = Sys.getenv("genderize_api"))





result <- list.files("data/processed/genderized_names/", full.names = TRUE) %>%
  map_dfr(read_csv, col_types = cols(
    name = col_character(),
    gender = col_character(),
    probability = col_double(),
    count = col_double(),
    country_id = col_character()
  ))
