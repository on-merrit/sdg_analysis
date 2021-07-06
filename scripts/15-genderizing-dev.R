# remotes::install_github("tklebel/genderizeR")

Sys.setenv(SPARK_HOME = "/usr/hdp/current/spark2-client")
library(sparklyr)
library(tidyverse)
library(arrow)
library(genderizeR)
library(dtplyr)
source(here::here("R/helpers.R"))

config <- spark_config()
config$spark.executor.cores <- 15
config$spark.executor.instances <- 5
config$spark.executor.memory <- "60G"
sc <- spark_connect(master = "yarn-client", config = config,
                    app_name = "genderizing")

author_metadata <- spark_read_csv(sc,
                                  "/user/tklebel/sdg/data/sdg_author_data.csv",
                                  name = "author_metadata")


# trying out the API
x = c("Winston J. Durant, ASHP past president, dies at 84",
      "JAN BASZKIEWICZ (3 JANUARY 1930 - 27 JANUARY 2011) IN MEMORIAM",
      "Maria Sklodowska-Curie")

# Search for terms that could be first names
# If you have your API key you can authorize access to the API with apikey argument
# e.g. findGivenNames(x, progress = FALSE, apikey = 'your_api_key')
givenNames = findGivenNames(x, progress = FALSE)

givenNames = givenNames[count > 100]
givenNames
genderize(x, genderDB = givenNames, progress = TRUE)

# splicing first names -----
# how many authors do we have?
n_authors <- author_metadata %>%
  count() %>%
  collect() %>%
  pull(n)
n_authors

# get about 100.000 authors
dev_set <- author_metadata %>%
  sdf_sample(fraction = 1e+5/n_authors, replacement = FALSE, seed = 79823679) %>%
  select(starts_with("author"))

# https://stackoverflow.com/a/44660625/3149349
names_extracted <- dev_set %>%
  mutate(first_name = regexp_extract(author_normalizedname,
                                     "(\\\\w{2,}?)\\\\s", 1))

names_extracted %>%
  sdf_distinct(first_name) %>%
  count()
# half of them are distinct names

the_names <- names_extracted %>%
  sdf_distinct(first_name) %>%
  collect()

spark_disconnect(sc)


## genderize the names
the_names_sample <- the_names %>%
  head(100)
write_csv(the_names_sample, "data/processed/genderizing_sample.csv")

genderized_names <- the_names_sample %>%
  pull(first_name) %>%
  findGivenNames(textPrepare = FALSE)

genderized_names


gender_data <- lazy_dt(genderized_names)

gender_data %>%
  as_tibble()


# workflow:
# - create new file on disk that stores the authorid, full name and short name (gender key)
# - from this, create a file with all the distinct names and their gender (gender data)
#
# for analysis one then goes: author -> join gender key -> join gender data

# which names to include in analysis?
# - which cutoff for count? 50, 100, 200, XXX?
# - cutoff in terms of probability or use weights?
# maybe 100 names and 90% probability?
# anyways, could change afterwards, since data will be on disk available
#
# steps:
# - create gender key
# - see how many unique names we have
# - buy API key
# - set up pipeline and run genderizing


# create gender key -----
# get about 100.000 authors
authors <- author_metadata %>%
  select(starts_with("author"))

# https://stackoverflow.com/a/44660625/3149349
names_extracted <- authors %>%
  mutate(first_name = regexp_extract(author_normalizedname,
                                     "(\\\\w{2,}?)\\\\s", 1))

names_extracted %>%
  spark_write_csv("/user/tklebel/sdg/data/gender_key.csv", mode = "overwrite")

# find unique names -----
names_extracted %>%
  select(first_name) %>%
  sdf_distinct() %>%
  count()
# 509.732 --> the mid tier API should suffice

names_extracted %>%
  select(first_name) %>%
  sdf_distinct() %>%
  spark_write_csv("/user/tklebel/sdg/data/unique_names.csv", mode = "overwrite")

names_extracted %>%
  select(first_name) %>%
  sdf_distinct() %>%
  collect() %>%
  write_csv("data/processed/unique_names.csv")

## Genderizing ------
uniqe_names <- read_csv("data/processed/unique_names.csv")

nr <- nrow(uniqe_names)
n <- round(nr/50000) # 50 for actually running it

# https://stackoverflow.com/a/37145960/3149349
splits <- uniqe_names %>%
  mutate(split_seq = rep(1:ceiling(nr/n), each = n, length.out = nr))

count(splits, split_seq)

#
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
  head(2) %>%
  walk(run_genderizing, "data/processed/genderized_names/",
       total_groups = round(nr/n))





result <- list.files("data/processed/genderized_names/", full.names = TRUE) %>%
  map_dfr(read_csv, col_types = cols(
    name = col_character(),
    gender = col_character(),
    probability = col_double(),
    count = col_double(),
    country_id = col_character()
  ))


