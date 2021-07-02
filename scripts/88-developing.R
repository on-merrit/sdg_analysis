t1 <- papers %>%
  filter(year %in% 2015:2018,
         !is.na(is_oa)) %>%
  left_join(author_paper_affiliations_w_groups) %>%
  left_join(author_metadata) %>%
  filter(paper_author_cat != "single", SDG_label == "SDG_13") %>%
  filter(author_position %in% c("first_author", "last_author")) %>%
  # we need distinct because some authors have multiple affiliations and therefore
  # show up twice
  distinct(paperid, authorid, year, is_oa, is_funded, authorid,
         authorsequencenumber, paper_author_cat, author_position,
         year_first_paper) %>%
  collect()

# take sample
set.seed(2093784)
our_sample <- t1 %>%
  distinct(paperid) %>%
  slice_sample(n = 20000)

df <- our_sample %>%
  left_join(t1) %>%
  group_by(paperid, author_position) %>%
  summarise(is_funded = is_funded,
            is_oa = is_oa,
            age = year - year_first_paper) %>%
  pivot_wider(names_from = author_position, values_from = age,
              names_sep = ".") %>%
  summarise(paperid = paperid,
            is_funded = is_funded,
            is_oa = is_oa,
            across(ends_with("author"), ~.x[!is.na(.x)])) %>%
  distinct() %>%
  ungroup()


# truncate ages
df_trunc_age <- df %>%
  filter(first_author %in% 1:50 & last_author %in% 1:50)


# center and standardize coefficients
standardize_vec <- function(x) {
  x_mean <- x - mean(x, na.rm = TRUE)
  x_standard <- x_mean / (2*sd(x, na.rm = TRUE))
  x_standard
}

df_trunc_age <- df_trunc_age %>%
  mutate(across(contains("author"), .fns = standardize_vec))

mod1 <- glm(is_oa ~ is_funded + first_author + last_author, data = df_trunc_age,
            family = "binomial")

summary(mod1)
exp(coef(mod1))





t1 <- papers %>%
  head(200) %>%
  filter(year %in% 2015:2018,
         !is.na(is_oa)) %>%
  left_join(author_paper_affiliations_w_groups) %>%
  left_join(author_metadata) %>%
  filter(paper_author_cat != "single") %>%
  select(paperid, authorid, year, is_oa, is_funded, SDG_label, authorid,
         affiliationid,
         authorsequencenumber, paper_author_cat, author_position,
         year_first_paper) %>%
  filter(author_position %in% c("first_author", "last_author")) %>%
  group_by(paperid, author_position, is_funded, is_oa) %>%
  summarise(age = year - year_first_paper,
            affiliationid = affiliationid) %>%
  pivot_wider(names_from = author_position, values_from = age) %>%
  summarise(paperid = paperid,
            is_funded = is_funded,
            is_oa = is_oa,
            across(ends_with("author"), ~.x[!is.na(.x)])) %>%
  distinct()



## Model
Split by SDG, only take papers from 2015-2018

DV: OA or not
IVs:

  - paper is funded
- number of authors
- age of first author
- age of last author
- prestige of first author institution
- prestige of last author institution

These could be added, but have an issue with timing.

- normalised citations of first and last
- unique co authors of first and last


