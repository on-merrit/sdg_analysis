---
title: Which authors, institutions, nations, regions contribute work on these SDG
  areas (to which extent, and over time, and what characteristics of contributors
  can be observed)?
author: "Thomas Klebel"
date: "04 June, 2021"
output: 
  html_document:
    keep_md: true
---




# SDG over time
## Number of papers


```r
fos_counts <- papers %>% 
  count(fos_displayname, year) %>% 
  collect()
  
fos_counts %>% 
  drop_na() %>% 
  ggplot(aes(as_year(year), n, colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  labs(x = NULL, y = "# of papers", colour = NULL,
       title = "Development of SDG areas over time") 
```

![](03-sdg_who_files/figure-html/sdg_who_by_sdg_count-1.png)<!-- -->
Improvements:
- directly labeling the lines and removing the legend
- using not raw number of papers but % of MAG overall papers


## Citations/papers


```r
fos_citations <- papers %>% 
  group_by(year, fos_displayname) %>% 
  summarise(across(citations_norm, .fns = c(mean = mean, sd = sd))) %>% 
  collect()

fos_citations  %>% 
  drop_na() %>% 
  ggplot(aes(as_year(year), citations_norm_mean, colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "mean normalized citations", colour = NULL,
       title = "Impact of SDG research over time")
```

![](03-sdg_who_files/figure-html/sdg_who_by_sdg_citation-1.png)<!-- -->

```r
fos_citations %>% 
  drop_na() %>% 
  ggplot(aes(as_year(year), citations_norm_sd, colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "SD of normalized citations", colour = NULL,
       title = "Variability of impact of SDG research over time")
```

![](03-sdg_who_files/figure-html/sdg_who_by_sdg_citation-2.png)<!-- -->

Caveats:

- Standard deviation might not make much sense for such a skewed distribution ->
what would be better?


# Academic Age over time
## Counts

```r
author_paper_affiliations_w_groups <- make_author_groups(author_paper_affiliations)

age_base <- papers %>% 
  select(paperid, fos_displayname, year) %>% 
  left_join(author_paper_affiliations_w_groups) %>% 
  left_join(author_metadata) %>% 
  select(paperid, fos_displayname, year, author_position, authorid, 
         year_first_paper)
```

```
## Joining, by = "paperid"
```

```
## Joining, by = "authorid"
```

```r
age_cohorts <- age_base %>% 
  group_by(year, authorid) %>% 
  mutate(current_age = year - year_first_paper) %>% 
  filter(current_age > 0) %>% 
  mutate(age_cohort = case_when(
    year_first_paper < 1960 ~ NA_character_,
    year_first_paper < 1970 ~ "1960-1969",
    year_first_paper < 1980 ~ "1970-1979",
    year_first_paper < 1990 ~ "1980-1989",
    year_first_paper < 2000 ~ "1990-1999",
    year_first_paper < 2010 ~ "2000-2009",
    TRUE ~ "2010-2017"
  )) %>% 
  ungroup()

sdg_age <- age_cohorts %>% 
  count(fos_displayname, year, author_position, age_cohort) %>% 
  collect()
```


```r
sdg_age %>% 
  filter(age_cohort != "NA") %>% 
  ggplot(aes(as_year(year), n, colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  facet_grid(rows = vars(age_cohort), 
             cols = vars(author_position)) +
  scale_y_log10(labels = scales::comma) +
  labs(y = "# of papers", x = NULL, colour = NULL)
```

![](03-sdg_who_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

This is not very meaningful.

Better: simple averages/boxplots for how SDGs compare


```r
age_box_base <- age_cohorts %>% 
  filter(year == 2018, current_age > 0, current_age <= 50) %>% 
  db_compute_boxplot(fos_displayname, current_age)
```


```r
# since the code for plotting the boxplot is currently not exported in the 
# dbplot package, I'm copying it here directly
colnames(age_box_base) <- c(
  "x", "n", "lower", "middle", "upper", "max_raw", "min_raw",
  "iqr", "min_iqr", "max_iqr", "ymax", "ymin"
)

ggplot(age_box_base) +
  geom_boxplot(
    aes(
      x = x,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax,
      group = x,
      fill = x
    ),
    stat = "identity", show.legend = FALSE
  ) +
  labs(x = NULL, y = "Academic age", title = "Academic age as of 2018",
       caption = "Only ages above 0 and below 51 are included")
```

![](03-sdg_who_files/figure-html/sdg_who_age_box-1.png)<!-- -->




```r
mean_ages <- age_cohorts %>% 
  group_by(fos_displayname, year) %>% 
  filter(current_age > 0, current_age <= 50) %>% 
  summarise(mean_age = mean(current_age, na.rm = TRUE)) %>% 
  collect()
```


```r
ggplot(mean_ages, aes(as_year(year), mean_age, colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, colour = NULL, y = "Mean age at publication",
       title = "Author ages over time")
```

![](03-sdg_who_files/figure-html/sdg_who_age_over_time-1.png)<!-- -->


```r
mean_ages_p_position <- age_cohorts %>% 
  group_by(fos_displayname, year, author_position) %>% 
  filter(current_age > 0, current_age <= 50) %>% 
  summarise(mean_age = mean(current_age, na.rm = TRUE)) %>% 
  collect()
```


```r
mean_ages_p_position %>% 
  mutate(author_position = recode(
    author_position,
    first_author = "First author", last_author = "Last author",
    middle_author = "Middle author")) %>% 
  ggplot(aes(as_year(year), mean_age, 
                                 colour = fos_displayname)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(author_position), nrow = 2) +
  theme(legend.position = c(.8, .2)) +
  labs(x = NULL, colour = NULL, y = "Mean age at publication",
     title = "Author ages over time")
```

![](03-sdg_who_files/figure-html/sdg_who_age_position-1.png)<!-- -->


