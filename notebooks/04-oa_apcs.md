---
title: "Relationship between OA publishing, APCs and IF"
author: "Thomas Klebel"
date: "28 July, 2021"
output: 
  html_document:
    keep_md: true
---




# APC by sdg

```r
step1 <- papers %>% 
  # we can only look at journals, since doaj only is for journals
  # also only look at fairly recent papers
  # also only look at papers which actually are OA
  filter(!is.na(journalid), year %in% 2015:2018, is_oa) %>% 
  left_join(author_paper_affiliations) %>% 
  left_join(affils) %>% 
  left_join(journals, by = "journalid") %>% 
  # need to filter out some NAs, since the journal table seems to be older than
  # the rest of MAG? at least it does not contain some journals for which we
  # have papers
  filter(!is.na(APC), provider_cat != "Repository only") %>% 
  group_by(authorid, paperid) %>% 
  mutate(frac_value = 1/n()) 
```

```
## Joining, by = "paperid"
```

```
## Joining, by = c("citationcount", "affiliationid")
```

```r
apc_per_sdg <- step1 %>% 
  mutate(APC = if_else(APC == "NA", "Not in DOAJ", APC)) %>% 
  group_by(SDG_label) %>% 
  count(APC) %>% 
  collect()
```



```r
apc_per_sdg %>% 
  group_by(SDG_label) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(APC = factor(APC, levels = c("Not in DOAJ", "FALSE", "TRUE"),
                      labels = c("Not in DOAJ", "No", "Yes"))) %>%
  ggplot(aes(prop, fct_relevel(SDG_label, "SDG_3", after = 1), fill = APC)) +
  geom_col(width = .7) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Publishing involved an APC",
       caption = "2015-2018") +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE))
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
apc_per_sdg %>% 
  filter(APC != "Not in DOAJ") %>% 
  group_by(SDG_label) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(APC = factor(APC, levels = c("FALSE", "TRUE"),
                      labels = c("No", "Yes"))) %>%
  ggplot(aes(prop, fct_relevel(SDG_label, "SDG_3", after = 1), fill = APC)) +
  geom_col(width = .7) +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Publishing involved an APC",
       caption = "2015-2018") +
  theme(legend.position = "top")  +
  guides(fill = guide_legend(reverse = TRUE))
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Are researchers from lower ranking universities publishing with higher or 
lower APCs? In journals with higher or lower IF?

# OA with or without APC

Repeat figure from OA Who about university ranking and publishing types, but
this time with APC yes, no, and IF mean.


```r
apc_per_affiliation_per_sdg <- step1 %>% 
  mutate(APC = if_else(APC == "NA", "Not in DOAJ", APC)) %>% 
  group_by(affiliationid, SDG_label) %>% 
  count(APC, wt = frac_value)

# apc_per_affiliation_per_sdg %>% ungroup() %>% count(APC)

totals <- step1 %>% 
  group_by(affiliationid, SDG_label) %>% 
  summarise(n_frac_papers = sum(frac_value, na.rm = TRUE))

apc_per_affiliation_per_sdg <- apc_per_affiliation_per_sdg %>% 
  left_join(totals)
```

```
## Joining, by = c("affiliationid", "SDG_label")
```

```r
apc_per_affiliation_per_sdg_local <- apc_per_affiliation_per_sdg %>% 
  filter(n_frac_papers > 50) %>% 
  collect()
```







```r
apc_affiliation_leiden <- apc_per_affiliation_per_sdg_local %>%
  mutate(affiliationid = as.numeric(affiliationid)) %>% # needed for merging
  left_join(affil_leiden_key) %>%
  left_join(leiden_small_local) %>% 
  # remove those institutions that are not in leiden ranking
  filter(!is.na(University), Period == "2015–2018")
```

```
## Joining, by = "affiliationid"
```

```
## Joining, by = c("University", "Country")
```

The figure below uses fractional counting.


```r
p <- apc_affiliation_leiden %>% 
  group_by(SDG_label, affiliationid) %>% 
  mutate(apc_share = n/sum(n)) %>% 
  ggplot(aes(P_top10, apc_share, colour = APC)) +
  geom_point(size = .7, alpha = .4) +
  scale_x_log10() +
  geom_smooth() +
  facet_wrap(vars(SDG_label)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top") +
  labs(y = "Share of papers in category", colour = NULL,
       title = "Association between institutional prestige (2015-2018)\nand whether APCs are involved or not",
       caption = "Fractional counting")
p
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Very interesting: really high universities publish less in DAOJ journals. Why is
that? which types of journals are these? This is especially true in medicine.
Maybe there is something specific going on here?



```r
p2 <- apc_affiliation_leiden %>% 
  filter(APC != "Not in DOAJ") %>%
  group_by(SDG_label, affiliationid) %>% 
  mutate(apc_share = n/sum(n)) %>% 
  ggplot(aes(P_top10, apc_share, colour = APC)) +
  geom_point(size = .7, alpha = .4) +
  scale_x_log10() +
  geom_smooth() +
  facet_wrap(vars(SDG_label)) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top") +
  labs(y = "Share of papers in category", colour = NULL,
       title = "Association between institutional prestige (2015-2018) and\nwhether APCs are involved or not",
       caption = "Fractional counting")
p2
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

We can conclude:

- higher prestige institutions generally publish a larger share of their research
in OA journals that involve an APC

Next question: Is this also visible in terms of APC prices?
Then: how does this relate to IF?

And: is the APC (yes/no) divide also visible on regional level? Alternative 
explanation: lower ranked affiliations publish locally, and these local journals
might not have APCs. It still means: those researchers are not able to participate
in the high-ranking journal game.

Maybe correspondence analysis to relate these? IF in quartiles from SJR

What do we learn? 


# APC prices
The figures below use full counting, but for first and last authors separately.

```r
step1_apc_prices <- papers %>% 
  # we can only look at journals, since doaj only is for journals
  # also only look at fairly recent papers
  # also only look at papers which actually are OA
  filter(!is.na(journalid), year %in% 2015:2018, is_oa) %>% 
  left_join(author_paper_affiliations_w_groups) %>% 
  left_join(affils) %>% 
  left_join(journals, by = "journalid") %>% 
  # need to filter out some NAs, since the journal table seems to be older than
  # the rest of MAG? at least it does not contain some journals for which we
  # have papers
  filter(!is.na(APC), provider_cat != "Repository only") %>% 
  # keep only last and first authors
  filter(author_position %in% c("last_author", "first_author"))
```

```
## Joining, by = "paperid"
```

```
## Joining, by = c("citationcount", "affiliationid")
```

```r
mean_apc_per_affiliation_per_sdg <- step1_apc_prices %>% 
  group_by(affiliationid, SDG_label, author_position) %>% 
  summarise(mean_apc = mean(APC_in_dollar),
            n_papers = n())

mean_apc_per_affiliation_per_sdg_local <- mean_apc_per_affiliation_per_sdg %>% 
  filter(n_papers > 20) %>% 
  collect()
```


```r
apc_val_affiliation_leiden <- mean_apc_per_affiliation_per_sdg_local %>%
  mutate(affiliationid = as.numeric(affiliationid)) %>% # needed for merging
  left_join(affil_leiden_key) %>%
  left_join(leiden_small_local) %>% 
  # remove those institutions that are not in leiden ranking
  filter(!is.na(University), Period == "2015–2018")
```

```
## Joining, by = "affiliationid"
```

```
## Joining, by = c("University", "Country")
```

```r
firsts <- apc_val_affiliation_leiden %>% 
  filter(author_position == "first_author")

lasts <- apc_val_affiliation_leiden %>% 
  filter(author_position == "last_author")
```


```r
labels <- lasts %>% 
  group_by(SDG_label) %>% 
  summarise(cor = cor(mean_apc, P_top10)) %>% 
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

lasts %>% 
  ggplot(aes(P_top10, mean_apc)) +
  geom_point(aes(colour = n_papers), alpha = .4) +
  geom_smooth(show.legend = FALSE, colour = "grey30") +
  geom_text(data = labels, aes(label = cor, x = 50, y = 3700)) +
  facet_wrap(vars(SDG_label), nrow = 1) +
  scale_x_log10() +
  scale_colour_viridis_c(trans = "log10") +
  theme_bw() +
  theme(legend.position = "top", legend.key.width = unit(1.8, "lines")) +
  labs(title = "Mean APC per institution by institutional prestige (2015-2018)",
       colour = "Number of papers per institution",
       caption = "Full counting; last authors only")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Inclusion criterion was for an institution to have at least 20 papers per last
author. in SDG 13 the lower ranked institutions do not have that.

In general, higher prestige is related to higher APCs. The effect is strongest 
for SDG 2, with a steep incline at the lowest ranking universities.



```r
labels <- firsts %>% 
  group_by(SDG_label) %>% 
  summarise(cor = cor(mean_apc, P_top10)) %>% 
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

firsts %>% 
  ggplot(aes(P_top10, mean_apc)) +
  geom_point(aes(colour = n_papers), alpha = .4) +
  geom_smooth(show.legend = FALSE, colour = "grey30") +
  geom_text(data = labels, aes(label = cor, x = 50, y = 3700)) +
  facet_wrap(vars(SDG_label), nrow = 1) +
  scale_x_log10() +
  scale_colour_viridis_c(trans = "log10") +
  theme_bw() +
  theme(legend.position = "top", legend.key.width = unit(1.8, "lines")) +
  labs(title = "Mean APC per institution by institutional prestige (2015-2018)",
       colour = "Number of papers per institution",
       caption = "Full counting; first authors only")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


Same as above, but with zero apcs included into the mean


```r
mean_0_apc_per_affiliation_per_sdg <- step1_apc_prices %>% 
  mutate(APC_in_dollar = case_when(APC == "FALSE" ~ 0,
                                   TRUE ~ APC_in_dollar)) %>% 
  group_by(affiliationid, SDG_label, author_position) %>% 
  summarise(mean_apc = mean(APC_in_dollar),
            n_papers = n())

mean_0_apc_per_affiliation_per_sdg_local <- mean_0_apc_per_affiliation_per_sdg %>% 
  filter(n_papers > 20) %>% 
  collect()
```



```r
apc_val_affiliation_leiden <- mean_0_apc_per_affiliation_per_sdg_local %>%
  mutate(affiliationid = as.numeric(affiliationid)) %>% # needed for merging
  left_join(affil_leiden_key) %>%
  left_join(leiden_small_local) %>% 
  # remove those institutions that are not in leiden ranking
  filter(!is.na(University), Period == "2015–2018")
```

```
## Joining, by = "affiliationid"
```

```
## Joining, by = c("University", "Country")
```

```r
firsts <- apc_val_affiliation_leiden %>% 
  filter(author_position == "first_author")

lasts <- apc_val_affiliation_leiden %>% 
  filter(author_position == "last_author")
```


```r
labels <- lasts %>% 
  group_by(SDG_label) %>% 
  summarise(cor = cor(mean_apc, P_top10)) %>% 
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

lasts %>% 
  ggplot(aes(P_top10, mean_apc)) +
  geom_point(aes(colour = n_papers), alpha = .4) +
  geom_smooth(show.legend = FALSE, colour = "grey30") +
  geom_text(data = labels, aes(label = cor, x = 50, y = 3700)) +
  facet_wrap(vars(SDG_label), nrow = 1) +
  scale_x_log10() +
  scale_colour_viridis_c(trans = "log10") +
  theme_bw() +
  theme(legend.position = "top", legend.key.width = unit(1.8, "lines")) +
  labs(title = "Mean APC per institution by institutional prestige (2015-2018)",
       colour = "Number of papers per institution",
       caption = "Full counting; last authors only; including no APC as '0'")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


```r
labels <- firsts %>% 
  group_by(SDG_label) %>% 
  summarise(cor = cor(mean_apc, P_top10)) %>% 
  mutate(cor = glue::glue("r = {format(cor, nsmall = 2, digits = 2)}"))

firsts %>% 
  ggplot(aes(P_top10, mean_apc)) +
  geom_point(aes(colour = n_papers), alpha = .4) +
  geom_smooth(show.legend = FALSE, colour = "grey30") +
  geom_text(data = labels, aes(label = cor, x = 50, y = 3700)) +
  facet_wrap(vars(SDG_label), nrow = 1) +
  scale_x_log10() +
  scale_colour_viridis_c(trans = "log10") +
  theme_bw() +
  theme(legend.position = "top", legend.key.width = unit(1.8, "lines")) +
  labs(title = "Mean APC per institution by institutional prestige (2015-2018)",
       colour = "Number of papers per institution",
       caption = "Full counting; first authors only; including no APC as '0'")
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-15-1.png)<!-- -->





Visualise time component:

- do quantiles over time, with mean of APC per quantile of Ptop10


```r
cut_quantiles <- function(x) {
    cut(x, breaks = quantile(x, probs = seq(0, 1, by = .2), na.rm = TRUE), 
        labels = {1:5*20} %>% map_chr(~paste("p", . - 20, ., sep = "-")), 
        include.lowest = TRUE)
}


cut_quartiles <- function(x) {
    cut(x, breaks = quantile(x, probs = seq(0, 1, by = .25), na.rm = TRUE), 
        labels = {1:4*25} %>% map_chr(~paste("p", . - 25, ., sep = "-")), 
        include.lowest = TRUE)
}
```


```r
apc_time <- papers %>% 
  # we can only look at journals, since doaj only is for journals
  # also only look at fairly recent papers
  # also only look at papers which actually are OA
  filter(!is.na(journalid), is_oa) %>% 
  left_join(author_paper_affiliations_w_groups) %>% 
  left_join(affils) %>% 
  left_join(journals, by = "journalid") %>% 
  # need to filter out some NAs, since the journal table seems to be older than
  # the rest of MAG? at least it does not contain some journals for which we
  # have papers
  filter(!is.na(APC), provider_cat != "Repository only") %>% 
  # keep only last and first authors
  filter(author_position %in% c("last_author", "first_author"))
```

```
## Joining, by = "paperid"
```

```
## Joining, by = c("citationcount", "affiliationid")
```



```r
apc_time_summarised <- apc_time %>% 
  mutate(APC_in_dollar = case_when(APC == "FALSE" ~ 0,
                                   TRUE ~ APC_in_dollar)) %>% 
  group_by(affiliationid, SDG_label, year, author_position) %>% 
  summarise(mean_apc = mean(APC_in_dollar),
            n_papers = n()) %>% 
  collect()
```



```r
apc_time_leiden <- apc_time_summarised %>%
  mutate(affiliationid = as.numeric(affiliationid)) %>% # needed for merging
  left_join(affil_leiden_key) %>%
  left_join(leiden_small_local) %>% 
  # remove those institutions that are not in leiden ranking
  filter(!is.na(University), year == as.integer(last_year_of_period)) %>% 
  mutate(P_top10 = cut_quartiles(P_top10)) %>% 
  filter(!is.na(P_top10))
```

```
## Joining, by = "affiliationid"
```

```
## Joining, by = c("University", "Country")
```

```r
firsts <- apc_time_leiden %>% 
  filter(author_position == "first_author")

lasts <- apc_time_leiden %>% 
  filter(author_position == "last_author")
```



```r
plot_over_time <- function(df, indicator, y_var) {
  df %>% 
    group_by(SDG_label, year, {{indicator}}) %>% 
    mutate(y_median = median({{y_var}}, na.rm = TRUE)) %>% 
    ggplot(aes(as_year(year), y_median, colour = {{indicator}})) +
    geom_line() +
    facet_wrap(vars(fct_relevel(SDG_label, "SDG_13", after = 3))) +
    #scale_y_log10() +
    guides(colour = guide_legend(reverse = FALSE)) +
    labs(x = NULL, y = "Median of APCs in quartile") +
    theme_bw() +
    theme(legend.position = "top")
}
```



```r
firsts %>% 
  plot_over_time(P_top10, mean_apc) +
  labs(caption = "First authors; full counting")
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-21-1.png)<!-- -->



```r
lasts %>% 
  plot_over_time(P_top10, mean_apc) +
  labs(caption = "Last authors; full counting")
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Only SDG 3 really has a clear picture for the lowest percentile.


```r
firsts %>% 
  filter(SDG_label == "SDG_3") %>% 
  plot_over_time(P_top10, mean_apc) +
  labs(caption = "First authors; full counting")
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-23-1.png)<!-- -->



```r
lasts %>% 
  filter(SDG_label == "SDG_3") %>% 
  plot_over_time(P_top10, mean_apc) +
  labs(caption = "Last authors; full counting")
```

![](04-oa_apcs_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

