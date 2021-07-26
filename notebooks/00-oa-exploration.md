---
title: "OA Overview"
author: "Thomas Klebel"
date: "26 July, 2021"
output: 
  html_document:
    keep_md: true
---




# OA by FOS and year

```r
# aggregate oa_status -----
oa_status <- papers %>%
  filter(!is.na(is_oa)) %>%
  select(paperid, SDG_label, year, is_oa, provider_cat) 


oa_per_year <- oa_status %>%
  count(SDG_label, year, is_oa) %>%
  collect()

oa_status_per_year <- oa_status %>%
  count(SDG_label, year, provider_cat) %>%
  collect()
```




```r
oa_per_year %>%
  group_by(SDG_label, year) %>%
  mutate(oa_share = n/sum(n)) %>%
  filter(is_oa) %>%
  ggplot(aes(as_year(year), oa_share, colour = SDG_label,
             group = SDG_label)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by SDG",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")
```

![](00-oa-exploration_files/figure-html/sdg_oa_by_fos-1.png)<!-- -->


```r
oa_status_per_year %>%
  group_by(SDG_label, year) %>%
  filter(!is.na(provider_cat), provider_cat != "Not OA") %>%
  mutate(oa_share = n/sum(n)) %>%
  ggplot(aes(as_year(year), oa_share,
             colour = SDG_label,
             group = SDG_label)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(provider_cat)) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by SDG",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")
```

![](00-oa-exploration_files/figure-html/sdg_oa_type_by_fos-1.png)<!-- -->

The rise in OA is thus mainly due to rise in gold OA, slightly rise in hybrid.
However, unpaywall prefers gold over green: redo this figure with new 
categorisation.


# OA per funder 

```r
funder_overview <- papers %>% 
  # restrict to unpaywall
  filter(!is.na(is_oa), !is.na(is_funded)) %>% 
  group_by(is_funded, year, SDG_label) %>% 
  summarise(mean_oa = mean(as.numeric(is_oa))) %>% 
  collect()
```


```r
funder_overview %>% 
  ggplot(aes(as_year(year), mean_oa, colour = is_funded)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(SDG_label)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "% of publications which are OA", 
       title = "OA by funding status") +
  theme(legend.position = "top")
```

![](00-oa-exploration_files/figure-html/sdg_oa_by_funding_status-1.png)<!-- -->




```r
paper_oa_flag <- papers %>%
  select(paperid, is_oa, provider_cat, year, SDG_label)

oa_per_funder <- funded_projects %>%
  left_join(paper_oa_flag)
```

```
## Joining, by = "paperid"
```

```r
oa_per_funder_aggregated <- oa_per_funder %>%
  select(doi, funder_name, year, is_oa) %>%
  distinct() %>% # remove duplicate rows since many papers are funded by multiple projects
  group_by(year, funder_name) %>%
  count(is_oa) %>%
  filter(!is.na(is_oa)) %>%
  mutate(oa_share = n/sum(n),
         total_papers = sum(n)) %>%
  collect()
```


```r
text_labels <- oa_per_funder_aggregated %>%
  group_by(funder_name) %>%
  summarise(nn_papers = sum(total_papers)) %>%
  mutate(label = glue::glue("n = {format(nn_papers, big.mark = '.', decimal.mark = ',')}"))


oa_per_funder_aggregated %>%
  filter(is_oa) %>%
  ggplot(aes(lubridate::ymd(year, truncated = 2L), oa_share,
             group = funder_name)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x = lubridate::ymd("2017-01-01"),
                y = .4,
                label = label),
            data = text_labels) +
  facet_wrap(vars(str_wrap(funder_name, 50))) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, title = "OA share by funder",
       y = NULL, colour = NULL) +
  theme(legend.position = "top")
```

![](00-oa-exploration_files/figure-html/sdg_oa_by_funder-1.png)<!-- -->

Share of OA going down for CA institutes of health. There is an explanation
for this in the Nature 2018 paper by Lariviere and Sugimoto.



# OA by country

```r
oa_per_affiliation <- oa_status %>%
  left_join(author_paper_affiliations) %>%
  right_join(affils)
```

```
## Joining, by = "paperid"
```

```
## Joining, by = "affiliationid"
```

```r
oa_per_affiliation_selected <- oa_per_affiliation %>%
  group_by(paperid) %>%
  mutate(frac_count = 1 / max(authorsequencenumber, na.rm = TRUE)) %>%
  select(paperid, authorid, is_oa, provider_cat, year, country, frac_count,
         SDG_label) 

oa_per_country <- oa_per_affiliation_selected %>%
  group_by(country, is_oa) %>%
  summarise(sum_frac_oa = sum(frac_count)) %>%
  mutate(prop_oa = sum_frac_oa/sum(sum_frac_oa),
         sum_frac_total = sum(sum_frac_oa)) %>%
  collect()
```


## Correlation between indicators

```r
# use the mean of the 2015-2018 for now to reduce missing data
# maybe better to to lag values (drag forward if missing)
wb_local <- wb_indicators %>%
  filter(year >= 2015 & year <= 2018) %>%
  group_by(country_name, country_code, indicator_code, indicator_name) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  collect()

proper_countries <- wb_countries %>% 
  filter(!is.na(`Currency Unit`)) %>% 
  select(country_code = `Country Code`, short_name = `Short Name`,
         region = Region, income_group = `Income Group`)

wb_2015_2018 <- wb_local %>% 
  select(country_code, country_name, indicator_name, value) %>% 
  pivot_wider(names_from = "indicator_name", values_from = "value") %>% 
  right_join(proper_countries)
```

```
## Joining, by = "country_code"
```

```r
cor_matrix <- wb_2015_2018 %>% 
  select(-starts_with("country"), -short_name, -region, -income_group) %>% 
  cor(use = "pairwise.complete.obs")
```



```r
wb_2015_2018 %>% 
  filter(!is.na(country_name)) %>% 
  select(-short_name) %>% 
  vis_miss(cluster = TRUE)
```

![](00-oa-exploration_files/figure-html/wdi_missing-1.png)<!-- -->

Quite a large portion (about 40%) of ocuntries does not have data for R&D 
expenditure, even when averaging over the years 2015-2018. 



```r
plot_correlation(cor_matrix, cluster = TRUE)
```

![](00-oa-exploration_files/figure-html/wdi_correlations-1.png)<!-- -->



```r
## OA per gdp, % spent on research
oa_with_r_d_gdp <- oa_per_country %>%
  left_join(wb_local, by = c("country" = "country_code")) %>%
  select(-indicator_name) %>%
  filter(indicator_code %in% c("GB.XPD.RSDV.GD.ZS", "NY.GDP.MKTP.KD")) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  drop_na() %>%
  filter(is_oa)
oa_with_r_d_gdp %>%
  ggplot(aes(GB.XPD.RSDV.GD.ZS, prop_oa, colour = NY.GDP.MKTP.KD)) +
  geom_point(aes(size = NY.GDP.MKTP.KD), show.legend = FALSE) +
  scale_colour_viridis_c(trans = "log", option = "D", end = .9) +
  ggrepel::geom_text_repel(aes(label = country_name), show.legend = FALSE) +
  labs(x = "Expenditure towards research", y = "OA share") +
  scale_y_continuous(labels = scales::percent)
```

![](00-oa-exploration_files/figure-html/oa_sdg_by_country_rd-1.png)<!-- -->



```r
oa_with_gdp_per_cap <- oa_per_country %>%
  left_join(wb_local, by = c("country" = "country_code")) %>%
  select(-indicator_name) %>%
  filter(indicator_code %in% c("GB.XPD.RSDV.GD.ZS", "NY.GDP.PCAP.KD")) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  drop_na() %>%
  filter(is_oa)

oa_with_gdp_per_cap %>%
  filter(sum_frac_total >= 100) %>%
  ggplot(aes(NY.GDP.PCAP.KD, prop_oa)) +
  geom_point(aes(size = sum_frac_total, colour = sum_frac_total)) +
  geom_smooth(alpha = .3) +
  ggrepel::geom_text_repel(aes(label = country_name), seed = 66324613) +
  labs(x = "GDP per capita", y = "% of publications which are OA",
       colour = "# of publications", size = "# of publications") +
  scale_size_continuous(trans = "sqrt", labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10(breaks = c(1e+03, 5e+03, 1e+04, 5e+04, 1e+05),
                labels = scales::comma) +
  scale_colour_viridis_c(
    trans = "log", 
    # https://stackoverflow.com/a/20901094/3149349
    labels = scales::trans_format("identity", 
                                  format = function(x) scales::comma(round(x)))) +
  theme_bw() 
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](00-oa-exploration_files/figure-html/oa_sdg_per_country_gdp_p_cap-1.png)<!-- -->



```r
make_buckets <- function(df) {
  df %>% 
    mutate(year_bucket = case_when(year %in% 2009:2013 ~ "2009-2013",
                                   year %in% 2014:2018 ~ "2014-2018",
                                   TRUE ~ NA)) %>% 
    filter(!is.na(year_bucket))
  
}

oa_per_country_year <- oa_per_affiliation_selected %>%
  make_buckets() %>% 
  group_by(country, year_bucket, is_oa) %>%
  summarise(sum_frac_oa = sum(frac_count)) %>%
  mutate(prop_oa = sum_frac_oa/sum(sum_frac_oa),
         sum_frac_total = sum(sum_frac_oa)) %>%
  collect()


wb_buckets <- wb_indicators %>%
  make_buckets() %>% 
  group_by(country_name, country_code, indicator_code, indicator_name, 
           year_bucket) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  collect()



oa_with_gdp_per_cap <- oa_per_country_year %>%
  left_join(wb_buckets, by = c("country" = "country_code", "year_bucket")) %>%
  select(-indicator_name) %>%
  filter(indicator_code %in% c("GB.XPD.RSDV.GD.ZS", "NY.GDP.PCAP.KD")) %>%
  pivot_wider(names_from = indicator_code, values_from = value) %>%
  drop_na() %>%
  filter(is_oa)
```


```r
oa_with_gdp_per_cap %>%
  filter(sum_frac_total >= 100) %>%
  ggplot(aes(NY.GDP.PCAP.KD, prop_oa)) +
  geom_point(aes(size = sum_frac_total, colour = sum_frac_total)) +
  geom_smooth(alpha = .3) +
  ggrepel::geom_text_repel(aes(label = country_name), seed = 66324613) +
  labs(x = "GDP per capita", y = "% of publications which are OA",
       colour = "# of publications", size = "# of publications") +
  scale_size_continuous(trans = "sqrt", labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10(breaks = c(1e+03, 5e+03, 1e+04, 5e+04, 1e+05),
                labels = scales::comma) +
  scale_colour_viridis_c(
    trans = "log", 
    # https://stackoverflow.com/a/20901094/3149349
    labels = scales::trans_format("identity", 
                                  format = function(x) scales::comma(round(x)))) +
  theme_bw() +
  facet_wrap(vars(year_bucket), nrow = 2)
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](00-oa-exploration_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

things to do here: 

- include countries only if the yare above X pubs for both years.
- visualise with arrows:
  - only need this: https://stackoverflow.com/questions/38008863/how-to-draw-a-nice-arrow-in-ggplot2
  - need x and y from first bucket, xend and yend from second bucket
  - everyone will go up, so maybe show relative to mean increase?


# OA by country with author groups

```r
author_paper_affiliations_w_groups <- make_author_groups(author_paper_affiliations)

oa_per_affiliation <- paper_oa_flag %>%
  select(-SDG_label) %>%
  left_join(author_paper_affiliations_w_groups) %>%
  right_join(affils) %>% 
  filter(!is.na(is_oa))
```

```
## Joining, by = "paperid"
```

```
## Joining, by = "affiliationid"
```

```r
oa_per_country_over_years <- oa_per_affiliation %>%
  count(country, year, author_position, is_oa) %>%
  group_by(country, year, author_position) %>% 
  mutate(prop_oa = n/sum(n)) %>%
  collect()
```


## author positions by country


```r
country_names <- wb_indicators %>% 
  distinct(country_name, country_code) %>% 
  collect()
pdata <- oa_per_country_over_years %>% 
  group_by(country, author_position) %>% 
  summarise(n = sum(n)) %>% 
  add_proportion(n, order_var = author_position,
                 order_string = "first") %>% 
  left_join(country_names, by = c("country" = "country_code")) %>% 
  drop_na()
```

```
## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.
```

```r
p <- pdata %>% 
  ggplot(aes(fct_reorder(country_name, order), prop, fill = author_position)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Proportion")
plotly::ggplotly(p)
```

preserved8ee66e016e17f83

Here we could also look into the proportion of papers coming from single, dual 
or multi-author papers.



```r
gdp_p_cap <- wb_indicators %>% 
  filter(indicator_code == "NY.GDP.PCAP.KD") %>% 
  collect()
pdata <- oa_per_country_over_years %>% 
  group_by(country) %>% 
  mutate(country_with_low_n = any(n < 5)) %>% 
  filter(!country_with_low_n) %>% 
  left_join(gdp_p_cap, by = c("year", "country" = "country_code")) %>% 
  rename(gdp_p_cap = value) %>% 
  group_by(country, year, author_position) %>% 
  mutate(total_n = sum(n)) %>% 
  filter(is_oa)
pdata %>% 
  ggplot(aes(gdp_p_cap, prop_oa, size = total_n)) +
  geom_point() +
  facet_grid(rows = vars(year),
             cols = vars(author_position)) +
  # ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10()
```

![](00-oa-exploration_files/figure-html/sdg_oa_by_country_by_gdp-1.png)<!-- -->


### Only for first authors

```r
pdata %>% 
  filter(author_position == "first_author") %>% 
  ggplot(aes(gdp_p_cap, prop_oa, size = total_n)) +
  geom_point() +
  facet_wrap(vars(year)) +
  # ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10()
```

![](00-oa-exploration_files/figure-html/sdg_oa_by_country_by_gdp_first_authors-1.png)<!-- -->


## Animated and for all

```r
# approach for saving and embedding from https://stackoverflow.com/a/54849281/3149349
# and https://stackoverflow.com/a/50394922/3149349
p <- pdata %>%
  # filter(author_position == "first_author") %>%
  ggplot(aes(gdp_p_cap, prop_oa, size = n, group = country)) +
  geom_point() +
  facet_wrap(vars(author_position),
             nrow = 3) +
  # ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')
anim_save(here::here("animations/oa_by_country_author_position.mp4"), p, 
          renderer = ffmpeg_renderer())
```

<video width="960" height="720" controls>
  <source src="../animations/oa_by_country_author_position.mp4" type="video/mp4">
</video>


## Investigate country per SDG
Avenues to follow:

- only for first authors
- focus on SDG 3, but look if sample is big enough in other areas
- 2018


```r
oa_per_affiliation <- oa_status %>%
  left_join(author_paper_affiliations_w_groups) %>%
  right_join(affils)
```

```
## Joining, by = "paperid"
```

```
## Joining, by = "affiliationid"
```

```r
oa_per_affil_firsts <- oa_per_affiliation %>% 
  filter(author_position == "first_author", year %in% 2015:2018) %>% 
  group_by(country, SDG_label) %>% 
  summarise(prop_oa = mean(as.numeric(is_oa)),
            n_papers = n()) %>% 
  collect()
```



```r
oa_per_affil_firsts_w_groups <- oa_per_affil_firsts %>% 
  left_join(proper_countries, by = c("country" = "country_code"))
```


---------------------

TODO here:

do not visualise countries with boxplot, but sample from the data and plot, or
use dbplot variante.


-----------------------


```r
oa_per_affil_firsts_w_groups %>% 
  filter(!is.na(income_group)) %>% 
  mutate(income_group = fct_relevel(income_group, "High income", 
                                    "Upper middle income", 
                                    "Lower middle income", "Low income")) %>% 
  ggplot(aes(SDG_label, prop_oa, fill = income_group)) +
  geom_boxplot()
```

![](00-oa-exploration_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



```r
oa_per_affil_firsts_w_groups %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(SDG_label, prop_oa, fill = region)) +
  geom_boxplot(notch = TRUE)
```

```
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
## notch went outside hinges. Try setting notch=FALSE.
```

![](00-oa-exploration_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

This has the issue of treating Bermuda and USA equally (one data point in north
america).


```r
oa_per_affil_firsts_w_groups %>% 
  filter(!is.na(region)) %>% 
  group_by(region, SDG_label) %>% 
  summarise(mean_oa = weighted.mean(prop_oa, n_papers)) %>% 
  ggplot(aes(SDG_label, mean_oa, colour = region, group = region)) +
  geom_point() +
  geom_line()
```

```
## `summarise()` has grouped output by 'region'. You can override using the `.groups` argument.
```

![](00-oa-exploration_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
oa_per_affil_firsts_w_groups %>% 
  filter(!is.na(region)) %>% 
  group_by(region, SDG_label) %>% 
  ggplot(aes(region, prop_oa, colour = region, group = region, size = n_papers)) +
  geom_beeswarm() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(vars(SDG_label)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = NULL, y = "% of papers which are OA")
```

![](00-oa-exploration_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

to develop further: how to visualise country differences?
issue: low counts for many countries. maybe filter them out, maybe increase year
span.

one reason for low numbers: authorship positions are associated with country ->
look at this in detail in 04-sdg-with-whom



