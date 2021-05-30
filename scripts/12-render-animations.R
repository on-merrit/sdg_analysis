library(gganimate)

future::plan("multisession", workers = 4L)

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
animate(p, renderer = ffmpeg_renderer())




pdata %>%
  ggplot(aes(gdp_p_cap, prop_oa, size = n)) +
  geom_point() +
  facet_wrap(vars(author_position),
             nrow = 3) +
  # ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10()


p <- pdata %>%
  ggplot(aes(gdp_p_cap, prop_oa, size = n)) +
  geom_point() +
  facet_wrap(vars(author_position),
             nrow = 3) +
  # ggrepel::geom_text_repel(aes(label = country_name)) +
  labs(x = "GDP per capita", y = "OA share") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')
animate(p, renderer = ffmpeg_renderer())
# look here to make this faster: https://github.com/thomasp85/gganimate/issues/78
