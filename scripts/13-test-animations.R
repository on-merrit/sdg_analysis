library(future)

## install futures pull request
# remotes::install_github("thomasp85/gganimate#403")

plan("multisession", workers = 10)
library(gganimate)
library(gapminder)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
animate(p, renderer = ffmpeg_renderer(), res = 200, height = 960, width = 1280)

plot(p)
