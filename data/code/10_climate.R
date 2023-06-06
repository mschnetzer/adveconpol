library(tidyverse)
library(lubridate)
library(sf)

load("10_climate.RData")

# Filter data for very hot days (>= 30°)
findat <- tempdat |> 
  mutate(year = year(time)) |> 
  filter(tmax >= 30)

# Count hot days per year and county; if no hot day, fill in 0 rather than NA
yeardat <- findat |> count(year, name, .drop = FALSE) |> 
  complete(year, name, fill = list(n = 0))

# Combine data with map
plotdat <- blmap |> left_join(yeardat)

plotdat |> ggplot(aes(fill = n)) +
  facet_wrap(~year, ncol = 10) +
  geom_sf(linewidth = 0.05, color = "black") +
  geom_text(aes(label = year), x = 10, y = 48.3, size = 6, 
            family = "Roboto Condensed", hjust = 0, color = "gray40") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Number of very hot days (30+°C)") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(margin = margin(b = 1, unit = "lines")),
        plot.title.position = "plot",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")


# Vienna only
wien <- blmap |> filter(name == "Wien")
wiendat <- yeardat |> filter(name == "Wien")

wienplot <- wien |> left_join(wiendat)

wienplot |> ggplot(aes(fill = n)) +
  facet_wrap(~year, ncol = 10) +
  geom_sf(linewidth = 0.05, color = "black") +
  geom_text(aes(x=16.37505, y= 48.20915, label = round(n,0)), size = 5, family = "Hack",
            fontface = "bold", color = "white", alpha = 0.8) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Very hot days (30+°C) in Vienna",
       x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 9),
        legend.position = "none")
