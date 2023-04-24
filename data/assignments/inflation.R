library(tidyverse)
library(lubridate)
library(MetBrewer)
library(ggrepel)
library(gghighlight)


rawdata <- read.csv("inflation.csv")

inflation <- rawdata |> 
  select(geo, date = TIME_PERIOD, values = OBS_VALUE) |>
# Character into date format. Alternatively: mutate(date = as.Date(paste0(date, "-01")))
  mutate(date = ymd(date, truncated = 2)) |>
  filter(date < "2023-01-01")


inflation |> 
  ggplot(aes(x = date, y = values, group = geo, colour = geo)) +
# Line plot with defined line width
  geom_line(linewidth = 0.5) +
# Highlight only selected countries, do not use direct labels and specify how the unhighlighted lines should look like (thin and gray)
  gghighlight(geo %in% c("AT","ES","HU"), use_direct_label = F, 
              unhighlighted_params = list(linewidth = 0.2, color = "gray80")) +
# Manually created the labels (non-overlapping with ggrepel) and print them at the maximum date on the x-axis. The labels are composed with the glue package.
  geom_text_repel(data = inflation |> filter(geo %in% c("AT","ES","HU")) |> 
                    slice_max(date), size = 3,
                  aes(x=date, y=values, label = glue::glue("{geo}: {values}%"), color = geo), 
# The xlim command tells ggplot in which range the labels should be plotted. The NA in the second place just means no limit on the right.
                  xlim = c(as.Date("2023-01-01"), NA), 
                  family = "Roboto Condensed", fontface = "bold") +
# We can change the order of the color palette with []; We don't need a legend (guide_none), but alternatively you can specify the theme(legend.position = "none") below
  scale_color_manual(values = met.brewer("Lakota")[c(4,5,3)], guide = guide_none()) +
# Y-axis with percentage symbols
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
# Let's expand the limits of the x-axis to 2024, but the breaks should end with 2020; the labels are then set to %Y to show years!
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2010-01-01"), as.Date("2024-06-01")), 
               breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "5 years"),
               labels = scales::date_format(format = "%Y")) +
  labs(x = NULL, y = NULL, title = "Inflation in the European Union",
       subtitle = "Annual change in the Harmonized Consumer Prices Index 2010-2022",
       caption = "Source: Eurostat. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16),
        plot.title.position = "plot",
        plot.caption = element_text(size = 7, margin = margin(t=5, unit = "pt")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

ggsave("inflation.png", dpi = 320, width = 8, height = 5)
