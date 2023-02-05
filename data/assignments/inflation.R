library(tidyverse)
library(lubridate)
library(MetBrewer)
library(ggrepel)
library(msthemes)


rawdata <- read.csv("inflation.csv")

inflation <- rawdata |> 
  select(geo, date = TIME_PERIOD, values = OBS_VALUE) |> 
  mutate(date = ymd(date, truncated = 2)) |> 
  filter(date < "2023-01-01")


inflation |> 
  ggplot(aes(x = date, y = values, group = geo)) +
  geom_line(data = inflation |> filter(!geo %in% c("AT","ES","HU")), color = "gray80", linewidth = 0.2) +
  geom_line(data = inflation |> filter(geo == "HU"), aes(color = geo), linewidth = 0.5) +
  geom_line(data = inflation |> filter(geo == "ES"), aes(color = geo), linewidth = 0.5) +
  geom_line(data = inflation |> filter(geo == "AT"), aes(color = geo), linewidth = 0.5) +
  geom_text_repel(data = inflation |> filter(geo %in% c("AT","ES","HU")) |> slice_max(date), size = 3,
                  aes(x=date, y=values, label = glue::glue("{geo}: {values}%"), color = geo), 
                  xlim = c(as.Date("2023-01-01"), NA),family = "Roboto", fontface = "bold") +
  scale_color_manual(values = met.brewer("Lakota")[c(4,5,3)], guide = guide_none()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("2010-01-01"), as.Date("2025-01-01")), 
               breaks = seq(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "5 years"),
               labels = scales::date_format(format = "%Y")) +
  labs(x = NULL, y = NULL, title = "Inflation in the European Union",
       subtitle = "Annual change in the Harmonized Consumer Prices Index 2010-2022",
       caption = "Source: Eurostat. Figure: @matschnetzer") +
  theme_ms(alttf = T) +
  theme(plot.caption = element_text(margin = margin(t=5, unit = "pt")))

ggsave("inflation.png", dpi = 320, width = 8, height = 5)
