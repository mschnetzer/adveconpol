###########################
## 03 DATA VISUALIZATION ##
###########################

# Load general packages
library(tidyverse)
library(lubridate) # for dates and times
library(scales) # for scale layouts (breaks and labels)

##############
## PENGUINS ##
##############

# Load and assign data
library(palmerpenguins)
data <- penguins

# Take brief look
head(data)

data_summary <- data |>
  group_by(species) |>
  summarise(across(c(bill_length_mm, bill_depth_mm),
                   list(median = ~median(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE))))

data |> ggplot(aes(x = bill_length_mm, 
                   y = bill_depth_mm, 
                   color = species)) +
  geom_errorbar(
    data = data_summary,
    aes(x = bill_length_mm_median,
        ymin = bill_depth_mm_median - bill_depth_mm_sd,
        ymax = bill_depth_mm_median + bill_depth_mm_sd,
        color = species, 
        color = after_scale(colorspace::darken(color, .2, space = "combined"))
    ),
    inherit.aes = F, width = .8, size = .8
  ) +
  geom_errorbar(
    data = data_summary,
    aes(y = bill_depth_mm_median,
        xmin = bill_length_mm_median - bill_length_mm_sd,
        xmax = bill_length_mm_median + bill_length_mm_sd,
        color = species, 
        color = after_scale(colorspace::darken(color, .2, space = "combined"))
    ),
    inherit.aes = F, width = .8, size = .8
  ) +
  geom_point(size = 1.5, alpha = 0.5) +
  scale_color_manual(name = NULL,
                     values = MetBrewer::met.brewer("Lakota")) +
  scale_x_continuous(labels = scales::number_format(suffix="mm")) +
  scale_y_continuous(labels = scales::number_format(suffix="mm", accuracy = 1)) +
  annotate("text", x = c(34.7, 55.7, 50.7), y = c(20.7, 19, 13.6), color = MetBrewer::met.brewer("Lakota")[1:3], label = c("Adélie","Chinstrap","Gentoo"), fontface = "bold", size = 4) +
  labs(x = "Bill length", y = "Bill depth",
       title = "Penguins are awesome",
       subtitle = "Depth and length of bills") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 15, family="Roboto"),
        plot.subtitle = element_text(size = 13, family="Roboto"),
        panel.grid.minor = element_blank())
  



##############
## EUROSTAT ##
##############

library(eurostat)

retdata <- get_eurostat("sts_trtu_m", filters = list(indic_bt = "TOVV", s_adj = "CA", unit = "PCH_SM", nace_r2 = "G47", geo = c("AT","DE","FR","ES","IT","PT")), time_format = "date", select_time = "M", type = "code")

plotdat <- retdata |> 
  mutate(time = ym(time)) |> 
  slice_max(time, n = 10, by = geo) |> 
  label_eurostat(dic = "geo", lang = "en") |> 
  mutate(geo = case_when(str_detect(geo, "Germany") ~ "Germany", TRUE ~ geo))

plotdat |> 
  ggplot(aes(x = time, y = values)) +
  geom_segment(aes(y = 0, yend = values, xend = time, 
                   color = ifelse(values < 0, "green", "red"))) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = ifelse(values < 0, "green", "red"))) + 
  facet_wrap(~geo) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  theme_minimal() +
  labs(x = "", y = "",
       title = "How did retail sales change in the last 10 months?",
       subtitle = "Percentage change compared to the same period in previous year",
       caption = "Note: Data is calendar but not seasonally adjusted\n Data: Eurostat. Figure: @matschnetzer") +
  theme(legend.position = "none",
        plot.subtitle = element_text(margin = margin(t = 5, b = 10, unit = "pt")),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_text(size = 10))

ggsave(filename = "retail_sales.png", dpi = 320, width = 8, height = 4)
