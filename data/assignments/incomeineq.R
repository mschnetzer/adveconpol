library(tidyverse)

# Load data
raw <- read_csv("incomeineq.csv")

# Show all available countries, but we don't want country groups (EA18, EU15, etc.) 
unique(raw$geo)
filtered <- raw |> filter(str_length(geo) < 3, TIME_PERIOD %in% c(2000,2020))

# Not all countries have both years available -> filter those with both values!
filtered |> count(geo)
plotdata <- filtered |> filter(n() > 1, .by = geo)

plotdata |> 
  ggplot(aes(x = geo, y = OBS_VALUE)) +
  geom_line(aes(group = geo), linewidth = 3, color = "gray90") +
  geom_point(aes(color = factor(TIME_PERIOD)), size = 3) +
  scale_color_manual(name = NULL, values = c("goldenrod1","midnightblue"),
                     guide = guide_legend(direction = "horizontal")) + 
  labs(x = NULL, y = "Gini index", title = "Change in income inequality in Europe",
       subtitle = "Gini coefficient of disposable household income, 2000-2020",
       caption = "Source: Eurostat. Figure: @matschnetzer") +
  theme_minimal() +
  theme(legend.position = c(0.8,0.9),
        plot.title.position = "plot",
        plot.caption = element_text(size = 7, margin = margin(t = 1, unit = "lines")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

ggsave("incomeineq.png", width = 8, height = 5, dpi = 320)
