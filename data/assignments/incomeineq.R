library(tidyverse)
library(eurostat)
library(MetBrewer)
library(msthemes)

raw <- get_eurostat("ilc_di12", time_format = "num")

raw |> filter(str_length(geo) < 3, time %in% c(2000,2020)) |> 
  filter(n()>1, .by = geo) |> 
  ggplot(aes(x = geo, y = values)) +
  geom_line(aes(group = geo), linewidth = 3, color = "gray90") +
  geom_point(aes(color = factor(time)), size = 3) +
  scale_color_manual(name = NULL, values = met.brewer("Juarez")[c(3,6)],
                     guide = guide_legend(direction = "horizontal")) + 
  labs(x = NULL, y = "Gini index", title = "Change in income inequality in Europe",
       subtitle = "Gini coefficient of disposable household income, 2000-2020",
       caption = "Source: Eurostat. Figure: @matschnetzer") +
  theme_ms() +
  theme(legend.position = c(0.8,0.9),
        plot.caption = element_text(margin = margin(t = 5, unit = "pt")))

ggsave("~/Desktop/incomeineq.png", width = 8, height = 5, dpi = 320)
