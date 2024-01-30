library(tidyverse)
library(waffle)
library(MetBrewer)

data <- read.csv("mobility.csv")

data |> filter(Parents %in% c("Apprenticeship/Lower secondary",
                              "Tertiary education")) |> 
  mutate(Children = factor(Children, levels = c("Tertiary education",
                                                "Higher secondary",
                                                "Apprenticeship/Lower secondary",
                                                "Compulsory school"))) |> 
  ggplot() +
  geom_waffle(aes(fill = Children, values = Share), size = 1.125, n_rows = 5, na.rm=T, color = "white",
              make_proportional = T) + 
  facet_wrap(~Parents, ncol = 1) +
  scale_fill_manual(values=met.brewer("Lakota"), "Education of descendants (25-44 years)") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  labs(title = "Educational persistence in Austria") +
  theme_minimal(base_family = "Roboto Condensed") +
  coord_equal() +
  theme_enhance_waffle() +
  theme(strip.text.x=element_text(size = 12, margin=margin(b = 5, t = 5), hjust = 0),
        plot.caption = element_text(margin = margin(t = 4)),
        plot.title = element_text(margin = margin(b = 6), family = "Playfair Display", size = 20),
        legend.title=element_text(size = 9))

ggsave("~/Desktop/edumob.png", width=8, height=4, dpi=320)
