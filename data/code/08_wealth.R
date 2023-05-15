########################
## 08 WEALTH · THEMES ##
########################

librarian::shelf(tidyverse, readxl, countrycode, ggtext, MetBrewer)

# Load original data from ECB: https://www.ecb.europa.eu/home/pdf/research/hfcn/HFCS_Statistical_tables_Wave_2017_May2021.zip?078368a4438f4d94712367f6615445aa
hfcs <- readxl::read_xlsx("HFCS_Statistical_tables_Wave 2017_May2021.xlsx", 
                  sheet = "J3 Net wealth per household ", range = "A4:Z24") |> 
  janitor::clean_names() |> 
  rename(measure = x1) |> select(-x2, -x3) |> 
  filter(measure %in% c("Mean", "p20", "p50", "p80")) |> 
  mutate(across(-measure, as.numeric))

# Alternatively, load local RData file
# load("08_wealth.RData")

# Data into long format with "pivot_longer", capitalize country codes with "toupper" and merge country names from countrycode package with "left_join"
findat <- hfcs |> 
  pivot_longer(-measure, names_to = "country", values_to = "value") |> 
  mutate(across(c(country, measure), toupper)) |> 
  left_join(countrycode::codelist |> select(iso2c, country.name.en), 
            by = c("country" = "iso2c")) |> 
  mutate(country.name.en = ifelse(country == "EURO_AREA", "Euro Area", country.name.en),
         country.name.en = fct_reorder(country.name.en, value))

findat |> 
  ggplot(aes(x = country.name.en, y = value, group = country, color = measure)) +
  geom_line(color = "gray90", alpha = 0.15, linewidth = 2) +
  geom_point(size = 2) +
  scale_color_manual(values = met.brewer("Isfahan2"), 
                     guide = guide_legend(override.aes = list(size = 4))) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", suffix = "K")) +
  coord_flip() +
  labs(x = NULL, y = NULL, color = NULL, 
       title = "Who are the <span style='color:gold;'>**richest**</span> Europeans?",
       subtitle = "Percentiles of net wealth distributions in thousand Euros",
       caption = "Data: HFCS 2017, ECB. Figure: @matschnetzer") + 
  theme_minimal(base_family = "Roboto Condensed") +
  theme(plot.background = element_rect(fill = "black"),
        plot.margin = margin(t = 1, b = 1, l = 1, r = 1, unit = "lines"),
        plot.title.position = "plot",
        plot.title = element_markdown(size = 20),
        plot.subtitle = element_text(margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(margin = margin(t = 2, unit = "lines"), size = 7),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.1, color = "gray80"),
        legend.position = c(0.65, 0.3),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 8))

ggsave("wealth.png", width = 6, height = 5, dpi = 320)
