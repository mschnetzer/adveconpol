############################
## 04 GROWTH · GEOMETRIES ##
############################

# load packages
library(tidyverse)
library(eurostat)
library(MetBrewer)

# Search datasets for GDP (per capita)
search_eurostat("GDP") |> View()

# get GDPpc data from Eurostat
rawgdp <- get_eurostat("nama_10_pc", time_format = "num", type = "label", filters = list(geo = c("AT","FR","IT","DE","ES")))

# Alternatively, load local RData file
# load("04_growth.RData")

View(rawgdp)

gdp <- rawgdp |> 
  filter(unit %in% c("Chain linked volumes (2010), euro per capita",
            "Chain linked volumes, percentage change on previous period, per capita"),
         na_item == "Gross domestic product at market prices") |>  
  mutate(geo = ifelse(str_detect(geo, "Germany"), "Germany", geo)) |> 
  filter(time %in% 2000:2021) |> 
  drop_na()


## Let's try different geometries
# 1. Line plot with evolution of GDPpc
gdp |> filter(unit == "Chain linked volumes (2010), euro per capita") |> 
  ggplot(aes(x = time, y = values, group = geo, color = geo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = NULL, values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", big.mark = ",")) +
  labs(x = NULL, y = NULL,  title = "Evolution of GDP 2000-2021", 
       subtitle = "GDP per capita in €") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")
ggsave("gdp_evolution.png", width = 6, height = 4, dpi = 320)

# Annotation within the plot; for casual style, try "stat = 'smooth'"
library(geomtextpath)
gdp |> filter(unit == "Chain linked volumes (2010), euro per capita") |> 
  ggplot(aes(x = time, y = values, group = geo, color = geo)) +
  geomtextpath::geom_textline(aes(label = geo), hjust = 0.75, vjust = 0.5, #stat = "smooth",
                              size = 3, fontface = "bold") +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::number_format(prefix = "€", big.mark = ",")) +
  labs(x = NULL, y = NULL, title = "Evolution of GDP 2000-2021", 
       subtitle = "GDP per capita in €") +
  theme_minimal() +
  theme(legend.position = "none")
  

# 2. Create base year for better comparability of evolution & reorder countries for legend
gdpbase <- gdp |> filter(unit == "Chain linked volumes (2010), euro per capita") |> 
  mutate(base = values / values[time == 2000] * 100, .by = geo) |> 
  mutate(geo = fct_reorder2(geo, time, base))

gdpbase |> ggplot(aes(x = time, y = base, group = geo, color = geo)) +
  geom_hline(yintercept = 100, color = "black", linewidth = 0.5) +
  geom_line(linewidth = 0.8) +
  geom_label(aes(label = round(base,0)), data = gdpbase |> slice_max(time, n=1), 
            hjust = 0.5, size = 2.5, show.legend = F) +
  scale_color_manual(name = NULL, values = met.brewer("Juarez")) +
  scale_x_continuous(expand = c(0.07,0)) +
  labs(x = NULL, y = NULL, title = "Evolution of GDP", 
       subtitle = "GDP per capita (2000 = 100)") +
  theme_minimal() +
  theme(legend.position = "right")


# 3. Boxplots with growth rates
gdpgrowth <- gdp |> filter(str_detect(unit, "percentage"))

gdpgrowth |> ggplot(aes(x = geo, y = values, group = geo, color = geo)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = NULL, y = NULL, title = "Dispersion of GDP growth rates",
       subtitle = "Growth rates of per capita GDP between 2000 and 2021") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


# 4. Facets with bar charts
gdpgrowth |> ggplot(aes(x = time, y = values, fill = geo)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~geo) +
  scale_fill_manual(values = met.brewer("Juarez")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = NULL, y = NULL, title = "GDP growth rates from 2000 to 2021") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# 5. Lollipop chart 
gdpgrowth |> ggplot(aes(x = time, y = values)) + 
  geom_segment(aes(xend = time, yend = 0), color = "gray90", size = 2.5) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  geom_point(aes(color = ifelse(values > 0, "pos", "neg")), size = 2.5) +
  facet_wrap(~geo) +
  scale_color_manual(values = c("pos" = "darkolivegreen3", "neg" = "red")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = NULL, y = NULL, title = "GDP growth rates from 2000 to 2021") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.3),
        plot.title = element_text(hjust = .5, size = 20))
