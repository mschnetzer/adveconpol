########################
## 09 MOBILITY · MAPS ##
########################

librarian::shelf(tidyverse, eurostat, ggtext, showtext)

# And here are the new packages for today
librarian::shelf(rnaturalearth, rnaturalearthdata, giscoR, sf)

# Load Google fonts that we use
font_add_google("Alfa Slab One", family = "Alfa Slab One")
font_add_google("Roboto Condensed", family = "Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 320)

# MAP 1: Social mobility in European OECD countries
load("09_mobility.RData")
 
# Load map for Europe from naturalearth
map <- ne_countries(continent = "Europe", scale = "medium", returnclass = "sf")

# Alternatively, get map from Eurostat
# map <- gisco_get_countries(region = "Europe", resolution = "10", epsg = 4326)

# Set limits for Continental Europe
map |> ggplot() + geom_sf() + 
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)

# Change projection to Mercator
map |> st_transform('EPSG:3857') |> 
  ggplot() + geom_sf() +
  coord_sf(xlim = c(-3090000, 4500000), ylim = c(11500000,  4100000), expand = F)

# Merge the OECD mobility data to the map
finmap <- map |> left_join(socmob, by = c("iso_a3" = "iso3c"))

finmap |> st_transform('EPSG:3857') |> 
  mutate(generations = factor(generations)) |>
  filter(iso_a3 != "ISL") |> 
  ggplot() +
  geom_sf(aes(fill = generations), color = "black") +
  scale_fill_manual(name = "Expected **number of generations** <br>it takes an offspring from a family at the <br>bottom 10% to reach the mean income.",
                    values = RColorBrewer::brewer.pal(6, "YlOrRd"),
                    na.value = "gray90",
                    guide = guide_legend(title.position = "top", 
                                        label.position = "bottom",
                                        keywidth = 2, keyheight = 0.8,
                                        nrow = 1,
                                        override.aes = list(color = NA))) +
  annotate("text", x = -800000, y = 10700000, label = "Social Mobility in Europe",
           family = "Alfa Slab One", size = 7, hjust = 0.5) +
  coord_sf(xlim = c(-2790000, 4500000), ylim = c(11500000,  4100000), expand = F) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.27,0.78),
        legend.title = element_markdown(hjust = 0.5, size = 12, lineheight = 1.1), 
        legend.text = element_text(size = 11),
        legend.spacing.x = unit(0, "pt"))

ggsave("mobility.png", width = 8, height = 8, dpi = 320)

# MAP 2: Disposable income on NUTS-2 level

# Load data via eurostat package or locally: load("09_mobility.RData")
raw_inc <- get_eurostat("nama_10r_2hhinc", time_format = "raw", 
                         filters = list(unit = "PPS_EU27_2020_HAB", time = "2020", 
                                        na_item = "B6N", direct = "BAL"))

# Load Eurostat map for Europe
eumap_nuts2 <- get_eurostat_geospatial(resolution = "10", nuts_level = "2", year = 2021)

# Load background world map from rnaturalearth
bgmap <- ne_countries(continent = "Europe", returnclass = "sf", scale = "medium")

# Cut disposable income data into 8 classes (style: "equal" for equal distance between thresholds, "quantile" for equal group sizes, etc.)
inc_data <- raw_inc |> drop_na() |>   
  mutate(cat = cut_to_classes(values, n = 8, style = "quantile")) 

# Transform to ETRS89 projection
plotdat <- left_join(eumap_nuts2, inc_data) |> 
  st_transform('EPSG:3035')

plotdat |> 
  ggplot() + 
  geom_sf(data = bgmap, fill = "gray90", color = "white", linewidth = 0.1) +
  geom_sf(aes(fill = cat), linewidth = 0.05, color = "white") +
  scale_fill_brewer(palette = "YlOrRd", na.translate = F,
                    name = "Disposable income\nin PPP Euro",
                    guide = guide_legend(keywidth = 0.5, keyheight = 1.5)) +
  coord_sf(xlim = c(2500000, 6100000), ylim = c(1600000, 5200000)) +
  labs(caption = "Data: Eurostat (nama_10r_2hhinc)") +
  theme_void(base_family = "Roboto Condensed") +
  theme(legend.position = "right",
        plot.caption = element_text(color = "gray40", size = 8))

ggsave("income_nuts3.png", width = 8, height = 6, dpi = 320)
 