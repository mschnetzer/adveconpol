########################
## 09 MOBILITY · MAPS ##
########################

librarian::shelf(tidyverse, eurostat)

# And here are the new packages for today
librarian::shelf(rnaturalearth, rnaturalearthdata, sf)

# Load data via eurostat package or locally: load("07_mobility.RData")
raw_inc <- get_eurostat("nama_10r_2hhinc", time_format = "raw", 
                         filters = list(unit = "PPS_EU27_2020_HAB", time = "2020", 
                                        na_item = "B6N", direct = "BAL"))


eumap_nuts2 <- get_eurostat_geospatial(resolution = "10", nuts_level = "2", year = 2021)

inc_data <- raw_inc |> drop_na() |>   
  mutate(cat = cut_to_classes(values, n = 8)) 

plot_dat <- left_join(eumap_nuts2, inc_data) |> 
  st_transform(crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs')

bgmap <- ne_countries(continent = "Europe", returnclass = "sf", scale = "medium")

plot_dat |> 
  ggplot() + 
  geom_sf(data = bgmap, fill = "gray95", color = "white", linewidth = 0.1) +
  geom_sf(aes(fill = cat), linewidth = 0.05, color = "white") +
  scale_fill_brewer(palette = "YlOrRd", na.translate=FALSE,
                    name = str_wrap("Disposable income in PPP Euro", width = 10),
                    guide = guide_legend(keywidth = 0.5, keyheight = 1.5)) +
  coord_sf(xlim = c(2500000, 6100000), ylim = c(1600000, 5200000)) +
  labs(caption = "Data: Eurostat (nama_10r_2hhinc)") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(family = "Roboto Condensed"),
        legend.text = element_text(family = "Roboto Condensed"),
        plot.caption = element_text(family = "Roboto Condensed", color = "gray40"),
        legend.spacing.y = unit(1, "cm"))

