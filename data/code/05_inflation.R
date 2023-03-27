###########################
## 05 INFLATION · COLORS ##
###########################

# Let's learn a new way to load multiple packages
librarian::shelf(tidyverse, janitor, ggstream, ggtext)

# Open data by Statistics Austria: https://data.statistik.gv.at
rawdata <- read.csv2("https://data.statistik.gv.at/data/OGD_vpi15_VPI_2015_1.csv")
labels <- read.csv2("https://data.statistik.gv.at/data/OGD_vpi15_VPI_2015_1_HEADER.csv")
coicop <- read.csv2("https://data.statistik.gv.at/data/OGD_vpi15_VPI_2015_1_C-VPI5-0.csv")

# Clean up data: 1) Extract date; 2) Label COICOP; 3) Get into right shape for summarizing
## Note: Take a look at regular expressions (regex) for string manipulation: https://stringr.tidyverse.org/articles/regular-expressions.html

inflation <- rawdata |> 
  mutate(date = ym(str_extract(C.VPIZR.0, "(\\d)+"))) |> 
  left_join(coicop |> select(code, en_name), by = c("C.VPI5.0"="code")) |> 
  select(date, coicop = en_name, contribution = F.VPIEFVJM) |> 
  drop_na() |> 
  pivot_wider(names_from = coicop, values_from = contribution) |> 
  clean_names()

# Summarize COICOP categories and reshape for ggplot (long format)
plotdat <- inflation |> 
  mutate(`Housing & Energy` = x04_housing_water_electricity_gas_and_other_fuels +
           x05_furnishings_household_equipment_and_routine_maintenance_of_the_house,
         Others = x02_alcoholic_beverages_tobacco_and_narcotics + x03_clothing_and_footwear +
           x06_health + x08_communication + x10_education + 
           x12_miscellaneous_goods_and_services) |> 
  select(Date = date, 
         Inflation = total_index_according_to_coicop,
         Food = x01_food_and_non_alcoholic_beverages,
         `Housing & Energy`,
         Transport = x07_transport,
         `Recreation & Culture` = x09_recreation_and_culture,    
         `Restaurants & Hotels` = x11_restaurants_and_hotels,              
         Others) |> 
  pivot_longer(cols = -Date, names_to = "Coicop", values_to = "Contribution") |> 
  mutate(Coicop = fct_reorder2(Coicop, Date, Contribution, .desc = F)) |> 
  filter(Date >= "2019-01-01")

# Let's test the data
plotdat |> slice_max(Date, n=1)
plotdat |> slice_max(Date, n=1) |> filter(Coicop != "Inflation") |> 
  summarise(sum(Contribution))


baseplot <- plotdat |> filter(!Coicop == "Inflation") |> 
  ggplot(aes(x = Date, y = Contribution, group = Coicop, fill = Coicop)) +
  geom_stream(type = "mirror") +
  scale_x_date(date_labels = "%b %y", position = "top", breaks = "4 months") +
  coord_flip(expand = F) +
  labs(x=NULL, y=NULL) +
  annotate("label", x = as.Date("2020-11-01"), y = -5.3, hjust = 0,
           label = "Inflation in Austria", family = "Playfair Display", size = 5, 
           label.size = NA) +
  annotate("text_box", x = as.Date("2020-10-01"), y = -5.3, hjust = 0, vjust = 1,
           label = "The figure shows contributions of selected COICOP consumption categories to the aggregate CPI. The values depict year-on-year changes.", 
           family = "Raleway", size = 3, color = "gray20", width = unit(5.2, "cm"), 
           box.colour = NA) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.27),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.text = element_text(size = 9, family = "Raleway"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.2),
        axis.text.y = element_text(color = "gray50", size = 7, family = "Raleway"),
        axis.text.x = element_blank())


# Load some color libraries
library(colorspace)
library(RColorBrewer)
library(MetBrewer)
library(wesanderson)
library(futurevisions)
library(viridis)

# Common RColorBrewer palette
baseplot + scale_fill_manual(values = c("gray90", brewer.pal(name="Accent", n=5)), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# You can lighten or darken the palette with the colorspace package
baseplot + scale_fill_manual(values = c("gray90", darken(brewer.pal(name="Accent", n=5), .3)), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# Nice MetBrewer palette: https://github.com/BlakeRMills/MetBrewer
baseplot + scale_fill_manual(values = c("gray90", met.brewer("Juarez")[-4]), 
                  name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# Lighten the palette
baseplot + scale_fill_manual(values = c("gray90", lighten(met.brewer("Juarez")[-4], .1)), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# Meet the wesanderson palette: https://github.com/karthik/wesanderson
baseplot + scale_fill_manual(values = c("gray90", wes_palette("Zissou1")), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# Futurevisions is also awesome: https://github.com/JoeyStanley/futurevisions
baseplot + scale_fill_manual(values = c("gray90", futurevisions("cancri")), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

# Viridis colorblind-friendly palette
baseplot + scale_fill_manual(values = viridis(n=6, option = "B",  direction = -1), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

baseplot + scale_fill_manual(values = viridis(n=6, option = "D",  direction = -1), 
                             name = "", guide = guide_legend(reverse = TRUE, keywidth = 0.5))

ggsave("inflation.png", width = 6, height = 8, dpi=320)