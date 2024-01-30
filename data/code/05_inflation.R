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
  mutate(Coicop = fct_relevel(Coicop, "Inflation", "Others", "Transport", "Recreation & Culture", "Restaurants & Hotels", "Food", "Housing & Energy")) |> 
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

ggsave("plots/inflation_stream.png", width = 6, height = 8, dpi=320, bg = "white")


## Now, let's create a line chart with inflation rates across Europe

# remotes::install_github("https://github.com/jimjam-slam/ggflags")
library(ggflags)
library(gghighlight)

# Load local data
load("05_inflation.RData")

euinfdata |> filter(time >= "2020-01-01") |> 
  ggplot(aes(x = time, y= values, color = geo.de)) +
  geom_hline(yintercept = 0, linewidth = 0.1, color = "gray40") +
  geom_line(linewidth = 0.8, aes(group = geo)) +
  gghighlight(unhighlighted_params = list(color = "gray80", linewidth = 0.2), use_direct_label = F) +
  geom_label(data = euinfdata |> slice_max(time, n=1), size = 2.3,
             aes(label = glue::glue("{round(values,1)}%")), nudge_y = -0.5,
             nudge_x = 20, label.padding = unit(0.15,"lines"), fill = "gray98") +
  geom_label(data = euinfdata |> slice_max(values, n=1, with_ties = F ,by = geo), size = 2.3,
             aes(label = glue::glue("{round(values,1)}%")),
             nudge_y = 1, label.padding = unit(0.15,"lines"), fill = "gray98") +
  geom_flag(data = euinfdata |> slice_min(time, n=1), size = 5,
            aes(x = as.Date("2020-04-01"), y = 5, country = tolower(iso2c))) +
  geom_text(data = euinfdata |> slice_min(time, n=1), size = 3, hjust = 0,
            aes(x = as.Date("2020-06-01"), y = 5, label = toupper(geo))) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_date(limits = c(NA_Date_, as.Date("2023-05-01"))) +
  scale_color_manual(values = met.brewer("Tiepolo")) +
  facet_wrap(~geo.de) +
  labs(x = NULL, y = NULL,
       title = "Inflation roller coaster",
       subtitle = "Harmonised inflation rates (HIPC) for selected European countries",
       caption = "Source: Eurostat. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 8) +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11, margin = margin(b=1, unit="lines")),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1, "lines"))

ggsave("plots/inflation_eu.png", width = 8, height = 4.2, dpi=320, bg = "white")
