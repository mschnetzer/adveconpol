librarian::shelf(tidyverse, janitor, ggstream, ggtext, colorspace, MetBrewer, msthemes)

raw <- readxl::read_xlsx("energymix.xlsx",
                         col_types = c("numeric","text","numeric"))

translate <- tribble(~Art, ~Type,
                     "Kohle", "Coal",
                     "Erdöl", "Oil",
                     "Erdgas", "Natural gas",
                     "Biogene Brennstoffe", "Biogenic fuels",
                     "Sonstige Brennstoffe", "Other fuels",
                     "Speicherkraftwerke", "Storage power plant",
                     "Laufkraftwerke", "Flow power plant",
                     "Wind", "Wind",
                     "Photovoltaik", "Photovoltaics",
                     "Geothermie", "Geothermics"
                     )


plotdat <- raw |>
  rename(Art = `Group: Laufkraftwerke - Speicherkraft...`) |> 
  left_join(translate) |>
  mutate(Jahr = ymd(Jahr, truncated = 2L),
         Type = factor(Type, levels = rev(translate$Type)))

pal <- met.brewer("Lakota")[c(2,1,5,3)]
cols <- c(rbind(lighten(pal, 0.1), darken(pal, 0.2)))

plotdat |> filter(!Art %in% c("Sonstige Brennstoffe","Geothermie")) |> 
  ggplot(aes(x = Jahr, y = Value, group = Type, fill = Type)) +
  geom_stream(type = "mirror") +
  annotate("label", x = as.Date("1950-01-01"), y = 43000, hjust = 0,
           label = "Energy mix in Austria", family = "Futura", size = 4, 
           label.size = NA) +
  annotate("label", x = as.Date("1950-04-01"), y = 33000, hjust = 0, vjust = 1,
           label = "Power generation in GWh since 1950\nData: E-Control | Figure: @matschnetzer", 
           family = "Roboto", size = 2, color = "gray20", label.size = NA, fill = "white") +
  scale_fill_manual(values = cols, name = "", guide = guide_legend(keywidth = 0.3, keyheight = 1)) +
  scale_x_date(position = "bottom") +
  labs(x=NULL, y=NULL) +
  coord_cartesian(expand = F) +
  theme_ms() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        legend.text = element_text(size = 7),
        panel.grid.major.y = element_blank())

ggsave("energy.png", dpi = 320, width = 8, height = 2)
