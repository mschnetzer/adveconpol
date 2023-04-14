########################
## 06 LABOUR · LABELS ##
########################

librarian::shelf(tidyverse, ggtext, eurostat, MetBrewer)

# Here are the new packages we will use
# remotes::install_github("https://github.com/jimjam-slam/ggflags")
librarian::shelf(ggflags, countrycode, sysfonts, showtext, ggrepel)

# Load data directly from Eurostat or load("06_labour.RData")
rawdat <- get_eurostat("une_rt_m", filters = list(geo = c("AT","DE","FR","ES","IT"), age = "TOTAL", sex = "T", s_adj = "SA", unit = "PC_ACT"), type = "label")

# Shorten timeline and fix Germany label, add ISO2c code for flags
unemp <- rawdat |> 
  filter(time >= "2005-01-01") |>
  drop_na() |> 
  mutate(geo = ifelse(str_detect(geo, "Germany"), "Germany", geo)) |> 
  left_join(countrycode::codelist |> select(country.name.en, iso2c), by = c("geo" = "country.name.en"))

# Create dataset for annotations
labunemp <- tribble(~geo, ~time, ~values, ~label,
                    "Austria", as.Date("2007-01-01"), 14, "The unemployment rate in Austria hit a historic high during the Covid-19 pandemic in June 2020.",
                    "Germany", as.Date("2013-01-01"), 10, "Unemployment in Germany has decreased substantially since 2005.",
                    "Spain", as.Date("2010-01-01"), 13, "During the economic crisis in 2013, Spain recorded alarming unemployment rates up to 26%.")

# Add Google Font for the labels
sysfonts::font_add_google("Roboto Condensed", family = "Roboto Condensed")
sysfonts::font_add_google("Oswald", family = "Oswald")
showtext_auto()
showtext_opts(dpi = 320)

unemp |> 
  ggplot(aes(x = time, y= values, color = geo)) +
  geom_hline(yintercept = 0, linewidth = 0.1, color = "gray40") +
  geom_area(aes(fill = geo)) +
  geom_line(linewidth = 0.6, aes(group = geo)) +
  geom_label_repel(data = unemp |> slice_min(values, n=1, with_ties = F ,by = geo), 
             size = 2.5, nudge_y = -2, label.padding = unit(0.15,"lines"), family = "Roboto Condensed",
             aes(label = glue::glue("Min: {round(values,1)}%")),
             min.segment.length = unit(2, unit="pt"), segment.colour = "black", 
             segment.size = 0.3, arrow = arrow(type = "open", length = unit(0.01, "npc"))) +
  geom_label_repel(data = unemp |> slice_max(values, n=1, with_ties = F ,by = geo), 
             size = 2.5, nudge_y = 2, label.padding = unit(0.15, "lines"), family = "Roboto Condensed",
             aes(label = glue::glue("Max: {round(values,1)}%")),
             min.segment.length = unit(2, unit="pt"), segment.colour = "black", 
             segment.size = 0.3, arrow = arrow(type = "open", length = unit(0.01, "npc"))) +
  geom_flag(data = unemp |> slice_min(time, n=1), size = 6,
            aes(x = as.Date("2011-01-01"), y = 32, country = tolower(iso2c))) +
  geom_text(data = unemp |> slice_min(time, n=1), size = 4, hjust = 0, family = "Roboto Condensed",
            aes(x = as.Date("2013-01-01"), y = 32, label = toupper(geo))) +
  geom_text(data = labunemp, aes(label = str_wrap(label, width = 20)), 
            colour = ifelse(labunemp$geo == "Spain", "white", "gray20"), 
            lineheight = 0.9, hjust = 0, size = 2.6, family = "Roboto Condensed") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = colorspace::darken(met.brewer("Nattier")[5:1], 0.2)) +
  scale_fill_manual(values = colorspace::lighten(met.brewer("Nattier")[5:1], 0.2)) +
  facet_wrap(~geo, nrow = 1) +
  labs(x = NULL, y = NULL,
       title = "Looking for a job",
       subtitle = "Unemployment rates 2005-2023, *seasonally adjusted*",
       caption = "Data: Eurostat. Figure: @matschnetzer") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(family = "Oswald", size = 24),
        plot.subtitle = element_markdown(family = "Roboto Condensed", color = "gray40", size = 14,
                                     margin = margin(b = 1, unit = "lines")),
        plot.caption = element_text(family = "Roboto Condensed", color = "gray40", size = 7,
                                    margin = margin(t = 1, unit = "lines")),
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing.x = unit(1, "lines"))

ggsave(filename = "unemp.png", width = 10, height = 4.5, dpi = 320)
 