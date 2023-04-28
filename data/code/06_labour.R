########################
## 06 LABOUR · LABELS ##
########################

librarian::shelf(tidyverse, ggtext, eurostat, MetBrewer)

# Here are the new packages we will use
# remotes::install_github("https://github.com/jimjam-slam/ggflags")
librarian::shelf(ggflags, countrycode, sysfonts, showtext, ggrepel)

# Load data directly from Eurostat or load("06_labour.RData")
rawdat <- get_eurostat("une_rt_m", 
                       filters = list(geo = c("AT","DE","FR","ES","IT"), 
                                      age = "TOTAL", 
                                      sex = "T", 
                                      s_adj = "SA", 
                                      unit = "PC_ACT"), 
                       type = "label",       # Get labels instead of Eurostat codes
                       time_format = "date") # Get date in date format

# Shorten timeline and fix Germany label, add ISO2c code for flags
# Left_join means: Keep all columns of the first-mentioned dataset and add the selected columns of the second dataset. The "by" option gives the identifier columns in both datasets for the merger.
unemp <- rawdat |> 
  filter(time >= "2005-01-01") |>
  drop_na() |> 
  mutate(geo = ifelse(str_detect(geo, "Germany"), "Germany", geo)) |> 
  left_join(countrycode::codelist |> select(country.name.en, iso2c), by = c("geo" = "country.name.en"))

# Create dataset for annotations
# With "tribble" you can create a dataframe where the first row are the column names with a tilde, and then each row is a row in the dataframe
labunemp <- tribble(~geo, ~time, ~values, ~label,
                    "Austria", as.Date("2007-01-01"), 14, "The unemployment rate in Austria hit a historic high during the Covid-19 pandemic in June 2020.",
                    "Germany", as.Date("2013-01-01"), 10, "Unemployment in Germany has decreased substantially since 2005.",
                    "Spain", as.Date("2010-01-01"), 13, "During the economic crisis in 2013, Spain recorded alarming unemployment rates up to 26%.")

# Add Google Font for the labels
sysfonts::font_add_google("Roboto Condensed", family = "Roboto Condensed")
sysfonts::font_add_google("Oswald", family = "Oswald")
# When loading fonts from Google, don't forget the following command!
showtext_auto()
showtext_opts(dpi = 320)

unemp |> 
  ggplot(aes(x = time, y= values, color = geo)) +
  geom_hline(yintercept = 0, linewidth = 0.1, color = "gray40") +
  geom_area(aes(fill = geo)) +
  geom_line(linewidth = 0.6, aes(group = geo)) +
# We take only the minimum values per country (slice_min by geo) and create non-overlapping labels with the ggrepel package. If you do not like the glue-package you can create the labels with paste0: label = paste0("Min:", round(values,1), "%").
  geom_label_repel(data = unemp |> slice_min(values, n=1, with_ties = F ,by = geo), 
             size = 2.5, nudge_y = -2, label.padding = unit(0.15,"lines"), 
             family = "Roboto Condensed",
             aes(label = glue::glue("Min: {round(values,1)}%")),
             min.segment.length = unit(2, unit="pt"), segment.colour = "black", 
             segment.size = 0.3, arrow = arrow(type = "open", length = unit(0.01, "npc"))) +
# Let's do the same with the maximum values. The segment options define the line between the label and the data point. We also include an arrow!
  geom_label_repel(data = unemp |> slice_max(values, n=1, with_ties = F ,by = geo), 
             size = 2.5, nudge_y = 2, label.padding = unit(0.15, "lines"), 
             family = "Roboto Condensed",
             aes(label = glue::glue("Max: {round(values,1)}%")),
             min.segment.length = unit(2, unit="pt"), segment.colour = "black", 
             segment.size = 0.3, arrow = arrow(type = "open", length = unit(0.01, "npc"))) +
# ggflags needs 2-digit country codes (that we take from countrycode::codelist above). These should be in small letters, so we execute tolower(iso2c)!
  geom_flag(data = unemp |> slice_min(time, n=1), size = 6,
            aes(x = as.Date("2011-01-01"), y = 32, country = tolower(iso2c))) +
# Now we add the country names. We need each name only once, that's why we slice the dataframe. As we want the country in capital letters, we execute toupper(geo)
  geom_text(data = unemp |> slice_min(time, n=1), size = 4, hjust = 0, 
            family = "Roboto Condensed",
            aes(x = as.Date("2013-01-01"), y = 32, label = toupper(geo))) +
# Annotate the figure with the labels in the dataframe created above. We want the text to be gray with the exception of Spain where the color should be white.
  geom_text(data = labunemp, aes(label = str_wrap(label, width = 20)), 
            colour = ifelse(labunemp$geo == "Spain", "white", "gray20"), 
            lineheight = 0.9, hjust = 0, size = 2.6, family = "Roboto Condensed") +
# Have percentage symbols on the y-axis
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
# With the colorspace package we can "darken" or "lighten" color palettes! Here the fill scale of the area chart is lighter, the color of the lines are darker.
  scale_color_manual(values = colorspace::darken(met.brewer("Nattier")[5:1], 0.2)) +
  scale_fill_manual(values = colorspace::lighten(met.brewer("Nattier")[5:1], 0.2)) +
# Create small multiples by country
  facet_wrap(~geo, nrow = 1) +
# We can even add Markdown in our labels. The * in the subtitles mean italics. But we have to tell ggplot that subtitle should be rendered as markdown in the theme below!!
  labs(x = NULL, y = NULL,
       title = "Looking for a job",
       subtitle = "Unemployment rates 2005-2023, *seasonally adjusted*",
       caption = "Data: Eurostat. Figure: @matschnetzer") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(family = "Oswald", size = 24),
# Subtitle should be rendered as markdown (from ggtext package) rather than element_text!
        plot.subtitle = element_markdown(family = "Roboto Condensed", color = "gray40", 
                                         size = 14),
        plot.caption = element_text(family = "Roboto Condensed", color = "gray40", size = 7,
                                    margin = margin(t = 1, unit = "lines")),
        plot.title.position = "plot",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing.x = unit(1, "lines"))

ggsave(filename = "plots/unemp.png", width = 10, height = 4.5, dpi = 320)
 