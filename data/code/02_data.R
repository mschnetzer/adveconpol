############################
# 02 BASICS OF DATA HANDLING
############################

# Load the tidyverse
library(tidyverse)

##############
## PENGUINS ##
##############

# Load library
library(palmerpenguins)

# Assign data
data <- penguins

# Take a look at the data
head(data)
View(data)

# What are the specific classes of the variables
glimpse(data)
levels(data$species)

# Show some descriptive statistics
summary(data$bill_length_mm) # There are 2 NA values
mean(data$bill_length_mm, na.rm = T)
sd(data$bill_length_mm, na.rm = T)

## What is the mean flipper length?


# The summary command even works for factor variables
summary(data$species)

## How many penguins were recorded on Torgersen island?


# Basic tidyr: filter and select. Use a pipe!
data |> select(species, island)
data |> filter(species == "Adelie")
data |> filter(species %in% c("Adelie","Gentoo"))

## Filter penguins with a bill length greater than 45!


# Select and rename
data |> select(Art = species, Insel = island, Geschlecht = sex)

# Counting
data |> count(island)
data |> count(species, island)
data |> count(species, island, .drop = FALSE)

# Calculate summarise statistics
data |> summarise(meanbill = mean(bill_length_mm, na.rm = T),
                  sdbill = sd(bill_length_mm, na.rm = T),
                  meanflipper = mean(flipper_length_mm, na.rm = T),
                  sdflipper = sd(flipper_length_mm, na.rm = T))

# Summary statistics by group
data |> group_by(species) |> 
  summarise(meanbill = mean(bill_length_mm, na.rm = T))
data |> summarise(meanbill = mean(bill_length_mm, na.rm = T), .by = species)

## Calculate maximum flipper length by island


# Median body mass
data |> summarise(median = median(body_mass_g, na.rm = T),
                  medianq = quantile(body_mass_g, probs = 0.5, na.rm = T))

## Calculate P25 of body mass by sex


# Summary statistic for selected variable type
data |> summarise(across(c(bill_length_mm, bill_depth_mm), ~mean(., na.rm = T)))
data |> summarise(across(where(is.numeric), ~mean(., na.rm = T)))

# Control with nice select function
data |> select(contains("length")) # other options: starts_with; ends_with
data |> summarise(across(contains("length"), ~mean(., na.rm = T)))

# Drop observations with missing values (and save)
data <- data |> drop_na()

# Create new variables with mutate (and don't forget to save in dataset)
data <- data |> mutate(totallength = bill_length_mm + flipper_length_mm)
data <- data |> mutate(color = case_when(sex == "male" ~ "darkgreen",
                                         sex == "female" ~ "darkred"))

# Arrange data by column in descending order
data |> arrange(desc(bill_length_mm))

# Get top 3 observations for bill length by species
top3 <- data |> slice_max(bill_length_mm, n = 3, by = species)

# Get bottom 10% of observations with smallest bill length
top10p <- data |> slice_min(bill_length_mm, prop = 0.1)

## Show 3 observations with smallest body mass by island


############
## PART 2 ##
############

devtools::install_github("ropengov/eurostat")
library(eurostat)

# Search eurostat database for GDP data
search_eurostat("GDP") |> View()

# Download the complete national accounts dataset
gdp <- get_eurostat("nama_10_gdp", time_format = "num", type = "label",
                    filters = list(geo = "AT"))

# Alternatively, load local RData file
# load("02_data.RData")

# Let's take a look at all the components
unique(gdp$na_item)
unique(gdp$unit)

gdpat <- gdp |> filter(unit == "Current prices, million euro",
                       na_item %in% c("Gross domestic product at market prices",
                                      "Compensation of employees"))

gdpat_wide <- gdpat |>
  pivot_wider(names_from = "na_item", values_from = "values")

# Create sub-datasets
sub1 <- gdpat_wide |> select(geo, time, `Gross domestic product at market prices`)
sub2 <- gdpat_wide |> select(geo, time, `Compensation of employees`)

# Join dataset
fulldata <- left_join(sub1, sub2)

# Calculate wage share in % of GDP
fulldata <- fulldata |> 
  mutate(wageshare = `Compensation of employees`/`Gross domestic product at market prices`*100)

fulldata |> 
  ggplot(aes(x = time, y = wageshare)) + 
  geom_line()
