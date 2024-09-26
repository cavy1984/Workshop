install.packages("dplyr") #dataframe manipulation
install.packages("tidyr") #reshaping data
install.packages("palmerpenguins") # demo datasheet
library(dplyr)
library(tidyr)
library(palmerpenguins)
library(ggplot2)

penguins_data<- penguins
head(penguins)
str(penguins)

## use the pipe function |>
new_object <-penguins_data |>
  head()
new_object
write.csv(penguins_data,
          "penguins.csv",
          row.names = FALSE)
# Select operates on column
penguins_locations <-penguins_data |>
  select(species,island)
# Filter operates on rows
adelie_penguins <-penguins_data |>
  filter(species == "Adelie")
#add or modify columns-"mutate"
#create a new data frame with the original data and add a column of "penguins_ratio"
penguins_ratio <-penguins_data |>
  mutate(bill_length_mm_ratio = bill_length_mm /bill_depth_mm )

#create a new data frame with the original data and add a column of "penguins_rounded"
# the "round" function will round the data
# the ", digitd = 2" after teh round command will specify the digits to keep in rounding. Default without this code, will be 0 decimal 
penguins_rounded <-penguins_data |>
  mutate(bill_length_mm_rounded =round(bill_length_mm, digits = 2))

#create a new data frame with the original data and add rounded columns ended with mm
#and append "_rounded" to these new columns to the data frame data
penguins_rounded <-penguins_data |>
  mutate (across(ends_with("mm"), round, .names ="{.col}_rounded"))

#Write a function to define decimals of all data end with mm and lb
penguins_rounded <-penguins_data |>
  mutate (across(ends_with("mm"), \(x) {round(x, digits = 2)}, .names ="{.col}_rounded"),
          across(ends_with("g"), \(x) {round(x, digits = 1)}, .names ="{.col}_rounded")
          )

#summarize the dataframe
penguin_species <- penguins_data |>
  group_by(species, island) |>
  summarize(total_penguins = n(),
            total_penguin_biomass = sum(body_mass_g)) |>
  ungroup()
# make sure to use "ungroup()" to reset the dataframe group status avoid error in downstream analysis


###Sept26, 2024 code practice for data manipulation.

# Create a new branch for this session's work

# Create a new script, call it "07-data-manip.R"

# Install dplyr and tidyr

if (FALSE) {
  install.packages("dplyr")
  install.packages("tidyr")
  install.packages("palmerpenguins")
  install.packages("ggplot2")
}

library(dplyr)
library(tidyr)
library(palmerpenguins)
library(ggplot2)

# Load data

penguins_data <- penguins

# Create a directory to store the raw data
# Store the raw data there

write.csv(
  penguins_data,
  "penguins_data_raw.csv",
  row.names = F
)

#### Looking at the penguins data ####

penguins_data
head(penguins_data)
tail(penguins_data)

str(penguins_data)
# str is useful for tibbles
# note different data types
# note factors
# note presence of NAs

#### Data manipulation ####

##### Assignment and pipes #####

# We assign values to variable names using the <-
# The = also works in *most* instances
# We use pipes to chain multiple operations together.
# Keyboard shortcuts: look up in Tools -> Keyboard Shortcuts Help

##### Subsetting and filtering data #####

penguin_locations <- penguins |>
  select(species, island)

penguin_measures <- penguins |>
  select(species, ends_with("mm"))

adelie_penguins <- penguins |>
  filter(species == "Adelie")

##### Adding columns (mutate) #####

penguins_ratio <- penguins |>
  mutate(bill_length_depth_ratio = bill_length_mm / bill_depth_mm)

penguins_rounded <- penguins |>
  mutate(bill_length_mm_rounded = round(bill_length_mm, digits = 0))

penguins_rounded <- penguins |>
  mutate(across(ends_with("mm"), round))

penguins_rounded <- penguins |>
  mutate(across(ends_with("mm"), round, .names = "{.col}_rounded"))

##### Split-apply-combine #####

penguin_summary <- penguins |>
  group_by(island, species) |>
  summarize(total_penguins = n(),
            total_penguin = sum(body_mass_g)) |>
  ungroup()

penguin_totals <- penguins |>
  group_by(island, species) |>
  summarize(total_penguins = n(),
            total_penguin = sum(body_mass_g, na.rm = T)) |>
  ungroup()

penguin_summary <- penguins |>
  group_by(island, species, sex) |>
  summarize(across(ends_with("mm"), mean)) |>
  ungroup()

penguin_summary <- penguins |>
  group_by(island, species, sex) |>
  summarize(across(ends_with("mm"), \(x) mean(x, na.rm = T))) |>
  ungroup()

##### Reshaping #####

penguins_site_by_species <- penguin_totals |>
  select(species, island, total_penguins)

penguins_site_by_species <- penguin_totals |>
  select(species, island, total_penguins) |>
  tidyr::pivot_wider(id_cols = island,
                     names_from = species,
                     values_from = total_penguins)

penguins_site_by_species <- penguin_totals |>
  select(species, island, total_penguins) |>
  tidyr::pivot_wider(
    id_cols = island,
    names_from = species,
    values_from = total_penguins,
    values_fill =  0
  )

penguins_back_to_totals <- penguins_site_by_species |>
  tidyr::pivot_longer(-island, names_to = "Species", values_to = "Abundance")

##### joins + advanced joins #####

island_coordinates <- data.frame(
  island = c("Biscoe", "Dream", "Torgersen"),
  latitude = c(-65.433, -64.733, -64.766),
  longitude = c(-65.5, -64.344, -64.083)
)

ggplot(island_coordinates, aes(latitude, longitude)) +
  geom_point() +
  geom_text(aes(label = island), nudge_x = .07)

penguins_coords <- left_join(penguins, island_coordinates)

ggplot(penguins_coords, aes(longitude, body_mass_g, color = species)) +
  geom_jitter() +
  scale_color_viridis_d(option = "mako", begin = .2, end = .8)

#### Modify - add - commit ####

# adds penguins_data_raw repo
# commit and push

#### Palmer penguins citation ####
citation("palmerpenguins")










