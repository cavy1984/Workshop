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

#summarize the dataframe
penguin_species <- penguins_data |>
  group_by(species, island) |>
  summarize(total_penguins = n(),
            total_penguin_biomass = sum(body_mass_g)) |>
  ungroup()
# make sure to use "ungroup()" to reset the dataframe group status avoid error in downstream analysis


