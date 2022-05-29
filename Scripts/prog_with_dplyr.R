# DataCamp Course
# ---- Programming with dplyr by Chester Ismay -----

# Load packages
pacman::p_load("tidyverse", "rlang")
# Load data
imf_data <- readRDS("Data/imf_data.rds")
head(imf_data)
world_bank_data <- readRDS("Data/world_bank_data.rds")
head(world_bank_data)
# 1. Hold Your Selected Leaders Accountable ####

# Choose the country, year, usd_conversion_rate, and gdp_in_billions_of_usd columns in that order from imf_data.
selected_imf <- imf_data %>% 
  select(country, year, usd_conversion_rate, gdp_in_billions_of_usd)

# Subset the rows of selected_imf where country is in countries_vector and assign to subsetted_imf; then, print subsetted_imf to check.
countries_vector <- c("Austria", "Argentina", "Bangladesh", "Belize", "New Zealand", "Tanzania")
                                        
subsetted_imf <- selected_imf %>% filter(country %in% countries_vector)
  