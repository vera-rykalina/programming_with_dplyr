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

# select() ####
# Choose the country, year, usd_conversion_rate, and gdp_in_billions_of_usd columns in that order from imf_data.
selected_imf <- imf_data %>% 
  select(country, year, usd_conversion_rate, gdp_in_billions_of_usd)

# filter() ####
# Subset the rows of selected_imf where country is in countries_vector and assign to subsetted_imf; then, print subsetted_imf to check.
countries_vector <- c("Austria", "Argentina", "Bangladesh", "Belize", "New Zealand", "Tanzania")
                                        
subsetted_imf <- selected_imf %>% filter(country %in% countries_vector)

# summarise() ####  
# Calculate the median gdp_in_billions as median_gdp across the years, grouped by each country. Then calculate the median gdp_in_billions as median_gdp across the years, grouped by each country.
subsetted_imf %>% 
  mutate(gdp_in_billions = gdp_in_billions_of_usd / 
           usd_conversion_rate) %>% 
  group_by(country) %>% 
  summarise(median_gdp=median(gdp_in_billions)) 
# %>% pivot_wider(names_from=country, values_from = median_gdp)

# The helper functions in the tidyselect package: starts_with() and ends_with() from tidyselect and select()

# starts_with() ####
# Pick columns related to governmental measurements that begin with "gov".
imf_data %>% 
  select(starts_with("gov"))

# That selection isn't very helpful, so pick year, country, and those variables starting with "gov" in that order.
imf_data %>% 
  select(year, country, starts_with("gov"))

# ends_with()
# Return year, country, and those columns in imf_data that conclude with "change".
imf_data %>% 
  select(year, country, ends_with("change"))

# contains() ####
# looks for some string
# Pick country name, year, and columns with the string "gdp" in their name.
imf_data %>% 
  select(country, year, contains("gdp")) %>% 
  colnames()

# Pick country, year, and columns with names matching "as_perc" to identify those corresponding to percentages of GDP.
imf_data %>% 
  select(country, year, contains("as_perc")) %>% 
  colnames()

# matches() ####
# Pick country, year, and columns that have "perc" or "rate" in their names.
imf_data_1 <- imf_data %>% 
  select(country, year, matches("perc|rate"))

# alternative way with contains()
imf_data_2 <- imf_data %>% 
  select(country, year, contains(c("perc","rate")))
combi <-  cbind(imf_data_1, imf_data_2)
head(combi)

# Pick country, year, and columns that start with "gov" using a regular expression instead of using starts_with().
imf_data %>% 
  select(country, year, matches("^gov"))

# Pick country, year, and columns ending with "gdp" using a regular expression instead of using ends_with().
imf_data %>% 
  select(country, year, matches("gdp$"))

# 2. Providing relocation assistance ####
# everything() ####
# Remind yourself of the order of the imf_data column names.
names(imf_data)

# Create reordered_imf by reordering the columns of imf_data so that all columns ending with "gdp" are after year using select(), regular expressions, and everything(). 

reordered_imf <- imf_data %>% 
  select(iso:year,
         matches("gdp$"),
         everything())

# Then, print the names() of reordered_imf.
  names(reordered_imf)

# last_col() ####
# Choose columns iso through year. Then, match for columns starting with "gov" using regular expressions. Finally, have your remaining columns ordered as gdp_in_billions_of_usd to the last column.
  imf_data %>% 
    select(iso:year, matches("^gov"),
      gdp_in_billions_of_usd:last_col()) %>% 
    names()

# relocate() ####
# The relocate() function is helpful when you want to keep all of the columns in your data but move some of them around. select() can do this too, but it works better when a subset of columns is needed instead.
  
# Move the consumer_price_index column to appear after usd_conversion_rate and assign to relocated_cpi.
relocated_cpi <- imf_data %>% 
    relocate(consumer_price_index, 
             .after=usd_conversion_rate)
# Then, take a glimpse() at relocated_cpi.
glimpse(relocated_cpi)

# Shift population_in_millions to be before consumer_price_index in relocated_cpi.
relocated_cpi %>% 
  relocate(population_in_millions, .before=consumer_price_index) %>% names()
