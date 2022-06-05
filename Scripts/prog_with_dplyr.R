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

# across() within mutate() ####
# You can use the across() function inside of mutate() to specify which columns you'd like to work with, which function to apply to these columns, and any text that you'd like to add on to the names of the new columns.

# Multiply all columns that contain "as_perc_gdp" by gdp_in_billions_of_usd, keeping only the columns used in the analysis, and assign this result to imf_new_cols.
imf_new_cols <- imf_data %>% 
  mutate(across(.cols = contains("as_perc_gdp"), 
                .fns = ~ .x * gdp_in_billions_of_usd),
                .keep="used") 
imf_new_cols

# Replace _as_perc_gdp with empty string in tibble names
names(imf_new_cols) <- sub(
                  pattern="_as_perc_gdp",
                  replace="",
                  x=names(imf_new_cols))
# Display tibble with new names
names(imf_new_cols)

# across() within summarize() ####
# Keep only rows corresponding to Bolivia. Go across all columns containing "perc". Find the minimum value for each of these columns. Add "min_" to the beginning of each of the column names.
imf_data %>% 
  filter(country=="Bolivia") %>% 
  summarize(across(.cols=contains("perc"), 
                  .fns=min, .names="min_{.col}")) %>% 
  pivot_longer(cols = everything(), 
               names_to="Purpose", values_to = "Minimum")

# across() with count() ####
# The count() function also works with across() to provide the number of rows matching each combination of the variables specified.
# count() across character class columns and then arrange the count tibble with count decreasing.

imf_data %>% 
  count(across(.cols=where(is.character)), sort=TRUE)

# rowwise() and c_across() ####
# rowwise() can be a handy tool in your dplyr programming toolbox when combined with c_across(). Together, they allow you to perform calculations across different variables on each row. For example, this can be useful for counting missing values across each row for chosen variables.

# Set the pipeline up for calculations across each row. Create a column num_missing that contains each row's number of missing values in the columns gdp_in_billions_of_usd through to the last column in imf_data. Sort the results by number of missing entries in decreasing order.
imf_data %>% 
  rowwise() %>% 
  mutate(num_missing = sum(is.na(
    c_across(gdp_in_billions_of_usd:last_col()))
  )) %>% 
  select(country:year, num_missing) %>% 
  arrange(desc(num_missing)) 

# if_any() and if_all() ####
# You can use the if_any() and if_all() functions with select() helper verbs to check for conditions being met across any or all columns. 
# if_any -> or, if_all -> and
# Recall the between() function in dplyr can be useful for specifying a range of values (from one to another).

# Choose rows with negative values in any column names ending with "perc_change".
imf_data %>%
  filter(if_any(.cols = matches("perc_change$"), 
                .fns = ~ .x < 0)) %>% 
  select(country, year, ends_with("perc_change"))

# Choose rows where both import and export percentages have remained stable from the previous year (between -1% and 1%).
imf_data %>%
  filter(if_all(
    .cols=matches("perc_change$"),
    .fns = ~ between(.x, -1, 1))) %>%
  select(country, year, ends_with("perc_change"))

# 3. Set Theory Claus and The North Pole ####
# Subseting data
imf_subset <- imf_data %>% 
  select(iso, country, year, population_in_millions)

asia_wb <- world_bank_data %>% 
  filter(continent == "Asia") %>% 
  select(iso, country, year, contains(c("ferti", "college")))

# Return all the rows in asia_wb and only rows in imf_subset with matching key values.

asia_wb %>% 
  left_join(imf_subset)

# Return only the rows with matching key values in both asia_wb and imf_subset.

asia_wb %>% 
  inner_join(imf_subset)

# Return those rows in asia_wb that do not have matching key values in imf_subset.
asia_wb %>% 
  anti_join(imf_subset)
