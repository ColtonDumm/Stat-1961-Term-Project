---
title: "Home Prices Project"
author: "Colton Dumm and Ryan Quinlan"
date: "`r Sys.Date()`"
output: word_document
---

### This is the link to the redfin data that is used in the project for download. 

Warning it is 2 gb so it will take a second to load in and use. This is the link: https://redfin-public-data.s3.us-west-2.amazonaws.com/redfin_covid19/weekly_housing_market_data_most_recent.tsv000


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Load in all of the necessary data and libraries for the project

```{r}
# Load all of the necessary libraries 
library(dplyr)
library(readr)
library(tidyverse)
library(scales) # For dollar formatting
```

```{r}
# Read in the Redfin data
redfin_data <- read_tsv("weekly_housing_data_most_recent.tsv000")
```

# Exploratory Data Analysis 

To get a good sense of what we can do with the Redfin data we have obtained on housing we need to first understand what we are working with so we can determine what questions we can answer.

### The Structure of the data:

```{r}
# Look at the structure of the data
str(redfin_data)
```

### The Summary of the data:

The summary function helps us to achieve this by looking at the stats of the quantatative features of out data.

```{r}
# Look at a summary of the data we are working with
summary(redfin_data)
```

### A Look at the Catagorical Features:

The summary function gives us a good look at the stats of the numerical values but we also need to check the categorical features as well.

```{r}
# To check the categorical features
lapply(RedfinData[, sapply(RedfinData, is.character)], table)
```

### Data Visualization

First to get a better idea of the data and what is going on we should look at the US as a whole. We will make the us_trends dataset which is a culmination of all of the counties in the US to get a geasp on what is going on in the nation.

```{r}
us_trends <- redfin_data %>% 
  group_by(period_begin) %>% 
  summarize(
    avg_median_sale_price = mean(median_sale_price, na.rm = TRUE),
    total_active_listings = sum(active_listings, na.rm = TRUE),
    avg_median_days_on_market = mean(median_days_on_market, na.rm = TRUE),
    avg_months_of_supply = mean(months_of_supply, na.rm = TRUE),
    avg_percent_price_drops = mean(percent_active_listings_with_price_drops, na.rm = TRUE)
  )
```

Now that we have our us_trends data we can visualize some of the features of the data.

### Median Sale Price Over Time

```{r}
ggplot(us_trends, aes(x = period_begin, y = avg_median_sale_price)) +
  geom_line(color = "blue") +
  labs(
    title = "Average Median Sale Price Over Time (United States)",
    x = "Time",
    y = "Average Median Sale Price (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar) + # Format y-axis as dollar amounts using the scales library
  theme_minimal()


```

```{r}
ggplot(us_trends, aes(x = period_begin, y = total_active_listings)) +
  geom_line(color = "green") +
  labs(
    title = "Total Active Listings Over Time (United States)",
    x = "Time",
    y = "Total Active Listings"
  ) +
  scale_y_continuous(labels = scales::comma) + # Format the y axis to have commas for large numbers
  theme_minimal()

```

```{r}
ggplot(us_trends, aes(x = period_begin, y = avg_median_days_on_market)) +
  geom_line(color = "orange") +
  labs(
    title = "Average Median Days on Market Over Time (United States)",
    x = "Time",
    y = "Average Median Days on Market"
  ) +
  theme_minimal()

```

```{r}
# Months of Supply over Time
months_of_supply_plot <- ggplot(us_trends, aes(x = period_begin, y = avg_months_of_supply)) +
  geom_line(color = "red") +
  labs(
    title = "Months of Supply Over Time",
    x = "Time",
    y = "Months of Supply"
  ) +
  theme_minimal()

# Display the plot
months_of_supply_plot
```

We will now take a look at the relationship between the between the supply of houses and the median sale price of a house

```{r}
ggplot(us_trends, aes(x = avg_months_of_supply, y = avg_median_sale_price)) +
  geom_point() +
  labs(title = "Supply vs. Price Relationship")
```


```{r}
# Percent Price Drops over Time
percent_price_drops_plot <- ggplot(us_trends, aes(x = period_begin, y = avg_percent_price_drops)) +
  geom_line(color = "purple") +
  labs(
    title = "Percent Price Drops Over Time",
    x = "Time",
    y = "Percent Price Drops"
  ) +
  theme_minimal()

# Display the plot
percent_price_drops_plot
```


### Narrowing down to PA metro and county data
```{r}
#obtain county data for PA
library(dplyr)
pa_data_county <- redfin_data %>% 
  filter(grepl(", PA$", region_name))
```

```{r}
head(pa_data_county)
```

```{r}
#obtain metro data for PA
pa_data_metro = redfin_data %>%
  filter(grepl(", PA metro area$", region_name))
```


```{r}
head(pa_data_metro)
```

```{r}
# Filter the data to get the PA data
pa_data <- redfin_data %>%
  filter(grepl("PA", region_name)) # Filters rows where region_name contains "PA"
```

Now we will break the data set into the different duration of observations of weekly, monthly, and quarterly
```{r}
# Subset for 1-week duration
pa_1_week_county <- pa_data_county %>%
  filter(duration == "1 weeks")

# Subset for 4-week duration
pa_1_week_county <- pa_data_county %>%
  filter(duration == "4 weeks")

# Subset for 12-week duration
pa_12_weeks_county <- pa_data_county %>%
  filter(duration == "12 weeks")
```


```{r}
# Summarize median sale price trends for each subset
summary_1_week <- pa_1_week_county %>%
  group_by(period_begin) %>%
  summarize(median_sale_price = mean(median_sale_price, na.rm = TRUE))

summary_4_weeks <- pa_1_week_county %>%
  group_by(period_begin) %>%
  summarize(median_sale_price = mean(median_sale_price, na.rm = TRUE))

summary_12_weeks <- pa_12_weeks_county %>%
  group_by(period_begin) %>%
  summarize(median_sale_price = mean(median_sale_price, na.rm = TRUE))
```

```{r}
# Load ggplot2 for visualization
library(ggplot2)

# Plot for 1-week data
ggplot(summary_1_week, aes(x = period_begin, y = median_sale_price)) +
  geom_line(color = "blue") +
  labs(title = "Median Sale Price (1 Week)", x = "Period Begin", y = "Median Sale Price") +
  theme_minimal()

# Plot for 4-week data
ggplot(summary_4_weeks, aes(x = period_begin, y = median_sale_price)) +
  geom_line(color = "green") +
  labs(title = "Median Sale Price (4 Weeks)", x = "Period Begin", y = "Median Sale Price") +
  theme_minimal()

# Plot for 12-week data
ggplot(summary_12_weeks, aes(x = period_begin, y = median_sale_price)) +
  geom_line(color = "red") +
  labs(title = "Median Sale Price (12 Weeks)", x = "Period Begin", y = "Median Sale Price") +
  theme_minimal()
```




## Buyer or Seller markets in each PA County

Now we will dive into the analysis of which counties are buyer or seller markets: 
```{r}
# Classify counties as buyer or seller markets
market_conditions <- pa_12_weeks_county %>%
  group_by(region_name) %>%
  summarize(
    median_months_of_supply = median(months_of_supply, na.rm = TRUE)
  ) %>%
  mutate(market_type = case_when(
    median_months_of_supply < 5 ~ "Seller's Market",
    median_months_of_supply > 7 ~ "Buyer's Market",
    TRUE ~ "Balanced Market"
  ))

# View results
print(market_conditions)
```

Visualization of the results
```{r}
# Count the number of counties in each market type
market_summary <- market_conditions %>%
  group_by(market_type) %>%
  summarize(count = n())

# Create a bar plot of market types
ggplot(market_summary, aes(x = market_type, y = count, fill = market_type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Market Types Across PA Counties",
    x = "Market Type",
    y = "Number of Counties"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Seller's Market" = "red", "Balanced Market" = "gray", "Buyer's Market" = "blue"))
```

## Growth of each county and metro ranked 

```{r}
# Calculate medians for all numeric features grouped by region_name
region_medians <- pa_12_weeks_county %>%
  group_by(region_name) %>%
  summarize(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))

# Add in all of the non-numeric features that are applicable
region_medians <- pa_12_weeks_county %>%
  group_by(region_name) %>%
  summarize(
    region_type = first(region_type), # Take the first value of region_type
    duration = unique(duration), # Take unique values of duration
    across(where(is.numeric), ~ median(.x, na.rm = TRUE))
  )

# View the resulting dataset
print(region_medians)
```

```{r}
# Create a histogram of median_sale_price_yoy
ggplot(region_medians, aes(x = median_sale_price_yoy)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Median Sale Price Year over Year Growth Across Pa Counties",
    x = "Median Sale Price YoY Growth (%)",
    y = "Count"
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent) 
```


# Treating this point as the start of the data set rebuild: 
First we will need to gather the housing data. This is the county data from redfin that may be more accurate: 

```{r}
# Read in the county data
redfin_county_data <-  read_tsv("county_market_tracker.tsv000")
```
Narrow it down to just PA counties
```{r}
# Filter the dataset to include only Pennsylvania counties
pa_counties <- redfin_county_data %>%
  filter(state_code == "PA", property_type == "All Residential")
```

Next we need to gather the US Census data from 2012 - 2023 (2024+ data has not been posted)
```{r}
# This is the individual key to pull data from the US  Census API
library(tidycensus)

#census_api_key("f4d11303611baacc118cb4efc72dcc127a506176", install = TRUE)
```
These are all of the variables that we can search through from the census data
```{r}
# Load all variables for ACS 2020 5-year estimates
acs_vars <- load_variables(2020, "acs5", cache = TRUE)

# View the first few rows of the variable list
head(acs_vars)
```

Pull in all of the data we need from the census for the years 2012 to 2023 and transform it into the correct form
```{r}
# This is for the 5 year averages

# Define the years 
years <- seq(2012, 2023, by = 1) # 2024 is not posted yet nor is 2025 as it is currently 2025

# Initialize an empty list to store data for each year
all_data <- list()

# Loop through each year and pull data
for (year in years) {
  cat("Pulling data for year:", year, "\n")
  
  # Pull ACS 5-Year Estimates for Pennsylvania counties
  pa_acs <- get_acs(
    geography = "county",
    variables = c(
      total_population = "B01003_001", # Total population
      median_household_income = "B19013_001", # Median household income
      poverty_rate_total = "C17002_001", # Poverty rate
      pop_poverty_under_0.5 = "C17002_002", # Extremely poor
      pop_poverty_0.5_to_0.99 = "C17002_003", # Below the poverty line
      median_home_value = "B25077_001", # Median home value
      homeownership_rate_owner = "B25003_002", # Owner-occupied housing
      homeownership_rate_renter = "B25003_003", # Renter-occupied housing
      total_pop_over_25 = "B15003_001",       # The amount of the population over 25 which is what the education counts count for 
      total_accounted_for = "B15003_001",     # This is the total amount of educational categories accounted for
      high_school_graduates = "B15003_017", # High school graduates
      associates_degree = "B15003_021", # Associates degree
      bachelor_degree = "B15003_022", # Bachelor’s degree
      masters_degree = "B15003_023", # Masters degree
      professional_school_degree = "B15003_024", # Professional school degree
      doctorate_degree = "B15003_025", #Doctorate degree
      unemployment = "B23025_005", # Unemployed
      labor_force = "B23025_003", # Labor force
      insured_pop = "B27001_001", # Population that is insured
      total_households = "B11001_001",  # Total amount of households
      household_children = "B11001_002",  # Households that have children
      household_married = "B11005_004", # Married couple in the house
      single_male_parent_households = "B11005_006", # Single male parent households
      single_female_parent_households = "B11005_007"  # Single female parent households
      
    ),
    state = "PA",
    year = year,
    survey = "acs5", 
    geometry = FALSE
  )
  
  # Reshape the data from long to wide format
  pa_acs_wide <- pa_acs %>%
    select(-moe) %>% # Remove margin of error column if not needed
    pivot_wider(
      names_from = variable, # Use variable names as column headers
      values_from = estimate # Use estimate values as cell values
    ) %>%
    mutate(year = year) %>% # Add a year column
    select(year, NAME, everything()) # Reorder columns to put year first
  
  # Store the data in the list
  all_data[[as.character(year)]] <- pa_acs_wide
}

# Combine all yearly data into a single dataframe
combined_data_5year <- bind_rows(all_data)

# View the combined data
print(combined_data_5year)
```

Generating the percentages for all of the features in the data and new percentages that may be important
```{r}
combined_data_5year <- combined_data_5year %>%
  mutate(
    households_with_children_percent = (household_children / total_households) * 100,
    households_married_percent = (household_married / total_households) * 100,
    single_parent_household_percent = ((single_male_parent_households + single_female_parent_households) / total_households) * 100,
    single_male_parent_household_percent = (single_male_parent_households / total_households) * 100,
    single_female_parent_household_percent = (single_female_parent_households / total_households) * 100,
    high_school_graduates_percent = (high_school_graduates / total_pop_over_25) * 100,
    higher_education_percent = ((associates_degree+bachelor_degree+masters_degree+professional_school_degree
      + doctorate_degree) / total_pop_over_25) * 100,
    advanced_degree_percent = ((masters_degree + professional_school_degree + doctorate_degree) / total_pop_over_25) * 100,
    advanced_vs_bachelor_percent = ((masters_degree + professional_school_degree + doctorate_degree) / bachelor_degree) * 100,
    associates_degree_percent = (associates_degree / total_pop_over_25) * 100,
    bachelor_degree_percent = (bachelor_degree / total_pop_over_25) * 100,
    professional_school_degree_percent = (professional_school_degree / total_pop_over_25) * 100,
    doctorate_degree_percent = (doctorate_degree / total_pop_over_25) * 100,
    labor_force_percent = (labor_force / total_population) * 100,
    unemployment_percent = (unemployment / total_population) * 100,
    owner_household_percent = (homeownership_rate_owner / total_households) * 100,
    renter_household_percent = (homeownership_rate_renter / total_households) * 100,
    owner_renter_ratio_percent = (homeownership_rate_owner / (homeownership_rate_owner + homeownership_rate_renter)) * 100,
    insured_percent = (insured_pop / total_population) * 100,
    uninsured_percent = 100 - insured_percent,
    poverty_percent = ((pop_poverty_under_0.5+pop_poverty_0.5_to_0.99) / poverty_rate_total) * 100,
    severe_poverty_percent = (pop_poverty_under_0.5 / poverty_rate_total) * 100,
    pop_over_25_percent = (total_pop_over_25 / total_population) * 100,
    pop_under_25_percent = ((total_population - total_pop_over_25) / total_population) * 100,
    unemployment_rate = (unemployment / labor_force) * 100,
    not_in_labor_force_percent = ((total_population - labor_force) / total_population) * 100,
    non_high_school_grad_percent = ((total_pop_over_25 - high_school_graduates) / total_pop_over_25) * 100
    )
```

Break out the data into months in a yearly step style
```{r}
# Expand yearly data to monthly
monthly_data_step <- combined_data_5year %>%
  mutate(month = list(seq(1, 12))) %>%  
  unnest(month) %>%                    
  arrange(year, month)  
```

Now we need to make the data into monthly form and to do this we will assume a linear change between each yearly data point and use that to generate the monthly data. This is a mildly strong assumption however since the data is already a 5 year aggrigate and the data is collected all year round to make the final number we are going to move forward with our idea to convert it to linear monthly data. There is no other way to do this unless we stick to having the yearly values be static and jump up each new year. 
```{r}
# Gather the numeric columns that will be transitioned to monthly
num_cols <- names(combined_data_5year)[sapply(combined_data_5year, is.numeric)]
num_cols <- setdiff(num_cols, "year")

# For each county make new columns for the next year value
combined_data_5year <- combined_data_5year %>%
  group_by(NAME) %>%
  arrange(year) %>%
  # Create a new column for each numeric column that holds the next year value
  mutate(across(all_of(num_cols), ~ lead(.x), .names = "next_{.col}")) %>%
  ungroup()

# Remove the final year as it would be flat and not adjusted
data_for_interp <- combined_data_5year %>%
  filter(!is.na(next_total_population))

# Expand the data so the yearly values have 12 new rows
monthly_data <- data_for_interp %>%
  uncount(12, .id = "month") %>%  # make the month column
  mutate(year_month = year + (month - 1) / 12)  # make the year month decimal value

# Calculate each month value
#   Formula: monthly_value = current_year_value + (month/12) * (next_year_value - current_year_value)
for (col in num_cols) {
  # Get the names of the current and next values
  cur_col  <- col
  next_col <- paste0("next_", col)
  
  monthly_data[[col]] <- monthly_data[[cur_col]] +
    (monthly_data$month / 12) * (monthly_data[[next_col]] - monthly_data[[cur_col]])
}

# keep the desired coulums
monthly_data <- monthly_data %>%
  select(year_month, NAME, GEOID, month, all_of(num_cols))

# Round all of the relevant features up so we do not have half of a person for example in our population count
cols_to_round <- num_cols[!grepl("percent", num_cols, ignore.case = TRUE)]
monthly_data <- monthly_data %>%
  mutate(across(all_of(cols_to_round), ceiling))
```
