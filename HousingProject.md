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











