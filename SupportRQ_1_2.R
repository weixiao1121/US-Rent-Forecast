rm(list = ls())

#Load relevant libraries
library(readxl)
library(dplyr)

#set working directory
setwd("/Users/mengxiyang/Documents/GaTech/MGT 6203/Group Project/")

#Load cleaned data with 'state' column 
housing_data <- read_excel("housing_cleaned.xlsx")

#Verify number of unique states in the dataset
#state_counts <- unique(housing_data$state)
#length(state_counts)

#Rename 'region' column to 'city' to avoid confusion
housing_data <- housing_data %>%
  rename(city = region)

#Create new 'US_region' column, using region maps from: https://www.cdc.gov/nchs/hus/sources-definitions/geographic-region.htm
housing_data <- housing_data %>%
  mutate(
    US_region = case_when(
      state %in% c('ct', 'me', 'ma', 'nh', 'nj', 'ny', 'pa', 'ri', 'vt') ~ 'Northeast',
      state %in% c('il', 'in', 'ia', 'ks', 'mi', 'mn', 'mo', 'ne', 'nd', 'oh', 'sd', 'wi') ~ 'Midwest',
      state %in% c('al', 'ar', 'de', 'fl', 'ga', 'ky', 'la', 'md', 'ms', 'nc', 'ok', 'sc', 'tn', 'tx', 'va', 'wv', 'dc') ~ 'South',
      state %in% c('ak', 'az', 'ca', 'co', 'hi', 'id', 'mt', 'nv', 'nm', 'or', 'ut', 'wa', 'wy') ~ 'West',
      TRUE ~ NA_character_
    )
  )

#Export data to CSV
#write.csv(housing_data, "housing_cleaned_US_region.csv", row.names = FALSE)

#Supporting RQ #1: Which neighborhoods have the highest variance in rental price for similar units? 

#Create subset of data for 1-bed apt only 
oneB_apt <- housing_data %>%
  filter(type == 'apartment' & beds == 1)

#Group by city and calculate variance, standard dev of rental prices 
oneB_apt_var <- oneB_apt %>%
  group_by(city) %>%
  summarize(
    num_count = n(), #number of observations
    variance_rental_price = var(price, na.rm = TRUE),
    sd_rental_price = sd(price, na.rm = TRUE),
    #cv_rental_price = sd_rental_price/ mean(price, na.rm = TRUE) *100
  ) %>%
  arrange(desc(sd_rental_price))

print(oneB_apt_var)

#Create subset for beginner homes, type = house/condo/townhouse, <3 beds and no more than 1500 sqft
beginner_home <- housing_data %>%
  filter(type %in% c("house", "condo", "townhouse") & beds <3 & sqfeet <1500)

#Group by city and calculate variance, standard dev of rental prices 
beginner_home_var <- beginner_home %>%
  group_by(city, type) %>%
  summarize(
    variance_rental_price = var(price, na.rm = TRUE),
    sd_rental_price = sd(price, na.rm = TRUE),
    #cv_rental_price = sd_rental_price/ mean(price, na.rm = TRUE) *100
  ) 

top_3_type <- beginner_home_var %>%
  group_by(type) %>%
  top_n(3, wt = sd_rental_price) %>%
  arrange(desc(type), desc(sd_rental_price))

top_3_type

#Supporting RQ #2: Which state has the highest and lowest median rental price?
state_median_rental <- housing_data %>% 
  group_by(state) %>%
  summarize(
    count = n(),
    median_rental_price = median(price, na.rm = TRUE))

sorted_median <- state_median_rental %>%
  arrange(desc(median_rental_price))

top_3_states <- head(sorted_median, 3)

bottom_3_states <- tail(sorted_median, 3)

print(top_3_states)
print(bottom_3_states)