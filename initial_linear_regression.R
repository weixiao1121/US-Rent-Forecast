rm(list = ls())

setwd("C:/Users/Patrick/Documents/OMSA/Spring 2024/MGT 6203 - Data Analytics in Business/Project")

data <- read.csv("housing_train.csv")

library(car)

#####

# Converted Preeti's data cleaning steps from python to R
df <- subset(data, price > 100 & price < 10000 &
               !(type %in% c('assisted living', 'land')) &
               sqfeet > 200 & sqfeet < 4000 &
               beds <= 10 & baths != 0)

df$laundry_options[df$laundry_options == "" | is.na(df$laundry_options)] <- 'no laundry on site'
df$parking_options[df$parking_options == "" |is.na(df$parking_options)] <- 'no parking'

drop_cols <- c('id', 'url', 'region', 'region_url', 'image_url', 'description', 'state')
df <- df[, !(names(df) %in% drop_cols)]

df <- na.omit(df)

#####

#create dummy variables for categorical variables
df$type <- factor(df$type, levels = c("apartment", "house", "townhouse", "condo", "duplex", "manufactured", "cottage/cabin", "loft", "flat", "in-law"))
type_dummy <- model.matrix(~ type - 1, data = df)

df$laundry_options <- factor(df$laundry_options, levels = c("w/d in unit", "laundry on site", "w/d hookups", "no laundry on site", "laundry in bldg"))
laundry_dummy <- model.matrix(~ laundry_options - 1, data = df)

df$parking_options <- factor(df$parking_options, levels = c("no parking" , "street parking", "off-street parking", "attached garage", "detached garage", "valet parking"))
parking_dummy <- model.matrix(~ parking_options - 1, data = df)


set.seed(96)

# Create a vector of indices for splitting
split_index <- sample(1:nrow(df), 0.7 * nrow(df))

# Split the data
train_data <- df[split_index, ]
test_data <- df[-split_index, ]

#initial model and results
model <- lm(price ~., data = train_data)
summary(model)

plot(model)

