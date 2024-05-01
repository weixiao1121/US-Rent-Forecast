rm(list = ls())

#Load relevant libraries
library(readxl)
library(grid)
library(ggplot2) #for plotting
library(gridExtra) #for arranging plots 
library(corrplot)
library(dplyr)
library(psych)
library(GGally)
library(car)

#set working directory
setwd("/Users/mengxiyang/Documents/GaTech/MGT 6203/Group Project/")

#Load training and testing data (70%/30%)
train_data <- read.csv("training_data_v2.csv")
test_data <- read.csv("testing_data_v2.csv")

#create dummy variables for categorical variables - training data
train_data$type <- factor(train_data$type, levels = c("apartment", "house", "townhouse", "condo", "duplex", "manufactured", "cottage/cabin", "loft", "flat", "in-law"))
type_dummy <- model.matrix(~ type - 1, data = train_data)

train_data$laundry_options <- factor(train_data$laundry_options, levels = c("w/d in unit", "laundry on site", "w/d hookups", "no laundry on site", "laundry in bldg"))
laundry_dummy <- model.matrix(~ laundry_options - 1, data = train_data)

train_data$parking_options <- factor(train_data$parking_options, levels = c("no parking" , "street parking", "off-street parking", "attached garage", "detached garage", "valet parking"))
parking_dummy <- model.matrix(~ parking_options - 1, data = train_data)

#create dummy variables for categorical variables - testing data
test_data$type <- factor(test_data$type, levels = c("apartment", "house", "townhouse", "condo", "duplex", "manufactured", "cottage/cabin", "loft", "flat", "in-law"))
type_dummy <- model.matrix(~ type - 1, data = test_data)

test_data$laundry_options <- factor(test_data$laundry_options, levels = c("w/d in unit", "laundry on site", "w/d hookups", "no laundry on site", "laundry in bldg"))
laundry_dummy <- model.matrix(~ laundry_options - 1, data = test_data)

test_data$parking_options <- factor(test_data$parking_options, levels = c("no parking" , "street parking", "off-street parking", "attached garage", "detached garage", "valet parking"))
parking_dummy <- model.matrix(~ parking_options - 1, data = test_data)

set.seed(123)

###Evaluate different log transformation models using training dataset 

#Linear-Linear model
LL_model_train <- lm(price ~., data = train_data)
summary(LL_model_train)

#Log-Linear model
LoL_model_train <- lm(log(price) ~., data = train_data)
summary(LoL_model_train)

#Linear-Log model, using log(x+1) for number of bedrooms due to presence of 0's
LLo_model_train <- lm(price ~. + log(sqfeet) + log(beds + 1) + log(baths), data = train_data)
summary(LLo_model_train)

#Log-Log model, using log(x+1) for number of bedrooms due to presence of 0's
LoLo_model_train <- lm(log(price) ~. + log(sqfeet) + log(beds + 1) + log(baths), data = train_data)
summary(LoLo_model_train)


###Evaluate different log transformation models using testing dataset 

#Linear-Linear model
LL_model_test <- lm(price ~., data = test_data)
summary(LL_model_test)

#Log-Linear model
LoL_model_test <- lm(log(price) ~., data = test_data)
summary(LoL_model_test)

#Linear-Log model, using log(x+1) for number of bedrooms due to presence of 0's
LLo_model_test <- lm(price ~. + log(sqfeet) + log(beds + 1) + log(baths), data = test_data)
summary(LLo_model_test)

#Log-Log model, using log(x+1) for number of bedrooms due to presence of 0's
LoLo_model_test <- lm(log(price) ~. + log(sqfeet) + log(beds + 1) + log(baths), data = test_data)
summary(LoLo_model_test)

#plot best
par(mfrow = c(2, 2))
plot(LL_model_train)

