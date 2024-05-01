library(glmnet)
library(dummies)
library(car)
train_data<- read.csv("/Users/xiaowei/Desktop/MGT6203/training_data_v2.csv")
test_data<- read.csv("/Users/xiaowei/Desktop/MGT6203/testing_data_v2.csv")
summary(train_data)

train_data <- subset(train_data, price > 100 & price < 10000 &
               !(type %in% c('assisted living', 'land')) &
               sqfeet > 200 & sqfeet < 4000 &
               beds <= 10 & baths != 0)

test_data <- subset(test_data, price > 100 & price < 10000 &
                       !(type %in% c('assisted living', 'land')) &
                       sqfeet > 200 & sqfeet < 4000 &
                       beds <= 10 & baths != 0)

train_data$laundry_options[train_data$laundry_options == "" | is.na(train_data$laundry_options)] <- 'no laundry on site'
train_data$parking_options[train_data$parking_options == "" |is.na(train_data$parking_options)] <- 'no parking'

test_data$laundry_options[test_data$laundry_options == "" | is.na(test_data$laundry_options)] <- 'no laundry on site'
test_data$parking_options[test_data$parking_options == "" |is.na(test_data$parking_options)] <- 'no parking'

drop_cols <- c('id', 'url', 'region', 'region_url', 'image_url', 'description', 'state')
train_data <- train_data[, !(names(train_data) %in% drop_cols)]
test_data <- test_data[, !(names(test_data) %in% drop_cols)]

train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

#####

#create dummy variables for categorical variables in train_data
train_data$type <- factor(train_data$type, levels = c("apartment", "house", "townhouse", "condo", "duplex", "manufactured", "cottage/cabin", "loft", "flat", "in-law"))
type_dummy <- model.matrix(~ type - 1, data = train_data)
train_data$type <-type_dummy

train_data$laundry_options <- factor(train_data$laundry_options, levels = c("w/d in unit", "laundry on site", "w/d hookups", "no laundry on site", "laundry in bldg"))
laundry_dummy <- model.matrix(~ laundry_options - 1, data = train_data)
train_data$laundry_options <-laundry_dummy

train_data$parking_options <- factor(train_data$parking_options, levels = c("no parking" , "street parking", "off-street parking", "attached garage", "detached garage", "valet parking"))
parking_dummy <- model.matrix(~ parking_options - 1, data = train_data)
train_data$parking_options <- parking_dummy


#create dummy variables for categorical variables in test_data

test_data$type <- factor(test_data$type, levels = c("apartment", "house", "townhouse", "condo", "duplex", "manufactured", "cottage/cabin", "loft", "flat", "in-law"))
type_dummy <- model.matrix(~ type - 1, data = test_data)
test_data$type <-type_dummy

test_data$laundry_options <- factor(test_data$laundry_options, levels = c("w/d in unit", "laundry on site", "w/d hookups", "no laundry on site", "laundry in bldg"))
laundry_dummy <- model.matrix(~ laundry_options - 1, data = test_data)
test_data$laundry_options <-laundry_dummy

test_data$parking_options <- factor(test_data$parking_options, levels = c("no parking" , "street parking", "off-street parking", "attached garage", "detached garage", "valet parking"))
parking_dummy <- model.matrix(~ parking_options - 1, data = test_data)
test_data$parking_options <- parking_dummy



set.seed(96)

#initial model and results
model <- lm(price ~., data = train_data)

#Split data into train and test
y_train <- train_data[,1]
X_train <- train_data[,-1] 

y_test <- test_data[,1]
X_test <- test_data[,-1]

#Scale the data before performing elastic net regression
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)
# alpha = 0.5 for Elastic Net; using 10-fold for cross validation
elastic_net_model <- cv.glmnet(X_train_scaled, y_train, alpha = 0.5, nfolds=10)  
elastic_net_model
best_lambda <- elastic_net_model$lambda.min
best_lambda


best_model <- glmnet(X_train_scaled, y_train, family='gaussian', alpha = 0.5, lambda = best_lambda)
elastic_net_coefs <- coef(elastic_net_model, s = best_lambda)
elastic_net_coefs

# Evaluate performance of original linear regression model

original_predictions <- predict(model, newdata = X_test)
original_mse <- mean((original_predictions - y_test)^2)
original_rmse <- sqrt(original_mse)
original_r_squared <- cor(original_predictions, y_test)^2
original_mae <- mean(abs(original_predictions - y_test))

# Evaluate performance of Elastic Net regression model
X_test_scaled_matrix <- as.matrix(X_test_scaled)

elastic_net_predictions <- predict(elastic_net_model, s = best_lambda, newx = X_test_scaled_matrix)
elastic_net_mse <- mean((elastic_net_predictions - y_test)^2)
elastic_net_rmse <- sqrt(elastic_net_mse)
elastic_net_r_squared <- cor(elastic_net_predictions, y_test)^2
elastic_net_mae <- mean(abs(elastic_net_predictions - y_test))

# Compare performance metrics
performance_comparison <- data.frame(
  Model = c("Original Model", "Elastic Net Model"),
  MSE = c(original_mse, elastic_net_mse),
  RMSE = c(original_rmse, elastic_net_rmse),
  R_squared = c(original_r_squared, elastic_net_r_squared),
  MAE = c(original_mae, elastic_net_mae)
)

performance_comparison
