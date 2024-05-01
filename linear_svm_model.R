library(e1071)
library(caret)
library(car)

setwd("C:/Users/Patrick/Documents/OMSA/Spring 2024/MGT 6203 - Data Analytics in Business/Project")

# Import trainging and testing datasets created for the initial linear regression model
train_data <- read.csv("training_data.csv")
test_data <- read.csv("testing_data.csv")

train_data[, c("sqfeet", "lat", "long")] <- scale(train_data[, c("sqfeet", "lat", "long")])
test_data[, c("sqfeet", "lat", "long")] <- scale(test_data[, c("sqfeet", "lat", "long")])


### sampled dataset using 10% of both datasets

train_size <- 0.1 * nrow(train_data)
test_size <- 0.1 * nrow(test_data)

# Sample the training data
train_sampled <- train_data[sample(nrow(train_data), train_size), ]

# Sample the test data
test_sampled <- test_data[sample(nrow(test_data), test_size), ]

X_train_sampled <- train_sampled[, !(names(train_sampled) %in% c('price'))]
y_train_sampled <- as.data.frame(train_sampled$price)
names(y_train_sampled) <- 'price'


X_test_sampled <- test_sampled[, !(names(test_sampled) %in% c('price'))]
y_test_sampled <- as.data.frame(test_sampled$price)
names(y_test_sampled) <- 'price'

X_train_sampled <- scale(X_train_sampled)
X_test_sampled <- scale(X_test_sampled)


# Create a linear SVM model

hyperparameters <- expand.grid(
  C = c(0.1, 1, 10)
)
# Define the training control
train_control <- trainControl(method = "cv"
                              , number = 3)

# Perform grid search
svm_model_cv <- train(price ~ .
                      , data = train_sampled
                      , method = "svmLinear"
                      , trControl = train_control
                      , tuneGrid = hyperparameters)


best_hyperparameters <- svm_model_cv$bestTune
best_svm_model <- svm(price ~ .
                      , data = train_sampled
                      , kernel = "linear"
                      , cost = best_hyperparameters$C
)
summary(best_svm_model)
coef(best_svm_model)


# Make predictions on the training data
predicted_prices_train <- predict(best_svm_model, X_train_sampled)

# Calculate R-squared for training data
actual_values_train <- y_train_sampled$price
predicted_values_train <- predicted_prices_train
mean_actual_value_train <- mean(actual_values_train)
sst_train <- sum((actual_values_train - mean_actual_value_train)^2)
sse_train <- sum((actual_values_train - predicted_values_train)^2)
r_squared_train <- 1 - (sse_train / sst_train)

# Calculate adjusted R-squared for training data
n_train <- length(actual_values_train)
p_train <- ncol(X_train_sampled)
adjusted_r_squared_train <- 1 - (1 - r_squared_train) * ((n_train - 1) / (n_train - p_train - 1))

# Print the results
print(paste("R-squared (Training Data):", r_squared_train))
print(paste("Adjusted R-squared (Training Data):", adjusted_r_squared_train))


# Make predictions on the testing data
predicted_prices <- predict(best_svm_model, X_test_sampled)

# Calculate R-squared
actual_values <- y_test_sampled$price
predicted_values <- predicted_prices
mean_actual_value <- mean(actual_values)
sst <- sum((actual_values - mean_actual_value)^2)
sse <- sum((actual_values - predicted_values)^2)
r_squared <- 1 - (sse / sst)

# Calculate adjusted R-squared
n <- length(actual_values)
p <- ncol(X_test_sampled)
adjusted_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))

# Print the results
print(paste("R-squared:", r_squared))
print(paste("Adjusted R-squared:", adjusted_r_squared))

