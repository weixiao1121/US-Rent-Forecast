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

rm(list = ls())

#Load cleaned data
housing_data <- read_excel("housing_cleaned.xlsx")
#View(housing_data)

housing_statistics <- summary(housing_data)
housing_statistics

#Outlier analysis for continuous numerical variables, box-and-whisker plots
boxplot_price <- ggplot(housing_data, aes(x = "", y = price)) +
  geom_boxplot() +
  labs(title = "Price")

boxplot_sqfeet <- ggplot(housing_data, aes(x = "", y = sqfeet)) +
  geom_boxplot() +
  labs(title = "Square footage")

boxplot_beds <- ggplot(housing_data, aes(x = "", y = beds)) +
  geom_boxplot() +
  labs(title = "Number of Bedrooms")

boxplot_baths <- ggplot(housing_data, aes(x = "", y = baths)) +
  geom_boxplot() +
  labs(title = "Number of Bathrooms")

# Combine boxplots into a grid
grid.arrange(boxplot_price, boxplot_sqfeet, boxplot_beds, boxplot_baths, 
             nrow = 2, ncol = 2,
             top = textGrob("Box and whisker plots for Continuous Variables",
                            gp = gpar(fontsize = 20, font = 3)))


#Correlation matrix on numerical, continuous columns <filtered data>
num_columns <- housing_data[, c("price", "sqfeet", "beds", "baths")]

lowerFn <- function(data, mapping, method = "lm", ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(color = "blue", size = 1) +
    geom_smooth(method = method, color = "red", size = 1,...)
  p
}

ggpairs(num_columns, 
        lower = list(continuous = wrap(lowerFn, method = "lm")),
        diag = list(continuous = wrap("barDiag", color = "blue")),
        upper = list(continuous = wrap("cor", size = 3))) + 
  labs(title = "Correlation Matrix for Continuous Variables")

##EDA section 
#Histograms of continuous variables
hist_price <- ggplot(data=housing_data, aes(x = price)) + 
  geom_histogram(fill="skyblue", color = "gray", bins = 50) + 
  labs(title="Price distribution") +
  labs(x="Price", y="Count")

hist_sqfeet <- ggplot(data=housing_data, aes(x = sqfeet)) + 
  geom_histogram(fill="skyblue", color = "gray", bins = 50) + 
  labs(title="Square footage distribution") +
  labs(x="Square Footage", y="Count")

hist_beds <- ggplot(data=housing_data, aes(x = beds)) + 
  geom_histogram(fill="skyblue", color = "gray", bins = 10) + 
  labs(title="Number of Bedroom distribution") +
  labs(x="Number of Bedrooms", y="Count")

hist_baths <- ggplot(data=housing_data, aes(x = baths)) + 
  geom_histogram(fill="skyblue", color = "gray", bins = 10) + 
  labs(title="Number of Bathroom distribution") +
  labs(x="Number of Bathroom", y="Count")

# Combine histograms into a grid
grid.arrange(hist_price, hist_sqfeet, hist_beds, hist_baths, 
             nrow = 2, ncol = 2,
             top = textGrob("Histograms of Continuous Variables",
                            gp = gpar(fontsize = 20, font = 3)))

#Visualization of Binary variables
#Convert binary variable columns from numeric to binary columns 
housing_data$cats_allowed <- ifelse(housing_data$cats_allowed == 1, TRUE, FALSE)
housing_data$dogs_allowed <- ifelse(housing_data$dogs_allowed == 1, TRUE, FALSE)
housing_data$smoking_allowed <- ifelse(housing_data$smoking_allowed == 1, TRUE, FALSE)
housing_data$wheelchair_access <- ifelse(housing_data$wheelchair_access == 1, TRUE, FALSE)
housing_data$electric_vehicle_charge <- ifelse(housing_data$electric_vehicle_charge == 1, TRUE, FALSE)
housing_data$comes_furnished <- ifelse(housing_data$comes_furnished == 1, TRUE, FALSE)

#Box plots of binary variables
box_cats <- ggplot(housing_data, aes(x = cats_allowed, y = price, fill=cats_allowed)) +
  geom_boxplot() +
  labs(title = "Price by Cats_allowed", size = 1) +
  theme(legend.position = "none")

box_dogs <- ggplot(housing_data, aes(x = dogs_allowed, y = price, fill = dogs_allowed)) +
  geom_boxplot() +
  labs(title = "Price by Dogs_allowed", size = 1) +
  theme(legend.position = "none")

box_smoke <- ggplot(housing_data, aes(x = smoking_allowed, y = price, fill = smoking_allowed)) +
  geom_boxplot() +
  labs(title = "Price by Smoking_allowed", size = 1) +
  theme(legend.position = "none")

box_wheel <- ggplot(housing_data, aes(x = wheelchair_access, y = price, fill = wheelchair_access)) +
  geom_boxplot() +
  labs(title = "Price by Wheelchair_access", size = 1) +
  theme(legend.position = "none")

box_elect <- ggplot(housing_data, aes(x = electric_vehicle_charge, y = price, fill = electric_vehicle_charge)) +
  geom_boxplot() +
  labs(title = "Price by Electric_vehicle_charge", size = 1) +
  theme(legend.position = "none")

box_furn <- ggplot(housing_data, aes(x = comes_furnished, y = price, fill = comes_furnished)) +
  geom_boxplot() +
  labs(title = "Price by Comes_furnished", size = 1) +
  theme(legend.position = "none")

# Combine Box plots into a grid
grid.arrange(box_cats, box_dogs, box_smoke, box_wheel, box_elect, box_furn,
             nrow = 2, ncol = 3,
             top = textGrob("Box plots of Binary Variables",
                            gp = gpar(fontsize = 15, font = 3)))

#Visualization of Categorical variables
box_type <- ggplot(housing_data, aes(x = type, y = price, fill = type)) +
  geom_boxplot() +
  labs(title = "Price by Housing Types", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

box_laund <- ggplot(housing_data, aes(x = laundry_options, y = price, fill = laundry_options)) +
  geom_boxplot() +
  labs(title = "Price by Laundry Options", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

box_park <- ggplot(housing_data, aes(x = parking_options, y = price, fill = parking_options)) +
  geom_boxplot() +
  labs(title = "Price by Parking Options", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

# Combine Box plots into a grid
grid.arrange(box_type, box_laund, box_park,
             nrow = 1, ncol = 3,
             top = textGrob("Box plots of Categorical Variables",
                            gp = gpar(fontsize = 15, font = 3)))

#Linear regression model using the 3 continuous variables
num_model <- lm(formula = price ~ sqfeet + beds + baths, data = housing_data)

#split the diagnostic plots into 2x2 grid
par(mfrow = c(2, 2))
plot(num_model)
main_title <- "Diagnostic Plots: Price Regressed on Continuous Variables"
title(main = main_title, outer = TRUE, cex = 1.5, line = -2)

# VIF
vif(num_model)
