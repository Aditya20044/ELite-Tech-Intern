# Install required packages (if not already installed)
install.packages(c("tidyverse", "ggplot2", "caret", "randomForest", "corrplot"))

# Load libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(corrplot)

# Load the dataset
car_data <- read.csv("/Users/adityakumar/Downloads/car_price_prediction_.csv")

# View the structure and summary
str(car_data)
summary(car_data)

ggplot(car_data, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Frequency")

# Select numeric columns for correlation analysis
numeric_features <- car_data %>% select(Engine.Size, Mileage, Year, Price)

# Correlation matrix
cor_matrix <- cor(numeric_features, use = "complete.obs")

# Plot correlation matrix
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Horsepower vs Price
ggplot(car_data, aes(x = Engine.Size, y = Price)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Engine Size vs Price", x = "Engine Size", y = "Price")

# data preprocessing
car_data <- car_data %>% drop_na()

# One-hot encoding for categorical variables
dummies <- dummyVars(~ ., data = car_data)
car_data_encoded <- data.frame(predict(dummies, newdata = car_data))


# Standardize numerical columns
car_data_scaled <- car_data_encoded %>%
  mutate(across(where(is.numeric), scale))

#split dataset
set.seed(123)
trainIndex <- createDataPartition(car_data_scaled$Price, p = 0.8, list = FALSE)
train_data <- car_data_scaled[trainIndex, ]
test_data <- car_data_scaled[-trainIndex, ]

#train datset
model_lm <- lm(Price ~ ., data = train_data)
summary(model_lm)

#randomforest
model_rf <- randomForest(Price ~ ., data = train_data, ntree = 100)
print(model_rf)

# Linear Regression Predictions
predictions_lm <- predict(model_lm, newdata = test_data)

# Random Forest Predictions
predictions_rf <- predict(model_rf, newdata = test_data)

# RMSE for Linear Regression
rmse_lm <- sqrt(mean((test_data$Price - predictions_lm)^2))
cat("Linear Regression RMSE:", rmse_lm, "\n")

# RMSE for Random Forest
rmse_rf <- sqrt(mean((test_data$Price - predictions_rf)^2))
cat("Random Forest RMSE:", rmse_rf, "\n")

#data visualization
ggplot(data.frame(Actual = test_data$Price, Predicted = predictions_rf),
       aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Prices", x = "Actual Prices", y = "Predicted Prices")





