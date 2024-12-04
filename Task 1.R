library(datasets)       
library(caret) 
install.packages("randomForest")
library(randomForest)   
library(ggplot2) 

# Loading and preview of dataset
data(iris)
head(iris)
str(iris)

# Visualize the data: Pair Plot

pairs(iris[, 1:4], col = iris$Species)

# Split the data into training and testing sets

set.seed(42)  
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Train a Random Forest model

set.seed(42)
rf_model <- randomForest(Species ~ ., data = trainData, importance = TRUE)

# Print model summary and plotting variable importance

print(rf_model)
varImpPlot(rf_model)

# Make predictions on the test data

predictions <- predict(rf_model, testData)

# Evaluate the model

confusionMatrix(predictions, testData$Species)

# Visualize the predictions using a scatter plot

ggplot(testData, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  geom_point(aes(shape = predictions), size = 3, alpha = 0.5) +
  labs(title = "True vs Predicted Species",
       x = "Petal Length",
       y = "Petal Width") +
  theme_minimal()