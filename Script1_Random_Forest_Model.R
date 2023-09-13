# Load the randomForest package
library(randomForest)

# Load your dataset
data <- read.csv("olink_dataset.csv")

# Build the random forest model
rf_model <- randomForest(y ~ ., data = train_data, ntree = 100, mtry = 7, replace = TRUE)

# Calculate the mean decrease accuracy
mean_decrease_acc <- importance(rf_model, type = 1)

# Print the mean decrease accuracy
print(mean_decrease_acc)
