# Load necessary libraries
library(caret)
library(C50)
library(plyr)
library(gmodels)
library(rpart)
library(rpart.plot)
library(ISLR)
library(gbm)
library(randomForest)

data <- read.csv("/Users/iamy8000/Desktop/UMD MIM/INST737/milestone 2/StockPrediction_data.csv")

################## Part 1: Split Data ##################
# Remove rows with missing "Open" values for "2021-03-19"
data <- data[!(is.na(data$Open) & data$Date == "2021-03-19"), ]
data <- data[, -which(names(data) == "Date")]

# Randomize data
set.seed(1234)
data <- data[order(runif(nrow(data))), ]

# Determine and create classes
data_range <- range(data$Close)
min_value <- data_range[1]
max_value <- data_range[2]
num_folds <- 5
range_width <- (max_value - min_value) / 270
data$Class <- cut(data$Close, 
                  breaks = seq(min_value, max_value, by = range_width),
                  labels = FALSE)

data <- data[!is.na(data$Class), ]

# Perform K-fold cross-validation
folds <- createFolds(data$Class, k = num_folds, list = TRUE)
train_datasets <- list()
test_datasets <- list()

for (i in 1:num_folds) {
  # Extract the indices for the current fold
  train_indices <- unlist(folds[-i])
  test_indices <- unlist(folds[i])
  
  # Create train and test datasets based on the indices
  train_set <- data[train_indices, ]
  test_set <- data[test_indices, ]
  
  # Store the datasets in the lists
  train_datasets[[i]] <- train_set
  test_datasets[[i]] <- test_set
}

# Show distribution in original data, train_data, and test_data
# Check the distribution of "Federal_Funds_Rate" 
par(mfrow = c(1, 3))
hist(data$Federal_Funds_Rate, main = "Data Distribution", xlab = "Federal Funds Rate", col = "lightblue")
hist(train_set$Federal_Funds_Rate, main = "Training Set Distribution", xlab = "Federal Funds Rate", col = "lightgreen")
hist(test_set$Federal_Funds_Rate, main = "Testing Set Distribution", xlab = "Federal Funds Rate", col = "lightcoral")
par(mfrow = c(1, 1))

# Check the distribution of "Unemployment_Rate"
par(mfrow=c(1,3)) 
hist(data$Unemployment_Rate, main = "Original Distribution", xlab = "Unemployment Rate", col = "lightblue")
hist(train_set$Unemployment_Rate, main = "Training Set Distribution", xlab = "Unemployment Rate", col = "lightgreen")
hist(test_set$Unemployment_Rate, main = "Testing Set Distribution", xlab = "Unemployment Rate", col = "lightcoral")
par(mfrow = c(1, 1))

# Check the distribution of "Monthly_Real_GDP_Index"
par(mfrow = c(1, 3))
hist(data$Monthly_Real_GDP_Index, main = "Data Distribution", xlab = "Monthly Real GDP Index", col = "lightblue")
hist(train_set$Monthly_Real_GDP_Index, main = "Training Set Distribution", xlab = "Monthly Real GDP Index", col = "lightgreen")
hist(test_set$Monthly_Real_GDP_Index, main = "Testing Set Distribution", xlab = "Monthly Real GDP Index", col = "lightcoral")
par(mfrow = c(1, 1))

# Check the distribution of "Monthly_Nominal_GDP_Index"
par(mfrow = c(1, 3))
hist(data$Monthly_Nominal_GDP_Index, main = "Data Distribution", xlab = "Monthly Nominal GDP Index", col = "lightblue")
hist(train_set$Monthly_Nominal_GDP_Index, main = "Training Set Distribution", xlab = "Monthly Nominal GDP Index", col = "lightgreen")
hist(test_set$Monthly_Nominal_GDP_Index, main = "Testing Set Distribution", xlab = "Monthly Nominal GDP Index", col = "lightcoral")
par(mfrow = c(1, 1))

################## Part 2: Decision Tree, Confusion Matrix ##################
# Define the target variable
train_set$Class <- as.factor(train_set$Class)

# Train a decision tree using C50
tree_model <- C5.0(Class ~ Source + Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate, data = train_set)
# tree_model <- C5.0(Class ~ Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate, data = train_set)

# display simple facts about the tree
tree_model

# display detailed information about the tree 
summary(tree_model)

# Evaluating Model Performance
train_predictions <- predict(tree_model, train_set, type = "class")
test_predictions <- predict(tree_model, test_set, type = "class")

CrossTable(test_set$Class, test_predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Create confusion matrices
train_confusion <- table(train_predictions, train_set$Class)
test_confusion <- table(test_predictions, test_set$Class)

# Compute percentages of correctly and incorrectly classified samples
train_accuracy <- sum(diag(train_confusion)) / sum(train_confusion)
test_accuracy <- sum(diag(test_confusion)) / sum(test_confusion)

cat("Accuracy on Training Data:", train_accuracy, "\n")
cat("Accuracy on Testing Data:", test_accuracy, "\n")

# Calculate the error rate
train_error_rate <- 1 - train_accuracy
test_error_rate <- 1 - test_accuracy

cat("Error Rate on Training Data:", train_error_rate, "\n")
cat("Error Rate on Testing Data:", test_error_rate, "\n")

################## Part 3: Boosting ##################
#Train a boosted decision tree using C50
train_set$Class <- as.factor(train_set$Class)
tree_model_boost <- C5.0(Class ~ Source + Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate,
                         data = train_set,
                         trials = 50,
                         control=C5.0Control(earlyStopping=FALSE))

# display information about the boosted tree
tree_model_boost
summary(tree_model_boost)

# Evaluating Model Performance
train_predictions_boost <- predict(tree_model_boost, train_set, type = "class")
test_predictions_boost <- predict(tree_model_boost, test_set, type = "class")

CrossTable(test_set$Class, test_predictions_boost,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Create confusion matrices
train_confusion_boost <- table(train_predictions_boost, train_set$Class)
test_confusion_boost <- table(test_predictions_boost, test_set$Class)

# Compute percentages of correctly and incorrectly Classified samples
train_accuracy_boost <- sum(diag(train_confusion_boost)) / sum(train_confusion_boost)
test_accuracy_boost <- sum(diag(test_confusion_boost)) / sum(test_confusion_boost)

cat("Accuracy on Training Data:", train_accuracy_boost, "\n")
cat("Accuracy on Testing Data:", test_accuracy_boost, "\n")

################## Part 4: Bagging and random forests ##################
# Define the target variable
train_set$Class <- as.factor(train_set$Class)

# Bagging - Train and Test with Different Numbers of Trees
mtry_values <- c(2, 3, 4, 5)
bagging_results <- list()

for (mtry in mtry_values) {
  # Train
  bagging_model <- randomForest(Class ~ Source + Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate, 
                                data = train_set, 
                                mtry = mtry, 
                                importance = TRUE)
  
  # Prediction on Training Data
  train_predictions_bagging <- predict(bagging_model, train_set)
  train_accuracy_rate_bagging <- sum(diag(table(train_predictions_bagging, train_set$Class))) / sum(table(train_predictions_bagging, train_set$Class))
  train_error_rate_bagging <- 1 - train_accuracy_rate_bagging
  
  # Prediction on Testing Data
  test_predictions_bagging <- predict(bagging_model, test_set)
  test_accuracy_rate_bagging <- sum(diag(table(test_predictions_bagging, test_set$Class))) / sum(table(test_predictions_bagging, test_set$Class))
  test_error_rate_bagging <- 1 - test_accuracy_rate_bagging
  
  bagging_results[[as.character(mtry)]] <- list(model = bagging_model,
                                                train_accuracy = train_accuracy_rate_bagging,
                                                train_error = train_error_rate_bagging,
                                                test_accuracy = test_accuracy_rate_bagging,
                                                test_error = test_error_rate_bagging)
  
  cat(paste("Results with mtry =", mtry, "\n"))
  cat("Accuracy Rate with Bagging on Training Data:", train_accuracy_rate_bagging, "\n")
  cat("Error Rate with Bagging on Training Data:", train_error_rate_bagging, "\n")
  cat("Accuracy Rate with Bagging on Testing Data:", test_accuracy_rate_bagging, "\n")
  cat("Error Rate with Bagging on Testing Data:", test_error_rate_bagging, "\n")
  cat("\n")
}


# Random Forests - Train and Test with Different Numbers of Trees
num_trees <- c(10, 50, 100, 200)
random_forest_results <- list()

for (n_trees in num_trees) {
  rf_model <- randomForest(Class ~ Source + Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate, 
                           data = train_set, 
                           ntree = n_trees, 
                           mtry = 2,
                           importance = TRUE)
  
  random_forest_results[[as.character(n_trees)]] <- rf_model
  
  # Use importance() to calculate variable importance scores
  variable_importance <- importance(rf_model)
  cat("Variable Importance Scores for", n_trees, "Trees:\n")
  #print(variable_importance)
  
  # Prediction on Training Data
  train_predictions_rf <- predict(rf_model, train_set)
  train_accuracy_rate_rf <- sum(diag(table(train_predictions_rf, train_set$Class))) / sum(table(train_predictions_rf, train_set$Class))
  train_error_rate_rf <- 1 - sum(diag(table(train_predictions_rf, train_set$Class))) / sum(table(train_predictions_rf, train_set$Class))
  cat(paste("Accuracy Rate with Random Forests (", n_trees, "trees) on Training Data:", train_accuracy_rate_rf, "\n"))
  cat(paste("Error Rate with Random Forests (", n_trees, "trees) on Training Data:", train_error_rate_rf, "\n"))
  
  # Prediction on Testing Data
  test_predictions_rf <- predict(rf_model, test_set)
  test_accuracy_rate_rf <- sum(diag(table(test_predictions_rf, test_set$Class))) / sum(table(test_predictions_rf, test_set$Class))
  test_error_rate_rf <- 1 - sum(diag(table(test_predictions_rf, test_set$Class))) / sum(table(test_predictions_rf, test_set$Class))
  cat(paste("Accuracy Rate with Random Forests (", n_trees, "trees) on Testing Data:", test_accuracy_rate_rf, "\n"))
  cat(paste("Error Rate with Random Forests (", n_trees, "trees) on Testing Data:", test_error_rate_rf, "\n"))
}




