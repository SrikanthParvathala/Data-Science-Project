# Loading necessary libraries
library(e1071)
library(caret)

# Dataframe
data <- read.csv("C:\\Users\\12408\\Downloads\\StockPrediction_data (1).csv")

# Creating dummy variables for 'Source' and removing the original 'Source' column
dummy_vars <- model.matrix(~Source - 1, data=data)  # -1 to drop intercept column
dummy_df <- as.data.frame(dummy_vars)
data <- cbind(data, dummy_df)
data$Source <- NULL

# Selecting only the relevant columns
data <- data[, c("Close", "Monthly_Nominal_GDP_Index", "Monthly_Real_GDP_Index", "Unemployment_Rate", "Federal_Funds_Rate", names(dummy_df))]

# Handling missing values (if any)
data$Close[is.na(data$Close)] <- mean(data$Close, na.rm = TRUE)
data$Monthly_Nominal_GDP_Index[is.na(data$Monthly_Nominal_GDP_Index)] <- mean(data$Monthly_Nominal_GDP_Index, na.rm = TRUE)
data$Monthly_Real_GDP_Index[is.na(data$Monthly_Real_GDP_Index)] <- mean(data$Monthly_Real_GDP_Index, na.rm = TRUE)
data$Unemployment_Rate[is.na(data$Unemployment_Rate)] <- mean(data$Unemployment_Rate, na.rm = TRUE)
data$Federal_Funds_Rate[is.na(data$Federal_Funds_Rate)] <- mean(data$Federal_Funds_Rate, na.rm = TRUE)

# Feature Scaling 
scaled_columns <- c("Monthly_Nominal_GDP_Index", "Monthly_Real_GDP_Index", "Unemployment_Rate", "Federal_Funds_Rate")
data[scaled_columns] <- scale(data[scaled_columns])

# training and testing sets
set.seed(123)  # for reproducibility
train_indices <- createDataPartition(data$Close, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Training and Testing a Linear SVM Model
svr_model_linear <- svm(Close ~ ., data = train_data, type = 'eps-regression', kernel = 'linear')
predictions_linear <- predict(svr_model_linear, test_data)
rmse_linear <- sqrt(mean((predictions_linear - test_data$Close)^2))
print(paste("RMSE for Linear Kernel:", rmse_linear))


# RBF Kernel
svr_model_rbf <- svm(Close ~ ., data = train_data, type = 'eps-regression', kernel = 'radial')
predictions_rbf <- predict(svr_model_rbf, test_data)
rmse_rbf <- sqrt(mean((predictions_rbf - test_data$Close)^2))
print(paste("RMSE for RBF Kernel:", rmse_rbf))

# Polynomial Kernel
svr_model_poly <- svm(Close ~ ., data = train_data, type = 'eps-regression', kernel = 'polynomial')
predictions_poly <- predict(svr_model_poly, test_data)
rmse_poly <- sqrt(mean((predictions_poly - test_data$Close)^2))
print(paste("RMSE for Polynomial Kernel:", rmse_poly))

# Sigmoid Kernel
svr_model_sigmoid <- svm(Close ~ ., data = train_data, type = 'eps-regression', kernel = 'sigmoid')
predictions_sigmoid <- predict(svr_model_sigmoid, test_data)
rmse_sigmoid <- sqrt(mean((predictions_sigmoid - test_data$Close)^2))
print(paste("RMSE for Sigmoid Kernel:", rmse_sigmoid))

# Hyperparameter Tuning for the RBF kernel
tune_result <- tune(svm, Close ~ ., data = train_data, type = 'eps-regression', 
                    kernel = 'radial', ranges = list(cost = 10^(-1:2), gamma = 10^(-2:1)))
best_model <- tune_result$best.model
print(best_model)

# Cross-validation
cv_results <- train(Close ~ ., data = train_data, method = "svmRadial", 
                    trControl = trainControl(method = "cv", number = 10))
print(cv_results)

# Checking for Overfitting and Comparing training and testing RMSE for the RBF Kernel
predictions_train_rbf <- predict(svr_model_rbf, train_data)
rmse_train_rbf <- sqrt(mean((predictions_train_rbf - train_data$Close)^2))
print(paste("Training RMSE for RBF Kernel:", rmse_train_rbf))
print(paste("Testing RMSE for RBF Kernel:", rmse_rbf))
best_model
cv_results

library(ggplot2)
# actual vs. predicted values
ggplot() +
  geom_point(aes(x = test_data$Close, y = predictions_rbf), color = "black") +
  geom_line(aes(x = test_data$Close, y = test_data$Close), color = "green") +
  labs(title = "Actual vs. Predicted Close Values", x = "Actual", y = "Predicted") +
  theme_minimal()
# Calculating residuals
residuals_rbf <- predictions_rbf - test_data$Close

# Histogram of residuals
hist(residuals_rbf, breaks = 30, main = "Distribution of Residuals", xlab = "Residuals", col = "gray", border = "black")

