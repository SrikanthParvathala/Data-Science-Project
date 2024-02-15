# Load necessary library
install.packages("dplyr")
library(dplyr)
library(ggplot2)

data <- read.csv(file.choose(), header=T)

# Pre-processing the data
# Converting 'Source' into dummy variables
dummy_vars <- model.matrix(~Source - 1, data=data)  # -1 to drop intercept column
# Converting the matrix to a data frame
dummy_df <- as.data.frame(dummy_vars)
# Binding the dummy variables to the original data
data <- cbind(data, dummy_df)
# Dropping the original 'Source' column
data$Source <- NULL

# View the transformed data
head(data)
attach(data)
names(data)

# Splitting the data into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# a. Linear Regression Parameters
# Create scatterplots with linear regression lines for each feature
ggplot(data, aes(x = Monthly_Nominal_GDP_Index, y = Close)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Monthly Nominal GDP Index')
ggplot(data, aes(x = Monthly_Real_GDP_Index, y = Close)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Monthly Real GDP Index')
ggplot(data, aes(x = Unemployment_Rate, y = Close)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Unemployment Rate')
ggplot(data, aes(x = Federal_Funds_Rate, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Federal Funds Rate')
ggplot(data, aes(x = Sourceamazon, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Source Amazon')
ggplot(data, aes(x = Sourceapple, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Source Apple')
ggplot(data, aes(x = Sourcefacebook, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Source Facebook')
ggplot(data, aes(x = Sourcegoogle, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Source Google')
ggplot(data, aes(x = Sourcenetflix, y = Close)) +geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") + labs(title = 'Scatter Plot for Source Netflix')

# Univariate regression models
fit.lm1 <- lm(Close ~ Monthly_Nominal_GDP_Index, data=train_data)
fit.lm2 <- lm(Close ~ Monthly_Real_GDP_Index, data=train_data)
fit.lm3 <- lm(Close ~ Unemployment_Rate, data=train_data)
fit.lm4 <- lm(Close ~ Federal_Funds_Rate, data=train_data)
fit.lm5 <- lm(Close ~ Sourceamazon, data=train_data)
fit.lm6 <- lm(Close ~ Sourceapple, data=train_data)
fit.lm7 <- lm(Close ~ Sourcefacebook, data=train_data)
fit.lm8 <- lm(Close ~ Sourcegoogle, data=train_data)
fit.lm9 <- lm(Close ~ Sourcenetflix, data=train_data)

# Display regression summaries
print(summary(fit.lm1))
print(summary(fit.lm2))
print(summary(fit.lm3))
print(summary(fit.lm4))
print(summary(fit.lm5))
print(summary(fit.lm6))
print(summary(fit.lm7))
print(summary(fit.lm8))
print(summary(fit.lm9))

# Plotting residuals vs fitted values for each model
par(mfrow=c(2,2))
plot(fitted(fit.lm1), residuals(fit.lm1), main="Residuals vs Fitted (Nominal GDP)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm2), residuals(fit.lm2), main="Residuals vs Fitted (Real GDP)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm3), residuals(fit.lm3), main="Residuals vs Fitted (Unemployment Rate)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm4), residuals(fit.lm4), main="Residuals vs Fitted (Federal Funds Rate)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm5), residuals(fit.lm5), main="Residuals vs Fitted (Amazon)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm6), residuals(fit.lm6), main="Residuals vs Fitted (Apple)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm7), residuals(fit.lm7), main="Residuals vs Fitted (Facebook)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm8), residuals(fit.lm8), main="Residuals vs Fitted (Google)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

plot(fitted(fit.lm9), residuals(fit.lm9), main="Residuals vs Fitted (Netflix)", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

#Plotting QQ Plot
par(mfrow=c(2,2))
qqnorm(residuals(fit.lm1), main="QQ Plot (Nominal GDP)")
qqline(residuals(fit.lm1), col="red")

qqnorm(residuals(fit.lm2), main="QQ Plot (Real GDP)")
qqline(residuals(fit.lm2), col="red")

qqnorm(residuals(fit.lm3), main="QQ Plot (Unemployment Rate)")
qqline(residuals(fit.lm3), col="red")

qqnorm(residuals(fit.lm4), main="QQ Plot (Federal Funds Rate)")
qqline(residuals(fit.lm4), col="red")

qqnorm(residuals(fit.lm5), main="QQ Plot (Amazon)")
qqline(residuals(fit.lm5), col="red")

qqnorm(residuals(fit.lm6), main="QQ Plot (Apple)")
qqline(residuals(fit.lm6), col="red")

qqnorm(residuals(fit.lm7), main="QQ Plot (Facebook)")
qqline(residuals(fit.lm7), col="red")

qqnorm(residuals(fit.lm8), main="QQ Plot (Google)")
qqline(residuals(fit.lm8), col="red")

qqnorm(residuals(fit.lm9), main="QQ Plot (Netflix)")
qqline(residuals(fit.lm9), col="red")


# Predict on testing data
# Required libraries
install.packages(c("modelr", "broom", "MASS"))
library(modelr)
library(broom)
library(MASS)

# Training a linear regression model
model <- lm(Close ~ ., data=train_data)

# Predicting on the testing data
predictions <- predict(model, newdata=test_data, interval="prediction")
confidence_interval <- predict(model, newdata=test_data, interval="confidence")

# Calculating the correlation (R-squared) and mean squared error
correlation <- cor(test_data$Close, predictions)^2
mse <- mean((test_data$Close - predictions)^2)

# Display results
results <- data.frame(Predicted_Close=predictions, 
                      Confidence_Lower=confidence_interval[, "lwr"],
                      Confidence_Upper=confidence_interval[, "upr"],
                      Prediction_Lower=predictions[, "lwr"],
                      Prediction_Upper=predictions[, "upr"])

print(results)
print(paste("Correlation (R-squared): ", correlation))
print(paste("Mean Squared Error (MSE): ", mse))

# Plot the actual closing prices
plot(test_data$Close, type="l", lwd=2, col="blue", ylim=c(min(prediction_interval_lower), max(prediction_interval_upper)),
     xlab="Index", ylab="Close Price", main="Predicted Closing Price with Confidence and Prediction Bands")

# Add the predicted values as a line
lines(predictions, col="red", lwd=2)

# Add the confidence bands
matlines(confidence_interval[, c("lwr", "upr")], lty=c(2,2), col=c("green", "green"), lwd=2)

# Add the prediction bands
matlines(predictions[, c("lwr", "upr")], lty=c(3,3), col=c("orange", "orange"), lwd=2)

# Add a legend
legend("topright", legend=c("Actual", "Predicted", "Confidence Band", "Prediction Band"),
       col=c("blue", "red", "green", "orange"), lty=c(1, 1, 2, 3), lwd=2)

# b. Multivariate regressions
# Evaluate different combinations of features 
# Required libraries
library(leaps)

# Define the full model formula
full_formula <- Close ~ Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + 
  Unemployment_Rate + Federal_Funds_Rate + Sourceamazon + 
  Sourceapple + Sourcefacebook + Sourcegoogle + Sourcenetflix

# Use the regsubsets function to get the best models for each number of predictors
best_subsets <- regsubsets(full_formula, data=train_data, nvmax=8)

# Get summary of best subsets
subsets_summary <- summary(best_subsets)

# Print the best model for each number of predictors
cat("Best models for each number of predictors:\n")
for (i in 1:8) {  # Adjusted for nvmax=8
  cat(paste("Number of predictors:", i), "\n")
  cat("Predictors:", names(which(subsets_summary$which[i, -1])), "\n")
  cat("Adjusted R2:", round(subsets_summary$adjr2[i], 5), "\n")
  cat("AIC:", round(subsets_summary$cp[i], 5), "\n\n")
}
# Multivariate regressions for all independent features
fit.multi <- lm(Close ~  Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate + Sourceamazon + Sourceapple + Sourcefacebook + Sourcegoogle + Sourcenetflix, data=train_data)
print(summary(fit.multi))

# training and testing sets has been split at the start after pre-processing the data at line 23
# Predictions on test set
predictions_multi <- predict(fit.multi, newdata=test_data)

# Computing correlation and mean square error for multivariate regression
cor_multi <- cor(predictions_multi, test_data$Close)
mse_multi <- mean((predictions_multi - test_data$Close)^2)

print(paste("Correlation (Multivariate Regression):", cor_multi))
print(paste("Mean Square Error (Multivariate Regression):", mse_multi))

# c. Regularization
# Load the necessary library
# Prepare matrix format required by glmnet
library(glmnet)

# Regularization for individual features
evaluate_regularization <- function(feature_name) {
  x_train <- as.matrix(train_data[, feature_name, drop = FALSE])
  x_train <- cbind(1, x_train) # Adding a column of ones
  
  y_train <- train_data$Close
  
  # Ridge Regression
  cv.ridge <- cv.glmnet(x_train, y_train, alpha=0)
  best_lambda_ridge <- cv.ridge$lambda.min
  ridge_best <- glmnet(x_train, y_train, alpha=0, lambda=best_lambda_ridge)
  
  # Lasso Regression
  cv.lasso <- cv.glmnet(x_train, y_train, alpha=1)
  best_lambda_lasso <- cv.lasso$lambda.min
  lasso_best <- glmnet(x_train, y_train, alpha=1, lambda=best_lambda_lasso)
  
  x_test <- as.matrix(test_data[, feature_name, drop = FALSE])
  x_test <- cbind(1, x_test) # Adding a column of ones
  
  y_test <- test_data$Close
  
  # Predictions
  predictions_ridge <- predict(ridge_best, newx=x_test, s=best_lambda_ridge, type='response')
  predictions_lasso <- predict(lasso_best, newx=x_test, s=best_lambda_lasso, type='response')
  
  # Ridge Regression Metrics
  cor_ridge <- cor(predictions_ridge, y_test)
  mse_ridge <- mean((predictions_ridge - y_test)^2)
  
  # Lasso Regression Metrics
  cor_lasso <- cor(predictions_lasso, y_test)
  mse_lasso <- mean((predictions_lasso - y_test)^2)
  
  return(list(cor_ridge=cor_ridge, mse_ridge=mse_ridge, cor_lasso=cor_lasso, mse_lasso=mse_lasso))
}

# Now, apply the function to each feature:
results_nominal_GDP <- evaluate_regularization('Monthly_Nominal_GDP_Index')
results_real_GDP <- evaluate_regularization('Monthly_Real_GDP_Index')
results_unemployment_rates <- evaluate_regularization('Unemployment_Rate')
results_federal_rates <- evaluate_regularization('Federal_Funds_Rate')
results_amazon <- evaluate_regularization('Sourceamazon')
results_apple <- evaluate_regularization('Sourceapple')
results_facebook <- evaluate_regularization('Sourcefacebook')
results_google <- evaluate_regularization('Sourcegoogle')
results_netflix <- evaluate_regularization('Sourcenetflix')

# Display Regularization results for individual features
results_nominal_GDP 
results_real_GDP 
results_unemployment_rates
results_federal_rates 
results_amazon 
results_apple
results_facebook
results_google
results_netflix

# Regularization for Multivariate features
x_train <- as.matrix(train_data[,c('Monthly_Nominal_GDP_Index', 'Monthly_Real_GDP_Index', 'Unemployment_Rate', 'Federal_Funds_Rate', 'Sourceamazon', 'Sourceapple', 'Sourcefacebook', 'Sourcegoogle', 'Sourcenetflix')])
y_train <- train_data$Close

# Ridge Regression (L2 Regularization)
ridge_model <- glmnet(x_train, y_train, alpha=0) # alpha=0 for ridge
cv.ridge <- cv.glmnet(x_train, y_train, alpha=0)
best_lambda_ridge <- cv.ridge$lambda.min
ridge_best <- glmnet(x_train, y_train, alpha=0, lambda=best_lambda_ridge)

# Lasso Regression (L1 Regularization)
lasso_model <- glmnet(x_train, y_train, alpha=1) # alpha=1 for lasso
cv.lasso <- cv.glmnet(x_train, y_train, alpha=1)
best_lambda_lasso <- cv.lasso$lambda.min
lasso_best <- glmnet(x_train, y_train, alpha=1, lambda=best_lambda_lasso)

# Predictions on test set
x_test <- as.matrix(test_data[,c('Monthly_Nominal_GDP_Index', 'Monthly_Real_GDP_Index', 'Unemployment_Rate', 'Federal_Funds_Rate', 'Sourceamazon', 'Sourceapple', 'Sourcefacebook', 'Sourcegoogle', 'Sourcenetflix')])
y_test <- test_data$Close

predictions_ridge <- predict(ridge_best, newx=x_test, s=best_lambda_ridge, type='response')
predictions_lasso <- predict(lasso_best, newx=x_test, s=best_lambda_lasso, type='response')

# Computing correlation and mean square error for Ridge and Lasso
cor_ridge <- cor(predictions_ridge, y_test)
mse_ridge <- mean((predictions_ridge - y_test)^2)

cor_lasso <- cor(predictions_lasso, y_test)
mse_lasso <- mean((predictions_lasso - y_test)^2)

print(paste("Correlation (Ridge Regression):", cor_ridge))
print(paste("Mean Square Error (Ridge Regression):", mse_ridge))

print(paste("Correlation (Lasso Regression):", cor_lasso))
print(paste("Mean Square Error (Lasso Regression):", mse_lasso))

# d. Repeating experiments with different random splits
num_iterations <- 10
correlations <- matrix(0, nrow=num_iterations, ncol=3) # columns for linear, ridge, and lasso
mses <- matrix(0, nrow=num_iterations, ncol=3)

for (i in 1:num_iterations) {
  train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Linear Regression
  fit.multi <- lm(Close ~ Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate + Sourceamazon + Sourceapple + Sourcefacebook + Sourcegoogle + Sourcenetflix, data=train_data)
  predictions_linear <- predict(fit.multi, newdata=test_data)
  
  cor_linear <- cor(predictions_linear, test_data$Close)
  mse_linear <- mean((predictions_linear - test_data$Close)^2)
  
  correlations[i,] <- c(cor_linear, cor_ridge, cor_lasso)
  mses[i,] <- c(mse_linear, mse_ridge, mse_lasso)
}

print(correlations)
print(mses)

# Average correlations for each model
avg_cor_linear <- mean(correlations[,1])
avg_cor_ridge <- mean(correlations[,2])
avg_cor_lasso <- mean(correlations[,3])

# Average MSEs for each model
avg_mse_linear <- mean(mses[,1])
avg_mse_ridge <- mean(mses[,2])
avg_mse_lasso <- mean(mses[,3])

# Print the averages
print(paste("Average Correlation (Linear Regression): ", avg_cor_linear))
print(paste("Average Correlation (Ridge Regression): ", avg_cor_ridge))
print(paste("Average Correlation (Lasso Regression): ", avg_cor_lasso))

print(paste("Average MSE (Linear Regression): ", avg_mse_linear))
print(paste("Average MSE (Ridge Regression): ", avg_mse_ridge))
print(paste("Average MSE (Lasso Regression): ", avg_mse_lasso))
