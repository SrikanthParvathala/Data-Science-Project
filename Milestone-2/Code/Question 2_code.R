# Loading libraries
install.packages("corrplot")
library(ggplot2)
library(pROC)
library(e1071)
library(caret)
library(corrplot)
library(dplyr)

# Dummy Dataframe
data <- read.csv("C:\\Users\\12408\\Downloads\\StockPrediction_data.csv")
names(data)
# Converting Source into dummy variables
dummy_vars <- model.matrix(~Source - 1, data=data)  # -1 to drop intercept column

# Converting the matrix to a data frame
dummy_df <- as.data.frame(dummy_vars)

# Binding the dummy variables to the original data
data <- cbind(data, dummy_df)

# Dropping the original 'Source' column
data$Source <- NULL

# Viewing the transformed data
head(data)
attach(data)

# Loading the data
data <- read.csv("C:\\Users\\12408\\Downloads\\StockPrediction_data.csv")

# Converting Source into dummy variables
dummy_vars <- model.matrix(~Source - 1, data=data)  # -1 to drop intercept column
dummy_df <- as.data.frame(dummy_vars)
data <- cbind(data, dummy_df)
data$Source <- NULL

# Creating binary outcome based on the direction of stock movement
data$Close_Diff <- c(diff(data$Close), NA)
data$Close_Up <- ifelse(data$Close_Diff > 0, 1, 0)
data <- data[-nrow(data), ] # Remove the last row since it won't have a label

# Featuring Engineering: Introduce lag feature for Close
data$Prev_Close <- c(NA, head(data$Close, -1))

# Scaling the data
preProcValues <- preProcess(data[, c("Monthly_Real_GDP_Index", "Unemployment_Rate", "Federal_Funds_Rate", "Prev_Close", names(dummy_df))], method = c("center", "scale"))
data <- predict(preProcValues, data)

# Splitting the data into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Loading the data
data <- read.csv("C:\\Users\\12408\\Downloads\\StockPrediction_data.csv")

# Creating binary outcome based on the direction of stock movement
data$Close_Diff <- c(diff(data$Close), NA)
data$Close_Up <- ifelse(data$Close_Diff > 0, 1, 0)
data <- data[-nrow(data), ] # Remove the last row since it won't have a label

# Featuring Engineering: Introducing lag feature for Close
data$Prev_Close <- c(NA, head(data$Close, -1))

# Data Preprocessing: Scale the data
preProcValues <- preProcess(data[, c("Monthly_Real_GDP_Index", "Unemployment_Rate", "Federal_Funds_Rate", "Prev_Close", "Source")], method = c("center", "scale"))
data <- predict(preProcValues, data)

# Splitting the data into training and testing sets
set.seed(42)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# 1. Histogram of Close Prices
ggplot(data, aes(x=Close)) + 
  geom_histogram(fill="blue", color="black", alpha=0.7) + 
  theme_minimal() + 
  labs(title="Distribution of Close Prices", x="Close Price", y="Frequency")

# 2. Correlation Plot
correlation_matrix <- cor(train_data[,c("Monthly_Real_GDP_Index", "Unemployment_Rate", "Federal_Funds_Rate", "Prev_Close")])
corrplot(correlation_matrix, method="circle")

#Logistic Regression Model
fit.logistic <- glm(Close_Up ~ Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate + Prev_Close + Source, 
                    data=train_data, family="binomial")

# Displaying regression summaries
summary_fit <- summary(fit.logistic)
print(summary_fit)

# Predicting on testing data
predicted_probs <- predict(fit.logistic, newdata=test_data, type="response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Confusion Matrix for Logistic Regression
conf_matrix_logistic <- table(test_data$Close_Up, predicted_classes)
print(conf_matrix_logistic)

# 3. ROC Curve
roc_obj <- roc(test_data$Close_Up, predicted_probs)
plot(roc_obj, main="ROC Curve for Logistic Regression", col="blue", lwd=2)
abline(h=0, v=1, col="gray", lty=2)
cat("AUC for Logistic Regression:", auc(roc_obj), "\n")

# Naive Bayes
# Without Laplace estimator
nb_model <- naiveBayes(Close_Up ~ Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate + Prev_Close + Source, 
                       data=train_data)
nb_predictions <- predict(nb_model, test_data)

# Confusion Matrix for Naive Bayes
conf_matrix_nb <- table(test_data$Close_Up, nb_predictions)
print(conf_matrix_nb)

# With Laplace estimator
nb_model_laplace <- naiveBayes(Close_Up ~ Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate + Prev_Close + Source, 
                               data=train_data, laplace=1)
nb_predictions_laplace <- predict(nb_model_laplace, test_data)

# Confusion Matrix for Naive Bayes with Laplace estimator
conf_matrix_nb_laplace <- table(test_data$Close_Up, nb_predictions_laplace)
print(conf_matrix_nb_laplace)

# Feature Importance for Logistic Regression
importance <- summary(fit.logistic)$coefficients[,4]
importance <- importance[-1] # remove intercept
feature_importance_df <- data.frame(Feature = names(importance), Importance = importance)
ggplot(feature_importance_df, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Feature Importance from Logistic Regression", x="", y="Importance")

