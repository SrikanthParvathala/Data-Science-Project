library (MASS)
library(ggplot2)

# Loading the data
data <- read.csv(file.choose(), header=TRUE)
data <- subset(data, select = -c(SN, Date, Open, High, Low, Adjusted_Close))

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
dim(train_data)
dim(test_data)

# 1. Filter Method
#Correlations between all features
corrV <- cor(train_data)
corrV

# Building correplot to visualize the correlation matrix. Show numbers (not colors); legend value range
library(corrplot)
corrplot(cor(train_data), method="number", is.corr=FALSE)

#Let's make default model.
model1 = lm(log(Close)~., data=train_data)
summary(model1)

# remove the less significant feature
model2 = update(model1, ~.-Unemployment_rate)
summary(model2)

# remove the another less significant feature
model3 = update(model2, ~.-Federal_Funds_Rate)
summary(model3)

# Train SVM with selected features
library(kernlab)
library(e1071)
#Selecting significant features (based on high Correlated features)
features <- c("Monthly_Nominal_GDP_Index", "Monthly_Real_GDP_Index", "Sourcefacebook",
              "Sourceapple", "Sourceamazon", "Sourcegoogle", "Sourcenetflix")

# Prepare the datasets
train_features <- train_data[, c(features, "Close")]
test_features <- test_data[, c(features, "Close")]
svm_model <- svm(log(Close) ~ ., data = train_features)

# Evaluate the model
predictions <- predict(svm_model, test_features)

# Calculating the correlation (R-squared) and mean squared error 
mse <- mean((log(test_features$Close) - predictions)^2)
correlation <- cor(test_features$Close, predictions)^2
print(mse)
print(correlation)


# 2. Hybrid method (rfe)

# load the library
library(mlbench)
library(caret)

# define the control using a random forest model trained with CV 
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RE algorithm (variables, labels, potential subset sizes to test,
results <- rfe(data[,-7], data[,7], sizes=c(1:11), rfeControl=control)
print(results)

# plot the results
plot(results, type=c("g", "o"))

# list the chosen features
best_features <- predictors(results)
best_features

# 3. Wrapper Method
# Define base intercept only model (no variables)
base.mod <- lm(Close ~ 1, data=train_data)  

# Full model with all predictors
all.mod <- lm(Close ~ . , data= train_data) 

# Perform step-wise algorithm . direction=both, forward, 
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), 
                direction = "forward", trace = 0, steps = 1000)  
 

# Get the shortlisted variable.
shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] 

# remove intercept
# Show
print(shortlistedVars)

#Model
summary(stepMod)
