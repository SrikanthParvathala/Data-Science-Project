# Load necessary libraries
library(neuralnet)

# Load and Preprocess the Data
stock_data <- read.csv("/Users/iamy8000/Desktop/UMD MIM/INST737/milestone 3/StockPrediction_data_google.csv")

normalize_data <- function(df) {
  numeric_columns <- sapply(df, is.numeric)
  df[numeric_columns] <- lapply(df[numeric_columns], normalize)
  return(df)
}

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

stock_data_normalized <- normalize_data(stock_data)

#Creating Training and Testing Sets
set.seed(123) 
indices <- sample(1:nrow(stock_data_normalized), size = 0.8 * nrow(stock_data_normalized))
train_data <- stock_data_normalized[indices, ]
test_data <- stock_data_normalized[-indices, ]


# Neural Network Model
formula <- Adjusted_Close ~ Monthly_Nominal_GDP_Index + Monthly_Real_GDP_Index + Unemployment_Rate + Federal_Funds_Rate
# Tuning
hidden_configurations <- list(c(50, 50),c(50, 50, 50))
activation_functions <- c("logistic", "tanh")
learning_rate <- 0.01    # learning rate
num_epochs <- 100        # number of training epochs
stepmax <- num_epochs * nrow(train_data) * 10

# Store results
results <- data.frame()

# Loop over configurations
for (hidden_neurons in hidden_configurations) {
  for (act_function in activation_functions) {
    # Train the model
    nn_model <- neuralnet(formula,
                          data = train_data,
                          hidden = hidden_neurons,
                          learningrate = learning_rate,
                          algorithm = 'rprop+',
                          act.fct = act_function,
                          linear.output = TRUE,
                          rep = 1,
                          stepmax = stepmax)
    
    # Make predictions
    input_data <- test_data[, -which(names(test_data) == "Adjusted_Close")]
    predictions <- compute(nn_model, input_data)
    
    # Record performance
    performance_metric <- cor(predictions$net.result, test_data$Adjusted_Close)
    results <- rbind(results, data.frame(
      Hidden_Layers = toString(hidden_neurons),
      Activation_Function = act_function,
      Performance_Metric = performance_metric
    ))
  }
}

# Output the results
print(results)

# Overfitting
calculate_performance <- function(model, data, actual) {
  predictions <- compute(model, data)
  return(cor(predictions$net.result, actual))
}

#Evaluate performance
train_performance <- calculate_performance(nn_model, train_data[, -which(names(train_data) == "Adjusted_Close")], train_data$Adjusted_Close)
test_performance <- calculate_performance(nn_model, test_data[, -which(names(test_data) == "Adjusted_Close")], test_data$Adjusted_Close)

# Compare performances
print(paste("Training Performance:", train_performance))
print(paste("Testing Performance:", test_performance))

# Check for overfitting
if (train_performance > test_performance) {
  if (train_performance - test_performance > 0.05) {
    print("Model may be overfitting")
  } else {
    print("Model seems to generalize well")
  }
} else {
  print("Model performs better on test data")
}

# Identify the best model configuration
best_model_index <- which.max(results$Performance_Metric)
best_model_configuration <- results[best_model_index, ]

# Train the best model again for plotting
best_model <- neuralnet(formula,
                        data = train_data,
                        hidden = as.numeric(strsplit(best_model_configuration$Hidden_Layers, ", ")[[1]]),
                        learningrate = learning_rate,
                        algorithm = 'rprop+',
                        act.fct = best_model_configuration$Activation_Function,
                        linear.output = TRUE,
                        rep = 1,
                        stepmax = stepmax)

# Plot the best model
print(best_model_configuration)
plot(best_model)