README for Stock Price Prediction Project

Overview

This project involves predicting stock prices using machine learning models in Python and R. The Python portion of the project utilizes popular libraries like Pandas, NumPy, and Scikit-learn, while the R portion leverages e1071 and caret. The primary goal is to train and evaluate various models to predict the closing price of stocks based on historical data.

Python Implementation

Key Features:
Data Processing: The dataset is normalized and one-hot encoded for categorical variables.
Model Training: Multiple models like Support Vector Machines (SVM), Random Forests, Decision Trees, and Neural Networks are trained.
Cross-Validation: The models are evaluated using Repeated K-Fold cross-validation to assess their performance.
Visualization: Mean Squared Error (MSE) of different models is visualized using boxplots and histograms.
Usage:
1. Upload the Dataset: The dataset in CSV format must be uploaded.
2. Run the Script: Execute the provided Python script. It processes the data and trains multiple models.
R Implementation

Key Features:
Data Preparation: Dummy variables are created for categorical data, and missing values are handled.
Feature Scaling: Numerical features are scaled.
SVM Models: Different kernels (linear, RBF, polynomial, sigmoid) are used for training SVM models.
Model Evaluation: Root Mean Squared Error (RMSE) is calculated for each model.
Hyperparameter Tuning**: The RBF kernel model undergoes hyperparameter tuning.
Visualization: Actual vs. predicted values and the distribution of residuals are visualized.

Usage:
1. **Load the Dataset**: Load the dataset using the provided path.
2. **Run the Script**: Execute the R script which includes training, testing, and evaluating the models.

Requirements

Python Libraries:
- pandas
- numpy
- sklearn
- matplotlib
- seaborn
- google.colab (for file upload)

R Packages:
- e1071
- caret
- ggplot2

Data Format

The dataset should be in CSV format with columns relevant to stock market data. Adjust the column names in the script according to your dataset.

