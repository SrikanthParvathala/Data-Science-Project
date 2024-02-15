import pandas as pd
from datetime import datetime

# Generate a timestamp
timestamp = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')

# Read the CSV files
df1 = pd.read_csv("US-Monthly-GDP-History-Data.csv")
df2 = pd.read_csv("unemployment.csv")

# Standardize the date formats
try:
    df1['Standardized_Date'] = pd.to_datetime(df1['DATE'], format='%Y - %b', errors='coerce')
    df2['Standardized_Date'] = pd.to_datetime(df2['DATE']).dt.to_period('M').dt.to_timestamp()

    # Merge the DataFrames on the standardized date column
    merged_df = pd.merge(df1, df2, left_on='Standardized_Date', right_on='Standardized_Date', how='inner')

    # Keep only the required columns
    final_df = merged_df[['Standardized_Date', 'Monthly Nominal GDP Index', 'Monthly Real GDP Index', 'UNRATE']].copy()

    # Convert datetime to Year-Month format
    final_df['Standardized_Date'] = final_df['Standardized_Date'].dt.strftime('%Y-%m')

    # Save the cleaned DataFrame to a new CSV file
    final_df.to_csv(f"GDP_Unrate_merged_file_{timestamp}.csv", index=False)
except Exception as e:
    print(f"An error occurred: {e}")
