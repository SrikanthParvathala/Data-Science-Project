Data Merging Project README

Project Overview

This Python script merges two different CSV files containing U.S. Monthly GDP and unemployment data, standardizes the date format, and saves the merged data to a new CSV file. This project uses the Pandas library for data manipulation and merging.

Requirements

- Python 3.6 or higher
- Pandas library
- IDE such as PyCharm or VS code


You can install Pandas using pip:
pip install pandas


File Structure
- main.py: Python script for merging and cleaning data.
- US-Monthly-GDP-History-Data.csv: Source CSV file containing U.S. Monthly GDP data.
- unemployment.csv: Source CSV file containing U.S. unemployment data.
- GDP_Unrate_merged_file_<timestamp>.csv: Output CSV file containing the merged data. The <timestamp> is dynamically generated based on when the script is run.


How to Run the Code
1. Open a terminal and navigate to the folder containing main.py.
2. Run the script using the command:
    python main.py

3. A new file named GDP_Unrate_merged_file_<timestamp>.csv will be generated in the current directory, where <timestamp> will be a unique timestamp corresponding to when the script was run.

