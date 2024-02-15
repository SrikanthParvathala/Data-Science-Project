install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("tidyr")


#importing the dataset(the file path might change)
my_data <- read.csv("/Users/srikanthp/Desktop/INST737 - Intro to Data Science/Projects/archive-2/Amazon.csv")

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


file_names <- c("amazon.csv", "facebook.csv", "google.csv", "apple.csv", "netflix.csv") 

# Initializing an empty data frame
merged_data <- data.frame()

# Looping through the files
for (file in file_names) {
# Reading the CSV file
  data <- read.csv(file)
  
# Converting the 'Date' column to Date class if it is not
  data$Date <- as.Date(data$Date, format="%Y-%m-%d") # adjust the format according to your date format
  
# Filtering the rows where Date is on or after 2015-01-01
  data <- filter(data, Date >= as.Date("2015-01-01"))
  
# Adding a new column 'Source' to identify the source of the data
  data$Source <- gsub(".csv", "", file)
# Merging the filtered data 
  merged_data <- rbind(merged_data, data)
}


amazon_data <- filter(merged_data, Source == "amazon")
# Shifting "Source" to the First column
merged_data <- merged_data %>% select(Source, everything())
write.csv(merged_data, "Merged_data.csv", row.names = FALSE)

# Reading the merged file
merged_data1 <- read.csv("Merged_Data.csv")

# Checking for Null Values
any_na <- any(is.na(merged_data))

if (any_na) {
  print("There are null values in the data frame.")
  na_count <- sapply(merged_data, function(x) sum(is.na(x)))
  print("Number of NA values in each column:")
  print(na_count)
} else {
  print("There are no null values in the entire data frame.")
}




#Renaming Adj.close to Adjusted_close
data <- data %>% 
  rename(Adjusted_Close = Adj.Close)

#importing the dataset
data <- read.csv (file.choose(), header=T)
str(data)
summary(data)


# Mean 
mean(data$Open)
mean(data$Close)
mean(data$Low)
mean(data$High)
mean(data$Adjusted_Close)


# Median 
median(data$Open)
median(data$Close)
median(data$Low)
median(data$High)
median(data$Adjusted_Close)

# Standard deviation
sd(data$Open)
sd(data$Close)
sd(data$Low)
sd(data$High)
sd(data$Adjusted_Close)

# Variance 
var(data$Open)
var(data$Close)
var(data$Low)
var(data$High)
var(data$Adjusted_Close)

#Boxplot
boxplot(data[,c("Open", "High", "Low", "Close")], main="Boxplot of Variables")

# Histogram for each variable
hist(data$Open, main="Histogram of Open")
hist(data$High, main="Histogram of High")
hist(data$Low, main="Histogram of Low")
hist(data$Close, main="Histogram of Close")

#ggplot
library(ggplot2)
ggplot(data, aes(x=Source, y=Open, fill=Source)) +
  geom_boxplot() +
  labs(title="Boxplot of Open")

ggplot(data, aes(x=Source, y=Close, fill=Source)) +
  geom_boxplot() +
  labs(title="Boxplot of Close")

ggplot(data, aes(x=Source, y=High, fill=Source)) +
  geom_boxplot() +
  labs(title="Boxplot of High")

ggplot(data, aes(x=Source, y=Low, fill=Source)) +
  geom_boxplot() +
  labs(title="Boxplot of Low")

ggplot(data, aes(x = Source, y = Adjusted_Close, fill = Source)) +
  geom_boxplot() +
  labs(title = "Boxplot of Adjusted Close")


#gg plot to compare company's opening and closing stock values since 2015
ggplot(data, aes(x = Date, y = Value, color = Source, linetype = Type, group = interaction(Source, Type))) +
  geom_line() +
  labs(title = "Comparison of Company's Opening and Closing Stock values from 2015",
       x = "Date",
       y = "Value") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# To save the file, use the path where the file needed to be saved
write.csv(data, "C:/Users/12408/Desktop/data.csv")

# For GDP & Unemployment Dataset analysis
#importing the dataset
df=read.csv (file.choose(), header=T)
str(df)
summary(df)

# Histogram for each variable
hist(df$Monthly.Nominal.GDP.Index, main="Histogram of Nominal GDP")
hist(df$Monthly.Real.GDP.Index, main="Histogram of Real GDP")
hist(df$UNRATE, main="Histogram of Unemployment")







