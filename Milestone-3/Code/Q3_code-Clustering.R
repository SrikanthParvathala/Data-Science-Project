# Install required packages if not already installed
install.packages("tidyverse")
install.packages("lubridate")
install.packages("factoextra")
install.packages("NbClust")
install.packages("dbscan")
install.packages("cluster")
install.packages("FactoMineR")
install.packages("fpc")

# Loading the data
data <- read.csv(file.choose(), header=TRUE)

# Converting 'Date' to numeric (number of days since the start of the dataset)
library(tidyverse)
library(lubridate)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
start_date <- min(data$Date)
data$Date <- as.numeric(difftime(data$Date, start_date, units = "days"))

# Pre-processing the data
# Converting 'Source' into dummy variables
dummy_vars <- model.matrix(~Source - 1, data=data)  # -1 to drop intercept column
# Converting the matrix to a data frame
dummy_df <- as.data.frame(dummy_vars)
# Binding the dummy variables to the original data
data <- cbind(data, dummy_df)
# Dropping the original 'Source' column
data$Source <- NULL

head(data)
attach(data)

# Normalizing the data
data_scaled <- scale(data[, sapply(data, is.numeric)])

# K-Means Clustering
library(factoextra)
library(NbClust)
set.seed(123)
# Intially considering k value as 2 and running the K-means
kmeans_result <- kmeans(data_scaled, centers = 2,nstart = 100)
fviz_cluster(kmeans_result, data = data_scaled)

# Finding the best K value using WSS method (Elbow method)
fviz_nbclust(data_scaled,kmeans, method = "wss")
kmeans_result <- kmeans(data_scaled, centers = 6,nstart = 100)
fviz_cluster(kmeans_result, data = data_scaled)

#Calculating element size and centroids
sizes <- kmeans_result$size
print(sizes)

centroids <- kmeans_result$centers
print(centroids)

# Hierarchical Clustering
library(cluster)
d <- dist(data_scaled, method = "euclidean")
hc1 <- hclust(d, method = "complete")
plot(hc1, cex=0.6, hang=-1)
rect.hclust(hc1, k = 6, border = 2)  # we choose 6 clusters, so K=6

# Cut the tree at the desired number of clusters
k <- 6  
clusters <- cutree(hc1, k)

# Number of elements per cluster
sizes_hc1 <- table(clusters)
print(sizes_hc1)

# Density Based (DBSCAN) Clustering
library(fpc)
library(FactoMineR)
library(dbscan)

#'data_scaled' is already pre-processed and scaled stock prediction data in line 24
# Applying PCA since the data is high-dimensional
pca_result <- PCA(data_scaled, graph = FALSE)
data_pca <- data.frame(pca_result$ind$coord[, 1:2])

# Estimating the appropriate epsilon value
dbscan::kNNdistplot(data_pca, k=5)
abline(h=0.06, lty=2) # Adjust the 0.15 based on your kNNdistplot

# Apply DBSCAN clustering and finding outliers
set.seed(123)
db <- fpc::dbscan(data_pca, eps=0.06, MinPts = 5) # Adjust eps and MinPts based on your dataset
db

# Plot DBSCAN results using dbscan's plot function
plot(db, data_pca, main="DBSCAN Clustering")

# To get the centroid values, aggregate the data based on cluster assignment
centroid_data <- aggregate(data_scaled, by=list(cluster=clusters), mean)
print(centroid_data)

# Number of elements per cluster
table(db$cluster)
