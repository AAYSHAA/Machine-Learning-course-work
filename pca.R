# PCA analysis
pca_result <- prcomp(standardized_features, center = TRUE)
print(summary(pca_result))

# Print eigenvalues and eigenvectors
print(pca_result$sdev) #this print the eigenvalues
print(pca_result$rotation)#this prints the eigenvectors

# Create a plot of the explained variance by each principal component
fviz_eig(pca_result)

# Create a plot of the cumulative variance explained by each principal component
fviz_eig(pca_result, choice = "variance")

# Create a plot of the correlation between variables and principal components
fviz_pca_var(pca_result)

# Calculate cumulative variance explained by each principal component
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))

# Find the number of principal components required to achieve cumulative score > 0.85
pc_cutoff <- min(which(cumulative_variance > 0.85))

# Select principal components
selected_pcs <- paste0("PC", 1:pc_cutoff)

# Create a transformed dataset with only the selected principal components as attributes
transformed_dataset <- predict(pca_result, standardized_features)[, 1:pc_cutoff]
colnames(transformed_dataset) <- selected_pcs

# Print selected PCs
print(selected_pcs)

# Print transformed dataset with selected PCs
head(transformed_dataset)

# Determine the number of clusters using NbClust
nb_clusters <- NbClust(transformed_dataset, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index = "all")
print(nb_clusters)

# Elbow method
wss <- numeric(10)
for (i in 2:10) {
  tryCatch({
    kmeans_model <- kmeans(transformed_dataset, centers = i, nstart = 25, iter.max = 100)
    wss[i] <- kmeans_model$tot.withinss
  }, warning = function(w) {
    cat("Warning:", conditionMessage(w), "\n")
    wss[i] <- NA
  })
}

# Plot the elbow method
elbow_plot <- data.frame(NumClusters = 2:10, WSS = wss[2:10])
elbow <- ggplot(elbow_plot, aes(x = NumClusters, y = WSS)) +
  geom_point() +
  geom_line() +
  labs(title = "Elbow Method", x = "Number of Clusters", y = "Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()

print(elbow)

# Compute gap statistics with increased number of bootstrap replicates
gap <- clusGap(transformed_dataset, FUN = function(x, k) kmeans(x, k, iter.max = 100), K.max = 10, B = 100)

# Plot the gap statistics
plot(gap, main = "Gap statistic for k-means clustering")

# Determine the optimal number of clusters using maxSE function
best_clusters <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")

# Check contents of best_clusters list
str(best_clusters)

# Silhouette Plot
fviz_nbclust(transformed_dataset, kmeans, method = 'silhouette')

# Add k=2
k <- 2
set.seed(123) # Set the seed for reproducibility
km <- kmeans(transformed_dataset, centers = k, nstart = 25)
fviz_cluster(km, data = transformed_dataset)
print(km)
print(km$centers)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
WSS<-sum(km$tot.withinss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
WSS_TSS_ratio <- WSS/TSS
cat("The ratio of between_cluster_sums_of_squares (BSS) over
total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")
cat("The ratio of within_cluster_sums_of_squares WSS) over
total_sum_of_squares (TSS) is", WSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", WSS, "\n")



# Add k=3
k <- 3
set.seed(123) # Set the seed for reproducibility
km <- kmeans(transformed_dataset, centers = k, nstart = 25)
fviz_cluster(km, data =transformed_dataset)
print(km)
print(km$centers)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
WSS<-sum(km$tot.withinss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
WSS_TSS_ratio <- WSS/TSS

cat("The ratio of between_cluster_sums_of_squares (BSS) over
total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")
cat("The ratio of within_cluster_sums_of_squares WSS) over
total_sum_of_squares (TSS) is", WSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", WSS, "\n")

# Add k= 8
k <- 8
set.seed(123) # Set the seed for reproducibility
km <- kmeans(transformed_dataset, centers = k, nstart = 25)
fviz_cluster(km, data = transformed_dataset)
print(km)
print(km$centers)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
WSS<-sum(km$tot.withinss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
WSS_TSS_ratio <- WSS/TSS
cat("The ratio of between_cluster_sums_of_squares (BSS) over
total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")
cat("The ratio of within_cluster_sums_of_squares WSS) over
total_sum_of_squares (TSS) is", WSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", WSS, "\n")

# Display the silhouette plot
# Compute silhouette values
sil <- silhouette(km$cluster, dist(transformed_dataset))

# Plot the silhouette
fviz_silhouette(sil)

# Calculate average silhouette width
average_silhouette_width <- mean(sil[, "sil_width"])
cat("Average silhouette width:", average_silhouette_width)

library(fpc) # Load the fpc package

# Calculate the Calinski-Harabasz index for 2-10 clusters
res <- NbClust(transformed_dataset, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "ch")
print(res)
print(res$Best.nc)

# Plot the results
barplot(res$Best.nc[2], names.arg = res$Best.nc[1], xlab = "Number of clusters", ylab ="Calinski-Harabasz index")



