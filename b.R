# Add k=2
k <- 2
set.seed(123) # Set the seed for reproducibility
km <- kmeans(standardized_features, centers = k, nstart = 25)
fviz_cluster(km, data = standardized_features)
print(km)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
cat("The ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", km$tot.withinss, "\n")

# Plot the WSS for k = 2
fviz_nbclust(standardized_features, kmeans, method = "wss") +
  ggtitle("WSS Plot") +
  xlab("Number of clusters") +
  ylab("Within-cluster sum of squares")



# Add k=3
k <- 3
set.seed(123) # Set the seed for reproducibility
km <- kmeans(standardized_features, centers = k, nstart = 25)
fviz_cluster(km, data = standardized_features)
print(km)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
cat("The ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", km$tot.withinss, "\n")

# Plot the WSS
fviz_nbclust(standardized_features, kmeans, method = "wss") +
  ggtitle("WSS Plot") +
  xlab("Number of clusters") +
  ylab("Within-cluster sum of squares")


# Add k=4
k <- 4
set.seed(123) # Set the seed for reproducibility
km <- kmeans(standardized_features, centers = k, nstart = 25)
fviz_cluster(km, data = standardized_features)
print(km)

# Display the BSS/TSS ratio
BSS <- sum(km$betweenss)
TSS <- sum(km$totss)
BSS_TSS_ratio <- BSS/TSS
cat("The ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_squares (TSS) is", BSS_TSS_ratio, "\n")

# Display the BSS and WSS indices
cat("The between_cluster_sums_of_squares (BSS) index is", BSS, "\n")
cat("The within_cluster_sums_of_squares (WSS) index is", km$tot.withinss, "\n")

# Plot the WSS for k = 4
fviz_nbclust(standardized_features, kmeans, method = "wss") +
  ggtitle("WSS Plot") +
  xlab("Number of clusters") +
  ylab("Within-cluster sum of squares")

# Display the silhouette plot
# Compute silhouette values
sil <- silhouette(km$cluster, dist(standardized_features))

# Plot the silhouette
fviz_silhouette(sil)

# Calculate average silhouette width
average_silhouette_width <- mean(sil[, "sil_width"])
cat("Average silhouette width:", average_silhouette_width)
