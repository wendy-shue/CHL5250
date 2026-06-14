library(here)
library(tidyverse)
library(caret)

df_imputed <- readRDS("../imputed.rds")
pca_scores <- readRDS("../pca_scores.rds")

full_data <- df_imputed %>%
  select(-comp_outcome) %>%
  bind_cols(pca_scores)

## 80/20 Train-Test Split
set.seed(123)
train_index <- createDataPartition(full_data$comp_outcome, p = 0.8, list = FALSE)
train_data <- full_data[train_index, ]
test_data  <- full_data[-train_index, ]

## K-Means Clustering
# Train data
train_pca_features <- train_data %>% 
  select(starts_with("PC")) %>%
  mutate(across(everything(), as.numeric))

set.seed(123)
kmeans_train <- kmeans(train_pca_features, centers = 4, nstart = 25)
train_data$Cluster <- as.factor(kmeans_train$cluster)

# Test data
test_pca_features <- test_data %>% 
  select(starts_with("PC")) %>%
  mutate(across(everything(), as.numeric))

assign_cluster <- function(new_data, centroids) {
  apply(new_data, 1, function(row) {
    distances <- apply(centroids, 1, function(center) sum((row - center)^2))
    return(which.min(distances))
  })
}

test_data$Cluster <- as.factor(assign_cluster(test_pca_features, kmeans_train$centers))

# Save datasets
saveRDS(train_data, "../train_clustered.rds")
saveRDS(test_data, "../test_clustered.rds")

## Elbow Plot
set.seed(123)
# Test k from 1 to 10 on the training features ONLY
wss <- sapply(1:10, function(k) {
  kmeans(train_pca_features, centers = k, nstart = 10)$tot.withinss
})

elbow_data <- data.frame(k = 1:10, WSS = wss)

elbow_plot <- ggplot(elbow_data, aes(x = k, y = WSS)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "darkgray", linewidth = 1) + 
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  labs(title = "Elbow Method for Optimal Clusters (Training Data)",
       x = "Number of Clusters (k)", 
       y = "Total Within-Cluster Sum of Squares")

print(elbow_plot)

## Cluster Map
cluster_map <- ggplot(train_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.4, size = 1.5) +
  stat_ellipse(level = 0.95, linewidth = 1, linetype = "solid") + 
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "K-Means Phenotypes in PCA Space (Training Data)",
       x = "Principal Component 1 (PC1)", 
       y = "Principal Component 2 (PC2)",
       color = "Phenotype")

print(cluster_map)
