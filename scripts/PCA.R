library(ggplot2)

imputated_data <- readRDS("../imputed.rds")
pca_data <- imputated_data

# separate out the outcome
outcome <- pca_data$comp_outcome

# PCA only works on numeric variables, so select those (excluding outcome)
predictors <- pca_data |>
  dplyr::select(-comp_outcome)|>
  dplyr::select(where(is.numeric)) 

# run PCA with centering and scaling (important if variables are on different scales)

pca_res <- prcomp(predictors, center = TRUE, scale. = TRUE)

# variance explained by each component
summary(pca_res)

### to account for 80% of the variance of the original data, 76 PCs are required 

# scree plot to help decide how many PCs to keep
plot(pca_res, type = "l", main = "Scree Plot")

# scores for first two PCs, with outcome attached for visualization
pca_scores <- as.data.frame(pca_res$x[, 1:76]) |> 
  mutate(ID=data$ID, follow_up_days=data$follow_up_days)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = factor(comp_outcome))) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA: PC1 vs PC2 by comp_outcome", color = "comp_outcome") +
  theme_minimal()
