library(here)
library(tidyverse)
library(survival)
library(survminer)
library(timeROC)
library(lmtest)

train_data <- readRDS("../train_clustered.rds")
test_data  <- readRDS("../test_clustered.rds")

## Survival Analysis
cox_base <- coxph(Surv(follow_up_days, comp_outcome) ~ demographics_age_index_visit + 
                    demographics_birth_sex, data = train_data)

cox_clust <- coxph(Surv(follow_up_days, comp_outcome) ~ demographics_age_index_visit + 
                     demographics_birth_sex + Cluster, data = train_data)

# Likelihood Ratio Test
lrt_cox <- lrtest(cox_base, cox_clust)
cat("Cox LRT p-value (Base vs Cluster):", lrt_cox$`Pr(>Chisq)`[2], "\n")

# LRT
lrt_cox <- lrtest(cox_base, cox_clust)
p_val <- lrt_cox$`Pr(>Chisq)`[2]
cat("Cox LRT p-value:", p_val, "\n")

## === AUC Line Plot (Test Data) ===
roc_times <- c(365, 1095, 1825) # 1, 3, and 5 years in days

# AUC for Base Model (Test)
time_roc_base_test <- timeROC(
  T = test_data$follow_up_days, 
  delta = test_data$comp_outcome,
  marker = predict(cox_base, newdata = test_data, type = "lp"), 
  cause = 1, weighting = "marginal", 
  times = roc_times, 
  iid = FALSE
)

# AUC for Cluster Model (Test)
time_roc_clust_test <- timeROC(
  T = test_data$follow_up_days, 
  delta = test_data$comp_outcome,
  marker = predict(cox_clust, newdata = test_data, type = "lp"), 
  cause = 1, weighting = "marginal", 
  times = roc_times, 
  iid = FALSE
)
auc_data_test <- data.frame(
  Horizon = rep(c("1 year", "3 years", "5 years"), 2),
  Model = rep(c("AUC_base", "AUC_cluster"), each = 3),
  AUC = c(time_roc_base_test$AUC[1:3], time_roc_clust_test$AUC[1:3])
)

auc_data_test$Horizon <- factor(auc_data_test$Horizon, levels = c("1 year", "3 years", "5 years"))

# Line Plot (Test)
auc_plot_test <- ggplot(auc_data_test, aes(x = Horizon, y = AUC, color = Model, group = Model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.3f", AUC)), 
            vjust = -1,
            size = 4,
            show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("AUC_base" = "#e76f64", "AUC_cluster" = "#56b4b9")) +
  labs(
    title = "Time-Dependent AUC at 1, 3, and 5 Years (Test Data)",
    x = "Prediction horizon",
    y = "AUC",
    color = "Model"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(auc_plot_test)

## === AUC Line Plot (Train Data) ===
# AUC for Base Model (Train)
time_roc_base_train <- timeROC(
  T = train_data$follow_up_days, 
  delta = train_data$comp_outcome,
  marker = predict(cox_base, newdata = train_data, type = "lp"), 
  cause = 1, weighting = "marginal", 
  times = roc_times, 
  iid = FALSE
)

# AUC for Cluster Model (Train)
time_roc_clust_train <- timeROC(
  T = train_data$follow_up_days, 
  delta = train_data$comp_outcome,
  marker = predict(cox_clust, newdata = train_data, type = "lp"), 
  cause = 1, weighting = "marginal", 
  times = roc_times, 
  iid = FALSE
)
auc_data_train <- data.frame(
  Horizon = rep(c("1 year", "3 years", "5 years"), 2),
  Model = rep(c("AUC_base", "AUC_cluster"), each = 3),
  AUC = c(time_roc_base_train$AUC[1:3], time_roc_clust_train$AUC[1:3])
)

auc_data_train$Horizon <- factor(auc_data_train$Horizon, levels = c("1 year", "3 years", "5 years"))

# Line Plot (Train)
auc_plot_train <- ggplot(auc_data_train, aes(x = Horizon, y = AUC, color = Model, group = Model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.3f", AUC)), 
            vjust = -1,
            size = 4,
            show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("AUC_base" = "#e76f64", "AUC_cluster" = "#56b4b9")) +
  labs(
    title = "Time-Dependent AUC at 1, 3, and 5 Years (Train Data)",
    x = "Prediction horizon",
    y = "AUC",
    color = "Model"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(auc_plot_train)

## === ROC Curve (Cluster Model - Test) ===
roc_curve_test <- data.frame(
  FP = c(time_roc_clust_test$FP[, 1], time_roc_clust_test$FP[, 2], time_roc_clust_test$FP[, 3]),
  TP = c(time_roc_clust_test$TP[, 1], time_roc_clust_test$TP[, 2], time_roc_clust_test$TP[, 3]),
  Horizon = factor(rep(c("1 Year", "3 Years", "5 Years"), each = nrow(time_roc_clust_test$FP)),
                   levels = c("1 Year", "3 Years", "5 Years"))
)

# Plot ROC Curves (Test)
roc_plot_test <- ggplot(roc_curve_test, aes(x = FP, y = TP, color = Horizon)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray") +
  theme_minimal() +
  scale_color_manual(
    values = c("1 Year" = "#4DAF4A", "3 Years" = "#377EB8", "5 Years" = "#E41A1C"),
    labels = c(
      paste0("1 Year (AUC = ", sprintf("%.3f", time_roc_clust_test$AUC[1]), ")"),
      paste0("3 Years (AUC = ", sprintf("%.3f", time_roc_clust_test$AUC[2]), ")"),
      paste0("5 Years (AUC = ", sprintf("%.3f", time_roc_clust_test$AUC[3]), ")")
    )
  ) +
  labs(
    title = "Time-Dependent ROC Curves for Cluster Model (Test Data)",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Time Horizon"
  ) +
  theme(legend.position = "bottom")

print(roc_plot_test)


## === ROC Curve Data (Cluster Model - Train) ===
roc_curve_train <- data.frame(
  FP = c(time_roc_clust_train$FP[, 1], time_roc_clust_train$FP[, 2], time_roc_clust_train$FP[, 3]),
  TP = c(time_roc_clust_train$TP[, 1], time_roc_clust_train$TP[, 2], time_roc_clust_train$TP[, 3]),
  Horizon = factor(rep(c("1 Year", "3 Years", "5 Years"), each = nrow(time_roc_clust_train$FP)),
                   levels = c("1 Year", "3 Years", "5 Years"))
)

# Plot ROC Curves (Train)
roc_plot_train <- ggplot(roc_curve_train, aes(x = FP, y = TP, color = Horizon)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray") +
  theme_minimal() +
  scale_color_manual(
    values = c("1 Year" = "#4DAF4A", "3 Years" = "#377EB8", "5 Years" = "#E41A1C"),
    labels = c(
      paste0("1 Year (AUC = ", sprintf("%.3f", time_roc_clust_train$AUC[1]), ")"),
      paste0("3 Years (AUC = ", sprintf("%.3f", time_roc_clust_train$AUC[2]), ")"),
      paste0("5 Years (AUC = ", sprintf("%.3f", time_roc_clust_train$AUC[3]), ")")
    )
  ) +
  labs(
    title = "Time-Dependent ROC Curves for Cluster Model (Train Data)",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "Time Horizon"
  ) +
  theme(legend.position = "bottom")

print(roc_plot_train)

## === KM plot (Test) ===
test_data$Cluster <- as.factor(test_data$Cluster)

km_fit_test <- survfit(Surv(follow_up_days, comp_outcome) ~ Cluster, data = test_data)

survival_plot_test <- ggsurvplot(
  km_fit_test,
  data = test_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = "jco",
  title = "Kaplan-Meier Survival Curve by Cluster (Test Data)",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "Cluster"
)

print(survival_plot_test)


## === KM plot (Train) ===
train_data$Cluster <- as.factor(train_data$Cluster)

km_fit_train <- survfit(Surv(follow_up_days, comp_outcome) ~ Cluster, data = train_data)

survival_plot_train <- ggsurvplot(
  km_fit_train,
  data = train_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  palette = "jco",
  title = "Kaplan-Meier Survival Curve by Cluster (Train Data)",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "Cluster"
)

print(survival_plot_train)
