library(here)
library(tidyverse)
library(survival)
library(survminer)
library(timeROC)
library(lmtest)
library(gt)
library(kableExtra)
library(knitr)
library(stringr)
library(pec)

train_data <- readRDS(here("data", "analysis/train_clustered.rds"))
test_data  <- readRDS(here("data", "analysis/test_clustered.rds"))

## Survival Analysis
cox_base <- coxph(Surv(follow_up_days, comp_outcome) ~ demographics_age_index_visit + 
                    demographics_birth_sex, data = train_data, x = TRUE)

cox_clust <- coxph(Surv(follow_up_days, comp_outcome) ~ demographics_age_index_visit + 
                     demographics_birth_sex + Cluster, data = train_data, x = TRUE)

# LRT
lrt_cox <- lrtest(cox_base, cox_clust)
lrt_cox

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


### ================ Generate Tables ===================

## --- TABLE 1: HORIZON PERFORMANCE METRICS ---

# 1. Calculate Time-Dependent C-index natively using the survival package
# The 'ymax' argument truncates the calculation to exactly 1, 3, and 5 years (in days)
c_indices <- c(
  concordance(cox_clust, newdata = test_data, ymax = 365)$concordance,
  concordance(cox_clust, newdata = test_data, ymax = 1095)$concordance,
  concordance(cox_clust, newdata = test_data, ymax = 1825)$concordance
)

perf_table <- data.frame(
  Horizon = c("1 year", "3 year", "5 year"),
  Evaluable_N = c(
    sum(test_data$follow_up_days >= 365 | test_data$comp_outcome == 1), 
    sum(test_data$follow_up_days >= 1095 | test_data$comp_outcome == 1), 
    sum(test_data$follow_up_days >= 1825 | test_data$comp_outcome == 1)
  ),
  Events = c(
    sum(test_data$comp_outcome == 1 & test_data$follow_up_days <= 365),
    sum(test_data$comp_outcome == 1 & test_data$follow_up_days <= 1095),
    sum(test_data$comp_outcome == 1 & test_data$follow_up_days <= 1825)
  ),
  AUC = c(time_roc_clust_test$AUC[1], time_roc_clust_test$AUC[2], time_roc_clust_test$AUC[3]),
  C_index = c_indices 
)

perf_table_formatted <- perf_table %>%
  mutate(
    Evaluable_N = formatC(Evaluable_N, format = "f", big.mark = ",", digits = 0),
    Events = formatC(Events, format = "f", big.mark = ",", digits = 0),
    AUC = sprintf("%.3f", AUC),
    C_index = sprintf("%.3f", C_index)
  )

table_horizon <- kable(perf_table_formatted, 
                       col.names = c("Horizon", "Evaluable N", "Events", "AUC", "C-index"),
                       align = c("l", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                full_width = FALSE, 
                position = "left") %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid #D3D3D3;")

print(table_horizon)

## --- TABLE 2: CLUSTER PROFILE (Training Set) ---

# 1. Calculate the cluster profile
cluster_profile <- train_data %>%
  group_by(Cluster) %>%
  summarise(
    N = n(),
    Mean_age = mean(demographics_age_index_visit, na.rm = TRUE),
    Female_pct = mean(demographics_birth_sex == "Female" | demographics_birth_sex == 2, na.rm = TRUE) * 100,
    Male_pct = mean(demographics_birth_sex == "Male" | demographics_birth_sex == 1, na.rm = TRUE) * 100,
    
    Overall_MACE = mean(comp_outcome == 1, na.rm = TRUE) * 100,
    MACE_1y = mean(comp_outcome == 1 & follow_up_days <= 365, na.rm = TRUE) * 100,
    MACE_3y = mean(comp_outcome == 1 & follow_up_days <= 1095, na.rm = TRUE) * 100,
    MACE_5y = mean(comp_outcome == 1 & follow_up_days <= 1825, na.rm = TRUE) * 100, # Added 5y horizon
    .groups = "drop"
  ) %>%
  mutate(Percent = (N / sum(N)) * 100) %>%
  relocate(Percent, .after = N)

cluster_profile_formatted <- cluster_profile %>%
  mutate(
    N = formatC(N, format = "f", big.mark = ",", digits = 0),
    Mean_age = sprintf("%.1f", Mean_age),
    Percent = paste0(sprintf("%.1f", Percent), "%"),
    Female_pct = paste0(sprintf("%.1f", Female_pct), "%"),
    Male_pct = paste0(sprintf("%.1f", Male_pct), "%"),
    Overall_MACE = paste0(sprintf("%.1f", Overall_MACE), "%"),
    MACE_1y = paste0(sprintf("%.1f", MACE_1y), "%"),
    MACE_3y = paste0(sprintf("%.1f", MACE_3y), "%"),
    MACE_5y = paste0(sprintf("%.1f", MACE_5y), "%")
  )

table_profile <- kable(cluster_profile_formatted, 
                       col.names = c("Cluster", "N", "Percent", "Mean age", "Female %", "Male %", 
                                     "Overall MACE", "1y MACE", "3y MACE", "5y MACE"),
                       caption = "<b>Table 1: Cluster Profile (Training Set)</b>",
                       align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r"),
                       escape = FALSE) %>% 
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                full_width = FALSE, 
                position = "left") %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid #D3D3D3;")

print(table_profile)


## --- TABLE 3: HAZARD RATIOS (Cox Model) ---

# 1. Extract and format the statistics using broom::tidy
hr_table <- tidy(cox_clust, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    term = case_when(
      term == "demographics_age_index_visit" ~ "Age (per year)",
      str_detect(term, "demographics_birth_sex") ~ "Sex (Female vs Male)", 
      term == "Cluster2" ~ "Cluster 2 vs 1",
      term == "Cluster3" ~ "Cluster 3 vs 1",
      term == "Cluster4" ~ "Cluster 4 vs 1",
      TRUE ~ term
    ),
    HR_CI = sprintf("%.2f (%.2f - %.2f)", estimate, conf.low, conf.high),
    P_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(term, HR_CI, P_value)

table_hr <- kable(hr_table, 
                  col.names = c("Clinical Variable", "Hazard Ratio (95% CI)", "P-Value"),
                  caption = "<b>Table 4: Multivariate Cox Proportional Hazards Model</b>",
                  align = c("l", "c", "r"),
                  escape = FALSE) %>%
  kable_styling(bootstrap_options = c("hover", "condensed"), 
                full_width = FALSE, 
                position = "left") %>%
  row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid #D3D3D3;") %>%
  add_indent(c(3, 4, 5)) 

print(table_hr)

