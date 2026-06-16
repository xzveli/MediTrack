# 04_model_development.R
# Phase 4B: Corrected Model Development for MediTrack
# Uses downsampling to reduce class imbalance

library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(rpart)
library(tibble)
library(ggplot2)

dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# -----------------------------
# 1. Load Cleaned Dataset
# -----------------------------

readmission_data <- read_csv(
  "data/processed/cleaned_hospital_readmission_2026.csv",
  show_col_types = FALSE
)

# -----------------------------
# 2. Prepare Target Variable
# -----------------------------

readmission_data <- readmission_data |>
  mutate(
    target = if_else(
      readmitted_within_30_days == 1,
      "Readmitted",
      "Not_Readmitted"
    ),
    target = factor(
      target,
      levels = c("Not_Readmitted", "Readmitted")
    ),
    gender = as.factor(gender),
    insurance_type = as.factor(insurance_type),
    admission_type = as.factor(admission_type),
    primary_diagnosis_group = as.factor(primary_diagnosis_group),
    icu_stay_flag = as.factor(icu_stay_flag),
    high_risk_medication_flag = as.factor(high_risk_medication_flag),
    followup_appointment_scheduled = as.factor(followup_appointment_scheduled),
    discharge_disposition = as.factor(discharge_disposition)
  )

cat("\nClass Distribution Before Balancing:\n")
print(table(readmission_data$target))

# -----------------------------
# 3. Select Modeling Variables
# -----------------------------

model_data <- readmission_data |>
  select(
    target,
    age,
    gender,
    insurance_type,
    socioeconomic_risk_score,
    previous_admissions_6m,
    previous_readmissions_1y,
    time_since_last_discharge,
    length_of_stay,
    admission_type,
    primary_diagnosis_group,
    comorbidity_index,
    chronic_disease_count,
    icu_stay_flag,
    severity_score,
    hb_a1c_level,
    creatinine_level,
    hemoglobin_level,
    average_systolic_bp,
    number_of_medications,
    medication_change_count,
    high_risk_medication_flag,
    followup_appointment_scheduled,
    discharge_disposition,
    medication_adherence_score
  ) |>
  drop_na()

# -----------------------------
# 4. Train-Test Split
# -----------------------------

train_index <- createDataPartition(
  model_data$target,
  p = 0.70,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTraining Set Class Distribution Before Balancing:\n")
print(table(train_data$target))

cat("\nTesting Set Class Distribution:\n")
print(table(test_data$target))

# -----------------------------
# 5. Balance Training Data Using Downsampling
# -----------------------------

train_balanced <- downSample(
  x = train_data[, setdiff(names(train_data), "target")],
  y = train_data$target
)

names(train_balanced)[ncol(train_balanced)] <- "target"

train_balanced$target <- factor(
  train_balanced$target,
  levels = c("Not_Readmitted", "Readmitted")
)

cat("\nTraining Set Class Distribution After Downsampling:\n")
print(table(train_balanced$target))

# -----------------------------
# 6. Logistic Regression Model
# -----------------------------

cat("\nLOGISTIC REGRESSION\n")

log_model <- glm(
  target ~ .,
  data = train_balanced,
  family = binomial
)

log_prob <- predict(
  log_model,
  newdata = test_data,
  type = "response"
)

log_pred <- if_else(
  log_prob > 0.50,
  "Readmitted",
  "Not_Readmitted"
)

log_pred <- factor(
  log_pred,
  levels = levels(test_data$target)
)

log_cm <- confusionMatrix(
  log_pred,
  test_data$target,
  positive = "Readmitted"
)

print(log_cm)

# -----------------------------
# 7. Decision Tree Model
# -----------------------------

cat("\nDECISION TREE\n")

tree_model <- rpart(
  target ~ .,
  data = train_balanced,
  method = "class",
  control = rpart.control(
    cp = 0.005,
    maxdepth = 6
  )
)

tree_pred <- predict(
  tree_model,
  newdata = test_data,
  type = "class"
)

tree_pred <- factor(
  tree_pred,
  levels = levels(test_data$target)
)

tree_cm <- confusionMatrix(
  tree_pred,
  test_data$target,
  positive = "Readmitted"
)

print(tree_cm)

# -----------------------------
# 8. Random Forest Model
# -----------------------------

cat("\nRANDOM FOREST\n")

rf_model <- randomForest(
  target ~ .,
  data = train_balanced,
  ntree = 400,
  mtry = floor(sqrt(ncol(train_balanced) - 1)),
  importance = TRUE
)

rf_pred <- predict(
  rf_model,
  newdata = test_data,
  type = "class"
)

rf_pred <- factor(
  rf_pred,
  levels = levels(test_data$target)
)

rf_cm <- confusionMatrix(
  rf_pred,
  test_data$target,
  positive = "Readmitted"
)

print(rf_cm)

# -----------------------------
# 9. Save Models
# -----------------------------

saveRDS(log_model, "models/logistic_regression_model.rds")
saveRDS(tree_model, "models/decision_tree_model.rds")
saveRDS(rf_model, "models/random_forest_model.rds")

# -----------------------------
# 10. Create Model Performance Summary
# -----------------------------

get_metrics <- function(cm) {
  
  accuracy <- as.numeric(cm$overall["Accuracy"])
  kappa <- as.numeric(cm$overall["Kappa"])
  sensitivity <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])
  precision <- as.numeric(cm$byClass["Precision"])
  recall <- as.numeric(cm$byClass["Recall"])
  f1 <- as.numeric(cm$byClass["F1"])
  
  tibble(
    accuracy = accuracy,
    kappa = kappa,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    f1_score = f1
  )
}

model_performance <- bind_rows(
  get_metrics(log_cm) |> mutate(model = "Logistic Regression", .before = 1),
  get_metrics(tree_cm) |> mutate(model = "Decision Tree", .before = 1),
  get_metrics(rf_cm) |> mutate(model = "Random Forest", .before = 1)
)

write_csv(
  model_performance,
  "outputs/tables/model_performance_summary.csv"
)

cat("\nMODEL PERFORMANCE SUMMARY\n")
print(model_performance)

# -----------------------------
# 11. Random Forest Feature Importance
# -----------------------------

rf_importance_raw <- importance(rf_model) |>
  as.data.frame()

rf_importance_raw$variable <- rownames(rf_importance_raw)

if ("MeanDecreaseGini" %in% names(rf_importance_raw)) {
  rf_importance_raw$Overall <- rf_importance_raw$MeanDecreaseGini
} else if ("MeanDecreaseAccuracy" %in% names(rf_importance_raw)) {
  rf_importance_raw$Overall <- rf_importance_raw$MeanDecreaseAccuracy
} else {
  numeric_cols <- sapply(rf_importance_raw, is.numeric)
  rf_importance_raw$Overall <- rowMeans(
    rf_importance_raw[, numeric_cols, drop = FALSE],
    na.rm = TRUE
  )
}

rf_importance <- rf_importance_raw |>
  arrange(desc(Overall)) |>
  select(variable, everything())

write_csv(
  rf_importance,
  "outputs/tables/random_forest_feature_importance.csv"
)

cat("\nTOP 10 RANDOM FOREST IMPORTANT FEATURES\n")
print(head(rf_importance, 10))

top_features <- rf_importance |>
  slice_max(order_by = Overall, n = 10)

p_importance <- ggplot(
  top_features,
  aes(
    x = reorder(variable, Overall),
    y = Overall
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Important Features for Readmission Prediction",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/09_random_forest_feature_importance.png",
  p_importance,
  width = 9,
  height = 6
)

# -----------------------------
# 12. Prediction Output for Dashboard
# -----------------------------

rf_prob <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)

prediction_output <- test_data |>
  mutate(
    actual_class = target,
    predicted_class = rf_pred,
    predicted_probability = rf_prob[, "Readmitted"],
    risk_level = case_when(
      predicted_probability < 0.30 ~ "Low Risk",
      predicted_probability <= 0.70 ~ "Medium Risk",
      predicted_probability > 0.70 ~ "High Risk"
    )
  )

write_csv(
  prediction_output,
  "data/processed/model_prediction_output.csv"
)

risk_distribution <- prediction_output |>
  count(risk_level) |>
  mutate(
    percent = round(n / sum(n) * 100, 2)
  )

write_csv(
  risk_distribution,
  "outputs/tables/risk_level_distribution.csv"
)

cat("\nRISK LEVEL DISTRIBUTION\n")
print(risk_distribution)

p_risk <- ggplot(
  prediction_output,
  aes(x = predicted_probability)
) +
  geom_histogram(bins = 30) +
  labs(
    title = "Predicted Readmission Risk Distribution",
    x = "Predicted Probability",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "outputs/figures/10_predicted_risk_distribution.png",
  p_risk,
  width = 8,
  height = 5
)

# -----------------------------
# 13. Save Confusion Matrices
# -----------------------------

capture.output(
  log_cm,
  file = "outputs/tables/logistic_regression_confusion_matrix.txt"
)

capture.output(
  tree_cm,
  file = "outputs/tables/decision_tree_confusion_matrix.txt"
)

capture.output(
  rf_cm,
  file = "outputs/tables/random_forest_confusion_matrix.txt"
)

cat("\nModel development completed successfully.\n")