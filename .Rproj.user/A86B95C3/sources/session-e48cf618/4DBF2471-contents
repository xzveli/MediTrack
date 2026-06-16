# 03_eda_analysis.R
# Phase 4A: Exploratory Data Analysis for MediTrack

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Load Cleaned Dataset
# -----------------------------

readmission_data <- read_csv(
  "data/processed/cleaned_hospital_readmission_2026.csv",
  show_col_types = FALSE
)

# -----------------------------
# 2. Readmission Distribution
# -----------------------------

readmission_summary <- readmission_data |>
  count(readmission_category) |>
  mutate(percent = round(n / sum(n) * 100, 2))

write_csv(
  readmission_summary,
  "outputs/tables/eda_readmission_summary.csv"
)

p1 <- ggplot(readmission_data,
             aes(x = readmission_category)) +
  geom_bar() +
  labs(
    title = "Distribution of Patient Readmissions",
    x = "Readmission Status",
    y = "Number of Patients"
  )

ggsave(
  "outputs/figures/01_readmission_distribution.png",
  p1,
  width = 8,
  height = 5
)

# -----------------------------
# 3. Age Distribution
# -----------------------------

p2 <- ggplot(readmission_data,
             aes(x = age)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Age Distribution of Patients",
    x = "Age",
    y = "Number of Patients"
  )

ggsave(
  "outputs/figures/02_age_distribution.png",
  p2,
  width = 8,
  height = 5
)

# -----------------------------
# 4. Readmission by Gender
# -----------------------------

p3 <- ggplot(readmission_data,
             aes(x = gender, fill = readmission_category)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Patient Readmission by Gender",
    x = "Gender",
    y = "Number of Patients",
    fill = "Readmission Status"
  )

ggsave(
  "outputs/figures/03_readmission_by_gender.png",
  p3,
  width = 8,
  height = 5
)

# -----------------------------
# 5. Readmission by Insurance Type
# -----------------------------

p4 <- ggplot(readmission_data,
             aes(x = insurance_type, fill = readmission_category)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Readmission by Insurance Type",
    x = "Insurance Type",
    y = "Number of Patients",
    fill = "Readmission Status"
  )

ggsave(
  "outputs/figures/04_readmission_by_insurance.png",
  p4,
  width = 8,
  height = 5
)

# -----------------------------
# 6. Diagnosis Group Distribution
# -----------------------------

p5 <- ggplot(readmission_data,
             aes(x = primary_diagnosis_group)) +
  geom_bar() +
  labs(
    title = "Distribution of Diagnosis Groups",
    x = "Primary Diagnosis Group",
    y = "Number of Patients"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/05_diagnosis_group_distribution.png",
  p5,
  width = 9,
  height = 5
)

# -----------------------------
# 7. Readmission by Diagnosis Group
# -----------------------------

p6 <- ggplot(readmission_data,
             aes(x = primary_diagnosis_group, fill = readmission_category)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Readmission by Diagnosis Group",
    x = "Primary Diagnosis Group",
    y = "Number of Patients",
    fill = "Readmission Status"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "outputs/figures/06_readmission_by_diagnosis.png",
  p6,
  width = 10,
  height = 6
)

# -----------------------------
# 8. Severity Score by Readmission
# -----------------------------

p7 <- ggplot(readmission_data,
             aes(x = readmission_category, y = severity_score)) +
  geom_boxplot() +
  labs(
    title = "Severity Score by Readmission Status",
    x = "Readmission Status",
    y = "Severity Score"
  )

ggsave(
  "outputs/figures/07_severity_score_by_readmission.png",
  p7,
  width = 8,
  height = 5
)

# -----------------------------
# 9. Previous Readmissions by Current Readmission
# -----------------------------

p8 <- ggplot(readmission_data,
             aes(x = readmission_category, y = previous_readmissions_1y)) +
  geom_boxplot() +
  labs(
    title = "Previous Readmissions by Current Readmission Status",
    x = "Readmission Status",
    y = "Previous Readmissions in 1 Year"
  )

ggsave(
  "outputs/figures/08_previous_readmissions_by_status.png",
  p8,
  width = 8,
  height = 5
)

# -----------------------------
# 10. Numeric Summary Table
# -----------------------------

numeric_summary <- readmission_data |>
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_length_of_stay = mean(length_of_stay, na.rm = TRUE),
    mean_severity_score = mean(severity_score, na.rm = TRUE),
    mean_chronic_disease_count = mean(chronic_disease_count, na.rm = TRUE),
    mean_number_of_medications = mean(number_of_medications, na.rm = TRUE),
    mean_medication_adherence_score = mean(medication_adherence_score, na.rm = TRUE)
  )

write_csv(
  numeric_summary,
  "outputs/tables/eda_numeric_summary.csv"
)

# -----------------------------
# 11. Print Results
# -----------------------------

print(readmission_summary)
print(numeric_summary)

cat("EDA charts and tables created successfully.\n")