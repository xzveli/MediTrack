# 02_cleaning_preprocessing.R
# Phase 3: Data Cleaning and Preprocessing for MediTrack

library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(tibble)

# Create output folders
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Load Raw Dataset
# -----------------------------

raw_file <- "data/raw/hospital_readmission_risk_dataset_2026_v1_18000rows.csv"

if (!file.exists(raw_file)) {
  stop("Raw dataset not found. Please check data/raw folder.")
}

readmission_raw <- read_csv(
  raw_file,
  na = c("", "NA", "N/A", "?", "Unknown", "unknown"),
  show_col_types = FALSE
)

initial_rows <- nrow(readmission_raw)
initial_cols <- ncol(readmission_raw)

cat("Initial rows:", initial_rows, "\n")
cat("Initial columns:", initial_cols, "\n")

# -----------------------------
# 2. Standardize Column Names
# -----------------------------

readmission_clean <- readmission_raw |>
  clean_names()

after_names_rows <- nrow(readmission_clean)
after_names_cols <- ncol(readmission_clean)

# View column names
print(names(readmission_clean))

# -----------------------------
# 3. Missing Value Summary
# -----------------------------

missing_summary <- readmission_clean |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) |>
  mutate(
    missing_percent = round((missing_count / nrow(readmission_clean)) * 100, 2)
  ) |>
  arrange(desc(missing_count))

write_csv(
  missing_summary,
  "outputs/tables/missing_value_summary.csv"
)

# -----------------------------
# 4. Remove Duplicate Records
# -----------------------------

rows_before_duplicates <- nrow(readmission_clean)

readmission_clean <- readmission_clean |>
  distinct()

rows_after_duplicates <- nrow(readmission_clean)
duplicates_removed <- rows_before_duplicates - rows_after_duplicates

cat("Duplicates removed:", duplicates_removed, "\n")

# -----------------------------
# 5. Convert Data Types
# -----------------------------

readmission_clean <- readmission_clean |>
  mutate(
    gender = as.factor(gender),
    insurance_type = as.factor(insurance_type),
    admission_type = as.factor(admission_type),
    primary_diagnosis_group = as.factor(primary_diagnosis_group),
    icu_stay_flag = as.factor(icu_stay_flag),
    high_risk_medication_flag = as.factor(high_risk_medication_flag),
    followup_appointment_scheduled = as.factor(followup_appointment_scheduled),
    discharge_disposition = as.factor(discharge_disposition),
    readmitted_within_30_days = as.factor(readmitted_within_30_days)
  )

# -----------------------------
# 6. Create Readmission Category
# -----------------------------

readmission_clean <- readmission_clean |>
  mutate(
    readmission_category = if_else(
      readmitted_within_30_days == 1,
      "Readmitted",
      "Not Readmitted"
    ),
    readmission_category = as.factor(readmission_category)
  )

# -----------------------------
# 7. Create Data Type Summary
# -----------------------------

data_type_summary <- tibble(
  variable = names(readmission_clean),
  data_type = sapply(readmission_clean, class)
)

write_csv(
  data_type_summary,
  "outputs/tables/data_type_summary.csv"
)

# -----------------------------
# 8. Create Target Distribution Table
# -----------------------------

readmission_distribution <- readmission_clean |>
  count(readmission_category) |>
  mutate(
    percent = round((n / sum(n)) * 100, 2)
  )

write_csv(
  readmission_distribution,
  "outputs/tables/readmission_distribution.csv"
)

# -----------------------------
# 9. Final Dataset Size
# -----------------------------

final_rows <- nrow(readmission_clean)
final_cols <- ncol(readmission_clean)

cat("Final rows:", final_rows, "\n")
cat("Final columns:", final_cols, "\n")

# -----------------------------
# 10. Cleaning Audit Table
# -----------------------------

cleaning_audit <- tibble(
  cleaning_step = c(
    "Raw dataset imported",
    "Column names standardized",
    "Missing values checked",
    "Duplicate records removed",
    "Data types converted",
    "Readmission category created",
    "Final cleaned dataset saved"
  ),
  rows_after_step = c(
    initial_rows,
    after_names_rows,
    after_names_rows,
    rows_after_duplicates,
    final_rows,
    final_rows,
    final_rows
  ),
  columns_after_step = c(
    initial_cols,
    after_names_cols,
    after_names_cols,
    ncol(readmission_clean),
    final_cols,
    final_cols,
    final_cols
  ),
  remarks = c(
    "Original CSV file loaded from data/raw folder",
    "Column names converted to snake_case format",
    "Missing value summary generated",
    paste("Duplicate records removed:", duplicates_removed),
    "Categorical variables converted to factor type",
    "Readmission category added for clearer analysis",
    "Cleaned dataset exported to data/processed folder"
  )
)

write_csv(
  cleaning_audit,
  "outputs/tables/cleaning_audit.csv"
)

# -----------------------------
# 11. Save Cleaned Dataset
# -----------------------------

write_csv(
  readmission_clean,
  "data/processed/cleaned_hospital_readmission_2026.csv"
)

# -----------------------------
# 12. Print Results
# -----------------------------

print(cleaning_audit)
print(readmission_distribution)
print(head(missing_summary, 10))