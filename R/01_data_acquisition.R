# 01_data_acquisition.R
# Phase 2: Data Acquisition for MediTrack

library(readr)
library(dplyr)
library(tibble)
library(httr2)
library(jsonlite)

# Create folders if they do not exist
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Load Main 2026 Dataset
# -----------------------------

main_file <- "data/raw/hospital_readmission_risk_dataset_2026_v1_18000rows.csv"

if (!file.exists(main_file)) {
  stop("Main dataset not found. Please check if the CSV is inside data/raw/.")
}

readmission_raw <- read_csv(
  main_file,
  na = c("", "NA", "N/A", "?", "Unknown", "unknown"),
  show_col_types = FALSE
)

main_summary <- tibble(
  dataset_name = "Hospital Readmission Risk Dataset 2026",
  file_name = "hospital_readmission_risk_dataset_2026_v1_18000rows.csv",
  source_type = "Public Kaggle CSV dataset",
  rows = nrow(readmission_raw),
  columns = ncol(readmission_raw),
  date_loaded = as.character(Sys.Date())
)

write_csv(main_summary, "outputs/tables/main_dataset_summary.csv")

print(main_summary)

# -----------------------------
# 2. Load CDC Static Backup CSV
# -----------------------------

cdc_backup_file <- "data/raw/U.S._Chronic_Disease_Indicators.csv"

if (!file.exists(cdc_backup_file)) {
  warning("CDC backup CSV not found. The live API will still be attempted.")
} else {
  cdc_backup <- read_csv(
    cdc_backup_file,
    show_col_types = FALSE
  )
  
  cdc_backup_summary <- tibble(
    dataset_name = "U.S. Chronic Disease Indicators",
    file_name = "U.S._Chronic_Disease_Indicators.csv",
    source_type = "CDC static CSV backup",
    rows = nrow(cdc_backup),
    columns = ncol(cdc_backup),
    date_loaded = as.character(Sys.Date())
  )
  
  write_csv(cdc_backup_summary, "outputs/tables/cdc_backup_summary.csv")
  
  print(cdc_backup_summary)
}

# -----------------------------
# 3. Download Live CDC API Data
# -----------------------------

get_cdc_api_data <- function(topic_choice = "Diabetes",
                             location_choice = "United States",
                             limit = 5000) {
  
  endpoint <- "https://data.cdc.gov/resource/hksd-2xuw.json"
  
  where_clause <- paste0(
    "topic='", topic_choice, "'",
    " AND locationdesc='", location_choice, "'",
    " AND datavalue IS NOT NULL"
  )
  
  request(endpoint) |>
    req_url_query(
      `$limit` = limit,
      `$select` = "yearstart, locationdesc, topic, question, datavalue, stratificationcategory1, stratification1",
      `$where` = where_clause,
      `$order` = "yearstart"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    as_tibble()
}

cdc_api_live <- tryCatch({
  get_cdc_api_data(
    topic_choice = "Diabetes",
    location_choice = "United States",
    limit = 5000
  )
}, error = function(e) {
  message("CDC API request failed: ", e$message)
  tibble()
})

write_csv(cdc_api_live, "data/raw/cdc_api_live_diabetes_raw.csv")

cdc_api_summary <- tibble(
  dataset_name = "CDC Chronic Disease Indicators Live API",
  api_endpoint = "https://data.cdc.gov/resource/hksd-2xuw.json",
  topic_requested = "Diabetes",
  location_requested = "United States",
  rows = nrow(cdc_api_live),
  columns = ncol(cdc_api_live),
  acquisition_method = "Live API request using httr2",
  error_handling = "tryCatch used to prevent script failure",
  date_loaded = as.character(Sys.Date())
)

write_csv(cdc_api_summary, "outputs/tables/cdc_api_summary.csv")

print(cdc_api_summary)

# -----------------------------
# 4. Preview Data
# -----------------------------

cat("\nMain dataset rows:", nrow(readmission_raw), "\n")
cat("Main dataset columns:", ncol(readmission_raw), "\n")
cat("CDC API rows:", nrow(cdc_api_live), "\n")
cat("CDC API columns:", ncol(cdc_api_live), "\n")