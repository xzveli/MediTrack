# app.R
# MediTrack Dashboard - Data-Validated Final Version

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(httr2)
library(jsonlite)

# ------------------------------------------------------------
# 1. File Paths
# ------------------------------------------------------------

readmission_path <- "data/processed/cleaned_hospital_readmission_2026.csv"
prediction_path <- "data/processed/model_prediction_output.csv"
performance_path <- "outputs/tables/model_performance_summary.csv"
importance_path <- "outputs/tables/random_forest_feature_importance.csv"

required_files <- c(
  readmission_path,
  prediction_path,
  performance_path,
  importance_path
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing required file(s):",
      paste(missing_files, collapse = ", "),
      "\nPlease rerun the previous phase scripts before opening the dashboard."
    )
  )
}

# ------------------------------------------------------------
# 2. Load Data
# ------------------------------------------------------------

readmission_data <- read_csv(readmission_path, show_col_types = FALSE)
model_predictions <- read_csv(prediction_path, show_col_types = FALSE)
model_performance <- read_csv(performance_path, show_col_types = FALSE)
feature_importance <- read_csv(importance_path, show_col_types = FALSE)

# ------------------------------------------------------------
# 3. Data Validation
# ------------------------------------------------------------

required_readmission_columns <- c(
  "age",
  "gender",
  "insurance_type",
  "primary_diagnosis_group",
  "severity_score",
  "number_of_medications",
  "chronic_disease_count",
  "previous_readmissions_1y",
  "medication_adherence_score"
)

missing_readmission_columns <- setdiff(
  required_readmission_columns,
  names(readmission_data)
)

if (length(missing_readmission_columns) > 0) {
  stop(
    paste(
      "The cleaned dataset is missing required column(s):",
      paste(missing_readmission_columns, collapse = ", ")
    )
  )
}

if (!"readmission_category" %in% names(readmission_data)) {
  
  if (!"readmitted_within_30_days" %in% names(readmission_data)) {
    stop("The dataset must contain either readmission_category or readmitted_within_30_days.")
  }
  
  readmission_data <- readmission_data |>
    mutate(
      readmission_category = if_else(
        as.character(readmitted_within_30_days) %in% c("1", "Readmitted"),
        "Readmitted",
        "Not Readmitted"
      )
    )
}

if (!"risk_level" %in% names(model_predictions)) {
  stop("The model prediction file must contain a risk_level column.")
}

if (!all(c("model", "accuracy", "recall", "f1_score") %in% names(model_performance))) {
  stop("The model performance file must contain model, accuracy, recall, and f1_score columns.")
}

if (!"Overall" %in% names(feature_importance)) {
  if ("overall" %in% names(feature_importance)) {
    feature_importance <- feature_importance |>
      rename(Overall = overall)
  } else {
    numeric_cols <- names(feature_importance)[sapply(feature_importance, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      stop("Feature importance file must contain an Overall column or at least one numeric importance column.")
    }
    
    feature_importance$Overall <- rowMeans(
      feature_importance[, numeric_cols, drop = FALSE],
      na.rm = TRUE
    )
  }
}

if (!"variable" %in% names(feature_importance)) {
  stop("Feature importance file must contain a variable column.")
}

# ------------------------------------------------------------
# 4. Prepare Data
# ------------------------------------------------------------

readmission_data <- readmission_data |>
  mutate(
    gender = as.character(gender),
    insurance_type = as.character(insurance_type),
    primary_diagnosis_group = as.character(primary_diagnosis_group),
    readmission_category = as.character(readmission_category),
    readmission_category = if_else(
      readmission_category %in% c("Readmitted", "1"),
      "Readmitted",
      "Not Readmitted"
    ),
    readmission_category = factor(
      readmission_category,
      levels = c("Readmitted", "Not Readmitted")
    ),
    readmitted_flag = if_else(readmission_category == "Readmitted", 1, 0)
  )

model_predictions <- model_predictions |>
  mutate(
    risk_level = as.character(risk_level),
    risk_level = factor(
      risk_level,
      levels = c("Low Risk", "Medium Risk", "High Risk")
    )
  )

if ("gender" %in% names(model_predictions)) {
  model_predictions <- model_predictions |>
    mutate(gender = as.character(gender))
}

if ("insurance_type" %in% names(model_predictions)) {
  model_predictions <- model_predictions |>
    mutate(insurance_type = as.character(insurance_type))
}

if ("primary_diagnosis_group" %in% names(model_predictions)) {
  model_predictions <- model_predictions |>
    mutate(primary_diagnosis_group = as.character(primary_diagnosis_group))
}

feature_importance <- feature_importance |>
  mutate(Overall = as.numeric(Overall))

model_performance_display <- model_performance |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

make_choices <- function(x) {
  c("All", sort(unique(na.omit(as.character(x)))))
}

app_date <- format(Sys.time(), "%B %d, %Y %I:%M %p")

dashboard_audit <- tibble(
  item = c(
    "Cleaned patient records",
    "Model prediction output records",
    "Model performance rows",
    "Feature importance rows",
    "Dashboard generated"
  ),
  value = c(
    as.character(nrow(readmission_data)),
    as.character(nrow(model_predictions)),
    as.character(nrow(model_performance)),
    as.character(nrow(feature_importance)),
    app_date
  )
)

# ------------------------------------------------------------
# 5. CDC API Functions
# ------------------------------------------------------------

empty_cdc_data <- function() {
  tibble(
    yearstart = integer(),
    locationdesc = character(),
    topic = character(),
    question = character(),
    datavalue = numeric(),
    stratificationcategory1 = character(),
    stratification1 = character()
  )
}

get_cdc_api_data <- function(topic_choice = "Diabetes",
                             location_choice = "United States",
                             limit = 5000) {
  
  endpoint <- "https://data.cdc.gov/resource/hksd-2xuw.json"
  
  where_clause <- paste0(
    "topic='", topic_choice, "'",
    " AND locationdesc='", location_choice, "'",
    " AND datavalue IS NOT NULL"
  )
  
  raw_response <- request(endpoint) |>
    req_url_query(
      `$limit` = limit,
      `$select` = "yearstart, locationdesc, topic, question, datavalue, stratificationcategory1, stratification1",
      `$where` = where_clause,
      `$order` = "yearstart"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  cdc_data <- as_tibble(raw_response)
  
  if (nrow(cdc_data) == 0) {
    return(empty_cdc_data())
  }
  
  cdc_data |>
    mutate(
      yearstart = as.integer(yearstart),
      datavalue = as.numeric(datavalue)
    )
}

get_cdc_backup <- function() {
  
  backup_path <- "data/raw/cdc_api_live_diabetes_raw.csv"
  
  if (!file.exists(backup_path)) {
    return(empty_cdc_data())
  }
  
  read_csv(backup_path, show_col_types = FALSE) |>
    mutate(
      yearstart = as.integer(yearstart),
      datavalue = as.numeric(datavalue)
    )
}

# ------------------------------------------------------------
# 6. UI
# ------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "MediTrack"),
  
  dashboardSidebar(
    
    tags$div(
      class = "sidebar-brand",
      tags$div(class = "brand-icon", icon("heartbeat")),
      tags$div(
        tags$div(class = "brand-main", "MediTrack"),
        tags$div(class = "brand-sub", "Dashboard")
      )
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Risk Factors", tabName = "risk", icon = icon("heartbeat")),
      menuItem("Model Results", tabName = "model", icon = icon("line-chart")),
      menuItem("CDC Live API", tabName = "cdc", icon = icon("globe")),
      menuItem("Patient Explorer", tabName = "patients", icon = icon("table"))
    ),
    
    tags$hr(class = "sidebar-line"),
    
    tags$div(class = "filter-title", "Filters"),
    
    sliderInput(
      "age_range",
      "Age Range",
      min = min(readmission_data$age, na.rm = TRUE),
      max = max(readmission_data$age, na.rm = TRUE),
      value = c(
        min(readmission_data$age, na.rm = TRUE),
        max(readmission_data$age, na.rm = TRUE)
      )
    ),
    
    selectInput(
      "gender_filter",
      "Gender",
      choices = make_choices(readmission_data$gender),
      selected = "All"
    ),
    
    selectInput(
      "insurance_filter",
      "Insurance Type",
      choices = make_choices(readmission_data$insurance_type),
      selected = "All"
    ),
    
    selectInput(
      "diagnosis_filter",
      "Diagnosis Group",
      choices = make_choices(readmission_data$primary_diagnosis_group),
      selected = "All"
    ),
    
    actionButton(
      "reset_filters",
      "Reset Filters",
      icon = icon("refresh"),
      class = "reset-btn"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        body, .content-wrapper, .right-side {
          background-color: #eef2f7;
          font-family: 'Segoe UI', Arial, sans-serif;
        }

        .main-header .logo {
          background-color: #ffffff !important;
          color: #111827 !important;
          font-weight: 800;
          border-bottom: 1px solid #e5e7eb;
        }

        .main-header .navbar {
          background-color: #ffffff !important;
          border-bottom: 1px solid #e5e7eb;
        }

        .main-sidebar {
          background: linear-gradient(180deg, #071426 0%, #0d2138 100%) !important;
          color: #ffffff;
        }

        .sidebar-menu > li > a {
          color: #dce7f7 !important;
          font-size: 15px;
          padding-top: 14px;
          padding-bottom: 14px;
        }

        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, #2563eb, #1d4ed8) !important;
          color: #ffffff !important;
          border-left-color: #60a5fa !important;
          border-radius: 8px;
          margin: 4px 12px;
        }

        .sidebar-menu > li > a:hover {
          background-color: rgba(255,255,255,0.08) !important;
          border-radius: 8px;
          margin: 4px 12px;
        }

        .sidebar-brand {
          display: flex;
          align-items: center;
          gap: 10px;
          padding: 20px 18px 18px 18px;
          color: white;
        }

        .brand-icon {
          font-size: 30px;
          color: #3b82f6;
        }

        .brand-main {
          font-size: 24px;
          font-weight: 800;
          line-height: 1;
        }

        .brand-sub {
          font-size: 14px;
          color: #cbd5e1;
          margin-top: 4px;
        }

        .sidebar-line {
          border-top: 1px solid rgba(255,255,255,0.18);
          margin: 18px 18px;
        }

        .filter-title {
          color: #ffffff;
          font-size: 19px;
          font-weight: 800;
          padding: 0 18px 10px 18px;
        }

        .sidebar label {
          color: #e5e7eb !important;
          font-weight: 600;
          padding-left: 8px;
        }

        .sidebar .form-group {
          padding: 0 14px;
        }

        .sidebar .selectize-input,
        .sidebar .form-control {
          background-color: #10253d !important;
          color: #ffffff !important;
          border: 1px solid #334155 !important;
          border-radius: 7px !important;
        }

        .reset-btn {
          margin: 10px 18px 20px 18px;
          width: calc(100% - 36px);
          background-color: transparent !important;
          color: #e5e7eb !important;
          border: 1px solid #475569 !important;
          border-radius: 8px !important;
          padding: 10px !important;
          font-weight: 600;
        }

        .reset-btn:hover {
          background-color: #1e3a5f !important;
          color: #ffffff !important;
        }

        .dark-heading {
          background: linear-gradient(135deg, #102a56, #071426);
          border-radius: 14px;
          padding: 22px 26px;
          margin-bottom: 18px;
          color: #ffffff;
          box-shadow: 0 3px 14px rgba(15, 23, 42, 0.18);
        }

        .page-title-dark {
          font-size: 30px;
          font-weight: 850;
          color: #ffffff;
          margin: 0;
        }

        .page-subtitle-dark {
          font-size: 15px;
          color: #bfdbfe;
          margin-top: 5px;
        }

        .right-text {
          text-align: right;
          font-size: 13px;
          color: #dbeafe;
        }

        .dark-card {
          background: linear-gradient(180deg, #102a56 0%, #071426 100%);
          border-radius: 16px;
          border: 1px solid rgba(96, 165, 250, 0.25);
          box-shadow: 0 5px 18px rgba(15, 23, 42, 0.25);
          padding: 18px;
          margin-bottom: 18px;
          min-height: 360px;
          color: #ffffff;
        }

        .dark-card-title {
          font-size: 18px;
          font-weight: 800;
          color: #ffffff;
          margin-bottom: 4px;
        }

        .dark-card-subtitle {
          font-size: 13px;
          color: #bfdbfe;
          margin-bottom: 10px;
        }

        .kpi-card, .mini-card {
          background: linear-gradient(135deg, #102a56, #071426);
          border-radius: 16px;
          padding: 22px;
          min-height: 112px;
          border: 1px solid rgba(96, 165, 250, 0.25);
          box-shadow: 0 5px 18px rgba(15, 23, 42, 0.22);
          display: flex;
          align-items: center;
          gap: 18px;
          margin-bottom: 18px;
          color: #ffffff;
        }

        .mini-card {
          min-height: 95px;
          padding: 18px;
        }

        .kpi-icon, .mini-icon {
          width: 64px;
          height: 64px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 26px;
          background: rgba(56, 189, 248, 0.18);
          color: #38bdf8;
        }

        .mini-icon {
          width: 50px;
          height: 50px;
          font-size: 20px;
        }

        .kpi-label, .mini-label {
          font-size: 16px;
          font-weight: 700;
          color: #bfdbfe;
          margin-bottom: 6px;
        }

        .mini-label {
          font-size: 13px;
        }

        .kpi-value, .mini-value {
          font-size: 34px;
          font-weight: 850;
          line-height: 1;
          color: #ffffff;
        }

        .mini-value {
          font-size: 25px;
        }

        .live-status {
          font-size: 12px;
          color: #dbeafe;
          margin-top: 8px;
          margin-bottom: 8px;
        }

        .progress-label {
          color: #ffffff;
          font-weight: 700;
          margin-top: 10px;
          margin-bottom: 4px;
        }

        .progress-value {
          float: right;
          color: #93c5fd;
        }

        .progress {
          background-color: rgba(255,255,255,0.14);
          height: 14px;
          border-radius: 999px;
          margin-bottom: 2px;
        }

        .progress-bar {
          border-radius: 999px;
        }

        .dark-card .form-control,
        .dark-card .selectize-input {
          background-color: #10253d !important;
          color: #ffffff !important;
          border: 1px solid #334155 !important;
          border-radius: 7px !important;
        }

        .dark-card label {
          color: #dbeafe !important;
        }

        .dark-card table.dataTable {
          color: #e5e7eb !important;
          background-color: transparent !important;
        }

        .dark-card table.dataTable thead th {
          color: #ffffff !important;
          border-bottom: 1px solid rgba(255,255,255,0.25) !important;
        }

        .dark-card table.dataTable tbody td {
          color: #e5e7eb !important;
          background-color: rgba(255,255,255,0.03) !important;
          border-top: 1px solid rgba(255,255,255,0.08) !important;
        }

        .dark-card .dataTables_info,
        .dark-card .dataTables_paginate,
        .dark-card .dataTables_filter,
        .dark-card .dataTables_length {
          color: #dbeafe !important;
        }

        .dark-card .dataTables_filter input,
        .dark-card .dataTables_length select {
          background-color: #10253d !important;
          color: #ffffff !important;
          border: 1px solid #334155 !important;
        }

        .dataTables_wrapper {
          font-size: 13px;
        }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          column(
            width = 8,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "MediTrack Dashboard"),
              tags$div(
                class = "page-subtitle-dark",
                "Patient Readmission Risk Analysis with validated dashboard calculations and Live CDC API"
              )
            )
          ),
          column(
            width = 4,
            tags$div(
              class = "dark-heading right-text",
              icon("refresh"),
              " Live CDC API enabled",
              tags$br(),
              textOutput("cdc_live_status_overview")
            )
          )
        ),
        
        fluidRow(
          column(width = 4, uiOutput("kpi_total_patients")),
          column(width = 4, uiOutput("kpi_readmitted_patients")),
          column(width = 4, uiOutput("kpi_readmission_rate"))
        ),
        
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Readmission Share"),
              tags$div(class = "dark-card-subtitle", "Calculated directly from cleaned patient records"),
              plotlyOutput("readmission_donut_overview", height = "300px")
            )
          ),
          column(
            width = 8,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Readmission Rate by Age Group"),
              tags$div(class = "dark-card-subtitle", "Calculated from filtered cleaned dataset"),
              plotlyOutput("age_group_area_overview", height = "300px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Average Risk Indicators"),
              tags$div(class = "dark-card-subtitle", "Actual average values by readmission status"),
              plotlyOutput("risk_indicator_bar_overview", height = "330px")
            )
          ),
          column(
            width = 4,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Model Performance"),
              tags$div(class = "dark-card-subtitle", "Fixed evaluation output from Phase 4 model testing"),
              uiOutput("model_progress_cards")
            )
          ),
          column(
            width = 4,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "CDC Live API Trend"),
              tags$div(class = "dark-card-subtitle", "Auto-refreshing public CDC data"),
              plotlyOutput("cdc_plot_overview", height = "300px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "demographics",
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "Demographics"),
              tags$div(class = "page-subtitle-dark", "Readmission rates by gender, insurance type, and age group")
            )
          )
        ),
        
        fluidRow(
          column(width = 4, uiOutput("demo_total_card")),
          column(width = 4, uiOutput("demo_average_age_card")),
          column(width = 4, uiOutput("demo_top_insurance_card"))
        ),
        
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Gender Readmission Rate"),
              tags$div(class = "dark-card-subtitle", "Percent readmitted within each gender group"),
              plotlyOutput("gender_rate_plot", height = "300px")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Insurance Readmission Rate"),
              tags$div(class = "dark-card-subtitle", "Percent readmitted within each insurance type"),
              plotlyOutput("insurance_rate_plot", height = "300px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Age Group Readmission Trend"),
              tags$div(class = "dark-card-subtitle", "Readmission rate by age group"),
              plotlyOutput("age_group_rate_demo", height = "330px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "risk",
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "Risk Factors"),
              tags$div(class = "page-subtitle-dark", "Clinical and healthcare indicators calculated from filtered patient records")
            )
          )
        ),
        
        fluidRow(
          column(width = 3, uiOutput("risk_severity_card")),
          column(width = 3, uiOutput("risk_medication_card")),
          column(width = 3, uiOutput("risk_chronic_card")),
          column(width = 3, uiOutput("risk_previous_card"))
        ),
        
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Medication Burden Groups"),
              tags$div(class = "dark-card-subtitle", "Patient counts by medication burden and readmission status"),
              plotlyOutput("medication_burden_plot", height = "300px")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Previous Readmission Groups"),
              tags$div(class = "dark-card-subtitle", "Patient counts by previous readmission group"),
              plotlyOutput("previous_group_plot", height = "300px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Severity Score Comparison"),
              tags$div(class = "dark-card-subtitle", "Severity score distribution by readmission status"),
              plotlyOutput("severity_plot_risk", height = "300px")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Severity vs Medication Burden"),
              tags$div(class = "dark-card-subtitle", "Sampled scatter plot from filtered patient records"),
              plotlyOutput("severity_medication_scatter", height = "300px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "model",
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "Model Results"),
              tags$div(class = "page-subtitle-dark", "Model outputs from Phase 4. Risk levels are from model test predictions, not all 18,000 patients.")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Model Performance Progress"),
              tags$div(class = "dark-card-subtitle", "F1-score progress bars with accuracy and recall"),
              uiOutput("model_progress_cards_modeltab")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Risk Level Distribution from Test Predictions"),
              tags$div(class = "dark-card-subtitle", "Filtered when matching fields exist in model prediction output"),
              plotlyOutput("risk_level_plot", height = "300px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Model Performance Table"),
              tags$div(class = "dark-card-subtitle", "Exact model evaluation values"),
              DTOutput("model_table")
            )
          ),
          column(
            width = 6,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Best Model Summary"),
              tags$div(class = "dark-card-subtitle", "Quick interpretation of model results"),
              uiOutput("best_model_summary")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Top Important Features"),
              tags$div(class = "dark-card-subtitle", "Fixed Random Forest feature importance output from Phase 4"),
              plotlyOutput("feature_importance_plot", height = "380px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "cdc",
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "CDC Live API"),
              tags$div(class = "page-subtitle-dark", "Auto-refreshing public health indicator from CDC Chronic Disease Indicators API")
            )
          )
        ),
        
        fluidRow(
          column(width = 4, uiOutput("cdc_latest_value_card")),
          column(width = 4, uiOutput("cdc_topic_card")),
          column(width = 4, uiOutput("cdc_update_card"))
        ),
        
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "CDC API Controls"),
              tags$div(class = "dark-card-subtitle", "Select a topic, location, and public health indicator"),
              
              selectInput(
                "cdc_topic",
                "CDC Topic",
                choices = c(
                  "Diabetes",
                  "Cardiovascular Disease",
                  "Chronic Obstructive Pulmonary Disease"
                ),
                selected = "Diabetes"
              ),
              
              selectInput(
                "cdc_location",
                "Location",
                choices = c(
                  "United States",
                  "California",
                  "Texas",
                  "New York",
                  "Florida"
                ),
                selected = "United States"
              ),
              
              actionButton(
                "refresh_api",
                "Refresh CDC Data",
                icon = icon("refresh"),
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              tags$div(
                class = "live-status",
                textOutput("cdc_live_status")
              ),
              
              br(),
              
              uiOutput("cdc_question_ui")
            )
          ),
          
          column(
            width = 8,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "CDC Chronic Disease Indicator Trend"),
              tags$div(class = "dark-card-subtitle", "Live public API data with automatic 60-second refresh"),
              plotlyOutput("cdc_plot", height = "420px")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "patients",
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-heading",
              tags$div(class = "page-title-dark", "Patient Explorer"),
              tags$div(class = "page-subtitle-dark", "Filtered patient-level records from the cleaned dataset")
            )
          )
        ),
        
        fluidRow(
          column(width = 4, uiOutput("patient_total_card")),
          column(width = 4, uiOutput("patient_rate_card")),
          column(width = 4, uiOutput("patient_top_diagnosis_card"))
        ),
        
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Filtered Patient Records"),
              tags$div(class = "dark-card-subtitle", "This table updates based on sidebar filters and comes from the cleaned dataset"),
              DTOutput("patient_table")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "audit",
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "dark-card",
              tags$div(class = "dark-card-title", "Dashboard Data Audit"),
              tags$div(class = "dark-card-subtitle", "Validation summary of loaded dashboard files"),
              DTOutput("audit_table")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 7. Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  cdc_refresh_timer <- reactiveTimer(60000, session)
  cdc_last_updated <- reactiveVal("CDC API not yet updated.")
  
  observeEvent(input$reset_filters, {
    updateSliderInput(
      session,
      "age_range",
      value = c(
        min(readmission_data$age, na.rm = TRUE),
        max(readmission_data$age, na.rm = TRUE)
      )
    )
    
    updateSelectInput(session, "gender_filter", selected = "All")
    updateSelectInput(session, "insurance_filter", selected = "All")
    updateSelectInput(session, "diagnosis_filter", selected = "All")
  })
  
  filtered_data <- reactive({
    
    data <- readmission_data |>
      filter(
        age >= input$age_range[1],
        age <= input$age_range[2]
      )
    
    if (input$gender_filter != "All") {
      data <- data |> filter(gender == input$gender_filter)
    }
    
    if (input$insurance_filter != "All") {
      data <- data |> filter(insurance_type == input$insurance_filter)
    }
    
    if (input$diagnosis_filter != "All") {
      data <- data |> filter(primary_diagnosis_group == input$diagnosis_filter)
    }
    
    data
  })
  
  filtered_predictions <- reactive({
    
    req(model_predictions)
    
    data <- model_predictions
    
    # SAFE DEFAULTS (PREVENT INPUT CRASHES)
    age_min <- ifelse(is.null(input$age_range), min(data$age, na.rm = TRUE), input$age_range[1])
    age_max <- ifelse(is.null(input$age_range), max(data$age, na.rm = TRUE), input$age_range[2])
    
    gender <- ifelse(is.null(input$gender_filter), "All", input$gender_filter)
    insurance <- ifelse(is.null(input$insurance_filter), "All", input$insurance_filter)
    diagnosis <- ifelse(is.null(input$diagnosis_filter), "All", input$diagnosis_filter)
    
    # AGE FILTER
    if ("age" %in% names(data)) {
      data <- data %>%
        filter(age >= age_min, age <= age_max)
    }
    
    # GENDER FILTER
    if ("gender" %in% names(data) && gender != "All") {
      data <- data %>% filter(gender == gender)
    }
    
    # INSURANCE FILTER
    if ("insurance_type" %in% names(data) && insurance != "All") {
      data <- data %>% filter(insurance_type == insurance)
    }
    
    # DIAGNOSIS FILTER
    if ("primary_diagnosis_group" %in% names(data) && diagnosis != "All") {
      data <- data %>% filter(primary_diagnosis_group == diagnosis)
    }
    
    return(data)
  })
  
  make_mini_card <- function(label, value, icon_name = "info-circle") {
    tags$div(
      class = "mini-card",
      tags$div(class = "mini-icon", icon(icon_name)),
      tags$div(
        tags$div(class = "mini-label", label),
        tags$div(class = "mini-value", value)
      )
    )
  }
  
  make_kpi_card <- function(label, value, icon_name = "info-circle") {
    tags$div(
      class = "kpi-card",
      tags$div(class = "kpi-icon", icon(icon_name)),
      tags$div(
        tags$div(class = "kpi-label", label),
        tags$div(class = "kpi-value", value)
      )
    )
  }
  
  output$kpi_total_patients <- renderUI({
    make_kpi_card(
      "Total Patients",
      format(nrow(filtered_data()), big.mark = ","),
      "users"
    )
  })
  
  output$kpi_readmitted_patients <- renderUI({
    
    total_readmitted <- filtered_data() |>
      filter(readmission_category == "Readmitted") |>
      nrow()
    
    make_kpi_card(
      "Readmitted Patients",
      format(total_readmitted, big.mark = ","),
      "plus-square"
    )
  })
  
  output$kpi_readmission_rate <- renderUI({
    
    data <- filtered_data()
    rate <- ifelse(
      nrow(data) == 0,
      0,
      mean(data$readmitted_flag == 1, na.rm = TRUE) * 100
    )
    
    make_kpi_card(
      "Readmission Rate",
      paste0(round(rate, 2), "%"),
      "percent"
    )
  })
  
  readmission_colors <- c(
    "Readmitted" = "#38bdf8",
    "Not Readmitted" = "#f97316"
  )
  
  dark_layout <- function(p,
                          x_title = "",
                          y_title = "",
                          showlegend = TRUE,
                          barmode = NULL,
                          margin = list(l = 55, r = 25, t = 25, b = 55)) {
    
    p <- layout(
      p,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#ffffff"),
      showlegend = showlegend,
      legend = list(font = list(color = "#ffffff")),
      xaxis = list(
        title = x_title,
        gridcolor = "rgba(255,255,255,0.12)",
        zerolinecolor = "rgba(255,255,255,0.12)"
      ),
      yaxis = list(
        title = y_title,
        gridcolor = "rgba(255,255,255,0.12)",
        zerolinecolor = "rgba(255,255,255,0.12)"
      ),
      margin = margin
    )
    
    if (!is.null(barmode)) {
      p <- layout(p, barmode = barmode)
    }
    
    p
  }
  
  age_group_summary <- reactive({
    
    filtered_data() |>
      mutate(
        age_group = cut(
          age,
          breaks = c(0, 29, 39, 49, 59, 69, 79, 120),
          labels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
          right = TRUE
        )
      ) |>
      filter(!is.na(age_group)) |>
      group_by(age_group) |>
      summarise(
        total_patients = n(),
        readmitted_patients = sum(readmission_category == "Readmitted", na.rm = TRUE),
        readmission_rate = round((readmitted_patients / total_patients) * 100, 2),
        .groups = "drop"
      )
  })
  
  output$readmission_donut_overview <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    plot_data <- data |>
      count(readmission_category, .drop = FALSE) |>
      mutate(
        percent = ifelse(sum(n) == 0, 0, round(n / sum(n) * 100, 2)),
        label_text = paste0(
          readmission_category,
          "<br>Count: ", format(n, big.mark = ","),
          "<br>Percent: ", percent, "%"
        )
      )
    
    plot_ly(
      plot_data,
      labels = ~readmission_category,
      values = ~n,
      type = "pie",
      hole = 0.65,
      textinfo = "percent",
      hoverinfo = "text",
      text = ~label_text,
      marker = list(colors = c("#38bdf8", "#f97316"))
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#ffffff"),
        showlegend = TRUE,
        margin = list(l = 10, r = 10, t = 10, b = 10)
      )
  })
  
  output$age_group_area_overview <- renderPlotly({
    
    age_data <- age_group_summary()
    req(nrow(age_data) > 0)
    
    plot_ly(
      age_data,
      x = ~age_group,
      y = ~readmission_rate,
      type = "scatter",
      mode = "lines+markers",
      fill = "tozeroy",
      line = list(color = "#38bdf8", width = 4),
      marker = list(color = "#f97316", size = 8),
      text = ~paste0(
        "Age Group: ", age_group,
        "<br>Total Patients: ", total_patients,
        "<br>Readmitted: ", readmitted_patients,
        "<br>Readmission Rate: ", readmission_rate, "%"
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Age Group",
        y_title = "Readmission Rate (%)",
        showlegend = FALSE
      )
  })
  
  output$risk_indicator_bar_overview <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    risk_data <- data |>
      group_by(readmission_category) |>
      summarise(
        `Severity Score` = mean(severity_score, na.rm = TRUE),
        `Number of Medications` = mean(number_of_medications, na.rm = TRUE),
        `Chronic Disease Count` = mean(chronic_disease_count, na.rm = TRUE),
        `Previous Readmissions` = mean(previous_readmissions_1y, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = -readmission_category,
        names_to = "risk_indicator",
        values_to = "average_value"
      )
    
    plot_ly(
      risk_data,
      x = ~risk_indicator,
      y = ~average_value,
      color = ~readmission_category,
      colors = readmission_colors,
      type = "bar",
      text = ~paste0(
        "Status: ", readmission_category,
        "<br>Indicator: ", risk_indicator,
        "<br>Average Value: ", round(average_value, 2)
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Risk Indicator",
        y_title = "Average Value",
        barmode = "group",
        margin = list(l = 55, r = 25, t = 25, b = 95)
      )
  })
  
  model_progress_ui <- function() {
    
    data <- model_performance_display |>
      mutate(
        accuracy_percent = round(accuracy * 100, 1),
        recall_percent = round(recall * 100, 1),
        f1_percent = round(f1_score * 100, 1)
      )
    
    tagList(
      lapply(seq_len(nrow(data)), function(i) {
        
        tags$div(
          style = "margin-bottom: 20px;",
          
          tags$div(
            class = "progress-label",
            data$model[i],
            tags$span(
              class = "progress-value",
              paste0(data$f1_percent[i], "% F1")
            )
          ),
          
          tags$div(
            class = "progress",
            tags$div(
              class = "progress-bar",
              role = "progressbar",
              style = paste0(
                "width:", data$f1_percent[i],
                "%; background: linear-gradient(90deg, #38bdf8, #22c55e);"
              )
            )
          ),
          
          tags$div(
            style = "font-size: 12px; color: #bfdbfe; margin-top: 4px;",
            paste0(
              "Accuracy: ", data$accuracy_percent[i],
              "% | Recall: ", data$recall_percent[i], "%"
            )
          )
        )
      })
    )
  }
  
  output$model_progress_cards <- renderUI({
    model_progress_ui()
  })
  
  output$model_progress_cards_modeltab <- renderUI({
    model_progress_ui()
  })
  
  output$demo_total_card <- renderUI({
    make_mini_card(
      "Filtered Patients",
      format(nrow(filtered_data()), big.mark = ","),
      "users"
    )
  })
  
  output$demo_average_age_card <- renderUI({
    data <- filtered_data()
    avg_age <- ifelse(nrow(data) == 0, 0, mean(data$age, na.rm = TRUE))
    
    make_mini_card(
      "Average Age",
      round(avg_age, 1),
      "calendar"
    )
  })
  
  output$demo_top_insurance_card <- renderUI({
    data <- filtered_data()
    
    top_insurance <- data |>
      count(insurance_type, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(insurance_type)
    
    if (length(top_insurance) == 0) {
      top_insurance <- "N/A"
    }
    
    make_mini_card(
      "Top Insurance",
      top_insurance,
      "id-card"
    )
  })
  
  output$gender_rate_plot <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    gender_rate <- data |>
      group_by(gender) |>
      summarise(
        total = n(),
        readmitted = sum(readmission_category == "Readmitted", na.rm = TRUE),
        rate = round(readmitted / total * 100, 2),
        .groups = "drop"
      )
    
    plot_ly(
      gender_rate,
      x = ~gender,
      y = ~rate,
      type = "bar",
      marker = list(color = "#38bdf8"),
      text = ~paste0(
        "Gender: ", gender,
        "<br>Total Patients: ", total,
        "<br>Readmitted: ", readmitted,
        "<br>Readmission Rate: ", rate, "%"
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Gender",
        y_title = "Readmission Rate (%)",
        showlegend = FALSE
      )
  })
  
  output$insurance_rate_plot <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    insurance_rate <- data |>
      group_by(insurance_type) |>
      summarise(
        total = n(),
        readmitted = sum(readmission_category == "Readmitted", na.rm = TRUE),
        rate = round(readmitted / total * 100, 2),
        .groups = "drop"
      ) |>
      arrange(rate)
    
    plot_ly(
      insurance_rate,
      x = ~rate,
      y = ~insurance_type,
      type = "bar",
      orientation = "h",
      marker = list(color = "#38bdf8"),
      text = ~paste0(
        "Insurance Type: ", insurance_type,
        "<br>Total Patients: ", total,
        "<br>Readmitted: ", readmitted,
        "<br>Readmission Rate: ", rate, "%"
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Readmission Rate (%)",
        y_title = "",
        showlegend = FALSE,
        margin = list(l = 120, r = 30, t = 20, b = 45)
      )
  })
  
  output$age_group_rate_demo <- renderPlotly({
    
    age_data <- age_group_summary()
    req(nrow(age_data) > 0)
    
    plot_ly(
      age_data,
      x = ~age_group,
      y = ~readmission_rate,
      type = "scatter",
      mode = "lines+markers",
      fill = "tozeroy",
      line = list(color = "#38bdf8", width = 4),
      marker = list(color = "#f97316", size = 8),
      text = ~paste0(
        "Age Group: ", age_group,
        "<br>Total Patients: ", total_patients,
        "<br>Readmitted: ", readmitted_patients,
        "<br>Readmission Rate: ", readmission_rate, "%"
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Age Group",
        y_title = "Readmission Rate (%)",
        showlegend = FALSE
      )
  })
  
  output$risk_severity_card <- renderUI({
    data <- filtered_data()
    value <- ifelse(nrow(data) == 0, 0, mean(data$severity_score, na.rm = TRUE))
    make_mini_card("Avg Severity", round(value, 2), "heartbeat")
  })
  
  output$risk_medication_card <- renderUI({
    data <- filtered_data()
    value <- ifelse(nrow(data) == 0, 0, mean(data$number_of_medications, na.rm = TRUE))
    make_mini_card("Avg Medications", round(value, 2), "medkit")
  })
  
  output$risk_chronic_card <- renderUI({
    data <- filtered_data()
    value <- ifelse(nrow(data) == 0, 0, mean(data$chronic_disease_count, na.rm = TRUE))
    make_mini_card("Avg Chronic Count", round(value, 2), "file-text")
  })
  
  output$risk_previous_card <- renderUI({
    data <- filtered_data()
    value <- ifelse(nrow(data) == 0, 0, mean(data$previous_readmissions_1y, na.rm = TRUE))
    make_mini_card("Avg Previous Readmits", round(value, 2), "history")
  })
  
  output$medication_burden_plot <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    med_data <- data |>
      mutate(
        medication_group = case_when(
          number_of_medications <= 5 ~ "Low medication burden",
          number_of_medications <= 10 ~ "Moderate medication burden",
          TRUE ~ "High medication burden"
        ),
        medication_group = factor(
          medication_group,
          levels = c(
            "Low medication burden",
            "Moderate medication burden",
            "High medication burden"
          )
        )
      ) |>
      count(medication_group, readmission_category)
    
    plot_ly(
      med_data,
      x = ~medication_group,
      y = ~n,
      color = ~readmission_category,
      colors = readmission_colors,
      type = "bar",
      text = ~paste0(
        "Medication Group: ", medication_group,
        "<br>Status: ", readmission_category,
        "<br>Count: ", format(n, big.mark = ",")
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Medication Burden Group",
        y_title = "Number of Patients",
        barmode = "group",
        margin = list(l = 55, r = 25, t = 25, b = 95)
      )
  })
  
  output$previous_group_plot <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    previous_data <- data |>
      mutate(
        previous_group = case_when(
          previous_readmissions_1y == 0 ~ "0 previous",
          previous_readmissions_1y == 1 ~ "1 previous",
          previous_readmissions_1y == 2 ~ "2 previous",
          TRUE ~ "3+ previous"
        ),
        previous_group = factor(
          previous_group,
          levels = c("0 previous", "1 previous", "2 previous", "3+ previous")
        )
      ) |>
      count(previous_group, readmission_category)
    
    plot_ly(
      previous_data,
      x = ~previous_group,
      y = ~n,
      color = ~readmission_category,
      colors = readmission_colors,
      type = "bar",
      text = ~paste0(
        "Previous Readmission Group: ", previous_group,
        "<br>Status: ", readmission_category,
        "<br>Count: ", format(n, big.mark = ",")
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Previous Readmission Group",
        y_title = "Number of Patients",
        barmode = "group"
      )
  })
  
  output$severity_plot_risk <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    plot_ly(
      data,
      x = ~readmission_category,
      y = ~severity_score,
      color = ~readmission_category,
      colors = readmission_colors,
      type = "box",
      hoverinfo = "x+y"
    ) |>
      dark_layout(
        x_title = "Readmission Status",
        y_title = "Severity Score",
        showlegend = FALSE
      )
  })
  
  output$severity_medication_scatter <- renderPlotly({
    
    data <- filtered_data()
    req(nrow(data) > 0)
    
    sample_data <- data |>
      sample_n(min(1200, nrow(data)))
    
    plot_ly(
      sample_data,
      x = ~number_of_medications,
      y = ~severity_score,
      color = ~readmission_category,
      colors = readmission_colors,
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, opacity = 0.65),
      text = ~paste0(
        "Status: ", readmission_category,
        "<br>Medications: ", number_of_medications,
        "<br>Severity Score: ", severity_score,
        "<br>Diagnosis Group: ", primary_diagnosis_group
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Number of Medications",
        y_title = "Severity Score"
      )
  })
  
  output$model_table <- renderDT({
    datatable(
      model_performance_display,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  output$best_model_summary <- renderUI({
    
    best_model <- model_performance_display |>
      arrange(desc(f1_score)) |>
      slice_head(n = 1)
    
    tags$div(
      style = "color: #e5e7eb; font-size: 14px; line-height: 1.7;",
      tags$p(tags$b("Best model based on F1-score:")),
      tags$h3(
        style = "color: #38bdf8; font-weight: 800;",
        best_model$model
      ),
      tags$p(paste0("Accuracy: ", round(best_model$accuracy * 100, 1), "%")),
      tags$p(paste0("Recall: ", round(best_model$recall * 100, 1), "%")),
      tags$p(paste0("F1-score: ", round(best_model$f1_score * 100, 1), "%")),
      tags$p("The model output is used for exploratory analytics, not clinical diagnosis.")
    )
  })
  
  output$risk_level_plot <- renderPlotly({
    
    data <- filtered_predictions()
    
    if (is.null(data) || nrow(data) == 0) {
      return(plot_ly(type = "pie", labels = "No Data", values = 1))
    }
    
    risk_data <- data %>%
      mutate(risk_level = as.character(risk_level)) %>%
      mutate(risk_level = ifelse(is.na(risk_level) | risk_level == "", "Unknown", risk_level)) %>%
      count(risk_level)
    
    plot_ly(
      risk_data,
      labels = ~risk_level,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(
        colors = c("#3b82f6", "#ef4444", "#22c55e", "#94a3b8"),
        line = list(color = "rgba(255,255,255,0.2)", width = 1)
      )
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#ffffff"),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1,
          y = 0.5
        )
      )
  })
  
  output$feature_importance_plot <- renderPlotly({
    
    top_features <- feature_importance |>
      slice_max(order_by = Overall, n = 10) |>
      arrange(Overall) |>
      mutate(variable = factor(variable, levels = variable))
    
    plot_ly(
      top_features,
      x = ~Overall,
      y = ~variable,
      type = "bar",
      orientation = "h",
      marker = list(color = "#38bdf8"),
      text = ~paste0(
        "Variable: ", variable,
        "<br>Importance Score: ", round(Overall, 3)
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Importance Score",
        y_title = "",
        showlegend = FALSE,
        margin = list(l = 170, r = 30, t = 20, b = 45)
      )
  })
  
  cdc_data <- reactive({
    
    cdc_refresh_timer()
    input$refresh_api
    
    topic_choice <- input$cdc_topic
    location_choice <- input$cdc_location
    
    api_result <- tryCatch({
      get_cdc_api_data(
        topic_choice = topic_choice,
        location_choice = location_choice
      )
    }, error = function(e) {
      get_cdc_backup()
    })
    
    if (nrow(api_result) == 0) {
      api_result <- get_cdc_backup()
    }
    
    cdc_last_updated(
      paste("Last CDC API update:", format(Sys.time(), "%B %d, %Y %I:%M:%S %p"))
    )
    
    api_result
  })
  
  output$cdc_live_status <- renderText({
    as.character(cdc_last_updated())
  })
  
  output$cdc_live_status_overview <- renderText({
    cdc_last_updated()
  })
  
  output$cdc_question_ui <- renderUI({
    
    data <- cdc_data()
    
    if (nrow(data) == 0 || !"question" %in% names(data)) {
      return(helpText("No CDC data loaded. Check internet connection or API availability."))
    }
    
    questions <- sort(unique(na.omit(data$question)))
    
    if (length(questions) == 0) {
      return(helpText("No CDC indicator questions available."))
    }
    
    selectInput(
      "cdc_question",
      "CDC Indicator Question",
      choices = questions,
      selected = questions[1]
    )
  })
  
  cdc_filtered <- reactive({
    
    data <- cdc_data()
    req(nrow(data) > 0)
    
    available_questions <- sort(unique(na.omit(data$question)))
    req(length(available_questions) > 0)
    
    selected_question <- input$cdc_question
    
    if (is.null(selected_question) || !(selected_question %in% available_questions)) {
      selected_question <- available_questions[1]
    }
    
    data |>
      filter(question == selected_question) |>
      group_by(yearstart) |>
      summarise(
        datavalue = mean(datavalue, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(yearstart)
  })
  
  output$cdc_latest_value_card <- renderUI({
    
    data <- cdc_filtered()
    
    if (nrow(data) == 0) {
      value <- "N/A"
    } else {
      value <- data |>
        arrange(desc(yearstart)) |>
        slice_head(n = 1) |>
        pull(datavalue)
      value <- round(value, 2)
    }
    
    make_mini_card("Latest CDC Value", value, "line-chart")
  })
  
  output$cdc_topic_card <- renderUI({
    make_mini_card("Selected Topic", input$cdc_topic, "globe")
  })
  
  output$cdc_update_card <- renderUI({
    make_mini_card("Live Refresh", "60 sec", "refresh")
  })
  
  make_cdc_plot_dark <- function(data) {
    
    plot_ly(
      data,
      x = ~yearstart,
      y = ~datavalue,
      type = "scatter",
      mode = "lines+markers",
      fill = "tozeroy",
      line = list(color = "#38bdf8", width = 4),
      marker = list(color = "#f97316", size = 8),
      text = ~paste0(
        "Year: ", yearstart,
        "<br>Value: ", round(datavalue, 2)
      ),
      hoverinfo = "text"
    ) |>
      dark_layout(
        x_title = "Year",
        y_title = "Data Value",
        showlegend = FALSE
      )
  }
  
  output$cdc_plot_overview <- renderPlotly({
    req(nrow(cdc_filtered()) > 0)
    make_cdc_plot_dark(cdc_filtered())
  })
  
  output$cdc_plot <- renderPlotly({
    req(nrow(cdc_filtered()) > 0)
    make_cdc_plot_dark(cdc_filtered())
  })
  
  output$patient_total_card <- renderUI({
    make_mini_card(
      "Filtered Records",
      format(nrow(filtered_data()), big.mark = ","),
      "table"
    )
  })
  
  output$patient_rate_card <- renderUI({
    
    data <- filtered_data()
    rate <- ifelse(nrow(data) == 0, 0, mean(data$readmitted_flag == 1, na.rm = TRUE) * 100)
    
    make_mini_card(
      "Filtered Readmission Rate",
      paste0(round(rate, 2), "%"),
      "percent"
    )
  })
  
  output$patient_top_diagnosis_card <- renderUI({
    
    data <- filtered_data()
    
    top_diag <- data |>
      count(primary_diagnosis_group, sort = TRUE) |>
      slice_head(n = 1) |>
      pull(primary_diagnosis_group)
    
    if (length(top_diag) == 0) {
      top_diag <- "N/A"
    }
    
    make_mini_card(
      "Top Diagnosis",
      top_diag,
      "stethoscope"
    )
  })
  
  output$patient_table <- renderDT({
    datatable(
      filtered_data(),
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  output$audit_table <- renderDT({
    datatable(
      dashboard_audit,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )
  })
}

# ------------------------------------------------------------
# 8. Run App
# ------------------------------------------------------------

shinyApp(ui, server)