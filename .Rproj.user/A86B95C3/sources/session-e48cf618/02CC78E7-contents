# 01_cdc_api_download.R

library(httr2)
library(jsonlite)
library(readr)
library(dplyr)
library(tibble)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

get_cdc_data <- function(topic_choice = "Diabetes",
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
    as_tibble() |>
    mutate(
      yearstart = as.integer(yearstart),
      datavalue = as.numeric(datavalue)
    )
}

cdc_diabetes <- tryCatch({
  get_cdc_data(
    topic_choice = "Diabetes",
    location_choice = "United States",
    limit = 5000
  )
}, error = function(e) {
  message("CDC API request failed: ", e$message)
  tibble()
})

write_csv(
  cdc_diabetes,
  "data/raw/cdc_chronic_disease_diabetes_api_raw.csv"
)

print(cdc_diabetes)
cat("Rows downloaded:", nrow(cdc_diabetes), "\n")
cat("Columns downloaded:", ncol(cdc_diabetes), "\n")