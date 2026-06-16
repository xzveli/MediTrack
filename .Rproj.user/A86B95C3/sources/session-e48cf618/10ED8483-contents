# 00_setup.R

packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "ggplot2",
  "plotly",
  "httr2",
  "jsonlite",
  "caret",
  "rpart",
  "rpart.plot",
  "randomForest",
  "DT",
  "shiny",
  "shinydashboard"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("R", recursive = TRUE, showWarnings = FALSE)
dir.create("models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/screenshots", recursive = TRUE, showWarnings = FALSE)