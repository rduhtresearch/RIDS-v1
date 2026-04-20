# dependencies.R
required_packages <- c(
  "DBI",
  "duckdb",
  "sodium",
  "shiny",
  "bs4Dash",
  "waiter",
  "shinyFeedback",
  "shinyjs",
  "reactable",
  "DT",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "readr",
  "openxlsx"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}