# ==============================================================================
# LIBRARIES
# ==============================================================================
library(DBI)
library(duckdb)
library(sodium)
library(shiny)
library(bs4Dash)
library(waiter)
library(shinyFeedback)
library(shinyjs)
library(reactable)
library(DT)
library(zip)

# ==============================================================================
# SOURCE UTILS
# ==============================================================================
source("R/utils/auth.r")
source("R/utils/add_study_arm.r")
source("R/utils/pipeline_fixed.r")
source("R/utils/posting_test.r")
source("R/utils/extract_cpms_id.r")
source("R/utils/template_build_main.r")
source("R/utils/posting_lines.r")
source("R/utils/dev_banner.r")
source("R/utils/add_cost_centres.r")
source("R/utils/assign_edge_keys.r")
source("R/utils/adjust.r")
source("R/utils/build_template.r")
source("R/utils/validate_ict_workbook.r")

# Custom activity source files
source("R/addons/custom_activities/ca_build_custom_rows.R",    local = FALSE)
source("R/addons/custom_activities/ca_schema.R",               local = FALSE)
source("R/addons/custom_activities/ca_ref_activities.R",       local = FALSE)
source("R/addons/custom_activities/ca_queries.R",              local = FALSE)
source("R/addons/custom_activities/ca_assign_edge_keys.R",     local = FALSE)
source("R/addons/custom_activities/apply_custom_activities.R", local = FALSE)

# ==============================================================================
# GLOBAL CONFIGURATION & INITIALIZATION
# ==============================================================================
message("=== GLOBAL LOAD ===\n")

# Load paths written by SETUP/new_setup.R
if (!file.exists("config.R")) {
  stop("config.R not found. Run R/SETUP/new_setup.R before launching the app.")
}
source("config.R")

# Connect to database
CON <- dbConnect(duckdb(), DB_DIR)
message("=== DB CONNECTED ===\n")

# Load paths from DB settings (admin may have updated them); fall back to config values
ICT_UPLOAD_DIR_DEFAULT <- ICT_UPLOAD_DIR

ICT_UPLOAD_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'ict_upload_dir'")
  if (nrow(val) > 0) val$value else ICT_UPLOAD_DIR_DEFAULT
}, error = function(e) ICT_UPLOAD_DIR_DEFAULT)

EDGE_OUTPUT_DIR_DEFAULT <- EDGE_OUTPUT_DIR

EDGE_OUTPUT_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'edge_output_dir'")
  if (nrow(val) > 0) val$value else EDGE_OUTPUT_DIR_DEFAULT
}, error = function(e) EDGE_OUTPUT_DIR_DEFAULT)
db_main()
# ==============================================================================
# SHINY SESSION CLEANUP
# ==============================================================================
onStop(function() {
  message("=== CLOSING DB ===\n")
  dbDisconnect(CON, shutdown = TRUE)
})

# ==============================================================================
# Version consts
# ==============================================================================
APP_VERSION      <- "0.4.0"
APP_LAST_UPDATED <- "2026-05-06"