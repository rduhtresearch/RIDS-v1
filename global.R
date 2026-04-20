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

source("R/utils/adjust.r")
source("R/utils/build_template.r")

# ==============================================================================
# GLOBAL CONFIGURATION & INITIALIZATION
# ==============================================================================
message("=== GLOBAL LOAD ===\n")

# Database directory reference
DB_DIR <- "~/nhs_finance_app_data/RIDS.duckdb"

# Database connection reference (Temporary/Initial)
CON <- dbConnect(duckdb(), "~/nhs_finance_app_data/RIDS.duckdb")

# Run database build/seed process
db_main()
message("=== INIT_DB DONE ===\n")

# Reconnect to the primary RIDS database
CON <- dbConnect(duckdb(), "~/nhs_finance_app_data/RIDS.duckdb")
message("=== DB CONNECTED ===\n")

# Default upload directory
ICT_UPLOAD_DIR_DEFAULT <- "/Users/tategraham/Documents/NHS/ict_dir"

# Load from DB if available, fall back to default
ICT_UPLOAD_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'ict_upload_dir'")
  if (nrow(val) > 0) val$value else ICT_UPLOAD_DIR_DEFAULT
}, error = function(e) ICT_UPLOAD_DIR_DEFAULT)

EDGE_OUTPUT_DIR_DEFAULT <- file.path(path.expand("~"), "edge_outputs")

EDGE_OUTPUT_DIR <- tryCatch({
  val <- dbGetQuery(CON, "SELECT value FROM app_settings WHERE key = 'edge_output_dir'")
  if (nrow(val) > 0) val$value else EDGE_OUTPUT_DIR_DEFAULT
}, error = function(e) EDGE_OUTPUT_DIR_DEFAULT)

# ==============================================================================
# SHINY SESSION CLEANUP
# ==============================================================================
onStop(function() {
  message("=== CLOSING DB ===\n")
  dbDisconnect(CON, shutdown = TRUE)
})
