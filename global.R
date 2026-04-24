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
