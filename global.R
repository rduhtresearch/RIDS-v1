# ==============================================================================
# LIBRARIES
# ==============================================================================
library(DBI)
library(duckdb)
library(sodium)
library(shiny)
library(shiny)
library(bs4Dash)
library(waiter)
library(shinyFeedback)

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

# ==============================================================================
# SHINY SESSION CLEANUP
# ==============================================================================
onStop(function() {
  message("=== CLOSING DB ===\n")
  dbDisconnect(CON, shutdown = TRUE)
})