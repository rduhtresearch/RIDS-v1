# ==============================================================================
# RIDS - Shared Drive Setup
# ==============================================================================
# Run this script from the shared app folder before anyone starts using RIDS.
#
# What it does:
# 1. Installs any missing R packages
# 2. Creates the shared folders if needed
# 3. Creates or updates the shared deployment config
# 4. Creates the central Windows launcher
# 5. Initialises the central DuckDB database if needed
# ==============================================================================

source("R/dependencies.R")
source("R/utils/deployment_config.R")

# ------------------------------------------------------------------------------
# 1. Shared deployment settings
# Edit these values, then run the whole script.
# ------------------------------------------------------------------------------
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DEPLOYMENT_DIR <- file.path(APP_DIR, "deployment")

DB_DIR <- file.path(APP_DIR, "data", "RIDS.duckdb")
ICT_UPLOAD_DIR <- file.path(APP_DIR, "uploads")
EDGE_OUTPUT_DIR <- file.path(APP_DIR, "outputs")

APP_HOST <- "127.0.0.1"
APP_PORT <- 3838L

# Reserved for a future SQL Server migration.
SQL_SERVER <- ""
SQL_DATABASE <- ""
SQL_DRIVER <- ""

# ------------------------------------------------------------------------------
# 2. Validation helpers
# ------------------------------------------------------------------------------
must_be_present <- function(path, label) {
  if (!nzchar(trimws(path))) {
    stop(label, " is required.")
  }
}

ensure_app_folder <- function(app_dir) {
  required_files <- c("app.R", "global.R", "R/setup.r")
  missing_files <- required_files[!file.exists(file.path(app_dir, required_files))]

  if (length(missing_files) > 0) {
    stop(
      "The shared app folder is missing required files: ",
      paste(missing_files, collapse = ", "),
      ". Run this script from the RIDS app folder."
    )
  }
}

upsert_setting <- function(con, key, value) {
  existing <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM app_settings WHERE key = ?",
    params = list(key)
  )$n[[1]]

  if (existing > 0) {
    dbExecute(
      con,
      "UPDATE app_settings SET value = ? WHERE key = ?",
      params = list(value, key)
    )
  } else {
    dbExecute(
      con,
      "INSERT INTO app_settings (key, value) VALUES (?, ?)",
      params = list(key, value)
    )
  }
}

# ------------------------------------------------------------------------------
# 3. Validate the shared app folder and requested paths
# ------------------------------------------------------------------------------
must_be_present(APP_DIR, "APP_DIR")
must_be_present(DB_DIR, "DB_DIR")
must_be_present(ICT_UPLOAD_DIR, "ICT_UPLOAD_DIR")
must_be_present(EDGE_OUTPUT_DIR, "EDGE_OUTPUT_DIR")
ensure_app_folder(APP_DIR)

deployment_config_path <- file.path(DEPLOYMENT_DIR, "deployment_config.R")
launcher_r_path <- file.path(DEPLOYMENT_DIR, "launch_app.R")
launcher_bat_path <- file.path(DEPLOYMENT_DIR, "Launch RIDS.bat")

dir.create(DEPLOYMENT_DIR, recursive = TRUE, showWarnings = FALSE)

dirs_to_create <- c(dirname(DB_DIR), ICT_UPLOAD_DIR, EDGE_OUTPUT_DIR)
for (path in dirs_to_create) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message("Created folder: ", path)
  } else {
    message("Folder already exists: ", path)
  }
}

# ------------------------------------------------------------------------------
# 4. Write the shared deployment config and launcher files
# ------------------------------------------------------------------------------
config <- list(
  storage_mode = "duckdb",
  db_dir = normalizePath(DB_DIR, winslash = "/", mustWork = FALSE),
  ict_upload_dir = normalizePath(ICT_UPLOAD_DIR, winslash = "/", mustWork = FALSE),
  edge_output_dir = normalizePath(EDGE_OUTPUT_DIR, winslash = "/", mustWork = FALSE),
  app_host = APP_HOST,
  app_port = as.integer(APP_PORT),
  sql_server = SQL_SERVER,
  sql_database = SQL_DATABASE,
  sql_driver = SQL_DRIVER
)

write_deployment_config(deployment_config_path, config)
write_launcher_r_script(
  path = launcher_r_path,
  app_dir = APP_DIR,
  config_path = deployment_config_path,
  app_host = APP_HOST,
  app_port = APP_PORT
)
write_launcher_bat(
  path = launcher_bat_path,
  launcher_r_path = launcher_r_path,
  app_port = APP_PORT
)

message("Deployment config written: ", deployment_config_path)
message("Launcher created: ", launcher_bat_path)

# ------------------------------------------------------------------------------
# 5. Initialise or refresh the central database safely
# ------------------------------------------------------------------------------
library(DBI)
library(duckdb)

db_already_exists <- file.exists(DB_DIR)

source("R/utils/auth.r")
source("R/setup.r")

CON <- dbConnect(duckdb(), DB_DIR)
on.exit(try(dbDisconnect(CON, shutdown = TRUE), silent = TRUE), add = TRUE)

db_main()
upsert_setting(CON, "ict_upload_dir", config$ict_upload_dir)
upsert_setting(CON, "edge_output_dir", config$edge_output_dir)

if (db_already_exists) {
  message("Existing database found. Schema and settings have been checked.")
} else {
  message("New database created and initialised.")
}

# ------------------------------------------------------------------------------
# 6. Finish
# ------------------------------------------------------------------------------
message("")
message("Setup complete.")
message("")
message("Next steps:")
message("1. Open: ", launcher_bat_path)
message("2. Wait for the browser to open.")
message("3. On first launch, create the first admin account in the app.")
