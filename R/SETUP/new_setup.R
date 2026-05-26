# ==============================================================================
# RIDS - Shared Drive Setup
# ==============================================================================
# Run this script from the shared app folder before anyone starts using RIDS.
#
# What it does:
# 1. Installs any missing R packages
# 2. Creates the shared runtime and releases folders if needed
# 3. Creates or updates the shared deployment config
# 4. Creates the central Windows launcher
# 5. Initialises the central DuckDB database if needed
# 6. Optionally bootstraps the first active release when HEAD is already tagged
# ==============================================================================

source("R/dependencies.R")
source("R/utils/deployment_config.R")
source("R/utils/release_management.R")

# ------------------------------------------------------------------------------
# 1. Shared deployment settings
# Edit these values, then run the whole script.
# ------------------------------------------------------------------------------
APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DEPLOYMENT_DIR <- file.path(APP_DIR, "deployment")
RELEASES_DIR <- file.path(APP_DIR, "releases")
SHARED_DIR <- file.path(APP_DIR, "shared")

DB_DIR <- file.path(SHARED_DIR, "data", "RIDS.duckdb")
ICT_UPLOAD_DIR <- file.path(SHARED_DIR, "uploads")
EDGE_OUTPUT_DIR <- file.path(SHARED_DIR, "outputs")
APP_LOG_DIR <- file.path(SHARED_DIR, "logs")

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
must_be_present(APP_LOG_DIR, "APP_LOG_DIR")
ensure_required_app_files(APP_DIR)

deployment_config_path <- file.path(SHARED_DIR, "deployment_config.R")
current_release_path <- file.path(SHARED_DIR, "current_release.txt")
deploy_log_path <- file.path(SHARED_DIR, "deploy_log.tsv")
launcher_r_path <- file.path(DEPLOYMENT_DIR, "launch_app.R")
launcher_bat_path <- file.path(DEPLOYMENT_DIR, "Launch RIDS.bat")

dir.create(DEPLOYMENT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RELEASES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SHARED_DIR, recursive = TRUE, showWarnings = FALSE)

dirs_to_create <- c(dirname(DB_DIR), ICT_UPLOAD_DIR, EDGE_OUTPUT_DIR, APP_LOG_DIR)
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
  app_log_dir = normalizePath(APP_LOG_DIR, winslash = "/", mustWork = FALSE),
  app_host = APP_HOST,
  app_port = as.integer(APP_PORT),
  sql_server = SQL_SERVER,
  sql_database = SQL_DATABASE,
  sql_driver = SQL_DRIVER
)

write_deployment_config(deployment_config_path, config)
write_launcher_r_script(
  path = launcher_r_path,
  releases_dir = RELEASES_DIR,
  current_release_path = current_release_path,
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
upsert_setting(CON, "app_log_dir", config$app_log_dir)

if (db_already_exists) {
  message("Existing database found. Schema and settings have been checked.")
} else {
  message("New database created and initialised.")
}

# ------------------------------------------------------------------------------
# 6. Bootstrap the first release if HEAD is already tagged
# ------------------------------------------------------------------------------
current_release <- read_release_pointer(current_release_path)

if (!nzchar(current_release)) {
  head_tag <- git_exact_tag(APP_DIR, "HEAD")

  if (nzchar(head_tag)) {
    initial_release_dir <- file.path(RELEASES_DIR, head_tag)
    if (!file.exists(file.path(initial_release_dir, "app.R"))) {
      export_git_snapshot(APP_DIR, head_tag, initial_release_dir, overwrite = FALSE)
    }
    run_release_smoke_check(initial_release_dir, deployment_config_path)
    write_release_pointer(current_release_path, head_tag)
    append_deploy_log(
      deploy_log_path,
      action = "bootstrap",
      version = head_tag,
      status = "success",
      message = "Initial release created during setup."
    )
    message("Initial release bootstrapped from Git tag: ", head_tag)
  } else {
    message("No Git tag is checked out at HEAD, so no active release was created yet.")
    message("After setup, publish the first tagged release with:")
    message("Rscript R/SETUP/release_publish.R publish --version vX.Y.Z")
  }
} else {
  message("Current active release already set to: ", current_release)
}

# ------------------------------------------------------------------------------
# 7. Finish
# ------------------------------------------------------------------------------
message("")
message("Setup complete.")
message("")
message("Next steps:")
message("1. If no release is active yet, publish one with R/SETUP/release_publish.R.")
message("2. Open: ", launcher_bat_path)
message("3. Wait for the browser to open.")
message("4. On first launch, create the first admin account in the app.")
