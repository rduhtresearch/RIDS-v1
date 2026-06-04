repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(repo_dir, "R/utils/deployment_config.R"))
source(file.path(repo_dir, "R/utils/release_management.R"))

run_test_result <- function(label, fn) {
  result <- fn()
  failed <- result$failed %||% 0L

  if (!identical(as.integer(failed), 0L)) {
    stop(label, " failed with ", failed, " failing checks.")
  }

  invisible(result)
}

run_bootstrap_check <- function() {
  temp_root <- tempfile("rids_ci_runtime_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "deployment_config.R")
  config <- list(
    storage_mode = "duckdb",
    db_dir = file.path(temp_root, "data", "RIDS.duckdb"),
    ict_upload_dir = file.path(temp_root, "uploads"),
    edge_output_dir = file.path(temp_root, "outputs"),
    app_log_dir = file.path(temp_root, "logs"),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )

  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)

  write_deployment_config(config_path, config)
  run_release_smoke_check(repo_dir, config_path)
  message("Bootstrap check passed.")
}

run_working_tree_release_check <- function() {
  temp_root <- tempfile("rids_ci_release_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  config_path <- file.path(temp_root, "shared", "deployment_config.R")
  release_dir <- file.path(temp_root, "releases", default_bootstrap_release_version())

  config <- list(
    storage_mode = "duckdb",
    db_dir = file.path(temp_root, "shared", "data", "RIDS.duckdb"),
    ict_upload_dir = file.path(temp_root, "shared", "uploads"),
    edge_output_dir = file.path(temp_root, "shared", "outputs"),
    app_log_dir = file.path(temp_root, "shared", "logs"),
    app_host = "127.0.0.1",
    app_port = 3838L,
    sql_server = "",
    sql_database = "",
    sql_driver = ""
  )

  dir.create(dirname(config$db_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(config$ict_upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$edge_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$app_log_dir, recursive = TRUE, showWarnings = FALSE)

  write_deployment_config(config_path, config)
  export_working_tree_snapshot(repo_dir, release_dir, overwrite = TRUE)
  run_release_smoke_check(release_dir, config_path)
  message("Working-tree release bootstrap check passed.")
}

source(file.path(repo_dir, "R/addons/custom_activities/ca_build_custom_rows.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_schema.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_ref_activities.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_queries.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/ca_assign_edge_keys.R"), local = FALSE)
source(file.path(repo_dir, "R/addons/custom_activities/apply_custom_activities.R"), local = FALSE)

run_bootstrap_check()
run_working_tree_release_check()

source(file.path(repo_dir, "R/tests/test_ca_build_custom_rows.R"), local = FALSE)
run_test_result("test_ca_build_custom_rows.R", run_ca_tests)

source(file.path(repo_dir, "R/tests/test_ca_ref_activities.R"), local = FALSE)
run_test_result("test_ca_ref_activities.R", run_ca_ref_tests)

source(file.path(repo_dir, "R/tests/test_ca_queries.R"), local = FALSE)
run_test_result("test_ca_queries.R", run_ca_query_tests)

source(file.path(repo_dir, "R/tests/test_ca_chunk3.R"), local = FALSE)
run_test_result("test_ca_chunk3.R", run_ca_chunk3_tests)

source(file.path(repo_dir, "R/tests/test_contract_cost_source_of_truth.R"), local = FALSE)
run_test_result("test_contract_cost_source_of_truth.R", run_contract_cost_source_of_truth_tests)

message("All CI checks passed.")
