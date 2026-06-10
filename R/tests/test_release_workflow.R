# ==============================================================================
# tests/test_release_workflow.R
#
# Focused regression tests for the working-tree-only release workflow.
# ======================================================================

.release_passed <- 0L
.release_failed <- 0L

.release_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .release_passed <<- .release_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .release_failed <<- .release_failed + 1L
  }
}

run_release_workflow_tests <- function() {
  cat("\n=== release workflow tests ===\n\n")
  .release_passed <<- 0L
  .release_failed <<- 0L

  repo_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  rscript_path <- Sys.which("Rscript")
  if (!nzchar(rscript_path)) {
    stop("Rscript was not found. These release workflow tests require Rscript.")
  }

  temp_root <- tempfile("rids_release_workflow_")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

  temp_repo <- file.path(temp_root, "repo")
  export_working_tree_snapshot(repo_dir, temp_repo, overwrite = TRUE)

  shared_dir <- file.path(temp_repo, "shared")
  releases_dir <- file.path(temp_repo, "releases")
  config_path <- file.path(shared_dir, "deployment_config.R")
  current_release_path <- file.path(shared_dir, "current_release.txt")
  script_path <- file.path(temp_repo, "R", "SETUP", "release_publish.R")

  dir.create(shared_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(releases_dir, recursive = TRUE, showWarnings = FALSE)

  config <- list(
    storage_mode = "duckdb",
    db_dir = normalizePath(file.path(shared_dir, "data", "RIDS.duckdb"), winslash = "/", mustWork = FALSE),
    ict_upload_dir = normalizePath(file.path(shared_dir, "uploads"), winslash = "/", mustWork = FALSE),
    edge_output_dir = normalizePath(file.path(shared_dir, "outputs"), winslash = "/", mustWork = FALSE),
    app_log_dir = normalizePath(file.path(shared_dir, "logs"), winslash = "/", mustWork = FALSE),
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

  run_release_command <- function(args) {
    original_wd <- getwd()
    on.exit(setwd(original_wd), add = TRUE)
    setwd(temp_repo)

    output <- suppressWarnings(system2(
      rscript_path,
      args = c(script_path, args),
      stdout = TRUE,
      stderr = TRUE
    ))

    list(
      status = attr(output, "status") %||% 0L,
      output = output
    )
  }

  cat("[ publish mode rejected ]\n")
  publish_result <- run_release_command(c("publish", "--version", "v0.5.0"))
  publish_output <- paste(publish_result$output, collapse = "\n")
  .release_expect("publish mode exits non-zero", publish_result$status != 0L)
  .release_expect(
    "publish mode explains manual labels",
    grepl("manual labels, not Git tags", publish_output, fixed = TRUE)
  )

  cat("\n[ publish-local and rollback ]\n")
  v041_result <- run_release_command(c("publish-local", "--version", "v0.4.1"))
  v050_result <- run_release_command(c("publish-local", "--version", "v0.5.0"))
  rollback_result <- run_release_command(c("rollback", "--version", "v0.4.1"))

  .release_expect("publish-local v0.4.1 succeeds", identical(v041_result$status, 0L))
  .release_expect("publish-local v0.5.0 succeeds", identical(v050_result$status, 0L))
  .release_expect("rollback succeeds", identical(rollback_result$status, 0L))
  .release_expect("v0.4.1 release folder created", dir.exists(file.path(releases_dir, "v0.4.1")))
  .release_expect("v0.5.0 release folder created", dir.exists(file.path(releases_dir, "v0.5.0")))
  .release_expect(
    "rollback repoints current release",
    identical(read_release_pointer(current_release_path), "v0.4.1")
  )

  deploy_log_path <- file.path(shared_dir, "deploy_log.tsv")
  deploy_log <- if (file.exists(deploy_log_path)) readLines(deploy_log_path, warn = FALSE) else character()
  .release_expect(
    "deploy log records working-tree publish",
    any(grepl("publish-local\tv0.5.0\tsuccess", deploy_log, fixed = TRUE))
  )
  .release_expect(
    "deploy log records rollback",
    any(grepl("rollback\tv0.4.1\tsuccess", deploy_log, fixed = TRUE))
  )

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .release_passed, "    FAILED: ", .release_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .release_passed, failed = .release_failed))
}
