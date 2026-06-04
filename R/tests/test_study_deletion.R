# ==============================================================================
# tests/test_study_deletion.R
#
# Focused tests for deleting a study run and its saved artifacts.
#
# Usage:
#   source("R/utils/study_deletion.R")
#   source("R/tests/test_study_deletion.R")
#   run_study_deletion_tests()
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

.passed <- 0L
.failed <- 0L

.expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .failed <<- .failed + 1L
  }
}

.expect_error <- function(label, expr) {
  err <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (!is.null(err)) {
    cat("  PASS  ", label, "  (errored as expected)\n", sep = "")
    .passed <<- .passed + 1L
  } else {
    cat("  FAIL  ", label, "  (expected error, got none)\n", sep = "")
    .failed <<- .failed + 1L
  }
}

run_study_deletion_tests <- function() {
  cat("\n=== study deletion tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit({
    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  dbExecute(con, "
    CREATE TABLE meta_data (
      id INTEGER,
      cpms_id VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR,
      saved_file_path VARCHAR,
      edge_zip_path VARCHAR
    )
  ")
  dbExecute(con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR
    )
  ")
  dbExecute(con, "
    CREATE TABLE posting_lines (
      cpms_id VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR
    )
  ")
  dbExecute(con, "
    CREATE TABLE addon_custom_activities (
      cpms_id VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR
    )
  ")
  dbExecute(con, "
    CREATE TABLE app_logs (
      upload_id VARCHAR,
      cpms_id VARCHAR
    )
  ")

  workbook_path <- tempfile(fileext = ".xlsx")
  zip_path <- tempfile(fileext = ".zip")
  writeLines("workbook", workbook_path)
  writeLines("zip", zip_path)

  dbExecute(
    con,
    "INSERT INTO meta_data (id, cpms_id, study_site, scenario_id, saved_file_path, edge_zip_path) VALUES (?, ?, ?, ?, ?, ?), (?, ?, ?, ?, ?, ?)",
    params = list(
      101L, "CP1", "RDUHT", "A", workbook_path, zip_path,
      202L, "CP1", "NDDHT", "B", NA_character_, NA_character_
    )
  )
  dbExecute(
    con,
    "INSERT INTO ict_costing_tbl (CPMS_ID, study_site, scenario_id) VALUES (?, ?, ?), (?, ?, ?)",
    params = list("CP1", "RDUHT", "A", "CP1", "NDDHT", "B")
  )
  dbExecute(
    con,
    "INSERT INTO posting_lines (cpms_id, study_site, scenario_id) VALUES (?, ?, ?), (?, ?, ?)",
    params = list("CP1", "RDUHT", "A", "CP1", "NDDHT", "B")
  )
  dbExecute(
    con,
    "INSERT INTO addon_custom_activities (cpms_id, study_site, scenario_id) VALUES (?, ?, ?), (?, ?, ?)",
    params = list("CP1", "RDUHT", "A", "CP1", "NDDHT", "B")
  )
  dbExecute(
    con,
    "INSERT INTO app_logs (upload_id, cpms_id) VALUES (?, ?), (?, ?)",
    params = list("101", "CP1", "202", "CP1")
  )

  cat("[ run deletion ]\n")
  result <- delete_study_run("CP1", "RDUHT", "A", con = con)

  .expect("meta row deleted for target run",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_data WHERE cpms_id = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'")$n[[1]] == 0L)
  .expect("other meta row remains",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_data WHERE cpms_id = 'CP1' AND study_site = 'NDDHT' AND scenario_id = 'B'")$n[[1]] == 1L)
  .expect("ict rows deleted for target run",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM ict_costing_tbl WHERE CPMS_ID = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'")$n[[1]] == 0L)
  .expect("posting lines deleted for target run",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM posting_lines WHERE cpms_id = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'")$n[[1]] == 0L)
  .expect("custom activities deleted for target run",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM addon_custom_activities WHERE cpms_id = 'CP1' AND study_site = 'RDUHT' AND scenario_id = 'A'")$n[[1]] == 0L)
  .expect("only target upload logs are deleted",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM app_logs WHERE upload_id = '101'")$n[[1]] == 0L &&
            dbGetQuery(con, "SELECT COUNT(*) AS n FROM app_logs WHERE upload_id = '202'")$n[[1]] == 1L)
  .expect("saved workbook file is removed", !file.exists(workbook_path))
  .expect("saved EDGE zip is removed", !file.exists(zip_path))
  .expect("deleted file list includes both artifacts",
          length(result$files$deleted) == 2L)
  .expect("row counts are reported per table",
          identical(
            unname(unlist(result$counts)),
            c(1L, 1L, 1L, 1L, 1L)
          ))

  cat("\n[ identity validation ]\n")
  .expect_error("rejects blank scenario_id",
                delete_study_run("CP1", "RDUHT", "   ", con = con, delete_files = FALSE))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  invisible(list(passed = .passed, failed = .failed))
}
