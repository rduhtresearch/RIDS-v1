suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
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

.make_adjust_rows <- function(contract_cost) {
  tibble(
    row_id = c(1L, 2L),
    Activity = c("Blood Test", "Blood Test"),
    staff_group = c(1L, 1L),
    scenario_id = c("A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm"),
    Study_Arm = c("Treatment", "Treatment"),
    Visit = c("VISIT - 001", "VISIT - 001"),
    posting_line_type_id = c("DIRECT", "INDIRECT"),
    posting_amount = c(100, 50),
    contract_cost = c(contract_cost, contract_cost)
  )
}

run_contract_cost_source_of_truth_tests <- function() {
  cat("\n=== contract cost source-of-truth tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  source("R/utils/posting_lines.r")
  source("R/utils/adjust.r")
  source("R/utils/template_build_main.r")
  source("R/utils/pipeline_fixed.r")

  db_path <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit({
    dbDisconnect(con, shutdown = TRUE)
    if (file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  dbExecute(con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR,
      Study VARCHAR,
      Visit_Number VARCHAR,
      Study_Arm VARCHAR,
      Visit_Label VARCHAR,
      Activity_Name VARCHAR,
      ICT_Cost DOUBLE,
      Contract_Cost DOUBLE,
      activity_occurrence_id VARCHAR,
      staff_group INTEGER
    )
  ")

  dbWriteTable(
    con,
    "ict_costing_tbl",
    tibble(
      CPMS_ID = c("CP1", "CP1", "CP1", "CP1"),
      study_site = c("RDUHT", "RDUHT", "NDDHT", "RDUHT"),
      scenario_id = c("A", "A", "A", "B"),
      Study = c("Study A", "Study A", "Study A", "Study A"),
      Visit_Number = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 001"),
      Study_Arm = c("Treatment", "Treatment", "Treatment", "Treatment"),
      Visit_Label = c("Screening", "Follow-up", "Screening", "Screening"),
      Activity_Name = c("Blood Test", NA_character_, "Blood Test", "Blood Test"),
      ICT_Cost = c(100.11, 200.22, 300.33, 400.44),
      Contract_Cost = c(123.45, 210.55, 333.33, 444.44),
      activity_occurrence_id = c("AO1", NA_character_, "AO2", "AO3"),
      staff_group = c(1L, 1L, 1L, 1L)
    ),
    append = TRUE
  )

  cat("[ join_ict_costs ]\n")
  joined_activity <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
  .expect("activity-level join uses saved Contract_Cost",
          identical(joined_activity$contract_cost[[1]], 123.45))
  .expect("join ignores other study_site rows for same CPMS",
          !identical(joined_activity$contract_cost[[1]], 333.33))
  .expect("join ignores other scenario rows for same site and CPMS",
          !identical(joined_activity$contract_cost[[1]], 444.44))

  joined_visit <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Visit = "VISIT - 002",
      Study_Arm = "Treatment",
      Activity = "MFF Summary",
      staff_group = 1L
    ),
    db_path
  )
  .expect("visit-level fallback uses saved Contract_Cost",
          identical(joined_visit$contract_cost[[1]], 210.55))

  joined_other_site <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "NDDHT",
      scenario_id = "A",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
  .expect("same CPMS joins to the active site only",
          identical(joined_other_site$contract_cost[[1]], 333.33))

  joined_other_scenario <- join_ict_costs(
    tibble(
      cpms_id = "CP1",
      study_site = "RDUHT",
      scenario_id = "B",
      Visit = "VISIT - 001",
      Study_Arm = "Treatment",
      Activity = "Blood Test",
      staff_group = 1L
    ),
    db_path
  )
  .expect("same CPMS and site joins to the active scenario only",
          identical(joined_other_scenario$contract_cost[[1]], 444.44))

  cat("\n[ persist_ict_to_duckdb ]\n")
  scoped_db_path <- tempfile(fileext = ".duckdb")
  scoped_con <- dbConnect(duckdb::duckdb(), dbdir = scoped_db_path)
  on.exit({
    if (inherits(scoped_con, "DBIConnection")) {
      dbDisconnect(scoped_con, shutdown = TRUE)
    }
    if (file.exists(scoped_db_path)) unlink(scoped_db_path)
  }, add = TRUE)

  dbExecute(scoped_con, "
    CREATE TABLE ict_costing_tbl (
      CPMS_ID VARCHAR,
      study_site VARCHAR,
      scenario_id VARCHAR,
      Study VARCHAR,
      Visit_Number VARCHAR,
      Study_Arm VARCHAR,
      Visit_Label VARCHAR,
      Activity_Name VARCHAR,
      ICT_Cost DOUBLE,
      Contract_Cost DOUBLE,
      activity_occurrence_id VARCHAR,
      staff_group INTEGER
    )
  ")
  dbDisconnect(scoped_con, shutdown = TRUE)

  persist_ict_to_duckdb(
    scoped_db_path,
    tibble(
      CPMS_ID = "CP1",
      study_site = "RDUHT",
      scenario_id = "A",
      Study = "Study A",
      Visit_Number = "VISIT - 001",
      Study_Arm = "Treatment",
      Visit_Label = "Screening",
      Activity_Name = "Blood Test",
      ICT_Cost = 100.11,
      activity_occurrence_id = "AO1",
      staff_group = 1L
    )
  )

  persist_ict_to_duckdb(
    scoped_db_path,
    tibble(
      CPMS_ID = c("CP1", "CP1"),
      study_site = c("NDDHT", "RDUHT"),
      scenario_id = c("A", "B"),
      Study = c("Study A", "Study A"),
      Visit_Number = c("VISIT - 001", "VISIT - 001"),
      Study_Arm = c("Treatment", "Treatment"),
      Visit_Label = c("Screening", "Screening"),
      Activity_Name = c("Blood Test", "Blood Test"),
      ICT_Cost = c(300.33, 400.44),
      activity_occurrence_id = c("AO2", "AO3"),
      staff_group = c(1L, 1L)
    )
  )

  scoped_con <- dbConnect(duckdb::duckdb(), dbdir = scoped_db_path, read_only = TRUE)
  persisted_rows <- dbGetQuery(
    scoped_con,
    "SELECT CPMS_ID, study_site, scenario_id FROM ict_costing_tbl ORDER BY study_site, scenario_id"
  )
  dbDisconnect(scoped_con, shutdown = TRUE)
  scoped_con <- NULL

  .expect("scoped persistence keeps separate site/scenario rows for same CPMS",
          nrow(persisted_rows) == 3L)
  .expect("saving one site does not delete the other site",
          any(persisted_rows$study_site == "NDDHT" & persisted_rows$scenario_id == "A"))
  .expect("saving one scenario does not delete the other scenario",
          any(persisted_rows$study_site == "RDUHT" & persisted_rows$scenario_id == "A") &&
            any(persisted_rows$study_site == "RDUHT" & persisted_rows$scenario_id == "B"))

  cat("\n[ adjust_posting_lines ]\n")
  adjusted_exact <- adjust_posting_lines(.make_adjust_rows(123.45))
  .expect("saved pence value is preserved as contract target",
          identical(adjusted_exact$contract_price[[1]], 123.45))
  .expect("adjusted totals reconcile to saved pence value",
          identical(round(sum(adjusted_exact$adjusted_amount), 2), 123.45))
  .expect("residual stays on the DIRECT row",
          isTRUE(adjusted_exact$is_residual_row[[1]]) &&
            isFALSE(adjusted_exact$is_residual_row[[2]]))

  adjusted_rounded <- adjust_posting_lines(.make_adjust_rows(123))
  .expect("whole-pound saved values remain whole-pound totals",
          identical(round(sum(adjusted_rounded$adjusted_amount), 2), 123))

  adjusted_missing <- adjust_posting_lines(.make_adjust_rows(NA_real_))
  .expect("missing contract cost propagates explicitly",
          all(is.na(adjusted_missing$contract_price)) &&
            all(is.na(adjusted_missing$adjusted_amount)) &&
            all(is.na(adjusted_missing$diff_check)))

  cat("\n[ template_build_main alignment ]\n")
  adjusted_template <- adjust_postings(.make_adjust_rows(123.45), c("Study_Arm", "Visit", "scenario_id"))
  .expect("template_build_main uses exact saved contract cost",
          identical(adjusted_template$contract_price[[1]], 123.45))
  .expect("template_build_main totals reconcile to saved pence value",
          identical(round(sum(adjusted_template$adjusted_amount), 2), 123.45))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .passed, failed = .failed))
}
