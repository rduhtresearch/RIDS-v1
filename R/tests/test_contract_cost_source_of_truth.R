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

.make_ssp_rows <- function() {
  tibble(
    row_id = c(11L, 12L, 13L, 14L),
    Activity = c("Blood Test", "ECG", "Blood Test", "Main Visit Summary"),
    staff_group = c(1L, 1L, 2L, 1L),
    scenario_id = c("A", "A", "A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm"),
    Study_Arm = c("SSP", "SSP", "SSP", "Treatment Arm"),
    Visit = c("VISIT - 001", "VISIT - 001", "VISIT - 001", "VISIT - 001"),
    Visit_Label = c("Screening", "Screening", "Screening", "Screening"),
    posting_line_type_id = c("DIRECT", "DIRECT", "DIRECT", "DIRECT"),
    posting_amount = c(10, 20, 15, 100),
    contract_cost = c(25, 40, 35, 200),
    adjusted_amount = c(25, 40, 35, 200),
    study_name = c("Study A", "Study A", "Study A", "Study A")
  )
}

run_contract_cost_source_of_truth_tests <- function() {
  cat("\n=== contract cost source-of-truth tests ===\n\n")
  .passed <<- 0L
  .failed <<- 0L

  source("R/utils/posting_lines.r")
  source("R/utils/template_build_main.r")
  source("R/utils/adjust.r")
  source("R/utils/assign_edge_keys.R")
  source("R/utils/build_template.r")
  source("R/utils/screening_failure_transform.R")
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

  adjusted_ssp <- adjust_posting_lines(.make_ssp_rows())
  ssp_rows <- adjusted_ssp %>% filter(Study_Arm == "SSP") %>% arrange(row_id)
  .expect("SSP rows keep separate saved contract costs",
          isTRUE(all.equal(unname(ssp_rows$contract_price), c(25, 40, 35))))
  .expect("SSP rows adjust to their own saved totals",
          isTRUE(all.equal(unname(round(ssp_rows$adjusted_amount, 2)), c(25, 40, 35))))

  cat("\n[ template_build_main alignment ]\n")
  adjusted_template <- adjust_postings(.make_adjust_rows(123.45), c("Study_Arm", "Visit", "scenario_id"))
  .expect("template_build_main uses exact saved contract cost",
          identical(adjusted_template$contract_price[[1]], 123.45))
  .expect("template_build_main totals reconcile to saved pence value",
          identical(round(sum(adjusted_template$adjusted_amount), 2), 123.45))

  cat("\n[ active EDGE template build ]\n")
  ssp_keyed <- assign_edge_keys(adjusted_ssp)
  ssp_template <- build_all_edge_templates(
    ssp_keyed,
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = c("SSP", "Treatment Arm"),
      Visit_Label = c("VISIT - 001 - Screening", "Screening"),
      Visit_Number = c("VISIT - 001", "VISIT - 001")
    ),
    edge_id = "EDGE-PROJ-1"
  )
  treatment_rows_built <- ssp_template[["Treatment Arm"]] %>% arrange(`Cost Item Description`)
  ssp_item_rows_built <- treatment_rows_built %>% filter(grepl("Blood Test|ECG", `Cost Item Description`))
  scheduled_rows_built <- treatment_rows_built %>% filter(!grepl("Blood Test|ECG", `Cost Item Description`))

  .expect("SSP repeated items on the same visit get separate EDGE keys",
          dplyr::n_distinct(ssp_keyed$edge_key[ssp_keyed$Study_Arm == "SSP"]) == 3L)
  .expect("SSP rows merge into the main arm template",
          !("SSP" %in% names(ssp_template)) && "Treatment Arm" %in% names(ssp_template))
  .expect("main arm template shows one row per source SSP item",
          nrow(ssp_item_rows_built) == 3L)
  .expect("merged SSP rows include the item names",
          all(grepl("Blood Test|ECG", ssp_item_rows_built$`Cost Item Description`)))
  .expect("SSP rows do not duplicate the visit prefix",
          !any(grepl("^VISIT - 001 - VISIT - 001", ssp_item_rows_built$`Cost Item Description`)))
  .expect("non-SSP scheduled rows still roll up by visit",
          nrow(scheduled_rows_built) == 1L &&
            identical(scheduled_rows_built$`Default Cost`[[1]], 200))
  .expect("main arm rows do not repeat a visit-only label",
          identical(
            build_edge_template_main(tibble(
              study_name = "Study A",
              Visit = "VISIT - 001",
              Study_Arm = "Treatment Arm",
              Visit_Label = "VISIT - 001",
              adjusted_amount = 50,
              sheet_name = "Treatment Arm",
              edge_key = "EDGE-0001"
            ))$`Cost Item Description`,
            "VISIT - 001"
          ))
  lookup_recovery_template <- build_all_edge_templates(
    assign_edge_keys(adjust_posting_lines(tibble(
      row_id = 1L,
      Activity = "Visit Summary",
      staff_group = 1L,
      scenario_id = "A",
      sheet_name = "Treatment Arm",
      Study_Arm = "Treatment Arm",
      Visit = "VISIT - 001",
      Visit_Label = "VISIT - 001",
      posting_line_type_id = "DIRECT",
      posting_amount = 100,
      contract_cost = 100,
      adjusted_amount = 100,
      study_name = "Study A"
    ))),
    visit_lookup = tibble(
      Study = "Study A",
      Study_Arm = "Treatment Arm",
      Visit_Label = "Screening",
      Visit_Number = "VISIT - 001"
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("main arm rows recover visit label from lookup when posting data only has visit number",
          identical(
            lookup_recovery_template[["Treatment Arm"]]$`Cost Item Description`,
            "VISIT - 001 - Screening"
          ))
  renumber_input <- tibble(
    row_id = c(1L, 2L, 3L, 4L, 5L),
    Activity = c("Visit Summary", "Visit Summary", "Visit Summary", "Questionnaire", "ECG"),
    staff_group = c(1L, 1L, 1L, 1L, 1L),
    scenario_id = c("A", "A", "A", "A", "A"),
    sheet_name = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm", "Treatment Arm"),
    Study_Arm = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "SSP", "SSP"),
    Visit = c("VISIT - 001", "VISIT - 004", "VISIT - 006", "VISIT - 002", "VISIT - 003"),
    Visit_Label = c("Screening", "Week 12", "Week 48", "Week 4", "Week 8"),
    posting_line_type_id = c("DIRECT", "DIRECT", "DIRECT", "DIRECT", "DIRECT"),
    posting_amount = c(100, 120, 140, 20, 30),
    contract_cost = c(100, 120, 140, 20, 30),
    adjusted_amount = c(100, 120, 140, 20, 30),
    study_name = c("Study A", "Study A", "Study A", "Study A", "Study A")
  )
  renumber_template <- build_all_edge_templates(
    assign_edge_keys(adjust_posting_lines(renumber_input)),
    visit_lookup = tibble(
      Study = c("Study A", "Study A", "Study A", "Study A", "Study A"),
      Study_Arm = c("Treatment Arm", "Treatment Arm", "Treatment Arm", "SSP", "SSP"),
      Visit_Label = c("Screening", "Week 12", "Week 48", "Week 4", "Week 8"),
      Visit_Number = c("VISIT - 001", "VISIT - 004", "VISIT - 006", "VISIT - 002", "VISIT - 003")
    ),
    edge_id = "EDGE-PROJ-1"
  )
  .expect("scheduled main visits are renumbered densely before SSP rows",
          identical(
            renumber_template[["Treatment Arm"]]$`Cost Item Description`,
            c(
              "VISIT - 001 - Screening",
              "VISIT - 002 - Week 12",
              "VISIT - 003 - Week 48",
              "VISIT - 004 - Week 4 - Questionnaire",
              "VISIT - 005 - Week 8 - ECG"
            )
          ))

  cat("\n[ screening failure duplication ]\n")
  screening_input <- tibble(
    row_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    scenario_id = rep("A", 6),
    row_category_auto = rep("BASELINE", 6),
    calc_tag = NA_character_,
    row_category = rep("BASELINE", 6),
    is_medic = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    cpms_id = rep("CP1", 6),
    study_site = rep("RDUHT", 6),
    study_name = rep("Study A", 6),
    Study_Arm = c("Arm A", "Arm A", "Arm B", "Arm B", "SSP", "SSP"),
    Activity = c("Visit Summary", "Visit Summary", "Visit Summary", "Visit Summary", "Blood Test", "Blood Test"),
    Visit = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 003", "VISIT - 001", "VISIT - 002"),
    posting_line_type_id = rep("DIRECT", 6),
    posting_amount = c(100, 120, 200, 240, 25, 30),
    destination_bucket = rep("DEST_RD", 6),
    destination_entity = rep("R&D", 6),
    cost_code = NA_character_,
    sheet_name = c("Arm A", "Arm A", "Arm B", "Arm B", "Arm A", "Arm A"),
    Visit_Label = c("Screening", "Follow-up", "Baseline", "Visit 3", "Screening", "Follow-up"),
    activity_occurrence_id = c("AO1", "AO2", "BO1", "BO3", "SO1", "SO2"),
    staff_group = c(1L, 1L, 1L, 1L, 2L, 2L),
    contract_cost = c(100, 120, 200, 240, 25, 30),
    Department = c("Dept A", "Dept A", "Dept B", "Dept B", "Lab", "Lab"),
    Staff.Role = c("Nurse", "Nurse", "Coordinator", "Coordinator", "Medical Staff", "Medical Staff"),
    activity_type = rep("Visit", 6),
    time_required = c(30, 45, 40, 50, 20, 20)
  )

  screening_disabled <- duplicate_screening_failure_rows(
    screening_input,
    include_screening_failure = FALSE
  )
  .expect("disabled duplication leaves rows unchanged",
          identical(screening_disabled, screening_input))

  screening_enabled <- duplicate_screening_failure_rows(
    screening_input,
    include_screening_failure = TRUE
  )
  screening_only <- screening_enabled %>%
    filter(grepl("SCREENING VISIT$", sheet_name)) %>%
    arrange(sheet_name, Study_Arm, row_id)

  .expect("enabled duplication adds one extra first-visit block per main arm template",
          nrow(screening_enabled) == nrow(screening_input) + 3L)
  .expect("duplicated rows come from the earliest canonical visit per main arm",
          identical(
            screening_only %>% distinct(sheet_name, Visit) %>% pull(Visit),
            c("VISIT - 001", "VISIT - 001")
          ))
  .expect("non-itemised duplicated rows get the screening arm identity",
          all(
            screening_only %>%
              filter(!Study_Arm %in% "SSP") %>%
              pull(Study_Arm) %in% c(
                "Arm A - SCREENING VISIT",
                "Arm B - SCREENING VISIT"
              )
          ))
  .expect("itemised duplicated rows stay itemised while moving into the screening template",
          all(
            screening_only %>%
              filter(Study_Arm == "SSP") %>%
              pull(sheet_name) == "Arm A - SCREENING VISIT"
          ))
  .expect("duplicated rows preserve visit labels and row-level source data",
          identical(
            screening_only %>%
              filter(Study_Arm != "SSP") %>%
              arrange(sheet_name, row_id) %>%
              pull(Visit_Label),
            c("Screening", "Baseline")
          ))

  screening_adjusted <- adjust_posting_lines(screening_enabled)
  screening_keyed <- assign_edge_keys(screening_adjusted)
  screening_templates <- build_all_edge_templates(
    screening_keyed,
    visit_lookup = tibble(
      Study = rep("Study A", 4),
      Study_Arm = c("Arm A", "Arm A", "Arm B", "Arm B"),
      Visit_Label = c("Screening", "Follow-up", "Baseline", "Visit 3"),
      Visit_Number = c("VISIT - 001", "VISIT - 002", "VISIT - 001", "VISIT - 003")
    ),
    edge_id = "EDGE-PROJ-1"
  )

  .expect("screening failure rows survive adjustment and EDGE key assignment",
          all(!is.na(screening_keyed$edge_key[grepl("SCREENING VISIT$", screening_keyed$sheet_name)])))
  .expect("screening failure templates are built as ordinary main-arm tabs",
          all(c("Arm A - SCREENING VISIT", "Arm B - SCREENING VISIT") %in% names(screening_templates)))
  .expect("screening templates reuse the original first-visit label in output",
          identical(
            screening_templates[["Arm A - SCREENING VISIT"]]$`Cost Item Description`,
            c("VISIT - 001 - Screening", "VISIT - 002 - Screening - Blood Test")
          ))

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .passed, "    FAILED: ", .failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .passed, failed = .failed))
}
