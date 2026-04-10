suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

build_all_edge_templates <- function(data) {
  
  .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
  
  .EDGE_COLS <- c(
    "EDGE Project ID",
    "Template Name",
    "Template Level (Project | Participant | ProjectSite)",
    "Project Arm (Participant only)",
    "Project Site Name (ProjectSite only)",
    "Cost Item Description",
    "Analysis Code",
    "Cost Category",
    "Default Cost",
    "Currency",
    "Department",
    "Overhead Cost",
    "Time"
  )
  
  # ── Assign EDGE keys ──────────────────────────────────────────────────────────
  
  special_keys <- data |>
    filter(sheet_name %in% .SPECIAL_SHEETS) |>
    distinct(sheet_name, Activity, row_id, staff_group, Study_Arm) |>
    mutate(edge_key = paste0("EDGE-", str_pad(row_number(), width = 4, pad = "0")))
  
  main_keys <- data |>
    filter(!sheet_name %in% .SPECIAL_SHEETS) |>
    distinct(Study_Arm, Visit) |>
    mutate(edge_key = paste0("EDGE-", str_pad(
      row_number() + nrow(special_keys), width = 4, pad = "0"
    )))
  
  data <- data |>
    left_join(special_keys, by = c("sheet_name", "Activity", "row_id", "staff_group", "Study_Arm")) |>
    left_join(main_keys,    by = c("Study_Arm", "Visit")) |>
    mutate(edge_key = coalesce(edge_key.x, edge_key.y)) |>
    select(-edge_key.x, -edge_key.y)
  
  # ── Build templates ───────────────────────────────────────────────────────────
  
  .build_special <- function(df) {
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(Study_Arm, sheet_name, Activity, row_id, staff_group, edge_key)
      ) |>
      mutate(
        `EDGE Project ID`                                      = NA,
        `Template Name`                                        = Study_Arm,
        `Template Level (Project | Participant | ProjectSite)` = "Participant",
        `Project Arm (Participant only)`                       = NA,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = paste0("VISIT - ", str_replace_all(Activity, "\\.", " ")),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  
  .build_main <- function(df) {
    visit_keys <- df |>
      filter(sheet_name != "Pharmacy") |>
      distinct(Study_Arm, Visit, edge_key)
    
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(study_name, Visit, Study_Arm, Visit_Label)
      ) |>
      left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
      mutate(
        `EDGE Project ID`                                      = NA,
        `Template Name`                                        = Study_Arm,
        `Template Level (Project | Participant | ProjectSite)` = "Participant",
        `Project Arm (Participant only)`                       = NA,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = paste0("VISIT - ", str_replace_all(Visit_Label, "\\.", " ")),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  
  # ── Dispatch and return ───────────────────────────────────────────────────────
  
  special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
  main_data    <- data |> filter(!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy")
  
  special_list <- special_data |>
    group_by(sheet_name) |>
    group_map(~ .build_special(.x), .keep = TRUE) |>
    setNames(sort(unique(special_data$sheet_name)))
  
  main_list <- main_data |>
    group_by(Study_Arm) |>
    group_map(~ .build_main(.x), .keep = TRUE) |>
    setNames(sort(unique(main_data$Study_Arm)))
  
  c(special_list, main_list)
}