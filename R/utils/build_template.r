# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
# })
# 
# build_all_edge_templates <- function(data) {
#   
#   .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
# # NOTE: new variables may need to be added in the future - this function should be param
#   
#   .EDGE_COLS <- c(
#     "EDGE Project ID",
#     "Template Name",
#     "Template Level (Project | Participant | ProjectSite)",
#     "Project Arm (Participant only)",
#     "Project Site Name (ProjectSite only)",
#     "Cost Item Description",
#     "Analysis Code",
#     "Cost Category",
#     "Default Cost",
#     "Currency",
#     "Department",
#     "Overhead Cost",
#     "Time", 
#     "Activity Type"
#   )
#   
#   # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
#   # the posting lines data already populated.
#   if (!"edge_key" %in% names(data)) {
#     stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
#          "Make sure assign_edge_keys() runs before this in the pipeline.")
#   }
#   
#   # ── Build templates ───────────────────────────────────────────────────────────
#   
#   .build_special <- function(df) {
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(Study_Arm, sheet_name, Activity, row_id, 
#                   staff_group, edge_key, Department, study_name, cpms_id)
#       ) |>
#       mutate(
#         `EDGE Project ID`                                      = cpms_id,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = "tbc",
#         `Project Arm (Participant only)`                       = Study_Arm,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = Department,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   .build_main <- function(df) {
#     visit_keys <- df |>
#       filter(sheet_name != "Pharmacy") |>
#       distinct(Study_Arm, Visit, edge_key)
#     
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(study_name, Visit, Study_Arm, Visit_Label)
#       ) |>
#       left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
#       mutate(
#         `EDGE Project ID`                                      = NA,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = "Participant",
#         `Project Arm (Participant only)`                       = NA,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = paste0("VISIT - ", str_replace_all(Visit_Label, "\\.", " ")),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = NA,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   # ── Dispatch and return ───────────────────────────────────────────────────────
#   
#   special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
#   main_data    <- data |> filter(!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy")
#   
#   special_list <- special_data |>
#     group_by(sheet_name) |>
#     group_map(~ .build_special(.x), .keep = TRUE) |>
#     setNames(sort(unique(special_data$sheet_name)))
#   
#   main_list <- main_data |>
#     group_by(Study_Arm) |>
#     group_map(~ .build_main(.x), .keep = TRUE) |>
#     setNames(sort(unique(main_data$Study_Arm)))
#   
#   c(special_list, main_list)
# }

# current 
# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
# })
# 
# build_all_edge_templates <- function(data, visit_lookup, edge_id) {
#   
#   .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
#   # NOTE: new variables may need to be added in the future - this function should be param
#   
#   .EDGE_COLS <- c(
#     "EDGE Project ID",
#     "Template Name",
#     "Template Level (Project | Participant | ProjectSite)",
#     "Project Arm (Participant only)",
#     "Project Site Name (ProjectSite only)",
#     "Cost Item Description",
#     "Analysis Code",
#     "Cost Category",
#     "Default Cost",
#     "Currency",
#     "Department",
#     "Overhead Cost",
#     "Time", 
#     "Activity Type"
#   )
#   
#   # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
#   # the posting lines data already populated.
#   if (!"edge_key" %in% names(data)) {
#     stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
#          "Make sure assign_edge_keys() runs before this in the pipeline.")
#   }
#   
#   if (missing(visit_lookup) || is.null(visit_lookup) || nrow(visit_lookup) == 0) {
#     stop("build_all_edge_templates(): 'visit_lookup' is required and must contain ",
#          "Study, Study_Arm, Visit_Label, Visit_Number.")
#   }
#   
#   # ── Build templates ───────────────────────────────────────────────────────────
#   
#   .build_special <- function(df) {
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(Study_Arm, sheet_name, Activity, row_id, 
#                   staff_group, edge_key, Department, study_name, cpms_id)
#       ) |>
#       mutate(
#         `EDGE Project ID`                                      = edge_id,
#         `Template Name`                                        = sheet_name,
#         `Template Level (Project | Participant | ProjectSite)` = NA,
#         `Project Arm (Participant only)`                       = sheet_name,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = Department,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   .build_main <- function(df) {
#     visit_keys <- df |>
#       filter(sheet_name != "Pharmacy") |>
#       distinct(Study_Arm, Visit, edge_key)
#     
#     df |>
#       summarise(
#         total = sum(adjusted_amount, na.rm = TRUE),
#         .by   = c(study_name, Visit, Study_Arm)
#       ) |>
#       left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
#       left_join(
#         visit_lookup |> select(Study, Study_Arm, Visit_Number, Visit_Label),
#         by = c("study_name" = "Study", "Study_Arm", "Visit" = "Visit_Number")
#       ) |>
#       arrange(Visit) |>
#       mutate(
#         `EDGE Project ID`                                      = edge_id,
#         `Template Name`                                        = Study_Arm,
#         `Template Level (Project | Participant | ProjectSite)` = NA,
#         `Project Arm (Participant only)`                       = NA,
#         `Project Site Name (ProjectSite only)`                 = NA,
#         `Cost Item Description`                                = paste0(
#           Visit, " - ", str_replace_all(Visit_Label, "\\.", " ")
#         ),
#         `Analysis Code`                                        = edge_key,
#         `Cost Category`                                        = "Research Cost",
#         `Default Cost`                                         = total,
#         `Currency`                                             = "GBP",
#         `Department`                                           = NA,
#         `Overhead Cost`                                        = NA,
#         `Time`                                                 = NA,
#         `Activity Type`                                        = NA
#       ) |>
#       select(all_of(.EDGE_COLS))
#   }
#   
#   # ── Dispatch and return ───────────────────────────────────────────────────────
#   
#   special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
#   main_data    <- data |> filter(!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy")
#   
#   special_list <- special_data |>
#     group_by(sheet_name) |>
#     group_map(~ .build_special(.x), .keep = TRUE) |>
#     setNames(sort(unique(special_data$sheet_name)))
#   
#   main_list <- main_data |>
#     group_by(Study_Arm) |>
#     group_map(~ .build_main(.x), .keep = TRUE) |>
#     setNames(sort(unique(main_data$Study_Arm)))
#   
#   c(special_list, main_list)
# }
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

build_all_edge_templates <- function(data, visit_lookup, edge_id) {
  
  .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
  .CUSTOM_SHEET   <- "Custom Activities"
  # NOTE: new variables may need to be added in the future - this function should be param
  
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
    "Time", 
    "Activity Type"
  )
  
  # NOTE: edge_key is assigned upstream by assign_edge_keys() and arrives on
  # the posting lines data already populated.
  if (!"edge_key" %in% names(data)) {
    stop("build_all_edge_templates(): incoming data is missing 'edge_key'. ",
         "Make sure assign_edge_keys() runs before this in the pipeline.")
  }
  
  if (missing(visit_lookup) || is.null(visit_lookup) || nrow(visit_lookup) == 0) {
    stop("build_all_edge_templates(): 'visit_lookup' is required and must contain ",
         "Study, Study_Arm, Visit_Label, Visit_Number.")
  }
  
  # ── Build templates ───────────────────────────────────────────────────────────
  
  .build_special <- function(df) {
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(Study_Arm, sheet_name, Activity, row_id, 
                  staff_group, edge_key, Department, study_name, cpms_id)
      ) |>
      mutate(
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = sheet_name,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = sheet_name,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = Department,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  
  # ── ADDON ── Build custom-activity rows in the chosen Study_Arm template ──
  # Custom rows are user-added activities that should appear inside the
  # selected arm's template alongside the visit rows. One EDGE row per
  # custom_activity (i.e. per edge_key), summing the slot amounts. Description
  # comes from the user-entered Activity name.
  .build_custom <- function(df) {
    df |>
      filter(sheet_name == .CUSTOM_SHEET) |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(Study_Arm, Activity, edge_key, study_name, cpms_id)
      ) |>
      mutate(
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = Study_Arm,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = NA,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = str_replace_all(Activity, "\\.", " "),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  # ──────────────────────────────────────────────────────────────────────────
  
  .build_main <- function(df) {
    # Custom rows are excluded from the visit-style build path — they're
    # handled separately by .build_custom() and merged below.
    df <- df |> filter(sheet_name != .CUSTOM_SHEET)
    
    if (nrow(df) == 0) {
      # Empty arm (everything was custom) — return an empty template with
      # the right column shape so downstream code doesn't break.
      return(
        tibble::tibble(!!!setNames(rep(list(character(0)), length(.EDGE_COLS)), .EDGE_COLS))
      )
    }
    
    visit_keys <- df |>
      filter(sheet_name != "Pharmacy") |>
      distinct(Study_Arm, Visit, edge_key)
    
    df |>
      summarise(
        total = sum(adjusted_amount, na.rm = TRUE),
        .by   = c(study_name, Visit, Study_Arm)
      ) |>
      left_join(visit_keys, by = c("Study_Arm", "Visit")) |>
      left_join(
        visit_lookup |> select(Study, Study_Arm, Visit_Number, Visit_Label),
        by = c("study_name" = "Study", "Study_Arm", "Visit" = "Visit_Number")
      ) |>
      arrange(Visit) |>
      mutate(
        `EDGE Project ID`                                      = edge_id,
        `Template Name`                                        = Study_Arm,
        `Template Level (Project | Participant | ProjectSite)` = NA,
        `Project Arm (Participant only)`                       = NA,
        `Project Site Name (ProjectSite only)`                 = NA,
        `Cost Item Description`                                = paste0(
          Visit, " - ", str_replace_all(Visit_Label, "\\.", " ")
        ),
        `Analysis Code`                                        = edge_key,
        `Cost Category`                                        = "Research Cost",
        `Default Cost`                                         = total,
        `Currency`                                             = "GBP",
        `Department`                                           = NA,
        `Overhead Cost`                                        = NA,
        `Time`                                                 = NA,
        `Activity Type`                                        = NA
      ) |>
      select(all_of(.EDGE_COLS))
  }
  
  # ── Dispatch and return ───────────────────────────────────────────────────────
  
  special_data <- data |> filter(sheet_name %in% .SPECIAL_SHEETS)
  
  # Main data = everything that isn't a "special sheet" AND isn't custom,
  # plus Pharmacy (which appears in both buckets historically).
  # Custom rows are handled separately and merged into the per-arm output below.
  main_data <- data |> filter(
    (!sheet_name %in% .SPECIAL_SHEETS | sheet_name == "Pharmacy") &
      sheet_name != .CUSTOM_SHEET
  )
  
  custom_data <- data |> filter(sheet_name == .CUSTOM_SHEET)
  
  message("[CA] build_all_edge_templates: special rows=", nrow(special_data),
          " main rows=", nrow(main_data),
          " custom rows=", nrow(custom_data))
  
  special_list <- special_data |>
    group_by(sheet_name) |>
    group_map(~ .build_special(.x), .keep = TRUE) |>
    setNames(sort(unique(special_data$sheet_name)))
  
  main_list <- main_data |>
    group_by(Study_Arm) |>
    group_map(~ .build_main(.x), .keep = TRUE) |>
    setNames(sort(unique(main_data$Study_Arm)))
  
  message("[CA] special arms: ", paste(names(special_list), collapse = " / "))
  message("[CA] main arms: ",    paste(names(main_list),    collapse = " / "))
  
  # ── ADDON ── Merge custom rows into their selected arms' templates ────────
  # For each Study_Arm that has custom rows, build the custom block and
  # bind_rows() it onto the existing arm template. The arm could be in
  # main_list OR special_list (e.g. if the user picked "Pharmacy"), so we
  # try main first, then special, then create a new entry.
  if (nrow(custom_data) > 0) {
    custom_built <- .build_custom(custom_data)
    message("[CA] custom_built arms: ",
            paste(unique(custom_built$`Template Name`), collapse = " / "),
            " (", nrow(custom_built), " rows)")
    
    for (arm in unique(custom_built$`Template Name`)) {
      arm_custom <- custom_built |> filter(`Template Name` == arm)
      
      if (arm %in% names(main_list)) {
        message("[CA] merging ", nrow(arm_custom), " custom rows into main_list[['", arm, "']]")
        main_list[[arm]] <- bind_rows(main_list[[arm]], arm_custom)
      } else if (arm %in% names(special_list)) {
        message("[CA] merging ", nrow(arm_custom), " custom rows into special_list[['", arm, "']]")
        special_list[[arm]] <- bind_rows(special_list[[arm]], arm_custom)
      } else {
        message("[CA] creating new main_list entry for '", arm, "'")
        main_list[[arm]] <- arm_custom
      }
    }
  }
  # ──────────────────────────────────────────────────────────────────────────
  
  c(special_list, main_list)
}