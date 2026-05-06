suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

#' Assign EDGE keys to posting lines.
#'
#' Mints stable identifiers used to join posting line rows back to their EDGE
#' template rows. Two grouping rules:
#'   - Special sheets (Unscheduled / Setup & Closedown / Pharmacy):
#'     one key per (sheet_name, Activity, row_id, staff_group, Study_Arm)
#'   - Main sheets:
#'     one key per (Study_Arm, Visit)
#'
#' @param data  Posting lines dataframe, post adjust + cost-centres.
#' @return  Same dataframe with `edge_key` column populated.
assign_edge_keys <- function(data) {
  
  .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
  
  required <- c("sheet_name", "Activity", "row_id", "staff_group", "Study_Arm", "Visit")
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("assign_edge_keys(): missing columns: ", paste(missing, collapse = ", "))
  }
  
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
  
  data |>
    left_join(special_keys, by = c("sheet_name", "Activity", "row_id", "staff_group", "Study_Arm")) |>
    left_join(main_keys,    by = c("Study_Arm", "Visit")) |>
    mutate(edge_key = coalesce(edge_key.x, edge_key.y)) |>
    select(-edge_key.x, -edge_key.y)
}