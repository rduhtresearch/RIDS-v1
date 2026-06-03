suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

.SCREENING_FAILURE_SUFFIX <- " - SCREENING VISIT"

#' Duplicate the earliest visit for each main EDGE arm as a screening failure.
#'
#' This is the only place the pipeline creates Screening Failure rows. By
#' rewriting the duplicated rows' sheet/arm identity here, the rest of the EDGE
#' export flow can treat them like ordinary main-arm records with no downstream
#' special case.
#'
#' @param data Posting-line input prior to Step 4 adjustment / EDGE build.
#' @param include_screening_failure Boolean flag from Step 1.
#' @return Original rows plus screening-failure duplicates when enabled.
duplicate_screening_failure_rows <- function(data, include_screening_failure = FALSE) {
  if (!isTRUE(include_screening_failure) || nrow(data) == 0) {
    return(data)
  }

  .SPECIAL_SHEETS <- c("Unscheduled Activities", "Setup & Closedown", "Pharmacy")
  .CUSTOM_SHEET   <- "Custom Activities"
  .ITEMISED_ARMS  <- c("SSP")

  required <- c("sheet_name", "Study_Arm", "Visit")
  missing  <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      "duplicate_screening_failure_rows(): missing columns: ",
      paste(missing, collapse = ", ")
    )
  }

  main_rows <- data |>
    filter(
      !sheet_name %in% .SPECIAL_SHEETS,
      sheet_name != .CUSTOM_SHEET
    ) |>
    mutate(
      template_arm = if_else(Study_Arm %in% .ITEMISED_ARMS, sheet_name, Study_Arm)
    ) |>
    filter(
      !is.na(template_arm),
      nzchar(trimws(template_arm)),
      !str_ends(template_arm, fixed(.SCREENING_FAILURE_SUFFIX))
    )

  if (nrow(main_rows) == 0) {
    return(data)
  }

  first_visits <- main_rows |>
    distinct(template_arm, Visit) |>
    mutate(
      visit_number = suppressWarnings(as.numeric(str_extract(Visit, "\\d+"))),
      visit_sort_missing = is.na(visit_number)
    ) |>
    arrange(template_arm, visit_sort_missing, visit_number, Visit) |>
    group_by(template_arm) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(template_arm, Visit)

  duplicates <- main_rows |>
    semi_join(first_visits, by = c("template_arm", "Visit")) |>
    mutate(
      screening_template_arm = paste0(template_arm, .SCREENING_FAILURE_SUFFIX),
      # Rewriting the duplicate rows' template identity here ensures posting
      # lines, joins, EDGE keys, and template tabs all treat screening failures
      # as ordinary main-arm records without any downstream branching.
      sheet_name = screening_template_arm,
      Study_Arm = if_else(
        Study_Arm %in% .ITEMISED_ARMS,
        Study_Arm,
        screening_template_arm
      )
    ) |>
    select(all_of(names(data)))

  bind_rows(data, duplicates)
}
