suppressPackageStartupMessages(library(dplyr))

adjust_posting_lines <- function(out) {
  
  .ADJUSTMENT_SPECIAL <- c("Unscheduled Activities", "Setup & Closedown")
  
  .adjust <- function(df, group_vars) {
    df %>%
      mutate(contract_price = round(contract_cost, 0)) %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(
        base_sum        = sum(posting_amount, na.rm = TRUE),
        contract_price  = first(contract_price),
        multiplier      = if_else(base_sum == 0, NA_real_, contract_price / base_sum),
        adjusted_amount = if_else(base_sum == 0, 0, round(posting_amount * multiplier, 2))
      ) %>%
      mutate(
        residual        = round(contract_price - sum(adjusted_amount, na.rm = TRUE), 2),
        has_direct      = any(posting_line_type_id == "DIRECT"),
        is_residual_row = if_else(
          has_direct,
          posting_line_type_id == "DIRECT" & row_number() == min(which(posting_line_type_id == "DIRECT")),
          row_number() == 1L
        ),
        adjusted_amount = if_else(
          is_residual_row,
          round(adjusted_amount + residual, 2),
          adjusted_amount
        )
      ) %>%
      mutate(
        adjusted_sum_check = round(sum(adjusted_amount, na.rm = TRUE), 2),
        diff_check         = round(contract_price - adjusted_sum_check, 2)
      ) %>%
      select(-has_direct) %>%
      ungroup()
  }
  
  bind_rows(
    out %>%
      filter(sheet_name %in% .ADJUSTMENT_SPECIAL) %>%
      .adjust(c("row_id", "Activity", "staff_group", "scenario_id")),
    
    out %>%
      filter(!sheet_name %in% .ADJUSTMENT_SPECIAL) %>%
      mutate(adj_group = trimws(if_else(sheet_name == "Pharmacy", Study_Arm, sheet_name))) %>%
      .adjust(c("adj_group", "Visit", "scenario_id")) %>%
      select(-adj_group)
  )
}