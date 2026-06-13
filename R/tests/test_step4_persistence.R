.step4_passed <- 0L
.step4_failed <- 0L

.step4_expect <- function(label, condition) {
  if (isTRUE(condition)) {
    cat("  PASS  ", label, "\n", sep = "")
    .step4_passed <<- .step4_passed + 1L
  } else {
    cat("  FAIL  ", label, "\n", sep = "")
    .step4_failed <<- .step4_failed + 1L
  }
}

run_step4_persistence_tests <- function() {
  cat("\n=== step4 persistence tests ===\n\n")
  .step4_passed <<- 0L
  .step4_failed <<- 0L

  original_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  edited_templates <- list(
    "Arm A" = data.frame(
      `Template Name` = "Arm A (Edited)",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )

  selected_edited <- step4_templates_for_export(
    edited_templates = edited_templates,
    original_templates = original_templates
  )
  .step4_expect(
    "edited templates are preferred for final persistence",
    identical(selected_edited, edited_templates)
  )

  selected_original <- step4_templates_for_export(
    edited_templates = NULL,
    original_templates = original_templates
  )
  .step4_expect(
    "original templates are used when there are no edits",
    identical(selected_original, original_templates)
  )

  selected_empty_edits <- step4_templates_for_export(
    edited_templates = list(),
    original_templates = original_templates
  )
  .step4_expect(
    "empty edited templates fall back to the original templates",
    identical(selected_empty_edits, original_templates)
  )

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("PASSED: ", .step4_passed, "    FAILED: ", .step4_failed, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")

  invisible(list(passed = .step4_passed, failed = .step4_failed))
}
