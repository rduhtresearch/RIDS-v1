source("R/utils/add_study_arm.r")
source("R/utils/pipeline_fixed.r")
source("R/utils/posting_test.r")
source("R/utils/template_build_main.r")
source("R/utils/adjust.r")
source("R/utils/build_template.r")
source("R/utils/posting_lines.r")

# # Pipeline call sequence:
# posting_lines_adjusted <- adjust_posting_lines(out)
# edge_templates         <- build_all_edge_templates(posting_lines_adjusted)


#input_file <- '/Users/tategraham/Downloads/(59904) - AriBio - POLARIS-AD - Final to agree to CTA.xlsx'
input_file <- '/Users/tategraham/Documents/NHS/R scripts/Refactor/testing_data/candy study.xlsx'

pw <- process_workbook(
  input_path = input_file,
  db_path    = DB_DIR
)

out <- generate_posting_plan(
  ict           = pw,
  rules_db_path = DB_DIR,
  scenario_id   = "A",
  ict_db_path   = DB_DIR
)

posting_lines_adjusted <- adjust_posting_lines(out)
View(posting_lines_adjusted)
tm <- build_all_edge_templates(posting_lines_adjusted)
View(tm)

View(posting_lines_adjusted)

View(pw$`Unscheduled Activities`)


