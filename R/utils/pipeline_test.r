source("R/utils/add_study_arm.r")
source("R/utils/pipeline_fixed.r")
source("R/utils/posting_test.r")
source("R/utils/template_build_main.r")

# input_file <- '/Users/tategraham/Downloads/(59904) - AriBio - POLARIS-AD - Final to agree to CTA.xlsx'
# 
# pw <- process_workbook(
#   input_path  = input_file,
#   archive_dir = NULL,   # e.g. "/path/to/archive"
#   export_path = NULL,   # e.g. "/path/to/output.xlsx"
#   db_dir      = dirname(input_file)
# )

#input_file <- '/Users/tategraham/Downloads/(59904) - AriBio - POLARIS-AD - Final to agree to CTA.xlsx'
input_file <- '/Users/tategraham/Documents/NHS/R scripts/Refactor/testing_data/candy study.xlsx'

pw <- process_workbook(
  input_path = input_file,
  db_path    = DB_DIR
)

View(pw)


