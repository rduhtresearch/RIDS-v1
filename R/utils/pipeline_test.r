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


input_file <- '/Users/tategraham/Downloads/(59904) - AriBio - POLARIS-AD - Final to agree to CTA.xlsx'
# input_file <- '/Users/tategraham/Documents/NHS/R scripts/Refactor/testing_data/candy study.xlsx'

pw <- process_workbook(
  input_path = input_file,
  db_path    = DB_DIR
)

a <- prepare_posting_input(
  ict = pw,
  scenario_id = "A",
  ict_db_path   = DB_DIR
)


b <- evaluate_posting_plan(
  prepared_df = a,
  rules_db_path = DB_DIR,
  scenario_id = "A",
)


# 
# View(pw$`AR1001 Treatment phase`)
# 
# out <- generate_posting_plan(
#   ict           = pw,
#   rules_db_path = DB_DIR,
#   scenario_id   = "A",
#   ict_db_path   = DB_DIR
# )
# 
# typeof(out)
# class(out)
# View(out)

posting_lines_adjusted <- adjust_posting_lines(b)
message(paste(names(posting_lines_adjusted), collapse = ", "))
message(paste(sapply(posting_lines_adjusted, class), collapse = ", "))

c <- posting_lines_adjusted |> filter(sheet_name == "AR1001 Treatment phase")
View(c)
View(posting_lines_adjusted)
tm <- build_all_edge_templates(posting_lines_adjusted)
View(tm)

con <- DBI::dbConnect(duckdb::duckdb(), "~/nhs_finance_app_data/RIDS.duckdb")
posting_lines <- DBI::dbGetQuery(con, "SELECT * FROM posting_lines") |> filter(study_name == "Candy Study")
View(posting_lines)

View(posting_lines_adjusted)

View(pw$`Unscheduled Activities`)


