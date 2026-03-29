# ==============================================================================
# SETUP & AUTHENTICATION
# ==============================================================================
source("R/utils/auth.r", local = FALSE)

# ==============================================================================
# 01_BUILD_RULES_DB_AH.R
# Purpose: Build + seed DuckDB rules DB for Scenarios A–H
# ==============================================================================

## Init database ---------------------------------------------------------------
init_db <- function() {
  cat("=== DB MAIN ===\n")
  tryCatch({
    dbGetQuery(CON, "SELECT 1")
    message("DB CONNECTION OK")
  }, error = function(e) {
    stop("DB ERROR: ", e$message)
  })
}

## ICT Cost data  --------------------------------------------------------------
ict_table <- function() {
  query <- c("
     CREATE TABLE IF NOT EXISTS ict_costing_tbl (
     CPMS_ID                VARCHAR,
     Study                  VARCHAR,
     Visit_Number           VARCHAR,
     Study_Arm              VARCHAR,
     Visit_Label            VARCHAR,
     Activity_Name          VARCHAR,
     ICT_Cost               DOUBLE,
     Contract_Cost          DOUBLE,
     activity_occurrence_id INTEGER,
     staff_group            INTEGER
     );"
  )
  
  dbExecute(CON, query)
}

## App data --------------------------------------------------------------------
meta_table <- function() {
  # 1. Define Table Schema
  query <- c(
     "DROP TABLE IF EXISTS ict_uploads;
      DROP TABLE IF EXISTS meta_data;    
      CREATE SEQUENCE IF NOT EXISTS upload_id_seq;
      CREATE TABLE IF NOT EXISTS meta_data (
        id               INTEGER PRIMARY KEY DEFAULT nextval('upload_id_seq'),
        scenario_id      VARCHAR,
        edge_id          VARCHAR,
        study_name       VARCHAR,
        notes            VARCHAR,
        uploaded_by      VARCHAR,
        upload_timestamp TIMESTAMP DEFAULT current_timestamp,
        original_filename VARCHAR,
        saved_file_path  VARCHAR
      );"
  )
  
  dbExecute(CON, query)
  message('meta built')
}
## User tables -----------------------------------------------------------------
user_tables <- function() {
  # 1. Define Table Schema
  queries <- c(
    "CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY,
      username TEXT UNIQUE,
      password_hash TEXT,
      role TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );"
  )
  
  # 4. Execute queries
  tryCatch({
    for (query in queries) {
      dbExecute(CON, query)
    }
    message("Database initialised successfully at ", DB_DIR)
  }, error = function(e) {
    stop("Failed to initialise database: ", e$message)
  })
}

## Populate user tables --------------------------------------------------------
seed_users <- function() {
  # 4. Check if the users table is already populated
  user_count <- dbGetQuery(CON, "SELECT COUNT(*) AS count FROM users")$count
  
  if (user_count == 0) {
    message("Seeding database with initial users...")
    
    # 5. Define the seed data
    seed_users <- data.frame(
      id = c(1, 2),
      username = c("admin_user", "dev_user"),
      password_hash = c(hash_password("admin123"), 
                        hash_password("dev123")),
      role = c("admin", "developer"),
      stringsAsFactors = FALSE
    )
    
    # 6. Insert the data
    dbAppendTable(CON, "users", seed_users)
    message("Seeding complete: Added ", nrow(seed_users), " users.")
  } else {
    message("Users table already contains data. Skipping seeding.")
  }
}

## Rules tables ----------------------------------------------------------------
build_rules_tables <- function() {
  cat("=== DB RULES ===\n")
  
  # 4) Helper to run SQL quickly
  exec_sql <- function(sql) dbExecute(CON, sql)
  
  # 5) Drop tables to rebuild cleanly
  exec_sql("DROP TABLE IF EXISTS dist_rules;")
  exec_sql("DROP TABLE IF EXISTS routing_rules;")
  exec_sql("DROP TABLE IF EXISTS amount_map;")
  exec_sql("DROP TABLE IF EXISTS provider_orgs;")
  exec_sql("DROP TABLE IF EXISTS posting_line_types;")
  exec_sql("DROP TABLE IF EXISTS rulesets;")
  
  # 6) Create tables
  # 6.1) Ruleset/version container
  exec_sql("
    CREATE TABLE rulesets (
      ruleset_id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      version TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      notes TEXT
    );
  ")
  
  # 6.2) Provider org list
  exec_sql("
    CREATE TABLE provider_orgs (
      provider_org TEXT PRIMARY KEY
    );
  ")
  
  # 6.3) Posting line types
  exec_sql("
    CREATE TABLE posting_line_types (
      posting_line_type_id TEXT PRIMARY KEY,
      label TEXT NOT NULL
    );
  ")
  
  # 6.4) Distribution rules
  exec_sql("
    CREATE TABLE dist_rules (
      dist_rule_id TEXT PRIMARY KEY,
      ruleset_id TEXT NOT NULL,
      scenario_id TEXT NOT NULL,
      row_category TEXT NOT NULL,
      condition_field TEXT,
      condition_op TEXT,
      condition_value TEXT,
      posting_line_type_id TEXT NOT NULL,
      priority INTEGER NOT NULL DEFAULT 100,
      notes TEXT,
      FOREIGN KEY (ruleset_id) REFERENCES rulesets(ruleset_id),
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  # 6.5) Amount mapping
  exec_sql("
    CREATE TABLE amount_map (
      posting_line_type_id TEXT PRIMARY KEY,
      base_mult DOUBLE NOT NULL,
      split_mult DOUBLE NOT NULL,
      applies_to_row_category TEXT NOT NULL,
      notes TEXT,
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  # 6.6) Routing rules
  exec_sql("
    CREATE TABLE routing_rules (
      routing_rule_id TEXT PRIMARY KEY,
      ruleset_id TEXT NOT NULL,
      scenario_id TEXT NOT NULL,
      condition_field TEXT,
      condition_op TEXT,
      condition_value TEXT,
      posting_line_type_id TEXT NOT NULL,
      destination_bucket TEXT NOT NULL,
      priority INTEGER NOT NULL DEFAULT 100,
      notes TEXT,
      FOREIGN KEY (ruleset_id) REFERENCES rulesets(ruleset_id),
      FOREIGN KEY (posting_line_type_id) REFERENCES posting_line_types(posting_line_type_id)
    );
  ")
  
  # 7) Seed base reference data
  exec_sql("
    INSERT INTO rulesets (ruleset_id, name, version, notes)
    VALUES ('COMM_AH_V1', 'Commercial Rules A–H', 'v1', 'A–H scenarios; MFF fixed at runtime param for MVP');
  ")
  
  exec_sql("
    INSERT INTO provider_orgs (provider_org) VALUES
    ('RDUHT'), ('CRF'), ('DPT'), ('UoE');
  ")
  
  exec_sql("
    INSERT INTO posting_line_types (posting_line_type_id, label) VALUES
    ('DIRECT', 'Direct Cost'),
    ('DIRECT_40_PI', 'Direct Cost 40% (PI)'),
    ('DIRECT_60_TEAM', 'Direct Cost 60% (Delivery/Team)'),
    ('CAPACITY_RD', 'Capacity (R&D)'),
    ('INDIRECT_50_DELIVERY', 'Indirect 50% (Delivery/Support)'),
    ('INDIRECT_25_TRUST', 'Indirect 25% (Trust Overhead)'),
    ('INDIRECT_25_PI', 'Indirect 25% (PI)');
  ")
  
  # 8) Seed amount_map (math parameters)
  exec_sql("INSERT INTO amount_map VALUES ('DIRECT', 1.0, 1.0, 'BOTH', 'AC * mff');")
  exec_sql("INSERT INTO amount_map VALUES ('CAPACITY_RD', 0.2, 1.0, 'BOTH', 'AC * 0.2 * mff');")
  exec_sql("INSERT INTO amount_map VALUES ('INDIRECT_50_DELIVERY', 0.7, 0.5, 'BASELINE', 'AC * 0.7 * mff * 0.5');")
  exec_sql("INSERT INTO amount_map VALUES ('INDIRECT_25_TRUST', 0.7, 0.25, 'BASELINE', 'AC * 0.7 * mff * 0.25');")
  exec_sql("INSERT INTO amount_map VALUES ('INDIRECT_25_PI', 0.7, 0.25, 'BASELINE', 'AC * 0.7 * mff * 0.25');")
  exec_sql("INSERT INTO amount_map VALUES ('DIRECT_40_PI', 1.0, 0.4, 'BASELINE', 'AC * mff * 0.4 (TRD medic split)');")
  exec_sql("INSERT INTO amount_map VALUES ('DIRECT_60_TEAM', 1.0, 0.6, 'BASELINE', 'AC * mff * 0.6 (TRD medic split)');")
  
  # 9) Seed dist_rules
  insert_dist_rule <- function(id, scenario, row_category, posting_line, priority,
                               condition_field = NA, condition_op = NA, condition_value = NA, notes = NA) {
    dbExecute(CON, "
      INSERT INTO dist_rules
        (dist_rule_id, ruleset_id, scenario_id, row_category,
         condition_field, condition_op, condition_value,
         posting_line_type_id, priority, notes)
      VALUES (?, 'COMM_AH_V1', ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      id, scenario, row_category,
      condition_field, condition_op, condition_value,
      posting_line, priority, notes
    ))
  }
  
  # 9.1) Rule Vectors
  baseline_std <- c("DIRECT", "CAPACITY_RD", "INDIRECT_50_DELIVERY", "INDIRECT_25_TRUST", "INDIRECT_25_PI")
  invest_std <- c("DIRECT", "CAPACITY_RD")
  training_std <- baseline_std
  
  # 9.2) Scenarios like A
  like_A <- c("A", "C", "E", "G", "H")
  for (sc in like_A) {
    pr <- 10
    for (pl in baseline_std) {
      insert_dist_rule(paste0(sc, "_BASE_", pl), sc, "BASELINE", pl, pr)
      pr <- pr + 10
    }
    pr <- 10
    for (pl in invest_std) {
      insert_dist_rule(paste0(sc, "_INV_", pl), sc, "INVESTIGATION", pl, pr)
      pr <- pr + 10
    }
    pr <- 10
    for (pl in training_std) {
      insert_dist_rule(paste0(sc, "_TRAIN_", pl), sc, "TRAINING_FEE", pl, pr)
      pr <- pr + 10
    }
  }
  
  # 9.3) TRD scenarios: B, D, F
  trd_scenarios <- c("B", "D", "F")
  for (sc in trd_scenarios) {
    pr <- 20
    for (pl in baseline_std) {
      insert_dist_rule(paste0(sc, "_BASE_NONMED_", pl), sc, "BASELINE", pl, pr,
                       condition_field = "is_medic", condition_op = "=", condition_value = "FALSE",
                       notes = "TRD scenario: non-medic baseline uses standard direct")
      pr <- pr + 10
    }
    pr_train <- 10
    for (pl in baseline_std) {
      insert_dist_rule(paste0(sc, "_TRAIN_", pl), sc, "TRAINING_FEE", pl, pr_train)
      pr_train <- pr_train + 10
    }
    insert_dist_rule(paste0(sc, "_BASE_MED_DIRECT40"), sc, "BASELINE", "DIRECT_40_PI", 5,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE",
                     notes = "TRD medic: 40% direct to PI")
    insert_dist_rule(paste0(sc, "_BASE_MED_DIRECT60"), sc, "BASELINE", "DIRECT_60_TEAM", 6,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE",
                     notes = "TRD medic: 60% direct to team")
    insert_dist_rule(paste0(sc, "_BASE_MED_CAP"), sc, "BASELINE", "CAPACITY_RD", 20,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I50"), sc, "BASELINE", "INDIRECT_50_DELIVERY", 30,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I25T"), sc, "BASELINE", "INDIRECT_25_TRUST", 40,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_BASE_MED_I25P"), sc, "BASELINE", "INDIRECT_25_PI", 50,
                     condition_field = "is_medic", condition_op = "=", condition_value = "TRUE")
    insert_dist_rule(paste0(sc, "_INV_DIRECT"), sc, "INVESTIGATION", "DIRECT", 10)
    insert_dist_rule(paste0(sc, "_INV_CAP"), sc, "INVESTIGATION", "CAPACITY_RD", 20)
  }
  
  # 10) Seed routing_rules
  insert_routing <- function(id, scenario, posting_line, dest_bucket, priority,
                             condition_field = NA, condition_op = NA, condition_value = NA, notes = NA) {
    dbExecute(CON, "
      INSERT INTO routing_rules
        (routing_rule_id, ruleset_id, scenario_id,
         condition_field, condition_op, condition_value,
         posting_line_type_id, destination_bucket, priority, notes)
      VALUES (?, 'COMM_AH_V1', ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      id, scenario,
      condition_field, condition_op, condition_value,
      posting_line, dest_bucket, priority, notes
    ))
  }
  
  # 10.1) Internal Routing
  internal_like <- c("A", "B", "C", "D", "E", "F")
  for (sc in internal_like) {
    insert_routing(paste0(sc, "_R_DIRECT"), sc, "DIRECT", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_D40"), sc, "DIRECT_40_PI", "DEST_PI_ORG", 10)
    insert_routing(paste0(sc, "_R_D60"), sc, "DIRECT_60_TEAM", "DEST_SUPPORT", 10)
    insert_routing(paste0(sc, "_R_CAP"), sc, "CAPACITY_RD", "DEST_RD", 10)
    insert_routing(paste0(sc, "_R_I50"), sc, "INDIRECT_50_DELIVERY", "DEST_SUPPORT", 10, notes = "50% indirect to support")
    insert_routing(paste0(sc, "_R_I25T"), sc, "INDIRECT_25_TRUST", "DEST_TRUST_OH", 10)
    insert_routing(paste0(sc, "_R_I25P"), sc, "INDIRECT_25_PI", "DEST_PI_ORG", 10)
  }
  
  # 10.2) External Routing
  external_like <- c("G", "H")
  for (sc in external_like) {
    insert_routing(paste0(sc, "_R_DIRECT"), sc, "DIRECT", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_CAP"), sc, "CAPACITY_RD", "DEST_RD", 10)
    insert_routing(paste0(sc, "_R_I50"), sc, "INDIRECT_50_DELIVERY", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_I25T"), sc, "INDIRECT_25_TRUST", "DEST_PROVIDER", 10)
    insert_routing(paste0(sc, "_R_I25P"), sc, "INDIRECT_25_PI", "DEST_PROVIDER", 10)
  }
  
  # 11) Audit summary
  cat("Built and seeded DB:", DB_DIR, "\n")
  cat("\nPosting line types:\n")
  print(dbGetQuery(CON, "SELECT * FROM posting_line_types ORDER BY posting_line_type_id;"))
  
  cat("\nDist rules count by scenario/category:\n")
  print(dbGetQuery(CON, "SELECT scenario_id, row_category, COUNT(*) AS n FROM dist_rules GROUP BY scenario_id, row_category ORDER BY scenario_id, row_category;"))
  
  cat("\nRouting rules count by scenario:\n")
  print(dbGetQuery(CON, "SELECT scenario_id, COUNT(*) AS n FROM routing_rules GROUP BY scenario_id ORDER BY scenario_id;"))
}

## Main Entry Point ------------------------------------------------------------
db_main <- function() {
  ict_table()
  meta_table()
  init_db()
  user_tables()
  seed_users()
  build_rules_tables()
}