suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

# ── Lookup tables ─────────────────────────────────────────────────────────────

DEPARTMENT_CC_LOOKUP <- tribble(
  ~Department,                      ~department_cost_centre,
  "CRDT Research nurse",            "50007",
  "CRF",                            "50112",
  "Cardiovascular",                 "03634",
  "Cherry Brook",                   "04134",
  "Clinical Chemistry",             "05644",
  "CFR",                            "50112",
  "Dietician",                      "23834",
  "Gastroenterology",               "15039",
  "Haematology",                    "06244",
  "Imaging Centre",                 "61105",
  "Microbiology",                   "06044",
  "Genetics",                       "02334",
  "Nuclear Medicine",               "04735",
  "Opthalmology",                   "13355",
  "Pathology",                      "05944",
  "Radiology",                      "06734",
  "Respritory",                     "22864",
  "Immunology Or Virology",         "06044",
  "Radiotherapy",                   "04734",
  "Inverventional Radiology",       "06739",
  "Dental Radiology",               "38495"
) %>%
  mutate(Department_clean = str_squish(Department))

SPECIALITY_CC_LOOKUP <- tribble(
  ~speciality,                    ~speciality_cost_centre,
  "Cardiology",                   "52000",
  "Paediatric",                   "53100",
  "Cancer",                       "58109",
  "Orthopedics & Rheumatology",   "59400",
  "Gastro",                       "61105",
  "Respiratory",                  "62100",
  "Renal",                        "63106",
  "Urology",                      "64100",
  "Stroke",                       "66206",
  "Geriatric",                    "66300",
  "Dendron",                      "67103",
  "Dermatology",                  "67302",
  "ED",                           "69101"
) %>%
  mutate(speciality_clean = str_squish(speciality))

# ── Public function ───────────────────────────────────────────────────────────

#' Attach cost_code (cost centre) to each posting line.
#'
#' Writes the resolved cost centre to the existing `cost_code` column on
#' posting_lines, using a row's posting_line_type_id, Staff_Role, and
#' Department to choose the lookup.
#'
#' @param posting_output  Posting lines dataframe, post `adjust_posting_lines()`
#'                        and post `rename(Staff_Role = Staff.Role)`.
#' @param study_speciality  The speciality NAME (string) for this study.
#'                          Pass `shared_state$speciality_name`.
#' @return  Same dataframe with `cost_code` populated. Working columns dropped.
add_cost_centres <- function(posting_output, study_speciality) {
  
  required <- c("Department", "Staff_Role", "posting_line_type_id")
  missing  <- setdiff(required, names(posting_output))
  if (length(missing) > 0) {
    stop("add_cost_centres(): missing columns: ", paste(missing, collapse = ", "))
  }
  
  posting_output %>%
    mutate(
      Department_clean = str_squish(Department),
      Staff_Role_clean = str_squish(Staff_Role),
      speciality_clean = str_squish(study_speciality)
    ) %>%
    left_join(
      DEPARTMENT_CC_LOOKUP %>% select(Department_clean, department_cost_centre),
      by = "Department_clean"
    ) %>%
    left_join(
      SPECIALITY_CC_LOOKUP %>% select(speciality_clean, speciality_cost_centre),
      by = "speciality_clean"
    ) %>%
    mutate(
      cost_code = case_when(
        posting_line_type_id == "CAPACITY_RD"          ~ "50000",
        posting_line_type_id == "INDIRECT_25_TRUST"    ~ "50000",
        posting_line_type_id == "INDIRECT_50_DELIVERY" ~ "50007",
        
        posting_line_type_id == "DIRECT" &
          Staff_Role_clean %in% c("Admin/Data Entry", "Nursing/Manager") &
          Department_clean == "Study Team"             ~ "50007",
        
        posting_line_type_id == "DIRECT" &
          Staff_Role_clean == "Medical Staff"          ~ department_cost_centre,
        
        posting_line_type_id == "INDIRECT_25_PI"       ~ speciality_cost_centre,
        
        TRUE ~ NA_character_
      )
    ) %>%
    select(-Department_clean, -Staff_Role_clean, -speciality_clean,
           -department_cost_centre, -speciality_cost_centre)
}