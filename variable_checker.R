library(tidycensus)
library(tidyverse)


# ── Variable lookup tables ────────────────────────────────────────────────────

# Age (S0101) — subject table
age_brackets <- tibble(
  census_code = c(
    "S0101_C01_002","S0101_C01_003","S0101_C01_004","S0101_C01_005",
    "S0101_C01_006","S0101_C01_007","S0101_C01_008","S0101_C01_009",
    "S0101_C01_010","S0101_C01_011","S0101_C01_012","S0101_C01_013",
    "S0101_C01_014","S0101_C01_015","S0101_C01_016","S0101_C01_017",
    "S0101_C01_018","S0101_C01_019"
  ),
  template_label = c(
    "0 to 4","5 to 9","10 to 14","15 to 19",
    "20 to 24","25 to 29","30 to 34","35 to 39",
    "40 to 44","45 to 49","50 to 54","55 to 59",
    "60 to 64","65 to 69","70 to 74","75 to 79",
    "80 to 84","85+"
  )
)

# LEP households (S1602) — subject table
lep_brackets <- tibble(
  census_code    = c("S1602_C01_001",    "S1602_C03_001"),
  template_label = c("Total Households", "LEP Households")
)

# Languages spoken at home (C16001) — detailed table
lang_brackets <- tibble(
  census_code = c(
    "C16001_003","C16001_006","C16001_009","C16001_012",
    "C16001_015","C16001_018","C16001_021","C16001_024",
    "C16001_027","C16001_030","C16001_033","C16001_036"
  ),
  template_label = c(
    "Spanish","French, Haitian, or Cajun","German or other West Germanic",
    "Russian, Polish, or other Slavic","Other Indo-European","Korean",
    "Chinese (incl. Mandarin, Cantonese)","Vietnamese",
    "Tagalog (incl. Filipino)","Other Asian and Pacific Island",
    "Arabic","Other Languages"
  )
)

# Race (B02001) — detailed table
race_brackets <- tibble(
  census_code = c(
    "B02001_001","B02001_002","B02001_003","B02001_004",
    "B02001_005","B02001_006","B02001_007","B02001_008"
  ),
  template_label = c(
    "Total (denominator)",
    "White alone","Black or African American alone",
    "American Indian and Alaska Native alone","Asian alone",
    "Native Hawaiian and Other Pacific Islander alone",
    "Some other race alone","Two or more races"
  )
)

# Hispanic/Latino origin (B03003) — detailed table
hispanic_brackets <- tibble(
  census_code    = c("B03003_002",          "B03003_003"),
  template_label = c("Not Hispanic or Latino", "Hispanic or Latino")
)

# Household income (B19001) — detailed table
income_brackets <- tibble(
  census_code = c(
    "B19001_001","B19001_002","B19001_003","B19001_004","B19001_005",
    "B19001_006","B19001_007","B19001_008","B19001_009","B19001_010",
    "B19001_011","B19001_012","B19001_013","B19001_014","B19001_015",
    "B19001_016","B19001_017"
  ),
  template_label = c(
    "Total Households (denominator)",
    "Less than $10,000","$10,000 to $14,999","$15,000 to $19,999",
    "$20,000 to $24,999","$25,000 to $29,999","$30,000 to $34,999",
    "$35,000 to $39,999","$40,000 to $44,999","$45,000 to $49,999",
    "$50,000 to $59,999","$60,000 to $74,999","$75,000 to $99,999",
    "$100,000 to $124,999","$125,000 to $149,999",
    "$150,000 to $199,999","$200,000 or more"
  )
)

# Poverty status (B17001) — detailed table
poverty_brackets <- tibble(
  census_code = c(
    "B17001_001","B17001_002","B17001_031"
  ),
  template_label = c(
    "Total (denominator)",
    "Below poverty level","At or above poverty level"
  )
)

# Vehicles available (B08201) — detailed table
veh_brackets <- tibble(
  census_code = c(
    "B08201_001","B08201_002","B08201_003",
    "B08201_004","B08201_005","B08201_006"
  ),
  template_label = c(
    "Total Households","No vehicle","1 vehicle",
    "2 vehicles","3 vehicles","4 or more vehicles"
  )
)

# Means of transportation to work (B08301) — detailed table
transpo_brackets <- tibble(
  census_code = c(
    "B08301_001","B08301_003","B08301_004","B08301_010",
    "B08301_016","B08301_017","B08301_018","B08301_019",
    "B08301_020","B08301_021"
  ),
  template_label = c(
    "Total Workers","Drove alone","Carpooled","Public transportation",
    "Taxi or ride-hailing services","Motorcycle","Bicycle",
    "Walked","Other means","Worked from home"
  )
)

# ── Generic checker function ──────────────────────────────────────────────────

check_acs_variables <- function(lookup_tbl, dataset, year, section_name) {
  codes <- lookup_tbl$census_code
  
  raw <- load_variables(year = year, dataset = dataset) |>
    filter(name %in% codes) |>
    left_join(lookup_tbl, join_by(name == census_code)) |>
    select(name, template_label, census_label = label, concept)
  
  missing <- setdiff(codes, raw$name)
  if (length(missing) > 0) {
    message(glue::glue(
      "\n⚠  [{section_name}] {length(missing)} code(s) NOT FOUND in {year} {dataset}:\n",
      "   ", paste(missing, collapse = ", ")
    ))
  } else {
    message(glue::glue(
      "\n✓  [{section_name}] All {length(codes)} codes present in {year} {dataset}"
    ))
  }
  
  raw
}

# ── Run all checks for a given year ──────────────────────────────────────────

check_all_sections <- function(year) {
  message(glue::glue("\n{'='<40}\nChecking ACS year: {year}\n{'='<40}"))
  
  list(
    age       = check_acs_variables(age_brackets,      "acs5/subject", year, "Age S0101"),
    lep       = check_acs_variables(lep_brackets,      "acs5/subject", year, "LEP S1602"),
    language  = check_acs_variables(lang_brackets,     "acs5",         year, "Language C16001"),
    race      = check_acs_variables(race_brackets,     "acs5",         year, "Race B02001"),
    hispanic  = check_acs_variables(hispanic_brackets, "acs5",         year, "Hispanic origin B03003"),
    income    = check_acs_variables(income_brackets,   "acs5",         year, "Income B19001"),
    poverty   = check_acs_variables(poverty_brackets,  "acs5",         year, "Poverty B17001"),
    vehicles  = check_acs_variables(veh_brackets,      "acs5",         year, "Vehicles B08201"),
    transpo   = check_acs_variables(transpo_brackets,  "acs5",         year, "Transportation B08301")
  )
}

# ── Usage ─────────────────────────────────────────────────────────────────────

results_2024 <- check_all_sections(2024)

# Inspect any section's full label comparison
results_2024$race
results_2024$income
results_2024$poverty
results_2024$age
results_2024$lep
results_2024$language
results_2024$vehicles
results_2024$transpo
results_2024$transpo
results_2024$transpo
results_2024$transpo
