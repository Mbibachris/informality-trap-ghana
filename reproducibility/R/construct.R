# Construct the dependent variables and recodes. R port of scripts/construct.py
# -- see that file's docstring for the Gate-3 construction-rule rationale.

library(dplyr)

# pandas compares NaN == "literal" as FALSE (not NA-propagating); R's `==`
# propagates NA instead. Every direct `==` against a possibly-missing column
# below must use this guard to reproduce the Python pipeline's semantics
# exactly (`%in%` already behaves like pandas .isin() -- NA -> FALSE -- so it
# needs no guard).
eq_safe <- function(x, val) !is.na(x) & x == val

EMPLOYEE_STATUSES <- c("Paid employee", "Casual worker")

VULNERABLE_STATUSES <- c(
  "Agric self-employed without employees",
  "Non-agric self-employed without employees",
  "Agric contributing family worker",
  "Non-agric contributing family worker"
)

FAMILY_WORKER_STATUSES <- c(
  "Agric contributing family worker",
  "Non-agric contributing family worker"
)

AGRICULTURE_INDUSTRY <- c("Agriculture, forestry and fishing")
INDUSTRY_SECTOR <- c(
  "Mining and quarrying",
  "Manufacturing",
  "Electricity, gas, steam and air conditioning supply",
  "Water supply; sewerage, waste management and remediation activities",
  "Construction"
)

EDU_BASIC <- c("Nursery", "Kindergarten", "Primary", "JSS/JHS", "Middle")
EDU_SECONDARY <- c(
  "SSS/SHS", "Secondary", "Voc/Tech/Commercial",
  "Postmiddle/secondary certificate", "Postmiddle/secondary diploma"
)
EDU_TERTIARY <- c(
  "Tertiary - HND", "Tertiary - Bachelor's Degree",
  "Tertiary - Post graduate certificate / diploma",
  "Tertiary - Master's Degree", "Tertiary - PhD"
)

build_dependent_variables <- function(df) {
  df <- df %>% mutate(employed_it = eq_safe(econact, "Employed"))

  is_employee <- df$status_in_job %in% EMPLOYEE_STATUSES
  written_contract <- eq_safe(df$has_contract, "Yes, written")
  has_social_security <- eq_safe(df$soc_sec_entitled, "Yes")
  both_missing <- is.na(df$has_contract) & is.na(df$soc_sec_entitled)
  informal_employee <- ifelse(both_missing, NA, !(written_contract & has_social_security))

  private_informal_sector <- eq_safe(df$inst_sector, "Private Informal")
  is_family_worker <- df$status_in_job %in% FAMILY_WORKER_STATUSES
  informal_nonemployee <- private_informal_sector | is_family_worker

  informal_it <- rep(NA_real_, nrow(df))
  emp <- df$employed_it
  informal_it[emp & is_employee] <- as.numeric(informal_employee[emp & is_employee])
  informal_it[emp & !is_employee] <- as.numeric(informal_nonemployee[emp & !is_employee])
  df$informal_it <- informal_it

  vulnerable_it <- rep(NA_real_, nrow(df))
  vulnerable_it[emp] <- as.numeric(df$status_in_job[emp] %in% VULNERABLE_STATUSES)
  df$vulnerable_it <- vulnerable_it

  is_informal <- !is.na(df$informal_it) & df$informal_it == 1
  is_formal <- !is.na(df$informal_it) & df$informal_it == 0
  labour_status4 <- rep(NA_character_, nrow(df))
  labour_status4[!emp & eq_safe(df$econact, "Unemployed")] <- "unemployed"
  labour_status4[!emp & eq_safe(df$econact, "Not active")] <- "out_of_lf"
  labour_status4[emp & is_informal] <- "informal_vulnerable_employed"
  labour_status4[emp & is_formal] <- "formal_employed"
  df$labour_status4 <- labour_status4

  df
}

build_recodes <- function(df) {
  df <- df %>%
    mutate(
      age_band = case_when(
        age >= 15 & age <= 24 ~ "youth_15_24",
        age >= 25 & age <= 54 ~ "prime_25_54",
        age >= 55 ~ "older_55_plus",
        TRUE ~ NA_character_
      ),
      sector3 = case_when(
        industry_major %in% AGRICULTURE_INDUSTRY ~ "agriculture",
        industry_major %in% INDUSTRY_SECTOR ~ "industry",
        !is.na(industry_major) ~ "services",
        TRUE ~ NA_character_
      ),
      edu_cat = case_when(
        eq_safe(ever_school, "Never attended") ~ "none",
        edu_level_raw %in% EDU_BASIC ~ "basic",
        edu_level_raw %in% EDU_SECONDARY ~ "secondary",
        edu_level_raw %in% EDU_TERTIARY ~ "tertiary",
        TRUE ~ NA_character_
      )
    )
  df
}
