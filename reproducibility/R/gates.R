# Numeric evidence for the five feasibility gates. R port of scripts/gates.py.

library(dplyr)

gate1_labour_universe <- function(df) {
  wa <- df %>% filter(age >= 15)
  by_q <- wa %>% group_by(t_within_year) %>% summarise(miss = mean(is.na(econact)), .groups = "drop")
  list(
    n_working_age_rows = nrow(wa),
    missing_rate_by_quarter = setNames(as.list(by_q$miss), as.character(by_q$t_within_year)),
    overall_missing_rate = mean(is.na(wa$econact)),
    pass = mean(is.na(wa$econact)) == 0
  )
}

gate2_within_year_balance <- function(df) {
  keys <- validate_keys(df)
  n_persons <- keys$n_unique_persons
  n_ge3 <- round(keys$share_ge3_quarters * n_persons)
  list(
    n_unique_persons = n_persons,
    n_persons_ge3_quarters = n_ge3,
    share_ge3_quarters = keys$share_ge3_quarters,
    share_all4_quarters = keys$share_all4_quarters,
    quarters_observed_distribution = keys$quarters_observed_distribution,
    pass = n_ge3 > 0
  )
}

gate3_informality_route <- function(df) {
  emp <- df %>% filter(econact == "Employed")
  is_employee <- emp$status_in_job %in% c("Paid employee", "Casual worker")
  employees <- emp[is_employee, ]
  nonemployees <- emp[!is_employee, ]
  list(
    n_employed = nrow(emp),
    share_employees = mean(is_employee),
    status_in_job_missing_rate = mean(is.na(emp$status_in_job)),
    inst_sector_missing_rate = mean(is.na(emp$inst_sector)),
    contract_missing_rate_within_employees = if (nrow(employees) > 0) mean(is.na(employees$has_contract)) else NA,
    contract_missing_rate_outside_employees = if (nrow(nonemployees) > 0) mean(is.na(nonemployees$has_contract)) else NA,
    soc_sec_missing_rate_within_employees = if (nrow(employees) > 0) mean(is.na(employees$soc_sec_entitled)) else NA,
    ssnit_missing_rate_within_employees = if (nrow(employees) > 0) mean(is.na(employees$ssnit_flag)) else NA,
    decision = paste(
      "informal_it lead DV, built from has_contract+soc_sec_entitled for employees",
      "and inst_sector (Private Informal) + family-worker status for non-employees.",
      "ssnit_flag excluded from the headline rule (too thin); kept as robustness-only."
    )
  )
}

gate4_panel_linkage <- function(df) validate_keys(df)

gate5_earnings_quality <- function(df) {
  emp <- df %>% filter(econact == "Employed")
  is_employee <- emp$status_in_job %in% c("Paid employee", "Casual worker")
  employees <- emp[is_employee, ]
  non_missing <- employees$earn_amount[!is.na(employees$earn_amount)]
  list(
    missing_rate_among_all_employed = mean(is.na(emp$earn_amount)),
    missing_rate_among_employees_only = if (nrow(employees) > 0) mean(is.na(employees$earn_amount)) else NA,
    n_non_missing = length(non_missing),
    summary_stats = if (length(non_missing) > 0) as.list(summary(non_missing)) else list(),
    verdict = paste(
      "Usable only as a wage-employee-subsample robustness variable",
      "(~20% of the employed are wage employees). Not usable as a",
      "general covariate for the full analytic sample -- status",
      "transitions remain the safer core, per the spec."
    )
  )
}

run_all_gates <- function(df) {
  list(
    gate1_labour_universe = gate1_labour_universe(df),
    gate2_within_year_balance = gate2_within_year_balance(df),
    gate3_informality_route = gate3_informality_route(df),
    gate4_panel_linkage = gate4_panel_linkage(df),
    gate5_earnings_quality = gate5_earnings_quality(df)
  )
}
