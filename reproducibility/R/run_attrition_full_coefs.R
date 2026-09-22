# Regenerates just the attrition-probit piece of the robustness battery
# (now with the full coefficient table, task 6) and merges the updated
# nonrandom_attrition_test entries back into robustness_battery.json --
# without re-running the expensive balanced/unbalanced and SSNIT-route
# quadrature refits that also live in run_robustness.R.

library(dplyr)
library(readr)
library(jsonlite)

YEARS <- c(2022, 2023, 2024)

main_attrition_full_coefs <- function() {
  results <- lapply(YEARS, nonrandom_attrition_test)

  rob_path <- "output/tables_r/robustness_battery.json"
  rob <- if (file.exists(rob_path)) fromJSON(rob_path, simplifyVector = FALSE) else list()
  rob$nonrandom_attrition_test <- results
  write_json(rob, rob_path, auto_unbox = TRUE, digits = NA)
  cat("\nupdated nonrandom_attrition_test in", rob_path, "\n")
  results
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R"); source("R/dynamics.R")
  source("R/run_robustness.R")  # for nonrandom_attrition_test(), FIXED_CATS etc.
  main_attrition_full_coefs()
}
