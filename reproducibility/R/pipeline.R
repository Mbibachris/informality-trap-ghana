# End-to-end orchestration: raw file -> analysis-ready person-quarter panel.
# R port of scripts/pipeline.py. No new logic -- calls the other R/ modules
# in the same order the validated Python pipeline uses.

library(dplyr)

source("R/io_loaders.R")
source("R/panel_keys.R")
source("R/roster.R")
source("R/construct.R")
source("R/dynamics.R")
source("R/gates.R")

PROCESSED_DIR_R <- file.path("data", "processed_r")

build_year_panel <- function(year, working_age_only = TRUE) {
  df <- load_year(year)
  df <- rename_to_variable_map(df)

  df <- add_keys(df)
  df <- add_household_size_and_children(df)

  df <- build_dependent_variables(df)
  df <- add_other_earners(df)
  df <- build_recodes(df)

  df <- build_dynamics(df)
  df <- add_within_person_category_shares(df, "sector3")

  if (working_age_only) df <- df %>% filter(age >= 15)

  df <- df %>% arrange(person_id, t_within_year)
  df
}

save_year_outputs <- function(df, year) {
  dir.create(PROCESSED_DIR_R, recursive = TRUE, showWarnings = FALSE)
  full_path <- file.path(PROCESSED_DIR_R, sprintf("%d_person_quarter_unbalanced.csv", year))
  readr::write_csv(df, full_path)
  balanced_path <- file.path(PROCESSED_DIR_R, sprintf("%d_person_quarter_balanced.csv", year))
  readr::write_csv(df %>% filter(balanced_i == 1), balanced_path)
  list(unbalanced = full_path, balanced = balanced_path)
}

build_and_save_all_years <- function(years = c(2022, 2023, 2024)) {
  manifest <- list()
  frames <- list()
  for (year in years) {
    df <- build_year_panel(year)
    paths <- save_year_outputs(df, year)
    year_gates <- run_all_gates(df)
    manifest[[as.character(year)]] <- list(
      n_rows = nrow(df), n_persons = n_distinct(df$person_id),
      paths = paths, gates = year_gates
    )
    frames[[as.character(year)]] <- df
  }
  pooled <- bind_rows(frames)
  pooled_path <- file.path(PROCESSED_DIR_R, "pooled_person_quarter.csv")
  readr::write_csv(pooled, pooled_path)
  manifest$pooled <- list(n_rows = nrow(pooled), path = pooled_path)
  manifest
}
