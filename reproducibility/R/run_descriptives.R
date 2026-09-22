# Means/SDs of every covariate by year, split by
# vulnerable_it status, on the estimation universe (employed, lag defined)
# for vulnerable_it -- the lead DV -- so this table lines up with the core
# model's own sample. N persons and N person-quarters reported per cell.

library(dplyr)
library(readr)
library(tidyr)
library(jsonlite)

YEARS <- c(2022, 2023, 2024)

NUMERIC_VARS <- c("age", "actual_hours", "hh_size", "hh_children_u15", "hh_other_earners")
CATEGORICAL_VARS <- c("sex", "edu_cat", "urbrur", "sector3")

describe_year <- function(year) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, "vulnerable_it")

  num_rows <- lapply(NUMERIC_VARS, function(v) {
    sub %>% group_by(vulnerable_it) %>%
      summarise(mean = mean(.data[[v]], na.rm = TRUE), sd = stats::sd(.data[[v]], na.rm = TRUE),
                n_obs = sum(!is.na(.data[[v]])), .groups = "drop") %>%
      mutate(year = year, variable = v, level = NA_character_, type = "numeric")
  })

  cat_rows <- lapply(CATEGORICAL_VARS, function(v) {
    sub %>% filter(!is.na(.data[[v]])) %>% group_by(vulnerable_it, level = .data[[v]]) %>%
      summarise(n_obs = n(), .groups = "drop") %>%
      group_by(vulnerable_it) %>% mutate(share = n_obs / sum(n_obs)) %>% ungroup() %>%
      mutate(year = year, variable = v, type = "categorical", mean = share, sd = NA_real_) %>%
      select(vulnerable_it, mean, sd, n_obs, year, variable, level, type)
  })

  n_summary <- sub %>% group_by(vulnerable_it) %>%
    summarise(n_person_quarters = n(), n_persons = n_distinct(person_id), .groups = "drop") %>%
    mutate(year = year)

  list(rows = bind_rows(num_rows, cat_rows), n_summary = n_summary)
}

main_descriptives <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  per_year <- lapply(YEARS, describe_year)
  rows <- bind_rows(lapply(per_year, `[[`, "rows"))
  n_summary <- bind_rows(lapply(per_year, `[[`, "n_summary"))

  rows <- rows %>%
    mutate(vulnerable_it = ifelse(vulnerable_it == 1, "vulnerable", "not_vulnerable")) %>%
    select(year, variable, level, type, vulnerable_it, mean, sd, n_obs)
  n_summary <- n_summary %>% mutate(vulnerable_it = ifelse(vulnerable_it == 1, "vulnerable", "not_vulnerable"))

  write_csv(rows, "output/tables_r/descriptives_by_year_vulnerable.csv")
  write_json(rows, "output/tables_r/descriptives_by_year_vulnerable.json", auto_unbox = TRUE, digits = NA)
  write_csv(n_summary, "output/tables_r/descriptives_n_summary.csv")
  write_json(n_summary, "output/tables_r/descriptives_n_summary.json", auto_unbox = TRUE, digits = NA)

  cat("wrote output/tables_r/descriptives_by_year_vulnerable.{csv,json} and descriptives_n_summary.{csv,json}\n")
  print(n_summary)
  invisible(list(rows = rows, n_summary = n_summary))
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R")
  main_descriptives()
}
