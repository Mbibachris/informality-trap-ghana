# Step 0 -- descriptive foundation (analysis plan §4, Step 0). R port of
# scripts/step0_descriptives.py. DV base rates + raw state-dependence signal
# per DV, per year -- decides which DV carries the "trap" headline.

library(dplyr)
library(readr)

YEARS <- c(2022, 2023, 2024)

raw_state_dependence <- function(df, dv, lag) {
  sub <- df %>% filter(!is.na(.data[[dv]]), !is.na(.data[[lag]]))
  p1 <- mean(sub[[dv]][sub[[lag]] == 1])
  p0 <- mean(sub[[dv]][sub[[lag]] == 0])
  list(n_lag1 = sum(sub[[lag]] == 1), n_lag0 = sum(sub[[lag]] == 0),
       p_given_1 = p1, p_given_0 = p0, raw_gap = p1 - p0)
}

run_step0 <- function() {
  rows <- list()
  for (year in YEARS) {
    df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
    emp <- df %>% filter(employed_it == TRUE)
    for (dv in c("informal_it", "vulnerable_it")) {
      lag <- paste0("L_", dv)
      base_rate <- mean(emp[[dv]], na.rm = TRUE)
      sd <- raw_state_dependence(df, dv, lag)
      rows[[length(rows) + 1]] <- data.frame(
        year = year, dv = dv, base_rate_among_employed = base_rate,
        n_lag1 = sd$n_lag1, n_lag0 = sd$n_lag0,
        P_y1_given_lag1 = sd$p_given_1, P_y1_given_lag0 = sd$p_given_0, raw_gap = sd$raw_gap
      )
    }
  }
  out <- bind_rows(rows)
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  write_csv(out, "output/tables_r/step0_dv_lead_decision.csv")
  print(out)
  out
}

if (sys.nframe() == 0) {
  setwd(here::here())
  run_step0()
}
