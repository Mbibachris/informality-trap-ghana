# Step 2 / RQ3 (analysis plan §6): pooled 2022-2024, year x lag interaction.
# Tests H0: beta_2022 = beta_2023 = beta_2024. R port of scripts/run_rq3_crisis.py.
# Person/cluster keys are year-qualified before pooling -- AHIES person_id
# and EA `cluster` codes are only unique WITHIN a year (three independent
# panels), so pooling without re-keying would merge unrelated
# people/EAs across years into the same "person"/"cluster".

library(dplyr)
library(readr)

YEARS <- c(2022, 2023, 2024)

load_pooled <- function(dv) {
  parts <- list()
  for (year in YEARS) {
    df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
    sub <- estimation_universe(df, dv)
    parts[[as.character(year)]] <- sub
  }
  pooled <- bind_rows(parts)
  pooled$person_id <- paste(pooled$year, pooled$person_id, sep = "_")
  pooled$cluster <- paste(pooled$year, pooled$cluster, sep = "_")
  pooled
}

run_rq3_one <- function(dv, n_quad = 12) {
  pooled <- load_pooled(dv)
  y2023 <- as.numeric(pooled$year == 2023)
  y2024 <- as.numeric(pooled$year == 2024)
  lag <- as.numeric(pooled[[paste0("L_", dv)]])
  extra <- list(year_2023 = y2023, year_2024 = y2024,
                year_2023_x_lag = y2023 * lag, year_2024_x_lag = y2024 * lag)

  d <- build_design(pooled, dv, extra_terms = extra)
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)

  ape_by_year <- list()
  for (yr in YEARS) {
    mask <- pooled$year == yr
    interacted <- list(
      year_2023_x_lag = d$X[mask, "year_2023"],
      year_2024_x_lag = d$X[mask, "year_2024"]
    )
    ape_by_year[[as.character(yr)]] <- ape(fit, "L_dv", X_override = d$X[mask, , drop = FALSE], interacted_cols = interacted)
  }

  wald_equal <- wald_test(fit, c("year_2023_x_lag", "year_2024_x_lag"))

  ct <- coef_table.re_probit_result(fit)
  write_csv(ct, sprintf("output/tables_r/rq3_pooled_%s_coefs.csv", dv))

  out <- list(
    dv = dv, n_obs = fit$n_obs, n_persons = fit$n_persons, n_clusters = fit$n_clusters,
    converged = fit$converged, loglik = fit$loglik, sigma_alpha = sigma_alpha_of(fit),
    ape_by_year = ape_by_year, wald_H0_beta_equal_across_years = wald_equal,
    coef_year2023_x_lag = list(coef = beta_of(fit)[match("year_2023_x_lag", d$cols)],
                                se = fit$se[match("year_2023_x_lag", d$cols)]),
    coef_year2024_x_lag = list(coef = beta_of(fit)[match("year_2024_x_lag", d$cols)],
                                se = fit$se[match("year_2024_x_lag", d$cols)])
  )
  jsonlite::write_json(out, sprintf("output/tables_r/rq3_pooled_%s_diagnostics.json", dv),
                        auto_unbox = TRUE, digits = NA)

  cat(sprintf("[RQ3 %s] N=%d sigma_a=%.3f\n", dv, fit$n_obs, sigma_alpha_of(fit)))
  for (yr in YEARS) {
    a <- ape_by_year[[as.character(yr)]]
    cat(sprintf("  APE(lag) %d: %.4f (se %.4f, CI [%.4f,%.4f])\n", yr, a$ape, a$se, a$ci_lo, a$ci_hi))
  }
  cat(sprintf("  H0 beta_2022=beta_2023=beta_2024: chi2(%d)=%.3f, p=%.4f\n",
              wald_equal$df, wald_equal$stat, wald_equal$p))
  out
}

main_rq3 <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  for (dv in c("informal_it", "vulnerable_it")) run_rq3_one(dv)
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_rq3()
}
