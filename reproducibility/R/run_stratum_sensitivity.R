# Stratum sensitivity (analysis plan §9.6: "sensitivity to the region x
# urbrur stratum proxy"). Additional robustness model, NOT a replacement
# for the headline spec: the core design already carries `region` and
# `urbrur` as separate main-effect dummies; this refit ADDS the full
# region x urbrur interaction (one extra dummy per non-base region,
# region_<X>_x_urbrur_Urban), saturating the same 32-cell stratum proxy
# codebook.md documents as the design-variable stand-in, and compares the
# lead-DV (vulnerable_it) APE(lag) against the headline core-model APE.
# Merged into the existing robustness_battery.json under a new
# "stratum_sensitivity" key rather than re-running the whole battery.

library(dplyr)
library(readr)
library(jsonlite)

YEARS <- c(2022, 2023, 2024)
LEAD_DV <- "vulnerable_it"

stratum_sensitivity_one <- function(year, dv = LEAD_DV, n_quad = 12) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)

  urban <- as.numeric(sub$urbrur == "Urban")
  region_levels <- setdiff(sort(unique(sub$region)), BASE_LEVEL$region)
  extra <- setNames(
    lapply(region_levels, function(lvl) as.numeric(sub$region == lvl) * urban),
    paste0("region_", region_levels, "_x_urbrur_Urban")
  )

  d <- build_design(sub, dv, extra_terms = extra)
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)
  a <- ape(fit, "L_dv")
  wald_strat <- wald_test(fit, names(extra))

  out <- list(year = year, dv = dv, n_obs = fit$n_obs, n_extra_interaction_terms = length(extra),
              ape_lag = a, sigma_alpha = sigma_alpha_of(fit), converged = fit$converged,
              wald_H0_no_stratum_interaction = wald_strat)
  cat(sprintf("[stratum sensitivity %d %s] N=%d APE(lag)=%.4f (se %.4f) sigma_a=%.3f wald_interaction_p=%.4f\n",
              year, dv, fit$n_obs, a$ape, a$se, sigma_alpha_of(fit), wald_strat$p))
  out
}

main_stratum_sensitivity <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  results <- lapply(YEARS, stratum_sensitivity_one)

  rob_path <- "output/tables_r/robustness_battery.json"
  rob <- if (file.exists(rob_path)) jsonlite::fromJSON(rob_path, simplifyVector = FALSE) else list()
  rob$stratum_sensitivity <- results
  write_json(rob, rob_path, auto_unbox = TRUE, digits = NA)
  cat("\nmerged stratum_sensitivity into", rob_path, "\n")
  results
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_stratum_sensitivity()
}
