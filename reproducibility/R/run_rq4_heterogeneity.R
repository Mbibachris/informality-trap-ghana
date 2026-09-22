# Step 4 / RQ4 (analysis plan §7): heterogeneity in state dependence by
# sex, age band, education, locality. Lead DV (vulnerable_it), per year.
# R port of scripts/run_rq4_heterogeneity.py.

library(dplyr)
library(readr)

YEARS <- c(2022, 2023, 2024)
GROUPS <- c("sex", "age_band", "edu_cat", "urbrur")
LEAD_DV <- "vulnerable_it"

run_rq4_one <- function(year, group, dv = LEAD_DV, n_quad = 12) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  lag <- as.numeric(sub[[paste0("L_", dv)]])

  base_level <- BASE_LEVEL[[group]]
  levels_present <- setdiff(sort(unique(sub[[group]])), base_level)
  extra <- list()
  for (lvl in levels_present) {
    dummy <- as.numeric(sub[[group]] == lvl)
    extra[[paste0(group, "_", lvl, "_x_lag")]] <- dummy * lag
  }

  d <- build_design(sub, dv, extra_terms = extra)
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)

  inter_names <- names(extra)
  subgroup_ape <- list()
  base_mask <- sub[[group]] == base_level
  interacted_base <- setNames(lapply(inter_names, function(nm) d$X[base_mask, nm]), inter_names)
  subgroup_ape[[base_level]] <- ape(fit, "L_dv", X_override = d$X[base_mask, , drop = FALSE], interacted_cols = interacted_base)
  for (lvl in levels_present) {
    mask <- sub[[group]] == lvl
    interacted <- setNames(lapply(inter_names, function(nm) d$X[mask, nm]), inter_names)
    subgroup_ape[[lvl]] <- ape(fit, "L_dv", X_override = d$X[mask, , drop = FALSE], interacted_cols = interacted)
  }

  wald_het <- wald_test(fit, inter_names)

  out <- list(year = year, group = group, dv = dv, n_obs = fit$n_obs,
              converged = fit$converged, sigma_alpha = sigma_alpha_of(fit),
              subgroup_ape = subgroup_ape, wald_H0_no_heterogeneity = wald_het)
  jsonlite::write_json(out, sprintf("output/tables_r/rq4_%d_%s_%s.json", year, group, dv),
                        auto_unbox = TRUE, digits = NA)

  cat(sprintf("[RQ4 %d %s] N=%d sigma_a=%.3f wald_het p=%.4f\n", year, group, fit$n_obs, sigma_alpha_of(fit), wald_het$p))
  for (lvl in names(subgroup_ape)) {
    a <- subgroup_ape[[lvl]]
    cat(sprintf("   %s: APE=%.4f (se %.4f)\n", lvl, a$ape, a$se))
  }
  out
}

main_rq4 <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  for (year in YEARS) for (group in GROUPS) run_rq4_one(year, group)
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_rq4()
}
