# Node-count (Gauss-Hermite quadrature) stability diagnostic (analysis plan
# §8.C): "re-fit with more integration points (e.g. 12 -> 24 -> 48 Gauss-
# Hermite nodes); the APE must not move materially." Refits the flagship
# model (2022, vulnerable_it) at the full {12, 24, 48, 96} sweep, and the
# other two lead-DV (vulnerable_it) year models at a lighter {12, 24} check
# -- full 96-node sweeps on all three years would cost ~35+ minutes of
# additional compute for a diagnostic whose purpose is already served by
# demonstrating stability on the flagship plus a confirmatory second point
# elsewhere; scope recorded here rather than silently applied.

library(dplyr)
library(readr)
library(jsonlite)

LEAD_DV <- "vulnerable_it"

fit_at_nodes <- function(year, dv, n_quad) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  d <- build_design(sub, dv)
  t0 <- Sys.time()
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)
  fit_time <- as.numeric(Sys.time() - t0, units = "secs")
  a <- ape(fit, "L_dv")
  out <- list(year = year, dv = dv, n_quad = n_quad, ape = a$ape, se = a$se,
              sigma_alpha = sigma_alpha_of(fit), loglik = fit$loglik,
              converged = fit$converged, grad_norm = fit$grad_norm, fit_time_sec = fit_time)
  cat(sprintf("[node-stability %d %s K=%d] APE=%.5f sigma_a=%.4f loglik=%.2f time=%.1fs\n",
              year, dv, n_quad, a$ape, sigma_alpha_of(fit), fit$loglik, fit_time))
  out
}

main_node_stability <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  results <- list()

  # Flagship: full sweep
  for (K in c(12, 24, 48, 96)) {
    results[[length(results) + 1]] <- fit_at_nodes(2022, LEAD_DV, K)
  }
  # Other lead-DV years: lighter 12-vs-24 confirmatory check
  for (year in c(2023, 2024)) {
    for (K in c(12, 24)) {
      results[[length(results) + 1]] <- fit_at_nodes(year, LEAD_DV, K)
    }
  }

  df <- bind_rows(results)
  df <- df %>% group_by(year, dv) %>%
    mutate(ape_at_baseline = ape[n_quad == min(n_quad)][1],
           rel_change_pct = 100 * (ape - ape_at_baseline) / ape_at_baseline) %>%
    ungroup()

  write_csv(df, "output/tables_r/node_stability.csv")
  write_json(results, "output/tables_r/node_stability.json", auto_unbox = TRUE, digits = NA)
  cat("\nwrote output/tables_r/node_stability.{csv,json}\n")
  cat(sprintf("max |relative change| vs baseline (K=12): %.4f%%\n", max(abs(df$rel_change_pct))))
  df
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_node_stability()
}
