# Auto-generated manifest of every file in output/tables_r/ (itself
# git-ignored, not committed -- it regenerates locally by running the
# pipeline). Written back into that same directory, so it's local
# documentation of a local, regenerable directory, not a committed doc.
# One-line description plus the results-notebook section that reads each
# file. Descriptions are matched by filename pattern (most specific
# first); anything unmatched gets a generic fallback rather than being
# silently omitted, so the manifest can never drift out of sync with
# what's actually on disk (it is regenerated from a directory listing
# every time, not hand-maintained prose).

library(dplyr)

PATTERNS <- list(
  list(rx = "^fit_statistics\\.", desc = "Fit statistics (log-lik, AIC, BIC, N, convergence) for all 6 core models.", sec = "2.1"),
  list(rx = "^full_coef_vulnerable_it_(long|wide)\\.", desc = "Full parameter vector (every design-matrix column + sigma_alpha), vulnerable_it, all 3 years.", sec = "2.1"),
  list(rx = "^full_coef_informal_it_(long|wide)\\.", desc = "Full parameter vector (every design-matrix column + sigma_alpha), informal_it, all 3 years.", sec = "2.1"),
  list(rx = "^descriptives_by_year_vulnerable\\.", desc = "Covariate means/SDs (numeric) and shares (categorical) by year x vulnerable_it status.", sec = "1.2"),
  list(rx = "^descriptives_n_summary\\.", desc = "N person-quarters and N persons by year x vulnerable_it status (denominators for the descriptives table).", sec = "1.2"),
  list(rx = "^subgroup_ape_by_dimension\\.", desc = "Subgroup lag-APE (+SE, CI) by year and heterogeneity dimension (sex/age_band/edu_cat/urbrur), vulnerable_it.", sec = "4.1"),
  list(rx = "^subgroup_interaction_wald\\.", desc = "Formal Wald test (chi2, df, p) of the lag x characteristic interaction, by year and dimension.", sec = "4.1"),
  list(rx = "^peryear_vs_pooled_reconciliation\\.", desc = "Sigma_alpha-swap vs. beta-swap decomposition of the per-year-vs-pooled APE gap, both DVs, all years.", sec = "3.1"),
  list(rx = "^mundlak_scaling\\.", desc = "Mundlak/CRE AME vs. RE-probit APE on one scale: ratio, deflation factor sqrt(1+sigma_alpha^2), verdict.", sec = "6.4"),
  list(rx = "^attrition_probit_\\d{4}_vulnerable_it_coefs\\.csv$", desc = "Full attrition-probit coefficient table (one year) -- initial status + all covariates + constant, cluster-robust SE.", sec = "6.3"),
  list(rx = "^core_\\d{4}_vulnerable_it_coefs\\.csv$", desc = "Full coefficient table for one core dynamic RE-probit model (one year), vulnerable_it: every param, SE, z, p, + implied sigma_alpha.", sec = "2.1"),
  list(rx = "^core_\\d{4}_informal_it_coefs\\.csv$", desc = "Full coefficient table for one core dynamic RE-probit model (one year), informal_it: every param, SE, z, p, + implied sigma_alpha.", sec = "2.1"),
  list(rx = "^core_\\d{4}_(vulnerable_it|informal_it)_diagnostics\\.json$", desc = "Diagnostics for one core model: N, loglik, sigma_alpha, corrected APE, Wooldridge-aux Wald test, naive-probit comparison, instability flags.", sec = "2 / 6.1"),
  list(rx = "^core_models_summary\\.csv$", desc = "One-row-per-model summary of all 6 core models: APE, sigma_alpha, instability flags -- feeds the main §2 table.", sec = "2"),
  list(rx = "^node_stability\\.", desc = "Gauss-Hermite node-count (K=12/24/48/96) sensitivity sweep: APE and sigma_alpha at each K.", sec = "6.1"),
  list(rx = "^rq2_event_study\\.json$", desc = "Within-2022 entry-vs-inflation probits (both DVs, 2 inflation measures) + early-entrant persistence.", sec = "5"),
  list(rx = "^rq3_pooled_(vulnerable_it|informal_it)_coefs\\.csv$", desc = "Full coefficient table for the pooled (year x lag interaction) crisis-comparison model, one DV.", sec = "3"),
  list(rx = "^rq3_pooled_(vulnerable_it|informal_it)_diagnostics\\.json$", desc = "Pooled crisis-comparison model diagnostics: by-year APE, sigma_alpha, H0(beta equal across years) Wald test.", sec = "3"),
  list(rx = "^rq4_\\d{4}_.*_vulnerable_it\\.json$", desc = "One heterogeneity model (year x dimension): subgroup APEs + interaction Wald test.", sec = "4"),
  list(rx = "^robustness_battery\\.json$", desc = "Combined robustness battery: attrition (balanced/unbalanced + full nonrandom-attrition coefficients), Mundlak/CRE, SSNIT route, stratum sensitivity.", sec = "6.3-6.6"),
  list(rx = "^step0_dv_lead_decision\\.csv$", desc = "Step-0 DV base rates and raw state-dependence signal (P(y=1|lag)) that motivates the vulnerable_it lead-DV decision.", sec = "1"),
  list(rx = "^\\d{4}_transition_matrix\\.csv$", desc = "4-state (formal/informal-vulnerable/unemployed/out-of-LF) row-normalized transition probabilities, adjacent quarters only, one year.", sec = "1.1"),
  list(rx = "^\\d{4}_gross_flows\\.csv$", desc = "4-state transition gross (unweighted) flow counts, adjacent quarters only, one year -- companion to the transition matrix.", sec = "1.1"),
  list(rx = "^\\d{4}_attrition_diagnostics\\.csv$", desc = "Baseline-characteristic comparison of attriters vs. completers (Welch t-tests), one year.", sec = "6.3"),
  list(rx = "^brms_crosscheck.*\\.json$", desc = "Bayesian (brms/Stan) cross-check of the flagship 2022 vulnerable_it model: posterior APE, credible interval, Rhat, APE-definition confirmation.", sec = "6.2"),
  list(rx = "^brms_crosscheck.*\\.rds$", desc = "Saved brms posterior draws object (the fitted brmsfit) for the flagship cross-check -- lets the APE be recomputed without re-fitting.", sec = "6.2")
)

describe_file <- function(fname) {
  for (p in PATTERNS) if (grepl(p$rx, fname, perl = TRUE)) return(p)
  list(desc = "(no pattern matched -- describe manually if this file is load-bearing)", sec = "?")
}

main_manifest <- function() {
  files <- sort(list.files("output/tables_r"))
  files <- files[files != "MANIFEST.md"]

  lines <- c(
    "# `output/tables_r/` manifest",
    "",
    "Local-only, git-ignored directory -- regenerate by running the R",
    "estimation stage (see `README.md`).",
    "",
    sprintf("Auto-generated by `R/run_manifest.R` from the actual directory contents (%d files at last generation) -- regenerate after any script that writes here changes its output, so this list can never silently go stale.", length(files)),
    "",
    "`Notebook §` is the section of `Rmd/02_results.Rmd` that reads this file, once knitted.",
    "",
    "| File | Contents | Notebook § |",
    "|---|---|---|"
  )
  for (f in files) {
    d <- describe_file(f)
    lines <- c(lines, sprintf("| `%s` | %s | %s |", f, d$desc, d$sec))
  }

  writeLines(lines, "output/tables_r/MANIFEST.md")
  cat("wrote output/tables_r/MANIFEST.md (", length(files), "files listed)\n")
  invisible(files)
}

if (sys.nframe() == 0) {
  setwd(here::here())
  main_manifest()
}
