# Step 1 (analysis plan §5): core dynamic RE-probit + Wooldridge, per year,
# both DVs. Also fits the naive pooled probit (no Wooldridge aux, no random
# intercept) as the diagnostic comparison point (plan §8.A). R port of
# scripts/run_core_models.py.

library(dplyr)
library(readr)
library(marginaleffects)
library(sandwich)

YEARS <- c(2022, 2023, 2024)
DVS <- c("informal_it", "vulnerable_it")

naive_pooled_probit <- function(X, y, cols, cluster_idx) {
  drop <- c("dv_i1", WOOLDRIDGE_MEAN_COLS)
  keep <- setdiff(cols, drop)
  Xn <- X[, keep, drop = FALSE]
  df_glm <- as.data.frame(Xn, check.names = FALSE)  # preserve names with spaces (region dummies)
  df_glm$y <- y
  df_glm$.cluster <- factor(cluster_idx)
  # backtick every term: several dummy column names contain spaces (e.g.
  # "region_North East", "region_Western North") which a bare formula
  # string cannot parse.
  form <- as.formula(paste("y ~ 0 +", paste(sprintf("`%s`", keep), collapse = " + ")))
  m <- glm(form, data = df_glm, family = binomial(link = "probit"))
  vc <- sandwich::vcovCL(m, cluster = df_glm$.cluster, type = "HC1")
  b <- coef(m)["L_dv"]
  se <- sqrt(vc["L_dv", "L_dv"])
  # L_dv is binary (0/1): the marginal effect of interest is the discrete
  # P(y=1|L=1)-P(y=1|L=0) difference, NOT the continuous derivative
  # dnorm(Xb)*beta that avg_slopes()/statsmodels' get_margeff() compute by
  # default for a variable that isn't explicitly flagged as a dummy --
  # verified this distinction matters a lot here (0.40 discrete vs 0.26
  # from the derivative version on the same fitted model in an earlier,
  # uncorrected pass). avg_comparisons(variables=list(L_dv=0:1)) forces the
  # correct discrete contrast.
  marg <- avg_comparisons(m, variables = list(L_dv = 0:1), vcov = vc)
  list(coef_lag = unname(b), se_lag = unname(se),
       ame_lag = marg$estimate[1], ame_se_lag = marg$std.error[1])
}

# Instability heuristics (plan §8.C): flag a core model rather than quietly
# report it. Three independent checks, any one of which trips the flag:
#  (a) at-risk (lag=0) N below 2000 -- the threshold that isolates exactly
#      the 2024 informal_it cell (N=1,783) from every other core model
#      (next-smallest is 3,683), so it is not tuned post hoc to that cell
#      alone, it is a round number chosen because that is where the actual
#      gap in the data falls.
#  (b) sigma_alpha < 0.5 -- well below the 0.8-1.6 range every other core
#      model's random-intercept SD falls in; a small sigma_alpha means the
#      Wooldridge/RE device found little residual person-level heterogeneity
#      to soak up, which on a thin at-risk sample is more likely a
#      data-sparsity artifact than a genuine finding.
#  (c) quasi-separation -- the population-averaged fitted probability
#      Phi(x'beta/sqrt(1+sigma_alpha^2)) lands within 1e-6 of 0 or 1 for any
#      observation, the standard symptom of a covariate pattern that
#      (near-)perfectly predicts the outcome.
AT_RISK_N_THRESHOLD <- 2000
SIGMA_ALPHA_THRESHOLD <- 0.5
SEPARATION_EPS <- 1e-6

instability_flags <- function(fit, n_lag0) {
  sigma_a <- sigma_alpha_of(fit)
  denom <- sqrt(1 + sigma_a^2)
  p_hat <- pnorm(as.numeric(fit$X %*% beta_of(fit)) / denom)
  quasi_sep <- any(p_hat < SEPARATION_EPS | p_hat > 1 - SEPARATION_EPS)
  thin_at_risk <- n_lag0 < AT_RISK_N_THRESHOLD
  sigma_low <- sigma_a < SIGMA_ALPHA_THRESHOLD
  list(
    n_lag0 = n_lag0, thin_at_risk = thin_at_risk,
    sigma_alpha_near_boundary = sigma_low, quasi_separation = quasi_sep,
    min_fitted_p = min(p_hat), max_fitted_p = max(p_hat),
    unstable = thin_at_risk || sigma_low || quasi_sep
  )
}

run_one <- function(year, dv, n_quad = 12) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  d <- build_design(sub, dv)

  t0 <- Sys.time()
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)
  fit_time <- as.numeric(Sys.time() - t0, units = "secs")

  ape_lag <- ape(fit, "L_dv")
  aux_names <- c("dv_i1", WOOLDRIDGE_MEAN_COLS)
  wald <- wald_test(fit, aux_names)
  naive <- naive_pooled_probit(d$X, d$y, d$cols, d$cluster_idx)

  n_lag0 <- sum(sub[[paste0("L_", dv)]] == 0)
  inst <- instability_flags(fit, n_lag0)

  ct <- coef_table.re_probit_result(fit)
  write_csv(ct, sprintf("output/tables_r/core_%d_%s_coefs.csv", year, dv))

  diag <- list(
    year = year, dv = dv, n_obs = fit$n_obs, n_persons = fit$n_persons, n_clusters = fit$n_clusters,
    n_quad = n_quad, loglik = fit$loglik, converged = fit$converged, grad_norm = fit$grad_norm,
    fit_time_sec = fit_time, sigma_alpha = sigma_alpha_of(fit), se_sigma_alpha = se_sigma_alpha_of(fit),
    ape_lag = ape_lag, wald_wooldridge_aux = wald, naive_pooled_probit_lag = naive,
    share_y1 = mean(sub[[dv]]), instability = inst
  )
  jsonlite::write_json(diag, sprintf("output/tables_r/core_%d_%s_diagnostics.json", year, dv),
                        auto_unbox = TRUE, digits = NA)

  cat(sprintf("[%d %s] N=%d persons=%d at_risk=%d APE(lag)=%.4f (se=%.4f) naive_coef=%.3f corrected_coef=%.3f sigma_a=%.3f unstable=%s converged=%s time=%.1fs\n",
              year, dv, fit$n_obs, fit$n_persons, n_lag0, ape_lag$ape, ape_lag$se, naive$coef_lag,
              beta_of(fit)[2], sigma_alpha_of(fit), inst$unstable, fit$converged, fit_time))
  diag
}

main_core_models <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  rows <- list()
  for (year in YEARS) for (dv in DVS) {
    diag <- run_one(year, dv)
    rows[[length(rows) + 1]] <- data.frame(
      year = diag$year, dv = diag$dv, n_obs = diag$n_obs, n_persons = diag$n_persons,
      n_lag0 = diag$instability$n_lag0,
      share_y1 = diag$share_y1, ape_lag = diag$ape_lag$ape, ape_lag_se = diag$ape_lag$se,
      sigma_alpha = diag$sigma_alpha, se_sigma_alpha = diag$se_sigma_alpha,
      wald_aux_stat = diag$wald_wooldridge_aux$stat, wald_aux_p = diag$wald_wooldridge_aux$p,
      naive_coef_lag = diag$naive_pooled_probit_lag$coef_lag, converged = diag$converged,
      thin_at_risk = diag$instability$thin_at_risk,
      sigma_alpha_near_boundary = diag$instability$sigma_alpha_near_boundary,
      quasi_separation = diag$instability$quasi_separation,
      unstable = diag$instability$unstable
    )
  }
  out <- bind_rows(rows)
  write_csv(out, "output/tables_r/core_models_summary.csv")
  cat("\nwrote output/tables_r/core_models_summary.csv\n")
  out
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_core_models()
}
