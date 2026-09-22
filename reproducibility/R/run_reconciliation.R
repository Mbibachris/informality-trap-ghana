# Two reconciliation diagnostics.
#
# 1. Per-year vs pooled reconciliation. Decomposes the per-year-vs-
# pooled APE gap (e.g. informal_it 2023: 0.022 vs 0.215) into a "sigma_alpha
# swap" component and a "beta re-identification" component, using the
# ACTUAL saved coefficient vectors from both specs applied to the SAME
# estimation-universe design matrix -- no refitting, just forward
# evaluation of the closed-form APE (R/re_probit.R::ape()'s formula,
# Phi(x'beta/sqrt(1+sigma_alpha^2))) at swapped inputs. Standard two-way
# decomposition: swap sigma_alpha only (keep per-year beta), swap beta only
# (keep per-year sigma_alpha); the leftover vs. the total gap is the
# interaction/residual term (APE is not additive in beta and sigma_alpha,
# so an exact-zero residual is not expected).
#
# Task 5: puts the Mundlak/CRE pooled-probit "AME" and the RE-probit APE on
# one scale and tests whether their ratio equals the pure deflation factor
# sqrt(1+sigma_alpha^2) -- i.e. whether the gap is "just" the random-
# intercept integration, or also reflects a different fitted beta.

library(dplyr)
library(readr)
library(jsonlite)
library(sandwich)
library(marginaleffects)

YEARS <- c(2022, 2023, 2024)
DVS <- c("vulnerable_it", "informal_it")

.named_theta <- function(coef_csv_path) {
  ct <- read_csv(coef_csv_path, show_col_types = FALSE)
  ct <- ct[ct$param != "sigma_alpha (implied)", ]
  setNames(ct$coef, ct$param)
}

# discrete APE of L_dv, evaluated at an arbitrary (X, beta, sigma_alpha) --
# same closed form as R/re_probit.R::ape(), standalone (not tied to a
# fitted `re_probit_result` object) so it can be evaluated at swapped
# beta/sigma_alpha combinations that were never actually optimized to.
.ape_at <- function(X, beta, sigma_alpha) {
  j <- match("L_dv", colnames(X))
  denom <- sqrt(1 + sigma_alpha^2)
  X1 <- X; X1[, j] <- 1
  X0 <- X; X0[, j] <- 0
  mean(pnorm(as.numeric(X1 %*% beta) / denom) - pnorm(as.numeric(X0 %*% beta) / denom))
}

reconcile_one <- function(year, dv) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  d <- build_design(sub, dv)  # 40-column per-year basis (no year interactions)
  X <- d$X

  theta_py <- .named_theta(sprintf("output/tables_r/core_%d_%s_coefs.csv", year, dv))
  beta_py <- theta_py[d$cols]
  sigma_py <- exp(theta_py[["log_sigma_alpha"]])

  theta_pool <- .named_theta(sprintf("output/tables_r/rq3_pooled_%s_coefs.csv", dv))
  sigma_pool <- exp(theta_pool[["log_sigma_alpha"]])

  # effective year-specific beta implied by the pooled model: same shared
  # covariate coefficients (pooling forces these equal across years), but
  # intercept and lag shifted by that year's interaction terms (2022 = base,
  # no shift).
  beta_pool_eff <- theta_pool[d$cols]  # base coefficients, same 40 names
  if (year == 2023) {
    beta_pool_eff[["const"]] <- beta_pool_eff[["const"]] + theta_pool[["year_2023"]]
    beta_pool_eff[["L_dv"]] <- beta_pool_eff[["L_dv"]] + theta_pool[["year_2023_x_lag"]]
  } else if (year == 2024) {
    beta_pool_eff[["const"]] <- beta_pool_eff[["const"]] + theta_pool[["year_2024"]]
    beta_pool_eff[["L_dv"]] <- beta_pool_eff[["L_dv"]] + theta_pool[["year_2024_x_lag"]]
  }

  ape_py_actual <- .ape_at(X, beta_py, sigma_py)
  ape_pool_slice <- .ape_at(X, beta_pool_eff, sigma_pool)
  ape_sigma_swap <- .ape_at(X, beta_py, sigma_pool)        # per-year beta, pooled sigma
  ape_beta_swap <- .ape_at(X, beta_pool_eff, sigma_py)      # pooled-implied beta, per-year sigma

  total_gap <- ape_pool_slice - ape_py_actual
  sigma_contrib <- ape_sigma_swap - ape_py_actual
  beta_contrib <- ape_beta_swap - ape_py_actual
  residual <- total_gap - sigma_contrib - beta_contrib

  data.frame(
    year = year, dv = dv,
    sigma_alpha_per_year = sigma_py, sigma_alpha_pooled = sigma_pool,
    deflation_per_year = sqrt(1 + sigma_py^2), deflation_pooled = sqrt(1 + sigma_pool^2),
    ape_per_year_actual = ape_py_actual, ape_pooled_slice = ape_pool_slice,
    total_gap = total_gap,
    sigma_swap_contribution = sigma_contrib,
    beta_swap_contribution = beta_contrib,
    residual_interaction = residual,
    pct_gap_from_sigma = 100 * sigma_contrib / total_gap,
    pct_gap_from_beta = 100 * beta_contrib / total_gap
  )
}

# --- Task 5: Mundlak scaling -------------------------------------------

mundlak_probit_ape <- function(year, dv) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  d <- build_design(sub, dv)
  df_glm <- as.data.frame(d$X, check.names = FALSE)
  df_glm$y <- d$y
  form <- as.formula(paste("y ~ 0 +", paste(sprintf("`%s`", d$cols), collapse = " + ")))
  m <- glm(form, data = df_glm, family = binomial(link = "probit"))
  vc <- sandwich::vcovCL(m, cluster = factor(d$cluster_idx), type = "HC1")
  marg <- avg_comparisons(m, variables = list(L_dv = 0:1), vcov = vc)
  list(year = year, dv = dv, ame_lag = marg$estimate[1], ame_lag_se = marg$std.error[1])
}

mundlak_scaling_one <- function(year, dv) {
  # vulnerable_it Mundlak numbers already exist in robustness_battery.json
  # (lead-DV robustness cut); informal_it was not run there (lead-DV-only
  # scope) -- computed fresh here for symmetry, same glm/avg_comparisons
  # machinery, not the slow quadrature engine.
  if (dv == "vulnerable_it") {
    rob <- fromJSON("output/tables_r/robustness_battery.json", simplifyVector = FALSE)
    hit <- Filter(function(x) x$year == year, rob$mundlak_cre_pooled_probit)[[1]]
    mundlak <- list(year = year, dv = dv, ame_lag = as.numeric(hit$ame_lag), ame_lag_se = as.numeric(hit$ame_lag_se))
  } else {
    mundlak <- mundlak_probit_ape(year, dv)
  }

  re_diag <- fromJSON(sprintf("output/tables_r/core_%d_%s_diagnostics.json", year, dv), simplifyVector = FALSE)
  ape_re <- as.numeric(re_diag$ape_lag$ape)
  sigma_alpha <- as.numeric(re_diag$sigma_alpha)
  deflation <- sqrt(1 + sigma_alpha^2)
  ratio <- mundlak$ame_lag / ape_re

  data.frame(
    year = year, dv = dv,
    mundlak_definition = "discrete P(1)-P(0) contrast via marginaleffects::avg_comparisons(variables=list(L_dv=0:1)) on the plain (no random intercept) pooled/Mundlak probit fit -- an APE, not a raw probit coefficient, but with NO sqrt(1+sigma_alpha^2) deflation applied (the model has no sigma_alpha term at all).",
    mundlak_ame = mundlak$ame_lag, mundlak_se = mundlak$ame_lag_se,
    re_probit_ape = ape_re, sigma_alpha = sigma_alpha, deflation_factor = deflation,
    ratio_mundlak_to_re = ratio,
    ratio_minus_deflation = ratio - deflation,
    verdict = ifelse(abs(ratio - deflation) < 0.25,
                      "ratio ~= sqrt(1+sigma_alpha^2): gap plausibly explained by RE integration alone",
                      "ratio far from sqrt(1+sigma_alpha^2): gap is NOT just RE integration -- beta is materially re-identified under pooling/Mundlak too")
  )
}

main_reconciliation <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)

  recon <- bind_rows(lapply(YEARS, function(y) bind_rows(lapply(DVS, function(dv) reconcile_one(y, dv)))))
  write_csv(recon, "output/tables_r/peryear_vs_pooled_reconciliation.csv")
  write_json(recon, "output/tables_r/peryear_vs_pooled_reconciliation.json", auto_unbox = TRUE, digits = NA)
  cat("wrote output/tables_r/peryear_vs_pooled_reconciliation.{csv,json}\n")
  print(recon %>% select(year, dv, sigma_alpha_per_year, sigma_alpha_pooled, total_gap, pct_gap_from_sigma, pct_gap_from_beta))

  scaling <- bind_rows(lapply(YEARS, function(y) bind_rows(lapply(DVS, function(dv) mundlak_scaling_one(y, dv)))))
  write_csv(scaling, "output/tables_r/mundlak_scaling.csv")
  write_json(scaling, "output/tables_r/mundlak_scaling.json", auto_unbox = TRUE, digits = NA)
  cat("\nwrote output/tables_r/mundlak_scaling.{csv,json}\n")
  print(scaling %>% select(year, dv, mundlak_ame, re_probit_ape, deflation_factor, ratio_mundlak_to_re, verdict))

  invisible(list(reconciliation = recon, mundlak_scaling = scaling))
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R")
  main_reconciliation()
}
