# Step 6 robustness battery (analysis plan §9), lead DV vulnerable_it
# unless noted. R port of scripts/run_robustness.py -- WITHOUT the
# simplified Heckman (1981) approximation, which degenerated (sigma_alpha
# -> boundary ~1e-7) in the original Python run and was dropped per
# instruction rather than re-implemented.
#
# 1. Alternative DV -- covered by the core-model comparison (run_core_models.R).
# 2. Attrition -- balanced vs full unbalanced APE, plus a non-random-
#    attrition test (attrition_i ~ dv_i1 + covariates).
# 3. Alternative estimator -- Mundlak/CRE pooled probit (same X, no random
#    intercept) as a cross-check on the Wooldridge beta.
# 4. Small-T sensitivity -- the two-estimator APE comparison itself.
# 5. SSNIT route -- rebuild informal_it on the employee subsample using
#    ssnit_flag instead of soc_sec_entitled, refit, compare.

library(dplyr)
library(readr)
library(marginaleffects)
library(sandwich)

YEARS <- c(2022, 2023, 2024)
LEAD_DV <- "vulnerable_it"

balanced_vs_unbalanced <- function(year, dv = LEAD_DV, n_quad = 12) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub_full <- estimation_universe(df, dv)
  sub_bal <- sub_full %>% filter(balanced_i == 1)

  d_f <- build_design(sub_full, dv)
  fit_f <- fit_re_probit(d_f$X, d_f$y, d_f$person_idx, d_f$cluster_idx, d_f$cols, n_quad = n_quad)
  ape_f <- ape(fit_f, "L_dv")

  d_b <- build_design(sub_bal, dv)
  fit_b <- fit_re_probit(d_b$X, d_b$y, d_b$person_idx, d_b$cluster_idx, d_b$cols, n_quad = n_quad)
  ape_b <- ape(fit_b, "L_dv")

  out <- list(year = year, dv = dv, full_n = fit_f$n_obs, full_ape = ape_f,
              balanced_n = fit_b$n_obs, balanced_ape = ape_b)
  cat(sprintf("[attrition %d] full APE=%.4f (n=%d) | balanced APE=%.4f (n=%d)\n",
              year, ape_f$ape, fit_f$n_obs, ape_b$ape, fit_b$n_obs))
  out
}

nonrandom_attrition_test <- function(year, dv = LEAD_DV) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  stem <- dv_stem(dv)
  i1_col <- paste0(stem, "_i1")
  person <- df %>% arrange(person_id, t_within_year) %>% group_by(person_id) %>% slice(1) %>% ungroup()
  required <- c(i1_col, "attrition_i", FIXED_CATS)
  person <- person[stats::complete.cases(person[, required]), ]

  pieces <- list(const = rep(1, nrow(person)), dv_i1 = as.numeric(person[[i1_col]]))
  for (cat in FIXED_CATS) {
    for (lvl in setdiff(sort(unique(person[[cat]])), BASE_LEVEL[[cat]])) {
      pieces[[paste0(cat, "_", lvl)]] <- as.numeric(person[[cat]] == lvl)
    }
  }
  df_glm <- as.data.frame(pieces, check.names = FALSE)
  df_glm$y <- as.numeric(person$attrition_i)
  keep <- names(pieces)
  form <- as.formula(paste("y ~ 0 +", paste(sprintf("`%s`", keep), collapse = " + ")))
  m <- glm(form, data = df_glm, family = binomial(link = "probit"))
  vc <- sandwich::vcovCL(m, cluster = factor(person$cluster), type = "HC1")
  b <- coef(m)["dv_i1"]; se <- sqrt(vc["dv_i1", "dv_i1"])
  pval <- 2 * (1 - pnorm(abs(b / se)))

  # The FULL coefficient table, not just dv_i1 --
  # every regressor (initial status + all covariates + constant), cluster-
  # robust SE, z, p. Written per year so the attrition model is as
  # auditable as the core state-dependence models.
  se_all <- sqrt(diag(vc))
  z_all <- coef(m) / se_all
  p_all <- 2 * (1 - pnorm(abs(z_all)))
  full_ct <- data.frame(param = gsub("`", "", names(coef(m))), coef = unname(coef(m)), se = unname(se_all),
                         z = unname(z_all), p = unname(p_all))
  write_csv(full_ct, sprintf("output/tables_r/attrition_probit_%d_%s_coefs.csv", year, dv))

  out <- list(year = year, dv = dv, n_persons = nrow(person), attrition_rate = mean(df_glm$y),
              coef_dv_i1_on_attrition = unname(b), se_dv_i1_on_attrition = unname(se),
              p_dv_i1_on_attrition = unname(pval), full_coef_table_csv = sprintf("attrition_probit_%d_%s_coefs.csv", year, dv))
  cat(sprintf("[nonrandom attrition %d] coef(dv_i1->attrition)=%.3f (se %.3f, p=%.4f) -- full table (%d params) written\n",
              year, out$coef_dv_i1_on_attrition, out$se_dv_i1_on_attrition, out$p_dv_i1_on_attrition, nrow(full_ct)))
  out
}

mundlak_cre_pooled_probit <- function(year, dv = LEAD_DV) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  sub <- estimation_universe(df, dv)
  d <- build_design(sub, dv)

  df_glm <- as.data.frame(d$X, check.names = FALSE)
  df_glm$y <- d$y
  keep <- d$cols
  form <- as.formula(paste("y ~ 0 +", paste(sprintf("`%s`", keep), collapse = " + ")))
  m <- glm(form, data = df_glm, family = binomial(link = "probit"))
  vc <- sandwich::vcovCL(m, cluster = factor(d$cluster_idx), type = "HC1")
  b <- coef(m)["L_dv"]; se <- sqrt(vc["L_dv", "L_dv"])
  marg <- avg_comparisons(m, variables = list(L_dv = 0:1), vcov = vc)

  out <- list(year = year, dv = dv, n_obs = nrow(sub), coef_lag = unname(b), se_lag = unname(se),
              ame_lag = marg$estimate[1], ame_lag_se = marg$std.error[1])
  cat(sprintf("[Mundlak/CRE pooled probit %d] AME(lag)=%.4f (se %.4f)\n", year, out$ame_lag, out$ame_lag_se))
  out
}

ssnit_route_informal <- function(year, n_quad = 12) {
  df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)
  is_emp_universe <- df$status_in_job %in% c("Paid employee", "Casual worker")
  written <- !is.na(df$has_contract) & df$has_contract == "Yes, written"
  ssnit_yes <- grepl("Yes", df$ssnit_flag, fixed = TRUE) & !is.na(df$ssnit_flag)
  informal_alt <- df$informal_it
  mask_emp_nonmiss <- is_emp_universe & !is.na(df$ssnit_flag) & !is.na(df$has_contract)
  informal_alt[mask_emp_nonmiss] <- as.numeric(!(written[mask_emp_nonmiss] & ssnit_yes[mask_emp_nonmiss]))
  informal_alt[is_emp_universe & !mask_emp_nonmiss] <- NA
  df$informal_ssnit_it <- informal_alt

  df <- df %>% arrange(person_id, t_within_year) %>% group_by(person_id) %>%
    mutate(L_informal_ssnit_it = dplyr::lag(informal_ssnit_it, 1),
           informal_ssnit_i1 = first_non_na(informal_ssnit_it)) %>%
    ungroup()

  sub <- estimation_universe(df, "informal_ssnit_it")
  if (nrow(sub) < 200) {
    cat(sprintf("[SSNIT route %d] sample too small (n=%d) -- skipping model fit\n", year, nrow(sub)))
    return(list(year = year, n_obs = nrow(sub), skipped = TRUE))
  }

  d <- build_design(sub, "informal_ssnit_it")
  fit <- fit_re_probit(d$X, d$y, d$person_idx, d$cluster_idx, d$cols, n_quad = n_quad)
  a <- ape(fit, "L_dv")
  out <- list(year = year, n_obs = fit$n_obs, ape_lag = a, sigma_alpha = sigma_alpha_of(fit), converged = fit$converged)
  cat(sprintf("[SSNIT route %d] N=%d APE(lag)=%.4f (se %.4f)\n", year, fit$n_obs, a$ape, a$se))
  out
}

main_robustness <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  results <- list(attrition_balanced_vs_unbalanced = list(), nonrandom_attrition_test = list(),
                   mundlak_cre_pooled_probit = list(), ssnit_route = list())
  for (year in YEARS) {
    results$attrition_balanced_vs_unbalanced[[length(results$attrition_balanced_vs_unbalanced)+1]] <- balanced_vs_unbalanced(year)
    results$nonrandom_attrition_test[[length(results$nonrandom_attrition_test)+1]] <- nonrandom_attrition_test(year)
    results$mundlak_cre_pooled_probit[[length(results$mundlak_cre_pooled_probit)+1]] <- mundlak_cre_pooled_probit(year)
    results$ssnit_route[[length(results$ssnit_route)+1]] <- ssnit_route_informal(year)
  }
  jsonlite::write_json(results, "output/tables_r/robustness_battery.json", auto_unbox = TRUE, digits = NA)
  cat("\nwrote output/tables_r/robustness_battery.json\n")
  results
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R"); source("R/re_probit.R"); source("R/dynamics.R")
  main_robustness()
}
