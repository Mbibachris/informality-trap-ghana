# Step 3 / RQ2 (analysis plan §6): within-2022 entry into informal/
# vulnerable employment vs inflation acceleration. R port of
# scripts/run_rq2_event_study.py -- see that file for the identification
# caveat (inflation only varies by quarter within 2022, i.e. at most 3
# independent macro data points identify the coefficient; descriptive-
# causal at best, not a natural experiment).

library(dplyr)
library(readr)
library(marginaleffects)
library(sandwich)

.load_2022_with_cpi <- function() {
  df <- read_csv("data/processed_r/2022_person_quarter_unbalanced.csv", show_col_types = FALSE)
  cpi <- read_csv("data/raw/cpi/ghana_cpi_quarterly.csv", show_col_types = FALSE)
  df %>% left_join(cpi, by = c("year", "t_within_year"))
}

run_entry_model <- function(dv, inflation_col) {
  df <- .load_2022_with_cpi()
  lag_col <- paste0("L_", dv)
  at_risk <- df %>% filter(employed_it == TRUE, .data[[lag_col]] == 0)
  required <- c(dv, inflation_col, TIME_VARYING_NUM, "sector3", FIXED_CATS, "cluster")
  at_risk <- at_risk[stats::complete.cases(at_risk[, required]), ]

  infl <- as.numeric(at_risk[[inflation_col]])
  infl_std <- (infl - mean(infl)) / stats::sd(infl)

  pieces <- list(const = rep(1, nrow(at_risk)), inflation_std = infl_std)
  for (c in TIME_VARYING_NUM) {
    v <- as.numeric(at_risk[[c]])
    sd <- stats::sd(v); if (is.na(sd) || sd < 1e-8) sd <- 1
    pieces[[c]] <- (v - mean(v)) / sd
  }
  sector3_dum <- matrix(0, nrow(at_risk), 0)
  for (lvl in setdiff(sort(unique(at_risk$sector3)), BASE_LEVEL$sector3)) {
    pieces[[paste0("sector3_", lvl)]] <- as.numeric(at_risk$sector3 == lvl)
  }
  for (cat in FIXED_CATS) {
    for (lvl in setdiff(sort(unique(at_risk[[cat]])), BASE_LEVEL[[cat]])) {
      pieces[[paste0(cat, "_", lvl)]] <- as.numeric(at_risk[[cat]] == lvl)
    }
  }

  df_glm <- as.data.frame(pieces, check.names = FALSE)
  df_glm$y <- as.numeric(at_risk[[dv]])
  keep <- names(pieces)
  form <- as.formula(paste("y ~ 0 +", paste(sprintf("`%s`", keep), collapse = " + ")))
  m <- glm(form, data = df_glm, family = binomial(link = "probit"))
  cluster_f <- factor(at_risk$cluster)
  vc <- sandwich::vcovCL(m, cluster = cluster_f, type = "HC1")

  b <- coef(m)["inflation_std"]; se <- sqrt(vc["inflation_std", "inflation_std"])
  pval <- 2 * (1 - pnorm(abs(b / se)))
  # inflation_std is continuous, so the derivative-based avg_slopes() (not
  # a discrete contrast) is the right marginal effect here.
  marg <- avg_slopes(m, variables = "inflation_std", vcov = vc)

  out <- list(
    dv = dv, inflation_measure = inflation_col, n_obs = nrow(at_risk),
    n_entered = sum(df_glm$y), entry_rate = mean(df_glm$y),
    coef_inflation = unname(b), se_inflation = unname(se), p_inflation = unname(pval),
    ame_inflation_per_sd = marg$estimate[1], ame_inflation_se = marg$std.error[1],
    converged = m$converged
  )
  cat(sprintf("[RQ2 entry %s ~ %s] N=%d entry_rate=%.3f AME(1sd infl)=%.4f (se %.4f, p=%.4f)\n",
              dv, inflation_col, out$n_obs, out$entry_rate, out$ame_inflation_per_sd,
              out$ame_inflation_se, out$p_inflation))
  out
}

early_entrant_persistence <- function(dv) {
  df <- read_csv("data/processed_r/2022_person_quarter_unbalanced.csv", show_col_types = FALSE)
  lag_col <- paste0("L_", dv)
  df <- df %>% arrange(person_id, t_within_year)

  entrants <- df %>% filter(t_within_year %in% c(2, 3), .data[[lag_col]] == 0, .data[[dv]] == 1) %>%
    select(person_id, entry_t = t_within_year)

  last_obs <- df %>% arrange(t_within_year) %>% group_by(person_id) %>% slice_tail(n = 1) %>% ungroup() %>%
    select(person_id, last_t = t_within_year, last_status = all_of(dv))

  merged <- entrants %>% left_join(last_obs, by = "person_id") %>% filter(last_t > entry_t)
  # pandas' Series.mean() skips NaN by default; R's mean() does not -- a
  # person's last observed quarter can itself have an undefined DV (e.g.
  # not employed that quarter), matching the Python behaviour needs na.rm.
  list(dv = dv, n_early_entrants_with_later_obs = nrow(merged),
       stay_rate_by_q4_or_last_obs = mean(merged$last_status, na.rm = TRUE))
}

main_rq2 <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  results <- list(entry_models = list(), persistence_of_early_entrants = list())
  for (dv in c("informal_it", "vulnerable_it")) {
    for (infl_col in c("inflation_yoy_qt", "inflation_accel_qt")) {
      results$entry_models[[length(results$entry_models) + 1]] <- run_entry_model(dv, infl_col)
    }
    pers <- early_entrant_persistence(dv)
    cat(sprintf("[RQ2 persistence %s] stay_rate=%.3f (n=%d)\n", dv, pers$stay_rate_by_q4_or_last_obs, pers$n_early_entrants_with_later_obs))
    results$persistence_of_early_entrants[[length(results$persistence_of_early_entrants) + 1]] <- pers
  }
  jsonlite::write_json(results, "output/tables_r/rq2_event_study.json", auto_unbox = TRUE, digits = NA)
  cat("\nwrote output/tables_r/rq2_event_study.json\n")
  results
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/design.R")
  main_rq2()
}
