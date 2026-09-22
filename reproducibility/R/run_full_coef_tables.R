# Full parameter vectors + fit statistics for
# every core model. Does NOT refit -- reuses the coefficient tables
# run_core_models.R already writes per model (core_<year>_<dv>_coefs.csv,
# which is coef_table.re_probit_result(fit): every design-matrix column,
# labelled, with SE/z/p, plus the implied sigma_alpha row) and the
# diagnostics JSON (loglik, N, convergence). Only new work here: AIC/BIC
# and reshaping into one long-format table per DV (main-text: vulnerable,
# appendix: informal), each with a footer row carrying the corrected
# discrete lag APE + SE (NOT the raw L_dv coefficient above it -- the two
# are different quantities, see analysis-plan §5 / R/re_probit.R::ape()).

library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)

YEARS <- c(2022, 2023, 2024)
DVS <- c("vulnerable_it", "informal_it")

fit_statistics_row <- function(year, dv) {
  diag <- fromJSON(sprintf("output/tables_r/core_%d_%s_diagnostics.json", year, dv))
  ct <- read_csv(sprintf("output/tables_r/core_%d_%s_coefs.csv", year, dv), show_col_types = FALSE)
  # number of estimated parameters = every row except the LAST ("sigma_alpha
  # (implied)"), which is not a free parameter -- it's exp(log_sigma_alpha),
  # already counted via the log_sigma_alpha row above it.
  k <- sum(ct$param != "sigma_alpha (implied)")
  loglik <- diag$loglik
  n_obs <- diag$n_obs
  n_persons <- diag$n_persons
  aic <- -2 * loglik + 2 * k
  # BIC's "N" is ambiguous for a random-intercept model (person-quarters vs.
  # independent persons/clusters); report both rather than picking one
  # silently, and flag which is conventionally preferred.
  bic_nobs <- -2 * loglik + k * log(n_obs)
  bic_npersons <- -2 * loglik + k * log(n_persons)
  data.frame(
    year = year, dv = dv, k_params = k, loglik = loglik,
    aic = aic, bic_n_obs = bic_nobs, bic_n_persons = bic_npersons,
    n_obs = n_obs, n_persons = n_persons, n_clusters = diag$n_clusters,
    sigma_alpha = diag$sigma_alpha, se_sigma_alpha = diag$se_sigma_alpha,
    ape_lag = diag$ape_lag$ape, ape_lag_se = diag$ape_lag$se,
    converged = diag$converged, grad_norm = diag$grad_norm, n_quad = diag$n_quad
  )
}

full_coef_long <- function(dv) {
  rows <- lapply(YEARS, function(y) {
    ct <- read_csv(sprintf("output/tables_r/core_%d_%s_coefs.csv", y, dv), show_col_types = FALSE)
    ct$year <- y
    ct
  })
  bind_rows(rows) %>% select(year, param, coef, se, z, p)
}

main_full_coef_tables <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)

  fit_stats <- bind_rows(lapply(YEARS, function(y) bind_rows(lapply(DVS, function(dv) fit_statistics_row(y, dv)))))
  write_csv(fit_stats, "output/tables_r/fit_statistics.csv")
  write_json(fit_stats, "output/tables_r/fit_statistics.json", auto_unbox = TRUE, digits = NA)

  for (dv in DVS) {
    long <- full_coef_long(dv)
    write_csv(long, sprintf("output/tables_r/full_coef_%s_long.csv", dv))
    write_json(long, sprintf("output/tables_r/full_coef_%s_long.json", dv), auto_unbox = TRUE, digits = NA)

    wide <- long %>%
      mutate(cell = sprintf("%.4f (%.4f)", coef, se)) %>%
      select(year, param, cell) %>%
      pivot_wider(names_from = year, values_from = cell)
    write_csv(wide, sprintf("output/tables_r/full_coef_%s_wide.csv", dv))
    write_json(wide, sprintf("output/tables_r/full_coef_%s_wide.json", dv), auto_unbox = TRUE, digits = NA)
  }

  cat("wrote output/tables_r/fit_statistics.{csv,json} and full_coef_{vulnerable_it,informal_it}_{long,wide}.{csv,json}\n")
  print(fit_stats)
  invisible(list(fit_stats = fit_stats))
}

if (sys.nframe() == 0) {
  setwd(here::here())
  main_full_coef_tables()
}
