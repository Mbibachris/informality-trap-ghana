# Design-matrix builder for the core spec (analysis plan §5). R port of
# scripts/design.py -- shared by the per-year core models, the pooled
# crisis model, and heterogeneity cuts.

library(dplyr)

TIME_VARYING_NUM <- c("hh_size", "hh_children_u15", "hh_other_earners", "actual_hours")
WOOLDRIDGE_MEAN_COLS <- c(
  "mean_hh_size_i", "mean_hh_children_u15_i", "mean_hh_other_earners_i",
  "mean_actual_hours_i", "mean_sector3_i__agriculture", "mean_sector3_i__services"
)
FIXED_CATS <- c("sex", "age_band", "edu_cat", "urbrur", "region")
BASE_LEVEL <- list(
  sex = "Male", age_band = "prime_25_54", edu_cat = "none",
  urbrur = "Rural", region = "Greater Accra", sector3 = "industry"
)

dv_stem <- function(dv) sub("_it$", "", dv)

estimation_universe <- function(df, dv) {
  lag_col <- paste0("L_", dv)
  i1_col <- paste0(dv_stem(dv), "_i1")
  sub <- df %>% filter(employed_it == TRUE, !is.na(.data[[lag_col]]))
  required <- c(dv, lag_col, i1_col, TIME_VARYING_NUM, "sector3", FIXED_CATS,
                WOOLDRIDGE_MEAN_COLS, "t_within_year", "person_id", "cluster")
  sub <- sub[stats::complete.cases(sub[, required]), ]
  sub
}

.dummy_matrix <- function(x, prefix, base_level) {
  levels_present <- sort(unique(x))
  levels_present <- setdiff(levels_present, base_level)
  out <- matrix(0, nrow = length(x), ncol = length(levels_present))
  colnames(out) <- paste0(prefix, "_", levels_present)
  for (j in seq_along(levels_present)) out[, j] <- as.numeric(x == levels_present[j])
  out
}

# Returns list(X, y, person_idx, cluster_idx, cols, standardize_stats).
# Column order: intercept, L_dv, time-varying nums (standardized), sector3
# dummies, fixed-cat dummies, wooldridge aux (dv_i1 + means, standardized on
# the SAME scale as their raw counterparts), t_within_year dummies (base=2),
# then any extra_terms (list of name=vector) appended raw.
build_design <- function(sub, dv, extra_terms = NULL, standardize_stats = NULL) {
  lag_col <- paste0("L_", dv)
  n <- nrow(sub)
  new_stats <- is.null(standardize_stats)
  stats_store <- if (new_stats) list() else standardize_stats

  pieces <- list(const = rep(1, n))
  pieces[["L_dv"]] <- as.numeric(sub[[lag_col]])

  for (col in TIME_VARYING_NUM) {
    v <- as.numeric(sub[[col]])
    if (new_stats) {
      mu <- mean(v); sd <- stats::sd(v); if (is.na(sd) || sd < 1e-8) sd <- 1
      stats_store[[col]] <- c(mu = mu, sd = sd)
    }
    ms <- stats_store[[col]]
    pieces[[col]] <- (v - ms["mu"]) / ms["sd"]
  }

  sector3_dum <- .dummy_matrix(sub$sector3, "sector3", BASE_LEVEL$sector3)
  for (cn in colnames(sector3_dum)) pieces[[cn]] <- sector3_dum[, cn]

  for (cat in FIXED_CATS) {
    dum <- .dummy_matrix(sub[[cat]], cat, BASE_LEVEL[[cat]])
    for (cn in colnames(dum)) pieces[[cn]] <- dum[, cn]
  }

  pieces[["dv_i1"]] <- as.numeric(sub[[paste0(dv_stem(dv), "_i1")]])

  for (col in WOOLDRIDGE_MEAN_COLS) {
    v <- as.numeric(sub[[col]])
    if (new_stats) {
      mu <- mean(v); sd <- stats::sd(v); if (is.na(sd) || sd < 1e-8) sd <- 1
      stats_store[[col]] <- c(mu = mu, sd = sd)
    }
    ms <- stats_store[[col]]
    pieces[[col]] <- (v - ms["mu"]) / ms["sd"]
  }

  t_dum <- .dummy_matrix(as.integer(sub$t_within_year), "t", 2)
  for (cn in colnames(t_dum)) pieces[[cn]] <- t_dum[, cn]

  if (!is.null(extra_terms)) {
    for (nm in names(extra_terms)) pieces[[nm]] <- as.numeric(extra_terms[[nm]])
  }

  cols <- names(pieces)
  X <- do.call(cbind, pieces)
  colnames(X) <- cols

  y <- as.numeric(sub[[dv]])
  person_idx <- as.integer(factor(sub$person_id))
  cluster_idx <- as.integer(factor(sub$cluster))

  list(X = X, y = y, person_idx = person_idx, cluster_idx = cluster_idx,
       cols = cols, standardize_stats = stats_store)
}
