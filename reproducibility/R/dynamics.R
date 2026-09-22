# Lags, initial conditions, within-person means (Wooldridge device), and
# sample-structure/attrition columns. R port of scripts/dynamics.py.
# Assumes the panel is a single year and gets sorted by (person_id, t) here.

library(dplyr)
library(stringr)

DV_COLS <- c("informal_it", "vulnerable_it")

TIME_VARYING_NUMERIC_COVARIATES <- c("hh_size", "hh_children_u15", "hh_other_earners", "actual_hours")

# pandas' `SeriesGroupBy.transform("first")` -- what scripts/dynamics.py
# actually uses to build the Wooldridge initial condition -- defaults to
# skipna=True: it returns the first NON-MISSING value in the group, not
# literally the value at the group's first row. Confirmed empirically
# (pandas 2.3.3): transform("first") on [NA, 1, 0] returns 1, not NA. R's
# dplyr::first() has no such skip -- it always returns the literal first
# element. This function replicates pandas' actual (skipna) behaviour so the
# initial condition matches the already-validated Python pipeline exactly;
# returns NA only if every value in the group is NA (person never observed
# with a defined DV, e.g. never employed all year).
first_non_na <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) return(NA)
  x[idx[1]]
}

add_lags_and_initial_conditions <- function(df, dv_cols = DV_COLS) {
  df <- df %>% arrange(person_id, t_within_year) %>% group_by(person_id)
  for (col in dv_cols) {
    lag_name <- paste0("L_", col)
    i1_name <- paste0(str_remove(col, "_it$"), "_i1")
    df <- df %>%
      mutate(
        !!lag_name := dplyr::lag(.data[[col]], 1),
        !!i1_name := first_non_na(.data[[col]])
      )
  }
  df %>% ungroup()
}

add_within_person_means <- function(df, covariates = TIME_VARYING_NUMERIC_COVARIATES) {
  df <- df %>% group_by(person_id)
  for (col in covariates) {
    if (!col %in% names(df)) next
    mean_name <- paste0("mean_", col, "_i")
    df <- df %>% mutate(
      !!col := as.numeric(.data[[col]]),
      !!mean_name := mean(.data[[col]], na.rm = TRUE)
    )
  }
  df %>% ungroup()
}

add_within_person_category_shares <- function(df, col, categories = NULL) {
  if (is.null(categories)) categories <- sort(unique(na.omit(df[[col]])))
  df <- df %>% group_by(person_id)
  for (cat in categories) {
    safe_cat <- tolower(gsub(" ", "_", cat))
    out_name <- paste0("mean_", col, "_i__", safe_cat)
    df <- df %>% mutate(
      !!out_name := {
        ind <- ifelse(is.na(.data[[col]]), NA_real_, as.numeric(.data[[col]] == cat))
        mean(ind, na.rm = TRUE)
      }
    )
  }
  df %>% ungroup()
}

add_sample_structure <- function(df) {
  df %>%
    group_by(person_id) %>%
    mutate(
      n_quarters_i = n_distinct(t_within_year),
      balanced_i = as.integer(n_quarters_i == 4),
      .last_q = max(t_within_year),
      attrition_i = as.integer(.last_q < 4),
      quarter_of_exit = ifelse(attrition_i == 1, .last_q, NA_integer_)
    ) %>%
    ungroup() %>%
    select(-.last_q)
}

build_dynamics <- function(df) {
  df <- add_lags_and_initial_conditions(df)
  df <- add_within_person_means(df)
  df <- add_sample_structure(df)
  df
}
