# Simple non-random-attrition test: baseline (first-observed-quarter)
# characteristics of attriters vs completers. R port of scripts/attrition.py.

library(dplyr)

BASELINE_NUMERIC_COLS <- c("age", "informal_i1", "vulnerable_i1", "hh_size")

.welch_t <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  n1 <- length(a); n2 <- length(b)
  if (n1 < 2 || n2 < 2) return(c(diff = NA_real_, t = NA_real_))
  m1 <- mean(a); m2 <- mean(b)
  v1 <- var(a); v2 <- var(b)
  se <- sqrt(v1 / n1 + v2 / n2)
  if (se == 0) return(c(diff = NA_real_, t = NA_real_))
  c(diff = m1 - m2, t = (m1 - m2) / se)
}

attrition_diagnostics <- function(df, numeric_cols = BASELINE_NUMERIC_COLS) {
  baseline <- df %>% arrange(person_id, t_within_year) %>% group_by(person_id) %>% slice(1) %>% ungroup()
  rows <- list()
  for (col in numeric_cols) {
    if (!col %in% names(baseline)) next
    vals <- as.numeric(baseline[[col]])
    attriters <- vals[baseline$attrition_i == 1]
    completers <- vals[baseline$attrition_i == 0]
    dt <- .welch_t(attriters, completers)
    rows[[length(rows) + 1]] <- data.frame(
      variable = col,
      mean_attriters = mean(attriters, na.rm = TRUE),
      mean_completers = mean(completers, na.rm = TRUE),
      difference = dt["diff"],
      welch_t_stat = dt["t"],
      flag_large_diff = if (!is.na(dt["t"])) abs(dt["t"]) > 1.96 else NA
    )
  }
  bind_rows(rows)
}
