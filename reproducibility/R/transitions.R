# Quarter-to-quarter transition matrices for the 4-state labour status.
# R port of scripts/transitions.py.

library(dplyr)
library(tidyr)

STATE_ORDER <- c("formal_employed", "informal_vulnerable_employed", "unemployed", "out_of_lf")

quarter_pairs <- function(df) {
  df <- df %>% arrange(person_id, t_within_year) %>% group_by(person_id) %>%
    mutate(prev_t = dplyr::lag(t_within_year, 1), prev_status = dplyr::lag(labour_status4, 1)) %>%
    ungroup() %>%
    filter(prev_t == t_within_year - 1) %>%
    select(person_id, t_within_year, prev_status, status = labour_status4)
  df
}

transition_matrix <- function(df, weight_col = "weight_person") {
  pairs <- quarter_pairs(df)
  if (!is.null(weight_col) && weight_col %in% names(df)) {
    w <- df %>% group_by(person_id) %>% summarise(w = dplyr::first(.data[[weight_col]]), .groups = "drop")
    pairs <- pairs %>% left_join(w, by = "person_id") %>% mutate(w = ifelse(is.na(w), 1.0, w))
  } else {
    pairs$w <- 1.0
  }
  pairs$prev_status <- factor(pairs$prev_status, levels = STATE_ORDER)
  pairs$status <- factor(pairs$status, levels = STATE_ORDER)
  tab <- pairs %>% group_by(prev_status, status, .drop = FALSE) %>% summarise(w = sum(w), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = status, values_from = w, values_fill = 0)
  mat <- as.matrix(tab[, STATE_ORDER])
  rownames(mat) <- as.character(tab$prev_status)
  mat <- mat[STATE_ORDER, STATE_ORDER]
  row_totals <- rowSums(mat)
  sweep(mat, 1, ifelse(row_totals == 0, NA, row_totals), "/")
}

gross_flows <- function(df) {
  pairs <- quarter_pairs(df)
  pairs$prev_status <- factor(pairs$prev_status, levels = STATE_ORDER)
  pairs$status <- factor(pairs$status, levels = STATE_ORDER)
  tab <- table(pairs$prev_status, pairs$status)
  as.data.frame.matrix(tab)
}
