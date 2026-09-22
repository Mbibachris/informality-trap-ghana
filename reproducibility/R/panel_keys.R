# Within-year panel keys. R port of scripts/panel_keys.py -- hhid cannot
# link a household across quarters (bakes the quarter into its own value);
# the stable key is (cluster, HholdID) for the household and
# (cluster, HholdID, personid) for the person, within a single year.

library(dplyr)

add_keys <- function(df) {
  df %>%
    mutate(
      hh_key = paste(as.integer(cluster), as.integer(HholdID), sep = "_"),
      person_id = paste(hh_key, as.integer(personid), sep = "_"),
      # `qtr` is a running counter across the three raw files (1-4 in 2022,
      # 5-8 in 2023, 9-12 in 2024), NOT a within-year 1-4 index -- parse
      # t_within_year from the last character of the `quarter` string
      # ("2022Q1" -> 1) instead, which is reliable regardless of year.
      t_within_year = as.integer(substr(quarter, nchar(quarter), nchar(quarter)))
    )
}

validate_keys <- function(df) {
  n_rows <- nrow(df)
  n_persons <- n_distinct(df$person_id)

  q_cov <- df %>% group_by(person_id) %>% summarise(nq = n_distinct(t_within_year), .groups = "drop")
  dist <- table(q_cov$nq)
  share_ge3 <- mean(q_cov$nq >= 3)
  share_all4 <- mean(q_cov$nq == 4)

  out <- list(
    n_person_quarter_rows = n_rows,
    n_unique_persons = n_persons,
    quarters_observed_distribution = as.list(dist),
    share_ge3_quarters = share_ge3,
    share_all4_quarters = share_all4
  )

  sex_col <- if ("sex" %in% names(df)) "sex" else if ("s1aq1" %in% names(df)) "s1aq1" else NA
  if (!is.na(sex_col)) {
    multi <- q_cov$person_id[q_cov$nq >= 2]
    sub <- df[df$person_id %in% multi, c("person_id", sex_col)]
    names(sub)[2] <- "sexval"
    sex_nunique <- sub %>% group_by(person_id) %>% summarise(nu = n_distinct(sexval), .groups = "drop")
    out$share_multiquarter_sex_constant <- mean(sex_nunique$nu == 1)
  }
  out
}
