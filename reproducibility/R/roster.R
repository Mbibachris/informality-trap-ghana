# Household-roster aggregates. R port of scripts/roster.py -- household
# size/composition are not raw columns; counted from roster rows sharing a
# household in the same quarter.

library(dplyr)

add_household_size_and_children <- function(df) {
  df %>%
    group_by(hh_key, t_within_year) %>%
    mutate(
      hh_size = n_distinct(person_id),
      hh_children_u15 = sum(age < 15, na.rm = TRUE)
    ) %>%
    ungroup()
}

# Must be called after construct::build_dependent_variables() (needs employed_it).
add_other_earners <- function(df) {
  df %>%
    group_by(hh_key, t_within_year) %>%
    mutate(
      .hh_employed_total = sum(as.integer(employed_it), na.rm = TRUE),
      hh_other_earners = pmax(.hh_employed_total - as.integer(employed_it), 0)
    ) %>%
    ungroup() %>%
    select(-.hh_employed_total)
}
