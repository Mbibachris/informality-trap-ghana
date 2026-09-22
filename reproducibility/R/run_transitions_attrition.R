# 4-state transition matrices, gross flows, and attrition diagnostics, per
# year (analysis plan §4 Step 0 point 2/4). R port of the descriptive half
# of notebooks/03_descriptives_transitions.ipynb.

library(dplyr)
library(readr)

YEARS <- c(2022, 2023, 2024)

main_transitions_attrition <- function() {
  dir.create("output/tables_r", recursive = TRUE, showWarnings = FALSE)
  for (year in YEARS) {
    df <- read_csv(sprintf("data/processed_r/%d_person_quarter_unbalanced.csv", year), show_col_types = FALSE)

    tm <- transition_matrix(df)
    tm_df <- as.data.frame(tm)
    tm_df <- cbind(prev_status = rownames(tm_df), tm_df)
    write_csv(tm_df, sprintf("output/tables_r/%d_transition_matrix.csv", year))

    gf <- gross_flows(df)
    gf <- cbind(prev_status = rownames(gf), gf)
    write_csv(gf, sprintf("output/tables_r/%d_gross_flows.csv", year))

    ad <- attrition_diagnostics(df)
    write_csv(ad, sprintf("output/tables_r/%d_attrition_diagnostics.csv", year))

    cat(sprintf("[%d] transition matrix / gross flows / attrition diagnostics written\n", year))
  }
}

if (sys.nframe() == 0) {
  setwd(here::here())
  source("R/transitions.R"); source("R/attrition.R")
  main_transitions_attrition()
}
