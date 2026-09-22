# Tidy export of the RQ4 heterogeneity results
# (subgroup lag APE + SE, and the formal Wald test of the lag x
# characteristic interaction) that back the §5 heterogeneity figure.
# Reuses the already-fitted rq4_<year>_<group>_vulnerable_it.json files
# (run_rq4_heterogeneity.R) -- no refitting.

library(dplyr)
library(readr)
library(jsonlite)

main_subgroup_export <- function() {
  files <- list.files("output/tables_r", pattern = "^rq4_.*\\.json$", full.names = TRUE)

  subgroup_rows <- list()
  wald_rows <- list()
  for (f in files) {
    x <- fromJSON(f, simplifyVector = FALSE)
    for (lvl in names(x$subgroup_ape)) {
      a <- x$subgroup_ape[[lvl]]
      subgroup_rows[[length(subgroup_rows) + 1]] <- data.frame(
        year = x$year, dv = x$dv, dimension = x$group, level = lvl,
        ape_lag = as.numeric(a$ape), se = as.numeric(a$se),
        ci_lo = as.numeric(a$ci_lo), ci_hi = as.numeric(a$ci_hi)
      )
    }
    wald_rows[[length(wald_rows) + 1]] <- data.frame(
      year = x$year, dv = x$dv, dimension = x$group,
      wald_chi2 = as.numeric(x$wald_H0_no_heterogeneity$stat),
      wald_df = x$wald_H0_no_heterogeneity$df,
      wald_p = as.numeric(x$wald_H0_no_heterogeneity$p)
    )
  }

  subgroup_df <- bind_rows(subgroup_rows) %>% arrange(dimension, year, level)
  wald_df <- bind_rows(wald_rows) %>% arrange(dimension, year) %>% distinct()

  write_csv(subgroup_df, "output/tables_r/subgroup_ape_by_dimension.csv")
  write_json(subgroup_df, "output/tables_r/subgroup_ape_by_dimension.json", auto_unbox = TRUE, digits = NA)
  write_csv(wald_df, "output/tables_r/subgroup_interaction_wald.csv")
  write_json(wald_df, "output/tables_r/subgroup_interaction_wald.json", auto_unbox = TRUE, digits = NA)

  cat("wrote output/tables_r/subgroup_ape_by_dimension.{csv,json} and subgroup_interaction_wald.{csv,json}\n")
  print(wald_df)
  invisible(list(subgroup = subgroup_df, wald = wald_df))
}

if (sys.nframe() == 0) {
  setwd(here::here())
  main_subgroup_export()
}
