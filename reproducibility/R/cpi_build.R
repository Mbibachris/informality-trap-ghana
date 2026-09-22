# Build data/raw/cpi/ghana_cpi_quarterly.csv from the raw GSS CPI release.
# R port of scripts/cpi_build.py -- see that file for full construction
# rationale (national headline CPI, GSS's own published YoY series averaged
# to quarters as the primary inflation_qt; the raw file stacks three
# Indicator series under one value column and MUST be filtered on
# `Indicator`, not just Region/Product, or they get silently averaged
# together).

library(readr)
library(dplyr)
library(stringr)

RAW_CPI_PATH <- file.path("data", "raw", "CPI_20260915-124542.csv")
CPI_OUT_PATH <- file.path("data", "raw", "cpi", "ghana_cpi_quarterly.csv")

build_cpi_quarterly <- function() {
  raw <- read_csv(RAW_CPI_PATH, skip = 1, show_col_types = FALSE)
  raw <- raw %>% filter(Region == "Ghana", Product == "All products")
  raw <- raw %>% mutate(
    year = as.integer(str_sub(Month, 1, 4)),
    month = as.integer(str_sub(Month, 6, 7)),
    t_within_year = ((month - 1) %/% 3) + 1,
    value = suppressWarnings(as.numeric(`All sources`))
  )

  index_lvl <- raw %>% filter(Indicator == "Consumer Price Index")
  yoy <- raw %>% filter(Indicator == "Year-on-year inflation (%)")
  mom <- raw %>% filter(Indicator == "Month-on-month inflation (%)")

  q_index <- index_lvl %>% group_by(year, t_within_year) %>% summarise(cpi_index_qt = mean(value), .groups = "drop")
  q_yoy <- yoy %>% group_by(year, t_within_year) %>% summarise(inflation_yoy_qt = mean(value), .groups = "drop")
  q_mom <- mom %>% group_by(year, t_within_year) %>% summarise(mom_inflation_avg_qt = mean(value), .groups = "drop")

  q <- q_index %>% left_join(q_yoy, by = c("year", "t_within_year")) %>%
    left_join(q_mom, by = c("year", "t_within_year")) %>%
    arrange(year, t_within_year) %>%
    mutate(qseq = year * 4 + t_within_year)

  q <- q %>% arrange(qseq) %>%
    mutate(
      inflation_qoq_qt = (cpi_index_qt / dplyr::lag(cpi_index_qt, 1) - 1) * 100,
      inflation_accel_qt = inflation_qoq_qt - dplyr::lag(inflation_qoq_qt, 1),
      inflation_yoy_from_index_qt = (cpi_index_qt / dplyr::lag(cpi_index_qt, 4) - 1) * 100,
      inflation_qt = inflation_yoy_qt
    )

  out <- q %>% filter(year >= 2022, year <= 2024) %>%
    select(year, t_within_year, cpi_index_qt, inflation_yoy_qt, inflation_yoy_from_index_qt,
           inflation_qoq_qt, inflation_accel_qt, inflation_qt)
  out
}

if (sys.nframe() == 0) {
  out <- build_cpi_quarterly()
  dir.create(dirname(CPI_OUT_PATH), recursive = TRUE, showWarnings = FALSE)
  write_csv(out, CPI_OUT_PATH)
  print(out)
  cat("wrote", CPI_OUT_PATH, "\n")
}
