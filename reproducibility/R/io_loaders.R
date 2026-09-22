# Raw-file loaders for the three independent AHIES within-year panels
# (2022, 2023, 2024). R port of scripts/io_loaders.py -- see that file's
# docstring for the "why" (three independent panels, byte-identical
# 838-column schema across years, column list driven by
# config/variable_map.csv so the loader and codebook can never drift apart).
#
# 2022 and 2023 ship as a single wide CSV inside a zip; 2024 ships as a
# Stata .dta. Only the columns needed for the analysis are pulled.

library(readr)
library(dplyr)
library(stringr)
library(haven)

# All paths below are relative to the project root; callers must run with
# that as the working directory (every Rmd in Rmd/ sets this via `here::i_am()`).

RAW_DIR <- file.path("data", "raw")
VARIABLE_MAP_PATH <- file.path("config", "variable_map.csv")

ALWAYS_PULL <- c("hhid", "HholdID", "cluster", "personid", "new_pid", "quarter", "qtr")

RAW_FILES <- list(
  `2022` = file.path(RAW_DIR, "2022", "2022 AHIES Q1-Q4_Rev_20250827_csv.zip"),
  `2023` = file.path(RAW_DIR, "2023", "2023 AHIES Q1-Q4_Rev_20250827_csv.zip"),
  `2024` = file.path(RAW_DIR, "2024", "2024 AHIES Q1-Q4_20250827.dta")
)

variable_map <- function() {
  read_csv(VARIABLE_MAP_PATH, show_col_types = FALSE)
}

confirmed_raw_columns <- function() {
  vm <- variable_map()
  confirmed <- vm %>%
    filter(status == "CONFIRMED", !is.na(ahies_code), ahies_code != "",
           !str_starts(ahies_code, stringr::fixed("("))) %>%
    pull(ahies_code)
  unique(c(ALWAYS_PULL, confirmed))
}

variable_rename_map <- function() {
  vm <- variable_map()
  vm %>%
    filter(status == "CONFIRMED", !is.na(ahies_code), ahies_code != "",
           !str_starts(ahies_code, stringr::fixed("(")), !is.na(variable), variable != "") %>%
    select(ahies_code, variable)
}

rename_to_variable_map <- function(df) {
  m <- variable_rename_map()
  m <- m[m$ahies_code %in% names(df), ]
  idx <- match(m$ahies_code, names(df))
  names(df)[idx] <- m$variable
  df
}

.csv_inner_name <- function(zpath) {
  listing <- utils::unzip(zpath, list = TRUE)
  names <- listing$Name[grepl("\\.csv$", tolower(listing$Name))]
  if (length(names) != 1) stop(sprintf("expected exactly one CSV in %s, found %s", zpath, paste(names, collapse = ", ")))
  names
}

.load_csv_year <- function(year, columns) {
  zpath <- RAW_FILES[[as.character(year)]]
  if (!file.exists(zpath)) stop(sprintf("raw file not found: %s", zpath))
  inner <- .csv_inner_name(zpath)
  con <- unz(zpath, inner)
  # read header first to know which of `columns` actually exist, mirroring
  # pandas' usecols=lambda c: c in columns (silently ignores names not present)
  header <- names(read_csv(con, n_max = 0, show_col_types = FALSE, locale = locale(encoding = "latin1")))
  keep <- intersect(columns, header)
  con2 <- unz(zpath, inner)
  df <- read_csv(con2, col_select = all_of(keep), show_col_types = FALSE,
                 locale = locale(encoding = "latin1"), guess_max = 200000)
  df
}

.load_dta_year <- function(year, columns) {
  dpath <- RAW_FILES[[as.character(year)]]
  if (!file.exists(dpath)) stop(sprintf("raw file not found: %s", dpath))
  df <- haven::read_dta(dpath)
  keep <- intersect(columns, names(df))
  df <- df[, keep, drop = FALSE]
  # haven returns labelled doubles for value-labelled columns; convert to the
  # same character-category representation the CSV years use (pandas'
  # read_stata(convert_categoricals=True) equivalent) so the three years are
  # directly comparable/concatenable.
  df <- df %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
    mutate(across(where(is.factor), as.character))
  df
}

load_year <- function(year, columns = NULL) {
  if (is.null(columns)) columns <- confirmed_raw_columns()
  if (year %in% c(2022, 2023)) {
    df <- .load_csv_year(year, columns)
  } else if (year == 2024) {
    df <- .load_dta_year(year, columns)
  } else {
    stop(sprintf("unknown AHIES year %s; expected 2022, 2023 or 2024", year))
  }
  if ("hhid" %in% names(df)) df$hhid <- as.character(df$hhid)
  df <- df %>% mutate(year = year, .before = 1)
  df
}
