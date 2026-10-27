# ---- Packages ----
packages <- c("DBI", "data.table", "dplyr","edgar", "janitor", "RPostgres", "readxl",
              "tidyverse", "usethis", "stringr")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages, require, character.only = TRUE))

# ---- Config ----
INPUT_DIR <- file.path("data/")
OUTPUT_DIR <- file.path("output/")
ANALYSIS_PANEL_RDS <- file.path(INPUT_DIR, "compustat_ai_analysis_panel.rds")

# Reported and estimated samples are restricted to this window. Earlier raw
# Compustat years may be loaded only to construct lagged 2015 covariates.
ANALYSIS_START_YEAR <- 2015L
ANALYSIS_END_YEAR <- 2025L
MIN_DID_TREATED_COHORT <- 2016L
HHI_PRE_TREATMENT_BASE_YEAR <- MIN_DID_TREATED_COHORT - 1L
if (MIN_DID_TREATED_COHORT <= ANALYSIS_START_YEAR ||
    MIN_DID_TREATED_COHORT > ANALYSIS_END_YEAR) {
  stop("The first DID cohort requires an observed pre-treatment year.")
}

#FINAL_ANALYSIS_EXCLUDED_NAICS2 <- c() c("22", "52", "53", "99")
FINAL_ANALYSIS_INCLUDED_EXCHG <- c(11L, 12L, 14L, 17L, 21L)

build_final_analysis_panel <- function(data) {
  data |>
    mutate(
      year = as.integer(year),
      naics2 = as.character(naics2),
      exchg = as.integer(exchg)
    ) |>
    filter(
      !is.na(year),
      year >= ANALYSIS_START_YEAR,
      year <= ANALYSIS_END_YEAR,
      exchg %in% FINAL_ANALYSIS_INCLUDED_EXCHG
    )
}
