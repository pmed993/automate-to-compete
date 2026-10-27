#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# 6. Generate panel diagnostics
# ------------------------------------------------------------------------------
#
# Purpose:
#   Produce a small, clear set of diagnostics for the final research panel.
#
# The eligible and matched samples are kept conceptually separate:
#
#   1. panel_scope
#      Final eligible Compustat sample after year / exchange / industry filters.
#
#   2. panel_analysis
#      Matched causal panel used downstream. Every firm-year has an observed
#      contemporaneous ai_score.
#
#   3. panel_score
#      Compatibility alias of panel_analysis for score / intensity diagnostics.
#
# This script intentionally avoids legacy CIK-year matching diagnostics and
# never mixes missing-score observations into score-distribution denominators.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(dplyr)
library(ggplot2)
library(scales)


# ---- Paths --------------------------------------------------------------------

MERGED_PANEL_RDS <- file.path(
  INPUT_DIR,
  "compustat_ai_panel.rds"
)

PANEL_DIAGNOSTICS_OUTPUT_DIR <- file.path(
  OUTPUT_DIR,
  "diagnostics"
)

PANEL_DIAGNOSTICS_FIGURES_DIR <- file.path(
  OUTPUT_DIR,
  "figures"
)

PANEL_DIAGNOSTICS_BUNDLE_RDS <- file.path(
  PANEL_DIAGNOSTICS_OUTPUT_DIR,
  "panel_diagnostics_bundle.rds"
)

SAVE_PANEL_DIAGNOSTICS <- TRUE
SAVE_PANEL_DIAGNOSTICS_FIGURES <- TRUE


# ---- Helpers ------------------------------------------------------------------

round_numeric_cols <- function(data, digits = 3) {
  data |>
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
}


# ---- Load data ----------------------------------------------------------------

if (!file.exists(MERGED_PANEL_RDS)) {
  stop(
    "Merged panel not found: ",
    MERGED_PANEL_RDS,
    ". Run the panel-building scripts first."
  )
}

if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop(
    "Final analysis panel not found: ",
    ANALYSIS_PANEL_RDS,
    ". Run 4. build_or_load_panel_data.R first."
  )
}

panel_all <- readRDS(
  MERGED_PANEL_RDS
)

panel_analysis <- readRDS(
  ANALYSIS_PANEL_RDS
)


# ---- Standardise required variables -------------------------------------------

required_scope_cols <- c(
  "cik",
  "year",
  "datadate",
  "ai_score",
  "has_ai_history",
  "ai_adopted",
  "ai_adoption_year",
  "ai_report_adoption_year"
)

missing_scope_cols <- setdiff(
  required_scope_cols,
  names(panel_all)
)

if (length(missing_scope_cols) > 0L) {
  stop(
    "Merged panel is missing required columns: ",
    paste(missing_scope_cols, collapse = ", ")
  )
}

required_analysis_cols <- c(
  "cik",
  "year",
  "datadate",
  "ai_score",
  "has_ai_history",
  
  # Main treatment
  "first_ai_filing_date",
  "ai_adopted",
  "ai_adoption_year",
  "ai_report_adoption_year",
  
  # Strong treatment
  "first_ai3_filing_date",
  "ai_adopted3",
  "ai_adoption_3_year",
  "ai_report_adoption_3_year"
)

missing_analysis_cols <- setdiff(
  required_analysis_cols,
  names(panel_analysis)
)

if (length(missing_analysis_cols) > 0L) {
  stop(
    "Final analysis panel is missing required columns: ",
    paste(missing_analysis_cols, collapse = ", ")
  )
}


panel_all <- panel_all |>
  mutate(
    cik = as.character(cik),
    year = as.integer(year),
    datadate = as.Date(datadate),
    
    ai_score = as.integer(ai_score),
    
    # Main treatment
    first_ai_filing_date = as.Date(first_ai_filing_date),
    ai_adopted = as.integer(ai_adopted),
    ai_adoption_year = as.integer(ai_adoption_year),
    ai_report_adoption_year = as.integer(ai_report_adoption_year),
    
    # Strong treatment
    first_ai3_filing_date = as.Date(first_ai3_filing_date),
    ai_adopted3 = as.integer(ai_adopted3),
    ai_adoption_3_year = as.integer(ai_adoption_3_year),
    ai_report_adoption_3_year = as.integer(ai_report_adoption_3_year),
    
    has_ai_history = as.logical(has_ai_history)
  )


panel_analysis <- panel_analysis |>
  mutate(
    cik = as.character(cik),
    year = as.integer(year),
    datadate = as.Date(datadate),
    
    ai_score = as.integer(ai_score),
    
    # Main treatment
    first_ai_filing_date = as.Date(first_ai_filing_date),
    ai_adopted = as.integer(ai_adopted),
    ai_adoption_year = as.integer(ai_adoption_year),
    ai_report_adoption_year = as.integer(ai_report_adoption_year),
    
    # Strong treatment
    first_ai3_filing_date = as.Date(first_ai3_filing_date),
    ai_adopted3 = as.integer(ai_adopted3),
    ai_adoption_3_year = as.integer(ai_adoption_3_year),
    ai_report_adoption_3_year = as.integer(ai_report_adoption_3_year),
    
    has_ai_history = as.logical(has_ai_history)
  ) |>
  arrange(
    cik,
    datadate
  )

# ---- Final eligible Compustat scope -------------------------------------------

panel_scope <- build_final_analysis_panel(
  panel_all
) |>
  arrange(
    cik,
    datadate
  )


# ---- Observed-score subset -----------------------------------------------------

panel_score <- panel_analysis |>
  filter(
    !is.na(ai_score)
  )


# ---- Core QA ------------------------------------------------------------------

# Unique annual observation per firm / fiscal period.
duplicate_scope <- panel_scope |>
  count(
    cik,
    datadate,
    name = "n"
  ) |>
  filter(
    n > 1L
  )

duplicate_analysis <- panel_analysis |>
  count(
    cik,
    datadate,
    name = "n"
  ) |>
  filter(
    n > 1L
  )

if (nrow(duplicate_scope) > 0L) {
  stop(
    "Eligible panel contains duplicate CIK-datadate observations."
  )
}

if (nrow(duplicate_analysis) > 0L) {
  stop(
    "Final causal panel contains duplicate CIK-datadate observations."
  )
}


# Causal panel should only contain firms with known AI history and treatment.
if (anyNA(panel_analysis$has_ai_history) || 
    any(panel_analysis$has_ai_history != TRUE)) {
  stop(
    "Final causal panel contains missing or FALSE has_ai_history values."
  )
}

if (anyNA(panel_analysis$ai_adopted) || anyNA(panel_analysis$ai_score)) {
  stop(
    "Final causal panel contains missing treatment or annual AI-score values."
  )
}


# Observed AI scores must be 1, 2 or 3.
if (nrow(panel_score) > 0L && any(!panel_score$ai_score %in% 1:3)) {
  stop(
    "Observed AI scores outside the allowed values 1, 2, 3."
  )
}


if (any(!panel_analysis$ai_adopted %in% c(0L, 1L))) {
  stop(
    "ai_adopted contains values other than 0 and 1."
  )
}

if (any(!panel_analysis$ai_adopted3 %in% c(0L, 1L))) {
  stop(
    "ai_adopted3 contains values other than 0 and 1."
  )
}

# Main treatment must be absorbing: no 1 -> 0 reversals.
treatment_reversals <- panel_analysis |>
  arrange(
    cik,
    datadate
  ) |>
  group_by(
    cik
  ) |>
  summarise(
    reversal = any(
      diff(ai_adopted) < 0L,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  filter(
    reversal
  )

if (nrow(treatment_reversals) > 0L) {
  stop(
    "Found ",
    nrow(treatment_reversals),
    " firms with treatment reversals (1 -> 0)."
  )
}

if (anyNA(panel_analysis$ai_adopted3)) {
  stop(
    "Final causal panel contains missing ai_adopted3 values."
  )
}


strong_treatment_reversals <- panel_analysis |>
  arrange(
    cik,
    datadate
  ) |>
  group_by(
    cik
  ) |>
  summarise(
    reversal = any(
      diff(ai_adopted3) < 0L,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  filter(
    reversal
  )

if (nrow(strong_treatment_reversals) > 0L) {
  stop(
    "Found ",
    nrow(strong_treatment_reversals),
    " firms with strong-treatment reversals (1 -> 0)."
  )
}


strong_without_main <- panel_analysis |>
  filter(
    ai_adopted3 == 1L,
    ai_adopted != 1L
  )

if (nrow(strong_without_main) > 0L) {
  stop(
    "Found ",
    nrow(strong_without_main),
    " observations where strong treatment = 1 but main treatment != 1."
  )
}


invalid_strong_timing <- panel_analysis |>
  filter(
    !is.na(first_ai_filing_date),
    !is.na(first_ai3_filing_date),
    first_ai3_filing_date < first_ai_filing_date
  ) |>
  distinct(
    cik,
    first_ai_filing_date,
    first_ai3_filing_date
  )

if (nrow(invalid_strong_timing) > 0L) {
  stop(
    "Found firms whose first score-3 filing predates their first score>=2 filing."
  )
}

# The cohort is constructed before the matched-outcome filter. Its first
# treated financial year may therefore have no matched filing. Check treatment
# status against the stored cohort for every retained outcome year instead.

adoption_year_qa <- panel_analysis |>
  filter(
    ai_adopted != as.integer(
      ai_adoption_year > 0L & year >= ai_adoption_year
    )
  )

if (nrow(adoption_year_qa) > 0L) {
  stop(
    "Found ",
    nrow(adoption_year_qa),
    " matched firm-years where main treatment disagrees with its cohort."
  )
}


# Apply the same check to strong adoption.

strong_adoption_year_qa <- panel_analysis |>
  filter(
    ai_adopted3 != as.integer(
      ai_adoption_3_year > 0L & year >= ai_adoption_3_year
    )
  )

if (nrow(strong_adoption_year_qa) > 0L) {
  stop(
    "Found ",
    nrow(strong_adoption_year_qa),
    " matched firm-years where strong treatment disagrees with its cohort."
  )
}

# ---- Panel overview ------------------------------------------------------------

n_scope_periods <- nrow(
  panel_scope
)

n_scope_firms <- n_distinct(
  panel_scope$cik
)

n_history_firms <- panel_scope |>
  distinct(
    cik,
    has_ai_history
  ) |>
  summarise(
    n = sum(
      has_ai_history == TRUE,
      na.rm = TRUE
    )
  ) |>
  pull(
    n
  )

n_causal_periods <- nrow(
  panel_analysis
)

n_causal_firms <- n_distinct(
  panel_analysis$cik
)

n_scored_periods <- nrow(
  panel_score
)

n_score_missing_causal <- sum(
  is.na(panel_analysis$ai_score)
)

panel_overview <- tibble::tibble(
  metric = c(
    "Eligible Compustat fiscal periods",
    "Eligible Compustat firms",
    "Firms with observed AI history",
    "Causal analysis fiscal periods",
    "Causal analysis firms",
    "Periods with contemporaneous AI score",
    "Causal periods without contemporaneous AI score",
    "AI-score coverage in eligible sample",
    "Causal-panel retention rate",
    "Years covered",
    "Treatment definition",
    "Report-period treated share (known cohort)"
  ),
  value = c(
    scales::comma(n_scope_periods),
    scales::comma(n_scope_firms),
    scales::comma(n_history_firms),
    scales::comma(n_causal_periods),
    scales::comma(n_causal_firms),
    scales::comma(n_scored_periods),
    scales::comma(n_score_missing_causal),
    scales::percent(
      mean(!is.na(panel_scope$ai_score)),
      accuracy = 0.1
    ),
    scales::percent(
      n_causal_periods / n_scope_periods,
      accuracy = 0.1
    ),
    paste0(
      min(panel_analysis$year, na.rm = TRUE),
      "-",
      max(panel_analysis$year, na.rm = TRUE)
    ),
    "First post-filing fiscal period after ai_score >= 2; absorbing thereafter",
    scales::percent(
      mean(panel_analysis$ai_adopted == 1L),
      accuracy = 0.1
    )
  )
)


# ---- Coverage and treatment by year -------------------------------------------

scope_by_year <- panel_scope |>
  group_by(
    year
  ) |>
  summarise(
    eligible_periods = n(),
    periods_with_score = sum(
      !is.na(ai_score)
    ),
    periods_without_score = sum(
      is.na(ai_score)
    ),
    score_coverage = mean(
      !is.na(ai_score)
    ),
    .groups = "drop"
  )


causal_by_year <- panel_analysis |>
  group_by(
    year
  ) |>
  summarise(
    causal_observations = n(),
    causal_firms = n_distinct(cik),
    treated_observations = sum(
      ai_adopted == 1L
    ),
    untreated_observations = sum(
      ai_adopted == 0L
    ),
    treated_share = mean(
      ai_adopted == 1L
    ),
    causal_rows_with_score = sum(
      !is.na(ai_score)
    ),
    causal_rows_without_score = sum(
      is.na(ai_score)
    ),
    .groups = "drop"
  )


score_by_year <- panel_score |>
  group_by(
    year
  ) |>
  summarise(
    scored_observations = n(),
    mean_ai_score = mean(
      ai_score
    ),
    share_score_1 = mean(
      ai_score == 1L
    ),
    share_score_2 = mean(
      ai_score == 2L
    ),
    share_score_3 = mean(
      ai_score == 3L
    ),
    .groups = "drop"
  )


panel_by_year <- scope_by_year |>
  full_join(
    causal_by_year,
    by = "year"
  ) |>
  left_join(
    score_by_year,
    by = "year"
  ) |>
  arrange(
    year
  ) |>
  round_numeric_cols()


# ---- Treatment summary for the Compustat-EDGAR matched sample -----------------
# Use the same contemporaneously matched firm roster as the pre-treatment
# balance table in `7. summary_stats.R`. This prevents firms represented only by
# causal-panel rows with a missing annual AI score from entering this summary.

firm_treatment_summary <- panel_score |>
  group_by(
    cik
  ) |>
  summarise(
    first_year = min(
      year,
      na.rm = TRUE
    ),
    last_year = max(
      year,
      na.rm = TRUE
    ),
    ai_adoption_year = first(
      ai_adoption_year
    ),
    .groups = "drop"
  ) |>
  mutate(
    # Post-filing cohort agrees with the DID and pre-treatment balance table.
    ever_treated = ai_adoption_year > 0L,
    first_treated_year = if_else(
      ever_treated,
      ai_adoption_year,
      NA_integer_
    )
  )

if (
  nrow(firm_treatment_summary) != n_distinct(panel_score$cik) ||
  any(
    firm_treatment_summary$ever_treated !=
      (firm_treatment_summary$ai_adoption_year > 0L),
    na.rm = TRUE
  )
) {
  stop("Matched-sample firm treatment summary failed its roster or status QA.")
}


treatment_status_summary <- firm_treatment_summary |>
  mutate(
    treatment_status = case_when(
      is.na(ever_treated) ~ "Treatment cohort unknown",
      ever_treated ~ "Ever treated",
      TRUE ~ "Never treated"
    )
  ) |>
  count(
    treatment_status,
    name = "n_firms"
  ) |>
  mutate(
    share_firms = n_firms / sum(n_firms)
  ) |>
  round_numeric_cols()


treatment_cohorts <- firm_treatment_summary |>
  filter(
    ever_treated,
    !is.na(first_treated_year)
  ) |>
  count(
    first_treated_year,
    name = "n_firms"
  ) |>
  arrange(
    first_treated_year
  )


# ---- Score distributions ------------------------------------------------------

score_distribution_overall <- panel_score |> 
  count(
    ai_score,
    name = "n_firm_years"
  ) |>
  mutate(
    share_firm_years = n_firm_years / sum(n_firm_years),
    score_label = case_when(
      ai_score == 1L ~ "1: No disclosed current implementation",
      ai_score == 2L ~ "2: Emerging / bounded implementation",
      ai_score == 3L ~ "3: Established / integrated implementation"
    )
  ) |>
  arrange(
    ai_score
  ) |>
  round_numeric_cols()


score_distribution_by_year <- panel_score |>
  count(
    year,
    ai_score,
    name = "n_firm_years"
  ) |>
  group_by(
    year
  ) |>
  mutate(
    share_firm_years = n_firm_years / sum(n_firm_years),
    score_label = factor(
      ai_score,
      levels = c(1, 2, 3),
      labels = c(
        "1: No disclosed current implementation",
        "2: Emerging / bounded implementation",
        "3: Established / integrated implementation"
      )
    )
  ) |>
  ungroup()


# ---- NAICS2 summary ------------------------------------------------------------

naics2_summary <- panel_analysis |>
  filter(
    !is.na(naics2),
    naics2 != ""
  ) |>
  group_by(
    naics2
  ) |>
  summarise(
    n_firm_years = n(),
    n_firms = n_distinct(cik),
    treated_share = mean(
      ai_adopted == 1L
    ),
    score_coverage = mean(
      !is.na(ai_score)
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(n_firm_years)
  ) |>
  round_numeric_cols()


# ---- Plots --------------------------------------------------------------------

p_treated_share_by_year <- ggplot(
  causal_by_year,
  aes(
    x = year,
    y = treated_share
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      min(causal_by_year$year),
      max(causal_by_year$year),
      by = 1
    ),
    labels = function(x) sprintf("%.0f", x)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Share treated"
  ) +
  theme_minimal(
    base_size = 12
  )


p_mean_ai_score_by_year <- ggplot(
  score_by_year,
  aes(
    x = year,
    y = mean_ai_score
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      min(score_by_year$year),
      max(score_by_year$year),
      by = 1
    ),
    labels = function(x) sprintf("%.0f", x)
  ) +
  scale_y_continuous(
    breaks = c(1, 1.5, 2, 2.5, 3)
  ) +
  coord_cartesian(
    ylim = c(1, 3)
  ) +
  labs(
    x = "Year",
    y = "Mean AI score"
  ) +
  theme_minimal(
    base_size = 12
  )


p_ai_score_mix_by_year <- ggplot(
  score_distribution_by_year,
  aes(
    x = year,
    y = share_firm_years,
    fill = score_label
  )
) +
  geom_col(
    width = 0.78,
    colour = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = c(
      "1: No disclosed current implementation" = "#CBD5DF",
      "2: Emerging / bounded implementation" = "#5B9ABD",
      "3: Established / integrated implementation" = "#1F4E79"
    ),
    labels = c(
      "1  No disclosed current implementation",
      "2  Emerging / bounded implementation",
      "3  Established / integrated implementation"
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      min(score_distribution_by_year$year),
      max(score_distribution_by_year$year),
      by = 1
    ),
    labels = function(x) sprintf("%.0f", x)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Share of scored firm-years",
    fill = NULL
  ) +
  theme_minimal(
    base_size = 12
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )


# ---- Save figures -------------------------------------------------------------

if (SAVE_PANEL_DIAGNOSTICS_FIGURES) {
  
  dir.create(
    PANEL_DIAGNOSTICS_FIGURES_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  ggsave(
    file.path(
      PANEL_DIAGNOSTICS_FIGURES_DIR,
      "p_treated_share_by_year.png"
    ),
    p_treated_share_by_year,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      PANEL_DIAGNOSTICS_FIGURES_DIR,
      "p_mean_ai_score_by_year.png"
    ),
    p_mean_ai_score_by_year,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    file.path(
      PANEL_DIAGNOSTICS_FIGURES_DIR,
      "p_ai_score_mix_by_year.png"
    ),
    p_ai_score_mix_by_year,
    width = 9,
    height = 5.5,
    dpi = 300
  )
}


# ---- Save diagnostics bundle --------------------------------------------------

panel_diagnostics <- list(
  panel_overview = panel_overview,
  panel_by_year = panel_by_year,
  treatment_status_summary = treatment_status_summary,
  treatment_cohorts = treatment_cohorts,
  score_distribution_overall = score_distribution_overall,
  score_distribution_by_year = score_distribution_by_year,
  naics2_summary = naics2_summary,
  treatment_reversals = treatment_reversals,
  p_treated_share_by_year = p_treated_share_by_year,
  p_mean_ai_score_by_year = p_mean_ai_score_by_year,
  p_ai_score_mix_by_year = p_ai_score_mix_by_year
)

if (SAVE_PANEL_DIAGNOSTICS) {
  
  dir.create(
    PANEL_DIAGNOSTICS_OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  saveRDS(
    panel_diagnostics,
    PANEL_DIAGNOSTICS_BUNDLE_RDS
  )
}


# ---- Console output -----------------------------------------------------------

cat(
  "\nPanel diagnostics completed.\n\n"
)

print(
  panel_overview
)

cat(
  "\nPanel by year:\n"
)

print(
  panel_by_year
)

cat(
  "\nTreatment status:\n"
)

print(
  treatment_status_summary
)

cat(
  "\nTreatment cohorts:\n"
)

print(
  treatment_cohorts
)

cat(
  "\nTreatment reversals:",
  nrow(treatment_reversals),
  "\n"
)

if (SAVE_PANEL_DIAGNOSTICS) {
  cat(
    "\nSaved diagnostics bundle to:",
    PANEL_DIAGNOSTICS_BUNDLE_RDS,
    "\n"
  )
}
