#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Lagged market competition as a determinant of first AI adoption
# ------------------------------------------------------------------------------
# Discrete-time first-adoption risk-set models:
#
#   FirstAdopt_it = alpha + beta HHI_m,t-1 + gamma X_i,t-1
#                   + year FE + NAICS3 FE + error_it.
#
# The companion binary specification replaces continuous HHI with an indicator
# for high competition (HHI <= 1,800); low competition is the reference group.
#
# HHI is the continuous sales-based Herfindahl-Hirschman Index calculated for
# every NAICS3 market-year from the full project Compustat panel. Each at-risk
# firm-year t is assigned the HHI of the firm's NAICS3 market in t-1. Therefore,
# a firm first treated in cohort g is evaluated using HHI_m,g-1, never HHI in g
# or later. Non-event comparison observations in year t use the same timing rule.
#
# Higher HHI means greater concentration and less competition, so the competitive
# arm-race prediction is beta < 0. Because HHI varies within NAICS3 over time,
# NAICS3 and year fixed effects can both be included. Standard errors are
# clustered by the lagged NAICS3 market. This is a predictive determinant model,
# not a causal estimate of market competition, and HHI reflects public firms in
# the project Compustat universe rather than the entire product market.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})


# ---- Settings ----------------------------------------------------------------
HHI_DETERMINANT_OUTPUT_DIR <- file.path(
  sub("/+$", "", OUTPUT_DIR),
  "hhi_ai_determinant"
)
HHI_DETERMINANT_BUNDLE_RDS <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_bundle.rds"
)
HHI_MARKET_YEAR_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_naics3_market_year.csv"
)
FIRM_YEAR_LAGGED_HHI_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "firm_year_lagged_hhi.csv"
)
HHI_DETERMINANT_COEFFICIENTS_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_coefficients.csv"
)
HHI_DETERMINANT_ALL_COEFFICIENTS_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_all_coefficients_table.csv"
)
HHI_DETERMINANT_ALL_COEFFICIENTS_MD <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_all_coefficients_table.md"
)
HHI_DETERMINANT_PUBLICATION_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_publication_table.csv"
)
HHI_DETERMINANT_PUBLICATION_MD <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_publication_table.md"
)
HHI_BINARY_ROBUSTNESS_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_binary_robustness_table.csv"
)
HHI_BINARY_ROBUSTNESS_MD <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_binary_robustness_table.md"
)
HHI_BINARY_IDENTIFICATION_AUDIT_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_binary_identification_audit.csv"
)
HHI_DETERMINANT_SAMPLE_AUDIT_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_sample_audit.csv"
)
HHI_DETERMINANT_QA_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "hhi_ai_determinant_qa.csv"
)
HHI_DETERMINANT_COVERAGE_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "lagged_hhi_coverage_audit.csv"
)
HHI_DETERMINANT_MANIFEST_CSV <- file.path(
  HHI_DETERMINANT_OUTPUT_DIR,
  "run_manifest.csv"
)

FULL_PANEL_RDS <- file.path(
  sub("/+$", "", INPUT_DIR),
  "compustat_ai_panel.rds"
)
SAVE_HHI_DETERMINANT_OUTPUTS <- isTRUE(get0(
  "SAVE_HHI_DETERMINANT_OUTPUTS",
  ifnotfound = TRUE
))

MIN_MODEL_ADOPTION_YEAR <- ANALYSIS_START_YEAR + 1L
HHI_FIRST_YEAR <- ANALYSIS_START_YEAR
HHI_LAST_YEAR <- ANALYSIS_END_YEAR - 1L
HHI_SCALE <- 1000
HHI_COMPETITION_CUTOFF <- 1800
WINSOR_PROBS <- c(0.01, 0.99)
MIN_CLUSTER_COUNT <- 30L

TREATMENT_DEFINITIONS <- tribble(
  ~treatment_id, ~treatment_label, ~cohort_var,
  "ai_adoption_ge_2", "AI adoption >= 2", "ai_adoption_year",
  "strong_ai_adoption_3", "Strong AI adoption = 3", "ai_adoption_3_year"
)

MODEL_SPECIFICATIONS <- tribble(
  ~model_spec, ~model_spec_label, ~focal_term, ~expected_arm_race_sign,
  "continuous_hhi", "Continuous HHI", "hhi_l1_per_1000", "negative",
  "binary_competition", "Binary competition", "high_competition_l1", "positive"
)

MODEL_GRID <- tidyr::crossing(
  model_spec = MODEL_SPECIFICATIONS$model_spec,
  treatment_id = TREATMENT_DEFINITIONS$treatment_id
) |>
  left_join(MODEL_SPECIFICATIONS, by = "model_spec") |>
  left_join(TREATMENT_DEFINITIONS, by = "treatment_id") |>
  mutate(
    model_spec = factor(model_spec, levels = MODEL_SPECIFICATIONS$model_spec),
    treatment_id = factor(treatment_id, levels = TREATMENT_DEFINITIONS$treatment_id)
  ) |>
  arrange(model_spec, treatment_id) |>
  mutate(
    model_spec = as.character(model_spec),
    treatment_id = as.character(treatment_id),
    model_key = paste(model_spec, treatment_id, sep = "__")
  )

FIRM_CONTROL_LABELS <- c(
  log_at_l1 = "Size (log assets, t-1)",
  roa_l1_w = "ROA (t-1, winsorized)",
  cash_ratio_l1 = "Cash ratio (t-1)",
  leverage_l1_w = "Leverage (t-1, winsorized)",
  capx_intensity_l1 = "CAPX intensity (t-1)",
  rd_intensity_l1_filled = "R&D intensity (t-1)",
  rd_reporter_l1 = "R&D reporter (t-1)",
  log_labor_productivity_l1 = "Labour productivity (log, t-1)",
  firm_age_l1 = "Firm age (years, t-1)"
)
FIRM_CONTROLS <- names(FIRM_CONTROL_LABELS)
TERM_LABELS <- c(
  hhi_l1_per_1000 = "NAICS3 sales HHI (t-1, per 1,000 points)",
  high_competition_l1 = "High competition (HHI t-1 <= 1,800)",
  FIRM_CONTROL_LABELS
)


# ---- Helpers -----------------------------------------------------------------
normalize_cik <- function(x) {
  numeric_cik <- suppressWarnings(as.numeric(as.character(x)))
  if_else(
    is.finite(numeric_cik),
    as.character(as.integer(numeric_cik)),
    NA_character_
  )
}

assert_unique_keys <- function(data, keys, label) {
  duplicates <- data |>
    count(across(all_of(keys)), name = "n") |>
    filter(n > 1L)
  if (nrow(duplicates) > 0L) {
    stop(label, " has duplicate rows on key: ", paste(keys, collapse = ", "))
  }
  invisible(TRUE)
}

winsorize_finite <- function(x, probs = WINSOR_PROBS) {
  valid <- is.finite(x)
  if (sum(valid) < 2L) {
    stop("Fewer than two finite observations are available for winsorisation.")
  }
  bounds <- as.numeric(quantile(
    x[valid],
    probs = probs,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  ))
  values <- rep(NA_real_, length(x))
  values[valid] <- pmin(pmax(x[valid], bounds[1]), bounds[2])
  list(values = values, bounds = bounds)
}

significance_stars <- function(p_value) {
  case_when(
    is.na(p_value) ~ "",
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_estimate <- function(value) {
  sub("^-0\\.0000$", "0.0000", sprintf("%.4f", value))
}

format_count <- function(value) {
  format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
}

markdown_row <- function(values) {
  values <- gsub("|", "\\|", as.character(values), fixed = TRUE)
  paste0("| ", paste(values, collapse = " | "), " |")
}

tidy_fixest_model <- function(model, model_row, source_data) {
  coefficient_matrix <- as.data.frame(coeftable(model))
  coefficient_matrix$term <- rownames(coefficient_matrix)
  rownames(coefficient_matrix) <- NULL
  names(coefficient_matrix)[seq_len(4L)] <- c(
    "estimate", "std_error", "statistic", "p_value"
  )

  model_data <- source_data[fixest::obs(model), , drop = FALSE]
  if (sum(coefficient_matrix$term == model_row$focal_term) != 1L) {
    stop(model_row$model_key, " did not estimate exactly one focal coefficient.")
  }

  as_tibble(coefficient_matrix) |>
    transmute(
      model_key = model_row$model_key,
      model_spec = model_row$model_spec,
      model_spec_label = model_row$model_spec_label,
      treatment_id = model_row$treatment_id,
      treatment_label = model_row$treatment_label,
      cohort_var = model_row$cohort_var,
      term,
      term_label = if_else(
        term %in% names(TERM_LABELS),
        unname(TERM_LABELS[term]),
        term
      ),
      estimate,
      std_error,
      statistic,
      p_value,
      p_value_arm_race_one_sided = if_else(
        term == model_row$focal_term,
        case_when(
          model_row$expected_arm_race_sign == "negative" & estimate < 0 ~ p_value / 2,
          model_row$expected_arm_race_sign == "negative" ~ 1 - p_value / 2,
          model_row$expected_arm_race_sign == "positive" & estimate > 0 ~ p_value / 2,
          TRUE ~ 1 - p_value / 2
        ),
        NA_real_
      ),
      ci_low = estimate - 1.96 * std_error,
      ci_high = estimate + 1.96 * std_error,
      expected_arm_race_sign = if_else(
        term == model_row$focal_term,
        model_row$expected_arm_race_sign,
        NA_character_
      ),
      n_obs = nobs(model),
      n_firms = n_distinct(model_data$cik),
      n_first_adoptions = sum(model_data$first_adopt),
      n_naics3_clusters = n_distinct(model_data$hhi_naics3_l1)
    )
}

write_publication_markdown <- function(data, path) {
  display <- data |>
    select(row_label, all_of(TREATMENT_DEFINITIONS$treatment_id))
  names(display) <- c(
    "",
    "(1) AI adoption >= 2",
    "(2) Strong AI adoption = 3"
  )

  lines <- c(
    "# Lagged market competition and first AI adoption",
    "",
    markdown_row(names(display)),
    markdown_row(rep("---", ncol(display))),
    apply(display, 1L, markdown_row),
    "",
    "Notes: The dependent variable equals one in a firm's first adoption year. ",
    "Each row at risk in year t uses the sales-based HHI for the firm's NAICS3 ",
    "market in t-1 and firm controls measured in the immediately preceding ",
    "consecutive fiscal year. Higher HHI indicates less competition, so the arm-race ",
    "prediction is a negative continuous-HHI coefficient. Models include year and ",
    "lagged-NAICS3 fixed effects. Standard errors, in parentheses, are clustered ",
    "by lagged NAICS3 market. * p<0.10; ** p<0.05; *** p<0.01."
  )
  writeLines(lines, path)
}

write_binary_robustness_markdown <- function(data, path) {
  display <- data |>
    select(row_label, all_of(TREATMENT_DEFINITIONS$treatment_id))
  names(display) <- c(
    "",
    "(1) AI adoption >= 2",
    "(2) Strong AI adoption = 3"
  )

  lines <- c(
    "# Binary competition robustness check",
    "",
    markdown_row(names(display)),
    markdown_row(rep("---", ncol(display))),
    apply(display, 1L, markdown_row),
    "",
    "Notes: This is a secondary functional-form robustness check, not the primary ",
    "competition result. High competition equals one when lagged NAICS3 HHI is at ",
    "or below 1,800; low competition is the reference group. With NAICS3 fixed ",
    "effects, the coefficient is identified only by markets crossing this cutoff. ",
    "The table reports that identifying support explicitly. Standard errors are ",
    "clustered by lagged NAICS3 market."
  )
  writeLines(lines, path)
}

write_all_coefficients_markdown <- function(data, path) {
  display <- data |>
    select(row_label, all_of(MODEL_GRID$model_key))
  model_headers <- paste0(
    "(", seq_len(nrow(MODEL_GRID)), ") ",
    MODEL_GRID$model_spec_label, ": ", MODEL_GRID$treatment_label
  )
  names(display) <- c("", model_headers)

  lines <- c(
    "# Complete coefficient results: competition and first AI adoption",
    "",
    markdown_row(names(display)),
    markdown_row(rep("---", ncol(display))),
    apply(display, 1L, markdown_row),
    "",
    "Notes: This diagnostic table reports every estimated coefficient. The concise ",
    "publication table reports only the focal continuous-HHI and binary-competition ",
    "coefficients. All models use the same lagged controls, year and lagged-NAICS3 ",
    "fixed effects, and NAICS3-clustered standard errors."
  )
  writeLines(lines, path)
}


# ---- Load and validate data ---------------------------------------------------
required_full_columns <- c("cik", "year", "naics2", "naics3", "sale")
required_analysis_columns <- c(
  "cik", "year", "fyear", "lag_source_fyear", "lag_is_consecutive",
  "ai_adoption_year", "ai_adoption_3_year", "rd_intensity_l1",
  "roa_l1", "leverage_l1",
  setdiff(
    FIRM_CONTROLS,
    c("rd_intensity_l1_filled", "roa_l1_w", "leverage_l1_w")
  )
)

if (!file.exists(FULL_PANEL_RDS)) stop("Missing full panel: ", FULL_PANEL_RDS)
if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop("Missing analysis panel: ", ANALYSIS_PANEL_RDS)
}

full_panel <- readRDS(FULL_PANEL_RDS)
analysis_panel <- readRDS(ANALYSIS_PANEL_RDS) |>
  build_final_analysis_panel()
if (!"ai_score" %in% names(analysis_panel) ||
    anyNA(analysis_panel$ai_score)) {
  stop("HHI determinants source panel must contain only matched AI-score years.")
}

missing_full <- setdiff(required_full_columns, names(full_panel))
missing_analysis <- setdiff(required_analysis_columns, names(analysis_panel))
if (length(missing_full) > 0L) {
  stop("Full panel is missing: ", paste(missing_full, collapse = ", "))
}
if (length(missing_analysis) > 0L) {
  stop("Analysis panel is missing: ", paste(missing_analysis, collapse = ", "))
}

full_panel <- full_panel |>
  mutate(
    cik = normalize_cik(cik),
    year = as.integer(year),
    naics2 = as.character(naics2),
    naics3 = as.character(naics3),
    sale = as.numeric(sale)
  )
analysis_panel <- analysis_panel |>
  mutate(
    cik = normalize_cik(cik),
    year = as.integer(year),
    fyear = as.integer(fyear),
    lag_source_fyear = as.integer(lag_source_fyear)
  )

assert_unique_keys(full_panel, c("cik", "year"), "Full Compustat panel")
assert_unique_keys(analysis_panel, c("cik", "year"), "Analysis panel")


# ---- Annual sales-based NAICS3 HHI -------------------------------------------
# The market denominator includes all valid firm sales in the full project
# Compustat panel, not only firms that later enter a particular regression.
hhi_source <- full_panel |>
  filter(
    year >= HHI_FIRST_YEAR,
    year <= HHI_LAST_YEAR,
    !is.na(cik),
    !is.na(naics3),
    nzchar(naics3),
    is.finite(sale),
    sale > 0
  )

hhi_market_year <- hhi_source |>
  group_by(year, naics3) |>
  mutate(
    market_sales = sum(sale),
    sales_share_pct = 100 * sale / market_sales
  ) |>
  summarise(
    naics2 = first(naics2),
    n_firms = n_distinct(cik),
    market_sales = first(market_sales),
    hhi = sum(sales_share_pct^2),
    .groups = "drop"
  ) |>
  arrange(year, naics3)

assert_unique_keys(hhi_market_year, c("year", "naics3"), "NAICS3-year HHI")
if (any(!is.finite(hhi_market_year$hhi)) ||
    any(hhi_market_year$hhi <= 0 | hhi_market_year$hhi > 10000 + 1e-8)) {
  stop("Calculated HHI falls outside (0, 10,000].")
}

hhi_time_variation <- hhi_market_year |>
  group_by(naics3) |>
  summarise(
    n_years = n_distinct(year),
    hhi_sd = sd(hhi),
    .groups = "drop"
  )
if (!any(is.finite(hhi_time_variation$hhi_sd) & hhi_time_variation$hhi_sd > 0)) {
  stop("HHI has no within-NAICS3 time variation; NAICS3 FE would absorb it.")
}


# ---- Assign each firm-year its own preceding-year market HHI -----------------
# The firm's NAICS3 assignment is also taken from t-1. This prevents current or
# post-adoption industry reclassification from determining its competition value.
firm_year_lagged_hhi <- full_panel |>
  filter(
    year >= HHI_FIRST_YEAR,
    year <= HHI_LAST_YEAR,
    !is.na(cik),
    !is.na(naics3),
    nzchar(naics3)
  ) |>
  transmute(
    cik,
    outcome_year = year + 1L,
    hhi_year = year,
    hhi_naics2_l1 = naics2,
    hhi_naics3_l1 = naics3
  ) |>
  left_join(
    hhi_market_year |>
      transmute(
        hhi_year = year,
        hhi_naics3_l1 = naics3,
        hhi_market_n_firms = n_firms,
        hhi_market_sales = market_sales,
        hhi_l1 = hhi
      ),
    by = c("hhi_year", "hhi_naics3_l1")
  ) |>
  mutate(hhi_l1_per_1000 = hhi_l1 / HHI_SCALE) |>
  arrange(cik, outcome_year)

assert_unique_keys(
  firm_year_lagged_hhi,
  c("cik", "outcome_year"),
  "Firm-year lagged HHI lookup"
)

coverage_audit <- analysis_panel |>
  transmute(cik, outcome_year = year) |>
  left_join(
    firm_year_lagged_hhi |>
      select(cik, outcome_year, hhi_l1),
    by = c("cik", "outcome_year")
  ) |>
  mutate(
    analysis_year = outcome_year,
    eligible_hhi_year = analysis_year >= MIN_MODEL_ADOPTION_YEAR,
    has_lagged_hhi = is.finite(hhi_l1)
  ) |>
  group_by(analysis_year) |>
  summarise(
    analysis_observations = n(),
    eligible_observations = sum(eligible_hhi_year),
    observations_with_lagged_hhi = sum(eligible_hhi_year & has_lagged_hhi),
    eligible_coverage_pct = 100 * observations_with_lagged_hhi /
      eligible_observations,
    .groups = "drop"
  )

determinant_base <- analysis_panel |>
  left_join(
    firm_year_lagged_hhi,
    by = c("cik", "year" = "outcome_year")
  ) |>
  filter(
    year >= MIN_MODEL_ADOPTION_YEAR,
    year <= ANALYSIS_END_YEAR,
    lag_is_consecutive %in% TRUE,
    lag_source_fyear == fyear - 1L,
    hhi_year == year - 1L,
    is.finite(hhi_l1_per_1000),
    !is.na(hhi_naics3_l1)
  ) |>
  mutate(
    rd_intensity_l1_filled = coalesce(rd_intensity_l1, 0),
    rd_reporter_l1 = as.integer(rd_reporter_l1),
    # Match the competition convention used in the HHI heterogeneity analysis:
    # HHI <= 1,800 is high competition; HHI > 1,800 is low competition.
    high_competition_l1 = as.integer(hhi_l1 <= HHI_COMPETITION_CUTOFF),
    competition_regime_l1 = factor(
      if_else(high_competition_l1 == 1L, "High competition", "Low competition"),
      levels = c("Low competition", "High competition")
    ),
    hhi_naics3_l1 = factor(hhi_naics3_l1)
  )

roa_winsor <- winsorize_finite(determinant_base$roa_l1)
leverage_winsor <- winsorize_finite(determinant_base$leverage_l1)
determinant_base <- determinant_base |>
  mutate(
    roa_l1_w = roa_winsor$values,
    leverage_l1_w = leverage_winsor$values
  )


# ---- Construct first-adoption risk sets --------------------------------------
build_risk_set <- function(treatment_row) {
  cohort_var <- treatment_row$cohort_var
  determinant_base |>
    # Unknown treatment timing must not enter the never-treated risk set.
    filter(!is.na(.data[[cohort_var]])) |>
    mutate(
      # The project panel stores never-treated firms as cohort 0. Convert that
      # sentinel to NA before constructing the risk set.
      cohort_year = na_if(as.integer(.data[[cohort_var]]), 0L),
      first_adopt = as.integer(!is.na(cohort_year) & year == cohort_year)
    ) |>
    # Exclude left-censored adopters and all observations after first treatment.
    filter(
      is.na(cohort_year) | cohort_year >= MIN_MODEL_ADOPTION_YEAR,
      is.na(cohort_year) | year <= cohort_year
    ) |>
    mutate(
      treatment_id = treatment_row$treatment_id,
      treatment_label = treatment_row$treatment_label,
      cohort_var = treatment_row$cohort_var
    )
}

risk_sets <- pmap(
  TREATMENT_DEFINITIONS,
  function(treatment_id, treatment_label, cohort_var) {
    build_risk_set(tibble(
      treatment_id = treatment_id,
      treatment_label = treatment_label,
      cohort_var = cohort_var
    ))
  }
)
names(risk_sets) <- TREATMENT_DEFINITIONS$treatment_id

model_variables <- c(
  "first_adopt", "hhi_l1_per_1000", "high_competition_l1", FIRM_CONTROLS,
  "year", "hhi_naics3_l1"
)
model_data <- map(risk_sets, function(data) {
  data |>
    filter(if_all(all_of(model_variables), ~ !is.na(.x)))
})


# ---- Estimate continuous and binary competition models -----------------------
build_model_formula <- function(focal_term) {
  as.formula(paste0(
    "first_adopt ~ ", focal_term, " + ",
    paste(FIRM_CONTROLS, collapse = " + "),
    " | year + hhi_naics3_l1"
  ))
}

models <- pmap(MODEL_GRID, function(
    model_spec, treatment_id, model_spec_label, focal_term,
    expected_arm_race_sign, treatment_label, cohort_var, model_key) {
  feols(
    build_model_formula(focal_term),
    data = model_data[[treatment_id]],
    vcov = ~hhi_naics3_l1,
    notes = FALSE,
    warn = FALSE
  )
})
names(models) <- MODEL_GRID$model_key

coefficient_results <- map_dfr(seq_len(nrow(MODEL_GRID)), function(index) {
  model_row <- MODEL_GRID[index, ]
  tidy_fixest_model(
    models[[model_row$model_key]],
    model_row,
    model_data[[model_row$treatment_id]]
  )
})

# This object deliberately contains only each specification's competition term.
# It drives the concise publication table; all controls remain in the complete
# coefficient output below.
hhi_results <- coefficient_results |>
  filter(
    (model_spec == "continuous_hhi" & term == "hhi_l1_per_1000") |
      (model_spec == "binary_competition" & term == "high_competition_l1")
  )


# ---- QA ----------------------------------------------------------------------
qa_rows <- map_dfr(seq_len(nrow(MODEL_GRID)), function(index) {
    model_row <- MODEL_GRID[index, ]
    data <- model_data[[model_row$treatment_id]]
    model <- models[[model_row$model_key]]
    event_data <- data |>
      filter(first_adopt == 1L)

    qa <- tribble(
      ~check, ~passed, ~value,
      "No observations after first adoption",
      all(is.na(data$cohort_year) | data$year <= data$cohort_year),
      as.character(sum(!is.na(data$cohort_year) & data$year > data$cohort_year)),
      "Never-treated cohort sentinel zero is absent from the model risk set",
      !any(data$cohort_year == 0L, na.rm = TRUE),
      as.character(sum(data$cohort_year == 0L, na.rm = TRUE)),
      "Firm controls use the immediately preceding fiscal year",
      all(data$lag_is_consecutive & data$lag_source_fyear == data$fyear - 1L),
      as.character(sum(!data$lag_is_consecutive | data$lag_source_fyear != data$fyear - 1L)),
      "Every HHI value comes from calendar year t-1",
      all(data$hhi_year == data$year - 1L),
      as.character(sum(data$hhi_year != data$year - 1L)),
      "Binary competition indicator uses the pre-specified HHI 1,800 cutoff",
      all(data$high_competition_l1 == as.integer(data$hhi_l1 <= HHI_COMPETITION_CUTOFF)),
      as.character(sum(data$high_competition_l1 != as.integer(data$hhi_l1 <= HHI_COMPETITION_CUTOFF))),
      "Both competition regimes enter the estimation sample",
      n_distinct(data$high_competition_l1) == 2L,
      as.character(n_distinct(data$high_competition_l1)),
      "Every treated event in cohort g uses HHI from g-1",
      all(event_data$hhi_year == event_data$cohort_year - 1L),
      as.character(sum(event_data$hhi_year != event_data$cohort_year - 1L)),
      "No event uses concurrent or post-treatment HHI",
      all(event_data$hhi_year < event_data$cohort_year),
      as.character(sum(event_data$hhi_year >= event_data$cohort_year)),
      "At most one first-adoption event per firm",
      all((data |> count(cik, wt = first_adopt))$n <= 1L),
      as.character(max((data |> count(cik, wt = first_adopt))$n)),
      "Model includes year and lagged-NAICS3 fixed effects",
      setequal(model$fixef_vars, c("year", "hhi_naics3_l1")),
      paste(model$fixef_vars, collapse = ", "),
      "Inference is clustered by lagged NAICS3",
      identical(all.vars(model$summary_flags$vcov), "hhi_naics3_l1"),
      paste(all.vars(model$summary_flags$vcov), collapse = ", "),
      "The requested competition term is estimated",
      model_row$focal_term %in% names(coef(model)),
      paste(names(coef(model)), collapse = ", "),
      "At least 30 lagged-NAICS3 clusters enter the model",
      n_distinct(data$hhi_naics3_l1) >= MIN_CLUSTER_COUNT,
      as.character(n_distinct(data$hhi_naics3_l1))
    )

    qa |>
      mutate(
        model_key = model_row$model_key,
        model_spec = model_row$model_spec,
        model_spec_label = model_row$model_spec_label,
        treatment_id = model_row$treatment_id,
        treatment_label = model_row$treatment_label,
        .before = 1L
      )
})

global_qa <- tribble(
  ~model_key, ~model_spec, ~model_spec_label, ~treatment_id,
  ~treatment_label, ~check, ~passed, ~value,
  "all", "all", "All specifications", "all", "All models", "HHI lies in (0, 10,000]",
  all(hhi_market_year$hhi > 0 & hhi_market_year$hhi <= 10000 + 1e-8),
  paste0(sprintf("%.2f", range(hhi_market_year$hhi)), collapse = " to "),
  "all", "all", "All specifications", "all", "All models", "HHI lookup is unique by NAICS3 and year",
  !anyDuplicated(hhi_market_year[c("year", "naics3")]),
  as.character(nrow(hhi_market_year)),
  "all", "all", "All specifications", "all", "All models", "Firm lagged-HHI lookup is unique by CIK and outcome year",
  !anyDuplicated(firm_year_lagged_hhi |> select(cik, outcome_year)),
  as.character(nrow(firm_year_lagged_hhi)),
  "all", "all", "All specifications", "all", "All models", "HHI varies within at least one NAICS3 market over time",
  any(is.finite(hhi_time_variation$hhi_sd) & hhi_time_variation$hhi_sd > 0),
  as.character(sum(is.finite(hhi_time_variation$hhi_sd) & hhi_time_variation$hhi_sd > 0))
)
qa_results <- bind_rows(global_qa, qa_rows)
if (any(!qa_results$passed)) {
  failed <- qa_results |>
    filter(!passed) |>
    pull(check)
  stop("HHI determinant QA failed: ", paste(unique(failed), collapse = "; "))
}


# ---- Sample audit and publication table --------------------------------------
sample_audit <- map_dfr(seq_len(nrow(MODEL_GRID)), function(index) {
    model_row <- MODEL_GRID[index, ]
    risk_data <- risk_sets[[model_row$treatment_id]]
    data <- model_data[[model_row$treatment_id]]
    result <- hhi_results |>
      filter(model_key == model_row$model_key)
    switchers <- data |>
      group_by(cik) |>
      summarise(n_markets = n_distinct(hhi_naics3_l1), .groups = "drop") |>
      summarise(n = sum(n_markets > 1L)) |>
      pull(n)

    tibble(
      model_key = model_row$model_key,
      model_spec = model_row$model_spec,
      model_spec_label = model_row$model_spec_label,
      focal_term = model_row$focal_term,
      treatment_id = model_row$treatment_id,
      treatment_label = model_row$treatment_label,
      cohort_var = model_row$cohort_var,
      candidate_risk_set_observations = nrow(risk_data),
      candidate_risk_set_firms = n_distinct(risk_data$cik),
      candidate_first_adoption_events = sum(risk_data$first_adopt),
      model_observations = nrow(data),
      model_firms = n_distinct(data$cik),
      first_adoption_events = sum(data$first_adopt),
      model_event_retention_pct = 100 * first_adoption_events /
        candidate_first_adoption_events,
      first_model_year = min(data$year),
      last_model_year = max(data$year),
      first_hhi_year = min(data$hhi_year),
      last_hhi_year = max(data$hhi_year),
      naics3_fixed_effects = n_distinct(data$hhi_naics3_l1),
      naics3_market_clusters = n_distinct(data$hhi_naics3_l1),
      firms_switching_naics3 = switchers,
      mean_hhi_l1 = mean(data$hhi_l1),
      median_hhi_l1 = median(data$hhi_l1),
      min_hhi_l1 = min(data$hhi_l1),
      max_hhi_l1 = max(data$hhi_l1),
      high_competition_observations = sum(data$high_competition_l1 == 1L),
      low_competition_observations = sum(data$high_competition_l1 == 0L),
      focal_estimate = result$estimate,
      focal_std_error = result$std_error,
      focal_p_value_two_sided = result$p_value,
      focal_p_value_arm_race_one_sided = result$p_value_arm_race_one_sided,
      arm_race_predicted_sign = model_row$expected_arm_race_sign,
      estimated_sign = if_else(result$estimate < 0, "negative", "positive")
    )
})

binary_identification_audit <- map2_dfr(
  TREATMENT_DEFINITIONS$treatment_id,
  TREATMENT_DEFINITIONS$treatment_label,
  function(treatment_id, treatment_label) {
    data <- model_data[[treatment_id]]
    market_support <- data |>
      mutate(hhi_naics3_l1 = as.character(hhi_naics3_l1)) |>
      group_by(hhi_naics3_l1) |>
      summarise(
        n_regimes = n_distinct(high_competition_l1),
        .groups = "drop"
      )
    crossing_markets <- market_support |>
      filter(n_regimes == 2L) |>
      pull(hhi_naics3_l1)
    identifying_data <- data |>
      filter(as.character(hhi_naics3_l1) %in% crossing_markets)

    tibble(
      treatment_id,
      treatment_label,
      total_naics3_clusters = n_distinct(data$hhi_naics3_l1),
      cutoff_crossing_naics3_clusters = length(crossing_markets),
      observations_in_crossing_clusters = nrow(identifying_data),
      share_observations_in_crossing_clusters = 100 * nrow(identifying_data) /
        nrow(data),
      events_in_crossing_clusters = sum(identifying_data$first_adopt),
      total_first_adoption_events = sum(data$first_adopt),
      share_events_in_crossing_clusters = 100 *
        events_in_crossing_clusters / total_first_adoption_events
    )
  }
)

publication_values <- hhi_results |>
  transmute(
    model_spec,
    treatment_id,
    estimate = paste0(format_estimate(estimate), significance_stars(p_value)),
    std_error = paste0("(", format_estimate(std_error), ")"),
    observations = format_count(n_obs),
    firms = format_count(n_firms),
    first_adoptions = format_count(n_first_adoptions)
  )

build_focal_rows <- function(model_spec_value, focal_label, start_order = 1L) {
  values <- publication_values |>
    filter(.data$model_spec == .env$model_spec_value)
  bind_rows(
    values |>
      transmute(
        treatment_id, row_order = start_order,
        row_label = focal_label, value = estimate
      ),
    values |>
      transmute(
        treatment_id, row_order = start_order + 1L,
        row_label = "", value = std_error
      ),
    values |>
      transmute(
        treatment_id, row_order = start_order + 2L,
        row_label = "Observations", value = observations
      ),
    values |>
      transmute(
        treatment_id, row_order = start_order + 3L,
        row_label = "Firms", value = firms
      ),
    values |>
      transmute(
        treatment_id, row_order = start_order + 4L,
        row_label = "First-adoption events", value = first_adoptions
      )
  )
}

common_specification_rows <- function(start_order) {
  tibble(
    row_order = seq.int(start_order, start_order + 3L),
    row_label = c(
      "Lagged firm controls", "Year fixed effects", "NAICS3 fixed effects",
      "NAICS3-clustered standard errors"
    )
  ) |>
    crossing(treatment_id = TREATMENT_DEFINITIONS$treatment_id) |>
    mutate(value = "Yes")
}

make_wide_focal_table <- function(long_data) {
  long_data |>
  mutate(
    treatment_id = factor(treatment_id, levels = TREATMENT_DEFINITIONS$treatment_id)
  ) |>
  arrange(row_order, treatment_id) |>
  pivot_wider(
    id_cols = c(row_order, row_label),
    names_from = treatment_id,
    values_from = value
  ) |>
  arrange(row_order) |>
  select(-row_order)
}

# Main table: continuous HHI only.
publication_table <- bind_rows(
  build_focal_rows(
    "continuous_hhi",
    "NAICS3 sales HHI (t-1, per 1,000)"
  ),
  common_specification_rows(10L)
) |>
  make_wide_focal_table()

# Secondary robustness table: binary cutoff plus its effective identifying
# support under NAICS3 fixed effects.
binary_support_rows <- bind_rows(
  binary_identification_audit |>
    transmute(
      treatment_id, row_order = 7L,
      row_label = "Cutoff-crossing NAICS3 markets",
      value = format_count(cutoff_crossing_naics3_clusters)
    ),
  binary_identification_audit |>
    transmute(
      treatment_id, row_order = 8L,
      row_label = "Events in cutoff-crossing markets",
      value = format_count(events_in_crossing_clusters)
    )
)

binary_robustness_table <- bind_rows(
  build_focal_rows(
    "binary_competition",
    "High competition (HHI t-1 <= 1,800)"
  ),
  binary_support_rows,
  common_specification_rows(10L)
) |>
  make_wide_focal_table()

# A separate diagnostic table reports all control coefficients from both model
# families. The publication table above intentionally remains focal-only.
term_order <- c("hhi_l1_per_1000", "high_competition_l1", FIRM_CONTROLS)
all_coefficients_table <- bind_rows(
  coefficient_results |>
    transmute(
      model_key, term,
      term_order = match(term, term_order) * 2L - 1L,
      row_label = term_label,
      value = paste0(format_estimate(estimate), significance_stars(p_value))
    ),
  coefficient_results |>
    transmute(
      model_key, term,
      term_order = match(term, term_order) * 2L,
      row_label = "",
      value = paste0("(", format_estimate(std_error), ")")
    )
) |>
  arrange(term_order, model_key) |>
  pivot_wider(
    id_cols = c(term_order, row_label),
    names_from = model_key,
    values_from = value
  ) |>
  arrange(term_order) |>
  select(-term_order) |>
  mutate(across(everything(), ~ replace_na(.x, "")))


# ---- Save --------------------------------------------------------------------
output_manifest <- tibble(
  output = c(
    "bundle", "market_year_hhi", "firm_year_lagged_hhi", "coefficients",
    "all_coefficients_table_csv", "all_coefficients_table_markdown",
    "publication_csv", "publication_markdown", "sample_audit", "qa",
    "binary_robustness_csv", "binary_robustness_markdown",
    "binary_identification_audit", "coverage_audit", "run_manifest"
  ),
  path = c(
    HHI_DETERMINANT_BUNDLE_RDS, HHI_MARKET_YEAR_CSV,
    FIRM_YEAR_LAGGED_HHI_CSV, HHI_DETERMINANT_COEFFICIENTS_CSV,
    HHI_DETERMINANT_ALL_COEFFICIENTS_CSV,
    HHI_DETERMINANT_ALL_COEFFICIENTS_MD,
    HHI_DETERMINANT_PUBLICATION_CSV, HHI_DETERMINANT_PUBLICATION_MD,
    HHI_DETERMINANT_SAMPLE_AUDIT_CSV, HHI_DETERMINANT_QA_CSV,
    HHI_BINARY_ROBUSTNESS_CSV, HHI_BINARY_ROBUSTNESS_MD,
    HHI_BINARY_IDENTIFICATION_AUDIT_CSV,
    HHI_DETERMINANT_COVERAGE_CSV, HHI_DETERMINANT_MANIFEST_CSV
  ),
  design = "primary continuous HHI at t-1; binary cutoff robustness only"
)

bundle <- list(
  design = list(
    equation = paste0(
      "FirstAdopt_it = alpha + beta HHI_m,t-1 + gamma X_i,t-1 + ",
      "year FE + NAICS3 FE + error_it"
    ),
    binary_equation = paste0(
      "FirstAdopt_it = alpha + beta HighCompetition_m,t-1 + gamma X_i,t-1 + ",
      "year FE + NAICS3 FE + error_it"
    ),
    binary_definition = "High competition equals 1 when lagged HHI <= 1,800; low competition is the reference group.",
    hhi_timing = "For every at-risk row t, HHI uses the firm's NAICS3 market in t-1.",
    event_timing = "A cohort-g event uses HHI from g-1.",
    expected_hhi_sign = "negative",
    interpretation = "Predictive/descriptive; not a causal competition effect.",
    hhi_universe = "Full project Compustat panel with finite positive sales.",
    inference = "One-way clustered by lagged NAICS3 market."
  ),
  settings = list(
    analysis_start_year = ANALYSIS_START_YEAR,
    analysis_end_year = ANALYSIS_END_YEAR,
    hhi_first_year = HHI_FIRST_YEAR,
    hhi_last_year = HHI_LAST_YEAR,
    min_model_adoption_year = MIN_MODEL_ADOPTION_YEAR,
    hhi_scale = HHI_SCALE,
    hhi_competition_cutoff = HHI_COMPETITION_CUTOFF,
    winsor_probs = WINSOR_PROBS,
    roa_winsor_bounds = roa_winsor$bounds,
    leverage_winsor_bounds = leverage_winsor$bounds,
    firm_controls = FIRM_CONTROLS
  ),
  hhi_market_year = hhi_market_year,
  hhi_time_variation = hhi_time_variation,
  coverage_audit = coverage_audit,
  risk_sets = risk_sets,
  model_data = model_data,
  models = models,
  coefficients = coefficient_results,
  hhi_results = hhi_results,
  sample_audit = sample_audit,
  binary_identification_audit = binary_identification_audit,
  qa = qa_results,
  publication_table = publication_table,
  binary_robustness_table = binary_robustness_table,
  all_coefficients_table = all_coefficients_table,
  output_manifest = output_manifest
)

if (SAVE_HHI_DETERMINANT_OUTPUTS) {
  dir.create(HHI_DETERMINANT_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, HHI_DETERMINANT_BUNDLE_RDS)
  write_csv(hhi_market_year, HHI_MARKET_YEAR_CSV, na = "")
  write_csv(firm_year_lagged_hhi, FIRM_YEAR_LAGGED_HHI_CSV, na = "")
  write_csv(coefficient_results, HHI_DETERMINANT_COEFFICIENTS_CSV, na = "")
  write_csv(all_coefficients_table, HHI_DETERMINANT_ALL_COEFFICIENTS_CSV, na = "")
  write_all_coefficients_markdown(
    all_coefficients_table,
    HHI_DETERMINANT_ALL_COEFFICIENTS_MD
  )
  write_csv(publication_table, HHI_DETERMINANT_PUBLICATION_CSV, na = "")
  write_publication_markdown(publication_table, HHI_DETERMINANT_PUBLICATION_MD)
  write_csv(binary_robustness_table, HHI_BINARY_ROBUSTNESS_CSV, na = "")
  write_binary_robustness_markdown(
    binary_robustness_table,
    HHI_BINARY_ROBUSTNESS_MD
  )
  write_csv(
    binary_identification_audit,
    HHI_BINARY_IDENTIFICATION_AUDIT_CSV,
    na = ""
  )
  write_csv(sample_audit, HHI_DETERMINANT_SAMPLE_AUDIT_CSV, na = "")
  write_csv(qa_results, HHI_DETERMINANT_QA_CSV, na = "")
  write_csv(coverage_audit, HHI_DETERMINANT_COVERAGE_CSV, na = "")
  write_csv(output_manifest, HHI_DETERMINANT_MANIFEST_CSV, na = "")
}

cat("\nLagged NAICS3 HHI determinant models complete.\n")
print(
  sample_audit |>
    select(
      model_spec_label, treatment_label, model_observations, model_firms,
      first_adoption_events, naics3_market_clusters,
      focal_estimate, focal_std_error,
      focal_p_value_two_sided, focal_p_value_arm_race_one_sided
    )
)
cat("\nAll ", nrow(qa_results), " QA checks passed.\n", sep = "")
if (SAVE_HHI_DETERMINANT_OUTPUTS) {
  cat("Outputs: ", HHI_DETERMINANT_OUTPUT_DIR, "\n", sep = "")
}
