#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Characteristics associated with first AI adoption
# ------------------------------------------------------------------------------
# This is a descriptive adoption model, not a causal treatment-effect model.
# The main specification is a linear probability model (LPM) estimated on the
# firm-year risk set. Eventual adopters remain in the sample through their first
# adoption year and leave the risk set afterwards; never-adopters are retained.
#
# All time-varying characteristics are measured in the immediately preceding
# fiscal year. Same-year values and averages containing the adoption year are
# deliberately excluded because they may already be affected by AI adoption.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(dplyr)
library(fixest)
library(purrr)
library(tibble)


# ---- Settings ----------------------------------------------------------------
DETERMINANTS_OUTPUT_DIR <- file.path(OUTPUT_DIR, "ai_adoption_determinants")
DETERMINANTS_BUNDLE_RDS <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adoption_determinants_bundle.rds"
)
DETERMINANTS_COEFFICIENTS_CSV <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adoption_lpm_coefficients.csv"
)
DETERMINANTS_COMPARISON_CSV <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adoption_lpm_univariate_vs_joint.csv"
)
DETERMINANTS_CHARACTERISTICS_CSV <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adopter_characteristics.csv"
)
DETERMINANTS_SAMPLE_AUDIT_CSV <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adoption_lpm_sample_audit.csv"
)
DETERMINANTS_RD_MISSINGNESS_AUDIT_CSV <- file.path(
  DETERMINANTS_OUTPUT_DIR,
  "ai_adoption_rd_missingness_audit.csv"
)

SAVE_DETERMINANTS_OUTPUTS <- TRUE

# Firms first detected in 2015 are left-censored because the AI score panel does
# not reveal whether they adopted before the first observed year.
MIN_MODEL_ADOPTION_YEAR <- 2016L

# Winsorize the two unbounded accounting ratios used in the main model. CAPX and
# R&D intensities were already winsorized before their lags were constructed.
WINSOR_PROBS <- c(0.01, 0.99)

# Use the same significance convention as the DiD and competition tables.
SIGNIFICANCE_CODES <- c("***" = 0.01, "**" = 0.05, "*" = 0.10)


# ---- Model variables ----------------------------------------------------------
var_labels <- c(
  log_at_l1 = "Size (log assets, t-1)",
  roa_l1_w = "ROA (t-1, winsorized)",
  cash_ratio_l1 = "Cash ratio (t-1)",
  leverage_l1_w = "Leverage (t-1, winsorized)",
  capx_intensity_l1 = "CAPX intensity (t-1)",
  rd_intensity_l1_filled = "R&D intensity (t-1; 0 if non-reporter)",
  rd_reporter_l1 = "R&D reporter (t-1)",
  log_labor_productivity_l1 = "Labour productivity (log, t-1)",
  firm_age_l1 = "Firm age (years, t-1)"
#  aiie = "Industry AI exposure"
)

# Pre-specify the joint model here. The same variables are each estimated once
# on their own and once together in the joint specification.
joint_vars <- names(var_labels)

# Short headings keep the conventional wide regression table readable. Each
# heading corresponds to a one-variable model; the joint specification is the
# final column.
univariate_column_labels <- c(
  log_at_l1 = "Size",
  roa_l1_w = "ROA",
  cash_ratio_l1 = "Cash ratio",
  leverage_l1_w = "Leverage",
  capx_intensity_l1 = "CAPX intensity",
  rd_intensity_l1_filled = "R&D intensity (0 if non-reporter)",
  rd_reporter_l1 = "R&D reporter",
  log_labor_productivity_l1 = "Labour productivity",
  firm_age_l1 = "Firm age"
#  aiie = "AI exposure"
)


# ---- Helpers -----------------------------------------------------------------
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

winsorize_vec <- function(x, probs = WINSOR_PROBS) {
  if (all(is.na(x))) return(x)
  bounds <- quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, bounds[[1]]), bounds[[2]])
}

build_lpm_formula <- function(rhs_vars) {
  as.formula(
    paste0(
      "first_adopt ~ ",
      paste(rhs_vars, collapse = " + "),
      " | year + naics3"
    )
  )
}

significance_stars <- function(p_value) {
  case_when(
    p_value < SIGNIFICANCE_CODES[["***"]] ~ "***",
    p_value < SIGNIFICANCE_CODES[["**"]] ~ "**",
    p_value < SIGNIFICANCE_CODES[["*"]] ~ "*",
    TRUE ~ ""
  )
}

extract_term_result <- function(model, term) {
  coefficient_matrix <- coeftable(model)
  if (!term %in% rownames(coefficient_matrix)) {
    stop("Term not found in fitted model: ", term)
  }

  tibble(
    estimate = coefficient_matrix[term, 1],
    std_error = coefficient_matrix[term, 2],
    statistic = coefficient_matrix[term, 3],
    p_value = coefficient_matrix[term, 4]
  )
}

format_lpm_result <- function(estimate, std_error, p_value) {
  paste0(
    sprintf("%.4f", estimate),
    significance_stars(p_value),
    " (",
    sprintf("%.4f", std_error),
    ")"
  )
}

format_etable_r2 <- function(table, digits = 4L) {
  r2_rows <- table[[1L]] %in% c("R2", "Within R2")
  number_format <- paste0("%.", digits, "f")

  table[r2_rows, -1L] <- lapply(
    table[r2_rows, -1L, drop = FALSE],
    \(x) sprintf(number_format, as.numeric(x))
  )

  table
}

tidy_fixest_model <- function(model, model_name) {
  model_nobs <- nobs(model)
  coefficient_matrix <- as.data.frame(coeftable(model))
  coefficient_matrix$term <- rownames(coefficient_matrix)
  rownames(coefficient_matrix) <- NULL
  names(coefficient_matrix)[seq_len(4L)] <- c(
    "estimate",
    "std_error",
    "statistic",
    "p_value"
  )

  as_tibble(coefficient_matrix) |>
    transmute(
      model = model_name,
      term,
      term_label = if_else(
        term %in% names(var_labels),
        unname(var_labels[term]),
        term
      ),
      estimate,
      std_error,
      statistic,
      p_value,
      ci_low = estimate - 1.96 * std_error,
      ci_high = estimate + 1.96 * std_error,
      n_obs = model_nobs
    )
}

summarise_characteristic <- function(data, variable) {
  event_values <- data[[variable]][data$first_adopt == 1L]
  risk_values <- data[[variable]][data$first_adopt == 0L]

  event_sd <- sd(event_values)
  risk_sd <- sd(risk_values)
  pooled_sd <- sqrt(
    (
      (length(event_values) - 1L) * event_sd^2 +
        (length(risk_values) - 1L) * risk_sd^2
    ) /
      (length(event_values) + length(risk_values) - 2L)
  )

  tibble(
    variable,
    variable_label = unname(var_labels[[variable]]),
    adopter_n = length(event_values),
    at_risk_non_event_n = length(risk_values),
    adopter_mean = mean(event_values),
    at_risk_non_event_mean = mean(risk_values),
    adopter_median = median(event_values),
    at_risk_non_event_median = median(risk_values),
    standardized_mean_difference = if_else(
      is.finite(pooled_sd) & pooled_sd > 0,
      (mean(event_values) - mean(risk_values)) / pooled_sd,
      NA_real_
    )
  )
}

summarise_model_sample <- function(model, model_name, model_type, source_data) {
  model_data <- source_data[fixest::obs(model), , drop = FALSE]

  tibble(
    model = model_name,
    model_type,
    characteristic = if (model_name %in% names(var_labels)) {
      unname(var_labels[[model_name]])
    } else {
      "All covariates"
    },
    n_obs = nrow(model_data),
    n_firms = n_distinct(model_data$cik),
    n_first_adoptions = sum(model_data$first_adopt),
    first_year = min(model_data$year),
    last_year = max(model_data$year)
  )
}

summarise_rd_missingness <- function(data, sample) {
  tibble(
    sample,
    firm_years = nrow(data),
    reported_intensity_observed = sum(
      data$rd_reporter_l1 == 1L & !is.na(data$rd_intensity_l1)
    ),
    nonreporters_zero_filled = sum(data$rd_reporter_l1 == 0L),
    reporter_intensity_undefined = sum(
      data$rd_reporter_l1 == 1L & is.na(data$rd_intensity_l1)
    ),
    filled_intensity_nonmissing = sum(!is.na(data$rd_intensity_l1_filled))
  )
}

# ---- Load final analysis panel ------------------------------------------------
if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop(
    "Final analysis panel not found: ", ANALYSIS_PANEL_RDS,
    ". Run 4. build_or_load_panel_data.R first."
  )
}

panel_ai <- readRDS(ANALYSIS_PANEL_RDS)
if (!"ai_score" %in% names(panel_ai) || anyNA(panel_ai$ai_score)) {
  stop("Determinants source panel must contain only matched annual AI-score years.")
}

required_cols <- unique(c(
  "cik",
  "year",
  "fyear",
  "naics2",
  "naics3",
  "ai_adoption_year",
  "lag_source_fyear",
  "lag_is_consecutive",
  "rd_reporter_l1",
  "log_at_l1",
  "roa_l1",
  "cash_ratio_l1",
  "leverage_l1",
  "capx_intensity_l1",
  "rd_intensity_l1",
  "log_labor_productivity_l1",
  "firm_age_l1",
  "aiie"
))

missing_cols <- setdiff(required_cols, names(panel_ai))
if (length(missing_cols) > 0L) {
  stop(
    "Analysis panel is missing determinants variables: ",
    paste(missing_cols, collapse = ", "),
    ". Rebuild the annual and merged panels before running this script."
  )
}


# ---- Audit lag integrity ------------------------------------------------------
source_lag_vars <- c(
  "log_at_l1",
  "roa_l1",
  "cash_ratio_l1",
  "leverage_l1",
  "capx_intensity_l1",
  "rd_intensity_l1",
  "log_labor_productivity_l1",
  "firm_age_l1"
)

invalid_lag_values <- panel_ai |>
  filter(
    !(lag_is_consecutive %in% TRUE),
    if_any(all_of(source_lag_vars), ~ !is.na(.x))
  )

if (nrow(invalid_lag_values) > 0L) {
  stop(
    "Found ", nrow(invalid_lag_values),
    " rows with non-consecutive fiscal years but non-missing lagged values. ",
    "Rebuild the annual Compustat panel with the corrected lag logic."
  )
}

invalid_lag_years <- panel_ai |>
  filter(
    lag_is_consecutive %in% TRUE,
    lag_source_fyear != fyear - 1L
  )

if (nrow(invalid_lag_years) > 0L) {
  stop(
    "Found consecutive-lag rows where lag_source_fyear != fyear - 1."
  )
}


# ---- Build the first-adoption risk set ----------------------------------------
excluded_left_censored_firms <- panel_ai |>
  filter(
    !is.na(ai_adoption_year),
    ai_adoption_year > 0L,
    ai_adoption_year < MIN_MODEL_ADOPTION_YEAR
  ) |>
  summarise(n = n_distinct(cik)) |>
  pull(n)

risk_panel_before_lag_filter <- panel_ai |>
  mutate(
    cik = as.character(cik),
    year = as.integer(year),
    fyear = as.integer(fyear),
    naics3 = as.character(naics3),
    ai_adoption_year = as.integer(ai_adoption_year),
    first_adopt = as.integer(
      ai_adoption_year >= MIN_MODEL_ADOPTION_YEAR &
        year == ai_adoption_year
    )
  ) |>
  filter(
    !is.na(cik), cik != "",
    !is.na(year),
    !is.na(naics3), naics3 != "",
    ai_adoption_year == 0L | ai_adoption_year >= MIN_MODEL_ADOPTION_YEAR,
    ai_adoption_year == 0L | year <= ai_adoption_year
  ) |>
  arrange(cik, year)

invalid_risk_lag_rows <- sum(
  !(risk_panel_before_lag_filter$lag_is_consecutive %in% TRUE)
)


# Audit lagged panel and lost ciks

lost_event_firms <- risk_panel_before_lag_filter |>
  filter(
    first_adopt == 1L,
    !(lag_is_consecutive %in% TRUE)
  ) |>
  distinct(cik)


event_lag_audit <- risk_panel_before_lag_filter |>
  filter(first_adopt == 1L) |>
  summarise(
    adoption_events_before_lag_filter = n(),
    events_with_consecutive_lag = sum(lag_is_consecutive %in% TRUE),
    events_lost_due_to_lag = sum(!(lag_is_consecutive %in% TRUE)),
    share_events_retained = events_with_consecutive_lag /
      adoption_events_before_lag_filter
  )

print(event_lag_audit)

lost_event_qa <- risk_panel_before_lag_filter |>
  semi_join(
    lost_event_firms,
    by = "cik"
  ) |>
  filter(
    first_adopt == 1L
  ) |>
  select(
    cik,
    year,
    fyear,
    ai_adoption_year,
    first_adopt,
    lag_source_fyear,
    lag_is_consecutive
  ) |>
  arrange(
    year,
    cik
  )

print(lost_event_qa)


det_panel <- risk_panel_before_lag_filter |>
  filter(
    lag_is_consecutive %in% TRUE
  ) |>
  mutate(
    roa_l1_w = winsorize_vec(roa_l1),
    leverage_l1_w = winsorize_vec(leverage_l1),
    
    rd_reporter_l1 =
      as.integer(rd_reporter_l1),
    
    rd_intensity_l1_filled =
      if_else(
        rd_reporter_l1 == 0L,
        0,
        rd_intensity_l1
      )
  )

# Compustat non-reporting is treated as a distinct disclosure margin. Set the
# lagged intensity to zero only when lagged XRD is not reported and include the
# reporter indicator separately. If XRD is reported but the ratio is undefined
# (for example, because beginning assets are unavailable or non-positive), keep
# the intensity missing; those rows are excluded whenever the intensity enters.
if (
  anyNA(det_panel$rd_reporter_l1) ||
  any(
    det_panel$rd_reporter_l1 == 0L &
      (
        is.na(det_panel$rd_intensity_l1_filled) |
          det_panel$rd_intensity_l1_filled != 0
      )
  ) ||
  any(
    det_panel$rd_reporter_l1 == 1L &
      is.na(det_panel$rd_intensity_l1) !=
        is.na(det_panel$rd_intensity_l1_filled)
  ) ||
  any(
    det_panel$rd_reporter_l1 == 1L &
      !is.na(det_panel$rd_intensity_l1) &
      det_panel$rd_intensity_l1 != det_panel$rd_intensity_l1_filled
  )
) {
  stop("R&D non-reporter zero-fill rule failed its QA check.")
}

event_counts <- det_panel |>
  filter(first_adopt == 1L) |>
  count(cik, name = "n_events")

if (any(event_counts$n_events != 1L)) {
  stop("Each adopting firm must have exactly one first-adoption event.")
}


# ---- Estimation samples -------------------------------------------------------
# Each one-variable model uses every eligible risk-set row for which its own
# covariate is observed. The joint model uses complete cases for all covariates.
# fixest applies these model-specific missing-value filters from det_panel.

# ---- Estimate one-variable and joint LPMs -------------------------------------
univariate_models <- map(
  joint_vars,
  ~ feols(
    build_lpm_formula(.x),
    data = det_panel,
    vcov = ~ cik
  )
)
names(univariate_models) <- joint_vars

joint_model <- feols(
  build_lpm_formula(joint_vars),
  data = det_panel,
  vcov = ~ cik
)

joint_model_panel <- det_panel[fixest::obs(joint_model), , drop = FALSE]

if (sum(joint_model_panel$first_adopt) == 0L) {
  stop("The joint determinants sample contains no first-adoption events.")
}

rd_missingness_audit <- bind_rows(
  summarise_rd_missingness(det_panel, "Eligible risk set"),
  summarise_rd_missingness(joint_model_panel, "Joint LPM estimation sample")
)

univariate_results <- imap_dfr(
  univariate_models,
  ~ extract_term_result(.x, .y) |>
    mutate(
      variable = .y,
      n_obs = nobs(.x),
      .before = 1L
    )
)

joint_results <- map_dfr(
  joint_vars,
  ~ extract_term_result(joint_model, .x) |>
    mutate(
      variable = .x,
      n_obs = nobs(joint_model),
      .before = 1L
    )
)

# Retain a tidy coefficient-and-sample comparison for CSV output and
# programmatic use.
determinants_comparison <- univariate_results |>
  select(
    variable,
    univariate_estimate = estimate,
    univariate_std_error = std_error,
    univariate_p_value = p_value,
    univariate_n_obs = n_obs
  ) |>
  left_join(
    joint_results |>
      select(
        variable,
        joint_estimate = estimate,
        joint_std_error = std_error,
        joint_p_value = p_value,
        joint_n_obs = n_obs
      ),
    by = "variable"
  ) |>
  mutate(
    characteristic = unname(var_labels[variable]),
    `One-variable LPM` = format_lpm_result(
      univariate_estimate,
      univariate_std_error,
      univariate_p_value
    ),
    `Joint LPM` = format_lpm_result(
      joint_estimate,
      joint_std_error,
      joint_p_value
    )
  ) |>
  select(
    characteristic,
    `One-variable LPM`,
    univariate_n_obs,
    `Joint LPM`,
    joint_n_obs
  )

# Main regression table: one column per standalone characteristic, followed by
# the pre-specified joint model, matching the earlier table design.
display_models <- c(
  setNames(
    univariate_models,
    unname(univariate_column_labels[names(univariate_models)])
  ),
  list("Joint model" = joint_model)
)

determinants_table <- etable(
  display_models,
  dict = var_labels,
  fitstat = ~ n + r2 + wr2,
  signif.code = SIGNIFICANCE_CODES,
  digits.stats = 4,
  postprocess.df = format_etable_r2
)

coefficient_table <- bind_rows(
  imap_dfr(univariate_models, tidy_fixest_model) |>
    mutate(model_type = "one_variable", .after = model),
  tidy_fixest_model(joint_model, "Joint LPM") |>
    mutate(model_type = "joint", .after = model)
)

model_sample_audit <- bind_rows(
  imap_dfr(
    univariate_models,
    ~ summarise_model_sample(.x, .y, "one_variable", det_panel)
  ),
  summarise_model_sample(joint_model, "Joint LPM", "joint", det_panel)
)

adopter_characteristics <- imap_dfr(
  univariate_models,
  ~ summarise_characteristic(
    det_panel[fixest::obs(.x), , drop = FALSE],
    .y
  )
)


# ---- Sample and methodology overview -----------------------------------------
det_sample_overview <- tibble(
  metric = c(
    "Model",
    "Outcome",
    "Covariate timing",
    "Fixed effects",
    "Standard errors",
    "Candidate risk-set firm-years",
    "Rows without a consecutive prior fiscal year",
    "Eligible risk-set firm-years",
    "R&D missing-data rule",
    "Eligible rows with reported R&D intensity",
    "Eligible non-reporters assigned zero R&D intensity",
    "Eligible reporters with undefined R&D intensity",
    "One-variable sample rule",
    "One-variable firm-years (range)",
    "Joint-model firm-years",
    "Joint-model firms",
    "Joint-model first-adoption events",
    "Left-censored firms excluded",
    "Years covered",
    "Mean annual first-adoption rate"
  ),
  value = c(
    "One-variable LPMs and pre-specified joint LPM",
    "First AI-adoption treatment year",
    "Immediately preceding consecutive fiscal year (t-1)",
    "Calendar year and NAICS3",
    "One-way clustered by firm (CIK)",
    format(nrow(risk_panel_before_lag_filter), big.mark = ","),
    format(invalid_risk_lag_rows, big.mark = ","),
    format(nrow(det_panel), big.mark = ","),
    paste0(
      "Set lagged R&D intensity to zero only when the lagged R&D-reporter ",
      "indicator equals zero; include the reporter indicator separately; ",
      "leave undefined reporter ratios missing"
    ),
    format(
      rd_missingness_audit$reported_intensity_observed[
        rd_missingness_audit$sample == "Eligible risk set"
      ],
      big.mark = ","
    ),
    format(
      rd_missingness_audit$nonreporters_zero_filled[
        rd_missingness_audit$sample == "Eligible risk set"
      ],
      big.mark = ","
    ),
    format(
      rd_missingness_audit$reporter_intensity_undefined[
        rd_missingness_audit$sample == "Eligible risk set"
      ],
      big.mark = ","
    ),
    "Available cases for the covariate in each model",
    paste0(
      format(
        min(model_sample_audit$n_obs[model_sample_audit$model_type == "one_variable"]),
        big.mark = ","
      ),
      "-",
      format(
        max(model_sample_audit$n_obs[model_sample_audit$model_type == "one_variable"]),
        big.mark = ","
      )
    ),
    format(nrow(joint_model_panel), big.mark = ","),
    format(n_distinct(joint_model_panel$cik), big.mark = ","),
    format(sum(joint_model_panel$first_adopt), big.mark = ","),
    format(excluded_left_censored_firms, big.mark = ","),
    paste0(min(det_panel$year), "-", max(det_panel$year)),
    round(safe_mean(det_panel$first_adopt), 4)
  )
)


# ---- Bundle and save outputs --------------------------------------------------
determinants_bundle <- list(
  det_sample_overview = det_sample_overview,
  lag_audit = list(
    source_lag_variables = source_lag_vars,
    invalid_lag_values = nrow(invalid_lag_values),
    risk_rows_without_consecutive_lag = invalid_risk_lag_rows
  ),
  det_panel = det_panel,
  joint_model_panel = joint_model_panel,
  rd_missingness_audit = rd_missingness_audit,
  model_specs = list(
    one_variable = as.list(joint_vars),
    joint = joint_vars,
    sample_rule = list(
      one_variable = "Available cases for the covariate in each model",
      joint = "Complete cases for all joint-model covariates"
    ),
    rd_missing_data_rule = paste0(
      "Lagged R&D intensity is set to zero only for lagged R&D non-reporters; ",
      "the lagged reporter indicator enters separately. Reporter rows with an ",
      "undefined intensity remain missing."
    )
  ),
  univariate_models = univariate_models,
  joint_model = joint_model,
  determinants_table = determinants_table,
  determinants_etable = determinants_table,
  determinants_comparison = determinants_comparison,
  coefficient_table = coefficient_table,
  model_sample_audit = model_sample_audit,
  adopter_characteristics = adopter_characteristics
)

if (SAVE_DETERMINANTS_OUTPUTS) {
  dir.create(DETERMINANTS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(determinants_bundle, DETERMINANTS_BUNDLE_RDS)
  readr::write_csv(coefficient_table, DETERMINANTS_COEFFICIENTS_CSV)
  readr::write_csv(determinants_comparison, DETERMINANTS_COMPARISON_CSV)
  readr::write_csv(adopter_characteristics, DETERMINANTS_CHARACTERISTICS_CSV)
  readr::write_csv(model_sample_audit, DETERMINANTS_SAMPLE_AUDIT_CSV)
  readr::write_csv(
    rd_missingness_audit,
    DETERMINANTS_RD_MISSINGNESS_AUDIT_CSV
  )
}


# ---- Console output -----------------------------------------------------------
cat("\nEstimated first-adoption LPMs.\n")
cat("Covariates: consecutive fiscal-year t-1 values only.\n")
cat("Eligible risk-set rows:", format(nrow(det_panel), big.mark = ","), "\n")
cat(
  "One-variable sample rows:",
  format(
    min(model_sample_audit$n_obs[model_sample_audit$model_type == "one_variable"]),
    big.mark = ","
  ),
  "to",
  format(
    max(model_sample_audit$n_obs[model_sample_audit$model_type == "one_variable"]),
    big.mark = ","
  ),
  "\n"
)
cat("Joint-model rows:", format(nrow(joint_model_panel), big.mark = ","), "\n")
cat("Joint-model firms:", format(n_distinct(joint_model_panel$cik), big.mark = ","), "\n")
cat("Joint-model first-adoption events:", format(sum(joint_model_panel$first_adopt), big.mark = ","), "\n")
cat(
  "Joint-model R&D composition:",
  format(
    rd_missingness_audit$reported_intensity_observed[
      rd_missingness_audit$sample == "Joint LPM estimation sample"
    ],
    big.mark = ","
  ),
  "reported-intensity rows +",
  format(
    rd_missingness_audit$nonreporters_zero_filled[
      rd_missingness_audit$sample == "Joint LPM estimation sample"
    ],
    big.mark = ","
  ),
  "zero-filled non-reporter rows\n"
)
cat("Invalid/stale lag values retained: 0\n")

cat("\nOne-variable LPM columns followed by the joint model:\n")
print(determinants_table)
cat("\nAll models use year and NAICS3 fixed effects and firm-clustered standard errors.\n")

if (SAVE_DETERMINANTS_OUTPUTS) {
  cat("\nSaved determinants outputs to:", DETERMINANTS_OUTPUT_DIR, "\n")
}
