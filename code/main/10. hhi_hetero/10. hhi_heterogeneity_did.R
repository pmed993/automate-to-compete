#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# HHI heterogeneity in the effect of AI adoption
# ------------------------------------------------------------------------------
# This script estimates the preferred specification from 9. did.R separately
# for firms in high- and low-competition markets and formally tests whether the
# two overall ATTs differ.
#
# For each treated cohort g, both treated and eligible comparison firms are
# classified by their own NAICS3 market and market HHI in calendar year g-1.
# HHI is calculated from the broader Compustat panel.  The regime is fixed
# within each cohort's ATT(g,t) comparisons, although a comparison firm may
# belong to different regimes when serving different cohorts.
#
# Estimation inherits the main design:
#   * unbalanced panel;
#   * treated cohorts beginning in 2016;
#   * not-yet-treated comparison firms;
#   * cohort-specific g-1 controls for treated and comparison firms;
#   * doubly robust DRDID 2x2 cells aggregated with did::aggte();
#   * first-observed NAICS3 market cluster multiplier-bootstrap inference;
#   * the preferred firm + NAICS2 control specification only; and
#   * ROA omitted when operating profitability is the outcome.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(did)
library(DRDID)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)

source("code/main/9. did/9. did_gminus1_helpers.R")


# ---- Settings ----------------------------------------------------------------
HHI_OUTPUT_ROOT <- Sys.getenv("HHI_OUTPUT_ROOT", unset = OUTPUT_DIR)
HHI_OUTPUT_DIR <- file.path(HHI_OUTPUT_ROOT, "hhi_heterogeneity_did")
HHI_MARKET_YEAR_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_market_year.csv")
HHI_FIRM_REGIME_CSV <- file.path(HHI_OUTPUT_DIR, "firm_cohort_hhi_regime.csv")
HHI_REGIME_SUMMARY_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_regime_summary.csv")
HHI_COVERAGE_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_sample_coverage.csv")
HHI_COHORT_MEMBERSHIP_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_cohort_firm_membership.csv")
HHI_COHORT_COVERAGE_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_cohort_treated_coverage.csv")
HHI_SUPPORT_EXCLUSIONS_CSV <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_industry_support_exclusions.csv"
)
HHI_ATT_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_heterogeneity_att_preferred.csv")
HHI_DIFFERENCE_CSV <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_heterogeneity_high_minus_low.csv"
)
HHI_COHORT_EFFECT_CSV <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_heterogeneity_cohort_effects.csv"
)
HHI_CELL_AUDIT_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_att_gt_cells.csv")
HHI_COVARIATE_AUDIT_CSV <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_pre_treatment_covariate_audit.csv"
)
HHI_SAMPLE_AUDIT_CSV <- file.path(HHI_OUTPUT_DIR, "hhi_sample_audit.csv")
HHI_WALD_CSV <- file.path(HHI_OUTPUT_ROOT, "hhi_heterogeneity_wald_pretrends.csv")
HHI_WALD_FULL_CSV <- file.path(
  HHI_OUTPUT_ROOT,
  "hhi_heterogeneity_wald_pretrends_full.csv"
)
HHI_WALD_LATEX <- file.path(HHI_OUTPUT_ROOT, "hhi_heterogeneity_wald_pretrends.tex")
HHI_PUBLICATION_CSV <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_heterogeneity_publication_table.csv"
)
HHI_PUBLICATION_MD <- file.path(
  HHI_OUTPUT_DIR,
  "hhi_heterogeneity_publication_table.md"
)
HHI_PANEL_RDS <- file.path(HHI_OUTPUT_DIR, "panel_analysis_with_hhi.rds")
HHI_RESULTS_RDS <- file.path(HHI_OUTPUT_DIR, "hhi_heterogeneity_results.rds")

FULL_PANEL_RDS <- file.path(INPUT_DIR, "compustat_ai_panel.rds")
SAVE_HHI_OUTPUTS <- isTRUE(get0("SAVE_HHI_OUTPUTS", ifnotfound = TRUE))

HHI_NAICS_DIGITS <- as.integer(get0(
  "HHI_NAICS_DIGITS_OVERRIDE",
  ifnotfound = 3L
))
HHI_COMPETITION_CUTOFF <- 1800
HHI_REGIMES <- c("high_competition", "low_competition")
HHI_REGIME_LABELS <- c(
  high_competition = "High competition",
  low_competition = "Low competition"
)
HHI_CLUSTER_VAR <- "hhi_cluster_market"
HHI_CLUSTER_LABEL <- paste0("first-observed NAICS", HHI_NAICS_DIGITS, " market")

MIN_TREATED_COHORT <- as.integer(Sys.getenv(
  "HHI_MIN_TREATED_COHORT", unset = as.character(MIN_DID_TREATED_COHORT)
))
DID_EST_METHOD <- "dr"
CONTROL_GROUP <- "notyettreated"
DID_BASE_PERIOD <- "varying"
ALLOW_UNBALANCED_PANEL <- TRUE
BEST_DID_SPEC_NAME <- "main_controls_naics2"
DID_BITERS <- as.integer(get0(
  "HHI_DID_BITERS",
  ifnotfound = get0("DID_BITERS", ifnotfound = 1000L)
))
DID_BOOTSTRAP_SEED <- as.integer(get0(
  "HHI_DID_BOOTSTRAP_SEED",
  ifnotfound = 123L
))
DID_OVERLAP_THRESHOLD <- 0.995
DID_MIN_CELL_SIZE <- 5L

OUTCOME_LABELS <- c(
  log_emp = "Employment (log)",
  log_labor_productivity = "Labour productivity (log)",
  log_sale = "Sales (log)",
  log_xopr = "Operating costs (log)",
  operating_profitability_w = "Operating profitability (OIBDP/total assets)"
)
DID_OUTCOMES <- names(OUTCOME_LABELS)
WALD_OUTCOME_LABELS <- c(
  log_emp = "Employment",
  log_xopr = "Operating costs",
  log_labor_productivity = "Labour productivity",
  log_sale = "Sales",
  operating_profitability_w = "Operating profitability"
)
WALD_PRETREND_EVENTS <- -4L:-1L

FIRM_DID_CONTROLS <- c(
  "log_at",
  "roa",
  "cash_ratio",
  "rd_reporter",
  "firm_age",
  "capx_intensity_y_w"
)
PROFITABILITY_DID_CONTROLS <- setdiff(FIRM_DID_CONTROLS, "roa")

TREATMENT_DEFINITIONS <- tribble(
  ~treatment_id, ~treatment_label, ~cohort_var,
  "ai_adoption_2", "AI adoption >= 2",
  Sys.getenv("HHI_2_COHORT_VAR", unset = "ai_adoption_year"),
  "ai_adoption_3", "Strong AI adoption = 3",
  Sys.getenv("HHI_3_COHORT_VAR", unset = "ai_adoption_3_year")
)

stopifnot(
  ANALYSIS_START_YEAR < MIN_TREATED_COHORT,
  DID_EST_METHOD == "dr",
  CONTROL_GROUP == "notyettreated",
  DID_BASE_PERIOD == "varying",
  ALLOW_UNBALANCED_PANEL,
  BEST_DID_SPEC_NAME == "main_controls_naics2",
  all(DID_OUTCOMES %in% names(OUTCOME_LABELS)),
  DID_BITERS >= 2L
)


# ---- General helpers ---------------------------------------------------------
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

resolve_controls <- function(outcome) {
  firm_controls <- if (identical(outcome, "operating_profitability_w")) {
    PROFITABILITY_DID_CONTROLS
  } else {
    FIRM_DID_CONTROLS
  }
  c(firm_controls, "naics2_f")
}

classify_competition <- function(hhi) {
  case_when(
    !is.finite(hhi) ~ NA_character_,
    hhi <= HHI_COMPETITION_CUTOFF ~ "high_competition",
    TRUE ~ "low_competition"
  )
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
  sub("^-0\\.000$", "0.000", sprintf("%.3f", value))
}

format_count <- function(value) {
  format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
}


# ---- Construct annual HHI regimes for cohort g-1 baselines -------------------
if (!file.exists(FULL_PANEL_RDS)) {
  stop("Full merged Compustat panel not found: ", FULL_PANEL_RDS)
}
if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop("Final analysis panel not found: ", ANALYSIS_PANEL_RDS)
}

full_panel <- readRDS(FULL_PANEL_RDS) |>
  as_tibble() |>
  mutate(cik = normalize_cik(cik), year = as.integer(year))
analysis_panel <- readRDS(ANALYSIS_PANEL_RDS) |>
  as_tibble() |>
  mutate(cik = normalize_cik(cik), year = as.integer(year))
if (!"ai_score" %in% names(analysis_panel) ||
    anyNA(analysis_panel$ai_score)) {
  stop("HHI DID source panel must contain only matched annual AI-score years.")
}

required_full <- c("cik", "year", "sale")
required_analysis <- unique(c(
  "cik", "year", "naics2", DID_OUTCOMES, FIRM_DID_CONTROLS,
  TREATMENT_DEFINITIONS$cohort_var
))
missing_full <- setdiff(required_full, names(full_panel))
missing_analysis <- setdiff(required_analysis, names(analysis_panel))
if (length(missing_full) > 0L) {
  stop("Full panel is missing: ", paste(missing_full, collapse = ", "))
}
if (length(missing_analysis) > 0L) {
  stop("Analysis panel is missing: ", paste(missing_analysis, collapse = ", "))
}

assert_unique_keys(full_panel, c("cik", "year"), "Full merged panel")
assert_unique_keys(analysis_panel, c("cik", "year"), "Analysis panel")

preferred_naics_column <- paste0("naics", HHI_NAICS_DIGITS)
if (preferred_naics_column %in% names(full_panel)) {
  full_panel <- full_panel |>
    mutate(hhi_market = as.character(.data[[preferred_naics_column]]))
} else if ("naics" %in% names(full_panel)) {
  full_panel <- full_panel |>
    mutate(hhi_market = substr(as.character(naics), 1L, HHI_NAICS_DIGITS))
} else {
  stop(
    "Full panel requires `", preferred_naics_column,
    "` or raw `naics` to construct HHI."
  )
}

full_panel <- full_panel |>
  mutate(
    hhi_market = if_else(
      is.na(hhi_market) | hhi_market %in% c("", "NA"),
      NA_character_,
      hhi_market
    )
  )

hhi_source <- full_panel |>
  filter(
    year >= ANALYSIS_START_YEAR,
    year <= ANALYSIS_END_YEAR,
    !is.na(cik),
    !is.na(hhi_market),
    is.finite(sale),
    sale > 0
  ) |>
  select(cik, year, hhi_market, sale)
assert_unique_keys(hhi_source, c("cik", "year"), "HHI source panel")

hhi_market_year <- hhi_source |>
  group_by(hhi_market, year) |>
  summarise(
    n_firms = n_distinct(cik),
    market_sales = sum(sale),
    hhi = sum((100 * sale / market_sales)^2),
    .groups = "drop"
  ) |>
  arrange(year, hhi_market)

if (any(
  !is.finite(hhi_market_year$hhi) |
    hhi_market_year$hhi <= 0 |
    hhi_market_year$hhi > 10000 + 1e-8
)) {
  stop("Constructed HHI contains values outside the valid 0-10,000 range.")
}

firm_hhi_regime <- full_panel |>
  filter(
    year >= ANALYSIS_START_YEAR,
    year < ANALYSIS_END_YEAR,
    !is.na(cik)
  ) |>
  select(cik, baseline_year = year, baseline_hhi_market = hhi_market) |>
  left_join(
    hhi_market_year |>
      transmute(
        baseline_year = year,
        baseline_hhi_market = hhi_market,
        hhi_base = hhi,
        hhi_base_n_firms = n_firms,
        hhi_base_market_sales = market_sales
      ),
    by = c("baseline_year", "baseline_hhi_market")
  ) |>
  mutate(competition_regime = classify_competition(hhi_base)) |>
  arrange(baseline_year, cik)
assert_unique_keys(firm_hhi_regime, c("cik", "baseline_year"), "Annual firm HHI regime")

# A stable cluster identifier permits the same firm and market to contribute
# to multiple cohort comparisons and, potentially, both regimes.
firm_cluster_market <- full_panel |>
  filter(!is.na(cik), !is.na(hhi_market)) |>
  arrange(cik, year) |>
  group_by(cik) |>
  summarise(hhi_cluster_market = first(hhi_market), .groups = "drop")
assert_unique_keys(firm_cluster_market, "cik", "Firm cluster market")

analysis_hhi <- analysis_panel |>
  left_join(
    select(firm_hhi_regime, -hhi_base_market_sales),
    by = c("cik", "year" = "baseline_year")
  ) |>
  left_join(firm_cluster_market, by = "cik") |>
  mutate(
    competition_regime = factor(competition_regime, levels = HHI_REGIMES),
    naics2_f = factor(as.character(naics2))
  ) |>
  arrange(cik, year) |>
  group_by(cik) |>
  mutate(firm_id = cur_group_id()) |>
  ungroup()

regime_summary <- firm_hhi_regime |>
  filter(!is.na(competition_regime)) |>
  group_by(baseline_year, competition_regime) |>
  summarise(
    firms = n_distinct(cik),
    markets = n_distinct(baseline_hhi_market),
    mean_hhi = mean(hhi_base),
    median_hhi = median(hhi_base),
    median_market_firms = median(hhi_base_n_firms),
    .groups = "drop"
  ) |>
  arrange(match(competition_regime, HHI_REGIMES))

hhi_coverage <- analysis_hhi |>
  filter(year < ANALYSIS_END_YEAR) |>
  group_by(baseline_year = year) |>
  summarise(
    analysis_firms = n_distinct(cik),
    classified_firms = sum(!is.na(competition_regime)),
    unclassified_firms = sum(is.na(competition_regime)),
    classified_pct = 100 * classified_firms / analysis_firms,
    .groups = "drop"
  )

cohort_membership <- map_dfr(seq_len(nrow(TREATMENT_DEFINITIONS)), function(i) {
  definition <- TREATMENT_DEFINITIONS[i, ]
  analysis_hhi |>
    filter(year >= MIN_TREATED_COHORT - 1L,
           year < ANALYSIS_END_YEAR) |>
    transmute(
      treatment_id = definition$treatment_id,
      cohort_year = year + 1L,
      baseline_year = year,
      firm_id,
      cik,
      own_cohort = as.integer(.data[[definition$cohort_var]]),
      baseline_hhi_market,
      hhi_base,
      competition_regime = as.character(competition_regime),
      eligible_treated = own_cohort == cohort_year,
      potentially_eligible_control = own_cohort == 0L |
        own_cohort > cohort_year
    )
})
assert_unique_keys(
  cohort_membership,
  c("treatment_id", "cohort_year", "firm_id"),
  "Cohort HHI membership"
)
cohort_treated_coverage <- cohort_membership |>
  filter(eligible_treated) |>
  group_by(treatment_id, cohort_year, competition_regime) |>
  summarise(
    treated_firms_with_matched_baseline = n_distinct(firm_id),
    markets = n_distinct(baseline_hhi_market),
    .groups = "drop"
  )


# ---- DiD sample and industry-support helpers --------------------------------
build_regime_sample <- function(data, cohort_var) {
  data |>
    mutate(cohort = as.integer(.data[[cohort_var]])) |>
    filter(
      !is.na(year),
      !is.na(cohort),
      cohort == 0L | cohort >= MIN_TREATED_COHORT,
      !is.na(.data[[HHI_CLUSTER_VAR]])
    ) |>
    mutate(naics2_f = factor(as.character(naics2_f))) |>
    arrange(firm_id, year)
}

filter_cohort_regime <- function(eligible, data, cohort_year, target_year, regime) {
  baseline_ids <- data |>
    filter(
      year == cohort_year - 1L,
      as.character(competition_regime) == regime
    ) |>
    select(firm_id)
  eligible |>
    semi_join(baseline_ids, by = "firm_id") |>
    # For placebo cells, g-1 HHI must also precede a control's own adoption.
    filter(cohort == cohort_year | cohort == 0L |
             cohort > max(target_year, cohort_year - 1L))
}

# NAICS2 adjustment requires at least one eligible comparison firm in every
# cohort-time cell. A treated firm in an unsupported baseline industry is
# removed from that treatment/regime sample for all event times. This preserves
# a stable treated composition and is recorded explicitly below.
identify_industry_support_exclusions <- function(data, treatment_id, regime) {
  firm_meta <- data |>
    distinct(firm_id, cik, cohort)
  cohorts <- sort(unique(firm_meta$cohort[firm_meta$cohort >= MIN_TREATED_COHORT]))
  targets <- sort(unique(data$year))
  targets <- targets[targets > min(targets)]

  raw_exclusions <- map_dfr(cohorts, function(g) {
    baseline <- data |>
      filter(
        year == g - 1L,
        as.character(competition_regime) == regime
      ) |>
      transmute(
        firm_id, cik, cohort,
        baseline_naics2 = as.character(naics2_f)
      )

    treated <- baseline |>
      filter(cohort == g, !is.na(baseline_naics2))

    if (nrow(treated) == 0L) {
      return(tibble())
    }

    map_dfr(targets, function(target_year) {
      supported_sectors <- baseline |>
        filter(
          cohort != g,
          cohort == 0L | cohort > max(target_year, g - 1L),
          !is.na(baseline_naics2)
        ) |>
        distinct(baseline_naics2) |>
        pull(baseline_naics2)

      unsupported <- treated |>
        filter(!baseline_naics2 %in% supported_sectors) |>
        select(firm_id, cik, cohort, baseline_naics2)

      if (nrow(unsupported) == 0L) {
        return(tibble())
      }

      unsupported |>
        transmute(
          treatment_id,
          competition_regime = regime,
          cohort_year = g,
          firm_id,
          cik,
          baseline_year = g - 1L,
          baseline_naics2,
          unsupported_target_year = target_year
        )
    })
  })

  if (nrow(raw_exclusions) == 0L) {
    return(tibble(
      treatment_id = character(),
      competition_regime = character(),
      cohort_year = integer(),
      firm_id = integer(),
      cik = character(),
      baseline_year = integer(),
      baseline_naics2 = character(),
      first_unsupported_target_year = integer(),
      last_unsupported_target_year = integer(),
      unsupported_cells = integer(),
      exclusion_reason = character()
    ))
  }

  raw_exclusions |>
    group_by(
      treatment_id, competition_regime, cohort_year, firm_id, cik,
      baseline_year, baseline_naics2
    ) |>
    summarise(
      first_unsupported_target_year = min(unsupported_target_year),
      last_unsupported_target_year = max(unsupported_target_year),
      unsupported_cells = n_distinct(unsupported_target_year),
      exclusion_reason = paste0(
        "No eligible not-yet-treated comparison firm in baseline NAICS2 ",
        "for at least one cohort-time cell"
      ),
      .groups = "drop"
    )
}


# ---- One treatment/outcome/regime model -------------------------------------
run_hhi_model <- function(
    sample,
    treatment_id,
    treatment_label,
    cohort_var,
    outcome,
    regime,
    bootstrap_seed
) {
  controls <- resolve_controls(outcome)
  required <- unique(c(
    "firm_id", "year", "cohort", "competition_regime",
    HHI_CLUSTER_VAR, outcome, controls
  ))
  missing <- setdiff(required, names(sample))
  if (length(missing) > 0L) {
    stop("HHI DiD sample is missing: ", paste(missing, collapse = ", "))
  }

  model_sample <- sample |>
    select(all_of(required))
  input_n_obs <- nrow(model_sample)
  input_n_firms <- n_distinct(model_sample$firm_id)

  set.seed(bootstrap_seed)
  custom <- did_build_gminus1_mp(
    data = model_sample,
    outcome = outcome,
    controls = controls,
    est_method = DID_EST_METHOD,
    control_group = CONTROL_GROUP,
    base_period = DID_BASE_PERIOD,
    min_treated_cohort = MIN_TREATED_COHORT,
    bstrap = TRUE,
    biters = DID_BITERS,
    cband = TRUE,
    min_cell_size = DID_MIN_CELL_SIZE,
    overlap_threshold = DID_OVERLAP_THRESHOLD,
    cluster_var = HHI_CLUSTER_VAR,
    allow_missing_pretrend = TRUE,
    eligible_filter = function(eligible, data, cohort_year, target_year) {
      filter_cohort_regime(
        eligible, data, cohort_year, target_year, regime
      )
    }
  )

  failed_cells <- custom$cells |>
    filter(!estimable, target_year >= cohort_year)
  if (nrow(failed_cells) > 0L) {
    first_failure <- failed_cells[1L, ]
    stop(
      treatment_id, "/", outcome, "/", regime, ": ",
      nrow(failed_cells), " post-treatment ATT(g,t) cells failed after the documented ",
      "industry-support restriction. First failure: g=",
      first_failure$cohort_year, ", t=", first_failure$target_year, ", ",
      first_failure$support_warning
    )
  }

  set.seed(bootstrap_seed)
  overall <- did::aggte(
    custom$mp,
    type = "group",
    na.rm = TRUE,
    bstrap = TRUE,
    biters = DID_BITERS,
    cband = TRUE,
    clustervars = c("firm_id", HHI_CLUSTER_VAR)
  )

  estimate <- as.numeric(overall$overall.att)
  std_error <- as.numeric(overall$overall.se)
  p_value <- if (
    is.finite(estimate) && is.finite(std_error) && std_error > 0
  ) {
    2 * pnorm(-abs(estimate / std_error))
  } else {
    NA_real_
  }

  contributor_meta <- custom$firm_meta

  att_row <- tibble(
    treatment_id,
    treatment_label,
    cohort_var,
    outcome,
    outcome_label = unname(OUTCOME_LABELS[[outcome]]),
    competition_regime = regime,
    competition_label = unname(HHI_REGIME_LABELS[[regime]]),
    spec_name = BEST_DID_SPEC_NAME,
    spec_label = "Firm + industry controls",
    controls = paste(controls, collapse = ", "),
    estimate,
    std_error,
    p_value,
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    n_obs = custom$n_obs,
    n_firms = custom$n_firms,
    n_clusters = custom$n_clusters,
    cluster_variable = HHI_CLUSTER_VAR,
    inference = paste0(HHI_CLUSTER_LABEL, " multiplier bootstrap"),
    n_treated_firms = n_distinct(
      contributor_meta$firm_id[
        contributor_meta$aggregation_cohort >= MIN_TREATED_COHORT
      ]
    ),
    n_never_treated_firms = n_distinct(
      contributor_meta$firm_id[contributor_meta$cohort == 0L]
    ),
    estimable_att_gt = sum(custom$cells$estimable),
    overlap_warning_cells = sum(custom$cells$support_status == "WARNING")
  )

  cohort_rows <- tibble(
    treatment_id,
    treatment_label,
    outcome,
    outcome_label = unname(OUTCOME_LABELS[[outcome]]),
    competition_regime = regime,
    competition_label = unname(HHI_REGIME_LABELS[[regime]]),
    cohort_year = as.integer(overall$egt),
    estimate = as.numeric(overall$att.egt),
    std_error = as.numeric(overall$se.egt)
  ) |>
    mutate(
      p_value = if_else(
        is.finite(std_error) & std_error > 0,
        2 * pnorm(-abs(estimate / std_error)),
        NA_real_
      ),
      ci_low = estimate - 1.96 * std_error,
      ci_high = estimate + 1.96 * std_error
    )

  cell_rows <- custom$cells |>
    mutate(
      treatment_id,
      treatment_label,
      outcome,
      competition_regime = regime,
      controls = paste(controls, collapse = ", "),
      .before = 1L
    )

  covariate_audit <- cell_rows |>
    transmute(
      treatment_id,
      outcome,
      competition_regime,
      cohort_year,
      target_year,
      baseline_year,
      treated_covariate_year,
      control_covariate_year,
      latest_covariate_year,
      post_treatment_covariate_rows,
      timing_pass =
        treated_covariate_year == cohort_year - 1L &
        control_covariate_year == cohort_year - 1L &
        latest_covariate_year == cohort_year - 1L &
        post_treatment_covariate_rows == 0L
    )

  sample_audit <- tibble(
    treatment_id,
    outcome,
    competition_regime = regime,
    input_n_obs,
    estimation_n_obs = custom$n_obs,
    observations_not_used = input_n_obs - custom$n_obs,
    input_n_firms,
    estimation_n_firms = custom$n_firms,
    firms_not_used = input_n_firms - custom$n_firms,
    estimation_n_clusters = custom$n_clusters,
    cluster_variable = HHI_CLUSTER_VAR,
    sample_definition = paste0(
      "Unique outcome firm-years contributing to at least one ATT(g,t); ",
      "missingness handled cell by cell"
    )
  )

  full_pretrend <- custom$full_pretrend
  common_pretrend <- custom$common_pretrend
  wald_full_row <- tibble(
    treatment_id,
    outcome,
    competition_regime = regime,
    wald_statistic = full_pretrend$statistic,
    df = full_pretrend$df,
    p_value = full_pretrend$p_value
  )
  wald_row <- tibble(
    treatment_id,
    outcome,
    competition_regime = regime,
    wald_statistic = common_pretrend$statistic,
    df = common_pretrend$df,
    p_value = common_pretrend$p_value
  )
  wald_full_coefficients <- full_pretrend$coefficients |>
    mutate(
      treatment_id,
      outcome,
      competition_regime = regime,
      .before = 1L
    )
  wald_coefficients <- common_pretrend$coefficients |>
    mutate(
      treatment_id,
      outcome,
      competition_regime = regime,
      .before = 1L
    )

  cat(
    "Full-window aggregated pre-trend coefficients:", treatment_id,
    "|", outcome, "|", regime, "\n"
  )
  print(wald_full_coefficients |> select(event_time, estimate, std_error))
  if (full_pretrend$rank_deficient) {
    cat(
      "Rank-deficient full-window covariance: rank",
      full_pretrend$covariance_rank, "of",
      full_pretrend$restrictions, "restrictions.\n"
    )
  }
  cat(
    "Common-window aggregated pre-trend coefficients:", treatment_id,
    "|", outcome, "|", regime, "\n"
  )
  print(wald_coefficients |> select(event_time, estimate, std_error))
  if (common_pretrend$rank_deficient) {
    cat(
      "Rank-deficient pre-trend covariance: rank",
      common_pretrend$covariance_rank, "of",
      common_pretrend$restrictions, "restrictions.\n"
    )
  }

  list(
    model = custom$mp,
    overall = overall,
    att_row = att_row,
    cohort_rows = cohort_rows,
    cell_rows = cell_rows,
    covariate_audit = covariate_audit,
    sample_audit = sample_audit,
    wald_full_row = wald_full_row,
    wald_full_coefficients = wald_full_coefficients,
    wald_row = wald_row,
    wald_coefficients = wald_coefficients
  )
}


# ---- Estimate the preferred specification by competition regime -------------
model_grid <- crossing(
  treatment_id = TREATMENT_DEFINITIONS$treatment_id,
  outcome = DID_OUTCOMES,
  competition_regime = HHI_REGIMES
) |>
  left_join(TREATMENT_DEFINITIONS, by = "treatment_id") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, DID_OUTCOMES),
    match(competition_regime, HHI_REGIMES)
  ) |>
  mutate(bootstrap_seed = DID_BOOTSTRAP_SEED + row_number() - 1L)

prepared_samples <- list()
support_exclusions <- list()

for (treatment_index in seq_len(nrow(TREATMENT_DEFINITIONS))) {
  definition <- TREATMENT_DEFINITIONS[treatment_index, ]
  for (regime in HHI_REGIMES) {
    key <- paste(definition$treatment_id, regime, sep = "::")
    sample <- build_regime_sample(analysis_hhi, definition$cohort_var)
    exclusions <- identify_industry_support_exclusions(
      sample,
      definition$treatment_id,
      regime
    )
    excluded_ids <- unique(exclusions$firm_id)
    if (length(excluded_ids) > 0L) {
      sample <- sample |>
        filter(!firm_id %in% excluded_ids)
    }
    prepared_samples[[key]] <- sample
    support_exclusions[[key]] <- exclusions
  }
}

support_exclusion_table <- bind_rows(support_exclusions) |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(competition_regime, HHI_REGIMES),
    cohort_year,
    firm_id
  )

model_results <- pmap(
  model_grid,
  function(
      treatment_id,
      outcome,
      competition_regime,
      treatment_label,
      cohort_var,
      bootstrap_seed
  ) {
    cat(
      "\nEstimating HHI heterogeneity:", treatment_id,
      "|", outcome,
      "|", competition_regime,
      "| cohort-gminus1", toupper(DID_EST_METHOD), "\n"
    )
    key <- paste(treatment_id, competition_regime, sep = "::")
    run_hhi_model(
      sample = prepared_samples[[key]],
      treatment_id = treatment_id,
      treatment_label = treatment_label,
      cohort_var = cohort_var,
      outcome = outcome,
      regime = competition_regime,
      bootstrap_seed = bootstrap_seed
    )
  }
)

att_results <- map_dfr(model_results, "att_row") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, DID_OUTCOMES),
    match(competition_regime, HHI_REGIMES)
  )
cohort_effects <- map_dfr(model_results, "cohort_rows")
cell_audit <- map_dfr(model_results, "cell_rows")
covariate_audit <- map_dfr(model_results, "covariate_audit")
sample_audit <- map_dfr(model_results, "sample_audit")
wald_results <- map_dfr(model_results, "wald_row") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, names(WALD_OUTCOME_LABELS)),
    match(competition_regime, HHI_REGIMES)
  )
wald_full_results <- map_dfr(model_results, "wald_full_row") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, names(WALD_OUTCOME_LABELS)),
    match(competition_regime, HHI_REGIMES)
  )
wald_coefficients <- map_dfr(model_results, "wald_coefficients") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, names(WALD_OUTCOME_LABELS)),
    match(competition_regime, HHI_REGIMES),
    event_time
  )
wald_full_coefficients <- map_dfr(model_results, "wald_full_coefficients") |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, names(WALD_OUTCOME_LABELS)),
    match(competition_regime, HHI_REGIMES),
    event_time
  )

if (any(!covariate_audit$timing_pass)) {
  stop("HHI QA failed: at least one model did not use the common cohort g-1 baseline.")
}
if (any(att_results$n_obs > nrow(analysis_panel))) {
  stop("HHI QA failed: a subgroup exceeds the matched descriptive sample.")
}
if (any(!cell_audit$estimable &
        cell_audit$target_year >= cell_audit$cohort_year)) {
  stop("HHI QA failed: non-estimable post-treatment ATT(g,t) cells reached the output stage.")
}
if (anyDuplicated(att_results[c(
  "treatment_id", "outcome", "competition_regime"
)])) {
  stop("HHI QA failed: duplicate subgroup ATT rows.")
}
if (
  any(att_results$cluster_variable != HHI_CLUSTER_VAR) ||
  any(att_results$n_clusters < 2L)
) {
  stop(
    "HHI QA failed: subgroup inference is not clustered by ",
    HHI_CLUSTER_LABEL, "."
  )
}
expected_wald_tests <- nrow(TREATMENT_DEFINITIONS) *
  length(DID_OUTCOMES) * length(HHI_REGIMES)
expected_wald_coefficients <- crossing(
  treatment_id = TREATMENT_DEFINITIONS$treatment_id,
  outcome = names(WALD_OUTCOME_LABELS),
  competition_regime = HHI_REGIMES,
  event_time = WALD_PRETREND_EVENTS
)
missing_wald_coefficients <- anti_join(
  expected_wald_coefficients,
  wald_coefficients,
  by = c("treatment_id", "outcome", "competition_regime", "event_time")
)
if (
  nrow(wald_results) != expected_wald_tests ||
    anyDuplicated(wald_results[c(
      "treatment_id", "outcome", "competition_regime"
    )]) ||
    nrow(wald_coefficients) != nrow(expected_wald_coefficients) ||
    nrow(missing_wald_coefficients) > 0L
) {
  stop(
    "HHI QA failed: each model must contribute one Wald test and exactly ",
    "four aggregated coefficients for event times -4:-1."
  )
}
if (any(wald_results$df != length(WALD_PRETREND_EVENTS))) {
  rank_deficient_tests <- wald_results |>
    filter(df != length(WALD_PRETREND_EVENTS))
  warning(
    "Rank-deficient aggregated pre-trend covariance in ",
    nrow(rank_deficient_tests), " model(s); the reported df is the actual rank."
  )
  print(rank_deficient_tests)
}
full_restriction_counts <- wald_full_coefficients |>
  group_by(treatment_id, outcome, competition_regime) |>
  summarise(
    restrictions = n(),
    min_event_time = min(event_time),
    max_event_time = max(event_time),
    .groups = "drop"
  )
full_wald_validation <- wald_full_results |>
  left_join(
    full_restriction_counts,
    by = c("treatment_id", "outcome", "competition_regime")
  )
if (
  nrow(wald_full_results) != expected_wald_tests ||
    anyDuplicated(wald_full_results[c(
      "treatment_id", "outcome", "competition_regime"
    )]) ||
    anyNA(full_wald_validation$restrictions) ||
    any(full_wald_validation$max_event_time != -1L) ||
    any(full_wald_validation$df > full_wald_validation$restrictions)
) {
  stop(
    "HHI QA failed: full-window tests must use every available aggregated ",
    "negative event-time coefficient through e=-1."
  )
}
if (any(full_wald_validation$df != full_wald_validation$restrictions)) {
  rank_deficient_full_tests <- full_wald_validation |>
    filter(df != restrictions)
  warning(
    "Rank-deficient full-window aggregated pre-trend covariance in ",
    nrow(rank_deficient_full_tests),
    " model(s); the reported df is the actual rank."
  )
  print(rank_deficient_full_tests)
}


# ---- Formal high-minus-low ATT contrast -------------------------------------
high_results <- att_results |>
  filter(competition_regime == "high_competition") |>
  select(
    treatment_id, treatment_label, outcome, outcome_label,
    high_att = estimate,
    high_se = std_error,
    high_n_obs = n_obs,
    high_n_firms = n_firms,
    high_n_clusters = n_clusters,
    high_n_treated_firms = n_treated_firms
  )
low_results <- att_results |>
  filter(competition_regime == "low_competition") |>
  select(
    treatment_id, outcome,
    low_att = estimate,
    low_se = std_error,
    low_n_obs = n_obs,
    low_n_firms = n_firms,
    low_n_clusters = n_clusters,
    low_n_treated_firms = n_treated_firms
  )

model_lookup <- set_names(
  model_results,
  paste(
    model_grid$treatment_id,
    model_grid$outcome,
    model_grid$competition_regime,
    sep = "::"
  )
)

cluster_multiplier_contrast <- function(
    high_model,
    low_model,
    estimate_difference,
    bootstrap_seed
) {
  high_if <- as.numeric(
    high_model$overall$inf.function$selective.inf.func
  )
  low_if <- as.numeric(
    low_model$overall$inf.function$selective.inf.func
  )
  high_cluster <- as.character(
    high_model$overall$DIDparams$cluster_vector
  )
  low_cluster <- as.character(
    low_model$overall$DIDparams$cluster_vector
  )
  high_id <- high_model$overall$DIDparams$data$firm_id
  low_id <- low_model$overall$DIDparams$data$firm_id

  if (
    length(high_if) != length(high_cluster) ||
    length(low_if) != length(low_cluster) ||
    length(high_id) != length(high_if) ||
    length(low_id) != length(low_if) ||
    anyDuplicated(high_id) || anyDuplicated(low_id) ||
    anyNA(high_cluster) ||
    anyNA(low_cluster)
  ) {
    stop(
      "HHI contrast bootstrap is missing aligned firm and market metadata."
    )
  }

  high_n <- length(high_if)
  low_n <- length(low_if)
  aligned <- full_join(
    tibble(firm_id = high_id, high_if, high_cluster),
    tibble(firm_id = low_id, low_if, low_cluster),
    by = "firm_id"
  )
  inconsistent_market <- aligned |>
    filter(!is.na(high_cluster), !is.na(low_cluster),
           high_cluster != low_cluster)
  if (nrow(inconsistent_market) > 0L) {
    stop("A firm has different cluster markets across competition regimes.")
  }
  combined_n <- nrow(aligned)
  combined_if <- (combined_n / high_n) * coalesce(aligned$high_if, 0) -
    (combined_n / low_n) * coalesce(aligned$low_if, 0)
  combined_cluster <- coalesce(aligned$high_cluster, aligned$low_cluster)
  n_clusters <- n_distinct(combined_cluster)

  bootstrap_params <- high_model$overall$DIDparams
  bootstrap_params$cluster_vector <- combined_cluster
  bootstrap_params$cluster_vector_var <- HHI_CLUSTER_VAR
  bootstrap_params$clustervars <- c("firm_id", HHI_CLUSTER_VAR)
  bootstrap_params$bstrap <- TRUE
  bootstrap_params$biters <- DID_BITERS
  bootstrap_params$cband <- FALSE

  set.seed(bootstrap_seed)
  bootstrap <- getFromNamespace("mboot", "did")(
    matrix(combined_if, ncol = 1L),
    bootstrap_params,
    return_V = FALSE
  )
  std_error <- as.numeric(bootstrap$se[[1L]])

  # mboot stores sqrt(G)-scaled cluster bootstrap draws. Convert them back
  # to draws of the high-minus-low estimator before computing its empirical
  # pointwise test and confidence interval.
  bootstrap_errors <- as.numeric(bootstrap$bres[, 1L]) *
    sqrt(n_clusters) / combined_n
  p_value <- (
    1 + sum(abs(bootstrap_errors) >= abs(estimate_difference))
  ) / (length(bootstrap_errors) + 1)
  critical_value <- as.numeric(
    quantile(
      abs(bootstrap_errors),
      probs = 1 - bootstrap_params$alp,
      type = 1L,
      names = FALSE
    )
  )

  tibble(
    std_error_difference = std_error,
    standardized_statistic = estimate_difference / std_error,
    p_value_difference = p_value,
    ci_low_difference = estimate_difference - critical_value,
    ci_high_difference = estimate_difference + critical_value,
    bootstrap_critical_value = critical_value,
    contrast_clusters = n_clusters,
    bootstrap_iterations = DID_BITERS,
    inference = paste0(
      "Joint ", HHI_CLUSTER_LABEL,
      " multiplier bootstrap; shared controls aligned by firm"
    )
  )
}

difference_results <- high_results |>
  inner_join(low_results, by = c("treatment_id", "outcome")) |>
  mutate(
    contrast = "High competition - Low competition",
    estimate_difference = high_att - low_att
  )

contrast_inference <- difference_results |>
  select(treatment_id, outcome, estimate_difference) |>
  mutate(bootstrap_seed = DID_BOOTSTRAP_SEED + 10000L + row_number()) |>
  pmap_dfr(function(
      treatment_id,
      outcome,
      estimate_difference,
      bootstrap_seed
  ) {
    high_key <- paste(
      treatment_id, outcome, "high_competition", sep = "::"
    )
    low_key <- paste(
      treatment_id, outcome, "low_competition", sep = "::"
    )
    cluster_multiplier_contrast(
      high_model = model_lookup[[high_key]],
      low_model = model_lookup[[low_key]],
      estimate_difference = estimate_difference,
      bootstrap_seed = bootstrap_seed
    ) |>
      mutate(treatment_id, outcome, .before = 1L)
  })

difference_results <- difference_results |>
  left_join(
    contrast_inference,
    by = c("treatment_id", "outcome")
  ) |>
  arrange(
    match(treatment_id, TREATMENT_DEFINITIONS$treatment_id),
    match(outcome, DID_OUTCOMES)
  )

expected_contrasts <- nrow(TREATMENT_DEFINITIONS) * length(DID_OUTCOMES)
if (nrow(difference_results) != expected_contrasts) {
  stop("HHI QA failed: not every treatment/outcome has both competition regimes.")
}


# ---- Publication table -------------------------------------------------------
publication_table <- difference_results |>
  mutate(
    `High competition ATT` = paste0(
      format_estimate(high_att), significance_stars(2 * pnorm(-abs(high_att / high_se)))
    ),
    `High competition SE` = sprintf("(%.3f)", high_se),
    `Low competition ATT` = paste0(
      format_estimate(low_att), significance_stars(2 * pnorm(-abs(low_att / low_se)))
    ),
    `Low competition SE` = sprintf("(%.3f)", low_se),
    `Difference: high - low` = paste0(
      format_estimate(estimate_difference), significance_stars(p_value_difference)
    ),
    `Difference SE` = sprintf("(%.3f)", std_error_difference),
    `Difference p-value` = sprintf("%.3f", p_value_difference),
    `High N` = format_count(high_n_obs),
    `Low N` = format_count(low_n_obs),
    `High firms` = format_count(high_n_firms),
    `Low firms` = format_count(low_n_firms),
    `High market clusters` = format_count(high_n_clusters),
    `Low market clusters` = format_count(low_n_clusters)
  ) |>
  select(
    treatment_id, treatment_label, outcome, outcome_label,
    `High competition ATT`, `High competition SE`,
    `Low competition ATT`, `Low competition SE`,
    `Difference: high - low`, `Difference SE`, `Difference p-value`,
    `High N`, `Low N`, `High firms`, `Low firms`,
    `High market clusters`, `Low market clusters`
  )

markdown_row <- function(values) {
  values <- gsub("|", "\\|", as.character(values), fixed = TRUE)
  paste0("| ", paste(values, collapse = " | "), " |")
}

write_publication_markdown <- function(data, path) {
  display_columns <- c(
    "Outcome", "High competition", "Low competition",
    "High - low", "Difference p-value"
  )
  lines <- c("# HHI heterogeneity in AI-adoption effects", "")

  for (treatment in TREATMENT_DEFINITIONS$treatment_id) {
    treatment_label <- TREATMENT_DEFINITIONS |>
      filter(treatment_id == treatment) |>
      pull(treatment_label)
    table <- data |>
      filter(treatment_id == treatment) |>
      transmute(
        Outcome = outcome_label,
        `High competition` = paste0(
          `High competition ATT`, " ", `High competition SE`
        ),
        `Low competition` = paste0(
          `Low competition ATT`, " ", `Low competition SE`
        ),
        `High - low` = paste0(
          `Difference: high - low`, " ", `Difference SE`
        ),
        `Difference p-value`
      )
    lines <- c(
      lines,
      paste0("## ", treatment_label),
      "",
      markdown_row(display_columns),
      markdown_row(rep("---", length(display_columns))),
      apply(table, 1L, markdown_row),
      ""
    )
  }

  lines <- c(
    lines,
    paste0(
      "Notes: For each treatment cohort g, treated firms and eligible controls ",
      "are assigned using their own NAICS", HHI_NAICS_DIGITS,
      " market HHI in g-1, held fixed across that cohort's comparisons. High competition ",
      "is HHI <= ", format(HHI_COMPETITION_CUTOFF, big.mark = ","),
      "; low competition is HHI > ",
      format(HHI_COMPETITION_CUTOFF, big.mark = ","), ". "
    ),
    paste0(
      "Each subgroup uses the preferred unbalanced cohort-gminus1 doubly robust ",
      "specification with not-yet-treated controls, pre-treatment firm controls, ",
      "and NAICS2 controls. ROA is omitted for operating profitability. ",
      "Operating profitability is OIBDP divided by strictly positive total assets ",
      "and is pooled-winsorized at the 1st and 99th percentiles of the eligible ",
      "year/exchange sample. Standard ",
      "errors use a multiplier bootstrap clustered by firms' first-observed NAICS",
      HHI_NAICS_DIGITS, " markets. The difference is high-competition ATT minus low-competition ATT ",
      "and is tested in a joint NAICS", HHI_NAICS_DIGITS,
      " cluster multiplier bootstrap that preserves shared controls. ",
      "Each regime's overall ATT uses its own treated-cohort weights, so the ",
      "pooled difference also reflects cohort composition. Clustering ",
      "allows arbitrary dependence within a market but does not itself resolve ",
      "spillover-related interference. * p < 0.10; ** p < 0.05; *** p < 0.01."
    )
  )
  writeLines(lines, path, useBytes = TRUE)
}

format_wald_latex_p_value <- function(value) {
  if (!is.finite(value)) {
    "--"
  } else if (value < 0.001) {
    "$<0.001$"
  } else {
    sprintf("%.3f", value)
  }
}

write_wald_pretrends_latex <- function(data, path) {
  all_four_restrictions <- all(data$df == length(WALD_PRETREND_EVENTS))
  panel_labels <- c(
    ai_adoption_2 = "Panel A: AI adoption $\\geq 2$",
    ai_adoption_3 = "Panel B: Strong AI adoption $=3$"
  )
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Joint Wald tests of HHI-subgroup pre-treatment coefficients}",
    "\\label{tab:hhi-wald-pretrends}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    paste0(
      " & \\multicolumn{2}{c}{High competition}",
      " & \\multicolumn{2}{c}{Low competition} \\\\"
    ),
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Outcome & Wald & p-value & Wald & p-value \\\\",
    "\\midrule"
  )

  for (treatment in TREATMENT_DEFINITIONS$treatment_id) {
    lines <- c(
      lines,
      paste0(
        "\\multicolumn{5}{l}{\\textit{",
        panel_labels[[treatment]],
        "}} \\\\"
      )
    )
    for (outcome_name in names(WALD_OUTCOME_LABELS)) {
      outcome_rows <- data |>
        filter(treatment_id == treatment, outcome == outcome_name)
      high <- outcome_rows |>
        filter(competition_regime == "high_competition")
      low <- outcome_rows |>
        filter(competition_regime == "low_competition")
      if (nrow(high) != 1L || nrow(low) != 1L) {
        stop("LaTeX Wald table requires one row per treatment/outcome/regime.")
      }
      lines <- c(
        lines,
        paste0(
          WALD_OUTCOME_LABELS[[outcome_name]], " & ",
          sprintf("%.3f", high$wald_statistic), " & ",
          format_wald_latex_p_value(high$p_value), " & ",
          sprintf("%.3f", low$wald_statistic), " & ",
          format_wald_latex_p_value(low$p_value), " \\\\"
        )
      )
    }
    if (!identical(treatment, tail(TREATMENT_DEFINITIONS$treatment_id, 1L))) {
      lines <- c(lines, "\\addlinespace")
    }
  }

  restriction_note <- if (all_four_restrictions) {
    paste0(
      "Each test jointly imposes $ATT(e=-4)=ATT(e=-3)=ATT(e=-2)=",
      "ATT(e=-1)=0$ (df = 4)."
    )
  } else {
    paste0(
      "Degrees of freedom equal the actual rank of each four-coefficient ",
      "covariance matrix; see the accompanying CSV."
    )
  }
  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.96\\linewidth}",
    "\\footnotesize",
    paste0(
      "Notes: ", restriction_note, " Tests use aggregated dynamic ",
      "Callaway--Sant'Anna coefficients from the preferred conditional ",
      "specification and first-observed NAICS", HHI_NAICS_DIGITS,
      " market-clustered influence-function covariance matrices."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path, useBytes = TRUE)
}


# ---- Save --------------------------------------------------------------------
model_keys <- paste(
  model_grid$treatment_id,
  model_grid$outcome,
  model_grid$competition_regime,
  sep = "::"
)
named_models <- set_names(map(model_results, "model"), model_keys)

hhi_results <- list(
  design = list(
    hhi_baseline = "Each treatment cohort g uses its own g-1 HHI",
    hhi_market = paste0("NAICS", HHI_NAICS_DIGITS),
    hhi_cutoff = HHI_COMPETITION_CUTOFF,
    minimum_treated_cohort = MIN_TREATED_COHORT,
    estimator = DID_EST_METHOD,
    control_group = CONTROL_GROUP,
    base_period = DID_BASE_PERIOD,
    allow_unbalanced_panel = ALLOW_UNBALANCED_PANEL,
    specification = BEST_DID_SPEC_NAME,
    firm_controls = FIRM_DID_CONTROLS,
    operating_profitability_firm_controls = PROFITABILITY_DID_CONTROLS,
    operating_profitability_construction = paste0(
      "OIBDP divided by strictly positive finite total assets; one pooled ",
      "1st/99th-percentile winsorisation calculated on the eligible ",
      "year/exchange sample before AI-history selection."
    ),
    difference_definition = "High competition ATT - Low competition ATT",
    inference_cluster = HHI_CLUSTER_LABEL,
    bootstrap = "cluster multiplier bootstrap",
    bootstrap_iterations = DID_BITERS
  ),
  hhi_market_year = hhi_market_year,
  firm_hhi_regime = firm_hhi_regime,
  regime_summary = regime_summary,
  coverage = hhi_coverage,
  cohort_membership = cohort_membership,
  cohort_treated_coverage = cohort_treated_coverage,
  support_exclusions = support_exclusion_table,
  att_results = att_results,
  difference_results = difference_results,
  cohort_effects = cohort_effects,
  cell_audit = cell_audit,
  covariate_audit = covariate_audit,
  sample_audit = sample_audit,
  wald_full_results = wald_full_results,
  wald_full_coefficients = wald_full_coefficients,
  wald_results = wald_results,
  wald_coefficients = wald_coefficients,
  models = named_models
)

if (SAVE_HHI_OUTPUTS) {
  dir.create(HHI_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(hhi_market_year, HHI_MARKET_YEAR_CSV)
  write_csv(firm_hhi_regime, HHI_FIRM_REGIME_CSV)
  write_csv(regime_summary, HHI_REGIME_SUMMARY_CSV)
  write_csv(hhi_coverage, HHI_COVERAGE_CSV)
  write_csv(cohort_membership, HHI_COHORT_MEMBERSHIP_CSV)
  write_csv(cohort_treated_coverage, HHI_COHORT_COVERAGE_CSV)
  write_csv(support_exclusion_table, HHI_SUPPORT_EXCLUSIONS_CSV)
  write_csv(att_results, HHI_ATT_CSV)
  write_csv(difference_results, HHI_DIFFERENCE_CSV)
  write_csv(cohort_effects, HHI_COHORT_EFFECT_CSV)
  write_csv(cell_audit, HHI_CELL_AUDIT_CSV)
  write_csv(covariate_audit, HHI_COVARIATE_AUDIT_CSV)
  write_csv(sample_audit, HHI_SAMPLE_AUDIT_CSV)
  write_csv(wald_full_results, HHI_WALD_FULL_CSV)
  write_csv(wald_results, HHI_WALD_CSV)
  write_wald_pretrends_latex(wald_results, HHI_WALD_LATEX)
  write_csv(publication_table, HHI_PUBLICATION_CSV)
  write_publication_markdown(publication_table, HHI_PUBLICATION_MD)
  # Market value is no longer part of the DiD/HHI analysis. Keep the upstream
  # panel unchanged, but do not carry this unused variable into HHI outputs.
  saveRDS(
    select(analysis_hhi, -any_of("log_market_cap")),
    HHI_PANEL_RDS
  )
  saveRDS(hhi_results, HHI_RESULTS_RDS)
}


# ---- Console summary ---------------------------------------------------------
cat(
  "\nEstimated HHI heterogeneity models using",
  "firm + industry controls | cohort-gminus1 DR.\n"
)
cat(
  "HHI classification: cohort g-1 NAICS", HHI_NAICS_DIGITS,
  "; high competition <=",
  HHI_COMPETITION_CUTOFF,
  "; low competition >", HHI_COMPETITION_CUTOFF, "\n"
)
cat(
  "Inference: ", HHI_CLUSTER_LABEL,
  " cluster multiplier bootstrap with",
  DID_BITERS, "iterations.\n"
)
cat(
  "Industry-support exclusions:", nrow(support_exclusion_table),
  "firm-treatment records across",
  n_distinct(support_exclusion_table$firm_id), "unique firms.\n"
)
cat("\nFull-window aggregated event-study Wald tests:\n")
print(wald_full_results)
cat("\nCommon-window aggregated event-study Wald tests (event times -4:-1):\n")
print(wald_results)
cat("\nHigh-minus-low ATT tests:\n")
print(
  difference_results |>
    select(
      treatment_label, outcome_label, high_att, low_att,
      estimate_difference, std_error_difference, p_value_difference
    )
)
if (SAVE_HHI_OUTPUTS) {
  cat("\nSaved HHI heterogeneity outputs to:\n", HHI_OUTPUT_DIR, "\n")
  cat("Saved full-window Wald CSV to:\n", HHI_WALD_FULL_CSV, "\n")
  cat("Saved corrected Wald CSV to:\n", HHI_WALD_CSV, "\n")
  cat("Saved corrected Wald LaTeX table to:\n", HHI_WALD_LATEX, "\n")
}
