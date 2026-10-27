#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Callaway--Sant'Anna DiD: firm outcomes after AI adoption
# ------------------------------------------------------------------------------
# The main design keeps the unbalanced panel and not-yet-treated comparison
# group. Each ATT(g,t) uses two-period outcome pairs, while every conditioning
# variable for treated and comparison firms comes from the treated cohort's
# common calendar-year baseline g-1.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(did)
library(DRDID)
library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(tibble)
library(tidyr)

source("code/main/9. did/9. did_gminus1_helpers.R")


# ---- Settings ----------------------------------------------------------------
DID_OUTPUT_DIR <- Sys.getenv(
  "DID_OUTPUT_DIR", unset = file.path(OUTPUT_DIR, "did_firm_outcomes")
)
DID_DYNAMIC_PLOT_DIR <- file.path(DID_OUTPUT_DIR, "dynamic_plots")
DID_RESULTS_RDS <- file.path(DID_OUTPUT_DIR, "did_results.rds")
DID_ATT_CSV <- file.path(DID_OUTPUT_DIR, "did_att_estimates.csv")
DID_EVENT_STUDY_CSV <- file.path(DID_OUTPUT_DIR, "did_event_study_best_spec.csv")
DID_WALD_CSV <- file.path(DID_OUTPUT_DIR, "did_parallel_trends_wald.csv")
DID_COHORT_COUNTS_CSV <- file.path(DID_OUTPUT_DIR, "did_cohort_counts.csv")
DID_COVARIATE_TIMING_CSV <- file.path(
  DID_OUTPUT_DIR,
  "did_pre_treatment_covariate_audit.csv"
)
# Historical filename retained for downstream compatibility. It now compares
# the full input with the cell-specific unbalanced estimation sample.
DID_BALANCED_SAMPLE_LOSS_CSV <- file.path(
  DID_OUTPUT_DIR,
  "did_balanced_panel_sample_loss.csv"
)
DID_ATT_GT_CELL_CSV <- file.path(DID_OUTPUT_DIR, "did_att_gt_cells.csv")
DID_COHORT_BASELINE_LOSS_CSV <- file.path(
  DID_OUTPUT_DIR,
  "did_cohort_baseline_sample_loss.csv"
)
DID_COHORT_EFFECT_DIR <- file.path(DID_OUTPUT_DIR, "cohort_effect")
DID_COHORT_EFFECT_CSV <- file.path(
  DID_COHORT_EFFECT_DIR,
  "did_cohort_effects_all.csv"
)
DID_COHORT_EFFECT_RDS <- file.path(
  DID_COHORT_EFFECT_DIR,
  "did_cohort_effects_all.rds"
)
DID_PUBLICATION_CSV <- file.path(DID_OUTPUT_DIR, "did_publication_tables.csv")
DID_PUBLICATION_MD <- file.path(DID_OUTPUT_DIR, "did_publication_tables.md")

SAVE_DID_OUTPUTS <- get0("SAVE_DID_OUTPUTS", ifnotfound = TRUE)
MIN_TREATED_COHORT <- MIN_DID_TREATED_COHORT
DID_EST_METHOD <- "dr"
CONTROL_GROUP <- "notyettreated"
DID_BASE_PERIOD <- "varying"
DID_2_COHORT_VAR <- Sys.getenv(
  "DID_2_COHORT_VAR", unset = "ai_adoption_year"
)
DID_3_COHORT_VAR <- Sys.getenv(
  "DID_3_COHORT_VAR", unset = "ai_adoption_3_year"
)
DID_TREATMENT_TIMING <- if (
  identical(DID_2_COHORT_VAR, "ai_adoption_year") &&
    identical(DID_3_COHORT_VAR, "ai_adoption_3_year")
) {
  paste0(
    "The main cohort is the first Compustat fiscal period after the first ",
    "qualifying 10-K filing date. Its g-1 outcome may already reflect AI ",
    "described in that filing; ATTs describe incremental changes after ",
    "disclosure, not the full effect of initial AI use."
  )
} else if (
  identical(DID_2_COHORT_VAR, "ai_report_adoption_year") &&
    identical(DID_3_COHORT_VAR, "ai_report_adoption_3_year")
) {
  paste0(
    "Timing comparison: the cohort is the first positive 10-K's reported ",
    "Compustat fiscal period. The g-1 outcome precedes that report period. ",
    "The SEC filing date remains a separate public-disclosure timestamp."
  )
} else {
  paste("Custom cohort variables:", DID_2_COHORT_VAR, DID_3_COHORT_VAR)
}
ALLOW_UNBALANCED_PANEL <- TRUE
BEST_DID_SPEC_NAME <- "main_controls_naics2"

DID_BITERS <- as.integer(get0("DID_BITERS", ifnotfound = 1000L))
DID_BOOTSTRAP_SEED <- 123L
DID_OVERLAP_THRESHOLD <- 0.995
DID_MIN_CELL_SIZE <- 5L

# Keep the outcome names and publication labels in one canonical object. This
# deliberately does not read a pre-existing `DID_OUTCOMES` object: doing so
# allowed stale interactive-session state (including the removed market-value
# outcome) to leak into a new run.
OUTCOME_LABELS <- c(
  log_emp = "Employment (log)",
  log_labor_productivity = "Labour productivity (log)",
  log_sale = "Sales (log)",
  log_xopr = "Operating costs (log)",
  operating_profitability_w = "Operating profitability (OIBDP/total assets)"
)
DID_OUTCOMES <- names(OUTCOME_LABELS)

# CAPX retains its economic meaning but uses the panel's existing winsorized
# ratio. The raw ratio has a maximum above 2,000 and was the sole source of
# exploding cohort estimates and propensity separation in the design audit.
# This choice is based on support, not estimated treatment effects.
FIRM_DID_CONTROLS <- c(
  "log_at",
  "roa",
  "cash_ratio",
  "rd_reporter",
  "firm_age",
  "capx_intensity_y_w"
)
PROFITABILITY_DID_CONTROLS <- setdiff(FIRM_DID_CONTROLS, "roa")
MAIN_DID_CONTROLS <- c(FIRM_DID_CONTROLS, "naics2_f")

resolve_did_controls <- function(outcome, controls) {
  if (identical(outcome, "operating_profitability_w")) {
    return(setdiff(controls, "roa"))
  }
  controls
}

if (!ALLOW_UNBALANCED_PANEL) {
  stop("The main design requires ALLOW_UNBALANCED_PANEL = TRUE.")
}
if (DID_BASE_PERIOD != "varying") {
  stop("The cohort-gminus1 implementation requires DID_BASE_PERIOD = 'varying'.")
}
if (DID_EST_METHOD != "dr") {
  stop("The identification-driven main estimator must remain doubly robust ('dr').")
}

DID_SPECIFICATIONS <- tibble(
  spec_order = 1:3,
  spec_name = c("unconditional", "naics2", "main_controls_naics2"),
  spec_label = c(
    "Unconditional",
    "Industry controls",
    "Firm + industry controls"
  ),
  column_label = paste0(
    "(", 1:3, ") ",
    c("Unconditional", "Industry controls", "Firm + industry controls")
  ),
  naics2_controls = c("No", "Yes", "Yes"),
  firm_controls = c("No", "No", "Yes"),
  controls = list(character(), "naics2_f", MAIN_DID_CONTROLS)
)

stopifnot(
  nrow(DID_SPECIFICATIONS) == 3L,
  BEST_DID_SPEC_NAME %in% DID_SPECIFICATIONS$spec_name
)

# ---- General helpers ---------------------------------------------------------
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

assessment_label <- function(p_value) {
  case_when(
    is.na(p_value) ~ "Not available",
    p_value < 0.05 ~ "Reject",
    TRUE ~ "Do not reject"
  )
}

build_did_sample <- function(data, cohort_var) {
  required <- c("cik", "year", cohort_var)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Variables required for the DiD sample are missing: ", paste(missing, collapse = ", "))
  }

  data |>
    mutate(
      cik = as.character(cik),
      year = as.integer(year),
      cohort = as.integer(.data[[cohort_var]]),
      naics2_f = factor(as.character(naics2))
    ) |>
    filter(
      !is.na(cik), cik != "",
      !is.na(year),
      !is.na(cohort),
      cohort == 0L | cohort >= MIN_TREATED_COHORT
    ) |>
    arrange(cik, year) |>
    group_by(cik) |>
    mutate(firm_id = cur_group_id()) |>
    ungroup()
}


# ---- One treatment/outcome/specification ------------------------------------
run_cs_did <- function(
    data,
    outcome,
    controls,
    spec_order,
    spec_name,
    spec_label,
    bootstrap_seed,
    cohort_var,
    compute_dynamic
) {
  outcome_label <- if (outcome %in% names(OUTCOME_LABELS)) {
    unname(OUTCOME_LABELS[[outcome]])
  } else {
    outcome
  }

  required <- unique(c("cik", "firm_id", "year", "cohort", outcome, controls))
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("DiD variables not found in the analysis panel: ", paste(missing, collapse = ", "))
  }
  if (nrow(filter(count(data, firm_id, year), n > 1L)) > 0L) {
    stop("Duplicate firm-year observations found in the DiD sample.")
  }

  sample <- data |>
    select(all_of(required))
  input_n_obs <- nrow(sample)
  input_n_firms <- n_distinct(sample$firm_id)

  set.seed(bootstrap_seed)
  custom <- did_build_gminus1_mp(
    data = sample,
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
    overlap_threshold = DID_OVERLAP_THRESHOLD
  )
  cs_model <- custom$mp

  failed_cells <- custom$cells |>
    filter(!estimable)
  if (nrow(failed_cells) > 0L) {
    stop(
      cohort_var, ", ", outcome, ", ", spec_name, ": ",
      nrow(failed_cells), " ATT(g,t) cell(s) were not estimable."
    )
  }

  set.seed(bootstrap_seed)
  overall_att <- did::aggte(
    cs_model,
    type = "group",
    na.rm = FALSE,
    bstrap = TRUE,
    biters = DID_BITERS,
    cband = TRUE,
    clustervars = "firm_id"
  )

  full_test <- custom$full_pretrend
  common_test <- custom$common_pretrend
  full_event_times <- full_test$coefficients$event_time
  full_window <- if (
    identical(
      full_event_times,
      seq.int(min(full_event_times), max(full_event_times))
    )
  ) {
    paste0("Event time ", min(full_event_times), ":", max(full_event_times))
  } else {
    paste0("Event times ", paste(full_event_times, collapse = ","))
  }
  wald_test_tbl <- tibble(
    spec_order,
    spec_name,
    spec_label,
    outcome,
    Outcome = outcome_label,
    full_window,
    `Wald statistic` = full_test$statistic,
    df = full_test$df,
    `p-value` = full_test$p_value,
    Assessment = assessment_label(full_test$p_value),
    full_restrictions = full_test$restrictions,
    full_rank_deficient = full_test$rank_deficient,
    common_window = "Event time -4:-1",
    `Common-window Wald statistic` = common_test$statistic,
    `Common-window df` = common_test$df,
    `Common-window p-value` = common_test$p_value,
    `Common-window assessment` = assessment_label(common_test$p_value)
  )

  overall_tbl <- tibble(
    spec_order,
    spec_name,
    spec_label,
    outcome,
    outcome_label,
    controls = if (length(controls) == 0L) "None (~1)" else paste(controls, collapse = ", "),
    covariate_reference_period = if (length(controls) == 0L) {
      "No covariates"
    } else {
      "Treated cohort's calendar year g-1 for treated and comparison firms"
    },
    base_period = DID_BASE_PERIOD,
    n_obs = custom$n_obs,
    n_firms = custom$n_firms,
    estimate = as.numeric(overall_att$overall.att),
    std_error = as.numeric(overall_att$overall.se),
    p_value = 2 * pnorm(-abs(estimate / std_error)),
    ci_low = estimate - 1.96 * std_error,
    ci_high = estimate + 1.96 * std_error,
    estimable_att_gt = sum(custom$cells$estimable),
    na_att_gt = sum(!custom$cells$estimable),
    overlap_warning_cells = sum(custom$cells$support_status == "WARNING"),
    max_abs_att_gt = max(abs(custom$cells$estimate), na.rm = TRUE),
    max_att_gt_se = max(custom$cells$std_error, na.rm = TRUE),
    max_control_odds_weight = max(custom$cells$max_control_odds_weight, na.rm = TRUE)
  )

  treated_counts <- custom$firm_meta |>
    filter(aggregation_cohort >= MIN_TREATED_COHORT) |>
    count(aggregation_cohort, name = "cohort_firms") |>
    transmute(
      cohort_year = as.integer(aggregation_cohort),
      cohort_firms = as.integer(cohort_firms)
    )

  cohort_tbl <- tibble(
    spec_order,
    spec_name,
    spec_label,
    outcome,
    outcome_label,
    controls = if (length(controls) == 0L) "None (~1)" else paste(controls, collapse = ", "),
    cohort_var,
    cohort_year = as.integer(overall_att$egt),
    estimate = as.numeric(overall_att$att.egt),
    std_error = as.numeric(overall_att$se.egt),
    n_obs = custom$n_obs,
    n_firms = custom$n_firms
  ) |>
    mutate(
      p_value = if_else(
        is.finite(std_error) & std_error > 0,
        2 * pnorm(-abs(estimate / std_error)),
        NA_real_
      ),
      ci_low = estimate - 1.96 * std_error,
      ci_high = estimate + 1.96 * std_error
    ) |>
    left_join(treated_counts, by = "cohort_year") |>
    arrange(cohort_year)

  dynamic_tbl <- tibble()
  if (compute_dynamic) {
    set.seed(bootstrap_seed)
    dynamic_att <- did::aggte(
      cs_model,
      type = "dynamic",
      min_e = -4,
      max_e = 6,
      na.rm = FALSE,
      bstrap = TRUE,
      biters = DID_BITERS,
      cband = TRUE,
      clustervars = "firm_id"
    )
    dynamic_tbl <- tibble(
      spec_order,
      spec_name,
      spec_label,
      outcome,
      outcome_label,
      n_obs = custom$n_obs,
      n_firms = custom$n_firms,
      event_time = dynamic_att$egt,
      estimate = dynamic_att$att.egt,
      std_error = dynamic_att$se.egt,
      simultaneous_critical_value = dynamic_att$crit.val.egt,
      ci_low = estimate - simultaneous_critical_value * std_error,
      ci_high = estimate + simultaneous_critical_value * std_error
    ) |>
      arrange(event_time)
  }

  sample_loss_tbl <- tibble(
    spec_order,
    spec_name,
    spec_label,
    outcome,
    cohort_var,
    input_n_obs,
    estimation_n_obs = custom$n_obs,
    observations_lost = input_n_obs - custom$n_obs,
    observation_loss_pct = 100 * observations_lost / input_n_obs,
    input_n_firms,
    estimation_n_firms = custom$n_firms,
    firms_lost = input_n_firms - custom$n_firms,
    firm_loss_pct = 100 * firms_lost / input_n_firms,
    covariate_rows_used = custom$n_covariate_rows,
    any_rows_used = custom$n_any_used_rows,
    sample_definition = paste0(
      "Unique outcome firm-years contributing to at least one ATT(g,t); ",
      "missingness is handled cell by cell"
    )
  )

  covariate_timing_tbl <- tibble(
    spec_order,
    spec_name,
    spec_label,
    outcome,
    cohort_var,
    panel = TRUE,
    allow_unbalanced_panel = ALLOW_UNBALANCED_PANEL,
    control_group = CONTROL_GROUP,
    base_period = DID_BASE_PERIOD,
    estimator = "project cohort-gminus1 wrapper: DRDID cells + did::aggte",
    estimation_method = DID_EST_METHOD,
    covariate_reference_period = if (length(controls) == 0L) {
      "No covariates"
    } else {
      "For cohort g, every treated/control covariate is measured in calendar year g-1"
    },
    permits_post_treatment_controls = FALSE,
    comparison_firm_uses_own_treatment_baseline = FALSE,
    firm_controls = paste(intersect(controls, FIRM_DID_CONTROLS), collapse = ", "),
    naics2_control = "naics2_f" %in% controls
  )

  cell_tbl <- custom$cells |>
    mutate(
      spec_order,
      spec_name,
      spec_label,
      outcome,
      cohort_var,
      controls = if (length(controls) == 0L) "None (~1)" else paste(controls, collapse = ", "),
      .before = 1L
    )
  cohort_loss_tbl <- cell_tbl |>
    select(
      spec_order, spec_name, spec_label, outcome, cohort_var,
      cohort_year, target_year, baseline_year, outcome_base_year,
      eligible_treated, n_treated, treated_lost,
      eligible_controls, n_controls, controls_lost
    )

  list(
    model = cs_model,
    overall_tbl = overall_tbl,
    cohort_tbl = cohort_tbl,
    dynamic_tbl = dynamic_tbl,
    wald_test_tbl = wald_test_tbl,
    covariate_timing_tbl = covariate_timing_tbl,
    balance_loss_tbl = sample_loss_tbl,
    cell_tbl = cell_tbl,
    cohort_loss_tbl = cohort_loss_tbl
  )
}


# ---- Publication table helpers ----------------------------------------------
build_panel_rows <- function(att_data, panel_label, row_offset) {
  spec_columns <- DID_SPECIFICATIONS$column_label
  display <- att_data |>
    left_join(select(DID_SPECIFICATIONS, spec_order, column_label), by = "spec_order") |>
    arrange(spec_order) |>
    mutate(stars = significance_stars(p_value))
  if (
    nrow(display) != nrow(DID_SPECIFICATIONS) ||
      n_distinct(display$spec_order) != nrow(DID_SPECIFICATIONS) ||
      any(is.na(display$column_label))
  ) {
    stop("Publication table does not contain exactly one estimate per specification.")
  }

  bind_rows(
    tibble(
      row_order = row_offset,
      row_type = "panel",
      row_label = panel_label,
      column_label = spec_columns,
      value = ""
    ),
    display |>
      transmute(
        row_order = row_offset + 1L, row_type = "estimate", row_label = "ATT",
        column_label, value = paste0(format_estimate(estimate), stars)
      ),
    display |>
      transmute(
        row_order = row_offset + 2L, row_type = "standard_error", row_label = "",
        column_label, value = sprintf("(%.3f)", std_error)
      ),
    display |>
      transmute(
        row_order = row_offset + 3L, row_type = "observations", row_label = "Observations",
        column_label, value = format_count(n_obs)
      ),
    display |>
      transmute(
        row_order = row_offset + 4L, row_type = "firms", row_label = "Firms",
        column_label, value = format_count(n_firms)
      )
  )
}

build_control_rows <- function(row_offset) {
  bind_rows(
    DID_SPECIFICATIONS |>
      transmute(
        row_order = row_offset,
        row_type = "control",
        row_label = "NAICS2 controls",
        column_label,
        value = naics2_controls
      ),
    DID_SPECIFICATIONS |>
      transmute(
        row_order = row_offset + 1L,
        row_type = "control",
        row_label = "Firm controls",
        column_label,
        value = firm_controls
      )
  )
}

build_publication_table <- function(att_2, att_3, outcome_name) {
  spec_columns <- DID_SPECIFICATIONS$column_label
  bind_rows(
    build_panel_rows(
      filter(att_2, outcome == outcome_name),
      "Panel A: AI adoption ≥ 2",
      1L
    ),
    build_panel_rows(
      filter(att_3, outcome == outcome_name),
      "Panel B: Strong AI adoption = 3",
      7L
    ),
    build_control_rows(13L)
  ) |>
    mutate(column_label = factor(column_label, levels = spec_columns)) |>
    arrange(row_order, column_label) |>
    pivot_wider(names_from = column_label, values_from = value) |>
    mutate(
      outcome = outcome_name,
      outcome_label = unname(OUTCOME_LABELS[[outcome_name]]),
      .before = 1L
    ) |>
    select(
      outcome, outcome_label, row_order, row_type, row_label,
      all_of(spec_columns)
    )
}

markdown_row <- function(values) {
  escaped <- gsub("|", "\\\\|", as.character(values), fixed = TRUE)
  paste0("| ", paste(escaped, collapse = " | "), " |")
}

write_publication_markdown <- function(publication_tables, path) {
  spec_columns <- DID_SPECIFICATIONS$column_label
  lines <- c("# Callaway--Sant'Anna estimates", "")
  for (outcome_name in DID_OUTCOMES) {
    table <- publication_tables |>
      filter(outcome == outcome_name) |>
      arrange(row_order) |>
      mutate(
        row_label = if_else(
          row_type == "panel",
          paste0("**", row_label, "**"),
          row_label
        )
      ) |>
      select(row_label, all_of(spec_columns))
    lines <- c(
      lines,
      paste0("## ", unname(OUTCOME_LABELS[[outcome_name]])),
      "",
      markdown_row(names(table)),
      markdown_row(rep("---", ncol(table))),
      apply(table, 1L, markdown_row),
      ""
    )
  }

  best_spec_label <- DID_SPECIFICATIONS |>
    filter(spec_name == BEST_DID_SPEC_NAME) |>
    pull(column_label)
  lines <- c(
    lines,
    paste0(
      "Notes: Estimates use the unbalanced cohort-specific Callaway--Sant'Anna ",
      "design with not-yet-treated controls and doubly robust 2x2 estimators. ",
      "Observations are unique CIK-year outcome rows used by at least one ",
      "ATT(g,t), counted once across comparisons. Treatment cohorts begin in ",
      MIN_TREATED_COHORT, ", while outcome years begin in ",
      ANALYSIS_START_YEAR, " for pre-treatment comparisons. ",
      "All estimation outcomes require a contemporaneously matched and scored ",
      "annual filing, as does the main descriptive table. ",
      "See the sample reconciliation appendix. ",
      "For each treated cohort g, all controls for treated and comparison firms ",
      "are measured in calendar year g-1. Parentheses contain firm-level ",
      "multiplier-bootstrap standard errors. Specification (2) includes two-digit ",
      "NAICS indicators. Specification (3) additionally includes pre-treatment ",
      "firm size, ROA, cash ratio, R&D reporting status, firm age, and winsorized ",
      "capital-expenditure intensity. ROA is omitted when operating profitability ",
      "is the outcome because both variables measure profitability relative to ",
      "assets. Operating profitability is OIBDP divided by strictly positive total ",
      "assets and is pooled-winsorized at the 1st and 99th percentiles of the ",
      "eligible year/exchange sample. * p < 0.10; ** p < 0.05; *** p < 0.01."
    ),
    paste0("Dynamic estimates use the configured preferred model: ", best_spec_label, ".")
  )
  writeLines(lines, path, useBytes = TRUE)
}


# ---- Load panel and estimate model grid -------------------------------------
if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop(
    "Final analysis panel not found: ", ANALYSIS_PANEL_RDS,
    ". Run 4. build_or_load_panel_data.R first."
  )
}
panel_ai <- readRDS(ANALYSIS_PANEL_RDS)
if (!"ai_score" %in% names(panel_ai) || anyNA(panel_ai$ai_score)) {
  stop(
    "The DiD source panel includes unmatched AI-score years. Rebuild the ",
    "matched analysis panel with 4. build_or_load_panel_data.R."
  )
}
if (!DID_3_COHORT_VAR %in% names(panel_ai)) {
  stop("The final analysis panel does not contain ", DID_3_COHORT_VAR, ".")
}

model_grid <- crossing(
  outcome = DID_OUTCOMES,
  spec_order = DID_SPECIFICATIONS$spec_order
) |>
  left_join(DID_SPECIFICATIONS, by = "spec_order") |>
  arrange(match(outcome, DID_OUTCOMES), spec_order) |>
  mutate(
    controls = map2(outcome, controls, resolve_did_controls),
    bootstrap_seed = DID_BOOTSTRAP_SEED + row_number() - 1L,
    compute_dynamic = spec_name == BEST_DID_SPEC_NAME
  )

stopifnot(
  all(map_lgl(
    model_grid$controls[model_grid$outcome == "operating_profitability_w"],
    ~ !"roa" %in% .x
  )),
  all(map_lgl(
    model_grid$controls[
      model_grid$outcome != "operating_profitability_w" &
        model_grid$spec_name == "main_controls_naics2"
    ],
    ~ "roa" %in% .x
  ))
)

order_model_table <- function(data) {
  ordered <- data |>
    mutate(
      outcome_order = match(outcome, DID_OUTCOMES),
      spec_sort_order = match(spec_name, DID_SPECIFICATIONS$spec_name)
    )
  if ("event_time" %in% names(ordered)) {
    ordered <- arrange(ordered, outcome_order, spec_sort_order, event_time)
  } else {
    ordered <- arrange(ordered, outcome_order, spec_sort_order)
  }
  select(ordered, -outcome_order, -spec_sort_order)
}

estimate_did_bundle <- function(data, cohort_var) {
  cohort_counts <- data |>
    distinct(cik, cohort) |>
    count(cohort, name = "n_firms") |>
    arrange(cohort) |>
    transmute(cohort_year = cohort, n_firms)

  model_results <- pmap(
    select(
      model_grid,
      outcome, controls, spec_order, spec_name, spec_label,
      bootstrap_seed, compute_dynamic
    ),
    function(
        outcome, controls, spec_order, spec_name, spec_label,
        bootstrap_seed, compute_dynamic
    ) {
      cat(
        "\nEstimating:", cohort_var, "|", outcome, "|", spec_label,
        "| cohort-gminus1", toupper(DID_EST_METHOD), "\n"
      )
      run_cs_did(
        data = data,
        outcome = outcome,
        controls = controls,
        spec_order = spec_order,
        spec_name = spec_name,
        spec_label = spec_label,
        bootstrap_seed = bootstrap_seed,
        cohort_var = cohort_var,
        compute_dynamic = compute_dynamic
      )
    }
  )

  list(
    cohort_counts = cohort_counts,
    att_table = order_model_table(bind_rows(map(model_results, "overall_tbl"))),
    cohort_effect_table = bind_rows(map(model_results, "cohort_tbl")) |>
      arrange(match(outcome, DID_OUTCOMES), spec_order, cohort_year),
    event_study_table = order_model_table(bind_rows(map(model_results, "dynamic_tbl"))),
    wald_test_table = order_model_table(bind_rows(map(model_results, "wald_test_tbl"))),
    covariate_timing_table = order_model_table(
      bind_rows(map(model_results, "covariate_timing_tbl"))
    ),
    balance_loss_table = order_model_table(
      bind_rows(map(model_results, "balance_loss_tbl"))
    ),
    cell_table = bind_rows(map(model_results, "cell_tbl")) |>
      arrange(match(outcome, DID_OUTCOMES), spec_order, cohort_year, target_year),
    cohort_loss_table = bind_rows(map(model_results, "cohort_loss_tbl")) |>
      arrange(match(outcome, DID_OUTCOMES), spec_order, cohort_year, target_year),
    models = map(model_results, "model")
  )
}

did_bundle_2 <- panel_ai |>
  build_did_sample(DID_2_COHORT_VAR) |>
  estimate_did_bundle(DID_2_COHORT_VAR)
did_bundle_3 <- panel_ai |>
  build_did_sample(DID_3_COHORT_VAR) |>
  estimate_did_bundle(DID_3_COHORT_VAR)


# ---- Combine treatment definitions ------------------------------------------
TREATMENT_DEFINITIONS <- tibble(
  treatment_order = 1:2,
  treatment_id = c("ai_adoption_ge_2", "strong_ai_adoption_3"),
  treatment_label = c("AI adoption ≥ 2", "Strong AI adoption = 3"),
  cohort_var = c(DID_2_COHORT_VAR, DID_3_COHORT_VAR)
)

tag_treatment <- function(data, treatment_row) {
  data |>
    mutate(
      treatment_order = treatment_row$treatment_order,
      treatment_id = treatment_row$treatment_id,
      treatment_label = treatment_row$treatment_label,
      cohort_var = treatment_row$cohort_var,
      .before = 1L
    )
}

treatment_2 <- slice(TREATMENT_DEFINITIONS, 1L)
treatment_3 <- slice(TREATMENT_DEFINITIONS, 2L)
combine_treatments <- function(component, arrange_vars) {
  bind_rows(
    tag_treatment(did_bundle_2[[component]], treatment_2),
    tag_treatment(did_bundle_3[[component]], treatment_3)
  ) |>
    arrange(!!!syms(arrange_vars))
}

att_table <- combine_treatments(
  "att_table", c("treatment_order", "outcome", "spec_order")
)
cohort_effect_table <- combine_treatments(
  "cohort_effect_table",
  c("treatment_order", "outcome", "spec_order", "cohort_year")
)
event_study_table <- combine_treatments(
  "event_study_table", c("treatment_order", "outcome", "event_time")
)
wald_test_table <- combine_treatments(
  "wald_test_table", c("treatment_order", "outcome", "spec_order")
)
covariate_timing_table <- combine_treatments(
  "covariate_timing_table", c("treatment_order", "outcome", "spec_order")
)
balance_loss_table <- combine_treatments(
  "balance_loss_table", c("treatment_order", "outcome", "spec_order")
)
cell_table <- combine_treatments(
  "cell_table",
  c("treatment_order", "outcome", "spec_order", "cohort_year", "target_year")
)
cohort_loss_table <- combine_treatments(
  "cohort_loss_table",
  c("treatment_order", "outcome", "spec_order", "cohort_year", "target_year")
)
cohort_counts <- combine_treatments(
  "cohort_counts", c("treatment_order", "cohort_year")
)

publication_tables <- map_dfr(
  DID_OUTCOMES,
  ~ build_publication_table(
    did_bundle_2$att_table,
    did_bundle_3$att_table,
    .x
  )
)

did_results <- list(
  specifications = DID_SPECIFICATIONS,
  estimator = "cohort-specific g-1 DRDID cells aggregated with did::aggte",
  treatment_timing = DID_TREATMENT_TIMING,
  preferred_event_study_specification = BEST_DID_SPEC_NAME,
  outcome_construction = list(
    operating_profitability = paste0(
      "OIBDP divided by strictly positive finite total assets; one pooled ",
      "1st/99th-percentile winsorisation calculated on the eligible ",
      "year/exchange sample before AI-history selection."
    )
  ),
  covariate_timing = list(
    panel = TRUE,
    allow_unbalanced_panel = ALLOW_UNBALANCED_PANEL,
    base_period = DID_BASE_PERIOD,
    control_group = CONTROL_GROUP,
    firm_controls = FIRM_DID_CONTROLS,
    operating_profitability_firm_controls = PROFITABILITY_DID_CONTROLS,
    operating_profitability_control_exception = paste0(
      "ROA is omitted when operating_profitability_w is the outcome because net ",
      "income/assets and OIBDP/assets are overlapping profitability constructs."
    ),
    interpretation = paste0(
      "For every treated cohort g, all treated and eligible not-yet-treated ",
      "comparison firms use covariates observed in calendar year g-1. No ",
      "post-treatment covariates enter post-treatment ATT(g,t) nuisance models."
    )
  ),
  treatment_definitions = TREATMENT_DEFINITIONS,
  cohort_counts = cohort_counts,
  cohort_effect_table = cohort_effect_table,
  att_table = att_table,
  publication_tables = publication_tables,
  event_study_table = event_study_table,
  wald_test_table = wald_test_table,
  covariate_timing_table = covariate_timing_table,
  balance_loss_table = balance_loss_table,
  cell_table = cell_table,
  cohort_loss_table = cohort_loss_table,
  models = list(
    main_treatment = did_bundle_2$models,
    strong_treatment = did_bundle_3$models
  )
)


# ---- Dynamic-output validation and plots ------------------------------------
validate_best_dynamic_output <- function(plot_data) {
  unexpected <- setdiff(unique(plot_data$spec_name), BEST_DID_SPEC_NAME)
  if (length(unexpected) > 0L) {
    stop("Dynamic output contains non-preferred specifications: ", paste(unexpected, collapse = ", "))
  }
  expected <- crossing(
    treatment_id = TREATMENT_DEFINITIONS$treatment_id,
    outcome = DID_OUTCOMES
  )
  missing <- anti_join(
    expected,
    distinct(plot_data, treatment_id, outcome),
    by = c("treatment_id", "outcome")
  )
  if (nrow(missing) > 0L) {
    stop(
      "Preferred-specification dynamic estimates are missing for: ",
      paste0(missing$treatment_id, "/", missing$outcome, collapse = ", ")
    )
  }
  invisible(expected)
}

save_best_dynamic_plots <- function(plot_data) {
  expected <- validate_best_dynamic_output(plot_data)
  dir.create(DID_DYNAMIC_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

  expected_plot_files <- expected |>
    transmute(
      path = file.path(
        DID_DYNAMIC_PLOT_DIR,
        paste0("did_", treatment_id, "_dynamic_", outcome, ".png")
      )
    ) |>
    pull(path)
  existing_plot_files <- list.files(
    DID_DYNAMIC_PLOT_DIR,
    pattern = "^did_.*_dynamic_.*\\.png$",
    full.names = TRUE
  )
  stale_plot_files <- setdiff(existing_plot_files, expected_plot_files)
  if (length(stale_plot_files) > 0L) {
    unlink(stale_plot_files)
  }

  pwalk(expected, function(treatment_id, outcome) {
    outcome_data <- plot_data |>
      filter(
        .data$treatment_id == .env$treatment_id,
        .data$outcome == .env$outcome
      )
    dynamic_plot <- ggplot(outcome_data, aes(x = event_time, y = estimate)) +
      geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
      geom_vline(
        xintercept = -0.5,
        color = "gray55",
        linetype = "dashed",
        linewidth = 0.4
      ) +
      geom_ribbon(
        aes(ymin = ci_low, ymax = ci_high),
        fill = "#12436D",
        alpha = 0.18
      ) +
      geom_line(color = "#12436D", linewidth = 0.55) +
      geom_point(color = "#12436D", size = 1) +
      scale_x_continuous(breaks = sort(unique(outcome_data$event_time))) +
      labs(
        title = paste("Dynamic treatment effects:", outcome_data$outcome_label[[1L]]),
        subtitle = paste0(
          outcome_data$treatment_label[[1L]],
          " | ", outcome_data$spec_label[[1L]]
        ),
        x = "Years relative to treatment",
        y = "ATT",
        caption = "Shaded area: simultaneous 95% confidence band."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "gray40")
      )
    ggsave(
      file.path(
        DID_DYNAMIC_PLOT_DIR,
        paste0("did_", treatment_id, "_dynamic_", outcome, ".png")
      ),
      dynamic_plot,
      width = 8,
      height = 5,
      dpi = 300
    )
  })
}

validate_best_dynamic_output(event_study_table)


# ---- Save outputs -------------------------------------------------------------
if (SAVE_DID_OUTPUTS) {
  dir.create(DID_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(DID_COHORT_EFFECT_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(att_table, DID_ATT_CSV)
  write_csv(cohort_effect_table, DID_COHORT_EFFECT_CSV)
  saveRDS(cohort_effect_table, DID_COHORT_EFFECT_RDS)
  write_csv(event_study_table, DID_EVENT_STUDY_CSV)
  write_csv(wald_test_table, DID_WALD_CSV)
  write_csv(cohort_counts, DID_COHORT_COUNTS_CSV)
  write_csv(covariate_timing_table, DID_COVARIATE_TIMING_CSV)
  write_csv(balance_loss_table, DID_BALANCED_SAMPLE_LOSS_CSV)
  write_csv(cell_table, DID_ATT_GT_CELL_CSV)
  write_csv(cohort_loss_table, DID_COHORT_BASELINE_LOSS_CSV)
  write_csv(publication_tables, DID_PUBLICATION_CSV)
  write_publication_markdown(publication_tables, DID_PUBLICATION_MD)
  saveRDS(did_results, DID_RESULTS_RDS)
  save_best_dynamic_plots(event_study_table)
}


# ---- Console summary ----------------------------------------------------------
cat("\nEstimated unbalanced cohort-gminus1 Callaway--Sant'Anna models.\n")
cat("Estimator:", toupper(DID_EST_METHOD), "\n")
cat(
  "Preferred event-study specification:",
  DID_SPECIFICATIONS$column_label[
    DID_SPECIFICATIONS$spec_name == BEST_DID_SPEC_NAME
  ],
  "\n"
)
walk(DID_OUTCOMES, function(outcome_name) {
  cat("\n", unname(OUTCOME_LABELS[[outcome_name]]), "\n", sep = "")
  print(
    publication_tables |>
      filter(outcome == outcome_name) |>
      select(-outcome, -outcome_label, -row_order, -row_type)
  )
})
if (SAVE_DID_OUTPUTS) {
  cat("\nSaved consolidated DiD outputs to:\n", DID_OUTPUT_DIR, "\n")
  cat("Saved cohort-specific ATT table to:\n", DID_COHORT_EFFECT_CSV, "\n")
  cat("Saved ATT(g,t) support audit to:\n", DID_ATT_GT_CELL_CSV, "\n")
  cat("Saved cohort-specific sample-loss audit to:\n", DID_COHORT_BASELINE_LOSS_CSV, "\n")
  cat("Saved preferred-specification dynamic plots to:\n", DID_DYNAMIC_PLOT_DIR, "\n")
}
