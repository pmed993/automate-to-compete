#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Identification diagnostics for code/main/9. did.R
# ------------------------------------------------------------------------------
# This script does not replace the publication model.  It compares the design
# choices requested for the final Callaway--Sant'Anna implementation and writes
# a self-contained diagnostic bundle under output/did_firm_outcomes/design_diagnostics.
#
# The cohort-specific implementation estimates each 2x2 ATT(g,t) on firms
# observed in both outcome periods.  For every cohort g, all conditioning
# variables for treated and eligible control firms are taken from calendar year
# g-1.  The resulting influence functions are embedded in one common firm
# universe and aggregated with did::aggte(); ATT(g,t) values are not averaged by
# hand.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(data.table)
library(did)
library(DRDID)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)

DIAGNOSTIC_OUTPUT_DIR <- file.path(
  OUTPUT_DIR,
  "did_firm_outcomes",
  "design_diagnostics"
)
DIAGNOSTIC_COMPARISON_CSV <- file.path(
  DIAGNOSTIC_OUTPUT_DIR,
  "did_design_comparison.csv"
)
DIAGNOSTIC_CELL_CSV <- file.path(
  DIAGNOSTIC_OUTPUT_DIR,
  "did_cohort_time_support.csv"
)
DIAGNOSTIC_COHORT_LOSS_CSV <- file.path(
  DIAGNOSTIC_OUTPUT_DIR,
  "did_cohort_baseline_sample_loss.csv"
)
DIAGNOSTIC_BUNDLE_RDS <- file.path(
  DIAGNOSTIC_OUTPUT_DIR,
  "did_design_diagnostics.rds"
)

SAVE_DID_DIAGNOSTICS <- isTRUE(
  get0("SAVE_DID_DIAGNOSTICS", ifnotfound = TRUE)
)

MIN_TREATED_COHORT <- MIN_DID_TREATED_COHORT
CONTROL_GROUP <- "notyettreated"
BASE_PERIOD <- "varying"
COMMON_BASELINE_YEAR <- 2015L
OVERLAP_THRESHOLD <- 0.995

DID_OUTCOMES <- c(
  "log_emp",
  "log_labor_productivity",
  "log_sale",
  "log_xopr",
  "operating_profitability_w"
)

FIRM_CONTROLS <- c(
  "log_at",
  "roa",
  "cash_ratio",
  "rd_reporter",
  "firm_age",
  "capx_intensity_y"
)

RAW_CURRENT_CONTROLS <- c(FIRM_CONTROLS, "naics2_f")
STABLE_CONTROLS <- c(
  setdiff(FIRM_CONTROLS, "capx_intensity_y"),
  "capx_intensity_y_w",
  "naics2_f"
)

TREATMENTS <- tribble(
  ~treatment_id, ~treatment_label, ~cohort_var,
  "ai_adoption_ge_2", "AI adoption >= 2", "ai_adoption_year",
  "strong_ai_adoption_3", "Strong AI adoption = 3", "ai_adoption_3_year"
)

DESIGNS <- tribble(
  ~design_order, ~design_id, ~design_label, ~implementation, ~est_method, ~control_profile,
  1L, "balanced_current_dr", "Balanced + current controls + DR", "stock_current", "dr", "raw_current",
  2L, "unbalanced_period_reg", "Unbalanced + period-specific controls + REG", "stock_current", "reg", "raw_current",
  3L, "unbalanced_2015_dr", "Unbalanced + universal 2015 controls + DR", "stock_2015", "dr", "raw_current",
  4L, "unbalanced_2015_reg", "Unbalanced + universal 2015 controls + REG", "stock_2015", "reg", "raw_current",
  5L, "unbalanced_gminus1_dr", "Unbalanced + cohort g-1 current controls + DR", "cohort_gminus1", "dr", "raw_current",
  6L, "unbalanced_gminus1_reg", "Unbalanced + cohort g-1 current controls + REG", "cohort_gminus1", "reg", "raw_current",
  7L, "unbalanced_gminus1_stable_dr", "Unbalanced + cohort g-1 stable controls + DR", "cohort_gminus1", "dr", "winsorized_capx",
  8L, "unbalanced_gminus1_stable_reg", "Unbalanced + cohort g-1 stable controls + REG", "cohort_gminus1", "reg", "winsorized_capx"
) |>
  mutate(
    allow_unbalanced_panel = design_id != "balanced_current_dr",
    covariate_timing = case_when(
      implementation == "stock_current" & allow_unbalanced_panel ~
        "Period-specific rows used by did's unbalanced route",
      implementation == "stock_current" ~
        "Earlier period in each balanced-panel 2x2 comparison",
      implementation == "stock_2015" ~
        "Universal calendar-year 2015 baseline repeated over time",
      TRUE ~
        "Treated cohort's calendar-year g-1 baseline for treated and controls"
    )
  )

requested_designs <- get0(
  "DID_DIAGNOSTIC_DESIGNS",
  ifnotfound = DESIGNS$design_id
)
DESIGNS <- DESIGNS |>
  filter(design_id %in% requested_designs)

requested_treatments <- get0(
  "DID_DIAGNOSTIC_TREATMENTS",
  ifnotfound = TREATMENTS$treatment_id
)
TREATMENTS <- TREATMENTS |>
  filter(treatment_id %in% requested_treatments)

controls_for_design <- function(design, outcome) {
  override <- get0("DID_DIAGNOSTIC_CONTROLS", ifnotfound = NULL)
  controls <- if (!is.null(override)) {
    override
  } else if (design$control_profile == "winsorized_capx") {
    STABLE_CONTROLS
  } else {
    RAW_CURRENT_CONTROLS
  }
  if (identical(outcome, "operating_profitability_w")) {
    controls <- setdiff(controls, "roa")
  }
  controls
}

safe_scalar <- function(x) {
  if (length(x) == 0L || !is.finite(x[[1L]])) NA_real_ else as.numeric(x[[1L]])
}

collapse_messages <- function(x) {
  x <- unique(trimws(x[nzchar(trimws(x))]))
  if (length(x) == 0L) "None" else paste(x, collapse = " | ")
}

capture_conditions <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(message = conditionMessage(e)), class = "diagnostic_error")
  )
  list(value = value, warnings = unique(warnings))
}

prepare_panel <- function(data, cohort_var) {
  data |>
    mutate(
      cik = as.character(cik),
      year = as.integer(year),
      cohort = as.integer(.data[[cohort_var]]),
      naics2_f = factor(as.character(naics2))
    ) |>
    filter(
      !is.na(cik),
      cik != "",
      !is.na(year),
      !is.na(cohort),
      cohort == 0L | cohort >= MIN_TREATED_COHORT
    ) |>
    arrange(cik, year) |>
    group_by(cik) |>
    mutate(firm_id = cur_group_id()) |>
    ungroup()
}

build_formula <- function(controls) {
  if (length(controls) == 0L) ~1 else reformulate(controls)
}

build_2015_panel <- function(data, controls) {
  baseline_names <- paste0(controls, "_2015")
  baseline <- data |>
    filter(year == COMMON_BASELINE_YEAR) |>
    select(firm_id, all_of(controls))
  if (length(controls) > 0L) {
    baseline <- baseline |>
      rename_with(~ paste0(.x, "_2015"), all_of(controls))
  }

  out <- left_join(data, baseline, by = "firm_id")
  if ("naics2_f" %in% controls) {
    out$naics2_f_2015 <- factor(
      as.character(out$naics2_f_2015),
      levels = levels(data$naics2_f)
    )
  }
  list(data = out, controls = baseline_names)
}

stock_diagnostic <- function(data, outcome, design) {
  input <- data
  controls <- controls_for_design(design, outcome)

  if (design$implementation == "stock_2015") {
    baseline <- build_2015_panel(data, controls)
    input <- baseline$data
    controls <- baseline$controls
  }

  required <- unique(c("firm_id", "year", "cohort", outcome, controls))
  input <- input |>
    select(all_of(required))

  fit <- capture_conditions(
    did::att_gt(
      yname = outcome,
      tname = "year",
      idname = "firm_id",
      gname = "cohort",
      xformla = build_formula(controls),
      data = input,
      panel = TRUE,
      allow_unbalanced_panel = design$allow_unbalanced_panel,
      control_group = CONTROL_GROUP,
      base_period = BASE_PERIOD,
      est_method = design$est_method,
      bstrap = FALSE,
      cband = FALSE,
      faster_mode = TRUE,
      compute_inffunc = TRUE
    )
  )

  if (inherits(fit$value, "diagnostic_error")) {
    comparison <- tibble(
      n_obs = NA_integer_, n_firms = NA_integer_, overall_att = NA_real_,
      overall_se = NA_real_, pretrend_wald = NA_real_, pretrend_p_value = NA_real_,
      estimable_att_gt = 0L, na_att_gt = NA_integer_, max_abs_att_gt = NA_real_,
      max_att_gt_se = NA_real_, overlap_support_warnings = collapse_messages(c(fit$warnings, fit$value$message)),
      status = "FAILED"
    )
    return(list(comparison = comparison, cells = tibble(), model = NULL))
  }

  model <- fit$value
  overall_capture <- capture_conditions(
    did::aggte(
      model,
      type = "group",
      na.rm = TRUE,
      bstrap = FALSE,
      cband = FALSE,
      clustervars = "firm_id"
    )
  )

  estimation_data <- model$DIDparams$data
  n_obs <- nrow(estimation_data)
  n_firms <- dplyr::n_distinct(estimation_data$firm_id)
  overall_error <- inherits(overall_capture$value, "diagnostic_error")
  overall <- if (overall_error) NULL else overall_capture$value

  cells <- tibble(
    cohort_year = as.integer(model$group),
    target_year = as.integer(model$t),
    event_time = as.integer(model$t - model$group),
    estimate = as.numeric(model$att),
    std_error = as.numeric(model$se),
    estimable = is.finite(model$att) & is.finite(model$se),
    n_treated = NA_integer_,
    n_controls = NA_integer_,
    baseline_year = NA_integer_,
    min_propensity = NA_real_,
    max_propensity = NA_real_,
    max_control_odds_weight = NA_real_,
    support_status = if_else(is.finite(model$att), "Estimated by stock did", "Not estimable"),
    support_warning = "See model-level warnings"
  )

  comparison <- tibble(
    n_obs = n_obs,
    n_firms = n_firms,
    overall_att = if (is.null(overall)) NA_real_ else safe_scalar(overall$overall.att),
    overall_se = if (is.null(overall)) NA_real_ else safe_scalar(overall$overall.se),
    pretrend_wald = safe_scalar(model$W),
    pretrend_p_value = safe_scalar(model$Wpval),
    estimable_att_gt = sum(is.finite(model$att) & is.finite(model$se)),
    na_att_gt = sum(!is.finite(model$att) | !is.finite(model$se)),
    max_abs_att_gt = if (any(is.finite(model$att))) max(abs(model$att), na.rm = TRUE) else NA_real_,
    max_att_gt_se = if (any(is.finite(model$se))) max(model$se, na.rm = TRUE) else NA_real_,
    overlap_support_warnings = collapse_messages(c(
      fit$warnings,
      overall_capture$warnings,
      if (overall_error) overall_capture$value$message else character()
    )),
    status = if_else(is.null(overall), "FAILED_AGGREGATION", "ESTIMATED")
  )

  list(comparison = comparison, cells = cells, model = model)
}

independent_columns <- function(x, control_rows) {
  if (ncol(x) == 1L) return(1L)
  qr_fit <- qr(x[control_rows, , drop = FALSE], tol = 1e-10, LAPACK = FALSE)
  sort(qr_fit$pivot[seq_len(qr_fit$rank)])
}

cell_gminus1 <- function(
    data, outcome, cohort_year, target_year, est_method, global_ids, controls
) {
  baseline_year <- cohort_year - 1L
  outcome_base_year <- if (target_year >= cohort_year) baseline_year else target_year - 1L

  eligible_ids <- data |>
    distinct(firm_id, cohort) |>
    filter(cohort == cohort_year | cohort == 0L | cohort > target_year)

  treated_ids <- eligible_ids |>
    filter(cohort == cohort_year) |>
    pull(firm_id)

  control_ids <- eligible_ids |>
    filter(cohort == 0L | cohort > target_year) |>
    pull(firm_id)

  baseline <- data |>
    filter(year == baseline_year, firm_id %in% c(treated_ids, control_ids)) |>
    select(firm_id, all_of(controls))
  if (length(controls) > 0L) {
    baseline <- baseline |>
      rename_with(~ paste0("x_", .x), all_of(controls))
  }

  y0 <- data |>
    filter(year == outcome_base_year, firm_id %in% c(treated_ids, control_ids)) |>
    select(firm_id, y0 = all_of(outcome))

  y1 <- data |>
    filter(year == target_year, firm_id %in% c(treated_ids, control_ids)) |>
    select(firm_id, y1 = all_of(outcome))

  cell <- eligible_ids |>
    inner_join(y0, by = "firm_id") |>
    inner_join(y1, by = "firm_id") |>
    inner_join(baseline, by = "firm_id") |>
    mutate(D = as.integer(cohort == cohort_year)) |>
    filter(
      is.finite(y0),
      is.finite(y1),
      if_all(starts_with("x_"), ~ !is.na(.x))
    )

  n_treated <- sum(cell$D == 1L)
  n_controls <- sum(cell$D == 0L)
  base_result <- list(
    cohort_year = cohort_year,
    target_year = target_year,
    event_time = target_year - cohort_year,
    baseline_year = baseline_year,
    outcome_base_year = outcome_base_year,
    n_treated = n_treated,
    n_controls = n_controls
  )

  fail <- function(message, extra = list()) {
    c(base_result, list(
      estimate = NA_real_, std_error = NA_real_, inf = NULL,
      contributor_ids = integer(), min_propensity = NA_real_,
      max_propensity = NA_real_, max_control_odds_weight = NA_real_,
      support_status = "FAILED", support_warning = message
    ), extra)
  }

  if (n_treated < 5L || n_controls < 5L) {
    return(fail("Fewer than five treated or control firms after pair/baseline completeness filters"))
  }

  if ("naics2_f" %in% controls) {
    treated_sectors <- unique(as.character(cell$x_naics2_f[cell$D == 1L]))
    control_sectors <- unique(as.character(cell$x_naics2_f[cell$D == 0L]))
    unsupported_treated <- setdiff(treated_sectors, control_sectors)
    if (length(unsupported_treated) > 0L) {
      return(fail(paste0(
        "No eligible controls in treated NAICS2 sector(s): ",
        paste(sort(unsupported_treated), collapse = ", ")
      )))
    }

    cell$x_naics2_f <- factor(
      as.character(cell$x_naics2_f),
      levels = levels(data$naics2_f)
    )
  }
  x_formula <- build_formula(paste0("x_", controls))
  x <- model.matrix(x_formula, data = cell)
  keep <- independent_columns(x, cell$D == 0L)
  x <- x[, keep, drop = FALSE]

  if (ncol(x) >= n_controls || ncol(x) >= n_treated + n_controls - 2L) {
    return(fail("Covariate dimension is too large for the available treated/control firms"))
  }

  ps_capture <- capture_conditions(
    glm.fit(x = x, y = cell$D, family = binomial())
  )
  ps <- if (inherits(ps_capture$value, "diagnostic_error")) {
    rep(NA_real_, nrow(cell))
  } else {
    ps_capture$value$fitted.values
  }
  max_control_weight <- if (any(cell$D == 0L & is.finite(ps))) {
    max(ps[cell$D == 0L] / pmax(1 - ps[cell$D == 0L], 1e-12), na.rm = TRUE)
  } else {
    NA_real_
  }

  estimator <- if (est_method == "dr") DRDID::drdid_panel else DRDID::reg_did_panel
  estimate_capture <- capture_conditions(
    estimator(
      y1 = cell$y1,
      y0 = cell$y0,
      D = cell$D,
      covariates = x,
      boot = FALSE,
      inffunc = TRUE
    )
  )

  if (inherits(estimate_capture$value, "diagnostic_error")) {
    return(fail(collapse_messages(c(
      ps_capture$warnings,
      estimate_capture$warnings,
      estimate_capture$value$message
    ))))
  }

  estimate <- estimate_capture$value
  n_global <- length(global_ids)
  n_cell <- nrow(cell)
  global_inf <- rep(0, n_global)
  global_inf[match(cell$firm_id, global_ids)] <-
    (n_global / n_cell) * as.numeric(estimate$att.inf.func)

  ps_problem <- if (
    inherits(ps_capture$value, "diagnostic_error") ||
      !isTRUE(ps_capture$value$converged) ||
      any(!is.finite(ps_capture$value$coefficients))
  ) {
    "Propensity model did not converge to finite coefficients"
  } else {
    character()
  }

  overlap_flags <- c(
    if (any(is.finite(ps)) && max(ps, na.rm = TRUE) >= OVERLAP_THRESHOLD)
      paste0("max propensity >= ", OVERLAP_THRESHOLD) else character(),
    ps_problem,
    estimate_capture$warnings
  )

  c(base_result, list(
    estimate = as.numeric(estimate$ATT),
    std_error = as.numeric(estimate$se),
    inf = global_inf,
    contributor_ids = cell$firm_id,
    min_propensity = if (any(is.finite(ps))) min(ps, na.rm = TRUE) else NA_real_,
    max_propensity = if (any(is.finite(ps))) max(ps, na.rm = TRUE) else NA_real_,
    max_control_odds_weight = max_control_weight,
    support_status = if (length(overlap_flags) == 0L) "PASS" else "WARNING",
    support_warning = collapse_messages(overlap_flags)
  ))
}

build_custom_mp <- function(data, outcome, est_method, controls) {
  global_ids <- sort(unique(data$firm_id))
  cohorts <- sort(unique(data$cohort[data$cohort >= MIN_TREATED_COHORT]))
  targets <- sort(unique(data$year))
  targets <- targets[targets > min(targets)]

  cells <- map(
    cohorts,
    function(g) map(targets, ~ cell_gminus1(
      data = data,
      outcome = outcome,
      cohort_year = g,
      target_year = .x,
      est_method = est_method,
      global_ids = global_ids,
      controls = controls
    ))
  ) |>
    list_flatten()

  att <- map_dbl(cells, "estimate")
  se <- map_dbl(cells, "std_error")
  group <- map_dbl(cells, "cohort_year")
  target <- map_dbl(cells, "target_year")

  inf <- matrix(NA_real_, nrow = length(global_ids), ncol = length(cells))
  for (j in seq_along(cells)) {
    if (!is.null(cells[[j]]$inf)) inf[, j] <- cells[[j]]$inf
  }

  n_global <- length(global_ids)
  finite_cells <- is.finite(att) & apply(inf, 2L, function(z) all(is.finite(z)))
  V <- matrix(NA_real_, nrow = length(cells), ncol = length(cells))
  if (any(finite_cells)) {
    V[finite_cells, finite_cells] <-
      crossprod(inf[, finite_cells, drop = FALSE]) / n_global
  }

  pre <- which(finite_cells & group > target)
  W <- NA_real_
  Wpval <- NA_real_
  if (length(pre) > 0L) {
    pre_v <- V[pre, pre, drop = FALSE]
    if (all(is.finite(pre_v)) && rcond(pre_v) > 1e-12) {
      W <- as.numeric(n_global * crossprod(att[pre], solve(pre_v, att[pre])))
      Wpval <- 1 - pchisq(W, length(pre))
    }
  }

  firm_meta <- data |>
    distinct(firm_id, cohort)
  baseline_complete <- data |>
    filter(year == cohort - 1L, cohort >= MIN_TREATED_COHORT) |>
    transmute(
      firm_id,
      complete_own_gminus1 = if_all(all_of(controls), ~ !is.na(.x))
    )
  firm_meta <- firm_meta |>
    left_join(baseline_complete, by = "firm_id") |>
    mutate(
      complete_own_gminus1 = coalesce(complete_own_gminus1, cohort == 0L),
      aggregation_cohort = if_else(
        cohort >= MIN_TREATED_COHORT & complete_own_gminus1,
        cohort,
        0L
      )
    )

  agg_data <- firm_meta |>
    transmute(
      firm_id,
      year = min(data$year),
      cohort = aggregation_cohort,
      .w = 1
    ) |>
    arrange(match(firm_id, global_ids))

  dp <- did::DIDparams(
    yname = outcome,
    tname = "year",
    idname = "firm_id",
    gname = "cohort",
    xformla = ~1,
    data = as.data.frame(agg_data),
    control_group = CONTROL_GROUP,
    anticipation = 0,
    alp = 0.05,
    bstrap = FALSE,
    biters = 0L,
    clustervars = "firm_id",
    cband = FALSE,
    print_details = FALSE,
    faster_mode = FALSE,
    pl = FALSE,
    cores = 1L,
    est_method = est_method,
    base_period = BASE_PERIOD,
    panel = FALSE,
    true_repeated_cross_sections = FALSE,
    n = n_global,
    nG = length(cohorts),
    nT = length(unique(data$year)),
    tlist = sort(unique(data$year)),
    glist = cohorts
  )
  dp$allow_unbalanced_panel <- TRUE

  mp <- did::MP(
    group = group,
    t = target,
    att = att,
    V_analytical = V,
    se = se,
    c = qnorm(0.975),
    inffunc = inf,
    n = n_global,
    W = W,
    Wpval = Wpval,
    alp = 0.05,
    DIDparams = dp
  )

  cell_table <- map_dfr(cells, function(x) tibble(
    cohort_year = x$cohort_year,
    target_year = x$target_year,
    event_time = x$event_time,
    baseline_year = x$baseline_year,
    outcome_base_year = x$outcome_base_year,
    n_treated = x$n_treated,
    n_controls = x$n_controls,
    estimate = x$estimate,
    std_error = x$std_error,
    estimable = is.finite(x$estimate) & is.finite(x$std_error),
    min_propensity = x$min_propensity,
    max_propensity = x$max_propensity,
    max_control_odds_weight = x$max_control_odds_weight,
    support_status = x$support_status,
    support_warning = x$support_warning
  ))

  contributor_ids <- sort(unique(unlist(map(cells, "contributor_ids"))))
  list(
    mp = mp,
    cells = cell_table,
    contributor_ids = contributor_ids,
    n_obs = data |>
      filter(firm_id %in% contributor_ids, is.finite(.data[[outcome]])) |>
      nrow(),
    n_firms = length(contributor_ids),
    firm_meta = firm_meta
  )
}

custom_diagnostic <- function(data, outcome, design) {
  controls <- controls_for_design(design, outcome)
  fit <- capture_conditions(
    build_custom_mp(data, outcome, design$est_method, controls)
  )
  if (inherits(fit$value, "diagnostic_error")) {
    comparison <- tibble(
      n_obs = NA_integer_, n_firms = NA_integer_, overall_att = NA_real_,
      overall_se = NA_real_, pretrend_wald = NA_real_, pretrend_p_value = NA_real_,
      estimable_att_gt = 0L, na_att_gt = NA_integer_, max_abs_att_gt = NA_real_,
      max_att_gt_se = NA_real_, overlap_support_warnings = collapse_messages(c(fit$warnings, fit$value$message)),
      status = "FAILED"
    )
    return(list(comparison = comparison, cells = tibble(), model = NULL, firm_meta = tibble()))
  }

  custom <- fit$value
  overall_capture <- capture_conditions(
    did::aggte(
      custom$mp,
      type = "group",
      na.rm = TRUE,
      bstrap = FALSE,
      cband = FALSE,
      clustervars = "firm_id"
    )
  )
  overall_error <- inherits(overall_capture$value, "diagnostic_error")
  overall <- if (overall_error) NULL else overall_capture$value
  estimated_cells <- custom$cells |> filter(estimable)

  warning_cells <- custom$cells |>
    filter(support_status != "PASS") |>
    count(support_status, support_warning, sort = TRUE) |>
    mutate(message = paste0(n, " cell(s): ", support_warning)) |>
    pull(message)

  comparison <- tibble(
    n_obs = custom$n_obs,
    n_firms = custom$n_firms,
    overall_att = if (is.null(overall)) NA_real_ else safe_scalar(overall$overall.att),
    overall_se = if (is.null(overall)) NA_real_ else safe_scalar(overall$overall.se),
    pretrend_wald = safe_scalar(custom$mp$W),
    pretrend_p_value = safe_scalar(custom$mp$Wpval),
    estimable_att_gt = nrow(estimated_cells),
    na_att_gt = nrow(custom$cells) - nrow(estimated_cells),
    max_abs_att_gt = if (nrow(estimated_cells)) max(abs(estimated_cells$estimate)) else NA_real_,
    max_att_gt_se = if (nrow(estimated_cells)) max(estimated_cells$std_error) else NA_real_,
    overlap_support_warnings = collapse_messages(c(
      fit$warnings,
      overall_capture$warnings,
      if (overall_error) overall_capture$value$message else character(),
      warning_cells
    )),
    status = if_else(is.null(overall), "FAILED_AGGREGATION", "ESTIMATED")
  )

  list(
    comparison = comparison,
    cells = custom$cells,
    model = custom$mp,
    firm_meta = custom$firm_meta
  )
}

if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop("Final analysis panel not found: ", ANALYSIS_PANEL_RDS)
}

panel_raw <- readRDS(ANALYSIS_PANEL_RDS)

diagnostic_results <- list()
result_index <- 1L

for (treatment_index in seq_len(nrow(TREATMENTS))) {
  treatment <- TREATMENTS[treatment_index, ]
  panel <- prepare_panel(panel_raw, treatment$cohort_var)

  for (outcome in DID_OUTCOMES) {
    for (design_index in seq_len(nrow(DESIGNS))) {
      design <- DESIGNS[design_index, ]
      cat(
        "\nDiagnostic:", treatment$treatment_label,
        "|", outcome,
        "|", design$design_label, "\n"
      )

      result <- if (design$implementation == "cohort_gminus1") {
        custom_diagnostic(panel, outcome, design)
      } else {
        stock_diagnostic(panel, outcome, design)
      }

      result$comparison <- bind_cols(
        treatment |>
          select(treatment_id, treatment_label, cohort_var),
        design |>
          select(
            design_order, design_id, design_label, implementation,
            est_method, control_profile, allow_unbalanced_panel, covariate_timing
          ),
        tibble(outcome = outcome),
        result$comparison
      )

      if (nrow(result$cells) > 0L) {
        result$cells <- bind_cols(
          treatment |>
            select(treatment_id, treatment_label, cohort_var),
          design |>
            select(design_order, design_id, design_label, est_method, control_profile),
          tibble(outcome = outcome),
          result$cells
        )
      }

      diagnostic_results[[result_index]] <- result
      result_index <- result_index + 1L
    }
  }
}

comparison_table <- map_dfr(diagnostic_results, "comparison") |>
  arrange(treatment_id, match(outcome, DID_OUTCOMES), design_order)

cell_table <- map_dfr(diagnostic_results, "cells")
if (nrow(cell_table) > 0L) {
  cell_table <- cell_table |>
    arrange(treatment_id, match(outcome, DID_OUTCOMES), design_order, cohort_year, target_year)
}

cohort_loss_table <- if (nrow(cell_table) == 0L) {
  tibble()
} else {
  cell_table |>
    filter(grepl("gminus1", design_id)) |>
    group_by(treatment_id, treatment_label, design_id, outcome, cohort_year, baseline_year) |>
    summarise(
      min_treated_firms_across_cells = min(n_treated, na.rm = TRUE),
      max_treated_firms_across_cells = max(n_treated, na.rm = TRUE),
      min_control_firms_across_cells = min(n_controls, na.rm = TRUE),
      max_control_firms_across_cells = max(n_controls, na.rm = TRUE),
      estimable_cells = sum(estimable),
      failed_cells = sum(!estimable),
      .groups = "drop"
    )
}

if (SAVE_DID_DIAGNOSTICS) {
  dir.create(DIAGNOSTIC_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(comparison_table, DIAGNOSTIC_COMPARISON_CSV)
  write_csv(cell_table, DIAGNOSTIC_CELL_CSV)
  write_csv(cohort_loss_table, DIAGNOSTIC_COHORT_LOSS_CSV)
  saveRDS(
    list(
      package_versions = c(
        did = as.character(packageVersion("did")),
        DRDID = as.character(packageVersion("DRDID"))
      ),
      designs = DESIGNS,
      comparison = comparison_table,
      cells = cell_table,
      cohort_loss = cohort_loss_table,
      models = map(diagnostic_results, "model")
    ),
    DIAGNOSTIC_BUNDLE_RDS
  )
}

cat("\nDesign comparison:\n")
print(
  comparison_table |>
    select(
      treatment_label, outcome, design_label, n_obs, n_firms,
      overall_att, overall_se, pretrend_p_value, estimable_att_gt,
      na_att_gt, max_abs_att_gt, max_att_gt_se, status
    ),
  n = Inf
)

if (SAVE_DID_DIAGNOSTICS) {
  cat("\nSaved design diagnostics to:", DIAGNOSTIC_OUTPUT_DIR, "\n")
}
