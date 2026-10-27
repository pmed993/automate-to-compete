#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Dynamic HHI heterogeneity figures
# ------------------------------------------------------------------------------
# Uses the saved cohort-gminus1 MP objects from 10. hhi_heterogeneity_did.R.
# It does not re-estimate ATT(g,t) cells. Instead, it:
#   1. aggregates those cells by event time for each competition regime;
#   2. constructs joint high-minus-low event-time contrasts after aligning
#      shared firms across cohort-specific competition regimes; and
#   3. saves two 5 x 2 faceted figures and their underlying CSV files.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(did)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(tibble)
  library(tidyr)
})

source("code/config/dynamic_figure_style.R")


# ---- Paths and settings ------------------------------------------------------
HHI_OUTPUT_DIR <- file.path(
  Sys.getenv("HHI_OUTPUT_ROOT", unset = "output"),
  "hhi_heterogeneity_did"
)
HHI_RESULTS_RDS <- file.path(HHI_OUTPUT_DIR, "hhi_heterogeneity_results.rds")
HHI_DYNAMIC_DIR <- file.path(HHI_OUTPUT_DIR, "dynamic_plots")

HHI_DYNAMIC_SUBGROUP_CSV <- file.path(
  HHI_DYNAMIC_DIR,
  "hhi_dynamic_competition_regimes.csv"
)
HHI_DYNAMIC_DIFFERENCE_CSV <- file.path(
  HHI_DYNAMIC_DIR,
  "hhi_dynamic_high_minus_low.csv"
)
HHI_DYNAMIC_OVERLAY_PNG <- file.path(
  HHI_DYNAMIC_DIR,
  "hhi_dynamic_competition_overlay.png"
)
HHI_DYNAMIC_DIFFERENCE_PNG <- file.path(
  HHI_DYNAMIC_DIR,
  "hhi_dynamic_high_minus_low.png"
)

HHI_DYNAMIC_MIN_EVENT <- as.integer(Sys.getenv(
  "HHI_DYNAMIC_MIN_EVENT",
  unset = "-4"
))
HHI_DYNAMIC_MAX_EVENT <- as.integer(Sys.getenv(
  "HHI_DYNAMIC_MAX_EVENT",
  unset = "6"
))
HHI_DYNAMIC_BOOTSTRAP_SEED <- as.integer(Sys.getenv(
  "HHI_DYNAMIC_BOOTSTRAP_SEED",
  unset = "24680"
))

TREATMENT_ORDER <- c("ai_adoption_2", "ai_adoption_3")
TREATMENT_LABELS <- c(
  ai_adoption_2 = "AI adoption ≥ 2",
  ai_adoption_3 = "Strong AI adoption = 3"
)

OUTCOME_LABELS <- c(
  log_emp = "Employment \n(log)",
  log_xopr = "Operating \ncosts (log)",
  log_labor_productivity = "Productivity \n(log)",
  log_sale = "Sales (log)",
  operating_profitability_w = "Operating \nprofitability"
)
OUTCOME_ORDER <- names(OUTCOME_LABELS)

REGIME_ORDER <- c("high_competition", "low_competition")
REGIME_LABELS <- c(
  high_competition = "High competition",
  low_competition = "Low competition"
)

TREATMENT_COLOURS <- c(
  ai_adoption_ge_2 = "#12436D",
  strong_ai_adoption_3 = "#7A1F3D"
)

REGIME_COLOURS_T2 <- c(
  high_competition = "#12436D",
  low_competition = "#4DD0D0"
)

REGIME_COLOURS_T3 <- c(
  high_competition = "#7A1F3D",
  low_competition = "#ff6b4d" 
)

REGIME_SHAPES <- c(
  high_competition = 16,  # High competition = circle
  low_competition = 17   # Low competition = triangle
)

REGIME_LINETYPES <- c(
  high_competition = "solid",
  low_competition = "22"
)

# The saved HHI models use compact internal treatment IDs. Map those IDs to
# the requested publication-style treatment colour names without changing any
# model keys or result files.
TREATMENT_STYLE_KEYS <- c(
  ai_adoption_2 = "ai_adoption_ge_2",
  ai_adoption_3 = "strong_ai_adoption_3"
)

OVERLAY_COLOURS <- c(
  "ai_adoption_2::high_competition" =
    unname(REGIME_COLOURS_T2[["high_competition"]]),
  "ai_adoption_2::low_competition" =
    unname(REGIME_COLOURS_T2[["low_competition"]]),
  "ai_adoption_3::high_competition" =
    unname(REGIME_COLOURS_T3[["high_competition"]]),
  "ai_adoption_3::low_competition" =
    unname(REGIME_COLOURS_T3[["low_competition"]])
)

stopifnot(
  identical(
    unname(REGIME_COLOURS_T2[["high_competition"]]),
    unname(TREATMENT_COLOURS[["ai_adoption_ge_2"]])
  ),
  identical(
    unname(REGIME_COLOURS_T3[["high_competition"]]),
    unname(TREATMENT_COLOURS[["strong_ai_adoption_3"]])
  )
)

if (
  !is.finite(HHI_DYNAMIC_MIN_EVENT) ||
    !is.finite(HHI_DYNAMIC_MAX_EVENT) ||
    HHI_DYNAMIC_MIN_EVENT >= HHI_DYNAMIC_MAX_EVENT
) {
  stop("The dynamic event-time bounds are invalid.")
}
if (!file.exists(HHI_RESULTS_RDS)) {
  stop(
    "Saved HHI model bundle not found: ",
    HHI_RESULTS_RDS,
    "\nRun code/main/10. hhi_hetero/10. hhi_heterogeneity_did.R first."
  )
}


# ---- Load and validate saved HHI models -------------------------------------
hhi_results <- readRDS(HHI_RESULTS_RDS)
required_bundle_items <- c("design", "att_results", "models")
missing_bundle_items <- setdiff(required_bundle_items, names(hhi_results))
if (length(missing_bundle_items) > 0L) {
  stop(
    "HHI result bundle is missing: ",
    paste(missing_bundle_items, collapse = ", ")
  )
}

model_meta <- hhi_results$att_results |>
  select(
    treatment_id,
    treatment_label,
    outcome,
    outcome_label,
    competition_regime,
    competition_label,
    n_obs,
    n_firms,
    n_clusters
  ) |>
  mutate(
    model_key = paste(
      treatment_id,
      outcome,
      competition_regime,
      sep = "::"
    )
  )

if (anyDuplicated(model_meta$model_key)) {
  stop("The HHI bundle contains duplicate treatment/outcome/regime models.")
}

expected_models <- crossing(
  treatment_id = TREATMENT_ORDER,
  outcome = OUTCOME_ORDER,
  competition_regime = REGIME_ORDER
) |>
  mutate(model_key = paste(treatment_id, outcome, competition_regime, sep = "::"))

missing_models <- anti_join(
  expected_models,
  model_meta,
  by = c("treatment_id", "outcome", "competition_regime", "model_key")
)
if (nrow(missing_models) > 0L) {
  stop(
    "Missing HHI models: ",
    paste(missing_models$model_key, collapse = ", ")
  )
}

saved_models <- hhi_results$models
if (length(saved_models) != nrow(model_meta)) {
  stop("The number of saved HHI models does not match the model metadata.")
}
if (is.null(names(saved_models)) || any(names(saved_models) == "")) {
  names(saved_models) <- model_meta$model_key
}
if (!all(model_meta$model_key %in% names(saved_models))) {
  stop("Saved HHI model names do not align with the model metadata.")
}
saved_models <- saved_models[model_meta$model_key]

saved_bootstrap_iterations <- hhi_results$design$bootstrap_iterations
if (is.null(saved_bootstrap_iterations)) {
  saved_bootstrap_iterations <- 1000L
}
HHI_DYNAMIC_BITERS <- as.integer(Sys.getenv(
  "HHI_DYNAMIC_BITERS",
  unset = as.character(saved_bootstrap_iterations)
))
if (!is.finite(HHI_DYNAMIC_BITERS) || HHI_DYNAMIC_BITERS < 2L) {
  stop("HHI_DYNAMIC_BITERS must be an integer of at least 2.")
}


# ---- Dynamic subgroup estimates ---------------------------------------------
aggregate_dynamic_model <- function(model, meta, bootstrap_seed) {
  if (!inherits(model, "MP")) {
    stop(meta$model_key, " is not a did::MP object.")
  }

  cluster_var <- model$DIDparams$cluster_vector_var
  cluster_vector <- as.character(model$DIDparams$cluster_vector)
  if (
    is.null(cluster_var) ||
      length(cluster_var) != 1L ||
      length(cluster_vector) != model$n ||
      anyNA(cluster_vector)
  ) {
    stop(meta$model_key, " is missing aligned market-cluster metadata.")
  }
  if (!identical(as.character(model$DIDparams$yname), meta$outcome)) {
    stop(meta$model_key, " has an outcome that does not match its metadata.")
  }

  set.seed(bootstrap_seed)
  dynamic <- did::aggte(
    model,
    type = "dynamic",
    min_e = HHI_DYNAMIC_MIN_EVENT,
    max_e = HHI_DYNAMIC_MAX_EVENT,
    na.rm = TRUE,
    bstrap = TRUE,
    biters = HHI_DYNAMIC_BITERS,
    cband = TRUE,
    clustervars = c("firm_id", cluster_var)
  )

  critical_value <- as.numeric(dynamic$crit.val.egt)
  if (length(critical_value) != 1L || !is.finite(critical_value)) {
    stop(meta$model_key, " did not return a valid simultaneous critical value.")
  }

  table <- tibble(
    treatment_id = meta$treatment_id,
    treatment_label = unname(TREATMENT_LABELS[[meta$treatment_id]]),
    outcome = meta$outcome,
    outcome_label = unname(OUTCOME_LABELS[[meta$outcome]]),
    competition_regime = meta$competition_regime,
    competition_label = unname(REGIME_LABELS[[meta$competition_regime]]),
    event_time = as.integer(dynamic$egt),
    estimate = as.numeric(dynamic$att.egt),
    std_error = as.numeric(dynamic$se.egt),
    simultaneous_critical_value = critical_value,
    ci_low = estimate - simultaneous_critical_value * std_error,
    ci_high = estimate + simultaneous_critical_value * std_error,
    n_obs = meta$n_obs,
    n_firms = meta$n_firms,
    n_clusters = n_distinct(cluster_vector),
    cluster_variable = cluster_var,
    bootstrap_iterations = HHI_DYNAMIC_BITERS
  )

  list(aggregation = dynamic, table = table)
}

dynamic_models <- vector("list", nrow(model_meta))
names(dynamic_models) <- model_meta$model_key

for (model_index in seq_len(nrow(model_meta))) {
  meta <- model_meta[model_index, ]
  cat("Dynamic HHI aggregation:", meta$model_key, "\n")
  dynamic_models[[meta$model_key]] <- aggregate_dynamic_model(
    model = saved_models[[meta$model_key]],
    meta = meta,
    bootstrap_seed = HHI_DYNAMIC_BOOTSTRAP_SEED + model_index - 1L
  )
}

dynamic_subgroups <- map_dfr(dynamic_models, "table") |>
  arrange(
    match(treatment_id, TREATMENT_ORDER),
    match(outcome, OUTCOME_ORDER),
    match(competition_regime, REGIME_ORDER),
    event_time
  )


# ---- Joint dynamic high-minus-low contrasts --------------------------------
dynamic_cluster_contrast <- function(
    high_dynamic,
    low_dynamic,
    treatment_id,
    outcome,
    bootstrap_seed
) {
  common_events <- intersect(high_dynamic$egt, low_dynamic$egt) |>
    sort()
  if (length(common_events) == 0L) {
    stop(treatment_id, "/", outcome, " has no common dynamic event times.")
  }

  high_columns <- match(common_events, high_dynamic$egt)
  low_columns <- match(common_events, low_dynamic$egt)
  high_if <- as.matrix(
    high_dynamic$inf.function$dynamic.inf.func.e[, high_columns, drop = FALSE]
  )
  low_if <- as.matrix(
    low_dynamic$inf.function$dynamic.inf.func.e[, low_columns, drop = FALSE]
  )
  high_cluster <- as.character(high_dynamic$DIDparams$cluster_vector)
  low_cluster <- as.character(low_dynamic$DIDparams$cluster_vector)
  high_id <- high_dynamic$DIDparams$data$firm_id
  low_id <- low_dynamic$DIDparams$data$firm_id
  high_cluster_var <- high_dynamic$DIDparams$cluster_vector_var
  low_cluster_var <- low_dynamic$DIDparams$cluster_vector_var

  if (
    nrow(high_if) != length(high_cluster) ||
      nrow(low_if) != length(low_cluster) ||
      length(high_id) != nrow(high_if) ||
      length(low_id) != nrow(low_if) ||
      anyDuplicated(high_id) || anyDuplicated(low_id) ||
      anyNA(high_cluster) ||
      anyNA(low_cluster) ||
      !identical(high_cluster_var, low_cluster_var)
  ) {
    stop(treatment_id, "/", outcome, " has misaligned cluster metadata.")
  }
  high_n <- nrow(high_if)
  low_n <- nrow(low_if)
  union_id <- sort(unique(c(high_id, low_id)))
  combined_n <- length(union_id)
  high_position <- match(high_id, union_id)
  low_position <- match(low_id, union_id)
  combined_if <- matrix(0, nrow = combined_n, ncol = length(common_events))
  combined_if[high_position, ] <- (combined_n / high_n) * high_if
  combined_if[low_position, ] <- combined_if[low_position, , drop = FALSE] -
    (combined_n / low_n) * low_if
  combined_cluster <- rep(NA_character_, combined_n)
  combined_cluster[high_position] <- high_cluster
  shared <- !is.na(combined_cluster[low_position])
  if (any(combined_cluster[low_position][shared] != low_cluster[shared])) {
    stop(treatment_id, "/", outcome, " has inconsistent firm cluster markets.")
  }
  combined_cluster[low_position] <- low_cluster
  n_clusters <- n_distinct(combined_cluster)

  bootstrap_params <- high_dynamic$DIDparams
  bootstrap_params$cluster_vector <- combined_cluster
  bootstrap_params$cluster_vector_var <- high_cluster_var
  bootstrap_params$clustervars <- c("firm_id", high_cluster_var)
  bootstrap_params$bstrap <- TRUE
  bootstrap_params$biters <- HHI_DYNAMIC_BITERS
  bootstrap_params$cband <- TRUE

  set.seed(bootstrap_seed)
  bootstrap <- getFromNamespace("mboot", "did")(
    combined_if,
    bootstrap_params,
    return_V = FALSE
  )

  high_estimate <- as.numeric(high_dynamic$att.egt[high_columns])
  low_estimate <- as.numeric(low_dynamic$att.egt[low_columns])
  estimate_difference <- high_estimate - low_estimate
  std_error <- as.numeric(bootstrap$se)
  critical_value <- as.numeric(bootstrap$crit.val)
  if (
    length(std_error) != length(common_events) ||
      length(critical_value) != 1L ||
      !is.finite(critical_value)
  ) {
    stop(treatment_id, "/", outcome, " returned invalid contrast inference.")
  }

  # did::mboot stores sqrt(G)-scaled cluster-bootstrap draws. Rescale them to
  # event-time estimate errors for finite-bootstrap empirical pointwise tests.
  bootstrap_errors <- as.matrix(bootstrap$bres) * sqrt(n_clusters) / combined_n
  p_value <- vapply(
    seq_along(common_events),
    function(event_index) {
      (
        1 + sum(
          abs(bootstrap_errors[, event_index]) >=
            abs(estimate_difference[event_index])
        )
      ) / (nrow(bootstrap_errors) + 1)
    },
    numeric(1L)
  )

  tibble(
    treatment_id,
    treatment_label = unname(TREATMENT_LABELS[[treatment_id]]),
    outcome,
    outcome_label = unname(OUTCOME_LABELS[[outcome]]),
    contrast = "High competition - Low competition",
    event_time = as.integer(common_events),
    high_att = high_estimate,
    low_att = low_estimate,
    estimate_difference,
    std_error_difference = std_error,
    p_value_difference = p_value,
    simultaneous_critical_value = critical_value,
    ci_low_difference =
      estimate_difference - simultaneous_critical_value * std_error_difference,
    ci_high_difference =
      estimate_difference + simultaneous_critical_value * std_error_difference,
    contrast_clusters = n_clusters,
    cluster_variable = high_cluster_var,
    bootstrap_iterations = HHI_DYNAMIC_BITERS
  )
}

contrast_grid <- crossing(
  treatment_id = TREATMENT_ORDER,
  outcome = OUTCOME_ORDER
) |>
  mutate(bootstrap_seed = HHI_DYNAMIC_BOOTSTRAP_SEED + 10000L + row_number())

dynamic_differences <- pmap_dfr(
  contrast_grid,
  function(treatment_id, outcome, bootstrap_seed) {
    high_key <- paste(treatment_id, outcome, "high_competition", sep = "::")
    low_key <- paste(treatment_id, outcome, "low_competition", sep = "::")
    dynamic_cluster_contrast(
      high_dynamic = dynamic_models[[high_key]]$aggregation,
      low_dynamic = dynamic_models[[low_key]]$aggregation,
      treatment_id = treatment_id,
      outcome = outcome,
      bootstrap_seed = bootstrap_seed
    )
  }
) |>
  arrange(
    match(treatment_id, TREATMENT_ORDER),
    match(outcome, OUTCOME_ORDER),
    event_time
  )


# ---- Plot data and structural QA --------------------------------------------
dynamic_subgroups_plot <- dynamic_subgroups |>
  mutate(
    series_colour_key = paste(
      treatment_id,
      competition_regime,
      sep = "::"
    ),
    treatment_label = factor(
      treatment_id,
      levels = TREATMENT_ORDER,
      labels = unname(TREATMENT_LABELS[TREATMENT_ORDER])
    ),
    outcome_label = factor(
      outcome,
      levels = OUTCOME_ORDER,
      labels = unname(OUTCOME_LABELS[OUTCOME_ORDER])
    ),
    competition_regime = factor(competition_regime, levels = REGIME_ORDER)
  )

dynamic_differences_plot <- dynamic_differences |>
  mutate(
    treatment_colour_key = unname(TREATMENT_STYLE_KEYS[treatment_id]),
    treatment_label = factor(
      treatment_id,
      levels = TREATMENT_ORDER,
      labels = unname(TREATMENT_LABELS[TREATMENT_ORDER])
    ),
    outcome_label = factor(
      outcome,
      levels = OUTCOME_ORDER,
      labels = unname(OUTCOME_LABELS[OUTCOME_ORDER])
    )
  )

event_breaks <- seq.int(HHI_DYNAMIC_MIN_EVENT, HHI_DYNAMIC_MAX_EVENT)


# ---- Figure 1: overlaid competition dynamics --------------------------------
competition_overlay_plot <- ggplot(
  dynamic_subgroups_plot,
  aes(
    x = event_time,
    y = estimate,
    colour = series_colour_key,
    fill = series_colour_key,
    shape = competition_regime,
    linetype = competition_regime,
    group = competition_regime
  )
) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.35) +
  geom_vline(
    xintercept = -0.5,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.10,
    colour = NA,
    linetype = 0
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.5) +
  facet_grid(
    rows = vars(outcome_label),
    cols = vars(treatment_label),
    scales = "free_y",
    space = "fixed"
  ) +
  scale_x_continuous(breaks = event_breaks) +
  scale_colour_manual(
    values = OVERLAY_COLOURS,
    guide = "none"
  ) +
  scale_fill_manual(
    values = OVERLAY_COLOURS,
    guide = "none"
  ) +
  scale_shape_manual(
    values = REGIME_SHAPES,
    breaks = REGIME_ORDER,
    labels = unname(REGIME_LABELS[REGIME_ORDER]),
    name = NULL
  ) +
  scale_linetype_manual(
    values = REGIME_LINETYPES,
    breaks = REGIME_ORDER,
    labels = unname(REGIME_LABELS[REGIME_ORDER]),
    name = NULL
  ) +
  guides(
    fill = "none",
    colour = "none",
    shape = guide_legend(
      override.aes = list(
        colour = "grey20",
        size = 2.2,
        linewidth = 0.9,
        alpha = 1
      )
    ),
    linetype = guide_legend()
  ) +
  labs(
#    title = "Dynamic effects of AI adoption by market competition",
    x = "Years relative to first AI adoption",
    y = "Dynamic ATT"
  ) +
  dynamic_figure_theme(legend_position = "bottom") +
  theme(
    legend.box.spacing = grid::unit(0.15, "lines"),
    legend.key.width = grid::unit(1.5, "lines")
  )


# ---- Figure 2: dynamic heterogeneity contrast -------------------------------
competition_difference_plot <- ggplot(
  dynamic_differences_plot,
  aes(
    x = event_time,
    y = estimate_difference,
    colour = treatment_colour_key,
    fill = treatment_colour_key,
    group = treatment_colour_key
  )
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_vline(
    xintercept = -0.5,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  geom_ribbon(
    aes(ymin = ci_low_difference, ymax = ci_high_difference),
    colour = NA,
    alpha = 0.14
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.5) +
  facet_grid(
    rows = vars(outcome_label),
    cols = vars(treatment_label),
    scales = "free_y",
    space = "fixed"
  ) +
  scale_x_continuous(breaks = event_breaks) +
  scale_colour_manual(values = TREATMENT_COLOURS, guide = "none") +
  scale_fill_manual(values = TREATMENT_COLOURS, guide = "none") +
  labs(
    title = "Dynamic difference between competition regimes",
    subtitle = "High-competition ATT minus low-competition ATT",
    x = "Years relative to first AI adoption",
    y = expression(Delta * "ATT"[e]),
    caption = paste0(
      "Shaded areas are simultaneous 95% confidence bands from a joint ",
      "cohort-g-1 NAICS3 regimes and a joint market-cluster multiplier bootstrap."
    )
  ) +
  dynamic_figure_theme(legend_position = "none")

validate_faceted_figure <- function(plot, label) {
  plot_build <- ggplot_build(plot)
  if (
    nrow(plot_build$layout$layout) !=
      length(TREATMENT_ORDER) * length(OUTCOME_ORDER) ||
      length(plot_build$layout$panel_scales_x) != 1L ||
      length(plot_build$layout$panel_scales_y) != length(OUTCOME_ORDER)
  ) {
    stop(
      label,
      " faceting QA failed: expected 10 panels, one x scale, and five y scales."
    )
  }
  invisible(TRUE)
}

validate_faceted_figure(competition_overlay_plot, "Competition overlay")
validate_faceted_figure(competition_difference_plot, "Competition difference")


# ---- Save --------------------------------------------------------------------
dir.create(HHI_DYNAMIC_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(dynamic_subgroups, HHI_DYNAMIC_SUBGROUP_CSV)
write_csv(dynamic_differences, HHI_DYNAMIC_DIFFERENCE_CSV)

ggsave(
  HHI_DYNAMIC_OVERLAY_PNG,
  competition_overlay_plot,
  width = 12,
  height = 12.5,
  dpi = 300,
  bg = "white"
)
ggsave(
  HHI_DYNAMIC_DIFFERENCE_PNG,
  competition_difference_plot,
  width = 12,
  height = 12.5,
  dpi = 300,
  bg = "white"
)

cat("\nSaved dynamic HHI outputs to:\n")
cat(" - ", HHI_DYNAMIC_SUBGROUP_CSV, "\n", sep = "")
cat(" - ", HHI_DYNAMIC_DIFFERENCE_CSV, "\n", sep = "")
cat(" - ", HHI_DYNAMIC_OVERLAY_PNG, "\n", sep = "")
cat(" - ", HHI_DYNAMIC_DIFFERENCE_PNG, "\n", sep = "")
