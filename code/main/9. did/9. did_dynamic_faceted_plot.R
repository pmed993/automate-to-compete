#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Combined dynamic Callaway--Sant'Anna DiD figure
# ------------------------------------------------------------------------------
# Rebuilds the preferred-specification event studies from the saved CSV. The
# figure has treatment definitions in columns and outcomes in rows. Free y
# scales are permitted across outcome rows, but facet_grid guarantees that the
# two treatment definitions share the same y scale within each outcome row.
# Running this script does not re-estimate any DiD models.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

source("code/config/dynamic_figure_style.R")


# ---- Paths -------------------------------------------------------------------
DID_OUTPUT_DIR <- Sys.getenv(
  "DID_OUTPUT_DIR", unset = file.path("output", "did_firm_outcomes")
)
DID_DYNAMIC_PLOT_DIR <- file.path(DID_OUTPUT_DIR, "dynamic_plots")
DID_EVENT_STUDY_CSV <- file.path(
  DID_OUTPUT_DIR,
  "did_event_study_best_spec.csv"
)
DID_DYNAMIC_FACETED_PNG <- file.path(
  DID_DYNAMIC_PLOT_DIR,
  "did_dynamic_faceted.png"
)


# ---- Expected layout ---------------------------------------------------------
TREATMENT_ORDER <- c("ai_adoption_ge_2", "strong_ai_adoption_3")
TREATMENT_LABELS <- c(
  ai_adoption_ge_2 = "AI adoption ≥ 2",
  strong_ai_adoption_3 = "Strong AI adoption = 3"
)

OUTCOME_LABELS <- c(
  log_emp = "Employment \n(log)",
  log_xopr = "Operating \ncosts (log)",
  log_labor_productivity = "Productivity \n(log)",
  log_sale = "Sales (log)",
  operating_profitability_w = "Operating \nprofitability"
)
OUTCOME_ORDER <- names(OUTCOME_LABELS)

TREATMENT_COLOURS <- c(
  ai_adoption_ge_2 = "#12436D",
  strong_ai_adoption_3 = "#7A1F3D"
)


# ---- Validation --------------------------------------------------------------
if (!file.exists(DID_EVENT_STUDY_CSV)) {
  stop(
    "Preferred-specification event-study file not found: ",
    DID_EVENT_STUDY_CSV,
    "\nRun code/main/9. did/9. did.R first."
  )
}

event_study <- read_csv(DID_EVENT_STUDY_CSV, show_col_types = FALSE)

required_columns <- c(
  "treatment_id", "spec_name", "outcome", "event_time", "estimate",
  "ci_low", "ci_high"
)
missing_columns <- setdiff(required_columns, names(event_study))
if (length(missing_columns) > 0L) {
  stop(
    "Event-study file is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

unexpected_treatments <- setdiff(unique(event_study$treatment_id), TREATMENT_ORDER)
unexpected_outcomes <- setdiff(unique(event_study$outcome), OUTCOME_ORDER)
if (length(unexpected_treatments) > 0L || length(unexpected_outcomes) > 0L) {
  stop(
    "Unexpected event-study series. Treatments: ",
    paste(unexpected_treatments, collapse = ", "),
    "; outcomes: ",
    paste(unexpected_outcomes, collapse = ", ")
  )
}

expected_series <- tidyr::crossing(
  treatment_id = TREATMENT_ORDER,
  outcome = OUTCOME_ORDER
)
observed_series <- event_study |>
  distinct(treatment_id, outcome)
missing_series <- anti_join(
  expected_series,
  observed_series,
  by = c("treatment_id", "outcome")
)
if (nrow(missing_series) > 0L) {
  stop(
    "Missing treatment/outcome series: ",
    paste0(
      missing_series$treatment_id,
      "/",
      missing_series$outcome,
      collapse = ", "
    )
  )
}

duplicate_points <- event_study |>
  count(treatment_id, outcome, event_time) |>
  filter(n != 1L)
if (nrow(duplicate_points) > 0L) {
  stop("Each treatment/outcome/event-time point must appear exactly once.")
}

if (
  any(!is.finite(event_study$event_time)) ||
    any(!is.finite(event_study$estimate)) ||
    any(!is.finite(event_study$ci_low)) ||
    any(!is.finite(event_study$ci_high)) ||
    any(event_study$ci_low > event_study$estimate) ||
    any(event_study$ci_high < event_study$estimate)
) {
  stop("Event-study estimates or confidence-band limits are invalid.")
}

if (n_distinct(event_study$spec_name) != 1L) {
  stop("The combined figure must contain exactly one DiD specification.")
}


# ---- Figure data -------------------------------------------------------------
plot_data <- event_study |>
  mutate(
    treatment_id = factor(treatment_id, levels = TREATMENT_ORDER),
    treatment_label = factor(
      unname(TREATMENT_LABELS[as.character(treatment_id)]),
      levels = unname(TREATMENT_LABELS[TREATMENT_ORDER])
    ),
    outcome_label = factor(
      unname(OUTCOME_LABELS[outcome]),
      levels = unname(OUTCOME_LABELS[OUTCOME_ORDER])
    )
  ) |>
  arrange(outcome_label, treatment_id, event_time)

event_breaks <- sort(unique(plot_data$event_time))


# ---- Combined 5 x 2 event-study figure --------------------------------------
dynamic_faceted_plot <- ggplot(
  plot_data,
  aes(
    x = event_time,
    y = estimate,
    colour = treatment_id,
    fill = treatment_id,
    group = treatment_id
  )
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_vline(
    xintercept = -0.5,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.35
  ) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.16,
    colour = NA
  ) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.25) +
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
    x = "Years relative to first AI adoption",
    y = "Dynamic ATT"
  ) +
  dynamic_figure_theme(legend_position = "none")

# `facet_grid(scales = "free_y")` must create one common x scale and one y scale
# per outcome row. If it creates 10 y scales, the two treatment columns would no
# longer be directly comparable within outcomes.
plot_build <- ggplot_build(dynamic_faceted_plot)
if (
  nrow(plot_build$layout$layout) !=
    length(TREATMENT_ORDER) * length(OUTCOME_ORDER) ||
    length(plot_build$layout$panel_scales_x) != 1L ||
    length(plot_build$layout$panel_scales_y) != length(OUTCOME_ORDER)
) {
  stop("Faceting QA failed: expected 10 panels, one x scale, and five y scales.")
}


# ---- Save --------------------------------------------------------------------
dir.create(DID_DYNAMIC_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

ggsave(
  DID_DYNAMIC_FACETED_PNG,
  dynamic_faceted_plot,
  width = 12,
  height = 12.5,
  dpi = 300,
  bg = "white"
)

cat("Saved combined dynamic DiD figure to:\n")
cat(" - ", DID_DYNAMIC_FACETED_PNG, "\n", sep = "")
