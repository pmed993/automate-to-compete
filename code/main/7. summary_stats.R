#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Summary statistics for the contemporaneously matched analysis panel
# ------------------------------------------------------------------------------
# The main descriptive table and the DiD use only Compustat firm-years with a
# contemporaneously matched, scored EDGAR annual filing. Market HHI is still
# constructed from the full Compustat denominator before joining this sample.
#
# The script:
# 1. produce the main descriptive table for outcomes and controls;
# 2. summarise and visualise the fixed-2015 HHI competition regimes;
# 3. compare firm characteristics by adoption status;
# 4. compare ever-treated firms at g-1 with never-treated firms at first observation;
# 5. trace both AI-adoption definitions over time;
# 6. trace mean AI scores across key NAICS2 sectors;
# 7. compare AI adoption across the largest NAICS2 sectors;
# 8. compare pre-treatment characteristic distributions for adopters and non-adopters;
# 9. compare contemporaneous firm-characteristic distributions by AI-adoption status; and
# 10. decompose treated firm-years into strong and not-yet-strong adoption.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(dplyr)
library(tidyr)
library(ggplot2)


# ---- Settings ----------------------------------------------------------------
SUMMARY_STATS_OUTPUT_DIR <- file.path(OUTPUT_DIR, "summary_stats")
SUMMARY_STATS_BUNDLE_RDS <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "summary_stats_bundle.rds"
)
SUMMARY_STATS_MAIN_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "table_3_5_matched_panel.csv"
)
SUMMARY_SAMPLE_AUDIT_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "sample_construction_audit.csv"
)
PRE_TREATMENT_BALANCE_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "pre_treatment_balance.csv"
)
PRE_TREATMENT_BALANCE_AUDIT_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "pre_treatment_balance_audit.csv"
)
PRE_TREATMENT_BALANCE_BY_HHI_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "pre_treatment_balance_by_hhi.csv"
)
BASELINE_COMPETITION_SUMMARY_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "baseline_competition_summary.csv"
)
COMPETITION_REGIME_SUMMARY_CSV <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "competition_regime_summary.csv"
)
MATCHED_ANALYSIS_PANEL_RDS <- file.path(
  INPUT_DIR,
  "compustat_ai_analysis_matched_panel.rds"
)
MERGED_PANEL_RDS <- file.path(INPUT_DIR, "compustat_ai_panel.rds")
FULL_COMPUSTAT_PANEL_RDS <- file.path(
  INPUT_DIR,
  "compustat_annual_panel.rds"
)
KEY_DISTRIBUTION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "key_variable_distributions_by_adoption_status.png"
)
ADOPTION_PREVALENCE_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adoption_prevalence_over_time.png"
)
ADOPTION_BY_COMPETITION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adoption_prevalence_by_competition.png"
)
ADOPTION_BY_ANNUAL_COMPETITION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adoption_prevalence_by_annual_competition.png"
)
ADOPTER_COMPOSITION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adopter_composition_over_time.png"
)
ADOPTION_OVERVIEW_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adoption_prevalence_and_composition.png"
)
KEY_SECTOR_SCORE_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_score_evolution_key_sectors.png"
)
INDUSTRY_ADOPTION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "ai_adoption_across_major_industries.png"
)
ADOPTION_STATUS_DISTRIBUTION_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "firm_characteristic_distributions_by_ai_adoption.png"
)
NAICS3_HHI_HISTOGRAM_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "naics3_hhi_histogram.png"
)
FIRM_HHI_HISTOGRAM_PNG <- file.path(
  SUMMARY_STATS_OUTPUT_DIR,
  "firm_hhi_histogram.png"
)

SAVE_SUMMARY_BUNDLE <- TRUE
SAVE_SUMMARY_FIGURES <- TRUE

HHI_BASE_YEAR <- HHI_PRE_TREATMENT_BASE_YEAR
HHI_COMPETITION_CUTOFF <- 1800
HHI_HISTOGRAM_BINWIDTH <- 250
FIRM_HHI_HISTOGRAM_YEAR <- 2025L

# Fixed sectors used for the long-run mean-score comparison. Construction is
# retained even though its adoption rate is low because it is economically
# important and provides an informative contrast with technology-intensive
# sectors.
KEY_SECTOR_CODES <- c("21", "23", "31-33", "44-45", "51", "52", "54", "56", "61", "62")
KEY_SECTOR_LABELS <- c(
  "21" = "Mining",
  "23" = "Construction",
  "31-33" = "Manufacturing",
  "44-45" = "Retail Trade",
  "51" = "Information",
  "52" = "Finance",
  "54" = "Professional & Scientific",
  "56" = "Admin Services",
  "61" = "Education",
  "62" = "Health Care"
)

# The industry comparison uses the latest three years and selects sectors by
# sample size, not by their AI-adoption rate.
INDUSTRY_COMPARISON_WINDOW_YEARS <- 2L
N_LARGEST_NAICS2_SECTORS <- 13L

KEY_DISTRIBUTION_LABELS <- c(
  log_at = "Firm size (log assets)",
  roa = "ROA",
  cash_ratio = "Cash ratio",
  leverage = "Leverage"
)

ADOPTION_STATUS_DISTRIBUTION_LABELS <- c(
  log_at = "Firm size (log assets)",
  log_labor_productivity = "Labour productivity (log)",
  log_emp = "Employment (log)",
  log_avg_wage = "Average wage (log)",
  rd_intensity_y_w = "R&D activity (winsorized intensity)"
)

# ---- Helpers -----------------------------------------------------------------
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) NA_real_ else as.numeric(quantile(x, probs = prob, na.rm = TRUE, names = FALSE))
}

round_numeric_cols <- function(data, digits = 3) {
  data |>
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

descriptive_table <- function(data, vars, labels) {
  data |>
    select(all_of(vars)) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(variable) |>
    summarise(
      n = sum(!is.na(value)),
      mean = safe_mean(value),
      sd = safe_sd(value),
      min = safe_quantile(value, 0),
      p25 = safe_quantile(value, 0.25),
      median = safe_quantile(value, 0.50),
      p75 = safe_quantile(value, 0.75),
      max = safe_quantile(value, 1),
      .groups = "drop"
    ) |>
    mutate(variable = recode(variable, !!!labels)) |>
    rename(
      Variable = variable,
      N = n,
      Mean = mean,
      SD = sd,
      Min = min,
      P25 = p25,
      Median = median,
      P75 = p75,
      Max = max
    ) |>
    round_numeric_cols()
}

group_summary_table <- function(data, vars, labels) {
  data |>
    filter(!is.na(ai_adopted)) |>
    select(ai_adopted, all_of(vars)) |>
    pivot_longer(
      cols = all_of(vars),
      names_to = "characteristic",
      values_to = "value"
    ) |>
    group_by(ai_adopted, characteristic) |>
    summarise(
      n = sum(!is.na(value)),
      mean = safe_mean(value),
      sd = safe_sd(value),
      median = safe_quantile(value, 0.50),
      p25 = safe_quantile(value, 0.25),
      p75 = safe_quantile(value, 0.75),
      .groups = "drop"
    ) |>
    mutate(characteristic = recode(characteristic, !!!labels)) |>
    round_numeric_cols()
}

firm_char_density_plot <- function(data, vars, labels) {
  plot_data <- data |>
    filter(ai_adopted %in% c(0L, 1L)) |>
    mutate(
      adoption_group = factor(
        ai_adopted,
        levels = c(0L, 1L),
        labels = c("Non-adopter", "AI adopter")
      )
    ) |>
    select(adoption_group, all_of(vars)) |>
    pivot_longer(
      cols = all_of(vars),
      names_to = "characteristic",
      values_to = "value"
    ) |>
    filter(!is.na(value)) |>
    mutate(
      characteristic = factor(
        recode(characteristic, !!!labels),
        levels = unname(labels[vars])
      )
    )

  ggplot(
    plot_data,
    aes(x = value, fill = adoption_group, colour = adoption_group)
  ) +
    geom_density(alpha = 0.22, linewidth = 0.9, adjust = 1.05) +
    facet_wrap(~ characteristic, scales = "free", ncol = 2) +
    scale_colour_manual(
      values = c("Non-adopter" = "#9E9E9E", "AI adopter" = "#2C7FB8"),
      name = NULL
    ) +
    scale_fill_manual(
      values = c("Non-adopter" = "#9E9E9E", "AI adopter" = "#2C7FB8"),
      name = NULL
    ) +
    labs(
      title = "Distribution of firm characteristics by AI adoption",
      subtitle = "Firm-year observations classified by contemporaneous AI-adoption status",
      x = NULL,
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

pre_treatment_adopter_table <- function(data, vars, labels) {
  balance_rows <- data |>
    mutate(
      balance_group = if_else(
        ever_treated == 1L,
        "ever_treated",
        "never_treated"
      )
    ) |>
    select(balance_group, all_of(vars)) |>
    pivot_longer(
      cols = all_of(vars),
      names_to = "characteristic",
      values_to = "value"
    ) |>
    group_by(balance_group, characteristic) |>
    summarise(
      n = sum(!is.na(value)),
      mean = safe_mean(value),
      sd = safe_sd(value),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = balance_group,
      values_from = c(n, mean, sd),
      names_glue = "{balance_group}_{.value}"
    ) |>
    mutate(
      variable_order = match(characteristic, vars),
      diff = ever_treated_mean - never_treated_mean,
      pooled_sd = sqrt(
        (
          (never_treated_n - 1L) * never_treated_sd^2 +
            (ever_treated_n - 1L) * ever_treated_sd^2
        ) /
          (never_treated_n + ever_treated_n - 2L)
      ),
      normalized_diff = if_else(
        is.finite(pooled_sd) & pooled_sd > 0,
        diff / pooled_sd,
        NA_real_
      ),
      characteristic = recode(characteristic, !!!labels)
    ) |>
    arrange(variable_order) |>
    rename(
      `Variable (baseline)` = characteristic,
      `Never-treated (mean)` = never_treated_mean,
      `Ever-treated (mean)` = ever_treated_mean,
      Diff = diff,
      `Norm. diff` = normalized_diff
    ) |>
    select(
      `Variable (baseline)`,
      `Never-treated (mean)`,
      `Ever-treated (mean)`,
      Diff,
      `Norm. diff`
    ) |>
    round_numeric_cols()

  # The final N row counts firms with an observed baseline row. The separate
  # audit reports firms whose exact g-1 observation is unavailable.
  group_counts <- data |>
    filter(baseline_observed) |>
    count(ever_treated, name = "n_firms")

  never_treated_n <- group_counts$n_firms[group_counts$ever_treated == 0L]
  ever_treated_n <- group_counts$n_firms[group_counts$ever_treated == 1L]

  bind_rows(
    balance_rows,
    tibble::tibble(
      `Variable (baseline)` = "N (firms with baseline)",
      `Never-treated (mean)` = as.numeric(never_treated_n),
      `Ever-treated (mean)` = as.numeric(ever_treated_n),
      Diff = NA_real_,
      `Norm. diff` = NA_real_
    )
  )
}


# ---- Load matched analysis sample and full Compustat HHI universe -------------
if (!file.exists(MATCHED_ANALYSIS_PANEL_RDS)) {
  stop(
    "Compustat-EDGAR matched analysis panel not found: ",
    MATCHED_ANALYSIS_PANEL_RDS,
    ". Run 4. build_or_load_panel_data.R first."
  )
}
if (!file.exists(FULL_COMPUSTAT_PANEL_RDS)) {
  stop(
    "Full Compustat panel not found: ", FULL_COMPUSTAT_PANEL_RDS,
    ". Run 3. get_panel_data/1. build_compustat_annual_panel.R first."
  )
}

# Construct contemporaneous sales-based NAICS3 HHI using the full Compustat
# market denominator before restricting to the Compustat-EDGAR matched sample.
# HHI is on the conventional 0--10,000 scale.
full_panel_hhi_source <- readRDS(FULL_COMPUSTAT_PANEL_RDS) |>
  transmute(
    cik = as.character(cik),
    year = as.integer(year),
    naics3 = as.character(naics3),
    sale = as.numeric(sale)
  )

hhi_naics3_by_year <- full_panel_hhi_source |>
  filter(
    !is.na(year),
    year >= ANALYSIS_START_YEAR,
    year <= ANALYSIS_END_YEAR,
    !is.na(naics3),
    nzchar(naics3),
    is.finite(sale),
    sale > 0
  ) |>
  group_by(year, naics3) |>
  summarise(
    hhi_naics3 = sum((100 * sale / sum(sale))^2),
    .groups = "drop"
  )

if (
  nrow(hhi_naics3_by_year) == 0L ||
  any(!is.finite(hhi_naics3_by_year$hhi_naics3)) ||
  any(hhi_naics3_by_year$hhi_naics3 <= 0) ||
  any(hhi_naics3_by_year$hhi_naics3 > 10000 + 1e-8)
) {
  stop("NAICS3 HHI construction failed: values must lie in (0, 10,000].")
}

if (!file.exists(ANALYSIS_PANEL_RDS) || !file.exists(MERGED_PANEL_RDS)) {
  stop("The analysis or merged panel is missing; rebuild the panels first.")
}
eligible_compustat_input <- build_final_analysis_panel(readRDS(MERGED_PANEL_RDS))
causal_analysis_input <- readRDS(ANALYSIS_PANEL_RDS)
matched_analysis_input <- readRDS(MATCHED_ANALYSIS_PANEL_RDS)

if (anyNA(matched_analysis_input$ai_score)) {
  stop(
    "The matched analysis sample contains missing `ai_score` values; ",
    "use only contemporaneously matched Compustat-EDGAR firm-years."
  )
}

panel_summary <- matched_analysis_input |>
  mutate(
    cik = as.character(cik),
    year = as.integer(year),
    naics3 = as.character(naics3),
    rd_reporter = as.integer(rd_reporter),
    repurchases = case_when(
      is.na(prstkc) ~ 0,
      prstkc < 0 ~ NA_real_,
      TRUE ~ prstkc
    ),
    repurchase_binary = case_when(
      is.na(prstkc) ~ 0L,
      prstkc < 0 ~ NA_integer_,
      prstkc > 0 ~ 1L,
      TRUE ~ 0L
    ),
    repurchase_intensity_at = if_else(
      !is.na(at_l1) & at_l1 > 0,
      repurchases / at_l1,
      NA_real_
    ),
    ever_treated = as.integer(ai_adoption_year > 0L)
  ) |>
  filter(
    !is.na(year),
    year >= ANALYSIS_START_YEAR,
    year <= ANALYSIS_END_YEAR
  ) |>
  left_join(
    hhi_naics3_by_year,
    by = c("year", "naics3")
  )

if (
  nrow(panel_summary) != nrow(matched_analysis_input) ||
  nrow(distinct(panel_summary, cik, year)) != nrow(panel_summary)
) {
  stop(
    "Matched-sample QA failed: filtering or the HHI join changed the ",
    "firm-year sample."
  )
}

panel_descriptive <- panel_summary

if (
  nrow(panel_descriptive) != nrow(causal_analysis_input) ||
  nrow(distinct(panel_descriptive, cik, year)) != nrow(panel_descriptive) ||
  nrow(distinct(eligible_compustat_input, cik, year)) !=
    nrow(eligible_compustat_input) ||
  anyNA(causal_analysis_input$ai_score) ||
  !setequal(
    paste(as.character(causal_analysis_input$cik),
          as.integer(causal_analysis_input$year)),
    paste(panel_summary$cik, panel_summary$year)
  )
) {
  stop("Matched descriptive and DiD source samples disagree.")
}

summary_sample_audit <- tibble::tibble(
  dataset = c(
    "Eligible Compustat sample (year and exchange filters)",
    "Treatment-history construction pool (known filing history)",
    "Contemporaneously matched analysis / descriptive sample",
    "Full Compustat panel used to construct HHI"
  ),
  firm_years = c(
    nrow(eligible_compustat_input),
    sum(eligible_compustat_input$has_ai_history == TRUE),
    nrow(panel_descriptive),
    nrow(full_panel_hhi_source)
  ),
  firms = c(
    n_distinct(eligible_compustat_input$cik),
    n_distinct(eligible_compustat_input$cik[
      eligible_compustat_input$has_ai_history == TRUE
    ]),
    n_distinct(panel_descriptive$cik),
    n_distinct(full_panel_hhi_source$cik)
  )
)


panel_score_summary <- panel_summary |>
  filter(!is.na(ai_score))

# ---- Main descriptive table ---------------------------------------------------
overall_var_labels <- c(
  emp = "Employment (000s)",
  sale = "Sales ($m)",
  xopr = "Operating costs ($m)",
  labor_productivity = "Labour productivity (sales/emp)",
  operating_profitability_w = "Operating profit (OIBDP/assets)",
  at = "Total assets ($m)",
  firm_age = "Firm age (years)",
  cash_ratio = "Cash ratio",
  leverage = "Leverage",
  roa = "ROA",
  capx_intensity_y_w = "CAPX intensity",
  rd_intensity_y_w = "R&D intensity (reporters only)",
  rd_reporter = "R&D reporter (0/1)",
  hhi_naics3 = "HHI (NAICS3)",
  ai_score = "AI adoption score (1/2/3)"
)

overall_var_sections <- c(
  emp = "Outcomes",
  sale = "Outcomes",
  xopr = "Outcomes",
  labor_productivity = "Outcomes",
  operating_profitability_w = "Outcomes",
  at = "Covariates",
  firm_age = "Covariates",
  cash_ratio = "Covariates",
  leverage = "Covariates",
  roa = "Covariates",
  capx_intensity_y_w = "Covariates",
  rd_intensity_y_w = "Covariates",
  rd_reporter = "Covariates",
  hhi_naics3 = "Covariates",
  ai_score = "AI measure"
)

firm_char_var_labels <- c(
  aiie = "Industry AI exposure (AIIE)",
  log_at = "Firm size (log assets)",
  log_market_cap = "Market value (log)",
  log_sale = "Sales (log)",
  log_emp = "Employment (log)",
  log_avg_wage = "Average wage (log)",
  log_labor_productivity = "Labour productivity (log)",
  cash_ratio = "Cash ratio",
  roa = "ROA",
  leverage = "Leverage",
  tobins_q = "Tobin's Q",
  repurchase_binary = "Repurchase indicator",
  repurchase_intensity_at = "Repurchases / lagged assets",
  rd_intensity_y_w = "R&D intensity (winsorized)",
  capx_intensity_y_w = "CAPX intensity (winsorized)",
  total_inv_intensity_y_w = "Total investment intensity (winsorized)"
)

overall_summary <- descriptive_table(
  panel_descriptive,
  names(overall_var_labels),
  overall_var_labels
) |>
  mutate(
    summary_order = match(Variable, unname(overall_var_labels)),
    Section = unname(
      overall_var_sections[names(overall_var_labels)[summary_order]]
    ),
    .before = Variable
  ) |>
  arrange(summary_order) |>
  select(-summary_order)

overall_summary_notes <- paste0(
  "This table describes the ",
  format(nrow(panel_descriptive), big.mark = ","),
  " unique 2015–2025 firm-years with a contemporaneously matched and scored ",
  "EDGAR annual filing. This is the parent sample for all DID specifications; ",
  "cohort, outcome-pair, and covariate requirements reduce their N. ",
  "R&D intensity is winsorized XRD divided by beginning assets and is reported ",
  "only when Compustat XRD and a valid denominator are observed (N = ",
  format(sum(!is.na(panel_descriptive$rd_intensity_y_w)), big.mark = ","),
  "). The determinants regressions instead set lagged R&D intensity to zero ",
  "only for lagged non-reporters and include a separate lagged R&D-reporter ",
  "indicator; undefined ratios for reporters remain missing."
)


# ---- Competition regime summary ----------------------------------------------
# Match the HHI heterogeneity design: assign each firm to its fixed 2015 NAICS3
# market and classify that market using its 2015 sales-based HHI.
hhi_base_year <- hhi_naics3_by_year |>
  filter(year == HHI_BASE_YEAR) |>
  transmute(
    baseline_naics3 = naics3,
    hhi_base = hhi_naics3
  )

firm_baseline_market <- full_panel_hhi_source |>
  filter(
    year == HHI_BASE_YEAR,
    !is.na(cik), cik != "",
    !is.na(naics3), naics3 != ""
  ) |>
  transmute(
    cik,
    baseline_naics3 = naics3
  )

if (
  anyDuplicated(hhi_base_year$baseline_naics3) ||
  anyDuplicated(firm_baseline_market$cik)
) {
  stop("Fixed-2015 HHI lookup contains duplicate market or firm keys.")
}

competition_regime_firm_assignment <- panel_summary |>
  distinct(cik) |>
  left_join(firm_baseline_market, by = "cik") |>
  left_join(hhi_base_year, by = "baseline_naics3") |>
  mutate(
    competition_regime = case_when(
      !is.finite(hhi_base) ~ NA_character_,
      hhi_base <= HHI_COMPETITION_CUTOFF ~ "high_competition",
      hhi_base > HHI_COMPETITION_CUTOFF ~ "low_competition"
    ),
    competition_regime = factor(
      competition_regime,
      levels = c("high_competition", "low_competition")
    )
  )

competition_regime_coverage <- competition_regime_firm_assignment |>
  summarise(
    analysis_firms = n(),
    classified_firms = sum(!is.na(competition_regime)),
    unclassified_firms = sum(is.na(competition_regime)),
    classified_share = classified_firms / analysis_firms
  )

competition_regime_panel <- panel_summary |>
  inner_join(
    competition_regime_firm_assignment |>
      filter(!is.na(competition_regime)) |>
      select(cik, baseline_naics3, hhi_base, competition_regime),
    by = "cik"
  )

competition_regime_firm_stats <- competition_regime_panel |>
  distinct(cik, competition_regime, baseline_naics3, hhi_base) |>
  group_by(competition_regime) |>
  summarise(
    n_firms = n(),
    mean_hhi = mean(hhi_base),
    median_hhi = median(hhi_base),
    n_naics3_markets = n_distinct(baseline_naics3),
    .groups = "drop"
  )

competition_regime_stats <- competition_regime_panel |>
  group_by(competition_regime) |>
  summarise(
    n_firm_years = n(),
    # This cross-section describes the matched filing score in each firm-year.
    # The later-starting absorbing DiD treatment is a different measure.
    n_ai_adopting_firm_years = sum(ai_score >= 2L),
    ai_adoption_rate_pct = 100 * mean(ai_score >= 2L),
    mean_log_assets = safe_mean(log_at),
    .groups = "drop"
  ) |>
  left_join(
    competition_regime_firm_stats,
    by = "competition_regime"
  ) |>
  mutate(share_firms = n_firms / sum(n_firms))

if (
  nrow(competition_regime_stats) != 2L ||
  any(!is.finite(competition_regime_stats$mean_hhi)) ||
  abs(sum(competition_regime_stats$share_firms) - 1) > 1e-10
) {
  stop("Competition-regime summary failed its coverage or share QA.")
}

competition_regime_stat_labels <- c(
  n_firms = "N firms",
  share_firms = "Share of firms",
  n_firm_years = "N firm-years",
  mean_hhi = "Mean HHI",
  median_hhi = "Median HHI",
  n_naics3_markets = "N NAICS3 markets",
  n_ai_adopting_firm_years = "N AI-adopting firm-years (score ≥2)",
  ai_adoption_rate_pct = "AI adoption rate (score ≥2, %)",
  mean_log_assets = "Mean firm size (log assets)"
)

competition_regime_summary <- competition_regime_stats |>
  select(
    competition_regime,
    all_of(names(competition_regime_stat_labels))
  ) |>
  pivot_longer(
    cols = -competition_regime,
    names_to = "statistic",
    values_to = "value"
  ) |>
  mutate(
    statistic_order = match(
      statistic,
      names(competition_regime_stat_labels)
    ),
    statistic = recode(statistic, !!!competition_regime_stat_labels)
  ) |>
  arrange(statistic_order) |>
  select(-statistic_order) |>
  pivot_wider(
    names_from = competition_regime,
    values_from = value
  ) |>
  rename(
    Statistic = statistic,
    `High competition (HHI ≤ 1,800)` = high_competition,
    `Low competition (HHI > 1,800)` = low_competition
  ) |>
  round_numeric_cols()

hhi_histogram_data <- competition_regime_panel |>
  distinct(baseline_naics3, hhi_base, competition_regime) |>
  mutate(
    competition_regime = factor(
      competition_regime,
      levels = c("high_competition", "low_competition"),
      labels = c(
        "High competition (HHI ≤ 1,800)",
        "Low competition (HHI > 1,800)"
      )
    )
  )

if (
  nrow(hhi_histogram_data) !=
    sum(competition_regime_stats$n_naics3_markets)
) {
  stop("HHI histogram data must contain one row per classified NAICS3 market.")
}

hhi_histogram_plot <- function(
  data,
  title,
  subtitle,
  y_label,
  grey_bars = FALSE
) {
  competition_region_data <- tibble(
    competition_regime = factor(
      c(
        "High competition (HHI ≤ 1,800)",
        "Low competition (HHI > 1,800)"
      ),
      levels = levels(data$competition_regime)
    ),
    xmin = c(0, HHI_COMPETITION_CUTOFF),
    xmax = c(HHI_COMPETITION_CUTOFF, 10000),
    ymin = -Inf,
    ymax = Inf
  )

  competition_region_layer <- if (grey_bars) {
    geom_rect(
      data = competition_region_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = competition_regime
      ),
      inherit.aes = FALSE,
      alpha = 0.45
    )
  } else {
    list(
      annotate(
        "rect",
        xmin = 0,
        xmax = HHI_COMPETITION_CUTOFF,
        ymin = -Inf,
        ymax = Inf,
        fill = "#D9EEF7",
        alpha = 0.45
      ),
      annotate(
        "rect",
        xmin = HHI_COMPETITION_CUTOFF,
        xmax = 10000,
        ymin = -Inf,
        ymax = Inf,
        fill = "#F9DEDE",
        alpha = 0.45
      )
    )
  }

  histogram_layer <- if (grey_bars) {
    geom_histogram(
      fill = "grey65",
      binwidth = HHI_HISTOGRAM_BINWIDTH,
      boundary = HHI_COMPETITION_CUTOFF %% HHI_HISTOGRAM_BINWIDTH,
      colour = "white",
      linewidth = 0.3,
      alpha = 0.9,
      show.legend = FALSE
    )
  } else {
    geom_histogram(
      aes(fill = competition_regime),
      binwidth = HHI_HISTOGRAM_BINWIDTH,
      boundary = HHI_COMPETITION_CUTOFF %% HHI_HISTOGRAM_BINWIDTH,
      colour = "white",
      linewidth = 0.3,
      alpha = 0.9
    )
  }

  competition_colours <- if (grey_bars) {
    c(
      "High competition (HHI ≤ 1,800)" = "#F9DEDE",
      "Low competition (HHI > 1,800)" = "#D9EEF7"
    )
  } else {
    c(
      "High competition (HHI ≤ 1,800)" = "#F4A6A6",
      "Low competition (HHI > 1,800)" = "#9ECAE1"
    )
  }

  competition_region_label_layer <- if (grey_bars) {
    list(
      annotate(
        "text",
        x = 250,
        y = Inf,
        label = "High competition",
        hjust = 0,
        vjust = 1.35,
     #   fontface = "bold",
        size = 3.8
      ),
      annotate(
        "text",
        x = 9750,
        y = Inf,
        label = "Low competition",
        hjust = 1,
        vjust = 1.35,
   #     fontface = "bold",
        size = 3.8
      )
    )
  } else {
    NULL
  }

  ggplot(data, aes(x = hhi_base)) +
    competition_region_layer +
    histogram_layer +
    geom_vline(
      xintercept = HHI_COMPETITION_CUTOFF,
      colour = "black",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    annotate(
      "text",
      x = HHI_COMPETITION_CUTOFF,
      y = Inf,
      label = "HHI = 1,800",
      hjust = -0.08,
      vjust = 1.35,
      size = 3.6
    ) +
    competition_region_label_layer +
    scale_fill_manual(
      values = competition_colours,
      name = NULL
    ) +
    guides(
      fill = if (grey_bars) {
        "none"
      } else {
        guide_legend(
          override.aes = list(
            alpha = 0.9,
            colour = NA
          )
        )
      }
    ) +
    scale_x_continuous(
      breaks = c(0, HHI_COMPETITION_CUTOFF, 4000, 6000, 8000, 10000),
      labels = scales::label_comma(),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 6),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
  #    title = title,
      subtitle = subtitle,
      x = "NAICS3 sales HHI",
      y = y_label
    ) +
    coord_cartesian(xlim = c(0, 10000), clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

p_naics3_hhi_histogram <- hhi_histogram_plot(
  hhi_histogram_data,
  title = "Distribution of 2015 NAICS3 market concentration",
  subtitle = paste0(
    "Sales-based HHI across classified markets; dashed line marks the ",
    "competition-regime cutoff"
  ),
  y_label = "Number of NAICS3 markets"
)

# A contemporaneous 2025 cross-section, with one observation per matched firm.
# Market HHI above uses the full Compustat sales denominator.
hhi_firm_histogram_data <- panel_summary |>
  filter(year == FIRM_HHI_HISTOGRAM_YEAR, is.finite(hhi_naics3)) |>
  transmute(
    cik,
    year,
    hhi_base = hhi_naics3,
    competition_regime = factor(
      if_else(
        hhi_naics3 <= HHI_COMPETITION_CUTOFF,
        "high_competition",
        "low_competition"
      ),
      levels = c("high_competition", "low_competition"),
      labels = c(
        "High competition (HHI ≤ 1,800)",
        "Low competition (HHI > 1,800)"
      )
    )
  )

if (
  anyDuplicated(hhi_firm_histogram_data$cik) ||
  nrow(hhi_firm_histogram_data) == 0L
) {
  stop("Firm-level HHI histogram requires a nonempty cross-section with one row per classified firm.")
}

p_firm_hhi_histogram <- hhi_histogram_plot(
  hhi_firm_histogram_data,
  title = "Distribution of 2025 NAICS3 market concentration across firms",
  subtitle = paste0(
    FIRM_HHI_HISTOGRAM_YEAR,
    " distribution: grey bars show firms; shaded regions define the competition regimes"
  ),
  y_label = "Number of firms",
  grey_bars = TRUE
)

firm_chars <- names(firm_char_var_labels)
firm_char_summary <- group_summary_table(
  panel_summary,
  firm_chars,
  firm_char_var_labels
)

firm_char_summary_wide <- firm_char_summary |>
  pivot_wider(
    names_from = ai_adopted,
    values_from = c(n, mean, sd, median, p25, p75),
    names_glue = "ai_{ai_adopted}_{.value}"
  ) |>
  arrange(characteristic)

p_firm_char_distributions_by_adoption <- firm_char_density_plot(
  panel_summary,
  names(ADOPTION_STATUS_DISTRIBUTION_LABELS),
  ADOPTION_STATUS_DISTRIBUTION_LABELS
)


# ---- AI-adoption prevalence over time -----------------------------------------
ai_adoption_prevalence_by_year <- panel_summary |>
  group_by(year) |>
  summarise(
    n_firm_years = n(),
    `Any adoption (≥2)` = mean(ai_adopted == 1L),
    `Limited (=2)` = mean(
      ai_adopted == 1L & ai_adopted3 == 0L
    ),
    `Strong (=3)` = mean(ai_adopted3 == 1L),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(
      `Any adoption (≥2)`,
      `Limited (=2)`,
      `Strong (=3)`
    ),
    names_to = "adoption_definition",
    values_to = "share_firm_years"
  ) |>
  mutate(
    adoption_definition = factor(
      adoption_definition,
      levels = c(
        "Any adoption (≥2)",
        "Limited (=2)",
        "Strong (=3)"
      )
    )
  )

p_ai_adoption_prevalence <- ggplot(
  ai_adoption_prevalence_by_year,
  aes(
    x = year,
    y = share_firm_years,
    colour = adoption_definition
  )
) +
  geom_line(linewidth = 1) +
  geom_point(
    size = 2.2
  ) +
  scale_colour_manual(
    values = c(
      "Any adoption (≥2)" = "#28A197",
      "Limited (=2)" = "black",
      "Strong (=3)" = "#C20E35"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(ai_adoption_prevalence_by_year$year)),
    expand = expansion(mult = c(0.015, 0.015)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 6),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
  #  title = "AI adoption prevalence over time",
  #  subtitle = "The two treatment definitions used in the empirical analysis",
    x = NULL,
    y = "Share of firm-years"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# ---- AI-adoption prevalence by competition regime ----------------------------
# This is cumulative adoption prevalence because `ai_adopted` is absorbing.
# Competition is fixed using each firm's 2015 NAICS3 market, whose HHI is
# calculated from the full Compustat universe. Reported rates use only the
# Compustat-EDGAR matched analysis sample.
competition_trend_labels <- c(
  high_competition = "High competition (HHI ≤ 1,800)",
  low_competition = "Low competition (HHI > 1,800)"
)

ai_adoption_by_competition_year <- competition_regime_panel |>
  mutate(
    competition_group = factor(
      unname(competition_trend_labels[as.character(competition_regime)]),
      levels = unname(competition_trend_labels)
    )
  ) |>
  group_by(year, competition_group) |>
  summarise(
    n_firm_years = n(),
    n_firms = n_distinct(cik),
    adoption_prevalence = mean(ai_adopted == 1L),
    .groups = "drop"
  ) |>
  arrange(year, competition_group)

if (
  nrow(ai_adoption_by_competition_year) !=
    2L * n_distinct(ai_adoption_by_competition_year$year) ||
  any(!is.finite(ai_adoption_by_competition_year$adoption_prevalence)) ||
  any(
    ai_adoption_by_competition_year$adoption_prevalence < 0 |
      ai_adoption_by_competition_year$adoption_prevalence > 1
  )
) {
  stop("Competition-specific AI-adoption trend failed its coverage or rate QA.")
}

p_ai_adoption_by_competition <- ggplot(
  ai_adoption_by_competition_year,
  aes(
    x = year,
    y = adoption_prevalence,
    colour = competition_group,
    group = competition_group
  )
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.3) +
  scale_colour_manual(
    values = c(
      "High competition (HHI ≤ 1,800)" = "#12436D",
      "Low competition (HHI > 1,800)" = "#C20E35"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(ai_adoption_by_competition_year$year)),
    expand = expansion(mult = c(0.015, 0.015)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 6),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "AI adoption over time by market competition",
    subtitle = paste0(
      "Cumulative adoption (score ≥2); regimes use full-Compustat 2015 ",
      "NAICS3 HHI"
    ),
    x = NULL,
    y = "Share of firms with AI adoption"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# ---- AI adoption by annually reclassified competition regime -----------------
# Unlike the fixed-2015 heterogeneity design above, this descriptive chart uses
# the contemporaneous full-Compustat NAICS3 HHI in each year. Firms can therefore
# move between competition regimes as their market's concentration changes.
ai_adoption_by_annual_competition_year <- panel_summary |>
  filter(is.finite(hhi_naics3)) |>
  mutate(
    competition_group = if_else(
      hhi_naics3 <= HHI_COMPETITION_CUTOFF,
      competition_trend_labels[["high_competition"]],
      competition_trend_labels[["low_competition"]]
    ),
    competition_group = factor(
      competition_group,
      levels = unname(competition_trend_labels)
    )
  ) |>
  group_by(year, competition_group) |>
  summarise(
    n_firm_years = n(),
    n_firms = n_distinct(cik),
    n_naics3_markets = n_distinct(naics3),
    adoption_prevalence = mean(ai_adopted == 1L),
    mean_hhi = mean(hhi_naics3),
    .groups = "drop"
  ) |>
  arrange(year, competition_group)

if (
  nrow(ai_adoption_by_annual_competition_year) !=
    2L * n_distinct(ai_adoption_by_annual_competition_year$year) ||
  any(!is.finite(ai_adoption_by_annual_competition_year$adoption_prevalence)) ||
  any(
    ai_adoption_by_annual_competition_year$adoption_prevalence < 0 |
      ai_adoption_by_annual_competition_year$adoption_prevalence > 1
  )
) {
  stop("Annual competition-specific AI-adoption trend failed its QA checks.")
}

p_ai_adoption_by_annual_competition <- ggplot(
  ai_adoption_by_annual_competition_year,
  aes(
    x = year,
    y = adoption_prevalence,
    colour = competition_group,
    group = competition_group
  )
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.3) +
  scale_colour_manual(
    values = c(
      "High competition (HHI ≤ 1,800)" = "#12436D",
      "Low competition (HHI > 1,800)" = "#C20E35"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(ai_adoption_by_annual_competition_year$year)),
    expand = expansion(mult = c(0.015, 0.015)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 6),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "AI adoption by annually measured market competition",
    subtitle = paste0(
      "Cumulative adoption (score ≥2); markets are reclassified each year ",
      "using full-Compustat NAICS3 HHI"
    ),
    x = NULL,
    y = "Share of firms with AI adoption"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# ---- Composition of AI adopters over time ------------------------------------
# The denominator here differs from the prevalence graph: it contains only
# firm-years already treated under the main AI-adoption definition. Because the
# treatment measures are absorbing, the first category is intentionally not
# labelled "Score 2"; it identifies firms that have adopted but have not yet
# reached the strong-adoption threshold.
ai_adopter_composition_by_year <- panel_summary |>
  filter(
    ai_adopted == 1L,
    !is.na(ai_adopted3)
  ) |>
  mutate(
    adopter_status = if_else(
      ai_adopted3 == 1L,
      "Strong (=3)",
      "Limited (=2)"
    ),
    adopter_status = factor(
      adopter_status,
      levels = c(
        "Limited (=2)",
        "Strong (=3)"
      )
    )
  ) |>
  count(year, adopter_status, name = "n_firm_years", .drop = FALSE) |>
  group_by(year) |>
  mutate(
    n_ai_adopter_firm_years = sum(n_firm_years),
    share_ai_adopters = n_firm_years / n_ai_adopter_firm_years
  ) |>
  ungroup()

composition_qa <- ai_adopter_composition_by_year |>
  group_by(year) |>
  summarise(
    n_statuses = n(),
    share_sum = sum(share_ai_adopters),
    .groups = "drop"
  )

if (
  nrow(composition_qa) == 0L ||
  any(composition_qa$n_statuses != 2L) ||
  any(abs(composition_qa$share_sum - 1) > 1e-10)
) {
  stop("AI-adopter composition QA failed: yearly shares must sum to one.")
}

p_ai_adopter_composition <- ggplot(
  ai_adopter_composition_by_year,
  aes(
    x = year,
    y = share_ai_adopters,
    fill = adopter_status
  )
) +
  geom_col(
    width = 0.78
  ) +
#  geom_text(
#    aes(label = scales::percent(share_ai_adopters, accuracy = 1)),
#    position = position_stack(vjust = 0.5),
#    colour = "white",
#    size = 3.2,
#    fontface = "bold",
#    show.legend = FALSE
#  ) +
  scale_fill_manual(
    values = c(
      "Limited (=2)" = "#12436D",
      "Strong (=3)" = "#C20E35"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = sort(unique(ai_adopter_composition_by_year$year)),
    expand = expansion(mult = c(0.015, 0.015)),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = NULL,
    subtitle = "Share of treated firms",
    x = NULL,
    y = "Share of AI-adopting firm-years"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p_ai_adoption_overview <- patchwork::wrap_plots( 
  p_ai_adoption_prevalence + 
    labs( title = NULL, subtitle = "Share of firms" ),
  p_ai_adopter_composition + 
    labs( title = NULL, subtitle = "Share of treated firms" ) + 
    guides(fill = "none"), 
  ncol = 2, 
  widths = c(1, 1), 
  guides = "collect" ) &
  theme(legend.position = "bottom")


# ---- Mean AI score across key sectors ----------------------------------------
key_sector_score_by_year <- panel_summary |>
  filter(naics2 %in% KEY_SECTOR_CODES,
         !is.na(ai_score)
         ) |>
  group_by(year, naics2) |>
  summarise(
    n_firm_years = n(),
    mean_ai_score = safe_mean(ai_score),
    share_ai_adopted = mean(ai_score >= 2L),
    .groups = "drop"
  ) |>
  mutate(
    sector_label = recode(naics2, !!!KEY_SECTOR_LABELS)
  )

key_sector_overview <- key_sector_score_by_year |>
  group_by(naics2, sector_label) |>
  summarise(
    mean_ai_score = weighted.mean(mean_ai_score, n_firm_years, na.rm = TRUE),
    share_ai_adopted = weighted.mean(share_ai_adopted, n_firm_years, na.rm = TRUE),
    n_firm_years = sum(n_firm_years),
    .groups = "drop"
  ) |>
  arrange(desc(mean_ai_score), desc(share_ai_adopted)) |>
  round_numeric_cols()

key_sector_end_labels <- key_sector_score_by_year |>
  group_by(sector_label) |>
  filter(year == max(year, na.rm = TRUE)) |>
  ungroup()

p_mean_ai_score_by_key_sector <- ggplot(
  key_sector_score_by_year,
  aes(x = year, y = share_ai_adopted, colour = sector_label)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_text(
    data = key_sector_end_labels,
    aes(label = sector_label),
    hjust = 0,
    nudge_x = 0.18,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = sort(unique(key_sector_score_by_year$year)),
    expand = expansion(mult = c(0.01, 0.14))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual(values = c(
    "Finance" = "#D95F02",
    "Admin Services" = "#90E0EF",
    "Education" = "#AF0000",
    "Mining" = "#A6761D",
    "Information" = "#1B9E77",
    "Professional & Scientific" = "#3D3D3D",
    "Health Care" = "#7570B3",
    "Manufacturing" = "#E7298A",
    "Retail Trade" = "#66A61E",
    "Construction" = "#666666"
  )) +
  labs(
#    title = "Share AI-adoption across key sectors",
#    subtitle = "Share firm-years with an observed AI score",
    x = "Year",
    y = "Share of firm-years"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 95, 5.5, 5.5)
  )

# ---- AI adoption across major industries -------------------------------------
industry_comparison_end_year <- max(panel_summary$year, na.rm = TRUE)
industry_comparison_start_year <- max(min(panel_summary$year, na.rm = TRUE),2022)

industry_adoption_summary <- panel_summary |>
  filter(
    !is.na(naics2),
    naics2 != "",
    naics2 != "99") |>
  mutate(
    industry_label = if_else(
      !is.na(naics2_title) & naics2_title != "",
      naics2_title,
      paste("NAICS2", naics2)
    )
  ) |>
  group_by(naics2, industry_label) |>
  summarise(
    n_firm_years = n(),
    n_firms = n_distinct(cik),
    share_ai_adoption = mean(ai_adopted == 1L),
    share_strong_ai_adoption = mean(ai_adopted3 == 1L),
    .groups = "drop"
  ) 

industry_adoption_summary <- industry_adoption_summary |>
  arrange(share_ai_adoption) |>
  mutate(
    industry_label = factor(
      industry_label,
      levels = industry_label
    )
  )

p_ai_adoption_by_industry <- ggplot(
  industry_adoption_summary,
  aes(y = industry_label)
) +
  geom_segment(
    aes(
      x = share_strong_ai_adoption,
      xend = share_ai_adoption,
      yend = industry_label
    ),
    colour = "grey82",
    linewidth = 1.4,
    lineend = "round"
  ) +
  geom_point(
    aes(x = share_ai_adoption, colour = "Any adoption (≥2)"),
    size = 3
  ) +
  geom_point(
    aes(x = share_strong_ai_adoption, colour = "Strong (=3)"),
    size = 3
  ) +
  scale_colour_manual(
    values = c(
      "Any adoption (≥2)" = "#12436D",
      "Strong (=3)" = "#7A1F3D"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::breaks_pretty(n = 7),
    limits = c(0, NA),
    expand = expansion(mult = c(0.01, 0.06))
  ) +
  labs(
    title = "AI adoption across major industries",
    subtitle = paste0(
      industry_comparison_start_year,
      "–",
      industry_comparison_end_year,
      " rates for the ",
      nrow(industry_adoption_summary),
      " largest NAICS2 sectors in the sample"
    ),
    x = "Share of firm-years",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )


# ---- Firm-level pre-treatment balance -----------------------------------------
# Every firm contributes at most one baseline: g-1 for ever-treated firms, and
# the first observed year for never-treated firms. Exact baselines missing from
# the matched panel remain in the audit but have no values in the table.
PRE_TREATMENT_BALANCE_LABELS <- c(
  log_at = "Total assets (log)",
  firm_age = "Firm age",
  cash_ratio = "Cash ratio",
  leverage = "Leverage",
  roa = "ROA",
  capx_intensity_y_w = "CAPX intensity",
  rd_intensity_y_w = "R&D intensity",
  rd_reporter = "R&D reporter",
  labor_productivity = "Labour productivity",
  emp = "Employment",
  sale = "Sales",
  hhi_naics3 = "HHI (NAICS3)"
)

pre_treatment_firm_roster <- panel_summary |>
  filter(
    !is.na(cik), cik != "",
    !is.na(year),
    !is.na(ai_adoption_year)
  ) |>
  group_by(cik) |>
  summarise(
    ai_adoption_year = first(ai_adoption_year),
    n_adoption_year_values = n_distinct(ai_adoption_year),
    first_observed_year = min(year),
    .groups = "drop"
  ) |>
  mutate(
    ever_treated = as.integer(ai_adoption_year > 0L),
    baseline_year = if_else(
      ever_treated == 1L,
      ai_adoption_year - 1L,
      first_observed_year
    )
  )

if (any(pre_treatment_firm_roster$n_adoption_year_values != 1L)) {
  stop("Each firm must have one consistent AI-adoption year.")
}

pre_treatment_baseline_values <- panel_summary |>
  transmute(
    cik,
    baseline_year = year,
    across(all_of(names(PRE_TREATMENT_BALANCE_LABELS))),
    # Match the determinant analysis: non-reporters have zero R&D intensity,
    # while the reporter indicator separately captures the disclosure margin.
    rd_intensity_y_w = if_else(
      rd_reporter == 0L,
      0,
      rd_intensity_y_w
    ),
    baseline_observed = TRUE
  )

pre_treatment_balance_data <- pre_treatment_firm_roster |>
  left_join(
    pre_treatment_baseline_values,
    by = c("cik", "baseline_year")
  ) |>
  mutate(baseline_observed = coalesce(baseline_observed, FALSE))

if (
  nrow(pre_treatment_balance_data) != nrow(pre_treatment_firm_roster) ||
  anyDuplicated(pre_treatment_balance_data$cik) ||
  any(pre_treatment_balance_data$ever_treated == 1L &
        pre_treatment_balance_data$baseline_year !=
          pre_treatment_balance_data$ai_adoption_year - 1L) ||
  any(pre_treatment_balance_data$ever_treated == 0L &
        pre_treatment_balance_data$baseline_year !=
          pre_treatment_balance_data$first_observed_year) ||
  !all(c(0L, 1L) %in% pre_treatment_balance_data$ever_treated)
) {
  stop("Firm-level pre-treatment balance failed its timing or uniqueness QA.")
}

pre_treatment_baseline_audit <- pre_treatment_balance_data |>
  mutate(
    adoption_group = if_else(ever_treated == 1L, "Ever-treated", "Never-treated")
  ) |>
  group_by(adoption_group) |>
  summarise(
    n_firms_in_roster = n(),
    n_with_baseline_observation = sum(baseline_observed),
    n_with_baseline_hhi = sum(is.finite(hhi_naics3)),
    baseline_year_min = min(baseline_year),
    baseline_year_max = max(baseline_year),
    .groups = "drop"
  )

pre_treatment_balance <- pre_treatment_adopter_table(
  pre_treatment_balance_data,
  names(PRE_TREATMENT_BALANCE_LABELS),
  PRE_TREATMENT_BALANCE_LABELS
)

pre_treatment_balance_by_hhi_data <- pre_treatment_balance_data |>
  filter(is.finite(hhi_naics3)) |>
  mutate(
    hhi_group = if_else(
      hhi_naics3 <= HHI_COMPETITION_CUTOFF,
      "High competition (HHI ≤ 1,800)",
      "Low competition (HHI > 1,800)"
    )
  )

if (
  nrow(pre_treatment_balance_by_hhi_data) !=
    sum(is.finite(pre_treatment_balance_data$hhi_naics3)) ||
  !all(c(0L, 1L) %in% pre_treatment_balance_by_hhi_data$ever_treated[
    pre_treatment_balance_by_hhi_data$hhi_group == "High competition (HHI ≤ 1,800)"
  ]) ||
  !all(c(0L, 1L) %in% pre_treatment_balance_by_hhi_data$ever_treated[
    pre_treatment_balance_by_hhi_data$hhi_group == "Low competition (HHI > 1,800)"
  ])
) {
  stop("Baseline HHI balance groups failed coverage QA.")
}

pre_treatment_balance_by_hhi <- bind_rows(lapply(
  c("High competition (HHI ≤ 1,800)", "Low competition (HHI > 1,800)"),
  function(group) {
    pre_treatment_adopter_table(
      filter(pre_treatment_balance_by_hhi_data, hhi_group == group),
      names(PRE_TREATMENT_BALANCE_LABELS),
      PRE_TREATMENT_BALANCE_LABELS
    ) |>
      mutate(`Baseline HHI group` = group, .before = 1)
  }
))

# Table 3.7: classify each firm once using its pre-treatment baseline HHI, then
# describe all matched firm-years of the classified firms. The score-based rate
# must use the same annual AI measure as the overall score distribution.
baseline_competition_firms <- pre_treatment_balance_by_hhi_data |>
  left_join(
    panel_summary |>
      transmute(cik, baseline_year = year, baseline_naics3 = naics3),
    by = c("cik", "baseline_year")
  )

if (
  nrow(baseline_competition_firms) != nrow(pre_treatment_balance_by_hhi_data) ||
  anyDuplicated(baseline_competition_firms$cik) ||
  anyNA(baseline_competition_firms$baseline_naics3)
) {
  stop("Baseline competition assignment failed its market or uniqueness QA.")
}

baseline_competition_panel <- panel_summary |>
  inner_join(
    baseline_competition_firms |>
      select(cik, hhi_group),
    by = "cik"
  )

baseline_competition_firm_stats <- baseline_competition_firms |>
  group_by(hhi_group) |>
  summarise(
    n_firms = n(),
    mean_hhi = mean(hhi_naics3),
    median_hhi = median(hhi_naics3),
    n_naics3_markets = n_distinct(baseline_naics3),
    .groups = "drop"
  )

baseline_competition_stats <- baseline_competition_panel |>
  group_by(hhi_group) |>
  summarise(
    n_firm_years = n(),
    n_ai_adopting_firm_years = sum(ai_score >= 2L),
    ai_adoption_rate_pct = 100 * mean(ai_score >= 2L),
    mean_log_assets = safe_mean(log_at),
    .groups = "drop"
  ) |>
  left_join(baseline_competition_firm_stats, by = "hhi_group") |>
  mutate(share_firms_pct = 100 * n_firms / sum(n_firms))

if (
  nrow(baseline_competition_stats) != 2L ||
  sum(baseline_competition_stats$n_firms) !=
    nrow(baseline_competition_firms) ||
  sum(baseline_competition_stats$n_firm_years) !=
    nrow(baseline_competition_panel)
) {
  stop("Baseline competition summary failed its coverage QA.")
}

baseline_competition_labels <- c(
  n_firms = "Number of firms",
  share_firms_pct = "Share of firms (%)",
  n_firm_years = "Number of firm-years",
  mean_hhi = "Mean HHI",
  median_hhi = "Median HHI",
  n_naics3_markets = "Number of NAICS3 markets",
  n_ai_adopting_firm_years = "AI-adopting firm-years (score ≥2)",
  ai_adoption_rate_pct = "AI adoption rate (score ≥2, %)",
  mean_log_assets = "Mean firm size (log assets)"
)

baseline_competition_summary <- baseline_competition_stats |>
  select(hhi_group, all_of(names(baseline_competition_labels))) |>
  pivot_longer(-hhi_group, names_to = "statistic", values_to = "value") |>
  mutate(
    statistic_order = match(statistic, names(baseline_competition_labels)),
    statistic = recode(statistic, !!!baseline_competition_labels)
  ) |>
  arrange(statistic_order) |>
  select(-statistic_order) |>
  pivot_wider(names_from = hhi_group, values_from = value) |>
  rename(Statistic = statistic) |>
  round_numeric_cols()


# ---- Pre-treatment distribution plots -----------------------------------------
key_distribution_data <- pre_treatment_balance_data |>
  select(ever_treated, all_of(names(KEY_DISTRIBUTION_LABELS))) |>
  pivot_longer(
    cols = all_of(names(KEY_DISTRIBUTION_LABELS)),
    names_to = "variable",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(
    adoption_group = if_else(
      ever_treated == 1L,
      "Ever AI adopter",
      "Never AI adopter"
    ),
    variable_label = recode(variable, !!!KEY_DISTRIBUTION_LABELS)
  )

p_key_variable_distributions <- ggplot(
  key_distribution_data,
  aes(x = value, color = adoption_group, fill = adoption_group)
) +
  geom_density(alpha = 0.22, linewidth = 0.9, adjust = 1.05) +
  facet_wrap(~ variable_label, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c(
      "Never AI adopter" = "#9E9E9E",
      "Ever AI adopter" = "#2C7FB8"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Never AI adopter" = "#9E9E9E",
      "Ever AI adopter" = "#2C7FB8"
    ),
    name = NULL
  ) +
  labs(
    title = "Pre-treatment distributions of key firm characteristics",
    subtitle = "Ever-treated at g−1; never-treated at first observed year",
    x = NULL,
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ---- Save bundle --------------------------------------------------------------
summary_stats_bundle <- list(
  sample_audit = summary_sample_audit,
  overall_summary = overall_summary,
  overall_summary_notes = overall_summary_notes,
  competition_regime_summary = competition_regime_summary,
  competition_regime_coverage = competition_regime_coverage,
  competition_regime_firm_assignment = competition_regime_firm_assignment,
  hhi_histogram_data = hhi_histogram_data,
  p_naics3_hhi_histogram = p_naics3_hhi_histogram,
  hhi_firm_histogram_data = hhi_firm_histogram_data,
  p_firm_hhi_histogram = p_firm_hhi_histogram,
  firm_char_summary = firm_char_summary,
  firm_char_summary_wide = firm_char_summary_wide,
  ai_adoption_prevalence_by_year = ai_adoption_prevalence_by_year,
  ai_adoption_by_competition_year = ai_adoption_by_competition_year,
  ai_adoption_by_annual_competition_year =
    ai_adoption_by_annual_competition_year,
  ai_adopter_composition_by_year = ai_adopter_composition_by_year,
  key_sector_score_by_year = key_sector_score_by_year,
  key_sector_overview = key_sector_overview,
  industry_adoption_summary = industry_adoption_summary,
  pre_treatment_balance_data = pre_treatment_balance_data,
  pre_treatment_baseline_audit = pre_treatment_baseline_audit,
  pre_treatment_balance = pre_treatment_balance,
  pre_treatment_balance_by_hhi = pre_treatment_balance_by_hhi,
  baseline_competition_summary = baseline_competition_summary,
  key_distribution_data = key_distribution_data,
  p_ai_adoption_prevalence = p_ai_adoption_prevalence,
  p_ai_adoption_by_competition = p_ai_adoption_by_competition,
  p_ai_adoption_by_annual_competition =
    p_ai_adoption_by_annual_competition,
  p_ai_adopter_composition = p_ai_adopter_composition,
  p_ai_adoption_overview = p_ai_adoption_overview,
  p_mean_ai_score_by_key_sector = p_mean_ai_score_by_key_sector,
  p_ai_adoption_by_industry = p_ai_adoption_by_industry,
  p_firm_char_distributions_by_adoption = p_firm_char_distributions_by_adoption,
  p_key_variable_distributions = p_key_variable_distributions
)

if (SAVE_SUMMARY_BUNDLE) {
  dir.create(SUMMARY_STATS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  saveRDS(summary_stats_bundle, SUMMARY_STATS_BUNDLE_RDS)
  write.csv(overall_summary, SUMMARY_STATS_MAIN_CSV, row.names = FALSE)
  write.csv(summary_sample_audit, SUMMARY_SAMPLE_AUDIT_CSV, row.names = FALSE)
  write.csv(pre_treatment_balance, PRE_TREATMENT_BALANCE_CSV, row.names = FALSE)
  write.csv(pre_treatment_baseline_audit, PRE_TREATMENT_BALANCE_AUDIT_CSV, row.names = FALSE)
  write.csv(pre_treatment_balance_by_hhi, PRE_TREATMENT_BALANCE_BY_HHI_CSV, row.names = FALSE)
  write.csv(baseline_competition_summary, BASELINE_COMPETITION_SUMMARY_CSV, row.names = FALSE)
  write.csv(competition_regime_summary, COMPETITION_REGIME_SUMMARY_CSV, row.names = FALSE)
}

if (SAVE_SUMMARY_FIGURES) {
  dir.create(SUMMARY_STATS_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = KEY_DISTRIBUTION_PNG,
    plot = p_key_variable_distributions,
    width = 11,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = ADOPTION_PREVALENCE_PNG,
    plot = p_ai_adoption_prevalence,
    width = 10,
    height = 5.5,
    dpi = 300
  )
  ggsave(
    filename = ADOPTION_BY_COMPETITION_PNG,
    plot = p_ai_adoption_by_competition,
    width = 10,
    height = 5.5,
    dpi = 300
  )
  ggsave(
    filename = ADOPTION_BY_ANNUAL_COMPETITION_PNG,
    plot = p_ai_adoption_by_annual_competition,
    width = 10,
    height = 5.5,
    dpi = 300
  )
  ggsave(
    filename = ADOPTER_COMPOSITION_PNG,
    plot = p_ai_adopter_composition,
    width = 10,
    height = 5.5,
    dpi = 300
  )
  ggsave(
    filename = ADOPTION_OVERVIEW_PNG,
    plot = p_ai_adoption_overview,
    width = 16,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = KEY_SECTOR_SCORE_PNG,
    plot = p_mean_ai_score_by_key_sector,
    width = 11,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = INDUSTRY_ADOPTION_PNG,
    plot = p_ai_adoption_by_industry,
    width = 11,
    height = 7.5,
    dpi = 300
  )
  ggsave(
    filename = ADOPTION_STATUS_DISTRIBUTION_PNG,
    plot = p_firm_char_distributions_by_adoption,
    width = 11,
    height = 8,
    dpi = 300
  )
  ggsave(
    filename = NAICS3_HHI_HISTOGRAM_PNG,
    plot = p_naics3_hhi_histogram,
    width = 10,
    height = 5.5,
    dpi = 300
  )
  ggsave(
    filename = FIRM_HHI_HISTOGRAM_PNG,
    plot = p_firm_hhi_histogram,
    width = 10,
    height = 5.5,
    dpi = 300
  )
}


# ---- Console output -----------------------------------------------------------
cat("\nGenerated summary statistics.\n")
cat(
  "Main descriptive matched-panel rows:",
  format(nrow(panel_descriptive), big.mark = ","), "\n"
)
cat(
  "Compustat-EDGAR matched analysis rows:",
  format(nrow(panel_summary), big.mark = ","),
  "\n"
)
cat(
  "Full Compustat rows used as the HHI universe:",
  format(nrow(full_panel_hhi_source), big.mark = ","),
  "\n"
)
cat("Years covered:", min(panel_summary$year, na.rm = TRUE), "to", max(panel_summary$year, na.rm = TRUE), "\n")

if (SAVE_SUMMARY_BUNDLE) {
  cat("Saved summary stats bundle to:", SUMMARY_STATS_BUNDLE_RDS, "\n")
  cat("Saved pre-treatment balance table to:", PRE_TREATMENT_BALANCE_CSV, "\n")
  cat("Saved pre-treatment balance audit to:", PRE_TREATMENT_BALANCE_AUDIT_CSV, "\n")
  cat("Saved baseline HHI balance table to:", PRE_TREATMENT_BALANCE_BY_HHI_CSV, "\n")
  cat("Saved baseline competition summary to:", BASELINE_COMPETITION_SUMMARY_CSV, "\n")
  cat("Saved competition regime summary to:", COMPETITION_REGIME_SUMMARY_CSV, "\n")
}

if (SAVE_SUMMARY_FIGURES) {
  cat("Saved key distribution figure to:", KEY_DISTRIBUTION_PNG, "\n")
  cat("Saved AI-adoption prevalence figure to:", ADOPTION_PREVALENCE_PNG, "\n")
  cat(
    "Saved AI-adoption-by-competition figure to:",
    ADOPTION_BY_COMPETITION_PNG,
    "\n"
  )
  cat(
    "Saved annually reclassified AI-adoption-by-competition figure to:",
    ADOPTION_BY_ANNUAL_COMPETITION_PNG,
    "\n"
  )
  cat("Saved AI-adopter composition figure to:", ADOPTER_COMPOSITION_PNG, "\n")
  cat("Saved combined AI-adoption figure to:", ADOPTION_OVERVIEW_PNG, "\n")
  cat("Saved key-sector mean-score figure to:", KEY_SECTOR_SCORE_PNG, "\n")
  cat("Saved industry-adoption figure to:", INDUSTRY_ADOPTION_PNG, "\n")
  cat("Saved adoption-status distribution figure to:", ADOPTION_STATUS_DISTRIBUTION_PNG, "\n")
  cat("Saved NAICS3 HHI histogram to:", NAICS3_HHI_HISTOGRAM_PNG, "\n")
  cat("Saved firm-level HHI histogram to:", FIRM_HHI_HISTOGRAM_PNG, "\n")
}
