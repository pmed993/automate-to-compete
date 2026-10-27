#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build or load the panel data used in the analysis
# ------------------------------------------------------------------------------
#
# Default:
#   source("code/main/4. build_or_load_panel_data.R")
#
# Rebuild merged Compustat + AI panel only:
#   BUILD_PANEL_DATA <- TRUE
#   REBUILD_ANNUAL_PANEL <- FALSE
#   REFRESH_FROM_WRDS <- FALSE
#   source("code/main/4. build_or_load_panel_data.R")
#
# Rebuild annual Compustat panel and merged panel:
#   BUILD_PANEL_DATA <- TRUE
#   REBUILD_ANNUAL_PANEL <- TRUE
#   REFRESH_FROM_WRDS <- FALSE
#   source("code/main/4. build_or_load_panel_data.R")
#
# Refresh Compustat from WRDS and rebuild everything:
#   BUILD_PANEL_DATA <- TRUE
#   REBUILD_ANNUAL_PANEL <- TRUE
#   REFRESH_FROM_WRDS <- TRUE
#   source("code/main/4. build_or_load_panel_data.R")
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(data.table)


# ---- User options -------------------------------------------------------------

BUILD_PANEL_DATA <- isTRUE(
  get0("BUILD_PANEL_DATA", ifnotfound = FALSE)
)

REBUILD_ANNUAL_PANEL <- isTRUE(
  get0("REBUILD_ANNUAL_PANEL", ifnotfound = FALSE)
)

REFRESH_FROM_WRDS <- isTRUE(
  get0("REFRESH_FROM_WRDS", ifnotfound = FALSE)
)


# ---- Paths --------------------------------------------------------------------

ANNUAL_PANEL_RDS <- get0(
  "ANNUAL_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_annual_panel.rds"
  )
)

MERGED_PANEL_RDS <- get0(
  "MERGED_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_panel.rds"
  )
)

MATCHED_PANEL_RDS <- get0(
  "MATCHED_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_matched_panel.rds"
  )
)

ANALYSIS_MATCHED_PANEL_RDS <- get0(
  "ANALYSIS_MATCHED_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_analysis_matched_panel.rds"
  )
)

FINAL_UNMATCHED_CSV <- get0(
  "FINAL_UNMATCHED_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_analysis_unmatched.csv"
  )
)

PROFITABILITY_AUDIT_CSV <- get0(
  "PROFITABILITY_AUDIT_CSV",
  ifnotfound = file.path(
    OUTPUT_DIR,
    "panel_data",
    "operating_profitability_winsor_audit.csv"
  )
)

PROFITABILITY_WINSOR_LOWER <- as.numeric(get0(
  "PROFITABILITY_WINSOR_LOWER",
  ifnotfound = 0.01
))

PROFITABILITY_WINSOR_UPPER <- as.numeric(get0(
  "PROFITABILITY_WINSOR_UPPER",
  ifnotfound = 0.99
))

if (
  !is.finite(PROFITABILITY_WINSOR_LOWER) ||
    !is.finite(PROFITABILITY_WINSOR_UPPER) ||
    PROFITABILITY_WINSOR_LOWER < 0 ||
    PROFITABILITY_WINSOR_UPPER > 1 ||
    PROFITABILITY_WINSOR_LOWER >= PROFITABILITY_WINSOR_UPPER
) {
  stop("Profitability winsorisation probabilities must satisfy 0 <= lower < upper <= 1.")
}


# ---- Decide whether to rebuild ------------------------------------------------

needs_annual_build <-
  REFRESH_FROM_WRDS ||
  REBUILD_ANNUAL_PANEL ||
  !file.exists(ANNUAL_PANEL_RDS)

needs_merged_build <-
  BUILD_PANEL_DATA ||
  needs_annual_build ||
  !file.exists(MERGED_PANEL_RDS) ||
  !file.exists(MATCHED_PANEL_RDS)


# ---- Build or load panel -------------------------------------------------------

if (needs_merged_build) {
  
  SAVE_OUTPUTS <- TRUE
  SAVE_MERGED_OUTPUTS <- TRUE
  REBUILD_ANNUAL_PANEL <- needs_annual_build
  
  source(
    "code/main/3. get_panel_data/2. build_compustat_ai_panel.R"
  )
  
} else {
  
  comp <- readRDS(
    ANNUAL_PANEL_RDS
  )
  
  panel <- readRDS(
    MERGED_PANEL_RDS
  )
  
  panel_ai <- readRDS(
    MATCHED_PANEL_RDS
  )
}


setDT(comp)
setDT(panel)
setDT(panel_ai)


# ---- Apply final research-sample filters --------------------------------------
#
# Apply the final year, exchange and industry restrictions to the FULL merged
# Compustat panel first.
#
# This keeps the correct denominator for EDGAR / AI-score coverage.

panel_analysis_scope <- build_final_analysis_panel(
  panel
)

setDT(panel_analysis_scope)


# ---- Operating profitability outcome ----------------------------------------
#
# OIBDP is a fiscal-year flow and total assets are the scale variable. Ratios
# are defined only when both inputs are finite and total assets are strictly
# positive. Estimate one pooled pair of winsorisation bounds on the eligible
# year/exchange sample, before restricting on AI-history availability, and use
# those same bounds for every year, cohort, and treatment group.

required_profitability_cols <- c("oibdp", "at")
missing_profitability_cols <- setdiff(
  required_profitability_cols,
  names(panel_analysis_scope)
)

if (length(missing_profitability_cols) > 0L) {
  stop(
    "Cannot construct operating profitability; panel is missing: ",
    paste(missing_profitability_cols, collapse = ", ")
  )
}

panel_analysis_scope[, operating_profitability := fifelse(
  is.finite(oibdp) & is.finite(at) & at > 0,
  oibdp / at,
  NA_real_
)]

valid_profitability <- is.finite(
  panel_analysis_scope$operating_profitability
)

if (sum(valid_profitability) < 2L) {
  stop("Fewer than two valid operating-profitability observations are available.")
}

profitability_winsor_bounds <- as.numeric(quantile(
  panel_analysis_scope$operating_profitability[valid_profitability],
  probs = c(PROFITABILITY_WINSOR_LOWER, PROFITABILITY_WINSOR_UPPER),
  na.rm = TRUE,
  names = FALSE,
  type = 7
))

if (
  length(profitability_winsor_bounds) != 2L ||
    any(!is.finite(profitability_winsor_bounds)) ||
    profitability_winsor_bounds[1] > profitability_winsor_bounds[2]
) {
  stop("Operating-profitability winsorisation bounds are invalid.")
}

panel_analysis_scope[, operating_profitability_w := fifelse(
  is.finite(operating_profitability),
  pmin(
    pmax(operating_profitability, profitability_winsor_bounds[1]),
    profitability_winsor_bounds[2]
  ),
  NA_real_
)]

# Remove the superseded names if an older cached merged panel was loaded. This
# prevents stale upstream values from being used accidentally in later scripts.
legacy_profitability_cols <- intersect(
  c("operating_profit", "operating_profit_w"),
  names(panel_analysis_scope)
)
if (length(legacy_profitability_cols) > 0L) {
  panel_analysis_scope[, (legacy_profitability_cols) := NULL]
}

profitability_audit <- data.table(
  outcome = "operating_profitability_w",
  numerator = "OIBDP",
  denominator = "total assets",
  eligible_observations = nrow(panel_analysis_scope),
  valid_raw_observations = sum(valid_profitability),
  missing_or_nonfinite_oibdp = sum(!is.finite(panel_analysis_scope$oibdp)),
  missing_nonfinite_or_nonpositive_assets = sum(
    !is.finite(panel_analysis_scope$at) | panel_analysis_scope$at <= 0
  ),
  lower_probability = PROFITABILITY_WINSOR_LOWER,
  upper_probability = PROFITABILITY_WINSOR_UPPER,
  lower_bound = profitability_winsor_bounds[1],
  upper_bound = profitability_winsor_bounds[2],
  observations_below_lower_bound = sum(
    panel_analysis_scope$operating_profitability < profitability_winsor_bounds[1],
    na.rm = TRUE
  ),
  observations_above_upper_bound = sum(
    panel_analysis_scope$operating_profitability > profitability_winsor_bounds[2],
    na.rm = TRUE
  ),
  raw_median = median(
    panel_analysis_scope$operating_profitability,
    na.rm = TRUE
  ),
  winsorized_mean = mean(
    panel_analysis_scope$operating_profitability_w,
    na.rm = TRUE
  ),
  winsorized_sd = sd(
    panel_analysis_scope$operating_profitability_w,
    na.rm = TRUE
  )
)

if (
  any(
    is.finite(panel_analysis_scope$operating_profitability_w) &
      (
        panel_analysis_scope$operating_profitability_w <
          profitability_winsor_bounds[1] |
          panel_analysis_scope$operating_profitability_w >
            profitability_winsor_bounds[2]
      )
  ) ||
    any(
      is.na(panel_analysis_scope$operating_profitability) !=
        is.na(panel_analysis_scope$operating_profitability_w)
    )
) {
  stop("Operating-profitability construction QA failed.")
}


# ---- Validate required treatment variables -----------------------------------

required_panel_cols <- c(
  "cik",
  "year",
  "datadate",
  "ai_score",
  "has_ai_history",
  
  # Main adoption treatment
  "first_ai_filing_date",
  "ai_adopted",
  "ai_adoption_year",
  "ai_report_adoption_year",
  
  # Strong-adoption treatment
  "first_ai3_filing_date",
  "ai_adopted3",
  "ai_adoption_3_year",
  "ai_report_adoption_3_year"
)

missing_panel_cols <- setdiff(
  required_panel_cols,
  names(panel_analysis_scope)
)

if (length(missing_panel_cols) > 0L) {
  stop(
    "Merged panel is missing required columns: ",
    paste(missing_panel_cols, collapse = ", "),
    ". Rebuild 2. build_compustat_ai_panel.R first."
  )
}


if (anyNA(panel_analysis_scope$has_ai_history)) {
  stop(
    "`has_ai_history` contains NA values. ",
    "This should be defined for every firm in build_compustat_ai_panel.R."
  )
}


# ---- Main causal analysis panel -----------------------------------------------
#
# Require a contemporaneously matched, scored annual filing for every outcome
# firm-year. Firm-level filing history still determines the first positive
# report period, but an unmatched Compustat period cannot enter estimation or the main
# descriptive table because its annual adoption score is unobserved.

panel_analysis <- panel_analysis_scope[
  has_ai_history == TRUE &
    !is.na(ai_adopted) &
    !is.na(ai_score)
]


# ---- Compatibility alias for the matched analysis panel -----------------------

panel_analysis_matched <- copy(panel_analysis)


# ---- Missing-score observations excluded from causal analysis -----------------
# Retain a count for coverage diagnostics, not for estimation.

panel_analysis_score_missing <- panel_analysis_scope[
  has_ai_history == TRUE & is.na(ai_score)
]


# ---- Unmatched fiscal periods in full eligible sample -------------------------
#
# Used only for EDGAR / score-coverage diagnostics.

panel_analysis_unmatched <- panel_analysis_scope[
  is.na(ai_score)
]


# ---- Basic panel QA ------------------------------------------------------------

if (
  panel_analysis[
    ,
    any(has_ai_history != TRUE)
  ]
) {
  stop(
    "Causal panel contains firms without observed AI filing history."
  )
}


if (
  panel_analysis[
    ,
    any(is.na(ai_adopted))
  ]
) {
  stop(
    "Causal panel contains observations with missing ai_adopted."
  )
}

if (anyNA(panel_analysis$ai_score) ||
    nrow(panel_analysis) != nrow(panel_analysis_matched) ||
    nrow(panel_analysis) != panel_analysis_scope[!is.na(ai_score), .N]) {
  stop("Causal sample must contain only contemporaneously scored firm-years.")
}


duplicate_causal_rows <- panel_analysis[
  ,
  .N,
  by = .(
    cik,
    datadate
  )
][
  N > 1L
]

if (nrow(duplicate_causal_rows) > 0L) {
  stop(
    "Causal analysis panel contains duplicate CIK-datadate observations."
  )
}


# ---- Contemporaneous AI-score coverage ----------------------------------------

n_final_periods <- nrow(
  panel_analysis_scope
)

n_score_matched_periods <- panel_analysis_scope[
  !is.na(ai_score),
  .N
]

n_score_unmatched_periods <- panel_analysis_scope[
  is.na(ai_score),
  .N
]

score_match_rate <- if (n_final_periods > 0L) {
  
  n_score_matched_periods / n_final_periods
  
} else {
  
  NA_real_
}


# ---- Main causal-panel size ----------------------------------------------------

n_causal_periods <- nrow(
  panel_analysis
)

n_causal_firms <- uniqueN(
  panel_analysis$cik
)

n_history_score_missing <- nrow(
  panel_analysis_score_missing
)

causal_panel_share <- if (n_final_periods > 0L) {
  
  n_causal_periods / n_final_periods
  
} else {
  
  NA_real_
}


# ---- Firm-level EDGAR coverage ------------------------------------------------

firm_coverage <- panel_analysis_scope[
  ,
  .(
    has_ai_history = any(has_ai_history == TRUE),
    n_periods = .N,
    n_score_periods = sum(!is.na(ai_score))
  ),
  by = cik
]


firm_coverage_summary <- firm_coverage[
  ,
  .(
    eligible_firms = .N,
    firms_with_ai_history = sum(has_ai_history),
    firms_without_ai_history = sum(!has_ai_history),
    firm_history_coverage = mean(has_ai_history)
  )
]


# ---- AI-score coverage by fiscal-period year ----------------------------------

coverage_by_year <- panel_analysis_scope[
  ,
  .(
    compustat_periods = .N,
    matched_score_periods = sum(!is.na(ai_score)),
    unmatched_score_periods = sum(is.na(ai_score)),
    score_match_rate = mean(!is.na(ai_score))
  ),
  by = year
][
  order(year)
]


# ---- Main causal panel by year ------------------------------------------------

causal_panel_by_year <- panel_analysis[
  ,
  .(
    observations = .N,
    firms = uniqueN(cik),
    observed_ai_score = sum(!is.na(ai_score)),
    missing_ai_score = sum(is.na(ai_score)),
    treated = sum(ai_adopted == 1L),
    untreated = sum(ai_adopted == 0L)
  ),
  by = year
][
  order(year)
]


# ---- Console output -----------------------------------------------------------

cat(
  "\nFinal research-sample contemporaneous AI-score coverage:\n"
)

cat(
  "Eligible Compustat fiscal periods:",
  format(n_final_periods, big.mark = ","),
  "\n"
)

cat(
  "Periods with AI score:",
  format(n_score_matched_periods, big.mark = ","),
  "\n"
)

cat(
  "Periods without AI score:",
  format(n_score_unmatched_periods, big.mark = ","),
  "\n"
)

cat(
  "AI-score match rate:",
  sprintf("%.1f%%", 100 * score_match_rate),
  "\n"
)


cat(
  "\nMain causal analysis panel:\n"
)

cat(
  "Causal observations:",
  format(n_causal_periods, big.mark = ","),
  "\n"
)

cat(
  "Causal firms:",
  format(n_causal_firms, big.mark = ","),
  "\n"
)

cat(
  "AI-history observations excluded for missing contemporaneous AI score:",
  format(n_history_score_missing, big.mark = ","),
  "\n"
)

cat(
  "Share of eligible Compustat periods retained in causal panel:",
  sprintf("%.1f%%", 100 * causal_panel_share),
  "\n"
)


cat(
  "\nFirm-level AI filing-history coverage:\n"
)

print(
  firm_coverage_summary
)


cat(
  "\nAI-score coverage by fiscal-period year:\n"
)

print(
  coverage_by_year
)


cat(
  "\nMain causal panel by fiscal-period year:\n"
)

print(
  causal_panel_by_year
)


# ---- Save final analysis outputs ----------------------------------------------

dir.create(
  dirname(ANALYSIS_PANEL_RDS),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  dirname(PROFITABILITY_AUDIT_CSV),
  recursive = TRUE,
  showWarnings = FALSE
)


# Main matched dataset used in DiD and other causal analyses
saveRDS(
  panel_analysis,
  ANALYSIS_PANEL_RDS
)


# Compatibility copy for scripts that explicitly request the matched panel
saveRDS(
  panel_analysis_matched,
  ANALYSIS_MATCHED_PANEL_RDS
)


# Fiscal periods without contemporaneous AI score
fwrite(
  panel_analysis_unmatched,
  FINAL_UNMATCHED_CSV
)

fwrite(
  profitability_audit,
  PROFITABILITY_AUDIT_CSV
)


# ---- Final object types -------------------------------------------------------

setDT(panel_analysis)
setDT(panel_analysis_matched)
setDT(panel_analysis_scope)
setDT(panel_analysis_score_missing)
setDT(panel_analysis_unmatched)
setDT(firm_coverage)
