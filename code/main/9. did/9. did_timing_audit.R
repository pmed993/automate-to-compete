#!/usr/bin/env Rscript

# Audit whether the annual outcome immediately before the assigned DiD cohort
# already has a qualifying AI score. Run after building the matched panel.

source("code/config/global_settings.R")
library(data.table)

panel <- as.data.table(readRDS(file.path(
  INPUT_DIR, "compustat_ai_analysis_panel.rds"
)))
if (anyDuplicated(panel, by = c("cik", "year")) || anyNA(panel$ai_score)) {
  stop("Timing audit requires one scored row per firm-year.")
}

audit_definition <- function(label, threshold, old_cohort, report_cohort) {
  old_baseline <- panel[
    get(old_cohort) >= MIN_DID_TREATED_COHORT &
      year == get(old_cohort) - 1L
  ]
  report_baseline <- panel[
    get(report_cohort) >= MIN_DID_TREATED_COHORT &
      year == get(report_cohort) - 1L
  ]
  first_report <- panel[
    get(report_cohort) >= MIN_DID_TREATED_COHORT &
      year == get(report_cohort)
  ]
  if (any(report_baseline$ai_score >= threshold) ||
      any(first_report$ai_score < threshold)) {
    stop("First-report timing QA failed for ", label)
  }
  data.table(
    treatment = label,
    threshold = threshold,
    old_gminus1_rows = nrow(old_baseline),
    old_gminus1_already_positive = sum(old_baseline$ai_score >= threshold),
    report_gminus1_rows = nrow(report_baseline),
    report_gminus1_already_positive = sum(report_baseline$ai_score >= threshold),
    report_g_rows = nrow(first_report),
    report_g_positive = sum(first_report$ai_score >= threshold),
    first_report_cohort_unmapped_firms = uniqueN(
      panel[is.na(get(report_cohort)), cik]
    ),
    excluded_2015_report_cohort_firms = uniqueN(
      panel[get(report_cohort) == 2015L, cik]
    ),
    report_2016_cohort_firms = uniqueN(
      panel[get(report_cohort) == 2016L, cik]
    ),
    report_2025_cohort_firms = uniqueN(
      panel[get(report_cohort) == 2025L, cik]
    )
  )
}

audit <- rbindlist(list(
  audit_definition("AI adoption >= 2", 2L,
                   "ai_adoption_year", "ai_report_adoption_year"),
  audit_definition("Strong AI adoption = 3", 3L,
                   "ai_adoption_3_year", "ai_report_adoption_3_year")
))
output_path <- file.path(OUTPUT_DIR, "timing_comparison", "treatment_timing_audit.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
fwrite(audit, output_path)
print(audit)
cat("Saved timing audit to", output_path, "\n")
