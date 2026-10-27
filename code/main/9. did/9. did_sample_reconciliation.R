#!/usr/bin/env Rscript

# Reconstruct the firm-year union used by each saved DiD specification from
# its cohort-time cells. This is an independent sample audit: no ATT is re-fit.
# Run after 9. did.R from the repository root.

library(data.table)
source("code/config/global_settings.R")

panel_path <- "data/compustat_ai_analysis_panel.rds"
matched_path <- "data/compustat_ai_analysis_matched_panel.rds"
unmatched_path <- "data/compustat_ai_analysis_unmatched.csv"
results_dir <- "output/did_firm_outcomes"
min_treated_cohort <- MIN_DID_TREATED_COHORT
last_outcome_year <- 2025L
appendix_path <- "sample_reconciliation_appendix.md"
audit_path <- file.path(results_dir, "did_sample_reconciliation.csv")
preferred_summary_path <- file.path(
  results_dir, "did_preferred_sample_summary_stats.csv"
)
main_summary_path <- "output/summary_stats/table_3_5_matched_panel.csv"

required_files <- c(
  panel_path, matched_path, unmatched_path,
  main_summary_path,
  file.path(results_dir, "did_att_estimates.csv"),
  file.path(results_dir, "did_att_gt_cells.csv"),
  file.path(results_dir, "cohort_effect", "did_cohort_effects_all.csv")
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing inputs: ", paste(missing_files, collapse = ", "))
}

panel <- as.data.table(readRDS(panel_path))
matched <- as.data.table(readRDS(matched_path))
unmatched <- fread(
  unmatched_path,
  select = c("cik", "year", "ai_score", "has_ai_history")
)
att <- fread(file.path(results_dir, "did_att_estimates.csv"))
cells <- fread(file.path(results_dir, "did_att_gt_cells.csv"))
effects <- fread(file.path(results_dir, "cohort_effect", "did_cohort_effects_all.csv"))
main_summary <- fread(main_summary_path)

panel[, `:=`(cik = as.character(cik), year = as.integer(year))]
matched[, `:=`(cik = as.character(cik), year = as.integer(year))]
unmatched[, `:=`(cik = as.character(cik), year = as.integer(year))]
if (anyDuplicated(panel, by = c("cik", "year")) ||
    anyDuplicated(matched, by = c("cik", "year")) ||
    anyDuplicated(unmatched, by = c("cik", "year"))) {
  stop("An analysis, descriptive, or unmatched panel has duplicate CIK-year keys.")
}
if (any(!is.na(unmatched$ai_score)) ||
    any(paste(unmatched$cik, unmatched$year) %in%
        paste(panel$cik, panel$year))) {
  stop("The excluded panel overlaps the matched analysis panel.")
}
if (nrow(matched) != sum(!is.na(panel$ai_score)) ||
    anyNA(matched$ai_score) ||
    !setequal(
      paste(matched$cik, matched$year),
      paste(panel[!is.na(ai_score), cik], panel[!is.na(ai_score), year])
    )) {
  stop("The matched panel is not exactly the scored subset of the causal panel.")
}
if (anyNA(panel$has_ai_history) || !all(panel$has_ai_history)) {
  stop("Causal panel contains firms without known AI filing history.")
}
# A missing fiscal-report cohort is allowed only for a firm whose first
# qualifying filing could not be assigned to a Compustat period.
if (panel[is.na(ai_report_adoption_year) & is.na(first_ai_filing_date), .N] > 0L ||
    panel[is.na(ai_report_adoption_3_year) & is.na(first_ai3_filing_date), .N] > 0L) {
  stop("Undefined report-year adoption cohort without a qualifying filing.")
}
if (!identical(sort(unique(panel$year)), 2015:2025)) {
  stop("Unexpected panel years; review the appendix's calendar-year wording.")
}
if (any(!cells$estimable) || any(att$na_att_gt != 0L)) {
  stop("An ATT(g,t) cell failed; the fast sample reconstruction needs review.")
}

model_keys <- c("treatment_id", "outcome", "spec_name")
if (anyDuplicated(att, by = model_keys)) stop("Duplicate model rows in ATT file.")
if (nrow(att) != 30L) stop("Expected two treatments, five outcomes, three specifications.")

fmt <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)
row_md <- function(x) paste0("| ", paste(x, collapse = " | "), " |")

audit_one <- function(i) {
  model <- att[i]
  model_cells <- cells[
    treatment_id == model$treatment_id &
      outcome == model$outcome & spec_name == model$spec_name
  ]
  if (nrow(model_cells) != model$estimable_att_gt) {
    stop("Cell count disagrees with ATT table for row ", i)
  }

  cohort_var <- as.character(model$cohort_var)
  outcome <- as.character(model$outcome)
  controls <- if (model$controls == "None (~1)") {
    character()
  } else {
    trimws(strsplit(model$controls, ",", fixed = TRUE)[[1L]])
  }
  actual_controls <- sub("^naics2_f$", "naics2", controls)
  required <- c(cohort_var, outcome, actual_controls)
  if (!all(required %in% names(panel))) {
    stop("Missing panel columns for model row ", i)
  }

  eligible <- panel[
    get(cohort_var) == 0L | get(cohort_var) >= min_treated_cohort,
    c("cik", "year", "ai_score", cohort_var), with = FALSE
  ]
  setnames(eligible, cohort_var, "cohort")
  cohort_map <- unique(eligible[, .(cik, cohort)])
  if (anyDuplicated(cohort_map, by = "cik")) {
    stop("Treatment cohort changes within a firm for model row ", i)
  }
  cohort_lookup <- setNames(cohort_map$cohort, cohort_map$cik)
  years <- sort(unique(panel$year))
  outcome_ids <- setNames(lapply(years, function(y) {
    values <- panel[[outcome]]
    panel$cik[panel$year == y & is.finite(values)]
  }), years)
  control_ids <- setNames(lapply(years, function(y) {
    valid <- panel$year == y
    for (v in actual_controls) valid <- valid & !is.na(panel[[v]])
    panel$cik[valid]
  }), years)

  used_parts <- vector("list", nrow(model_cells))
  treated_ids <- character()
  for (j in seq_len(nrow(model_cells))) {
    cell <- model_cells[j]
    ids <- Reduce(intersect, list(
      outcome_ids[[as.character(cell$outcome_base_year)]],
      outcome_ids[[as.character(cell$target_year)]],
      control_ids[[as.character(cell$baseline_year)]]
    ))
    cohorts <- unname(cohort_lookup[ids])
    ids <- ids[!is.na(cohorts) & (
      cohorts == cell$cohort_year | cohorts == 0L |
        cohorts > cell$target_year
    )]
    cohorts <- unname(cohort_lookup[ids])
    if (sum(cohorts == cell$cohort_year) != cell$n_treated ||
        sum(cohorts != cell$cohort_year) != cell$n_controls) {
      stop("Cell contributors disagree with saved counts for row ", i,
           ", cohort ", cell$cohort_year, ", target ", cell$target_year)
    }
    if (cell$target_year >= cell$cohort_year) {
      treated_ids <- union(treated_ids, ids[cohorts == cell$cohort_year])
    }
    used_parts[[j]] <- data.table(
      cik = rep(ids, 2L),
      year = rep(c(cell$outcome_base_year, cell$target_year), each = length(ids))
    )
  }
  used <- unique(rbindlist(used_parts))
  used <- merge(
    used, panel[, .(cik, year, ai_score)],
    by = c("cik", "year"), all.x = TRUE, sort = FALSE
  )
  outcome_rows <- panel[, c("cik", "year", outcome), with = FALSE]
  setnames(outcome_rows, outcome, "outcome_value")
  used <- merge(
    used, outcome_rows,
    by = c("cik", "year"), all.x = TRUE, sort = FALSE
  )
  if (any(!is.finite(used$outcome_value))) {
    stop("A reconstructed outcome row is missing or non-finite for row ", i)
  }
  if (nrow(used) != model$n_obs || uniqueN(used$cik) != model$n_firms) {
    stop("Unique contributor counts disagree with saved ATT table for row ", i)
  }
  effect_rows <- effects[
    treatment_id == model$treatment_id &
      outcome == model$outcome & spec_name == model$spec_name
  ]
  if (uniqueN(effect_rows$cohort_year) != nrow(effect_rows) ||
      length(treated_ids) != sum(effect_rows$cohort_firms) ||
      uniqueN(unname(cohort_lookup[treated_ids])) != nrow(effect_rows)) {
    stop("Treated firm or cohort counts disagree with saved effects for row ", i)
  }
  reconstructed_cohorts <- data.table(
    cohort_year = as.integer(unname(cohort_lookup[treated_ids]))
  )[, .(cohort_firms = .N), by = cohort_year]
  if (!setequal(
    paste(reconstructed_cohorts$cohort_year, reconstructed_cohorts$cohort_firms),
    paste(effect_rows$cohort_year, effect_rows$cohort_firms)
  )) {
    stop("Per-cohort treated firm counts disagree for model row ", i)
  }

  data.table(
    treatment_id = model$treatment_id,
    treatment_label = model$treatment_label,
    outcome = outcome,
    outcome_label = model$outcome_label,
    spec_name = model$spec_name,
    spec_label = model$spec_label,
    eligible_input_firm_years = nrow(eligible),
    eligible_input_matched = sum(!is.na(eligible$ai_score)),
    eligible_input_unmatched = sum(is.na(eligible$ai_score)),
    estimation_firm_years = nrow(used),
    estimation_matched = sum(!is.na(used$ai_score)),
    estimation_unmatched = sum(is.na(used$ai_score)),
    estimation_pre_treatment_cohort = sum(used$year < min_treated_cohort),
    estimation_firms = uniqueN(used$cik),
    treated_firms = length(treated_ids),
    treatment_cohorts = uniqueN(unname(cohort_lookup[treated_ids])),
    first_cohort = min(unname(cohort_lookup[treated_ids])),
    last_cohort = max(unname(cohort_lookup[treated_ids])),
    outcome_mean = mean(used$outcome_value),
    outcome_sd = sd(used$outcome_value)
  )
}

audit <- rbindlist(lapply(seq_len(nrow(att)), audit_one))
if (any(audit$estimation_matched + audit$estimation_unmatched !=
        audit$estimation_firm_years) ||
    any(audit$estimation_unmatched != 0L) ||
    any(audit$estimation_firm_years > nrow(panel))) {
  stop("DiD estimation includes an unmatched or out-of-sample firm-year.")
}
fwrite(audit, audit_path)

outcome_bridge <- data.table(
  outcome = c(
    "log_emp", "log_labor_productivity", "log_sale", "log_xopr",
    "operating_profitability_w"
  ),
  descriptive_variable = c(
    "Employment (000s)", "Labour productivity (sales/emp)",
    "Sales ($m)", "Operating costs ($m)",
    "Operating profit (OIBDP/assets)"
  )
)
outcome_bridge[, descriptive_n := main_summary$N[
  match(descriptive_variable, main_summary$Variable)
]]
outcome_bridge[, largest_did_n := vapply(
  outcome,
  function(v) max(audit[outcome == v, estimation_firm_years]),
  integer(1L)
)]
outcome_bridge[, difference := descriptive_n - largest_did_n]
if (anyNA(outcome_bridge) || any(outcome_bridge$difference < 0L) ||
    main_summary[Variable == "Firm age (years)", N] != nrow(panel) ||
    main_summary[Variable == "AI adoption score (1/2/3)", N] != nrow(matched)) {
  stop("Main descriptive table does not cover every DID outcome sample.")
}

max_model <- audit[which.max(estimation_firm_years)]
preferred <- audit[
  treatment_id == "strong_ai_adoption_3" &
    outcome == "log_xopr" & spec_name == "main_controls_naics2"
]
if (nrow(max_model) != 1L || nrow(preferred) != 1L) {
  stop("Could not identify the operating-cost preferred model.")
}

eligible_firm_years <- nrow(panel) + nrow(unmatched)
eligible_firms <- uniqueN(c(panel$cik, unmatched$cik))
history_unmatched <- unmatched[has_ai_history == TRUE]
history_firm_years <- nrow(panel) + nrow(history_unmatched)
history_firms <- uniqueN(c(panel$cik, history_unmatched$cik))

lines <- c(
  "# Appendix: Descriptive and DID sample reconciliation",
  "",
  "## Sample definitions and count bridge",
  "",
  paste0("The eligible Compustat sample first applies the ", ANALYSIS_START_YEAR, "–", ANALYSIS_END_YEAR, " and exchange filters. The treatment-history pool retains firms with an observed EDGAR filing history. The main DID cohort is the first Compustat fiscal period after the first qualifying 10-K filing date. The **main descriptive and DID source sample** then requires a contemporaneously matched and scored annual filing for each outcome firm-year. An unmatched year cannot contribute an outcome. DID includes treatment cohorts from ", min_treated_cohort, ", excludes earlier cohorts, and applies each outcome and specification's two-period completeness requirements. The first positive report-period cohort is reported separately as a timing comparison."),
  "",
  row_md(c("Stage", "Unique firm-years", "Firms", "Contemporaneous match", "No contemporaneous match")),
  row_md(rep("---", 5L)),
  row_md(c("Eligible Compustat sample", fmt(eligible_firm_years), fmt(eligible_firms), fmt(nrow(panel)), fmt(nrow(unmatched)))),
  row_md(c("Treatment-history pool (known EDGAR history)", fmt(history_firm_years), fmt(history_firms), fmt(nrow(panel)), fmt(nrow(history_unmatched)))),
  row_md(c("Main descriptive / DID source sample", fmt(nrow(panel)), fmt(uniqueN(panel$cik)), fmt(sum(!is.na(panel$ai_score))), fmt(sum(is.na(panel$ai_score))))),
  row_md(c("DID eligible input: adoption score ≥2", fmt(audit[treatment_id == "ai_adoption_ge_2", eligible_input_firm_years][1L]), fmt(uniqueN(panel[ai_adoption_year == 0L | ai_adoption_year >= min_treated_cohort, cik])), fmt(audit[treatment_id == "ai_adoption_ge_2", eligible_input_matched][1L]), fmt(audit[treatment_id == "ai_adoption_ge_2", eligible_input_unmatched][1L]))),
  row_md(c("DID eligible input: strong adoption =3", fmt(preferred$eligible_input_firm_years), fmt(uniqueN(panel[ai_adoption_3_year == 0L | ai_adoption_3_year >= min_treated_cohort, cik])), fmt(preferred$eligible_input_matched), fmt(preferred$eligible_input_unmatched))),
  "",
  paste0("For strong adoption, excluding firms whose first treatment cohort is before ", min_treated_cohort, " removes ", fmt(nrow(panel) - preferred$eligible_input_firm_years), " firm-years from the matched source sample. Outcome-pair and control availability then determine the reported operating-cost counts."),
  "",
  paste0("The largest reported DID count is **", fmt(max_model$estimation_firm_years), "** for ", max_model$outcome_label, " under ", max_model$treatment_label, " (", max_model$spec_label, "). The preferred strong-adoption operating-cost model uses **", fmt(preferred$estimation_firm_years), "** unique outcome firm-years. All contributing outcome rows have a matched annual AI score, and every DID N is at most the ", fmt(nrow(panel)), "-row descriptive sample."),
  "",
  "Each financial-outcome row of the main descriptive table has at least as many nonmissing observations as the largest DID model for that outcome:",
  "",
  row_md(c("Outcome", "Descriptive N", "Largest DID N", "Difference")),
  row_md(rep("---", 4L))
)
for (i in seq_len(nrow(outcome_bridge))) {
  x <- outcome_bridge[i]
  lines <- c(lines, row_md(c(
    x$descriptive_variable, fmt(x$descriptive_n),
    fmt(x$largest_did_n), fmt(x$difference)
  )))
}
lines <- c(lines,
  "",
  paste0("The first permitted treatment cohort is **", min_treated_cohort, "**. The DID uses 2015–2025 outcomes; for example, ", fmt(preferred$estimation_pre_treatment_cohort), " of the preferred operating-cost outcome firm-years occur in the 2015 baseline year. The N in each DID column is the union of distinct CIK-year outcome rows used by at least one ATT(g,t), counted once even if a firm-year enters several cohort comparisons. It is not a sum of cell sample sizes. Firms, treated firms, and cohorts are distinct counts; treated firms contributed to at least one post-treatment cell."),
  "",
  "## Outcome-specific estimation samples",
  "",
  "Every row below was reconstructed from the saved cohort-time cells and checked against the reported model N, firm N, treated cohort weights, and cell-level treated/control counts. All `Unmatched outcome years` counts must be zero.",
  "",
  row_md(c("Treatment", "Outcome", "Specification", "Firm-years", "Unmatched outcome years", "Firms", "Treated firms", "Cohorts")),
  row_md(rep("---", 8L))
)
for (i in seq_len(nrow(audit))) {
  x <- audit[i]
  lines <- c(lines, row_md(c(
    x$treatment_label, x$outcome_label, x$spec_label,
    fmt(x$estimation_firm_years), fmt(x$estimation_unmatched),
    fmt(x$estimation_firms), fmt(x$treated_firms),
    paste0(x$treatment_cohorts, " (", x$first_cohort, "–", x$last_cohort, ")")
  )))
}
preferred_models <- audit[spec_name == "main_controls_naics2"]
fwrite(
  preferred_models[, .(
    treatment_id, treatment_label, outcome, outcome_label,
    estimation_firm_years, estimation_firms, treated_firms,
    treatment_cohorts, first_cohort, last_cohort,
    outcome_mean, outcome_sd, estimation_matched, estimation_unmatched
  )],
  preferred_summary_path
)
lines <- c(
  lines, "",
  "## Summary statistics on the preferred DID samples",
  "",
  "The mean and standard deviation below use the **exact outcome firm-years** in each preferred firm-and-industry-controls DID model. The outcome is on the scale used in estimation. Each N therefore equals the corresponding preferred DID column; other variables can have their own nonmissing counts. All contributing firm-years have an observed contemporaneous AI score.",
  "",
  row_md(c("Treatment", "DID outcome", "Firm-years (N)", "Firms", "Treated firms", "Mean", "SD", "With annual AI score")),
  row_md(rep("---", 8L))
)
for (i in seq_len(nrow(preferred_models))) {
  x <- preferred_models[i]
  lines <- c(lines, row_md(c(
    x$treatment_label, x$outcome_label,
    fmt(x$estimation_firm_years), fmt(x$estimation_firms),
    fmt(x$treated_firms), sprintf("%.3f", x$outcome_mean),
    sprintf("%.3f", x$outcome_sd), fmt(x$estimation_matched)
  )))
}
preferred_cohorts <- effects[spec_name == "main_controls_naics2"]
lines <- c(
  lines, "",
  "## Treated firms by cohort in the preferred specification",
  "",
  "Each cell is the number of distinct firms from that adoption cohort contributing to at least one post-treatment ATT(g,t) for the named outcome. Cohort counts sum to the `Treated firms` column above.",
  "",
  row_md(c("Treatment", "Outcome", as.character(min_treated_cohort:last_outcome_year), "Total")),
  row_md(rep("---", last_outcome_year - min_treated_cohort + 4L))
)
for (i in seq_len(nrow(preferred_models))) {
  x <- preferred_models[i]
  cohort_rows <- preferred_cohorts[
    treatment_id == x$treatment_id & outcome == x$outcome
  ]
  counts <- cohort_rows$cohort_firms[
    match(min_treated_cohort:last_outcome_year, cohort_rows$cohort_year)
  ]
  if (anyNA(counts) || sum(counts) != x$treated_firms) {
    stop("Preferred cohort decomposition failed for row ", i)
  }
  lines <- c(lines, row_md(c(
    x$treatment_label, x$outcome_label,
    vapply(counts, fmt, character(1L)), fmt(x$treated_firms)
  )))
}
lines <- c(lines, "", paste0(
  "Source: `data/compustat_ai_analysis_panel.rds`, `data/compustat_ai_analysis_matched_panel.rds`, and saved outputs from `code/main/9. did/9. did.R`. Rebuild this appendix with `Rscript \"code/main/9. did/9. did_sample_reconciliation.R\"`. Machine-readable tables are `output/did_firm_outcomes/did_sample_reconciliation.csv` and `output/did_firm_outcomes/did_preferred_sample_summary_stats.csv`."
))
writeLines(lines, appendix_path, useBytes = TRUE)
cat("Verified", nrow(audit), "DID samples and wrote", appendix_path, "\n")
