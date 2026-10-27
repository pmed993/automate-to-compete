#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build merged Compustat + AI panel
# ------------------------------------------------------------------------------
# This script:
#   1. Loads the annual Compustat panel.
#   2. Adds AIIE industry exposure and BTOS validation data.
#   3. Loads filing-level AI scores and SEC filing metadata.
#   4. Matches each 10-K to the Compustat fiscal period it reports using:
#
#      a. exact CIK + report-date matching; then
#      b. a unique nearest report date within +/-7 business days.
#
#   5. Retains SEC filing_date for the main post-filing cohort and constructs
#      the first positive report-period cohort for a separate timing comparison.
#   6. Creates `panel`, `panel_ai`, `unmatched`, and a match audit.
#
# `ai_score = 1` means an observed filing was classified as no disclosed
# current AI adoption. An unmatched Compustat observation remains NA.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")


# ---- Settings ----------------------------------------------------------------

SAVE_MERGED_OUTPUTS <- isTRUE(
  get0("SAVE_MERGED_OUTPUTS", ifnotfound = FALSE)
)

REBUILD_ANNUAL_PANEL <- isTRUE(
  get0("REBUILD_ANNUAL_PANEL", ifnotfound = FALSE)
)

SKIP_AI_EXPOSURE <- isTRUE(
  get0("SKIP_AI_EXPOSURE", ifnotfound = FALSE)
)

NEAREST_REPORT_MAX_BUSINESS_DAYS <- as.integer(
  get0(
    "NEAREST_REPORT_MAX_BUSINESS_DAYS",
    ifnotfound = 7L
  )
)

if (
  length(NEAREST_REPORT_MAX_BUSINESS_DAYS) != 1L ||
  is.na(NEAREST_REPORT_MAX_BUSINESS_DAYS) ||
  NEAREST_REPORT_MAX_BUSINESS_DAYS < 0L
) {
  stop("NEAREST_REPORT_MAX_BUSINESS_DAYS must be one non-negative integer.")
}


AI_EXPOSURE_FILE <- get0(
  "AI_EXPOSURE_FILE",
  ifnotfound = file.path(INPUT_DIR, "AIOE_DataAppendix.xlsx")
)

AI_EXPOSURE_SHEET <- get0(
  "AI_EXPOSURE_SHEET",
  ifnotfound = "Appendix B"
)

AI_FILING_MASTER_FILE <- get0(
  "AI_FILING_MASTER_FILE",
  ifnotfound = file.path(
    INPUT_DIR,
    "llm_score",
    "llm_extraction_filing_master.csv"
  )
)

EDGAR_MANIFEST_MAIN_FILE <- get0(
  "EDGAR_MANIFEST_MAIN_FILE",
  ifnotfound = file.path(
    "cache",
    "edgar_keyword_windows",
    "filing_manifest.csv"
  )
)

EDGAR_MANIFEST_2025_FILE <- get0(
  "EDGAR_MANIFEST_2025_FILE",
  ifnotfound = file.path(
    "cache",
    "edgar_keyword_windows_fy2025_completion",
    "filing_manifest.csv"
  )
)


BTOS_Q7_NAICS2_SUMMARY_CSV <- get0(
  "BTOS_Q7_NAICS2_SUMMARY_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "btos_q7_naics2_summary.csv"
  )
)

ANNUAL_PANEL_RDS <- get0(
  "ANNUAL_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_annual_panel.rds"
  )
)


OUTPUT_MERGED_PANEL_RDS <- get0(
  "OUTPUT_MERGED_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_panel.rds"
  )
)

OUTPUT_MERGED_PANEL_CSV <- get0(
  "OUTPUT_MERGED_PANEL_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_panel.csv"
  )
)

OUTPUT_MATCHED_PANEL_RDS <- get0(
  "OUTPUT_MATCHED_PANEL_RDS",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_matched_panel.rds"
  )
)

OUTPUT_MATCHED_PANEL_CSV <- get0(
  "OUTPUT_MATCHED_PANEL_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_matched_panel.csv"
  )
)

OUTPUT_UNMATCHED_CSV <- get0(
  "OUTPUT_UNMATCHED_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_unmatched.csv"
  )
)

OUTPUT_MATCH_AUDIT_CSV <- get0(
  "OUTPUT_MATCH_AUDIT_CSV",
  ifnotfound = file.path(
    INPUT_DIR,
    "compustat_ai_match_audit.csv"
  )
)


# ---- Required columns ---------------------------------------------------------

ANNUAL_REQUIRED_COLS <- c(
  "cik",
  "year",
  "fyear",
  "datadate",
  "naics2",
  "naics4",
  "lag_source_fyear",
  "lag_is_consecutive",
  "firm_age_l1",
  "rd_reporter_l1",
  "rd_intensity_y",
  "capx_intensity_y",
  "total_inv_intensity_y",
  "rd_intensity_y_w",
  "capx_intensity_y_w",
  "total_inv_intensity_y_w"
)

BTOS_NUMERIC_COLS <- c(
  "btos_q7_ai_share_yes_mean",
  "btos_q7_ai_share_yes_mean_2023_2024",
  "btos_q7_ai_share_yes_mean_2023_2025",
  "btos_q7_ai_share_yes_latest",
  "btos_q7_ai_share_yes_se_latest",
  "btos_q7_ai_share_validation"
)

BTOS_INTEGER_COLS <- c(
  "btos_q7_n_periods",
  "btos_q7_n_periods_2023_2024",
  "btos_q7_n_periods_2023_2025"
)

BTOS_CHARACTER_COLS <- c(
  "btos_q7_first_smpdt",
  "btos_q7_last_smpdt"
)


# ---- Helpers -----------------------------------------------------------------

normalize_cik <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(out),
    NA_character_,
    as.character(as.integer(out))
  )
}


assert_unique_keys <- function(data, keys, label) {
  
  dupes <- as.data.table(data)[
    ,
    .N,
    by = keys
  ][
    N > 1L
  ]
  
  if (nrow(dupes) > 0L) {
    stop(
      label,
      " has duplicate rows on key: ",
      paste(keys, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}


min_idate <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0L) {
    return(as.IDate(NA))
  }
  
  min(x)
}


# Signed weekday distance from `from_date` to `to_date`.
#
# A positive value means the SEC report date is later than the Compustat date;
# a negative value means it is earlier. The calculation excludes Saturdays and
# Sundays. Public holidays are not subtracted, making the eligibility rule
# weakly more conservative at the edge of the window without adding a calendar
# dependency.
business_day_gap_one <- function(from_date, to_date) {

  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)

  if (is.na(from_date) || is.na(to_date)) {
    return(NA_integer_)
  }

  if (from_date == to_date) {
    return(0L)
  }

  direction <- if (to_date > from_date) 1L else -1L
  lower_date <- min(from_date, to_date)
  upper_date <- max(from_date, to_date)

  dates_between <- seq.Date(
    lower_date + 1,
    upper_date,
    by = "day"
  )

  weekday_number <- as.POSIXlt(dates_between)$wday
  direction * sum(weekday_number %in% 1:5)
}


business_day_gap <- Vectorize(
  business_day_gap_one,
  vectorize.args = c("from_date", "to_date"),
  USE.NAMES = FALSE
)


# ---- Load AIIE ---------------------------------------------------------------

load_ai_exposure <- function(path, sheet_name) {
  
  if (!file.exists(path)) {
    stop("AI exposure file not found: ", path)
  }
  
  exposure <- read_excel(
    path,
    sheet = sheet_name
  ) |>
    clean_names() |>
    as.data.table()
  
  required_cols <- c("naics", "aiie")
  missing_cols <- setdiff(required_cols, names(exposure))
  
  if (length(missing_cols) > 0L) {
    stop(
      "AI exposure file is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  exposure[,  naics4 := as.character(as.integer(naics))]
  
  exposure <- exposure[
    !is.na(naics4) &
      nchar(naics4) == 4,
    .(
      naics4,
      aiie
    )
  ]
  
  assert_unique_keys(
    exposure,
    "naics4",
    "AI exposure data"
  )
  
  exposure
}


# ---- Load BTOS ---------------------------------------------------------------

load_btos_validation <- function(path) {
  
  btos <- fread(path)
  
  btos[, naics2 := as.character(naics2)]
  
  keep_cols <- intersect(
    c(
      "naics2",
      BTOS_INTEGER_COLS,
      BTOS_NUMERIC_COLS,
      BTOS_CHARACTER_COLS
    ),
    names(btos)
  )
  
  btos <- btos[, ..keep_cols]
  
  assert_unique_keys(
    btos,
    "naics2",
    "BTOS validation data"
  )
  
  btos
}


# ---- Load filing-level AI data -----------------------------------------------

load_ai_filings <- function(
    scored_file,
    manifest_main_file,
    manifest_2025_file
) {
  
  # ---- Load scored filing master ---------------------------------------------
  
  scored <- fread(scored_file)
  
  required_scored <- c(
    "cik",
    "accession_number",
    "claude_ai_score"
  )
  
  missing_scored <- setdiff(
    required_scored,
    names(scored)
  )
  
  if (length(missing_scored) > 0L) {
    stop(
      "Scored filing master is missing: ",
      paste(missing_scored, collapse = ", ")
    )
  }
  
  scored[, cik := normalize_cik(cik)]
  
  scored[
    ,
    accession_number :=
      trimws(as.character(accession_number))
  ]
  
  # Preserve the extraction/scoring year only for audit.
  # It is NOT used for the Compustat match.
  if ("year" %in% names(scored)) {
    setnames(
      scored,
      "year",
      "scored_year_raw"
    )
  }
  
  # Avoid collision with SEC manifest form_type.
  if ("form_type" %in% names(scored)) {
    setnames(
      scored,
      "form_type",
      "scored_form_type"
    )
  }
  
  scored[
    ,
    ai_score :=
      suppressWarnings(
        as.integer(claude_ai_score)
      )
  ]
  
  if (
    scored[
      is.na(ai_score) |
      !ai_score %in% 1:3,
      .N
    ] > 0L
  ) {
    stop(
      "Scored filing master contains missing or invalid AI scores."
    )
  }
  
  assert_unique_keys(
    scored,
    c(
      "cik",
      "accession_number"
    ),
    "Scored filing master"
  )
  
  
  # ---- Load EDGAR manifest(s) -------------------------------------------------
  
  manifest_main <- fread(
    manifest_main_file
  )

  manifest_main[
    ,
    manifest_source := "main"
  ]

  manifest_parts <- list(manifest_main)

  # The publication workflow stores fiscal-2025 reports filed in 2026 in this
  # separate completion manifest. It is needed with the main filing-year
  # manifest. It may be omitted only when manifest_main_file already contains
  # both runs, for example the combined manifest created by the Python helper.
  if (
    length(manifest_2025_file) == 1L &&
      !is.na(manifest_2025_file) &&
      nzchar(manifest_2025_file) &&
      file.exists(manifest_2025_file) &&
      !identical(
        normalizePath(manifest_2025_file, mustWork = TRUE),
        normalizePath(manifest_main_file, mustWork = TRUE)
      )
  ) {
    manifest_2025 <- fread(
      manifest_2025_file
    )

    manifest_2025[
      ,
      manifest_source := "fy2025_completion"
    ]

    manifest_parts[[length(manifest_parts) + 1L]] <- manifest_2025
  }

  manifest <- rbindlist(
    manifest_parts,
    use.names = TRUE,
    fill = TRUE
  )
  
  
  # ---- Standardise SEC metadata ----------------------------------------------
  
  required_manifest <- c(
    "cik",
    "accession_number",
    "form_type",
    "report_date",
    "filing_date"
  )
  
  missing_manifest <- setdiff(
    required_manifest,
    names(manifest)
  )
  
  if (length(missing_manifest) > 0L) {
    stop(
      "EDGAR manifest is missing: ",
      paste(missing_manifest, collapse = ", ")
    )
  }
  
  manifest[
    ,
    cik := normalize_cik(cik)
  ]
  
  manifest[
    ,
    accession_number :=
      trimws(as.character(accession_number))
  ]
  
  manifest[
    ,
    report_date := as.IDate(report_date)
  ]
  
  manifest[
    ,
    filing_date := as.IDate(filing_date)
  ]
  
  
  # ---- Remove only TRUE duplicate CIK-accession records ----------------------
  
  # Same accession may legitimately belong to more than one registrant.
  # Therefore DO NOT deduplicate accession_number alone.
  
  if ("extract_updated_at" %in% names(manifest)) {
    
    setorder(
      manifest,
      cik,
      accession_number,
      -extract_updated_at
    )
  }
  
  manifest <- manifest[
    !duplicated(
      manifest,
      by = c(
        "cik",
        "accession_number"
      )
    )
  ]
  
  assert_unique_keys(
    manifest,
    c(
      "cik",
      "accession_number"
    ),
    "Combined EDGAR manifest"
  )
  
  
  # ---- Create unambiguous SEC variables --------------------------------------
  
  manifest_meta <- manifest[
    ,
    .(
      cik,
      accession_number,
      
      # Fiscal period represented by filing
      ai_report_date = report_date,
      
      # Date disclosure became public
      ai_filing_date = filing_date,
      
      # Calendar year of Compustat fiscal observation
      ai_report_year =
        as.integer(
          format(
            report_date,
            "%Y"
          )
        ),
      
      # Calendar filing year; audit/timing only
      ai_filing_year =
        as.integer(
          format(
            filing_date,
            "%Y"
          )
        ),
      
      ai_form_type = form_type,
      ai_company_name = company_name,
      ai_primary_document = primary_document,
      ai_source_url = source_url,
      manifest_source
    )
  ]
  
  
  # ---- Attach SEC metadata to scored filings ---------------------------------
  
  ai_filings <- merge(
    scored,
    manifest_meta,
    by = c(
      "cik",
      "accession_number"
    ),
    all.x = TRUE,
    sort = FALSE
  )
  
  # At this point every scored row must have exactly one SEC metadata match.
  if (nrow(ai_filings) != nrow(scored)) {
    stop(
      "SEC metadata merge changed the number of scored filing rows."
    )
  }
  
  
  missing_dates <- ai_filings[
    is.na(ai_report_date) |
      is.na(ai_filing_date)
  ]
  
  if (nrow(missing_dates) > 0L) {
    
    fwrite(
      missing_dates,
      file.path(
        INPUT_DIR,
        "scored_filings_missing_sec_dates.csv"
      )
    )
    
    stop(
      nrow(missing_dates),
      " scored filings are missing SEC report_date or filing_date. ",
      "See scored_filings_missing_sec_dates.csv."
    )
  }
  
  
  # Optional cross-check of form labels.
  if ("scored_form_type" %in% names(ai_filings)) {
    
    bad_forms <- ai_filings[
      !is.na(scored_form_type) &
        !is.na(ai_form_type) &
        scored_form_type != ai_form_type
    ]
    
    if (nrow(bad_forms) > 0L) {
      stop(
        "Scored filing form_type disagrees with SEC manifest form_type."
      )
    }
  }
  
  
  # ---- Canonical annual filing rule --------------------------------------------
  
  # For each CIK + fiscal report_date:
  #
  #   1. Prefer an original 10-K whenever one exists.
  #   2. If no 10-K exists, use a 10-K/A.
  #   3. Within the preferred form type, keep the earliest filing_date.
  #   4. accession_number provides a deterministic final tie-break.
  
  ai_filings <- ai_filings[
    ai_form_type %in% c(
      "10-K",
      "10-K/A"
    )
  ]
  
  
  # Form priority:
  # original 10-K always preferred to amendment
  ai_filings[
    ,
    form_priority := fcase(
      ai_form_type == "10-K",   1L,
      ai_form_type == "10-K/A", 2L,
      default = 99L
    )
  ]
  
  
  # Audit the raw multiplicity before choosing the canonical filing
  canonical_candidates <- ai_filings[
    ,
    .(
      n_candidate_filings = .N,
      has_10k = any(ai_form_type == "10-K"),
      has_10ka = any(ai_form_type == "10-K/A")
    ),
    by = .(
      cik,
      ai_report_date
    )
  ]
  
  
  # Sort so the desired filing is always first:
  #
  # 10-K before 10-K/A
  # earliest filing within form
  # accession as deterministic tie-break
  setorder(
    ai_filings,
    cik,
    ai_report_date,
    form_priority,
    ai_filing_date,
    accession_number
  )
  
  
  # Keep one canonical annual filing per CIK + report_date
  ai_filings <- ai_filings[
    ,
    .SD[1L],
    by = .(
      cik,
      ai_report_date
    )
  ]
  
  
  ai_filings[, form_priority := NULL]
  
  
  # Final QA
  assert_unique_keys(
    ai_filings,
    c(
      "cik",
      "ai_report_date"
    ),
    "Canonical annual AI filings"
  )

  
  canonical_form_summary <- ai_filings[
    ,
    .(
      fiscal_periods = .N
    ),
    by = ai_form_type
  ][
    order(ai_form_type)
  ]
  
  print(canonical_form_summary)
  
  
  fallback_10ka <- ai_filings[
    ai_form_type == "10-K/A"
  ]
  
  cat(
    "\nFiscal periods using 10-K/A because no original 10-K was available:",
    nrow(fallback_10ka),
    "\n"
  )
  
  bad_10ka_selection <- fallback_10ka[
    canonical_candidates[
      has_10k == TRUE
    ],
    on = .(
      cik,
      ai_report_date
    ),
    nomatch = 0L
  ]
  
  if (nrow(bad_10ka_selection) > 0L) {
    stop(
      "Some 10-K/A filings were selected even though an original 10-K existed."
    )
  }
  
  # Return one canonical filing per CIK + report_date
  return(ai_filings)
}
# ---- Load annual Compustat panel ----------------------------------------------

if (
  REBUILD_ANNUAL_PANEL ||
  !file.exists(ANNUAL_PANEL_RDS)
) {
  
  source(
    "code/main/3. get_panel_data/1. build_compustat_annual_panel.R"
  )
  
} else {
  
  comp <- readRDS(ANNUAL_PANEL_RDS)
}

setDT(comp)


missing_annual_cols <- setdiff(
  ANNUAL_REQUIRED_COLS,
  names(comp)
)

if (length(missing_annual_cols) > 0L) {
  stop(
    "Annual Compustat panel is missing required columns: ",
    paste(missing_annual_cols, collapse = ", ")
  )
}


# ---- Standardize Compustat keys -----------------------------------------------

comp[
  ,
  `:=`(
    cik = normalize_cik(cik),
    fyear = as.integer(fyear),
    datadate = as.IDate(datadate)
  )
]


# `year` is explicitly the Compustat fiscal-period calendar year.
# It is NOT the SEC filing year.
comp[
  ,
  year := as.integer(
    format(datadate, "%Y")
  )
]


assert_unique_keys(
  comp,
  c("cik", "datadate"),
  "Annual Compustat panel"
)


# Downstream staggered DiD uses calendar-year time, so the annual panel must
# contain at most one observation per firm and calendar year.
assert_unique_keys(
  comp,
  c("cik", "year"),
  "Annual Compustat panel"
)


# ---- Add industry AI exposure -------------------------------------------------

comp_panel <- copy(comp)

if (SKIP_AI_EXPOSURE) {
  
  comp_panel[
    ,
    aiie := NA_real_
  ]
  
} else {
  
  ai_exposure <- load_ai_exposure(
    AI_EXPOSURE_FILE,
    AI_EXPOSURE_SHEET
  )
  
  comp_panel <- merge(
    comp_panel,
    ai_exposure,
    by = "naics4",
    all.x = TRUE,
    sort = FALSE
  )
}


comp_panel[
  ,
  `:=`(
    ai_rd_intensity = rd_intensity_y * aiie,
    ai_capx_intensity = capx_intensity_y * aiie,
    ai_inv_intensity = total_inv_intensity_y * aiie,
    ai_rd_intensity_w = rd_intensity_y_w * aiie,
    ai_capx_intensity_w = capx_intensity_y_w * aiie,
    ai_inv_intensity_w = total_inv_intensity_y_w * aiie
  )
]


# ---- Add BTOS validation ------------------------------------------------------

if (file.exists(BTOS_Q7_NAICS2_SUMMARY_CSV)) {
  
  btos_validation <- load_btos_validation(
    BTOS_Q7_NAICS2_SUMMARY_CSV
  )
  
  comp_panel <- merge(
    comp_panel,
    btos_validation,
    by = "naics2",
    all.x = TRUE,
    sort = FALSE
  )
  
} else {
  
  for (col in BTOS_NUMERIC_COLS) {
    comp_panel[, (col) := NA_real_]
  }
  
  for (col in BTOS_INTEGER_COLS) {
    comp_panel[, (col) := NA_integer_]
  }
  
  for (col in BTOS_CHARACTER_COLS) {
    comp_panel[, (col) := NA_character_]
  }
}


for (col in BTOS_NUMERIC_COLS) {
  if (!col %in% names(comp_panel)) {
    comp_panel[, (col) := NA_real_]
  }
}

for (col in BTOS_INTEGER_COLS) {
  if (!col %in% names(comp_panel)) {
    comp_panel[, (col) := NA_integer_]
  }
}

for (col in BTOS_CHARACTER_COLS) {
  if (!col %in% names(comp_panel)) {
    comp_panel[, (col) := NA_character_]
  }
}


assert_unique_keys(
  comp_panel,
  c("cik", "datadate"),
  "Compustat panel before EDGAR merge"
)


# ---- Load AI filings ----------------------------------------------------------

ai_filings <- load_ai_filings(
  AI_FILING_MASTER_FILE,
  EDGAR_MANIFEST_MAIN_FILE,
  EDGAR_MANIFEST_2025_FILE
)

# ---- Define AI-history universe and treatment dates ----------------------------
#
# The main disclosure-timed treatment uses SEC filing_date because this is
# when the disclosure becomes observable. The reported fiscal period of the
# first positive filing is mapped below for the timing comparison.
#
# AI-history coverage is defined at the FIRM level, not the firm-year level.
# This broad panel records firm-level treatment history. The downstream
# descriptive and DID analysis sample requires a scored filing in each outcome
# year, so unmatched Compustat outcome rows do not enter those models.
#
# Allow the preceding report year because a fiscal-year 2014 10-K filed in
# 2015 can determine treatment before the first 2015 outcome.
#
# Very old filings/amendments are excluded from treatment-date construction.

treatment_filings <- ai_filings[
  ai_report_year >= ANALYSIS_START_YEAR - 1L &
    ai_report_year <= ANALYSIS_END_YEAR
]


# Firms with at least one scored annual filing relevant to the analysis window.
# This is deliberately NOT based on whether each Compustat year has a matched
# ai_score.
ai_firm_history <- unique(
  treatment_filings[
    ,
    .(
      cik,
      has_ai_history = TRUE
    )
  ]
)


# First observable qualifying disclosure dates.
treatment_dates <- treatment_filings[
  ,
  .(
    first_ai_filing_date = min_idate(
      ai_filing_date[
        ai_score >= 2L
      ]
    ),
    first_ai3_filing_date = min_idate(
      ai_filing_date[
        ai_score == 3L
      ]
    )
  ),
  by = cik
]


assert_unique_keys(
  ai_firm_history,
  "cik",
  "Firm-level AI history"
)

assert_unique_keys(
  treatment_dates,
  "cik",
  "Firm-level AI treatment dates"
)


# ---- Merge Compustat and EDGAR ------------------------------------------------
#
# Matching hierarchy:
#
#   1. Exact CIK + fiscal report date.
#   2. For a still-unmatched Compustat period, the unique nearest SEC report
#      date for the same CIK within +/- NEAREST_REPORT_MAX_BUSINESS_DAYS.
#
# Exact matches always take precedence. A filing already used by an exact match
# is unavailable to the fallback, and no fallback filing may be assigned to more
# than one Compustat period. Equidistant nearest candidates remain unmatched.
#
# filing_date is retained separately for treatment timing.

ai_filings[
  ,
  match_datadate := ai_report_date
]


n_comp_before_merge <- nrow(comp_panel)


comp_match_keys <- unique(
  comp_panel[
    ,
    .(
      cik,
      datadate
    )
  ]
)


# Stage 1: exact fiscal-report-date matches.
exact_matches <- merge(
  comp_match_keys,
  ai_filings,
  by.x = c(
    "cik",
    "datadate"
  ),
  by.y = c(
    "cik",
    "match_datadate"
  ),
  all = FALSE,
  sort = FALSE
)

setDT(exact_matches)

exact_matches[
  ,
  `:=`(
    match_method = "exact_report_date",
    report_date_gap_days = 0L,
    report_date_gap_business_days = 0L
  )
]

assert_unique_keys(
  exact_matches,
  c("cik", "datadate"),
  "Exact Compustat-EDGAR matches"
)


# Stage 2: build fallback candidates only for unmatched Compustat periods and
# unused filings. The 14-calendar-day prefilter is deliberately wider than the
# seven-weekday eligibility window and avoids an unnecessary CIK-level cross
# product before the business-day calculation.
unmatched_match_keys <- comp_match_keys[
  !exact_matches,
  on = .(
    cik,
    datadate
  )
]

used_exact_filings <- unique(
  exact_matches[
    ,
    .(
      cik,
      ai_report_date
    )
  ]
)

available_fallback_filings <- ai_filings[
  !used_exact_filings,
  on = .(
    cik,
    ai_report_date
  )
]

fallback_candidates <- merge(
  unmatched_match_keys,
  available_fallback_filings,
  by = "cik",
  all = FALSE,
  allow.cartesian = TRUE,
  sort = FALSE
)

setDT(fallback_candidates)

fallback_candidates[
  ,
  report_date_gap_days := as.integer(
    ai_report_date - datadate
  )
]

fallback_candidates <- fallback_candidates[
  !is.na(report_date_gap_days) &
    abs(report_date_gap_days) <= 14L
]

if (nrow(fallback_candidates) > 0L) {

  fallback_candidates[
    ,
    report_date_gap_business_days := as.integer(
      business_day_gap(
        datadate,
        ai_report_date
      )
    )
  ]

  fallback_candidates <- fallback_candidates[
    !is.na(report_date_gap_business_days) &
      abs(report_date_gap_business_days) <=
        NEAREST_REPORT_MAX_BUSINESS_DAYS
  ]
}


# Candidate-level QA retained for the saved match audit.
if (nrow(fallback_candidates) > 0L) {

  fallback_candidates[
    ,
    absolute_report_date_gap_days := abs(report_date_gap_days)
  ]

  fallback_candidate_stats <- fallback_candidates[
    ,
    .(
      fallback_eligible_candidates = .N,
      fallback_min_absolute_gap_days = min(
        absolute_report_date_gap_days
      ),
      fallback_nearest_candidates = sum(
        absolute_report_date_gap_days ==
          min(absolute_report_date_gap_days)
      )
    ),
    by = .(
      cik,
      datadate
    )
  ]

  nearest_fallback_matches <- fallback_candidates[
    fallback_candidate_stats,
    on = .(
      cik,
      datadate,
      absolute_report_date_gap_days =
        fallback_min_absolute_gap_days
    ),
    nomatch = 0L
  ][
    fallback_nearest_candidates == 1L
  ]

} else {

  fallback_candidate_stats <- data.table(
    cik = character(),
    datadate = as.IDate(character()),
    fallback_eligible_candidates = integer(),
    fallback_min_absolute_gap_days = integer(),
    fallback_nearest_candidates = integer()
  )

  nearest_fallback_matches <- fallback_candidates
}


# Enforce one-to-one filing use. In the unlikely event that the same filing is
# the unique nearest candidate for multiple Compustat periods, all conflicting
# assignments remain unmatched for manual review.
if (nrow(nearest_fallback_matches) > 0L) {

  fallback_filing_use <- nearest_fallback_matches[
    ,
    .N,
    by = .(
      cik,
      ai_report_date
    )
  ]

  reused_fallback_filings <- fallback_filing_use[N > 1L]

  nearest_fallback_matches <- nearest_fallback_matches[
    !reused_fallback_filings,
    on = .(
      cik,
      ai_report_date
    )
  ]

} else {

  reused_fallback_filings <- data.table(
    cik = character(),
    ai_report_date = as.IDate(character()),
    N = integer()
  )
}


nearest_fallback_matches[
  ,
  match_method := paste0(
    "unique_nearest_report_date_",
    NEAREST_REPORT_MAX_BUSINESS_DAYS,
    "bd"
  )
]


# `match_datadate` is the filing's own report date and is no longer needed once
# each filing has been mapped to the Compustat `datadate` key.
if ("match_datadate" %in% names(exact_matches)) {
  exact_matches[, match_datadate := NULL]
}

if ("match_datadate" %in% names(nearest_fallback_matches)) {
  nearest_fallback_matches[, match_datadate := NULL]
}

fallback_internal_cols <- intersect(
  c(
    "absolute_report_date_gap_days",
    "fallback_eligible_candidates",
    "fallback_min_absolute_gap_days",
    "fallback_nearest_candidates"
  ),
  names(nearest_fallback_matches)
)

if (length(fallback_internal_cols) > 0L) {
  nearest_fallback_matches[, (fallback_internal_cols) := NULL]
}


matched_filing_map <- rbindlist(
  list(
    exact_matches,
    nearest_fallback_matches
  ),
  use.names = TRUE,
  fill = TRUE
)

assert_unique_keys(
  matched_filing_map,
  c("cik", "datadate"),
  "Final Compustat-EDGAR filing map"
)

assert_unique_keys(
  matched_filing_map,
  c("cik", "ai_report_date"),
  "Final Compustat-EDGAR filing map"
)

# The filing date records when investors could observe a disclosure. For the
# annual-outcome DiD, the first positive filing instead identifies the fiscal
# period in which the filing says AI was already in use. Map that report period
# to its Compustat datadate, including the permitted near-date matches. A first
# positive filing without a unique Compustat match has unknown fiscal-period
# timing and must not be assigned to a later cohort or to the control group.
first_qualifying_report <- function(score_threshold, column_name) {
  qualifying <- copy(treatment_filings[ai_score >= score_threshold])
  setorder(qualifying, cik, ai_filing_date, ai_report_date)
  first <- qualifying[, .SD[1L], by = cik][
    , .(cik, ai_report_date)
  ]
  mapped <- merge(
    first,
    matched_filing_map[, .(cik, ai_report_date, datadate)],
    by = c("cik", "ai_report_date"),
    all.x = TRUE,
    sort = FALSE
  )
  mapped[, (column_name) := as.integer(format(datadate, "%Y"))]
  mapped[, c("ai_report_date", "datadate") := NULL]
  assert_unique_keys(mapped, "cik", paste0(column_name, " mapping"))
  mapped
}

report_cohorts <- merge(
  first_qualifying_report(2L, "ai_report_adoption_year"),
  first_qualifying_report(3L, "ai_report_adoption_3_year"),
  by = "cik",
  all = TRUE,
  sort = FALSE
)
assert_unique_keys(report_cohorts, "cik", "Fiscal-report adoption cohorts")


panel <- merge(
  comp_panel,
  matched_filing_map,
  by = c(
    "cik",
    "datadate"
  ),
  all.x = TRUE,
  sort = FALSE
)

setDT(panel)


if (nrow(panel) != n_comp_before_merge) {
  stop(
    "Compustat-EDGAR merge changed the Compustat row count."
  )
}

panel[
  is.na(match_method),
  match_method := "unmatched"
]


# Attach firm-level treatment dates.
panel <- merge(
  panel,
  treatment_dates,
  by = "cik",
  all.x = TRUE,
  sort = FALSE
)

setDT(panel)


# Attach fiscal-report adoption cohorts for the timing comparison. A missing value
# is resolved below to 0 only when there was no qualifying filing at all.
panel <- merge(
  panel,
  report_cohorts,
  by = "cik",
  all.x = TRUE,
  sort = FALSE
)
setDT(panel)


# Attach firm-level AI-history indicator.
panel <- merge(
  panel,
  ai_firm_history,
  by = "cik",
  all.x = TRUE,
  sort = FALSE
)

setDT(panel)


panel[
  is.na(has_ai_history),
  has_ai_history := FALSE
]

panel[
  has_ai_history & is.na(first_ai_filing_date),
  ai_report_adoption_year := 0L
]
panel[
  has_ai_history & is.na(first_ai3_filing_date),
  ai_report_adoption_3_year := 0L
]


assert_unique_keys(
  panel,
  c("cik", "datadate"),
  "Merged Compustat + AI panel"
)

assert_unique_keys(
  panel,
  c("cik", "year"),
  "Merged Compustat + AI annual panel"
)


# Every exact match must have identical fiscal-period dates.
if (
  panel[
    match_method == "exact_report_date" &
      (
        is.na(ai_report_date) |
        ai_report_date != datadate |
        report_date_gap_days != 0L |
        report_date_gap_business_days != 0L
      ),
    .N
  ] > 0L
) {
  stop("Exact report-date match QA failed.")
}


fallback_match_method <- paste0(
  "unique_nearest_report_date_",
  NEAREST_REPORT_MAX_BUSINESS_DAYS,
  "bd"
)

# Every fallback match must be non-exact, inside the configured signed
# business-day interval, and internally consistent with the stored date gap.
if (
  panel[
    match_method == fallback_match_method &
      (
        is.na(ai_report_date) |
        ai_report_date == datadate |
        is.na(report_date_gap_business_days) |
        report_date_gap_business_days <
          -NEAREST_REPORT_MAX_BUSINESS_DAYS |
        report_date_gap_business_days >
          NEAREST_REPORT_MAX_BUSINESS_DAYS |
        report_date_gap_days !=
          as.integer(ai_report_date - datadate)
      ),
    .N
  ] > 0L
) {
  stop("Nearest-report-date fallback QA failed.")
}


# A matched score must have an approved match method; an unmatched period must
# not retain filing metadata.
if (
  panel[
    !is.na(ai_score) &
      !match_method %in% c(
        "exact_report_date",
        fallback_match_method
      ),
    .N
  ] > 0L ||
  panel[
    match_method == "unmatched" &
      !is.na(ai_report_date),
    .N
  ] > 0L
) {
  stop("Matched/unmatched method QA failed.")
}


setorder(
  panel,
  cik,
  datadate
)


# ---- Create absorbing AI adoption variables -----------------------------------
#
# `ai_score` is a time-varying fiscal-period measure and may be missing in later
# years.
#
# `ai_adopted` is the main binary causal treatment. It is absorbing:
# once the first qualifying disclosure has become observable, every later
# eligible Compustat outcome remains treated regardless of whether that later
# year has a matched ai_score.
#
# A Compustat period is treated only when its datadate is strictly AFTER the
# first qualifying SEC filing_date. This prevents a filing made after a fiscal
# year-end from retroactively treating that fiscal-year outcome.
#
# Firms in the relevant EDGAR/AI history but with no qualifying disclosure are
# coded 0 throughout the observed panel.
#
# Firms without relevant AI filing history remain NA because treatment status
# is not established for them.


# Main treatment: ai_score >= 2
panel[
  ,
  ai_adoption_year := {
    
    if (!isTRUE(has_ai_history[1L])) {
      
      NA_integer_
      
    } else {
      
      disclosure_date <- first_ai_filing_date[
        !is.na(first_ai_filing_date)
      ]
      
      if (length(disclosure_date) == 0L) {
        
        0L
        
      } else {
        
        treated_years <- year[
          datadate > disclosure_date[1L]
        ]
        
        if (length(treated_years) > 0L) {
          min(treated_years)
        } else {
          # Qualifying disclosure exists, but there is no post-disclosure
          # Compustat outcome inside the observed panel.
          0L
        }
      }
    }
  },
  by = cik
]


panel[
  ,
  ai_adopted := fcase(
    !has_ai_history,
    NA_integer_,
    
    is.na(first_ai_filing_date),
    0L,
    
    datadate > first_ai_filing_date,
    1L,
    
    default = 0L
  )
]


# Strong-adoption treatment: ai_score == 3
panel[
  ,
  ai_adoption_3_year := {
    
    if (!isTRUE(has_ai_history[1L])) {
      
      NA_integer_
      
    } else {
      
      disclosure_date <- first_ai3_filing_date[
        !is.na(first_ai3_filing_date)
      ]
      
      if (length(disclosure_date) == 0L) {
        
        0L
        
      } else {
        
        treated_years <- year[
          datadate > disclosure_date[1L]
        ]
        
        if (length(treated_years) > 0L) {
          min(treated_years)
        } else {
          0L
        }
      }
    }
  },
  by = cik
]


panel[
  ,
  ai_adopted3 := fcase(
    !has_ai_history,
    NA_integer_,
    
    is.na(first_ai3_filing_date),
    0L,
    
    datadate > first_ai3_filing_date,
    1L,
    
    default = 0L
  )
]


# ---- Treatment QA --------------------------------------------------------------
#
# These checks enforce the causal timing rules and make the script fail rather
# than silently producing an invalid staggered-treatment panel.


# 1. Firms outside the AI-history universe must have undefined treatment.
if (
  panel[
    !has_ai_history &
    (
      !is.na(ai_adopted) |
      !is.na(ai_adoption_year) |
      !is.na(ai_adopted3) |
      !is.na(ai_adoption_3_year)
    ),
    .N
  ] > 0L
) {
  stop(
    "Treatment variables are defined for firms without relevant AI filing history."
  )
}


# 2. Main treatment cannot begin on or before the disclosure date.
if (
  panel[
    ai_adopted == 1L &
    (
      is.na(first_ai_filing_date) |
      datadate <= first_ai_filing_date
    ),
    .N
  ] > 0L
) {
  stop(
    "Main AI treatment is active before the first qualifying filing becomes observable."
  )
}


# 3. Every post-disclosure period must remain treated.
if (
  panel[
    has_ai_history &
    !is.na(first_ai_filing_date) &
    datadate > first_ai_filing_date &
    ai_adopted != 1L,
    .N
  ] > 0L
) {
  stop(
    "Main AI treatment is not absorbing after the first qualifying filing."
  )
}


# 4. Strong-adoption treatment cannot begin on or before its disclosure date.
if (
  panel[
    ai_adopted3 == 1L &
    (
      is.na(first_ai3_filing_date) |
      datadate <= first_ai3_filing_date
    ),
    .N
  ] > 0L
) {
  stop(
    "Strong AI treatment is active before the first score-3 filing becomes observable."
  )
}


# 5. Every post-score-3 disclosure period must remain strongly treated.
if (
  panel[
    has_ai_history &
    !is.na(first_ai3_filing_date) &
    datadate > first_ai3_filing_date &
    ai_adopted3 != 1L,
    .N
  ] > 0L
) {
  stop(
    "Strong AI treatment is not absorbing after the first score-3 filing."
  )
}


# 6. No firm may switch from treated back to untreated.

main_reversals <- panel[
  has_ai_history == TRUE,
  .(
    reversal = any(
      diff(ai_adopted[order(datadate)]) < 0L,
      na.rm = TRUE
    )
  ),
  by = cik
][
  reversal == TRUE
]

if (nrow(main_reversals) > 0L) {
  stop(
    "Found ",
    nrow(main_reversals),
    " firms that switch from main AI treatment back to untreated."
  )
}


strong_reversals <- panel[
  has_ai_history == TRUE,
  .(
    reversal = any(
      diff(ai_adopted3[order(datadate)]) < 0L,
      na.rm = TRUE
    )
  ),
  by = cik
][
  reversal == TRUE
]

if (nrow(strong_reversals) > 0L) {
  stop(
    "Found ",
    nrow(strong_reversals),
    " firms that switch from score-3 treatment back to untreated."
  )
}


# 7. Cohort year must equal the first treated annual outcome year.
main_cohort_check <- panel[
  ai_adopted == 1L,
  .(
    first_treated_year = min(year)
  ),
  by = .(
    cik,
    ai_adoption_year
  )
][
  ai_adoption_year != first_treated_year
]


if (nrow(main_cohort_check) > 0L) {
  stop(
    "ai_adoption_year does not equal the first treated outcome year for some firms."
  )
}


strong_cohort_check <- panel[
  ai_adopted3 == 1L,
  .(
    first_treated_year = min(year)
  ),
  by = .(
    cik,
    ai_adoption_3_year
  )
][
  ai_adoption_3_year != first_treated_year
]


if (nrow(strong_cohort_check) > 0L) {
  stop(
    "ai_adoption_3_year does not equal the first strongly treated outcome year for some firms."
  )
}


# 8. Firms with relevant history but no qualifying disclosure must remain
# untreated throughout the observed panel.
if (
  panel[
    has_ai_history &
    is.na(first_ai_filing_date) &
    (
      ai_adoption_year != 0L |
      ai_adopted != 0L
    ),
    .N
  ] > 0L
) {
  stop(
    "Never-treated firms have inconsistent main treatment coding."
  )
}


if (
  panel[
    has_ai_history &
    is.na(first_ai3_filing_date) &
    (
      ai_adoption_3_year != 0L |
      ai_adopted3 != 0L
    ),
    .N
  ] > 0L
) {
  stop(
    "Never-score-3 firms have inconsistent strong-treatment coding."
  )
}


cat(
  "\nTreatment QA passed:",
  "main and score-3 treatment are absorbing and correctly timed.\n"
)


# ---- Build matched and unmatched panels ---------------------------------------

panel_analysis_window <- panel[ year >= ANALYSIS_START_YEAR &
                                  year <= ANALYSIS_END_YEAR]


panel_ai <- panel_analysis_window[!is.na(ai_score)]

# Verify that the fiscal-report cohort is the first observed positive period.
# In particular, an observed g-1 row must remain below the score threshold.
for (definition in list(
  list(cohort = "ai_report_adoption_year", threshold = 2L),
  list(cohort = "ai_report_adoption_3_year", threshold = 3L)
)) {
  cohort_column <- definition$cohort
  threshold <- definition$threshold
  if (panel_ai[
    get(cohort_column) > 0L & year < get(cohort_column) &
      ai_score >= threshold,
    .N
  ] > 0L || panel_ai[
    get(cohort_column) > 0L & year == get(cohort_column) &
      ai_score < threshold,
    .N
  ] > 0L) {
    stop("Fiscal-report cohort fails first-positive-score timing QA: ", cohort_column)
  }
}


unmatched <- panel_analysis_window[is.na(ai_score)]


assert_unique_keys(panel_ai, 
                   c("cik", "datadate"),
                   "Matched Compustat + AI panel")


# ---- Matching QA --------------------------------------------------------------

analysis_rows <- nrow(panel_analysis_window)

matched_rows <- nrow(panel_ai)

unmatched_rows <- nrow(unmatched)

match_rate <- if (analysis_rows > 0L) {
  100 * matched_rows / analysis_rows
} else {
  NA_real_
}


coverage_by_year <- panel_analysis_window[
  ,
  .(
    compustat_rows = .N,
    matched_rows = sum(!is.na(ai_score)),
    coverage = mean(!is.na(ai_score))
  ),
  by = year
][
  order(year)
]


coverage_by_match_method <- panel_analysis_window[
  ,
  .(
    fiscal_periods = .N
  ),
  by = match_method
][
  order(match_method)
]


# One row per Compustat fiscal period, including why an unmatched period was not
# eligible for the nearest-date fallback. Exact matches were not evaluated for
# fallback candidates and therefore retain NA candidate counts.
match_audit <- panel_analysis_window[
  ,
  .(
    cik,
    datadate,
    year,
    ai_report_date,
    ai_filing_date,
    accession_number,
    ai_form_type,
    ai_score,
    match_method,
    report_date_gap_days,
    report_date_gap_business_days
  )
]

match_audit <- merge(
  match_audit,
  fallback_candidate_stats,
  by = c(
    "cik",
    "datadate"
  ),
  all.x = TRUE,
  sort = FALSE
)

setDT(match_audit)

match_audit[
  match_method == "unmatched" &
    is.na(fallback_eligible_candidates),
  `:=`(
    fallback_eligible_candidates = 0L,
    fallback_nearest_candidates = 0L
  )
]

match_audit[
  ,
  match_audit_status := fcase(
    match_method == "exact_report_date",
    "matched_exact",
    match_method == fallback_match_method,
    "matched_unique_nearest",
    fallback_eligible_candidates == 0L,
    "unmatched_no_candidate_within_window",
    fallback_nearest_candidates > 1L,
    "unmatched_ambiguous_nearest",
    default = "unmatched_filing_reuse_conflict"
  )
]

setorder(
  match_audit,
  cik,
  datadate
)

assert_unique_keys(
  match_audit,
  c("cik", "datadate"),
  "Compustat-EDGAR match audit"
)


cat("\nBuilt merged Compustat + AI panel.\n")

cat(
  "Matching rule: exact CIK + report date, then unique nearest report date",
  "within +/-",
  NEAREST_REPORT_MAX_BUSINESS_DAYS,
  "business days\n"
)

cat("Compustat rows:",
    format(analysis_rows, big.mark = ","),
    "\n")

cat("Matched fiscal periods:",
    format(matched_rows, big.mark = ","),
    "\n")

cat("Unmatched fiscal periods:",
    format(unmatched_rows, big.mark = ","),
    "\n")

cat("Fiscal-period coverage:",
    sprintf("%.1f%%", match_rate),
    "\n")

cat("\nCoverage by fiscal-period year:\n")

print(coverage_by_year)

cat("\nFiscal periods by match method:\n")

print(coverage_by_match_method)


# Final-year completeness QA.
#
# Merely observing some filings in ANALYSIS_END_YEAR + 1 does not prove that the
# final report year is complete. Check both the maximum filing year and whether
# exact report-date coverage collapses relative to the preceding fiscal year.

max_filing_year <- max(
  ai_filings$ai_filing_year,
  na.rm = TRUE
)

if (
  max_filing_year <
  ANALYSIS_END_YEAR + 1L
) {
  
  warning(
    paste0(
      "EDGAR scored filings end in filing year ",
      max_filing_year,
      ". Fiscal year ",
      ANALYSIS_END_YEAR,
      " is likely incomplete because many ",
      ANALYSIS_END_YEAR,
      " 10-Ks are filed in ",
      ANALYSIS_END_YEAR + 1L,
      "."
    )
  )
}


final_year_coverage <- coverage_by_year[
  year == ANALYSIS_END_YEAR,
  coverage
]

previous_year_coverage <- coverage_by_year[
  year == ANALYSIS_END_YEAR - 1L,
  coverage
]


if (
  length(final_year_coverage) == 1L &&
  length(previous_year_coverage) == 1L &&
  is.finite(final_year_coverage) &&
  is.finite(previous_year_coverage) &&
  previous_year_coverage > 0 &&
  final_year_coverage < 0.5 * previous_year_coverage
) {
  
  warning(
    paste0(
      "Fiscal-period AI-score coverage falls sharply from ",
      sprintf("%.1f%%", 100 * previous_year_coverage),
      " in ",
      ANALYSIS_END_YEAR - 1L,
      " to ",
      sprintf("%.1f%%", 100 * final_year_coverage),
      " in ",
      ANALYSIS_END_YEAR,
      ". Treat the final-year contemporaneous ai_score as incomplete. ",
      "The matched descriptive and DID source sample excludes unscored ",
      "outcome firm-years, regardless of earlier treatment history."
    )
  )
}


# ---- Save outputs -------------------------------------------------------------

if (SAVE_MERGED_OUTPUTS) {
  
  saveRDS(panel,OUTPUT_MERGED_PANEL_RDS)
  fwrite(panel, OUTPUT_MERGED_PANEL_CSV)
  
  saveRDS(panel_ai, OUTPUT_MATCHED_PANEL_RDS)
  fwrite(panel_ai, OUTPUT_MATCHED_PANEL_CSV)
  
  fwrite(unmatched, OUTPUT_UNMATCHED_CSV)

  fwrite(match_audit, OUTPUT_MATCH_AUDIT_CSV)
  
  cat("\nSaved merged panel to:",
      OUTPUT_MERGED_PANEL_RDS,
      "and",
      OUTPUT_MERGED_PANEL_CSV,
      "\n")
  
  cat("Saved matched panel to:",
      OUTPUT_MATCHED_PANEL_RDS,
      "and",
      OUTPUT_MATCHED_PANEL_CSV,
      "\n")
  
  cat("Saved unmatched fiscal periods to:",
      OUTPUT_UNMATCHED_CSV,
      "\n")

  cat("Saved Compustat-EDGAR match audit to:",
      OUTPUT_MATCH_AUDIT_CSV,
      "\n")
}
