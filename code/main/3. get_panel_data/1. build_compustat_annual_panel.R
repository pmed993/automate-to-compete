#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build annual Compustat panel
# ------------------------------------------------------------------------------
# This script builds `comp`, a cleaned annual Compustat panel aligned to the
# filing-based AI data. The final saved output keeps one row per `cik-year`
# in the 2015 to 2025 analysis window.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")


# ---- Settings ----------------------------------------------------------------
# Set `REFRESH_FROM_WRDS <- TRUE` before sourcing this script if you want to
# rerun the WRDS query and overwrite the local raw CSV.
REFRESH_FROM_WRDS <- isTRUE(get0("REFRESH_FROM_WRDS", ifnotfound = FALSE))
SAVE_OUTPUTS <- isTRUE(get0("SAVE_OUTPUTS", ifnotfound = FALSE))
QUIET_ANNUAL_PANEL_OUTPUT <- isTRUE(get0("QUIET_ANNUAL_PANEL_OUTPUT", ifnotfound = FALSE))

RAW_COMPUSTAT_FILE <- get0("RAW_COMPUSTAT_FILE", ifnotfound = file.path(INPUT_DIR, "comp_funda.csv"))
OUTPUT_PANEL_RDS <- get0("OUTPUT_PANEL_RDS", ifnotfound = file.path(INPUT_DIR, "compustat_annual_panel.rds"))
OUTPUT_PANEL_CSV <- get0("OUTPUT_PANEL_CSV", ifnotfound = file.path(INPUT_DIR, "compustat_annual_panel.csv"))
NAICS2_LOOKUP_CSV <- get0("NAICS2_LOOKUP_CSV", ifnotfound = file.path(INPUT_DIR, "naics2_lookup.csv"))
NAICS3_LOOKUP_CSV <- get0("NAICS3_LOOKUP_CSV", ifnotfound = file.path(INPUT_DIR, "naics3_lookup.csv"))

# Keep enough pre-panel history for nested lag constructions. In particular,
# CAPX and R&D intensity in t-1 use beginning assets from t-2, and a 2015
# filing-year observation can correspond to fiscal year 2014. Fiscal-year 2012
# is therefore required to construct complete 2015 lagged intensities.
RAW_START_YEAR <- as.integer(get0("RAW_START_YEAR", ifnotfound = 2012L))
PANEL_START_YEAR <- as.integer(get0("PANEL_START_YEAR", ifnotfound = ANALYSIS_START_YEAR))
PANEL_END_YEAR <- as.integer(get0("PANEL_END_YEAR", ifnotfound = ANALYSIS_END_YEAR))

REQUIRED_COLS <- c(
  "gvkey", "cik", "fyear", "datadate", "indfmt", "datafmt",
  "consol", "costat", "popsrc", "naics", "fic", "first_compustat_fyear"
)

CORE_NUMERIC_COLS <- c(
  "act", "at", "che", "csho", "dlc", "dltt", "dv", "intan", "ni", "xrd",
  "xsga", "capx", "oancf", "prstkc", "prcc_f", "tstk", "sale", "emp",
  "xlr", "oibdp"
)


# ---- Helpers -----------------------------------------------------------------
winsorize_vec <- function(x, p_lo = 0.01, p_hi = 0.99) {
  if (all(is.na(x))) return(x)
  q <- quantile(x, probs = c(p_lo, p_hi), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

safe_log <- function(x) {
  out <- rep(NA_real_, length(x))
  keep <- !is.na(x) & x > 0
  out[keep] <- log(x[keep])
  out
}

normalize_naics2 <- function(x) {
  x <- fifelse(is.na(x), NA_character_, as.character(x))
  x[x %in% c("31", "32", "33")] <- "31-33"
  x[x %in% c("44", "45")] <- "44-45"
  x[x %in% c("48", "49")] <- "48-49"
  x
}

refresh_compustat_from_wrds <- function(output_file, raw_start_year) {
  user <- Sys.getenv("COMPUSTAT_USER")
  password <- Sys.getenv("COMPUSTAT_PSW")

  if (!nzchar(user) || !nzchar(password)) {
    stop("COMPUSTAT_USER and COMPUSTAT_PSW must be set when REFRESH_FROM_WRDS = TRUE.")
  }

  con <- dbConnect(
    RPostgres::Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    port = 9737,
    user = user,
    password = password,
    sslmode = "require",
    dbname = "wrds"
  )
  on.exit(dbDisconnect(con), add = TRUE)

  query <- paste(readLines("code/main/comp_query.txt"), collapse = " ")
  updated_query <- sub(
    "f\\.datadate\\s*>=\\s*'[0-9]{4}-[0-9]{2}-[0-9]{2}'",
    sprintf("f.datadate >= '%d-01-01'", raw_start_year),
    query
  )

  if (identical(updated_query, query)) {
    stop("Could not update the WRDS query start date in code/main/comp_query.txt.")
  }

  query <- updated_query
  raw_dt <- as.data.table(dbGetQuery(con, query))
  fwrite(raw_dt, output_file)
}

read_lookup <- function(path, key) {
  if (!file.exists(path)) {
    stop("Lookup file not found: ", path)
  }

  lookup <- fread(path)
  lookup[, (key) := as.character(get(key))]

  if (uniqueN(lookup[[key]]) != nrow(lookup)) {
    stop("Lookup file has duplicate ", key, " values: ", path)
  }

  lookup
}


# ---- Optional WRDS refresh ----------------------------------------------------
if (REFRESH_FROM_WRDS) {
  refresh_compustat_from_wrds(RAW_COMPUSTAT_FILE, RAW_START_YEAR)
}


# ---- Load raw Compustat data --------------------------------------------------
if (!file.exists(RAW_COMPUSTAT_FILE)) {
  stop("Raw Compustat file not found: ", RAW_COMPUSTAT_FILE)
}

dt <- fread(RAW_COMPUSTAT_FILE)

missing_cols <- setdiff(REQUIRED_COLS, names(dt))
if (length(missing_cols) > 0) {
  stop("Raw Compustat file is missing required columns: ", paste(missing_cols, collapse = ", "))
}


# ---- Standardize key columns --------------------------------------------------
dt[, gvkey := as.character(gvkey)]
dt[, cik := fifelse(is.na(cik) | cik == "", NA_character_, as.character(as.integer(as.numeric(cik))))]
dt[, fyear := as.integer(fyear)]
dt[, first_compustat_fyear := as.integer(first_compustat_fyear)]
dt[, datadate := as.Date(datadate)]
dt[, naics := suppressWarnings(as.integer(naics))]

if ("cyear" %in% names(dt)) dt[, cyear := as.integer(cyear)]
if ("sic" %in% names(dt)) dt[, sic := suppressWarnings(as.integer(sic))]
dt[, filing_year := fcoalesce(cyear, as.integer(format(datadate, "%Y")))]

numeric_cols <- intersect(CORE_NUMERIC_COLS, names(dt))
dt[, (numeric_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = numeric_cols]


# ---- Apply the annual sample filters ------------------------------------------
dt <- dt[
  indfmt == "INDL" &
    datafmt == "STD" &
    consol == "C" &
    costat == "A" &
    popsrc == "D" &
    fic == "USA" &
    !is.na(naics) &
    !is.na(fyear) &
    datadate >= as.Date(sprintf("%d-01-01", RAW_START_YEAR)) &
    fyear >= RAW_START_YEAR
]


# ---- Keep the best row in duplicate firm-years --------------------------------
# If a firm-year appears more than once, keep the row with the fewest missing
# core accounting variables. If there is still a tie, keep the latest datadate.
core_cols <- intersect(CORE_NUMERIC_COLS, names(dt))
dt[, missing_core_count := rowSums(is.na(.SD)), .SDcols = core_cols]

dt <- dt[
  order(gvkey, fyear, missing_core_count, -as.numeric(datadate)),
  .SD[1],
  by = .(gvkey, fyear)
]

dt[, missing_core_count := NULL]
setorder(dt, gvkey, fyear)


# ---- Add clean NAICS codes and titles -----------------------------------------
dt[, naics_chr := fifelse(is.na(naics), NA_character_, as.character(naics))]
dt[, naics2 := fifelse(!is.na(naics_chr) & nchar(naics_chr) >= 2, substr(naics_chr, 1, 2), NA_character_)]
dt[, naics3 := fifelse(!is.na(naics_chr) & nchar(naics_chr) >= 3, substr(naics_chr, 1, 3), NA_character_)]
dt[, naics4 := fifelse(!is.na(naics_chr) & nchar(naics_chr) >= 4, substr(naics_chr, 1, 4), NA_character_)]
dt[, naics2 := normalize_naics2(naics2)]
dt[, naics_chr := NULL]

naics2_lookup <- read_lookup(NAICS2_LOOKUP_CSV, "naics2")
naics3_lookup <- read_lookup(NAICS3_LOOKUP_CSV, "naics3")

dt <- merge(dt, naics2_lookup, by = "naics2", all.x = TRUE, sort = FALSE)
dt <- merge(dt, naics3_lookup, by = "naics3", all.x = TRUE, sort = FALSE)
setorder(dt, gvkey, fyear)


# ---- Start the final panel ----------------------------------------------------
comp <- copy(dt)


# ---- Build balance-sheet and operating measures -------------------------------
comp[, total_debt := fifelse(
  is.na(dlc) & is.na(dltt),
  NA_real_,
  fcoalesce(dlc, 0) + fcoalesce(dltt, 0)
)]

comp[, market_cap := fifelse(!is.na(csho) & !is.na(prcc_f), csho * prcc_f, NA_real_)]

comp[, log_at := safe_log(at)]
comp[, log_market_cap := safe_log(market_cap)]
comp[, log_sale := safe_log(sale)]
comp[, log_emp := safe_log(emp)]
comp[, log_xopr := safe_log(xopr)]

comp[, firm_age := fifelse(
  !is.na(first_compustat_fyear) & fyear >= first_compustat_fyear,
  fyear - first_compustat_fyear,
  NA_integer_
)]

comp[, leverage := fifelse(!is.na(at) & at > 0 & !is.na(total_debt), total_debt / at, NA_real_)]
comp[, cash_ratio := fifelse(!is.na(at) & at > 0, che / at, NA_real_)]
comp[, roa := fifelse(!is.na(at) & at > 0, ni / at, NA_real_)]
comp[, intangibles_ratio := fifelse(!is.na(at) & at > 0, intan / at, NA_real_)]
comp[, markup := fifelse(!is.na(sale) & sale > 0, oibdp / sale, NA_real_)]

comp[, avg_wage := fifelse(
  !is.na(emp) & emp > 0 & !is.na(xlr) & xlr > 0,
  xlr / emp,
  NA_real_
)]
comp[, log_avg_wage := safe_log(avg_wage)]

comp[, labor_productivity := fifelse(
  !is.na(emp) & emp > 0 & !is.na(sale) & sale > 0,
  sale / emp,
  NA_real_
)]
comp[, log_labor_productivity := safe_log(labor_productivity)]

comp[, value_added := fifelse(
  !is.na(oibdp) | !is.na(xlr),
  fcoalesce(oibdp, 0) + fcoalesce(xlr, 0),
  NA_real_
)]
comp[, value_added_per_employee := fifelse(
  !is.na(emp) & emp > 0 & !is.na(value_added) & value_added > 0,
  value_added / emp,
  NA_real_
)]
comp[, log_value_added_per_employee := safe_log(value_added_per_employee)]


# ---- Add lagged variables -----------------------------------------------------
# A lag is valid only when the previous Compustat record is the immediately
# preceding fiscal year. Keep the source year and validity flag in the saved
# panel so downstream models can audit the timing without recomputing lags.
comp[, lag_source_fyear := shift(fyear), by = gvkey]
comp[, lag_is_consecutive := !is.na(lag_source_fyear) & fyear == lag_source_fyear + 1L]

comp[, `:=`(
  at_l1 = fifelse(lag_is_consecutive, shift(at), NA_real_),
  log_at_l1 = fifelse(lag_is_consecutive, shift(log_at), NA_real_),
  log_emp_l1 = fifelse(lag_is_consecutive, shift(log_emp), NA_real_),
  markup_l1 = fifelse(lag_is_consecutive, shift(markup), NA_real_),
  leverage_l1 = fifelse(lag_is_consecutive, shift(leverage), NA_real_),
  cash_ratio_l1 = fifelse(lag_is_consecutive, shift(cash_ratio), NA_real_),
  roa_l1 = fifelse(lag_is_consecutive, shift(roa), NA_real_),
  firm_age_l1 = fifelse(lag_is_consecutive, shift(firm_age), NA_integer_),
  log_labor_productivity_l1 = fifelse(
    lag_is_consecutive,
    shift(log_labor_productivity),
    NA_real_
  )
), by = gvkey]


# ---- Build investment and valuation measures ----------------------------------
comp[, asset_growth_y := fifelse(
  !is.na(at_l1) & at_l1 > 0 & !is.na(at),
  (at - at_l1) / at_l1,
  NA_real_
)]

comp[, rd_reporter := !is.na(xrd)]

comp[, rd_intensity_y := fifelse(
  !is.na(at_l1) & at_l1 > 0 & !is.na(xrd),
  xrd / at_l1,
  NA_real_
)]

comp[, capx_intensity_y := fifelse(
  !is.na(at_l1) & at_l1 > 0 & !is.na(capx),
  capx / at_l1,
  NA_real_
)]

comp[, total_inv_intensity_y := fifelse(
  !is.na(at_l1) & at_l1 > 0 & (!is.na(xrd) | !is.na(capx)),
  (fcoalesce(xrd, 0) + fcoalesce(capx, 0)) / at_l1,
  NA_real_
)]

comp[, rd_intensity_y_w := winsorize_vec(rd_intensity_y)]
comp[, capx_intensity_y_w := winsorize_vec(capx_intensity_y)]
comp[, total_inv_intensity_y_w := winsorize_vec(total_inv_intensity_y)]

comp[, tobins_q := fifelse(
  !is.na(at) & at > 0 & !is.na(market_cap) & !is.na(total_debt),
  (market_cap + total_debt) / at,
  NA_real_
)]

comp[, operating_profitability := fifelse(
  is.finite(oibdp) & is.finite(at) & at > 0,
  oibdp / at,
  NA_real_
)]

# Do not winsorize this outcome here. The pooled 1st/99th-percentile bounds
# must be estimated after the project's year and exchange eligibility filters
# are applied. `4. build_or_load_panel_data.R` performs that transformation
# once and applies the same bounds to every treatment group, cohort, and year.

comp[, `:=`(
  capx_intensity_l1 = fifelse(
    lag_is_consecutive,
    shift(capx_intensity_y_w),
    NA_real_
  ),
  rd_intensity_l1 = fifelse(
    lag_is_consecutive,
    shift(rd_intensity_y_w),
    NA_real_
  ),
  rd_reporter_l1 = fifelse(lag_is_consecutive, shift(rd_reporter), NA),
  tobins_q_l1 = fifelse(lag_is_consecutive, shift(tobins_q), NA_real_)
), by = gvkey]


# ---- Keep the comparable cik-year analysis panel ------------------------------
# The LLM extraction data are keyed on cik-year, where year is the filing
# calendar year. Match that structure here before saving the annual panel.
comp[, year := filing_year]
comp <- comp[
  !is.na(cik) &
    cik != "" &
    !is.na(year) &
    year >= PANEL_START_YEAR &
    year <= PANEL_END_YEAR
]

comp[, output_missing_core_count := rowSums(is.na(.SD)), .SDcols = core_cols]
comp <- comp[
  order(cik, year, output_missing_core_count, -as.numeric(datadate)),
  .SD[1],
  by = .(cik, year)
]
comp[, output_missing_core_count := NULL]
setorder(comp, cik, year)


# ---- Save outputs -------------------------------------------------------------
if (SAVE_OUTPUTS) {
  saveRDS(comp, OUTPUT_PANEL_RDS)
  fwrite(comp, OUTPUT_PANEL_CSV)
}


# ---- Console output -----------------------------------------------------------
if (!QUIET_ANNUAL_PANEL_OUTPUT) {
  cat("\nBuilt annual Compustat panel.\n")
  cat("Rows:", format(nrow(comp), big.mark = ","), "\n")
  cat("Unique CIKs:", format(uniqueN(comp$cik), big.mark = ","), "\n")
  cat("Years:", min(comp$year, na.rm = TRUE), "to", max(comp$year, na.rm = TRUE), "\n")

  if (SAVE_OUTPUTS) {
    cat("Saved panel to:", OUTPUT_PANEL_RDS, "and", OUTPUT_PANEL_CSV, "\n")
  }
}
