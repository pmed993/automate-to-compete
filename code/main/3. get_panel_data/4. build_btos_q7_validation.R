#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build BTOS Question 7 validation tables
# ------------------------------------------------------------------------------
# This script downloads or reads the Census BTOS AI Core Questions workbook,
# extracts Question 7 ("use AI in producing goods or services"), and writes:
# - NAICS2 long and summary tables
# - NAICS3 long and summary tables
#
# The summary outputs use the final `btos_q7_*` column names consumed by the
# panel merge in step 2.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")


# ---- Settings ----------------------------------------------------------------
BTOS_AI_CORE_URL <- get0(
  "BTOS_AI_CORE_URL",
  ifnotfound = "https://www.census.gov/hfp/btos/downloads/AI%20Core%20Questions.xlsx"
)
BTOS_AI_CORE_XLSX <- get0("BTOS_AI_CORE_XLSX", ifnotfound = file.path(INPUT_DIR, "BTOS_AI_Core_Questions.xlsx"))

NAICS2_LOOKUP_CSV <- get0("NAICS2_LOOKUP_CSV", ifnotfound = file.path(INPUT_DIR, "naics2_lookup.csv"))

BTOS_Q7_NAICS2_LONG_CSV <- get0("BTOS_Q7_NAICS2_LONG_CSV", ifnotfound = file.path(INPUT_DIR, "btos_q7_naics2_long.csv"))
BTOS_Q7_NAICS3_LONG_CSV <- get0("BTOS_Q7_NAICS3_LONG_CSV", ifnotfound = file.path(INPUT_DIR, "btos_q7_naics3_long.csv"))
BTOS_Q7_NAICS2_SUMMARY_CSV <- get0("BTOS_Q7_NAICS2_SUMMARY_CSV", ifnotfound = file.path(INPUT_DIR, "btos_q7_naics2_summary.csv"))
BTOS_Q7_NAICS3_SUMMARY_CSV <- get0("BTOS_Q7_NAICS3_SUMMARY_CSV", ifnotfound = file.path(INPUT_DIR, "btos_q7_naics3_summary.csv"))

DOWNLOAD_BTOS_WORKBOOK <- isTRUE(get0("DOWNLOAD_BTOS_WORKBOOK", ifnotfound = FALSE))
SAVE_BTOS_OUTPUTS <- isTRUE(get0("SAVE_BTOS_OUTPUTS", ifnotfound = TRUE))


# ---- Helpers -----------------------------------------------------------------
parse_percent_value <- function(x) {
  x_chr <- str_squish(as.character(x))
  x_chr[x_chr %in% c("", ".", "S", "N")] <- NA_character_
  parse_number(x_chr) / 100
}

# In the BTOS sector workbook, combined sectors appear under the first code in
# the Census range, so 31, 44, and 48 should map to 31-33, 44-45, and 48-49.
normalize_btos_naics2 <- function(x) {
  case_when(
    x == "31" ~ "31-33",
    x == "44" ~ "44-45",
    x == "48" ~ "48-49",
    TRUE ~ x
  )
}

read_btos_dates <- function(path) {
  dates_raw <- as.data.table(read_excel(path, sheet = "Collection and Reference Dates"))

  parse_date_col <- function(x) {
    as.Date(as.character(x), format = "%m/%d/%Y")
  }

  dates <- data.table(
    smpdt = as.character(dates_raw$Smpdt),
    collection_start = parse_date_col(dates_raw[["Col Start"]]),
    collection_end = parse_date_col(dates_raw[["Col End"]]),
    reference_start = parse_date_col(dates_raw[["Ref Start"]]),
    reference_end = parse_date_col(dates_raw[["Ref End"]]),
    publication_date = as.Date(NA)
  )

  if ("Publication Date" %in% names(dates_raw)) {
    dates[, publication_date := parse_date_col(dates_raw[["Publication Date"]])]
  }

  dates[, reference_year := as.integer(format(reference_end, "%Y"))]
  unique(dates)
}

read_btos_q7_sheet <- function(path,
                               sheet_name,
                               code_col,
                               code_pattern,
                               code_name,
                               value_col,
                               code_transform = identity) {
  raw <- read_excel(path, sheet = sheet_name)
  cycle_cols <- names(raw)[str_detect(names(raw), "^20[0-9]{4}$")]

  raw |>
    filter(`Question ID` == 7, `Answer ID` == 1) |>
    mutate(
      code_raw = str_squish(as.character(.data[[code_col]])),
      code = code_transform(code_raw)
    ) |>
    filter(str_detect(code_raw, code_pattern)) |>
    select(code, all_of(cycle_cols)) |>
    pivot_longer(
      cols = all_of(cycle_cols),
      names_to = "smpdt",
      values_to = "value_raw"
    ) |>
    transmute(
      !!code_name := code,
      smpdt = as.character(smpdt),
      !!value_col := parse_percent_value(value_raw)
    ) |>
    as.data.table()
}

build_btos_q7_long <- function(path,
                               estimate_sheet,
                               se_sheet,
                               code_col,
                               code_pattern,
                               code_name,
                               title_lookup = NULL,
                               title_col = NULL,
                               code_transform = identity) {
  estimates <- read_btos_q7_sheet(
    path = path,
    sheet_name = estimate_sheet,
    code_col = code_col,
    code_pattern = code_pattern,
    code_name = code_name,
    value_col = "btos_q7_ai_share_yes",
    code_transform = code_transform
  )

  ses <- read_btos_q7_sheet(
    path = path,
    sheet_name = se_sheet,
    code_col = code_col,
    code_pattern = code_pattern,
    code_name = code_name,
    value_col = "btos_q7_ai_share_yes_se",
    code_transform = code_transform
  )

  out <- merge(estimates, ses, by = c(code_name, "smpdt"), all = TRUE, sort = FALSE)
  out <- merge(out, read_btos_dates(path), by = "smpdt", all.x = TRUE, sort = FALSE)

  if (!is.null(title_lookup) && !is.null(title_col)) {
    out <- merge(out, title_lookup, by = code_name, all.x = TRUE, sort = FALSE)

    if (out[is.na(get(title_col)) | get(title_col) == "", .N] > 0) {
      stop("Missing lookup titles after merging BTOS data on ", code_name, ".")
    }

    setcolorder(out, c(code_name, title_col, setdiff(names(out), c(code_name, title_col))))
  }

  setorderv(out, c(code_name, "smpdt"))

  if (out[, .N, by = c(code_name, "smpdt")][N > 1, .N] > 0) {
    stop("BTOS long table has duplicate code-wave rows for ", code_name, ".")
  }

  out
}

build_btos_q7_summary <- function(long_data, code_col, title_col = NULL) {
  dt <- copy(as.data.table(long_data))
  group_cols <- if (is.null(title_col)) code_col else c(code_col, title_col)

  latest <- dt[!is.na(btos_q7_ai_share_yes)][
    order(as.integer(smpdt)),
    .SD[.N],
    by = group_cols
  ][
    ,
    .(
      btos_q7_ai_share_yes_latest = btos_q7_ai_share_yes,
      btos_q7_ai_share_yes_se_latest = btos_q7_ai_share_yes_se
    ),
    by = group_cols
  ]

  summary <- dt[
    ,
    .(
      btos_q7_n_periods = sum(!is.na(btos_q7_ai_share_yes)),
      btos_q7_n_periods_2023_2024 = sum(!is.na(btos_q7_ai_share_yes) & reference_year %in% c(2023L, 2024L)),
      btos_q7_n_periods_2023_2025 = sum(!is.na(btos_q7_ai_share_yes) & reference_year %in% c(2023L, 2024L, 2025L)),
      btos_q7_first_smpdt = if (all(is.na(btos_q7_ai_share_yes))) NA_character_ else min(smpdt[!is.na(btos_q7_ai_share_yes)]),
      btos_q7_last_smpdt = if (all(is.na(btos_q7_ai_share_yes))) NA_character_ else max(smpdt[!is.na(btos_q7_ai_share_yes)]),
      btos_q7_ai_share_yes_mean = if (all(is.na(btos_q7_ai_share_yes))) NA_real_ else mean(btos_q7_ai_share_yes, na.rm = TRUE),
      btos_q7_ai_share_yes_mean_2023_2024 = if (
        sum(!is.na(btos_q7_ai_share_yes) & reference_year %in% c(2023L, 2024L)) == 0
      ) {
        NA_real_
      } else {
        mean(btos_q7_ai_share_yes[reference_year %in% c(2023L, 2024L)], na.rm = TRUE)
      },
      btos_q7_ai_share_yes_mean_2023_2025 = if (
        sum(!is.na(btos_q7_ai_share_yes) & reference_year %in% c(2023L, 2024L, 2025L)) == 0
      ) {
        NA_real_
      } else {
        mean(btos_q7_ai_share_yes[reference_year %in% c(2023L, 2024L, 2025L)], na.rm = TRUE)
      }
    ),
    by = group_cols
  ]

  summary <- merge(summary, latest, by = group_cols, all.x = TRUE, sort = FALSE)
  summary[, btos_q7_ai_share_validation := btos_q7_ai_share_yes_mean_2023_2025]
  setorderv(summary, code_col)

  if (summary[, uniqueN(get(code_col))] != nrow(summary)) {
    stop("BTOS summary has duplicate rows for ", code_col, ".")
  }

  summary
}


# ---- Download workbook if needed ---------------------------------------------
if (DOWNLOAD_BTOS_WORKBOOK || !file.exists(BTOS_AI_CORE_XLSX)) {
  download.file(BTOS_AI_CORE_URL, BTOS_AI_CORE_XLSX, mode = "wb", quiet = TRUE)
}

if (!file.exists(BTOS_AI_CORE_XLSX)) {
  stop("BTOS AI Core workbook not found: ", BTOS_AI_CORE_XLSX)
}


# ---- Load NAICS2 lookup from step 3 ------------------------------------------
if (!file.exists(NAICS2_LOOKUP_CSV)) {
  stop("NAICS2 lookup file is missing. Build step 3 first.")
}

naics2_lookup <- fread(NAICS2_LOOKUP_CSV)
naics2_lookup[, naics2 := as.character(naics2)]


# ---- Build long BTOS validation tables ---------------------------------------
btos_q7_naics2_long <- build_btos_q7_long(
  path = BTOS_AI_CORE_XLSX,
  estimate_sheet = "Sector Estimates",
  se_sheet = "Sector SE",
  code_col = "Sector",
  code_pattern = "^[0-9]{2}$",
  code_name = "naics2",
  title_lookup = naics2_lookup,
  title_col = "naics2_title",
  code_transform = normalize_btos_naics2
)

btos_q7_naics3_long <- build_btos_q7_long(
  path = BTOS_AI_CORE_XLSX,
  estimate_sheet = "Subsector Estimates",
  se_sheet = "Subsector SE",
  code_col = "Subsector",
  code_pattern = "^[0-9]{3}$",
  code_name = "naics3"
)


# ---- Build summary tables used in step 2 -------------------------------------
btos_q7_naics2_summary <- build_btos_q7_summary(
  btos_q7_naics2_long,
  code_col = "naics2",
  title_col = "naics2_title"
)

btos_q7_naics3_summary <- build_btos_q7_summary(
  btos_q7_naics3_long,
  code_col = "naics3"
)


# ---- Save outputs ------------------------------------------------------------
if (SAVE_BTOS_OUTPUTS) {
  fwrite(btos_q7_naics2_long, BTOS_Q7_NAICS2_LONG_CSV)
  fwrite(btos_q7_naics3_long, BTOS_Q7_NAICS3_LONG_CSV)
  fwrite(btos_q7_naics2_summary, BTOS_Q7_NAICS2_SUMMARY_CSV)
  fwrite(btos_q7_naics3_summary, BTOS_Q7_NAICS3_SUMMARY_CSV)
}


# ---- Console output ----------------------------------------------------------
cat("\nBuilt BTOS Question 7 AI adoption validation tables.\n")
cat("Source workbook:", BTOS_AI_CORE_XLSX, "\n")
cat("NAICS2 sectors:", uniqueN(btos_q7_naics2_long$naics2), "\n")
cat("NAICS3 subsectors:", uniqueN(btos_q7_naics3_long$naics3), "\n")
cat(
  "Wave range:",
  min(c(btos_q7_naics2_long$smpdt, btos_q7_naics3_long$smpdt), na.rm = TRUE),
  "to",
  max(c(btos_q7_naics2_long$smpdt, btos_q7_naics3_long$smpdt), na.rm = TRUE),
  "\n"
)

if (SAVE_BTOS_OUTPUTS) {
  cat("Saved NAICS2 long table to:", BTOS_Q7_NAICS2_LONG_CSV, "\n")
  cat("Saved NAICS3 long table to:", BTOS_Q7_NAICS3_LONG_CSV, "\n")
  cat("Saved NAICS2 summary table to:", BTOS_Q7_NAICS2_SUMMARY_CSV, "\n")
  cat("Saved NAICS3 summary table to:", BTOS_Q7_NAICS3_SUMMARY_CSV, "\n")
}
