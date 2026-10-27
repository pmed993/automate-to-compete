#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build official NAICS lookup tables
# ------------------------------------------------------------------------------
# This script reads the Census NAICS structure workbook and writes two clean
# lookup tables used by the rest of the panel-building process:
# - `naics2_lookup.csv`
# - `naics3_lookup.csv`
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")


# ---- Settings ----------------------------------------------------------------
NAICS_STRUCTURE_FILE <- get0("NAICS_STRUCTURE_FILE", ifnotfound = file.path(INPUT_DIR, "2022_NAICS_Structure.xlsx"))
NAICS2_LOOKUP_CSV <- get0("NAICS2_LOOKUP_CSV", ifnotfound = file.path(INPUT_DIR, "naics2_lookup.csv"))
NAICS3_LOOKUP_CSV <- get0("NAICS3_LOOKUP_CSV", ifnotfound = file.path(INPUT_DIR, "naics3_lookup.csv"))
SAVE_NAICS_LOOKUPS <- isTRUE(get0("SAVE_NAICS_LOOKUPS", ifnotfound = TRUE))

GROUPED_NAICS2_CODES <- c("31-33", "44-45", "48-49")


# ---- Helpers -----------------------------------------------------------------
normalize_naics_code <- function(x) {
  str_squish(as.character(x))
}

clean_naics_title <- function(x) {
  str_squish(as.character(x)) |>
    str_replace("\\s*T$", "")
}


# ---- Load Census workbook ----------------------------------------------------
if (!file.exists(NAICS_STRUCTURE_FILE)) {
  stop("NAICS structure workbook not found: ", NAICS_STRUCTURE_FILE)
}

naics_raw <- as.data.table(
  read_excel(
    NAICS_STRUCTURE_FILE,
    skip = 2,
    col_names = c("change_indicator", "naics_code", "naics_title")
  )
)

naics_raw[, naics_code := normalize_naics_code(naics_code)]
naics_raw[, naics_title := clean_naics_title(naics_title)]
naics_raw <- naics_raw[!is.na(naics_code) & naics_code != "" & !is.na(naics_title) & naics_title != ""]


# ---- Build lookup tables -----------------------------------------------------
naics2_lookup <- unique(
  naics_raw[
    str_detect(naics_code, "^[0-9]{2}$|^[0-9]{2}-[0-9]{2}$"),
    .(naics2 = naics_code, naics2_title = naics_title)
  ]
)
setorder(naics2_lookup, naics2)

naics3_lookup <- unique(
  naics_raw[
    str_detect(naics_code, "^[0-9]{3}$"),
    .(naics3 = naics_code, naics3_title = naics_title)
  ]
)
setorder(naics3_lookup, naics3)

if (naics2_lookup[, uniqueN(naics2)] != nrow(naics2_lookup)) {
  stop("NAICS2 lookup has duplicate codes.")
}
if (naics3_lookup[, uniqueN(naics3)] != nrow(naics3_lookup)) {
  stop("NAICS3 lookup has duplicate codes.")
}

missing_grouped_codes <- setdiff(GROUPED_NAICS2_CODES, naics2_lookup$naics2)
if (length(missing_grouped_codes) > 0) {
  stop("NAICS2 lookup is missing grouped sector codes: ", paste(missing_grouped_codes, collapse = ", "))
}


# ---- Save outputs ------------------------------------------------------------
if (SAVE_NAICS_LOOKUPS) {
  fwrite(naics2_lookup, NAICS2_LOOKUP_CSV)
  fwrite(naics3_lookup, NAICS3_LOOKUP_CSV)
}


# ---- Console output ----------------------------------------------------------
cat("\nBuilt official Census NAICS lookup tables.\n")
cat("Source workbook:", NAICS_STRUCTURE_FILE, "\n")
cat("NAICS2 sectors:", nrow(naics2_lookup), "\n")
cat("NAICS3 subsectors:", nrow(naics3_lookup), "\n")

if (SAVE_NAICS_LOOKUPS) {
  cat("Saved NAICS2 lookup to:", NAICS2_LOOKUP_CSV, "\n")
  cat("Saved NAICS3 lookup to:", NAICS3_LOOKUP_CSV, "\n")
}
