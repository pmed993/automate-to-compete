#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Build the CIK-year lookup used by the EDGAR AI-adoption pipeline
# ------------------------------------------------------------------------------
# This script intentionally reads the WRDS SQL from:
#   code/main/comp_query.txt
# so the EDGAR lookup uses the same Compustat sample definition as the main
# annual panel. In particular, comp_query.txt is the source of truth.
# ------------------------------------------------------------------------------


# ---- Packages ----------------------------------------------------------------
packages <- c("DBI", "RPostgres", "data.table", "readr")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(packages, require, character.only = TRUE))


# ---- Helpers -----------------------------------------------------------------
normalize_cik <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.character(as.integer(as.numeric(x))))
}


# ---- User options -------------------------------------------------------------
if (!exists("LOOKUP_DIR", inherits = FALSE)) {
  LOOKUP_DIR <- file.path("code", "main", "2. get_ai_adoption", "lookup")
}

if (!exists("LOOKUP_RDS", inherits = FALSE)) {
  LOOKUP_RDS <- file.path(LOOKUP_DIR, "cik_year.rds")
}

if (!exists("LOOKUP_CSV", inherits = FALSE)) {
  LOOKUP_CSV <- file.path(LOOKUP_DIR, "cik_year.csv")
}

if (!exists("COMPUSTAT_QUERY_FILE", inherits = FALSE)) {
  COMPUSTAT_QUERY_FILE <- file.path("code", "main", "comp_query.txt")
}

if (!exists("MAX_ANALYSIS_YEAR", inherits = FALSE)) {
  MAX_ANALYSIS_YEAR <- 2025L
}


# ---- WRDS connection ----------------------------------------------------------
ua <- Sys.getenv("COMPUSTAT_USER")
psw <- Sys.getenv("COMPUSTAT_PSW")

if (!nzchar(ua) || !nzchar(psw)) {
  stop("COMPUSTAT_USER and COMPUSTAT_PSW must be set to pull the CIK-year lookup from WRDS.")
}

if (!file.exists(COMPUSTAT_QUERY_FILE)) {
  stop("Compustat query file not found: ", COMPUSTAT_QUERY_FILE)
}

con <- dbConnect(
  RPostgres::Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user = ua,
  password = psw,
  sslmode = "require",
  dbname = "wrds"
)


# ---- Pull Compustat sample using the project SQL ------------------------------
comp_query <- readLines(COMPUSTAT_QUERY_FILE, warn = FALSE) |>
  paste(collapse = " ")

comp_data <- dbGetQuery(con, comp_query)
setDT(comp_data)


# ---- Keep only normalized unique CIK-year keys --------------------------------
comp_data[, cik := normalize_cik(cik)]
comp_data[, cyear := as.integer(cyear)]

cik_year <- unique(
  comp_data[
    !is.na(cik) & cik != "" & !is.na(cyear) & cyear <= MAX_ANALYSIS_YEAR,
    .(cik, year = cyear)
  ],
  by = c("cik", "year")
)

setorder(cik_year, year, cik)


# ---- Save outputs -------------------------------------------------------------
dir.create(LOOKUP_DIR, recursive = TRUE, showWarnings = FALSE)
saveRDS(cik_year, LOOKUP_RDS)
fwrite(cik_year, LOOKUP_CSV)
try(dbDisconnect(con), silent = TRUE)


# ---- Friendly console output --------------------------------------------------
cat("\nBuilt CIK-year lookup from comp_query.txt.\n")
cat("Rows:", nrow(cik_year), "\n")
cat("Unique CIKs:", uniqueN(cik_year$cik), "\n")
cat("Year span:", min(cik_year$year), "to", max(cik_year$year), "\n")
cat("Maximum analysis year:", MAX_ANALYSIS_YEAR, "\n")
cat("Saved RDS to:", LOOKUP_RDS, "\n")
cat("Saved CSV to:", LOOKUP_CSV, "\n")
