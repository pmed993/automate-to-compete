#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Validate filing-based AI adoption variable
# ------------------------------------------------------------------------------
# This script uses the FINAL CAUSAL ANALYSIS PANEL to:
# 1. validate the filing-based AI adoption treatment share against the
#    Census Annual Business Survey (ABS) at the NAICS2 level;
# 2. validate the filing-based AI adoption treatment share against BTOS
#    Question 7 at the NAICS2 level; and
# 3. describe the final analysis sample used downstream.
#
# Filing-based adoption is measured using the same absorbing binary treatment
# variable used in the causal analysis:
#
#     ai_adopted = 1 from the first post-filing fiscal period
#
# Once a firm becomes treated, it remains treated in subsequent periods.
# ------------------------------------------------------------------------------

source("code/config/global_settings.R")

library(dplyr)
library(ggplot2)


# ---- Settings ----------------------------------------------------------------
ABS_REFERENCE_YEAR <- c(2020, 2021, 2022)
ABS_QDESC <- "B70"
ABS_QDESC_LABEL <- "TECHADOPT"
ABS_BUSCHAR <- "T01A04"

BTOS_VALIDATION_START_YEAR <- 2023L
BTOS_VALIDATION_END_YEAR <- 2025L

ABS_MCB_DAT <- file.path(INPUT_DIR, "AB2200MCB01.dat")

VALIDATION_OUTPUT_DIR <- file.path(OUTPUT_DIR, "validation_adoption")
VALIDATION_FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")
VALIDATION_BUNDLE_RDS <- file.path(
  VALIDATION_OUTPUT_DIR,
  "validation_adoption_bundle.rds"
)

SAVE_VALIDATION_BUNDLE <- TRUE
SAVE_VALIDATION_FIGURES <- TRUE

VALIDATION_PLOT_BASE_SIZE <- 11
VALIDATION_POINT_SIZE <- 3.5
VALIDATION_LINE_WIDTH <- 0.6
VALIDATION_LABEL_SIZE <- 2.7
VALIDATION_CORR_LABEL_SIZE <- 4
VALIDATION_AXIS_TITLE_SIZE <- 13
VALIDATION_AXIS_TEXT_SIZE <- 11
VALIDATION_SIDE_BY_SIDE_WIDTH <- 9
VALIDATION_SIDE_BY_SIDE_HEIGHT <- 5.5


# ---- Helpers -----------------------------------------------------------------
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_cor <- function(x, y, min_n = 5) {
  keep <- !is.na(x) & !is.na(y)
  if (sum(keep) < min_n) return(NA_real_)
  if (sd(x[keep]) == 0 || sd(y[keep]) == 0) return(NA_real_)
  cor(x[keep], y[keep])
}

round_numeric_cols <- function(data, digits = 3) {
  data |>
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

safe_first_title <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0L) NA_character_ else x[1L]
}


read_abs_naics2_ai_use <- function(path) {
  if (!file.exists(path)) {
    stop("2023 ABS module file not found: ", path)
  }
  
  # AB2200MCB01.dat is several GB. Read only B70/T01A04 rows instead of
  # loading the complete Census table into memory.
  marker <- paste0("|", ABS_QDESC, "|", ABS_QDESC_LABEL, "|", ABS_BUSCHAR, "|")
  header_marker <- "#GEO_ID|GEO_LABEL|"
  quoted_path <- shQuote(path)
  search_command <- if (nzchar(Sys.which("rg"))) {
    paste(
      "rg -F --no-heading --color never -e",
      shQuote(header_marker),
      "-e",
      shQuote(marker),
      quoted_path
    )
  } else {
    paste(
      "grep -F -e",
      shQuote(header_marker),
      "-e",
      shQuote(marker),
      quoted_path
    )
  }
  
  abs_raw <- data.table::fread(
    cmd = search_command,
    sep = "|",
    colClasses = "character",
    showProgress = FALSE
  )
  
  required_cols <- c(
    "GEOTYPE", "ST", "NAICS2022", "NAICS2022_LABEL",
    "SEX", "ETH_GROUP", "RACE_GROUP", "VET_GROUP",
    "QDESC", "QDESC_LABEL", "BUSCHAR", "YEAR",
    "FIRMPDEMP", "FIRMPDEMP_F", "FIRMPDEMP_PCT",
    "FIRMPDEMP_PCT_F", "FIRMPDEMP_S", "FIRMPDEMP_PCT_S"
  )
  missing_cols <- setdiff(required_cols, names(abs_raw))
  if (length(missing_cols) > 0L) {
    stop(
      "Required columns missing from the ABS module file: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  abs_naics2 <- abs_raw |>
    filter(
      GEOTYPE == "01",
      ST == "00",
      nchar(NAICS2022) == 2L |
        NAICS2022 %in% c("31-33", "44-45", "48-49"),
      SEX == "001",
      ETH_GROUP == "001",
      RACE_GROUP == "00",
      VET_GROUP == "001",
      QDESC == ABS_QDESC,
      QDESC_LABEL == ABS_QDESC_LABEL,
      BUSCHAR == ABS_BUSCHAR,
      YEAR %in% as.character(ABS_REFERENCE_YEAR),
      is.na(FIRMPDEMP_PCT_F) | FIRMPDEMP_PCT_F == ""
    ) |>
    transmute(
      year = as.integer(YEAR),
      naics2 = NAICS2022,
      abs_naics2_title = NAICS2022_LABEL,
      abs_ai_adopting_firms = suppressWarnings(as.numeric(FIRMPDEMP)),
      abs_ai_adoption_share = suppressWarnings(as.numeric(FIRMPDEMP_PCT)) / 100
    ) |>
    filter(!is.na(abs_ai_adoption_share)) |>
    group_by(naics2, abs_naics2_title) |>
    summarise(
      abs_ai_adopting_firms = mean(abs_ai_adopting_firms, na.rm = TRUE),
      abs_ai_adoption_share = mean(abs_ai_adoption_share, na.rm = TRUE),
      n_abs_years = n_distinct(year),
      .groups = "drop"
    ) |>
    arrange(naics2)
  
  duplicate_naics2 <- abs_naics2 |>
    count(naics2) |>
    filter(n > 1L)
  if (nrow(duplicate_naics2) > 0L) {
    stop("The filtered ABS benchmark contains duplicated NAICS2 rows.")
  }
  
  abs_naics2
}


# ---- Load final causal analysis panel -----------------------------------------
if (!file.exists(ANALYSIS_PANEL_RDS)) {
  stop(
    "Final causal analysis panel not found: ",
    ANALYSIS_PANEL_RDS,
    ". Run 4. build_or_load_panel_data.R first."
  )
}

panel_ai <- readRDS(
  ANALYSIS_PANEL_RDS
) |>
  mutate(
    year = as.integer(year),
    ai_adopted = as.integer(ai_adopted),
    naics2 = as.character(naics2)
  )

if (anyNA(panel_ai$ai_adopted)) {
  stop("The final causal analysis panel contains missing `ai_adopted` values.")
}


# ---- Validation samples ------------------------------------------------------
panel_btos_validation <- panel_ai |>
  filter(
    !is.na(year),
    year >= BTOS_VALIDATION_START_YEAR,
    year <= BTOS_VALIDATION_END_YEAR,
    !is.na(ai_adopted),
    !is.na(naics2),
    naics2 != "",
    !is.na(btos_q7_ai_share_validation)
  )

panel_abs_validation <- panel_ai |>
  filter(
    !is.na(year),
    year %in% ABS_REFERENCE_YEAR,
    !is.na(ai_adopted),
    !is.na(naics2),
    naics2 != ""
  )


validation_sample_overview <- tibble::tibble(
  metric = c(
    "BTOS validation sample",
    "BTOS validation window",
    "BTOS firm-years",
    "BTOS unique firms",
    "ABS validation sample",
    "ABS validation window",
    "ABS firm-years",
    "ABS unique firms",
    "ABS NAICS2 sectors represented",
    "ABS filing-based adoption share"
  ),
  value = c(
    "Final causal analysis panel",
    paste0(BTOS_VALIDATION_START_YEAR, "-", BTOS_VALIDATION_END_YEAR),
    format(nrow(panel_btos_validation), big.mark = ","),
    format(dplyr::n_distinct(panel_btos_validation$cik), big.mark = ","),
    "Final causal analysis panel",
    paste0(min(ABS_REFERENCE_YEAR), "-", max(ABS_REFERENCE_YEAR)),
    format(nrow(panel_abs_validation), big.mark = ","),
    format(dplyr::n_distinct(panel_abs_validation$cik), big.mark = ","),
    dplyr::n_distinct(
      panel_abs_validation$naics2[
        !is.na(panel_abs_validation$naics2) &
          panel_abs_validation$naics2 != ""
      ]
    ),
    scales::percent(
      mean(panel_abs_validation$ai_adopted == 1L),
      accuracy = 0.1
    )
  )
)


# ---- Census ABS validation at NAICS2 -----------------------------------------
abs_ai_use_naics2 <- read_abs_naics2_ai_use(ABS_MCB_DAT)

# Match the combined NAICS categories used by the Census table.
panel_abs_naics2 <- panel_abs_validation |>
  mutate(
    naics2_validation = case_when(
      naics2 %in% c("31", "32", "33") ~ "31-33",
      naics2 %in% c("44", "45") ~ "44-45",
      naics2 %in% c("48", "49") ~ "48-49",
      TRUE ~ naics2
    )
  ) |>
  group_by(naics2 = naics2_validation) |>
  summarise(
    n = n(),
    n_firms = n_distinct(cik),
    filing_adoption_share = mean(ai_adopted == 1L, na.rm = TRUE),
    .groups = "drop"
  )

naics2_abs_validation <- panel_abs_naics2 |>
  inner_join(abs_ai_use_naics2, by = "naics2") |>
  mutate(
    naics2_title = abs_naics2_title
  ) |>
  arrange(desc(filing_adoption_share))

abs_validation_overview <- tibble::tibble(
  sample = "NAICS2 sectors in the 2020-2022 final causal analysis panel",
  validation_window = paste0(
    min(ABS_REFERENCE_YEAR),
    "-",
    max(ABS_REFERENCE_YEAR)
  ),
  external_measure = paste0(
    "2023 ABS: ", ABS_QDESC, "/", ABS_QDESC_LABEL, ", ", ABS_BUSCHAR,
    " (AI used in processes or methods)"
  ),
  corr_ai_adoption_share = safe_cor(
    naics2_abs_validation$filing_adoption_share,
    naics2_abs_validation$abs_ai_adoption_share
  ),
  n_industries = nrow(naics2_abs_validation)
) |>
  round_numeric_cols()


# ---- BTOS validation at NAICS2 -----------------------------------------------
naics2_btos_validation <- panel_btos_validation |>
  group_by(naics2) |>
  summarise(
    n = n(),
    n_firms = n_distinct(cik),
    naics2_title = safe_first_title(naics2_title),
    filing_adoption_share = mean(ai_adopted == 1L, na.rm = TRUE),
    btos_q7_ai_share_validation = safe_mean(
      btos_q7_ai_share_validation
    ),
    .groups = "drop"
  ) |>
  arrange(desc(filing_adoption_share))

btos_validation_overview <- tibble::tibble(
  sample = "NAICS2 industries in the final causal analysis panel",
  validation_window = paste0(
    BTOS_VALIDATION_START_YEAR,
    "-",
    BTOS_VALIDATION_END_YEAR
  ),
  external_measure = "BTOS Q7 mean yes-share (2023-2025)",
  corr_ai_adoption_share = safe_cor(
    naics2_btos_validation$filing_adoption_share,
    naics2_btos_validation$btos_q7_ai_share_validation
  ),
  n_industries = nrow(naics2_btos_validation)
) |>
  round_numeric_cols()


# ---- Side-by-side public benchmark figure ------------------------------------
validation_plot_data <- bind_rows(
  naics2_btos_validation |>
    transmute(
      benchmark = "BTOS (2023-2025)",
      naics2,
      naics2_title,
      external_ai_adoption_share = btos_q7_ai_share_validation,
      filing_adoption_share
    ),
  naics2_abs_validation |>
    transmute(
      benchmark = "Census ABS (2020-2022)",
      naics2,
      naics2_title,
      external_ai_adoption_share = abs_ai_adoption_share,
      filing_adoption_share
    )
) |>
  mutate(
    benchmark = factor(
      benchmark,
      levels = c("BTOS (2023-2025)", "Census ABS (2020-2022)")
    )
  )


validation_plot_labels <- validation_plot_data |>
  filter(
    naics2 %in%
      c("22", "21", "23", "31-33", "44-45", "51", "52", "54", "56", "61", "62")
  ) |>
  mutate(
    sector_label = case_when(
      naics2 == "22" ~ "Utilities",
      naics2 == "21" ~ "Mining",
      naics2 == "44-45" ~ "Retail trade",
      naics2 == "51" ~ "Information",
      naics2 == "54" ~ "Professional & Science",
      naics2 == "56" ~ "Admin services",
      naics2 == "61" ~ "Education",
      naics2 == "62" ~ "Health care",
      naics2 == "23" ~ "Construction",
      naics2 == "31-33" ~ "Manufacturing",
      naics2 == "52" ~ "Finance"
    ),
    label_hjust = case_when(
     naics2 %in% c("51") ~ 1.2,
      naics2 %in% c("54") ~ +1.10,
      TRUE ~ -0.05
    ),
    label_vjust = case_when(
      naics2 %in% c("22", "44-45", "52", "54", "56") ~ 1.3,
      TRUE ~ -0.65
    )
  )


validation_correlation_labels <- bind_rows(
  tibble::tibble(
    benchmark = "BTOS (2023-2025)",
    correlation = btos_validation_overview$corr_ai_adoption_share
  ),
  tibble::tibble(
    benchmark = "Census ABS (2020-2022)",
    correlation = abs_validation_overview$corr_ai_adoption_share
  )
) |>
  mutate(
    benchmark = factor(
      benchmark,
      levels = c("BTOS (2023-2025)", "Census ABS (2020-2022)")
    ),
    label = paste0(
      "Pearson r = ",
      formatC(correlation, format = "f", digits = 2)
    )
  )


validation_correlation_table <- bind_rows(
  tibble::tibble(
    benchmark = "BTOS Q7",
    reference_period = paste0(
      BTOS_VALIDATION_START_YEAR,
      "-",
      BTOS_VALIDATION_END_YEAR
    ),
    naics_level = "NAICS2",
    n_sectors = btos_validation_overview$n_industries,
    corr_ai_adoption_share = btos_validation_overview$corr_ai_adoption_share
  ),
  tibble::tibble(
    benchmark = "Census ABS",
    reference_period = paste0(
      min(ABS_REFERENCE_YEAR),
      "-",
      max(ABS_REFERENCE_YEAR)
    ),
    naics_level = "NAICS2",
    n_sectors = abs_validation_overview$n_industries,
    corr_ai_adoption_share = abs_validation_overview$corr_ai_adoption_share
  )
)


# Keep the same figure design; only the validated filing measure changes.
p_validation_side_by_side <- ggplot(
  validation_plot_data,
  aes(
    x = external_ai_adoption_share,
    y = filing_adoption_share
  )
) +
  geom_point(
    size = VALIDATION_POINT_SIZE,
    alpha = 0.9,
    fill = "#2C7FB8",
    color = "black",
    shape = 21,       # You can use 'shape' or 'pch'
    stroke = 0.5      # Optional: increase this number to make the black outline thicker
  )+
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "red",
    linewidth = VALIDATION_LINE_WIDTH,
    linetype = "dashed"
  ) +
  geom_text(
    data = validation_plot_labels,
    aes(
      label = sector_label,
      hjust = label_hjust,
      vjust = label_vjust
    ),
    size = VALIDATION_LABEL_SIZE,
    color = "gray20"
  ) +
  geom_label(
    data = validation_correlation_labels,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1.1,
    size = VALIDATION_CORR_LABEL_SIZE,
    linewidth = 0.3,
    fill = "white",
    color = "gray20"
  ) +
  facet_wrap(~ benchmark, ncol = 2, scales = "free") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  labs(
    x = "External AI adoption share",
    y = "EDGAR filing-based AI adoption share"
  ) +
  theme_minimal(base_size = VALIDATION_PLOT_BASE_SIZE) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = VALIDATION_AXIS_TITLE_SIZE),
    axis.text = element_text(size = VALIDATION_AXIS_TEXT_SIZE),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.caption = element_text(hjust = 0, color = "gray35")
  )


# ---- Final analysis panel -----------------------------------------------------
# The validation input is already the final causal analysis panel.
panel_analysis <- panel_ai

analysis_sample_overview <- tibble::tibble(
  metric = c(
    "Final analysis sample",
    "Firm-years",
    "Unique firms",
    "Years covered",
    "Treatment definition",
    "Included exchanges"
  ),
  value = c(
    "Main causal analysis panel",
    format(nrow(panel_analysis), big.mark = ","),
    format(dplyr::n_distinct(panel_analysis$cik), big.mark = ","),
    paste0(
      min(panel_analysis$year, na.rm = TRUE),
      "-",
      max(panel_analysis$year, na.rm = TRUE)
    ),
    "Absorbing treatment: ai_adopted = 1 from first post-filing fiscal period onward",
    paste(FINAL_ANALYSIS_INCLUDED_EXCHG, collapse = ", ")
  )
)


# ---- Save figures -------------------------------------------------------------
if (SAVE_VALIDATION_FIGURES) {
  dir.create(
    VALIDATION_FIGURES_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  ggsave(
    file.path(
      VALIDATION_FIGURES_DIR,
      "validation_abs_btos_naics2.png"
    ),
    p_validation_side_by_side,
    width = VALIDATION_SIDE_BY_SIDE_WIDTH,
    height = VALIDATION_SIDE_BY_SIDE_HEIGHT,
    dpi = 300
  )
  
  ggsave(
    file.path(
      VALIDATION_FIGURES_DIR,
      "validation.png"
    ),
    p_validation_side_by_side,
    width = VALIDATION_SIDE_BY_SIDE_WIDTH,
    height = VALIDATION_SIDE_BY_SIDE_HEIGHT,
    dpi = 300
  )
}


# ---- Save bundle --------------------------------------------------------------
validation_bundle <- list(
  validation_sample_overview = validation_sample_overview,
  analysis_sample_overview = analysis_sample_overview,
  abs_ai_use_naics2 = abs_ai_use_naics2,
  naics2_abs_validation = naics2_abs_validation,
  abs_validation_overview = abs_validation_overview,
  naics2_btos_validation = naics2_btos_validation,
  btos_validation_overview = btos_validation_overview,
  validation_correlation_table = validation_correlation_table,
  validation_plot_data = validation_plot_data,
  p_validation_side_by_side = p_validation_side_by_side
)

if (SAVE_VALIDATION_BUNDLE) {
  dir.create(
    VALIDATION_OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  saveRDS(
    validation_bundle,
    VALIDATION_BUNDLE_RDS
  )
}


# ---- Console output -----------------------------------------------------------
cat("\nValidated absorbing AI adoption treatment measure.\n")

cat(
  "ABS validation sample rows:",
  format(nrow(panel_abs_validation), big.mark = ","),
  "\n"
)

cat(
  "BTOS validation sample rows:",
  format(nrow(panel_btos_validation), big.mark = ","),
  "\n"
)

cat(
  "Final analysis sample rows:",
  format(nrow(panel_analysis), big.mark = ","),
  "\n"
)

cat(
  "ABS NAICS2 sectors:",
  nrow(naics2_abs_validation),
  "\n"
)

cat(
  "ABS correlation (AI adoption share):",
  abs_validation_overview$corr_ai_adoption_share,
  "\n"
)

cat(
  "BTOS NAICS2 sectors:",
  nrow(naics2_btos_validation),
  "\n"
)

cat(
  "BTOS correlation (AI adoption share):",
  btos_validation_overview$corr_ai_adoption_share,
  "\n"
)

if (SAVE_VALIDATION_BUNDLE) {
  cat(
    "Saved validation bundle to:",
    VALIDATION_BUNDLE_RDS,
    "\n"
  )
}
