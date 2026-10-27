#!/usr/bin/env Rscript

# Build a small, auditable CSV of illustrative AI-adoption scoring examples.
#
# This is a post-merge step. It joins:
#   1. final scores from llm_extraction_snippet_audit.csv;
#   2. firm names and filing metadata from filing_manifest.csv; and
#   3. exact keyword-window text from the assembled EDGAR RDS chunks.
#
# The curated selection CSV controls which filing/window is displayed for each
# score. Only those windows are retained; the script reads RDS files one at a
# time and releases each one immediately to keep memory use bounded.

usage <- function() {
  paste(
    "Usage:",
    "  Rscript build_ai_scoring_examples.R \\",
    "    --snippet-audit PATH \\",
    "    --filing-manifest PATH \\",
    "    --rds-dir PATH \\",
    "    [--selection PATH] \\",
    "    [--out PATH]",
    "",
    "Required:",
    "  --snippet-audit   Merged llm_extraction_snippet_audit.csv.",
    "  --filing-manifest EDGAR filing_manifest.csv containing company_name.",
    "  --rds-dir          Folder containing extract_df_chunk_XXXXX.rds files.",
    "",
    "Optional:",
    "  --selection        Curated filing/window selection CSV.",
    "  --out              Output CSV path.",
    sep = "\n"
  )
}

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    script_path <- sub("^--file=", "", file_arg)
    # Rscript encodes spaces as "~+~" in --file on some macOS builds.
    script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
    return(dirname(normalizePath(script_path, mustWork = FALSE)))
  }
  getwd()
}

parse_cli <- function(args) {
  defaults <- list(
    selection = file.path(script_dir(), "scoring_example_selection.csv"),
    out = file.path(
      "output", "summary_stats", "ai_scoring_example",
      "ai_adoption_scoring_examples.csv"
    )
  )
  if (any(args %in% c("-h", "--help"))) {
    cat(usage(), "\n")
    return(NULL)
  }
  if (length(args) %% 2L != 0L) {
    stop("Every command-line option must be followed by a value.\n", usage())
  }
  values <- defaults
  if (length(args)) {
    for (i in seq(1L, length(args), by = 2L)) {
      key <- sub("^--", "", args[[i]])
      key <- gsub("-", "_", key, fixed = TRUE)
      if (!key %in% c("snippet_audit", "filing_manifest", "rds_dir", "selection", "out")) {
        stop("Unknown option: ", args[[i]], "\n", usage())
      }
      values[[key]] <- args[[i + 1L]]
    }
  }
  missing <- setdiff(
    c("snippet_audit", "filing_manifest", "rds_dir"),
    names(values)[vapply(values, function(x) length(x) == 1L && nzchar(x), logical(1))]
  )
  if (length(missing)) {
    stop("Missing required option(s): --", paste(gsub("_", "-", missing), collapse = ", --"), "\n", usage())
  }
  values
}

read_character_csv <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path)
  read.csv(
    path,
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
}

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  }
}

normalize_cik <- function(x) {
  value <- trimws(as.character(x))
  value <- sub("^0+", "", value)
  value[is.na(value) | value == ""] <- NA_character_
  value
}

normalize_year <- function(x) {
  value <- suppressWarnings(as.integer(as.character(x)))
  ifelse(is.na(value), NA_character_, as.character(value))
}

normalize_window_id <- function(x) {
  value <- suppressWarnings(as.integer(as.character(x)))
  ifelse(is.na(value), NA_character_, as.character(value))
}

normalize_accession <- function(x) {
  value <- gsub("[^0-9]", "", trimws(as.character(x)))
  value[is.na(value) | value == ""] <- NA_character_
  value
}

filing_key <- function(cik, year, accession) {
  paste(normalize_cik(cik), normalize_year(year), normalize_accession(accession), sep = "|")
}

window_key <- function(cik, year, accession, window_id) {
  paste(
    normalize_cik(cik),
    normalize_year(year),
    normalize_accession(accession),
    normalize_window_id(window_id),
    sep = "|"
  )
}

display_matched_terms <- function(x) {
  terms <- trimws(unlist(strsplit(as.character(x), "|", fixed = TRUE)))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(NA_character_)
  terms <- terms[!duplicated(tolower(terms))]
  paste(terms, collapse = "; ")
}

load_and_validate_selection <- function(path) {
  selection <- read_character_csv(path, "Selection CSV")
  require_columns(
    selection,
    c("score", "cik", "year", "filing_accession", "window_id"),
    "Selection CSV"
  )
  if (!"display_firm_name" %in% names(selection)) {
    selection$display_firm_name <- NA_character_
  }
  selection$score <- suppressWarnings(as.integer(selection$score))
  if (any(is.na(selection$score) | !selection$score %in% 1:3)) {
    stop("Selection score must contain only 1, 2, or 3.")
  }
  selection$selection_order <- seq_len(nrow(selection))
  selection$filing_key <- filing_key(
    selection$cik, selection$year, selection$filing_accession
  )
  selection$window_key <- window_key(
    selection$cik, selection$year, selection$filing_accession, selection$window_id
  )
  if (anyDuplicated(selection$window_key)) {
    stop("Selection CSV contains duplicate filing/window rows.")
  }
  selection
}

match_scores <- function(selection, audit_path) {
  audit <- read_character_csv(audit_path, "Snippet audit")
  require_columns(
    audit,
    c("cik", "year", "filing_accession", "chunk_id", "ai_score", "parse_status"),
    "Snippet audit"
  )
  audit$filing_key <- filing_key(audit$cik, audit$year, audit$filing_accession)
  audit <- audit[audit$filing_key %in% selection$filing_key, , drop = FALSE]
  duplicates <- unique(audit$filing_key[duplicated(audit$filing_key)])
  if (length(duplicates)) {
    stop(
      "Snippet audit contains duplicate rows for selected filing(s): ",
      paste(duplicates, collapse = ", "),
      ". Merge a non-overlapping scoring run first."
    )
  }
  index <- match(selection$filing_key, audit$filing_key)
  if (anyNA(index)) {
    stop(
      "Selected filing(s) not found in snippet audit: ",
      paste(selection$filing_key[is.na(index)], collapse = ", ")
    )
  }
  matched <- audit[index, , drop = FALSE]
  actual_score <- suppressWarnings(as.integer(matched$ai_score))
  mismatch <- is.na(actual_score) | actual_score != selection$score
  if (any(mismatch)) {
    details <- paste0(
      selection$filing_key[mismatch],
      " expected ", selection$score[mismatch],
      " but audit reports ", matched$ai_score[mismatch]
    )
    stop("Selected example score mismatch: ", paste(details, collapse = "; "))
  }
  selection$scoring_chunk <- matched$chunk_id
  selection$score_status <- if ("score_status" %in% names(matched)) matched$score_status else NA_character_
  selection$parse_status <- matched$parse_status
  selection$model_snippet_chars <- if ("snippet_text_length" %in% names(matched)) {
    suppressWarnings(as.integer(matched$snippet_text_length))
  } else {
    NA_integer_
  }
  selection$model_snippet_at_limit <- if ("snippet_at_limit" %in% names(matched)) {
    value <- tolower(trimws(as.character(matched$snippet_at_limit)))
    ifelse(value == "true", TRUE, ifelse(value == "false", FALSE, NA))
  } else {
    NA
  }
  selection
}

match_firm_names <- function(selection, manifest_path) {
  manifest <- read_character_csv(manifest_path, "Filing manifest")
  require_columns(
    manifest,
    c("accession_number", "cik", "year", "company_name", "form_type"),
    "Filing manifest"
  )
  manifest$filing_key <- filing_key(
    manifest$cik, manifest$year, manifest$accession_number
  )
  manifest <- manifest[manifest$filing_key %in% selection$filing_key, , drop = FALSE]
  if (anyDuplicated(manifest$filing_key)) {
    manifest <- manifest[!duplicated(manifest$filing_key), , drop = FALSE]
  }
  index <- match(selection$filing_key, manifest$filing_key)
  if (anyNA(index)) {
    stop(
      "Selected filing(s) not found in filing manifest: ",
      paste(selection$filing_key[is.na(index)], collapse = ", ")
    )
  }
  manifest_name <- manifest$company_name[index]
  use_override <- !is.na(selection$display_firm_name) & nzchar(trimws(selection$display_firm_name))
  selection$firm_name <- ifelse(use_override, selection$display_firm_name, manifest_name)
  selection$manifest_form <- manifest$form_type[index]
  selection
}

extract_selected_windows <- function(selection, rds_dir) {
  if (!dir.exists(rds_dir)) stop("RDS directory not found: ", rds_dir)
  files <- sort(list.files(
    rds_dir,
    pattern = "^extract_df_chunk_[0-9]{5}\\.rds$",
    full.names = TRUE
  ))
  if (!length(files)) stop("No assembled extract_df_chunk_XXXXX.rds files found in: ", rds_dir)

  found <- vector("list", nrow(selection))
  for (path in files) {
    chunk <- readRDS(path)
    require_columns(
      chunk,
      c(
        "cik", "year", "accession_number", "form_type", "window_id",
        "keyword_hit_count", "matched_terms", "text"
      ),
      basename(path)
    )
    keys <- window_key(chunk$cik, chunk$year, chunk$accession_number, chunk$window_id)
    selected_index <- match(keys, selection$window_key)
    rows <- which(!is.na(selected_index))
    if (length(rows)) {
      for (row in rows) {
        target <- selected_index[[row]]
        if (!is.null(found[[target]])) {
          stop("Selected window appears in more than one RDS row: ", selection$window_key[[target]])
        }
        found[[target]] <- data.frame(
          form = trimws(as.character(chunk$form_type[[row]])),
          keyword_hits = display_matched_terms(chunk$matched_terms[[row]]),
          keyword_hit_count = suppressWarnings(as.integer(chunk$keyword_hit_count[[row]])),
          actual_extracted_chunk = trimws(as.character(chunk$text[[row]])),
          source_chunk = sub("\\.rds$", "", basename(path)),
          stringsAsFactors = FALSE
        )
      }
    }
    rm(chunk)
    invisible(gc(verbose = FALSE))
    if (all(vapply(found, Negate(is.null), logical(1)))) break
  }

  missing <- which(vapply(found, is.null, logical(1)))
  if (length(missing)) {
    stop(
      "Selected window(s) not found in assembled RDS chunks: ",
      paste(selection$window_key[missing], collapse = ", ")
    )
  }
  do.call(rbind, found)
}

build_ai_scoring_examples <- function(
  snippet_audit,
  filing_manifest,
  rds_dir,
  selection,
  out
) {
  selected <- load_and_validate_selection(selection)
  selected <- match_scores(selected, snippet_audit)
  selected <- match_firm_names(selected, filing_manifest)
  windows <- extract_selected_windows(selected, rds_dir)

  invalid_window <- is.na(windows$keyword_hit_count) |
    windows$keyword_hit_count < 1L |
    is.na(windows$keyword_hits) |
    !nzchar(windows$keyword_hits) |
    is.na(windows$actual_extracted_chunk) |
    !nzchar(windows$actual_extracted_chunk)
  if (any(invalid_window)) {
    stop("Every selected example must contain at least one keyword hit and non-empty text.")
  }

  form_mismatch <- !is.na(selected$manifest_form) &
    nzchar(selected$manifest_form) &
    toupper(trimws(selected$manifest_form)) != toupper(trimws(windows$form))
  if (any(form_mismatch)) {
    stop("Form mismatch between filing manifest and selected RDS window.")
  }

  result <- data.frame(
    score = selected$score,
    firm_name = selected$firm_name,
    year = suppressWarnings(as.integer(selected$year)),
    form = windows$form,
    keyword_hits = windows$keyword_hits,
    keyword_hit_count = windows$keyword_hit_count,
    actual_extracted_chunk = windows$actual_extracted_chunk,
    source_chunk = windows$source_chunk,
    window_id = suppressWarnings(as.integer(selected$window_id)),
    scoring_chunk = selected$scoring_chunk,
    cik = normalize_cik(selected$cik),
    filing_accession = selected$filing_accession,
    parse_status = selected$parse_status,
    score_status = selected$score_status,
    model_snippet_chars = selected$model_snippet_chars,
    model_snippet_at_limit = selected$model_snippet_at_limit,
    stringsAsFactors = FALSE
  )
  result <- result[order(selected$selection_order), , drop = FALSE]

  output_dir <- dirname(out)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(result, out, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  cat(
    sprintf(
      "Wrote %d verified scoring example(s) to %s\n",
      nrow(result), normalizePath(out, mustWork = FALSE)
    )
  )
  result
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- parse_cli(args)
  if (is.null(options)) return(invisible(0L))
  build_ai_scoring_examples(
    snippet_audit = options$snippet_audit,
    filing_manifest = options$filing_manifest,
    rds_dir = options$rds_dir,
    selection = options$selection,
    out = options$out
  )
  invisible(0L)
}

if (sys.nframe() == 0L) {
  tryCatch(
    main(),
    error = function(error) {
      message("ERROR: ", conditionMessage(error))
      quit(status = 1L, save = "no")
    }
  )
}
