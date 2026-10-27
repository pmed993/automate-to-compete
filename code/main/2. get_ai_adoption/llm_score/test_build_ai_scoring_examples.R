#!/usr/bin/env Rscript

source("code/main/2. get_ai_adoption/llm_score/build_ai_scoring_examples.R")

test_root <- tempfile("ai_scoring_examples_")
dir.create(test_root, recursive = TRUE)
rds_dir <- file.path(test_root, "rds")
dir.create(rds_dir)

selection_path <- file.path(test_root, "selection.csv")
audit_path <- file.path(test_root, "audit.csv")
manifest_path <- file.path(test_root, "manifest.csv")
out_path <- file.path(test_root, "examples.csv")

selection <- data.frame(
  score = c(1L, 2L, 3L),
  cik = c("1001", "1002", "1003"),
  year = c(2020L, 2021L, 2022L),
  filing_accession = c("0001-20-000001", "0002-21-000002", "0003-22-000003"),
  window_id = c(1L, 2L, 3L),
  display_firm_name = c("Firm One", "Firm Two", "Firm Three"),
  stringsAsFactors = FALSE
)
write.csv(selection, selection_path, row.names = FALSE)

audit <- data.frame(
  cik = selection$cik,
  year = selection$year,
  filing_accession = selection$filing_accession,
  chunk_id = c("score_chunk_1", "score_chunk_2", "score_chunk_3"),
  ai_score = selection$score,
  parse_status = "success",
  score_status = "ok",
  snippet_text_length = c(20L, 30L, 40L),
  snippet_at_limit = FALSE,
  stringsAsFactors = FALSE
)
write.csv(audit, audit_path, row.names = FALSE)

manifest <- data.frame(
  accession_number = selection$filing_accession,
  cik = selection$cik,
  year = selection$year,
  company_name = c("Manifest One", "Manifest Two", "Manifest Three"),
  form_type = "10-K",
  stringsAsFactors = FALSE
)
write.csv(manifest, manifest_path, row.names = FALSE)

windows <- data.frame(
  item = "keyword_window",
  cik = selection$cik,
  year = selection$year,
  accession_number = selection$filing_accession,
  form_type = "10-K",
  window_id = selection$window_id,
  keyword_hit_count = c(1L, 1L, 3L),
  matched_terms = c("AI", "artificial intelligence", "Deep learning|deep learning|AI"),
  text = c(
    "Avian influenza (AI) affected livestock.",
    "We are piloting artificial intelligence for claims.",
    "Our deep learning products enable AI services."
  ),
  stringsAsFactors = FALSE
)
saveRDS(windows[1:2, ], file.path(rds_dir, "extract_df_chunk_00001.rds"))
saveRDS(windows[3, , drop = FALSE], file.path(rds_dir, "extract_df_chunk_00002.rds"))

result <- build_ai_scoring_examples(
  snippet_audit = audit_path,
  filing_manifest = manifest_path,
  rds_dir = rds_dir,
  selection = selection_path,
  out = out_path
)

stopifnot(file.exists(out_path))
stopifnot(nrow(result) == 3L)
stopifnot(identical(result$score, 1:3))
stopifnot(identical(result$firm_name, selection$display_firm_name))
stopifnot(identical(result$source_chunk, c(
  "extract_df_chunk_00001",
  "extract_df_chunk_00001",
  "extract_df_chunk_00002"
)))
stopifnot(result$keyword_hits[[3L]] == "Deep learning; AI")
stopifnot(result$actual_extracted_chunk[[2L]] == windows$text[[2L]])

bad_selection <- selection
bad_selection$score[[2L]] <- 3L
bad_selection_path <- file.path(test_root, "bad_selection.csv")
write.csv(bad_selection, bad_selection_path, row.names = FALSE)
score_mismatch_failed <- tryCatch(
  {
    build_ai_scoring_examples(
      snippet_audit = audit_path,
      filing_manifest = manifest_path,
      rds_dir = rds_dir,
      selection = bad_selection_path,
      out = out_path
    )
    FALSE
  },
  error = function(error) grepl("score mismatch", conditionMessage(error), fixed = TRUE)
)
stopifnot(score_mismatch_failed)

cat("build_ai_scoring_examples tests passed\n")
