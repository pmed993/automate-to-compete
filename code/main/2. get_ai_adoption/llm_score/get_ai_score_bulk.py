#!/usr/bin/env python3
"""Bulk LLM scoring for filing-level AI adoption.

The process scores firm-level AI adoption from SEC Form 10-K disclosures.
It reads EDGAR whole-filing keyword-window chunks from local files or AWS S3,
combines them into one row per filing, optionally filters to a research lookup
of `cik` and `year`, and sends relevant filing text sections to AWS Bedrock.
"""

from __future__ import annotations

import argparse
import json
import logging
import os
import sys
import warnings
from pathlib import Path
from typing import Any, Optional, Sequence

import pandas as pd

import ai_adoption_utils as u
import aws_backend

MODEL_LABEL = "claude"
AUDIT_SEED = "ai-adoption-prefilter-audit-v1"


# ---------------------------------------------------------------------------
# Command-line arguments
# ---------------------------------------------------------------------------
# This function defines all options a user can pass when running the script.
# It also checks that numeric options are sensible before the process starts.
def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    raw_argv = list(argv) if argv is not None else sys.argv[1:]
    ap = argparse.ArgumentParser(
        description="Score selected EDGAR chunk files for AI adoption using AWS Bedrock bulk prompt invocation."
    )

    # Input source. Local files take precedence over any bucket configured in
    # the environment, which makes local reproducibility straightforward.
    ap.add_argument(
        "--input-dir",
        default=None,
        help="Local directory containing extract_df_chunk_XXXXX.rds files.",
    )
    ap.add_argument(
        "--s3-bucket",
        default=None,
        help="AWS S3 bucket. Defaults to AI_ADOPTION_S3_BUCKET when set.",
    )
    ap.add_argument(
        "--s3-prefix",
        default=None,
        help="Optional AWS S3 key prefix containing the chunk files.",
    )
    ap.add_argument(
        "--aws-region",
        default=None,
        help="AWS region for S3 and Bedrock; otherwise use boto3 configuration.",
    )
    ap.add_argument(
        "--team",
        default=None,
        help="Deprecated alias for an AWS S3 prefix; use --s3-prefix.",
    )
    ap.add_argument(
        "--chunk-prefix",
        default="",
        help="Deprecated subprefix appended to --s3-prefix for old commands.",
    )
    # Mutually exclusive chunk selection options. The user should choose one.
    selection = ap.add_mutually_exclusive_group(required=False)
    selection.add_argument(
        "--chunk-names",
        nargs="+",
        help="Chunk basenames, e.g. extract_df_chunk_00001.rds",
    )
    selection.add_argument(
        "--chunk-ids", nargs="+", type=int, help="Chunk ids, e.g. 1 2 18 37"
    )
    selection.add_argument(
        "--chunk-range",
        nargs=2,
        type=int,
        metavar=("START", "END"),
        help="Inclusive chunk id range.",
    )
    selection.add_argument(
        "--all-chunks",
        action="store_true",
        help="Process every chunk in the selected input. Use with care.",
    )
    selection.add_argument(
        "--repair-failed-from-csv",
        nargs="+",
        help="Existing score CSV file(s). Only rows with final parse_status=failed will be resubmitted and repaired in place.",
    )

    # QA and sample-filtering options.
    ap.add_argument(
        "--list-only", action="store_true", help="List available chunks and exit."
    )
    ap.add_argument(
        "--check-bedrock-config",
        action="store_true",
        help=(
            "Print the standard Bedrock backend and generation controls, "
            "then exit without reading chunks or sending a paid request."
        ),
    )
    ap.add_argument(
        "--max-chunks", type=int, default=0, help="Cap selected chunks after selection."
    )
    ap.add_argument(
        "--max-filings-per-chunk",
        type=int,
        default=0,
        help="QA option: label only first N matched filings per chunk.",
    )
    ap.add_argument(
        "--lookup-csv",
        default=None,
        help="CSV with cik and year columns. Only matching filings are labeled.",
    )
    ap.add_argument(
        "--max-analysis-year",
        type=int,
        default=u.DEFAULT_MAX_ANALYSIS_YEAR,
        help="Exclude later filing years before scoring; 0 disables the limit.",
    )
    # Prefilter options decide when no-keyword filings should skip the LLM.
    ap.add_argument(
        "--prefilter-mode",
        choices=["off", "hard_zero", "audit"],
        default=u.DEFAULT_PREFILTER_MODE,
        help="hard_zero skips no-keyword filings; audit samples no-keyword filings; off calls LLM on all non-empty filings.",
    )
    ap.add_argument("--prefilter-audit-rate", type=float, default=0.02)
    ap.add_argument(
        "--prefilter-audit-limit",
        type=int,
        default=0,
        help="Per-chunk audit call cap; 0 means no cap.",
    )
    # LLM and bulk invocation settings.
    ap.add_argument(
        "--model-id",
        default=u.MODEL_NAME,
        help="Bedrock model_id, e.g. eu.anthropic.claude-haiku-4-5-20251001-v1:0 or eu.anthropic.claude-sonnet-4-6.",
    )
    ap.add_argument(
        "--max-prompt-chars",
        type=int,
        default=u.DEFAULT_MAX_PROMPT_CHARS,
        help=(
            "Maximum filing-evidence characters sent per request; "
            "0 sends all extracted keyword windows for the filing."
        ),
    )
    ap.add_argument("--sentence-window", type=int, default=u.DEFAULT_SENTENCE_WINDOW)
    ap.add_argument(
        "--max-workers",
        type=int,
        default=5,
        help="Parallel Bedrock requests. Keep low to avoid 429 errors.",
    )
    # Output and logging settings.
    ap.add_argument(
        "--save-raw-json",
        action="store_true",
        help="Store parsed JSON and raw model output in the output CSV.",
    )
    ap.add_argument(
        "--skip-existing",
        action="store_true",
        help="Skip chunks whose score, summary, and snippet-audit outputs already exist.",
    )
    ap.add_argument(
        "--flat-output",
        action="store_true",
        help="Write directly to --out-dir instead of --out-dir/RUN_ID.",
    )
    ap.add_argument(
        "--out-dir", default=os.path.join(os.getcwd(), "output", "llm_scores")
    )
    ap.add_argument(
        "--log-level", default="INFO", choices=["DEBUG", "INFO", "WARNING", "ERROR"]
    )

    args = ap.parse_args(raw_argv)

    # Fail early for invalid arguments so the run does not start half-configured.
    if args.max_chunks < 0:
        ap.error("--max-chunks must be >= 0")
    if args.max_filings_per_chunk < 0:
        ap.error("--max-filings-per-chunk must be >= 0")
    if args.max_analysis_year < 0:
        ap.error("--max-analysis-year must be >= 0")
    if args.max_prompt_chars < 0:
        ap.error("--max-prompt-chars must be >= 0")
    if args.sentence_window < 0:
        ap.error("--sentence-window must be >= 0")
    if not 0.0 <= args.prefilter_audit_rate <= 1.0:
        ap.error("--prefilter-audit-rate must be between 0 and 1")
    if args.prefilter_audit_limit < 0:
        ap.error("--prefilter-audit-limit must be >= 0")
    if args.max_workers <= 0:
        ap.error("--max-workers must be > 0")
    if args.lookup_csv and not os.path.exists(args.lookup_csv):
        ap.error(f"--lookup-csv does not exist: {args.lookup_csv}")

    explicit_s3_options = {"--s3-bucket", "--s3-prefix", "--team", "--chunk-prefix"}
    if args.input_dir and any(option in raw_argv for option in explicit_s3_options):
        ap.error("--input-dir cannot be combined with S3 input options")
    if args.team:
        if args.s3_prefix is not None:
            ap.error("--team cannot be combined with --s3-prefix")
        warnings.warn(
            "--team is deprecated and is treated as --s3-prefix.",
            FutureWarning,
            stacklevel=2,
        )
        args.s3_prefix = args.team
    elif args.s3_prefix is None:
        args.s3_prefix = os.getenv("AI_ADOPTION_S3_PREFIX", "")
    if args.chunk_prefix:
        args.s3_prefix = "/".join(
            part.strip("/")
            for part in [args.s3_prefix, args.chunk_prefix]
            if part and part.strip("/")
        )
    if args.s3_bucket is None:
        args.s3_bucket = os.getenv("AI_ADOPTION_S3_BUCKET")
    if args.aws_region is None:
        args.aws_region = os.getenv("AWS_REGION") or os.getenv("AWS_DEFAULT_REGION")

    return args


# ---------------------------------------------------------------------------
# Output folder helper
# ---------------------------------------------------------------------------
# By default, each run gets its own timestamped output folder. This avoids
# overwriting previous QA runs. --flat-output disables that behaviour.
def output_dir_for_run(args: argparse.Namespace, run_id: str) -> str:
    return args.out_dir if args.flat_output else os.path.join(args.out_dir, run_id)


def input_backend(args: argparse.Namespace) -> str:
    """Validate and return the configured input backend name."""

    if args.input_dir:
        path = Path(args.input_dir).expanduser()
        if not path.is_dir():
            raise ValueError(f"Local input directory does not exist: {args.input_dir}")
        return "local"
    if not args.s3_bucket:
        raise ValueError(
            "No S3 bucket configured. Supply --s3-bucket or set "
            "AI_ADOPTION_S3_BUCKET."
        )
    return "s3"


def list_available_chunks(args: argparse.Namespace) -> dict[str, u.ChunkRef]:
    """List chunks from the selected local or AWS S3 input backend."""

    if input_backend(args) == "local":
        return u.list_local_chunks(args.input_dir)
    return u.list_s3_chunks(
        args.s3_bucket,
        args.s3_prefix,
        region_name=args.aws_region,
    )


def read_chunk(ref: u.ChunkRef, args: argparse.Namespace) -> pd.DataFrame:
    """Read one chunk from the selected local or AWS S3 input backend."""

    if input_backend(args) == "local":
        return u.read_rds_from_local(ref.path)
    return u.read_rds_from_s3(
        ref.path,
        bucket=args.s3_bucket,
        region_name=args.aws_region,
    )


def chunk_name_from_source_label(source_label: str) -> str:
    """Extract a non-sensitive chunk basename from old or current provenance."""

    text = str(source_label).strip()
    if ":" in text:
        text = text.split(":", 1)[1]
    return Path(text).name


def summary_path_for_output_csv(output_csv: str, model_label: str) -> str:
    """Convert one chunk score CSV path to its summary JSON path."""

    csv_path = Path(output_csv)
    suffix = f"_{model_label}_scores.csv"
    if csv_path.name.endswith(suffix):
        return str(
            csv_path.with_name(
                csv_path.name[: -len(suffix)] + f"_{model_label}_summary.json"
            )
        )
    return str(csv_path.with_suffix(".json"))


def snippet_audit_path_for_output_csv(output_csv: str, model_label: str) -> str:
    """Convert one chunk score CSV path to its companion snippet-audit CSV."""

    csv_path = Path(output_csv)
    suffix = f"_{model_label}_scores.csv"
    if csv_path.name.endswith(suffix):
        return str(
            csv_path.with_name(
                csv_path.name[: -len(suffix)] + f"_{model_label}_snippet_audit.csv"
            )
        )
    return str(csv_path.with_name(csv_path.stem + "_snippet_audit.csv"))


def boolean_series(values: pd.Series) -> pd.Series:
    """Normalize booleans read from CSV without treating the string 'False' as true."""

    return values.map(u.coerce_bool).astype(bool)


def rebuild_snippet_audit(
    records: list[dict[str, object]],
    wide_by_accession: dict[str, pd.Series],
) -> pd.DataFrame:
    """Rebuild the complete snippet audit from score rows and their source filing text."""

    pending: dict[str, dict[str, object]] = {}
    for record_index, record in enumerate(records):
        if not boolean_series(pd.Series([record.get("llm_called", False)])).iloc[0]:
            continue

        accession = str(record.get("accession_number", "")).strip()
        wide_row = wide_by_accession.get(accession)
        if wide_row is None:
            logging.warning(
                "Could not rebuild audit snippet for missing accession %s", accession
            )
            continue

        max_prompt_chars_value = pd.to_numeric(
            record.get("max_prompt_chars"), errors="coerce"
        )
        sentence_window_value = pd.to_numeric(
            record.get("sentence_window"), errors="coerce"
        )
        max_prompt_chars = (
            u.DEFAULT_MAX_PROMPT_CHARS
            if pd.isna(max_prompt_chars_value)
            else int(max_prompt_chars_value)
        )
        sentence_window = (
            u.DEFAULT_SENTENCE_WINDOW
            if pd.isna(sentence_window_value)
            else int(sentence_window_value)
        )
        snippet = u.extract_relevant_snippets(
            u.normalize_structured_text(wide_row.get("combined_text", "")),
            max_prompt_chars,
            sentence_window,
        )
        pending[str(record_index)] = {
            "record_index": record_index,
            "snippet_text": snippet,
        }

    return u.build_snippet_audit_table(records, pending)


MANIFEST_METRIC_FIELDS = [
    "n_filings",
    "n_llm_called",
    "n_scored",
    "n_unscored",
    "n_score_1",
    "n_score_2",
    "n_score_3",
    "n_ok",
    "n_ok_after_retry",
    "n_retry_attempted",
]


def manifest_metrics(summary: dict[str, object]) -> dict[str, object]:
    """Select the shared chunk metrics written to run and repair manifests."""

    return {
        field: summary[field] for field in MANIFEST_METRIC_FIELDS if field in summary
    }


FAILURE_MANIFEST_STATUSES = {
    "failed",
    "partial_failure",
    "repair_failed",
    "repair_partial_failure",
    "repair_no_rerunnable_rows",
}


def manifest_exit_code(rows: Sequence[dict[str, object]]) -> int:
    """Return nonzero when a run or repair manifest contains unresolved failure."""

    return int(
        any(str(row.get("status", "")) in FAILURE_MANIFEST_STATUSES for row in rows)
    )


def chunk_status_from_summary(summary: dict[str, object]) -> str:
    """Mark a written chunk incomplete when any filing remains unscored."""

    return "ok" if int(summary.get("n_unscored", 0)) == 0 else "partial_failure"


def repair_status_row(
    csv_path: str,
    status: str,
    *,
    source_label: str = "",
) -> dict[str, object]:
    """Build a manifest row for a repair that did not submit model calls."""

    return {
        "chunk_name": Path(csv_path).name,
        "source_label": source_label,
        "status": status,
        "output_csv": csv_path,
        "output_summary": summary_path_for_output_csv(csv_path, MODEL_LABEL),
        "n_repaired": 0,
    }


def failed_record_indices(score_df: pd.DataFrame) -> list[int]:
    """Return score-row positions that were called but still failed parsing."""

    llm_called = boolean_series(
        score_df.get("llm_called", pd.Series(False, index=score_df.index))
    )
    parse_failed = (
        score_df.get("parse_status", pd.Series("", index=score_df.index))
        .astype(str)
        .str.strip()
        .eq("failed")
    )
    return [int(index) for index in score_df.index[llm_called & parse_failed]]


def load_repair_source(
    score_df: pd.DataFrame,
    args: argparse.Namespace,
) -> tuple[str, str, dict[str, pd.Series]]:
    """Reload and reshape the source chunk referenced by an existing score CSV."""

    stored_label = str(score_df.get("source_label", pd.Series([""])).iloc[0])
    chunk_name = chunk_name_from_source_label(stored_label)
    if not u.CHUNK_RE.match(chunk_name):
        chunk_id = str(score_df.get("chunk_id", pd.Series([""])).iloc[0]).strip()
        chunk_name = f"{chunk_id}.rds" if chunk_id else ""
    available = list_available_chunks(args)
    if chunk_name not in available:
        raise FileNotFoundError(
            f"Source chunk {chunk_name!r} was not found in the selected input."
        )
    ref = available[chunk_name]
    raw = read_chunk(ref, args)
    wide = u.long_to_wide(raw)
    return (
        ref.name,
        ref.path,
        {str(row["accession_number"]).strip(): row for _, row in wide.iterrows()},
    )


def prepare_repair_prompts(
    records: list[dict[str, Any]],
    failed_indices: list[int],
    wide_by_accession: dict[str, pd.Series],
    *,
    args: argparse.Namespace,
    run_id: str,
    chunk_path: str,
) -> dict[str, dict[str, Any]]:
    """Rebuild snippets and retry prompts for failed score rows."""

    pending: dict[str, dict[str, Any]] = {}
    for index in failed_indices:
        record = records[index]
        accession = str(record.get("accession_number", "")).strip()
        source_row = wide_by_accession.get(accession)
        if source_row is None:
            logging.warning("Accession %s is missing from %s", accession, chunk_path)
            continue

        snippet = u.extract_relevant_snippets(
            u.normalize_structured_text(source_row.get("combined_text", "")),
            args.max_prompt_chars,
            args.sentence_window,
        )
        if not snippet:
            logging.warning("Could not rebuild snippet for accession %s", accession)
            continue

        record.update(
            run_id=run_id,
            script_version=u.SCRIPT_VERSION,
            prompt_version=u.PROMPT_VERSION,
            research_profile=u.RESEARCH_PROFILE,
            llm_model=args.model_id,
            llm_checkpoint=args.model_id,
            temperature=u.TEMPERATURE,
            max_new_tokens=u.DEFAULT_MAX_NEW_TOKENS,
            max_prompt_chars=args.max_prompt_chars,
            sentence_window=args.sentence_window,
            endpoint=args.model_id,
            snippet_chars=len(snippet),
            snippet_sha256=u.sha256_text(snippet),
        )
        pending[str(index)] = {
            "record_index": index,
            "prompt": u.build_ai_retry_prompt(snippet),
            "snippet_text": snippet,
        }
    return pending


def read_summary_json(path: str) -> dict[str, Any]:
    """Read existing chunk metadata when available."""

    if not os.path.exists(path):
        return {}
    try:
        return json.loads(Path(path).read_text())
    except Exception:
        logging.warning("Could not read existing summary JSON: %s", path)
        return {}


def require_compatible_existing_summary(
    summary: dict[str, Any],
    *,
    summary_path: str,
    chunk_name: str,
    source_label: str,
    args: argparse.Namespace,
) -> None:
    """Allow resume only when existing outputs match the current run exactly."""

    expected = {
        "chunk_name": chunk_name,
        "source_label": source_label,
        "script_version": u.SCRIPT_VERSION,
        "prompt_version": u.PROMPT_VERSION,
        "research_profile": u.RESEARCH_PROFILE,
        "endpoint": args.model_id,
        "prefilter_mode": args.prefilter_mode,
        "prefilter_audit_rate": float(args.prefilter_audit_rate),
        "prefilter_audit_limit": int(args.prefilter_audit_limit),
        "max_prompt_chars": int(args.max_prompt_chars),
        "sentence_window": int(args.sentence_window),
        "lookup_csv": args.lookup_csv,
        "max_filings_per_chunk": int(args.max_filings_per_chunk),
        "max_analysis_year": int(args.max_analysis_year),
    }
    mismatches = {}
    for field, expected_value in expected.items():
        found = summary.get(field)
        if field == "source_label":
            compatible = chunk_name_from_source_label(str(found or "")) == str(
                expected_value
            )
        else:
            compatible = found == expected_value
        if not compatible:
            mismatches[field] = {"expected": expected_value, "found": found}
    if mismatches:
        raise ValueError(
            "Existing outputs are incompatible with the current scoring run: "
            f"{summary_path} has mismatches {mismatches}. Use a new output "
            "directory or remove the incompatible chunk outputs before resuming."
        )


def require_current_score_profile(score_df: pd.DataFrame, source: str) -> None:
    """Prevent failed-row repair from mixing frozen scoring profiles."""

    expected = {
        "script_version": u.SCRIPT_VERSION,
        "prompt_version": u.PROMPT_VERSION,
        "research_profile": u.RESEARCH_PROFILE,
    }
    for field, expected_value in expected.items():
        if field not in score_df.columns:
            raise ValueError(f"{source} is missing required profile field {field!r}")
        values = {
            str(value).strip()
            for value in score_df[field]
            if value is not None and not pd.isna(value) and str(value).strip()
        }
        if values != {expected_value}:
            raise ValueError(
                f"Cannot repair {source} with the current prompt: {field} has "
                f"{sorted(values)}, expected {[expected_value]}. Rescore the chunk "
                "with the current frozen profile instead."
            )


def rebuild_repair_summary(
    repaired_df: pd.DataFrame,
    *,
    csv_path: str,
    chunk_path: str,
    source_label: str,
    run_id: str,
    args: argparse.Namespace,
) -> tuple[str, dict[str, Any]]:
    """Rebuild the chunk summary while preserving original run settings."""

    summary_path = summary_path_for_output_csv(csv_path, MODEL_LABEL)
    previous = read_summary_json(summary_path)
    summary = u.summarize_output(
        repaired_df,
        run_id=run_id,
        chunk_name=str(previous.get("chunk_name", Path(chunk_path).name)),
        source_label=str(previous.get("source_label", source_label)),
        endpoint=str(previous.get("endpoint", args.model_id)),
        prefilter_mode=str(
            previous.get(
                "prefilter_mode",
                repaired_df.get(
                    "prefilter_mode", pd.Series([args.prefilter_mode])
                ).iloc[0],
            )
        ),
        prefilter_audit_rate=float(
            previous.get("prefilter_audit_rate", args.prefilter_audit_rate)
        ),
        prefilter_audit_limit=int(
            previous.get("prefilter_audit_limit", args.prefilter_audit_limit)
        ),
        max_prompt_chars=int(previous.get("max_prompt_chars", args.max_prompt_chars)),
        sentence_window=int(previous.get("sentence_window", args.sentence_window)),
        lookup_csv=previous.get("lookup_csv", args.lookup_csv),
        max_filings_per_chunk=int(
            previous.get("max_filings_per_chunk", args.max_filings_per_chunk)
        ),
        max_analysis_year=int(
            previous.get("max_analysis_year", args.max_analysis_year)
        ),
        n_filings_before_lookup=int(
            previous.get("n_filings_before_lookup", len(repaired_df))
        ),
        n_filings_after_lookup=int(
            previous.get("n_filings_after_lookup", len(repaired_df))
        ),
        n_filings_before_year_filter=int(
            previous.get("n_filings_before_year_filter", len(repaired_df))
        ),
        n_filings_after_year_filter=int(
            previous.get("n_filings_after_year_filter", len(repaired_df))
        ),
        output_csv=csv_path,
    )
    u.write_json(summary_path, summary)
    return summary_path, summary


def repair_failed_rows_from_csv(
    csv_path: str,
    *,
    args: argparse.Namespace,
    run_id: str,
) -> dict[str, object]:
    """Retry only rows that still have a failed parser status."""

    csv_path = os.path.abspath(csv_path)
    if not os.path.exists(csv_path):
        raise FileNotFoundError(f"Repair CSV not found: {csv_path}")

    score_df = pd.read_csv(csv_path)
    if score_df.empty:
        return repair_status_row(csv_path, "repair_skipped_empty_csv")
    require_current_score_profile(score_df, csv_path)

    source_label = str(score_df.get("source_label", pd.Series([""])).iloc[0])
    failed_indices = failed_record_indices(score_df)
    if not failed_indices:
        return repair_status_row(
            csv_path, "repair_no_failed_rows", source_label=source_label
        )

    source_label, chunk_path, wide_by_accession = load_repair_source(score_df, args)
    records = score_df.to_dict(orient="records")
    for record in records:
        record["source_label"] = source_label
    pending = prepare_repair_prompts(
        records,
        failed_indices,
        wide_by_accession,
        args=args,
        run_id=run_id,
        chunk_path=chunk_path,
    )
    if not pending:
        return repair_status_row(
            csv_path, "repair_no_rerunnable_rows", source_label=source_label
        )

    save_raw_json = args.save_raw_json or bool(
        {"raw_response", "raw_json"}.intersection(score_df.columns)
    )
    results = invoke_bedrock(
        pending,
        model_id=args.model_id,
        max_workers=args.max_workers,
        region_name=args.aws_region,
    )
    u.apply_bulk_results(
        records, pending, results, save_raw_json=save_raw_json, is_retry=True
    )

    repaired_df = u.order_columns(pd.DataFrame(records), save_raw_json)
    repaired_df.to_csv(csv_path, index=False)
    audit_path = snippet_audit_path_for_output_csv(csv_path, MODEL_LABEL)
    rebuild_snippet_audit(records, wide_by_accession).to_csv(audit_path, index=False)
    summary_path, summary = rebuild_repair_summary(
        repaired_df,
        csv_path=csv_path,
        chunk_path=chunk_path,
        source_label=source_label,
        run_id=run_id,
        args=args,
    )

    n_failed_remaining = len(failed_record_indices(repaired_df))
    return {
        "chunk_name": summary["chunk_name"],
        "source_label": source_label,
        "status": "repair_ok" if n_failed_remaining == 0 else "repair_partial_failure",
        "output_csv": csv_path,
        "output_summary": summary_path,
        "snippet_audit_csv": audit_path,
        "n_repaired": len(pending),
        "n_failed_remaining": n_failed_remaining,
        **manifest_metrics(summary),
    }


# ---------------------------------------------------------------------------
# Process one chunk
# ---------------------------------------------------------------------------
def invoke_bedrock(
    pending: dict[str, dict[str, Any]],
    *,
    model_id: str,
    max_workers: int,
    region_name: Optional[str] = None,
):
    """Submit prompts with explicitly enforced generation controls."""

    return aws_backend.invoke_bedrock_prompts(
        u.iter_bedrock_prompts(pending),
        model_id=model_id,
        region_name=region_name,
        temperature=u.REQUESTED_TEMPERATURE,
        max_new_tokens=u.DEFAULT_MAX_NEW_TOKENS,
        max_workers=max_workers,
    )


def run_bedrock_config_check(args: argparse.Namespace) -> int:
    """Print resolved generation controls without invoking the model."""

    controls = {
        "temperature": u.REQUESTED_TEMPERATURE,
        "maxTokens": u.DEFAULT_MAX_NEW_TOKENS,
    }
    print("backend: aws_bedrock_converse")
    print(f"model_id: {args.model_id}")
    print(f"region: {args.aws_region or 'boto3 default'}")
    print(f"resolved generation controls: {json.dumps(controls, sort_keys=True)}")
    print("No Bedrock request was sent.")
    return 0


def score_pending_records(
    records: list[dict[str, Any]],
    pending: dict[str, dict[str, Any]],
    *,
    args: argparse.Namespace,
    chunk_name: str,
) -> None:
    """Run the initial model call and one automatic parser-failure retry."""

    if not pending:
        logging.info("No LLM calls needed for %s", chunk_name)
        return

    logging.info("Submitting %d LLM calls for %s", len(pending), chunk_name)
    results = invoke_bedrock(
        pending,
        model_id=args.model_id,
        max_workers=args.max_workers,
        region_name=args.aws_region,
    )
    u.apply_bulk_results(records, pending, results, save_raw_json=args.save_raw_json)

    retry_pending = {
        linked_obj: {
            "record_index": item["record_index"],
            "prompt": item["retry_prompt"],
        }
        for linked_obj, item in pending.items()
        if records[int(item["record_index"])].get("score_status")
        in u.RETRYABLE_PARSE_STATUSES
    }
    if not retry_pending:
        return

    logging.info(
        "Retrying %d parser-failure rows for %s", len(retry_pending), chunk_name
    )
    retry_results = invoke_bedrock(
        retry_pending,
        model_id=args.model_id,
        max_workers=args.max_workers,
        region_name=args.aws_region,
    )
    u.apply_bulk_results(
        records,
        retry_pending,
        retry_results,
        save_raw_json=args.save_raw_json,
        is_retry=True,
    )


# This is the core workflow for one extract_df_chunk_XXXXX.rds file:
# read -> reshape -> lookup filter -> prefilter/prompt prep -> bulk LLM ->
# parse results -> write CSV and JSON summary.
def process_chunk(
    ref: u.ChunkRef,
    *,
    args: argparse.Namespace,
    run_id: str,
    lookup: Optional[pd.DataFrame],
) -> dict[str, object]:
    # Build output paths for this chunk.
    chunk_id = Path(ref.name).stem
    run_out_dir = output_dir_for_run(args, run_id)
    os.makedirs(run_out_dir, exist_ok=True)

    out_csv = os.path.join(run_out_dir, f"{chunk_id}_{MODEL_LABEL}_scores.csv")
    out_json = summary_path_for_output_csv(out_csv, MODEL_LABEL)
    out_snippet_audit_csv = snippet_audit_path_for_output_csv(out_csv, MODEL_LABEL)
    source_label = ref.name

    # Optional resume behaviour: skip this chunk if final outputs already exist.
    if (
        args.skip_existing
        and os.path.exists(out_csv)
        and os.path.exists(out_json)
        and os.path.exists(out_snippet_audit_csv)
    ):
        require_compatible_existing_summary(
            read_summary_json(out_json),
            summary_path=out_json,
            chunk_name=ref.name,
            source_label=source_label,
            args=args,
        )
        logging.info("Skipping existing output for %s", ref.name)
        return {
            "chunk_name": ref.name,
            "source_label": source_label,
            "status": "skipped_existing",
            "output_csv": out_csv,
            "output_summary": out_json,
            "snippet_audit_csv": out_snippet_audit_csv,
        }

    # Read the RDS chunk and combine every keyword window into one filing row.
    logging.info("Reading %s", source_label)
    raw = read_chunk(ref, args)
    wide = u.long_to_wide(raw)

    # Keep only cik/year pairs that appear in the research lookup.
    # This happens before any LLM calls, so irrelevant filings are never labeled.
    n_before_lookup = len(wide)
    wide = u.filter_to_lookup(wide, lookup)
    n_after_lookup = len(wide)
    logging.info(
        "Lookup filter kept %d/%d filing rows", n_after_lookup, n_before_lookup
    )

    # The study period ends in 2025. Apply this even when older RDS chunks still
    # contain later filings so they can never create model calls accidentally.
    n_before_year_filter = len(wide)
    wide = u.filter_to_analysis_year(wide, args.max_analysis_year)
    n_after_year_filter = len(wide)
    logging.info(
        "Analysis-year filter kept %d/%d filing rows through %s",
        n_after_year_filter,
        n_before_year_filter,
        args.max_analysis_year if args.max_analysis_year > 0 else "no limit",
    )

    # For QA runs, optionally cap the number of filings processed in the chunk.
    if args.max_filings_per_chunk > 0:
        wide = wide.head(args.max_filings_per_chunk).copy()

    # Create output records for every filing. This also decides which rows need
    # the LLM and prepares prompts only for those rows.
    records, pending = u.prepare_records_and_prompts(
        wide,
        run_id=run_id,
        chunk_id=chunk_id,
        source_label=source_label,
        llm_model=args.model_id,
        llm_checkpoint=args.model_id,
        temperature=u.TEMPERATURE,
        max_new_tokens=u.DEFAULT_MAX_NEW_TOKENS,
        endpoint=args.model_id,
        prefilter_mode=args.prefilter_mode,
        audit_seed=AUDIT_SEED,
        prefilter_audit_rate=args.prefilter_audit_rate,
        prefilter_audit_limit=args.prefilter_audit_limit,
        max_prompt_chars=args.max_prompt_chars,
        sentence_window=args.sentence_window,
    )

    score_pending_records(records, pending, args=args, chunk_name=ref.name)

    snippet_audit_df = u.build_snippet_audit_table(records, pending)
    snippet_audit_df.to_csv(out_snippet_audit_csv, index=False)
    logging.info(
        "Wrote snippet audit %s (%d rows)", out_snippet_audit_csv, len(snippet_audit_df)
    )

    # Write the filing-level output CSV.
    out_df = u.order_columns(pd.DataFrame(records), args.save_raw_json)
    out_df.to_csv(out_csv, index=False)

    # Write a compact chunk-level summary for QA.
    summary = u.summarize_output(
        out_df,
        run_id=run_id,
        chunk_name=ref.name,
        source_label=source_label,
        endpoint=args.model_id,
        prefilter_mode=args.prefilter_mode,
        prefilter_audit_rate=args.prefilter_audit_rate,
        prefilter_audit_limit=args.prefilter_audit_limit,
        max_prompt_chars=args.max_prompt_chars,
        sentence_window=args.sentence_window,
        lookup_csv=args.lookup_csv,
        max_filings_per_chunk=args.max_filings_per_chunk,
        max_analysis_year=args.max_analysis_year,
        n_filings_before_lookup=n_before_lookup,
        n_filings_after_lookup=n_after_lookup,
        n_filings_before_year_filter=n_before_year_filter,
        n_filings_after_year_filter=n_after_year_filter,
        output_csv=out_csv,
    )
    u.write_json(out_json, summary)

    # Return one manifest row describing this chunk.
    logging.info("Wrote %s (%d rows)", out_csv, len(out_df))
    chunk_status = chunk_status_from_summary(summary)
    return {
        "chunk_name": ref.name,
        "source_label": source_label,
        "status": chunk_status,
        "output_csv": out_csv,
        "output_summary": out_json,
        "snippet_audit_csv": out_snippet_audit_csv,
        "n_filings_before_lookup": n_before_lookup,
        "n_filings_after_lookup": n_after_lookup,
        "n_filings_before_year_filter": n_before_year_filter,
        "n_filings_after_year_filter": n_after_year_filter,
        **manifest_metrics(summary),
    }


# ---------------------------------------------------------------------------
# Main process
# ---------------------------------------------------------------------------
def write_manifest(
    rows: list[dict[str, object]],
    *,
    out_dir: str,
    filename: str,
) -> None:
    """Write one run or repair manifest."""

    path = os.path.join(out_dir, filename)
    pd.DataFrame(rows).to_csv(path, index=False)
    logging.info("Wrote manifest: %s", path)


def run_repairs(args: argparse.Namespace) -> int:
    """Run explicit failed-row repairs and write their manifest."""

    run_id = u.utc_now()
    run_out_dir = output_dir_for_run(args, run_id)
    os.makedirs(run_out_dir, exist_ok=True)
    rows = []
    for csv_path in args.repair_failed_from_csv:
        try:
            rows.append(repair_failed_rows_from_csv(csv_path, args=args, run_id=run_id))
        except Exception as exc:
            logging.exception("Failed repairing %s", csv_path)
            absolute_path = os.path.abspath(csv_path)
            rows.append(
                {
                    "chunk_name": Path(csv_path).name,
                    "source_label": "",
                    "status": "repair_failed",
                    "output_csv": absolute_path,
                    "output_summary": summary_path_for_output_csv(
                        absolute_path, MODEL_LABEL
                    ),
                    "error": str(exc)[:1000],
                }
            )
    write_manifest(
        rows,
        out_dir=run_out_dir,
        filename=f"repair_manifest_{run_id}.csv",
    )
    return manifest_exit_code(rows)


def run_chunks(args: argparse.Namespace) -> int:
    """Select, score, and record all requested chunks."""

    available = list_available_chunks(args)
    if args.list_only:
        if not available:
            print("No chunk files found in the selected input.")
            return 0
        for name in available:
            print(name)
        return 0

    max_chunks = args.max_chunks if args.max_chunks > 0 else None
    selected = u.select_chunks(
        available,
        chunk_names=args.chunk_names,
        chunk_ids=args.chunk_ids,
        chunk_range=args.chunk_range,
        all_chunks=args.all_chunks,
        max_chunks=max_chunks,
    )

    run_id = u.utc_now()
    run_out_dir = output_dir_for_run(args, run_id)
    os.makedirs(run_out_dir, exist_ok=True)

    lookup = u.load_cik_year_lookup(args.lookup_csv)
    rows = []
    for ref in selected:
        try:
            rows.append(process_chunk(ref, args=args, run_id=run_id, lookup=lookup))
        except Exception as exc:
            logging.exception("Failed processing %s", ref.name)
            rows.append(
                {
                    "chunk_name": ref.name,
                    "source_label": ref.name,
                    "status": "failed",
                    "output_csv": "",
                    "output_summary": "",
                    "error": str(exc)[:1000],
                }
            )
    write_manifest(
        rows,
        out_dir=run_out_dir,
        filename=f"run_manifest_{run_id}.csv",
    )
    return manifest_exit_code(rows)


def main(argv: Optional[Sequence[str]] = None) -> int:
    """Parse configuration and dispatch either scoring or explicit repair."""

    args = parse_args(argv)
    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s | %(levelname)s | %(message)s",
    )
    if args.check_bedrock_config:
        return run_bedrock_config_check(args)
    return run_repairs(args) if args.repair_failed_from_csv else run_chunks(args)


# Standard Python entry point. This lets the file be run as a script.
if __name__ == "__main__":
    sys.exit(main())
