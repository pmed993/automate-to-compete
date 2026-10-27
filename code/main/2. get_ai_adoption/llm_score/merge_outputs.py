#!/usr/bin/env python3
"""Merge chunk-level LLM extraction outputs into filing-level and firm-year datasets.

This script is designed for the bulk LLM extraction scoring workflow.
It:
1. collects all chunk CSVs under a model output folder,
2. builds a filing-level master dataset,
3. optionally re-filters the merged outputs to a research cik-year lookup,
4. concatenates the per-chunk snippet-audit tables, and
5. builds a firm-year panel for downstream joins.

Typical usage from the repository root:
    python3 "code/main/2. get_ai_adoption/llm_score/merge_outputs.py" \
      --primary-dir output/final_llm_extraction_completed \
      --primary-label claude \
      --lookup-csv "code/main/2. get_ai_adoption/lookup/cik_year.csv" \
      --out-dir data/llm_score
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
from typing import Iterable

import pandas as pd

import ai_adoption_utils as u


IDENTITY_COLS = ["accession_number", "cik", "year"]
FILING_BASE_COLS = [
    "source_label",
    "chunk_id",
    "filing_accession",
    "form_type",
    "combined_chars",
    "keyword_hits",
    "prefilter_mode",
    "prefilter_decision",
    "prefilter_audit_sample",
    "snippet_chars",
    "snippet_sha256",
]

SNIPPET_AUDIT_REQUIRED_COLUMNS = {
    "cik",
    "year",
    "filing_accession",
    "llm_processed",
    "snippet_text_length",
    "snippet_text",
}

MODEL_METADATA_FIELDS = [
    "run_id",
    "script_version",
    "prompt_version",
    "research_profile",
    "llm_model",
    "llm_checkpoint",
    "temperature",
    "max_new_tokens",
    "max_prompt_chars",
    "sentence_window",
    "llm_called",
    "endpoint_attempts",
    "parse_status",
    "initial_score_status",
    "retry_attempted",
    "retry_score_status",
    "ai_score",
    "ai_score_label",
    "score_explanation",
    "score_status",
    "endpoint",
    "job_id",
    "raw_json_sha256",
    "raw_response",
    "raw_json",
    "source_csv",
]


def parse_args() -> argparse.Namespace:
    ap = argparse.ArgumentParser(
        description="Merge AI-adoption chunk outputs into final datasets."
    )
    ap.add_argument(
        "--primary-dir",
        default=None,
        help="Root folder containing the main model's chunk outputs.",
    )
    ap.add_argument(
        "--primary-label",
        default="claude",
        help="Short label used in score filenames and merged columns, e.g. claude.",
    )
    ap.add_argument("--out-dir", required=True, help="Folder for merged outputs.")
    ap.add_argument(
        "--lookup-csv",
        default=None,
        help="Optional cik-year lookup CSV. If provided, merged outputs are filtered to lookup keys after concatenation.",
    )
    args = ap.parse_args()
    if not args.primary_dir:
        ap.error("--primary-dir is required")
    return args


def find_score_files(root: str, pattern: str) -> list[Path]:
    files = sorted(Path(root).rglob(pattern))
    if not files:
        raise FileNotFoundError(f"No files found under {root!r} matching {pattern!r}")
    return files


def pattern_for_label(label: str) -> str:
    return f"*_{label}_scores.csv"


def snippet_audit_pattern_for_label(label: str) -> str:
    return f"*_{label}_snippet_audit.csv"


def read_concat(files: Iterable[Path]) -> pd.DataFrame:
    frames = []
    for path in files:
        df = pd.read_csv(path)
        if df.empty:
            continue
        df["source_csv"] = str(path)
        frames.append(df)
    if not frames:
        return pd.DataFrame()
    return pd.concat(frames, ignore_index=True)


def ensure_identity_types(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "accession_number" in out.columns:
        out["accession_number"] = out["accession_number"].astype("string")
    if "cik" in out.columns:
        out["cik"] = pd.to_numeric(out["cik"], errors="coerce").astype("Int64")
    if "year" in out.columns:
        out["year"] = pd.to_numeric(out["year"], errors="coerce").astype("Int64")
    return out


def load_lookup(path: str | None) -> pd.DataFrame | None:
    if not path:
        return None

    lookup = pd.read_csv(path)
    lookup.columns = [str(col).strip().lower() for col in lookup.columns]
    missing = {"cik", "year"} - set(lookup.columns)
    if missing:
        raise ValueError(f"Lookup CSV is missing required columns: {sorted(missing)}")

    lookup = lookup[["cik", "year"]].copy()
    lookup["cik_match"] = lookup["cik"].apply(u.normalize_cik)
    lookup["year_match"] = pd.to_numeric(lookup["year"], errors="coerce").astype(
        "Int64"
    )
    lookup = lookup[(lookup["cik_match"] != "") & lookup["year_match"].notna()]
    lookup = lookup.drop_duplicates(["cik_match", "year_match"]).reset_index(drop=True)

    if lookup.empty:
        raise ValueError(f"Lookup CSV contains no usable cik/year pairs: {path}")

    return lookup[["cik_match", "year_match"]]


def filter_to_lookup(
    df: pd.DataFrame, lookup: pd.DataFrame | None, label: str
) -> pd.DataFrame:
    if lookup is None or df.empty:
        return df

    out = ensure_identity_types(df)
    before = len(out)

    out["cik_match"] = out["cik"].apply(u.normalize_cik)
    out["year_match"] = pd.to_numeric(out["year"], errors="coerce").astype("Int64")
    out = out.merge(lookup, on=["cik_match", "year_match"], how="inner")
    out = out.drop(columns=["cik_match", "year_match"])
    out = out.reset_index(drop=True)

    print(f"Lookup filter kept {len(out)}/{before} {label} rows")
    return out


def require_uniform_model_profile(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    """Reject merges that mix outputs from different scoring definitions."""

    for field in ["script_version", "prompt_version", "research_profile"]:
        column = f"{prefix}_{field}"
        if column not in df.columns:
            continue
        values = sorted(
            {
                str(value).strip()
                for value in df[column]
                if value is not None and not pd.isna(value) and str(value).strip()
            }
        )
        if len(values) > 1:
            raise ValueError(
                f"Cannot merge mixed {field} values for {prefix}: {values}. "
                "Merge each frozen research profile separately."
            )
    return df


def rename_model_columns(df: pd.DataFrame, prefix: str) -> pd.DataFrame:
    out = ensure_identity_types(df)
    rename_map = {field: f"{prefix}_{field}" for field in MODEL_METADATA_FIELDS}
    out = out.rename(columns=rename_map)
    preferred = (
        IDENTITY_COLS
        + FILING_BASE_COLS
        + [f"{prefix}_{field}" for field in MODEL_METADATA_FIELDS]
    )
    existing = [col for col in preferred if col in out.columns]
    extra = [col for col in out.columns if col not in existing]
    return out[existing + extra]


def load_model_outputs(root: str, pattern: str, model: str) -> pd.DataFrame:
    files = find_score_files(root, pattern)
    print(f"Found {len(files)} {model} output files under {root}")
    df = read_concat(files)
    return rename_model_columns(df, model)


def load_snippet_audit_outputs(root: str, model_label: str) -> pd.DataFrame:
    """Load and concatenate all per-chunk snippet-audit CSVs for one model."""

    pattern = snippet_audit_pattern_for_label(model_label)
    files = sorted(Path(root).rglob(pattern))
    print(f"Found {len(files)} {model_label} snippet audit files under {root}")
    if not files:
        return pd.DataFrame()

    out = read_concat(files)
    if out.empty:
        return pd.DataFrame(
            columns=sorted(
                SNIPPET_AUDIT_REQUIRED_COLUMNS | {"model_label", "source_csv"}
            )
        )
    missing = SNIPPET_AUDIT_REQUIRED_COLUMNS - set(out.columns)
    if missing:
        raise ValueError(
            f"{model_label} snippet audit files are missing required columns: {sorted(missing)}"
        )
    out = ensure_identity_types(out)
    out["filing_accession"] = out["filing_accession"].astype("string")
    out["model_label"] = model_label
    return out


def build_aggregate_snippet_audit(audit_df: pd.DataFrame) -> pd.DataFrame:
    """Sort the concatenated filing-level snippet audit for inspection."""

    if audit_df.empty:
        return pd.DataFrame()
    sort_cols = [
        col
        for col in [
            "year",
            "cik",
            "filing_accession",
            "model_label",
            "chunk_id",
            "source_csv",
        ]
        if col in audit_df.columns
    ]
    return (
        audit_df.sort_values(sort_cols).reset_index(drop=True)
        if sort_cols
        else audit_df.reset_index(drop=True)
    )


def write_duplicate_report(dupes: pd.DataFrame, out_dir: str, filename: str) -> str:
    path = os.path.join(out_dir, filename)
    dupes.to_csv(path, index=False)
    return path


def require_unique_accessions(df: pd.DataFrame, out_dir: str) -> pd.DataFrame:
    """Reject duplicate filing rows; extraction keeps each accession intact."""

    out = ensure_identity_types(df)
    dupes = out[out.duplicated(subset=["accession_number"], keep=False)].sort_values(
        ["accession_number", "cik", "year"]
    )
    if dupes.empty:
        return out
    dupes_path = write_duplicate_report(
        dupes, out_dir, "filing_accession_duplicates.csv"
    )
    n_dupe_accessions = dupes["accession_number"].nunique()
    raise ValueError(
        f"Found {n_dupe_accessions} duplicated filing accessions. Review "
        f"{dupes_path} and remove overlapping score outputs before merging."
    )


def first_non_null(series: pd.Series) -> object:
    for value in series:
        if value is None or pd.isna(value):
            continue
        if isinstance(value, str) and not value.strip():
            continue
        return value
    return pd.NA


def aggregate_parse_status(series: pd.Series) -> str:
    statuses = {
        str(value).strip()
        for value in series
        if value is not None and not pd.isna(value) and str(value).strip()
    }
    if "retry_success" in statuses:
        return "retry_success"
    if "success" in statuses or "not_called" in statuses:
        return "success"
    return "failed"


def aggregate_model_firm_year(
    filing_df: pd.DataFrame,
    prefix: str,
) -> pd.DataFrame:
    """Aggregate filing-level 1-3 AI scores into one firm-year score."""

    score_col = f"{prefix}_ai_score"
    explanation_col = f"{prefix}_score_explanation"
    parse_status_col = f"{prefix}_parse_status"
    if score_col not in filing_df.columns and parse_status_col not in filing_df.columns:
        return pd.DataFrame(columns=["cik", "year"])

    out = ensure_identity_types(filing_df)
    rows: list[dict[str, object]] = []

    for (cik, year), group in out.groupby(["cik", "year"], dropna=False, sort=True):
        if score_col in group.columns:
            score_series = pd.to_numeric(group[score_col], errors="coerce")
        else:
            score_series = pd.Series(pd.NA, index=group.index, dtype="Float64")
        accessions = sorted(
            {
                str(value).strip()
                for value in group.get("accession_number", pd.Series(dtype="string"))
                if value is not None and not pd.isna(value) and str(value).strip()
            }
        )
        parse_status_series = group.get(parse_status_col, pd.Series(dtype="string"))
        n_chunks = int(len(group))
        n_chunks_scored = int(
            parse_status_series.isin(["success", "retry_success"]).sum()
        )
        n_chunks_failed = int((parse_status_series == "failed").sum())
        valid_scores = score_series.dropna()
        ai_score = int(valid_scores.max()) if not valid_scores.empty else pd.NA
        ai_score_label = (
            u.AI_SCORE_LABELS.get(int(ai_score), pd.NA) if pd.notna(ai_score) else pd.NA
        )
        max_score_explanation = ""
        if pd.notna(ai_score) and explanation_col in group.columns:
            explanation_series = group.loc[score_series == ai_score, explanation_col]
            max_score_explanation = str(
                first_non_null(explanation_series) or ""
            ).strip()
        rows.append(
            {
                "cik": cik,
                "year": year,
                "filing_accession": ";".join(accessions),
                "run_id": first_non_null(
                    group.get(f"{prefix}_run_id", pd.Series(dtype="object"))
                ),
                "script_version": first_non_null(
                    group.get(f"{prefix}_script_version", pd.Series(dtype="object"))
                ),
                "prompt_version": first_non_null(
                    group.get(f"{prefix}_prompt_version", pd.Series(dtype="object"))
                ),
                "research_profile": first_non_null(
                    group.get(f"{prefix}_research_profile", pd.Series(dtype="object"))
                ),
                "llm_model": first_non_null(
                    group.get(f"{prefix}_llm_model", pd.Series(dtype="object"))
                ),
                "llm_checkpoint": first_non_null(
                    group.get(f"{prefix}_llm_checkpoint", pd.Series(dtype="object"))
                ),
                "temperature": first_non_null(
                    group.get(f"{prefix}_temperature", pd.Series(dtype="object"))
                ),
                "max_new_tokens": first_non_null(
                    group.get(f"{prefix}_max_new_tokens", pd.Series(dtype="object"))
                ),
                "max_prompt_chars": first_non_null(
                    group.get(f"{prefix}_max_prompt_chars", pd.Series(dtype="object"))
                ),
                "sentence_window": first_non_null(
                    group.get(f"{prefix}_sentence_window", pd.Series(dtype="object"))
                ),
                "n_chunks": n_chunks,
                "n_chunks_scored": n_chunks_scored,
                "n_chunks_failed": n_chunks_failed,
                "parse_status": aggregate_parse_status(parse_status_series),
                "ai_score": ai_score,
                "ai_score_label": ai_score_label,
                "score_explanation": max_score_explanation,
            }
        )

    return pd.DataFrame(rows)


def build_firm_year_panel(filing_df: pd.DataFrame, primary_prefix: str) -> pd.DataFrame:
    """Aggregate filing scores using the maximum score within each firm-year."""

    score_col = f"{primary_prefix}_ai_score"
    if score_col not in filing_df.columns:
        return (
            ensure_identity_types(filing_df)[["cik", "year"]]
            .drop_duplicates()
            .reset_index(drop=True)
        )
    return ensure_identity_types(
        aggregate_model_firm_year(filing_df, primary_prefix)
    ).reset_index(drop=True)


def main() -> int:
    args = parse_args()
    os.makedirs(args.out_dir, exist_ok=True)
    lookup = load_lookup(args.lookup_csv)

    filing_df = load_model_outputs(
        args.primary_dir, pattern_for_label(args.primary_label), args.primary_label
    )
    filing_df = filter_to_lookup(filing_df, lookup, args.primary_label)
    filing_df = require_uniform_model_profile(filing_df, args.primary_label)
    filing_df = require_unique_accessions(filing_df, args.out_dir)

    snippet_audit_df = load_snippet_audit_outputs(args.primary_dir, args.primary_label)
    snippet_audit_df = filter_to_lookup(
        snippet_audit_df, lookup, f"{args.primary_label} snippet audit"
    )
    snippet_audit_df = build_aggregate_snippet_audit(snippet_audit_df)
    if not snippet_audit_df.empty:
        snippet_audit_out = os.path.join(
            args.out_dir, "llm_extraction_snippet_audit.csv"
        )
        snippet_audit_df.to_csv(snippet_audit_out, index=False)
        print(
            f"Wrote aggregate snippet audit: {snippet_audit_out} ({len(snippet_audit_df)} rows)"
        )
    else:
        print("No snippet audit files found; aggregate snippet audit was not written")

    filing_df = filing_df.sort_values(["year", "cik", "accession_number"]).reset_index(
        drop=True
    )

    filing_out = os.path.join(args.out_dir, "llm_extraction_filing_master.csv")
    filing_df.to_csv(filing_out, index=False)
    print(f"Wrote filing-level master: {filing_out} ({len(filing_df)} rows)")

    panel_df = build_firm_year_panel(filing_df, args.primary_label)
    panel_sort_cols = [
        col for col in ["year", "cik", "filing_accession"] if col in panel_df.columns
    ]
    panel_df = (
        panel_df.sort_values(panel_sort_cols).reset_index(drop=True)
        if panel_sort_cols
        else panel_df.reset_index(drop=True)
    )
    panel_out = os.path.join(args.out_dir, "llm_extraction_firm_year_panel.csv")
    panel_df.to_csv(panel_out, index=False)
    print(f"Wrote firm-year panel: {panel_out} ({len(panel_df)} rows)")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
