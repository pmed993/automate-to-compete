#!/usr/bin/env python3
"""Prepare the fiscal-2025 completion lookup and combine validated extractions.

The main extraction uses filing years 2015--2025. The completion extraction
uses report year 2025 for sample firms without that report year in the main
manifest. Its actual filing dates can be in 2026 and are never relabelled.
"""

from __future__ import annotations

import argparse
import json
import logging
from pathlib import Path
from typing import Any, Optional, Sequence

import pandas as pd

import edgar_keyword_extract as e


COMPLETION_YEAR = 2025
MAIN_DIR = Path("cache/edgar_keyword_windows")
COMPLETION_DIR = Path("cache/edgar_keyword_windows_fy2025_completion")
COMBINED_DIR = Path("cache/edgar_keyword_windows_combined")
LOOKUP_DIR = Path("code/main/2. get_ai_adoption/lookup")


def build_completion_lookup(
    lookup: pd.DataFrame, main_manifest: pd.DataFrame
) -> pd.DataFrame:
    """Select 2025 sample CIKs with no report-year-2025 main filing."""

    sample = lookup[["cik", "year"]].copy()
    sample["cik"] = sample["cik"].map(e.normalize_cik)
    sample["year"] = pd.to_numeric(sample["year"], errors="raise")
    sample = sample.loc[sample["year"].eq(COMPLETION_YEAR)]
    reported = main_manifest.loc[
        pd.to_datetime(main_manifest["report_date"], errors="coerce")
        .dt.year.eq(COMPLETION_YEAR),
        "cik",
    ].map(e.normalize_cik)
    result = sample.loc[~sample["cik"].isin(set(reported))].drop_duplicates()
    if result.empty:
        raise ValueError("No sample firms require a fiscal-2025 completion run.")
    return result.sort_values("cik").reset_index(drop=True)


def combine_manifests(
    main: pd.DataFrame, completion: pd.DataFrame
) -> tuple[pd.DataFrame, dict[str, Any]]:
    """Keep each accession once, with the main extraction taking precedence."""

    for label, frame, method in (
        ("main", main, "filing-date"),
        ("completion", completion, "report-date"),
    ):
        if frame["accession_number"].duplicated().any():
            raise ValueError(f"Duplicate accessions within the {label} manifest.")
        if not frame["year_match_method"].eq(method).all():
            raise ValueError(f"The {label} manifest must use {method} matching.")
    report_year = pd.to_datetime(completion["report_date"], errors="coerce").dt.year
    if not (
        report_year.eq(COMPLETION_YEAR).all()
        and completion["year"].eq(COMPLETION_YEAR).all()
    ):
        raise ValueError("Completion rows must have report year and scoring year 2025.")

    # Historical main manifests can still contain labelled 2026 rows. They do
    # not belong to the main research window; qualifying completion rows keep
    # year=2025 while retaining their actual filing_date/filing_year fields.
    primary = main.loc[main["year"].between(2015, COMPLETION_YEAR)].copy()
    supplement = completion.copy()
    primary["extraction_source"] = "main"
    supplement["extraction_source"] = "fy2025_completion"
    overlaps = sorted(
        set(primary["accession_number"]) & set(supplement["accession_number"])
    )
    merged = pd.concat([primary, supplement], ignore_index=True, sort=False)
    merged = merged.drop_duplicates("accession_number", keep="first")
    merged = merged.sort_values(["year", "cik", "accession_number"]).reset_index(drop=True)
    audit = {
        "main_manifest_rows": len(main),
        "main_rows_in_analysis_window": len(primary),
        "main_rows_outside_analysis_window": len(main) - len(primary),
        "completion_manifest_rows": len(completion),
        "completion_unique_additions": len(completion) - len(overlaps),
        "duplicate_accessions_retained_from_main": overlaps,
        "combined_unique_filings": len(merged),
        "completion_rule": "report year 2025 for CIKs missing that report year in main",
        "accession_precedence": "main",
    }
    return merged, audit


def merge_extractions(
    main_path: Path,
    completion_path: Path,
    output_dir: Path,
    *,
    filings_per_output: int = e.DEFAULT_FILINGS_PER_OUTPUT,
    overwrite: bool = False,
) -> dict[str, Any]:
    """Reassemble original JSON windows into globally numbered scoring chunks."""

    if output_dir.resolve() in {main_path.parent.resolve(), completion_path.parent.resolve()}:
        raise ValueError("The combined directory must differ from both source directories.")
    merged, audit = combine_manifests(
        e.load_manifest(main_path), e.load_manifest(completion_path)
    )
    result_validation = e.require_complete_extractions(merged)
    qa = e.qa_manifest(merged, result_validation=result_validation)
    if not qa["passed"]:
        raise RuntimeError("Combined extraction QA failed; source runs must be complete.")
    assembled = output_dir / "assembled"
    summary = e.assemble_compatible_chunks(
        merged, assembled, filings_per_output=filings_per_output, overwrite=overwrite
    )
    validation = e.validate_assembled_outputs(merged, assembled, summary)
    if not validation["passed"]:
        raise RuntimeError(f"Combined chunk validation failed: {validation}")
    summary["assembly_validation"] = validation
    summary["data_workspace_bundle"] = e.package_data_workspace_rds(
        assembled,
        output_dir / e.DEFAULT_DATA_WORKSPACE_DIRNAME,
        output_dir / f"{e.DEFAULT_DATA_WORKSPACE_DIRNAME}.zip",
        expected_chunks=int(summary["output_chunks"]),
    )
    audit["source_manifest_sha256"] = {
        "main": e.sha256_file(main_path),
        "fy2025_completion": e.sha256_file(completion_path),
    }
    e.atomic_write_dataframe(merged, output_dir / e.MANIFEST_NAME)
    e.atomic_write_json(assembled / "extraction_summary.json", summary)
    e.atomic_write_json(output_dir / "completion_merge_audit.json", audit)
    e.atomic_write_json(output_dir / "qa_summary.json", qa)
    return audit


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    lookup = commands.add_parser("lookup", help="Generate the fiscal-2025 completion lookup")
    lookup.add_argument("--lookup-csv", type=Path, default=LOOKUP_DIR / "cik_year.csv")
    lookup.add_argument("--main-manifest", type=Path, default=MAIN_DIR / e.MANIFEST_NAME)
    lookup.add_argument(
        "--output-csv", type=Path, default=LOOKUP_DIR / "cik_year_fy2025_completion.csv"
    )
    merge = commands.add_parser("merge", help="Combine completed main and FY2025 extractions")
    merge.add_argument("--main-manifest", type=Path, default=MAIN_DIR / e.MANIFEST_NAME)
    merge.add_argument("--completion-manifest", type=Path, default=COMPLETION_DIR / e.MANIFEST_NAME)
    merge.add_argument("--output-dir", type=Path, default=COMBINED_DIR)
    merge.add_argument("--filings-per-output", type=int, default=e.DEFAULT_FILINGS_PER_OUTPUT)
    merge.add_argument("--overwrite-chunks", action="store_true")
    args = parser.parse_args(argv)
    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    if args.command == "lookup":
        frame = build_completion_lookup(
            e.load_lookup(args.lookup_csv), e.load_manifest(args.main_manifest)
        )
        e.atomic_write_dataframe(frame, args.output_csv)
        print(f"Wrote {len(frame)} fiscal-2025 CIKs to {args.output_csv}")
    else:
        audit = merge_extractions(
            args.main_manifest, args.completion_manifest, args.output_dir,
            filings_per_output=args.filings_per_output, overwrite=args.overwrite_chunks,
        )
        print(json.dumps(audit, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
