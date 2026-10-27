#!/usr/bin/env python3
"""Retrieve high-recall AI keyword windows from SEC 10-K primary documents.

This module owns the EDGAR extraction stage only.  It:

1. builds a filing manifest from the SEC ``submissions.zip`` bulk archive;
2. keeps exact 10-K and 10-K/A filings for project CIK/filing-year keys;
3. downloads only the primary filing document in bounded raw-storage batches
   (with an explicit last-resort complete-submission fallback when
   ``primaryDocument`` is absent);
4. converts the whole filing to readable text and deletes each raw document
   after its extraction result has been atomically validated;
5. finds terms with the existing ``ai_adoption_utils.AI_KEYWORDS`` dictionary;
6. writes every merged sentence window without ranking or character limits; and
7. creates ``extract_df_chunk_XXXXX.rds`` files compatible with the existing
   AI-adoption scorer.

The pipeline is resumable.  Temporary raw downloads and per-filing extraction
results are written atomically, and the manifest records failures instead of
treating an empty placeholder as success.  Valid extraction results are reused
without re-downloading raw filings.
"""

from __future__ import annotations

import argparse
import bisect
import csv
import gzip
import hashlib
import html
import json
import logging
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import unicodedata
from collections import deque
from concurrent.futures import (
    FIRST_COMPLETED,
    ProcessPoolExecutor,
    ThreadPoolExecutor,
    as_completed,
    wait,
)
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Iterable, Iterator, Optional, Sequence
from urllib.parse import quote
from zipfile import ZIP_STORED, BadZipFile, ZipFile

import pandas as pd
import requests  # type: ignore[import-untyped]
from bs4 import BeautifulSoup, UnicodeDammit

try:
    from lxml import html as lxml_html
except ImportError:  # pragma: no cover - exercised only in minimal environments
    lxml_html = None


SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parents[2]
LLM_SCORE_DIR = SCRIPT_DIR.parent / "2. get_ai_adoption" / "llm_score"
if str(LLM_SCORE_DIR) not in sys.path:
    sys.path.insert(0, str(LLM_SCORE_DIR))

# Import, rather than copy, the project's dictionary and compiled regex rules.
import ai_adoption_utils as ai_utils  # noqa: E402


SEC_SUBMISSIONS_URL = (
    "https://www.sec.gov/Archives/edgar/daily-index/bulkdata/submissions.zip"
)
SEC_ARCHIVES_ROOT = "https://www.sec.gov/Archives/edgar/data"
SEC_SUBMISSIONS_API_ROOT = "https://data.sec.gov/submissions"
TARGET_FORMS = frozenset({"10-K", "10-K/A"})
RETRYABLE_HTTP_STATUSES = frozenset({403, 408, 429, 500, 502, 503, 504})
BLOCK_PAGE_MARKERS = (
    b"your request originates from an undeclared automated tool",
    b"request rate threshold exceeded",
    b"automated tool blocking",
)
MANIFEST_NAME = "filing_manifest.csv"
COVERAGE_NAME = "metadata_coverage.csv"
WINDOW_ITEM = "keyword_window"
EXTRACTION_VERSION = "2026-08-13-whole-filing-keyword-windows-v1"
ASSEMBLY_VERSION = "2026-10-19-keyword-window-rds-iso-dates-v3"
DEFAULT_CONTEXT_SENTENCES = 1
DEFAULT_RATE_PER_SECOND = 8.0
DEFAULT_DOWNLOAD_WORKERS = 4
DEFAULT_EXTRACT_WORKERS = 2
DEFAULT_FILINGS_PER_OUTPUT = 1000
DEFAULT_RAW_BATCH_GIB = 35.0
DEFAULT_CHECKPOINT_EVERY = 250
DEFAULT_SAMPLE_SEED = "edgar-keyword-extract-v1"
DEFAULT_DATA_WORKSPACE_DIRNAME = "data_workspace_rds"
GIB = 1024**3
DOWNLOADABLE_STATUSES = frozenset({"downloaded", "skipped_valid"})
VALID_EXTRACTION_STATUSES = frozenset({"extracted", "skipped_valid", "no_keyword"})
PURGED_DOWNLOAD_STATUS = "purged_after_extract"
PURGED_AFTER_FAILURE_STATUS = "purged_after_extract_failure"

BLOCK_TAGS = {
    "address",
    "article",
    "aside",
    "blockquote",
    "caption",
    "dd",
    "div",
    "dl",
    "dt",
    "figcaption",
    "figure",
    "footer",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "header",
    "li",
    "main",
    "nav",
    "ol",
    "p",
    "pre",
    "section",
    "table",
    "tr",
    "ul",
}
NON_READABLE_TAGS = {"script", "style", "noscript", "template", "svg"}
INLINE_SEPARATOR_TAGS = {"td", "th"}
BOUNDARY_RE = re.compile(r'(?:[.!?;]["\')\]]*[ \t]+|\n+)')
HORIZONTAL_SPACE_RE = re.compile(r"[^\S\r\n]+")
EXCESS_NEWLINES_RE = re.compile(r"\n{3,}")
SAFE_EXTENSION_RE = re.compile(r"^\.[A-Za-z0-9]{1,8}$")
SUBMISSION_DOCUMENT_RE = re.compile(
    rb"<DOCUMENT>(.*?)</DOCUMENT>", re.IGNORECASE | re.DOTALL
)
SUBMISSION_TYPE_RE = re.compile(rb"<TYPE>\s*([^\r\n<]+)", re.IGNORECASE)
SUBMISSION_TEXT_RE = re.compile(rb"<TEXT>(.*?)</TEXT>", re.IGNORECASE | re.DOTALL)


def _keyword_scan_anchor(keyword: str, pattern: str) -> str:
    """Return a mandatory literal substring used to skip impossible regexes."""

    if keyword == "a_i":
        return "a.i."
    pattern_lower = pattern.lower()
    candidates = [
        token for token in keyword.lower().split("_") if token in pattern_lower
    ]
    if not candidates:
        raise ValueError(f"AI keyword {keyword!r} has no safe literal scan anchor")
    return max(candidates, key=len)


AI_KEYWORD_SCAN_ANCHORS = {
    keyword: _keyword_scan_anchor(keyword, str(spec["pattern"]))
    for keyword, spec in ai_utils.AI_KEYWORDS.items()
}


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def sha256_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def normalize_cik(value: Any) -> str:
    return ai_utils.normalize_cik(value)


def parse_year(value: Any) -> Optional[int]:
    if value is None or pd.isna(value):
        return None
    match = re.match(r"^\s*(\d{4})", str(value))
    return int(match.group(1)) if match else None


def atomic_write_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temp_path: Optional[Path] = None
    try:
        with tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", dir=path.parent, delete=False
        ) as handle:
            temp_path = Path(handle.name)
            json.dump(payload, handle, ensure_ascii=False, indent=2)
        os.replace(temp_path, path)
    finally:
        if temp_path is not None and temp_path.exists():
            temp_path.unlink()


def atomic_write_dataframe(df: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", newline="", dir=path.parent, delete=False
    ) as handle:
        temp_path = Path(handle.name)
    try:
        df.to_csv(temp_path, index=False)
        os.replace(temp_path, path)
    finally:
        if temp_path.exists():
            temp_path.unlink()


def atomic_write_dataframe_gzip(df: pd.DataFrame, path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("wb", dir=path.parent, delete=False) as handle:
        temp_path = Path(handle.name)
    try:
        df.to_csv(temp_path, index=False, compression="gzip")
        os.replace(temp_path, path)
    finally:
        if temp_path.exists():
            temp_path.unlink()


def load_lookup(path: Path) -> pd.DataFrame:
    """Read and normalize the project's CIK-year research lookup."""

    lookup = pd.read_csv(path, dtype={"cik": "string"})
    lookup.columns = [str(column).strip().lower() for column in lookup.columns]
    missing = {"cik", "year"} - set(lookup.columns)
    if missing:
        raise ValueError(f"Lookup is missing columns: {sorted(missing)}")
    lookup = lookup[["cik", "year"]].copy()
    lookup["cik"] = lookup["cik"].map(normalize_cik)
    lookup["year"] = pd.to_numeric(lookup["year"], errors="coerce").astype("Int64")
    lookup = lookup[(lookup["cik"] != "") & lookup["year"].notna()]
    lookup = lookup.drop_duplicates(["cik", "year"]).sort_values(["cik", "year"])
    if lookup.empty:
        raise ValueError(f"Lookup contains no usable CIK-year keys: {path}")
    return lookup.reset_index(drop=True)


class RateLimiter:
    """Thread-safe limiter for aggregate request start times."""

    def __init__(self, requests_per_second: float) -> None:
        if requests_per_second <= 0:
            raise ValueError("requests_per_second must be positive")
        self.interval = 1.0 / requests_per_second
        self.lock = threading.Lock()
        self.next_allowed = 0.0

    def acquire(self) -> None:
        with self.lock:
            now = time.monotonic()
            wait = max(0.0, self.next_allowed - now)
            self.next_allowed = max(now, self.next_allowed) + self.interval
        if wait:
            time.sleep(wait)


_THREAD_LOCAL = threading.local()


def thread_session(user_agent: str) -> requests.Session:
    session = getattr(_THREAD_LOCAL, "session", None)
    if session is None:
        session = requests.Session()
        session.headers.update(
            {
                "User-Agent": user_agent,
                "Accept-Encoding": "gzip, deflate",
                "Accept": "text/html,application/xhtml+xml,text/plain,application/zip,*/*",
            }
        )
        _THREAD_LOCAL.session = session
    return session


def validate_download_file(path: Path, min_bytes: int = 200) -> tuple[bool, str]:
    if not path.exists():
        return False, "missing_file"
    size = path.stat().st_size
    if size < min_bytes:
        return False, f"too_small:{size}"
    with path.open("rb") as handle:
        prefix = handle.read(128 * 1024).lower()
    if any(marker in prefix for marker in BLOCK_PAGE_MARKERS):
        return False, "sec_block_page"
    return True, "ok"


def download_to_path(
    url: str,
    destination: Path,
    *,
    user_agent: str,
    limiter: RateLimiter,
    retries: int,
    timeout_seconds: float,
    min_bytes: int,
) -> tuple[str, str, int, str]:
    """Download one URL atomically and return status, error, size, sha256."""

    valid, reason = validate_download_file(destination, min_bytes=min_bytes)
    if valid:
        return "skipped_valid", "", destination.stat().st_size, sha256_file(destination)
    if destination.exists():
        logging.warning("Replacing invalid download %s (%s)", destination, reason)
        # A failed prior run can leave an invalid destination outside the
        # current batch's successful-byte accounting. Remove it before retrying
        # so stale raw files cannot silently consume the storage budget.
        destination.unlink()

    destination.parent.mkdir(parents=True, exist_ok=True)
    part_path = destination.with_name(
        f"{destination.name}.part.{os.getpid()}.{threading.get_ident()}"
    )
    last_error = "unknown_download_error"
    for attempt in range(retries + 1):
        try:
            limiter.acquire()
            session = thread_session(user_agent)
            with session.get(url, stream=True, timeout=timeout_seconds) as response:
                if response.status_code in RETRYABLE_HTTP_STATUSES:
                    retry_after = response.headers.get("Retry-After", "")
                    raise requests.HTTPError(
                        f"retryable HTTP {response.status_code}; Retry-After={retry_after}",
                        response=response,
                    )
                response.raise_for_status()
                with part_path.open("wb") as handle:
                    for chunk in response.iter_content(chunk_size=128 * 1024):
                        if chunk:
                            handle.write(chunk)
            valid, reason = validate_download_file(part_path, min_bytes=min_bytes)
            if not valid:
                raise ValueError(f"invalid response body: {reason}")
            digest = sha256_file(part_path)
            size = part_path.stat().st_size
            os.replace(part_path, destination)
            return "downloaded", "", size, digest
        except Exception as exc:  # network, HTTP, validation, or filesystem
            last_error = f"{type(exc).__name__}: {exc}"
            if part_path.exists():
                part_path.unlink()
            if attempt >= retries:
                break
            retry_after = 0.0
            response = getattr(exc, "response", None)
            if response is not None:
                try:
                    retry_after = float(response.headers.get("Retry-After", 0) or 0)
                except ValueError:
                    retry_after = 0.0
            time.sleep(min(30.0, max(retry_after, 1.5 * (2**attempt))))
    return "failed", last_error, 0, ""


def download_bulk_submissions(
    zip_path: Path,
    *,
    user_agent: str,
    rate_per_second: float,
    retries: int,
    timeout_seconds: float,
) -> None:
    limiter = RateLimiter(rate_per_second)
    status, error, _, _ = download_to_path(
        SEC_SUBMISSIONS_URL,
        zip_path,
        user_agent=user_agent,
        limiter=limiter,
        retries=retries,
        timeout_seconds=timeout_seconds,
        min_bytes=1024 * 1024,
    )
    if status == "failed":
        raise RuntimeError(f"Could not download SEC submissions.zip: {error}")
    try:
        with ZipFile(zip_path) as archive:
            # Opening the central directory catches malformed/truncated ZIPs.
            # Do not call testzip(): the official archive is very large and
            # that would decompress every filer's metadata before useful work.
            if not archive.infolist():
                raise BadZipFile("submissions.zip contains no members")
    except BadZipFile as exc:
        raise RuntimeError(f"Invalid SEC submissions archive: {zip_path}") from exc


def _columns_to_rows(columns: dict[str, Any]) -> Iterator[dict[str, Any]]:
    fields = (
        "accessionNumber",
        "form",
        "filingDate",
        "reportDate",
        "primaryDocument",
    )
    lengths = [len(columns.get(field, [])) for field in fields]
    count = max(lengths, default=0)
    for index in range(count):
        row: dict[str, Any] = {}
        for field in fields:
            values = columns.get(field, [])
            row[field] = values[index] if index < len(values) else ""
        yield row


def _safe_document_extension(primary_document: str) -> str:
    suffix = Path(primary_document).suffix.lower()
    if not SAFE_EXTENSION_RE.match(suffix):
        return ".html"
    if suffix in {".htm", ".xhtml"}:
        return ".html"
    return suffix


def _filing_urls(
    cik: str,
    accession: str,
    primary_document: str,
    *,
    allow_complete_submission_fallback: bool,
) -> tuple[str, str, str]:
    accession_compact = accession.replace("-", "")
    if primary_document:
        document_name = Path(primary_document).name
        url = (
            f"{SEC_ARCHIVES_ROOT}/{int(cik)}/{accession_compact}/"
            f"{quote(document_name)}"
        )
        return url, "primary_document", _safe_document_extension(document_name)
    if allow_complete_submission_fallback:
        return (
            f"{SEC_ARCHIVES_ROOT}/{int(cik)}/{accession_compact}/{accession}.txt",
            "complete_submission_fallback",
            ".txt",
        )
    return "", "missing_primary_document", ".html"


def _read_json_member(
    archive: ZipFile,
    member: str,
    names_by_basename: dict[str, str],
) -> dict[str, Any]:
    try:
        archive.getinfo(member)
        resolved: Optional[str] = member
    except KeyError:
        resolved = names_by_basename.get(Path(member).name)
    if not resolved:
        raise KeyError(member)
    with archive.open(resolved) as handle:
        return json.load(handle)


def _metadata_page_relevant(
    page: dict[str, Any], target_years: set[int], year_match: str
) -> bool:
    """Use SEC page date ranges to avoid loading irrelevant history."""

    if year_match != "filing-date":
        return True
    first_year = parse_year(page.get("filingFrom"))
    last_year = parse_year(page.get("filingTo"))
    if first_year is None or last_year is None:
        return True
    low, high = sorted((first_year, last_year))
    return any(low <= year <= high for year in target_years)


def build_manifest(
    lookup: pd.DataFrame,
    submissions_zip: Path,
    data_dir: Path,
    *,
    year_match: str = "filing-date",
    allow_complete_submission_fallback: bool = True,
    historical_page_loader: Optional[Callable[[str], dict[str, Any]]] = None,
) -> pd.DataFrame:
    """Build one manifest row per selected 10-K or 10-K/A accession."""

    years_by_cik = {
        cik: {int(year) for year in group["year"].dropna()}
        for cik, group in lookup.groupby("cik", sort=False)
    }
    selected: list[dict[str, Any]] = []
    historical_metadata_errors: list[str] = []
    with ZipFile(submissions_zip) as archive:
        archive_names = archive.namelist()
        names_by_basename = {Path(name).name: name for name in archive_names}
        for position, (cik, target_years) in enumerate(years_by_cik.items(), start=1):
            main_name = names_by_basename.get(f"CIK{int(cik):010d}.json")
            if not main_name:
                logging.warning("CIK %s is absent from submissions.zip", cik)
                continue
            try:
                data = _read_json_member(archive, main_name, names_by_basename)
            except Exception as exc:
                logging.error("Could not read SEC metadata for CIK %s: %s", cik, exc)
                continue

            company_name = str(data.get("name", "") or "")
            filing_groups: list[dict[str, Any]] = [
                data.get("filings", {}).get("recent", {})
            ]
            for page in data.get("filings", {}).get("files", []):
                if not _metadata_page_relevant(page, target_years, year_match):
                    continue
                page_name = str(page.get("name", "") or "")
                if not page_name:
                    continue
                try:
                    filing_groups.append(
                        _read_json_member(archive, page_name, names_by_basename)
                    )
                except Exception as exc:
                    if historical_page_loader is not None:
                        try:
                            filing_groups.append(historical_page_loader(page_name))
                            continue
                        except Exception as api_exc:
                            exc = api_exc
                    historical_metadata_errors.append(
                        f"CIK {cik}, {page_name}: {type(exc).__name__}: {exc}"
                    )
                    logging.warning(
                        "Could not read historical SEC metadata %s for CIK %s: %s",
                        page_name,
                        cik,
                        exc,
                    )

            for group in filing_groups:
                for filing in _columns_to_rows(group):
                    form_type = str(filing.get("form", "") or "").upper().strip()
                    if form_type not in TARGET_FORMS:
                        continue
                    accession = str(filing.get("accessionNumber", "") or "").strip()
                    filing_date = str(filing.get("filingDate", "") or "").strip()
                    report_date = str(filing.get("reportDate", "") or "").strip()
                    filing_year = parse_year(filing_date)
                    report_year = parse_year(report_date)
                    matched_year = (
                        report_year if year_match == "report-date" else filing_year
                    )
                    if not accession or matched_year not in target_years:
                        continue
                    primary_document = str(
                        filing.get("primaryDocument", "") or ""
                    ).strip()
                    url, document_kind, extension = _filing_urls(
                        cik,
                        accession,
                        primary_document,
                        allow_complete_submission_fallback=allow_complete_submission_fallback,
                    )
                    form_dir = form_type.replace("/", "-")
                    local_path = (
                        data_dir
                        / "raw"
                        / form_dir
                        / str(matched_year)
                        / f"{accession}{extension}"
                    )
                    result_path = (
                        data_dir
                        / "windows"
                        / form_dir
                        / str(matched_year)
                        / f"{accession}.json"
                    )
                    selected.append(
                        {
                            "accession_number": accession,
                            "cik": cik,
                            "company_name": company_name,
                            "year": matched_year,
                            "year_match_method": year_match,
                            "form_type": form_type,
                            "filing_date": filing_date,
                            "report_date": report_date,
                            "filing_year": filing_year,
                            "report_year": report_year,
                            "primary_document": primary_document,
                            "document_kind": document_kind,
                            "source_url": url,
                            "local_path": str(local_path),
                            "result_path": str(result_path),
                            "metadata_status": (
                                "selected" if url else "missing_primary_document"
                            ),
                            "download_status": "pending" if url else "not_downloadable",
                            "download_error": "",
                            "download_bytes": pd.NA,
                            "download_sha256": "",
                            "download_updated_at": "",
                            "processing_batch": pd.NA,
                            "raw_deleted_at": "",
                            "extract_status": "pending" if url else "not_downloadable",
                            "extract_error": "",
                            "window_count": pd.NA,
                            "keyword_hit_count": pd.NA,
                            "exact_duplicate_windows_removed": pd.NA,
                            "extract_updated_at": "",
                            "extraction_version": EXTRACTION_VERSION,
                        }
                    )
            if position % 500 == 0:
                logging.info(
                    "Scanned SEC submissions metadata for %d target CIKs", position
                )

    if historical_metadata_errors:
        preview = "\n".join(historical_metadata_errors[:10])
        raise RuntimeError(
            "Required historical SEC submissions metadata could not be read; "
            "refusing to build an incomplete manifest. First errors:\n" + preview
        )

    manifest = pd.DataFrame(selected)
    if manifest.empty:
        raise RuntimeError("No matching 10-K/10-K/A filings were found")
    manifest = manifest.drop_duplicates("accession_number", keep="first")
    manifest = manifest.sort_values(
        ["year", "cik", "filing_date", "form_type", "accession_number"]
    ).reset_index(drop=True)
    return manifest


def _allocate_proportional_quotas(counts: dict[Any, int], total: int) -> dict[Any, int]:
    """Allocate an exact sample total with capacity and largest remainders."""

    available = sum(counts.values())
    if total < 0 or total > available:
        raise ValueError(f"sample size {total} is outside [0, {available}]")
    if total == 0:
        return {key: 0 for key in counts}
    nonempty = [key for key, count in counts.items() if count > 0]
    minimum = 1 if total >= len(nonempty) else 0
    raw = {key: total * count / available for key, count in counts.items()}
    quotas = {
        key: min(count, max(minimum, int(raw[key]))) for key, count in counts.items()
    }
    while sum(quotas.values()) < total:
        candidates = [key for key in counts if quotas[key] < counts[key]]
        key = max(candidates, key=lambda item: (raw[item] - quotas[item], str(item)))
        quotas[key] += 1
    while sum(quotas.values()) > total:
        candidates = [key for key in counts if quotas[key] > minimum]
        key = max(candidates, key=lambda item: (quotas[item] - raw[item], str(item)))
        quotas[key] -= 1
    return quotas


def select_manifest_sample(
    manifest: pd.DataFrame, sample_size: int, *, seed: str = DEFAULT_SAMPLE_SEED
) -> tuple[pd.DataFrame, dict[str, Any]]:
    """Select a deterministic form/year-stratified live-filing QA sample."""

    if sample_size <= 0:
        raise ValueError("sample_size must be positive")
    eligible = manifest.loc[
        manifest["metadata_status"].eq("selected")
        & manifest["form_type"].isin(TARGET_FORMS)
    ].copy()
    if sample_size > len(eligible):
        raise ValueError(
            f"sample_size {sample_size} exceeds {len(eligible)} eligible filings"
        )
    eligible["_stable_sample_order"] = eligible["accession_number"].map(
        lambda accession: hashlib.sha256(
            f"{seed}|{accession}".encode("utf-8")
        ).hexdigest()
    )
    form_counts = {
        str(form): int(count)
        for form, count in eligible["form_type"].value_counts().items()
    }
    form_quotas = _allocate_proportional_quotas(form_counts, sample_size)
    selected_indices: list[int] = []
    used_ciks: set[str] = set()
    strata: dict[str, int] = {}

    for form_type in sorted(form_quotas):
        form_pool = eligible.loc[eligible["form_type"].eq(form_type)]
        year_counts = {
            int(year): int(count)
            for year, count in form_pool["year"].value_counts().items()
        }
        year_quotas = _allocate_proportional_quotas(year_counts, form_quotas[form_type])
        for year in sorted(year_quotas):
            quota = year_quotas[year]
            if quota == 0:
                continue
            candidates = form_pool.loc[form_pool["year"].eq(year)].sort_values(
                ["_stable_sample_order", "cik", "accession_number"]
            )
            chosen: list[int] = []
            for index, row in candidates.iterrows():
                if str(row["cik"]) in used_ciks:
                    continue
                chosen.append(index)
                used_ciks.add(str(row["cik"]))
                if len(chosen) == quota:
                    break
            if len(chosen) < quota:
                for index in candidates.index:
                    if index in chosen:
                        continue
                    chosen.append(index)
                    used_ciks.add(str(candidates.loc[index, "cik"]))
                    if len(chosen) == quota:
                        break
            if len(chosen) != quota:
                raise RuntimeError(
                    f"Could not fill sample stratum {form_type}/{year}: "
                    f"needed {quota}, selected {len(chosen)}"
                )
            selected_indices.extend(chosen)
            strata[f"{form_type}|{year}"] = len(chosen)

    if len(selected_indices) != sample_size:
        raise RuntimeError(
            f"Sample selection produced {len(selected_indices)} rows, "
            f"expected {sample_size}"
        )
    sample = manifest.loc[selected_indices].copy()
    sample = sample.sort_values(
        ["year", "form_type", "cik", "accession_number"]
    ).reset_index(drop=True)
    sample = sample.drop(columns=["_stable_sample_order"], errors="ignore")
    audit = {
        "selection": "deterministic proportional stratification by form and year",
        "seed": seed,
        "source_filings": len(eligible),
        "sample_filings": len(sample),
        "unique_ciks": int(sample["cik"].nunique()),
        "forms": {
            str(form): int(count)
            for form, count in sample["form_type"].value_counts().items()
        },
        "years": {
            str(int(year)): int(count)
            for year, count in sample["year"].value_counts().sort_index().items()
        },
        "strata": strata,
        "accessions": sample["accession_number"].tolist(),
    }
    return sample, audit


def load_sec_historical_page(
    page_name: str,
    *,
    cache_dir: Path,
    user_agent: str,
    limiter: RateLimiter,
    retries: int,
    timeout_seconds: float,
) -> dict[str, Any]:
    """Read a cached SEC continuation page or retrieve it from the official API."""

    safe_name = Path(page_name).name
    if not safe_name.lower().endswith(".json"):
        raise ValueError(f"unsafe SEC historical page name: {page_name}")
    cache_path = cache_dir / safe_name
    if cache_path.exists():
        try:
            return json.loads(cache_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            cache_path.unlink()
    status, error, _, _ = download_to_path(
        f"{SEC_SUBMISSIONS_API_ROOT}/{quote(safe_name)}",
        cache_path,
        user_agent=user_agent,
        limiter=limiter,
        retries=retries,
        timeout_seconds=timeout_seconds,
        min_bytes=20,
    )
    if status == "failed":
        raise RuntimeError(f"could not download {safe_name}: {error}")
    try:
        return json.loads(cache_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise RuntimeError(f"invalid SEC JSON page: {cache_path}") from exc


def load_manifest(path: Path) -> pd.DataFrame:
    manifest = pd.read_csv(
        path,
        dtype={
            "accession_number": "string",
            "cik": "string",
        },
    )
    manifest = manifest.fillna("")
    manifest["cik"] = manifest["cik"].map(normalize_cik)
    for column in ("year", "filing_year", "report_year"):
        manifest[column] = pd.to_numeric(manifest[column], errors="coerce").astype(
            "Int64"
        )
    return manifest


MANIFEST_PROGRESS_COLUMNS = (
    "download_status",
    "download_error",
    "download_bytes",
    "download_sha256",
    "download_updated_at",
    "processing_batch",
    "raw_deleted_at",
    "extract_status",
    "extract_error",
    "window_count",
    "keyword_hit_count",
    "exact_duplicate_windows_removed",
    "extract_updated_at",
    "extraction_version",
)


def merge_manifest_progress(
    fresh: pd.DataFrame, previous: pd.DataFrame
) -> pd.DataFrame:
    """Carry resumable state forward when SEC metadata is refreshed.

    An accession is immutable, but the source URL comparison adds a guard
    against accidentally reusing extraction state after a path-selection rule
    changes.
    """

    if fresh.empty or previous.empty or "accession_number" not in previous:
        return fresh
    prior = previous.drop_duplicates("accession_number", keep="last").set_index(
        "accession_number", drop=False
    )
    for index, row in fresh.iterrows():
        accession = str(row["accession_number"])
        if accession not in prior.index:
            continue
        old = prior.loc[accession]
        if isinstance(old, pd.DataFrame):
            old = old.iloc[-1]
        if str(old.get("source_url", "")) != str(row.get("source_url", "")):
            continue
        for column in MANIFEST_PROGRESS_COLUMNS:
            if column in previous.columns:
                fresh.at[index, column] = old.get(column, "")
    return fresh


def metadata_coverage(lookup: pd.DataFrame, manifest: pd.DataFrame) -> pd.DataFrame:
    """Audit how many selected annual forms exist for every requested CIK-year."""

    counts = (
        manifest.groupby(["cik", "year", "form_type"], dropna=False)
        .size()
        .unstack(fill_value=0)
        .reset_index()
    )
    for form_type in TARGET_FORMS:
        if form_type not in counts.columns:
            counts[form_type] = 0
    counts = counts.rename(
        columns={"10-K": "form_10k_count", "10-K/A": "form_10ka_count"}
    )
    coverage = lookup.merge(
        counts[["cik", "year", "form_10k_count", "form_10ka_count"]],
        on=["cik", "year"],
        how="left",
    )
    for column in ("form_10k_count", "form_10ka_count"):
        coverage[column] = coverage[column].fillna(0).astype(int)
    coverage["filing_count"] = coverage["form_10k_count"] + coverage["form_10ka_count"]
    coverage["has_matching_filing"] = coverage["filing_count"].gt(0)
    return coverage.sort_values(["year", "cik"]).reset_index(drop=True)


def _download_manifest_row(
    row: dict[str, Any],
    *,
    user_agent: str,
    limiter: RateLimiter,
    retries: int,
    timeout_seconds: float,
    min_bytes: int,
) -> dict[str, Any]:
    result: dict[str, Any] = {"index": int(row["_index"])}
    if str(row.get("metadata_status", "")) != "selected":
        result.update(
            download_status="not_downloadable",
            download_error="missing primary document and fallback disabled",
            download_bytes=0,
            download_sha256="",
        )
        return result
    status, error, size, digest = download_to_path(
        str(row["source_url"]),
        Path(str(row["local_path"])),
        user_agent=user_agent,
        limiter=limiter,
        retries=retries,
        timeout_seconds=timeout_seconds,
        min_bytes=min_bytes,
    )
    result.update(
        download_status=status,
        download_error=error,
        download_bytes=size,
        download_sha256=digest,
    )
    return result


def download_manifest_filings(
    manifest: pd.DataFrame,
    manifest_path: Path,
    *,
    user_agent: str,
    rate_per_second: float,
    workers: int,
    retries: int,
    timeout_seconds: float,
    min_bytes: int,
    save_every: int = DEFAULT_CHECKPOINT_EVERY,
) -> pd.DataFrame:
    """Download selected primary documents and checkpoint the manifest."""

    limiter = RateLimiter(rate_per_second)
    rows: list[dict[str, Any]] = []
    for index, row in manifest.iterrows():
        payload = row.to_dict()
        payload["_index"] = index
        rows.append(payload)

    completed = 0
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = [
            executor.submit(
                _download_manifest_row,
                row,
                user_agent=user_agent,
                limiter=limiter,
                retries=retries,
                timeout_seconds=timeout_seconds,
                min_bytes=min_bytes,
            )
            for row in rows
        ]
        for future in as_completed(futures):
            result = future.result()
            index = int(result.pop("index"))
            for key, value in result.items():
                manifest.at[index, key] = value
            manifest.at[index, "download_updated_at"] = utc_now()
            completed += 1
            if completed % save_every == 0:
                atomic_write_dataframe(manifest, manifest_path)
                logging.info("Downloaded/validated %d/%d filings", completed, len(rows))
    atomic_write_dataframe(manifest, manifest_path)
    return manifest


def download_next_storage_batch(
    manifest: pd.DataFrame,
    manifest_path: Path,
    pending_indices: deque[int],
    *,
    batch_id: int,
    max_batch_bytes: int,
    user_agent: str,
    rate_per_second: float,
    workers: int,
    retries: int,
    timeout_seconds: float,
    min_bytes: int,
    save_every: int = DEFAULT_CHECKPOINT_EVERY,
) -> tuple[pd.DataFrame, list[int], int]:
    """Download one storage-bounded batch with only a small in-flight queue.

    The byte limit is evaluated as responses finish.  At most ``workers - 1``
    already-started filings can make a batch exceed the target, which avoids
    serial downloads while keeping the overshoot tightly bounded.
    """

    if max_batch_bytes <= 0:
        raise ValueError("max_batch_bytes must be positive")
    limiter = RateLimiter(rate_per_second)
    batch_indices: list[int] = []
    batch_bytes = 0
    completed = 0

    def row_payload(index: int) -> dict[str, Any]:
        payload = manifest.loc[index].to_dict()
        payload["_index"] = index
        return payload

    with ThreadPoolExecutor(max_workers=workers) as executor:
        in_flight: dict[Any, int] = {}

        def fill_queue() -> None:
            while (
                pending_indices
                and len(in_flight) < workers
                and batch_bytes < max_batch_bytes
            ):
                index = pending_indices.popleft()
                future = executor.submit(
                    _download_manifest_row,
                    row_payload(index),
                    user_agent=user_agent,
                    limiter=limiter,
                    retries=retries,
                    timeout_seconds=timeout_seconds,
                    min_bytes=min_bytes,
                )
                in_flight[future] = index

        fill_queue()
        while in_flight:
            done, _ = wait(in_flight, return_when=FIRST_COMPLETED)
            for future in done:
                in_flight.pop(future)
                result = future.result()
                index = int(result.pop("index"))
                for key, value in result.items():
                    manifest.at[index, key] = value
                manifest.at[index, "download_updated_at"] = utc_now()
                completed += 1
                if str(result.get("download_status", "")) in DOWNLOADABLE_STATUSES:
                    size = int(result.get("download_bytes", 0) or 0)
                    batch_bytes += size
                    batch_indices.append(index)
                    manifest.at[index, "processing_batch"] = batch_id
                if completed % save_every == 0:
                    atomic_write_dataframe(manifest, manifest_path)
            fill_queue()

    atomic_write_dataframe(manifest, manifest_path)
    logging.info(
        "Raw batch %d downloaded %d filings (%.2f GiB); %d remain",
        batch_id,
        len(batch_indices),
        batch_bytes / GIB,
        len(pending_indices),
    )
    return manifest, batch_indices, batch_bytes


def normalize_readable_text(value: str) -> str:
    value = html.unescape(value).replace("\x00", " ")
    value = unicodedata.normalize("NFKC", value).replace("\u00a0", " ")
    lines = [
        HORIZONTAL_SPACE_RE.sub(" ", line).strip()
        for line in value.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    ]
    return EXCESS_NEWLINES_RE.sub("\n\n", "\n".join(lines)).strip()


def _local_tag_name(tag: Any) -> str:
    if not isinstance(tag, str):
        return ""
    return tag.rsplit("}", 1)[-1].split(":", 1)[-1].lower()


def _drop_lxml_tree(element: Any) -> None:
    """Remove an element while preserving readable tail text."""

    if element.getparent() is None:
        return
    element.drop_tree()


def _readable_text_lxml(payload: bytes) -> str:
    parser = lxml_html.HTMLParser(recover=True, huge_tree=True)
    root = lxml_html.fromstring(payload, parser=parser)

    for element in list(root.iter()):
        if element.getparent() is None:
            continue
        raw_tag = str(element.tag).lower()
        local_tag = _local_tag_name(element.tag)
        is_inline_xbrl = "inline" in raw_tag or raw_tag.startswith("ix:")
        style = str(element.attrib.get("style", "")).replace(" ", "").lower()
        hidden = (
            "hidden" in element.attrib
            or str(element.attrib.get("aria-hidden", "")).lower() == "true"
            or "display:none" in style
            or "visibility:hidden" in style
        )
        if (
            local_tag in NON_READABLE_TAGS
            or (is_inline_xbrl and local_tag in {"header", "hidden"})
            or hidden
        ):
            _drop_lxml_tree(element)

    for element in root.iter():
        local_tag = _local_tag_name(element.tag)
        if local_tag == "br":
            element.tail = "\n" + (element.tail or "")
        elif local_tag in BLOCK_TAGS:
            element.tail = "\n" + (element.tail or "")
        elif local_tag in INLINE_SEPARATOR_TAGS:
            element.tail = " " + (element.tail or "")
    return normalize_readable_text(root.text_content())


def _readable_text_bs4(payload: bytes) -> str:
    decoded = UnicodeDammit(payload).unicode_markup or payload.decode(
        "utf-8", errors="replace"
    )
    soup = BeautifulSoup(decoded, "html.parser")
    for tag in soup.find_all(NON_READABLE_TAGS):
        tag.decompose()
    for tag in soup.find_all(
        lambda item: bool(item.name)
        and str(item.name).lower() in {"ix:header", "ix:hidden"}
    ):
        tag.decompose()
    for tag in soup.find_all(True):
        if tag.attrs is None:
            continue
        style = str(tag.attrs.get("style", "")).replace(" ", "").lower()
        if (
            tag.has_attr("hidden")
            or str(tag.attrs.get("aria-hidden", "")).lower() == "true"
            or "display:none" in style
            or "visibility:hidden" in style
        ):
            tag.decompose()
    for br in soup.find_all("br"):
        br.replace_with("\n")
    for tag in soup.find_all(list(BLOCK_TAGS)):
        tag.append("\n")
    return normalize_readable_text(soup.get_text(" ", strip=False))


def readable_text_from_document(payload: bytes) -> str:
    """Convert an HTML, inline-XBRL, or plain-text primary document to text."""

    prefix = payload[:4096].lower()
    looks_like_markup = any(
        marker in prefix
        for marker in (
            b"<html",
            b"<body",
            b"<head",
            b"<div",
            b"<p",
            b"<table",
            b"<ix:",
            b"<!doctype",
            b"<document",
            b"<xbrl",
        )
    )
    if not looks_like_markup:
        decoded = UnicodeDammit(payload).unicode_markup or payload.decode(
            "utf-8", errors="replace"
        )
        return normalize_readable_text(decoded)
    if lxml_html is not None:
        return _readable_text_lxml(payload)
    logging.warning(
        "lxml is not installed; falling back to slower BeautifulSoup parsing"
    )
    return _readable_text_bs4(payload)


def primary_payload_from_complete_submission(payload: bytes, form_type: str) -> bytes:
    """Select only the annual-form DOCUMENT from an SEC submission fallback."""

    target = form_type.upper().strip()
    for document_match in SUBMISSION_DOCUMENT_RE.finditer(payload):
        block = document_match.group(1)
        type_match = SUBMISSION_TYPE_RE.search(block)
        if not type_match:
            continue
        candidate_type = (
            type_match.group(1).decode("ascii", errors="ignore").upper().strip()
        )
        if candidate_type != target:
            continue
        text_match = SUBMISSION_TEXT_RE.search(block)
        if text_match:
            return text_match.group(1)
        return block
    raise ValueError(f"complete submission contains no <DOCUMENT> with <TYPE>{target}")


@dataclass(frozen=True)
class SentenceSpan:
    text: str
    start: int
    end: int


@dataclass(frozen=True)
class DictionaryMatch:
    """One literal regex hit from the project's existing AI dictionary."""

    keyword: str
    start: int
    end: int
    text: str


def find_dictionary_matches(text: str) -> list[DictionaryMatch]:
    """Find dictionary terms without adoption-oriented routing filters.

    The downstream scorer deliberately disambiguates and ranks terms.  This
    retrieval stage has a different, high-recall job, so every literal match
    admitted by an ``AI_KEYWORDS`` regex is retained.  Overlapping expressions
    are represented by the longest expression because they identify the same
    source-text position and therefore produce the same sentence window.
    """

    candidates: list[DictionaryMatch] = []
    text_lower = text.lower()
    for keyword in ai_utils.ORDERED_AI_KEYWORD_NAMES:
        # Each anchor is a mandatory literal part of its rule. This cheap gate
        # avoids running dozens of full-document regex scans for absent terms.
        if AI_KEYWORD_SCAN_ANCHORS[keyword] not in text_lower:
            continue
        for match in ai_utils.COMPILED_AI_KEYWORD_RULES[keyword].finditer(text):
            candidates.append(
                DictionaryMatch(
                    keyword=str(keyword),
                    start=match.start(),
                    end=match.end(),
                    text=match.group(0),
                )
            )

    candidates.sort(
        key=lambda item: (item.start, -(item.end - item.start), item.keyword)
    )
    selected: list[DictionaryMatch] = []
    for candidate in candidates:
        if selected and candidate.start < selected[-1].end:
            continue
        selected.append(candidate)
    return selected


def sentence_spans(text: str) -> list[SentenceSpan]:
    """Split readable filing text while retaining character offsets."""

    spans: list[SentenceSpan] = []
    start = 0
    for boundary in BOUNDARY_RE.finditer(text):
        end = boundary.end()
        raw = text[start:end]
        left_trim = len(raw) - len(raw.lstrip())
        right_trimmed = raw.rstrip()
        trimmed_end = start + len(right_trimmed)
        sentence = raw.strip()
        if sentence:
            spans.append(
                SentenceSpan(
                    sentence, start + left_trim, max(start + left_trim, trimmed_end)
                )
            )
        start = end
    raw = text[start:]
    sentence = raw.strip()
    if sentence:
        left_trim = len(raw) - len(raw.lstrip())
        spans.append(SentenceSpan(sentence, start + left_trim, len(text.rstrip())))
    return spans


def _sentence_index_for_offset(
    spans: Sequence[SentenceSpan], starts: Sequence[int], offset: int
) -> int:
    index = bisect.bisect_right(starts, offset) - 1
    if index < 0:
        return 0
    while index + 1 < len(spans) and offset >= spans[index].end:
        index += 1
    return min(index, len(spans) - 1)


def merge_sentence_intervals(
    intervals: Iterable[tuple[int, int]],
) -> list[tuple[int, int]]:
    """Merge overlapping or immediately adjacent inclusive sentence intervals."""

    merged: list[list[int]] = []
    for start, end in sorted(intervals):
        if not merged or start > merged[-1][1] + 1:
            merged.append([start, end])
        else:
            merged[-1][1] = max(merged[-1][1], end)
    return [(start, end) for start, end in merged]


def extract_keyword_windows(
    text: str,
    *,
    context_sentences: int,
    filing_metadata: dict[str, Any],
) -> tuple[list[dict[str, Any]], dict[str, int]]:
    """Return every merged keyword sentence window in original filing order."""

    if context_sentences < 0:
        raise ValueError("context_sentences must be non-negative")
    # Match the dictionary literally here.  Adoption-oriented ambiguity checks,
    # routing, ranking, and prompt limits remain downstream in get_ai_adoption.
    matches = find_dictionary_matches(text)
    spans = sentence_spans(text)
    if not matches or not spans:
        return [], {
            "keyword_hit_count": len(matches),
            "exact_duplicate_windows_removed": 0,
        }

    starts = [span.start for span in spans]
    located_matches: list[tuple[Any, int, int]] = []
    intervals: list[tuple[int, int]] = []
    for match in matches:
        first = _sentence_index_for_offset(spans, starts, match.start)
        last = _sentence_index_for_offset(
            spans, starts, max(match.start, match.end - 1)
        )
        located_matches.append((match, first, last))
        intervals.append(
            (
                max(0, first - context_sentences),
                min(len(spans) - 1, last + context_sentences),
            )
        )

    merged = merge_sentence_intervals(intervals)
    provisional: list[dict[str, Any]] = []
    for interval_start, interval_end in merged:
        chunk_matches = [
            match
            for match, match_start, match_end in located_matches
            if match_end >= interval_start and match_start <= interval_end
        ]
        chunk_text = " ".join(
            span.text for span in spans[interval_start : interval_end + 1]
        ).strip()
        if not chunk_text or not chunk_matches:
            continue
        canonical = re.sub(r"\s+", " ", chunk_text).strip()
        provisional.append(
            {
                "item": WINDOW_ITEM,
                "year": int(filing_metadata["year"]),
                "accession_number": str(filing_metadata["accession_number"]),
                "cik": str(filing_metadata["cik"]),
                "form_type": str(filing_metadata["form_type"]),
                "text": chunk_text,
                "sentence_start": interval_start,
                "sentence_end": interval_end,
                "keyword_hit_count": len(chunk_matches),
                "keyword_names": "|".join(
                    sorted({str(match.keyword) for match in chunk_matches})
                ),
                "matched_terms": "|".join(
                    sorted({str(match.text) for match in chunk_matches}, key=str.lower)
                ),
                "text_sha256": sha256_text(canonical),
                "duplicate_occurrences": 1,
                "filing_date": str(filing_metadata.get("filing_date", "")),
                "report_date": str(filing_metadata.get("report_date", "")),
                "filing_year": parse_year(filing_metadata.get("filing_year")),
                "report_year": parse_year(filing_metadata.get("report_year")),
                "primary_document": str(filing_metadata.get("primary_document", "")),
                "source_url": str(filing_metadata.get("source_url", "")),
                "document_kind": str(filing_metadata.get("document_kind", "")),
                "context_sentences": context_sentences,
                "extraction_version": EXTRACTION_VERSION,
                "is_no_keyword_sentinel": False,
            }
        )

    # Exact normalized text only: no fuzzy/semantic near-deduplication.
    unique_by_hash: dict[str, dict[str, Any]] = {}
    ordered_unique: list[dict[str, Any]] = []
    duplicates_removed = 0
    for row in provisional:
        digest = str(row["text_sha256"])
        existing = unique_by_hash.get(digest)
        if existing is None:
            unique_by_hash[digest] = row
            ordered_unique.append(row)
            continue
        duplicates_removed += 1
        existing["duplicate_occurrences"] += 1
        existing["keyword_hit_count"] += int(row["keyword_hit_count"])
        existing["keyword_names"] = "|".join(
            sorted(
                set(str(existing["keyword_names"]).split("|"))
                | set(str(row["keyword_names"]).split("|"))
            )
        )
        existing["matched_terms"] = "|".join(
            sorted(
                set(str(existing["matched_terms"]).split("|"))
                | set(str(row["matched_terms"]).split("|")),
                key=str.lower,
            )
        )
    for window_id, row in enumerate(ordered_unique, start=1):
        row["window_id"] = window_id
    return ordered_unique, {
        "keyword_hit_count": len(matches),
        "exact_duplicate_windows_removed": duplicates_removed,
    }


def _read_existing_extraction(
    path: Path,
    source_sha256: str,
    context_sentences: int,
    accession_number: Optional[str] = None,
) -> Optional[dict[str, Any]]:
    if not path.exists():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    if payload.get("source_sha256") != source_sha256:
        return None
    if payload.get("extraction_version") != EXTRACTION_VERSION:
        return None
    if payload.get("context_sentences") != context_sentences:
        return None
    if accession_number is not None and str(payload.get("accession_number", "")) != str(
        accession_number
    ):
        return None
    try:
        readable_text_chars = int(payload.get("readable_text_chars", 0) or 0)
        keyword_hit_count = int(payload.get("keyword_hit_count", 0) or 0)
    except (TypeError, ValueError):
        return None
    if readable_text_chars <= 0 or keyword_hit_count < 0:
        return None
    windows = payload.get("windows")
    if not isinstance(windows, list):
        return None
    if not windows and keyword_hit_count != 0:
        return None
    if windows and keyword_hit_count <= 0:
        return None

    window_ids: list[int] = []
    text_hashes: list[str] = []
    for window in windows:
        if not isinstance(window, dict):
            return None
        try:
            window_ids.append(int(window.get("window_id", 0)))
        except (TypeError, ValueError):
            return None
        text = str(window.get("text", ""))
        text_hash = str(window.get("text_sha256", ""))
        if not text or text_hash != sha256_text(text):
            return None
        terms = [
            term for term in str(window.get("matched_terms", "")).split("|") if term
        ]
        folded_text = text.casefold()
        if not terms or not all(term.casefold() in folded_text for term in terms):
            return None
        text_hashes.append(text_hash)
    if window_ids != list(range(1, len(windows) + 1)):
        return None
    if len(text_hashes) != len(set(text_hashes)):
        return None
    return payload


def _extract_manifest_row(
    row: dict[str, Any], *, context_sentences: int, overwrite: bool
) -> dict[str, Any]:
    index = int(row["_index"])
    result_path = Path(str(row["result_path"]))
    source_path = Path(str(row["local_path"]))
    source_sha = str(row.get("download_sha256", "") or "")
    if not source_sha and source_path.exists():
        source_sha = sha256_file(source_path)
    if not overwrite:
        existing = _read_existing_extraction(
            result_path,
            source_sha,
            context_sentences,
            accession_number=str(row.get("accession_number", "")),
        )
        if existing is not None:
            windows = existing["windows"]
            return {
                "index": index,
                "extract_status": "skipped_valid" if windows else "no_keyword",
                "extract_error": "",
                "window_count": len(windows),
                "keyword_hit_count": int(existing.get("keyword_hit_count", 0)),
                "exact_duplicate_windows_removed": int(
                    existing.get("exact_duplicate_windows_removed", 0)
                ),
            }
    valid, reason = validate_download_file(source_path)
    if not valid:
        return {
            "index": index,
            "extract_status": "failed",
            "extract_error": f"source download invalid: {reason}",
            "window_count": 0,
            "keyword_hit_count": 0,
            "exact_duplicate_windows_removed": 0,
        }
    try:
        source_payload = source_path.read_bytes()
        if str(row.get("document_kind", "")) == "complete_submission_fallback":
            source_payload = primary_payload_from_complete_submission(
                source_payload, str(row.get("form_type", ""))
            )
        readable_text = readable_text_from_document(source_payload)
        if not readable_text:
            raise ValueError("readable filing text is empty")
        windows, metrics = extract_keyword_windows(
            readable_text,
            context_sentences=context_sentences,
            filing_metadata=row,
        )
        atomic_write_json(
            result_path,
            {
                "accession_number": str(row["accession_number"]),
                "source_sha256": source_sha,
                "extraction_version": EXTRACTION_VERSION,
                "context_sentences": context_sentences,
                "readable_text_chars": len(readable_text),
                "keyword_hit_count": metrics["keyword_hit_count"],
                "exact_duplicate_windows_removed": metrics[
                    "exact_duplicate_windows_removed"
                ],
                "windows": windows,
            },
        )
        return {
            "index": index,
            "extract_status": "extracted" if windows else "no_keyword",
            "extract_error": "",
            "window_count": len(windows),
            **metrics,
        }
    except Exception as exc:
        return {
            "index": index,
            "extract_status": "failed",
            "extract_error": f"{type(exc).__name__}: {exc}",
            "window_count": 0,
            "keyword_hit_count": 0,
            "exact_duplicate_windows_removed": 0,
        }


def extract_manifest_filings(
    manifest: pd.DataFrame,
    manifest_path: Path,
    *,
    context_sentences: int,
    workers: int,
    overwrite: bool,
    indices: Optional[Iterable[int]] = None,
    save_every: int = DEFAULT_CHECKPOINT_EVERY,
) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []
    selected_indices = list(manifest.index if indices is None else indices)
    for index in selected_indices:
        row = manifest.loc[index]
        if str(row.get("download_status", "")) not in DOWNLOADABLE_STATUSES:
            continue
        payload = row.to_dict()
        payload["_index"] = index
        rows.append(payload)

    completed = 0
    # HTML parsing and full-text regex scanning are CPU-bound. Processes scale
    # across cores; threads did not improve throughput in the benchmark.  Keep
    # only two tasks per worker in flight so a large storage batch does not put
    # thousands of filing payloads into the multiprocessing queue.
    with ProcessPoolExecutor(max_workers=workers) as executor:
        row_iterator = iter(rows)
        in_flight: dict[Any, None] = {}

        def fill_queue() -> None:
            while len(in_flight) < workers * 2:
                try:
                    row = next(row_iterator)
                except StopIteration:
                    return
                future = executor.submit(
                    _extract_manifest_row,
                    row,
                    context_sentences=context_sentences,
                    overwrite=overwrite,
                )
                in_flight[future] = None

        fill_queue()
        while in_flight:
            done, _ = wait(in_flight, return_when=FIRST_COMPLETED)
            for future in done:
                in_flight.pop(future)
                result = future.result()
                index = int(result.pop("index"))
                for key, value in result.items():
                    manifest.at[index, key] = value
                manifest.at[index, "extract_updated_at"] = utc_now()
                completed += 1
                if completed % save_every == 0:
                    atomic_write_dataframe(manifest, manifest_path)
                    logging.info(
                        "Extracted/validated %d/%d filings", completed, len(rows)
                    )
            fill_queue()
    atomic_write_dataframe(manifest, manifest_path)
    return manifest


def _cached_extraction_for_manifest_row(
    row: pd.Series, context_sentences: int
) -> Optional[dict[str, Any]]:
    source_sha = str(row.get("download_sha256", "") or "").strip()
    if not source_sha:
        return None
    return _read_existing_extraction(
        Path(str(row.get("result_path", ""))),
        source_sha,
        context_sentences,
        accession_number=str(row.get("accession_number", "")),
    )


def _apply_cached_extraction_state(
    manifest: pd.DataFrame, index: int, payload: dict[str, Any]
) -> None:
    windows = payload.get("windows", [])
    manifest.at[index, "extract_status"] = "skipped_valid" if windows else "no_keyword"
    manifest.at[index, "extract_error"] = ""
    manifest.at[index, "window_count"] = len(windows)
    manifest.at[index, "keyword_hit_count"] = int(payload.get("keyword_hit_count", 0))
    manifest.at[index, "exact_duplicate_windows_removed"] = int(
        payload.get("exact_duplicate_windows_removed", 0)
    )
    manifest.at[index, "extraction_version"] = EXTRACTION_VERSION


def purge_manifest_raw_files(
    manifest: pd.DataFrame,
    manifest_path: Path,
    indices: Iterable[int],
    *,
    raw_root: Path,
    context_sentences: int,
    delete_unvalidated: bool = False,
) -> tuple[pd.DataFrame, int, int]:
    """Delete raw documents after an extraction attempt is checkpointed.

    By default, only files with a validated result are removed.  The bounded
    ``run`` workflow also removes failed inputs after recording their errors so
    failures cannot accumulate past the storage budget; a rerun retrieves them
    again from the recorded SEC URL.
    """

    resolved_root = raw_root.resolve()
    purged = 0
    bytes_freed = 0
    for index in indices:
        row = manifest.loc[index]
        payload = _cached_extraction_for_manifest_row(row, context_sentences)
        if payload is None and not delete_unvalidated:
            logging.warning(
                "Retaining raw filing without a valid extraction result: %s",
                row.get("accession_number", index),
            )
            continue
        source_path = Path(str(row.get("local_path", ""))).resolve()
        try:
            source_path.relative_to(resolved_root)
        except ValueError as exc:
            raise RuntimeError(
                f"Refusing to purge raw path outside {resolved_root}: {source_path}"
            ) from exc
        if source_path.exists():
            bytes_freed += source_path.stat().st_size
            source_path.unlink()
        manifest.at[index, "download_status"] = (
            PURGED_DOWNLOAD_STATUS
            if payload is not None
            else PURGED_AFTER_FAILURE_STATUS
        )
        manifest.at[index, "raw_deleted_at"] = utc_now()
        if payload is not None:
            manifest.at[index, "download_error"] = ""
        purged += 1
    atomic_write_dataframe(manifest, manifest_path)
    return manifest, purged, bytes_freed


def process_manifest_in_storage_batches(
    manifest: pd.DataFrame,
    manifest_path: Path,
    *,
    data_dir: Path,
    user_agent: str,
    rate_per_second: float,
    download_workers: int,
    extract_workers: int,
    retries: int,
    timeout_seconds: float,
    min_bytes: int,
    context_sentences: int,
    overwrite_extractions: bool,
    max_batch_bytes: int,
    keep_raw: bool,
) -> pd.DataFrame:
    """Download, extract, checkpoint, and purge bounded groups of filings."""

    pending_indices: deque[int] = deque()
    reusable_indices: list[int] = []
    for index, row in manifest.iterrows():
        if str(row.get("metadata_status", "")) != "selected":
            continue
        payload = (
            None
            if overwrite_extractions
            else _cached_extraction_for_manifest_row(row, context_sentences)
        )
        if payload is not None:
            _apply_cached_extraction_state(manifest, index, payload)
            reusable_indices.append(index)
        else:
            pending_indices.append(index)
    atomic_write_dataframe(manifest, manifest_path)

    if reusable_indices and not keep_raw:
        manifest, purged, bytes_freed = purge_manifest_raw_files(
            manifest,
            manifest_path,
            reusable_indices,
            raw_root=data_dir / "raw",
            context_sentences=context_sentences,
        )
        logging.info(
            "Reused %d valid extraction results and purged %d raw files (%.2f GiB)",
            len(reusable_indices),
            purged,
            bytes_freed / GIB,
        )

    prior_batches = pd.to_numeric(
        manifest.get("processing_batch", pd.Series(dtype="float64")), errors="coerce"
    )
    batch_id = int(prior_batches.max()) + 1 if prior_batches.notna().any() else 1
    while pending_indices:
        before = len(pending_indices)
        manifest, batch_indices, batch_bytes = download_next_storage_batch(
            manifest,
            manifest_path,
            pending_indices,
            batch_id=batch_id,
            max_batch_bytes=max_batch_bytes,
            user_agent=user_agent,
            rate_per_second=rate_per_second,
            workers=download_workers,
            retries=retries,
            timeout_seconds=timeout_seconds,
            min_bytes=min_bytes,
        )
        if not batch_indices:
            logging.warning(
                "No downloadable filings succeeded among %d attempted rows",
                before - len(pending_indices),
            )
            continue
        manifest = extract_manifest_filings(
            manifest,
            manifest_path,
            context_sentences=context_sentences,
            workers=extract_workers,
            overwrite=overwrite_extractions,
            indices=batch_indices,
        )
        if not keep_raw:
            manifest, purged, bytes_freed = purge_manifest_raw_files(
                manifest,
                manifest_path,
                batch_indices,
                raw_root=data_dir / "raw",
                context_sentences=context_sentences,
                delete_unvalidated=True,
            )
            logging.info(
                "Completed raw batch %d: extracted %d, purged %d files "
                "(%.2f/%.2f GiB downloaded)",
                batch_id,
                len(batch_indices),
                purged,
                bytes_freed / GIB,
                batch_bytes / GIB,
            )
        batch_id += 1
    return manifest


def iter_filing_windows(manifest: pd.DataFrame) -> Iterator[list[dict[str, Any]]]:
    for _, row in manifest.sort_values(
        ["year", "cik", "filing_date", "accession_number"]
    ).iterrows():
        if str(row.get("extract_status", "")) not in VALID_EXTRACTION_STATUSES:
            continue
        result_path = Path(str(row["result_path"]))
        try:
            payload = json.loads(result_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as exc:
            raise RuntimeError(
                f"Could not read extraction result {result_path}"
            ) from exc
        windows = payload.get("windows", [])
        if windows:
            yield windows
            continue
        # Preserve the research-sample denominator.  The scorer needs one row
        # to emit its ordinary hard-zero result for a filing with no dictionary
        # hits, but the sentinel carries no filing text.
        yield [
            {
                "item": WINDOW_ITEM,
                "year": int(row["year"]),
                "accession_number": str(row["accession_number"]),
                "cik": str(row["cik"]),
                "form_type": str(row["form_type"]),
                "text": "",
                "window_id": 0,
                "sentence_start": pd.NA,
                "sentence_end": pd.NA,
                "keyword_hit_count": 0,
                "keyword_names": "",
                "matched_terms": "",
                "text_sha256": sha256_text(""),
                "duplicate_occurrences": 0,
                "filing_date": str(row.get("filing_date", "")),
                "report_date": str(row.get("report_date", "")),
                "filing_year": parse_year(row.get("filing_year")),
                "report_year": parse_year(row.get("report_year")),
                "primary_document": str(row.get("primary_document", "")),
                "source_url": str(row.get("source_url", "")),
                "document_kind": str(row.get("document_kind", "")),
                "context_sentences": payload.get("context_sentences"),
                "extraction_version": EXTRACTION_VERSION,
                "is_no_keyword_sentinel": True,
            }
        ]


def write_rds_from_csv(csv_path: Path, rds_path: Path) -> None:
    helper = SCRIPT_DIR / "write_keyword_window_rds.R"
    subprocess.run(
        ["Rscript", str(helper), str(csv_path), str(rds_path)],
        check=True,
    )


def _write_output_chunk(
    filing_groups: Sequence[list[dict[str, Any]]],
    output_dir: Path,
    chunk_id: int,
) -> tuple[int, int]:
    rows = [row for group in filing_groups for row in group]
    frame = pd.DataFrame(rows)
    required_first = ["item", "year", "accession_number", "cik", "form_type", "text"]
    remaining = [column for column in frame.columns if column not in required_first]
    frame = frame[required_first + remaining]
    stem = f"extract_df_chunk_{chunk_id:05d}"
    csv_gz_path = output_dir / f"{stem}.csv.gz"
    rds_path = output_dir / f"{stem}.rds"
    temp_rds = output_dir / f".{stem}.rds.part.{os.getpid()}"
    temp_csv_gz = output_dir / f".{stem}.csv.gz.part.{os.getpid()}"
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", newline="", suffix=".csv", delete=False
    ) as handle:
        temp_csv = Path(handle.name)
    try:
        frame.to_csv(temp_csv, index=False, quoting=csv.QUOTE_MINIMAL)
        write_rds_from_csv(temp_csv, temp_rds)
        with temp_csv.open("rb") as source, gzip.open(temp_csv_gz, "wb") as target:
            shutil.copyfileobj(source, target)
        os.replace(temp_rds, rds_path)
        os.replace(temp_csv_gz, csv_gz_path)
    finally:
        if temp_csv.exists():
            temp_csv.unlink()
        if temp_rds.exists():
            temp_rds.unlink()
        if temp_csv_gz.exists():
            temp_csv_gz.unlink()
    return len(filing_groups), len(frame)


def assembly_source_fingerprint(manifest: pd.DataFrame) -> str:
    """Hash extraction state and result contents used to build output chunks."""

    rows: list[dict[str, str]] = []
    for _, row in manifest.sort_values("accession_number").iterrows():
        result_path = Path(str(row.get("result_path", "")))
        extract_status = str(row.get("extract_status", ""))
        if extract_status in {"extracted", "skipped_valid"}:
            extract_status = "valid_windows"
        rows.append(
            {
                "accession_number": str(row.get("accession_number", "")),
                "download_sha256": str(row.get("download_sha256", "")),
                "extract_status": extract_status,
                "result_sha256": (
                    sha256_file(result_path) if result_path.is_file() else ""
                ),
            }
        )
    return sha256_text(json.dumps(rows, sort_keys=True, separators=(",", ":")))


def assemble_compatible_chunks(
    manifest: pd.DataFrame,
    output_dir: Path,
    *,
    filings_per_output: int,
    overwrite: bool,
) -> dict[str, Any]:
    """Create familiar RDS chunk files without dropping any extracted window."""

    if filings_per_output <= 0:
        raise ValueError("filings_per_output must be positive")
    source_fingerprint = assembly_source_fingerprint(manifest)
    existing_rds = (
        list(output_dir.glob("extract_df_chunk_*.rds")) if output_dir.exists() else []
    )
    existing_csv = (
        list(output_dir.glob("extract_df_chunk_*.csv.gz"))
        if output_dir.exists()
        else []
    )
    existing = existing_rds + existing_csv
    if existing and not overwrite:
        summary_path = output_dir / "extraction_summary.json"
        try:
            prior_summary = json.loads(summary_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            prior_summary = {}
        expected_chunks = int(prior_summary.get("output_chunks", -1))
        expected_stems = {
            f"extract_df_chunk_{chunk_id:05d}"
            for chunk_id in range(1, expected_chunks + 1)
        }
        rds_stems = {path.stem for path in existing_rds}
        csv_stems = {path.name.removesuffix(".csv.gz") for path in existing_csv}
        reusable = (
            prior_summary.get("extraction_version") == EXTRACTION_VERSION
            and prior_summary.get("assembly_version") == ASSEMBLY_VERSION
            and prior_summary.get("source_fingerprint") == source_fingerprint
            and int(prior_summary.get("filings_per_output_file", -1))
            == filings_per_output
            and rds_stems == expected_stems
            and csv_stems == expected_stems
        )
        if reusable:
            logging.info("Reusing %d validated output chunks", expected_chunks)
            return prior_summary
        raise FileExistsError(
            f"{output_dir} contains stale or incomplete output chunks; "
            "use --overwrite-chunks to rebuild them"
        )
    output_dir.mkdir(parents=True, exist_ok=True)
    if overwrite:
        for pattern in ("extract_df_chunk_*.rds", "extract_df_chunk_*.csv.gz"):
            for path in output_dir.glob(pattern):
                path.unlink()

    pending: list[list[dict[str, Any]]] = []
    chunk_id = 0
    total_filings = 0
    filings_without_windows = 0
    total_windows = 0
    total_input_rows = 0
    for filing_windows in iter_filing_windows(manifest):
        total_filings += 1
        input_windows = sum(
            int(row.get("window_id", 0) or 0) > 0 for row in filing_windows
        )
        total_windows += input_windows
        total_input_rows += len(filing_windows)
        if input_windows == 0:
            filings_without_windows += 1
        pending.append(filing_windows)
        if len(pending) < filings_per_output:
            continue
        chunk_id += 1
        filings, windows = _write_output_chunk(pending, output_dir, chunk_id)
        logging.info(
            "Wrote output chunk %05d: %d filings, %d rows", chunk_id, filings, windows
        )
        pending = []
    if pending:
        chunk_id += 1
        filings, windows = _write_output_chunk(pending, output_dir, chunk_id)

    summary = {
        "extraction_version": EXTRACTION_VERSION,
        "assembly_version": ASSEMBLY_VERSION,
        "created_at": utc_now(),
        "output_dir": str(output_dir),
        "output_chunks": chunk_id,
        "filings_in_output": total_filings,
        "filings_with_keyword_windows": total_filings - filings_without_windows,
        "filings_without_keyword_windows": filings_without_windows,
        "keyword_windows": total_windows,
        "scorer_input_rows": total_input_rows,
        "filings_per_output_file": filings_per_output,
        "source_fingerprint": source_fingerprint,
        "forms": manifest["form_type"].value_counts(dropna=False).to_dict(),
        "download_status": manifest["download_status"]
        .value_counts(dropna=False)
        .to_dict(),
        "extract_status": manifest["extract_status"]
        .value_counts(dropna=False)
        .to_dict(),
        "keyword_hits": int(
            pd.to_numeric(manifest["keyword_hit_count"], errors="coerce")
            .fillna(0)
            .sum()
        ),
        "exact_duplicate_windows_removed": int(
            pd.to_numeric(manifest["exact_duplicate_windows_removed"], errors="coerce")
            .fillna(0)
            .sum()
        ),
    }
    atomic_write_json(output_dir / "extraction_summary.json", summary)
    return summary


def package_data_workspace_rds(
    assembled_dir: Path,
    upload_dir: Path,
    zip_path: Path,
    *,
    expected_chunks: int,
) -> dict[str, Any]:
    """Copy only validated scorer RDS chunks into one upload-ready folder.

    The ZIP is a transport container for OneDrive and contains the RDS files at
    its root. The RDS writer already applies gzip compression, so ZIP_STORED
    avoids wasting CPU trying to compress them a second time.
    """

    if expected_chunks <= 0:
        raise ValueError("expected_chunks must be positive")
    resolved_assembled_dir = assembled_dir.resolve()
    resolved_upload_dir = upload_dir.resolve()
    resolved_zip_path = zip_path.resolve()
    if resolved_assembled_dir == resolved_upload_dir:
        raise ValueError("assembled_dir and upload_dir must be different directories")
    try:
        resolved_zip_path.relative_to(resolved_upload_dir)
    except ValueError:
        pass
    else:
        raise ValueError("zip_path must be outside the RDS-only upload directory")

    expected_names = [
        f"extract_df_chunk_{chunk_id:05d}.rds"
        for chunk_id in range(1, expected_chunks + 1)
    ]
    source_paths = [assembled_dir / name for name in expected_names]
    missing = [path.name for path in source_paths if not path.is_file()]
    if missing:
        raise FileNotFoundError(
            "Cannot create scoring-input bundle; assembled RDS files are "
            f"missing: {missing[:10]}"
        )

    upload_dir.mkdir(parents=True, exist_ok=True)
    expected_name_set = set(expected_names)
    for stale_path in upload_dir.iterdir():
        if stale_path.name in expected_name_set and stale_path.is_file():
            continue
        if stale_path.is_file() or stale_path.is_symlink():
            stale_path.unlink()
            continue
        raise RuntimeError(
            "RDS upload folder contains an unexpected directory; "
            f"remove it before packaging: {stale_path}"
        )

    copied_paths: list[Path] = []
    for source_path in source_paths:
        destination = upload_dir / source_path.name
        temp_destination = upload_dir / f".{source_path.name}.part.{os.getpid()}"
        try:
            shutil.copy2(source_path, temp_destination)
            if sha256_file(temp_destination) != sha256_file(source_path):
                raise OSError(f"Checksum mismatch while copying {source_path.name}")
            os.replace(temp_destination, destination)
        finally:
            temp_destination.unlink(missing_ok=True)
        copied_paths.append(destination)

    actual_names = {path.name for path in upload_dir.iterdir() if path.is_file()}
    if actual_names != expected_name_set:
        raise RuntimeError(
            "RDS upload folder does not contain the exact expected "
            "RDS chunk set"
        )

    zip_path.parent.mkdir(parents=True, exist_ok=True)
    temp_zip = zip_path.parent / f".{zip_path.name}.part.{os.getpid()}"
    try:
        with ZipFile(temp_zip, "w", compression=ZIP_STORED) as archive:
            for path in copied_paths:
                archive.write(path, arcname=path.name)
        with ZipFile(temp_zip) as archive:
            if set(archive.namelist()) != expected_name_set:
                raise RuntimeError("ZIP bundle does not contain the expected RDS files")
            bad_member = archive.testzip()
            if bad_member is not None:
                raise RuntimeError(f"ZIP integrity check failed for {bad_member}")
        os.replace(temp_zip, zip_path)
    finally:
        temp_zip.unlink(missing_ok=True)

    return {
        "directory": str(upload_dir),
        "zip_path": str(zip_path),
        "rds_files": len(copied_paths),
        "rds_bytes": sum(path.stat().st_size for path in copied_paths),
        "zip_bytes": zip_path.stat().st_size,
        "zip_sha256": sha256_file(zip_path),
    }


def validate_manifest_results(
    manifest: pd.DataFrame, *, max_reported_problems: int = 100
) -> dict[str, Any]:
    """Deeply validate saved per-filing JSON without requiring raw HTML."""

    problems: list[str] = []
    problem_count = 0
    checked = 0
    checked_windows = 0

    def problem(accession: str, message: str) -> None:
        nonlocal problem_count
        problem_count += 1
        if len(problems) < max_reported_problems:
            problems.append(f"{accession}: {message}")

    for _, row in manifest.iterrows():
        if str(row.get("extract_status", "")) not in VALID_EXTRACTION_STATUSES:
            continue
        accession = str(row.get("accession_number", ""))
        result_path = Path(str(row.get("result_path", "")))
        try:
            payload = json.loads(result_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as exc:
            problem(accession, f"unreadable result {result_path}: {type(exc).__name__}")
            continue
        checked += 1
        if str(payload.get("accession_number", "")) != accession:
            problem(accession, "result accession does not match manifest")
        if payload.get("extraction_version") != EXTRACTION_VERSION:
            problem(accession, "result extraction version is stale")
        if str(payload.get("source_sha256", "")) != str(row.get("download_sha256", "")):
            problem(accession, "result source hash does not match manifest")
        if int(payload.get("readable_text_chars", 0) or 0) <= 0:
            problem(accession, "result has no readable-text character count")
        windows = payload.get("windows")
        if not isinstance(windows, list):
            problem(accession, "windows is not a list")
            continue
        payload_hit_value = pd.to_numeric(
            payload.get("keyword_hit_count", 0), errors="coerce"
        )
        payload_hit_count = (
            -1 if pd.isna(payload_hit_value) else int(payload_hit_value)
        )
        manifest_hit_value = pd.to_numeric(
            row.get("keyword_hit_count", 0), errors="coerce"
        )
        manifest_hit_count = (
            -1 if pd.isna(manifest_hit_value) else int(manifest_hit_value)
        )
        if payload_hit_count != manifest_hit_count:
            problem(accession, "keyword-hit count differs between result and manifest")
        expected_value = pd.to_numeric(row.get("window_count", 0), errors="coerce")
        expected_count = 0 if pd.isna(expected_value) else int(expected_value)
        if len(windows) != expected_count:
            problem(
                accession,
                f"window count mismatch: JSON={len(windows)}, manifest={expected_count}",
            )
        status = str(row.get("extract_status", ""))
        if status == "no_keyword" and windows:
            problem(accession, "no_keyword status has saved windows")
        if status == "no_keyword" and payload_hit_count != 0:
            problem(accession, "no_keyword status has a nonzero keyword-hit count")
        if status != "no_keyword" and not windows:
            problem(accession, f"{status} status has no saved windows")
        window_ids: list[int] = []
        text_hashes: list[str] = []
        for window in windows:
            checked_windows += 1
            try:
                window_ids.append(int(window.get("window_id", 0)))
            except (TypeError, ValueError):
                problem(accession, "window has a non-integer id")
            text = str(window.get("text", ""))
            recorded_hash = str(window.get("text_sha256", ""))
            calculated_hash = sha256_text(text)
            text_hashes.append(recorded_hash)
            if recorded_hash != calculated_hash:
                problem(accession, "window text hash is invalid")
            terms = [
                term for term in str(window.get("matched_terms", "")).split("|") if term
            ]
            folded_text = text.casefold()
            if not terms or not all(term.casefold() in folded_text for term in terms):
                problem(accession, "a recorded matched term is absent from its window")
        if window_ids != list(range(1, len(windows) + 1)):
            problem(accession, "window ids are not consecutive in source order")
        if len(text_hashes) != len(set(text_hashes)):
            problem(accession, "duplicate window text hashes remain")

    return {
        "checked_filings": checked,
        "checked_windows": checked_windows,
        "problem_count": problem_count,
        "problems": problems,
        "problems_truncated": problem_count > len(problems),
        "passed": problem_count == 0,
    }


def qa_manifest(
    manifest: pd.DataFrame,
    coverage: Optional[pd.DataFrame] = None,
    result_validation: Optional[dict[str, Any]] = None,
) -> dict[str, Any]:
    duplicate_accessions = int(manifest["accession_number"].duplicated().sum())
    downloaded = manifest["download_status"].isin(
        set(DOWNLOADABLE_STATUSES)
        | {PURGED_DOWNLOAD_STATUS, PURGED_AFTER_FAILURE_STATUS}
    )
    extraction_failed = manifest["extract_status"].eq("failed")
    valid_extractions = manifest["extract_status"].isin(VALID_EXTRACTION_STATUSES)
    incomplete_extractions = ~valid_extractions
    if result_validation is None:
        result_validation = validate_manifest_results(manifest)
    raw_files_remaining = sum(
        bool(str(path).strip()) and Path(str(path)).is_file()
        for path in manifest.get("local_path", pd.Series(dtype="string"))
    )
    report = {
        "filings": len(manifest),
        "unique_accessions": int(manifest["accession_number"].nunique()),
        "duplicate_accessions": duplicate_accessions,
        "forms": manifest["form_type"].value_counts(dropna=False).to_dict(),
        "years": {
            "min": int(manifest["year"].min()),
            "max": int(manifest["year"].max()),
        },
        "downloaded_or_valid": int(downloaded.sum()),
        "raw_files_purged_after_extract": int(
            manifest["download_status"].eq(PURGED_DOWNLOAD_STATUS).sum()
        ),
        "raw_files_purged_after_extract_failure": int(
            manifest["download_status"].eq(PURGED_AFTER_FAILURE_STATUS).sum()
        ),
        "processing_batches": int(
            pd.to_numeric(
                manifest.get("processing_batch", pd.Series(dtype="float64")),
                errors="coerce",
            ).nunique()
        ),
        "download_failed": int(manifest["download_status"].eq("failed").sum()),
        "extracted": int(manifest["extract_status"].eq("extracted").sum()),
        "reused_valid_extractions": int(
            manifest["extract_status"].eq("skipped_valid").sum()
        ),
        "no_keyword": int(manifest["extract_status"].eq("no_keyword").sum()),
        "valid_extractions": int(valid_extractions.sum()),
        "incomplete_extractions": int(incomplete_extractions.sum()),
        "extract_failed": int(extraction_failed.sum()),
        "raw_files_remaining": int(raw_files_remaining),
        "downloaded_primary_document_bytes": int(
            pd.to_numeric(manifest["download_bytes"], errors="coerce").fillna(0).sum()
        ),
        "keyword_windows": int(
            pd.to_numeric(manifest["window_count"], errors="coerce").fillna(0).sum()
        ),
        "keyword_hits": int(
            pd.to_numeric(manifest["keyword_hit_count"], errors="coerce")
            .fillna(0)
            .sum()
        ),
        "result_validation": result_validation,
    }
    if coverage is not None:
        has_filing = coverage["has_matching_filing"].map(
            lambda value: str(value).strip().lower() in {"true", "1", "yes"}
        )
        report["requested_cik_years"] = len(coverage)
        report["requested_cik_years_with_filing"] = int(has_filing.sum())
        report["requested_cik_years_without_filing"] = int((~has_filing).sum())
    report["passed"] = bool(
        duplicate_accessions == 0
        and int(manifest["download_status"].eq("failed").sum()) == 0
        and int(incomplete_extractions.sum()) == 0
        and result_validation["passed"]
    )
    return report


def validate_assembled_outputs(
    manifest: pd.DataFrame, output_dir: Path, summary: dict[str, Any]
) -> dict[str, Any]:
    """Check final CSV/RDS chunk coverage and row parity."""

    expected_chunks = int(summary.get("output_chunks", 0))
    expected_stems = [
        f"extract_df_chunk_{chunk_id:05d}" for chunk_id in range(1, expected_chunks + 1)
    ]
    rds_paths = [output_dir / f"{stem}.rds" for stem in expected_stems]
    csv_paths = [output_dir / f"{stem}.csv.gz" for stem in expected_stems]
    missing_files = [str(path) for path in rds_paths + csv_paths if not path.is_file()]
    csv_rows = 0
    csv_accessions: set[str] = set()
    csv_rows_by_chunk: dict[str, int] = {}
    for path in csv_paths:
        if not path.is_file():
            continue
        frame = pd.read_csv(
            path,
            usecols=["accession_number"],
            dtype={"accession_number": "string"},
        )
        csv_rows += len(frame)
        csv_rows_by_chunk[path.stem.removesuffix(".csv")] = len(frame)
        csv_accessions.update(frame["accession_number"].dropna().astype(str))
    expected_accessions = set(manifest["accession_number"].astype(str))
    rds_rows_by_chunk: dict[str, int] = {}
    rds_error = ""
    if not missing_files and rds_paths:
        expression = (
            "args <- commandArgs(trailingOnly=TRUE); "
            "for (path in args) cat(basename(path), nrow(readRDS(path)), '\\n')"
        )
        try:
            completed = subprocess.run(
                ["Rscript", "-e", expression, *map(str, rds_paths)],
                check=True,
                capture_output=True,
                text=True,
            )
            for line in completed.stdout.splitlines():
                filename, row_count = line.strip().rsplit(None, 1)
                rds_rows_by_chunk[Path(filename).stem] = int(row_count)
        except (OSError, subprocess.CalledProcessError, ValueError) as exc:
            rds_error = f"{type(exc).__name__}: {exc}"
    csv_chunk_rows_normalized = {
        stem: csv_rows_by_chunk.get(stem, -1) for stem in expected_stems
    }
    rds_chunk_rows_normalized = {
        stem: rds_rows_by_chunk.get(stem, -1) for stem in expected_stems
    }
    checks = {
        "expected_chunk_files_exist": not missing_files,
        "csv_total_rows_match_summary": csv_rows
        == int(summary.get("scorer_input_rows", -1)),
        "csv_accessions_match_manifest": csv_accessions == expected_accessions,
        "rds_readable": not rds_error and len(rds_rows_by_chunk) == expected_chunks,
        "rds_csv_row_parity": rds_chunk_rows_normalized == csv_chunk_rows_normalized,
    }
    return {
        "output_chunks": expected_chunks,
        "csv_rows": csv_rows,
        "unique_accessions": len(csv_accessions),
        "missing_files": missing_files,
        "rds_error": rds_error,
        "csv_rows_by_chunk": csv_chunk_rows_normalized,
        "rds_rows_by_chunk": rds_chunk_rows_normalized,
        "checks": checks,
        "passed": all(checks.values()),
    }


def require_complete_extractions(manifest: pd.DataFrame) -> dict[str, Any]:
    """Prevent a full run from assembling a silently incomplete corpus."""

    valid = manifest["extract_status"].isin(VALID_EXTRACTION_STATUSES)
    if not bool(valid.all()):
        incomplete = (
            manifest.loc[~valid, "extract_status"].value_counts(dropna=False).to_dict()
        )
        raise RuntimeError(
            "EDGAR extraction is incomplete; rerun before assembly. "
            f"Incomplete statuses: {incomplete}"
        )
    validation = validate_manifest_results(manifest)
    if not validation["passed"]:
        raise RuntimeError(
            "EDGAR extraction result validation failed; refusing assembly. "
            f"First problems: {validation['problems'][:5]}"
        )
    return validation


def add_path_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--lookup-csv",
        type=Path,
        default=PROJECT_ROOT / "code/main/2. get_ai_adoption/lookup/cik_year.csv",
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=PROJECT_ROOT / "cache/edgar_keyword_windows",
    )
    parser.add_argument("--manifest", type=Path, default=None)
    parser.add_argument("--submissions-zip", type=Path, default=None)


def add_sec_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--email", required=True, help="SEC User-Agent contact email")
    parser.add_argument("--company", required=True, help="SEC User-Agent organization")
    parser.add_argument(
        "--rate-per-second", type=float, default=DEFAULT_RATE_PER_SECOND
    )
    parser.add_argument("--retries", type=int, default=4)
    parser.add_argument("--timeout-seconds", type=float, default=90.0)


def add_sample_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--sample-filings",
        type=int,
        default=None,
        help="Run a deterministic form/year-stratified sample of this many filings",
    )
    parser.add_argument(
        "--sample-seed",
        default=DEFAULT_SAMPLE_SEED,
        help="Stable seed used to choose --sample-filings",
    )


def add_data_workspace_bundle_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--data-workspace-dir",
        type=Path,
        default=None,
        help=(
            "Upload-ready directory containing only final RDS chunks; defaults "
            f"to DATA_DIR/{DEFAULT_DATA_WORKSPACE_DIRNAME}"
        ),
    )
    parser.add_argument(
        "--data-workspace-zip",
        type=Path,
        default=None,
        help=(
            "ZIP transport bundle for OneDrive; defaults to "
            f"DATA_DIR/{DEFAULT_DATA_WORKSPACE_DIRNAME}.zip"
        ),
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--log-level", choices=["DEBUG", "INFO", "WARNING", "ERROR"], default="INFO"
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    manifest_parser = subparsers.add_parser("manifest", help="Build filing manifest")
    add_path_arguments(manifest_parser)
    add_sec_arguments(manifest_parser)
    add_sample_arguments(manifest_parser)
    manifest_parser.add_argument(
        "--year-match", choices=["filing-date", "report-date"], default="filing-date"
    )
    manifest_parser.add_argument("--refresh-metadata", action="store_true")
    manifest_parser.add_argument(
        "--no-complete-submission-fallback", action="store_true"
    )

    download_parser = subparsers.add_parser("download", help="Download primary filings")
    add_path_arguments(download_parser)
    add_sec_arguments(download_parser)
    download_parser.add_argument(
        "--workers", type=int, default=DEFAULT_DOWNLOAD_WORKERS
    )
    download_parser.add_argument("--min-bytes", type=int, default=200)

    extract_parser = subparsers.add_parser("extract", help="Extract keyword windows")
    add_path_arguments(extract_parser)
    extract_parser.add_argument(
        "--context-sentences", type=int, default=DEFAULT_CONTEXT_SENTENCES
    )
    extract_parser.add_argument("--workers", type=int, default=DEFAULT_EXTRACT_WORKERS)
    extract_parser.add_argument("--overwrite-extractions", action="store_true")

    assemble_parser = subparsers.add_parser(
        "assemble", help="Write compatible RDS chunks"
    )
    add_path_arguments(assemble_parser)
    assemble_parser.add_argument(
        "--filings-per-output", type=int, default=DEFAULT_FILINGS_PER_OUTPUT
    )
    assemble_parser.add_argument("--output-dir", type=Path, default=None)
    assemble_parser.add_argument("--overwrite-chunks", action="store_true")
    add_data_workspace_bundle_arguments(assemble_parser)

    package_parser = subparsers.add_parser(
        "package", help="Build the upload-ready RDS folder and ZIP"
    )
    add_path_arguments(package_parser)
    package_parser.add_argument(
        "--assembled-dir",
        type=Path,
        default=None,
        help="Assembled chunk directory; defaults to DATA_DIR/assembled",
    )
    add_data_workspace_bundle_arguments(package_parser)

    qa_parser = subparsers.add_parser("qa", help="Summarize manifest QA")
    add_path_arguments(qa_parser)

    run_parser = subparsers.add_parser("run", help="Run all extraction stages")
    add_path_arguments(run_parser)
    add_sec_arguments(run_parser)
    add_sample_arguments(run_parser)
    run_parser.add_argument(
        "--year-match", choices=["filing-date", "report-date"], default="filing-date"
    )
    run_parser.add_argument("--refresh-metadata", action="store_true")
    run_parser.add_argument("--no-complete-submission-fallback", action="store_true")
    run_parser.add_argument(
        "--download-workers", type=int, default=DEFAULT_DOWNLOAD_WORKERS
    )
    run_parser.add_argument(
        "--extract-workers", type=int, default=DEFAULT_EXTRACT_WORKERS
    )
    run_parser.add_argument(
        "--raw-batch-gib",
        type=float,
        default=DEFAULT_RAW_BATCH_GIB,
        help=(
            "Approximate maximum GiB of temporary primary documents per batch; "
            "the default is 35"
        ),
    )
    run_parser.add_argument(
        "--keep-raw",
        action="store_true",
        help="Retain primary documents after extraction (requires full-corpus storage)",
    )
    run_parser.add_argument("--min-bytes", type=int, default=200)
    run_parser.add_argument(
        "--context-sentences", type=int, default=DEFAULT_CONTEXT_SENTENCES
    )
    run_parser.add_argument("--overwrite-extractions", action="store_true")
    run_parser.add_argument(
        "--filings-per-output", type=int, default=DEFAULT_FILINGS_PER_OUTPUT
    )
    run_parser.add_argument("--output-dir", type=Path, default=None)
    run_parser.add_argument("--overwrite-chunks", action="store_true")
    add_data_workspace_bundle_arguments(run_parser)
    return parser


def resolved_paths(args: argparse.Namespace) -> tuple[Path, Path, Path, Path]:
    data_dir = args.data_dir.expanduser().resolve()
    manifest_path = (
        args.manifest.expanduser().resolve()
        if args.manifest
        else data_dir / MANIFEST_NAME
    )
    submissions_zip = (
        args.submissions_zip.expanduser().resolve()
        if args.submissions_zip
        else data_dir / "sec" / "submissions.zip"
    )
    lookup_path = args.lookup_csv.expanduser().resolve()
    return data_dir, manifest_path, submissions_zip, lookup_path


def resolved_data_workspace_bundle_paths(
    args: argparse.Namespace, data_dir: Path
) -> tuple[Path, Path]:
    upload_dir = (
        args.data_workspace_dir.expanduser().resolve()
        if args.data_workspace_dir
        else data_dir / DEFAULT_DATA_WORKSPACE_DIRNAME
    )
    zip_path = (
        args.data_workspace_zip.expanduser().resolve()
        if args.data_workspace_zip
        else data_dir / f"{DEFAULT_DATA_WORKSPACE_DIRNAME}.zip"
    )
    return upload_dir, zip_path


def preflight_full_run(args: argparse.Namespace, data_dir: Path) -> dict[str, Any]:
    """Fail early when a long extraction would later hit a local setup error."""

    problems: list[str] = []
    if lxml_html is None:
        problems.append(
            "lxml is unavailable; activate .venv-edgar and install requirements "
            "before the full run"
        )

    rscript = shutil.which("Rscript")
    if not rscript:
        problems.append("Rscript is unavailable; final RDS assembly cannot run")
    else:
        check = subprocess.run(
            [
                rscript,
                "-e",
                (
                    "quit(status=if (requireNamespace('data.table', "
                    "quietly=TRUE)) 0 else 1)"
                ),
            ],
            capture_output=True,
            text=True,
        )
        if check.returncode != 0:
            problems.append("R package data.table is unavailable")

    assembled_dir = (
        args.output_dir.expanduser().resolve()
        if args.output_dir
        else data_dir / "assembled"
    )
    upload_dir, zip_path = resolved_data_workspace_bundle_paths(args, data_dir)
    if assembled_dir == upload_dir:
        problems.append(
            "assembled output and RDS upload directories must differ"
        )
    try:
        zip_path.relative_to(upload_dir)
    except ValueError:
        pass
    else:
        problems.append("the transfer ZIP must be outside its RDS-only folder")

    disk = shutil.disk_usage(data_dir)
    raw_batch_bytes = int(args.raw_batch_gib * GIB)
    reserve_bytes = 10 * GIB
    if args.sample_filings is None and disk.free < raw_batch_bytes + reserve_bytes:
        problems.append(
            "insufficient free disk for the configured raw batch plus 10 GiB "
            f"reserve: free={disk.free / GIB:.1f} GiB, "
            f"required={(raw_batch_bytes + reserve_bytes) / GIB:.1f} GiB"
        )

    if problems:
        raise RuntimeError("EDGAR full-run preflight failed: " + "; ".join(problems))

    report = {
        "python": sys.version.split()[0],
        "lxml_available": True,
        "rscript": str(rscript),
        "data_table_available": True,
        "free_disk_gib": round(disk.free / GIB, 1),
        "raw_batch_gib": float(args.raw_batch_gib),
        "reserve_gib": 10,
        "assembled_dir": str(assembled_dir),
        "data_workspace_dir": str(upload_dir),
        "data_workspace_zip": str(zip_path),
    }
    logging.info("Full-run preflight passed: %s", json.dumps(report, sort_keys=True))
    return report


def require_positive_args(args: argparse.Namespace) -> None:
    for name in (
        "rate_per_second",
        "timeout_seconds",
        "workers",
        "download_workers",
        "extract_workers",
        "filings_per_output",
        "raw_batch_gib",
    ):
        if hasattr(args, name) and getattr(args, name) <= 0:
            raise ValueError(f"--{name.replace('_', '-')} must be positive")
    if hasattr(args, "context_sentences") and args.context_sentences < 0:
        raise ValueError("--context-sentences must be non-negative")
    if hasattr(args, "sample_filings") and args.sample_filings is not None:
        if args.sample_filings <= 0:
            raise ValueError("--sample-filings must be positive")
    if hasattr(args, "retries") and args.retries < 0:
        raise ValueError("--retries must be non-negative")
    if hasattr(args, "rate_per_second") and args.rate_per_second > 10:
        raise ValueError("--rate-per-second must not exceed the SEC limit of 10")
    if hasattr(args, "company"):
        if not args.company.strip() or not args.email.strip():
            raise ValueError("--company and --email must contain non-whitespace text")
        if "@" not in args.email or "." not in args.email.rsplit("@", 1)[-1]:
            raise ValueError("--email must be a valid SEC contact email address")


def ensure_bulk_archive(
    submissions_zip: Path, args: argparse.Namespace, user_agent: str
) -> None:
    if args.refresh_metadata and submissions_zip.exists():
        submissions_zip.unlink()
    valid, _ = validate_download_file(submissions_zip, min_bytes=1024 * 1024)
    if valid:
        try:
            with ZipFile(submissions_zip) as archive:
                if archive.infolist():
                    return
            submissions_zip.unlink()
        except BadZipFile:
            submissions_zip.unlink()
    download_bulk_submissions(
        submissions_zip,
        user_agent=user_agent,
        rate_per_second=args.rate_per_second,
        retries=args.retries,
        timeout_seconds=args.timeout_seconds,
    )


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s %(levelname)s %(message)s",
    )
    try:
        require_positive_args(args)
        data_dir, manifest_path, submissions_zip, lookup_path = resolved_paths(args)
        data_dir.mkdir(parents=True, exist_ok=True)
        user_agent = ""
        result_validation: Optional[dict[str, Any]] = None
        if hasattr(args, "company"):
            user_agent = f"{args.company.strip()} {args.email.strip()}"

        if args.command == "run":
            preflight_full_run(args, data_dir)

        if args.command in {"manifest", "run"}:
            ensure_bulk_archive(submissions_zip, args, user_agent)
            lookup = load_lookup(lookup_path)
            metadata_limiter = RateLimiter(args.rate_per_second)
            full_manifest = build_manifest(
                lookup,
                submissions_zip,
                data_dir,
                year_match=args.year_match,
                allow_complete_submission_fallback=not args.no_complete_submission_fallback,
                historical_page_loader=lambda page_name: load_sec_historical_page(
                    page_name,
                    cache_dir=data_dir / "sec" / "historical_pages",
                    user_agent=user_agent,
                    limiter=metadata_limiter,
                    retries=args.retries,
                    timeout_seconds=args.timeout_seconds,
                ),
            )
            coverage_lookup = lookup
            manifest = full_manifest
            if args.sample_filings is not None:
                full_manifest_archive = data_dir / "sec" / "full_filing_manifest.csv.gz"
                atomic_write_dataframe_gzip(full_manifest, full_manifest_archive)
                manifest, sample_audit = select_manifest_sample(
                    full_manifest,
                    args.sample_filings,
                    seed=args.sample_seed,
                )
                sample_lookup = manifest[["cik", "year"]].drop_duplicates()
                sample_lookup = sample_lookup.sort_values(["cik", "year"])
                atomic_write_dataframe(sample_lookup, data_dir / "sample_lookup.csv")
                coverage_lookup = sample_lookup
                sample_audit["created_at"] = utc_now()
                sample_audit["full_manifest_archive"] = str(full_manifest_archive)
                atomic_write_json(data_dir / "sample_selection.json", sample_audit)
            if manifest_path.exists():
                manifest = merge_manifest_progress(
                    manifest, load_manifest(manifest_path)
                )
            atomic_write_dataframe(manifest, manifest_path)
            coverage = metadata_coverage(coverage_lookup, manifest)
            atomic_write_dataframe(coverage, data_dir / "sec" / COVERAGE_NAME)
            logging.info("Wrote manifest %s (%d filings)", manifest_path, len(manifest))

        if args.command == "download":
            manifest = load_manifest(manifest_path)
            manifest = download_manifest_filings(
                manifest,
                manifest_path,
                user_agent=user_agent,
                rate_per_second=args.rate_per_second,
                workers=args.workers,
                retries=args.retries,
                timeout_seconds=args.timeout_seconds,
                min_bytes=args.min_bytes,
            )

        if args.command == "extract":
            manifest = load_manifest(manifest_path)
            manifest = extract_manifest_filings(
                manifest,
                manifest_path,
                context_sentences=args.context_sentences,
                workers=args.workers,
                overwrite=args.overwrite_extractions,
            )

        if args.command == "run":
            manifest = load_manifest(manifest_path)
            manifest = process_manifest_in_storage_batches(
                manifest,
                manifest_path,
                data_dir=data_dir,
                user_agent=user_agent,
                rate_per_second=args.rate_per_second,
                download_workers=args.download_workers,
                extract_workers=args.extract_workers,
                retries=args.retries,
                timeout_seconds=args.timeout_seconds,
                min_bytes=args.min_bytes,
                context_sentences=args.context_sentences,
                overwrite_extractions=args.overwrite_extractions,
                max_batch_bytes=int(args.raw_batch_gib * GIB),
                keep_raw=args.keep_raw,
            )
            result_validation = require_complete_extractions(manifest)

        if args.command in {"assemble", "run"}:
            manifest = load_manifest(manifest_path)
            if result_validation is None:
                result_validation = require_complete_extractions(manifest)
            output_dir = (
                args.output_dir.expanduser().resolve()
                if args.output_dir
                else data_dir / "assembled"
            )
            summary = assemble_compatible_chunks(
                manifest,
                output_dir,
                filings_per_output=args.filings_per_output,
                overwrite=args.overwrite_chunks,
            )
            assembly_validation = validate_assembled_outputs(
                manifest, output_dir, summary
            )
            summary["assembly_validation"] = assembly_validation
            atomic_write_json(output_dir / "extraction_summary.json", summary)
            print(json.dumps(summary, indent=2, default=str))
            if not assembly_validation["passed"]:
                raise RuntimeError(
                    "Assembled EDGAR outputs failed validation; inspect "
                    f"{output_dir / 'extraction_summary.json'}"
                )
            upload_dir, zip_path = resolved_data_workspace_bundle_paths(args, data_dir)
            summary["data_workspace_bundle"] = package_data_workspace_rds(
                output_dir,
                upload_dir,
                zip_path,
                expected_chunks=int(summary["output_chunks"]),
            )
            atomic_write_json(output_dir / "extraction_summary.json", summary)
            print(
                json.dumps(
                    {"data_workspace_bundle": summary["data_workspace_bundle"]},
                    indent=2,
                )
            )

        if args.command == "package":
            manifest = load_manifest(manifest_path)
            require_complete_extractions(manifest)
            assembled_dir = (
                args.assembled_dir.expanduser().resolve()
                if args.assembled_dir
                else data_dir / "assembled"
            )
            summary_path = assembled_dir / "extraction_summary.json"
            if not summary_path.is_file():
                raise FileNotFoundError(
                    f"Missing assembled extraction summary: {summary_path}"
                )
            summary = json.loads(summary_path.read_text(encoding="utf-8"))
            if summary.get("assembly_version") != ASSEMBLY_VERSION:
                raise RuntimeError(
                    "Assembled chunks use an older output format; rebuild them "
                    "with the assemble command and --overwrite-chunks"
                )
            assembly_validation = validate_assembled_outputs(
                manifest, assembled_dir, summary
            )
            if not assembly_validation["passed"]:
                raise RuntimeError(
                    "Assembled EDGAR outputs failed validation; refusing to "
                    "create a transfer bundle"
                )
            upload_dir, zip_path = resolved_data_workspace_bundle_paths(args, data_dir)
            bundle = package_data_workspace_rds(
                assembled_dir,
                upload_dir,
                zip_path,
                expected_chunks=int(summary["output_chunks"]),
            )
            summary["assembly_validation"] = assembly_validation
            summary["data_workspace_bundle"] = bundle
            atomic_write_json(summary_path, summary)
            print(json.dumps({"data_workspace_bundle": bundle}, indent=2))

        if args.command in {"qa", "run"}:
            manifest = load_manifest(manifest_path)
            coverage_path = data_dir / "sec" / COVERAGE_NAME
            coverage = (
                pd.read_csv(coverage_path, dtype={"cik": "string"})
                if coverage_path.exists()
                else None
            )
            report = qa_manifest(
                manifest, coverage, result_validation=result_validation
            )
            report_path = data_dir / "qa_summary.json"
            atomic_write_json(report_path, report)
            print(json.dumps(report, indent=2, default=str))
            if not report["passed"]:
                raise RuntimeError(
                    f"EDGAR QA failed; inspect the saved report at {report_path}"
                )
        return 0
    except Exception as exc:
        logging.error("%s: %s", type(exc).__name__, exc)
        if args.log_level == "DEBUG":
            logging.exception("Extraction pipeline failed")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
