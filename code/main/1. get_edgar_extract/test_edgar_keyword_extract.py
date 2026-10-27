from __future__ import annotations

import json
import sys
import tempfile
import unittest
from collections import deque
from pathlib import Path
from unittest.mock import patch
from zipfile import ZipFile

import pandas as pd


THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

import edgar_keyword_extract as e


class EdgarKeywordExtractTests(unittest.TestCase):
    def test_atomic_json_write_removes_temp_file_on_serialization_failure(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            destination = root / "result.json"

            with self.assertRaises(TypeError):
                e.atomic_write_json(destination, {"not_json": object()})

            self.assertFalse(destination.exists())
            self.assertEqual(list(root.iterdir()), [])

    def test_manifest_uses_project_filing_year_and_keeps_amendment(self) -> None:
        recent = {
            "accessionNumber": ["0000001001-25-000001", "0000001001-25-000002", "x"],
            "form": ["10-K", "10-K/A", "8-K"],
            "filingDate": ["2025-02-15", "2025-03-01", "2025-03-02"],
            "reportDate": ["2024-12-31", "2024-12-31", "2024-12-31"],
            "primaryDocument": ["annual.htm", "amendment.htm", "event.htm"],
        }
        historical = {
            "accessionNumber": ["0000001001-24-000001"],
            "form": ["10-K"],
            "filingDate": ["2024-02-15"],
            "reportDate": ["2023-12-31"],
            "primaryDocument": ["annual-2023.htm"],
        }
        company = {
            "name": "Example Corp",
            "filings": {
                "recent": recent,
                "files": [{"name": "CIK0000001001-submissions-001.json"}],
            },
        }

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            archive_path = root / "submissions.zip"
            with ZipFile(archive_path, "w") as archive:
                archive.writestr("CIK0000001001.json", json.dumps(company))
                archive.writestr(
                    "CIK0000001001-submissions-001.json", json.dumps(historical)
                )
            lookup = pd.DataFrame(
                [{"cik": "1001", "year": 2024}, {"cik": "1001", "year": 2025}]
            )

            manifest = e.build_manifest(
                lookup, archive_path, root, year_match="filing-date"
            )

        self.assertEqual(len(manifest), 3)
        self.assertEqual(set(manifest["form_type"]), {"10-K", "10-K/A"})
        self.assertEqual(set(manifest["year"]), {2024, 2025})
        row = manifest.loc[manifest["accession_number"] == "0000001001-25-000001"].iloc[
            0
        ]
        self.assertEqual(int(row["year"]), 2025)
        self.assertEqual(int(row["filing_year"]), 2025)
        self.assertEqual(row["document_kind"], "primary_document")
        self.assertTrue(str(row["source_url"]).endswith("/annual.htm"))

    def test_readable_text_removes_non_visible_markup(self) -> None:
        document = b"""
        <html><head><style>.x {display:none}</style><script>machine learning</script></head>
        <body>
          <ix:header>hidden artificial intelligence metadata</ix:header>
          <div style="display:none">hidden AI text</div>
          <p>We use artificial intelligence in customer support.</p>
          <table><tr><td>Machine learning</td><td>operations</td></tr></table>
        </body></html>
        """

        text = e.readable_text_from_document(document)

        self.assertIn("We use artificial intelligence", text)
        self.assertIn("Machine learning", text)
        self.assertNotIn("hidden artificial", text)
        self.assertNotIn("hidden AI", text)

    def test_metadata_coverage_keeps_unmatched_research_keys_visible(self) -> None:
        lookup = pd.DataFrame(
            [
                {"cik": "1001", "year": 2024},
                {"cik": "1001", "year": 2025},
            ]
        )
        manifest = pd.DataFrame(
            [
                {
                    "cik": "1001",
                    "year": 2025,
                    "form_type": "10-K",
                },
                {
                    "cik": "1001",
                    "year": 2025,
                    "form_type": "10-K/A",
                },
            ]
        )

        coverage = e.metadata_coverage(lookup, manifest)

        self.assertEqual(len(coverage), 2)
        self.assertFalse(
            bool(coverage.loc[coverage["year"] == 2024, "has_matching_filing"].iloc[0])
        )
        row = coverage.loc[coverage["year"] == 2025].iloc[0]
        self.assertEqual(int(row["form_10k_count"]), 1)
        self.assertEqual(int(row["form_10ka_count"]), 1)

    def test_windows_merge_overlap_and_adjacency_but_keep_separate_regions(
        self,
    ) -> None:
        text = (
            "Introduction. "
            "We discuss artificial intelligence. "
            "First context. "
            "We use machine learning in production. "
            "Second context. "
            "A separating sentence. "
            "Another ordinary sentence. "
            "We deploy NLP for customer requests. "
            "Closing sentence."
        )
        metadata = {
            "year": 2024,
            "accession_number": "a1",
            "cik": "1001",
            "form_type": "10-K",
        }

        windows, metrics = e.extract_keyword_windows(
            text, context_sentences=1, filing_metadata=metadata
        )

        self.assertEqual(metrics["keyword_hit_count"], 3)
        self.assertEqual(len(windows), 2)
        self.assertIn("artificial intelligence", windows[0]["text"])
        self.assertIn("machine learning", windows[0]["text"])
        self.assertIn("NLP", windows[1]["text"])
        self.assertEqual([row["window_id"] for row in windows], [1, 2])

    def test_only_exact_normalized_duplicate_windows_are_removed(self) -> None:
        text = (
            "We use machine learning. "
            "An unrelated separating sentence. "
            "We use machine learning."
        )
        metadata = {
            "year": 2024,
            "accession_number": "a1",
            "cik": "1001",
            "form_type": "10-K",
        }

        windows, metrics = e.extract_keyword_windows(
            text, context_sentences=0, filing_metadata=metadata
        )

        self.assertEqual(len(windows), 1)
        self.assertEqual(metrics["exact_duplicate_windows_removed"], 1)
        self.assertEqual(windows[0]["duplicate_occurrences"], 2)

    def test_retrieval_does_not_apply_adoption_disambiguation(self) -> None:
        text = "Claude Bernard is named here. AI = American Indians."
        metadata = {
            "year": 2024,
            "accession_number": "a1",
            "cik": "1001",
            "form_type": "10-K",
        }

        windows, _ = e.extract_keyword_windows(
            text, context_sentences=0, filing_metadata=metadata
        )

        terms = "|".join(row["keyword_names"] for row in windows)
        self.assertIn("claude", terms)
        self.assertIn("ai", terms)

    def test_changed_context_invalidates_cached_extraction(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            result_path = Path(tmp) / "result.json"
            result_path.write_text(
                json.dumps(
                    {
                        "accession_number": "a1",
                        "source_sha256": "abc",
                        "extraction_version": e.EXTRACTION_VERSION,
                        "context_sentences": 1,
                        "readable_text_chars": 25,
                        "keyword_hit_count": 0,
                        "windows": [],
                    }
                ),
                encoding="utf-8",
            )

            self.assertIsNotNone(e._read_existing_extraction(result_path, "abc", 1))
            self.assertIsNone(e._read_existing_extraction(result_path, "abc", 2))

    def test_manifest_refresh_preserves_progress_for_same_source(self) -> None:
        fresh = pd.DataFrame(
            [
                {
                    "accession_number": "a1",
                    "source_url": "https://www.sec.gov/a1.htm",
                    "download_status": "pending",
                    "download_sha256": "",
                    "extract_status": "pending",
                }
            ]
        )
        previous = pd.DataFrame(
            [
                {
                    "accession_number": "a1",
                    "source_url": "https://www.sec.gov/a1.htm",
                    "download_status": e.PURGED_DOWNLOAD_STATUS,
                    "download_sha256": "abc",
                    "extract_status": "extracted",
                    "window_count": 3,
                }
            ]
        )

        merged = e.merge_manifest_progress(fresh, previous)

        self.assertEqual(merged.loc[0, "download_status"], e.PURGED_DOWNLOAD_STATUS)
        self.assertEqual(merged.loc[0, "download_sha256"], "abc")
        self.assertEqual(int(merged.loc[0, "window_count"]), 3)

    def test_manifest_sample_is_deterministic_and_stratified(self) -> None:
        rows = []
        accession = 0
        for form_type, count in (("10-K", 100), ("10-K/A", 20)):
            for position in range(count):
                accession += 1
                rows.append(
                    {
                        "accession_number": f"a{accession:04d}",
                        "cik": str(100000 + accession),
                        "year": 2020 + (position % 4),
                        "form_type": form_type,
                        "metadata_status": "selected",
                    }
                )
        manifest = pd.DataFrame(rows)

        first, audit = e.select_manifest_sample(manifest, 30, seed="test-seed")
        second, _ = e.select_manifest_sample(manifest, 30, seed="test-seed")

        self.assertEqual(
            first["accession_number"].tolist(), second["accession_number"].tolist()
        )
        self.assertEqual(len(first), 30)
        self.assertEqual(first["cik"].nunique(), 30)
        self.assertEqual(
            first["form_type"].value_counts().to_dict(), {"10-K": 25, "10-K/A": 5}
        )
        self.assertEqual(set(first["year"]), {2020, 2021, 2022, 2023})
        self.assertEqual(audit["sample_filings"], 30)

    def test_result_validation_detects_tampered_window_text(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            result_path = root / "result.json"
            text = "We use machine learning."
            result_path.write_text(
                json.dumps(
                    {
                        "accession_number": "a1",
                        "source_sha256": "source-hash",
                        "extraction_version": e.EXTRACTION_VERSION,
                        "context_sentences": 1,
                        "readable_text_chars": len(text),
                        "windows": [
                            {
                                "window_id": 1,
                                "text": text,
                                "text_sha256": "tampered-hash",
                                "matched_terms": "machine learning",
                            }
                        ],
                    }
                ),
                encoding="utf-8",
            )
            manifest = pd.DataFrame(
                [
                    {
                        "accession_number": "a1",
                        "download_sha256": "source-hash",
                        "extract_status": "extracted",
                        "window_count": 1,
                        "result_path": str(result_path),
                    }
                ]
            )

            validation = e.validate_manifest_results(manifest)
            reusable = e._read_existing_extraction(
                result_path, "source-hash", 1, accession_number="a1"
            )

        self.assertFalse(validation["passed"])
        self.assertIsNone(reusable)
        self.assertTrue(
            any("text hash" in problem for problem in validation["problems"])
        )

    def test_assembled_validation_checks_csv_rds_parity_and_coverage(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            output_dir = root / "assembled"
            output_dir.mkdir()
            frame = pd.DataFrame(
                [
                    {
                        "item": "keyword_window",
                        "year": 2025,
                        "accession_number": "a1",
                        "cik": "1001",
                        "form_type": "10-K",
                        "text": "machine learning",
                    }
                ]
            )
            csv_path = output_dir / "extract_df_chunk_00001.csv.gz"
            rds_path = output_dir / "extract_df_chunk_00001.rds"
            frame.to_csv(csv_path, index=False, compression="gzip")
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as handle:
                temp_csv = Path(handle.name)
            try:
                frame.to_csv(temp_csv, index=False)
                e.write_rds_from_csv(temp_csv, rds_path)
            finally:
                temp_csv.unlink(missing_ok=True)
            self.assertEqual(rds_path.read_bytes()[:2], b"\x1f\x8b")
            manifest = pd.DataFrame([{"accession_number": "a1"}])

            validation = e.validate_assembled_outputs(
                manifest,
                output_dir,
                {"output_chunks": 1, "scorer_input_rows": 1},
            )

        self.assertTrue(validation["passed"])
        self.assertEqual(validation["rds_rows_by_chunk"], {"extract_df_chunk_00001": 1})

    def test_data_workspace_bundle_contains_only_expected_rds_chunks(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            assembled_dir = root / "assembled"
            upload_dir = root / "data_workspace_rds"
            zip_path = root / "data_workspace_rds.zip"
            assembled_dir.mkdir()
            upload_dir.mkdir()
            expected = {
                "extract_df_chunk_00001.rds": b"first-rds",
                "extract_df_chunk_00002.rds": b"second-rds",
            }
            for name, content in expected.items():
                (assembled_dir / name).write_bytes(content)
            (assembled_dir / "extract_df_chunk_00001.csv.gz").write_bytes(b"csv")
            (upload_dir / "extract_df_chunk_99999.rds").write_bytes(b"stale")
            (upload_dir / ".DS_Store").write_bytes(b"metadata")
            (upload_dir / "do-not-upload.csv").write_bytes(b"csv")

            bundle = e.package_data_workspace_rds(
                assembled_dir,
                upload_dir,
                zip_path,
                expected_chunks=2,
            )

            self.assertEqual(
                {path.name for path in upload_dir.iterdir()}, set(expected)
            )
            self.assertEqual(int(bundle["rds_files"]), 2)
            self.assertTrue(zip_path.is_file())
            with ZipFile(zip_path) as archive:
                self.assertEqual(set(archive.namelist()), set(expected))
                self.assertIsNone(archive.testzip())
                for name, content in expected.items():
                    self.assertEqual(archive.read(name), content)

    def test_invalid_existing_download_is_removed_after_failed_retry(self) -> None:
        class FailedSession:
            def get(self, *_args, **_kwargs):
                raise RuntimeError("offline")

        with tempfile.TemporaryDirectory() as tmp:
            destination = Path(tmp) / "filing.htm"
            destination.write_bytes(b"invalid")
            with patch.object(e, "thread_session", return_value=FailedSession()):
                status, _, size, digest = e.download_to_path(
                    "https://www.sec.gov/example.htm",
                    destination,
                    user_agent="Example test@example.com",
                    limiter=e.RateLimiter(1000),
                    retries=0,
                    timeout_seconds=1,
                    min_bytes=200,
                )

            self.assertEqual(status, "failed")
            self.assertEqual(size, 0)
            self.assertEqual(digest, "")
            self.assertFalse(destination.exists())

    def test_download_batch_stops_at_byte_budget(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rows = []
            for index in range(4):
                rows.append(
                    {
                        "accession_number": f"a{index}",
                        "metadata_status": "selected",
                        "source_url": f"https://www.sec.gov/a{index}.htm",
                        "local_path": str(root / f"a{index}.htm"),
                        "download_status": "pending",
                    }
                )
            manifest = pd.DataFrame(rows)
            manifest_path = root / "manifest.csv"
            pending = deque(manifest.index)

            def fake_download(row, **_kwargs):
                path = Path(row["local_path"])
                path.write_bytes(b"x" * 60)
                return {
                    "index": int(row["_index"]),
                    "download_status": "downloaded",
                    "download_error": "",
                    "download_bytes": 60,
                    "download_sha256": e.sha256_file(path),
                }

            with patch.object(e, "_download_manifest_row", side_effect=fake_download):
                result, batch_indices, batch_bytes = e.download_next_storage_batch(
                    manifest,
                    manifest_path,
                    pending,
                    batch_id=1,
                    max_batch_bytes=100,
                    user_agent="Example test@example.com",
                    rate_per_second=8,
                    workers=1,
                    retries=0,
                    timeout_seconds=1,
                    min_bytes=1,
                )

        self.assertEqual(batch_indices, [0, 1])
        self.assertEqual(batch_bytes, 120)
        self.assertEqual(list(pending), [2, 3])
        self.assertEqual(set(result.loc[batch_indices, "processing_batch"]), {1})

    def test_purge_removes_only_files_with_valid_results(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            raw_root = root / "raw"
            raw_root.mkdir()
            valid_raw = raw_root / "valid.htm"
            invalid_raw = raw_root / "invalid.htm"
            valid_raw.write_text("machine learning", encoding="utf-8")
            invalid_raw.write_text("artificial intelligence", encoding="utf-8")
            valid_sha = e.sha256_file(valid_raw)
            valid_result = root / "valid.json"
            valid_result.write_text(
                json.dumps(
                    {
                        "accession_number": "valid",
                        "source_sha256": valid_sha,
                        "extraction_version": e.EXTRACTION_VERSION,
                        "context_sentences": 1,
                        "readable_text_chars": len("machine learning"),
                        "keyword_hit_count": 1,
                        "exact_duplicate_windows_removed": 0,
                        "windows": [
                            {
                                "window_id": 1,
                                "text": "machine learning",
                                "text_sha256": e.sha256_text("machine learning"),
                                "matched_terms": "machine learning",
                            }
                        ],
                    }
                ),
                encoding="utf-8",
            )
            manifest = pd.DataFrame(
                [
                    {
                        "accession_number": "valid",
                        "local_path": str(valid_raw),
                        "result_path": str(valid_result),
                        "download_sha256": valid_sha,
                        "download_status": "downloaded",
                        "extract_status": "extracted",
                    },
                    {
                        "accession_number": "invalid",
                        "local_path": str(invalid_raw),
                        "result_path": str(root / "missing.json"),
                        "download_sha256": e.sha256_file(invalid_raw),
                        "download_status": "downloaded",
                    },
                ]
            )
            manifest_path = root / "manifest.csv"

            result, purged, bytes_freed = e.purge_manifest_raw_files(
                manifest,
                manifest_path,
                manifest.index,
                raw_root=raw_root,
                context_sentences=1,
            )

            self.assertFalse(valid_raw.exists())
            self.assertTrue(invalid_raw.exists())
            self.assertEqual(purged, 1)
            self.assertGreater(bytes_freed, 0)
            self.assertEqual(result.loc[0, "download_status"], e.PURGED_DOWNLOAD_STATUS)
            self.assertEqual(result.loc[0, "extract_status"], "extracted")
            self.assertEqual(result.loc[1, "download_status"], "downloaded")

            result, purged, _ = e.purge_manifest_raw_files(
                result,
                manifest_path,
                [1],
                raw_root=raw_root,
                context_sentences=1,
                delete_unvalidated=True,
            )
            self.assertFalse(invalid_raw.exists())
            self.assertEqual(purged, 1)
            self.assertEqual(
                result.loc[1, "download_status"], e.PURGED_AFTER_FAILURE_STATUS
            )

    def test_storage_batched_run_purges_raw_and_resumes_without_download(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rows = []
            for index in range(3):
                accession = f"a{index}"
                rows.append(
                    {
                        "year": 2025,
                        "accession_number": accession,
                        "cik": "1001",
                        "form_type": "10-K",
                        "filing_date": "2025-02-01",
                        "report_date": "2024-12-31",
                        "primary_document": f"{accession}.htm",
                        "document_kind": "primary_document",
                        "metadata_status": "selected",
                        "source_url": f"https://www.sec.gov/{accession}.htm",
                        "local_path": str(root / "raw" / f"{accession}.htm"),
                        "result_path": str(root / "windows" / f"{accession}.json"),
                        "download_status": "pending",
                        "download_sha256": "",
                        "extract_status": "pending",
                    }
                )
            manifest = pd.DataFrame(rows)
            manifest_path = root / "filing_manifest.csv"

            def fake_download(row, **_kwargs):
                path = Path(row["local_path"])
                path.parent.mkdir(parents=True, exist_ok=True)
                payload = (
                    "<html><body><p>We deploy machine learning in operations."
                    "</p></body></html>" * 10
                ).encode("utf-8")
                path.write_bytes(payload)
                return {
                    "index": int(row["_index"]),
                    "download_status": "downloaded",
                    "download_error": "",
                    "download_bytes": len(payload),
                    "download_sha256": e.sha256_bytes(payload),
                }

            with patch.object(
                e, "_download_manifest_row", side_effect=fake_download
            ) as downloader:
                first = e.process_manifest_in_storage_batches(
                    manifest,
                    manifest_path,
                    data_dir=root,
                    user_agent="Example test@example.com",
                    rate_per_second=8,
                    download_workers=1,
                    extract_workers=1,
                    retries=0,
                    timeout_seconds=1,
                    min_bytes=1,
                    context_sentences=1,
                    overwrite_extractions=False,
                    max_batch_bytes=1,
                    keep_raw=False,
                )

            self.assertEqual(downloader.call_count, 3)
            self.assertEqual(set(first["download_status"]), {e.PURGED_DOWNLOAD_STATUS})
            self.assertEqual(set(first["processing_batch"]), {1, 2, 3})
            self.assertEqual(len(list((root / "windows").glob("*.json"))), 3)
            self.assertEqual(len(list((root / "raw").glob("*.htm"))), 0)

            with patch.object(e, "_download_manifest_row") as downloader:
                second = e.process_manifest_in_storage_batches(
                    first,
                    manifest_path,
                    data_dir=root,
                    user_agent="Example test@example.com",
                    rate_per_second=8,
                    download_workers=1,
                    extract_workers=1,
                    retries=0,
                    timeout_seconds=1,
                    min_bytes=1,
                    context_sentences=1,
                    overwrite_extractions=False,
                    max_batch_bytes=1,
                    keep_raw=False,
                )

            downloader.assert_not_called()
            self.assertEqual(set(second["download_status"]), {e.PURGED_DOWNLOAD_STATUS})

    def test_full_run_completeness_check_rejects_failed_filing(self) -> None:
        manifest = pd.DataFrame(
            [
                {"accession_number": "ok", "extract_status": "extracted"},
                {"accession_number": "bad", "extract_status": "failed"},
            ]
        )

        with self.assertRaisesRegex(RuntimeError, "incomplete"):
            e.require_complete_extractions(manifest)

    def test_missing_optional_year_is_json_serializable(self) -> None:
        windows, _ = e.extract_keyword_windows(
            "We use machine learning.",
            context_sentences=0,
            filing_metadata={
                "year": 2025,
                "accession_number": "a1",
                "cik": "1001",
                "form_type": "10-K",
                "report_year": pd.NA,
            },
        )

        self.assertIsNone(windows[0]["report_year"])
        json.dumps(windows)

    def test_no_keyword_filing_yields_metadata_only_sentinel(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            result_path = Path(tmp) / "result.json"
            result_path.write_text(
                json.dumps({"context_sentences": 1, "windows": []}),
                encoding="utf-8",
            )
            manifest = pd.DataFrame(
                [
                    {
                        "year": 2025,
                        "accession_number": "a1",
                        "cik": "1001",
                        "form_type": "10-K",
                        "filing_date": "2025-02-01",
                        "result_path": str(result_path),
                        "extract_status": "no_keyword",
                    }
                ]
            )

            groups = list(e.iter_filing_windows(manifest))

        self.assertEqual(len(groups), 1)
        self.assertEqual(groups[0][0]["window_id"], 0)
        self.assertEqual(groups[0][0]["text"], "")
        self.assertTrue(groups[0][0]["is_no_keyword_sentinel"])

    def test_process_pool_extracts_multiple_filings(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rows = []
            for number, text in enumerate(
                [
                    "We deploy artificial intelligence in customer support.",
                    "This filing contains ordinary business discussion.",
                ],
                start=1,
            ):
                accession = f"a{number}"
                source_path = root / f"{accession}.html"
                source_path.write_text(
                    "<html><body>" + (f"<p>{text}</p>" * 10) + "</body></html>",
                    encoding="utf-8",
                )
                rows.append(
                    {
                        "year": 2025,
                        "accession_number": accession,
                        "cik": "1001",
                        "form_type": "10-K",
                        "local_path": str(source_path),
                        "result_path": str(root / f"{accession}.json"),
                        "download_sha256": e.sha256_file(source_path),
                        "download_status": "downloaded",
                    }
                )
            manifest_path = root / "manifest.csv"

            result = e.extract_manifest_filings(
                pd.DataFrame(rows),
                manifest_path,
                context_sentences=1,
                workers=2,
                overwrite=False,
            )

        self.assertEqual(set(result["extract_status"]), {"extracted", "no_keyword"})

    def test_complete_submission_is_only_a_missing_primary_fallback(self) -> None:
        primary = e._filing_urls(
            "1001",
            "0000001001-24-000001",
            "annual.htm",
            allow_complete_submission_fallback=True,
        )
        fallback = e._filing_urls(
            "1001",
            "0000001001-24-000001",
            "",
            allow_complete_submission_fallback=True,
        )

        self.assertEqual(primary[1], "primary_document")
        self.assertEqual(fallback[1], "complete_submission_fallback")
        self.assertTrue(fallback[0].endswith("0000001001-24-000001.txt"))

    def test_complete_submission_fallback_reads_only_target_form(self) -> None:
        submission = b"""
        <DOCUMENT><TYPE>10-K/A\n<TEXT><html>Target machine learning text.</html></TEXT></DOCUMENT>
        <DOCUMENT><TYPE>EX-99\n<TEXT><html>Exhibit artificial intelligence text.</html></TEXT></DOCUMENT>
        """

        selected = e.primary_payload_from_complete_submission(submission, "10-K/A")

        self.assertIn(b"Target machine learning", selected)
        self.assertNotIn(b"Exhibit artificial intelligence", selected)


if __name__ == "__main__":
    unittest.main()
