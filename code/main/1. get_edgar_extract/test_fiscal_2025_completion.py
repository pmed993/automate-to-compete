from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import edgar_keyword_extract as e
import fiscal_2025_completion as f


def row(accession, cik, year, report_date, filing_date, method):
    return {
        "accession_number": accession, "cik": cik, "year": year,
        "form_type": "10-K", "report_date": report_date,
        "filing_date": filing_date, "filing_year": int(filing_date[:4]),
        "report_year": int(report_date[:4]), "year_match_method": method,
    }


class Fiscal2025CompletionTests(unittest.TestCase):
    def test_lookup_targets_only_2025_sample_firms_missing_that_report_year(self):
        lookup = pd.DataFrame({"cik": ["001", "2", "3", "4"], "year": [2025, 2025, 2025, 2024]})
        primary = pd.DataFrame({"cik": ["1", "2", "4"], "report_date": ["2025-09-30", "2024-12-31", "2025-06-30"]})
        result = f.build_completion_lookup(lookup, primary)
        self.assertEqual(result.to_dict("records"), [{"cik": "2", "year": 2025}, {"cik": "3", "year": 2025}])

    def test_2026_completion_dates_survive_and_main_accession_wins(self):
        primary = pd.DataFrame([
            row("same", "1", 2025, "2025-09-30", "2025-11-14", "filing-date"),
            row("outside", "2", 2026, "2026-06-30", "2026-08-01", "filing-date"),
        ])
        completion = pd.DataFrame([
            row("same", "99", 2025, "2025-09-30", "2025-11-14", "report-date"),
            row("new", "3", 2025, "2025-12-31", "2026-02-15", "report-date"),
        ])
        merged, audit = f.combine_manifests(primary, completion)
        self.assertEqual(set(merged.accession_number), {"same", "new"})
        self.assertEqual(merged.loc[merged.accession_number.eq("same"), "cik"].iloc[0], "1")
        late = merged.loc[merged.accession_number.eq("new")].iloc[0]
        self.assertEqual((late["year"], late["filing_year"], late["filing_date"]), (2025, 2026, "2026-02-15"))
        self.assertEqual(audit["completion_unique_additions"], 1)
        self.assertEqual(audit["main_rows_outside_analysis_window"], 1)

    def test_merge_rejects_a_non_2025_report_or_wrong_year_rule(self):
        primary = pd.DataFrame([row("a", "1", 2025, "2024-12-31", "2025-02-01", "filing-date")])
        for report_date, method in [("2024-12-31", "report-date"), ("2025-12-31", "filing-date")]:
            with self.subTest(report_date=report_date, method=method):
                completion = pd.DataFrame([row("b", "2", 2025, report_date, "2026-02-01", method)])
                with self.assertRaises(ValueError):
                    f.combine_manifests(primary, completion)

    def test_combined_rds_chunks_keep_sentinels_dates_and_unique_accessions(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            manifests = []
            for name, accession, filing_date, report_date, method in [
                ("main", "a", "2025-02-01", "2024-12-31", "filing-date"),
                ("completion", "b", "2026-02-01", "2025-12-31", "report-date"),
            ]:
                folder = root / name
                folder.mkdir()
                record = row(accession, "1001", 2025, report_date, filing_date, method)
                result = folder / "result.json"
                # A no-hit result must still produce one scorer-input row.
                result.write_text(json.dumps({
                    "accession_number": accession, "source_sha256": "source-hash",
                    "extraction_version": e.EXTRACTION_VERSION, "context_sentences": 1,
                    "readable_text_chars": 20, "keyword_hit_count": 0, "windows": [],
                }))
                record.update({
                    "result_path": str(result), "download_sha256": "source-hash",
                    "download_status": e.PURGED_DOWNLOAD_STATUS, "download_bytes": 100,
                    "extract_status": "no_keyword", "window_count": 0,
                    "keyword_hit_count": 0, "exact_duplicate_windows_removed": 0,
                })
                path = folder / "filing_manifest.csv"
                pd.DataFrame([record]).to_csv(path, index=False)
                manifests.append(path)
            output = root / "combined"
            audit = f.merge_extractions(*manifests, output, filings_per_output=1)
            self.assertEqual(audit["combined_unique_filings"], 2)
            self.assertEqual(len(list((output / "assembled").glob("*.rds"))), 2)
            self.assertTrue(json.loads((output / "qa_summary.json").read_text())["passed"])
            late = pd.read_csv(output / "assembled/extract_df_chunk_00002.csv.gz").iloc[0]
            self.assertEqual((int(late.year), int(late.filing_year)), (2025, 2026))
            self.assertTrue(late.is_no_keyword_sentinel)
            # ISO text avoids pyreadr's incorrect conversion of integer IDate.
            subprocess.run([
                "Rscript", "-e",
                'x <- readRDS(commandArgs(trailingOnly=TRUE)[1]); '
                'stopifnot(identical(x$filing_date, "2026-02-01"), '
                'identical(x$report_date, "2025-12-31"))',
                str(output / "assembled/extract_df_chunk_00002.rds"),
            ], check=True, capture_output=True, text=True)
            # Re-running with unchanged inputs reuses validated chunks.
            self.assertEqual(f.merge_extractions(*manifests, output, filings_per_output=1)["combined_unique_filings"], 2)


if __name__ == "__main__":
    unittest.main()
