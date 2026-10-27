from __future__ import annotations

import sys
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pandas as pd


THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

import ai_adoption_utils as u


class KeywordWindowInputTests(unittest.TestCase):
    def test_multiple_windows_are_preserved_in_source_order(self) -> None:
        raw = pd.DataFrame(
            [
                {
                    "item": "keyword_window",
                    "year": 2024,
                    "accession_number": "a1",
                    "cik": "1001",
                    "form_type": "10-K",
                    "window_id": 2,
                    "text": "Second machine learning disclosure.",
                },
                {
                    "item": "keyword_window",
                    "year": 2024,
                    "accession_number": "a1",
                    "cik": "1001",
                    "form_type": "10-K",
                    "window_id": 1,
                    "text": "First artificial intelligence disclosure.",
                },
            ]
        )

        wide = u.long_to_wide(raw)

        self.assertEqual(len(wide), 1)
        combined = wide.loc[0, "combined_text"]
        self.assertLess(combined.index("First artificial"), combined.index("Second machine"))

    def test_new_window_input_always_keeps_amendment(self) -> None:
        raw = pd.DataFrame(
            [
                {
                    "item": "keyword_window",
                    "year": 2024,
                    "accession_number": "a1",
                    "cik": "1001",
                    "form_type": "10-K/A",
                    "window_id": 1,
                    "text": "We use machine learning.",
                }
            ]
        )

        self.assertEqual(len(u.long_to_wide(raw)), 1)

    def test_non_window_item_is_rejected(self) -> None:
        raw = pd.DataFrame(
            [
                {
                    "item": "unsupported_section",
                    "year": 2024,
                    "accession_number": "a1",
                    "cik": "1001",
                    "form_type": "10-K/A",
                    "text": "Unsupported input text.",
                }
            ]
        )

        with self.assertRaises(ValueError):
            u.long_to_wide(raw)

    def test_no_keyword_sentinel_preserves_filing_without_false_hit(self) -> None:
        raw = pd.DataFrame(
            [
                {
                    "item": "keyword_window",
                    "year": 2024,
                    "accession_number": "a1",
                    "cik": "1001",
                    "form_type": "10-K",
                    "window_id": 0,
                    "text": "",
                    "is_no_keyword_sentinel": True,
                }
            ]
        )

        wide = u.long_to_wide(raw)

        self.assertEqual(len(wide), 1)
        self.assertEqual(wide.loc[0, "combined_text"], "")
        self.assertEqual(u.count_ai_keywords(wide.loc[0, "combined_text"]), 0)

    def test_s3_listing_rejects_duplicate_chunk_basenames(self) -> None:
        class FakePaginator:
            def paginate(self, **_kwargs):
                return [
                    {
                        "Contents": [
                            {
                                "Key": "team-root/new/extract_df_chunk_00001.rds"
                            },
                            {
                                "Key": "team-root/legacy/extract_df_chunk_00001.rds"
                            },
                        ]
                    }
                ]

        class FakeClient:
            def get_paginator(self, _name):
                return FakePaginator()

        with self.assertRaisesRegex(ValueError, "Duplicate chunk basename"):
            u.list_s3_chunks(
                "example-bucket",
                "research-inputs",
                s3_client=FakeClient(),
            )

    def test_standard_s3_reader_uses_configured_bucket_and_key(self) -> None:
        class FakeBody:
            def read(self):
                return b"mock-rds"

        class FakeClient:
            def __init__(self) -> None:
                self.request = None

            def get_object(self, **kwargs):
                self.request = kwargs
                return {"Body": FakeBody()}

        expected = pd.DataFrame({"item": ["keyword_window"]})
        client = FakeClient()
        fake_pyreadr = SimpleNamespace(read_r=lambda _path: {None: expected})
        with patch.dict(sys.modules, {"pyreadr": fake_pyreadr}):
            actual = u.read_rds_from_s3(
                "research-inputs/extract_df_chunk_00001.rds",
                bucket="example-bucket",
                s3_client=client,
            )

        self.assertTrue(actual.equals(expected))
        self.assertEqual(
            client.request,
            {
                "Bucket": "example-bucket",
                "Key": "research-inputs/extract_df_chunk_00001.rds",
            },
        )

if __name__ == "__main__":
    unittest.main()
