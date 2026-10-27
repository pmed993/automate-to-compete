from __future__ import annotations

import hashlib
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stderr
from io import StringIO
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pandas as pd


THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

import ai_adoption_utils as u
import aws_backend
import get_ai_score_bulk as b
import merge_outputs as m


class LlmExtractionScoreTests(unittest.TestCase):
    def test_central_keyword_dictionary_has_routing_and_ranking_metadata(self) -> None:
        required = {
            "pattern",
            "category",
            "routing_weight",
            "ranking_weight",
            "case_sensitive",
        }
        self.assertTrue(u.AI_KEYWORDS)
        for keyword, metadata in u.AI_KEYWORDS.items():
            with self.subTest(keyword=keyword):
                self.assertTrue(required.issubset(metadata))

    def test_explicit_ai_abbreviation_triggers_prefilter(self) -> None:
        text = "We launched new AI tools and ML workflows in customer support."
        self.assertGreater(u.count_ai_keywords(text), 0)

    def test_dotted_ai_abbreviation_triggers_prefilter(self) -> None:
        text = "We launched new A.I.-based tools in customer support."
        self.assertGreater(u.count_ai_keywords(text), 0)

    def test_specialized_dictionary_term_triggers_prefilter(self) -> None:
        text = "The product uses OpenCV and computer vision for object detection."
        self.assertGreater(u.count_ai_keywords(text), 0)

    def test_non_dictionary_term_does_not_trigger_prefilter(self) -> None:
        text = "The company expanded analytics across the business."
        self.assertEqual(u.count_ai_keywords(text), 0)

    def test_ml_unit_does_not_trigger_prefilter(self) -> None:
        text = "The company uses a 200 mg/ml formulation in its product."
        self.assertEqual(u.count_ai_keywords(text), 0)

    def test_ml_abbreviation_is_case_sensitive(self) -> None:
        self.assertEqual(u.count_ai_keywords("The dosage was 5 mL."), 0)
        self.assertEqual(u.count_ai_keywords("The dosage was 10ml."), 0)
        self.assertEqual(u.count_ai_keywords("The page contains html markup."), 0)

        text = "The company uses ML to forecast customer demand."
        matches = u.find_keyword_matches(text)
        self.assertEqual([match.keyword for match in matches], ["ml"])
        candidate = u.create_anchor_candidates(u.normalize_and_segment_text(text))[0]
        self.assertGreater(candidate.operational_score, 0)
        self.assertGreaterEqual(candidate.quality_band, 3)

    def test_ml_initials_do_not_trigger_prefilter(self) -> None:
        text = "Matson Logistics (ML) provides rail intermodal services."
        self.assertEqual(u.count_ai_keywords(text), 0)

    def test_known_ambiguous_terms_do_not_route_without_ai_context(self) -> None:
        examples = [
            "The company implemented a remote learning model during the pandemic.",
            "Heat transfer systems include generator and transformer coolers.",
            "She studied at Universite Claude Bernard in Lyon.",
            "NHW = Non-Hispanic Whites and AI = American Indians.",
            "The aircraft crew includes two experienced copilots.",
        ]
        for text in examples:
            with self.subTest(text=text):
                self.assertEqual(u.count_ai_keywords(text), 0)

    def test_contextual_named_models_and_transformers_still_route(self) -> None:
        examples = [
            "We use a transformer model for language generation.",
            "We use Anthropic Claude in customer support.",
            "We deployed Claude in customer support.",
            "We deployed CLAUDE in customer support.",
            "Our team deployed the Google Gemini model.",
            "Our team uses Gemini for internal research.",
            "Our team uses GEMINI for internal research.",
            "We deployed Microsoft Copilot to our employees.",
        ]
        for text in examples:
            with self.subTest(text=text):
                self.assertGreater(u.count_ai_keywords(text), 0)

    def test_genuine_ai_use_is_a_high_priority_anchor(self) -> None:
        text = (
            "The platform utilizes artificial intelligence to personalise "
            "student learning."
        )
        candidate = u.create_anchor_candidates(u.normalize_and_segment_text(text))[0]
        self.assertEqual(candidate.matched_keyword, "artificial_intelligence")
        self.assertEqual(candidate.quality_band, 4)
        self.assertGreater(candidate.operational_score, 0)

    def test_current_use_ranks_above_future_and_industry_context(self) -> None:
        text = (
            "Artificial intelligence is changing the competitive environment. "
            "We are exploring how artificial intelligence may improve our operations. "
            "We use machine learning models in production to detect fraudulent transactions."
        )
        candidates = u.create_anchor_candidates(u.normalize_and_segment_text(text))
        ranked = u.score_anchor_candidates(candidates)
        self.assertEqual(len(candidates), 3)
        self.assertIn("exploring", " ".join(item.anchor_text for item in candidates))
        self.assertIn(
            "competitive environment", " ".join(item.anchor_text for item in candidates)
        )
        self.assertIn("in production", ranked[0].anchor_text)
        self.assertEqual(ranked[0].quality_band, 5)

    def test_ai_powered_product_outranks_repetitive_risk_discussion(self) -> None:
        product = (
            "Robotic waste sorting equipment consists of smart robots, powered by "
            "AI software, designed to pick, sort and recycle waste."
        )
        risk = (
            "Issues related to the development, deployment and use of artificial "
            "intelligence technologies could result in regulatory risk and liability."
        )
        text = "\n\n".join([risk, risk.replace("regulatory", "legal"), product])

        candidates = u.create_anchor_candidates(u.normalize_and_segment_text(text))
        product_candidate = next(
            candidate for candidate in candidates if "powered by AI" in candidate.anchor_text
        )
        risk_candidate = next(
            candidate
            for candidate in candidates
            if "regulatory risk" in candidate.anchor_text
        )
        extracted = u.extract_relevant_snippets(text, max_chars=300, sentence_window=0)

        self.assertTrue(product_candidate.strong_current_use)
        self.assertGreater(product_candidate.quality_band, risk_candidate.quality_band)
        self.assertIn(product, extracted)

    def test_anchor_survives_when_surrounding_window_exceeds_budget(self) -> None:
        previous = "Previous context " + "ordinary disclosure " * 35 + "."
        anchor = (
            "We use machine learning in production to detect fraudulent transactions."
        )
        following = "Following context " + "ordinary disclosure " * 35 + "."
        text = "\n".join([previous, anchor, following])

        snippet = u.extract_relevant_snippets(text, max_chars=180, sentence_window=1)

        self.assertIn(anchor, snippet)
        self.assertLessEqual(len(snippet), 180)

    def test_near_duplicate_anchor_disclosure_is_removed(self) -> None:
        text = (
            "We use machine learning in production to detect fraudulent transactions. "
            "Our company uses machine learning in production to detect fraudulent transactions."
        )
        candidates = u.create_anchor_candidates(u.normalize_and_segment_text(text))
        deduplicated = u.deduplicate_candidates(candidates)

        self.assertEqual(len(candidates), 2)
        self.assertEqual(len(deduplicated), 1)

    def test_newline_bullets_and_long_lists_create_bounded_segments(self) -> None:
        long_product = "Product features " + "component " * 160
        text = (
            "PRODUCT OVERVIEW:\n"
            "Products\n"
            "• First product\n"
            "• Second product; Third product.\n"
            f"{long_product}"
        )
        segments = u.normalize_and_segment_text(text)

        self.assertGreaterEqual(len(segments), 5)
        self.assertTrue(
            all(len(segment.text) <= u.MAX_TEXT_SEGMENT_CHARS for segment in segments)
        )

    def test_structural_lines_remain_in_filing_order(self) -> None:
        text = (
            "BUSINESS OVERVIEW\n"
            "We use artificial intelligence in our products.\n"
            "PROPERTIES\n"
            "We maintain offices in several cities.\n"
            "OPERATING REVIEW\n"
            "We use machine learning to forecast demand."
        )

        segments = u.normalize_and_segment_text(text)
        segment_text = [segment.text for segment in segments]

        self.assertLess(
            segment_text.index("We use artificial intelligence in our products."),
            segment_text.index("We use machine learning to forecast demand."),
        )

    def test_keyword_survives_oversized_unpunctuated_segment(self) -> None:
        text = (
            "component " * 120
            + "we use artificial intelligence to optimize operations "
            + "component " * 120
        )

        snippet = u.extract_relevant_snippets(text, max_chars=300, sentence_window=1)

        self.assertIn("artificial intelligence", snippet)
        self.assertLessEqual(len(snippet), 300)

    def test_no_keyword_audit_samples_late_proxy_evidence(self) -> None:
        beginning = "Opening corporate description " + "general information " * 35
        middle = "Middle financial discussion " + "ordinary performance " * 35
        ending = (
            "We deploy intelligent automation and advanced analytics across "
            "our internal operations."
        )
        text = "\n".join([beginning, middle, ending])
        self.assertEqual(u.count_ai_keywords(text), 0)

        snippet = u.extract_relevant_snippets(text, max_chars=500, sentence_window=1)

        self.assertIn("intelligent automation", snippet)
        self.assertLessEqual(len(snippet), 500)

    def test_no_keyword_audit_covers_beginning_middle_and_end(self) -> None:
        text = "\n".join(
            [
                "BEGINMARKER general corporate description.",
                "First intervening section.",
                "MIDDLEMARKER general financial discussion.",
                "Second intervening section.",
                "ENDMARKER concluding corporate discussion.",
            ]
        )

        snippet = u.extract_relevant_snippets(text, max_chars=300, sentence_window=1)

        self.assertIn("BEGINMARKER", snippet)
        self.assertIn("MIDDLEMARKER", snippet)
        self.assertIn("ENDMARKER", snippet)

    def test_student_brands_anchor_is_preserved_with_window_one(self) -> None:
        genuine = (
            "Student Brands utilizes deep data analytics and artificial intelligence "
            "to drive its content management system, the Content Brain."
        )
        remote = (
            "Our results were impacted as schools implemented a remote learning "
            "model and curtailed on-campus activities."
        )
        text = "\n".join([genuine, remote, remote, remote])

        window_zero = u.extract_relevant_snippets(
            text, max_chars=300, sentence_window=0
        )
        window_one = u.extract_relevant_snippets(text, max_chars=300, sentence_window=1)

        self.assertIn(genuine, window_zero)
        self.assertIn(genuine, window_one)
        self.assertLessEqual(window_one.count("remote learning model"), 1)

    def test_window_context_cannot_change_selected_anchors(self) -> None:
        text = "\n".join(
            [
                "Background context about the business and its customers.",
                "We use machine learning in production to detect fraud.",
                "Supporting context about transaction review procedures.",
                "We are exploring how artificial intelligence may improve reporting.",
                "Additional discussion of future reporting plans.",
            ]
        )
        segments = u.normalize_and_segment_text(text)
        selected = u.select_anchor_sentences(
            u.deduplicate_candidates(u.create_anchor_candidates(segments)),
            max_chars=300,
        )
        window_zero = u.extract_relevant_snippets(text, 300, 0)
        window_one = u.extract_relevant_snippets(text, 300, 1)

        for candidate in selected:
            self.assertIn(candidate.anchor_text, window_zero)
            self.assertIn(candidate.anchor_text, window_one)

    def test_distinct_anchors_take_priority_over_supporting_context(self) -> None:
        first_anchor = "We use machine learning in production to detect fraud."
        context = (
            "This supporting operational discussion is useful but contains no direct "
            "artificial-intelligence evidence."
        )
        second_anchor = (
            "We deploy artificial intelligence to automate customer support workflows."
        )
        text = "\n".join([first_anchor, context, second_anchor])

        snippet = u.extract_relevant_snippets(text, max_chars=145, sentence_window=1)

        self.assertIn(first_anchor, snippet)
        self.assertIn(second_anchor, snippet)
        self.assertNotIn(context, snippet)

    def test_short_adjacent_context_is_kept_when_budget_remains(self) -> None:
        anchor = "We use machine learning."
        context = "It is deployed across customer support."
        oversized_tail = "Unrelated conventional disclosure. " * 20
        text = "\n".join([anchor, context, oversized_tail])

        window_zero = u.extract_relevant_snippets(
            text, max_chars=180, sentence_window=0
        )
        window_one = u.extract_relevant_snippets(
            text, max_chars=180, sentence_window=1
        )

        self.assertNotIn(context, window_zero)
        self.assertIn(anchor, window_one)
        self.assertIn(context, window_one)

    def test_parse_exact_score(self) -> None:
        result = u.parse_model_output_payload("2")
        self.assertEqual(result["status"], "ok")
        self.assertEqual(result["ai_score"], 2)
        self.assertEqual(
            result["ai_score_label"], "emerging_or_bounded_implementation"
        )

    def test_prompt_output_contract_remains_one_score_only(self) -> None:
        prompt = u.build_ai_prompt("We use machine learning in production.")
        self.assertIn("Return only one character: 1, 2, or 3.", prompt)
        self.assertNotIn('"evidence"', prompt)

    def test_full_1800_character_evidence_reaches_the_prompt(self) -> None:
        evidence = "A" * 1800
        prompt = u.build_ai_prompt(evidence)
        retry_prompt = u.build_ai_retry_prompt(evidence)
        self.assertEqual(len(prompt), 2779)
        self.assertEqual(len(retry_prompt), 2863)
        self.assertIn(f"<filing_text>\n{evidence}\n</filing_text>", prompt)
        self.assertIn(f"<filing_text>\n{evidence}\n</filing_text>", retry_prompt)

    def test_zero_evidence_budget_preserves_all_extracted_windows(self) -> None:
        text = (
            "First window: we use machine learning in production.\n\n"
            + "Middle disclosure. " * 500
            + "\n\nFinal window: our AI service is currently available."
        )
        expected = u.normalize_structured_text(text)
        self.assertGreater(len(expected), 1800)
        self.assertEqual(u.extract_relevant_snippets(expected, 0, 1), expected)

    def test_evidence_below_positive_budget_passes_through_intact(self) -> None:
        text = (
            "First window: we use machine learning in production.\n\n"
            "Second window without another keyword remains available to the model."
        )
        expected = u.normalize_structured_text(text)
        self.assertLess(len(expected), 10_000)
        self.assertEqual(u.extract_relevant_snippets(text, 10_000, 1), expected)

    def test_capped_settings_are_the_frozen_defaults(self) -> None:
        self.assertEqual(u.MODEL_NAME, "eu.anthropic.claude-sonnet-4-6")
        self.assertEqual(u.TEMPERATURE, 0.0)
        self.assertEqual(u.REQUESTED_TEMPERATURE, 0.0)
        self.assertEqual(u.DEFAULT_MAX_NEW_TOKENS, 8)
        self.assertEqual(u.DEFAULT_MAX_PROMPT_CHARS, 2_400)
        args = b.parse_args(["--chunk-ids", "1"])
        self.assertEqual(args.model_id, "eu.anthropic.claude-sonnet-4-6")
        self.assertEqual(args.max_prompt_chars, 2_400)
        self.assertEqual(args.prefilter_mode, "hard_zero")
        self.assertEqual(args.max_analysis_year, 2025)
        self.assertEqual(len(u.build_ai_prompt("A" * 2_400)), 3_379)
        self.assertEqual(len(u.build_ai_retry_prompt("A" * 2_400)), 3_463)
        with redirect_stderr(StringIO()), self.assertRaises(SystemExit):
            b.parse_args(["--chunk-ids", "1", "--max-prompt-chars", "-1"])

    def test_prompt_uses_concise_ordered_score_derivation(self) -> None:
        prompt = u.build_ai_prompt("We use machine learning in production.")
        self.assertLessEqual(len(u._score_rubric()), 1000)
        self.assertIn("You are an expert analyst", prompt)
        self.assertIn("Do not rely on outside knowledge", prompt)
        self.assertIn("current use or specific active implementation", prompt)
        self.assertIn("Score 2 - Emerging or bounded implementation", prompt)
        self.assertIn("operational use is explicit and", prompt)
        self.assertIn(
            "AI-enabled products or services currently provided by the firm count",
            prompt,
        )
        self.assertIn("If the evidence is ambiguous, choose the lower score", prompt)
        self.assertNotIn("AI may be developed internally or obtained externally", prompt)
        self.assertNotIn("Realized importance", prompt)
        self.assertNotIn("meaningful realized effect", prompt)

    def test_retry_prompt_preserves_one_character_contract(self) -> None:
        prompt = u.build_ai_retry_prompt("We use machine learning in production.")
        self.assertIn("You are an expert analyst", prompt)
        self.assertIn("Do not rely on outside knowledge", prompt)
        self.assertIn("current use or specific active implementation", prompt)
        self.assertIn("operational use is explicit and", prompt)
        self.assertIn("Output exactly one ASCII digit", prompt)
        self.assertNotIn('"implementation_stage"', prompt)

    def test_concise_profile_versions_are_frozen(self) -> None:
        self.assertEqual(u.SCRIPT_VERSION, "2026-08-14-llm_extraction_v21")
        self.assertEqual(u.PROMPT_VERSION, "llm_extraction_claude_v8")
        self.assertEqual(u.RESEARCH_PROFILE, "llm_extraction_ai_1to3_v12")

    def test_scientific_prompts_match_the_frozen_content(self) -> None:
        main_hash = hashlib.sha256(
            u.build_ai_prompt("SENTINEL EVIDENCE").encode()
        ).hexdigest()
        retry_hash = hashlib.sha256(
            u.build_ai_retry_prompt("SENTINEL EVIDENCE").encode()
        ).hexdigest()
        self.assertEqual(
            main_hash,
            "8be33e13db665e76a4afb34bbc0264b85cf804c212ec182cbef2ccc4c36f5f25",
        )
        self.assertEqual(
            retry_hash,
            "148ea2ded976c1ab35f724e321d1e903f5f71cb3dfabb22a6a62a6a59f8ee631",
        )

    def test_bedrock_invocation_sends_frozen_generation_parameters(self) -> None:
        class FakeClient:
            def __init__(self) -> None:
                self.request = None

            def converse(self, **kwargs):
                self.request = kwargs
                return {"output": {"message": {"content": [{"text": "3"}]}}}

        client = FakeClient()
        result = aws_backend.invoke_bedrock_prompt(
            "prompt",
            model_id=u.MODEL_NAME,
            temperature=u.REQUESTED_TEMPERATURE,
            max_new_tokens=u.DEFAULT_MAX_NEW_TOKENS,
            client=client,
        )

        self.assertEqual(result, "3")
        self.assertEqual(client.request["modelId"], u.MODEL_NAME)
        self.assertEqual(
            client.request["inferenceConfig"],
            {"temperature": 0.0, "maxTokens": 8},
        )
        self.assertEqual(
            client.request["messages"],
            [{"role": "user", "content": [{"text": "prompt"}]}],
        )

    def test_bedrock_bulk_mock_preserves_existing_result_contract(self) -> None:
        class FakeClient:
            def converse(self, **_kwargs):
                return {"output": {"message": {"content": [{"text": "2"}]}}}

        results = list(
            aws_backend.invoke_bedrock_prompts(
                [("row-1", "prompt")],
                model_id=u.MODEL_NAME,
                temperature=0.0,
                max_new_tokens=8,
                max_workers=1,
                client=FakeClient(),
            )
        )
        records = [{"prefilter_audit_sample": False}]
        u.apply_bulk_results(
            records,
            {"row-1": {"record_index": 0}},
            results,
            save_raw_json=False,
        )
        self.assertEqual(records[0]["ai_score"], 2)
        self.assertEqual(records[0]["score_status"], "ok")

    def test_bedrock_config_check_is_available_without_chunk_selection(self) -> None:
        args = b.parse_args(["--check-bedrock-config"])
        self.assertTrue(args.check_bedrock_config)
        self.assertFalse(args.all_chunks)

    def test_input_configuration_has_no_bucket_or_team_default(self) -> None:
        with patch.dict(os.environ, {}, clear=True):
            args = b.parse_args(["--chunk-ids", "1"])
        self.assertIsNone(args.s3_bucket)
        self.assertIsNone(args.team)
        self.assertEqual(args.s3_prefix, "")
        with self.assertRaisesRegex(ValueError, "No S3 bucket configured"):
            b.input_backend(args)

    def test_s3_configuration_uses_environment_and_cli_precedence(self) -> None:
        with patch.dict(
            os.environ,
            {
                "AI_ADOPTION_S3_BUCKET": "environment-bucket",
                "AI_ADOPTION_S3_PREFIX": "environment-prefix",
            },
            clear=True,
        ):
            from_environment = b.parse_args(["--chunk-ids", "1"])
            from_cli = b.parse_args(
                [
                    "--s3-bucket",
                    "cli-bucket",
                    "--s3-prefix",
                    "cli-prefix",
                    "--chunk-ids",
                    "1",
                ]
            )
        self.assertEqual(from_environment.s3_bucket, "environment-bucket")
        self.assertEqual(from_environment.s3_prefix, "environment-prefix")
        self.assertEqual(from_cli.s3_bucket, "cli-bucket")
        self.assertEqual(from_cli.s3_prefix, "cli-prefix")

    def test_deprecated_team_flag_maps_to_generic_s3_prefix(self) -> None:
        with self.assertWarns(FutureWarning):
            args = b.parse_args(
                [
                    "--s3-bucket",
                    "example-bucket",
                    "--team",
                    "my-project",
                    "--chunk-prefix",
                    "extracted-rds",
                    "--chunk-ids",
                    "1",
                ]
            )
        self.assertEqual(args.s3_prefix, "my-project/extracted-rds")

    def test_local_input_mode_lists_chunks_without_aws(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            chunk = Path(tmp) / "extract_df_chunk_00001.rds"
            chunk.touch()
            args = b.parse_args(["--input-dir", tmp, "--all-chunks"])
            available = b.list_available_chunks(args)
            self.assertEqual(list(available), [chunk.name])
            self.assertEqual(b.input_backend(args), "local")

    def test_local_and_explicit_s3_inputs_are_mutually_exclusive(self) -> None:
        with tempfile.TemporaryDirectory() as tmp, redirect_stderr(
            StringIO()
        ), self.assertRaises(SystemExit):
            b.parse_args(
                [
                    "--input-dir",
                    tmp,
                    "--s3-bucket",
                    "example-bucket",
                    "--all-chunks",
                ]
            )

    def test_concise_rubric_does_not_change_output_schema(self) -> None:
        columns = u.preferred_output_columns(save_raw_json=False)
        self.assertIn("ai_score", columns)
        self.assertIn("ai_score_label", columns)
        self.assertNotIn("implementation_stage", columns)
        self.assertNotIn("organizational_scope", columns)
        self.assertNotIn("realized_importance", columns)

    def test_skip_existing_requires_current_profile_and_settings(self) -> None:
        args = SimpleNamespace(
            model_id=u.MODEL_NAME,
            prefilter_mode="hard_zero",
            prefilter_audit_rate=0.02,
            prefilter_audit_limit=0,
            max_prompt_chars=1800,
            sentence_window=1,
            lookup_csv=None,
            max_filings_per_chunk=0,
            max_analysis_year=2025,
        )
        summary = {
            "chunk_name": "extract_df_chunk_00001.rds",
            "source_label": "extract_df_chunk_00001.rds",
            "script_version": u.SCRIPT_VERSION,
            "prompt_version": u.PROMPT_VERSION,
            "research_profile": u.RESEARCH_PROFILE,
            "endpoint": u.MODEL_NAME,
            "prefilter_mode": "hard_zero",
            "prefilter_audit_rate": 0.02,
            "prefilter_audit_limit": 0,
            "max_prompt_chars": 1800,
            "sentence_window": 1,
            "lookup_csv": None,
            "max_filings_per_chunk": 0,
            "max_analysis_year": 2025,
        }
        b.require_compatible_existing_summary(
            summary,
            summary_path="summary.json",
            chunk_name="extract_df_chunk_00001.rds",
            source_label="extract_df_chunk_00001.rds",
            args=args,
        )

        for field, incompatible_value in [
            ("prompt_version", "obsolete_prompt"),
            ("lookup_csv", "different_lookup.csv"),
            ("max_filings_per_chunk", 20),
            ("max_analysis_year", 2026),
        ]:
            with self.subTest(field=field):
                incompatible = summary.copy()
                incompatible[field] = incompatible_value
                with self.assertRaisesRegex(ValueError, "incompatible"):
                    b.require_compatible_existing_summary(
                        incompatible,
                        summary_path="summary.json",
                        chunk_name="extract_df_chunk_00001.rds",
                        source_label="extract_df_chunk_00001.rds",
                        args=args,
                    )

    def test_analysis_year_filter_excludes_2026(self) -> None:
        filings = pd.DataFrame(
            {
                "accession_number": ["a2025", "a2026"],
                "year": [2025, 2026],
            }
        )
        filtered = u.filter_to_analysis_year(filings, 2025)
        self.assertEqual(filtered["accession_number"].tolist(), ["a2025"])
        self.assertEqual(len(u.filter_to_analysis_year(filings, 0)), 2)

    def test_repairs_reject_obsolete_scoring_profiles(self) -> None:
        current = pd.DataFrame(
            {
                "script_version": [u.SCRIPT_VERSION],
                "prompt_version": [u.PROMPT_VERSION],
                "research_profile": [u.RESEARCH_PROFILE],
            }
        )
        b.require_current_score_profile(current, "scores.csv")

        obsolete = current.copy()
        obsolete["research_profile"] = "llm_extraction_ai_1to3_v2"
        with self.assertRaisesRegex(ValueError, "Cannot repair"):
            b.require_current_score_profile(obsolete, "scores.csv")

    def test_manifest_exit_code_and_partial_chunk_status(self) -> None:
        self.assertEqual(b.chunk_status_from_summary({"n_unscored": 0}), "ok")
        self.assertEqual(
            b.chunk_status_from_summary({"n_unscored": 1}), "partial_failure"
        )
        self.assertEqual(b.manifest_exit_code([{"status": "ok"}]), 0)
        self.assertEqual(b.manifest_exit_code([{"status": "skipped_existing"}]), 0)
        self.assertEqual(b.manifest_exit_code([{"status": "partial_failure"}]), 1)
        self.assertEqual(b.manifest_exit_code([{"status": "failed"}]), 1)
        self.assertEqual(
            b.manifest_exit_code([{"status": "repair_no_rerunnable_rows"}]), 1
        )

    def test_parse_json_score_fallback(self) -> None:
        payload = """
        {
          "score": 3
        }
        """
        result = u.parse_model_output_payload(payload)
        self.assertEqual(result["status"], "ok")
        self.assertEqual(result["ai_score"], 3)
        self.assertEqual(
            result["ai_score_label"], "established_and_integrated_implementation"
        )

    def test_parse_conflicting_scores_fails(self) -> None:
        result = u.parse_model_output_payload("The answer could be 2 or 3.")
        self.assertEqual(result["status"], "conflicting_scores")
        self.assertTrue(pd.isna(result["ai_score"]))

    def test_parse_missing_output_fails(self) -> None:
        result = u.parse_model_output_payload("")
        self.assertEqual(result["status"], "missing_output")
        self.assertTrue(pd.isna(result["ai_score"]))

    def test_retry_success_updates_retry_status(self) -> None:
        records = [
            {
                "endpoint_attempts": 1,
                "prefilter_audit_sample": False,
                "retry_attempted": False,
            }
        ]
        pending = {"0": {"record_index": 0}}

        u.apply_bulk_results(
            records,
            pending,
            [("0", {"model_response_string": "3"})],
            save_raw_json=False,
            is_retry=True,
        )

        self.assertEqual(records[0]["parse_status"], "retry_success")
        self.assertEqual(records[0]["score_status"], "ok_after_retry")
        self.assertEqual(records[0]["retry_score_status"], "ok")
        self.assertEqual(records[0]["endpoint_attempts"], 2)

    def test_scoring_helper_retries_parser_failure_once(self) -> None:
        records = [
            {
                "endpoint_attempts": 1,
                "prefilter_audit_sample": False,
                "retry_attempted": False,
            }
        ]
        pending = {
            "0": {
                "record_index": 0,
                "prompt": "initial prompt",
                "retry_prompt": "retry prompt",
            }
        }
        args = SimpleNamespace(
            model_id="claude",
            max_workers=1,
            save_raw_json=False,
            aws_region=None,
        )

        with patch.object(
            b,
            "invoke_bedrock",
            side_effect=[
                [("0", {"model_response_string": "2 or 3"})],
                [("0", {"model_response_string": "2"})],
            ],
        ) as invoke:
            b.score_pending_records(
                records,
                pending,
                args=args,
                chunk_name="chunk_1",
            )

        self.assertEqual(invoke.call_count, 2)
        self.assertEqual(records[0]["ai_score"], 2)
        self.assertEqual(records[0]["score_status"], "ok_after_retry")

    def test_snippet_audit_table_keeps_processed_text_and_skipped_rows(self) -> None:
        wide = pd.DataFrame(
            [
                {
                    "accession_number": "a1",
                    "cik": "1001",
                    "year": 2024,
                    "form_type": "10-K",
                    "combined_chars": 60,
                    "combined_text": "We currently use machine learning in customer support.",
                },
                {
                    "accession_number": "a2",
                    "cik": "1002",
                    "year": 2024,
                    "form_type": "10-K",
                    "combined_chars": 50,
                    "combined_text": "We expanded conventional analytics and reporting.",
                },
            ]
        )
        records, pending = u.prepare_records_and_prompts(
            wide,
            run_id="r1",
            chunk_id="extract_df_chunk_00001",
            source_label="test",
            llm_model="claude",
            llm_checkpoint="claude",
            temperature=0.0,
            max_new_tokens=32,
            endpoint="claude",
            prefilter_mode="hard_zero",
            audit_seed="seed",
            prefilter_audit_rate=0.0,
            prefilter_audit_limit=0,
            max_prompt_chars=1800,
            sentence_window=0,
        )
        u.apply_bulk_results(
            records,
            pending,
            [("0", {"model_response_string": "2"})],
            save_raw_json=False,
        )

        audit = u.build_snippet_audit_table(records, pending)
        self.assertEqual(audit.columns.tolist(), u.SNIPPET_AUDIT_COLUMNS)
        self.assertEqual(audit["llm_processed"].tolist(), [True, False])
        self.assertGreater(int(audit.loc[0, "snippet_text_length"]), 0)
        self.assertIn("machine learning", audit.loc[0, "snippet_text"])
        self.assertEqual(int(audit.loc[1, "snippet_text_length"]), 0)
        self.assertEqual(audit.loc[1, "snippet_text"], "")

    def test_csv_boolean_parser_does_not_treat_false_string_as_true(self) -> None:
        parsed = b.boolean_series(
            pd.Series([True, False, "true", "False", "1", "0", None])
        )
        self.assertEqual(
            parsed.tolist(), [True, False, True, False, True, False, False]
        )
        self.assertFalse(u.coerce_bool("False"))

    def test_rebuilt_audit_keeps_only_text_sent_to_llm(self) -> None:
        records = [
            {
                "accession_number": "a1",
                "filing_accession": "a1",
                "cik": "1001",
                "year": 2024,
                "llm_called": "True",
                "max_prompt_chars": 1800,
                "sentence_window": 0,
            },
            {
                "accession_number": "a2",
                "filing_accession": "a2",
                "cik": "1002",
                "year": 2024,
                "llm_called": "False",
                "max_prompt_chars": 1800,
                "sentence_window": 0,
            },
        ]
        wide_by_accession = {
            "a1": pd.Series({"combined_text": "We use machine learning."}),
            "a2": pd.Series({"combined_text": "Conventional analytics only."}),
        }

        audit = b.rebuild_snippet_audit(records, wide_by_accession)

        self.assertEqual(audit["llm_processed"].tolist(), [True, False])
        self.assertIn("machine learning", audit.loc[0, "snippet_text"])
        self.assertEqual(audit.loc[1, "snippet_text"], "")

    def test_aggregate_snippet_audit_concatenates_chunk_files(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for chunk_id, cik in [(1, 1001), (2, 1002)]:
                pd.DataFrame(
                    [
                        {
                            "cik": cik,
                            "year": 2024,
                            "filing_accession": f"a{chunk_id}",
                            "chunk_id": f"extract_df_chunk_{chunk_id:05d}",
                            "llm_processed": True,
                            "snippet_text_length": 18,
                            "snippet_text": "AI adoption text.",
                        }
                    ]
                ).to_csv(
                    root / f"extract_df_chunk_{chunk_id:05d}_claude_snippet_audit.csv",
                    index=False,
                )

            loaded = m.load_snippet_audit_outputs(str(root), "claude")
            aggregate = m.build_aggregate_snippet_audit(loaded)
            self.assertEqual(len(aggregate), 2)
            self.assertEqual(set(aggregate["model_label"]), {"claude"})
            self.assertEqual(
                set(aggregate["filing_accession"].astype(str)), {"a1", "a2"}
            )

    def test_firm_year_takes_max_score(self) -> None:
        df = pd.DataFrame(
            [
                {
                    "accession_number": "a1",
                    "cik": 1001,
                    "year": 2024,
                    "claude_run_id": "r1",
                    "claude_script_version": "s1",
                    "claude_prompt_version": "p1",
                    "claude_llm_model": "claude",
                    "claude_llm_checkpoint": "claude-4-5",
                    "claude_temperature": 0.0,
                    "claude_max_new_tokens": 32,
                    "claude_parse_status": "success",
                    "claude_ai_score": 1,
                    "claude_ai_score_label": "no_disclosed_current_implementation",
                    "claude_score_explanation": "Parsed AI adoption score 1.",
                },
                {
                    "accession_number": "a2",
                    "cik": 1001,
                    "year": 2024,
                    "claude_run_id": "r1",
                    "claude_script_version": "s1",
                    "claude_prompt_version": "p1",
                    "claude_llm_model": "claude",
                    "claude_llm_checkpoint": "claude-4-5",
                    "claude_temperature": 0.0,
                    "claude_max_new_tokens": 32,
                    "claude_parse_status": "success",
                    "claude_ai_score": 3,
                    "claude_ai_score_label": "established_and_integrated_implementation",
                    "claude_score_explanation": "Parsed AI adoption score 3.",
                },
            ]
        )

        panel = m.build_firm_year_panel(df, "claude")
        self.assertEqual(int(panel.loc[0, "ai_score"]), 3)
        self.assertEqual(
            panel.loc[0, "ai_score_label"], "established_and_integrated_implementation"
        )

    def test_merge_rejects_mixed_prompt_profiles(self) -> None:
        df = pd.DataFrame(
            {
                "claude_script_version": [u.SCRIPT_VERSION, u.SCRIPT_VERSION],
                "claude_prompt_version": ["llm_extraction_claude_v2", u.PROMPT_VERSION],
                "claude_research_profile": [
                    "llm_extraction_ai_1to3_v2",
                    u.RESEARCH_PROFILE,
                ],
            }
        )
        with self.assertRaisesRegex(ValueError, "mixed prompt_version"):
            m.require_uniform_model_profile(df, "claude")

    def test_duplicate_accessions_are_rejected_with_report(self) -> None:
        df = pd.DataFrame(
            [
                {
                    "accession_number": "a1",
                    "cik": 1001,
                    "year": 2024,
                    "combined_chars": 2000,
                    "snippet_chars": 1800,
                    "claude_llm_called": True,
                    "claude_ai_score": 1,
                },
                {
                    "accession_number": "a1",
                    "cik": 1001,
                    "year": 2024,
                    "combined_chars": 1000,
                    "snippet_chars": 900,
                    "claude_llm_called": True,
                    "claude_ai_score": 3,
                },
            ]
        )

        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaises(ValueError):
                m.require_unique_accessions(df, tmp)
            self.assertTrue((Path(tmp) / "filing_accession_duplicates.csv").exists())

if __name__ == "__main__":
    unittest.main()
