"""Utility functions for filing-level AI adoption labeling.

Helper functions for local/AWS S3 access, RDS reading, lookup filtering, prompt
construction, snippet extraction, model-output parsing, and output formatting.
"""

from __future__ import annotations

import hashlib
import json
import logging
import os
import re
import tempfile
from dataclasses import dataclass, replace
from difflib import SequenceMatcher
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable, Optional, Sequence

import pandas as pd


# ---------------------------------------------------------------------------
# Versions, defaults, and text-matching patterns
# ---------------------------------------------------------------------------
# These values are written into every output file so results can be traced back
# to the exact script and prompt version that produced them.
SCRIPT_VERSION = "2026-08-14-llm_extraction_v21"
PROMPT_VERSION = "llm_extraction_claude_v8"
RESEARCH_PROFILE = "llm_extraction_ai_1to3_v12"
MODEL_NAME = "eu.anthropic.claude-sonnet-4-6"
REQUESTED_TEMPERATURE = 0.0
TEMPERATURE: Optional[float] = REQUESTED_TEMPERATURE
DEFAULT_MAX_NEW_TOKENS = 8
# The capped production profile passes through short filing extracts intact and
# applies anchor-first selection only when evidence exceeds the cap.
# Zero remains available to send every extracted filing window without a cap.
DEFAULT_MAX_PROMPT_CHARS = 2_400
DEFAULT_SENTENCE_WINDOW = 1
DEFAULT_PREFILTER_MODE = "hard_zero"
DEFAULT_MAX_ANALYSIS_YEAR = 2025

DEFAULT_BUCKET = os.getenv("AI_ADOPTION_S3_BUCKET")

# Chunk files must follow this exact naming pattern to be picked up.
CHUNK_RE = re.compile(r"^extract_df_chunk_(\d{5})\.rds$")

# Generic text cleanup and structural segmentation patterns.
WHITESPACE_RE = re.compile(r"\s+")
HORIZONTAL_WHITESPACE_RE = re.compile(r"[^\S\r\n]+")
EXCESS_NEWLINES_RE = re.compile(r"\n{3,}")
SENTENCE_SPLIT_RE = re.compile(r"(?<=[\.\!\?\;])\s+")
BULLET_RE = re.compile(r"[\u2022\u2023\u25e6\u2043\u2219\u25aa\u25cf]")
INLINE_LIST_ITEM_RE = re.compile(r"\s+(?=(?:\d{1,2}|[A-Za-z])\)\s+[A-Z])")
TABLE_SEPARATOR_RE = re.compile(r"\s+\|\s+")
MAX_TEXT_SEGMENT_CHARS = 900
MAX_CONTEXT_SEGMENT_CHARS = 360
MIN_TRUNCATED_CONTEXT_CHARS = 40
MIN_ROUTING_WEIGHT = 2

# One metadata-rich dictionary is the unique source of truth for routing and
# ranking. Each rule is compiled independently so abbreviations can remain
# case-sensitive while ordinary phrases are matched case-insensitively.
AI_KEYWORDS: dict[str, dict[str, Any]] = {
    "generative_artificial_intelligence": {
        "pattern": r"\bgenerative artificial intelligence\b",
        "category": "explicit_ai",
        "routing_weight": 10,
        "ranking_weight": 10,
        "case_sensitive": False,
    },
    "artificial_intelligence": {
        "pattern": r"\bartificial intelligence\b",
        "category": "explicit_ai",
        "routing_weight": 10,
        "ranking_weight": 10,
        "case_sensitive": False,
    },
    "generative_ai": {
        "pattern": r"\bgenerative AI\b",
        "category": "explicit_ai",
        "routing_weight": 10,
        "ranking_weight": 10,
        "case_sensitive": False,
    },
    "genai": {
        "pattern": r"\bGenAI\b",
        "category": "explicit_ai",
        "routing_weight": 10,
        "ranking_weight": 10,
        "case_sensitive": False,
    },
    "machine_learning": {
        "pattern": r"\bmachine learning\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "deep_learning": {
        "pattern": r"\bdeep[- ]learning\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "large_language_model": {
        "pattern": r"\blarge[- ]language models?\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "foundation_model": {
        "pattern": r"\bfoundation models?\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "multimodal_model": {
        "pattern": r"\bmultimodal models?\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "natural_language_processing": {
        "pattern": r"\bnatural language processing\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "reinforcement_learning": {
        "pattern": r"\breinforcement learning\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "supervised_learning": {
        "pattern": r"\bsupervised learning\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "unsupervised_learning": {
        "pattern": r"\bunsupervised learning\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "neural_network": {
        "pattern": r"\bneural networks?\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "adversarial_network": {
        "pattern": r"\badversarial networks?\b",
        "category": "explicit_ai",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "computer_vision": {
        "pattern": r"\bcomputer vision\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "machine_vision": {
        "pattern": r"\bmachine vision\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "machine_translation": {
        "pattern": r"\bmachine translation\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "object_detection": {
        "pattern": r"\bobject detection\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "object_recognition": {
        "pattern": r"\bobject recognition\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "speech_recognition": {
        "pattern": r"\bspeech recognition\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "recommender_system": {
        "pattern": r"\brecommender systems?\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "ai_chatbot": {
        "pattern": r"\bAI chatbot\b",
        "category": "explicit_ai",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "a_i": {
        "pattern": r"(?<!\w)A\.I\.(?!\w)",
        "category": "abbreviation",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": False,
    },
    "ai": {
        "pattern": r"(?<![A-Za-z0-9])AI(?![A-Za-z0-9])",
        "category": "abbreviation",
        "routing_weight": 7,
        "ranking_weight": 7,
        "case_sensitive": True,
        "disambiguator": "ai",
    },
    "ml": {
        "pattern": r"(?<!\()(?<![A-Za-z0-9/])ML(?![A-Za-z0-9/])",
        "category": "abbreviation",
        "routing_weight": 6,
        "ranking_weight": 6,
        "case_sensitive": True,
    },
    "nlp": {
        "pattern": r"(?<![A-Za-z0-9])NLP(?![A-Za-z0-9])",
        "category": "abbreviation",
        "routing_weight": 8,
        "ranking_weight": 8,
        "case_sensitive": True,
    },
    "llm": {
        "pattern": r"(?<![A-Za-z0-9])LLMs?(?![A-Za-z0-9])",
        "category": "abbreviation",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": True,
    },
    "gpt": {
        "pattern": r"\bGPT(?:-\d+(?:\.\d+)?)?\b",
        "category": "named_model",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "chatgpt": {
        "pattern": r"\bChatGPT\b",
        "category": "named_model",
        "routing_weight": 9,
        "ranking_weight": 9,
        "case_sensitive": False,
    },
    "claude": {
        "pattern": r"\bClaude\b",
        "category": "named_model",
        "routing_weight": 5,
        "ranking_weight": 5,
        "case_sensitive": False,
        "disambiguator": "claude",
    },
    "gemini": {
        "pattern": r"\bGemini\b",
        "category": "named_model",
        "routing_weight": 5,
        "ranking_weight": 5,
        "case_sensitive": False,
        "disambiguator": "gemini",
    },
    "copilot": {
        "pattern": r"\bCopilots?\b",
        "category": "named_model",
        "routing_weight": 6,
        "ranking_weight": 6,
        "case_sensitive": False,
        "disambiguator": "copilot",
    },
    "decision_tree": {
        "pattern": r"\bdecision trees?\b",
        "category": "statistical_method",
        "routing_weight": 6,
        "ranking_weight": 5,
        "case_sensitive": False,
    },
    "random_forest": {
        "pattern": r"\brandom forests?\b",
        "category": "statistical_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "gradient_boosting": {
        "pattern": r"\bgradient boosting\b",
        "category": "statistical_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "support_vector_machine": {
        "pattern": r"\bsupport vector machines?\b",
        "category": "statistical_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "kernel_method": {
        "pattern": r"\bkernel methods?\b",
        "category": "statistical_method",
        "routing_weight": 6,
        "ranking_weight": 5,
        "case_sensitive": False,
    },
    "principal_component_analysis": {
        "pattern": r"\bprincipal component analysis\b",
        "category": "statistical_method",
        "routing_weight": 5,
        "ranking_weight": 4,
        "case_sensitive": False,
    },
    "latent_dirichlet_allocation": {
        "pattern": r"\blatent Dirichlet allocation\b",
        "category": "statistical_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "latent_semantic_analysis": {
        "pattern": r"\blatent semantic analysis\b",
        "category": "statistical_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "predictive_model": {
        "pattern": r"\bpredictive models?\b",
        "category": "ambiguous",
        "routing_weight": 4,
        "ranking_weight": 3,
        "case_sensitive": False,
    },
    "learning_model": {
        "pattern": r"\blearning models?\b",
        "category": "ambiguous",
        "routing_weight": 1,
        "ranking_weight": 0,
        "case_sensitive": False,
        "disambiguator": "learning_model",
    },
    "transformer": {
        "pattern": r"\btransformers?\b",
        "category": "ambiguous",
        "routing_weight": 2,
        "ranking_weight": 1,
        "case_sensitive": False,
        "disambiguator": "transformer",
    },
    "feature_extraction": {
        "pattern": r"\bfeature extraction\b",
        "category": "ambiguous",
        "routing_weight": 5,
        "ranking_weight": 4,
        "case_sensitive": False,
    },
    "image_processing": {
        "pattern": r"\bimage processing\b",
        "category": "ambiguous",
        "routing_weight": 5,
        "ranking_weight": 4,
        "case_sensitive": False,
    },
    "image_recognition": {
        "pattern": r"\bimage recognition\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "image_segmentation": {
        "pattern": r"\bimage segmentation\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "pattern_recognition": {
        "pattern": r"\bpattern recognition\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "recognition_system": {
        "pattern": r"\brecognition systems?\b",
        "category": "ambiguous",
        "routing_weight": 4,
        "ranking_weight": 3,
        "case_sensitive": False,
    },
    "genetic_algorithm": {
        "pattern": r"\bgenetic algorithms?\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "opinion_mining": {
        "pattern": r"\bopinion mining\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "sentiment_analysis": {
        "pattern": r"\bsentiment analysis\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "sentiment_classification": {
        "pattern": r"\bsentiment classification\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "text_mining": {
        "pattern": r"\btext mining\b",
        "category": "ai_method",
        "routing_weight": 7,
        "ranking_weight": 6,
        "case_sensitive": False,
    },
    "word_embedding": {
        "pattern": r"\bword embeddings?\b",
        "category": "ai_method",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": False,
    },
    "virtual_agent": {
        "pattern": r"\bvirtual agents?\b",
        "category": "ambiguous",
        "routing_weight": 5,
        "ranking_weight": 4,
        "case_sensitive": False,
    },
    "libsvm": {
        "pattern": r"\bLibsvm\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": False,
    },
    "mahout": {
        "pattern": r"\bMahout\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": True,
    },
    "opencv": {
        "pattern": r"\bOpenCV\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": True,
    },
    "word2vec": {
        "pattern": r"\bWord2vec\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": False,
    },
    "xgboost": {
        "pattern": r"\bXGBoost\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": False,
    },
    "keras": {
        "pattern": r"\bkeras\b",
        "category": "named_technology",
        "routing_weight": 8,
        "ranking_weight": 7,
        "case_sensitive": False,
    },
}

# Compatibility alias for callers that only need the published regex strings.
AI_KEYWORD_PATTERNS = [str(spec["pattern"]) for spec in AI_KEYWORDS.values()]


@dataclass(frozen=True)
class KeywordMatch:
    """One validated dictionary match with routing and ranking metadata."""

    keyword: str
    category: str
    routing_weight: int
    ranking_weight: int
    start: int
    end: int
    text: str


@dataclass(frozen=True)
class TextSegment:
    """One structurally delimited filing segment."""

    text: str
    position: int


@dataclass(frozen=True)
class AnchorCandidate:
    """An AI anchor and the internal signals used to rank it."""

    anchor_text: str
    previous_text: str
    next_text: str
    matched_keyword: str
    keyword_category: str
    keyword_weight: int
    routing_weight: int
    operational_score: int
    current_use_score: int
    negation_or_future_score: int
    strong_current_use: bool
    position: int
    match_start: int
    match_end: int
    total_score: int
    quality_band: int


ORDERED_AI_KEYWORD_NAMES = sorted(
    AI_KEYWORDS,
    key=lambda name: (
        -int(AI_KEYWORDS[name]["ranking_weight"]),
        -len(str(AI_KEYWORDS[name]["pattern"])),
        name,
    ),
)

COMPILED_AI_KEYWORD_RULES = {
    name: re.compile(
        str(AI_KEYWORDS[name]["pattern"]),
        0 if bool(AI_KEYWORDS[name].get("case_sensitive", False)) else re.IGNORECASE,
    )
    for name in ORDERED_AI_KEYWORD_NAMES
}

AI_KEYWORD_NAMES_BY_CASE = {
    case_sensitive: [
        name
        for name in ORDERED_AI_KEYWORD_NAMES
        if bool(AI_KEYWORDS[name].get("case_sensitive", False)) == case_sensitive
    ]
    for case_sensitive in (False, True)
}


def _compile_ai_keyword_unions() -> dict[bool, re.Pattern[str]]:
    """Compile fast case-specific unions derived from the central dictionary."""

    unions: dict[bool, re.Pattern[str]] = {}
    for case_sensitive in (False, True):
        patterns = [
            str(AI_KEYWORDS[name]["pattern"])
            for name in AI_KEYWORD_NAMES_BY_CASE[case_sensitive]
        ]
        unions[case_sensitive] = re.compile(
            "|".join(patterns),
            0 if case_sensitive else re.IGNORECASE,
        )
    return unions


COMPILED_AI_KEYWORDS = _compile_ai_keyword_unions()

# These words help rank snippets. They do not determine the final label.
# They only make operational evidence more likely to be sent to the LLM.
OPERATIONAL_CUES = re.compile(
    r"(?i)(?:\b(?:"
    r"use|uses|used|using|utilise|utilises|utilised|utilising|"
    r"utilize|utilizes|utilized|utilizing|deploy|deploys|deployed|deploying|deployment|"
    r"implement|implements|implemented|implementing|integrate|integrates|integrated|integrating|"
    r"incorporate|incorporates|incorporated|incorporating|apply|applies|applied|applying|"
    r"leverage|leverages|leveraged|leveraging|embed|embeds|embedded|operate|operates|operated|"
    r"automate|automates|automated|automating|recommend|recommends|detect|detects|detected|"
    r"forecast|forecasts|forecasting|predict|predicts|predicting|underwrite|underwrites|"
    r"optimize|optimizes|optimized|optimizing|optimise|optimises|optimised|optimising|"
    r"personalize|personalizes|personalized|personalizing|personalise|personalises|"
    r"develop|develops|developed|developing|introduce|introduces|introduced|launch|launches|launched"
    r")\b|\b(?:powered|enabled)\s+by\b|\bbuilt\s+with\b|\bin\s+production\b)"
)

# These words are common in risk factors, future plans, or generic discussion.
# Snippets dominated by these cues are down-ranked during excerpt selection.
LOW_VALUE_CUES = re.compile(
    r"(?i)\b("
    r"risk|risks|could|may|might|intend|intends|plan|plans|future|potential|"
    r"regulation|regulatory|competition|competitors|cybersecurity|trend|trends|market|markets|"
    r"opportunity|opportunities|demand"
    r")\b"
)

FUTURE_OR_HYPOTHETICAL_CUES = re.compile(
    r"(?i)\b(?:plan(?:s|ned|ning)?\s+to|intend(?:s|ed|ing)?\s+to|expect(?:s|ed|ing)?\s+to|"
    r"may\s+(?:use|deploy|implement|adopt|apply)|might\s+(?:use|deploy|implement|adopt|apply)|"
    r"explor(?:e|es|ed|ing)|consider(?:s|ed|ing)?|potential(?:ly)?|in the future|looking forward)\b"
)

FIRM_CONTEXT_CUES = re.compile(
    r"(?i)\b(?:we|our|us|the company|company's|company’s|platform|product|products|service|services|"
    r"operations|employees|customers|clients|workflow|process|processes|system|systems)\b"
)

PRODUCTION_OR_STRATEGIC_CUES = re.compile(
    r"(?i)\b(?:in production|production-level|commerciali[sz](?:e|es|ed|ing|ation)|ongoing|"
    r"revenue|cost savings?|productivity|efficien(?:cy|cies)|strategy|strategic|at scale|enterprise-wide|"
    r"across (?:the )?(?:business|company|operations)|mission-critical)\b"
)

INDUSTRY_OR_RISK_CUES = re.compile(
    r"(?i)\b(?:industry|industries|competitor|competitors|competitive environment|third[- ]party|"
    r"regulation|regulatory|law|laws|risk|risks|cyberattack|cyber attack|threat actor|market trend)\b"
)

NEGATION_CUES = re.compile(
    r"(?i)\b(?:do not|does not|did not|not currently|no current|without)\s+"
    r"(?:use|using|deploy|deploying|implement|implementing|adopt|adopting|apply|applying|"
    r"integrate|integrating|incorporate|incorporating|leverage|leveraging)\b"
)

# Strong, explicit current-use formulations should outrank generic risk-factor
# sentences even when the latter spell out "artificial intelligence" and the
# operational sentence uses only the shorter "AI" abbreviation.
STRONG_CURRENT_USE_CUES = re.compile(
    r"(?i)(?:"
    r"\b(?:we|the company)\s+(?:(?:are|have)\s+)?(?:also\s+)?(?:currently\s+)?"
    r"(?:use|uses|using|utilize|utilizes|utilizing|deploy|deploys|deploying|"
    r"implement|implements|implementing|integrate|integrates|integrating|"
    r"leverage|leverages|leveraging)\b|"
    r"\bAI[- ](?:powered|enabled)\b|"
    r"\b(?:powered|enabled)\s+by\s+(?:AI|artificial intelligence)\b|"
    r"\bour\s+(?:AI|artificial intelligence)[- ](?:powered|enabled)\b"
    r")"
)

OTHER_FIRM_CUES = re.compile(
    r"(?i)\b(?:competitor|competitors|other companies|other firms|third[- ]party)\b"
)

BACKUP_AUDIT_CUES = re.compile(
    r"(?i)\b(?:technology|technologies|automation|automated|analytics|algorithm|algorithms|"
    r"predictive analytics|predictive systems?|intelligent systems?|intelligent automation|"
    r"data science|decision engine|recommendation engine|computer[- ]assisted|autonomous systems?)\b"
)

AI_SCORE_LABELS = {
    1: "no_disclosed_current_implementation",
    2: "emerging_or_bounded_implementation",
    3: "established_and_integrated_implementation",
}

SNIPPET_AUDIT_COLUMNS = [
    "cik",
    "year",
    "filing_accession",
    "chunk_id",
    "llm_processed",
    "snippet_text_length",
    "max_prompt_chars",
    "snippet_at_limit",
    "keyword_hits",
    "prefilter_decision",
    "parse_status",
    "ai_score",
    "score_status",
    "snippet_text",
]

RETRYABLE_PARSE_STATUSES = {
    "missing_output",
    "no_score_found",
    "invalid_score",
    "conflicting_scores",
}

SCORE_TOKEN_RE = re.compile(r"\b([123])\b")


# ---------------------------------------------------------------------------
# Lightweight data structures
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class ChunkRef:
    """A chunk file available from a configured local or AWS S3 source."""

    name: str
    path: str


# ---------------------------------------------------------------------------
# Small generic helpers
# ---------------------------------------------------------------------------
# These helpers keep formatting, hashing, CIK matching, and keyword counting
# consistent across the main script and output QA.
def utc_now() -> str:
    """Return a compact UTC timestamp used as the run id."""

    return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")


def sha256_text(text: str) -> str:
    """Hash text so snippets can be audited without storing the full text."""

    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def stable_unit_interval(value: str) -> float:
    """Map a string to a stable 0-to-1 value for reproducible audit sampling."""

    digest = hashlib.sha256(value.encode("utf-8")).hexdigest()
    return int(digest[:16], 16) / float(16**16)


def normalize_whitespace(text: Any) -> str:
    """Convert missing values to empty strings and collapse noisy whitespace."""

    if text is None:
        return ""
    try:
        if bool(pd.isna(text)):
            return ""
    except (TypeError, ValueError):
        pass
    return WHITESPACE_RE.sub(" ", str(text).replace("\x00", " ")).strip()


def normalize_structured_text(text: Any) -> str:
    """Clean filing text while preserving newlines as structural boundaries."""

    if text is None:
        return ""
    try:
        if bool(pd.isna(text)):
            return ""
    except (TypeError, ValueError):
        pass
    cleaned = str(text).replace("\x00", " ").replace("\r\n", "\n").replace("\r", "\n")
    cleaned = BULLET_RE.sub("\n", cleaned)
    cleaned = TABLE_SEPARATOR_RE.sub("\n", cleaned)
    cleaned = INLINE_LIST_ITEM_RE.sub("\n", cleaned)
    lines = [
        HORIZONTAL_WHITESPACE_RE.sub(" ", line).strip() for line in cleaned.split("\n")
    ]
    cleaned = "\n".join(lines)
    return EXCESS_NEWLINES_RE.sub("\n\n", cleaned).strip()


def normalize_cik(value: Any) -> str:
    """Normalize CIK values so 1750 and 0001750 match the same firm."""

    if value is None or (isinstance(value, float) and pd.isna(value)):
        return ""
    text = str(value).strip()
    if text.endswith(".0"):
        text = text[:-2]
    text = re.sub(r"\D", "", text)
    return text.lstrip("0") or ("0" if text else "")


def coerce_bool(value: Any) -> bool:
    """Convert scalar CSV-style boolean values without treating 'False' as true."""

    if value is None or pd.isna(value):
        return False
    return str(value).strip().lower() in {"true", "1", "yes"}


def count_ai_keywords(text: str) -> int:
    """Count validated, routing-eligible AI dictionary matches."""

    return len(find_keyword_matches(text))


def _local_match_context(text: str, start: int, end: int, radius: int = 220) -> str:
    """Return compact text around a match for ambiguity checks."""

    return text[max(0, start - radius) : min(len(text), end + radius)]


def _keyword_match_allowed(
    keyword: str,
    text: str,
    start: int,
    end: int,
) -> bool:
    """Apply narrow disambiguation rules to otherwise valid regex matches."""

    spec = AI_KEYWORDS[keyword]
    if int(spec.get("routing_weight", 0)) < MIN_ROUTING_WEIGHT:
        return False
    disambiguator = str(spec.get("disambiguator", ""))
    if not disambiguator:
        return int(spec.get("routing_weight", 0)) > 0

    context = _local_match_context(text, start, end)
    if disambiguator == "ai":
        # SEC demographic tables commonly define AI as American Indian(s).
        return not bool(
            re.search(r"\bAI\s*=\s*American Indians?\b", context, re.IGNORECASE)
        )
    if disambiguator == "transformer":
        return bool(
            re.search(
                r"(?i)\b(?:AI|ML|artificial intelligence|machine learning|deep learning|neural|"
                r"attention|encoder|decoder|language model|vision transformer|transformer[- ]based|"
                r"transformer (?:model|models|architecture|architectures|network|networks))\b",
                context,
            )
        )
    if disambiguator == "claude":
        if re.search(r"(?i)\bClaude Bernard\b", context):
            return False
        return bool(
            re.search(
                r"(?i)\b(?:Anthropic|AI|artificial intelligence|LLM|language model|assistant|chatbot|"
                r"Claude (?:API|model|models|Sonnet|Haiku|Opus|\d)|"
                r"(?:use|uses|used|using|deploy|deploys|deployed|integrate|integrates|integrated|"
                r"implement|implements|implemented|leverage|leverages|leveraged) Claude)\b",
                context,
            )
        )
    if disambiguator == "gemini":
        if re.search(r"(?i)\bCap Gemini\b", context):
            return False
        return bool(
            re.search(
                r"(?i)\b(?:Google|AI|artificial intelligence|generative|LLM|language model|assistant|"
                r"chatbot|Gemini (?:API|model|models|assistant|\d)|"
                r"(?:use|uses|used|using|deploy|deploys|deployed|integrate|integrates|integrated|"
                r"implement|implements|implemented|leverage|leverages|leveraged) Gemini)\b",
                context,
            )
        )
    if disambiguator == "copilot":
        return bool(
            re.search(
                r"(?i)\b(?:(?:Microsoft|GitHub|AI|artificial intelligence|generative)\b[^.]{0,80}\bCopilots?|"
                r"Copilots?\s+(?:assistant|software|application|applications|tool|tools)|"
                r"(?:use|uses|used|using|deploy|deploys|deployed|integrate|integrates|integrated|"
                r"implement|implements|implemented|leverage|leverages|leveraged) (?:a )?Copilots?)\b",
                context,
            )
        )
    if disambiguator == "learning_model":
        # Bare educational/business "learning model" language is not an AI
        # trigger. A nearby explicit term will route the filing independently.
        return bool(
            re.search(
                r"(?i)\b(?:AI|artificial intelligence|machine learning|deep learning|neural network|"
                r"language model|LLM)\b",
                context,
            )
        ) and not bool(
            re.search(
                r"(?i)\b(?:remote|hybrid|educational|instructional|in-person|school|campus)\s+learning models?\b",
                context,
            )
        )
    return int(spec.get("routing_weight", 0)) > 0


def find_keyword_matches(text: str) -> list[KeywordMatch]:
    """Find deterministic, non-overlapping matches from the central dictionary."""

    if not text:
        return []

    candidates: list[KeywordMatch] = []
    for case_sensitive, union in COMPILED_AI_KEYWORDS.items():
        rule_names = AI_KEYWORD_NAMES_BY_CASE[case_sensitive]
        for match in union.finditer(text):
            keyword = next(
                name
                for name in rule_names
                if COMPILED_AI_KEYWORD_RULES[name].fullmatch(match.group(0))
            )
            spec = AI_KEYWORDS[keyword]
            if not _keyword_match_allowed(keyword, text, match.start(), match.end()):
                continue
            candidates.append(
                KeywordMatch(
                    keyword=keyword,
                    category=str(spec["category"]),
                    routing_weight=int(spec["routing_weight"]),
                    ranking_weight=int(spec["ranking_weight"]),
                    start=match.start(),
                    end=match.end(),
                    text=match.group(0),
                )
            )

    # Resolve overlaps between the case-sensitive and insensitive unions.
    candidates.sort(
        key=lambda item: (
            item.start,
            -item.ranking_weight,
            -(item.end - item.start),
            item.keyword,
        )
    )
    selected: list[KeywordMatch] = []
    for candidate in candidates:
        if selected and candidate.start < selected[-1].end:
            continue
        selected.append(candidate)
    return selected


def chunk_name_from_id(chunk_id: int) -> str:
    """Convert chunk id 1 to extract_df_chunk_00001.rds."""

    if chunk_id < 1:
        raise ValueError("Chunk ids must be positive integers.")
    return f"extract_df_chunk_{chunk_id:05d}.rds"


# ---------------------------------------------------------------------------
# Output formatting helpers
# ---------------------------------------------------------------------------
# These functions keep the per-filing CSV columns stable and write JSON summary
# files in a readable format.
def preferred_output_columns(save_raw_json: bool) -> list[str]:
    """Return the preferred CSV column order."""

    cols = [
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
        "source_label",
        "chunk_id",
        "accession_number",
        "filing_accession",
        "cik",
        "year",
        "form_type",
        "combined_chars",
        "keyword_hits",
        "prefilter_mode",
        "prefilter_decision",
        "prefilter_audit_sample",
        "snippet_chars",
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
        "snippet_sha256",
        "raw_json_sha256",
    ]
    if save_raw_json:
        cols.append("raw_response")
        cols.append("raw_json")
    return cols


def order_columns(df: pd.DataFrame, save_raw_json: bool) -> pd.DataFrame:
    """Put key QA columns first while preserving any extra columns at the end."""

    preferred = preferred_output_columns(save_raw_json)
    if df.empty:
        return pd.DataFrame(columns=preferred)
    existing = [col for col in preferred if col in df.columns]
    extra = [col for col in df.columns if col not in existing]
    return df[existing + extra]


def build_snippet_audit_table(
    records: Sequence[dict[str, Any]],
    pending: dict[str, dict[str, Any]],
) -> pd.DataFrame:
    """Build the filing-level snippet table used for manual inspection.

    ``llm_processed`` records whether the filing was submitted to the LLM. Rows
    skipped by the prefilter remain in the table with an empty snippet and a
    zero snippet length.
    """

    snippet_by_record_index = {
        int(item["record_index"]): str(item.get("snippet_text", "") or "")
        for item in pending.values()
    }
    rows: list[dict[str, Any]] = []
    for record_index, record in enumerate(records):
        snippet_text = snippet_by_record_index.get(record_index, "")
        llm_processed = coerce_bool(record.get("llm_called", False))
        max_prompt_chars = int(record.get("max_prompt_chars", 0) or 0)
        rows.append(
            {
                "cik": record.get("cik", ""),
                "year": record.get("year", pd.NA),
                "filing_accession": record.get(
                    "filing_accession", record.get("accession_number", "")
                ),
                "chunk_id": record.get("chunk_id", ""),
                "llm_processed": llm_processed,
                "snippet_text_length": len(snippet_text),
                "max_prompt_chars": max_prompt_chars,
                "snippet_at_limit": bool(
                    llm_processed
                    and max_prompt_chars > 0
                    and len(snippet_text) >= max_prompt_chars
                ),
                "keyword_hits": int(record.get("keyword_hits", 0) or 0),
                "prefilter_decision": record.get("prefilter_decision", ""),
                "parse_status": record.get("parse_status", ""),
                "ai_score": record.get("ai_score", pd.NA),
                "score_status": record.get("score_status", ""),
                "snippet_text": snippet_text,
            }
        )

    return pd.DataFrame(rows, columns=SNIPPET_AUDIT_COLUMNS)


def write_json(path: str, payload: dict[str, Any]) -> None:
    """Write a JSON file with stable formatting."""

    with open(path, "w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, ensure_ascii=False, default=str)


# ---------------------------------------------------------------------------
# Local and AWS S3 input helpers
# ---------------------------------------------------------------------------
def _add_chunk_ref(
    refs: dict[str, ChunkRef], *, name: str, path: str, source_description: str
) -> None:
    """Add a uniquely named chunk to a source listing."""

    if not CHUNK_RE.match(name):
        return
    if name in refs and refs[name].path != path:
        raise ValueError(
            f"Duplicate chunk basename under {source_description}: {name!r} "
            f"exists at both {refs[name].path!r} and {path!r}. Select an input "
            "location containing only one extraction run."
        )
    refs[name] = ChunkRef(name=name, path=path)


def list_local_chunks(input_dir: str) -> dict[str, ChunkRef]:
    """List valid RDS chunks recursively beneath a local input directory."""

    root = Path(input_dir).expanduser()
    if not root.is_dir():
        raise ValueError(f"Local input directory does not exist: {input_dir}")

    refs: dict[str, ChunkRef] = {}
    for path in sorted(root.rglob("*.rds")):
        _add_chunk_ref(
            refs,
            name=path.name,
            path=str(path.resolve()),
            source_description="the selected local input directory",
        )
    return dict(sorted(refs.items()))


def list_s3_chunks(
    bucket: Optional[str],
    prefix: str = "",
    *,
    region_name: Optional[str] = None,
    s3_client: Any = None,
) -> dict[str, ChunkRef]:
    """List valid RDS chunks beneath a generic AWS S3 prefix."""

    if not bucket:
        raise ValueError(
            "No S3 bucket configured. Supply --s3-bucket or set "
            "AI_ADOPTION_S3_BUCKET."
        )
    if s3_client is None:
        import boto3

        kwargs = {"region_name": region_name} if region_name else {}
        s3_client = boto3.client("s3", **kwargs)

    clean_prefix = prefix.strip("/")
    list_prefix = f"{clean_prefix}/" if clean_prefix else ""
    paginator = s3_client.get_paginator("list_objects_v2")
    refs: dict[str, ChunkRef] = {}
    for page in paginator.paginate(Bucket=bucket, Prefix=list_prefix):
        for obj in page.get("Contents", []):
            path = str(obj.get("Key", ""))
            _add_chunk_ref(
                refs,
                name=Path(path).name,
                path=path,
                source_description="the selected S3 prefix",
            )
    return dict(sorted(refs.items()))


# ---------------------------------------------------------------------------
# Chunk selection helpers
# ---------------------------------------------------------------------------
# Users can select chunks by exact filename, numeric id, numeric range, or all
# available chunks. This function turns that choice into concrete ChunkRef rows.
def select_chunks(
    available: dict[str, ChunkRef],
    chunk_names: Optional[Sequence[str]],
    chunk_ids: Optional[Sequence[int]],
    chunk_range: Optional[Sequence[int]],
    all_chunks: bool,
    max_chunks: Optional[int],
) -> list[ChunkRef]:
    if chunk_names:
        names = [Path(name).name for name in chunk_names]
        bad = [name for name in names if not CHUNK_RE.match(name)]
        if bad:
            raise ValueError(f"Invalid chunk filename(s): {bad}")
    elif chunk_ids:
        names = [chunk_name_from_id(int(chunk_id)) for chunk_id in chunk_ids]
    elif chunk_range:
        start, end = int(chunk_range[0]), int(chunk_range[1])
        if end < start:
            raise ValueError("--chunk-range requires START <= END")
        names = [chunk_name_from_id(chunk_id) for chunk_id in range(start, end + 1)]
    elif all_chunks:
        selected = list(available.values())
        return selected[:max_chunks] if max_chunks else selected
    else:
        raise ValueError(
            "No chunks selected. Use --chunk-ids, --chunk-range, --chunk-names, or --all-chunks."
        )

    selected = []
    seen = set()
    for name in names:
        if name not in available:
            raise FileNotFoundError(f"Chunk not found in the selected input: {name}")
        ref = available[name]
        if ref.path not in seen:
            selected.append(ref)
            seen.add(ref.path)

    return selected[:max_chunks] if max_chunks else selected


# ---------------------------------------------------------------------------
# RDS reading
# ---------------------------------------------------------------------------
def _first_rds_dataframe(result: dict[Any, Any], source_name: str) -> pd.DataFrame:
    """Return the first data frame from a pyreadr result."""

    if not result:
        raise ValueError(f"No readable objects found in {source_name}")
    df = next(iter(result.values()))
    if not isinstance(df, pd.DataFrame):
        raise TypeError(f"First object in {source_name} is not a DataFrame")
    return df


def read_rds_from_local(path: str) -> pd.DataFrame:
    """Read one RDS chunk from a local path."""

    import pyreadr

    return _first_rds_dataframe(pyreadr.read_r(path), Path(path).name)


def read_rds_from_s3(
    path: str,
    *,
    bucket: Optional[str],
    region_name: Optional[str] = None,
    s3_client: Any = None,
) -> pd.DataFrame:
    """Read one RDS object through the standard boto3 S3 client."""

    if not bucket:
        raise ValueError(
            "No S3 bucket configured. Supply --s3-bucket or set "
            "AI_ADOPTION_S3_BUCKET."
        )
    if s3_client is None:
        import boto3

        kwargs = {"region_name": region_name} if region_name else {}
        s3_client = boto3.client("s3", **kwargs)

    body = s3_client.get_object(Bucket=bucket, Key=path)["Body"].read()
    import pyreadr

    with tempfile.NamedTemporaryFile(suffix=".rds", delete=True) as tmp:
        tmp.write(body)
        tmp.flush()
        result = pyreadr.read_r(tmp.name)
    return _first_rds_dataframe(result, Path(path).name)


# ---------------------------------------------------------------------------
# Lookup loading and filing-level data reshaping
# ---------------------------------------------------------------------------
# The lookup filter is where the research sample enters the process. The LLM is
# only called for filing rows that survive this cik/year filter.
def load_cik_year_lookup(path: Optional[str]) -> Optional[pd.DataFrame]:
    """Read the optional research lookup and normalize cik/year match keys."""

    if not path:
        return None

    lookup = pd.read_csv(path)
    lookup.columns = [str(col).strip().lower() for col in lookup.columns]
    missing = {"cik", "year"} - set(lookup.columns)
    if missing:
        raise ValueError(f"Lookup CSV is missing required columns: {sorted(missing)}")

    lookup = lookup[["cik", "year"]].copy()
    lookup["cik_match"] = lookup["cik"].apply(normalize_cik)
    lookup["year_match"] = pd.to_numeric(lookup["year"], errors="coerce").astype(
        "Int64"
    )
    lookup = lookup[(lookup["cik_match"] != "") & lookup["year_match"].notna()]
    lookup = lookup.drop_duplicates(["cik_match", "year_match"]).reset_index(drop=True)
    if lookup.empty:
        raise ValueError(f"Lookup CSV contains no usable cik/year pairs: {path}")

    logging.info("Loaded lookup %s with %d unique cik/year pairs", path, len(lookup))
    return lookup[["cik_match", "year_match"]]


def filter_to_analysis_year(df: pd.DataFrame, max_year: int) -> pd.DataFrame:
    """Exclude filings after the configured terminal analysis year."""

    if max_year <= 0 or df.empty:
        return df.copy()
    years = pd.to_numeric(df["year"], errors="coerce")
    return df[years.le(max_year)].copy().reset_index(drop=True)


def long_to_wide(df: pd.DataFrame) -> pd.DataFrame:
    """Join whole-filing keyword windows into one row per filing."""

    required = {"item", "year", "accession_number", "cik", "form_type", "text"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")

    # Standardise key fields before filtering and grouping.
    work = df.copy()
    work["item"] = work["item"].astype(str).str.strip().str.lower()
    work["form_type"] = work["form_type"].astype(str).str.upper().str.strip()
    work["accession_number"] = work["accession_number"].astype(str).str.strip()
    work["cik"] = work["cik"].astype(str).str.strip()
    work["text"] = work["text"].apply(normalize_structured_text)

    base_cols = [
        "accession_number",
        "cik",
        "year",
        "form_type",
        "combined_text",
        "combined_chars",
    ]
    window_mask = work["item"].eq("keyword_window")
    if not bool(window_mask.all()):
        unsupported = sorted(set(work.loc[~window_mask, "item"]))
        raise ValueError(
            "Only whole-filing keyword_window input is supported; "
            f"found item values: {unsupported}"
        )

    work = work[work["form_type"].isin({"10-K", "10-K/A"})]

    if work.empty:
        return pd.DataFrame(columns=base_cols)

    windows = work.copy()
    windows["_input_order"] = range(len(windows))
    order_cols = ["accession_number", "cik", "year", "form_type"]
    if "window_id" in windows.columns:
        windows["_window_order"] = pd.to_numeric(
            windows["window_id"], errors="coerce"
        )
    elif "sentence_start" in windows.columns:
        windows["_window_order"] = pd.to_numeric(
            windows["sentence_start"], errors="coerce"
        )
    else:
        windows["_window_order"] = windows["_input_order"]
    windows = windows.sort_values(
        order_cols + ["_window_order", "_input_order"], kind="stable"
    )

    def join_exact_unique(values: pd.Series) -> str:
        seen: set[str] = set()
        kept: list[str] = []
        for value in values:
            text = str(value).strip()
            if text and text not in seen:
                seen.add(text)
                kept.append(text)
        return "\n\n".join(kept)

    wide = windows.groupby(order_cols, as_index=False, sort=False).agg(
        window_text=("text", join_exact_unique)
    )
    wide["year"] = pd.to_numeric(wide["year"], errors="coerce").astype("Int64")
    wide["combined_text"] = wide["window_text"]
    wide["combined_chars"] = wide["combined_text"].str.len()
    return wide[base_cols].sort_values(["accession_number"]).reset_index(drop=True)


def filter_to_lookup(
    wide: pd.DataFrame, lookup: Optional[pd.DataFrame]
) -> pd.DataFrame:
    """Keep only filing rows whose normalized cik/year appears in the lookup."""

    if lookup is None or wide.empty:
        return wide

    work = wide.copy()
    work["cik_match"] = work["cik"].apply(normalize_cik)
    work["year_match"] = pd.to_numeric(work["year"], errors="coerce").astype("Int64")
    filtered = work.merge(lookup, on=["cik_match", "year_match"], how="inner")
    filtered = filtered.drop(columns=["cik_match", "year_match"])
    dedupe_cols = [
        col
        for col in ["accession_number", "cik", "year", "form_type"]
        if col in filtered.columns
    ]
    filtered = filtered.drop_duplicates(dedupe_cols)
    return filtered.reset_index(drop=True)


# ---------------------------------------------------------------------------
# Snippet extraction
# ---------------------------------------------------------------------------
# Full filings are too long for practical prompting. The staged extractor below
# selects and preserves individual keyword anchors first, then spends only the
# remaining budget on nearby context. Window size therefore cannot evict an
# anchor that was already selected.
def _safe_long_segment_cut(text: str, limit: int) -> int:
    """Choose a readable cut point without splitting a dictionary expression."""

    if len(text) <= limit:
        return len(text)
    lower_bound = max(1, int(limit * 0.55))
    breakpoints = [
        match.end()
        for match in re.finditer(r"(?:[,|:]\s+|\s+)", text[: limit + 1])
        if match.end() >= lower_bound
    ]
    cut = breakpoints[-1] if breakpoints else limit
    for match in find_keyword_matches(text[: limit + 80]):
        if match.start < cut < match.end:
            before = text.rfind(" ", lower_bound, match.start)
            cut = before + 1 if before >= lower_bound else min(match.end, limit)
            break
    return max(1, cut)


def _split_long_text_unit(text: str) -> list[str]:
    """Split unusually long SEC list/table units into prompt-safe segments."""

    remaining = normalize_whitespace(text)
    parts: list[str] = []
    while len(remaining) > MAX_TEXT_SEGMENT_CHARS:
        cut = _safe_long_segment_cut(remaining, MAX_TEXT_SEGMENT_CHARS)
        part = remaining[:cut].strip(" ,|;:")
        if part:
            parts.append(part)
        remaining = remaining[cut:].strip()
    if remaining:
        parts.append(remaining)
    return parts


def normalize_and_segment_text(text: str) -> list[TextSegment]:
    """Preserve filing structure, then create bounded sentence-like segments."""

    structured = normalize_structured_text(text)
    if not structured:
        return []

    segments: list[TextSegment] = []
    for raw_line in structured.split("\n"):
        line = raw_line.strip()
        if not line:
            continue
        for raw_part in SENTENCE_SPLIT_RE.split(line):
            part = raw_part.strip()
            if not part:
                continue
            for bounded_part in _split_long_text_unit(part):
                segments.append(
                    TextSegment(
                        text=bounded_part,
                        position=len(segments),
                    )
                )
    return segments


def _distance_between_spans(
    left_start: int,
    left_end: int,
    right_start: int,
    right_end: int,
) -> int:
    if left_end < right_start:
        return right_start - left_end
    if right_end < left_start:
        return left_start - right_end
    return 0


def _linked_operational_score(text: str, matches: Sequence[KeywordMatch]) -> int:
    """Reward operational language only when it is close to an AI match."""

    best_distance: Optional[int] = None
    for cue in OPERATIONAL_CUES.finditer(text):
        for match in matches:
            distance = _distance_between_spans(
                cue.start(), cue.end(), match.start, match.end
            )
            best_distance = (
                distance if best_distance is None else min(best_distance, distance)
            )
    if best_distance is None:
        return 0
    if best_distance <= 80:
        return 6
    if best_distance <= 180:
        return 4
    if best_distance <= 300:
        return 2
    return 0


def create_anchor_candidates(segments: Sequence[TextSegment]) -> list[AnchorCandidate]:
    """Create one metadata-rich anchor candidate per matched text segment."""

    candidates: list[AnchorCandidate] = []
    for index, segment in enumerate(segments):
        matches = find_keyword_matches(segment.text)
        if not matches:
            continue
        primary = sorted(
            matches,
            key=lambda item: (
                -item.ranking_weight,
                -item.routing_weight,
                item.start,
                item.keyword,
            ),
        )[0]
        operational_score = _linked_operational_score(segment.text, matches)
        future_or_hypothetical = bool(FUTURE_OR_HYPOTHETICAL_CUES.search(segment.text))
        low_value = bool(LOW_VALUE_CUES.search(segment.text))
        industry_or_risk = bool(INDUSTRY_OR_RISK_CUES.search(segment.text))
        negated_use = bool(NEGATION_CUES.search(segment.text))
        firm_context = bool(FIRM_CONTEXT_CUES.search(segment.text))
        production_or_strategic = bool(
            PRODUCTION_OR_STRATEGIC_CUES.search(segment.text)
        )
        strong_current_use = bool(STRONG_CURRENT_USE_CUES.search(segment.text))
        if future_or_hypothetical or negated_use or OTHER_FIRM_CUES.search(segment.text):
            strong_current_use = False

        current_use_score = 0
        if operational_score and firm_context and not negated_use:
            current_use_score += 3
        if production_or_strategic:
            current_use_score += 3
        if re.search(
            r"(?i)\b(?:currently|continue|continues|ongoing|have|has)\b", segment.text
        ):
            current_use_score += 1

        negation_or_future_score = 0
        if future_or_hypothetical:
            negation_or_future_score -= 4
        if low_value:
            negation_or_future_score -= 1 if operational_score else 2
        if industry_or_risk:
            negation_or_future_score -= 2 if operational_score else 3
        if negated_use:
            negation_or_future_score -= 6

        total_score = (
            primary.ranking_weight * 10
            + operational_score
            + current_use_score
            + negation_or_future_score
        )
        current_link = (
            operational_score > 0 and not future_or_hypothetical and not negated_use
        )
        risk_or_speculative = (
            future_or_hypothetical
            or negated_use
            or ((low_value or industry_or_risk) and not strong_current_use)
        )
        if strong_current_use:
            quality_band = 5
        elif (
            current_link
            and firm_context
            and not risk_or_speculative
            and primary.ranking_weight >= 8
        ):
            quality_band = 4
        elif current_link and not risk_or_speculative:
            quality_band = 3
        elif primary.ranking_weight >= 7:
            quality_band = 2
        elif negation_or_future_score < 0:
            quality_band = 1
        else:
            quality_band = 0

        candidates.append(
            AnchorCandidate(
                anchor_text=segment.text,
                previous_text=(
                    segments[index - 1].text if index > 0 else ""
                ),
                next_text=(
                    segments[index + 1].text
                    if index + 1 < len(segments) else ""
                ),
                matched_keyword=primary.keyword,
                keyword_category=primary.category,
                keyword_weight=primary.ranking_weight,
                routing_weight=primary.routing_weight,
                operational_score=operational_score,
                current_use_score=current_use_score,
                negation_or_future_score=negation_or_future_score,
                strong_current_use=strong_current_use,
                position=segment.position,
                match_start=primary.start,
                match_end=primary.end,
                total_score=total_score,
                quality_band=quality_band,
            )
        )
    return candidates


def score_anchor_candidates(
    candidates: Sequence[AnchorCandidate],
) -> list[AnchorCandidate]:
    """Return candidates in deterministic evidence-priority order."""

    return sorted(
        candidates,
        key=lambda candidate: (
            -candidate.quality_band,
            -candidate.total_score,
            -candidate.keyword_weight,
            -candidate.routing_weight,
            -len(candidate.anchor_text),
            candidate.position,
        ),
    )


def _dedupe_tokens(text: str) -> list[str]:
    """Normalize common filing noise for exact and near-duplicate checks."""

    cleaned = re.sub(
        r"(?i)\b(?:table of contents|index to form 10-k|index to fs)\b", " ", text
    )
    tokens = re.findall(r"[a-z]+", normalize_whitespace(cleaned).lower())
    ignored = {"the", "we", "our", "company"}
    canonical = {
        "uses": "use",
        "used": "use",
        "using": "use",
        "utilises": "utilise",
        "utilised": "utilise",
        "utilising": "utilise",
        "utilizes": "utilize",
        "utilized": "utilize",
        "utilizing": "utilize",
    }
    return [canonical.get(token, token) for token in tokens if token not in ignored]


def _near_duplicate_text(left: str, right: str) -> bool:
    """Detect lightweight sentence-level near duplicates without dependencies."""

    left_tokens = _dedupe_tokens(left)
    right_tokens = _dedupe_tokens(right)
    if left_tokens == right_tokens:
        return True
    if min(len(left_tokens), len(right_tokens)) < 8:
        return False
    length_ratio = min(len(left_tokens), len(right_tokens)) / max(
        len(left_tokens), len(right_tokens)
    )
    if length_ratio < 0.65:
        return False
    left_set, right_set = set(left_tokens), set(right_tokens)
    jaccard = len(left_set & right_set) / max(1, len(left_set | right_set))
    shared_prefix = left_tokens[:12] == right_tokens[:12]
    sequence_ratio = SequenceMatcher(None, left_tokens, right_tokens).ratio()
    return (
        jaccard >= 0.82
        or sequence_ratio >= 0.84
        or (shared_prefix and length_ratio >= 0.75)
    )


def deduplicate_candidates(
    candidates: Sequence[AnchorCandidate],
) -> list[AnchorCandidate]:
    """Keep the strongest/most complete representative of repeated disclosure."""

    kept: list[AnchorCandidate] = []
    for candidate in score_anchor_candidates(candidates):
        if any(
            _near_duplicate_text(candidate.anchor_text, existing.anchor_text)
            for existing in kept
        ):
            continue
        kept.append(candidate)
    return kept


def _crop_text_around_span(text: str, start: int, end: int, limit: int) -> str:
    """Crop an oversized anchor around its keyword without cutting the anchor."""

    if len(text) <= limit:
        return text
    if limit <= 0:
        return ""
    match_mid = (start + end) // 2
    crop_start = max(0, match_mid - limit // 2)
    crop_end = min(len(text), crop_start + limit)
    crop_start = max(0, crop_end - limit)
    if crop_start:
        next_space = text.find(" ", crop_start, min(len(text), crop_start + 60))
        if next_space != -1 and next_space < start:
            crop_start = next_space + 1
    if crop_end < len(text):
        previous_space = text.rfind(" ", max(crop_start, crop_end - 60), crop_end)
        if previous_space > end:
            crop_end = previous_space
    prefix = "... " if crop_start else ""
    suffix = " ..." if crop_end < len(text) else ""
    available = max(0, limit - len(prefix) - len(suffix))
    return (prefix + text[crop_start : crop_start + available].strip() + suffix)[:limit]


def select_anchor_sentences(
    candidates: Sequence[AnchorCandidate], max_chars: int
) -> list[AnchorCandidate]:
    """Select anchors first, independently of the requested context window."""

    if max_chars <= 0:
        return []
    selected: list[AnchorCandidate] = []
    used = 0
    for candidate in score_anchor_candidates(candidates):
        separator_chars = 2 if selected else 0
        available = max_chars - used - separator_chars
        if available <= 0:
            break
        anchor_text = candidate.anchor_text
        if len(anchor_text) > available:
            if selected:
                continue
            anchor_text = _crop_text_around_span(
                anchor_text,
                candidate.match_start,
                candidate.match_end,
                available,
            )
        selected.append(replace(candidate, anchor_text=anchor_text))
        used += separator_chars + len(anchor_text)
    return selected


def _context_excerpt(text: str, limit: int, *, take_tail: bool) -> str:
    """Trim context at word boundaries while keeping the side nearest the anchor."""

    if len(text) <= limit:
        return text
    if take_tail:
        start = max(0, len(text) - limit)
        boundary = text.find(" ", start, min(len(text), start + 60))
        start = boundary + 1 if boundary != -1 else start
        return ("... " + text[start:].strip())[:limit]
    end = min(len(text), limit)
    boundary = text.rfind(" ", max(0, end - 60), end)
    end = boundary if boundary > 0 else end
    return (text[:end].strip() + " ...")[:limit]


def add_context_within_budget(
    selected: Sequence[AnchorCandidate],
    segments: Sequence[TextSegment],
    max_chars: int,
    sentence_window: int,
) -> list[str]:
    """Add useful neighbouring segments without changing the selected anchors."""

    if not selected:
        return []
    pieces: list[tuple[int, int, str]] = [
        (candidate.position, 1, candidate.anchor_text) for candidate in selected
    ]
    used = sum(len(text) for _, _, text in pieces) + 2 * (len(pieces) - 1)
    if sentence_window <= 0 or used >= max_chars:
        return [text for _, _, text in sorted(pieces)]

    selected_positions = {candidate.position for candidate in selected}
    seen_texts = [candidate.anchor_text for candidate in selected]
    ranked_selected = score_anchor_candidates(selected)
    for candidate in ranked_selected:
        for distance in range(1, sentence_window + 1):
            for direction in (-1, 1):  # previous before next, as documented
                position = candidate.position + direction * distance
                if position < 0 or position >= len(segments):
                    continue
                if position in selected_positions:
                    continue
                context_segment = segments[position]
                context_text = context_segment.text
                if any(_near_duplicate_text(context_text, seen) for seen in seen_texts):
                    continue
                remaining = max_chars - used - 2
                if remaining <= 0:
                    return [text for _, _, text in sorted(pieces)]
                if (
                    len(context_text) > remaining
                    and remaining < MIN_TRUNCATED_CONTEXT_CHARS
                ):
                    continue
                context_text = _context_excerpt(
                    context_text,
                    min(MAX_CONTEXT_SEGMENT_CHARS, remaining),
                    take_tail=direction < 0,
                )
                pieces.append((position, 0 if direction < 0 else 2, context_text))
                seen_texts.append(context_text)
                used += 2 + len(context_text)
    return [text for _, _, text in sorted(pieces)]


def build_final_extraction(parts: Sequence[str], max_chars: int) -> str:
    """Join already-budgeted extraction parts deterministically."""

    return "\n\n".join(part.strip() for part in parts if part.strip())[
        :max_chars
    ].strip()


def _positional_excerpt(text: str, limit: int, position: str) -> str:
    """Take a beginning, middle, or ending excerpt from one structural segment."""

    if len(text) <= limit:
        return text
    if position == "end":
        return _context_excerpt(text, limit, take_tail=True)
    if position == "middle":
        start = max(0, (len(text) - limit) // 2)
        return normalize_whitespace(text[start : start + limit])
    return _context_excerpt(text, limit, take_tail=False)


def run_no_keyword_audit_if_needed(
    segments: Sequence[TextSegment], max_chars: int
) -> str:
    """Build a distributed audit excerpt when the primary dictionary has no hit."""

    if not segments or max_chars <= 0:
        return ""

    proxy_candidates: list[tuple[int, int, str, int, int]] = []
    for segment in segments:
        proxy_matches = list(BACKUP_AUDIT_CUES.finditer(segment.text))
        if not proxy_matches:
            continue
        operational = 2 if OPERATIONAL_CUES.search(segment.text) else 0
        firm_context = 2 if FIRM_CONTEXT_CUES.search(segment.text) else 0
        score = min(12, len(proxy_matches) * 3) + operational + firm_context
        first = proxy_matches[0]
        proxy_candidates.append(
            (score, segment.position, segment.text, first.start(), first.end())
        )

    pieces: list[tuple[int, str]] = []
    seen: list[str] = []
    used = 0
    proxy_budget = int(max_chars * 0.6)
    for _, position, text, start, end in sorted(
        proxy_candidates, key=lambda item: (-item[0], item[1])
    ):
        excerpt = _crop_text_around_span(
            text, start, end, min(MAX_TEXT_SEGMENT_CHARS, proxy_budget)
        )
        if any(_near_duplicate_text(excerpt, existing) for existing in seen):
            continue
        separator_chars = 2 if pieces else 0
        if used + separator_chars + len(excerpt) > proxy_budget:
            continue
        pieces.append((position, excerpt))
        seen.append(excerpt)
        used += separator_chars + len(excerpt)

    # Always sample structurally distinct beginning, middle, and end positions.
    coverage_positions = [
        (0, "beginning"),
        (len(segments) // 2, "middle"),
        (len(segments) - 1, "end"),
    ]
    unique_coverage: list[tuple[int, str]] = []
    seen_positions: set[int] = set()
    for position, label in coverage_positions:
        if position not in seen_positions:
            unique_coverage.append((position, label))
            seen_positions.add(position)

    for offset, (position, label) in enumerate(unique_coverage):
        remaining_slots = len(unique_coverage) - offset
        remaining = max_chars - used - (2 if pieces else 0)
        if remaining <= 0:
            break
        allocation = max(1, remaining // remaining_slots)
        excerpt = _positional_excerpt(
            segments[position].text,
            min(MAX_TEXT_SEGMENT_CHARS, allocation),
            "end" if label == "end" else label,
        )
        if any(_near_duplicate_text(excerpt, existing) for existing in seen):
            continue
        pieces.append((position, excerpt))
        seen.append(excerpt)
        used += (2 if len(pieces) > 1 else 0) + len(excerpt)

    return build_final_extraction([text for _, text in sorted(pieces)], max_chars)


def extract_relevant_snippets(
    full_text: str, max_chars: int, sentence_window: int
) -> str:
    """Return all extracted evidence, or apply an optional positive budget."""

    if max_chars == 0:
        return full_text.strip()

    normalized = normalize_structured_text(full_text)
    if not normalized:
        return ""
    if len(normalized) <= max_chars:
        return normalized

    segments = normalize_and_segment_text(normalized)
    if not segments:
        return ""
    candidates = create_anchor_candidates(segments)
    if not candidates:
        return run_no_keyword_audit_if_needed(segments, max_chars)
    deduplicated = deduplicate_candidates(candidates)
    selected = select_anchor_sentences(deduplicated, max_chars)
    parts = add_context_within_budget(
        selected,
        segments,
        max_chars,
        sentence_window,
    )
    return build_final_extraction(parts, max_chars)


# ---------------------------------------------------------------------------
# Prompt construction
# ---------------------------------------------------------------------------
# The scoring prompt is intentionally short and rigid so Claude can return one
# clean ordinal score with minimal parsing risk.
def _score_rubric() -> str:
    """Return the concise 1-3 AI-adoption decision framework."""

    return (
        "Score 1 - No disclosed current implementation: No concrete current use or "
        "specific active implementation. Plans, exploration, risks, general discussion, "
        "capacity building, and AI activity by other firms do not count.\n"
        "Score 2 - Emerging or bounded implementation: A specific AI application is "
        "in active development, testing, or pilot, or is in limited production use.\n"
        "Score 3 - Established and integrated implementation: Current production or "
        "operational use is explicit and AI is deployed at meaningful scale or embedded "
        "in a core product, service, function, or process.\n\n"
        "AI-enabled products or services currently provided by the firm count.\n\n"
        "If the evidence is ambiguous, choose the lower score.\n"
    )


def build_ai_prompt(text: str) -> str:
    """Build the main Claude-style 1-3 AI-adoption scoring prompt."""

    return (
        "You are an expert analyst. Using the classification rules below, classify the "
        "firm's disclosed AI adoption using only the extracted filing evidence. Do not "
        "rely on outside knowledge.\n\n"
        f"{_score_rubric()}\n"
        "Return only one character: 1, 2, or 3.\n\n"
        "EXTRACTED FILING EVIDENCE:\n"
        "<filing_text>\n"
        f"{text}\n"
        "</filing_text>\n"
    )


def build_ai_retry_prompt(text: str) -> str:
    """Build a stricter retry prompt that asks for only one digit."""

    return (
        "You are an expert analyst. Using the classification rules below, classify the "
        "firm's disclosed AI adoption using only the extracted filing evidence. Do not "
        "rely on outside knowledge.\n\n"
        f"{_score_rubric()}\n"
        "Output exactly one ASCII digit—1, 2, or 3—and nothing else. "
        "Do not include an explanation, punctuation, markdown, or JSON.\n\n"
        "EXTRACTED FILING EVIDENCE:\n"
        "<filing_text>\n"
        f"{text}\n"
        "</filing_text>\n"
    )


# ---------------------------------------------------------------------------
# Model response parsing
# ---------------------------------------------------------------------------
# Model endpoints can return strings, dictionaries, lists, prompt echoes, or
# repeated JSON. These helpers extract the generated text, validate candidate
# JSON label objects, and reject conflicting label codes.
def extract_text_from_response(resp: Any) -> str:
    """Extract generated text from common endpoint response shapes."""

    if resp is None:
        return ""
    if isinstance(resp, str):
        return resp
    if isinstance(resp, list):
        for item in resp:
            text = extract_text_from_response(item)
            if text:
                return text
        return json.dumps(resp, ensure_ascii=False)
    if isinstance(resp, dict):
        for key in [
            "generated_text",
            "text",
            "output",
            "response",
            "completion",
            "content",
        ]:
            if key in resp and resp[key] is not None:
                text = extract_text_from_response(resp[key])
                if text:
                    return text
        for key in ["choices", "message", "outputs"]:
            if key in resp:
                text = extract_text_from_response(resp[key])
                if text:
                    return text
        return json.dumps(resp, ensure_ascii=False)
    return str(resp)


def extract_balanced_json_objects(text: str) -> list[str]:
    """Find balanced JSON-looking objects while respecting quoted strings."""

    objects: list[str] = []
    start = 0
    while start < len(text):
        if text[start] != "{":
            start += 1
            continue

        depth = 0
        in_string = False
        escape = False
        found_end = None

        for i in range(start, len(text)):
            ch = text[i]
            if in_string:
                if escape:
                    escape = False
                elif ch == "\\":
                    escape = True
                elif ch == '"':
                    in_string = False
                continue

            if ch == '"':
                in_string = True
            elif ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
                if depth == 0:
                    found_end = i
                    break

        if found_end is None:
            start += 1
        else:
            objects.append(text[start : found_end + 1])
            start = found_end + 1
    return objects


def parse_score_int(value: Any) -> Optional[int]:
    """Parse a conservative 1-3 score value."""

    if isinstance(value, bool):
        return None
    if isinstance(value, int) and value in {1, 2, 3}:
        return int(value)
    if isinstance(value, float) and value in {1.0, 2.0, 3.0}:
        return int(value)
    if value is None or (isinstance(value, float) and pd.isna(value)):
        return None
    text = normalize_whitespace(value)
    return int(text) if text in {"1", "2", "3"} else None


def build_score_parse_result(
    score: int,
    *,
    raw_json: str,
    status: str,
) -> dict[str, Any]:
    """Build the standard parsed-result object for the 1-3 score workflow."""

    score_label = AI_SCORE_LABELS[int(score)]
    return {
        "ai_score": int(score),
        "ai_score_label": score_label,
        "score_explanation": f"Parsed AI adoption score {int(score)} ({score_label}).",
        "status": status,
        "raw_json": raw_json,
    }


def parse_model_output_payload(text: str) -> dict[str, Any]:
    """Parse raw model output into one validated 1-3 AI adoption score."""

    if not text:
        return {
            "ai_score": pd.NA,
            "ai_score_label": pd.NA,
            "score_explanation": "No model output returned.",
            "status": "missing_output",
            "raw_json": "",
        }

    normalized_text = normalize_whitespace(text)
    candidates: list[tuple[int, str]] = []

    exact_score = parse_score_int(normalized_text)
    if exact_score is not None:
        candidates.append((exact_score, normalized_text))

    for candidate in extract_balanced_json_objects(text):
        try:
            obj = json.loads(candidate)
        except Exception:
            continue
        if not isinstance(obj, dict):
            continue
        for key in [
            "ai_score",
            "score",
            "ai_adoption_score",
            "ai_level_code",
            "ai_adoption_level_code",
        ]:
            parsed = parse_score_int(obj.get(key))
            if parsed is not None:
                candidates.append((parsed, candidate))
                break

    for match in SCORE_TOKEN_RE.finditer(normalized_text):
        candidates.append((int(match.group(1)), match.group(1)))

    if not candidates:
        return {
            "ai_score": pd.NA,
            "ai_score_label": pd.NA,
            "score_explanation": "Model output did not contain a usable score.",
            "status": "no_score_found",
            "raw_json": "",
        }

    unique_scores = {score for score, _ in candidates}
    if len(unique_scores) > 1:
        return {
            "ai_score": pd.NA,
            "ai_score_label": pd.NA,
            "score_explanation": "Model output contained conflicting score values.",
            "status": "conflicting_scores",
            "raw_json": candidates[-1][1],
        }

    score, raw_json = candidates[-1]
    return build_score_parse_result(score, raw_json=raw_json, status="ok")


# ---------------------------------------------------------------------------
# Per-filing record preparation
# ---------------------------------------------------------------------------
# These helpers create the output row for each filing, apply empty-text and
# prefilter rules, and build prompts only for filings that need the LLM.
def base_output_record(
    row: pd.Series,
    *,
    run_id: str,
    source_label: str,
    chunk_id: str,
    llm_model: str,
    llm_checkpoint: str,
    temperature: Optional[float],
    max_new_tokens: int,
    max_prompt_chars: int,
    sentence_window: int,
    endpoint: str,
    prefilter_mode: str,
    prefilter_audit_sample: bool,
) -> dict[str, Any]:
    """Create the standard output record before labeling decisions are applied."""

    year = row.get("year", pd.NA)
    year = int(year) if pd.notna(year) else pd.NA
    return {
        "run_id": run_id,
        "script_version": SCRIPT_VERSION,
        "prompt_version": PROMPT_VERSION,
        "research_profile": RESEARCH_PROFILE,
        "llm_model": llm_model,
        "llm_checkpoint": llm_checkpoint,
        "temperature": (
            float(temperature) if temperature is not None else pd.NA
        ),
        "max_new_tokens": int(max_new_tokens),
        "max_prompt_chars": int(max_prompt_chars),
        "sentence_window": int(sentence_window),
        "source_label": source_label,
        "chunk_id": chunk_id,
        "accession_number": str(row.get("accession_number", "")).strip(),
        "filing_accession": str(row.get("accession_number", "")).strip(),
        "cik": str(row.get("cik", "")).strip(),
        "year": year,
        "form_type": str(row.get("form_type", "")).strip(),
        "combined_chars": int(row.get("combined_chars", 0) or 0),
        "keyword_hits": 0,
        "prefilter_mode": prefilter_mode,
        "prefilter_decision": "not_applicable",
        "prefilter_audit_sample": bool(prefilter_audit_sample),
        "snippet_chars": 0,
        "llm_called": False,
        "endpoint_attempts": 0,
        "parse_status": "not_called",
        "initial_score_status": "",
        "retry_attempted": False,
        "retry_score_status": "",
        "ai_score": pd.NA,
        "ai_score_label": pd.NA,
        "score_explanation": "",
        "score_status": "",
        "endpoint": endpoint,
        "job_id": "",
        "snippet_sha256": "",
        "raw_json_sha256": "",
    }


# ---------------------------------------------------------------------------
# Bedrock bulk invocation preparation
# ---------------------------------------------------------------------------
# The model backend accepts (identifier, prompt) pairs. The identifier is the
# bridge back to the correct filing row.
def iter_bedrock_prompts(
    pending: dict[str, dict[str, Any]],
) -> Iterable[tuple[str, str]]:
    """Yield identifier/prompt pairs for the configured model backend."""

    for linked_obj, item in pending.items():
        yield linked_obj, str(item["prompt"])


def prepare_record_prompt(
    record: dict[str, Any],
    *,
    record_index: int,
    full_text: str,
    keyword_hits: int,
    audit_sample: bool,
    prefilter_mode: str,
    max_prompt_chars: int,
    sentence_window: int,
    job_id: str,
) -> Optional[dict[str, Any]]:
    """Apply prefilter rules and return a prompt item when an LLM call is needed."""

    if not full_text:
        record.update(
            ai_score=1,
            ai_score_label=AI_SCORE_LABELS[1],
            score_explanation="No keyword-window text was available for this filing.",
            score_status="empty_text_zero",
            prefilter_decision="empty_text",
        )
        return None

    should_prefilter = (
        keyword_hits == 0
        and prefilter_mode in {"hard_zero", "audit"}
        and not audit_sample
    )
    if should_prefilter:
        record.update(
            ai_score=1,
            ai_score_label=AI_SCORE_LABELS[1],
            score_explanation="No AI dictionary keywords were detected by the prefilter, so the filing was assigned score 1 without an LLM call.",
            score_status="prefilter_zero_no_keyword",
            prefilter_decision=f"{prefilter_mode}_zero_no_keyword",
        )
        return None

    snippet = extract_relevant_snippets(full_text, max_prompt_chars, sentence_window)
    if not snippet:
        record.update(
            parse_status="failed",
            score_explanation="Snippet extraction returned empty text.",
            score_status="snippet_extraction_failed",
        )
        return None

    if audit_sample:
        decision = "audit_call_no_keyword"
    elif keyword_hits == 0:
        decision = "llm_call_no_trigger_prefilter_off"
    else:
        decision = "keyword_hit_llm_call"

    record.update(
        prefilter_decision=decision,
        snippet_chars=len(snippet),
        llm_called=True,
        endpoint_attempts=1,
        job_id=job_id,
        snippet_sha256=sha256_text(snippet),
    )
    return {
        "record_index": record_index,
        "prompt": build_ai_prompt(snippet),
        "retry_prompt": build_ai_retry_prompt(snippet),
        "snippet_text": snippet,
    }


def prepare_records_and_prompts(
    wide: pd.DataFrame,
    *,
    run_id: str,
    chunk_id: str,
    source_label: str,
    llm_model: str,
    llm_checkpoint: str,
    temperature: Optional[float],
    max_new_tokens: int,
    endpoint: str,
    prefilter_mode: str,
    audit_seed: str,
    prefilter_audit_rate: float,
    prefilter_audit_limit: int,
    max_prompt_chars: int,
    sentence_window: int,
) -> tuple[list[dict[str, Any]], dict[str, dict[str, Any]]]:
    """
    Prepare output records and collect pending LLM prompts.

    Returns:
        records: one output row per filing, including prefilter-zero rows
        pending: only rows that need a bulk LLM call
    """

    records: list[dict[str, Any]] = []
    pending: dict[str, dict[str, Any]] = {}
    audit_calls = 0

    for row_number, (_, row) in enumerate(wide.iterrows(), start=1):
        # Work out whether this filing should be skipped, audited, or sent to
        # the LLM.
        accession = str(row.get("accession_number", "")).strip()
        full_text = normalize_structured_text(row.get("combined_text", ""))
        keyword_hits = count_ai_keywords(full_text)

        # Audit mode sends a stable sample of no-keyword filings to the LLM so
        # the false-zero risk of the prefilter can be measured.
        audit_sample = False
        if prefilter_mode == "audit" and keyword_hits == 0:
            sampled = (
                stable_unit_interval(f"{audit_seed}|{accession}") < prefilter_audit_rate
            )
            under_limit = (
                prefilter_audit_limit <= 0 or audit_calls < prefilter_audit_limit
            )
            audit_sample = sampled and under_limit
            if audit_sample:
                audit_calls += 1

        # Start with the standard output fields, then fill in the labeling path.
        record = base_output_record(
            row,
            run_id=run_id,
            source_label=source_label,
            chunk_id=chunk_id,
            llm_model=llm_model,
            llm_checkpoint=llm_checkpoint,
            temperature=temperature,
            max_new_tokens=max_new_tokens,
            max_prompt_chars=max_prompt_chars,
            sentence_window=sentence_window,
            endpoint=endpoint,
            prefilter_mode=prefilter_mode,
            prefilter_audit_sample=audit_sample,
        )
        record["keyword_hits"] = keyword_hits

        record_index = len(records)
        prompt_item = prepare_record_prompt(
            record,
            record_index=record_index,
            full_text=full_text,
            keyword_hits=keyword_hits,
            audit_sample=audit_sample,
            prefilter_mode=prefilter_mode,
            max_prompt_chars=max_prompt_chars,
            sentence_window=sentence_window,
            job_id=f"{run_id}_{chunk_id}_{row_number:06d}",
        )
        if prompt_item is not None:
            pending[str(record_index)] = prompt_item

        records.append(record)

    return records, pending


# ---------------------------------------------------------------------------
# Bulk result handling
# ---------------------------------------------------------------------------
# This function takes the iterator returned by the configured model backend,
# matches each result back to the correct filing row, and fills in the score,
# status, and raw-response hashes.
def response_text(result_body: Any) -> str:
    """Extract model text from one Bedrock bulk result."""

    if not isinstance(result_body, dict):
        return ""
    direct_text = str(result_body.get("model_response_string", "")).strip()
    if direct_text:
        return direct_text
    return extract_text_from_response(result_body.get("response_json", result_body))


def apply_endpoint_error(
    record: dict[str, Any],
    error: Any,
    *,
    is_retry: bool,
    save_raw_json: bool,
) -> None:
    """Record a failed Bedrock request."""

    if save_raw_json:
        record["raw_response"] = str(error)
    if is_retry:
        record.update(
            parse_status="failed",
            retry_score_status="endpoint_error",
            score_explanation=f"Retry endpoint error: {str(error)[:500]}",
            score_status="retry_endpoint_error",
        )
        return
    record.update(
        parse_status="failed",
        ai_score=pd.NA,
        ai_score_label=pd.NA,
        score_explanation=f"Endpoint error: {str(error)[:500]}",
        initial_score_status="endpoint_error",
        score_status="endpoint_error",
    )


def apply_parsed_result(
    record: dict[str, Any],
    parsed: dict[str, Any],
    raw_text: str,
    *,
    is_retry: bool,
    save_raw_json: bool,
) -> None:
    """Apply one parsed score result to its filing record."""

    status = str(parsed["status"])
    raw_json = str(parsed["raw_json"])
    if save_raw_json:
        record["raw_response"] = raw_text
        record["raw_json"] = raw_json
    record["raw_json_sha256"] = sha256_text(raw_json) if raw_json else ""

    if status == "ok":
        audit_sample = bool(record.get("prefilter_audit_sample"))
        if is_retry:
            score_status = (
                "prefilter_audit_ok_after_retry" if audit_sample else "ok_after_retry"
            )
        else:
            score_status = "prefilter_audit_ok" if audit_sample else "ok"
        record.update(
            parse_status="retry_success" if is_retry else "success",
            ai_score=parsed["ai_score"],
            ai_score_label=parsed["ai_score_label"],
            score_explanation=parsed["score_explanation"],
            score_status=score_status,
        )
        if is_retry:
            record["retry_score_status"] = status
        else:
            record["initial_score_status"] = score_status
        return

    record.update(
        parse_status="failed",
        ai_score=pd.NA,
        ai_score_label=pd.NA,
        score_explanation=parsed["score_explanation"],
        score_status=f"retry_{status}" if is_retry else status,
    )
    record["retry_score_status" if is_retry else "initial_score_status"] = status


def apply_bulk_results(
    records: list[dict[str, Any]],
    pending: dict[str, dict[str, Any]],
    results: Iterable[tuple[Any, dict[str, Any]]],
    *,
    save_raw_json: bool,
    is_retry: bool = False,
) -> None:
    """Apply Bedrock bulk results to the prepared output records in place."""

    for linked_obj, result_body in results:
        item = pending[str(linked_obj)]
        record = records[item["record_index"]]
        if is_retry:
            record["retry_attempted"] = True
            record["endpoint_attempts"] = (
                int(record.get("endpoint_attempts", 0) or 0) + 1
            )

        if isinstance(result_body, dict) and "error" in result_body:
            apply_endpoint_error(
                record,
                result_body.get("error", ""),
                is_retry=is_retry,
                save_raw_json=save_raw_json,
            )
            continue

        raw_text = response_text(result_body)
        apply_parsed_result(
            record,
            parse_model_output_payload(raw_text),
            raw_text,
            is_retry=is_retry,
            save_raw_json=save_raw_json,
        )


# ---------------------------------------------------------------------------
# Chunk summary
# ---------------------------------------------------------------------------
# The JSON summary gives reviewers a quick chunk-level QA view without opening
# the full filing-level CSV.
def summarize_output(
    out_df: pd.DataFrame,
    *,
    run_id: str,
    chunk_name: str,
    source_label: str,
    endpoint: str,
    prefilter_mode: str,
    prefilter_audit_rate: float,
    prefilter_audit_limit: int,
    max_prompt_chars: int,
    sentence_window: int,
    lookup_csv: Optional[str],
    max_filings_per_chunk: int,
    max_analysis_year: int,
    n_filings_before_lookup: int,
    n_filings_after_lookup: int,
    n_filings_before_year_filter: int,
    n_filings_after_year_filter: int,
    output_csv: str,
) -> dict[str, Any]:
    """Build the per-chunk JSON summary."""

    score_series = (
        pd.to_numeric(out_df["ai_score"], errors="coerce")
        if not out_df.empty
        else pd.Series(dtype=float)
    )
    n_scored = int(score_series.notna().sum()) if not out_df.empty else 0
    return {
        "run_id": run_id,
        "chunk_name": chunk_name,
        "source_label": source_label,
        "script_version": SCRIPT_VERSION,
        "prompt_version": PROMPT_VERSION,
        "research_profile": RESEARCH_PROFILE,
        "endpoint": endpoint,
        "prefilter_mode": prefilter_mode,
        "prefilter_audit_rate": prefilter_audit_rate,
        "prefilter_audit_limit": prefilter_audit_limit,
        "max_prompt_chars": int(max_prompt_chars),
        "sentence_window": int(sentence_window),
        "lookup_csv": lookup_csv,
        "max_filings_per_chunk": int(max_filings_per_chunk),
        "max_analysis_year": int(max_analysis_year),
        "n_filings_before_lookup": int(n_filings_before_lookup),
        "n_filings_after_lookup": int(n_filings_after_lookup),
        "n_filings_before_year_filter": int(n_filings_before_year_filter),
        "n_filings_after_year_filter": int(n_filings_after_year_filter),
        "n_filings": int(len(out_df)),
        "n_unique_cik": int(out_df["cik"].nunique()) if not out_df.empty else 0,
        "n_llm_called": int(out_df["llm_called"].sum()) if not out_df.empty else 0,
        "n_scored": n_scored,
        "n_unscored": int(len(out_df) - n_scored),
        "n_score_1": int((score_series == 1).sum()) if not out_df.empty else 0,
        "n_score_2": int((score_series == 2).sum()) if not out_df.empty else 0,
        "n_score_3": int((score_series == 3).sum()) if not out_df.empty else 0,
        "n_ok": int((out_df["score_status"] == "ok").sum()) if not out_df.empty else 0,
        "n_ok_after_retry": (
            int((out_df["score_status"] == "ok_after_retry").sum())
            if not out_df.empty
            else 0
        ),
        "n_prefilter_audit_ok_after_retry": (
            int((out_df["score_status"] == "prefilter_audit_ok_after_retry").sum())
            if not out_df.empty
            else 0
        ),
        "n_prefilter_audit_ok": (
            int((out_df["score_status"] == "prefilter_audit_ok").sum())
            if not out_df.empty
            else 0
        ),
        "n_prefilter_zero": (
            int((out_df["score_status"] == "prefilter_zero_no_keyword").sum())
            if not out_df.empty
            else 0
        ),
        "n_retry_attempted": (
            int(out_df["retry_attempted"].sum())
            if not out_df.empty and "retry_attempted" in out_df.columns
            else 0
        ),
        "status_counts": (
            out_df["score_status"].value_counts(dropna=False).to_dict()
            if not out_df.empty
            else {}
        ),
        "mean_ai_score": None if n_scored == 0 else float(score_series.mean()),
        "output_csv": output_csv,
    }
