#!/usr/bin/env python3

from __future__ import annotations

import argparse
import html
import re
from pathlib import Path

import pandas as pd


PRE_BLOCK_RE = re.compile(r"<pre>(.*?)</pre>", re.IGNORECASE | re.DOTALL)
SENTENCE_SPLIT_RE = re.compile(r"(?<=[.!?])\s+")

# "Strong" terms match the conceptual definition in the prompt more closely.
STRONG_AI_RE = re.compile(
    r"(?i)\b("
    r"artificial intelligence|machine learning|deep learning|neural networks?|"
    r"artificial neural networks?|computer vision|natural language processing|"
    r"natural language understanding|machine translation|generative ai|genai|"
    r"large language models?|foundation models?|transformer models?|"
    r"conversational ai|virtual assistants?|copilots?|ai assistants?|"
    r"ai agents?|agentic ai|llms?"
    r")\b"
)

# "Broad" terms help identify why suspicion rules fired, but are noisier.
BROAD_AI_RE = re.compile(
    r"(?i)\b("
    r"big data|business intelligence|data science|data scientists?|"
    r"predictive analytics?|recommendation engines?|expert systems?|"
    r"data mining|pattern recognition|anomaly detection|"
    r"decision engines?|fraud detection|recommender systems?"
    r")\b"
)

OPERATIONAL_RE = re.compile(
    r"(?i)\b("
    r"use|uses|used|using|deploy|deploys|deployed|deployment|implemented|implements|"
    r"integrated|integrates|integration|embedded|operates|operational|automate|"
    r"automates|automated|recommend|recommends|detect|detects|forecast|forecasts|"
    r"predict|predicts|optimize|optimizes|personalize|personalizes|"
    r"supports?|provides?|offers?|delivers?|powers?|enables?|allowing|facilitates?|"
    r"built|builds|based on"
    r")\b"
)

FIRST_PERSON_RE = re.compile(
    r"(?i)\b("
    r"we|our|company|business|businesses|firm|products?|services?|platforms?|"
    r"systems?|tools?|applications?|solution|solutions"
    r")\b"
)

ADOPTION_RE = re.compile(
    r"(?i)\b("
    r"use|uses|used|using|leverage|leverages|leveraged|leveraging|apply|applies|applied|applying|"
    r"adopt|adopts|adopted|adopting|deploy|deploys|deployed|deploying|integrate|integrates|"
    r"integrated|integrating|embed|embeds|embedded|embedding|power|powers|powered|powering|"
    r"enable|enables|enabled|enabling|build|builds|built|building|develop|develops|developed|developing"
    r")\b.{0,80}\b("
    r"artificial intelligence|machine learning|deep learning|neural networks?|"
    r"artificial neural networks?|computer vision|natural language processing|"
    r"natural language understanding|machine translation|generative ai|genai|"
    r"large language models?|foundation models?|transformer models?|"
    r"conversational ai|virtual assistants?|copilots?|ai assistants?|"
    r"ai agents?|agentic ai|llms?"
    r")\b"
)

ADOPTION_RE_REVERSED = re.compile(
    r"(?i)\b("
    r"artificial intelligence|machine learning|deep learning|neural networks?|"
    r"artificial neural networks?|computer vision|natural language processing|"
    r"natural language understanding|machine translation|generative ai|genai|"
    r"large language models?|foundation models?|transformer models?|"
    r"conversational ai|virtual assistants?|copilots?|ai assistants?|"
    r"ai agents?|agentic ai|llms?"
    r")\b.{0,80}\b("
    r"use|uses|used|using|leverage|leverages|leveraged|leveraging|apply|applies|applied|applying|"
    r"adopt|adopts|adopted|adopting|deploy|deploys|deployed|deploying|integrate|integrates|"
    r"integrated|integrating|embed|embeds|embedded|embedding|power|powers|powered|powering|"
    r"enable|enables|enabled|enabling|build|builds|built|building|develop|develops|developed|developing"
    r")\b"
)

AI_POWERED_RE = re.compile(
    r"(?i)\b(ai-powered|ai\/ml-powered|machine learning-powered|powered by artificial intelligence|powered by machine learning)\b"
)

MARKET_CONTEXT_RE = re.compile(
    r"(?i)\b("
    r"applications? for|applications? in|targeting|targeted|target|markets? for|market|industry|"
    r"customers?|opportunity|opportunities|growth markets?|focus on|focused on|invest in|investing in|"
    r"litigation|lawsuit|regulation|regulatory|competition|driving demand|trend|trends"
    r")\b"
)

LOW_VALUE_RE = re.compile(
    r"(?i)\b("
    r"risk|risks|could|may|might|intend|intends|plan|plans|future|potential|"
    r"regulation|regulatory|competition|competitors|cybersecurity|pilot|pilots|"
    r"proof of concept|experiment|experiments|research|market|industry|demand|"
    r"opportunity|opportunities|expect|expects|expected|forecast|"
    r"litigation|lawsuit|alleging|unauthorized|copied|copying"
    r")\b"
)


def parse_args() -> argparse.Namespace:
    script_path = Path(__file__).resolve()
    project_root = script_path.parents[3]

    parser = argparse.ArgumentParser(
        description="Audit suspicious llm_scoring filings using simple text heuristics."
    )
    parser.add_argument(
        "--suspects",
        default=str(project_root / "output" / "filings" / "suspected_accessions.csv"),
        help="CSV of suspicious accessions.",
    )
    parser.add_argument(
        "--html-dir",
        default=str(project_root / "output" / "filings"),
        help="Directory containing filing_*.html pages.",
    )
    parser.add_argument(
        "--out-csv",
        default=str(project_root / "output" / "filings" / "suspected_accessions_audit.csv"),
        help="Audit detail CSV output path.",
    )
    parser.add_argument(
        "--out-summary",
        default=str(project_root / "output" / "filings" / "suspected_accessions_audit_summary.md"),
        help="Audit summary markdown output path.",
    )
    return parser.parse_args()


def normalize_whitespace(text: str) -> str:
    return " ".join(text.split()).strip()


def load_filing_text(html_path: Path) -> str:
    raw = html_path.read_text(encoding="utf-8")
    blocks = [html.unescape(match.group(1)) for match in PRE_BLOCK_RE.finditer(raw)]
    return normalize_whitespace(" ".join(blocks))


def split_sentences(text: str) -> list[str]:
    text = normalize_whitespace(text)
    if not text:
        return []
    return [part.strip() for part in SENTENCE_SPLIT_RE.split(text) if part.strip()]


def sentence_window(sentences: list[str], idx: int, radius: int = 1) -> str:
    lo = max(0, idx - radius)
    hi = min(len(sentences), idx + radius + 1)
    return " ".join(sentences[lo:hi])


def classify_filing(text: str) -> dict[str, object]:
    sentences = split_sentences(text)
    candidates: list[dict[str, object]] = []

    for idx, sentence in enumerate(sentences):
        if not (STRONG_AI_RE.search(sentence) or BROAD_AI_RE.search(sentence)):
            continue

        window = sentence_window(sentences, idx, radius=1)
        strong_hits = len(STRONG_AI_RE.findall(window))
        broad_hits = len(BROAD_AI_RE.findall(window))
        operational_hits = len(OPERATIONAL_RE.findall(window))
        first_person_hits = len(FIRST_PERSON_RE.findall(window))
        low_value_hits = len(LOW_VALUE_RE.findall(window))
        adoption_match = bool(ADOPTION_RE.search(window) or ADOPTION_RE_REVERSED.search(window) or AI_POWERED_RE.search(window))
        market_context = bool(MARKET_CONTEXT_RE.search(window))

        score = 0
        if strong_hits:
            score += 4
        if broad_hits:
            score += 1
        if operational_hits:
            score += 2
        if first_person_hits:
            score += 1
        if adoption_match:
            score += 3
        if market_context:
            score -= 1
        score -= min(low_value_hits, 2)

        if adoption_match and first_person_hits and not low_value_hits:
            label = "likely_operational_ai"
        elif adoption_match or (strong_hits and operational_hits and first_person_hits and not market_context):
            label = "possible_operational_ai"
        elif strong_hits or (broad_hits and operational_hits and first_person_hits):
            label = "ambiguous_ai_discussion"
        else:
            label = "generic_or_non_operational"

        candidates.append(
            {
                "sentence_index": idx,
                "score": score,
                "label": label,
                "window": window,
                "strong_hits": strong_hits,
                "broad_hits": broad_hits,
                "operational_hits": operational_hits,
                "first_person_hits": first_person_hits,
                "low_value_hits": low_value_hits,
                "adoption_match": adoption_match,
                "market_context": market_context,
            }
        )

    if not candidates:
        return {
            "heuristic_label": "no_ai_evidence_found",
            "top_evidence": "",
            "top_score": 0,
            "strong_ai_windows": 0,
            "broad_ai_windows": 0,
            "operational_windows": 0,
            "candidate_count": 0,
        }

    candidates.sort(key=lambda item: (-int(item["score"]), int(item["sentence_index"])))
    top = candidates[0]

    if any(item["label"] == "likely_operational_ai" for item in candidates):
        heuristic_label = "likely_operational_ai"
    elif any(item["label"] == "possible_operational_ai" for item in candidates):
        heuristic_label = "possible_operational_ai"
    elif any(item["label"] == "ambiguous_ai_discussion" for item in candidates):
        heuristic_label = "ambiguous_ai_discussion"
    else:
        heuristic_label = "generic_or_non_operational"

    return {
        "heuristic_label": heuristic_label,
        "top_evidence": str(top["window"]),
        "top_score": int(top["score"]),
        "strong_ai_windows": sum(1 for item in candidates if int(item["strong_hits"]) > 0),
        "broad_ai_windows": sum(1 for item in candidates if int(item["broad_hits"]) > 0),
        "operational_windows": sum(1 for item in candidates if int(item["operational_hits"]) > 0),
        "candidate_count": len(candidates),
    }


def add_issue_flag(row: pd.Series) -> str:
    reason = row["reason"]
    label = row["heuristic_label"]

    if reason == "llm_parse_failure":
        if label == "likely_operational_ai":
            return "parser_failure_high_risk"
        if label == "possible_operational_ai":
            return "parser_failure_medium_risk"
        return "parser_failure_probably_harmless"

    if reason == "zero_score_many_keywords":
        if label in {"likely_operational_ai", "possible_operational_ai"}:
            return "zero_score_false_negative_likely"
        return "zero_score_probably_ok"

    if reason == "low_positive_score":
        if label in {"generic_or_non_operational", "no_ai_evidence_found"}:
            return "low_positive_false_positive_likely"
        if label == "ambiguous_ai_discussion":
            return "low_positive_ambiguous"
        return "low_positive_probably_ok"

    if reason == "switch_off_after_positive":
        if label == "likely_operational_ai":
            return "switch_off_false_negative_likely"
        if label == "possible_operational_ai":
            return "switch_off_maybe_false_negative"
        return "switch_off_rule_noisy_or_ok"

    return "unclassified"


def write_summary(df: pd.DataFrame, out_path: Path) -> None:
    by_reason = (
        df.groupby(["reason", "issue_flag"])
        .size()
        .rename("n")
        .reset_index()
        .sort_values(["reason", "n"], ascending=[True, False])
    )
    label_counts = (
        df.groupby(["reason", "heuristic_label"])
        .size()
        .rename("n")
        .reset_index()
        .sort_values(["reason", "n"], ascending=[True, False])
    )

    lines = [
        "# Suspicious Filing Audit",
        "",
        f"Rows audited: {len(df)}",
        "",
        "## By reason and issue flag",
        "",
        by_reason.to_markdown(index=False),
        "",
        "## By reason and heuristic label",
        "",
        label_counts.to_markdown(index=False),
        "",
        "## Highest-risk examples",
        "",
    ]

    high_risk = df[
        df["issue_flag"].isin(
            {
                "parser_failure_high_risk",
                "zero_score_false_negative_likely",
                "switch_off_false_negative_likely",
            }
        )
    ].copy()

    if high_risk.empty:
        lines.append("No high-risk examples were found by the heuristic audit.")
    else:
        preview = high_risk[
            [
                "reason",
                "accession_number",
                "cik",
                "year",
                "llama_score",
                "keyword_hits",
                "heuristic_label",
                "issue_flag",
                "top_evidence",
            ]
        ].head(25)
        lines.append(preview.to_markdown(index=False))

    out_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    args = parse_args()

    suspects_path = Path(args.suspects).resolve()
    html_dir = Path(args.html_dir).resolve()
    out_csv = Path(args.out_csv).resolve()
    out_summary = Path(args.out_summary).resolve()

    out_csv.parent.mkdir(parents=True, exist_ok=True)

    suspects = pd.read_csv(suspects_path)

    audit_rows: list[dict[str, object]] = []
    for row in suspects.to_dict(orient="records"):
        accession = row["accession_number"]
        html_path = html_dir / f"filing_{accession}.html"
        if not html_path.exists():
            raise FileNotFoundError(f"Missing filing HTML for {accession}: {html_path}")

        text = load_filing_text(html_path)
        audit = classify_filing(text)
        audit_rows.append({**row, **audit})

    audit_df = pd.DataFrame(audit_rows)
    audit_df["issue_flag"] = audit_df.apply(add_issue_flag, axis=1)
    audit_df["top_evidence"] = audit_df["top_evidence"].map(lambda x: normalize_whitespace(str(x))[:1200])

    audit_df = audit_df.sort_values(
        ["reason", "issue_flag", "keyword_hits", "year", "accession_number"],
        ascending=[True, True, False, True, True],
    ).reset_index(drop=True)

    audit_df.to_csv(out_csv, index=False)
    write_summary(audit_df, out_summary)

    print(f"Wrote audit detail: {out_csv}")
    print(f"Wrote audit summary: {out_summary}")


if __name__ == "__main__":
    main()
