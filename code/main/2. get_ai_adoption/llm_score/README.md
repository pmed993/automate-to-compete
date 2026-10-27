# LLM Extraction Scoring Methodology

This folder contains the simplified LLM extraction pipeline for scoring firm AI
adoption from SEC 10-K text. The design is meant for empirical work where the
goal is a reproducible filing-level score, not a long-form model explanation.

Run every command below from the repository root with the project's Python
environment active.

## Research Target

The output is a disclosure-based 1-3 score of the firm's AI implementation
maturity in the filing evidence. The decision rules use implementation stage
and organizational scope or core integration. The model returns only the
derived score, so the downstream data contract is unchanged.

Rubric:

- `1`: no disclosed current AI implementation
- `2`: emerging or bounded AI implementation
- `3`: established and integrated AI implementation

Interpretation:

- `1` includes no disclosed firm implementation, industry or third-party
  context, risk discussion, plans, preliminary exploration, and general
  capacity building without a specific active application.
- `2` requires a concrete specific development, test, or pilot, or bounded
  current production use that does not satisfy score 3.
- `3` requires explicit current production or operational use **and** evidence
  of meaningful organizational scale or integration into a core activity.
  AI-enabled products or services currently provided by the firm count.
  Strategy language and expected benefits alone are insufficient.

The logical derivation is:

```text
score 1: no concrete active firm implementation
score 2: concrete implementation AND (specific pilot OR bounded production use)
score 3: current production use AND (meaningful scale OR core integration)
```

A narrow use can therefore receive score 3 when it is demonstrably core.
Ambiguous evidence receives the lower score.

This is a measure of disclosed AI adoption, not a direct measure of true AI
deployment, AI capability, AI spending, or productivity.

## Frozen Research Profile

The pipeline is now frozen as:

```text
research_profile = llm_extraction_ai_1to3_v12
script_version   = 2026-08-14-llm_extraction_v21
prompt_version   = llm_extraction_claude_v8
```

Recommended default settings:

```text
model_label       = claude
model_id          = eu.anthropic.claude-sonnet-4-6
temperature       = 0.0
max_prompt_chars  = 2400
sentence_window   = 1
max_new_tokens    = 8
prefilter_mode    = hard_zero
max_analysis_year = 2025
```

Install the public dependencies before scoring:

```bash
python3 -m pip install -r "code/main/2. get_ai_adoption/llm_score/requirements.txt"
```

The minimum AWS SDK is `boto3 1.34.131`, which provides Bedrock Converse. The
frozen request was tested with both `boto3` and `botocore` at `1.34.131`. Run the
offline check against the installed SDK from the repository root; no AWS
credentials or network requests are needed:

```bash
python3 -m unittest discover \
  -s "code/main/2. get_ai_adoption/llm_score" -p "test_aws_backend_sdk.py"
```

These settings are written into the row-level CSV output and the chunk summary
JSON so runs can be reproduced later. Source provenance records only the chunk
basename; bucket names, full S3 locations, account identifiers, usernames, and
credentials are not written to scoring outputs.

The v12 research profile uses the v8 concise prompt and a 2,400-character
filing-evidence cap. The prompt is based only on implementation stage
and scope or core integration. Earlier score files remain valid as historical
outputs but must not be mixed with v12 files. The extracted EDGAR RDS chunks can
be reused; run v12 scoring into a new output directory.

## Why This Design

The workflow is intentionally narrow and conservative in structure:

1. one filing-level score per 10-K
2. one shared keyword dictionary
3. one concise ordinal decision framework
4. deterministic parsing and retry logic
5. stable prefilter and snippet extraction

This is closer to a text-classification pipeline than a free-form reasoning
prompt.

## Input Data

The scripts expect EDGAR chunk files such as:

```text
extract_df_chunk_00001.rds
extract_df_chunk_00002.rds
```

Each chunk should contain at least:

```text
item
year
accession_number
cik
form_type
text
```

The scorer also uses `code/main/2. get_ai_adoption/lookup/cik_year.csv` to preserve
the licensed Compustat-derived research sample. That generated lookup is not distributed in
the public repository. Create it from the repository root with
`Rscript "code/main/2. get_ai_adoption/get_cik_year.R"` after setting
`COMPUSTAT_USER` and `COMPUSTAT_PSW`, or pass `--lookup-csv` with an equivalent
file.

The current EDGAR extractor writes multiple `keyword_window` rows per filing
from the whole 10-K or 10-K/A primary document. The loader preserves every
window in source order, joins them into one filing-level input, and includes
both forms. A filing with no extracted hit is represented by an empty-text
sentinel, allowing the scorer to retain it as a score-1 observation without an
LLM request instead of dropping it from the sample.

Use the combined inputs from the main filing-year run and the fiscal-2025
completion described in the [EDGAR README](../../1.%20get_edgar_extract/README.md#required-fiscal-2025-completion-and-combined-scoring-inputs).
`--max-analysis-year 2025` applies to the input's research/scoring `year`, not
directly to its actual SEC filing date. Main inputs use filing year; completion
inputs use report year 2025 and can have `filing_year = 2026`. Both retain the
real SEC filing dates for downstream treatment timing. Old main rows labelled
`year = 2026` remain outside the sample.

The invocation-volume benchmarks below refer to the main filing-year corpus
before the fiscal-2025 completion. Use the combined run's summaries for its
actual filing and model-call counts.

## Prefilter Design

`AI_KEYWORDS` is the single published source of truth for AI terminology. Each
entry carries its category, routing weight, ranking weight, case policy, and an
optional ambiguity check. Routing and ranking therefore use the same auditable
dictionary without treating every term as equally precise.

Explicit terms such as `artificial intelligence`, `machine learning`, and
`large language model` receive high routing and ranking weights. Ambiguous terms
such as `transformer`, `Claude`, and `Gemini` must pass local context checks and
receive lower ranking priority. Bare `learning model` has a non-routing weight;
it can appear only as context around a separately validated AI anchor.

Concrete current-use formulations such as `we use`, `AI-powered`, and `powered
by AI` receive the highest evidence-priority band. Generic risk, competitor,
future, and regulatory sentences cannot displace this direct operational or
product evidence merely because they spell out `artificial intelligence`.

Abbreviations are compiled independently. Standalone `AI`, `ML`, `NLP`, and
`LLM` use case-sensitive token boundaries, so dosage units such as `5 mL` do
not match `ML`. Known non-AI meanings such as `AI = American Indians`,
electrical transformers, remote learning models, and `Claude Bernard` are
excluded by deterministic disambiguation rules.

Recommended budget-constrained production mode:

```text
--prefilter-mode hard_zero
```

This repeats the deterministic keyword check before calling the LLM and keeps
the observed request count to approximately 10,281 after the year filter.
Empty no-hit sentinels still receive score 1 without an LLM request.

The higher-recall alternative is:

```text
--prefilter-mode off
```

`off` sends every non-empty filing-level extraction and produced 11,837 calls
in the current corpus, including 1,304 extracts rejected by the second keyword
pass.

Recommended audit mode for validation:

```text
--prefilter-mode audit --prefilter-audit-rate 0.02
```

## Filing Evidence Sent To The Model

The EDGAR stage has already reduced the full filing to merged keyword windows.
The default, `max_prompt_chars = 2400`, selects anchor-first
evidence within that limit. Direct keyword-hit text takes priority over
surrounding context.

With a positive character cap, the scorer:

1. normalizes whitespace while preserving newlines, bullets, headings, and lists
2. passes the complete filing-level extract through unchanged when it fits
3. only for extracts above the cap, creates bounded structural segments
4. finds validated dictionary matches with keyword-specific metadata
5. creates individual anchor candidates and removes near-duplicates
6. selects direct keyword evidence before surrounding context
7. adds context with the remaining budget and restores filing order

Selected anchors are independent of `sentence_window`: increasing the window
can add context, but it cannot remove an anchor. Context is truncated or
omitted before direct AI evidence is discarded.

In capped audit mode, a no-keyword extract that exceeds the cap ranks broader
technology, automation, analytics, algorithm, and intelligent-system proxy
segments and samples the beginning, middle, and end. Set `max_prompt_chars = 0`
to send the complete filing-level extraction instead.

## Prompt Design

The prompt is intentionally short:

```text
You are an expert analyst. Using the classification rules below, classify the
firm's disclosed AI adoption using only the extracted filing evidence. Do not
rely on outside knowledge.
[frozen 1-3 rubric]
Return only one character: 1, 2, or 3.
[extracted filing evidence]
```

`max_prompt_chars` limits only the extracted filing evidence, not the complete
request. The configured value `2400` produces a maximum 3,379-character
main prompt and a 3,463-character retry prompt. Set it to `0` to send every
extracted keyword window for the filing without a character cap. Complete
prompts are submitted to Bedrock without a second truncation step.

## Input Volume And Invocation Considerations

The previous local Sonnet run provides the relevant empirical benchmark. It
made 6,363 model calls with a 2,400-character evidence cap and only three
retries. Its maximum prompt-character envelope was 21.37 million characters.

After excluding 2026, the current corpus produces 10,281 calls under
`hard_zero`. A local preparation pass found that 4,106 calls exceeded 2,000
characters and 2,959 exceeded 3,000 characters. The number affected by the
2,400-character cap therefore lies between 2,959 and 4,106 calls (28.8% to
39.9%). Anchor-first selection retains direct keyword sentences before spending
the remaining budget on adjacent context.

The eight-token generation limit does not reduce the dominant input-token cost.
It prevents rare verbose completions and bounds their output-token cost. Before
submitting a request, the pipeline maps the frozen settings directly to the AWS
Bedrock Converse API as `temperature = 0.0` and `maxTokens = 8`. If Bedrock or
the selected model rejects a required control, the request fails rather than
silently using a materially different research configuration. Bedrock use may
incur charges in the researcher's AWS account.

Verify the resolved public configuration without making a request:

```bash
python3 "code/main/2. get_ai_adoption/llm_score/get_ai_score_bulk.py" --check-bedrock-config
```

The output must report `maxTokens` equal to `8`, `temperature` equal to `0.0`,
and `No Bedrock request was sent.`

Prompt principles:

- decide only from filing text
- no chain-of-thought
- no JSON output
- no evidence-summary field
- one final score only
- if the text is weak or only speculative, stay at the lower score

## Parsing Rules

The parser accepts:

- an exact response of `1`, `2`, or `3`
- a JSON fallback containing a score field
- a single unambiguous score token in the response

If the model returns conflicting scores, the row is marked failed and retried
once with a stricter score-only prompt.

## Main Outputs

Per chunk:

```text
extract_df_chunk_XXXXX_claude_scores.csv
extract_df_chunk_XXXXX_claude_summary.json
extract_df_chunk_XXXXX_claude_snippet_audit.csv
```

The snippet-audit CSV contains one row for every filing in the chunk. Filings
submitted to the LLM have `llm_processed = True` and retain the exact
`snippet_text` sent to the model. Prefiltered filings remain visible with an
empty snippet and `snippet_text_length = 0`. The table also records
`max_prompt_chars` and `snippet_at_limit`. In unlimited mode,
`max_prompt_chars = 0` and `snippet_at_limit = False`.

Per run:

```text
run_manifest_<RUN_ID>.csv
```

Running `merge_outputs.py` over the chunk folder writes:

```text
llm_extraction_snippet_audit.csv
llm_extraction_filing_master.csv
llm_extraction_firm_year_panel.csv
```

### Reproducible scoring-example table

After the merge completes, `build_ai_scoring_examples.R` creates a compact,
paper-audit CSV containing one curated EDGAR keyword window for each score. It
combines the final score, SEC firm metadata, exact keyword hits, and the complete
stored extraction-window text. The selection is transparent and editable in
`scoring_example_selection.csv`; no model call or additional cost is involved.

After merging into `data/llm_score` as shown below, run:

```bash
Rscript "code/main/2. get_ai_adoption/llm_score/build_ai_scoring_examples.R" \
  --snippet-audit data/llm_score/llm_extraction_snippet_audit.csv \
  --filing-manifest cache/edgar_keyword_windows_combined/filing_manifest.csv \
  --rds-dir cache/edgar_keyword_windows_combined/assembled \
  --selection "code/main/2. get_ai_adoption/llm_score/scoring_example_selection.csv" \
  --out output/summary_stats/ai_scoring_example/ai_adoption_scoring_examples.csv
```

The script reads the assembled RDS files sequentially and stops once all selected
windows are found, so it does not load the full extraction corpus into memory.
It fails rather than writing a misleading table if a filing is absent, a selected
window is missing, an accession is duplicated, or the final score differs from
the score declared in the selection file. The resulting columns include:

```text
score
firm_name
year
form
keyword_hits
keyword_hit_count
actual_extracted_chunk
source_chunk
window_id
scoring_chunk
cik
filing_accession
parse_status
score_status
model_snippet_chars
model_snippet_at_limit
```

Duplicate accessions stop the merge and are written to
`filing_accession_duplicates.csv` for review. The extractor keeps all windows
for one accession in the same input chunk, so duplicates indicate overlapping
score outputs rather than a valid filing split.

Important output fields:

```text
run_id
script_version
prompt_version
research_profile
llm_model
temperature
max_new_tokens
max_prompt_chars
sentence_window
prefilter_mode
keyword_hits
llm_called
parse_status
score_status
ai_score
ai_score_label
score_explanation
```

## Running The Scorer

The scorer accepts either local RDS files or an AWS S3 location. Local input
takes precedence if a bucket is also present in the environment.

### Local input

```bash
python3 "code/main/2. get_ai_adoption/llm_score/get_ai_score_bulk.py" \
  --input-dir cache/edgar_keyword_windows_combined/assembled \
  --all-chunks \
  --lookup-csv "code/main/2. get_ai_adoption/lookup/cik_year.csv" \
  --max-analysis-year 2025 \
  --model-id eu.anthropic.claude-sonnet-4-6 \
  --prefilter-mode hard_zero \
  --max-prompt-chars 2400 \
  --sentence-window 1 \
  --skip-existing \
  --flat-output \
  --out-dir output/final_llm_extraction_completed
```

### AWS S3 input

```bash
python3 "code/main/2. get_ai_adoption/llm_score/get_ai_score_bulk.py" \
  --s3-bucket YOUR_BUCKET \
  --s3-prefix YOUR_PREFIX \
  --all-chunks \
  --lookup-csv "code/main/2. get_ai_adoption/lookup/cik_year.csv" \
  --max-analysis-year 2025 \
  --model-id eu.anthropic.claude-sonnet-4-6 \
  --prefilter-mode hard_zero \
  --max-prompt-chars 2400 \
  --sentence-window 1 \
  --skip-existing \
  --flat-output \
  --out-dir output/final_llm_extraction_completed
```

The bucket can instead be supplied through `AI_ADOPTION_S3_BUCKET`, and the
prefix through `AI_ADOPTION_S3_PREFIX`. `--aws-region` overrides the region
resolved by boto3. Credentials are discovered through the standard boto3 chain,
such as an AWS profile, environment-based credentials, an IAM role, or AWS SSO;
never put credentials in this repository or in command examples.

For S3 input, upload the RDS files from
`cache/edgar_keyword_windows_combined/data_workspace_rds` to the selected prefix.
Use the combined files only; the two source extractions reuse chunk basenames.

For compatibility, the old `--team` option remains available as a deprecated
alias for `--s3-prefix` and defaults to `None`. The old `--chunk-prefix` option
is appended to the configured S3 prefix. New commands should use `--s3-prefix`.

The scorer deliberately requires `--all-chunks` or another explicit chunk
selection option; it will not guess which uploaded files should be processed.
`--skip-existing` skips a chunk only when its stored profile, model, prefilter,
prompt budget, source path, and other scoring settings match the current run.
`--flat-output` makes this resume behavior work across separate invocations.
Incompatible outputs stop that chunk instead of being silently reused.

Use a fresh scoring directory when first switching from the main-only chunks to
the combined chunks. Combined assembly changes chunk membership; old scores
must not be reused solely because their chunk numbers match. The examples use
`output/final_llm_extraction_completed` for this reason. Resume with
`--skip-existing` only while continuing the same combined input set.

The command exits nonzero when a chunk fails or any filing remains unscored.
Inspect the run manifest and repair or rerun incomplete chunks before merging.

`max_new_tokens` and temperature are fixed by the frozen research profile and
sent explicitly to Bedrock.
After scoring, aggregate the chunk outputs with:

```bash
python3 "code/main/2. get_ai_adoption/llm_score/merge_outputs.py" \
  --primary-dir output/final_llm_extraction_completed \
  --primary-label claude \
  --lookup-csv "code/main/2. get_ai_adoption/lookup/cik_year.csv" \
  --out-dir data/llm_score
```

This writes `data/llm_score/llm_extraction_filing_master.csv` at the default input
location used by the R panel builder. The aggregate snippet audit and firm-year
panel are saved in the same folder. Continue with the panel-building commands
in the [root README](../../../../README.md#reproduction-order).

## Interpretation For Empirical Use

The recommended main treatment is:

```text
ai_score ∈ {1, 2, 3}
```

Suggested interpretation:

- `1` means no current AI implementation is disclosed
- `2` means emerging or bounded AI implementation is disclosed
- `3` means established and integrated AI implementation is disclosed

For new extraction runs, this variable should be described in papers as a
disclosure-based AI adoption score built from AI-keyword sentence windows in
whole 10-K and 10-K/A primary documents using a frozen prompt and deterministic
post-processing rules.

## Limitations

- It measures disclosed adoption, not latent adoption.
- It depends on the research sample and EDGAR text extraction quality.
- The hard-zero prefilter excludes 1,304 non-empty extracts in the full corpus,
  and the 2,400-character evidence cap applies anchor-first selection to between
  2,959 and 4,106 model-bound extracts in the current through-2025 corpus. Use
  `--prefilter-mode off
  --max-prompt-chars 0` only when maximum recall takes priority over cost.
- Small LLMs can still make classification errors, which is why the workflow
  keeps prompt structure minimal and output parsing strict.
