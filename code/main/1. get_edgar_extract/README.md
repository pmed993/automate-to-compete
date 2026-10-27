# EDGAR whole-filing keyword extraction

This stage retrieves high-recall AI disclosure candidates from whole SEC annual
filings. It is the project's only EDGAR extraction workflow.

## What it does

1. Reads the project's existing `cik_year.csv` research keys. The main run uses
   filing years 2015–2025; a separate completion run retrieves fiscal-2025
   reports filed in 2026 for firms missing that report year in the main run.
2. Uses the SEC `submissions.zip` bulk metadata to identify exact `10-K` and
   `10-K/A` filings. If a referenced historical continuation page is not in the
   bulk file, it is fetched and cached from the official Submissions API.
3. Matches the lookup's `year` to SEC `filingDate` by default. This preserves
   the project's established filing-calendar-year key. `reportDate`, report
   year, and filing year are also retained for audit.
4. Downloads the filing's `primaryDocument`, not every exhibit. If SEC metadata
   lacks a primary document, the complete-submission text is an explicit,
   last-resort fallback; disable it with
   `--no-complete-submission-fallback`.
5. Uses a storage-bounded raw batch (35 GiB by default), extracts each filing,
   checkpoints its result or error, and deletes the batch's primary documents
   before downloading the next batch.
6. Converts the whole primary filing to readable text and applies the existing
   `AI_KEYWORDS` regex dictionary from `ai_adoption_utils.py`.
7. Extracts every hit sentence with a configurable number of surrounding
   sentences, defaulting to `±1`.
8. Merges overlapping or immediately adjacent windows and removes only exact
   whitespace-normalized duplicate chunks.
9. Writes long-format RDS chunks that the existing AI-adoption pipeline can
   read. Filings without a dictionary hit receive a metadata-only, empty-text
   sentinel row so the scorer still emits its normal hard-zero observation and
   the research-sample denominator is not lost.

This stage does not rank evidence, impose a character budget, pick a best
window, or decide whether a mention proves adoption. Ambiguous dictionary terms
are deliberately retained here; disambiguation and adoption scoring remain in
`get_ai_adoption`.

## Required research-sample lookup

The default input is
`code/main/2. get_ai_adoption/lookup/cik_year.csv`. It is generated from the
project's licensed Compustat sample and is therefore not distributed in the
public repository. With `COMPUSTAT_USER` and `COMPUSTAT_PSW` set locally, create
it from the repository root with:

```bash
Rscript "code/main/2. get_ai_adoption/get_cik_year.R"
```

Alternatively, pass `--lookup-csv /path/to/cik_year.csv`. A replacement must
contain unique `cik` and `year` columns and will define a different research
sample.

## Main filing-year run

Create an environment once:

```bash
python3 -m venv .venv-edgar
source .venv-edgar/bin/activate
python3 -m pip install -r "code/main/1. get_edgar_extract/requirements.txt"
```

Then run from the repository root using a truthful SEC contact identity:

```bash
python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" run \
  --company "YOUR_ORGANIZATION" \
  --email "you@example.com" \
  --raw-batch-gib 35 \
  --context-sentences 1 \
  --extract-workers 2
```

`--raw-batch-gib 35` is a temporary **disk-space** target, not a request to
hold 35 GiB in RAM. Downloads can exceed the target by at most the few filings
already in flight. Keep at least 10–15 GiB additional free for the SEC metadata
archive, unusually large filings, manifests, window JSON and assembled output.
Use a smaller value if the volume has less than roughly 50 GiB free.

Defaults:

- lookup: `code/main/2. get_ai_adoption/lookup/cik_year.csv`
- work directory: `cache/edgar_keyword_windows`
- year rule: SEC filing year
- forms: both `10-K` and `10-K/A`
- SEC request rate: 8 requests/second, capped at 10
- extraction workers: 2 by default and recommended on this 8 GiB machine
- temporary raw batch: approximately 35 GiB
- temporary primary documents: deleted after every raw batch
- manifest checkpoint interval: 250 completed filings
- storage grouping: 1,000 whole filings per output file

The bulk metadata, manifest, and per-filing results are resumable. A second run
reuses valid extraction results without re-downloading deleted primary
documents. Failed extraction inputs are also deleted after their error is
checkpointed, preventing failures from accumulating outside the storage
budget; a rerun downloads those filings again. A full `run` refuses to assemble
outputs while any filing remains incomplete. Output chunks are protected from
accidental overwrite; pass `--overwrite-chunks` when intentionally rebuilding
them.

Changing `--context-sentences` invalidates old per-filing windows and causes the
affected primary documents to be downloaded again. Pass `--refresh-metadata`
when a later run should replace the cached nightly SEC bulk archive.

Use `--keep-raw` only when intentionally retaining the complete raw corpus; in
that mode the 35 GiB batch boundary still controls processing, but disk usage
accumulates across batches.

Every full `run` writes `qa_summary.json` automatically. QA reopens every saved
filing result and checks its source hash, extraction version, accession, window
count, source-order IDs, text hashes, duplicate hashes, and that every recorded
matched term occurs in its window. Assembly is refused when status or result
validation is incomplete. It also reopens every RDS chunk, compares its row
count with the compressed CSV mirror, and verifies that assembled accessions
exactly match the filing manifest.

## Required fiscal-2025 completion and combined scoring inputs

The paper's fiscal-2025 accounting outcomes require reports that firms submitted
in 2026. Stopping at the main filing-year extraction omits these reports. Run
the following steps after the main run, using the same Python environment and
repository-root working directory.

First, generate the completion lookup. The helper selects CIKs with a 2025 key
in the research lookup but no report-year-2025 filing in the main manifest:

```bash
python3 "code/main/1. get_edgar_extract/fiscal_2025_completion.py" lookup
```

This writes
`code/main/2. get_ai_adoption/lookup/cik_year_fy2025_completion.csv`. The recorded
project manifests reproduce the existing 3,979-CIK completion lookup with this
rule. A later refresh of SEC or Compustat data can change that count.

Then extract those firms' report-year-2025 filings into a separate directory.
Reusing the main run's submissions archive keeps both runs on the same metadata
snapshot:

```bash
python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" run \
  --lookup-csv "code/main/2. get_ai_adoption/lookup/cik_year_fy2025_completion.csv" \
  --year-match report-date \
  --data-dir cache/edgar_keyword_windows_fy2025_completion \
  --submissions-zip cache/edgar_keyword_windows/sec/submissions.zip \
  --company "YOUR_ORGANIZATION" \
  --email "you@example.com" \
  --raw-batch-gib 35 \
  --context-sentences 1 \
  --extract-workers 2
```

Finally, combine the two validated extractions:

```bash
python3 "code/main/1. get_edgar_extract/fiscal_2025_completion.py" merge
```

The helper keeps the main run's 2015–2025 rows, adds completion accessions not
already present, and gives the main record precedence when an accession occurs
in both manifests. It reassembles the original JSON windows into one globally
numbered RDS set, retaining no-hit sentinels and preventing chunk-name collisions.
Existing combined chunks are reused only when compatible; use
`merge --overwrite-chunks` to intentionally rebuild stale combined outputs.

The recorded manifests contain 43,103 in-window main filings and 3,704 completion
rows, with one shared accession: **46,806 unique scoring inputs**, including
**3,703 newly added filings submitted in 2026 for fiscal 2025**.

The completion rows use `year = 2025` as their research/scoring key. Their real
`filing_date` and `filing_year = 2026` remain unchanged. The R panel builder uses
the real filing date for treatment timing, so completion does not backdate an
AI-adoption event into 2025. Filing and report dates remain ISO text in the RDS
files so R and Python read them consistently.

Combined outputs are under `cache/edgar_keyword_windows_combined`:

```text
filing_manifest.csv                 unique combined accession list
completion_merge_audit.json          source hashes, counts, duplicate precedence
qa_summary.json                     combined extraction QA
assembled/extract_df_chunk_*.rds     use this folder for local scoring
assembled/extract_df_chunk_*.csv.gz
data_workspace_rds/                 upload this RDS-only folder for S3 scoring
data_workspace_rds.zip
```

Point the scorer at `cache/edgar_keyword_windows_combined/assembled`, or upload
the combined RDS-only folder to one clean S3 prefix. Do not mix the two original
RDS directories: each starts numbering at `extract_df_chunk_00001.rds`. The
R panel builder reads both original source manifests at their default paths;
the scoring-example table can use the combined manifest and combined RDS set.

## Deterministic live samples

Use `--sample-filings` to exercise the complete workflow without processing the
entire research sample:

```bash
python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" run \
  --company "YOUR_ORGANIZATION" \
  --email "you@example.com" \
  --sample-filings 1100 \
  --raw-batch-gib 35 \
  --extract-workers 2
```

Sampling is deterministic and proportional across form type and filing year.
It prefers distinct CIKs within each stratum, then fills any remaining quota.
Change the stable selection with `--sample-seed`. Selected accessions and
stratum counts are saved in `sample_selection.json`; sample CIK-year keys are
saved in `sample_lookup.csv`, and the complete unsampled manifest is retained
as `sec/full_filing_manifest.csv.gz` for audit.

Pass an existing archive with `--submissions-zip /path/to/submissions.zip` to
reuse SEC bulk metadata instead of downloading another approximately 1.5 GiB
copy.

Keep `--year-match filing-date` for the main run. Use `--year-match report-date`
with the generated fiscal-2025 completion lookup as documented above.

## Capacity and runtime estimate

The main project lookup contains 49,200 requested CIK-year keys across 5,479 CIKs
and filing years 2015–2025. Removing 2026 leaves 43,103 matching extracted
filings: 38,433 `10-K` and 4,670 `10-K/A`. A later refreshed SEC archive may
differ slightly.

The transfer and runtime benchmarks below cover the main filing-year run. Allow
additional time for the fiscal-2025 completion and combined assembly.

A stratified sample of 1,100 actual SEC primary documents across years and
forms transferred 3.30 GiB and had these transfer sizes:

- overall median: 2.39 MiB
- overall mean: 3.07 MiB
- largest sampled primary document: 67.8 MiB
- weighted projection: about 130 GiB of primary documents
- official submissions metadata ZIP: about 1.45 GiB at the time tested

With the default 35 GiB temporary raw budget, the projected 130 GiB transfer is
processed in approximately four raw batches instead of being retained at
once. Plan for roughly 45–50 GiB free at startup. Final requirements depend on
the number and length of keyword windows, but they should be far below the full
raw corpus. If space becomes constrained, lower `--raw-batch-gib`; this changes
only the number of processing batches, not the extracted content.

At the default 8 SEC requests/second, approximately 43,103 filing requests have
an absolute request-start floor of about 1.5 hours. Approximate raw-transfer floors for
130 GiB are:

| Sustained download speed | Transfer floor |
|---:|---:|
| 20 Mbit/s | 14.5 hours |
| 50 Mbit/s | 5.8 hours |
| 100 Mbit/s | 2.9 hours |
| 200 Mbit/s | 1.5 hours, where the SEC request rate also becomes limiting |

Local sparse-hit benchmarks after the parser and matcher optimizations achieved
about 5.6 MiB/s with one extraction process and 20 MiB/s with four processes on
2 MiB documents. That implies roughly 1.3 hours of pure extraction at four
workers; real inline-XBRL complexity and disk I/O can increase this.

A practical end-to-end expectation, including metadata, HTTP overhead, retries,
local extraction, and assembly, is:

- fast stable 100+ Mbit/s connection: approximately 5–9 hours
- typical 50 Mbit/s connection: approximately 8–13 hours
- 20 Mbit/s connection: approximately 17–24 hours

For planning, run it overnight and allow up to 24 hours. The pipeline checkpoints
downloads and per-filing extraction, so an interrupted run can be restarted with
the same command.

## Run individual stages

The `run` command is the recommended storage-bounded workflow. The individual
commands remain available for debugging and controlled reruns:

```bash
python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" manifest \
  --company "YOUR_ORGANIZATION" --email "you@example.com"

python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" download \
  --company "YOUR_ORGANIZATION" --email "you@example.com"

python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" extract \
  --context-sentences 1

python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" assemble

python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" qa
```

The standalone `download` command intentionally downloads and retains every
primary filing; it does **not** apply the 35 GiB purge cycle. Do not use it for
the full sample unless the destination has space for the entire raw corpus.

## Outputs and handoff

Important outputs under `cache/edgar_keyword_windows` are:

```text
filing_manifest.csv
sample_lookup.csv                       sample-only CIK-year keys
sample_selection.json                  deterministic sample audit
sec/metadata_coverage.csv             every requested CIK-year, including misses
sec/full_filing_manifest.csv.gz       unsampled manifest for sample runs
raw/10-K/...                          temporary primary documents
raw/10-K-A/...                        temporary amendments
windows/10-K/...                      auditable per-filing JSON results
windows/10-K-A/...
assembled/extract_df_chunk_*.rds      scorer-compatible inputs
assembled/extract_df_chunk_*.csv.gz
assembled/extraction_summary.json
data_workspace_rds/extract_df_chunk_*.rds  upload-ready RDS-only folder
data_workspace_rds.zip                    OneDrive transfer bundle
qa_summary.json
```

After a successful full run, `raw/` should contain no successfully extracted
filings. The manifest retains their source URLs, byte counts, SHA-256 hashes,
processing-batch identifiers and deletion timestamps for audit and resume.

Every RDS window row contains the existing required fields:

```text
item = keyword_window
year
accession_number
cik
form_type
text
```

It also contains window offsets, keyword names, source URL, filing/report
dates, primary-document name, hashes, and extraction version. All windows for
one accession stay in one RDS file, so a filing cannot be split across storage
chunks. A no-hit sentinel is marked with `window_id = 0` and
`is_no_keyword_sentinel = TRUE`; it contains no filing text and is not counted
as a keyword window in the extraction summary.

After successful assembly validation, the pipeline automatically copies the
exact final RDS set into `data_workspace_rds/` and creates
`data_workspace_rds.zip`. The folder contains no CSV mirrors or temporary
filings. Transfer the ZIP using an approved method, then either use the
extracted RDS directory locally or upload the files to a clean AWS S3 prefix
used by `get_ai_score_bulk.py`.

To recreate the folder and ZIP later without rerunning SEC downloads or text
extraction:

```bash
python3 "code/main/1. get_edgar_extract/edgar_keyword_extract.py" package \
  --data-dir cache/edgar_keyword_windows
```

The package command revalidates the assembled chunks against the filing
manifest before copying anything. The ZIP uses the RDS filenames at its root,
so extraction produces the upload-ready files directly. The scorer joins all
`keyword_window` rows for a filing in source order and keeps both `10-K` and
`10-K/A`.

## Dependencies

Python dependencies are listed in this folder's `requirements.txt`. Assembly
also requires `Rscript` and the R package `data.table`, matching the existing
RDS workflow.
