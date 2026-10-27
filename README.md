# Automate to Compete: How Market Competition Shapes the Effects of AI

Research code and reproducibility instructions for the paper. This repository
contains source code and tests; rebuilding the analysis requires external data
and access to the services listed below.

- **Author:** Pio Francesco Medolla
- **Affiliation:** King's College London
- **Contact:** [pio.medolla@kcl.ac.uk](mailto:pio.medolla@kcl.ac.uk)
- **Version:** October 2026

## Suggested citation

Medolla, Pio Francesco. 2026. “Automate to Compete: How Market Competition
Shapes the Effects of AI.” King's College London, October 2026.

## Overview
This project studies firm-level AI adoption disclosed in SEC filings, its
relationship with firm outcomes, and differences associated with industry
concentration. The workflow combines:

- SEC filing text from EDGAR
- LLM-based AI adoption scores built from those filings
- Compustat annual accounting data
- External AI exposure measures matched by industry

## Reproducibility Requirements

Run all commands from the repository root. The code was last verified with
Python 3.12 and R 4.5.2. Raw filings, licensed Compustat data, credentials,
generated panels, caches, and model outputs are deliberately excluded from the
repository.

### External services and data

- **SEC EDGAR:** internet access and a truthful organization/contact email are
  required to download 10-K and 10-K/A filings. The extraction stage uses the
  official SEC submissions metadata and filing archives.
- **WRDS/Compustat:** a valid WRDS subscription is required to rebuild the
  research-sample lookup and annual accounting panel. Set `COMPUSTAT_USER` and
  `COMPUSTAT_PSW` in the local environment; never save them in this repository.
  Compustat source data are licensed and are not redistributed here.
- **AWS Bedrock:** rebuilding the filing-level AI scores requires an AWS account,
  credentials discoverable by the standard `boto3` credential chain, access to
  the configured Anthropic model in the selected AWS region, and permission to
  invoke it through the Bedrock Converse API. The frozen profile currently uses
  `eu.anthropic.claude-sonnet-4-6`. Model calls can incur AWS charges.
- **AWS S3 (optional):** extracted RDS chunks may be read from S3 using
  `--s3-bucket` and `--s3-prefix`. S3 is not required when `--input-dir` points
  to local chunks.
- **Public reference data:** download the [2022 NAICS structure workbook from
  the Census Bureau](https://www.census.gov/naics/) and save it as
  `data/2022_NAICS_Structure.xlsx`. The BTOS script downloads the Census AI Core
  Questions workbook when it is absent. The optional validation stage also
  requires the Census ABS module file `AB2200MCB01.dat` in `data/`.
- **Industry AI exposure:** download [`AIOE_DataAppendix.xlsx` from the
  authors' repository](https://github.com/AIOE-Data/AIOE/blob/main/AIOE_DataAppendix.xlsx)
  and save it as `data/AIOE_DataAppendix.xlsx`. The panel builder reads the
  `Appendix B` sheet for the AIIE measure by four-digit NAICS. This file is
  not included in this repository. Credit: Felten, Raj, and Seamans (2021),
  *Occupational, industry, and geographic exposure to artificial intelligence:
  A novel dataset and its potential uses*, Strategic Management Journal 42(12),
  2195–2217.

The generated `cik_year.csv` lookup is intentionally not distributed because it
is derived from the licensed Compustat sample. Recreate it after setting the WRDS
environment variables:

```bash
Rscript "code/main/2. get_ai_adoption/get_cik_year.R"
```

### Python packages

Create an isolated environment and install both stage-specific requirement
files:

```bash
python3 -m venv .venv
source .venv/bin/activate
python3 -m pip install --upgrade pip
python3 -m pip install -r "code/main/1. get_edgar_extract/requirements.txt"
python3 -m pip install -r "code/main/2. get_ai_adoption/llm_score/requirements.txt"
```

The direct Python dependencies are `pandas`, `requests`, `beautifulsoup4`,
`lxml`, `pyreadr`, and `boto3` (including its `botocore` dependency).
The scorer requires `boto3 >= 1.34.131` for Bedrock Converse; the minimum version
is covered by an offline test against the real AWS SDK service model.

### R packages

Install the direct R dependencies before running the pipeline:

```r
install.packages(c(
  "DBI", "data.table", "did", "dplyr", "DRDID", "edgar", "fixest",
  "ggplot2", "janitor", "patchwork", "purrr", "readr", "readxl",
  "RPostgres", "scales", "stringr", "tibble", "tidyr", "tidyverse",
  "usethis"
))
```

The last verified package versions were: DBI 1.2.3, data.table 1.18.2.1,
did 2.5.0, dplyr 1.2.0, DRDID 1.3.0, edgar 2.0.8, fixest 0.14.2,
ggplot2 4.0.1, janitor 2.2.1, patchwork 1.3.2, purrr 1.2.1, readr 2.1.6,
readxl 1.4.5, RPostgres 1.4.8, scales 1.4.0, stringr 1.6.0, tibble 3.3.1,
tidyr 1.3.2, tidyverse 2.0.0, and usethis 3.2.1.

### Reproduction order

1. Install the Python and R dependencies above.
2. Generate `code/main/2. get_ai_adoption/lookup/cik_year.csv` from WRDS.
3. Follow the [EDGAR extraction instructions](code/main/1.%20get_edgar_extract/README.md)
   to run the main filing-year extraction, generate and extract the fiscal-2025
   completion sample, and combine both runs. Score the combined RDS set under
   `cache/edgar_keyword_windows_combined/assembled`. This includes fiscal-2025
   reports submitted in 2026, while retaining their actual filing dates.
4. Follow the [LLM scoring instructions](code/main/2.%20get_ai_adoption/llm_score/README.md)
   to score and merge the chunks. This is the stage that requires Bedrock.
   The examples write chunk scores to `output/final_llm_extraction_completed` and merge
   them into `data/llm_score`. The panel builder then reads
   `data/llm_score/llm_extraction_filing_master.csv` directly.
5. Create `data/` and place the NAICS and AIIE workbooks at the paths listed
   above, then run scripts 3 and 4 in
   `code/main/3. get_panel_data` to build the NAICS and BTOS inputs.

   ```bash
   mkdir -p data
   Rscript "code/main/3. get_panel_data/3. build_naics_lookup_tables.R"
   Rscript "code/main/3. get_panel_data/4. build_btos_q7_validation.R"
   ```

6. Build and save the annual, merged, and final analysis panels using the panel
   entry point below. For a fresh download, set `COMPUSTAT_USER` and
   `COMPUSTAT_PSW` locally first, then run:

   ```bash
   Rscript -e 'BUILD_PANEL_DATA <- TRUE; REBUILD_ANNUAL_PANEL <- TRUE; REFRESH_FROM_WRDS <- TRUE; source("code/main/4. build_or_load_panel_data.R")'
   ```

   This downloads or replaces `data/comp_funda.csv` from WRDS, including the
   pre-2015 history needed for lagged controls. The entry point sets
   `SAVE_OUTPUTS <- TRUE` and `SAVE_MERGED_OUTPUTS <- TRUE` before running the
   annual and merged panel scripts, then saves
   `data/compustat_ai_analysis_panel.rds` for the analysis scripts.

   To rebuild from an existing licensed `data/comp_funda.csv` without downloading
   it again, use this command instead:

   ```bash
   Rscript -e 'BUILD_PANEL_DATA <- TRUE; REBUILD_ANNUAL_PANEL <- TRUE; REFRESH_FROM_WRDS <- FALSE; source("code/main/4. build_or_load_panel_data.R")'
   ```

   Both commands rebuild and replace the generated panels. The panel builder
   reads the two source manifests generated in step 3:
   `cache/edgar_keyword_windows/filing_manifest.csv` and
   `cache/edgar_keyword_windows_fy2025_completion/filing_manifest.csv`.

7. Run analysis scripts 5--11 in numerical order. Script 5 additionally requires
   the Census ABS module; it is a validation stage rather than an input to the
   main treatment construction.

Exact numerical reproduction requires access to the same licensed Compustat
sample and external model service. The repository supplies the code, frozen
prompt/profile, matching rules, and tests needed to audit and rerun the workflow,
but it does not redistribute restricted source data or credentials.

### Offline verification

The automated tests do not make live SEC, WRDS, S3, or Bedrock requests:

```bash
python3 -m unittest discover \
  -s "code/main/1. get_edgar_extract" -p "test_*.py"
python3 -m unittest discover \
  -s "code/main/2. get_ai_adoption/llm_score" -p "test_*.py"
Rscript "code/main/2. get_ai_adoption/llm_score/test_build_ai_scoring_examples.R"
```

## Overall Process
1. Extract high-recall AI keyword windows from whole SEC 10-K and 10-K/A
   primary documents, including the fiscal-2025 completion for reports filed in
   2026. The active workflow and run commands are documented in
   [the EDGAR extraction README](code/main/1.%20get_edgar_extract/README.md).

2. Build firm-year AI adoption measures from filings.
   Main scripts live in
   [`code/main/2. get_ai_adoption`](code/main/2.%20get_ai_adoption), with the
   reproducible LLM scoring methodology documented in the
   [LLM scoring README](code/main/2.%20get_ai_adoption/llm_score/README.md).

3. Build the annual Compustat panel and merge AI measures.
   Main scripts live in
   [`code/main/3. get_panel_data`](code/main/3.%20get_panel_data):
   [`1. build_compustat_annual_panel.R`](code/main/3.%20get_panel_data/1.%20build_compustat_annual_panel.R)
   and
   [`2. build_compustat_ai_panel.R`](code/main/3.%20get_panel_data/2.%20build_compustat_ai_panel.R).

   The EDGAR merge first uses exact `CIK + report_date` matches. Remaining
   Compustat periods may use the unique nearest SEC report date for the same CIK
   within `-7 <= business-day gap <= +7`. Exact matches take precedence;
   ambiguous matches and reused filings are rejected. The saved
   `data/compustat_ai_match_audit.csv` records the method and date gap for every
   fiscal period.

4. Build or load the final analysis panel.
   See
   [`4. build_or_load_panel_data.R`](code/main/4.%20build_or_load_panel_data.R).

5. Validate the treatment variables and generate panel diagnostics and summary
   statistics using scripts 5--7 in `code/main`.

   Table 3.5 and all DID models draw from the same 32,537 firm-years with a
   contemporaneously matched and scored annual filing. The raw winsorized
   R&D-intensity ratio (`XRD` divided by beginning assets) is reported only
   when Compustat reports `XRD` and the denominator is valid; its 18,168
   nonmissing firm-years are therefore reporter observations.
   The first-adoption determinants regressions use lagged R&D instead. They set
   lagged R&D intensity to zero only for observations with a lagged R&D-reporter
   indicator of zero and include that indicator separately, so the intensity
   coefficient is conditional on the disclosure margin. Reporters whose ratio
   is undefined remain missing and are excluded when R&D intensity enters a
   model. Consequently, the 23,195-row joint regression contains 12,320
   reported-intensity observations and 10,875 zero-filled non-reporter
   observations; it is not a complete-case sample on raw R&D intensity.
   The generated `ai_adoption_rd_missingness_audit.csv` records this bridge.

6. Estimate the main Callaway--Sant'Anna models using the unbalanced,
   cohort-specific `g-1` doubly robust design. Treatment cohorts start in
   2016, using 2015 as the first pre-treatment outcome and covariate year.
   The main cohort is the first Compustat fiscal period after the first
   qualifying 10-K filing date. The first positive report-period cohort is
   retained as a separate timing comparison. The main `g-1` outcome can
   already reflect AI described in the filing, so interpret the main ATT as
   an incremental post-disclosure change, not the full effect of initial AI use.
   The implemented outcomes are employment, labour productivity, sales,
   operating costs, and operating profitability. See
   [`9. did.R`](code/main/9.%20did/9.%20did.R).

   The sample reconciliation script writes
   `sample_reconciliation_appendix.md` and model-level audit tables after the
   DID models. Run it and the timing audit with:

   ```bash
   Rscript "code/main/9. did/9. did_sample_reconciliation.R"
   Rscript "code/main/9. did/9. did_timing_audit.R"
   ```

   The timing audit writes `output/timing_comparison/treatment_timing_audit.csv`
   for the post-filing and first-positive-report cohort definitions. To run the
   report-period DID and HHI sensitivity in separate output directories:

   ```bash
   DID_2_COHORT_VAR=ai_report_adoption_year DID_3_COHORT_VAR=ai_report_adoption_3_year DID_OUTPUT_DIR=output/timing_comparison/report_period_cohort/did_firm_outcomes Rscript "code/main/9. did/9. did.R"
   HHI_2_COHORT_VAR=ai_report_adoption_year HHI_3_COHORT_VAR=ai_report_adoption_3_year HHI_OUTPUT_ROOT=output/timing_comparison/report_period_cohort Rscript "code/main/10. hhi_hetero/10. hhi_heterogeneity_did.R"
   ```

7. Estimate HHI heterogeneity using the preferred main specification and test
   the difference between high- and low-competition market ATTs. For each
   2016--2025 treatment cohort `g`, treated firms and eligible controls are
   classified using their own NAICS3 market HHI in `g-1` (2015--2024).
   Classification is held fixed within that cohort's comparisons. A comparison
   firm can appear in different regimes across cohorts, so the high-minus-low
   bootstrap aligns shared firms and clusters by each firm's first-observed
   NAICS3 market. Sparse placebo cells with fewer than five treated firms are
   excluded from pre-trend aggregation; all post-treatment cells must be
   estimable. Each reported subgroup N counts unique contributing firm-years.
   See
   [`10. hhi_heterogeneity_did.R`](code/main/10.%20hhi_hetero/10.%20hhi_heterogeneity_did.R).

   ```bash
   Rscript "code/main/10. hhi_hetero/10. hhi_heterogeneity_did.R"
   Rscript "code/main/10. hhi_hetero/10. hhi_dynamic_plots.R"
   Rscript "code/main/10. hhi_hetero/10. hhi_common_cohort_contrast.R"
   ```

   The third command checks the high-minus-low contrast with identical
   treated-cohort weights in both regimes. Its multiplier-bootstrap inference
   conditions on the observed pooled cohort shares. The main table keeps the
   standard `did::aggte(type = "group")` weights for each regime.

   The former fixed-2015 headline tables are archived in
   `output/hhi_heterogeneity_fixed_2015_snapshot/` for the design comparison.

8. Test whether continuous sales-based NAICS3 concentration in `t-1` predicts
   first AI adoption in `t`. This event year uses the same post-filing cohort
   as the main DID. The firm-year risk-set model uses the firm's own
   prior-year market HHI and prior-year controls, year and NAICS3 fixed effects,
   and NAICS3-clustered standard errors. Thus, a cohort-`g` event always uses
   HHI measured in `g-1`. Continuous HHI is the primary specification and the
   only competition measure in the main table. A binary high-competition model
   (`HHI <= 1,800`, with low competition as the reference) is retained only as
   a separately labelled robustness check. All control coefficients are saved
   in a separate diagnostic table.
   See
   [`11. hhi_ai_determinant.R`](code/main/11.%20hhi_ai_determinant.R).

   ```bash
   Rscript "code/main/11. hhi_ai_determinant.R"
   ```

## Main Outputs
- Annual Compustat panel
- Merged Compustat + AI adoption and exposure panel
- Matched AI panel and unmatched observations
- Diagnostics of panel data
- Analysis tables/figures
- Main cohort-specific `g-1` Callaway--Sant'Anna estimates
- HHI subgroup ATTs and formal high-minus-low competition contrasts
- Lagged continuous-NAICS3-HHI first-adoption determinant estimates
- Fixed-2015 continuous-HHI models of first and strong AI adoption

## Analysis Scope

The completed outcome analysis estimates how disclosed AI adoption relates to
employment, labour productivity, sales, operating costs, and operating
profitability. The market-structure analysis studies heterogeneous effects by
industry concentration and whether lagged NAICS3 HHI predicts first AI
adoption. Firm size, profitability, cash, R&D reporting, firm age, and capital
expenditure intensity enter selected specifications as pre-treatment controls
rather than as additional outcomes.
