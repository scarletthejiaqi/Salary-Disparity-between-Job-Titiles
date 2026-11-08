# U.S. Pay & Hiring Trends, 2019 to now

An interactive dashboard of **offered salaries for 52 professional roles in the U.S., by seniority, employer and location, from 2019 to now**, next to **market-wide hiring demand**. This is the **updated version (v2)** of my earlier coursework project, *Salary Disparity between Job Titles*, which inspired it. Version 1 was a Shiny app on a static Kaggle dataset of about 6,700 records with no time dimension. Version 2 keeps the same question, how pay differs across job titles and seniority, and rebuilds it on public, regularly updated government and industry data, with a reproducible pipeline, tests and documented methodology. The v1 code is preserved in the Git history (`main` branch before this rebuild).

Live app (after deployment): [shinyapps.io/Salary_Disparity_between_Job_Titles](https://scarlett-hjq.shinyapps.io/Salary_Disparity_between_Job_Titles/)

## What you can answer with it

| Page | Questions |
|---|---|
| **Overview** | What does the typical professional job offer now, and how much has that grown since 2019, before and after inflation? Which role families gained most? Where is hiring demand versus before the pandemic? |
| **Salary explorer** | How has pay for *my* role changed each year (or half-year), by seniority, for new hires, in nominal or inflation-adjusted dollars? |
| **Seniority ladder** | How much more does each step from junior to senior to director pay, and are those gaps widening? Is the share of entry-level jobs shrinking? |
| **Compare roles** | Which roles pay the most at the same level, which grew fastest since 2019, and which are seeing more or fewer filings? |
| **Hiring demand** | Are postings for my field up or down (Indeed)? Are advertised wages still rising? How many openings are there overall (JOLTS)? |
| **Employers & locations** | Which employers sponsor the most positions for a role and what do they offer? How does pay differ by state and city? |

Every chart has a **Table** tab and a CSV download. Statistics based on fewer than 30 filings are not shown.

## Data

| Source | What it adds | Coverage | Licence |
|---|---|---|---|
| [U.S. DOL OFLC LCA disclosure files](https://www.dol.gov/agencies/eta/foreign-labor/performance) (H-1B, H-1B1, E-3) | Job title, occupation, employer, worksite, **offered salary**, **DOL wage level (I–IV)** for every certified filing | Filings received Jan 2019 onward, about 5 million filings | Public domain |
| [Indeed Hiring Lab Job Postings Index](https://github.com/hiring-lab/job_postings_tracker) | Job postings by occupational sector, Feb 2020 = 100 | Feb 2020 onward, daily | CC BY 4.0 |
| [Indeed Hiring Lab Wage Tracker](https://github.com/hiring-lab/indeed-wage-tracker) | Year-over-year growth of advertised wages by sector | 2019 onward, monthly | CC BY 4.0 |
| [BLS JOLTS & CPI-U via FRED](https://fred.stlouisfed.org/) | Total job openings; inflation adjustment | Monthly | Public domain |

**Important caveat:** LCA filings cover positions that employers intend to fill with H-1B, H-1B1 or E-3 workers, mostly in tech, engineering, finance, healthcare, research and education. They are a strong benchmark for professional roles at sponsoring employers, not for every U.S. job. Read the in-app *Methodology* page before drawing conclusions, especially about 2026, when H-1B policy changed.

## How it works

```
DOL / Indeed / FRED  ──►  pipeline/ (R + a small Python xlsx reader)  ──►  data/*.csv  ──►  Shiny app (R/, app.R)
   ~3 GB raw              download → extract → clean & classify →          ~7 MB of            reads only the
   (data-raw/, ignored)   aggregate → market data                          aggregated tables   published tables
```

- **Role taxonomy**: 14 families and 52 roles. Ordered job-title rules come first, limited to compatible occupation (SOC) codes. They separate, for example, Data Engineer, ML Engineer, DevOps/SRE and Software Engineer, which all share SOC 15-1252. SOC codes (2018 and 2010) are the fallback. See `pipeline/lib/taxonomy.R`.
- **Seniority**: two views. One comes from job titles (Entry, Mid/Unspecified, Senior, Lead/Staff/Principal, Manager, Director & Executive; `pipeline/lib/seniority.R`). The other is DOL's prevailing-wage Levels I–IV. The methodology page cross-checks them.
- **Salary**: the lower bound of the offered range, annualised. Base pay only. Medians and percentiles come from individual filings, de-duplicated by case number (latest determination wins).

## Project structure

```
app.R                  Shiny entry point (bslib page_navbar)
R/                     App code, auto-sourced by Shiny: theme, data loading, metrics, charts, page modules
content/               Methodology and About pages (Markdown)
www/styles.css         Dashboard styles
data/                  Published, aggregated tables the app reads (committed)
pipeline/              Reproducible data pipeline
  00_config.R          Paths, source URLs, analysis constants
  01_download.R        Download DOL / Indeed / FRED files (cached)
  02_extract_lca.R     Stream needed columns from each DOL spreadsheet to parquet
  03_clean_lca.R       De-duplicate, filter, annualise, classify roles & seniority
  04_build_tables.R    Aggregate into the app tables
  05_market_data.R     Indeed, JOLTS and CPI tables
  lib/                 Pure, unit-tested helpers (taxonomy, seniority, wages, employers, ...)
  xlsx_stream.py       Low-memory streaming .xlsx reader
tests/testthat/        Unit tests (pipeline + app helpers) and app smoke tests
```

## Run it locally

Requirements: R ≥ 4.4. Rebuilding the data also needs Python 3 with `lxml`.

```sh
make setup          # install the locked package versions with renv
make run            # start the app at http://127.0.0.1:8080 (uses the committed data/)
make test           # unit + smoke tests
```

### Refresh the data

DOL publishes a new quarter roughly every three months, and Indeed updates weekly.

```sh
make data           # download everything new and rebuild data/ (first run: ~3 GB download, about 1 hour)
make data-tables    # rebuild data/ from already-extracted files only
```

The pipeline is memory-safe by default: it handles one file at a time, streams each spreadsheet (about 60 MB), runs at low CPU priority, and peaks at roughly 1 GB in the cleaning and aggregation steps. Later refreshes only process new DOL files. On a large machine you can parallelise extraction with `PIPELINE_CORES=3 make data`.

## Deploy

**Posit Connect Cloud (recommended):** at [connect.posit.cloud](https://connect.posit.cloud), choose *Publish → Shiny*, pick this GitHub repository, the `main` branch and `app.R`. Connect Cloud reads `manifest.json`; run `make manifest` and commit after changing packages or data. Turn on *Automatically publish on push* to redeploy on every push to `main`.

**shinyapps.io:**

```r
rsconnect::deployApp(appName = "Salary_Disparity_between_Job_Titles")
```

`.rscignore` keeps the pipeline, raw data and tests out of the deployment bundle.

## Author

Jiaqi (Scarlett) He · [Scarlett_hjq@outlook.com](mailto:Scarlett_hjq@outlook.com)

Data: U.S. Department of Labor OFLC; Indeed Hiring Lab (CC BY 4.0); U.S. Bureau of Labor Statistics via FRED. This project is for information only and is not legal, immigration or financial advice.
