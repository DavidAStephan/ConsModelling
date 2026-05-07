# Code guide — Australian household consumption model

A walkthrough of every R file in the project. Intended for someone who
has just cloned the repository and wants to understand what's where, how
data flows through the pipeline, and where to make a particular kind of
change.

If you are looking for a summary of:

- the **modelling content** (specifications, results, decisions) → see [`project_status.md`](project_status.md)
- the **data sources** (ABS catalogues, RBA tables, the master dataset) → see [`data.md`](data.md)
- the **paper draft** → see [`wp_draft.md`](wp_draft.md)
- the **outstanding modelling work** → see [`next_steps.md`](next_steps.md)

This guide is the **code-level companion** to those documents.

---

## 1. Architecture in one picture

The pipeline is a two-stage process (data construction → estimation)
plus utilities. There are **three execution modes** that share the
estimation stage but differ in how they build the master dataset:

```
                            ┌────────────────────────────────────┐
                            │ ABS workbooks  (data_raw/*.xlsx)   │
                            │ RBA mortgage rate (rba_filrhlbvs.csv)
                            │ ABS 15+ population (population_workingage.csv)
                            │ Income components (household_income.csv)
                            │ RBA E13 burden (e13-data.csv)
                            └────────────────────────────────────┘
                                            │
                                            ▼
   Mode (1): COLD REBUILD                 ┌─────────────────────────┐
   australia_consumption_model.R   ─────►│ australia_data_download │
                                          │ produces: master tibble │
                                          │ -> outputs/*_dataset.rds│
                                          └────────────┬────────────┘
                                                       │
   Mode (2): FAST REPLAY                               │
   run_estimation_from_rds.R       ─── reads RDS ──────┤
                                                       │
   Mode (3): OFFLINE / HAND-EDIT                       │
   load_master_from_csv.R          ─── reads CSV ──────┤
                                                       │
                                                       ▼
                                          ┌─────────────────────────┐
                                          │ australia_estimation.R  │
                                          │ 16 pipeline steps:      │
                                          │  1-3  build model_data  │
                                          │  4    estimate 8 specs  │
                                          │       across 4 samples  │
                                          │  5    results tables    │
                                          │  6    cointegration     │
                                          │  7-12 robustness suite  │
                                          │  13   italy comparison  │
                                          │  14   model summary     │
                                          │  15   plots             │
                                          │  16   williams compare  │
                                          └────────────┬────────────┘
                                                       │
                                                       ▼
                            ┌────────────────────────────────────┐
                            │  outputs/                          │
                            │  ~30 CSVs + ~10 PNGs + summary.md  │
                            └────────────────────────────────────┘
```

Auxiliaries:

- `model_helpers.R` — shared utility functions, sourced by both Part 1
  and Part 2. Includes the dead-code Williams CCI infrastructure
  (`build_williams_cci_basis`, etc.) that is wired in conditionally.
- `export_master_csv.R` — one-shot script that converts the cached RDS
  to a portable CSV (Mode 3 input).
- `williams_comparison.R` — post-estimation analysis script. Builds the
  side-by-side comparison table against Williams (2010, 2012) and
  writes the markdown commentary that anchors the WP §9. Sourced by
  `australia_estimation.R` as Step 16.

---

## 2. The eight R files in detail

### 2.1 `australia_consumption_model.R` (97 lines) — master orchestrator

**Role.** Top-level entry point for **Mode 1** (cold rebuild). Sources
the data download, sources the estimation script, and prints status
banners between the two stages. Run this when you want a complete
end-to-end pipeline including data refresh.

**Invocation.** `Rscript Ausreplication/R/australia_consumption_model.R`

**What it does, line-by-line.**
- Lines 18–33: load required packages.
- Lines 38–50: figure out the script's own location so paths work
  whether the script is run via Rscript or sourced interactively.
- Lines 51–55: sources `australia_data_download.R`. After this line,
  the variable `master` exists in the local environment as a tibble of
  ~85 columns × 180 quarterly rows.
- Lines 62–86: rename three columns (`d_ln_cons_pc → dlcons`,
  `ln_ydi_real_pc → lincome`, `ln_cons_real_pc → lcons`) and add the
  canonical ECM term `ecm_lag = lag(lcons, 1) - lincome`. The result
  is a tibble called `model_data` that the estimation script expects
  to find in its environment.
- Lines 88–97: source `australia_estimation.R`, which executes its
  full 16-step main block.

**Modify when:**
- You want to add a new column rename.
- You want to change the ECM convention (currently negative-restoration
  Engle-Granger; flip back to positive-restoration by changing line 80
  to `ln_y_over_c = lincome - lag(lcons, 1)` — note this also requires
  changes downstream).

**Don't modify when:** doing routine analysis; this script is glue, not
content.

---

### 2.2 `australia_data_download.R` (1,285 lines) — data construction

**Role.** Part 1 of the pipeline. Builds the `master` tibble from raw
inputs (ABS workbooks, user-supplied CSVs, optional live RBA fetches).
Saves the result to `outputs/australia_model_dataset.rds` for later
reuse.

**Run directly?** Not normally. It's sourced by either
`australia_consumption_model.R` (Mode 1) or by an interactive session.
It reads from `data_raw/` and writes to `outputs/`.

**Internal section structure.** The file is organised in numbered
sections corresponding to logical data-construction stages:

| Section | Contents | Approx. lines |
|---|---|---|
| 1 | Date spine 1980Q1–2024Q4 (180 obs) | 173–177 |
| 2 | Raw input loading (10 ABS workbooks + 4 user CSVs) | 180–710 |
| 3 | Master tibble assembly via left-joins | 740–805 |
| 4 | Real per-capita variables, deflation, scaled-income, NPY | 810–870 |
| 5 | Annualised income and wealth ratios | 875–930 |
| 6 | House-price splice (3-layer chain) | 935–960 |
| 7 | Credit conditions (housing-loan-flow + optional Williams basis) | 970–1010 |
| 8 | Mortgage burden (synthetic) and RBA E13 burden | 1015–1075 |
| 9 | Coverage report and assertions | 1080–1130 |
| 10 | Save outputs (RDS + coverage CSV) | 1135–1285 |

**Key configuration variable** (top of file, around line 70):
```r
USE_INSTITUTIONAL_CCI <- FALSE  # toggle Williams 4-knot SDMMA spline
```
Default `FALSE` preserves the simpler observable-CCI path. Setting
`TRUE` attaches the four `sdmma_*` smoothed-step columns to `master`,
enabling Spec 8 in `australia_estimation.R`.

**Key inputs (data_raw/):**

| File | Purpose |
|---|---|
| `5206008_*.xlsx` | ABS HFCE (consumption real + nominal) |
| `5206020_*.xlsx` | ABS Household Income Account (legacy GDI source) |
| `5232035.xlsx` | ABS Household Balance Sheet — *binding sample start 1988Q3* |
| `560101.xlsx` | ABS Lending Indicators (housing flow, FHB share) |
| `6202001.xlsx` | ABS Labour Force (unemployment rate) |
| `641601.xlsx`, `643201.xlsx` | ABS house price indexes |
| `3101059.xlsx` | ABS ERP (used for prime-age share, NOT for total population) |
| `houseprice_old.csv` | Pre-2003 house price back-fill |
| `rba_filrhlbvs.csv` | **User-supplied** RBA F6 mortgage rate (1959+) |
| `population_workingage.csv` | **User-supplied** ABS A84423091W 15+ population |
| `household_income.csv` | **User-supplied** 10 income components for Williams NPY |
| `e13-data.csv` | RBA E13 housing-loan-payment burden |

For full provenance see [`data.md`](data.md).

**Modify when:**
- You're sourcing a new input series.
- An ABS workbook vintage changes and the regex pattern in
  `pick_abs()` no longer matches.
- You want to change a unit conversion or splice point.

---

### 2.3 `australia_estimation.R` (3,411 lines) — the analytical core

**Role.** Part 2 of the pipeline. Reads `model_data` from its environment
(produced upstream), fits the eight specifications across multiple sample
windows, runs the full robustness suite, and writes ~30 output files.

**Run directly?** No. It is `source()`-d by all three execution modes.
Sourcing it triggers the full 16-step main block.

**Internal section structure.**

| Section | Contents | Approx. lines |
|---|---|---|
| Top of file | Library loads, output_dir resolution, `PI_METHOD` flag | 1–80 |
| A | `add_model_variables()` — short-run & dummy construction | 80–100 |
| A | `compute_income_volatility()` — AR(8) residual proxy | 100–105 |
| A | `construct_permanent_income()` — rolling AR(8) PI | 105–250 |
| A | `construct_permanent_income_italy()` — Jordà LP PI | 255–365 |
| A | `compare_pi_methods()` — side-by-side AR vs LP refit | 370–445 |
| A2 | `run_pi_sensitivity()` — 18-cell PI grid | 450–550 |
| C | `model_diagnostics()` — DW, BP, AR(1)/(4), Chow, RESET, BIC | 555–660 |
| D | `add_model_variables()` — short-run terms (was duplicated from A; clean up TODO) | 660–765 |
| E | `fit_ecm_spec()` — single-spec OLS+NW HAC fit | 770–870 |
| F | `run_all_specifications()` — 8 specs (incl. Spec 8 Williams CCI) | 875–1140 |
| F1 | `run_specifications_covid_robust()` — 4-sample variant | 1145–1190 |
| F2 | `fit_consumption_with_williams_cci()` — spline knot survival | 1195–1330 |
| F3 | `build_lambda_robustness_table()` | 1335–1420 |
| F4 | `fit_rolling_window()` — 60-quarter rolling estimates | 1420–1500 |
| F0 | `run_cointegration_battery()` — ADF + PO + Johansen | 1505–1600 |
| G | `build_results_table()` — coefficient + diagnostics CSVs | 1605–1815 |
| G2 | `select_preferred_spec()` — 4-screen rubric + BIC tiebreak | 1820–1980 |
| G3 | `test_nla_restriction()` — Wald test of γ_LA + γ_LOANS = 0 | 1985–2050 |
| H2 | `build_comparison_table()` — Italy/Australia λ + structural γ | 2055–2170 |
| H3 | `write_model_summary()` — narrative .md output | 2175–2330 |
| I | `pick_preferred_spec_object()` — helper for downstream | 2335–2360 |
| J | `run_italy_style_robustness()` — IV + SUR + Chow + scaled + NPY + Drehmann | 2365–2620 |
| K | `plot_longrun_decomposition()` — headline policy chart | 2625–2755 |
| H | `plot_actual_vs_fitted()` — basic diagnostic plot | 2760–2830 |
| MAIN | 16-step main execution block | 2835–end |

**The main block — pipeline steps.**

After the function definitions, the script unconditionally runs a
16-step pipeline (lines 2835–end). Each step prints `[Step N] ...`
to the console.

| Step | Purpose | Output(s) |
|---|---|---|
| 1 | Build short-run + dummy variables | (in-memory) |
| 2 | AR(8) income volatility | `abs_income_resid` column |
| 3 | Construct permanent income (AR or Italy LP) | `ln_yp_over_y` column |
| 4 | Estimate 8 specs across 4 sample variants (full, pre-COVID, COVID-dropped, COVID rich-dummies); incorporates the iterative Williams-CCI fit if `USE_INSTITUTIONAL_CCI=TRUE` | spec list objects |
| 5 | Build coefficient + diagnostics tables | `australia_*_results.csv`, `australia_*_diagnostics.csv` |
| 5b | Wald test of NLA cross-equation restriction | `australia_nla_restriction_test.csv` |
| 6 | Cointegration battery (ADF + PO + Johansen) | `australia_cointegration.csv` |
| 7 | Consolidate λ across 4 sample variants | `australia_lambda_robustness.csv` |
| 8 | Select preferred spec (4-screen rubric + BIC) | `australia_spec_selection.csv` |
| 9 | Break battery on preferred spec | `australia_breaks.csv` |
| 10 | Conditional WLS robustness | `australia_wls_robustness.csv` |
| 11 | PI filter sensitivity (18 variants) | `australia_permanent_income_sensitivity.csv` |
| 11b | AR vs Italy LP method comparison | `australia_pi_method_comparison.csv` |
| 12 | Rolling-window estimation (60 quarters) | `australia_rolling_coefs.csv` + `.png` |
| 13 | Italy/Australia structural-parameter comparison | `italy_australia_comparison.csv`, `italy_australia_lambda.csv` |
| 14 | Narrative model summary | `australia_model_summary.md` |
| 15 | Preferred spec + Spec 1 plots | `australia_*_actual_vs_fitted.png` + `_residuals.png` |
| 16 | Williams comparison (sources `williams_comparison.R`) | `australia_williams_comparison.csv` + `.md`, `australia_williams_spec8_comparison.csv` |

**Key configuration variables** (top of file):

```r
PI_METHOD <- "italy"  # canonical: Jordà LP. Set to "ar" for rolling AR(8) robustness.
```

The canonical method is Italy LP (resolved 2026-05-07; NS-100). Setting
`PI_METHOD <- "ar"` reverts Step 3 to the rolling AR(8) forecaster used
historically. The AR vs Italy comparison output (Step 11b) is produced
regardless of which method is selected as canonical.

**Modify when:**
- Adding a new specification → edit `run_all_specifications()` (around
  line 875).
- Adding a new robustness check → add a new block in
  `run_italy_style_robustness()` (around line 2365), following the
  pattern of the existing six (IV, SUR, Chow, scaled, NPY, Drehmann).
- Changing the preferred-spec selector → edit
  `select_preferred_spec()` (around line 1820).
- Adding a new pipeline step → insert in the main block; preserve the
  `[Step N]` console output convention.

---

### 2.4 `model_helpers.R` (953 lines) — shared utilities

**Role.** A single library of utility functions sourced by Part 1 and
Part 2. Keeps non-pipeline-specific code out of the pipeline scripts.

**Categories of contents:**

- **Unit conversion** (lines 142–230):
  `rescale_to_millions(value, units)` — converts ABS balance-sheet
  series from `$ Billions` to `$ Millions`.
  `parse_quarter_label_date(s)` — parses `"Mar 2020"`, `"Jun 1998"`
  ABS quarter-end conventions to first-of-quarter `Date`.
  `splice_house_price_series(base, overlay)` — chain-link splicing
  used by the 3-layer house-price construction.
- **Stationarity tests** (lines 233–295):
  `run_adf_drift(x, lags = 4)` — ADF test with drift, returning
  statistic and 5% critical value. Wrapped in `tryCatch` against
  the `urca` dependency.
- **Long-run estimation** (lines 300–390):
  `fit_long_run_spec(...)` and `fit_dols_spec(...)` — DOLS
  (dynamic OLS) cointegrating-regression estimator. Currently
  dead code; available if a long-run-only refit is ever wanted.
- **Permanent income alternatives** (lines 395–480):
  `compute_expected_log_income_path()`, `adaptive_permanent_income_log()`,
  `compute_log_yp_over_y()`. The last function had a documented
  matrix-vs-vector bug that was fixed; it now correctly applies
  discount weights when given a horizon-step matrix of forecasts.
- **Williams CCI infrastructure** (lines 633–702):
  `build_credit_regime_basis(dates)` — sets up the regime indicator
  basis used by the legacy `construct_institutional_cci`.
  `construct_institutional_cci(input, basis)` — Muellbauer-style
  regime + indicator blend; the `Option-A` legacy CCI from earlier
  iterations.
  `smoothed_step(date_vec, knot_date)` — produces the SDMMA
  (5-MA of 4-MA of step) smoothed-step transition.
  `build_williams_cci_basis(dates, knots, sign_priors)` — assembles
  the four-knot Williams basis at 1979Q1, 1992Q1, 1998Q1, 2007Q1.
- **State-space CCI factor** (lines 410–620): `build_credit_ssm_factor`
  — Kalman-filter latent CCI extractor. Currently unused; available
  for the multi-equation extension (NS-031).

**Modify when:** writing a new utility function that's used in more
than one place. New functions should follow the section-comment-block
convention with a one-paragraph docstring at the top.

---

### 2.5 `run_estimation_from_rds.R` (75 lines) — fast replay

**Role.** Mode 2 entry point. Loads `outputs/australia_model_dataset.rds`
(produced by a previous Mode 1 run) and re-runs the estimation block
without rebuilding the master dataset. Takes ~30 seconds end-to-end.

**Invocation.** `Rscript Ausreplication/R/run_estimation_from_rds.R`

**What it does.**
- Lines 1–25: same package loading and path resolution as the
  orchestrator.
- Lines 28–35: load RDS; abort if not found.
- Lines 37–47: build `model_data` from `master` exactly as the
  orchestrator does (same renames, same `ecm_lag`).
- Lines 50–end: source `australia_estimation.R`.

**Use this when:** you've made a code change to estimation and want to
test the effect without re-running the data download (which takes
several minutes due to ABS workbook parsing). This is the default
inner-loop developer workflow.

**Don't use when:** you've changed `australia_data_download.R` or any
input file under `data_raw/` — those changes won't be reflected because
the cached RDS won't have been rebuilt.

---

### 2.6 `export_master_csv.R` (151 lines) — RDS → portable CSV

**Role.** One-shot script. Reads `outputs/australia_model_dataset.rds`,
backfills any variables that the estimation script normally
reconstructs at runtime (so the CSV is self-contained), reorders
columns by topic for readability, and writes
`data_raw/master_data.csv` with **17 significant digits** (full IEEE 754
double precision; default `readr::write_csv` truncates to ~15 digits
which leaves room for round-trip drift).

**Invocation.** `Rscript Ausreplication/R/export_master_csv.R`

**Use this when:**
- After a Mode 1 cold-rebuild, to refresh the portable CSV.
- When sharing the dataset with someone who can't run the data
  download (no internet, no ABS access).
- When you want to inspect or hand-edit a specific cell of the master
  data.

**Output:** `data_raw/master_data.csv` (180 rows × 85 columns,
~240 KB). The CSV is committed to git and serves as the canonical
"frozen vintage" of the master data.

---

### 2.7 `load_master_from_csv.R` (101 lines) — CSV-based estimation

**Role.** Mode 3 entry point. Reads `data_raw/master_data.csv` and
runs the full estimation pipeline without touching ABS workbooks or
the RDS. Useful when offline, when working with a manually edited
dataset, or for portability.

**Invocation.** `Rscript Ausreplication/R/load_master_from_csv.R`

**Subtle gotcha.** CSV round-trip is at machine precision (~1e-10 max
abs diff per column). Most outputs match Mode 2 exactly; **edge-case
Chow-stability flags can flip** because `strucchange::sctest` is
bit-sensitive when the test statistic is near a critical value. The
script prints a warning at startup. For bit-identical reproduction,
prefer Mode 2 (RDS).

**Use this when:**
- You don't have network access and need to re-estimate.
- You want to patch a known data error in the master dataset by
  hand-editing the CSV.
- You're sharing the project with someone who doesn't have the RDS.

---

### 2.8 `williams_comparison.R` (396 lines) — comparison vs Williams (2010, 2012)

**Role.** Post-estimation analysis. Builds the side-by-side comparison
table against Williams' published Table 1 and writes a near-publishable
markdown commentary that is the basis for WP §9.

**Run directly?** Yes, you can — but it's normally sourced as Step 16
of the main `australia_estimation.R` block.

**Invocation.** `Rscript Ausreplication/R/williams_comparison.R`

**Inputs.**
- `outputs/australia_full_results.csv` — Spec 6 and Spec 8 OLS results
- Hard-coded Williams (2010) Table 1 values inside the script (with
  the BIS chapter lambda = 0.286 and the wealth γ values 0.0488,
  0.022, 0.159).

**Outputs.**
- `outputs/australia_williams_comparison.csv` — wide table, 13 terms,
  with Williams' γ, Williams' implied OLS (γ × |λ_W|), our OLS,
  our implied γ (= our OLS / |our λ|), and percentage gaps in both
  forms.
- `outputs/australia_williams_spec8_comparison.csv` — Spec 8
  CCI-interaction comparison.
- `outputs/australia_williams_comparison.md` — markdown commentary
  ~80 lines, structured for direct inclusion in the WP.

**Methodology framing.** Under canonical `PI_METHOD = "italy"`, our λ
is within 25% of Williams' published value (−0.218 vs −0.286). The
implied long-run γ on individual wealth terms undershoots Williams by
roughly a factor of four — attributed to truncated CCI variation on
the post-deregulation 1988Q4+ sample. Under the `PI_METHOD = "ar"`
robustness column we instead see |λ| ~0.05 with a wrong-signed
permanent-income coefficient (the historical "Australian PI puzzle"),
which we treat as a methodology artefact.

**Modify when:**
- Williams' published values need updating (e.g. if a new edition
  of the BIS chapter appears with different numbers).
- You want to add additional terms to the comparison.
- The comparison framing for the WP needs adjustment.

---

## 3. Configuration flags

There are **two** runtime flags that materially change pipeline
behaviour. Both are at the top of their respective files.

### `PI_METHOD` (in `australia_estimation.R`, ~line 45)

```r
PI_METHOD <- "italy"  # canonical (resolved 2026-05-07): Jordà (2005) local projection
PI_METHOD <- "ar"     # robustness column: rolling AR(8) + trend + ogive
```

Italy LP (a) uses the labour-force-share predictor, (b) gives a
positive long-run permanent-income coefficient (resolving the
Australian PI puzzle reported under AR), (c) gives `|λ| ≈ 0.22`,
within 25 per cent of Williams' published −0.286. The comparison
output (Step 11b) is produced regardless of which is canonical.

### `USE_INSTITUTIONAL_CCI` (in `australia_data_download.R`, ~line 70)

```r
USE_INSTITUTIONAL_CCI <- FALSE  # default: housing-loan-flow proxy only
USE_INSTITUTIONAL_CCI <- TRUE   # adds Williams 4-knot SDMMA basis
```

When `TRUE`:
- Attaches four `sdmma_*` columns (smoothed steps at 1979/1992/1998/2007)
  to `master`.
- Adds an institutional CCI overlay (regime + indicator blend) that
  back-fills `cci_ratio` pre-2002.
- Enables Spec 8 in `australia_estimation.R` (the CCI-interactions
  spec). Without `cci_williams` available, Spec 8 silently returns
  `NULL` and is filtered out of downstream steps.

Note: even with `TRUE`, only 2 of 4 Williams knots survive sign-prior
reduction on the 1988+ sample (the 1979 and 1992 knots fail; the 1998
and 2007 knots survive). Full identification requires sample
back-extension to ~1975Q1, which is the standing research priority.

---

## 4. Common workflows

### "I want to re-estimate without changing anything."
```
Rscript Ausreplication/R/run_estimation_from_rds.R
```
~30 seconds. Reuses the cached RDS.

### "I want to refresh the master dataset from raw inputs."
```
Rscript Ausreplication/R/australia_consumption_model.R
```
A few minutes; re-parses ABS workbooks (cached) and the user CSVs.
Refreshes `outputs/australia_model_dataset.rds`.

### "I want to run offline / from the portable CSV."
```
Rscript Ausreplication/R/load_master_from_csv.R
```
Reads `data_raw/master_data.csv`. No internet, no RDS needed.

### "I want to refresh the portable CSV after a cold rebuild."
```
Rscript Ausreplication/R/australia_consumption_model.R
Rscript Ausreplication/R/export_master_csv.R
```

### "I want to switch to the AR robustness column."
Open `Ausreplication/R/australia_estimation.R`, change
`PI_METHOD <- "italy"` to `PI_METHOD <- "ar"` (around line 45), then
`Rscript Ausreplication/R/run_estimation_from_rds.R`. Compare
`australia_pi_method_comparison.csv` for the side-by-side.

### "I want to add a new specification."
1. In `australia_estimation.R`, find `run_all_specifications()` (around
   line 875).
2. Add a new `spec9 <- fit_ecm_spec(...)` block after Spec 8.
3. Add `spec9 = spec9` to the returned list.
4. Run `Rscript Ausreplication/R/run_estimation_from_rds.R`.
5. The new spec will automatically appear in `australia_full_results.csv`,
   `australia_full_diagnostics.csv`, and the spec selector.

### "I want to add a new robustness check."
1. In `australia_estimation.R`, find `run_italy_style_robustness()`
   (around line 2365).
2. Copy one of the existing six blocks (IV, SUR, Chow, scaled, NPY,
   Drehmann) as a template.
3. Wrap your check in `tryCatch` with a `path` variable for the
   output CSV.
4. Re-run the pipeline.

### "I want to add a new variable to `master`."
1. In `australia_data_download.R`, locate the relevant section
   (probably §2 for new ABS series, §4 for derived series).
2. Build the variable as a named tibble with a `date` column.
3. Add it to the `master <- master %>% left_join(...)` chain in §3.
4. Add it to the coverage report `for (v in c(...))` loop in §9.
5. If it should be range-checked, add a `stopifnot()` assertion in §9.
6. Run a cold rebuild: `Rscript Ausreplication/R/australia_consumption_model.R`.
7. Refresh the portable CSV: `Rscript Ausreplication/R/export_master_csv.R`.

### "I want to change the preferred-spec selection criteria."
Edit `select_preferred_spec()` in `australia_estimation.R`
(around line 1820). The four screens (sign, cointegration, λ,
stability) are evaluated in turn; modify the threshold or rule for
any one. The function returns a tibble with `is_preferred = TRUE`
flagged on the chosen row.

---

## 5. Test suite

### Location
`tests/testthat/` contains five files:

- `test-units.R` — unit conversion (`rescale_to_millions`, etc.)
- `test-dates.R` — date parsers (ABS quarter-label conventions)
- `test-stats.R` — ADF and DOLS helpers
- `test-cci.R` — institutional CCI smoke tests
- `test-permanent_income.R` — `compute_log_yp_over_y` and `adaptive_permanent_income_log`

The suite has 22 `test_that` blocks; all should pass with no skips.

### Run
```
Rscript tests/testthat.R
```

### Add new tests when:
- Writing new utilities in `model_helpers.R`.
- Fixing a bug — write a regression test that would have caught the
  bug.
- Adding a non-trivial new function in `australia_estimation.R`.

The CI workflow at `.github/workflows/check.yml` runs tests + the full
estimation pipeline on every push.

---

## 6. Function index — alphabetical

Functions defined in the project, with file and approximate line
number:

| Function | File | Line | Purpose |
|---|---|---:|---|
| `add_model_variables()` | australia_estimation.R | 660 | Builds short-run + dummy variables on `model_data` |
| `adaptive_permanent_income_log()` | model_helpers.R | 480 | EWMA log-income smoothing |
| `build_comparison_table()` | australia_estimation.R | 2055 | Italy/Australia λ + structural γ comparison |
| `build_credit_regime_basis()` | model_helpers.R | 633 | Regime-indicator basis for legacy CCI |
| `build_credit_ssm_factor()` | model_helpers.R | 410 | Kalman-filter latent CCI (currently unused) |
| `build_lambda_robustness_table()` | australia_estimation.R | 1335 | Consolidate λ across 4 sample variants |
| `build_results_table()` | australia_estimation.R | 1605 | Coefficient + diagnostics CSVs |
| `build_williams_cci_basis()` | model_helpers.R | 698 | 4-knot SDMMA spline basis at 1979/1992/1998/2007 |
| `compare_pi_methods()` | australia_estimation.R | 370 | Side-by-side AR vs Italy LP refit |
| `compute_expected_log_income_path()` | model_helpers.R | 395 | Multi-step expected log income |
| `compute_income_volatility()` | australia_estimation.R | 100 | AR(8) residual proxy |
| `compute_log_yp_over_y()` | model_helpers.R | 425 | log(y^p/y) with discount weights |
| `construct_institutional_cci()` | model_helpers.R | 657 | Muellbauer regime+indicator CCI |
| `construct_permanent_income()` | australia_estimation.R | 105 | Rolling AR(8) PI forecaster |
| `construct_permanent_income_italy()` | australia_estimation.R | 255 | Jordà (2005) LP PI forecaster |
| `fit_consumption_with_williams_cci()` | australia_estimation.R | 1195 | Williams 4-knot spline survival fit |
| `fit_dols_spec()` | model_helpers.R | 320 | Dynamic OLS cointegrating regression |
| `fit_ecm_spec()` | australia_estimation.R | 770 | Single-spec OLS+NW HAC fit |
| `fit_long_run_spec()` | model_helpers.R | 300 | Static cointegrating regression |
| `fit_rolling_window()` | australia_estimation.R | 1420 | 60-quarter rolling estimates |
| `model_diagnostics()` | australia_estimation.R | 555 | DW + BP + AR1/AR4 + Chow + RESET + BIC |
| `monthly_to_quarterly()` | australia_data_download.R | 95 | Quarterly mean of monthly obs |
| `pick_abs()` | australia_data_download.R | 130 | Series picker by name regex on ABS workbooks |
| `pick_preferred_spec_object()` | australia_estimation.R | 2335 | Helper for downstream pipeline steps |
| `plot_actual_vs_fitted()` | australia_estimation.R | 2760 | Spec fit plus residual chart |
| `plot_longrun_decomposition()` | australia_estimation.R | 2625 | Headline policy chart of long-run contributions |
| `read_abs_ts_workbook()` | model_helpers.R | (helper) | Parse ABS time-series workbook (works around `readabs` Windows path bug) |
| `rescale_to_millions()` | model_helpers.R | 142 | Convert balance-sheet `$ Billions` to `$ Millions` |
| `run_adf_drift()` | model_helpers.R | 233 | ADF test with drift |
| `run_all_specifications()` | australia_estimation.R | 875 | Estimate the 8 specs |
| `run_cointegration_battery()` | australia_estimation.R | 1505 | ADF + Phillips-Ouliaris + Johansen per spec |
| `run_italy_style_robustness()` | australia_estimation.R | 2365 | Six robustness blocks (IV, SUR, Chow, scaled, NPY, Drehmann) |
| `run_pi_sensitivity()` | australia_estimation.R | 450 | 18-variant PI grid |
| `run_specifications_covid_robust()` | australia_estimation.R | 1145 | 4 sample variants for λ stability |
| `select_preferred_spec()` | australia_estimation.R | 1820 | 4-screen rubric + BIC tiebreak |
| `smoothed_step()` | model_helpers.R | (helper) | SDMMA = 5-MA of 4-MA of step dummy |
| `splice_house_price_series()` | model_helpers.R | 208 | Chain-link splicing |
| `test_nla_restriction()` | australia_estimation.R | 1985 | Wald test of γ_LA + γ_LOANS = 0 |
| `write_model_summary()` | australia_estimation.R | 2175 | Generate `australia_model_summary.md` |

Line numbers are approximate; consult the file if a number looks off.

---

## 7. Output reference — what each file contains

For each output produced by the pipeline, this section says where it
comes from and what it represents. Cross-link from this when you find
yourself wondering "what is `australia_X.csv`?".

### Coefficient tables

- `australia_full_results.csv` — Spec 1–8 coefficients on full sample
  (1988Q4–2024Q4, n≈86 for disaggregated specs). Columns include
  `ols_estimate`, `nw_se`, `t_stat`, `p_value`, `lambda`,
  `structural_param` (= ols/lambda), `expected_sign`, `sign_ok`.
- `australia_precovid_results.csv` — same on pre-COVID sub-sample.
- `australia_all_results.csv` — combined long-format.

### Diagnostics

- `australia_full_diagnostics.csv` — n_obs, se_pct, adj_r2, dw,
  lm_het_pval, lm_het_pval_no_events, het_diagnosis, ar1_pval, ar4_pval,
  chow_pval, reset_pval, schwarz, loglik per spec.
- `australia_precovid_diagnostics.csv` — same on pre-COVID.
- `australia_all_diagnostics.csv` — combined.

### Spec-selection and stability

- `australia_spec_selection.csv` — pass/fail per the 4 screens, BIC,
  `is_preferred` flag.
- `australia_lambda_robustness.csv` — λ for each (spec × sample
  variant); 8 specs × 4 variants = 32 rows.
- `australia_breaks.csv` — supF, breakpoints, CUSUM for the preferred
  spec.
- `australia_recursive_coefficients_*.png` — recursive coefficient
  plots.

### Substantive tests

- `australia_nla_restriction_test.csv` — Wald test of γ_LA + γ_LOANS=0
  for Specs 4/5/6 × 2 samples.
- `australia_williams_cci_knots.csv` — Spec 8 CCI-knot survival
  (1979 aliased, 1992 sign-violator, 1998 +ve, 2007 −ve on current sample).
- `australia_spec8_sign_prior_verdicts.csv` — Spec 8 interaction-term
  sign verdicts vs Williams.
- `australia_cointegration.csv` — ADF + PO + Johansen per spec.
- `australia_chow_battery.csv` — Chow tests at 1995Q1, 2000Q1, 2008Q3,
  2020Q1 on the preferred spec.

### Italy-style robustness suite

- `australia_iv_robustness.csv` — OLS vs IV on current income.
- `australia_joint_pi_robustness.csv` — single-equation vs SUR.
- `australia_drehmann_robustness.csv` — flat real rate vs amortising.
- `australia_scaled_income_robustness.csv` — disposable income vs
  50/50 scaled with labour-transfer.
- `australia_williams_income_robustness.csv` — disposable income vs
  Williams' NPY (2009 §4.2.1).
- `australia_wls_robustness.csv` — OLS-NW vs WLS on the preferred
  spec.
- `australia_permanent_income_sensitivity.csv` — 18-cell PI grid.
- `australia_pi_method_comparison.csv` + `australia_pi_method_meta.csv` —
  AR vs Italy LP method comparison.
- `australia_rolling_coefs.csv` + `.png` — 60-quarter rolling
  coefficients on the preferred spec.

### Williams-comparison outputs

- `australia_williams_comparison.csv` — wide table, 13 terms.
- `australia_williams_comparison.md` — markdown commentary
  (basis for WP §9).
- `australia_williams_spec8_comparison.csv` — CCI-interaction match.

### Italy-Australia (uses published Italy reference)

- `italy_australia_comparison.csv` — common structural parameters.
- `italy_australia_lambda.csv` — λ side-by-side with explicit
  `lambda_source` and `note` columns.
- `italy_table1_results.csv` — published Italy reference (read-only,
  not a pipeline output).

### Charts

- `australia_longrun_decomposition.png` + `.csv` — headline policy
  chart of long-run contributions to log(c/y).
- `australia_spec1_lognetworth_*.png` — baseline aggregate-net-worth
  fit/residuals.
- `australia_spec6_preferred_*.png` — preferred-spec fit/residuals.
- `australia_recursive_coefficients_*.png` — recursive coefficients.
- `australia_rolling_coefs.png` — rolling coefficient paths.

### Underlying data

- `australia_model_dataset.csv` — coverage table (variable, n_obs,
  date_from, date_to).
- `australia_model_dataset.rds` — the `master` tibble (for Mode 2).

### Narrative

- `australia_model_summary.md` — auto-generated markdown summary
  with traffic-light diagnostics, preferred-spec rationale, headline
  results.

---

## 8. Where to look for what

| Question | Answer |
|---|---|
| "Where is the canonical entry point?" | `australia_consumption_model.R` (Mode 1) |
| "How is permanent income computed?" | `construct_permanent_income()` (AR) and `construct_permanent_income_italy()` (LP) in `australia_estimation.R` |
| "How is the Williams 4-knot spline implemented?" | `build_williams_cci_basis()` in `model_helpers.R` and `fit_consumption_with_williams_cci()` in `australia_estimation.R` |
| "Where is the spec selector?" | `select_preferred_spec()` in `australia_estimation.R` (line ~1820) |
| "Where is the Italy comparison logic?" | `build_comparison_table()` in `australia_estimation.R` (line ~2055) |
| "Where is the Williams comparison logic?" | `williams_comparison.R` (whole file) |
| "Where is the master dataset built?" | `australia_data_download.R` §3 (the `master <- ... %>% left_join(...)` chain) |
| "How do I add a new ABS series?" | See the §4 'Common workflows' walkthrough |
| "How do I run only the cointegration tests?" | Source `australia_estimation.R` interactively, then call `run_cointegration_battery(model_data, specs_full, output_dir)` directly |
| "Where are the published Williams numbers stored?" | Hard-coded inside `williams_comparison.R` (the `williams <- tribble(...)` block near the top) |

---

**Generated alongside the May 2026 repo cleanup. Cross-linked with
`data.md`, `project_status.md`, `wp_draft.md`, and `next_steps.md`.**
