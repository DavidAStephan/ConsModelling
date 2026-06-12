# Code guide — Australian household consumption model

A walkthrough of every R file in the project. Intended for someone who
has just cloned the repository and wants to understand what's where,
how data flows through the pipeline, and where to make a particular
kind of change.

If you are looking for a summary of:

- the **modelling content** (specifications, results, decisions) → see [`wp_draft.md`](wp_draft.md) §3 (data), §4 (model), §6 (specs), §7 (results)
- the **data sources** (ABS catalogues, RBA tables, the master dataset) → see [`data.md`](data.md)
- the **paper draft** → see [`wp_draft.md`](wp_draft.md)
- the **multi-equation companion paper** → see [`../../LIVES/docs/companion_paper_draft.md`](../../LIVES/docs/companion_paper_draft.md)
- the **forward-looking plan** (what's next) → see [`next_steps_plan_2026.md`](next_steps_plan_2026.md)
- the **multi-equation plan and progress log** → see [`../../LIVES/docs/multi_equation_plan.md`](../../LIVES/docs/multi_equation_plan.md) and [`../../LIVES/docs/phase_a_progress.md`](../../LIVES/docs/phase_a_progress.md)
- the **paper rendering instructions** → see [`RENDER.md`](RENDER.md)

This guide is the **code-level companion** to those documents.

---

## 1. Architecture in one picture

The pipeline is a two-stage process (data construction → estimation)
plus a **multi-equation LIVES extension** that consumes the same master
dataset to fit a 2-, 3-, and 4-equation SUR system and a Wald test of
Williams' Table 1 calibrations.

```
                            ┌────────────────────────────────────┐
                            │ ABS workbooks  (data_raw/*.xlsx)   │
                            │ RBA mortgage rate (rba_filrhlbvs.csv)
                            │ ABS 15+ population (population_workingage.csv)
                            │ Income components (household_income.csv)
                            │ RBA E13 burden (e13-data.csv)
                            │ RBA D-tables (D01/D02/D03 hist .xlsx)
                            │ Treasury TRYM HPI (house_price_history_long.csv)
                            │ Historical labour force (labour_force_historic.csv)
                            └────────────────────────────────────┘
                                            │
                                            ▼
   Mode (1): COLD REBUILD                 ┌─────────────────────────┐
   australia_consumption_model.R   ─────►│ australia_data_download │
                                          │ produces: master tibble │
                                          │ → outputs/*_dataset.rds │
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
                                          │ pipeline steps:         │
                                          │  1-3   build model_data │
                                          │  4a    iterated Williams│
                                          │        CCI knot survival│
                                          │  4     estimate 14 specs│
                                          │        × 4 sample variants│
                                          │  5-7   results+coint+λ  │
                                          │  7b    Spec 11 variants │
                                          │  8     spec selection   │
                                          │  9/9b  Italy-style robust│
                                          │        (preferred+Spec11)│
                                          │ 10     long-run decomp  │
                                          │ 11     §10.2 counterfacts│
                                          │ 16-22  source_isolated  │
                                          │        sub-scripts      │
                                          └────────────┬────────────┘
                                                       │
                                                       ▼
                            ┌────────────────────────────────────┐
                            │  outputs/                          │
                            │  ~70 CSVs + ~29 PNGs + summary.md  │
                            └────────────────────────────────────┘
                                            │
                                            ▼   (uses cached RDS)
                            ┌────────────────────────────────────┐
                            │ LIVES/ multi-equation extension    │
                            │                                    │
                            │ lives_data_prep.R                  │
                            │  → derives HP, M, HEW regressors   │
                            │  → builds hew_proxy from           │
                            │    Δ(fin_loans_proxy)              │
                            │  → lives_model_data.rds            │
                            │                                    │
                            │ joint_cci_identification.R         │
                            │  → 4-eq joint sign survival        │
                            │  → cci_williams_joint{,_h,_m}      │
                            │                                    │
                            │ lives_sur_2eq.R   (C + HP)         │
                            │ lives_sur_3eq.R   (C + HP + M)     │
                            │ lives_sur_4eq.R   (C + HP + M + W) │
                            │ williams_calibration_test.R         │
                            │  → χ² Wald test of Williams Table 1│
                            │                                    │
                            │ outputs: ~14 CSVs + model-data RDS │
                            └────────────────────────────────────┘
```

Auxiliaries:

- `model_helpers.R` — shared utility functions, sourced by both Part 1
  and Part 2. Includes the Williams CCI infrastructure
  (`build_williams_cci_basis`, `build_williams_cci_basis_canonical`,
  `build_williams_cci_basis_sectional`, the SDMMA smoothed-step
  primitives), the Kalman CCI factor extractor, the DOLS
  long-run estimator, and the unit/date utilities.
- `export_master_csv.R` — one-shot script that converts the cached RDS
  to a portable CSV (Mode 3 input).
- `williams_comparison.R` — post-estimation analysis. Builds the
  side-by-side comparison against Williams (2010, 2012) for WP §9.
  Sourced by `australia_estimation.R` at the end of the main block.

Total R code in the project: **30 scripts, ~13,900 lines** across
`Australia/R/` (21 files) and `LIVES/R/` (9 files).

**Isolation architecture (June 2026 fix).** The post-estimation
sub-scripts (williams_comparison, knot_experiment, the CCI comparisons
and placebos, cci_fit_decomposition, oos_forecast) are sourced via
`source_isolated()` (defined near the end of `australia_estimation.R`),
which evaluates each script in a fresh child environment
(`new.env(parent = globalenv())`). The sub-scripts can *read* pipeline
state (`model_data`, `specs_full`, …) but their own assignments stay
local. A previous version sourced them at top level, so
`knot_experiment.R` / `cci_placebo_test.R` silently REASSIGNED the
global `model_data` (rebuilt with the AR permanent-income constructor),
contaminating every later step — the old committed IV/SUR tables had
been estimated on AR-PI data. Do not revert to top-level sourcing.

---

## 2. `Australia/R/` — the headline pipeline

The 21 scripts that build the master dataset, fit the fourteen
single-equation specifications, run the robustness suite, and write
all `Australia/outputs/` CSVs and PNGs.

### 2.1 Pipeline entry points and core engine

#### 2.1.1 `australia_consumption_model.R` (97 lines) — master orchestrator

**Role.** Top-level entry point for **Mode 1** (cold rebuild). Sources
the data download, sources the estimation script, and prints status
banners between the two stages.

**Invocation.** `Rscript Australia/R/australia_consumption_model.R`

**What it does.** Loads packages, resolves the script's own
directory, sources `australia_data_download.R` (which makes the
`master` tibble available), renames three columns to the estimation
script's expected names (`d_ln_cons_pc → dlcons`,
`ln_ydi_real_pc → lincome`, `ln_cons_real_pc → lcons`), adds the
canonical ECM term `ecm_lag = lag(lcons, 1) − lincome`, then sources
`australia_estimation.R`.

**Modify when:** changing the ECM convention, adding a global column
rename, or chaining additional pre-estimation transforms.

**Don't modify when:** doing routine analysis; this script is glue.

#### 2.1.2 `australia_data_download.R` (1,815 lines) — data construction

**Role.** Part 1 of the pipeline. Builds the `master` tibble from raw
inputs (ABS workbooks, user-supplied CSVs, RBA D-tables, TRYM HPI,
historical labour-force). Saves to `outputs/australia_model_dataset.rds`
for later reuse by Modes 2 and 3.

**Run directly?** Not normally. Sourced by `australia_consumption_model.R`
or an interactive session.

**Internal sections** (roughly):

| Section | Contents |
|---|---|
| 1 | Date spine 1976Q3–2024Q4 (n = 194) — back-extended in May 2026 |
| 2 | Raw input loading (10 ABS workbooks + 7 user CSVs + 3 RBA D-tables) |
| 3 | Master assembly via left-joins |
| 4 | Real per-capita variables, deflation, scaled-income, NPY |
| 5 | Annualised income and disaggregated wealth ratios |
| 6 | 4-layer house-price splice (TRYM 1959Q3 → legacy → bridge → current) |
| 7 | Credit conditions (housing-loan-flow CCI + Williams basis if enabled) |
| 8 | M3 household proxy + back-extension wealth proxies (`*_proxy`) |
| 9 | Coverage report and assertions |
| 10 | Save outputs (RDS + coverage CSV) |

**Key configuration variables** (decoupled June 2026, NS-131 — the old
single `USE_INSTITUTIONAL_CCI` flag is gone):
```r
USE_WILLIAMS_SDMMA_BASIS    <- TRUE   # attach the 15-knot Williams SDMMA basis (canonical)
USE_INSTITUTIONAL_CCI_OVERLAY <- FALSE # pre-2002 cci_ratio backfill (canonical OFF)
```

**Key inputs (`data_raw/`):**

| File | Purpose |
|---|---|
| `5206008_*.xlsx` | ABS HFCE (chain volume + nominal) |
| `5206020_*.xlsx`, `5204055011do001-005.xlsx` | ABS Household Income Account |
| `5232035.xlsx` | ABS Household Balance Sheet — binding sample start 1988Q3 |
| `560101.xlsx` | ABS Lending Indicators (housing flow, FHB share) |
| `6202001.xlsx` | ABS Labour Force (unemployment, 15+ population) |
| `641601.xlsx`, `643201.xlsx` | ABS house price indexes |
| `3101059.xlsx`, `310101.xlsx`, `310104.xlsx` | ABS Estimated Resident Population |
| `houseprice_old.csv` | Pre-2003 house-price back-fill |
| `house_price_history_long.csv` | Treasury TRYM historical HPI (1959Q3+) |
| `labour_force_historic.csv` | Pre-1978 ABS Cat 6204.0 labour-force compilation |
| `d01hist.xlsx`, `d02hist.xlsx`, `d03hist.xlsx` | RBA D-tables (1959Q3+ credit + M3) |
| `rba_filrhlbvs.csv` | RBA F6 mortgage rate (1959+) |
| `population_workingage.csv` | ABS A84423091W 15+ population |
| `household_income.csv` | 10 income components for Williams NPY |
| `e13-data.csv` | RBA E13 housing-loan-payment burden |

For full provenance see [`data.md`](data.md).

**Modify when:** sourcing a new input series, an ABS workbook vintage
changes and the regex in `pick_abs()` no longer matches, or you want
to change a splice point.

#### 2.1.3 `australia_estimation.R` (4,470 lines) — the analytical core

**Role.** Part 2 of the pipeline. Reads `model_data` from its
environment (produced upstream), fits the **fourteen specifications**
(Spec 1–12 plus Spec 6b and Spec 7b) across the four sample variants
(full, pre-COVID, COVID-dropped, COVID-rich — `covid_rich` now covers
all fourteen), runs the full robustness suite (on the
selector-preferred spec AND on the Spec 11 headline), runs the §10.2
policy counterfactuals, and writes the headline outputs.

**Run directly?** No. Sourced by all three execution modes.

**Internal section structure.**

| Section | Contents |
|---|---|
| Top of file | Library loads, output_dir resolution, `PI_METHOD` flag |
| A | `add_model_variables()`, `compute_income_volatility()`, both PI forecasters (AR with a `gfc_ogive` toggle; De Bonis direct forecast), `run_pi_sensitivity()` |
| C | `model_diagnostics()` — DW, BP, AR(1)/(4), Chow, RESET, BIC. The Chow test uses `strucchange::sctest`, with a manual common-coefficient F-test fallback when sctest is incomputable; the method used is recorded in the `chow_method` column of the diagnostics CSVs (`sctest` / `manual_common_coef`) |
| E | `fit_ecm_spec()` — single-spec OLS+NW HAC fit |
| F | `run_all_specifications()` — Specs 1–12 plus 6b and 7b (Spec 11 = faithful LIVES headline; Spec 12 = calibrated LIVES via `fit_lives_calibrated_spec()`) |
| F1 | `run_specifications_covid_robust()` — 4-sample variant |
| F2 | `fit_consumption_with_williams_cci()` — **iterated** spline knot survival |
| F3 | `build_lambda_robustness_table()` |
| F4 | `fit_rolling_window()` — 60-quarter rolling estimates |
| F0 | `run_cointegration_battery()` — ADF + PO + Johansen |
| G | `build_results_table()` |
| G2 | `select_preferred_spec()` — 4-screen rubric + BIC tiebreak |
| G3 | `test_nla_restriction()` — Wald test of γ_LA + γ_LOANS = 0 |
| H2 | `build_comparison_table()` — Italy / Australia structural γ |
| H3 | `write_model_summary()` — narrative .md output |
| I | `pick_preferred_spec_object()` — helper for downstream |
| J | `run_italy_style_robustness()` — six robustness blocks |
| K | `run_counterfactuals()` — §10.2 NS-012 policy counterfactuals |
| K | `plot_longrun_decomposition()` — headline policy chart |
| H | `plot_actual_vs_fitted()` |
| MAIN | Pipeline step block |

**The main block — pipeline steps.**

| Step | Purpose | Key output(s) |
|---|---|---|
| 1 | Build short-run + dummy variables | (in-memory) |
| 2 | AR(8) income volatility | `abs_income_resid` |
| 3 | Construct permanent income (AR or De Bonis direct forecast) | `ln_yp_over_y` |
| 4a | Iterated Williams CCI fit (multi-pass sign-survival, max 10 iters; 4 knots survive: 2007Q3, 2009Q1, 2019Q1, 2020Q2) | `cci_williams` attached to model_data; `australia_cci_williams_series.csv`, `australia_cci_williams_path.png`, `australia_cci_interaction_corr.csv` |
| 4 | Estimate 14 specs × 4 sample variants | spec objects |
| 5 | Build coefficient + diagnostics tables | `australia_*_results.csv`, `*_diagnostics.csv` (with `chow_method`) |
| 5b | Wald test of NLA cross-equation restriction | `australia_nla_restriction_test.csv` |
| 6 | Cointegration battery | `australia_cointegration.csv` (with a `note` column) |
| 7 | λ across 4 sample variants | `australia_lambda_robustness.csv` |
| 7b | Spec 11 full coefficient vector × 4 variants | `australia_spec11_variants.csv` |
| 8 | Select preferred spec | `australia_spec_selection.csv` |
| 9 | Italy-style robustness on the selector-preferred spec (IV, SUR, Chow, Drehmann, scaled, NPY) | 6+ CSVs |
| 9b | Same suite re-run on the Spec 11 headline | `*_spec11.csv` variants incl. `australia_iv_diagnostics_spec11.csv` |
| 10 | Long-run decomposition plots (preferred + Spec 11) | `australia_longrun_decomposition*.png` + `.csv` |
| 11 | Policy counterfactuals (no-APRA, no-COVID, CCI=peak vs zero) | `australia_counterfactuals*.csv` + `.png` |
| 16–22 | Sub-scripts via `source_isolated()`: williams_comparison, knot_experiment, cci_method_comparison, cci_alternatives, cci_placebo_test, cci_fit_decomposition, oos_forecast | see §2.2, §2.4 |

Other auxiliary steps run inline: PI sensitivity grid, AR vs direct
forecast method comparison, rolling-window estimation, narrative model
summary, preferred-spec + Spec 1 plots.

The IV block instruments the two regressors containing current income —
`ecm_lag` and `ln_yp_over_y` — with six instruments (income lags 1/2/4,
unemployment lags 1/2, mortgage-rate lag 1), reports Newey-West HAC SEs,
and writes first-stage / Wu-Hausman / Sargan diagnostics to
`australia_iv_diagnostics*.csv` (first-stage F ≈ 29–74). A previous
version treated `ecm_lag` as exogenous.

**Key configuration variables** (top of file):

```r
PI_METHOD <- "italy"   # canonical: De Bonis et al. (2020) direct forecast.
                       # "ar" for rolling AR(8) robustness (gfc_ogive toggleable).
```

The canonical method is the Italy-style direct forecast (resolved
2026-05-07): a single full-sample regression of the pre-aggregated
discounted future-income target on time-t predictors, per De Bonis,
Liberati, Muellbauer & Rondinelli (2020, Appendix A.2). An earlier
draft mislabelled this a "Jordà (2005) local projection"; the
implementation is one regression of the pre-aggregated target, not
per-horizon projections. The AR vs direct-forecast comparison output is
produced regardless of which method is canonical.

**Modify when:**
- Adding a new specification → edit `run_all_specifications()`.
- Adding a new robustness check → add a block in
  `run_italy_style_robustness()`, following the existing six.
- Changing the preferred-spec selector → edit `select_preferred_spec()`.
- Adding a new pipeline step → insert in the MAIN block; preserve
  the `[Step N]` console-output convention.

#### 2.1.4 `model_helpers.R` (1,264 lines) — shared utilities

**Role.** Library of utility functions sourced by Part 1 and Part 2.

**Categories of contents:**

- **Unit conversion and parsing** — `rescale_to_millions()`,
  `parse_quarter_label_date()`, `splice_house_price_series()`.
- **Stationarity tests** — `run_adf_drift()` (wrapped in `tryCatch`).
- **Long-run estimation** — `fit_long_run_spec()`, `fit_dols_spec()`
  (DOLS, currently dead code but available).
- **Permanent income alternatives** —
  `compute_expected_log_income_path()`,
  `adaptive_permanent_income_log()`, `compute_log_yp_over_y()`.
- **Williams CCI infrastructure**:
  - `smoothed_step()` — SDMMA (5-MA of 4-MA of step).
  - `build_williams_cci_basis()` — maximal 15-knot candidate basis.
  - `build_williams_cci_basis_canonical()` — canonical 4-knot
    (1979/1992/1998/2007), retained as robustness benchmark.
  - `build_williams_cci_basis_sectional()` — sectional sign-prior
    basis per Williams Aust §5.1 (one prior per period).
- **State-space CCI** — `build_credit_ssm_factor()`, the Kalman-filter
  latent factor used by Spec 9.
- **Legacy regime CCI** — `build_credit_regime_basis()`,
  `construct_institutional_cci()` (still on disk; superseded by the
  Williams SDMMA basis).
- **ABS workbook reading** — `read_abs_ts_workbook()` (works around a
  `readabs` Windows path bug).

**Modify when:** writing a utility used in more than one place.

#### 2.1.5 `run_estimation_from_rds.R` (75 lines) — Mode 2 fast replay

**Role.** Loads the cached `outputs/australia_model_dataset.rds` and
re-runs the estimation block without rebuilding the master. ~30 s.

**Invocation.** `Rscript Australia/R/run_estimation_from_rds.R`

**Use when:** you've changed estimation code and want to test
without re-running data download. The default inner-loop dev workflow.

**Don't use when:** you've changed `australia_data_download.R` or any
`data_raw/` input — the cached RDS won't reflect those changes.

#### 2.1.6 `export_master_csv.R` (151 lines) — RDS → portable CSV

**Role.** One-shot script. Reads
`outputs/australia_model_dataset.rds`, back-fills variables that the
estimation script reconstructs at runtime (so the CSV is
self-contained), reorders columns by topic, and writes
`data_raw/master_data.csv` with **17 significant digits** (full IEEE
754 precision; `readr::write_csv` default of ~15 leaves room for
round-trip drift).

**Use when:** sharing the dataset, hand-editing a cell, or refreshing
the portable CSV after a Mode 1 cold rebuild.

#### 2.1.7 `load_master_from_csv.R` (101 lines) — Mode 3 offline replay

**Role.** Reads `data_raw/master_data.csv` and runs the full
estimation pipeline without touching ABS workbooks or the RDS.

**Subtle gotcha.** CSV round-trip is at machine precision (~1e-10 max
abs diff per column). Most outputs match Mode 2 exactly; **edge-case
Chow-stability flags can flip** because `strucchange::sctest` is
bit-sensitive near a critical value. Prints a warning at startup.
For bit-identical reproduction, prefer Mode 2 (RDS).

#### 2.1.8 `williams_comparison.R` (402 lines) — comparison vs Williams (2010, 2012)

**Role.** Post-estimation analysis. Builds the side-by-side comparison
against Williams' published Table 1 and writes a near-publishable
markdown commentary that is the basis for WP §9.

**Outputs:**
- `australia_williams_comparison.csv` — wide table, 13 terms, with
  Williams' γ, his implied OLS at his |λ|, our OLS, our implied γ
  (= OLS / |λ|), and percentage gaps in both forms.
- `australia_williams_spec8_comparison.csv` — Spec 8 CCI-interaction
  comparison.
- `australia_williams_comparison.md` — ~80-line markdown commentary.

**Methodology framing.** Under canonical `PI_METHOD = "italy"`, the
conventional baseline Spec 6 λ is −0.239 (t = −2.55, n = 86) — about
83 % of Williams' published −0.286. Spec 6's implied structural γ on
the individual wealth terms is small and individually insignificant
(γ_HA = 0.009, γ_NLA = 0.035 vs Williams' 0.0488 / 0.159), but with
wide n = 86 bands Williams' values are not rejected
(`australia_gamma_inference.csv`). The WP reads the Spec 6 null on
`ha_y` as a *theory-predicted* null — Williams' housing MPC is zero at
CCI = 0 — and the headline comparison is now against Spec 11, where
γ_NLA = 0.060 [0.022, 0.098] excludes Williams' 0.159 and
γ_IFA = 0.035 [0.012, 0.057] includes his 0.022. Under
`PI_METHOD = "ar"` the |λ| collapses to ~0.05–0.09 with the historical
"Australian PI puzzle" (wrong-signed PI coefficient) — treated as a
methodology artefact in the WP.

### 2.2 CCI exploration scripts (standalone)

#### 2.2.1 `cci_alternatives.R` (303 lines)

**Role.** Builds five **public-data CCI alternatives** for the
4-way comparison: a PCA-of-credit-indicators factor, a
credit-gap-derived index, a macroprudential-intensity dummy, and
two cross-checks. Writes `australia_cci_methods_summary.csv` and
`australia_cci_method_comparison.csv`.

**Run directly?** Yes. Standalone; produces outputs without going
through the main estimation pipeline.

**Modify when:** adding another alternative CCI construction for the
4-way comparison.

#### 2.2.2 `cci_fit_decomposition.R` (206 lines)

**Role.** Builds the **CCI fit decomposition** that compares Spec 6
(no CCI) against Spec 8 (Williams CCI) and Spec 9 (Kalman CCI).
Reports adj R² gain attributable to CCI inclusion and the
mean |%-shift| on the disaggregated wealth coefficients.

**Outputs:** `australia_cci_fit_decomposition.csv` and `.md`.

#### 2.2.3 `cci_method_comparison.R` (216 lines)

**Role.** Produces the **4-way CCI method comparison** chart and
CSV — Williams 4-knot, Williams maximal-GETS, Kalman, sectional —
each fit to the consumption equation under common settings.

**Outputs:** `australia_cci_method_4way.csv`,
`australia_cci_method_summary.md`,
`australia_cci_4way_comparison.png`,
`australia_cci_series_comparison.png`.

#### 2.2.4 `cci_placebo_test.R` (422 lines)

**Role.** Two random-knot **placebo batteries** on the 1988Q3+
baseline sample, sourced into an isolated child environment so it
reuses the canonical pipeline `model_data` (Italy-PI) rather than
rebuilding it:

1. *Literal Williams 4-knot*: 200 draws of 4 random knot dates, each
   fit via the same sign-survival reduction as the canonical 4-knot.
2. *Deployed protocol* (new): 200 draws that replace the 15
   institutional knots with random dates and random sign priors, then
   run the same **iterated** maximal-GETS reduction as the deployed
   `cci_williams` — a placebo of exactly the construction the headline
   uses.

**Output:** `australia_williams_knot_placebo.csv` + `.png` +
`_verdict.csv` (literal), and
`australia_williams_knot_placebo_deployed.csv` +
`_deployed_verdict.csv` (deployed protocol).

**Headline result on current vintage:** the literal Williams 4-knot is
at the placebo median — R² at the 45th percentile, |λ| at the 56th
(verdict: "detrending critique vindicated"). The deployed 15-knot
iterated protocol does materially better — R² at the 84th percentile,
|λ| at the 80th, 4 surviving knots vs a placebo median of 5 (verdict:
"moderate support").

#### 2.2.5 `cci_placebo_extended.R` (230 lines)

**Role.** Literal 4-knot protocol on the **back-extended 1976Q3+
sample**. Tests whether the longer pre-deregulation window
strengthens or weakens the canonical Williams 4-knot identification.

**Output:** `australia_williams_knot_placebo_extended.csv` plus
two PNG charts (`_lambda.png`, `_r2.png`) and a summary CSV.

**Headline result (current vintage):** R² at the 36th percentile, |λ|
at the 26th — the canonical 4-knot specification remains below the
random-knot median on the extended sample.

#### 2.2.6 `cci_placebo_maximal_gets_extended.R` (316 lines)

**Role.** Random-knot placebo on the **maximal-GETS canonical**
(15-knot candidate set with sign-prior reduction) on the
back-extended sample. 200 draws of 15 random knots and 15 random
sign priors.

**Output:** `australia_williams_knot_placebo_maximal_extended.csv`
plus two PNG charts and a summary CSV.

**Headline result (current vintage):** maximal-GETS canonical at the
48th R² percentile, 70th |λ| percentile — at the random-knot median on
fit, above it on adjustment speed.

#### 2.2.7 `knot_experiment.R` (475 lines)

**Role.** **Knot experiment** comparing 7 CCI spline variants (the
published 3- and 4-knot specs, extended macroprudential sets, an
"Australia within-sample" set, and the maximal-candidate set pruned by
Hendry-Krolzig sign-prior reduction). The script that motivated the
maximal-GETS → 15-knot canonical choice in May 2026. Now sourced into
an isolated child environment and reuses the canonical pipeline
`model_data` (Italy PI), rebuilding only the standalone variants.

**Output:** `australia_knot_experiment.csv`,
`australia_knot_experiment_estimates.csv`,
`australia_knot_experiment.md`.

### 2.3 Back-extension scripts

#### 2.3.1 `refit_spec1_extended.R` (156 lines)

**Role.** Refits **Spec 1 (aggregate net worth)** on the
back-extended 1976Q3+ sample using `ln_networth_y_proxy`, and
compares to the 1988+ baseline. The data-driven test of whether the
aggregate wealth elasticity is stable when the sample doubles in
length.

**Output:** `spec1_extended_comparison.csv`.

**Headline result:** wealth elasticity stable (+0.112 → +0.107,
−4 %); λ slightly more negative on longer sample.

#### 2.3.2 `refit_spec46_extended.R` (185 lines)

**Role.** Refits **Spec 4** (disaggregated no-CCI) and **Spec 6**
(preferred) on the back-extended sample. Spec 4 uses the
disaggregated wealth proxies (`ha_y_proxy`, `nla_y_proxy`,
`eq_y_proxy`, `super_y_proxy`). Spec 6 itself can't fit on the
extended sample (binding `cci_ratio` at 2002Q3+) so the Spec 6 row
is a no-op for now; the back-extended preferred form is **Spec 6b**
defined in `run_all_specifications()` proper.

**Output:** `spec46_extended_comparison.csv`.

### 2.4 Out-of-sample forecasting

#### 2.4.1 `oos_forecast.R` (541 lines)

**Role.** Rolling **out-of-sample forecast validation** on six specs
(Spec 4, Spec 6, Spec 7, Spec 8, Spec 9, and now Spec 11 — a previous
version skipped the headline as "missing regressors") over 36
expanding-window cuts from 2015Q1 to 2023Q4 at horizons
h ∈ {1, 4, 8}, with random-walk-with-drift and AR(1) benchmark
forecasters.

**Output:** `australia_oos_rmse.csv`,
`australia_oos_forecasts.csv`,
`australia_oos_forecast_paths.png`,
`australia_oos_rolling_rmse.png`.

**Headline finding:** at h = 1 Spec 8 and Spec 11 edge out the
RW-drift benchmark on RMSE (0.0290 / 0.0292 vs 0.0309); at h = 4 and
h = 8 the random walk dominates. Standard "macro forecasting puzzle"
recorded honestly in the WP.

### 2.5 Standalone inference and nesting scripts

#### 2.5.1 `gamma_inference.R` (222 lines)

**Role.** NS-126/NS-134 — attaches uncertainty to the implied
structural γ_i = β_i/|λ| ratios, for **both Spec 6 (conventional
baseline) and Spec 11 (LIVES headline)**: (1) delta-method SEs/CIs
from the Newey-West vcov of (β, λ); (2) an aggregate wealth γ_W with
delta-method CI; (3) a seeded moving-block residual bootstrap
(block length 8, B = 1000, random block starts — a previous version
generated starts from a deterministic cycle and was not a bootstrap);
(4) Williams-in-CI flags. For Spec 11 the housing reference is
Williams' *peak* MPC (0.0488) because the deployed CCI is
peak-normalised.

**Run:** `Rscript Australia/R/gamma_inference.R` (standalone).
**Output:** `australia_gamma_inference.csv`.

#### 2.5.2 `martin_nesting.R` (96 lines)

**Role.** NS-135 — tests whether MARTIN's consumption block (income/
net-wealth homogeneity with net-wealth elasticity ≈ 0.17) is accepted
on the contemporary data via H0: coef(lincome) = 1 in
`lcons ~ lincome + log(networth_y) + real_rate`.

**Output:** `australia_martin_nesting.csv`.

#### 2.5.3 `pi_realtime_diagnostic.R` (96 lines)

**Role.** Diagnostic for the real-time (expanding-window) variant of
the De Bonis direct-forecast permanent income; feeds the look-ahead
caveat and `australia_pi_realtime_robustness.csv`.

---

## 3. `LIVES/R/` — the multi-equation extension

Nine scripts that take the cached master dataset, derive the
multi-equation regressors (HP, M, HEW), fit the 2-, 3-, and 4-equation
SUR systems, and test Williams' Table 1 calibrations as parameter
restrictions. Implements **Phase A** of the multi-equation plan in
[`../../LIVES/docs/multi_equation_plan.md`](../../LIVES/docs/multi_equation_plan.md);
findings logged in
[`../../LIVES/docs/phase_a_progress.md`](../../LIVES/docs/phase_a_progress.md).

### 3.1 Data preparation

#### 3.1.1 `lives_data_prep.R` (227 lines)

**Role.** Loads the master dataset built by
`australia_data_download.R`, sources the Australia estimation
helpers (without running the main block), refits the consumption
equation with the maximal-GETS Williams CCI to attach `cci_williams`,
and derives the **LIVES-specific regressors** needed by the
HP / M / HEW equations.

**Regressors built:**

- House-price equation: `log_hp_real`, `dlog_hp_real`,
  `ln_hp_over_yd`, `ecm_lag_H`, `log_credit_y`, `real_rate_x_cci`,
  `d_log_credit_y`, `d_real_rate`, `d4_log_hp_real`.
- Mortgage-stock equation: `log_M_real`, `dlog_M_real`,
  `log_M_over_y`, `ecm_lag_M`.
- HEW equation: `hew_proxy = Δ(fin_loans_proxy) / ydi_ann_nom`
  (credit-flow proxy; Williams' literal definition includes a
  dwelling-investment subtraction not yet sourced), `hew_proxy_z`
  (Williams' heteroskedasticity-correction form), `ecm_lag_W`.

**Output:** `LIVES/outputs/lives_model_data.rds`.

**Run:** `Rscript LIVES/R/lives_data_prep.R` (after the headline
pipeline has cached `Australia/outputs/australia_model_dataset.rds`).

### 3.2 CCI identification

#### 3.2.1 `joint_cci_identification.R` (282 lines)

**Role.** Fits the consumption, HP, mortgage and HEW equations each
with the full 15-knot Williams CCI basis, then requires sign-prior
survival in **all four** equations simultaneously to retain a knot.
Constructs three weighted variants from the joint-survivors:

- `cci_williams_joint` (consumption-equation-weighted, legacy).
- `cci_williams_joint_h` (HP-weighted — Williams' ζ_h = 1
  normalisation).
- `cci_williams_joint_m` (mortgage-weighted).

All three peak-normalised to unity.

**Output:** `LIVES/outputs/lives_joint_cci_survival.csv`,
`LIVES/outputs/lives_model_data.rds` (updated with the three
joint-CCI columns).

**Headline result:** only **1 knot** (1986 financial deregulation)
survives the 4-equation joint test. The three weighted variants
collapse to mathematical identity after peak-normalisation when
based on a single knot, so the ζ_h = 1 normalisation is empirically
inert on the contemporary Australian data.

#### 3.2.2 `sectional_cci_test.R` (298 lines)

**Role.** Implements Williams (Aust §5.1) **sectional sign priors**
— one prior per institutional period rather than per knot. Runs the
random-knot placebo on the back-extended sample under the sectional
constraint.

**Output:** `LIVES/outputs/sectional_cci_comparison.csv`,
`LIVES/outputs/sectional_placebo_summary.csv`.

**Headline result (current vintage):** sectional canonical at the
37th R² percentile, 60th |λ| percentile — still below the random-knot
median on fit, against the pre-implementation hypothesis that
sectional priors would strengthen identification.

### 3.3 Standalone equation diagnostics

#### 3.3.1 `house_price_equation.R` (162 lines)

**Role.** Standalone fit of the Williams-style **house-price ECM**
(Aust §5.1 eq 11 spirit): real-house-price diff regressed on log
income, log credit-to-income, real rate, prime-age share, CCI, the
ECM lag, and short-run dynamics. Sign violation on `cci_williams`
documented in `phase1_findings.md`.

**Output:** `LIVES/outputs/hp_equation_standalone.csv`.

#### 3.3.2 `mortgage_stock_equation.R` (122 lines)

**Role.** Standalone fit of the **mortgage-stock ECM** (Williams Aust
§5.1 eq 12 spirit): real mortgage stock diff regressed on log income,
HP-to-income, real rate, prime-age, CCI, ECM lag, and short-run dynamics.

**Output:** `LIVES/outputs/mortgage_stock_equation_standalone.csv`.

### 3.4 SUR systems

#### 3.4.1 `lives_sur_2eq.R` (253 lines)

**Role.** Phase 1 **two-equation SUR** (consumption + house prices)
on the back-extended sample.

**Output:** `LIVES/outputs/lives_sur_2eq_coefs.csv`,
`lives_sur_2eq_compare.csv` (OLS vs SUR side-by-side),
`lives_sur_2eq_resid_corr.csv`.

**Headline result (current vintage):** ρ̂(ε_C, ε_H) ≈ −0.013 under
SUR (−0.011 under OLS) — negligible. Joint estimation gives no
efficiency gain at the quarterly frequency. The case for
multi-equation LIVES therefore rests on cross-equation parameter
restrictions, not on residual covariance.

#### 3.4.2 `lives_sur_3eq.R` (281 lines)

**Role.** Phase 3 **three-equation SUR** (consumption + HP + mortgage
stock) using `cci_williams_joint`. Compares three regimes:
single-eq OLS with cons-only CCI, single-eq OLS with joint CCI, and
3-eq SUR with joint CCI.

**Output:** `LIVES/outputs/lives_sur_3eq_coefs.csv`,
`lives_phase3_comparison.csv`.

**Headline result:** joint CCI identification **flips** the HP
equation's CCI sign from significantly negative to significantly
positive — Williams' cross-equation sign restriction working as
intended — but does NOT fix the mortgage equation's sign violation.

#### 3.4.3 `lives_sur_4eq.R` (239 lines)

**Role.** Phase A **four-equation SUR** (consumption + HP + M + HEW)
under four regimes: cci_williams baseline + three weighted joint
variants (cons / HP / M). Tests A2 (ζ_h = 1 normalisation) by
comparing the HP-weighted variant against the cons-weighted variant.

**Output:** `LIVES/outputs/lives_sur_4eq_coefs.csv`,
`lives_sur_4eq_residcorr.csv`,
`lives_phase_a_summary.csv`.

**Headline result:** with only 1 knot surviving the 4-eq joint
test, all three weighted variants are mathematically identical
after peak-normalisation. Mortgage and HEW residuals correlate at
**+0.83** — the proxy HEW is essentially the change-form of the
mortgage-stock LHS. A properly constructed HEW with the
dwelling-investment subtraction is the binding constraint on
further multi-equation work.

### 3.5 Phase B — Williams calibration test

#### 3.5.1 `williams_calibration_test.R` (186 lines)

**Role.** Phase B item B2. Refits Spec 6 on the canonical Italy-LP
master and tests **Williams' six Table 1 calibrations** (γ_HA,
γ_IFA, γ_NLA, ln_hp_over_y at CCI = 0, ψ_0, λ implicitly) as linear
restrictions on the OLS coefficient vector using
`car::linearHypothesis` with the Newey–West vcov.

**Output:** `LIVES/outputs/williams_calibration_wald.csv`.

**Headline result (current vintage):** Williams' Table 1 calibrations
are **not rejected** as a system of restrictions: joint Wald
χ²(6) = 7.55, p = 0.27 across all six; χ²(4) = 1.83, p = 0.77 for the
four wealth restrictions alone; no individual restriction rejects
(γ_HA: χ²(1) = 1.54, p = 0.22). The implied-OLS target for each
restriction is γ × |λ̂| (the structural convention γ = OLS/|λ|); an
earlier version compared against γ × λ̂ (signed), which flipped the
target sign and spuriously produced χ²(6) = 29.1 — fixed 2026-06
([NS-125](next_steps_plan_2026.md)). The non-rejection is partly a
low-power result (n = 86, wide NW bands) — note the tension with
Spec 12, where *imposing* the same calibrations collapses λ to −0.029.
Discussion in
[`companion_paper_draft.md §7`](../../LIVES/docs/companion_paper_draft.md).

---

## 4. Configuration flags

Three runtime flags materially change pipeline behaviour. All are at
the top of their respective files.

### `PI_METHOD` (in `australia_estimation.R`, ~line 45)

```r
PI_METHOD <- "italy"  # canonical (resolved 2026-05-07): De Bonis et al.
                      # (2020) direct single-regression forecast
PI_METHOD <- "ar"     # robustness column: rolling AR(8) + trend + ogive
                      # (the GFC ogive is now a real gfc_ogive toggle)
```

The direct forecast (a) uses the labour-force-share predictor,
(b) flips the long-run permanent-income coefficient to positive
(resolving the Australian PI puzzle under AR), (c) gives Spec 6
|λ| ≈ 0.24 and Spec 11 |λ| ≈ 0.45. The AR vs direct-forecast
comparison is produced regardless of which method is canonical. (Do
not call this method a "Jordà local projection" — it is one full-sample
regression of the pre-aggregated discounted target.)

### `USE_WILLIAMS_SDMMA_BASIS` / `USE_INSTITUTIONAL_CCI_OVERLAY`
(in `australia_data_download.R`, ~line 70)

The single overloaded `USE_INSTITUTIONAL_CCI` flag was decoupled in
June 2026 (NS-131) into two independent switches, so the canonical
configuration (basis ON, overlay OFF) is reproducible from a cold
rebuild:

```r
USE_WILLIAMS_SDMMA_BASIS      <- TRUE   # canonical: attach the 15-knot SDMMA basis
USE_INSTITUTIONAL_CCI_OVERLAY <- FALSE  # canonical: NO pre-2002 cci_ratio backfill
```

When `USE_WILLIAMS_SDMMA_BASIS = TRUE`, the 15-knot Williams candidate
SDMMA basis is attached and `fit_consumption_with_williams_cci()` runs
the **iterated** sign-survival reduction (up to 10 iterations; on the
current vintage **4 knots survive**: sdmma_2007_09 (GFC), sdmma_2009_01
(FHB Boost), sdmma_2019_01 (Hayne RC), sdmma_2020_04 (COVID)). When
`FALSE`, Specs 8/9/10/11/12 return `NULL` and are filtered out of
downstream pipeline steps. `USE_INSTITUTIONAL_CCI_OVERLAY = TRUE`
backfills `cci_ratio` pre-2002 with the Muellbauer-style
regime+indicator blend (shifts the Spec 2/5/6 short-run CCI term).

---

## 5. Common workflows

### "I want to re-estimate without changing anything."
```
Rscript Australia/R/run_estimation_from_rds.R
```
~30 seconds. Reuses the cached RDS.

### "I want to refresh the master dataset from raw inputs."
```
Rscript Australia/R/australia_consumption_model.R
```
A few minutes; re-parses ABS workbooks (cached) and the user CSVs.

### "I want to run offline / from the portable CSV."
```
Rscript Australia/R/load_master_from_csv.R
```

### "I want to refresh the portable CSV after a cold rebuild."
```
Rscript Australia/R/australia_consumption_model.R
Rscript Australia/R/export_master_csv.R
```

### "I want to run the multi-equation LIVES pipeline."
```
Rscript LIVES/R/lives_data_prep.R                # build LIVES model_data
Rscript LIVES/R/joint_cci_identification.R       # 4-eq joint CCI survival
Rscript LIVES/R/lives_sur_2eq.R                  # phase 1 (C+HP)
Rscript LIVES/R/lives_sur_3eq.R                  # phase 3 (C+HP+M)
Rscript LIVES/R/lives_sur_4eq.R                  # phase A (C+HP+M+HEW)
Rscript LIVES/R/williams_calibration_test.R      # phase B Wald test
```

### "I want to switch to the AR robustness column."
Open `Australia/R/australia_estimation.R`, change
`PI_METHOD <- "italy"` to `PI_METHOD <- "ar"`, then
`Rscript Australia/R/run_estimation_from_rds.R`.

### "I want to add a new specification."
1. In `australia_estimation.R`, find `run_all_specifications()`.
2. Add a new `specXX <- fit_ecm_spec(...)` block.
3. Add `specXX = specXX` to the returned list.
4. Run `Rscript Australia/R/run_estimation_from_rds.R`.

### "I want to add a new robustness check."
1. In `australia_estimation.R`, find `run_italy_style_robustness()`.
2. Copy one of the existing blocks (IV, SUR, Chow, Drehmann, scaled,
   NPY) as a template.
3. Wrap in `tryCatch` with a `path` variable for the output CSV.

### "I want to add a new policy counterfactual."
1. In `australia_estimation.R`, find `run_counterfactuals()`.
2. Build a `modify_X_and_refit()` call with the regressors to zero
   out or override.
3. Append a scenario to the `scenarios` list with `delta_dlog_c`
   and `cum_delta_log_c` columns.
4. Optionally extend the summary block with the h = 4 / h = 8 /
   end-of-sample pick.

### "I want to add a new variable to `master`."
1. In `australia_data_download.R`, locate the relevant section.
2. Build the variable as a named tibble with a `date` column.
3. Add it to the `master <- master %>% left_join(...)` chain.
4. Add it to the coverage report `for (v in c(...))` loop.
5. Run a cold rebuild and refresh the portable CSV.

### "I want to change the preferred-spec selection criteria."
Edit `select_preferred_spec()` in `australia_estimation.R`.

### "I want to render the working paper to PDF."
See [`RENDER.md`](RENDER.md). Default path:
```
brew install pandoc typst
cd Australia/docs/ && make pdf
```

---

## 6. Test suite

### Location
`tests/testthat/` contains:

- `test-units.R` — unit conversion (`rescale_to_millions`, etc.).
- `test-dates.R` — date parsers (ABS quarter-label conventions).
- `test-stats.R` — ADF and DOLS helpers.
- `test-cci.R` — institutional CCI smoke tests.
- `test-permanent_income.R` — `compute_log_yp_over_y` and
  `adaptive_permanent_income_log`.

24 `test_that` blocks; all should pass with no skips.

### Run
```
Rscript tests/testthat.R
```

The CI workflow at `.github/workflows/check.yml` runs the test
suite + the full estimation pipeline (from cached RDS) on every
push.

---

## 7. Function index — alphabetical

Functions defined in the project, with file and approximate line
number. Numbers are approximate; consult the file if a number looks
off.

| Function | File | Purpose |
|---|---|---|
| `adaptive_permanent_income_log()` | model_helpers.R | EWMA log-income smoothing |
| `add_model_variables()` | australia_estimation.R | Short-run + dummy + Δ²log CCI construction (incl. `d2_log_creditd02_lag2` for Spec 6b) |
| `build_comparison_table()` | australia_estimation.R | Italy / Australia λ + structural γ comparison |
| `build_credit_regime_basis()` | model_helpers.R | Regime-indicator basis (legacy) |
| `build_credit_ssm_factor()` | model_helpers.R | Kalman state-space CCI factor (Spec 9) |
| `build_lambda_robustness_table()` | australia_estimation.R | λ across 4 sample variants |
| `build_results_table()` | australia_estimation.R | Coefficient + diagnostics CSVs |
| `build_williams_cci_basis()` | model_helpers.R | Maximal-GETS 15-knot SDMMA basis |
| `build_williams_cci_basis_canonical()` | model_helpers.R | Williams 4-knot canonical (1979/1992/1998/2007) |
| `build_williams_cci_basis_sectional()` | model_helpers.R | Sectional sign-prior basis (Aust §5.1) |
| `compare_pi_methods()` | australia_estimation.R | AR vs Italy LP side-by-side refit |
| `compute_expected_log_income_path()` | model_helpers.R | Multi-step expected log income |
| `compute_income_volatility()` | australia_estimation.R | AR(8) residual proxy |
| `compute_log_yp_over_y()` | model_helpers.R | log(y^p/y) with discount weights |
| `construct_institutional_cci()` | model_helpers.R | Muellbauer regime + indicator blend (legacy) |
| `construct_permanent_income()` | australia_estimation.R | Rolling AR(8) PI forecaster (`gfc_ogive` toggle) |
| `construct_permanent_income_italy()` | australia_estimation.R | De Bonis et al. (2020) direct-forecast PI (`real_time` option) |
| `fit_consumption_with_williams_cci()` | australia_estimation.R | **Iterated** Williams CCI knot-survival fit (max 10 iter) |
| `fit_lives_calibrated_spec()` | australia_estimation.R | Spec 12 — calibrated LIVES headline (Williams Table 1 imposed) |
| `fit_dols_spec()` | model_helpers.R | Dynamic OLS cointegrating regression |
| `fit_ecm_spec()` | australia_estimation.R | Single-spec OLS + NW HAC fit |
| `fit_long_run_spec()` | model_helpers.R | Static cointegrating regression |
| `fit_rolling_window()` | australia_estimation.R | 60-quarter rolling coefficients |
| `fit_williams_prior_spec()` | australia_estimation.R | Spec 10 — Williams-calibrated iterative fixed-point OLS |
| `model_diagnostics()` | australia_estimation.R | DW + BP + AR(1)/(4) + Chow + RESET + BIC |
| `monthly_to_quarterly()` | australia_data_download.R | Quarterly mean of monthly observations |
| `pick_abs()` | australia_data_download.R | Series picker by regex on ABS workbooks |
| `pick_preferred_spec_object()` | australia_estimation.R | Helper for downstream pipeline steps |
| `plot_actual_vs_fitted()` | australia_estimation.R | Spec fit + residual chart |
| `plot_longrun_decomposition()` | australia_estimation.R | Headline policy chart of long-run contributions |
| `read_abs_ts_workbook()` | model_helpers.R | Parse ABS time-series workbook |
| `rescale_to_millions()` | model_helpers.R | Convert balance-sheet `$ Billions` to `$ Millions` |
| `run_adf_drift()` | model_helpers.R | ADF test with drift |
| `run_all_specifications()` | australia_estimation.R | Estimate all 14 specs |
| `run_cointegration_battery()` | australia_estimation.R | ADF + Phillips-Ouliaris + Johansen per spec (writes a `note` column) |
| `run_counterfactuals()` | australia_estimation.R | §10.2 NS-012 policy counterfactuals |
| `run_italy_style_robustness()` | australia_estimation.R | Six robustness blocks (IV, SUR, Chow, Drehmann, scaled, NPY); `file_suffix = "_spec11"` re-runs it on the headline |
| `source_isolated()` | australia_estimation.R | Source a sub-script into a fresh child environment (pipeline-state read-only) |
| `run_pi_sensitivity()` | australia_estimation.R | 18-variant PI grid |
| `run_specifications_covid_robust()` | australia_estimation.R | 4 sample variants for λ stability |
| `select_preferred_spec()` | australia_estimation.R | 4-screen rubric + BIC tiebreak |
| `smoothed_step()` | model_helpers.R | SDMMA = 5-MA of 4-MA of step dummy |
| `splice_house_price_series()` | model_helpers.R | Chain-link splicing |
| `test_nla_restriction()` | australia_estimation.R | Wald test of γ_LA + γ_LOANS = 0 |
| `write_model_summary()` | australia_estimation.R | Generate `australia_model_summary.md` |

### LIVES-specific functions

| Function | File | Purpose |
|---|---|---|
| `build_eqns()` | lives_sur_3eq.R / lives_sur_4eq.R | Parametrise the equations by which CCI variant to use |
| `fit_equation_with_basis()` | joint_cci_identification.R | Fit one equation with the full 15-knot Williams basis |
| `fit_sur_regime()` | lives_sur_4eq.R | Estimate a four-equation SUR under a given CCI variant |
| `peak_normalise()` | joint_cci_identification.R / lives_sur_4eq.R | Peak-normalise CCI series to unity |
| `pull_lm()` / `pull_sur()` / `pull_sur_eq()` | lives_sur_3eq.R / lives_sur_4eq.R | Coefficient table extraction helpers |
| `lr_struct()` | lives_sur_3eq.R | Convert OLS coef to structural γ = OLS / |λ| |
| `modify_X_and_refit()` | lives_sur_4eq.R | Recompute fitted values with modified design matrix |

---

## 8. Output reference — what each file contains

### Coefficient tables (Australia)

- `australia_full_results.csv` — Specs 1–12 plus 6b + 7b coefficients
  on the full sample. Columns include `ols_estimate`, `nw_se`,
  `t_stat`, `p_value`, `lambda`, `structural_param` (= ols/|λ|),
  `expected_sign`, `sign_ok`.
- `australia_precovid_results.csv` — same on the pre-COVID sub-sample.
- `australia_all_results.csv` — combined long-format.
- `australia_spec11_variants.csv` — the Spec 11 full coefficient
  vector across all four sample variants (full / precovid /
  covid_dropped / covid_rich).
- `australia_spec11_ogive_robustness.csv` — Spec 11 with the AR-PI
  GFC ogive on (headline) vs off (λ = −0.574).

### Diagnostics

- `australia_full_diagnostics.csv` — n_obs, se_pct, adj_r2, dw,
  lm_het_pval, het_diagnosis, ar1_pval, ar4_pval, chow_pval,
  `chow_method` (`sctest` or `manual_common_coef` fallback),
  reset_pval, schwarz, loglik per spec.
- `australia_precovid_diagnostics.csv` — same on pre-COVID.
- `australia_all_diagnostics.csv` — combined.

### Spec-selection and stability

- `australia_spec_selection.csv` — pass/fail per the 4 screens, BIC,
  `is_preferred` flag. No spec passes the EG cointegration screen, so
  the selector falls back to most-passes and currently picks
  Spec3_LevelNetWorth; the BIC-best spec is Spec 11 (−954.8). The
  paper's headline is Spec 11 on substantive grounds, not the
  selector.
- `australia_lambda_robustness.csv` — λ for each (spec × sample
  variant).
- `australia_breaks.csv` — supF, breakpoints, CUSUM for the preferred
  spec.
- `australia_recursive_coefficients_*.png` — recursive coefficient
  plots.
- `australia_rolling_coefs.csv` + `.png` — 60-quarter rolling
  coefficients on the preferred spec.

### Substantive tests

- `australia_nla_restriction_test.csv` — Wald test of γ_LA + γ_LOANS=0
  for Specs 4/5/6 × 2 samples.
- `australia_williams_cci_knots.csv` — Williams CCI knot survival
  under the iterated maximal-GETS reduction (4 survivors: 2007Q3,
  2009Q1, 2019Q1, 2020Q2).
- `australia_cci_williams_series.csv` — the deployed CCI path and its
  five interaction regressors; charted in
  `australia_cci_williams_path.png` (zero pre-2007Q3, plateau at 1
  over 2010–18, ≈ −1.6 post-2022).
- `australia_cci_interaction_corr.csv` — correlation matrix of the
  CCI interaction block (|ρ| 0.66–0.97 — the collinearity that
  motivates Spec 12).
- `australia_spec8_sign_prior_verdicts.csv` — Spec 8 interaction-term
  sign verdicts vs Williams.
- `australia_cointegration.csv` — ADF + PO + Johansen per spec, with a
  `note` column.
- `australia_gamma_inference.csv` — delta-method + seeded
  block-bootstrap CIs on the structural γ, Spec 6 and Spec 11.
- `australia_martin_nesting.csv` — MARTIN consumption-block nesting
  test.
- `australia_chow_battery.csv` / `_spec11.csv` — Chow tests at 1995Q1,
  2000Q1, 2008Q3, 2020Q1.

### CCI exploration outputs

- `australia_cci_method_4way.csv` — 4-way comparison summary.
- `australia_cci_method_comparison.csv` — full coefficient table.
- `australia_cci_method_summary.md` — markdown commentary.
- `australia_cci_4way_comparison.png` — chart.
- `australia_cci_series_comparison.png` — the four CCI series plotted
  side by side.
- `australia_cci_fit_decomposition.csv` + `.md` — Spec 6 (no CCI)
  vs Spec 8 / Spec 9 (with CCI) decomposition.
- `australia_knot_experiment*.csv` + `.md` — knot-by-knot
  identification experiment outputs.

### Placebo battery

- `australia_williams_knot_placebo.csv` + `.png` +
  `_verdict.csv` — literal Williams 4-knot on 1988+ (45th R²
  percentile — the detrending-critique benchmark).
- `australia_williams_knot_placebo_deployed.csv` +
  `_deployed_verdict.csv` — deployed-protocol placebo (iterated
  15-knot reduction, random knots + random priors; 84th R²
  percentile, moderate support).
- `australia_williams_knot_placebo_extended*.csv` + `*.png` —
  literal Williams 4-knot on 1976Q3+ (36th R² / 26th |λ|).
- `australia_williams_knot_placebo_maximal_extended*.csv` + `*.png` —
  maximal-GETS canonical on 1976Q3+ (48th R² / 70th |λ|).

### Italy-style robustness suite

Each of the following also exists in a `*_spec11.csv` variant — the
suite runs on both the selector-preferred spec and the Spec 11
headline (Step 9b):

- `australia_iv_robustness.csv` — OLS vs IV (instruments `ecm_lag` +
  `ln_yp_over_y` with lagged income/unemployment/mortgage-rate
  instruments).
- `australia_iv_diagnostics.csv` — first-stage F, Wu-Hausman, Sargan.
- `australia_joint_pi_robustness.csv` — single-eq vs joint PI+cons SUR
  (now estimated on the canonical pipeline `model_data`; the old
  committed tables had been contaminated by AR-PI data from
  non-isolated sub-script sourcing).
- `australia_drehmann_robustness.csv` — flat real rate vs amortising.
- `australia_scaled_income_robustness.csv` — disposable vs 50/50 scaled.
- `australia_williams_income_robustness.csv` — disposable vs Williams NPY.
- `australia_wls_robustness.csv` — OLS-NW vs WLS on preferred spec.
- `australia_permanent_income_sensitivity.csv` — 18-cell PI grid.
- `australia_pi_method_comparison.csv` + `_meta.csv` — AR vs De Bonis
  direct forecast.
- `australia_pi_realtime_robustness.csv` — full-sample vs real-time
  expanding-window PI (look-ahead caveat).

### Out-of-sample validation

- `australia_oos_rmse.csv` — RMSE per (spec × horizon).
- `australia_oos_forecasts.csv` — full forecast paths.
- `australia_oos_forecast_paths.png`, `australia_oos_rolling_rmse.png`.

### Back-extension

- `spec1_extended_comparison.csv` — Spec 1 on 1988+ vs 1976+.
- `spec46_extended_comparison.csv` — Spec 4 on 1988+ vs 1976+.

### Williams comparison

- `australia_williams_comparison.csv` — wide table, 13 terms.
- `australia_williams_comparison.md` — markdown commentary (basis for
  WP §9).
- `australia_williams_spec8_comparison.csv` — CCI-interaction match.

### Counterfactuals (NS-012)

- `australia_counterfactuals.csv` — long-format date × scenario × value.
- `australia_counterfactuals_summary.csv` — h = 4q, h = 8q,
  end-of-sample log-c gaps per scenario.
- `australia_counterfactual_paths.png` — comparison chart.

### Underlying data

- `australia_model_dataset.csv` — coverage table.
- `australia_model_dataset.rds` — `master` tibble (for Mode 2).
- `data_raw/master_data.csv` — portable CSV (for Mode 3).

### Narrative

- `australia_model_summary.md` — auto-generated markdown summary with
  traffic-light diagnostics, preferred-spec rationale, headline
  results.

### LIVES outputs

- `lives_joint_cci_survival.csv` — knot-by-knot survival across the
  four equations.
- `lives_model_data.rds` — LIVES-prepped tibble with `cci_williams`,
  `cci_williams_joint{,_h,_m}`, HEW proxy.
- `lives_sur_2eq_coefs.csv`, `lives_sur_2eq_compare.csv`,
  `lives_sur_2eq_resid_corr.csv` — Phase 1 SUR.
- `lives_sur_3eq_coefs.csv`, `lives_phase3_comparison.csv` — Phase 3.
- `lives_sur_4eq_coefs.csv`, `lives_sur_4eq_residcorr.csv`,
  `lives_phase_a_summary.csv` — Phase A four-equation SUR.
- `hp_equation_standalone.csv`,
  `mortgage_stock_equation_standalone.csv` — single-equation
  diagnostics.
- `sectional_cci_comparison.csv`, `sectional_placebo_summary.csv` —
  sectional sign-prior comparison.
- `williams_calibration_wald.csv` — Phase B Wald test results.

---

## 9. Where to look for what

| Question | Answer |
|---|---|
| "Where is the canonical entry point?" | `australia_consumption_model.R` (Mode 1) |
| "How is permanent income computed?" | `construct_permanent_income()` (AR) and `construct_permanent_income_italy()` (De Bonis direct forecast) in `australia_estimation.R` |
| "Where is Spec 11 (LIVES headline) defined?" | `run_all_specifications()` in `australia_estimation.R`, after Spec 8 (search `Spec11_LIVES_Headline`) |
| "Where is Spec 12 (calibrated LIVES) defined?" | `fit_lives_calibrated_spec()` in `australia_estimation.R` |
| "How is the Williams spline implemented?" | `build_williams_cci_basis()` and friends in `model_helpers.R`; iterated knot-survival in `fit_consumption_with_williams_cci()` in `australia_estimation.R` |
| "Where is the spec selector?" | `select_preferred_spec()` in `australia_estimation.R` |
| "Where is the §10.2 counterfactual code?" | `run_counterfactuals()` in `australia_estimation.R`, Step 11 of MAIN |
| "Where is the Italy comparison logic?" | `build_comparison_table()` in `australia_estimation.R` |
| "Where is the Williams comparison logic?" | `williams_comparison.R` (whole file) |
| "Where is the master dataset built?" | `australia_data_download.R` §3 (the `master <- ... %>% left_join(...)` chain) |
| "Where is Spec 6b defined?" | `run_all_specifications()` in `australia_estimation.R`, just after Spec 6 |
| "Where is the HEW proxy constructed?" | `LIVES/R/lives_data_prep.R`, in the mutate block adding `hew_proxy` |
| "Where is the joint-CCI survival test?" | `LIVES/R/joint_cci_identification.R` |
| "Where is the Williams calibration Wald test?" | `LIVES/R/williams_calibration_test.R` |
| "How do I add a new ABS series?" | See the §5 'Common workflows' walkthrough |
| "Where are the published Williams numbers stored?" | Hard-coded inside `williams_comparison.R` (the `williams <- tribble(...)` block) and inside `LIVES/R/williams_calibration_test.R` |
| "How do I render the WP to PDF?" | `cd Australia/docs/ && make pdf` (needs pandoc + typst; see `RENDER.md`) |

---

**Generated alongside the May 2026 repo cleanup; refreshed for
Phase A / B / Spec 6b / Quarto in May 2026 (2026-05-21); refreshed
again in June 2026 for the ln_hp_over_y fix, Spec 11/12, the
deployed-protocol placebo, pipeline isolation (`source_isolated`),
and the full output re-run (2026-06-12).
Cross-linked with `data.md`, `wp_draft.md`,
`next_steps.md` (historical backlog),
`next_steps_plan_2026.md` (forward-looking tier plan),
`../../LIVES/docs/multi_equation_plan.md` and
`../../LIVES/docs/phase_a_progress.md`.**
