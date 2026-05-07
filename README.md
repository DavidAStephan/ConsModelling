# Australian Household Consumption Model

## Project status

Active research repository for a Muellbauer-Williams Australian
household consumption ECM, targeting a central-bank working paper. As
of 2026-05-07 the canonical permanent-income method is `"italy"` (Jordà
2005 local projection); see [`Ausreplication/docs/project_status.md`](Ausreplication/docs/project_status.md)
for the latest preferred-spec coefficients and
[`Ausreplication/docs/wp_draft.md`](Ausreplication/docs/wp_draft.md) for
the WP draft.

## What this is

A single-equation, Muellbauer-style error-correction model (ECM) for
Australian household consumption per capita, with a parallel Italy
implementation used as a benchmark for cross-country comparison.
Following the consumption equation in the Duca / Muellbauer "LIVES"
framework, short-run consumption growth is driven by an error-correction
term in income relative to lagged consumption, plus the real mortgage
rate, wealth-to-income ratios, a permanent-income proxy, and a
credit-conditions index. The two countries share the same estimation
engine and reporting structure so long-run coefficients and the speed of
adjustment (λ) can be compared on a like-for-like basis.

## Entry points

- Full pipeline (download, build dataset, estimate, write outputs):
  `Rscript Ausreplication/R/australia_consumption_model.R`
- Fast re-estimation from the cached dataset (skips ABS downloads):
  `Rscript Ausreplication/R/run_estimation_from_rds.R`
- Offline / hand-edit re-estimation from the portable CSV:
  `Rscript Ausreplication/R/load_master_from_csv.R`

The Australia master orchestrator is
[australia_consumption_model.R](Ausreplication/R/australia_consumption_model.R);
data construction lives in
[australia_data_download.R](Ausreplication/R/australia_data_download.R)
and estimation in
[australia_estimation.R](Ausreplication/R/australia_estimation.R).

## Estimation specifications

Eleven specifications are fit on both the full sample (1988Q4–2024Q4)
and a pre-COVID sample (1988Q4–2019Q4):

- **Spec 1** — log net worth (aggregate).
- **Spec 2** — log net worth plus a credit-conditions short-run term (Δ² log CCI, lag 2).
- **Spec 3** — net worth in levels.
- **Spec 4** — disaggregated wealth (housing, equities, superannuation, net liquid assets) without credit conditions.
- **Spec 5** — disaggregated wealth with the full short-run dynamics block (CCI shock, income acceleration, unemployment shock, income volatility).
- **Spec 6** — disaggregated wealth with a post-2008 break in the permanent-income coefficient. The narrative headline specification (Williams form).
- **Spec 7** — Spec 6 + cohort terms (prime-age share, FHB share) and synthetic mortgage burden.
- **Spec 7b** — Spec 7 with the RBA E13 measured mortgage payment burden over the post-2009 sample.
- **Spec 8** — Williams CCI interactions on the maximal-GETS basis (15-knot candidate set → 6 surviving via Hendry-Krolzig sign-prior reduction).
- **Spec 9** — Spec 8 with the Kalman state-space CCI replacing the Williams smoothed-step spline.
- **Spec 10** — Williams-prior calibrated specification (γ_IFA = 0.022, ψ_0 = 0.20, ψ_1 = 0.93, ϖ = 1.2; iterative fixed-point OLS).

The canonical permanent-income method is `PI_METHOD = "italy"` (Jordà
2005 local projection with labour-force-share predictor); the
historical AR(8) forecaster is retained as a robustness column.

## Outputs

Written to [Ausreplication/outputs/](Ausreplication/outputs/).

Per-spec coefficient tables:
[australia_full_results.csv](Ausreplication/outputs/australia_full_results.csv),
[australia_precovid_results.csv](Ausreplication/outputs/australia_precovid_results.csv),
[australia_all_results.csv](Ausreplication/outputs/australia_all_results.csv).

Diagnostics:
[australia_full_diagnostics.csv](Ausreplication/outputs/australia_full_diagnostics.csv),
[australia_precovid_diagnostics.csv](Ausreplication/outputs/australia_precovid_diagnostics.csv).

Italy–Australia comparison:
[italy_australia_comparison.csv](Ausreplication/outputs/italy_australia_comparison.csv),
[italy_australia_lambda.csv](Ausreplication/outputs/italy_australia_lambda.csv).

Data coverage / cached dataset:
[australia_model_dataset.csv](Ausreplication/outputs/australia_model_dataset.csv),
[australia_model_dataset.rds](Ausreplication/outputs/australia_model_dataset.rds).

Plots (Spec 1 baseline, Spec 6 preferred):
[australia_spec1_lognetworth_actual_vs_fitted.png](Ausreplication/outputs/australia_spec1_lognetworth_actual_vs_fitted.png),
[australia_spec1_lognetworth_residuals.png](Ausreplication/outputs/australia_spec1_lognetworth_residuals.png),
[australia_spec6_preferred_actual_vs_fitted.png](Ausreplication/outputs/australia_spec6_preferred_actual_vs_fitted.png),
[australia_spec6_preferred_residuals.png](Ausreplication/outputs/australia_spec6_preferred_residuals.png).

Additional outputs (now produced by every run):
[australia_model_summary.md](Ausreplication/outputs/australia_model_summary.md),
[australia_cointegration.csv](Ausreplication/outputs/australia_cointegration.csv),
[australia_williams_comparison.csv](Ausreplication/outputs/australia_williams_comparison.csv),
[australia_williams_knot_placebo.png](Ausreplication/outputs/australia_williams_knot_placebo.png),
[australia_cci_fit_decomposition.md](Ausreplication/outputs/australia_cci_fit_decomposition.md),
[australia_oos_rmse.csv](Ausreplication/outputs/australia_oos_rmse.csv),
[australia_oos_forecast_paths.png](Ausreplication/outputs/australia_oos_forecast_paths.png).

## Data sources

All series are public.

- ABS 5206008 — Household Final Consumption Expenditure (chain volume + current price).
- ABS 5206020 — Household Income Account (gross disposable income, mortgage interest, wages).
- ABS 5232035 — Household Balance Sheet (financial assets, liabilities, residential land and dwellings). Binding sample start: 1988Q3.
- ABS 641601 — Residential Property Price Indexes (bridge to 2003).
- ABS 643201 — Total Value of Dwellings (mean price, current).
- ABS 560101 — Lending Indicators (housing credit flow, FHB share).
- ABS 6202001 — Labour Force (unemployment rate, population aged 15+).
- ABS 3101059 — Estimated Resident Population (per-capita deflation, cohort shares).
- RBA `f06hist.xlsx` — Standard Variable Mortgage Rate (historical).
- `data_raw/houseprice_old.csv` — pre-2003 house-price back-fill.

## Current limitations

- This is a single-equation consumption model, not the full multi-equation LIVES system. Wealth, debt, house prices and credit conditions are taken as conditioning variables rather than jointly modelled. See [Scoping decision required](#scoping-decision-required--lives-extension).
- The estimation sample begins 1988Q4, bound by ABS 5232035 (household balance sheet). Williams' canonical 4-knot CCI (1979/1992/1998/2007) is at the median of a random-knot placebo distribution on this sample (only one knot survives sign-prior reduction); the maximal-GETS reduction is the methodological response. Sample back-extension to ~1975 would resolve the truncated-CCI identification.
- The implied long-run γ on individual wealth terms is roughly a quarter of Williams' published values under canonical Italy LP, attributed to the same truncated-CCI variation problem.
- Heteroskedasticity is structural in every full-sample spec. Newey-West HAC standard errors are reported.
- The joint SUR consumption-plus-PI block currently fails on a CHOLMOD singular-matrix error under the new Italy LP sample; not a binding gap for the WP narrative.
- Out-of-sample forecasting at h ≥ 4 quarters does not systematically beat random-walk-with-drift — a standard "macro forecasting puzzle" finding the WP records honestly.

## Scoping decision required — LIVES extension

The repo currently implements the Muellbauer/Williams consumption
equation only. Whether to extend toward the full LIVES system is a
scoping decision the project owner needs to make. Three options, framed
as choices rather than recommendations:

**Option A — Stay single-equation.** Rebrand as "an Australian counterpart to the consumption equation in the Muellbauer/Williams LIVES system" rather than "a LIVES adaptation". Lowest effort. Most defensible against critique that wealth and credit conditions are endogenous to consumption only via this single equation, because the README would no longer claim otherwise.

**Option B — Two-equation system.** Extend to consumption plus housing wealth (or consumption plus debt) using `vars::VAR` or seemingly unrelated regression. Roughly 1–2 weeks of work. Addresses the most pointed wealth-endogeneity objection while keeping the system small enough to estimate and document.

**Option C — Full LIVES port.** Consumption plus housing wealth, debt, house prices and credit conditions, with cross-equation restrictions on the long-run coefficients. Multi-month effort. Faithful to Muellbauer/Williams.

This requires user judgement. The README should be updated once a choice is made.
