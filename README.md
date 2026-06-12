# Australian Household Consumption Model

## Project status

Active research repository for a Muellbauer-Williams Australian
household consumption ECM, targeting a central-bank working paper.
The canonical permanent-income method is `"italy"` — the De Bonis,
Liberati, Muellbauer & Rondinelli (2020) direct single-regression
forecast of the discounted income aggregate (an earlier draft
mislabelled this a "Jordà (2005) local projection"). The headline
empirical work — back-extension to 1976Q3, the faithful LIVES headline
specification (Spec 11), the calibration cross-check (Spec 12), the
placebo battery, and a multi-equation LIVES scaffold — has landed; see
[`Australia/docs/wp_draft.md`](Australia/docs/wp_draft.md)
for the full state-of-play (data construction, identification, results,
robustness, Williams comparison, conclusion) and
[`Australia/docs/next_steps_plan_2026.md`](Australia/docs/next_steps_plan_2026.md)
for the forward-looking tier-based plan.

## What this is

A single-equation, Muellbauer-style error-correction model (ECM) for
Australian household consumption per capita, with a parallel Italy
implementation used as a benchmark for cross-country comparison.
The headline specification (Spec 11) is the faithful form of the
consumption equation in the Duca / Muellbauer / Williams "LIVES"
framework (Williams' Eq 7): short-run consumption growth is driven by
an error-correction term in income relative to lagged consumption, plus
wealth-to-income ratios and a credit-conditions index (CCI) that enters
the long run through interaction channels — housing wealth scaled by
CCI, the real rate scaled by CCI, the house-price-to-income ratio scaled
by (1 − 1.2·CCI), permanent income geared by ψ(CCI), and an
autonomous-consumption CCI intercept. A conventional constant-MPC
wealth ECM (Spec 6) is retained as the baseline for comparison with the
prior literature. The Australia and Italy implementations share the same
estimation engine and reporting structure so long-run coefficients and
the speed of adjustment (λ) can be compared on a like-for-like basis.

Headline results (full sample 1988Q3–2024Q4, n = 146): Spec 11
λ = −0.448 (t = −3.57); across the three COVID-controlled variants the
identified speed of adjustment is λ ≈ −0.25 (pre-COVID −0.266,
t = −4.85, n = 126; COVID-dropped −0.248, t = −6.66; COVID-rich −0.242).
Wealth MPCs are significant and COVID-robust: structural γ_NLA = 0.060
(95% CI [0.022, 0.098]) and γ_IFA = 0.035 ([0.012, 0.057]). The
structural permanent-income gearing ψ comes out at ≈ 1.02–1.13, above
the theoretical 0.95 bound — disclosed, not hidden. The conventional
baseline Spec 6 has λ = −0.239 (t = −2.55, n = 86); the
Williams-calibrated variants collapse (Spec 12 λ = −0.029, Spec 10
λ = −0.057), ruling out calibration transfer.

## Entry points

- Full pipeline (download, build dataset, estimate, write outputs):
  `Rscript Australia/R/australia_consumption_model.R`
- Fast re-estimation from the cached dataset (skips ABS downloads):
  `Rscript Australia/R/run_estimation_from_rds.R`
- Offline / hand-edit re-estimation from the portable CSV:
  `Rscript Australia/R/load_master_from_csv.R`

The Australia master orchestrator is
[australia_consumption_model.R](Australia/R/australia_consumption_model.R);
data construction lives in
[australia_data_download.R](Australia/R/australia_data_download.R)
and estimation in
[australia_estimation.R](Australia/R/australia_estimation.R).

## Estimation specifications

Fourteen specifications are fit on the full sample (1988Q3–2024Q4) and
on pre-COVID (1988Q3–2019Q4), COVID-dropped, and COVID-rich sample
variants (all fourteen run on all four variants):

- **Spec 1** — log net worth (aggregate).
- **Spec 2** — log net worth plus a credit-conditions short-run term (Δ² log CCI, lag 2).
- **Spec 3** — net worth in levels.
- **Spec 4** — disaggregated wealth (housing, equities, superannuation, net liquid assets) without credit conditions.
- **Spec 5** — disaggregated wealth with the full short-run dynamics block (CCI shock, income acceleration, unemployment shock, income volatility).
- **Spec 6** — disaggregated wealth with a post-2008 break in the permanent-income coefficient. The **conventional baseline** (generic constant-MPC wealth ECM; no CCI interaction channels).
- **Spec 6b** — Spec 6 form on the back-extended 1976Q3+ sample with the long-history short-run CCI term.
- **Spec 7** — Spec 6 + cohort terms (prime-age share, FHB share) and synthetic mortgage burden.
- **Spec 7b** — Spec 7 with the RBA E13 measured mortgage payment burden over the post-2009 sample.
- **Spec 8** — Williams CCI interactions added on top of the level-wealth terms, on the maximal-GETS basis (15-knot candidate set → 4 surviving knots via iterated Hendry-Krolzig sign-prior reduction: 2007Q3, 2009Q1, 2019Q1, 2020Q2).
- **Spec 9** — Spec 8 with the Kalman state-space CCI replacing the Williams smoothed-step spline.
- **Spec 10** — Williams-prior calibrated specification (γ_IFA = 0.022, ψ_0 = 0.20, ψ_1 = 0.93, ϖ = 1.2; iterative fixed-point OLS).
- **Spec 11** — **the headline**: the faithful LIVES consumption equation (Williams Eq 7). Housing wealth enters *only* through the CCI interaction (no standalone `ha_y` level — the theory says its MPC is zero at CCI = 0), illiquid financial assets are combined (`ilfa_y = eq_y + super_y`), and the full six-channel CCI mechanism is the core long run.
- **Spec 12** — Spec 11's equation form with Williams' Table 1 calibrations imposed on the interaction block (the feasible response to the interaction-block collinearity); λ collapses to −0.029, the key negative result.

The canonical permanent-income method is `PI_METHOD = "italy"` — the De
Bonis et al. (2020) direct forecast: the discounted weighted average of
realised future log income is regressed on time-t predictors (including
the labour-force-share predictor) in a single full-sample regression.
The historical rolling-AR(8) forecaster is retained as a robustness
column, with a real `gfc_ogive` on/off toggle.

## Outputs

Written to [Australia/outputs/](Australia/outputs/).

Per-spec coefficient tables:
[australia_full_results.csv](Australia/outputs/australia_full_results.csv),
[australia_precovid_results.csv](Australia/outputs/australia_precovid_results.csv),
[australia_all_results.csv](Australia/outputs/australia_all_results.csv).

Spec 11 headline detail:
[australia_spec11_variants.csv](Australia/outputs/australia_spec11_variants.csv)
(full coefficient vector × 4 sample variants),
[australia_spec11_ogive_robustness.csv](Australia/outputs/australia_spec11_ogive_robustness.csv)
(no-ogive PI variant: λ = −0.574),
[australia_gamma_inference.csv](Australia/outputs/australia_gamma_inference.csv)
(delta-method + seeded block-bootstrap CIs on structural γ, Spec 6 and Spec 11),
[australia_longrun_contributions_spec11.csv](Australia/outputs/australia_longrun_contributions_spec11.csv)
+ [australia_longrun_decomposition_spec11.png](Australia/outputs/australia_longrun_decomposition_spec11.png).

CCI construction:
[australia_cci_williams_series.csv](Australia/outputs/australia_cci_williams_series.csv)
(the deployed CCI path and its interaction regressors),
[australia_cci_williams_path.png](Australia/outputs/australia_cci_williams_path.png),
[australia_cci_interaction_corr.csv](Australia/outputs/australia_cci_interaction_corr.csv)
(interaction correlation matrix, |ρ| 0.66–0.97),
[australia_williams_cci_knots.csv](Australia/outputs/australia_williams_cci_knots.csv).

Placebo battery:
[australia_williams_knot_placebo.csv](Australia/outputs/australia_williams_knot_placebo.csv)
+ verdict (literal Williams 4-knot: 45th R² percentile — at the random-knot
median, the detrending critique),
[australia_williams_knot_placebo_deployed.csv](Australia/outputs/australia_williams_knot_placebo_deployed.csv)
+ [_verdict.csv](Australia/outputs/australia_williams_knot_placebo_deployed_verdict.csv)
(deployed 15-knot iterated protocol: 84th R² percentile — moderate support).

Diagnostics and robustness:
[australia_full_diagnostics.csv](Australia/outputs/australia_full_diagnostics.csv),
[australia_precovid_diagnostics.csv](Australia/outputs/australia_precovid_diagnostics.csv),
[australia_iv_diagnostics.csv](Australia/outputs/australia_iv_diagnostics.csv) /
[australia_iv_diagnostics_spec11.csv](Australia/outputs/australia_iv_diagnostics_spec11.csv)
(first-stage F, Wu-Hausman, Sargan), and `*_spec11.csv` variants of the
IV / joint-PI / Chow / scaled-income / Williams-income / Drehmann
robustness suite (the Italy-style suite runs on both the
selector-preferred spec and Spec 11).

Italy–Australia comparison:
[italy_australia_comparison.csv](Australia/outputs/italy_australia_comparison.csv),
[italy_australia_lambda.csv](Australia/outputs/italy_australia_lambda.csv).

Data coverage / cached dataset:
[australia_model_dataset.csv](Australia/outputs/australia_model_dataset.csv),
[australia_model_dataset.rds](Australia/outputs/australia_model_dataset.rds).

Plots (Spec 1 baseline, Spec 6 conventional baseline):
[australia_spec1_lognetworth_actual_vs_fitted.png](Australia/outputs/australia_spec1_lognetworth_actual_vs_fitted.png),
[australia_spec1_lognetworth_residuals.png](Australia/outputs/australia_spec1_lognetworth_residuals.png),
[australia_spec6_preferred_actual_vs_fitted.png](Australia/outputs/australia_spec6_preferred_actual_vs_fitted.png),
[australia_spec6_preferred_residuals.png](Australia/outputs/australia_spec6_preferred_residuals.png).

Additional outputs (produced by every run):
[australia_model_summary.md](Australia/outputs/australia_model_summary.md),
[australia_cointegration.csv](Australia/outputs/australia_cointegration.csv),
[australia_spec_selection.csv](Australia/outputs/australia_spec_selection.csv),
[australia_williams_comparison.csv](Australia/outputs/australia_williams_comparison.csv),
[australia_cci_fit_decomposition.md](Australia/outputs/australia_cci_fit_decomposition.md),
[australia_oos_rmse.csv](Australia/outputs/australia_oos_rmse.csv),
[australia_oos_forecast_paths.png](Australia/outputs/australia_oos_forecast_paths.png),
[australia_martin_nesting.csv](Australia/outputs/australia_martin_nesting.csv).

## Data sources

All series are public.

- ABS 5206008 — Household Final Consumption Expenditure (chain volume + current price).
- ABS 5206020 — Household Income Account (gross disposable income, mortgage interest, wages).
- ABS 5232035 — Household Balance Sheet (financial assets, liabilities, residential land and dwellings). Binding sample start: 1988Q3.
- ABS 641601 — Residential Property Price Indexes (bridge layer, 2003Q3–2021Q4).
- ABS 643201 — Total Value of Dwellings (mean price; current layer, 2011Q3+).
- ABS 560101 — Lending Indicators (housing credit flow, FHB share).
- ABS 6202001 — Labour Force (unemployment rate, labour force).
- ABS 3101059 — Estimated Resident Population (per-capita deflation, cohort shares).
- RBA F6 (`data_raw/rba_filrhlbvs.csv`) — Standard Variable Mortgage Rate (1959+, cached locally).
- RBA D01/D02/D03 historical tables — total credit and M3 (back-extension to 1976Q3).
- Treasury TRYM (`data_raw/house_price_history_long.csv`) — historical house prices from 1959Q3.
- `data_raw/houseprice_old.csv` — legacy ABS RPPI layer (1986Q2–2005Q2).
- `data_raw/labour_force_historic.csv` — pre-1978 labour force / population.

## Current limitations

- **CCI identification is concentrated post-2007.** The deployed CCI's
  four surviving knots are 2007Q3, 2009Q1, 2019Q1 and 2020Q2, so the
  series is exactly zero before 2007Q3, plateaus at 1 over 2010–18, and
  sits at ≈ −1.6 after 2022 — roughly 70 quarters of identifying
  variation in a 146-quarter sample. The literal Williams 4-knot CCI
  sits at the random-knot placebo median (45th R² percentile — the
  detrending critique); the deployed iterated 15-knot protocol does
  better (84th percentile, moderate support) but is not decisive.
- **The structural ψ exceeds its theoretical bound.** The implied
  permanent-income gearing ψ ≈ 1.02–1.13 across Spec 11 variants,
  above the 0.95 ceiling implied by the discount structure. Disclosed
  as a calibration tension, not resolved.
- **No specification passes Engle-Granger cointegration.** ADF and
  Phillips-Ouliaris on the long-run residual fail in every spec
  (Johansen rank tests are more favourable); the spec selector
  therefore falls back to most-screens-passed and picks Spec 3, while
  the BIC-best specification is Spec 11 (−954.8). The paper's headline
  rests on Spec 11's economic and statistical coherence, not on the
  selector.
- The estimation sample begins 1988Q3, bound by ABS 5232035 (household
  balance sheet); back-extension proxies from 1976Q3 support Spec 6b
  and the extended placebo battery.
- Structural-γ inference (Spec 11, delta method and block bootstrap):
  the NLA interval [0.022, 0.098] *excludes* Williams' 0.159; the IFA
  interval [0.012, 0.057] *includes* his 0.022. The housing comparison
  is against Williams' peak MPC (0.0488), which our interval excludes.
- Wealth-to-income ratios enter contemporaneously; the timing
  convention leaves simultaneity to the IV robustness block (which
  instruments `ecm_lag` and `ln_yp_over_y` with lagged income,
  unemployment and mortgage-rate instruments; first-stage F ≈ 35–72).
- Heteroskedasticity is structural in every full-sample spec.
  Newey-West HAC standard errors are reported.
- Out-of-sample forecasting beats random-walk-with-drift at h = 1 only
  (Spec 8 and Spec 11 RMSE 0.0290/0.0292 vs RW 0.0309); at h = 4 and
  h = 8 the random walk dominates — a standard "macro forecasting
  puzzle" finding the WP records honestly.
- The canonical permanent-income series (`PI_METHOD = "italy"`) is a
  full-sample, two-sided *measurement* (in-sample fitted values of one
  full-sample regression), not a real-time forecast; its positive
  long-run PI coefficient and λ magnitude are full-sample properties. A
  causal expanding-window variant
  (`construct_permanent_income_italy(..., real_time = TRUE)`) is
  reported as a robustness column
  ([australia_pi_realtime_robustness.csv](Australia/outputs/australia_pi_realtime_robustness.csv))
  and is the version any forecasting use (e.g. MARTIN) must adopt;
  under it the long-run PI coefficient is modestly negative and
  λ ≈ −0.16 (Spec 6).

## Scope — single-equation headline, multi-equation companion

The headline paper is the consumption equation alone, framed as "an
Australian counterpart to the consumption equation in the
Muellbauer/Williams LIVES system". A companion multi-equation scaffold
lives under [`LIVES/`](LIVES/): data prep, joint 4-equation CCI
identification, 2-/3-/4-equation SUR systems (consumption + house
prices + mortgage stock + HEW), and a Wald test of Williams' Table 1
calibrations (χ²(6) = 7.55, p = 0.27 — not rejected). The SUR residual
correlation between the consumption and house-price equations is
negligible (ρ̂ ≈ −0.013), so joint estimation buys no efficiency at the
quarterly frequency; the case for the full LIVES system rests on
cross-equation parameter restrictions. A full LIVES port (cross-equation
long-run restrictions, properly constructed HEW with the
dwelling-investment subtraction) remains a multi-month effort and is
tracked in
[`LIVES/docs/multi_equation_plan.md`](LIVES/docs/multi_equation_plan.md).
