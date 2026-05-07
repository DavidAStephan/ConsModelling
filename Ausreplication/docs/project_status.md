# Australia ECM — Project status

A snapshot of where the model stands, what its preferred specification says,
and what's still on the table. Companion to
[`data.md`](data.md) (data sources) and the top-level
[`README.md`](../../README.md) (entry points and limitations).

Sample window: **1980Q1–2024Q4** (n=180 quarters), with the binding
sample start for disaggregated wealth specifications at **1988Q3** (ABS
5232035 household balance sheet).

---

## 1. Model

Single-equation error-correction model in the Muellbauer/LIVES tradition:

$$
\Delta \ln c_t = \lambda \left[ \alpha_0 + \sum_i \gamma_i \frac{A_{i,t-1}}{y_t} + \alpha_r r_t + \varphi \ln(y^p_t / y_t) + \mathrm{ecm\_lag}_t \right] + \text{short-run} + \varepsilon_t
$$

where `ecm_lag = ln c_{t-1} - ln y_t` (canonical Engle-Granger negative-
restoration convention; λ < 0 means restoring force).

Estimated by OLS with **Newey-West HAC standard errors**. Eight specifications
nested by wealth aggregation, credit-conditions treatment, and demographic
additions.

---

## 2. Specifications

| Spec | Description | Long-run regressors |
|------|-------------|---------------------|
| 1 | Aggregate net worth | `ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 2 | Spec 1 + Δ²log(CCI) lag-2 SR term | (same long-run as 1) |
| 3 | Net worth in levels | `networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 4 | Disaggregated wealth | `nla_y, eq_y, super_y, ha_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 5 | Spec 4 + full SR dynamics | (same long-run as 4) |
| 6 | Spec 5 + post-2008 break in φ | adds `ln_yp_over_y_post2008` |
| 7 | Spec 6 + cohort + synthetic burden | adds `prime_age_share, fhb_share`; replaces `real_rate` with synthetic `mortgage_burden` |
| 7b | Spec 6 + cohort + RBA E13 burden | as Spec 7 but uses `mortgage_payment_burden_rba` (post-2009 sample) |
| 8 | Williams CCI interactions (maximal-GETS) | `nla_y, eq_y, super_y, ha_y, hp_x_1_minus_cci, r_x_cci, ln_yp_over_y, yp_x_cci, ecm_lag` |
| 9 | Kalman state-space CCI interactions | as Spec 8 but with `cci_kalman` (single-factor SSM) replacing the Williams smoothed-step spline |
| 10 | Williams-prior calibrated | impose γ_IFA=0.022, ψ_0=0.20, ψ_1=0.93, ϖ=1.2; iterative fixed-point OLS |

**Nine narrative dummies** in the default set: `d2000_gst`, `d2008_gfc`,
`d2020_covid`, `d2020_rebound`, `d_neg_gearing_8587`, `d_recession_1991`,
`d_apra_2014`, `d_apra_2017`, `d_jobkeeper_2020`. Zero-variance dummies
are silently dropped per spec/sample.

---

## 3. Preferred specification — auto-selected

Implemented in `select_preferred_spec()`
([estimation.R, Section G2](../R/australia_estimation.R)). Each spec scored
on four screens:

1. **Sign screen** — every signed long-run coefficient has the right sign per
   the a-priori lookup
2. **Cointegration screen** — ADF on long-run residuals rejects unit root at 5%
   ([Section F0](../R/australia_estimation.R), Engle-Granger-style)
3. **λ screen** — sign of λ matches expected (negative) AND `|λ| ∈ (0.02, 0.30)`
4. **Stability screen** — Chow at 2008Q3 not rejected at 1% AND λ sign-stable
   across full / pre-COVID / COVID-dropped / COVID-rich-dummies samples

BIC tiebreaker. Under canonical `PI_METHOD = "italy"` (set in
[`australia_estimation.R`](../R/australia_estimation.R) line 45), no
spec passes all four screens; the selector falls back to the
most-passes / BIC tiebreak rule and returns Spec 3 as the
auto-preferred:

| Spec | Signs | Coint | λ | Stability | BIC | Auto-preferred |
|------|------|------|---|-----------|------|-----------|
| 1 | ✗ | ✗ | ✓ | ✗ | -923 | |
| 2 | ✗ | ✗ | ✓ | ✗ | -504 | |
| **3** | **✓** | ✗ | **✓** | ✗ | **-923** | **✓** *(BIC-best of 2-pass)* |
| 4 | ✗ | ✓ | ✓ | ✗ | -909 | |
| 5 | ✗ | ✓ | ✓ | ✗ | -498 | |
| 6 | ✗ | ✓ | ✓ | ✗ | -496 | |
| 7 | ✗ | ✓ | ✗ | ✓ | -501 | |
| 7b | ✗ | ✓ | ✗ | ✗ | -363 | |
| 8 | ✗ | NA | ✓ | ✗ | -911 | |
| 9 | ✗ | NA | ✓ | ✗ | -900 | |
| 10 | ✗ | NA | ✓ | ✓ | -492 | |

**Methodologically the disaggregated, Williams-form Spec 6 remains the
narrative headline spec** for the WP, because (a) it is the form
Williams (2010) and the LIVES tradition use, (b) it permits the
γ_LA + γ_LOANS = 0 cross-equation restriction test, and (c) the
sign-screen failure (eq_y small-negative under Italy LP) is a known
identification effect that disappears once CCI interactions are added
(Spec 8: eq_y = +0.036, t = 2.11). Spec 3 is the BIC-best 2-pass
alternative reported alongside.

Why the rubric tightens under Italy LP: because |λ| is roughly four
times larger under Italy LP than under AR, the implied long-run γ on
each disaggregated wealth term is correspondingly compressed, so a
modest negative coefficient on (e.g.) eq_y is no longer crowded out by
larger positives. Spec 3 (aggregated `networth_y`) avoids this by
lumping all wealth into a single positive coefficient.

---

## 4. Output catalogue

Everything in [`Ausreplication/outputs/`](../outputs/):

### Core results
- `australia_full_results.csv` / `australia_precovid_results.csv` —
  per-spec coefficient tables with sign-screen flagging (`expected_sign`,
  `sign_ok`, `signif_5pct`, `coef_label`)
- `australia_full_diagnostics.csv` / `australia_precovid_diagnostics.csv` —
  N, adj R², DW, AR(1), AR(4), Chow, RESET, BIC, log-likelihood,
  heteroskedasticity (with event-removed re-test and `het_diagnosis` field)
- `australia_all_results.csv` / `australia_all_diagnostics.csv` —
  combined full + precovid, easier for cross-period comparison

### Selection and screens
- `australia_spec_selection.csv` — pass/fail per screen, `is_preferred` flag
- `australia_cointegration.csv` — ADF + Phillips-Ouliaris + Johansen per spec
- `australia_breaks.csv` — supF / breakpoints / CUSUM for the preferred spec
- `australia_lambda_robustness.csv` — λ across 4 sample variants per spec
- `australia_wls_robustness.csv` — OLS-NW vs WLS coefficient comparison

### Italy-style robustness suite
- `australia_iv_robustness.csv` — OLS vs IV (current income instrumented)
- `australia_joint_pi_robustness.csv` — single-equation vs SUR joint with PI
- `australia_chow_battery.csv` — Chow at 1995/2000/2008/2020Q1
- `australia_drehmann_robustness.csv` — flat real rate vs amortising-mortgage adjusted
- `australia_scaled_income_robustness.csv` — placeholder (skipped — see
  [data.md §2.2](data.md) for the input gap that needs closing)

### Substantive tests
- `australia_nla_restriction_test.csv` — Wald test of γ_LA + γ_LOANS = 0
  (Italy's restriction). **Accepted in every spec/sample** (p > 0.05).
- `australia_williams_cci_knots.csv` — survival of the 4 Williams knots
  after sign-prior reduction. Currently 1979 aliased, 1992 violator,
  1998 +ve, 2007 -ve.
- `australia_spec8_sign_prior_verdicts.csv` — Spec 8 CCI-interaction terms
  vs paper's institutional sign priors. Only `hp_x_1_minus_cci` PASSES at
  5%; others uninformative on the truncated sample.
- `australia_permanent_income_sensitivity.csv` — λ and ψ sensitivity to
  18 PI-filter variants

### Headline charts
- `australia_longrun_decomposition.png` + `_contributions.csv` —
  **policy-facing chart**: stacked-area de-meaned contributions of each
  long-run regressor to log(c/y) for the preferred spec
- `australia_rolling_coefs.png` + `.csv` — 60-quarter rolling coefficient
  paths for the tracked terms in the preferred spec
- `australia_recursive_coefficients_*.png` — recursive estimates (CUSUM-style)
- `australia_spec6_preferred_actual_vs_fitted.png` and `_residuals.png` —
  preferred-spec fit
- `australia_spec1_lognetworth_actual_vs_fitted.png` and `_residuals.png` —
  baseline aggregate-net-worth fit, for comparison

### Cross-country comparison
- `italy_australia_comparison.csv` — common structural parameters
- `italy_australia_lambda.csv` — λ in each country's native convention with
  explicit `lambda_source` and `note` columns (the previous −0.0099 stale-
  file bug is fixed)
- `australia_williams_comparison.csv` + `.md` — side-by-side vs Williams
  (2010) WP 492 / Muellbauer-Williams (2012) Table 1. Both forms (γ and
  implied OLS) so the reader can disentangle "OLS coefficient differs"
  from "λ differs". **Headline finding:** OLS wealth coefficients agree
  with Williams to within 6-17% (ha_y +6%, nla_y -11%, ln_hp_over_y -17%);
  the 5x divergence in implied γ is almost entirely a λ story.
- `australia_williams_spec8_comparison.csv` — Spec 8 CCI-interaction
  comparison (only 2 of 4 Williams knots identify on our 1988+ sample).

### Narrative
- `australia_model_summary.md` — auto-generated markdown summary

### Underlying data
- `australia_model_dataset.csv` — coverage table (variable, n_obs, date_from,
  date_to)
- `australia_model_dataset.rds` — the `master` tibble itself, used by
  `run_estimation_from_rds.R`. **Note:** currently somewhat stale relative to
  the latest source code — see [data.md §7](data.md).

---

## 5. Headline statistical findings (latest run, canonical Italy LP)

1. **Spec 6 λ = −0.218 (NW SE 0.098), within 25% of Williams' −0.286.**
   Under canonical Italy LP. Under the AR robustness column λ = −0.052
   — that is the historical "Australian PI puzzle" specification, which
   we now treat as a methodology artefact.

2. **`ln_yp_over_y` = +0.302 under canonical Italy LP**, matching
   Williams' calibrated value (0.20) in sign and broad magnitude. Under
   AR it is −0.20. The Australian PI puzzle resolved.

3. **Implied long-run wealth γ undershoots Williams' published values
   by ~4×** under canonical Italy LP (e.g. ha_y γ = 0.028 vs Williams'
   0.0488; nla_y γ = 0.040 vs Williams' 0.159). Attributed to truncated
   CCI variation on the post-deregulation 1988Q4+ sample; would resolve
   under sample back-extension to ~1975 (NS-020).

4. **NLA cross-equation restriction γ_LA + γ_LOANS = 0 is accepted
   across the board** — every spec × sample combination has
   `restriction_accepted = TRUE` in
   [`australia_nla_restriction_test.csv`](../outputs/australia_nla_restriction_test.csv),
   p-values 0.27–0.79. Validates the Italian netting convention.

5. **λ sign-stable across all 4 sample variants for Spec 6** (full
   −0.218, pre-COVID −0.213, COVID-dropped −0.139, COVID-rich-dummies
   −0.173). The sign-stability screen passes; it is the Chow stability
   sub-criterion that fails. Spec 7 (cohort) is even tighter
   (range −0.20 to −0.37).

6. **Williams 4-knot CCI is at the placebo distribution median on the
   1988Q4+ sample** (placebo test in
   [`australia_williams_knot_placebo.png`](../outputs/australia_williams_knot_placebo.png)):
   Williams' canonical 4-knot adj-R² = 0.7268 sits at the 49th
   percentile of 200 random 4-knot draws. The maximal-GETS reduction
   (15 → 6 knots) is the methodologically defensible response and is
   the canonical CCI basis (`build_williams_cci_basis()`).

7. **Adding CCI does identification work, not detrending.** The fit
   decomposition ([`australia_cci_fit_decomposition.md`](../outputs/australia_cci_fit_decomposition.md))
   shows the Williams maximal-GETS CCI shifts wealth coefficients by
   150.7% on average between Spec 6 (no CCI) and Spec 8 (CCI
   interactions); the Kalman state-space CCI shifts them by 16.6%. R²
   actually drops slightly when CCI is added (Spec 8 adj R² 0.763 vs
   Spec 6's 0.812), so the CCI is not residual-absorbing.

8. **Out-of-sample forecasting**: at h = 1, structural specs match
   random-walk-with-drift (Spec 7 best at RMSE 0.0306; RW-drift 0.0310).
   At h ∈ {4, 8}, RW-drift dominates every structural spec by 5–15% in
   RMSE — a standard "macro forecasting puzzle" finding. See
   [`australia_oos_rmse.csv`](../outputs/australia_oos_rmse.csv).

9. **OLS ≈ IV** on the preferred spec under Italy LP — Italy's "a
   whisker away" finding (Italy.pdf p.32) replicates qualitatively. The
   joint SUR block currently fails on a CHOLMOD singular-matrix error
   under the new sample (the consumption + PI joint system has high
   collinearity); not a binding gap for the WP narrative.

10. **Heteroskedasticity is structural** (`het_diagnosis = "structural"`
    for every spec on the full sample) — Newey-West HAC is the right
    correction; WLS not needed.

---

## 6. Open items — what's left

The full backlog with stable IDs (NS-001 … NS-114) is in
[`next_steps.md`](next_steps.md). Highlights:

### Items requiring fresh data sourcing
- **Sample back-extension to ~1975 (NS-020)** — biggest empirical
  unlock. Needs pre-1988 ABS Financial Accounts annual data +
  Bonci-Coletta splicing. Would resolve the truncated-CCI identification
  problem and likely close the implied-γ gap with Williams. Days of work
  + careful unit reconciliation.
- **Document `houseprice_old.csv` provenance (NS-003)** — the splicing
  chain's earliest layer has no recorded source URL or vintage.
  ~15 min.
- **APRA / RBA / Treasury sourcing follow-ups** — see NS-021, NS-030,
  NS-107, NS-114.

### Items that need user judgement
- **LIVES extension scope (NS-101)** — single-equation, two-equation, or
  full LIVES port. Multi-equation work is multi-week to multi-month.
- **Counterfactuals (NS-012)** — three suggested for §10: no-APRA,
  no-JobKeeper, CCI-at-Williams-peak.
- **WP framing decisions (NS-102, 103, 104)** — target journal, BIS
  Shrapnel sourcing, companion paper structure.

### WP drafting items
- **§1 Introduction (NS-010)** and **§11 Conclusion (NS-011)** — currently
  skeleton; need full prose.
- **Auto-fill table placeholders (NS-001)** — splice
  `[TABLE-FROM-DATA: ...]` markers in §7-§9 from the relevant CSVs.
- **Verify [VERIFY] citation tags in lit review (NS-002)** — ~9 tags.
- **Quarto rendering pipeline (NS-013, NS-014)**.

### Recently completed (struck through in `next_steps.md`)
- Italy-style PI helper — landed; canonical method as of 2026-05-07.
- Williams maximal-GETS CCI knot identification — landed.
- Kalman state-space CCI extraction — landed (Spec 9).
- Spec 7b RBA E13 burden — landed.
- Spec 10 Williams-prior calibrated — landed.
- OOS forecast validation — landed (NS-033).
- Williams CCI placebo test, knot experiment, fit decomposition — landed.

---

## 7. Reproducibility status

- **Pipeline runs end-to-end** on cached RDS:
  `Rscript Ausreplication/R/run_estimation_from_rds.R`
- **All 22 testthat tests pass** (no skips):
  `Rscript tests/testthat.R`
- **`renv.lock` pins 107 packages** at R 4.5.3
- **CI workflow** at `.github/workflows/check.yml` parses every R file,
  runs the cached-RDS pipeline, runs tests
- **16 unpushed commits** on `main` (as of last reconciliation; check
  `git status`)

The pipeline is **not** quick to rebuild from scratch (`australia_consumption_model.R`
takes a few minutes due to RBA fetches and ABS workbook re-parsing on a cold
cache); the fast path through `run_estimation_from_rds.R` is ~30 seconds.

---

## 8. Where to look next

If you're picking this up cold:
1. Read [README.md](../../README.md) for the elevator pitch and limitations.
2. Read this file (project status) for what the model says.
3. Read [data.md](data.md) for what the inputs are and where they're flaky.
4. Look at [`outputs/australia_model_summary.md`](../outputs/australia_model_summary.md)
   for the auto-generated narrative of the latest run.
5. Look at [`outputs/australia_longrun_decomposition.png`](../outputs/australia_longrun_decomposition.png)
   for the headline policy chart.

The implementation is in:
- [`R/australia_consumption_model.R`](../R/australia_consumption_model.R) — orchestrator
- [`R/australia_data_download.R`](../R/australia_data_download.R) — Part 1
- [`R/australia_estimation.R`](../R/australia_estimation.R) — Part 2 (~3000 lines)
- [`R/model_helpers.R`](../R/model_helpers.R) — shared utilities
