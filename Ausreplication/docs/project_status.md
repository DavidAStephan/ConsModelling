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
| 7 | Spec 6 + life-cycle terms | adds `prime_age_share, fhb_share` (mortgage_burden dropped — collinearity) |
| 8 | CCI interactions per Aust paper eq 7 | `nla_y, eq_y, super_y, ha_y, hp_x_1_minus_cci, r_x_cci, ln_yp_over_y, yp_x_cci, ecm_lag` (only when `USE_INSTITUTIONAL_CCI = TRUE`) |

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

BIC tiebreaker. As of latest run:

| Spec | Signs | Coint | λ | Stability | BIC | Preferred |
|------|------|------|---|-----------|------|-----------|
| 1 | ✓ | ✗ | ✓ | ✗ | -824 | |
| 2 | ✓ | ✗ | ✓ | ✓ | -496 | |
| 3 | ✓ | ✗ | ✓ | ✗ | -825 | |
| 4 | ✓ | ✓ | ✓ | ✗ | -812 | |
| 5 | ✗ | ✓ | ✓ | ✗ | -489 | |
| **6** | ✓ | ✓ | ✓ | ✓ | -491 | **✓** |
| 7 | ✗ | ✓ | ✓ | ✓ | -496 | |
| 8 | ✗ | NA | ✓ | ✗ | -805 | |

**Spec 6 is the auto-preferred** — the only spec that passes all four screens.
Spec 7 has the strongest λ stability but its `nla_y` is small-negative due to
collinearity with `prime_age_share` (see [data.md §10](data.md) and the
Step A investigation in commit `3dbc18c`).

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

## 5. Headline statistical findings (latest run)

1. **NLA cross-equation restriction (Italy's `γ_LA + γ_LOANS = 0`) is
   accepted across the board** — every spec/sample combination has
   `restriction_accepted = TRUE` in
   [`australia_nla_restriction_test.csv`](../outputs/australia_nla_restriction_test.csv),
   p-values 0.27–0.79. This validates the netting choice for `nla_y`.

2. **The `nla_y` wrong-sign bug is gone in Specs 4–6** but resurfaces in
   Spec 7 because `prime_age_share` (coef ~+5) absorbs wealth-effect
   variance in the post-1988 sample. Demographics-vs-wealth collinearity
   over the post-deregulation period is real, not a fixable bug.

3. **λ stability across 4 sample variants** (full / pre-COVID /
   COVID-dropped / COVID-rich-dummies):
   - Spec 6 (preferred): −0.046 to −0.121 (factor ~3 spread)
   - Spec 7 (cohort): −0.21 to −0.25 (very tight; the most stable)
   The selector chose Spec 6 over Spec 7 because Spec 7 fails the sign
   screen on `nla_y`.

4. **Williams 4-knot CCI partially identifies on 1988Q4+ data**:
   1998 (NBFI/securitisation, +0.0015) and 2007 (GFC, −0.017) survive with
   correct signs; 1992 (banking distress) is a sign-violator (dropped); 1979
   (deregulation) is constant in our window and drops by collinearity. This
   is the expected outcome — the paper's full identification needs
   pre-1980 data.

5. **Italy comparator λ corrected to +0.519** (positive convention; was
   reported as −0.0099 before the cross-country comparison was reading
   from a stale file at the project root).

6. **OLS ≈ IV ≈ Joint-SUR** on the preferred spec. Italy's
   "the parameter estimates and even the standard errors are only a
   whisker away" finding (Italy.pdf p.32) replicates for Australia.

7. **Heteroskedasticity is event-driven** (`het_diagnosis = "structural"`
   for some specs, "event_driven" for others) — Newey-West HAC is the
   right correction; no need for WLS.

8. **Institutional CCI flag is OFF by default** (`USE_INSTITUTIONAL_CCI =
   FALSE` in `australia_data_download.R:70`). Set to `TRUE` to switch from
   the post-2002 housing-flow proxy to the Williams-2010 4-knot SDMMA
   spline + institutional overlay. See [data.md §5.2](data.md).

---

## 6. Open items — what's left

### Items deferred (work attempted but not landed)
- **Italy-style PI helper** — Agent C wrote a `construct_permanent_income_italy()`
  + `PI_METHOD` flag + `compare_pi_methods()` function set in a parallel
  worktree, but the worktree was based on a stale `main` so a full merge
  was infeasible. Only the bug fix and labour-force series were cherry-
  picked. The labour-force data is now in place; the helper itself can be
  rebuilt against current `main` in ~2–3 hours.
- **Spec 7 promotion** — Spec 7's λ-stability story is compelling but
  blocked by `nla_y` sign failure. Possible escape: add a
  `prime_age_share × nla_y` interaction so demographics scale wealth
  rather than competing with it, but that is exploratory work, not a
  defensible default.

### Items that need user judgement
- **CCI strategy** (currently default Path A — flow-only; Path B
  Williams-2010 spline available behind `USE_INSTITUTIONAL_CCI`). Three
  options were laid out in
  [README's "Scoping decision required"](../../README.md#scoping-decision-required--lives-extension)
  section — pick one explicitly so the project has a stable CCI baseline.
- **LIVES extension scope** — single-equation, two-equation, or full LIVES
  port. Same README section. Multi-equation work is multi-week to
  multi-month.

### Items requiring fresh data sourcing
- **Sample back-extension to 1980** — needs pre-1988 ABS Financial
  Accounts annual data + Bonci-Coletta splicing. Days of work + careful
  unit reconciliation.
- **Compensation of employees and social benefits** from ABS 5206020 —
  unblocks Italy-style scaled-income robustness check. ~30 min of
  data-download additions.
- **Document `houseprice_old.csv` provenance** — the splicing chain's
  earliest layer has no recorded source URL or vintage. ~15 min.

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
