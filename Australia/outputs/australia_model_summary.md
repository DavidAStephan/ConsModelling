# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1988Q3–2026Q1, n=151
- Pre-COVID sample: 1988Q3–2019Q4, n=126
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec3_LevelNetWorth** (selected by automated screen). Reason: signs=TRUE, coint=FALSE, λ=TRUE, stability=TRUE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| networth_y | 0.0012 | 0.43 | 0.671 | + | yes |
| ln_hp_over_y | 0.0047 | 0.28 | 0.780 | +/- | — |
| real_rate | -0.0002 | -0.59 | 0.558 | - | yes |
| ln_yp_over_y | 0.1945 | 3.59 | 0.000 | +/- | — |
| ecm_lag | -0.1894 | -2.99 | 0.003 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.732 | 2.32 | N | Y | N | Y | N | -957.7 | Y | N | Y | Y |
| Spec2_LogNetWorth_CCI | 0.773 | 2.44 | N | Y | N | Y | N | -538.7 | N | N | Y | Y |
| Spec3_LevelNetWorth | 0.732 | 2.32 | N | Y | N | Y | N | -957.9 | Y | N | Y | Y |
| Spec4_Disagg_NoCCI | 0.727 | 2.32 | N | Y | N | Y | N | -943.5 | N | N | Y | Y |
| Spec5_FullDisagg | 0.803 | 2.28 | Y | N | N | N | N | -531.9 | N | N | Y | Y |
| Spec6_Preferred | 0.807 | 2.17 | Y | Y | N | Y | N | -530.1 | N | N | Y | Y |
| Spec6b_LongHistSRCCI | 0.704 | 2.13 | Y | Y | N | N | N | -1152.3 | Y | N | Y | N |
| Spec7_CohortBurden | 0.834 | 2.23 | Y | N | N | Y | N | -537.8 | N | N | N | Y |
| Spec7b_RBABurden | 0.875 | 2.18 | Y | N | N | — | Y | -387.9 | N | N | N | Y |
| Spec8_CCI_Interactions | 0.817 | 1.97 | Y | N | N | N | N | -984.4 | N | N | N | Y |
| Spec9_KalmanCCI | 0.735 | 2.18 | Y | N | N | Y | N | -928.5 | N | — | Y | Y |
| Spec10_WilliamsPrior | 0.773 | 2.22 | Y | Y | N | Y | Y | -529.5 | Y | — | Y | N |
| Spec11_LIVES_Headline | 0.816 | 1.94 | Y | N | N | N | N | -987.3 | Y | N | N | Y |
| Spec12_LIVES_Calibrated | 0.687 | 2.14 | Y | N | N | N | N | -931.0 | Y | — | Y | N |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.1923 | -0.1608 | yes |
| Spec2_LogNetWorth_CCI | -0.1809 | -0.0799 | yes |
| Spec3_LevelNetWorth | -0.1894 | -0.1578 | yes |
| Spec4_Disagg_NoCCI | -0.1807 | -0.1188 | yes |
| Spec5_FullDisagg | -0.2251 | -0.0613 | yes |
| Spec6_Preferred | -0.2325 | -0.0859 | yes |
| Spec6b_LongHistSRCCI | -0.2462 | -0.2389 | yes |
| Spec7_CohortBurden | -0.3300 | -0.0548 | yes |
| Spec7b_RBABurden | -0.3768 | -0.0606 | yes |
| Spec8_CCI_Interactions | -0.4265 | -0.2182 | yes |
| Spec9_KalmanCCI | -0.2099 | -0.1572 | yes |
| Spec10_WilliamsPrior | -0.0432 | -0.0262 | yes |
| Spec11_LIVES_Headline | -0.4231 | -0.2651 | yes |
| Spec12_LIVES_Calibrated | -0.0311 | 0.0407 | no |

## Italy vs Australia (preferred specs)

_Italy comparison not available._

## Known issues
- Spec1_LogNetWorth fails: cointegration
- Spec2_LogNetWorth_CCI fails: sign, cointegration
- Spec3_LevelNetWorth fails: cointegration
- Spec4_Disagg_NoCCI fails: sign, cointegration
- Spec5_FullDisagg fails: sign, cointegration
- Spec6_Preferred fails: sign, cointegration
- Spec6b_LongHistSRCCI fails: cointegration, stability
- Spec7_CohortBurden fails: sign, cointegration, λ range/sign
- Spec7b_RBABurden fails: sign, cointegration, λ range/sign
- Spec8_CCI_Interactions fails: sign, cointegration, λ range/sign
- Spec9_KalmanCCI fails: sign
- Spec10_WilliamsPrior fails: stability
- Spec11_LIVES_Headline fails: cointegration, λ range/sign
- Spec12_LIVES_Calibrated fails: stability
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Australia/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Australia/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-07-26

