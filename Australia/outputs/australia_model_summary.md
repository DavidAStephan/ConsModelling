# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1988Q3–2024Q4, n=146
- Pre-COVID sample: 1988Q3–2019Q4, n=126
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec3_LevelNetWorth** (selected by automated screen). Reason: signs=TRUE, coint=FALSE, λ=TRUE, stability=TRUE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| networth_y | 0.0011 | 0.38 | 0.708 | + | yes |
| ln_hp_over_y | 0.0057 | 0.32 | 0.750 | +/- | — |
| real_rate | -0.0002 | -0.52 | 0.604 | - | yes |
| ln_yp_over_y | 0.1963 | 3.41 | 0.001 | +/- | — |
| ecm_lag | -0.1906 | -2.90 | 0.004 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.731 | 2.32 | N | Y | N | Y | N | -919.3 | Y | N | Y | Y |
| Spec2_LogNetWorth_CCI | 0.772 | 2.44 | N | Y | N | Y | N | -501.7 | N | N | Y | Y |
| Spec3_LevelNetWorth | 0.731 | 2.32 | N | Y | N | Y | N | -919.4 | Y | N | Y | Y |
| Spec4_Disagg_NoCCI | 0.726 | 2.32 | N | Y | N | Y | N | -905.3 | N | N | Y | Y |
| Spec5_FullDisagg | 0.802 | 2.25 | Y | Y | N | N | N | -494.6 | N | N | Y | Y |
| Spec6_Preferred | 0.804 | 2.15 | Y | Y | N | Y | N | -492.5 | N | N | Y | Y |
| Spec6b_LongHistSRCCI | 0.703 | 2.12 | Y | Y | N | N | N | -1114.0 | Y | N | Y | N |
| Spec7_CohortBurden | 0.834 | 2.21 | Y | N | N | Y | N | -500.6 | N | N | N | Y |
| Spec7b_RBABurden | 0.872 | 2.16 | Y | N | N | — | Y | -364.5 | N | N | N | Y |
| Spec8_CCI_Interactions | 0.827 | 1.92 | Y | Y | N | Y | N | -952.8 | N | N | N | Y |
| Spec9_KalmanCCI | 0.735 | 2.18 | Y | N | N | N | N | -890.6 | N | — | Y | N |
| Spec10_WilliamsPrior | 0.778 | 2.10 | Y | Y | N | Y | Y | -493.2 | Y | — | Y | N |
| Spec11_LIVES_Headline | 0.824 | 1.90 | Y | Y | N | N | N | -954.8 | Y | N | N | N |
| Spec12_LIVES_Calibrated | 0.686 | 2.08 | Y | N | N | N | N | -892.3 | N | — | Y | N |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.1934 | -0.1607 | yes |
| Spec2_LogNetWorth_CCI | -0.1860 | -0.0799 | yes |
| Spec3_LevelNetWorth | -0.1906 | -0.1578 | yes |
| Spec4_Disagg_NoCCI | -0.1824 | -0.1190 | yes |
| Spec5_FullDisagg | -0.2353 | -0.0607 | yes |
| Spec6_Preferred | -0.2386 | -0.0866 | yes |
| Spec6b_LongHistSRCCI | -0.2475 | -0.2397 | yes |
| Spec7_CohortBurden | -0.3414 | -0.0544 | yes |
| Spec7b_RBABurden | -0.3805 | -0.0610 | yes |
| Spec8_CCI_Interactions | -0.4583 | -0.2186 | yes |
| Spec9_KalmanCCI | -0.2088 | -0.1577 | yes |
| Spec10_WilliamsPrior | -0.0569 | -0.0187 | yes |
| Spec11_LIVES_Headline | -0.4483 | -0.2656 | yes |
| Spec12_LIVES_Calibrated | -0.0292 | 0.0291 | no |

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
- Spec9_KalmanCCI fails: sign, stability
- Spec10_WilliamsPrior fails: stability
- Spec11_LIVES_Headline fails: cointegration, λ range/sign, stability
- Spec12_LIVES_Calibrated fails: sign, stability
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Australia/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Australia/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-06-12

