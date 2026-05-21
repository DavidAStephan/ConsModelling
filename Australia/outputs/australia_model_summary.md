# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1988Q3–2024Q4, n=146
- Pre-COVID sample: 1988Q3–2019Q4, n=126
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec2_LogNetWorth_CCI** (selected by automated screen). Reason: signs=TRUE, coint=FALSE, λ=TRUE, stability=TRUE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| ln_networth_y | 0.0013 | 0.03 | 0.977 | + | yes |
| ln_hp_over_y | 0.0085 | 0.51 | 0.615 | +/- | — |
| real_rate | -0.0003 | -0.42 | 0.678 | - | yes |
| ln_yp_over_y | 0.2404 | 1.43 | 0.156 | +/- | — |
| ecm_lag | -0.1928 | -1.93 | 0.058 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.731 | 2.34 | N | N | N | — | N | -919.2 | Y | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.769 | 2.40 | N | Y | N | N | N | -500.8 | Y | N | Y | Y |
| Spec3_LevelNetWorth | 0.732 | 2.35 | N | N | N | — | N | -919.8 | Y | N | Y | N |
| Spec4_Disagg_NoCCI | 0.729 | 2.39 | N | N | N | — | N | -906.8 | N | Y | Y | N |
| Spec5_FullDisagg | 0.798 | 2.31 | N | N | N | N | N | -493.0 | N | Y | Y | Y |
| Spec6_Preferred | 0.807 | 2.18 | Y | Y | N | — | N | -493.8 | N | Y | Y | N |
| Spec7_CohortBurden | 0.833 | 2.20 | Y | N | N | — | N | -499.8 | N | Y | N | N |
| Spec7b_RBABurden | 0.869 | 2.16 | Y | N | N | — | N | -363.0 | N | Y | N | N |
| Spec8_CCI_Interactions | 0.821 | 1.87 | Y | N | N | — | N | -948.5 | Y | — | N | N |
| Spec9_KalmanCCI | 0.737 | 2.20 | Y | N | N | — | N | -895.9 | N | — | Y | N |
| Spec10_WilliamsPrior | 0.778 | 2.17 | Y | Y | N | — | Y | -493.7 | Y | — | Y | N |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.1772 | -0.1002 | yes |
| Spec2_LogNetWorth_CCI | -0.1928 | -0.0847 | yes |
| Spec3_LevelNetWorth | -0.1661 | -0.1013 | yes |
| Spec4_Disagg_NoCCI | -0.1404 | -0.1069 | yes |
| Spec5_FullDisagg | -0.1770 | -0.0970 | yes |
| Spec6_Preferred | -0.1801 | -0.1228 | yes |
| Spec7_CohortBurden | -0.3730 | -0.0518 | yes |
| Spec7b_RBABurden | -0.3755 | -0.0614 | yes |
| Spec8_CCI_Interactions | -0.4449 | -0.2470 | yes |
| Spec9_KalmanCCI | -0.1994 | -0.1723 | yes |
| Spec10_WilliamsPrior | -0.0480 | -0.0274 | yes |

## Italy vs Australia (preferred specs)

_Italy comparison not available._

## Known issues
- Spec1_LogNetWorth fails: cointegration, stability
- Spec2_LogNetWorth_CCI fails: cointegration
- Spec3_LevelNetWorth fails: cointegration, stability
- Spec4_Disagg_NoCCI fails: sign, stability
- Spec5_FullDisagg fails: sign
- Spec6_Preferred fails: sign, stability
- Spec7_CohortBurden fails: sign, λ range/sign, stability
- Spec7b_RBABurden fails: sign, λ range/sign, stability
- Spec8_CCI_Interactions fails: λ range/sign, stability
- Spec9_KalmanCCI fails: sign, stability
- Spec10_WilliamsPrior fails: stability
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Australia/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Australia/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-05-21

