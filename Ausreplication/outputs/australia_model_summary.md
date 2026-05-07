# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1988Q3–2024Q4, n=146
- Pre-COVID sample: 1988Q3–2019Q4, n=126
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec3_LevelNetWorth** (selected by automated screen). Reason: signs=TRUE, coint=FALSE, λ=TRUE, stability=FALSE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| networth_y | 0.0037 | 2.08 | 0.040 | + | yes |
| ln_hp_over_y | -0.0028 | -0.55 | 0.581 | +/- | — |
| real_rate | -0.0000 | -0.01 | 0.992 | - | yes |
| ln_yp_over_y | 0.1656 | 2.53 | 0.012 | +/- | — |
| ecm_lag | -0.1841 | -2.69 | 0.008 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.737 | 2.34 | N | N | N | — | N | -922.5 | N | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.778 | 2.37 | N | Y | N | — | N | -504.1 | N | N | Y | N |
| Spec3_LevelNetWorth | 0.738 | 2.35 | N | N | N | — | N | -923.1 | Y | N | Y | N |
| Spec4_Disagg_NoCCI | 0.733 | 2.37 | N | N | N | — | N | -908.7 | N | Y | Y | N |
| Spec5_FullDisagg | 0.809 | 2.26 | Y | N | N | — | N | -497.6 | N | Y | Y | N |
| Spec6_Preferred | 0.812 | 2.16 | Y | Y | N | — | N | -496.0 | N | Y | Y | N |
| Spec7_CohortBurden | 0.835 | 2.20 | Y | N | N | Y | N | -500.7 | N | Y | N | Y |
| Spec7b_RBABurden | 0.869 | 2.15 | Y | N | N | — | N | -362.9 | N | Y | N | N |
| Spec8_CCI_Interactions | 0.763 | 2.17 | Y | N | N | — | N | -910.7 | N | — | Y | N |
| Spec9_KalmanCCI | 0.745 | 2.18 | Y | N | N | — | N | -900.5 | N | — | Y | N |
| Spec10_WilliamsPrior | 0.770 | 2.21 | Y | Y | N | N | N | -492.0 | N | — | Y | Y |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.1925 | -0.1808 | yes |
| Spec2_LogNetWorth_CCI | -0.2096 | -0.2436 | yes |
| Spec3_LevelNetWorth | -0.1841 | -0.1801 | yes |
| Spec4_Disagg_NoCCI | -0.1635 | -0.1748 | yes |
| Spec5_FullDisagg | -0.2279 | -0.1898 | yes |
| Spec6_Preferred | -0.2183 | -0.2128 | yes |
| Spec7_CohortBurden | -0.3667 | -0.2009 | yes |
| Spec7b_RBABurden | -0.3460 | -0.0943 | yes |
| Spec8_CCI_Interactions | -0.2452 | -0.2388 | yes |
| Spec9_KalmanCCI | -0.2061 | -0.1752 | yes |
| Spec10_WilliamsPrior | -0.0817 | -0.0491 | yes |

## Italy vs Australia (preferred specs)

_Italy comparison not available._

## Known issues
- Spec1_LogNetWorth fails: sign, cointegration, stability
- Spec2_LogNetWorth_CCI fails: sign, cointegration, stability
- Spec3_LevelNetWorth fails: cointegration, stability
- Spec4_Disagg_NoCCI fails: sign, stability
- Spec5_FullDisagg fails: sign, stability
- Spec6_Preferred fails: sign, stability
- Spec7_CohortBurden fails: sign, λ range/sign
- Spec7b_RBABurden fails: sign, λ range/sign, stability
- Spec8_CCI_Interactions fails: sign, stability
- Spec9_KalmanCCI fails: sign, stability
- Spec10_WilliamsPrior fails: sign
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Ausreplication/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Ausreplication/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-05-07

