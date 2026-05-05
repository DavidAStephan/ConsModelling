# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1991Q4–2024Q4, n=133
- Pre-COVID sample: 1991Q4–2019Q4, n=113
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec4_Disagg_NoCCI** (selected by automated screen). Reason: signs=FALSE, coint=TRUE, λ=TRUE, stability=TRUE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| nla_y | -0.0025 | -0.60 | 0.551 | + | no |
| eq_y | 0.0227 | 1.15 | 0.254 | + | yes |
| super_y | -0.0081 | -2.27 | 0.025 | + | no |
| ha_y | 0.0059 | 1.28 | 0.203 | + | yes |
| ln_hp_over_y | -0.0158 | -2.81 | 0.006 | +/- | — |
| real_rate | -0.0008 | -1.73 | 0.086 | - | yes |
| ln_yp_over_y | -0.0051 | -6.11 | 0.000 | +/- | — |
| ecm_lag | -0.1250 | -2.65 | 0.009 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.649 | 2.07 | Y | Y | N | N | Y | -806.5 | N | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.653 | 2.05 | Y | Y | N | N | N | -804.0 | N | N | Y | N |
| Spec3_LevelNetWorth | 0.649 | 2.07 | Y | Y | N | N | Y | -806.6 | N | N | Y | N |
| Spec4_Disagg_NoCCI | 0.709 | 2.40 | N | N | N | Y | Y | -816.1 | N | Y | Y | Y |
| Spec5_FullDisagg | 0.728 | 2.25 | Y | N | N | Y | N | -809.9 | N | Y | Y | Y |
| Spec6_Preferred | 0.725 | 2.25 | Y | N | N | Y | N | -805.1 | N | Y | Y | Y |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.0393 | 0.0592 | no |
| Spec2_LogNetWorth_CCI | -0.0389 | 0.0594 | no |
| Spec3_LevelNetWorth | -0.0411 | 0.0588 | no |
| Spec4_Disagg_NoCCI | -0.1250 | -0.0560 | yes |
| Spec5_FullDisagg | -0.1412 | -0.0523 | yes |
| Spec6_Preferred | -0.1399 | -0.0403 | yes |

## Italy vs Australia (preferred specs)

| Term | Italy | Australia |
|------|-------|-----------|
| ecm_lag | — | -1.0000 |
| eq_y | — | 0.2376 |
| ha_y | 0.0046 | 0.0428 |
| ilfa_y | 0.0011 | — |
| ln_hp_over_y | — | -0.1113 |
| ln_yp_over_y | -0.0184 | -0.0802 |
| nla_y | 0.0255 | -0.0006 |
| real_rate | -0.0272 | -0.0086 |
| super_y | — | -0.0809 |

## Known issues
- Spec1_LogNetWorth fails: sign, cointegration, stability
- Spec2_LogNetWorth_CCI fails: sign, cointegration, stability
- Spec3_LevelNetWorth fails: sign, cointegration, stability
- Spec4_Disagg_NoCCI fails: sign
- Spec5_FullDisagg fails: sign
- Spec6_Preferred fails: sign
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Ausreplication/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Ausreplication/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-05-05

