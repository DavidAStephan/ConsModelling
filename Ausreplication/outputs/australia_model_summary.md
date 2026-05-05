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
| nla_y | -0.0030 | -0.63 | 0.533 | + | no |
| eq_y | 0.0237 | 1.16 | 0.248 | + | yes |
| super_y | -0.0068 | -1.79 | 0.076 | + | no |
| ha_y | 0.0061 | 1.28 | 0.202 | + | yes |
| ln_hp_over_y | -0.0160 | -2.67 | 0.009 | +/- | — |
| real_rate | -0.0001 | -0.16 | 0.874 | - | yes |
| ln_yp_over_y | -0.0049 | -6.00 | 0.000 | +/- | — |
| ecm_lag | -0.1258 | -2.69 | 0.008 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.648 | 2.05 | Y | Y | N | N | Y | -806.1 | N | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.685 | 2.02 | Y | Y | N | Y | N | -487.6 | N | N | Y | Y |
| Spec3_LevelNetWorth | 0.648 | 2.06 | Y | Y | N | N | Y | -806.1 | N | N | Y | N |
| Spec4_Disagg_NoCCI | 0.706 | 2.36 | N | N | N | Y | N | -814.6 | N | Y | Y | Y |
| Spec5_FullDisagg | 0.768 | 2.28 | Y | N | N | Y | N | -491.0 | N | Y | Y | Y |
| Spec6_Preferred | 0.768 | 2.28 | Y | N | N | Y | N | -487.8 | N | Y | Y | Y |
| Spec7_CohortBurden | 0.786 | 2.18 | Y | N | N | Y | N | -485.3 | N | Y | Y | Y |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.0402 | 0.0660 | no |
| Spec2_LogNetWorth_CCI | -0.0670 | 0.0814 | no |
| Spec3_LevelNetWorth | -0.0414 | 0.0656 | no |
| Spec4_Disagg_NoCCI | -0.1258 | -0.0359 | yes |
| Spec5_FullDisagg | -0.1657 | -0.0964 | yes |
| Spec6_Preferred | -0.1620 | -0.0782 | yes |
| Spec7_CohortBurden | -0.2179 | -0.1953 | yes |

## Italy vs Australia (preferred specs)

| Term | Italy | Australia |
|------|-------|-----------|
| ecm_lag | — | -1.0000 |
| eq_y | — | 0.3151 |
| ha_y | 0.0046 | 0.0406 |
| ilfa_y | 0.0011 | — |
| ln_hp_over_y | — | -0.1510 |
| ln_yp_over_y | -0.0184 | -0.3774 |
| nla_y | 0.0255 | 0.1730 |
| real_rate | -0.0272 | -0.0007 |
| super_y | — | -0.0664 |

## Known issues
- Spec1_LogNetWorth fails: sign, cointegration, stability
- Spec2_LogNetWorth_CCI fails: sign, cointegration
- Spec3_LevelNetWorth fails: sign, cointegration, stability
- Spec4_Disagg_NoCCI fails: sign
- Spec5_FullDisagg fails: sign
- Spec6_Preferred fails: sign
- Spec7_CohortBurden fails: sign
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Ausreplication/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Ausreplication/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-05-05

