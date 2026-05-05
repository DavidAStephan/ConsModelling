# Australia Household Consumption Model — Summary

## Sample and data
- Full sample: 1991Q4–2024Q4, n=133
- Pre-COVID sample: 1991Q4–2019Q4, n=113
- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.

## Preferred specification
**Spec6_Preferred** (selected by automated screen). Reason: signs=TRUE, coint=TRUE, λ=TRUE, stability=TRUE.

Long-run structural coefficients (preferred spec, full sample):

| Term | Coef | t-stat | p | Expected sign | Sign OK |
|------|------|--------|---|---------------|---------|
| nla_y | 0.0493 | 1.75 | 0.084 | + | yes |
| eq_y | 0.0434 | 1.44 | 0.156 | + | yes |
| super_y | 0.0110 | 1.13 | 0.264 | + | yes |
| ha_y | 0.0148 | 2.20 | 0.031 | + | yes |
| ln_hp_over_y | -0.0398 | -2.51 | 0.014 | +/- | — |
| real_rate | -0.0005 | -0.57 | 0.573 | - | yes |
| ln_yp_over_y | -0.1438 | -1.80 | 0.076 | +/- | — |
| ln_yp_over_y_post2008 | 0.1452 | 1.76 | 0.083 | +/- | — |
| ecm_lag | -0.0462 | -0.77 | 0.445 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.727 | 2.50 | N | N | N | — | N | -824.5 | Y | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.756 | 2.53 | N | N | N | N | N | -496.4 | Y | N | Y | Y |
| Spec3_LevelNetWorth | 0.728 | 2.48 | N | N | N | — | N | -825.3 | Y | N | Y | N |
| Spec4_Disagg_NoCCI | 0.724 | 2.54 | N | N | N | — | N | -811.9 | Y | Y | Y | N |
| Spec5_FullDisagg | 0.788 | 2.38 | N | N | N | — | N | -489.2 | N | Y | Y | N |
| Spec6_Preferred | 0.801 | 2.40 | N | N | N | N | N | -491.5 | Y | Y | Y | Y |
| Spec7_CohortBurden | 0.823 | 2.44 | N | N | N | — | N | -492.3 | N | Y | Y | N |
| Spec8_CCI_Interactions | 0.740 | 2.34 | N | N | N | — | N | -804.9 | N | — | Y | N |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.0797 | -0.0373 | yes |
| Spec2_LogNetWorth_CCI | -0.0873 | -0.0662 | yes |
| Spec3_LevelNetWorth | -0.0748 | -0.0237 | yes |
| Spec4_Disagg_NoCCI | -0.0676 | -0.0364 | yes |
| Spec5_FullDisagg | -0.0833 | -0.1445 | yes |
| Spec6_Preferred | -0.0462 | -0.1069 | yes |
| Spec7_CohortBurden | -0.2264 | -0.2157 | yes |
| Spec8_CCI_Interactions | -0.0938 | -0.0605 | yes |

## Italy vs Australia (preferred specs)

| Term | Italy | Australia |
|------|-------|-----------|
| ecm_lag | — | -1.0000 |
| eq_y | — | 0.9404 |
| ha_y | 0.0046 | 0.3210 |
| ilfa_y | 0.0011 | — |
| ln_hp_over_y | — | -0.8611 |
| ln_yp_over_y | -0.0184 | -3.1136 |
| nla_y | 0.0255 | 1.0666 |
| real_rate | -0.0272 | -0.0118 |
| super_y | — | 0.2392 |

## Known issues
- Spec1_LogNetWorth fails: cointegration, stability
- Spec2_LogNetWorth_CCI fails: cointegration
- Spec3_LevelNetWorth fails: cointegration, stability
- Spec4_Disagg_NoCCI fails: stability
- Spec5_FullDisagg fails: sign, stability
- Spec7_CohortBurden fails: sign, stability
- Spec8_CCI_Interactions fails: sign, stability
- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.
- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.
- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.
- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.

## Reproducibility
- Run: `Rscript Ausreplication/R/australia_consumption_model.R`
- Fast re-estimation: `Rscript Ausreplication/R/run_estimation_from_rds.R`
- Random seed: not used (OLS is deterministic).
- Date generated: 2026-05-05

