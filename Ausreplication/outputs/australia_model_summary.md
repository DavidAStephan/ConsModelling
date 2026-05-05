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
| nla_y | 0.0407 | 1.51 | 0.136 | + | yes |
| eq_y | 0.0442 | 1.54 | 0.128 | + | yes |
| super_y | 0.0125 | 1.21 | 0.229 | + | yes |
| ha_y | 0.0148 | 2.15 | 0.035 | + | yes |
| ln_hp_over_y | -0.0310 | -2.36 | 0.021 | +/- | — |
| real_rate | -0.0007 | -0.76 | 0.451 | - | yes |
| ln_yp_over_y | -0.2013 | -1.88 | 0.065 | +/- | — |
| ln_yp_over_y_post2008 | 0.2039 | 1.85 | 0.069 | +/- | — |
| ecm_lag | -0.0525 | -0.89 | 0.377 | - | yes |

## All specifications — diagnostics traffic light

| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |
|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|
| Spec1_LogNetWorth | 0.726 | 2.50 | N | N | N | — | N | -824.4 | Y | N | Y | N |
| Spec2_LogNetWorth_CCI | 0.757 | 2.52 | N | N | N | N | N | -496.6 | Y | N | Y | Y |
| Spec3_LevelNetWorth | 0.731 | 2.51 | N | N | N | — | N | -826.8 | Y | N | Y | N |
| Spec4_Disagg_NoCCI | 0.727 | 2.57 | N | N | N | — | N | -813.7 | Y | Y | Y | N |
| Spec5_FullDisagg | 0.793 | 2.38 | N | N | N | N | N | -491.0 | Y | Y | Y | Y |
| Spec6_Preferred | 0.807 | 2.35 | Y | N | N | N | N | -493.6 | Y | Y | Y | Y |
| Spec7_CohortBurden | 0.828 | 2.40 | N | N | N | — | N | -497.6 | N | Y | Y | N |
| Spec8_CCI_Interactions | 0.739 | 2.37 | N | N | N | — | N | -804.8 | N | — | Y | N |

## Lambda comparison (full vs pre-COVID)

| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |
|------|---------------|-------------|--------------|
| Spec1_LogNetWorth | -0.0814 | -0.0602 | yes |
| Spec2_LogNetWorth_CCI | -0.0828 | -0.0677 | yes |
| Spec3_LevelNetWorth | -0.0788 | -0.0523 | yes |
| Spec4_Disagg_NoCCI | -0.0682 | -0.0576 | yes |
| Spec5_FullDisagg | -0.0790 | -0.1311 | yes |
| Spec6_Preferred | -0.0525 | -0.0931 | yes |
| Spec7_CohortBurden | -0.2388 | -0.2160 | yes |
| Spec8_CCI_Interactions | -0.0740 | -0.0535 | yes |

## Italy vs Australia (preferred specs)

| Term | Italy | Australia |
|------|-------|-----------|
| ecm_lag | — | -1.0000 |
| eq_y | — | 0.8431 |
| ha_y | 0.0046 | 0.2817 |
| ilfa_y | 0.0011 | — |
| ln_hp_over_y | — | -0.5908 |
| ln_yp_over_y | -0.0184 | -3.8366 |
| nla_y | 0.0255 | 0.7752 |
| real_rate | -0.0272 | -0.0131 |
| super_y | — | 0.2376 |

## Known issues
- Spec1_LogNetWorth fails: cointegration, stability
- Spec2_LogNetWorth_CCI fails: cointegration
- Spec3_LevelNetWorth fails: cointegration, stability
- Spec4_Disagg_NoCCI fails: stability
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

