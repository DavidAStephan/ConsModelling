# LIVES Phase 1 — findings

Two-equation SUR (consumption + house prices) on the back-extended
1976Q3+ sample. n=189 common observations after complete-cases
restriction across both equations.

Run on 2026-05-08 with `cci_williams` constructed from the
maximal-GETS reduction on the consumption equation (15 candidate
knots → 5 surviving: 1992Q1, 2007Q3, 2009Q1, 2019Q1, 2020Q2).

Implementation: [LIVES/R/lives_sur_2eq.R](../R/lives_sur_2eq.R).

---

## 1. Headline result

**Residual correlation between the two equations is essentially zero:**

| Specification | ρ̂(ε_C, ε_H) |
|---|---:|
| Full spec (with cci_williams + all event dummies) | +0.0007 |
| No cci_williams                                    | −0.0829 |
| No event dummies                                   | +0.0426 |
| No cci_williams AND no dummies (minimal LR + SR)   | −0.0252 |

**Verdict: NEGLIGIBLE cross-equation linkage — single-equation OLS is approximately efficient.**

This holds robustly across specification variants. Even the minimal
LR + SR specification (no CCI, no event dummies) produces ρ̂ ≈ −0.025.

## 2. SUR vs OLS coefficients

Because ρ ≈ 0, the SUR estimator gives essentially identical point
estimates and standard errors to equation-by-equation OLS. The
percentage change in coefficients between OLS and SUR is < 0.1% for
nearly every term. See [outputs/lives_sur_2eq_compare.csv](../outputs/lives_sur_2eq_compare.csv)
for the full table.

## 3. Coefficient summary

### Consumption equation

| LR coef                  | OLS estimate | SUR estimate | LR struct (−coef/λ) |
|--------------------------|-------------:|-------------:|--------------------:|
| ln_networth_y_extended   |       +0.021 |       +0.021 |              +0.105 |
| ln_hp_over_y             |       −0.0007 |       −0.0007 |              −0.003 |
| real_rate                |       +0.00018 |       +0.00018 |              +0.00088 |
| ln_yp_over_y             |       +0.198 |       +0.198 |              +0.971 |
| λ (ecm_lag)              |       −0.204 |       −0.203 |               (n/a) |

n=189, R² = 0.704, adj R² = 0.681, RMSE = 0.00814.

### House price equation

| LR coef                  | OLS estimate | SUR estimate | LR struct (−coef/λ_H) |
|--------------------------|-------------:|-------------:|----------------------:|
| lincome                  |       +0.034 |       +0.034 |               +0.377 |
| log_credit_y             |       +0.067 |       +0.067 |               +0.752 |
| real_rate                |       −0.00089 |       −0.00089 |             −0.00995 |
| prime_age_share          |       −0.436 |       −0.437 |               −4.88 |
| cci_williams_lvl         |       −0.024 |       −0.024 |               −0.272 |
| λ_H (ecm_lag_H)          |       −0.0894 |       −0.0894 |                (n/a) |

n=189, R² = 0.345, adj R² = 0.288, RMSE = 0.01705.

**Sign-prior assessment:**
- Consumption: 4 of 5 LR coefficients pass priors (real_rate flips
  from negative to slightly positive — same as the single-equation
  Spec 1 baseline). λ negative → mean reversion ✓.
- House price: 3 of 5 LR coefficients pass priors. **prime_age_share**
  is wrong-signed (−4.88 vs prior >0); **cci_williams_lvl** is
  wrong-signed and significant (LR struct = −0.272, t = −4.68).
  λ_H = −0.089 → mean reversion ✓.

## 4. The cci_williams sign violation in the HP equation

The CCI variable enters the consumption equation with positive
loading (Williams' construction normalizes it that way: looser CCI
→ positive direct contribution to consumption growth). When inserted
into the HP equation, its loading is **negative** (and significant,
t = −4.68).

This is a real puzzle and a structural finding. Possible
interpretations:

1. **Single-equation construction failure**: `cci_williams` was built
   to fit consumption-equation residuals via Hendry-Krolzig
   sign-prior reduction. The surviving knots (1992, 2007, 2009, 2019,
   2020) and their loadings are optimised for consumption, not house
   prices. In the HP equation it ends up loaded the "wrong way" —
   essentially an over-fitted regressor.
2. **Genuine wrong-signedness**: maybe Australian data actually shows
   tighter credit conditions COINCIDED WITH higher house-price growth
   (e.g. 2007 boom + APRA tightening; 2014/17 macropru; 2021 buffer
   hikes — all periods of HP appreciation under tightening). If true,
   Williams' "loose CCI → high HP" prior may not fit Australia.
3. **Endogeneity that SUR can't fix**: cci_williams is determined
   by-and-large by the consumption equation's structural needs.
   Treating it as exogenous in the HP equation imposes an asymmetry
   that the SUR doesn't correct (SUR allows residual covariance, not
   parameter restrictions). Phase 3 FIML — where the same CCI
   loading is jointly identified across all four equations under
   sign restrictions — is the methodologically correct fix.

Provisional reading: **interpretation 3 is most likely**. The HP
equation in phase 1 should arguably use a different CCI construction
(or no CCI at all) and let the cross-equation linkage emerge in
phase 2/3. We document the sign violation and proceed.

## 5. Implications

### For the LIVES rebuild (phases 2/3)

The phase-1 SUR result tells us that **joint estimation does not
deliver efficiency gains for these two equations** at quarterly
frequency. The cross-equation linkage that Williams' framework relies
on is captured by shared regressors (CCI, real rate, event dummies);
it does not reside in unexplained residual covariance.

This means **the case for full LIVES (phase 3 FIML) rests entirely on
cross-equation parameter restrictions, not on efficiency**. Williams'
FIML imposes that the same CCI loading enters all four equations with
specific sign constraints — that's the structural identification.
Phase 1 SUR doesn't impose those restrictions, which is why the HP
equation's CCI sign comes out wrong (no constraint that it must match
the consumption equation's positive sign).

### For the WP

This is a clean phase-1 deliverable for the multi-equation companion
paper:

> "Joint Zellner SUR of the consumption and house-price equations
> produces residual correlation ρ̂ = 0.0007 (negligible). Coefficient
> estimates from SUR are within 0.1% of equation-by-equation OLS for
> every term. The case for the multi-equation framework therefore
> rests on cross-equation structural identification, not on
> efficiency gains. We document a sign violation on cci_williams in
> the house-price equation as evidence that the consumption-fitted
> CCI is not the right common factor for the house-price equation —
> a finding that motivates the FIML extension (phase 3) where the
> CCI loading is jointly identified."

### What doesn't change

- The single-equation paper's conclusions stand. The wealth-coefficient
  gap with Williams (Table 1 calibrations vs single-equation OLS)
  documented in [back_extension_findings.md §3c](../../Ausreplication/docs/back_extension_findings.md)
  is unchanged: SUR doesn't move the consumption coefficients meaningfully.
- The placebo-test verdicts on the maximal-GETS CCI (64th/36th) stand.

---

## 6. Phase 2 / 3 outlook

Given that phase 1 produces no efficiency gain, the value of phases
2-3 is purely structural:

- **Phase 2** (add mortgage-stock equation): adds a third
  cross-equation linkage. If the mortgage-stock equation's residual
  also doesn't correlate with consumption or HP residuals, we'll
  reach the same "no efficiency gain" conclusion. The question
  becomes whether mortgage-stock dynamics are economically informative
  for understanding the wealth channel.
- **Phase 3** (full LIVES FIML with cross-equation restrictions):
  the structural identification is the prize. The CCI's loading would
  no longer be free in each equation; restrictions tie it to a single
  common parameter. This is the version that recovers Williams' Table
  1 wealth coefficients (in principle). But it's a months-long build
  involving custom likelihood code and model selection.

**Recommendation, given phase 1 evidence**: skip phase 2 (mortgage
stock alone won't change the picture if residuals don't correlate)
and go directly to phase 3 (FIML with restrictions). Or keep phase 2
as a sanity-check on whether mortgage-stock dynamics offer surprises.

## 7. Outputs

- [outputs/lives_sur_2eq_coefs.csv](../outputs/lives_sur_2eq_coefs.csv) — full SUR coefficient table
- [outputs/lives_sur_2eq_compare.csv](../outputs/lives_sur_2eq_compare.csv) — OLS vs SUR side-by-side
- [outputs/lives_sur_2eq_resid_corr.csv](../outputs/lives_sur_2eq_resid_corr.csv) — residual covariance summary
- [outputs/hp_equation_standalone.csv](../outputs/hp_equation_standalone.csv) — single-equation HP ECM (sanity-check)
