# LIVES Phase 3 — findings

3-equation system (consumption + house prices + mortgage stock) with
**joint cross-equation CCI identification**: a knot is required to
satisfy its institutional sign prior in ALL THREE equations to be
included in `cci_williams_joint`.

This implements Williams' cross-equation sign restrictions through
coordinated knot survival rather than custom FIML likelihood code. The
phase-1 SUR found that joint estimation gives no efficiency gain
(ρ(ε_C, ε_H) ≈ 0); the phase-3 deliverable is therefore *structural
identification*, not efficiency.

Run on 2026-05-08, n=189 common observations 1977Q4–2024Q4.

Implementation:
- [LIVES/R/joint_cci_identification.R](../R/joint_cci_identification.R)
- [LIVES/R/lives_sur_3eq.R](../R/lives_sur_3eq.R)

---

## 1. Joint knot survival

Refit all 3 equations with the full 15-knot Williams CCI basis. Drop
knots whose coefficient sign violates its institutional prior in ANY
equation.

| Survival regime          | Surviving knots                                                              | n |
|--------------------------|------------------------------------------------------------------------------|--:|
| Consumption only (canonical pipeline) | 1979Q1, 1986Q1, 1992Q1, 2007Q3, 2017Q1, 2020Q2 | 6 |
| **Joint (C ∩ H ∩ M)**    | **1986Q1, 2017Q1**                                                           | **2** |

**Substantive finding:** of 6 knots that survive when fitted to
consumption alone, **only 2 survive joint sign-prior survival across
all three equations**. The other 4 (1979Q1 deregulation, 1992Q1
banking distress, 2007Q3 GFC, 2020Q2 COVID) sign-violate in the
house-price or mortgage-stock equations.

**Interpretation.** The maximal-GETS protocol's reliance on
consumption-equation sign survival was masking cross-equation
inconsistency. When required to behave as a true common factor, the
Williams candidate set has only 2 knots that can pass — both
institutional events with broad sectoral effects (1986 financial
deregulation; 2017 APRA macroprudential round II).

Correlation between the joint-identified cci_williams_joint and the
consumption-only cci_williams: **ρ = 0.33** — they are very different
time series.

---

## 2. The headline structural test

The phase-1 SUR found a **sign violation on `cci_williams` in the
house-price equation** (loading −0.024, t = −4.68; prior > 0).
Phase 3's central question: does joint identification fix this?

### House-price equation, CCI loading across regimes

| Regime                                     | CCI loading | Sign |
|--------------------------------------------|-----------:|------|
| (a) Single-eq OLS, consumption-fitted CCI |    −0.0243 | **−** ✗ violator |
| (b) Single-eq OLS, joint-identified CCI   |    +0.0238 | **+** ✓ |
| (c) 3-eq SUR, joint-identified CCI         |    +0.0236 | **+** ✓ |

**This is the structural identification working as intended.** The
HP equation's CCI loading flips from significantly negative to
significantly positive when we require the CCI to be jointly
identified across equations. Sign violation eliminated.

### Mortgage-stock equation, CCI loading across regimes

| Regime                                     | CCI loading | Sign |
|--------------------------------------------|-----------:|------|
| (a) Single-eq OLS, consumption-fitted CCI |   −0.00206 | **−** ✗ |
| (b) Single-eq OLS, joint-identified CCI   |   −0.00696 | **−** ✗ |
| (c) 3-eq SUR, joint-identified CCI         |   −0.00764 | **−** ✗ |

**Joint identification does NOT fix the mortgage equation's CCI sign
violation.** The reason: cci_williams_joint is built using
consumption-equation weights on the 2 surviving knots, then imposed
on M. The M equation prefers a different combination of those knots,
yielding a negative overall loading.

This is the limit of "joint sign survival" as an approximation to
true FIML. Williams' FIML imposes parameter equality across equations
(same CCI loading magnitude, not just same sign); our approximation
imposes only sign consistency. To fix the M equation's loading, we'd
need either (i) custom FIML with parameter restrictions, or (ii) a
weighted average of equation-specific coefficients to define
cci_williams_joint.

---

## 3. Consumption equation wealth coefficients

### Question: does joint CCI identification close the gap with Williams Table 1?

| Coefficient            | (a) cons-only CCI | (b) joint OLS | (c) joint SUR | Williams T1 |
|------------------------|------------------:|--------------:|--------------:|------------:|
| λ (ecm_lag)            |            −0.242 |        −0.204 |        −0.214 |     −0.286  |
| ln_networth_y_extended |            +0.027 |        +0.023 |        +0.023 |       (n/a) |
| ln_yp_over_y           |            +0.247 |        +0.189 |        +0.197 |       (+0.20)|

**Answer: no.** Wealth coefficient moves from 0.027 to 0.023 (15%
smaller) under joint identification. Permanent-income coefficient
shifts from +0.25 to +0.19, slightly closer to Williams' calibrated
+0.20 but the change is well within sampling noise. λ shrinks 12% in
magnitude (−0.24 → −0.21) — moving *away* from Williams' −0.286.

The wealth-coefficient gap with Williams persists across all three
regimes. The joint-identification fix is structural (eliminating the
HP-equation sign violation), not quantitative (closing the wealth-
elasticity gap).

This is consistent with the prior diagnosis from
[back_extension_findings.md §3c](../../Ausreplication/docs/back_extension_findings.md):
the wealth-coefficient gap is a **single-equation OLS framing** issue,
and the phase-3 approximation (joint sign survival) is not sufficient
to close it. True parameter-restriction FIML, with shared parameters
across equations, may close it — but that's beyond this phase.

---

## 4. Cross-equation residual correlation under SUR

3-equation SUR residual correlation matrix:

```
         cons      hp       m
cons   1.0000   −0.1074   +0.0910
hp    −0.1074    1.0000   −0.2001
m     +0.0910   −0.2001    1.0000
```

Correlations are small but non-zero. Phase 1 had ρ(ε_C, ε_H) ≈ 0
(the consumption-only CCI absorbed most cross-equation variation).
Phase 3 has ρ(ε_C, ε_H) = −0.11; ρ(ε_H, ε_M) = −0.20.

The non-zero cross-equation residual correlations under joint-CCI
specification mean SUR now offers small efficiency gains over
equation-by-equation OLS — not because we're using joint estimation,
but because the joint-identified CCI no longer absorbs the
cross-equation linkage that the consumption-fitted CCI was
inadvertently capturing.

---

## 5. Summary — what phase 3 establishes

1. **Joint cross-equation sign survival eliminates 4 of 6 knots.**
   Only 2 knots (1986 deregulation, 2017 APRA II) have signs
   consistent with their institutional priors across consumption,
   house prices, and mortgage stock simultaneously. The
   maximal-GETS protocol's apparent success in identifying 6 knots
   was overstated — 4 of them are
   consumption-equation-specific.
2. **Joint identification fixes the HP equation's sign violation.**
   The CCI loading flips from significantly negative (under
   consumption-only CCI) to significantly positive (under
   joint-identified CCI). This is the structural identification
   working as Williams' framework intends.
3. **Joint identification does NOT fix the mortgage equation's
   sign violation.** The 2-knot weighted CCI is insufficient to
   pass sign tests in all three equations simultaneously when
   weights are derived from one equation's coefficients. True
   parameter-restriction FIML would be needed.
4. **Wealth coefficients in consumption equation barely move.**
   The wealth-coefficient gap with Williams Table 1 — already
   diagnosed as a single-equation OLS framing issue — is not
   closed by sign-survival joint identification.
5. **3-eq SUR residual correlations are small** (|ρ| < 0.21),
   confirming that joint estimation's value is structural
   (cross-equation restrictions) rather than efficiency.

---

## 6. Implications for the WP and next steps

### For the central WP §5 narrative

The combined placebo + joint-identification evidence now produces a
**three-step sharpening of the §5 argument**:

1. **Literal Williams 4-knot CCI fails the placebo on both samples**
   (1988+: 49th/22nd; 1976+: 19th/10th).
2. **Maximal-GETS canonical CCI partially rescues identification**
   (1976+: 64th/36th, "weak support") — but is achieved by fitting to
   consumption alone, which masks cross-equation inconsistency.
3. **Joint cross-equation identification reveals only 2 knots survive
   sign-prior tests across consumption + HP + mortgage stock**,
   correctly eliminating the HP equation's sign violation but
   confirming that the wealth-coefficient gap with Williams (Table 1)
   is structural — single-equation OLS, even with joint sign survival,
   cannot recover Williams' calibrated values without imposing
   parameter equality across equations.

This is a clean empirical case for the multi-equation companion paper
(NS-031). It also empirically supports the interpretation in
[back_extension_findings.md §4](../../Ausreplication/docs/back_extension_findings.md):
the binding constraint is single-equation framing, not sample length.

### Outstanding work

1. **True parameter-restriction FIML.** Custom likelihood with shared
   parameters (e.g., `ϖ` in the wealth × (1−ϖ·cci) interaction
   restricted to be the same across equations). Requires custom MLE
   code; weeks of work.
2. **Use M-equation weights for cci_williams_joint.** A simpler
   experiment: rebuild cci_williams_joint using mortgage-equation
   coefficients on the 2 surviving knots. Does the M sign violation
   then disappear (and HP retain its sign-consistency)? Would test
   how robust the structural identification is to weight choice.
3. **HEW (4th equation).** Williams Table 4. Will further restrict
   the joint survivor set; may reduce the surviving knots to zero
   and force a re-think of the candidate basis.

---

## 7. Outputs

- [outputs/lives_joint_cci_survival.csv](../outputs/lives_joint_cci_survival.csv) — knot survival table per equation
- [outputs/lives_sur_3eq_coefs.csv](../outputs/lives_sur_3eq_coefs.csv) — coefficients for all 3 equations × 3 regimes
- [outputs/lives_phase3_comparison.csv](../outputs/lives_phase3_comparison.csv) — headline comparison table
- [outputs/lives_model_data.rds](../outputs/lives_model_data.rds) — updated model frame with `cci_williams_joint`
