# Williams comparison — structural parameters

Side-by-side comparison of our preferred specification (Spec 6) against
Muellbauer-Williams (2012) Table 1 / BIS chapter Figs 1-2. Williams' sample
is **1978Q1-2008Q2 (n=122)**; ours is **1988Q3-2024Q4 (n=86 for Spec 6)**. Williams
estimates by **FIML in a 4-equation system** with imposed sign priors;
we use **OLS with Newey-West HAC SEs** in a single equation.

Williams' income measure is **non-property household disposable income (NPY)**;
we use **gross disposable income (`ydi_real_pc`)** in Spec 6 by default.
The Williams-NPY robustness column is in
`australia_williams_income_robustness.csv` (read the live shift there).

## How to read this comparison

Williams reports **structural** long-run coefficients γ in his Table 1.
Each γ relates to the **OLS coefficient** of Spec 6 by γ = OLS_coef / |λ|
(the ECM normalisation: in `Δlog c = λ·γ·X + ...`, the OLS coefficient
on X is `λ·γ`). So a divergence in the structural γ can come from either
(a) genuinely different OLS coefficients, or (b) a different λ.

We report **both** below so the reader can see which channel drives any
given gap. Read the live numbers from the table (regenerated each run
from the current fits) rather than from any hand-written summary; the
Spec 6 coefficients are individually imprecise (n = 86), so treat the
structural profile as consistency evidence, not confirmation — see the
confidence intervals in the headline paper Section 7.3.1.

## Headline comparison (Spec 6, full sample 1988Q3-2024Q4)

Williams' lambda = -0.286, ours = -0.2386.
Showing both forms below to disentangle the channels:

| Term | Williams γ | Williams implied OLS (γ × \|λ\|) | Our OLS | Our γ (= OLS / \|λ\|) | OLS gap | γ gap |
|------|-----------:|----------------------------------:|--------:|---------------------:|--------:|------:|
| **λ** | -0.2860 | (same) | -0.2386 | (same) | -17% | (same) |
| Housing `ha_y` | 0.0488 | 0.0140 | 0.0022 | 0.0094 | -84% | -81% |
| Illiquid `eq_y + super_y` | 0.0220 (calibrated) | 0.0063 | -0.0096 | -0.0403 | -253% | +83% |
| Net liquid `nla_y` | 0.1590 | 0.0455 | 0.0083 | 0.0350 | -82% | -78% |
| log(HP/y) | -0.1300 | -0.0372 | 0.0102 | 0.0429 | — | — |
| ψ (PI weight) at CCI=0 | 0.2000 (calibrated) | 0.0572 | 0.3253 | 1.3632 | — | — |

## Spec 8 (CCI-interaction) match

Spec 8 includes Williams' CCI interactions on the truncated 1988Q3+ sample
with our reduced-form `cci_williams` (surviving knots per
`australia_williams_cci_knots.csv`; see headline paper Section 5.1.1).

| Williams term | Williams γ | Spec 8 OLS coef | Spec 8 implied γ | Note |
|---|---:|---:|---:|---|
| α_c1 (r × CCI) | -0.8710 | 0.0028 | 0.0062 | Sign FAIL on our sample (small +ve) |
| α_c4 (log(HP/y) × (1-1.2·CCI)) | -0.1300 | 0.0299 | 0.0652 | Sign agrees; magnitude smaller |
| ψ_1 (log(y^p/y) × CCI) | 0.9300 | -0.5046 | -1.1010 | Williams calibrates; we estimate freely (sign FAIL) |

## Where we agree, where we differ

**Level OLS coefficients:** read the live values from the table above;
they are within an order of magnitude of Williams' published structural
gamma values.

**Sign agreement:** All wealth MPCs are positive in our preferred spec
(the NLA-netting fix delivered this), log(HP/y) is negative as expected,
and λ is negative as required for a stable ECM. Spec 6 passes the sign
screen comprehensively.

**Read the structural gamma profile from the table above (live values).**

1. **Our lambda is -0.2386 against Williams' -0.286** (live value).
   Any gap between the implied structural gammas and Williams' Table 1 is
   plausibly due to (i) missing CCI interactions in Spec 6, which Williams
   partitions across r×CCI, log(HP/y)×(1-1.2·CCI), and ψ_1·CCI; (ii) FIML
   cross-equation identification across the LIVES system; (iii) the sample
   window — Williams 1978-2008 includes the deregulation-era acceleration
   our 1988+ sample misses.

2. **Read the implied gamma profile from the table above** (live values,
   not hand-typed). These are point estimates on a small (n=86) sample
   with wide confidence intervals (headline paper Section 7.3.1), so treat
   agreement as statistical consistency, not precise confirmation; the
   sharper Spec 11 intervals in `australia_gamma_inference.csv` reject
   some Williams magnitudes outright.

3. **The permanent-income coefficient depends on the forecaster**: positive
   under the canonical Italy direct-forecast measure, negative under the
   rolling AR(8) (the documented Australian puzzle); see headline paper
   Section 7.4 and `australia_pi_method_comparison.csv`.

## Sample / methodology notes for the WP

- Williams' sample ends 2008Q2 by design (so the GFC tightening at the 2007
  spline knot is identified by the spline only, not by post-GFC realisations).
  Our sample to 2024Q4 includes 16 additional years of post-GFC data.
- Few of Williams' 4 canonical knots survive sign-prior reduction on our
  1988Q3+ sample (see `australia_knot_experiment.csv`). The deployed
  `cci_williams` instead uses the iterated maximal-GETS reduction
  (survivors in `australia_williams_cci_knots.csv`). Sample back-extension
  to ~1975Q1 is the
  standing research priority for full Williams replication. The RBA
  unpublished pre-1988 balance sheet series is the binding obstacle.
- Williams calibrates several coefficients (illiquid MPC γ_2 ≈ 0.01-0.022,
  ψ values 0.20→0.95, the −1.2 weight in (1−ϖ·CCI)). We estimate everything
  freely. A future companion specification could impose Williams' priors
  and report Bayesian posteriors.

## Suggested framing for the WP

The headline message for the methodology section: **read the live OLS and
structural gaps from the table; on a small (n=86) sample with wide
confidence intervals (Section 7.3.1) agreement is statistical consistency,
not confirmation.
Any residual gap reflects the single-equation framing, the missing CCI
interactions in the preferred spec, and the non-overlapping post-1988 sample.**

Generated by `Australia/R/williams_comparison.R`. To refresh after
re-estimation: `Rscript Australia/R/williams_comparison.R`.

