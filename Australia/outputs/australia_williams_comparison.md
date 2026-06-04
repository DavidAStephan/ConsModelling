# Williams comparison — structural parameters

Side-by-side comparison of our preferred specification (Spec 6) against
Muellbauer-Williams (2012) Table 1 / BIS chapter Figs 1-2. Williams' sample
is **1978Q1-2008Q2 (n=122)**; ours is **1988Q4-2024Q4 (n=86)**. Williams
estimates by **FIML in a 4-equation system** with imposed sign priors;
we use **OLS with Newey-West HAC SEs** in a single equation.

Williams' income measure is **non-property household disposable income (NPY)**;
we use **gross disposable income (`ydi_real_pc`)** in Spec 6 by default.
The Williams-NPY robustness column shifts λ by ~+18% (see
`australia_williams_income_robustness.csv`).

## How to read this comparison

Williams reports **structural** long-run coefficients γ in his Table 1.
Each γ relates to the **OLS coefficient** of Spec 6 by γ = OLS_coef / |λ|
(the ECM normalisation: in `Δlog c = λ·γ·X + ...`, the OLS coefficient
on X is `λ·γ`). So a divergence in the structural γ can come from either
(a) genuinely different OLS coefficients, or (b) a different λ.

We report **both** below so the reader can see which channel drives any
given gap. The headline finding is that our OLS coefficients run about
14-37% below Williams' implied OLS values and our |λ| is also about 37%
below his (-0.180 vs -0.286), so the two deficits roughly cancel: the
implied structural γ profile is broadly consistent with Williams' Table 1
(γ_HA ≈ 0.049 vs 0.0488). The match is on individually-insignificant
coefficients (n=86) — consistency, not confirmation; see the confidence
intervals in the headline paper §7.3.1.

## Headline comparison (Spec 6, full sample 1988Q4-2024Q4)

Williams' λ = -0.286, ours = -0.1801. Both our OLS coefficients and our |λ| sit ~14-37% below Williams, so they largely cancel in the implied γ = OLS/|λ|.
Showing both forms below to disentangle the channels:

| Term | Williams γ | Williams implied OLS (γ × \|λ\|) | Our OLS | Our γ (= OLS / \|λ\|) | OLS gap | γ gap |
|------|-----------:|----------------------------------:|--------:|---------------------:|--------:|------:|
| **λ** | -0.2860 | (same) | -0.1801 | (same) | -37% | (same) |
| Housing `ha_y` | 0.0488 | 0.0140 | 0.0088 | 0.0491 | -37% | +1% |
| Illiquid `eq_y + super_y` | 0.0220 (calibrated) | 0.0063 | 0.0054 | 0.0300 | -14% | +36% |
| Net liquid `nla_y` | 0.1590 | 0.0455 | 0.0354 | 0.1963 | -22% | +23% |
| log(HP/y) | -0.1300 | -0.0372 | -0.0170 | -0.0943 | — | — |
| ψ (PI weight) at CCI=0 | 0.2000 (calibrated) | 0.0572 | 0.1999 | 1.1097 | — | — |

## Spec 8 (CCI-interaction) match

Spec 8 includes Williams' CCI interactions on the truncated 1988Q4+ sample
with our reduced-form `cci_williams` (3 knots — 2009Q1, 2019Q1, 2020Q2 —
surviving the iterated sign-prior reduction of §5.1.1).

| Williams term | Williams γ | Spec 8 OLS coef | Spec 8 implied γ | Note |
|---|---:|---:|---:|---|
| α_c1 (r × CCI) | -0.8710 | 0.0019 | 0.0043 | Sign FAIL on our sample (small +ve) |
| α_c4 (log(HP/y) × (1-1.2·CCI)) | -0.1300 | 0.0076 | 0.0172 | Sign agrees; magnitude smaller |
| ψ_1 (log(y^p/y) × CCI) | 0.9300 | -0.6113 | -1.3740 | Williams calibrates; we estimate freely (sign FAIL) |

## Where we agree, where we differ

**Agreement on the level OLS coefficients:** Spec 6's raw OLS coefficients
on `ha_y` (≈0.015) and `eq_y + super_y` (≈0.057) are within an order of
magnitude of Williams' published structural γ values (0.0488 and 0.022),
and `nla_y` (≈0.041) is similarly close to Williams' 0.159 in *level*.

**Sign agreement:** All wealth MPCs are positive in our preferred spec
(the NLA-netting fix delivered this), log(HP/y) is negative as expected,
and λ is negative as required for a stable ECM. Spec 6 passes the sign
screen comprehensively.

**The structural γ profile broadly matches Williams — via offsetting OLS and λ deficits.**

1. **|λ| is about 37% below Williams** (our -0.1801 vs −0.286). Our OLS
   coefficients are also ~14-37% below his implied OLS values, so the two
   deficits largely cancel in the implied structural γ. Any residual gap is
   plausibly due to (i) missing CCI interactions in Spec 6, which Williams
   partitions across r×CCI, log(HP/y)×(1-1.2·CCI), and ψ_1·CCI; (ii) FIML
   cross-equation identification across the LIVES system; (iii) the sample
   window — Williams 1978-2008 includes the deregulation-era acceleration
   our 1988+ sample misses.

2. **The implied γ profile is close to Williams.** Our implied γ = OLS/|λ|
   gives γ_HA ≈ 0.049 (Williams 0.0488), γ_NLA ≈ 0.196 (0.159), and
   γ_IFA ≈ 0.030 (0.022). These are point estimates on a small (n=86)
   sample with wide confidence intervals — every Williams value lies inside
   our 95% CI, but so does zero (headline paper §7.3.1) — so this is
   statistical consistency, not precise confirmation.

3. **Permanent-income coefficient is negative** in our estimation (the
   well-documented Australian puzzle), whereas Williams calibrates ψ₀=0.20.
   Adding the Italy-style local projection income forecaster might flip
   this — see open work item.

## Sample / methodology notes for the WP

- Williams' sample ends 2008Q2 by design (so the GFC tightening at the 2007
  spline knot is identified by the spline only, not by post-GFC realisations).
  Our sample to 2024Q4 includes 16 additional years of post-GFC data.
- Only 1 of Williams' 4 canonical knots survives sign-prior reduction on our
  1988Q4+ sample (2007; 1979 is aliased/constant within the window, 1992 and
  1998 violate their sign priors). The deployed `cci_williams` instead uses
  the iterated maximal-GETS reduction (3 knots, §5.1.1). Sample back-extension
  to ~1975Q1 is the
  standing research priority for full Williams replication. The RBA
  unpublished pre-1988 balance sheet series is the binding obstacle.
- Williams calibrates several coefficients (illiquid MPC γ_2 ≈ 0.01-0.022,
  ψ values 0.20→0.95, the −1.2 weight in (1−ϖ·CCI)). We estimate everything
  freely. A future companion specification could impose Williams' priors
  and report Bayesian posteriors.

## Suggested framing for the WP

The headline message for the methodology section: **our OLS coefficients run
~14-37% below Williams' implied OLS values and our |λ| is ~37% below his, so
the two largely cancel and the implied structural γ profile broadly matches
Williams' Table 1 — though on a small (n=86) sample with wide confidence
intervals (§7.3.1), so this is statistical consistency, not confirmation.
Any residual gap reflects the single-equation framing, the missing CCI
interactions in the preferred spec, and the non-overlapping post-1988 sample.**

Generated by `Australia/R/williams_comparison.R`. To refresh after
re-estimation: `Rscript Australia/R/williams_comparison.R`.

