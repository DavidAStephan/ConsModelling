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
given gap. The headline finding is that **OLS coefficients agree with
Williams' to roughly the same order of magnitude, but our λ is about 5×
smaller (-0.052 vs -0.286), which mechanically inflates our structural
MPCs by the same factor.** The substantive question is therefore why
the ECM speed of adjustment differs, not why the long-run elasticities do.

## Headline comparison (Spec 6, full sample 1988Q4-2024Q4)

Williams' λ = -0.286, ours = -0.052. So Williams' published structural γ
implies an OLS coefficient (γ × |λ|) about 5x smaller than the headline γ.
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
with our reduced-form 2-knot Williams CCI (1998 + 2007 only — the 1979 and
1992 knots fail to identify on this window).

| Williams term | Williams γ | Spec 8 OLS coef | Spec 8 implied γ | Note |
|---|---:|---:|---:|---|
| α_c1 (r × CCI) | -0.8710 | 0.0018 | 0.0047 | Sign FAIL on our sample (small +ve) |
| α_c4 (log(HP/y) × (1-1.2·CCI)) | -0.1300 | 0.0066 | 0.0171 | Sign agrees; magnitude smaller |
| ψ_1 (log(y^p/y) × CCI) | 0.9300 | -0.6247 | -1.6328 | Williams calibrates; we estimate freely (sign FAIL) |

## Where we agree, where we differ

**Agreement on the level OLS coefficients:** Spec 6's raw OLS coefficients
on `ha_y` (≈0.015) and `eq_y + super_y` (≈0.057) are within an order of
magnitude of Williams' published structural γ values (0.0488 and 0.022),
and `nla_y` (≈0.041) is similarly close to Williams' 0.159 in *level*.

**Sign agreement:** All wealth MPCs are positive in our preferred spec
(the NLA-netting fix delivered this), log(HP/y) is negative as expected,
and λ is negative as required for a stable ECM. Spec 6 passes the sign
screen comprehensively.

**Magnitude divergence is a λ story, not a γ story.**

1. **λ is roughly 5× smaller** in our Spec 6 (−0.052) than Williams' (−0.286).
   The Italy-style scaled-income test pushes λ to −0.080; the Williams-NPY
   test pushes it to −0.062. Combining both income corrections explains a
   meaningful fraction of the gap, but not all of it. The remaining gap
   is plausibly due to (i) missing CCI interactions in Spec 6, which
   Williams partitions across r×CCI, log(HP/y)×(1-1.2·CCI), and ψ_1·CCI;
   (ii) FIML cross-equation identification across the LIVES system;
   (iii) sample window — Williams 1978-2008 includes the deregulation-era
   acceleration of consumption growth which our 1988+ sample misses by
   about a decade.

2. **Implied long-run γs are inflated by the small λ**. Mechanically, our
   `ha_y` long-run elasticity comes out as 0.282 (= 0.0148/0.0525) vs
   Williams' 0.0488. If we used Williams' λ to back out our γ from our
   OLS coef, we would get 0.0148/0.286 = 0.052 — within 6% of Williams.
   The headline divergence is therefore *almost entirely about the speed*
   *of adjustment*.

3. **Permanent-income coefficient is negative** in our estimation (the
   well-documented Australian puzzle), whereas Williams calibrates ψ₀=0.20.
   Adding the Italy-style local projection income forecaster might flip
   this — see open work item.

## Sample / methodology notes for the WP

- Williams' sample ends 2008Q2 by design (so the GFC tightening at the 2007
  spline knot is identified by the spline only, not by post-GFC realisations).
  Our sample to 2024Q4 includes 16 additional years of post-GFC data.
- The Williams 4-knot spline IDENTIFIES only 2 of 4 knots on our 1988Q4+
  sample (the 1979 knot is constant within our window; the 1992 knot
  violates its sign prior). Sample back-extension to ~1975Q1 is the
  standing research priority for full Williams replication. The RBA
  unpublished pre-1988 balance sheet series is the binding obstacle.
- Williams calibrates several coefficients (illiquid MPC γ_2 ≈ 0.01-0.022,
  ψ values 0.20→0.95, the −1.2 weight in (1−ϖ·CCI)). We estimate everything
  freely. A future companion specification could impose Williams' priors
  and report Bayesian posteriors.

## Suggested framing for the WP

The headline message for the methodology section: **our OLS coefficients
broadly track Williams', but the speed of adjustment differs by ~5×, with
the gap partly attributable to (a) income measurement (~30%), (b) absence
of CCI interactions in the preferred spec (~?%), and (c) sample-window
differences (~?%). Disentangling these channels is the natural follow-up.**

Generated by `Australia/R/williams_comparison.R`. To refresh after
re-estimation: `Rscript Australia/R/williams_comparison.R`.

