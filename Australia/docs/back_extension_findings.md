# NS-020 back-extension findings

This document captures the empirical results from extending the sample
back to 1976Q3 (NS-020 phase 1) and re-running Spec 1 plus the
placebo test on the longer window. Last updated 2026-05-08.

The data side of NS-020 is documented in
[data.md](data.md) §3.3–3.5 (RBA D-tables), §3.4b (m3_household_proxy),
§4.3 (TRYM), §4.4 (labour_force_historic), and §5.1 (growth-rate
splicing).

---

## 1. Aggregate net-worth proxy (`ln_networth_y_proxy`)

The official `networth_y` is bounded at 1988Q3+ by ABS 5232 sectoral
accounts. To enable Spec 1–3 fits on the 1976Q3+ sample, the pipeline
now constructs a proxy:

1. **`housing_wealth_proxy`**: anchored at the first available official
   `housing_wealth` observation (1988Q3) and back-cast via
   `housing_wealth × (hpi × pop_millions)` growth, holding dwellings
   per capita constant.
2. **Raw aggregate**: `(m3_household_proxy + housing_wealth_proxy) /
   ydi_ann_nom`. M3 allocated to households via `wage_share / 100`
   (Williams 2010 used the household factor income share — the wage
   share is a reasonable approximation; documented simplification).
3. **Growth-rate splice** of the raw aggregate ratio onto the official
   `networth_y` at 1988Q3, so the proxy equals the official series
   from 1988Q3 onwards and back-casts smoothly.

**Caveats** (explicit in the data download script):
- Pre-1988 omits equities and super (quantitatively small in the
  1976–1988 window — Australian super pre-SGC was a negligible
  household asset class)
- Pre-1988 omits debt netting (mortgage debt was a much smaller share
  of household balance sheets in the 1970s than today)
- Use only for back-extension exercises; never as a substitute for
  the official series on the modern sample

The level of the proxy: 5.09× annual income (1976Q3) → 4.67 (1980Q1)
→ 4.72 (1985Q1) → 5.37 (1988Q3, anchored to official) → 10.23 (2024Q4).
This shape — flat through the 1970s/early 80s, rising sharply after
the 1985–86 deregulation — matches Australian historical wealth
accumulation patterns.

---

## 2. Spec 1 refit: 1988Q3+ baseline vs 1976Q3+ extended

Implementation: [refit_spec1_extended.R](../R/refit_spec1_extended.R).
Both fits use the same long-run regressors (`ln_networth_y`,
`ln_hp_over_y`, `real_rate`, `ln_yp_over_y`, `ecm_lag`), same dummies,
same Italy LP permanent income. The only difference is whether the
wealth regressor is the official `networth_y` (1988Q3+, n=146) or
`ln_networth_y_proxy` (1977Q3+, n=190 — bounded by `real_rate` /
`ln_yp_over_y` four-quarter inflation lag).

| Long-run coefficient        | Baseline 1988+ | Extended 1976+ | % change |
|-----------------------------|---------------:|---------------:|---------:|
| λ (ecm_lag)                 |        −0.177 |        −0.202 |    +14.2 |
| ln_networth_y               |         0.112 |         0.107 |    −3.97 |
| ln_hp_over_y                |        −0.0151 |        −0.00379 |   −74.8 |
| real_rate                   |       −0.00137 |       +0.00090 |   −165   |
| ln_yp_over_y                |         0.961 |         0.971 |    +1.05 |
| adj R² / RMSE (× 1000)      |  0.731 / 8.04 |  0.681 / 8.12 |          |

**Headline takeaways.**

1. **The wealth elasticity is stable across samples** (0.112 → 0.107,
   −4%). This is a positive validation of the proxy: doubling the
   sample length and adding a regime that includes the 1979/1985–86
   deregulation episode does *not* shift the wealth-to-consumption
   elasticity meaningfully. The proxy isn't a confounder; it's giving
   the same structural signal as the official series.
2. **Permanent-income elasticity is also stable** (0.961 → 0.971,
   +1%). Consumption-smoothing behaviour is unchanged.
3. **λ becomes more negative** (−0.18 → −0.20). Slightly faster
   error-correction speed on the longer sample. Plausible — the longer
   sample spans a period when households were less leveraged and could
   adjust consumption to wealth shocks more readily.
4. **House-price-to-income coefficient collapses to near zero**
   (−0.015 → −0.004). On the longer sample, the affordability /
   down-payment channel is weaker. Reasonable interpretation: the
   pre-1985 era had less variation in `hp_over_y` (less house-price
   volatility, less credit channel) so this coefficient is identified
   primarily by the post-1985 variation.
5. **Real-rate sign flip**, but both estimates are close to zero
   (−0.0014 → +0.0009) and statistically not different from zero.
   Real-rate enters the long-run weakly in either window.

**Result:** the proxy delivers a fit on the longer sample with
*broadly the same wealth/income elasticities* as the 1988+ baseline.
The main difference is the housing-affordability channel weakening,
which is consistent with the back-extension covering the
pre-deregulation regime.

---

## 3. Placebo test on the extended sample

Implementation: [cci_placebo_extended.R](../R/cci_placebo_extended.R).
Same procedure as the 1988+ placebo
([NS-106](next_steps.md#L408-L417)) but using the
1976Q3+ sample with Spec 1 (aggregate proxy). 200 random 4-knot draws
uniformly distributed in 1979–2007, fit alongside the Williams canonical
4-knot benchmark (1979/1992/1998/2007).

### Headline finding

| Metric                       | Williams canonical | Placebo median | Williams %ile |
|------------------------------|-------------------:|---------------:|--------------:|
| adjusted R²                  |             0.6794 |         0.6817 |       **19th** |
| \|λ\|                         |             0.1929 |         0.2023 |       **10th** |

**Verdict on the extended sample: DETRENDING CRITIQUE PERSISTS —
Williams is *below* the placebo median, not above it.**

### What this changes

The original 1988+ placebo found Williams' canonical 4-knot at the
49th/22nd percentile (sitting at the placebo median). The narrative we
landed on at the time was that the 1979 and 1986 knots couldn't really
identify on the 1988+ sample because they fell at or before the sample
start, and that the *real* test would be the back-extended sample where
data covers the deregulation episode.

That test has now been run, and the result is **worse**, not better:

- **Adj R²: 49th → 19th percentile**. Williams' specific knots are now
  outperformed by 4 in 5 random placements, vs roughly half on the
  short sample.
- **|λ|: 22nd → 10th percentile**. Williams' adjustment-speed was
  smaller than 78% of random draws on the short sample; smaller than
  90% on the long sample.

This is a real empirical result for the WP §5 ("Identification") and
§7 ("Robustness"). It strengthens, not weakens, the user's standing
detrending critique of the literal Williams 4-knot specification.

### Why might the verdict get *worse*?

Several non-mutually-exclusive possibilities:

1. **More degrees of freedom for random knots.** With n=190 instead of
   n=146, the placebo has more pre-knot data to fit each knot's
   smoothed-step transition. Williams' specific dates aren't special
   enough to beat that flexibility.
2. **The proxy adds noise.** `ln_networth_y_proxy` omits eq + super +
   debt pre-1988. Random knots may absorb that noise as easily as
   Williams' specific dates do — possibly more easily, since they
   adjust freely.
3. **Single-equation OLS is the wrong framework.** Williams (2010)
   identified the CCI in a 4-equation FIML system where the same CCI
   spline appears in consumption, house-price, mortgage-stock, and
   HEW equations. Cross-equation restrictions identify the spline in a
   way that single-equation OLS cannot. This is consistent with our
   prior conclusion — see
   [next_steps.md NS-031](next_steps.md#L269-L277) — that the
   multi-equation rebuild is the methodologically correct response.
4. **The literal 4-knot specification is genuinely poor.** Our
   maximal-GETS canonical CCI (15 candidate knots → 6 surviving via
   sign-prior reduction) sidesteps this problem by letting data choose
   the knots. The literal Williams 4-knot is a historical convention
   that may not be defensible regardless of sample length.

### What stays unchanged

- The maximal-GETS Williams CCI (current canonical) was never the
  subject of the placebo test and is unaffected.
- The Kalman state-space CCI is unaffected.
- The fit-decomposition finding (NS-108) — that adding CCI shifts wealth
  coefficients 150% under maximal-GETS, 16% under Kalman — is
  unaffected; both methods are doing identification work, not just
  detrending.
- Spec 1's structural wealth elasticity is **stable** between samples,
  validating the proxy.

---

## 3b. Maximal-GETS placebo on the extended sample

Implementation: [cci_placebo_maximal_gets_extended.R](../R/cci_placebo_maximal_gets_extended.R).
The §3 placebo tested the literal 4-knot Williams CCI. This companion
test asks the parallel question for the protocol that's actually
canonical in our pipeline: 15 candidate knot dates + 15 institutional
sign priors, sign-violators dropped, surviving combination defining
`cci_williams`.

The null is 200 draws of 15 random knots (uniform in 1979–2021, the
maximal-GETS window) plus 15 random ±1 priors, applying the same
drop-on-violation reduction. Same Spec 1 (aggregate proxy) on the
extended 1976Q3+ sample.

### Headline finding

| Metric                    | Canonical maximal-GETS | Placebo median | Canonical %ile |
|---------------------------|-----------------------:|---------------:|---------------:|
| adjusted R²               |                 0.6856 |         0.6833 |       **64th** |
| \|λ\|                      |                 0.2150 |         0.2230 |       **36th** |
| surviving knots           |               7 of 15 |              8 |              — |

**Verdict on the extended sample: WEAK SUPPORT — institutional choice
above median but not far.**

### What this tells us, contrasted with §3

| Specification                  | adj R² %ile | \|λ\| %ile | Verdict           |
|--------------------------------|------------:|----------:|-------------------|
| Literal Williams 4-knot (1988+)|        49th |      22nd | At placebo median |
| Literal Williams 4-knot (1976+)|        19th |      10th | Fails placebo     |
| Maximal-GETS canonical (1976+) |    **64th** |  **36th** | Weak support      |

The maximal-GETS protocol moves Williams from "clearly fails" (19th/10th
on the extended sample) to "weakly above median" (64th/36th). This is
a real lift — but the |λ| percentile is still below 50%, meaning random
combinations of 15 knots/priors actually produce *faster* mean
reversion than Williams' specific institutional choice in 64% of cases.

**Substantive interpretation.**

1. **The maximal-GETS protocol is doing identification work**, but
   most of its lift comes from the adaptiveness of the drop-on-violation
   reduction (15 candidate knots is a lot of flexibility) rather than
   from Williams' institutional choice of *which* knots and *which*
   priors to encode.
2. **Williams' institutional knowledge survives somewhat — but
   weakly.** The 64th-percentile R² is meaningful (institutional choice
   beats two-thirds of random combinations on fit) but doesn't reach
   the "STRONG SUPPORT" threshold (>90% on both metrics).
3. **The single-equation OLS framing limitation persists.** A 4-equation
   FIML system with cross-equation restrictions — Williams (2010)'s
   actual setup, NS-031 — would identify the CCI as a common factor in a
   way that single-equation OLS placebo tests cannot, regardless of
   whether the candidate set is 4 or 15. The placebo result here is
   consistent with that prior conclusion.

**For the WP, this is the right empirical sequence to report:**

- §5: literal 4-knot Williams fails the placebo on both samples
  (49th→19th deteriorating).
- §5: maximal-GETS protocol — the data-driven survival reduction — is
  what's canonical now, and rescues Williams from clear failure to
  weak support (64th/36th).
- §5: the residual placebo gap motivates the multi-equation extension
  (NS-031) as the methodologically appropriate next step.

This is a stronger empirical story than the original "we replicate
Williams on a short sample where his knots can't really be tested."
The back-extension lets the paper say: *the literal Williams replication
fails the falsification test on a sample that includes his own
deregulation episode; our maximal-GETS protocol partially rescues
identification; full rescue requires multi-equation FIML*.

---

## 3c. Spec 4 and Spec 6 on the extended sample (disaggregated wealth)

Implementation: [refit_spec46_extended.R](../R/refit_spec46_extended.R).
With the disaggregated wealth proxies (`ha_y_proxy`, `nla_y_proxy`,
`eq_y_proxy`, `super_y_proxy`) now in place, the entire Spec 4–7
battery can be refit on the 1976Q3+ window. This addresses the WP
§11 claim that the wealth-coefficient gap with Williams "could be
resolved by sample back-extension to ~1975."

### Spec 4 (disaggregated wealth, no CCI, no SR dynamics)

| Long-run coef           | Baseline 1988+ (n=146) | Extended 1976+ (n=190) | % change | Williams Table 1 |
|-------------------------|----------------------:|----------------------:|---------:|-----------------:|
| λ (ecm_lag)             |               −0.140 |               −0.193 |    +37.3 |           −0.286 |
| nla_y                   |               +0.035 |               −0.002 |    −106  |           +0.066 |
| eq_y                    |               −0.119 |               −0.104 |    −13.3 |           +0.013 |
| super_y                 |               +0.040 |               +0.024 |    −41.7 |           +0.013 |
| ha_y                    |               +0.068 |               +0.040 |    −41.6 |           +0.052 |
| ln_hp_over_y            |               −0.072 |               −0.028 |    −61.0 |          (n/a)   |
| real_rate               |               −0.0032 |               +0.0013 |    −141  |          (n/a)   |
| ln_yp_over_y            |               +1.07  |               +1.12  |    +4.33 |           +0.20  |
| adj R² / RMSE (× 1000)  |          0.729 / 7.97 |          0.679 / 8.08 |          |                 |

**Headline finding: the back-extension does NOT close the
wealth-coefficient gap with Williams.**

- λ moves from −0.140 → −0.193 (closer to Williams' −0.286, +37%
  improvement, but still 32% short).
- Wealth coefs `ha_y`, `super_y` get **smaller**, not larger; `nla_y`
  collapses to ~zero; `eq_y` retains its wrong sign.
- `ln_yp_over_y` remains huge (~+1.1) on both samples vs Williams' calibrated +0.20.

This **falsifies the §11 hypothesis** that the small wealth coefs
were primarily a sample-length issue. The binding constraint is *not*
the post-1988 sample window but the **single-equation OLS framing**.
Williams' values come from a 4-equation FIML system with cross-equation
sign restrictions and a different normalization; a single-equation OLS
of consumption on wealth ratios cannot recover them, regardless of
sample length.

### Spec 6 (preferred — disagg + post-2008 PI shift + SR dynamics)

| Result | Baseline 1988+ | Extended 1976+ |
|--------|---------------:|---------------:|
| n      |             86 |             86 |
| λ      |          −0.180|          −0.180|

**Spec 6 cannot be back-extended.** Its short-run regressors
(`d2_logcci_lag2`, `dd4_income`, `d2_log_unemp`,
`abs_income_resid`) include the credit-conditions variable which
depends on `cci_ratio` (= log of housing-loan-flow ratio, available
only 2002Q3+ from ABS 560101). So Spec 6 is bounded at 2002Q3+
regardless of the wealth-component proxy. n=86 in both samples;
identical coefficients.

To extend Spec 6, the short-run CCI variable would need to be replaced
with one that has a longer history — candidates: `housing_loan_flow`
log (depends on the same ABS 560101 series, same 2002Q3+ bound), or
a Δ²log of `credit_total_d02` (1976Q3+ but loses the FHB-share
identification), or pre-2002 use a constant 0 (essentially dropping the
SR-CCI channel pre-2002).

This is a separate methodological decision the user should make. As of
this report, the back-extension is "Spec 1 / Spec 4 only."

## 4. Implications for the WP

The findings sharpen rather than soften the methodological pivot
already documented in [cci_exploration.md §7](cci_exploration.md):

**(a) Section 5 ("Identification of credit conditions") should now
explicitly note that the literal 4-knot Williams specification fails
the random-knot placebo on both the 1988+ and 1976+ samples** — and
that this failure is one motivation for our methodological pivot to
the maximal-GETS CCI (which, by construction, doesn't presume specific
historical knot dates) and the Kalman state-space CCI (which extracts
a single latent factor without imposing knots at all).

**(b) Section 9 ("Williams comparison") can now report the structural
wealth elasticity from a 1977Q3+ aggregate-networth specification
(0.107) alongside the 1988Q3+ value (0.112).** That stability is the
substantive signal the back-extension was designed to produce.

**(c) The single-equation framing limitation is even more pointed.**
NS-031 (multi-equation LIVES) was already on the books as the
"big rebuild" that addresses Williams faithfully. The placebo result
on the extended sample is empirical confirmation that single-equation
OLS cannot recover Williams' specific knot identification, even with
the data Williams himself worked from.

---

## 5. Next steps

1. **Update WP §5 and §9** to reference the extended-sample placebo
   result and the proxy-validation finding. Suggested phrasing in
   §5: "The back-extension to 1976Q3 (NS-020 phase 1) does not rescue
   the literal 4-knot Williams CCI: the canonical knots remain at
   the 19th/10th percentile of random draws on the longer sample, a
   clearer failure than the 49th/22nd percentile result on the
   1988+ sample."
2. **Maximal-GETS placebo on the extended sample.** A natural
   follow-up: re-run the placebo with the maximal-GETS reduction
   protocol (15 candidate knots, sign-prior survival) on the extended
   sample. Hypothesis: the surviving knots from the data-driven search
   should outperform random draws by a much larger margin, because the
   search is precisely what the placebo is testing.
3. **Spec 8 (Williams CCI interactions) on the extended sample.** Spec
   8 uses the disaggregated wealth (bounded at 1988Q3+); to extend
   it, either build proxies for `nla_y, eq_y, super_y` or run a
   variant that uses the aggregate proxy.
4. **Document the proxy in the WP data appendix.** The aggregate
   networth proxy is a non-trivial contribution worth a paragraph
   in the appendix: it lets the WP make falsifiable claims about
   the literal Williams replication that the original 1988+ sample
   cannot.

---

**Outputs produced:**

- [outputs/spec1_extended_comparison.csv](../outputs/spec1_extended_comparison.csv)
- [outputs/australia_williams_knot_placebo_extended.csv](../outputs/australia_williams_knot_placebo_extended.csv)
- [outputs/australia_williams_knot_placebo_extended_summary.csv](../outputs/australia_williams_knot_placebo_extended_summary.csv)
- outputs/australia_williams_knot_placebo_extended_r2.png
- outputs/australia_williams_knot_placebo_extended_lambda.png
