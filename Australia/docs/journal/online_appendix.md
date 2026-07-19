# Online Appendix to [manuscript title TBD]

*Online appendix — not for print.*

This appendix supports the main text with (A) full data-construction
detail, (B) the complete twelve-specification battery, (C) the
robustness battery, (D) the credit-conditions-index (CCI) construction
and placebo battery, (E) the permanent-income construction, and (F) the
structural-parameter inference battery, including a bounds/stationarity
test and a nested bootstrap. Tables are numbered within each section (Table A1, A2,
… ; Table B1, B2, … ; and so on) and every table records its source
file(s) in the note beneath it.

**Contents**

- A. Data construction detail
- B. The twelve-specification battery
- C. Robustness battery
- D. Credit-conditions-index construction and placebo battery
- E. Permanent-income construction
- F. Structural-parameter inference: bounds test, ablation, Spec-11
  placebo, and the nested bootstrap

---

## A. Data construction detail

### A.1 Splice methodology — growth-rate chain-linking

For each adjacent pair of source layers, the splice anchors the level
at the first quarter where both series are non-missing, then
back-casts using the base series' own quarter-on-quarter growth rates:

```
chained[t] = overlay[t_anchor] × (base[t] / base[t_anchor])  for t < t_anchor
chained[t] = overlay[t]                                       for t >= t_anchor
```

This is the standard ABS chain-linking convention, used throughout in
preference to a mean-ratio splicing convention: it preserves the base
series' growth rates exactly and pins the level to the overlay series
at the join, so there is no level discontinuity at any join quarter —
including at the long TRYM/legacy house-price overlap and the
bridge/current house-price overlap, where a mean-ratio convention
would produce step jumps.

The relative house-price-to-income ratio used in estimation is

```
ln_hp_over_y = log(hpi × pop_millions / ydi_ann_nom)
```

i.e. the log of the nominal house-price index divided by nominal
annualised disposable income per capita. Because numerator and
denominator are both nominal, the consumption deflator cancels exactly
and the ratio is identical to the real house price over real income
per capita:

```
(hpi/defl) / (ydi_ann_nom/pop/defl) = hpi × pop_millions / ydi_ann_nom
```

This nominal/nominal (equivalently real/real) construction is the one
used throughout, in preference to a nominal/real construction: mixing
a nominal numerator with a real denominator (or vice versa) would
leave the economy-wide price level inside the ratio (correlation 0.98
with the consumption deflator), so the term would measure aggregate
inflation rather than relative house-price pressure. The nominal/
nominal construction avoids this by construction, since the deflator
cancels exactly.

### A.2 Credit conditions index — data sources

See §D for the full CCI construction and placebo battery. The two
observable/constructed proxies are: `cci_ratio` (log of housing-credit
flow, ABS Cat 5601.0, divided by an eight-quarter moving average of
nominal disposable income, available from 2002Q3) and `cci_williams`
(the smoothed-step spline described in §D). The first-home-buyer share
`fhb_share = fhb_loans / total_new_loans` is also constructed from ABS
Cat 5601.0 from 2002Q3.

### A.3 RBA D-tables (pre-1988 back-extension)

Three RBA historical statistical tables support the pre-1988 portion
of the back-extended sample.

**D03 — M3 (monetary aggregate).** `m3_aggregate` is loaded from
`d03hist.xlsx`, series `DMAM3N` (M3, original/not-seasonally-adjusted,
$ billion): total economy-wide currency plus transaction deposits plus
all other deposits at ADIs plus ADI-issued certificates of deposit.
Coverage: monthly, 1959Q3–2026Q1, aggregated to quarterly by the mean
of the three monthly observations. `DMAM3N` carries a definitional
series break at August 1976, where the level jumps by +14.25 log per
cent month-on-month against a mean monthly log change of 0.8 log per
cent over the full history. The break falls inside the opening
quarters of the 1976Q3+ back-extended spine: it makes the 1976Q4
quarterly log-difference of M3 (+9.5 log per cent) an outlier of
roughly 4.8 standard deviations of the series' quarterly-growth
distribution over the spine window, and the outlier propagates into
every M3-based back-extension proxy (`m3_household_proxy`,
`fin_deposits_proxy`, `nla_y_proxy`, `networth_y_proxy`) at the deepest
end of the back-extended sample. Back-extension results that lean on
the first spine quarters should be read with this break in mind. M3 is
the headline liquid-asset proxy for the pre-1988 portion of the
sample, where ABS sectoral household-balance-sheet deposits are not
available.

**D02 — total credit.** `credit_total_d02` is constructed by
growth-rate-splicing two RBA D02 series across the July 2019 RBA
conceptual reform: `DLCACN` (total credit, original, $ billion) for
1976Q3–2019Q2, and `DLCACSFN` (total credit including select financial
businesses, the post-2019 successor) for 2019Q3 onward. The two series
have no quarterly overlap, so the splice anchors levels at the
boundary (`first_post × pre[t] / last_pre`); the join is continuous in
level but the implicit growth rate at the boundary is exactly zero
(there is no overlap to estimate it from). This is total credit, not
housing-specific; the housing-specific D02 series (`DLCACOHN`,
`DLCACIHN`) only extends back to 1990Q1 in the current vintage.

**D01 — growth in selected financial aggregates.** Downloaded as
`d01hist.xlsx` (monthly growth rates of the aggregates whose levels
are in D02). Not needed for the current back-extension, since D02
already extends to 1976Q3, and is retained for future use.

### A.4 Pre-1978 labour force

A compiled historical labour-force dataset (188 quarterly rows,
1964Q3–2011Q2; sources: ABS Cat 6204.0 historical labour force
(archived), the ABS Year Book Australia, the Foster (1996) *Australian
Economic Statistics 1949–50 to 1996–97* compilation, and RBA
Occasional Paper No 8) provides total resident population, working-age
population (15–64), civilian labour force, and unemployed persons, all
in thousands; conceptually consistent with the current ABS Cat 6202.0
series (which begins February 1978) at the join.

The historic series are growth-rate-spliced onto the modern (1978+)
series at 1978Q1: `pop_15_64 → pop_millions` (the working-age
population denominator used in per-capita normalisation), `labour_force`
to the master `labour_force`, and `unemployed/labour_force × 100` to
the master `unemp_rate` (level replacement before 1978Q1). This splice
gives six quarters, 1976Q3–1977Q4, non-missing values for all
per-capita and labour-force-derived variables — the binding remaining
constraint for the 1976Q3 back-extension, since consumption, income,
mortgage rate, house prices, M3, total credit and the prime-age share
already extend to 1976Q3 on the raw download.

### A.5 Household-allocated M3 (`m3_household_proxy`)

The pre-1988 liquid-asset proxy allocates M3 to the household sector
via the wage share of GDP:

```
m3_household_proxy = m3_aggregate × wage_share / 100
```

The wage share (compensation of employees / GDP) is a documented
simplification for the back-extension: it captures most of household
factor income but omits mixed income and property income receivable
(together roughly ten percentage points, tracking wage share over
time). `wage_share` is taken from ABS Cat 5206.0 Table 24 (analytical
series A2302604K, 1959Q3–2024Q4; range 49–62 per cent over the sample,
from ~60 per cent in the 1970s to ~50 per cent today). Coverage of
`m3_household_proxy`: 1976Q3+, values $22bn (1976Q3) to $1,673bn
(2024Q4).

### A.6 Aggregate net-worth proxy (`networth_y_proxy`)

To enable the aggregate-net-worth specifications to fit on the
back-extended sample, an aggregate net-worth proxy combines M3
allocated to households with an `hpi × pop_millions` back-cast of
housing wealth, then growth-rate-splices that raw aggregate ratio onto
the official `networth_y` at 1988Q3 so that the proxy equals the
official series from 1988Q3 onward and back-casts smoothly through
1976Q3:

```
housing_wealth_proxy[t] = housing_wealth[1988Q3] × (hpi[t]/hpi[1988Q3])
                         × (pop_millions[t]/pop_millions[1988Q3])   for t < 1988Q3
                       = housing_wealth[t]                          for t >= 1988Q3

raw_proxy[t]  = (m3_household_proxy[t] × 1000 + housing_wealth_proxy[t]) / ydi_ann_nom[t]
scale         = networth_y[1988Q3] / raw_proxy[1988Q3]
networth_y_proxy[t] = networth_y[t]           for t >= 1988Q3
                     = raw_proxy[t] × scale    for t < 1988Q3
```

The `× 1000` term converts `m3_household_proxy` ($ billion, RBA D03
units) onto the $ million scale of `housing_wealth_proxy` and
`ydi_ann_nom`; without it the M3 term contributes roughly 0.01 per
cent of the numerator, rather than its intended ~12 per cent, and the
aggregate proxy collapses to a housing-only back-cast.

Caveats: the back-cast omits equities and superannuation (a small
household asset class pre-1992) and omits debt netting (mortgage debt
was a much smaller balance-sheet share in the 1970s than today); it is
used only for back-extension exercises, never as a substitute for the
official `networth_y` on the modern sample. Proxy values at key dates:
5.05× annual income (1976Q3), 4.71 (1980Q1), 4.78 (1985Q1), 5.37
(1988Q3, anchored to the official series), 10.19 (2024Q4) — consistent
with the historical Australian wealth-to-income path (flat through the
1970s/early 1980s, sharply rising post-1985 financial deregulation).

### A.7 Disaggregated wealth proxies

Four further proxies extend the disaggregated wealth components to
the back-extended 1976Q3+ sample. Each equals the official series for
$t \geq$ 1988Q3; for $t <$ 1988Q3 it back-casts via the most relevant
available aggregate.

- **`ha_y_proxy`** = `housing_wealth_proxy / ydi_ann_nom` (§A.6
  back-cast). Values: 2.68 (1976Q3) → 2.83 (1988Q3) → 6.41 (2024Q4).
- **`fin_deposits_proxy`** anchors `fin_deposits[1988Q3]` and grows by
  `m3_household_proxy`.
- **`fin_loans_proxy`** anchors `fin_loans[1988Q3]` and grows by
  `credit_total_d02` (§A.3).
- **`nla_y_proxy`** = `(fin_deposits_proxy − fin_loans_proxy) /
  ydi_ann_nom`. Values: +0.20 (1976Q3, households net liquid
  creditors) → −0.05 (1988Q3) → −0.72 (2024Q4, modern net-debtor
  position). The sign flip around 1988 captures the post-deregulation
  debt build-up — precisely the variation the credit-conditions
  channel is meant to explain but which the modern (1988Q3+) data
  window largely excludes.
- **`eq_y_proxy`** is held constant at its 1988Q3 value pre-1988
  (Australian household equity holdings were a small wealth share in
  the late 1970s/early 1980s). Value: 0.60 (constant) → 0.92 (2024Q4).
- **`super_y_proxy`** is a linear ramp from 0.1× the 1988Q3 value at
  1976Q3 to the 1988Q3 value, then official thereafter (the 0.1 anchor
  matches Williams' (2010) Table A.1 ballpark for the
  pre-Superannuation-Guarantee era). Values: 0.07 (1976Q3) → 0.66
  (1988Q3) → 2.44 (2024Q4).

These proxies are adequate for testing whether sample length is the
binding constraint on identifying the LIVES credit channels (§C.14–C.16
below show it is not) but should not be over-interpreted as
reconstructions of the 1970s household balance sheet: the equities
proxy is a constant-share assumption, the superannuation proxy a
linear ramp, and the deposits/debt proxies are aggregate-growth
back-casts rather than household-sector measurements. All headline
results use the **official** 1988Q3+ disaggregated series ($n=146$
full, $n=126$ pre-COVID); the proxies are confined to back-extension
robustness exercises.

**Coherence check.** At the 1988Q3 boundary, official `networth_y`
(broad, ABS closing net worth) = 5.37; the aggregate `networth_y_proxy`
= 5.37 by construction; the sum-of-disaggregated
`networth_y_disagg_proxy` (`ha_y_proxy + nla_y_proxy + eq_y_proxy +
super_y_proxy`) = 4.04. The roughly 25 per cent gap is the "other
wealth" component of ABS closing net worth (life-office reserves,
unincorporated business equity, etc.) absent from the narrow
definition.

### A.8 Master variable coverage tiers

**Table A1 — Master variable coverage under the 1976Q3+ spine.**
Source: model dataset build log.

| First non-NA | n | Variables (selected) |
|---|---:|---|
| 1976Q3 | ~56 | cons, ydi, hpi, mortgage_rate, M3, total credit, prime_age_share, all dummies, m3_household_proxy, ha_y_proxy, nla_y_proxy, eq_y_proxy, super_y_proxy, networth_y_proxy, ln_networth_y_proxy, cci_kalman, cons_real_pc, ydi_real_pc, npy_real_pc, labour_force, unemp_rate, lf_share, pop_millions, ln_cons_real_pc, ln_ydi_real_pc, ln_hp_over_y, ecm_lag (1976Q4) |
| 1977Q3 | 2 | real_rate, hicp_4q_ann (4-quarter CPI lag) |
| 1978Q2 | 1 | ydi_ann_8qma (8-quarter MA) |
| 1988Q3 | ~21 | official disaggregated wealth (ha_y, eq_y, super_y, ilfa_y = eq_y + super_y, nla_y, networth_y, debt_y), housing_wealth, fin_deposits/equities/super/loans (ABS 5232) — n = 146 |
| 2002Q3 | 5 | cci_ratio, fhb_share, housing_loan_flow, fhb_loans, non_fhb_loans — n = 90 |
| 2009Q1 | 2 | mortgage_interest_burden_rba, mortgage_payment_burden_rba (RBA E13) — n = 64 |

The 1988Q3 tier binds the faithful LIVES specification ($n=146$ full,
$n=126$ pre-COVID); the 2002Q3 tier binds the conventional baseline
($n=86$ full, $n=66$ pre-COVID). The combined illiquid financial ratio
`ilfa_y` first becomes available at 1988Q3, alongside its `eq_y` and
`super_y` constituents.

---

## B. The twelve-specification battery

**Table B1 — Specification number → manuscript name (the three named
specifications).**

| Spec | Manuscript name | Role |
|---|---|---|
| 6 | Conventional constant-MPC disaggregated ECM | Baseline (retained for comparability with prior literature) |
| 11 | Faithful LIVES specification | Headline (narrative lead) |
| 12 | Williams-calibration-imposed LIVES | Negative control (shows what fails to transfer) |

The ladder also includes a long-history variant (Spec 6b) and a
measured-burden variant (Spec 7b), giving fourteen fitted forms in
total across the two named "6/6b" and "7/7b" pairs plus Specs 1–5 and
8–12.

### B.1 The fourteen-specification ladder

**Table B2 — The specification ladder.** Each step adds one structural
element: Specs 1–3 are aggregate-net-worth error-correction models;
Specs 4–7b disaggregate wealth and add cohort/burden terms; Specs 8–9
introduce the CCI interactions (spline and Kalman extractions); Specs
10–12 are the Williams-aligned forms.

| Spec | Description | Long-run regressors / notes |
|---|---|---|
| 1 | Aggregate net worth | `ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 2 | Spec 1 + short-run CCI | adds Δ²log CCI lag 2 to short-run set |
| 3 | Net worth in levels | replaces `ln_networth_y` with `networth_y` |
| 4 | Disaggregated wealth | adds `nla_y, eq_y, super_y, ha_y`; drops aggregates |
| 5 | Spec 4 + full short-run dynamics | adds Δ²log CCI, ΔΔ₄income, Δ²log unemp, \|ε̂\| |
| **6** | **Conventional constant-MPC disaggregated ECM (baseline)** | Spec 5 + post-2008 PI break `ln_yp_over_y_post2008`; plain `ha_y/eq_y/super_y/nla_y`, CCI short-run only |
| 6b | Spec 6 with back-extension-compatible SR CCI | replaces Δ²log CCI with Δ²log RBA D02 credit; disaggregated wealth proxies; fits on n = 180 |
| 7 | Spec 6 + cohort terms + synthetic burden | adds `prime_age_share, fhb_share` |
| 7b | Spec 7 with RBA E13 measured burden | post-2009 sample only |
| 8 | Williams CCI interactions (free) | plain `real_rate`/`ln_hp_over_y` replaced by `r×CCI` and `log(HP/y)×(1−1.2·CCI)`, plus `ha_x_cci` and `log(y^p/y)×CCI`; plain `ha_y` retained |
| 9 | Spec 8 with Kalman state-space CCI | mirrors Spec 8's interaction set (incl. `ha_x_cci_k`) with `cci_kalman` in place of the spline |
| 10 | Williams-prior calibrated | γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93, ϖ = 1.2; iterative fixed-point OLS |
| **11** | **Faithful LIVES (free, headline)** | housing via `ha_x_cci` only; ζ_c·CCI intercept `cci_williams` restored; IFA combined (`ilfa_y`); `nla_y`, `hp_x_1_minus_cci`, `r_x_cci`, `ln_yp_over_y`, `yp_x_cci` |
| **12** | **Williams-calibration-imposed LIVES** | Spec 11 form with ψ₀ = 0.20, ψ₁ = 0.93, γ_IFA = 0.022 imposed via iterative fixed-point |

The structural difference between Spec 6 and Spec 11 is decisive for
the paper's central result. Spec 6 carries wealth as plain, constant
marginal propensities and lets the credit-conditions index enter only
as a short-run term. Under the LIVES theory the housing marginal
propensity to consume is zero at CCI = 0 and is unlocked only as
credit conditions ease, so housing enters Spec 11 only via the
interaction term `ha_x_cci`, the autonomous-consumption CCI loading
ζ_c (`cci_williams`) is restored, and the two illiquid-financial
components are combined into `ilfa_y` as the theory specifies. Source:
specification definitions; coefficient outputs in Table B4–B11 below.

### B.2 The four selection screens

Each estimable specification is screened on four criteria, with BIC as
tiebreak (source: specification-selection output):

1. **Sign screen** — every long-run coefficient with a non-ambiguous
   theoretical prior has the correct sign.
2. **Cointegration screen** — an Engle–Granger residual ADF test
   rejects the no-cointegration null at 5 per cent against MacKinnon
   (1991, 2010) critical values keyed to the regressor count. A
   Johansen trace statistic on one fixed trivariate subsystem
   (log consumption, log income, and either `ln_networth_y` or `ha_y`;
   $K=2$, restricted constant) is reported alongside but tests only the
   $r=0$ null of that small common subsystem, not each specification's
   own long run.
3. **Speed-of-adjustment screen** — λ has the correct (negative) sign
   and lies in $(0.02, 0.30)$.
4. **Stability screen** — Chow at 2008Q3 is not rejected at the 1 per
   cent level, and λ is sign-stable across at least 3 of the 4 sample
   variants (full, pre-COVID, COVID-dropped, COVID-rich-dummies). Where
   the standard `strucchange::sctest` design is singular, a manual
   common-coefficient Chow F-test is substituted (recorded in a
   `chow_method` field); a Chow that is incomputable even after the
   fallback is treated as neutral, not as a failure.

The cointegration battery covers Spec 8 and Spec 11 directly (both
fail, like every other specification: Spec 11 EG-ADF −3.13 against a
MacKinnon 5 per cent critical value of −5.47 for nine regressors; Spec
8 −3.40 against −5.70 for ten). Spec 9 is skipped (its de-meaned
Kalman interactions are constructed locally and not available to the
static long-run regression) as are Specs 10 and 12 (their long run is
a calibrated offset, not a freely estimated static regression, so an
Engle–Granger residual test is not applicable — see Table F3/F4 for
the bounds-test alternative applied to Specs 6, 11 and 12). The
speed-of-adjustment upper bound (|λ| < 0.30) binds for Specs 8 and 11
on the full sample (|λ| = 0.458 and 0.448); both are correctly signed,
strongly significant and sign-stable across all four sample variants,
and their pre-COVID λ (−0.219, −0.266) lies inside the screen
interval.

### B.3 Selector outcome

**Table B3 — Four-screen outcome card.** Source: specification-
selection output; BIC is the Schwarz criterion.

| Spec | Signs | Coint | λ | Stability | BIC |
|---|:-:|:-:|:-:|:-:|---:|
| 1 | ✓ | ✗ | ✓ | ✓ | −919.3 |
| 2 | ✗ | ✗ | ✓ | ✓ | −501.7 |
| **3** (selector-preferred) | **✓** | ✗ | **✓** | **✓** | **−919.4** |
| 4 | ✗ | ✗ | ✓ | ✓ | −905.3 |
| 5 | ✗ | ✗ | ✓ | ✓ | −494.6 |
| 6 (conventional baseline) | ✗ | ✗ | ✓ | ✓ | −492.5 |
| 6b | ✓ | ✗ | ✓ | ✗ | −1114.0 |
| 7 | ✗ | ✗ | ✗ | ✓ | −500.6 |
| 7b | ✗ | ✗ | ✗ | ✓ | −364.5 |
| 8 | ✗ | ✗ | ✗ | ✓ | −952.8 |
| 9 | ✗ | NA | ✓ | ✗ | −890.6 |
| 10 | ✓ | NA | ✓ | ✗ | −493.2 |
| **11** (faithful LIVES, headline) | **✓** | ✗ | ✗ | ✗ | **−954.8** |
| 12 | ✓ | NA | ✓ | ✗ | −893.8 |

Under the canonical setting no specification passes all four screens;
the selector falls back to a most-passes rule with BIC tiebreak. Spec
1 and Spec 3 each pass three screens, and Spec 3 (aggregate net worth
in levels) is the automated pick (−919.4 against Spec 1's −919.3) — a
conservative, non-LIVES form. Spec 6b carries the lowest raw BIC only
because it is fitted on the longest ($n=190$) back-extended sample and
is not directly comparable to the $n=146$/$n=86$ forms. Spec 11 (the
faithful LIVES form) carries the best BIC of all fourteen
specifications; what stands between it and the automated pick is the
conservative |λ| ceiling (breached by the COVID-inflated full-sample
estimate) and the 2008Q3 Chow test. The manuscript leads with Spec 11
on theoretical-form grounds, retains Spec 6 as the conventional
baseline, and carries Spec 12 (and its independent reproduction, Spec
10) as the negative control.

### B.4 Headline results — Spec 11 (faithful LIVES)

**Table B4 — Spec 11 full-sample long-run coefficients.** Full sample
1988Q3–2024Q4, $n=146$, adj-$R^2 = 0.824$. Structural $\gamma =$
OLS/|λ|.

| Term | OLS coef | t-stat | Implied γ | Williams |
|---|---:|---:|---:|---:|
| `ha_x_cci` (γ₁, housing × CCI) | +0.0025 | +0.71 | 0.005 | 0.049 |
| `nla_y` (γ_NLA) | +0.0269 | +3.75 | 0.060 | 0.159 |
| `ilfa_y` (γ_IFA) | +0.0155 | +3.09 | 0.035 | 0.022 |
| `cci_williams` (ζ_c) | +0.0001 | +0.01 | 0.000 | 0.190 |
| `hp_x_1_minus_cci` (α₄) | +0.0279 | +3.08 | 0.062 | −0.130 |
| `r_x_cci` (α₁) | +0.0028 | +4.55 | 0.006 | −0.871 |
| `ln_yp_over_y` (ψ₀) | +0.459 | +4.04 | 1.024 | 0.20 |
| `yp_x_cci` (ψ₁) | −0.510 | −1.51 | −1.138 | 0.93 |
| **`ecm_lag` (λ)** | **−0.448** | **−3.57** | (= 1) | −0.286 |

**Table B5 — Spec 11 across the four sample treatments.** Newey–West
HAC $t$-statistics in parentheses.

| Variant | n | λ (t) | `nla_y` (t) | `ilfa_y` (t) | `ha_x_cci` (t) | `ln_yp_over_y` (t) |
|---|---:|---:|---:|---:|---:|---:|
| Full sample | 146 | −0.448 (−3.6) | +0.0269 (3.8) | +0.0155 (3.1) | +0.0025 (0.7) | +0.459 (4.0) |
| Pre-COVID (to 2019Q4) | 126 | −0.266 (−4.8) | +0.0159 (1.8) | +0.0093 (1.7) | +0.0019 (0.9) | +0.298 (5.8) |
| COVID quarters dropped | 138 | −0.248 (−6.7) | +0.0170 (2.1) | +0.0098 (2.0) | +0.0023 (1.1) | +0.281 (8.7) |
| Quarterly COVID dummies | 146 | −0.242 (−6.2) | +0.0141 (3.1) | +0.0084 (3.3) | +0.0016 (0.8) | +0.278 (8.2) |

The full-sample λ (−0.448) is inflated by the COVID quarters — the
three pulse dummies are demonstrably insufficient, and replacing them
with quarterly dummies halves |λ|. The identified speed of adjustment
is λ ≈ −0.25 (pre-COVID/COVID-dropped/COVID-rich cluster,
$t$-ratios −4.8 to −6.7), within roughly 7 per cent of Williams' −0.286;
the net-liquid and illiquid-financial m.p.c.s are correctly signed in
every variant and significant at 5 per cent in three of the four.
Three qualifiers: (i) the housing-collateral interaction `ha_x_cci` is
correctly signed but insignificant in every variant (implied peak
m.p.c. 0.0055 against Williams' 0.049); the real-rate interaction is
wrong-signed and significant in every variant; the affordability
interaction is wrong-signed on the full sample; `yp_x_cci` is
wrong-signed full-sample but flips to the theory-correct sign
pre-COVID and COVID-dropped. (ii) The permanent-income gearing
ψ̂ = OLS/|λ| is 1.02 (full) to 1.13 (COVID-controlled) —
above the admissibility bound ψ ≤ 1 − η ≈ 0.95 (§E, §F). (iii) The
wealth-magnitude comparison with Williams is governed by the nested
intervals of Table F7, under which neither magnitude can be
distinguished from Williams' values (the fixed-regressor comparison of
Table F2 is superseded).

### B.5 The calibration route (Spec 12, Spec 10)

Spec 12 imposes Williams' scale-robust calibrations (γ_IFA = 0.022,
ψ₀ = 0.20, ψ₁ = 0.93) via an iterative fixed-point offset and frees
only the housing-collateral m.p.c., the net-liquid m.p.c. and λ. His
real-rate, affordability and intercept loadings cannot be imposed at
their published magnitudes without diverging the fixed point (the
percent-scaled real rate against a unit-normalised CCI makes his
α₁ = −0.871 roughly thirty times too large); the CCI-support problem
compounds this — on the deployed index (range [−2.1, 1] versus
Williams' [0, 0.8]) the imposed gearing ψ(CCI) = 0.20 + 0.93·CCI is
negative in the post-2019 regime and breaches Williams' own ψ ≤ 0.95
cap at the peak.

**Result: imposing Williams' permanent-income calibration collapses
the error-correction to λ ≈ 0** (Spec 12: λ = −0.030, $t=-0.74$,
flipping sign pre-COVID; independently reproduced by Spec 10,
λ = −0.048, $t=-0.78$, which keeps the rate and affordability channels
free). The Australian data freely estimate a structural
permanent-income gearing of order one — several times Williams' 0.20 —
so forcing his value injects a large, mis-signed contribution that
destroys the equilibrium. The LIVES *structure* transfers; Williams'
Australian *calibrations* do not.

### B.6 Baseline results — Spec 6 (conventional)

**Table B6 — Spec 6 long-run coefficients.** Full sample
1988Q3–2024Q4, $n=86$ (binding constraint: `cci_ratio` from ABS Cat
5601.0, available from 2002Q3).

| Term | OLS coef | NW SE | t-stat | Implied γ | Sign OK |
|---|---:|---:|---:|---:|:-:|
| `ha_y` | +0.0022 | 0.0076 | +0.30 | +0.009 | ✓ |
| `nla_y` | +0.0083 | 0.0351 | +0.24 | +0.035 | ✓ |
| `eq_y` | −0.0156 | 0.0517 | −0.30 | −0.065 | ✗ |
| `super_y` | +0.0060 | 0.0091 | +0.66 | +0.025 | ✓ |
| `ln_hp_over_y` | +0.0102 | 0.0444 | +0.23 | +0.043 | n/a |
| `real_rate` | −0.00053 | 0.0011 | −0.46 | −0.0022 | ✓ |
| `ln_yp_over_y` | +0.3253 | 0.2173 | +1.50 | +1.363 | n/a |
| `ln_yp_over_y_post2008` | +0.1704 | 0.1965 | +0.87 | +0.714 | n/a |
| **`ecm_lag` (λ)** | **−0.2386** | **0.0935** | **−2.55** | (=1) | ✓ |

(Short-run regressors and event dummies omitted; full vector in
Table B10.)

Speed of adjustment: λ = −0.239 ($t=-2.55$, $p=0.013$), about 83 per
cent of Williams' −0.286, but significance leans on the COVID quarters
— the pre-COVID estimate collapses to −0.087 ($t=-0.79$), against
Spec 11's pre-COVID −0.266 ($t=-4.85$). Housing wealth: OLS +0.0022
($t=0.30$), statistically indistinguishable from zero as a standalone
level — the coefficient the theory predicts to be ≈ 0 absent the CCI
interaction. Net liquid assets: OLS +0.0083 ($t=0.24$), implied
γ_NLA = 0.035, about a fifth of Williams' 0.159, correctly signed; the
γ_LA + γ_LOANS = 0 restriction is accepted at 5 per cent (Table C5).
Illiquid financial wealth decomposes into equities (γ = −0.065, wrong
sign, $t=-0.30$) and superannuation (γ = +0.025, $t=0.66$); the
combined γ_IFA = −0.040 is wrong-signed, dragged below zero by the
equities component — a small-sample artefact of the disaggregated
split that Spec 11's combined `ilfa_y` removes (+0.0155, $t=3.09$).
Permanent income: base coefficient +0.325 ($t=1.50$) plus a
post-2008 break of +0.170 ($t=0.87$), neither individually significant;
implied structural gearing at CCI = 0 is 1.36, well above Williams'
calibrated 0.20. λ is sign-stable across all four sample variants
(full −0.239, pre-COVID −0.087, COVID-dropped −0.162, COVID-rich
−0.177).

### B.7 Diagnostics summary — all fourteen specifications

**Table B7 — Diagnostics, full sample.** AR(1)/AR(4) are
Breusch–Godfrey $p$-values; "Het $p$" is the Breusch–Pagan-type LM
$p$-value with the `het_diagnosis` field in parentheses. A `chow_method`
field (not shown) records whether the 2008Q3 stability statistic used
`strucchange::sctest` or a manual split-sample Chow F-test where the
standard design was singular.

| Spec | n | adj R² | DW | AR(1) p | AR(4) p | RESET p | Het p (diagnosis) | BIC |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 (LogNetWorth) | 146 | 0.731 | 2.32 | 0.026 | 0.093 | <0.001 | <0.001 (struct) | −919.3 |
| 2 (LogNetWorth_CCI) | 86 | 0.772 | 2.44 | 0.020 | 0.165 | 0.003 | <0.001 (struct) | −501.7 |
| 3 (LevelNetWorth) | 146 | 0.731 | 2.32 | 0.025 | 0.088 | <0.001 | <0.001 (struct) | −919.4 |
| 4 (Disagg_NoCCI) | 146 | 0.726 | 2.32 | 0.018 | 0.067 | <0.001 | <0.001 (struct) | −905.3 |
| 5 (FullDisagg) | 86 | 0.802 | 2.25 | 0.107 | 0.087 | <0.001 | <0.001 (struct) | −494.6 |
| 6 (conventional baseline) | 86 | 0.804 | 2.15 | 0.339 | 0.301 | <0.001 | <0.001 (struct) | −492.5 |
| 6b (LongHistSRCCI) | 180 | 0.703 | 2.12 | 0.275 | 0.134 | <0.001 | <0.001 (struct) | −1114.0 |
| 7 (CohortBurden) | 86 | 0.834 | 2.21 | 0.203 | 0.050 | 0.018 | 0.002 (struct) | −500.6 |
| 7b (RBABurden) | 64 | 0.872 | 2.16 | 0.327 | 0.004 | 0.109 | 0.003 (struct) | −364.5 |
| 8 (CCI_Interactions) | 146 | 0.827 | 1.92 | 0.494 | 0.057 | 0.001 | <0.001 (struct) | −952.8 |
| 9 (KalmanCCI) | 146 | 0.735 | 2.18 | 0.140 | 0.018 | <0.001 | <0.001 (struct) | −890.6 |
| 10 (WilliamsPrior) | 86 | 0.773 | 2.22 | 0.186 | 0.142 | 0.035 | <0.001 (struct) | −493.3 |
| **11 (LIVES_Headline)** | 146 | 0.824 | 1.90 | 0.448 | 0.055 | 0.001 | <0.001 (struct) | **−954.8** |
| 12 (LIVES_Calibrated) | 146 | 0.687 | 2.14 | 0.317 | 0.010 | 0.017 | <0.001 (struct) | −893.8 |

Heteroscedasticity is structural in every full-sample specification —
the LM rejection survives dropping the event quarters in every case —
so Newey–West HAC standard errors are used throughout. RESET rejects
in every specification bar the short-sample Spec 7b ($p=0.109$); the
faithful Spec 11 is not exempt ($p=0.001$). Low-order serial correlation (Breusch–Godfrey
AR(1)) is concentrated in the aggregated and disaggregated-no-CCI
forms (Specs 1–4); at fourth order the rejections sit instead in
Specs 7, 7b, 9, 12, with Specs 8 and 11 borderline ($p=0.057$,
$0.055$). The headline LIVES form (Spec 11) carries the best BIC of
any $n=146$ specification, including the over-parameterised Spec 8
(−954.8 vs −952.8).

### B.8 Comparison with Williams (2010, 2012) — conventional baseline

Williams reports structural long-run coefficients γ; the OLS
coefficients relate to those γ by the ECM identity
OLS_coef = λ × γ, so reporting both forms separates the two channels.

**Table B8 — Spec 6 vs Williams' Table 1.**

| Term | Williams γ | Williams implied OLS | Our OLS | Our γ | OLS gap | γ gap |
|---|---:|---:|---:|---:|---:|---:|
| **λ** | **−0.2860** | (same) | **−0.2386** | (same) | **−17 %** | (same) |
| Housing wealth `ha_y` | 0.0488 | 0.0140 | 0.0022 | 0.0094 | −84 % | −81 % |
| Illiquid `eq_y + super_y` | 0.0220 | 0.0063 | −0.0096 | −0.0403 | wrong sign | wrong sign |
| Net liquid `nla_y` | 0.1590 | 0.0455 | 0.0083 | 0.0350 | −82 % | −78 % |
| log(HP/y) | −0.1300 | −0.0372 | +0.0102 | +0.0429 | wrong sign | wrong sign |
| ψ at CCI = 0 | 0.2000 | 0.0572 | 0.3253 | 1.3632 | — | — |

Williams' γ_HA = 0.0488 is the paper's derived peak housing MPC, not
the raw Table 1 coefficient (γ₁ = 0.0606); γ_NLA and γ_IFA are Table
1's raw coefficients. Williams' Table 1 appears in the full working-
paper version of Muellbauer and Williams (2012) (CEPR Discussion Paper
8386, revised 12 April 2012); the seven-page *BIS Papers* No. 64
chapter is a condensed version that does not itself contain Table 1.

The speed of adjustment is the coefficient that agrees with Williams
(−0.239 sits 17 per cent below his −0.286, well inside one standard
error); the disaggregated wealth OLS coefficients sit roughly 80 per
cent below his implied OLS values, and because the λ gap is small the
ECM identity does not rescale the wealth deficits away. Two readings
discipline this: none of the wealth gaps is sharply estimated (the
Spec 6 delta-method intervals in Table F1 are wide enough to contain
Williams' value and zero for every wealth coefficient), and Spec 6 is
not itself the LIVES equation — its housing channel is a standalone
level the theory predicts to be ≈ 0, so neither a match nor a miss
carries structural content. Spec 11 (Table B4, Table F2) is the
comparison that carries structural weight.

### B.9 λ across all fourteen specifications

**Table B9 — Speed of adjustment λ (`ecm_lag`), full and pre-COVID
samples.**

| Specification | λ full (n=146) | λ pre-COVID (n=126) |
|---|---|---|
| Spec 1 | −0.193 (t=−2.88)*** | −0.161 (t=−3.32)*** |
| Spec 2 | −0.186 (t=−2.07)** | −0.080 (t=−1.15) |
| Spec 3 | −0.191 (t=−2.90)*** | −0.158 (t=−3.17)*** |
| Spec 4 | −0.182 (t=−2.45)** | −0.119 (t=−1.95)* |
| Spec 5 | −0.235 (t=−2.47)** | −0.061 (t=−0.62) |
| Spec 6 | −0.239 (t=−2.55)** | −0.087 (t=−0.79) |
| Spec 6b | −0.248 (t=−3.96)*** | −0.240 (t=−4.61)*** |
| Spec 7 | −0.341 (t=−2.76)*** | −0.054 (t=−0.43) |
| Spec 7b | −0.381 (t=−2.43)** | −0.061 (t=−0.93) |
| Spec 8 | −0.458 (t=−3.52)*** | −0.219 (t=−3.95)*** |
| Spec 9 | −0.209 (t=−3.24)*** | −0.158 (t=−2.86)*** |
| Spec 10 | −0.048 (t=−0.78) | −0.025 (t=−0.57) |
| Spec 11 | −0.448 (t=−3.57)*** | −0.266 (t=−4.85)*** |
| Spec 12 | −0.030 (t=−0.74) | +0.041 (t=+2.03)** |

*** sig 1%, ** sig 5%, * sig 10%. Several specifications estimate on
different windows from the nominal $n=146$/$n=126$: Specs 2, 5, 6, 7
and 10 on $n=86$ because a 2002Q3-binding credit term shortens their
sample, Spec 7b on $n=64$, and Spec 6b on the $n=180$ back-extended
sample.

### B.10 Long-run coefficient matrix — five disaggregated specifications

**Table B10 — Long-run coefficients, full sample.** "—" = term not in
that specification. *** sig 1%, ** sig 5%, * sig 10%.

| Term | Spec 4 | Spec 5 | Spec 6 | Spec 8 | Spec 11 |
|---|---:|---:|---:|---:|---:|
| `nla_y` | +0.0045 (+0.39) | −0.0141 (−0.43) | +0.0083 (+0.24) | +0.0337 (+4.39)*** | +0.0269 (+3.75)*** |
| `eq_y` | −0.0079 (−0.41) | −0.0295 (−0.57) | −0.0156 (−0.30) | −0.0014 (−0.11) | — |
| `super_y` | +0.0025 (+0.36) | +0.0040 (+0.37) | +0.0060 (+0.66) | +0.0215 (+2.42)** | — |
| `ilfa_y` | — | — | — | — | +0.0155 (+3.09)*** |
| `ha_y` | +0.0035 (+0.53) | −0.0028 (−0.30) | +0.0022 (+0.30) | +0.0030 (+0.48) | — |
| `ha_x_cci` | — | — | — | +0.0043 (+0.87) | +0.0025 (+0.71) |
| `ln_hp_over_y` | +0.0038 (+0.13) | +0.0447 (+0.83) | +0.0102 (+0.23) | — | — |
| `hp_x_1_minus_cci` | — | — | — | +0.0299 (+1.38) | +0.0279 (+3.08)*** |
| `real_rate` | −0.0002 (−0.33) | −0.0014 (−0.87) | −0.0005 (−0.46) | — | — |
| `r_x_cci` | — | — | — | +0.0028 (+2.05)** | +0.0028 (+4.55)*** |
| `cci_williams` | — | — | — | — | +0.0001 (+0.01) |
| `ln_yp_over_y` | +0.2026 (+1.94)* | +0.3316 (+1.43) | +0.3253 (+1.50) | +0.4921 (+3.42)*** | +0.4591 (+4.04)*** |
| `ln_yp_over_y_post2008` | — | — | +0.1704 (+0.87) | — | — |
| `yp_x_cci` | — | — | — | −0.5046 (−2.43)** | −0.5101 (−1.51) |
| `ecm_lag` | −0.1824 (−2.45)** | −0.2353 (−2.47)** | −0.2386 (−2.55)** | −0.4583 (−3.52)*** | −0.4483 (−3.57)*** |

### B.11 Spec 11 vs Spec 12 — selected coefficients

**Table B11 — Faithful LIVES (Spec 11) vs Williams-calibration-imposed
(Spec 12), full and pre-COVID samples.** Convention: λ = `ecm_lag`
(negative = error-correction); structural γ = OLS/|λ|. *** sig 1%,
** sig 5%, * sig 10%.

| Term | Spec 11 full (n=146) | Spec 11 pre-COVID (n=126) | Spec 12 full (n=146) | Spec 12 pre-COVID (n=126) |
|---|---|---|---|---|
| λ (`ecm_lag`) | −0.448 (t=−3.57)*** | −0.266 (t=−4.85)*** | −0.030 (t=−0.74) | +0.041 (t=2.03)**, sign flip |
| `nla_y` | +0.0269 (t=3.75)*** | +0.0159 (t=1.81)* | +0.0011 (t=0.39) | −0.0002 (t=−0.09, wrong sign) |
| `ilfa_y` (=eq+super) | +0.0155 (t=3.09)*** | +0.0093 (t=1.74)* | imposed (γ=0.022) | imposed (γ=0.022) |
| `ha_x_cci` (γ₁) | +0.0025 (t=0.71) | +0.0019 (t=0.86) | −0.0012 (t=−0.88) | +0.0018 (t=1.52) |
| `hp_x_1_minus_cci` | +0.0279 (t=3.08)*** | +0.0137 (t=1.10) | — | — |
| `r_x_cci` | +0.0028 (t=4.55)*** | +0.0022 (t=3.47)*** | — | — |
| `cci_williams` (ζ_c) | +0.0001 (t=0.01) | +0.0199 (t=2.77)*** | — | — |
| `ln_yp_over_y` | +0.4591 (t=4.04)*** | +0.2982 (t=5.81)*** | imposed (ψ₀=0.20) | imposed (ψ₀=0.20) |
| `yp_x_cci` | −0.5101 (t=−1.51) | +0.1869 (t=1.80)* | imposed (ψ₁=0.93) | imposed (ψ₁=0.93) |
| Intercept | −0.0139 (t=−2.13)** | −0.0099 (t=−1.54) | +0.0078 (t=1.81)* | +0.0048 (t=2.13)** |
| adj-R² | 0.824 | 0.239 | 0.687 | 0.073 |
| SE (%) | 0.683 | 0.510 | 0.911 | 0.565 |
| BIC | −954.75 | −900.15 | −893.79 | −896.61 |

The contrast is the structure-transfers-but-calibrations-do-not
result: imposing Williams' Australian calibrations via the iterative
fixed point collapses the error-correction speed from −0.448 to a
statistically insignificant −0.030, and flips it to the wrong sign and
significant +0.041 ($t=2.03$) pre-COVID.

---

## C. Robustness battery

The robustness suite runs on the automated-selector specification
(Spec 3), on the conventional baseline (Spec 6) where the battery
requires the disaggregated wealth split, and — in parallel — on the
faithful LIVES specification (Spec 11).

### C.1 OLS vs IV on current income

Because current income enters both the error-correction term and the
permanent-income gap, both regressors are instrumented (income lagged
one, two and four quarters, unemployment lagged one and two quarters,
mortgage rate lagged one quarter — six instruments; both OLS and IV
columns carry Newey–West HAC standard errors).

| Frame | λ (OLS) | λ (IV) | Change | ln_yp_over_y (OLS) | ln_yp_over_y (IV) |
|---|---:|---:|---:|---:|---:|
| Spec 3, n=146 | −0.191 | −0.258 (SE 0.110) | +35% | +0.196 | +0.280 (+43%) |
| Spec 11, n=146 | −0.448 | −0.503 | +12% | — | — |

Spec 3 diagnostics: weak-instrument $F=73.6$ (`ln_yp_over_y`), 28.9
(`ecm_lag`); Wu–Hausman does not reject OLS exogeneity at 5 per cent
($p=0.095$); Sargan does not reject instrument validity at 5 per cent
($p=0.074$). Spec 11 diagnostics (endogenous set: `ecm_lag`,
`ln_yp_over_y`, `yp_x_cci`, `ha_x_cci`, `hp_x_1_minus_cci`; one
overidentifying restriction): Wu–Hausman rejects exogeneity
($p=0.002$); weak-instrument $F=126.0$ (`ln_yp_over_y`), 55.5
(`ha_x_cci`), 32.5 (`ecm_lag`), 15.2 (`hp_x_1_minus_cci`), but only 6.5
for `yp_x_cci` (below the conventional weak-instrument threshold);
`nla_y` attenuates to insignificance (0.0269 → 0.0040, $t\approx0.3$);
`ha_x_cci` and `hp_x_1_minus_cci` flip sign relative to OLS
(+0.0025→−0.013; +0.028→−0.015); Sargan $p=0.251$ (near-vacuous at
$df=1$). The Spec 11 IV design is underpowered: current-income
endogeneity is a live concern (Wu–Hausman), but the wealth-channel
point estimates cannot bear weight under instrumentation.

### C.2 Joint permanent-income + consumption SUR

Estimating the consumption equation jointly with a one-step-ahead
income-growth forecasting equation by SUR leaves every coefficient
within sampling noise of single-equation OLS.

| Frame | λ (OLS) | λ (SUR) | Δ% | ln_yp_over_y Δ% |
|---|---:|---:|---:|---:|
| Spec 3, n=146 | −0.191 | −0.204 | +7.0% | −4.5% |
| Spec 11, n=146 | −0.448 | −0.457 | +1.9% | −1.4% (`nla_y` −0.2%, `ilfa_y` +2.6%) |

The second equation is a one-step-ahead forecast, not the $k=40$-
quarter discounted projection that defines the permanent-income
regressor, so the SUR tests residual covariance, not generated-
regressor bias (the Table F1/F2 caveat applies). Single-equation OLS
is an acceptable estimator for the consumption block; the case for the
multi-equation framework rests on cross-equation parameter
restrictions, not on residual covariance or efficiency gain (§D.4,
§C.13).

### C.3 Chow battery and multiple-break tests

Chow tests on Spec 3 do not reject stability at 1995Q1 (stat 0.380,
$p=0.978$), 2000Q1 (0.740, $p=0.730$) or 2008Q3 (0.529, $p=0.912$),
while 2020Q1 is strongly rejected (11.12, $p=8.9\times10^{-16}$). On
Spec 11, 1995Q1 ($p=0.844$) and 2000Q1 ($p=0.152$) are stable, but
2008Q3 is rejected at 5 per cent (1.914, $p=0.017$) — consistent with
the GFC being where Spec 11's credit-interaction structure starts to
bind (the deployed CCI's first knot is 2007Q3, §D.2) — and 2020Q1 is
not computable on the COVID-dummied subsample. The Bai–Perron/CUSUM
battery on Spec 3 finds a single dominant break: supF = 169.96
($p=0$) dated 2019Q1 (the trimming convention places the date at the
segment boundary adjacent to COVID), with CUSUM $p=0.971$ (recursive
residuals stable away from that episode).

### C.4 Williams CCI interactions (Spec 8) — reallocation, not identification

**Table C1 — Sign-prior verdicts, Spec 8 free interactions.**

| Williams interaction | Sign prior | OLS coef | t | p | Verdict |
|---|---:|---:|---:|---:|---|
| `r × CCI` | − | +0.00283 | +2.05 | 0.042 | wrong sign, significant — FAIL |
| `log(HP/y) × (1 − 1.2·CCI)` | − | +0.0299 | +1.38 | 0.171 | wrong sign, insignificant — FAIL |
| `log(y^p/y) × CCI` | + | −0.5046 | −2.43 | 0.016 | wrong sign, significant — FAIL |
| `log(y^p/y)` | + | +0.4921 | +3.42 | 0.001 | right sign — PASS |
| `HA × CCI` (γ₁) | + | +0.00426 | +0.87 | 0.384 | right sign, insignificant |

Three of the four sign-priced interactions fail. Against Williams'
Table 1 the raw-coefficient gaps remain structural: α_c1 (r×CCI)
−0.871 vs our structural +0.0062; α_c4 (HP/y×(1−1.2·CCI)) −0.13 vs
+0.065; ψ₁ +0.93 (calibrated) vs −1.101 (freely estimated). Spec 8
re-allocates the long-run identification rather than recovering
Williams' channels: standalone `nla_y` strengthens (struct +0.073),
`super_y` strengthens (struct +0.047), and λ shifts from −0.239 (Spec
6) to −0.458 — past Williams' magnitude — while attaining the highest
adj-R² (0.827) among the $n=146$ specifications. Pre-COVID, the
standalone housing level becomes significant (`ha_y` +0.0154***,
$t=2.87$) but the interactions turn negative (`ha_x_cci` −0.0069,
$t=-1.42$; `hp_x_1_minus_cci` −0.0292, $t=-1.54$) while λ halves to
−0.219 ($t=-3.95$). The six CCI-interacted regressors act as flexible
parameter time-variation, not as a structurally identified
common-factor channel (§D.5).

### C.5 Net-liquid-assets restriction γ_LA + γ_LOANS = 0

**Table C2 — Wald test of H₀: γ_LA + γ_LOANS = 0** (`car::linearHypothesis`,
Newey–West variance).

| Spec | Sample | γ_LA + γ_LOANS | NW SE | t | p | Restriction |
|---|---|---:|---:|---:|---:|:-:|
| 4 | full | +0.0325 | 0.0445 | 0.731 | 0.465 | accepted |
| 5 | full | +0.0422 | 0.0999 | 0.422 | 0.673 | accepted |
| 6 | full | +0.0202 | 0.0987 | 0.204 | 0.838 | accepted |
| 4 | pre-COVID | +0.0282 | 0.0285 | 0.989 | 0.322 | accepted |
| 5 | pre-COVID | −0.0222 | 0.0499 | −0.445 | 0.656 | accepted |
| 6 | pre-COVID | −0.0026 | 0.0509 | −0.051 | 0.959 | accepted |

Accepted at the 5 per cent level in every specification × sample
combination — non-rejection-by-imprecision, but it validates the
Italian convention of netting deposits against debt and supports the
constructed `nla_y` series.

### C.6 Drehmann amortising-mortgage adjusted real rate

$N=25$ years (100 quarters), Spec 3, $n=146$. The substitution is
essentially inert: `ecm_lag` −0.1906 (base) → −0.1902 (Drehmann);
net-worth 0.00108 → 0.00129; `ln_yp_over_y` 0.1963 → 0.1959; real-rate
−0.00020 → −0.00015 (insignificant either way). Undefined for the
faithful LIVES specification, which carries no plain `real_rate` term
(its rate enters only through `r × CCI`).

### C.7 Scaled-income robustness

Averaging disposable income with labour-plus-transfer income (Spec 3,
$n=146$) shifts λ from −0.1906 to −0.2111 and `ln_yp_over_y` from
0.196 to 0.163, with net-worth roughly halving (0.00108 → 0.00061).
On Spec 11, λ moves from −0.448 to −0.525 while the wealth m.p.c.s
strengthen (`nla_y` 0.0269 → 0.0344, `ilfa_y` 0.0155 → 0.0175) and
`ln_yp_over_y` eases (0.459 → 0.376). The income-measure choice moves
|λ| by 0.02–0.08 in level but does not change the substantive ranking
or signs of the wealth coefficients in either frame.

### C.8 Williams non-property income (NPY) robustness

Replacing disposable income with `npy_real_pc` (Williams 2009 §4.2.1)
is close to inert on Spec 3's speed of adjustment (−0.1906 → −0.1857,
−2.6%), with `ln_yp_over_y` easing (0.196 → 0.164) and net-worth
staying negligible (0.00108 → 0.00044). On Spec 11 the NPY measure
trims λ (−0.448 → −0.395) while raising the wealth m.p.c.s (`nla_y`
0.0269 → 0.0413, `ilfa_y` 0.0155 → 0.0203) and easing `ln_yp_over_y`
(0.459 → 0.352). The income concept is not what separates the paper's
estimates from Williams': under his own income measure the
conventional λ is essentially unchanged and the LIVES wealth channels
strengthen.

### C.9 Permanent-income method comparison

See §E, Table E1.

### C.10 Permanent-income filter sensitivity

A grid over discount factor $\delta \in \{0.90, 0.95, 0.97\}$ and
horizon $k \in \{20, 40, 60\}$ quarters, ogive on/off, under the AR
constructor on Spec 3: λ ranges only from −0.0470 to −0.0482 across
the eighteen cells (baseline $\delta=0.95$, $k=40$, ogive on:
λ = −0.0479, structural PI weight −0.076, structural net-worth weight
+0.122). The ogive toggle moves λ by less than 0.0003 and the
structural PI weight by at most 0.03 within each $\delta\times k$
cell. The PI weight is more sensitive to the horizon (−0.21 at $k=20$
down to −0.04 at $k=60$) but never changes sign within the AR method.
An HP-filter ($\lambda=1600$) permanent income moves λ to −0.0820 and
flips the structural PI weight to +1.238. The within-AR-method PI
tuning is not what drives the |λ| gap with Williams; the dominant
factor is the AR-versus-Italy-direct-forecast method choice (§E).

For the headline LIVES specification: removing the GFC ogive moves
Spec 11's λ from −0.448 to −0.574 ($t=-4.63$) and `ln_yp_over_y` from
+0.459 to +0.604, leaving the structural gearing essentially unchanged
(ψ̂ ≈ 1.05 against the headline 1.02) — the §B.4/§E admissibility-bound
breach is not an ogive artefact.

### C.11 COVID-period robustness

All fourteen specifications carry all four sample variants (full,
pre-COVID, COVID-dropped, COVID-rich). λ is sign-stable across all
four for every specification except the two calibration-imposed ones.
Spec 6: −0.239 / −0.087 / −0.162 / −0.177 (all correctly signed, though
pre-COVID is small and insignificant). Spec 11: −0.448 / −0.266 /
−0.248 / −0.242 (sign-stable, full-sample inflated, the tightly
clustered −0.24 to −0.27 of the three COVID-controlled variants
treated as the identified value). Spec 10 (sign-flips to +0.009
COVID-dropped, +0.051 COVID-rich) and Spec 12 (sign-flips to +0.041
pre-COVID, +0.015 COVID-rich) are not sign-stable — an artefact of the
calibration collapse (§B.5), not of the COVID episode itself.

### C.12 Rolling-window estimation

A 60-quarter rolling estimation of Spec 3 (87 windows ending
2003Q2–2024Q4) shows λ holding in a band of roughly −0.13 to −0.27 for
windows ending before 2020; the net-worth coefficient trends down from
about +0.012 in the earliest windows to near zero through the
macroprudential era and mildly negative in the latest windows (−0.011
at 2024Q4, within one SE of zero). The COVID quarters destabilise the
short windows: λ briefly flips to +0.15 around 2021Q3 before settling
at −0.44 (SE 0.19) in the final window, where COVID dominates a
60-quarter span — read as a symptom of limited identifying variation
in the post-deregulation sample, not model instability.

### C.13 Out-of-sample forecast validation

**Table C3 — Rolling out-of-sample RMSE, 36 expanding-window cuts**
($h\in\{1,4,8\}$; $n=36$ at $h=1,4$; $n=32$ at $h=8$).

| Specification | h = 1 RMSE | h = 4 RMSE | h = 8 RMSE |
|---|---:|---:|---:|
| Benchmark RW drift | 0.03094 | 0.03094 | 0.03282 |
| Benchmark AR(1) | 0.03703 | 0.03102 | 0.03283 |
| Spec 4 (disagg, no CCI) | 0.03175 | 0.03182 | 0.03896 |
| Spec 6 (conventional baseline) | 0.03231 | 0.03293 | 0.04180 |
| Spec 7 (cohort-burden) | 0.03247 | 0.03164 | 0.03540 |
| Spec 8 (Williams CCI) | 0.02901 | 0.03323 | 0.04038 |
| Spec 9 (Kalman CCI) | 0.03206 | 0.03299 | 0.03929 |
| **Spec 11 (LIVES headline)** | 0.02919 | 0.03517 | 0.06402 |

At $h=1$, Spec 8 (0.0290) and Spec 11 (0.0292) beat the RW-drift
benchmark (0.0309). At $h=4$ and $h=8$ the random walk with drift
dominates every structural specification (best structural at $h=4$:
Spec 7, 0.0316 vs 0.0309; at $h=8$: Spec 7, 0.0354 vs 0.0328), and
Spec 11 is the worst performer at $h=8$ (0.0640). A construction
caveat applies to the whole exercise: the validator's permanent-income
input is the full-sample Italy-method measure, not a real-time
forecaster, and the credit-conditions series and its de-mean constants
are likewise full-sample objects. Both generated regressors therefore
embed information from beyond each expanding-window cut, so these
RMSEs measure fit stability under re-estimation, not genuine real-time
forecast accuracy — an upper bound on what a fully real-time
forecaster would deliver.

### C.14 Back-extension robustness — Spec 1 on the 1976Q3+ sample

**Table C4 — Spec 1 (aggregate net worth), structural γ = OLS/|λ|.**

| LR coefficient (γ) | 1988+ baseline (n=146) | 1976+ extended (n=190) | % change |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.1934 | −0.2090 | +8.1 |
| ln_networth_y | +0.0195 | +0.0467 | +140 |
| ln_hp_over_y | +0.0443 | +0.0375 | −15.5 |
| real_rate | −0.00115 | +0.00005 | sign flip |
| ln_yp_over_y | +1.0379 | +0.9913 | −4.5 |

The speed of adjustment and permanent-income elasticity are
essentially stable across samples (λ −0.193 → −0.209; γ_yp 1.04 →
0.99); the aggregate wealth elasticity more than doubles (0.019 →
0.047) but both values are small and imprecisely estimated (baseline
$t=0.17$ on the OLS coefficient) — read as the pre-deregulation regime
adding identifying variation to a coefficient the modern sample barely
pins down, not as datable parameter instability.

### C.15 Spec 4 on the back-extended sample

Structural γ: λ moves 11.5 per cent toward Williams (−0.1824 →
−0.2034, still 29 per cent short of −0.286), but the individual wealth
coefficients become smaller — γ_NLA collapses 95 per cent (+0.0245 →
+0.0013), γ_SUPER flips sign (+0.014 → −0.005), γ_HA eases (+0.019 →
+0.018), γ_EQ retains its wrong sign (−0.043 → −0.041). Sample length
is not the binding constraint on whether the disaggregated
single-equation form reproduces Williams' Table 1: the longer sample
sharpens the speed of adjustment but blunts, rather than sharpens, the
individual wealth channels.

### C.16 Spec 6b — conventional baseline on the back-extended sample

**Table C5 — Spec 6 vs Spec 6b, structural γ.** $n=180$ full / $n=160$
pre-COVID for Spec 6b.

| LR coefficient (γ) | Spec 6 (n=86) | Spec 6b (n=180) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.239 (t=−2.55) | −0.248 (t=−3.96) | −0.286 |
| ha_y / ha_y_proxy γ | 0.009 | 0.012 | 0.049 |
| nla_y / nla_y_proxy γ | 0.035 | 0.015 | 0.159 |
| eq_y / eq_y_proxy γ | −0.065 | −0.008 | (calibrated 0.011) |
| super_y / super_y_proxy γ | 0.025 | −0.001 | (calibrated 0.011) |
| ln_hp_over_y γ | +0.043 | +0.024 | −0.130 |
| ln_yp_over_y (CCI=0) γ | +1.363 | +1.113 | +0.20 (calibrated) |
| BIC | −492.5 | −1114.0 | n/a |

The speed of adjustment moves modestly closer to Williams (87 per
cent of −0.286 vs 83 per cent on Spec 6) and is far more sharply
estimated ($t$ improves from −2.55 to −3.96), identified in every
sample variant (pre-COVID −0.240, $t=-4.61$; COVID-dropped −0.240;
COVID-rich −0.234) — unlike Spec 6, whose λ collapses without the
COVID quarters. The wealth γ profile shifts toward still-smaller
individual elasticities (γ_NLA falls from 0.035 to 0.015, γ_SUPER
flips sign) while γ_HA stays positive but at a quarter of Williams'
0.049. `eq_y_proxy` is held constant at its 1988Q3 value pre-1988
(§A.7), so the near-zero back-extended γ_EQ and γ_SUPER should be read
as proxy-limited rather than as estimates of a true pre-1988
propensity.

### C.17 Maximal-GETS placebo on the back-extended sample

**Table C6 — Extended-sample placebo (1976Q3+).**

| Placebo variant (1976Q3+) | Canonical adj-R² | adj-R² percentile | \|λ\| percentile | Verdict |
|---|---:|---:|---:|---|
| Extended (Williams literal 4-knot) | 0.6801 | 36th | 26th | below median on both |
| Maximal-GETS (canonical 15-knot reduction) | 0.6836 | 48th | 70th | below R² median, above |λ| median |

On the back-extended sample the literal Williams 4-knot construction
sits below the placebo median on both metrics (adj-R² 0.6801 vs
placebo median 0.6815; |λ| 0.2023 vs 0.2079). The maximal-GETS
reduction does better on mean reversion (|λ| 0.2563 beats 70 per cent
of random draws against a placebo median of 0.2329) but still sits
just below the random median on fit (48th percentile), while retaining
fewer knots (7) than the placebo median (8). Neither extended-sample
construction is the deployed CCI: the deployed-protocol placebo on the
1988Q3+ estimation sample reaches the 84th adjusted-R² and 80th |λ|
percentiles (§D, Table D3).

### C.18 Sectional sign-prior CCI

A sectional CCI basis with one knot per period (1982/1990/1993/2007,
plus 2014/2017/2020/2021 extensions) placebo-tested on the
back-extended sample: the sectional canonical (adj-R² 0.6805, |λ|
0.2244) sits at the 37th adjusted-R² percentile and 60th |λ|
percentile — below the random median on fit, between the literal
4-knot and maximal-GETS constructions on mean reversion. On the modern
1988Q3+ sample the sectional reduction retains two survivors against
the deployed protocol's four, fitting somewhat worse (adj-R² 0.726 vs
0.754; λ = −0.203 vs −0.246) with a 0.69 correlation between the two
indices — a related but coarser credit signal (§D.4).

### C.19 Two-equation SUR (consumption + house prices)

Joint SUR estimation of the consumption equation and a house-price ECM
on the back-extended 1976Q3+ sample yields a residual correlation
$\hat\rho(\varepsilon_C,\varepsilon_H) = -0.0109$ under
equation-by-equation OLS and $-0.0133$ under SUR — negligible
cross-equation linkage. The SUR coefficients move by under 5 per cent
of themselves relative to OLS for every substantive term (10 per cent
on the near-zero intercept); the consumption equation's λ shifts from
−0.2101 to −0.2118 (+0.8 per cent) with SUR standard errors within
0.01 per cent of the OLS ones. Joint estimation gives no efficiency
gain at the quarterly frequency; the case for the multi-equation
framework rests on cross-equation parameter restrictions, not residual
covariance (§C.2, §D.5).

### C.20 Three-equation joint cross-equation CCI identification

See §D.5, Table D4, for the full joint-identification exercise
(cross-equation sign-survival across the consumption, house-price,
mortgage-stock and wealth equations).

---

## D. Credit-conditions-index construction and placebo battery

### D.1 The Muellbauer–Williams smoothed-step spline approach

Muellbauer and Williams (2012) construct CCI as a spline of `SDMMA`
smoothed-step dummies (a five-quarter moving average of a four-quarter
moving average of a 0/1 step) at institutional turning points in the
Australian financial-policy chronology, with each knot's coefficient
constrained by a sign prior derived from institutional history
(deregulation episodes positive, retrenchment episodes negative),
enforced by Hendry–Krolzig (2005) drop-on-violation general-to-specific
reduction. Williams' canonical paper uses four knots: 1979Q1 (Campbell
Committee, removal of interest-rate ceilings), 1992Q1 (NBFI distress
after the early-1990s recession), 1998Q1 (NBFI/securitisation
expansion), and 2007Q1 (GFC retrenchment).

**Table D1 — Direct replication of Williams' four canonical knots on
the 1988Q3-onward sample.**

| Williams knot | Sign prior | Status on 1988+ sample |
|---|---:|---|
| 1979Q1 | + | aliased (constant within window) |
| 1992Q1 | − | sign violator (data signal +ve) |
| 1998Q1 | + | sign violator (data signal −ve) |
| 2007Q1 | − | survives, coef ≈ −0.010 |

Only one of Williams' four canonical knots survives sign-prior
reduction on this sample. The 1979 deregulation knot is mechanically
uninformative because the smoothed step reaches unity by 1980Q2, three
years before the estimation window opens; the 1992 and 1998 knots fail
their institutional sign priors because the post-1988 sample observes
the recovery from the early-1990s banking distress and the late-1990s
NBFI period without the contrast against the prior tight regime that
would identify the loosening direction. This is a direct consequence
of the data window: ABS sectoral balance-sheet data begin only in
1988Q3, so the financial-liberalisation episode that most cleanly
identifies the credit channels largely predates the sample on which
the single-equation model is estimated. A direct 4-knot replication is
therefore identifying one episode — the 2007 GFC tightening — plus a
constant, not the four distinct episodes Williams' framework
attributes to the spline.

### D.2 The maximal-GETS Australian CCI (deployed construction)

Rather than impose Williams' published knot count, a maximal-GETS
approach starts from a richer 15-knot candidate set covering the
documented Australian financial-policy chronology (Campbell '79,
housing-finance deregulation '86, state-bank distress '90, banking
distress '92/'93, the Wallis report/APRA establishment '98, the GFC
tightening '07, the deposit-guarantee scheme '08, the FHB Boost '09,
the APRA macroprudential rounds '14/'17, the Hayne Royal Commission
'19, the APRA cap removal/buffer reduction '19Q3, the COVID/JobKeeper
episode '20, and the APRA buffer hike '21) and lets drop-on-violation
reduction prune knots that are aliased or violate their institutional
sign prior.

**Table D2 — Surviving knots on the 1988Q3–2024Q4 sample under
iterated drop-on-violation reduction (`cci_williams`).**

| Knot | Sign prior | Coef (OLS) | Reading |
|---|---:|---:|---|
| 2007Q3 | − | −0.0022 | GFC tightening onset |
| 2009Q1 | + | +0.0123 | First Home Buyer Boost (contestable, see below) |
| 2019Q1 | − | −0.0338 | Hayne Royal Commission lending crackdown |
| 2020Q2 | + | +0.0071 | COVID/JobKeeper income support |

Nine candidate knots (1990Q3, 1992Q1, 1993Q1, 1998Q3, 2008Q4, 2014Q4,
2017Q1, 2019Q3, 2021Q4) violate their institutional sign priors and
are dropped; 1979Q1 and 1986Q1 are aliased (constant within the
window). The `cci_williams` series is built from these four surviving
knots, peak-normalised to unity: identically zero from 1976 until
2007Q3, dips to ≈ −0.15 by late 2008, rises to its peak of 1 by
2010Q4, plateaus through 2018Q4, then falls to a trough of −2.12 in
2020Q4 and settles at ≈ −1.63 from 2022 onward (range −2.12 to +1.00).

Four implications follow. First, every credit channel in Specs 8 and
11 is identified off roughly 70 post-2007 quarters — before 2007Q4
each CCI interaction is exactly zero. Second, that all four surviving
knots are post-2007 is itself part of the identification story (§D.5):
the post-1988 sample carries usable sign-identifying variation only
around the GFC, macroprudential and pandemic episodes. Third, the
institutional reading of the surviving 2009Q1 "+" knot is contestable:
the First Home Buyer Boost was a fiscal stimulus rather than a
lending-standards easing, and the RBA's Financial Stability Reviews
record lending standards tightening through 2009, so the "+" prior
records a credit-demand event under a credit-supply label. Fourth, the
candidate basis embeds a documented sign-prior conflict at 1993Q1: the
maximal basis codes it −1 (the tail of the early-1990s bank
retrenchment) while the sectional basis (§D.4) codes the same date +1
(mortgage-originator entry, Aussie Home Loans); both institutional
readings are defensible for different events at the same date and the
conflict is disclosed rather than silently resolved.

The reduction protocol requires precise statement: at each pass the
consumption ECM is re-fitted with the currently surviving candidate
set and all knots whose coefficient violates its sign prior are
dropped simultaneously, iterating to a fixed point. This differs from
Williams' one-at-a-time, strongest-violator-first reduction; the
survivor set is protocol-dependent — a single-pass reduction over the
same 15-knot basis retains a different set of five knots (1992Q1,
2007Q3, 2017Q1, 2019Q1, 2020Q2, with 2009Q1 aliased rather than
surviving) and a different λ. The iterated protocol is the one
deployed, and it is the one placebo-tested "as deployed" in §D.3.

The construction is two-step, with pre-test re-use of the dependent
variable: the knots are first selected as plain additive long-run
regressors inside a constant-MPC consumption equation estimated on the
same Δln c series; the surviving combination, peak-normalised, is then
re-deployed multiplicatively (as `ha_x_cci`, `r_x_cci`, `yp_x_cci`,
`hp_x_1_minus_cci` and the `cci_williams` level) in Specs 8 and 11.
Spec 11's fit statistics are therefore conditional on a CCI that was
pre-fitted, under sign priors, to the same dependent variable — a
pre-test problem that the placebo battery (§D.3) quantifies but does
not remove.

The maximal-GETS construction is defensible on two grounds: (i) the
candidate set comes from documented Australian institutional history,
not authorial choice of specific dates; and (ii) the surviving knots
are those whose data signal aligns with their institutional sign
prior, so the spline is empirically selected rather than imposed. It
does not, however, deliver a structurally identified credit-conditions
factor on its own — see §D.5 and the interaction-collinearity result
in §C.4/§C.20.

### D.3 The placebo battery

Whether the spline identifies genuine credit-conditions turning points
— rather than acting as flexible detrending of the consumption-
equation residual — is tested with random-knot placebos: 200 draws of
knot dates, compared like-for-like with the institutional construction
under the *same* protocol. For the literal-Williams comparison all
four drawn knots are entered unconditionally (pure fit, no sign-prior
reduction). For the maximal-GETS comparison each draw of 15 candidate
knots passes through a single sign-prior reduction pass. The
**deployed-protocol placebo** replaces the 15 institutional knot dates
with random dates carrying the same sign-prior pattern and runs
exactly the iterated drop-on-violation reduction of §D.2, so the
deployed `cci_williams` construction is placebo-tested as deployed,
pre-test step included.

**Table D3 — Four placebo runs.**

| Construction | Protocol | Sample | adj R² %ile | \|λ\| %ile | Verdict |
|---|---|---|---:|---:|---|
| Literal Williams 4-knot | unconditional 4-knot | 1988Q3+ (n=146) | 45th | 56th | below R² median — detrending critique vindicated |
| Literal Williams 4-knot | unconditional 4-knot | 1976Q3+ (n=190) | 36th | 26th | below median on both — critique persists |
| Maximal-GETS canonical | single-pass reduce | 1976Q3+ (n=190) | 48th | 70th | below R² median — critique persists |
| **Deployed `cci_williams`** | **iterated reduce** | **1988Q3+ (n=146)** | **84th** | **80th** | **moderate support** |

The literal Williams 4-knot construction sits at or below the placebo
median on both samples (45th adjusted-R² percentile on the modern
sample, 36th/26th on the extended sample); Williams' specific
published knot dates, entered as published, do not outperform random
dates, and single-pass maximal-GETS reduction on the extended sample
does no better (48th R² percentile, retaining fewer knots than the
random median). The deployed construction fares better: under the
iterated reduction actually used to build `cci_williams`, the
institutional knot dates beat 84 per cent of random draws on adjusted
R² and 80 per cent on |λ|, while retaining fewer knots (4) than the
placebo median (5) — more fit with less flexibility than typical
random constructions. This is moderate, not strong, support: one in
six random draws still matches the deployed fit, the percentile is
specific to the iterated protocol, and the construction re-uses the
dependent variable. The standalone spline remains, at best, weakly
distinguished from flexible detrending; it is not on its own a
structurally identified common factor. (For the deployed *Spec-11*
multiplicative construction, carried through the full model rather
than the additive selection stage alone, see the deployed-protocol
Spec-11 placebo in Table F6.)

### D.4 Sectional sign priors (period-based) tested

Williams' Australian-paper §5.1 specification imposes sign priors over
periods rather than knot by knot:

| Period | Sign prior | Rationale |
|---|---:|---|
| 1982–1990 | non-negative | Financial deregulation |
| Early 1990s | non-positive | Banking sector distress |
| Mid-1990s–2006 | non-negative | New entrants, securitisation |
| 2007+ | non-positive | GFC |

A parallel CCI basis with one knot per period, extending Williams'
four periods to cover post-2008 events (APRA 2014, APRA 2017, COVID
2020, APRA 2021), placebo-tested on the back-extended sample with 200
draws of eight random knots and eight random ±1 priors, sits at the
37th adjusted-R² percentile and 60th |λ| percentile ("detrending
critique persists — sectional below random median"). Williams'
specific period dating does not outperform random period placements on
the post-deregulation-extended window. In the side-by-side coefficient
comparison the sectional basis retains only two survivors against the
maximal basis's four, with a weaker fit (adj-R² 0.726 vs 0.754; λ =
−0.203 vs −0.246) and a 0.69 correlation between the two indices — the
period-prior construction delivers a related but coarser credit signal
than the maximal-GETS reduction.

**Overall placebo verdict.** Neither the literal 4-knot construction,
nor a single-pass maximal-GETS reduction, nor sectional sign priors
delivers placebo support on the extended sample — the institutionally
dated knots per se do not beat random dates. The one construction that
does beat most random draws is the deployed iterated protocol on the
modern sample, and even that support is moderate and conditional on
the protocol. The CCI's standalone identification remains closer to a
single-equation OLS using flexible smoothed-step dummies than to a
structurally identified common factor.

### D.5 Why the spline alone cannot identify the CCI as a common factor

Williams (2010) and Duca and Muellbauer (2013, ECB WP 1581) estimate
the CCI inside a multi-equation system (consumption, house prices,
mortgage stock, home-equity withdrawal — Williams; consumption plus a
mortgage-refinancing rate via a Kalman-filter state-space model — Duca
and Muellbauer) where the same latent credit variable enters all
equations simultaneously, with a normalisation (ζ_h = 1 in the
house-price equation) identifying ζ_c, ζ_m, ζ_w as relative scalings.
This cross-equation parameter restriction is what identifies the CCI
as a common factor rather than as an equation-specific residual. In a
single-equation OLS estimation the spline is fit only to the
consumption-equation residual, with no constraint that the same knot
loadings satisfy sign priors in the house-price, mortgage-stock or
home-equity-withdrawal equations simultaneously.

**Table D4 — Cross-equation joint sign-survival.** The Williams
15-knot maximal candidate set is refit with the consumption equation,
the house-price equation, the mortgage-stock equation and a
home-equity-withdrawal proxy equation simultaneously, requiring each
knot to satisfy its institutional sign prior in every equation in
which it is estimable.

| Survival regime | Surviving knots | n |
|---|---|---:|
| Consumption equation only (extended sample) | 1979, 1986, 1992, 2007Q3, 2017Q1, 2020Q2 | 6 |
| Joint 3-equation (C ∩ H ∩ M) | 1986, 2017Q1 | 2 |
| **Joint 4-equation (C ∩ H ∩ M ∩ HEW)** | **1986** | **1** |

Of the six knots that pass their sign prior when fitted to consumption
alone, only 1986 (financial deregulation) and 2017Q1 (APRA
macroprudential round II) have signs consistent with their
institutional priors across consumption, house prices and mortgage
stock simultaneously, and only 1986 also survives the
home-equity-withdrawal equation. The consumption-only identification
is therefore overstated: four of the six consumption-passing knots are
consumption-equation-specific and do not survive a cross-equation
common-factor restriction.

**Table D5 — House-price-equation CCI loading under joint
identification.**

| HP equation, CCI loading | (a) cons-only CCI | (b) joint OLS | (c) joint SUR |
|---|---:|---:|---:|
| Estimate | −0.016 | +0.024 | +0.024 |
| Sign | ✗ violator | ✓ | ✓ |

Rebuilding `cci_williams_joint` from the two knots surviving the
three-equation test flips the house-price equation's CCI loading from
negative (consumption-only CCI) to positive (joint identification) —
Williams' cross-equation sign restriction working as intended. The
mortgage-stock equation's loading remains negative under this joint-
sign-survival approximation (which weights surviving knots by
consumption-equation coefficients); full FIML with parameter
restrictions across all four equations would be needed to flip it —
the exercise is illustrative of the mechanism, not a substitute for
the FIML estimate. A complementary two-equation SUR (consumption +
house prices) finds a negligible cross-equation residual correlation
($\hat\rho=-0.0109$ OLS, $-0.0133$ SUR — see §C.19): the case for joint
estimation rests entirely on cross-equation parameter restrictions,
not on residual-covariance efficiency gains.

**Interaction collinearity.** In the faithful LIVES form, CCI
multiplies six channels jointly inside the long-run bracket, so the
regressors that carry those channels are each approximately
proportional to CCI over the sample: the five CCI-carrying regressors
(`cci_williams`, `ha_x_cci`, `hp_x_1_minus_cci`, `r_x_cci`,
`yp_x_cci`) have absolute pairwise correlations between 0.66 and 0.97,
with the extremes $\rho(\text{cci\_williams}, \text{yp\_x\_cci}) =
-0.967$, $\rho(\text{cci\_williams}, \text{ha\_x\_cci}) = +0.890$, and
the weakest pair ($\text{hp\_x\_1\_minus\_cci}, \text{r\_x\_cci}$)
still at $0.66$. Five near-collinear regressors carrying distinct
structural meanings cannot be separately free-estimated off a single
equation: OLS allocates a near-singular design among them more or less
arbitrarily, producing wrong-signed and insignificant individual
loadings even when the joint contribution of the credit block is real
— consistent with the sign failures of Spec 8 (§C.4), the
identification-vs-detrending reallocation when the CCI is added to the
baseline (permanent-income loading +51 per cent, speed of adjustment
−92 per cent, net-liquid loading +303 per cent, superannuation loading
+258 per cent moving from Spec 6 to Spec 8), and the calibration
collapse under Specs 10/12 (§B.5).

---

## E. Permanent-income construction

Permanent income $y^p_t$ is the forward-looking object in the ratio
$\log(y^p_t/y_t)$, constructed as a discounted weighted average of
*expected* future log income over a 40-quarter horizon ($k=40$
quarters) at quarterly discount factor $\delta_q = 0.95^{1/4}$ (annual
discount factor $\delta=0.95$, annual discount rate $\eta=0.05$):

$$
\log(y^p_t/y_t) = E_t\!\left[\sum_{h=1}^{40} w_h \log(y_{t+h})\right] - \log(y_t),
\qquad w_h = \frac{\delta_q^{h-1}}{\sum_{h=1}^{40}\delta_q^{h-1}}.
$$

Two forecasters are implemented.

**Italy direct forecast (headline measure).** Following the Italian
implementation's direct (single-regression) forecast of the discounted
future-income aggregate (De Bonis, Liberati, Muellbauer and Rondinelli
2020, Appendix A.2), the discounted weighted average of future log
income is regressed, in a single full-sample equation, on
`log(lf_share)` (the labour-force-participation term capturing
slow-moving demographic effects on trend income), a trend, a
post-2008 split trend, the four-quarter moving average of log income,
the unemployment rate, and four-quarter-difference dynamics; the
permanent-income series is the fitted value. Three properties are
disclosed:

1. *Look-ahead and tail extrapolation.* The coefficients are estimated
   over the whole sample, so $y^p_t$ embeds information dated after
   $t$ and is non-causal — a two-sided measurement rather than a
   real-time forecast. The realised 40-quarter-ahead target is
   computable only up to 2014Q4, so the training sample ends there and
   the final forty quarters of $y^p$ — about 27 per cent of the
   estimation sample, including the entire COVID period — are
   out-of-training extrapolations from 2014-vintage coefficients,
   driven mainly by the deterministic trend terms.
2. *GFC learning ogive.* The series is multiplied by an ogive that
   declines from 1 to 0.5 over 2008Q3–2012Q2, so the headline
   regressor is half the raw discounted-gap measure over the
   post-2012 two-thirds of the sample. A no-ogive re-estimate of the
   headline specification (§C.10) leaves the structural conclusions
   unchanged.
3. *Forecaster choice and the Australian permanent-income puzzle.*
   Under a rolling AR(8) forecaster the $\log(y^p/y)$ coefficient is
   negative (the "Australian permanent-income puzzle"); under the
   Italy-style direct forecaster it is positive, and the Italy
   forecaster fits better (AR adj-$R^2 = 0.696$, Italy adj-$R^2 =
   0.731$, Spec 3 frame).

**Rolling AR(8) (robustness).** A rolling AR(8) regression of log
income on eight own lags plus a linear trend, a post-2008Q3 step dummy
and a trend-break interaction; forecasts are aggregated over the 40
horizons using the discount weights, with optional `unemp_rate`,
`log_oil`, `log_reer`, `log_stocks` predictors and the same 2008Q3
ogive.

### E.1 The real-time vs full-sample distinction

**Table E1 — Permanent-income method comparison, two specification
frames.**

| PI measure | Frame | λ (ecm_lag) | t | log(yp/y) | t | adj-R² |
|---|---|---:|---:|---:|---:|---:|
| AR (expanding-window) | Spec 3, n=146 | −0.0479 | −1.12 | −0.0036 | −3.96 | 0.696 |
| Italy LP (full-sample) | Spec 3, n=146 | −0.1906 | −2.90 | +0.1963 | +3.41 | 0.731 |
| AR (expanding-window) | Spec 6, n=86 | −0.0948 | −1.68 | −0.1583 | −1.68 | — |
| Italy LP (full-sample) | Spec 6, n=86 | −0.2386 | −2.55 | +0.3253 | +1.50 | — |
| Italy LP (real-time) | Spec 6, n=86 | −0.1594 | −2.39 | −0.1451 | −2.23 | — |

The full-sample Italy LP measure is the headline measure (framed as a
measurement, not a forecast); the AR forecaster delivers the negative
"Australian permanent-income puzzle" coefficient in both frames
(significant in the Spec 3 frame, $t=-3.96$). The real-time Italy LP
variant is causal — re-fitting the projection at each $t$ on data
whose full $k$-quarter horizon is realised by $t$ — and shows that
roughly two-thirds of the full-sample Italy $|\lambda|$ is genuine
(−0.159 against −0.239, itself significant at $t=-2.39$; the look-ahead
accounts for about a third), but that the **positive permanent-income
sign is not**: it reverses to −0.145 ($t=-2.23$) under the causal
measure. The full-sample measure is carried as the headline and its
positive-PI sign and part of its λ magnitude are disclosed as
full-sample, non-causal properties; the same look-ahead caveat attaches
to the strong permanent-income coefficient in the faithful LIVES
specification (Spec 11, `ln_yp_over_y` +0.459, $t=4.0$) — the real-time
column is the operational robustness check, and any forecasting use of
the equation (e.g. embedding it in a policy model) requires the
real-time variant or the AR forecaster, not the full-sample measure.

Structural reasons the AR and Italy measures diverge: the rolling
AR(8) forecaster lacks the labour-force-share predictor that captures
Australia's slow-moving demographic effects on trend income, compounds
short-run AR misspecification across 40 horizons, and over-estimates
persistence after large income shocks, all of which the one-step
direct projection avoids. The puzzle's reversal under the real-time
Italy measure is read as a feature of full-sample permanent-income
*measurement* rather than a clean real-time resolution.

### E.2 Discount and horizon sensitivity

The permanent-income discount and horizon settings are not load-bearing
for the speed of adjustment: across $\delta \in \{0.90, 0.95, 0.97\}$,
$k \in \{20, 40, 60\}$ and the ogive on/off toggle, λ in the AR/Spec 2
frame moves only at the third decimal (full grid in §C.10). The
forecaster *method* (Italy vs AR, full-sample vs real-time), not the
discount calibration, is the material choice.

### E.3 The structural gearing bound

In the faithful specification the permanent-income channel is strong
and correctly signed: `ln(y^p/y)` enters with OLS level coefficient
+0.459 ($t=4.04$) full sample and +0.298 ($t=5.81$) pre-COVID. By the
structural-recovery rule (γ = OLS/|λ|) the implied gearing is
OLS/|λ| ≈ 1.02 (full) to 1.13 (COVID-controlled) — **above the
theoretical admissibility bound ψ ≤ 1 − η ≈ 0.95** implied by the
discounting that defines $y^p$. The breach survives removing the ogive
(structural ≈ 1.05; §C.10), and the delta-method interval [0.86, 1.18]
(Table F2) does not exclude 0.95, so the violation is not itself
statistically decisive. Candidate explanations: the unit-income
restriction forcing $\log(y^p/y)$ to absorb low-frequency
consumption-to-income drift, and the measure's non-causal, post-2014
tail-extrapolated construction (above) — under the causal real-time
variant the coefficient reverses sign entirely (§E.1), so the strong
positive gearing is a property of the full-sample *measurement*, not
an operational forecasting relationship. Spec 11 does not separately
estimate the ψ₀/ψ₁ split: the credit-geared component enters through a
separate `yp_x_cci` interaction whose full-sample coefficient (−0.510,
$t=-1.51$) is insignificant and wrong-signed relative to Williams'
calibrated ψ₁ = +0.93, though it turns right-signed (+0.19, $t=1.80$)
pre-COVID — the data identify the level of the permanent-income
gearing but not its credit slope on a single equation (§B.5).

---

## F. Structural-parameter inference

This section reproduces the original delta-method and residual-
bootstrap confidence intervals on the implied structural coefficients
(Tables F1–F2), and then reports four further exercises: a
bounds/stationarity test of the long-run relationship (Tables
F3–F4), a form-vs-sample ablation of the Spec 6 → Spec 11 gap (Table
F5), a deployed-protocol placebo run on the full multiplicative Spec 11
construction (Table F6), and a nested bootstrap that re-estimates the
CCI-knot selection and the permanent-income/ecm-lag construction inside
each resample draw (Table F7). The nested-bootstrap intervals (Table
F7) are the primary basis for structural-coefficient inference;
Tables F1–F2 are retained for comparison only, since they
hold the CCI and permanent-income constructions fixed and are shown in
§F.4 to be anti-conservative by 5–190× relative to the nested
intervals.

### F.1 Delta-method and bootstrap confidence intervals (comparison only)

The implied structural coefficients $\gamma_i = \beta_i/|\lambda|$ are
ratios of two imprecisely estimated quantities. Delta-method standard
errors are computed from the Newey–West covariance of $(\beta_i,
\lambda)$ — which carries the correlation between each numerator and
the speed of adjustment — with a seeded moving-block residual
bootstrap (block length 8, $B=1000$) as a cross-check. Both hold the
right-hand side fixed: they propagate sampling uncertainty in the ECM
coefficients but not the first-stage uncertainty in the generated
permanent-income and CCI regressors (both are held at their
full-sample values in both methods), so the intervals below are a
lower bound on the true uncertainty — the gap the nested bootstrap
(§F.4) closes.

**Table F1 — Spec 6 (conventional baseline, n=86).**

| Term | Implied γ | 95% CI (delta) | Bootstrap median | Bootstrap 95% CI | Williams | Williams in CI? |
|---|---:|---:|---:|---:|---:|:-:|
| Housing `ha_y` | 0.009 | [−0.056, 0.075] | 0.008 | [−0.049, 0.107] | 0.0488 | ✓ |
| Net liquid `nla_y` | 0.035 | [−0.262, 0.332] | 0.027 | [−0.183, 0.361] | 0.159 | ✓ |
| Equities `eq_y` | −0.065 | [−0.468, 0.337] | −0.071 | [−0.303, 0.208] | 0.011 | ✓ |
| Super `super_y` | 0.025 | [−0.040, 0.091] | 0.027 | [−0.064, 0.141] | 0.011 | ✓ |
| log(HP/y) | 0.043 | [−0.308, 0.393] | 0.049 | [−0.400, 0.325] | −0.130 | ✓ |
| Permanent income | 1.363 | [0.285, 2.442] | 1.377 | [0.605, 2.192] | 0.200 | ✗ |
| **Wealth aggregate (Σ)** | **0.004** | **[−0.659, 0.668]** | — | — | 0.230 | ✓ |

Williams' Table 1 value lies inside the 95 per cent interval for every
Spec 6 coefficient except the permanent-income gearing (whose interval
excludes his calibrated 0.20 from above); the same intervals contain
zero for every wealth channel and values far from Williams. On the
conventional baseline the data cannot distinguish Williams' wealth
profile from a broad range of alternatives, including no wealth effect
at all — non-rejection driven by imprecision, not confirmation.

**Table F2 — Spec 11 (faithful LIVES headline, n=146).**

| Term | Implied γ | 95% CI (delta) | Bootstrap median | Bootstrap 95% CI | Williams | Williams in CI? |
|---|---:|---:|---:|---:|---:|:-:|
| Housing × CCI `ha_x_cci` (γ₁) | 0.0055 | [−0.010, 0.021] | 0.0052 | [−0.0092, 0.0204] | 0.0488 | ✗ |
| Net liquid `nla_y` | 0.060 | [0.022, 0.098] | 0.0609 | [0.0129, 0.1107] | 0.159 | ✗ |
| Illiquid `ilfa_y` | 0.035 | [0.012, 0.057] | 0.0347 | [0.0047, 0.0647] | 0.022 | ✓ |
| Affordability `hp_x_1_minus_cci` | 0.062 | [0.027, 0.098] | 0.0622 | [0.0328, 0.0973] | −0.130 | ✗ |
| Permanent income (ψ₀) | 1.024 | [0.865, 1.184] | 1.0221 | [0.8988, 1.1615] | 0.200 | ✗ |
| PI × CCI `yp_x_cci` (ψ₁) | −1.138 | [−2.099, −0.177] | −1.1374 | [−1.5228, −0.7349] | 0.930 | ✗ |
| **Wealth aggregate (Σ)** | **0.100** | **[0.036, 0.164]** | — | — | 0.230 | ✗ |

Under the fixed-regressor intervals, Williams' net-liquid m.p.c.
(0.159), housing m.p.c. (0.0488), affordability sign, permanent-income
gearing and credit slope would all lie outside the 95 per cent bands
while his illiquid m.p.c. (0.022) would lie inside — but Table F7
shows none of these individual-channel comparisons survives once
generated-regressor uncertainty is propagated, so no rejection of
Williams' calibration is claimed on any channel. Agreement with
Williams under this (fixed-regressor) inference
is on form — the error-correction speed and the significance pattern
of the wealth channels — not on the credit-channel calibrations. For
use in a calibrated policy model, the aggregate long-run wealth
coefficient is γ_W = 0.100 (delta CI [0.036, 0.164]): positive and
bounded away from zero, but below both Williams' 0.230 and a
calibrated net-wealth elasticity of ≈ 0.17 used in some Australian
policy models (the latter sits just outside the upper bound). §F.4
shows that once the generated-regressor uncertainty is honestly
carried, none of these individual-channel comparisons survives except
the sign and non-zero magnitude of λ itself.

### F.2 Bounds test and the imposed-ratio stationarity check

A Pesaran–Shin–Smith (PSS) bounds test on the unrestricted
error-correction representation is used to establish whether a level
(cointegrating) relationship exists, since the Engle–Granger residual
test (Table B2/§B.2) fails to reject no-cointegration for every
estimable specification.

**Table F3 — PSS bounds test.** Source: bounds-test output.

| Spec | k | n | F-stat | I(0) bound 5% | I(1) bound 5% | t-stat | I(0) bound | I(1) bound | Verdict |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|
| Spec 6 (conventional baseline) | 9 | 86 | 5.591 | 2.14 | 3.30 | −4.931 | −2.86 | −4.88 | **cointegrated** |
| Spec 11 (LIVES headline) | 9 | 145 | 9.572 | 2.14 | 3.30 | −5.083 | −2.86 | −4.88 | **cointegrated** |
| Spec 12 (calibrated) | — | — | — | — | — | — | — | — | infeasible (see note) |
| Spec 12, free-regressors-only diagnostic | 3 | 145 | 4.245 | 3.23 | 4.35 | −4.002 | −2.86 | −3.78 | inconclusive |

Both the F-statistic and the $t$-statistic clear their respective I(1)
upper bounds for Spec 6 (F = 5.591 > 3.30; $t=-4.931$ beyond $-4.88$)
and Spec 11 (F = 9.572 > 3.30; $t=-5.083$ beyond $-4.88$): a level
relationship is supported for both the conventional baseline and the headline
specification. Spec 12's long-run vector is partly hard-calibrated
(γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93 enter as a fixed, non-estimated
offset inside an iteratively solved fixed point, not as freely
estimated coefficients — the same reason the Engle–Granger screen
skips Specs 10/12, §B.2), so the PSS unrestricted-ECM is not
well-defined for Spec 12's actual estimating equation; the primary row
is recorded infeasible. A diagnostic restricted to the two freely
estimated long-run regressors only (`ha_x_cci`, `nla_y`, plus income)
is inconclusive (F = 4.245, between the 3.23/4.35 bounds) and excludes
the calibrated `ilfa_y`/`ln_yp_over_y`/`yp_x_cci` offset, so it
understates rather than tests Spec 12's true long run. Spec 12's long
run must therefore be described as imposed/calibrated, not tested.

**Table F4 — Stationarity of the imposed unit-income ratio
`ecm_lag = ln(cons_{t-1}) − ln(income_t)`.** Source: stationarity test
output (ADF with drift; KPSS level test).

| Window | n | ADF stat | ADF 5% CV | ADF verdict | KPSS stat | KPSS 5% CV | KPSS verdict |
|---|---:|---:|---:|---|---:|---:|---|
| Full | 193 | −2.327 | −2.88 | nonstationary | 0.997 | 0.463 | nonstationary |
| Spec 6 window | 86 | −2.428 | −2.89 | nonstationary | 0.849 | 0.463 | nonstationary |
| Spec 11 window | 146 | −2.500 | −2.88 | nonstationary | 0.783 | 0.463 | nonstationary |
| Spec 12 window | 146 | −2.500 | −2.88 | nonstationary | 0.783 | 0.463 | nonstationary |

ADF and KPSS agree unanimously that the imposed unit-income
consumption/income ratio is nonstationary in every window (ADF fails
to reject the unit-root null in all four windows; KPSS rejects the
stationarity null in all four). The level (cointegrating) relationship
established by Table F3 is therefore carried by the **full** long-run
regressor set — wealth, housing, credit conditions and permanent
income acting jointly — and **not** by the imposed unit-income
consumption/income ratio taken on its own.

### F.3 Form vs sample — ablation of the Spec 6 → Spec 11 gap

The λ gap between the conventional baseline (Spec 6, λ = −0.239, n = 86)
and the headline (Spec 11, λ = −0.448, n = 146) mixes two changes:
functional form and sample/CCI-series extension. A 2×2 ablation swaps
one factor at a time.

**Table F5 — Form-vs-sample ablation of λ.** Source: ablation output.
Baseline reproductions match the Spec 6 and Spec 11 estimates of
Tables B6 and B4 exactly.

| Cell | Regressor form | Estimation sample | n | λ | λ t-stat | `nla_y` (t) | `ilfa_y` (t) | `ln_yp_over_y` (t) |
|---|---|---|---:|---:|---:|---:|---:|---:|
| Baseline | Spec 6 form | Spec 6 sample (2002Q3–2024Q4) | 86 | −0.2386 | −2.552 | 0.0083 (0.24) | — | 0.3253 (1.50) |
| Baseline | Spec 11 form | Spec 11 sample (1988Q3–2024Q4) | 146 | −0.4483 | −3.567 | 0.0269 (3.75) | 0.0155 (3.09) | 0.4591 (4.04) |
| **Cell A — form only** | Spec 11 form | Spec 6 sample (2002Q3–2024Q4) | 86 | −0.5423 | −3.807 | 0.0513 (1.45) | 0.0206 (2.09) | 0.4893 (3.74) |
| **Cell B — sample/CCI-series only** | Spec 6 form | Spec 11 sample (1988Q3–2024Q4) | 146 | −0.2617 | −2.492 | 0.0192 (1.17) | — | 0.2783 (2.05) |

**Decomposition of the total gap** (Spec 6 baseline λ = −0.239 → Spec
11 baseline λ = −0.448; total gap −0.209): isolating functional form
on Spec 6's own 86-quarter window (Cell A) moves λ to −0.542 — a
larger movement than the entire gap to the headline estimate — whereas
holding form fixed and only extending the sample and switching the
credit-conditions series (Cell B) moves λ to just −0.262, an order of
magnitude smaller shift. The change in adjustment speed is driven by
functional form, not by the larger sample.

Two qualifications are load-bearing. First, for the disaggregated
wealth channels' *significance* — not their sign or rough magnitude —
form and sample are complements, not substitutes: form alone (Cell A)
gets the sign and rough magnitude of `nla_y` and `ilfa_y` but does not
reach conventional significance on $n=86$ ($t=1.45$ and $2.09$) and
only crosses significance once the sample is also extended (baseline
Spec 11 $t=3.75$ and $3.09$); "form alone delivers significant
disaggregated wealth channels" is not a supportable claim. Second,
Cell B is form-*approximate*, not form-identical: `cci_williams` spans
negative values (a max-scaled spline index), so Spec 6's
$\Delta^2\log(\text{CCI})$ short-run transform is undefined for it; a
level second difference of `cci_williams`, lagged two quarters, is
substituted as the closest available "acceleration in credit
conditions" analogue, and the long-run block (`nla_y`, `eq_y`,
`super_y`, `ha_y`, `ln_hp_over_y`, `real_rate`, `ln_yp_over_y`,
`ln_yp_over_y_post2008`) is otherwise unchanged from Spec 6. Net
permitted reading: **form is decisive for the adjustment speed**;
**form alone does not activate the wealth channels' significance** —
the two together do.

### F.4 Deployed-protocol placebo on the full Spec-11 construction

The §D.3 placebo battery validates the additive knot-selection stage
of the CCI (Table D3). A separate, deployed-protocol placebo carries
each of 200 random-knot draws through the **full multiplicative Spec
11 construction** — the same object actually estimated, not just the
additive selection stage — so that the placebo tests what is
deployed, not what is merely selected.

**Table F6 — Deployed-protocol Spec-11 placebo (198 finite draws of
200).** Source: deployed-protocol placebo output.

| Metric | Real (institutional) value | Placebo median | Placebo p10–p90 | Real percentile | Higher is better? |
|---|---:|---:|---:|---:|:-:|
| adj-R² | 0.8244 | 0.7610 | [0.7434, 0.8040] | **93rd** | yes |
| logLik | 532.20 | 509.68 | [504.49, 524.17] | **93rd** | yes |
| BIC | −954.75 | −909.72 | [−938.70, −899.34] | **93rd** | yes (lower BIC better) |
| \|λ\| | 0.4483 | 0.2402 | [0.1974, 0.3794] | **98th** | yes |
| Joint Wald F, CCI block (5 terms) | 6.88 | 2.32 | [1.14, 4.97] | **94th** | yes |

Meta: 200 knot draws targeted, 198 usable (finite) draws; the real
institutional construction has 4 surviving knots from 15 candidates
(additive stage), a joint-CCI-block Wald $p = 1.05\times10^{-5}$, and
$\lambda=-0.4483$ on $n=146$ (seed 20260611).

Carried through the full multiplicative Spec-11 construction, the
institutionally timed credit-conditions index ranks at the 93rd
percentile of the placebo distribution on model fit, at the 94th
percentile on the joint significance of the credit-conditions block
(Wald $F=6.9$ vs a placebo median of 2.3), and at the 98th percentile
on the estimated adjustment speed. The institutional knot placement is
doing genuine identifying work at the deployed stage, not merely at
the additive spline-selection stage of Table D3. This is
**distributional (percentile) evidence that the CCI series as a whole
is informative**; it is **not** evidence that any individual
credit-interaction channel is separately identified — those are
governed by Table F7. The 94th-percentile ranking of the CCI block
does not license a claim that any single channel within it (e.g.
housing × CCI, rate × CCI) is significant.

### F.5 The nested bootstrap — headline structural-coefficient inference

Tables F1–F2 hold the CCI-knot selection and the permanent-
income/`ecm_lag` construction fixed at their full-sample point
estimates, so they understate the true sampling uncertainty in the
structural coefficients (§F.1). A nested bootstrap ($B=199$ usable
draws) instead re-selects the CCI knot set (the additive drop-on-
violation reduction of §D.2) **and** the `ecm_lag`/permanent-income
construction **inside** each resample draw, so both stages of
generated-regressor uncertainty are carried through to the final
coefficient interval.

**Table F7 — Nested 95% confidence intervals, Spec 11 (headline).**
Source: nested-bootstrap output, $B=199$.

| Coefficient | Point | Conventional 95% CI | Nested median | Nested 95% CI | Nested excludes 0? | Williams | Williams in nested CI? |
|---|---:|---:|---:|---:|:-:|---:|:-:|
| **λ (adjustment speed)** | −0.4483 | [−0.6946, −0.2019] | −0.1686 | **[−0.2728, −0.0774]** | **YES** | −0.286 | — |
| `nla_y` (γ) | 0.0600 | [0.0224, 0.0975] | 0.0381 | [−0.1739, 0.3168] | no | 0.159 | TRUE |
| `ilfa_y` (γ) | 0.0345 | [0.0120, 0.0570] | 0.0122 | [−0.0857, 0.1643] | no | 0.022 | TRUE |
| `ln_yp_over_y` (γ) | 1.0242 | [0.8648, 1.1837] | 0.6960 | [−0.1238, 1.6014] | no | 0.200 | TRUE |
| `yp_x_cci` (γ) | −1.1379 | [−2.0987, −0.1771] | −25.597 | [−103.31, 24.46] | no | 0.930 | TRUE |
| `ha_x_cci` (γ) | 0.0055 | [−0.0104, 0.0214] | −0.2847 | [−3.662, 2.357] | no | 0.0488 | TRUE |
| `hp_x_1_minus_cci` (γ) | 0.0623 | [0.0269, 0.0976] | 0.0728 | [−0.0737, 0.3755] | no | −0.130 | FALSE |

Width-inflation of the nested interval relative to the conventional
(delta-method/NW) interval: λ ×0.40 (nested is *narrower*, since the
nested statistic is the raw coefficient rather than a ratio); the
structural γ ratios inflate by ×5.4–×6.5 for `nla_y`, `ilfa_y` and
`ln_yp_over_y`, ×6.4 for `hp_x_1_minus_cci`, and ×66–×189 for the two
ratio-heavy CCI-interaction γ (`yp_x_cci`, `ha_x_cci`) — a mechanical
consequence of dividing by a bootstrap-resampled λ whose distribution
includes draws close to zero.

**Binding reading.** Only λ survives: its nested interval
[−0.273, −0.077] excludes zero, so the ECM adjustment speed's sign and
non-zero magnitude are identified once knot-selection and
`ecm_lag`/permanent-income construction uncertainty are carried
honestly. Note the nested interval is centred well inside the
conventional interval ([−0.695, −0.202]) and the nested median
$|\lambda|\approx0.17$ is attenuated relative to the 0.45 point
estimate — a mechanical consequence of letting `ecm_lag` become a
bootstrap-generated regressor; λ's sign and significance are the
robust finding, with the level attenuation flagged. All six structural
γ channels are statistically indistinguishable from zero once
knot-selection and PI/`ecm_lag` construction uncertainty are honestly
propagated — each is reported as sign-only, with magnitude
unidentified. Williams' calibrated value lies inside the nested CI for
`nla_y`, `ilfa_y`, `ln_yp_over_y`, `yp_x_cci`, and `ha_x_cci`; for
`hp_x_1_minus_cci` Williams' −0.13 sits just outside the nested CI, but
that channel is itself indistinguishable from zero, so no
individual-channel "reject Williams" claim is available on any of the
six structural elasticities. The delta-method intervals of Tables
F1–F2 are therefore anti-conservative for the structural γ and are
retained here as an appendix comparison only, not as headline
inference.

---

### Table and note index

| Section | Tables |
|---|---|
| A. Data construction | A1 |
| B. Specification battery | B1–B11 |
| C. Robustness | C1–C6 |
| D. CCI construction and placebo | D1–D5 |
| E. Permanent income | E1 |
| F. Structural-parameter inference | F1–F7 |

**References.** Citations in this appendix are as in the main-text
reference list, plus: Foster, R. A. (1996). *Australian Economic
Statistics 1949–50 to 1996–97*. Reserve Bank of Australia Occasional
Paper No. 8. MacKinnon, J. G. (1991). Critical values for
cointegration tests. In R. F. Engle and C. W. J. Granger (eds.),
*Long-Run Economic Relationships: Readings in Cointegration*, 267–276.
Oxford: Oxford University Press. MacKinnon, J. G. (2010). Critical
values for cointegration tests. Queen's Economics Department Working
Paper No. 1227.
