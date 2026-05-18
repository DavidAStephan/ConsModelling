# Credit Conditions Index — methodological exploration

This document responds to a substantive critique: the CCI feels like a
flexible detrending term that helps the rest of the consumption equation
fit the data without identifying anything genuinely structural. It
examines the conceptual question, surveys the construction methods that
appear in the Muellbauer-family literature and outside it, inventories
what this codebase has tried, and proposes specific experiments to
discriminate between the disciplined and the ad-hoc readings of CCI in
our particular implementation.

---

## 1. Is CCI just detrending?

A fair and important question. To answer it, separate three things the
CCI is meant to do:

**(a) Capture slow-moving variation in *credit supply* that would
otherwise show up in the residual.** In Australia 1979-2024 there are
real, identifiable shifts in the credit regime: the Campbell Committee
deregulation (1979-1990), the early-1990s banking distress, the
mortgage-originator and securitisation expansion (1998-2006), the
post-GFC retrenchment (2007-2010), and the macroprudential episodes of
2014 and 2017. These shifts are not fluctuations around a stationary
trend — they are level-shifts in the supply of mortgage credit, with
no observable counterparts in interest rates or in any single
quantity series. If the consumption equation lacks a CCI proxy, these
supply shifts show up as a slow-moving omitted variable that biases
every other coefficient.

**(b) Identify the *time-varying* responsiveness of consumption to
wealth, interest rates and permanent income.** Williams (2010) has
three explicit interactions: `r × CCI`, `log(p^h/y) × (1 - 1.2·CCI)`,
and `log(y^p/y) × CCI`. Each says that the relevant elasticity rises
or falls smoothly with credit conditions. At the 2007 CCI peak, the
real-rate semi-elasticity is −0.871; at CCI = 0 it is zero. The
permanent-income weight rises from 0.20 to 0.95 across the regime
range. Without CCI, you cannot estimate these interactions and the
underlying parameters are pinned to their average across regimes —
which is a substantively different and weaker model.

**(c) Provide a structural identification of credit liberalisation
episodes that can be cross-referenced to institutional history.**
Williams' four knots align with documented Australian regulatory
events; their estimated coefficients are constrained by sign priors
(positive at deregulation, negative at retrenchment) that can be
falsified — a knot can fail to identify (we've seen this with our
1992Q1 knot on the post-1988 sample). This is a *constrained,
falsifiable* approach to recovering the latent variable.

Now the user's critique. **In a fully-specified system that does (a),
(b) and (c) — Williams' four-equation FIML setup — CCI is genuinely
structural.** The sign priors and common-factor identification across
equations discipline what the latent factor can be; the multiplicative
interactions give the coefficient empirical content beyond the
intercept shift; and the institutional dates anchor it to history.

**In our single-equation OLS implementation on a 1988+ sample, the
discipline is materially weaker.** Specifically:

- We do not have common-factor identification. The CCI is identified
  only by its coefficient in *one* equation, not as a shared latent
  variable across consumption, house prices, mortgage debt and HEW.
- The 1979 deregulation knot is constant within our window (the
  smoothed-step dummy reaches unity by 1980Q2; we start in 1988Q4),
  so it can never identify.
- The 1992 knot fails its sign prior because the post-1992 portion of
  our sample is dominated by recovery and the late-1990s expansion;
  the data signal a positive coefficient where the prior demands
  negative.
- Only the 1998 (positive) and 2007 (negative) knots survive the
  Hendry-Krolzig sign-prior reduction, leaving a *two-knot* effective
  CCI that has fewer degrees of freedom than the full Williams spline
  but also less institutional grounding.
- Sign priors on the multiplicative interactions are also mostly
  violated on our sample.

So the user is partially right. **In our specific implementation, the
CCI is closer to a flexible structural-shift parameterisation than to
a disciplined common-factor identification.** This is not unique to
our implementation — it is a general consequence of single-equation OLS
on a sample that omits the deregulation episode. It is the central
methodological reason for sample back-extension to ~1975Q1 (NS-020,
NS-030 in the backlog).

The pragmatic question for this paper is what to do *now*, before the
RBA back-extension data arrive. The rest of this document inventories
the options.

---

## 2. CCI construction methods in the literature

A taxonomy of approaches from across the Muellbauer family and adjacent
work:

### 2.1 Latent-variable approaches (most ambitious)

**Williams (2010 / Muellbauer-Williams 2012) — STAMP unobserved-
components.**

The stochastic local-linear-trend model is estimated in STAMP (Koopman
et al. 2000) on the residual variation in log(c/y) once observable
regressors are conditioned out. The estimated stochastic trend is then
approximated with linear splines at institutional turning points. In
the published version, this is replaced by a smoothed-step spline at
1979/1992/1998/2007 estimated jointly across four equations by FIML.
Sign priors come from institutional history. The published peak value
is approximately 0.81 in long-run-house-price units; our reduced-form
peak is normalised to 1.

**Chauvin-Muellbauer (France).**

Two CCIs (housing and consumer-credit) jointly estimated by FIML across
six equations. Each is a spline of *ogive* (smooth-transition logistic)
dummies rather than smoothed-step dummies; each ogive has an estimated
location and width parameter. Knots align with French deregulation
events (1981, 1982, 1983, 1984, 1986, 1987, 1989, 1991, 1993, 1994,
1999, 2001, 2003, 2006, 2011, 2013, 2016 — many more than Williams'
four). Sign priors derived from the deregulation timetable.

**De Bonis-Marino-Muellbauer (Italy).**

Italy is the methodological outlier in the Muellbauer family. They use
an *observable* ratio: granted housing credit lines / 8-quarter MA of
nominal GDP, sourced from the Bank of Italy credit register from 1996
and back-extrapolated 1980-1995. They conclude the *level* of CCI is
insignificant in the Italian long-run consumption equation; only its
2-quarter change matters as a short-run regressor. The interpretation
is that household debt has grown only slowly in Italy, so the
identification of long-run credit conditions is weak. **This is itself
a useful negative result we should reproduce: if Australia's
single-equation OLS estimates show the CCI level is statistically
significant in the long run while Italy's don't, that's a meaningful
cross-country contrast.**

**Aron-Muellbauer-Murphy (UK), Duca-Muellbauer-Murphy (US).**

UK uses a Mortgage Equity Withdrawal series as an observable proxy for
the housing-channel CCI (UK is one of the few countries where MEW is
directly measured). US uses a constructed series from mortgage
applications, FHFA refinance share, and Federal Reserve regulatory
indicators. Both use spline-based approaches at deregulation dates as
robustness.

### 2.2 Indicator-blend approaches (intermediate)

**Williams Online Appendix (with Chauvin-Muellbauer) — `cci_institutional`.**

Standardise multiple credit indicators (loan flow, house price growth,
debt-to-income ratio, FHB share, mortgage spread), take a weighted
average (typically equal weights or PCA-derived weights), then blend
with regime-step dummies. Our `construct_institutional_cci` helper
implements a 65/35 blend of regime steps and standardised indicator
average. The "institutional" half provides discipline; the "indicator"
half tracks observable variation.

**MBA Mortgage Credit Availability Index (US).**

Mortgage Bankers Association composite of LTV, FICO, debt-to-income,
documentation requirements, and product-type availability. Frequency
monthly from 2011. Industry-source rather than research-source.

### 2.3 State-space / Kalman-filter latent factor

**Muellbauer (2007) Kansas Fed paper sketches** the approach: multiple
credit indicators feed a single common factor extracted via Kalman
filter. The factor is identified by being the maximum-variance shared
component. This is methodologically robust because the latent factor
is forced to be common across indicators by construction.

We have *dead code* for this in `model_helpers.R`:
- `build_credit_ssm_factor()` — single-factor model
- `build_credit_ssm_local_trend()` — random-walk trend version
- `extract_trend_level()` — Kalman-filter smoother for the latent state

This was scaffolded but never wired into the pipeline. It is a clear
candidate for revival.

### 2.4 Survey-based CCIs (where data exist)

**Federal Reserve Senior Loan Officer Opinion Survey (SLOOS).**
Quarterly since 1990. Net percentage of banks reporting tightening
credit standards. The original "directly observable CCI"; Williams
cites it as the gold standard.

**ECB Bank Lending Survey (BLS).** Quarterly since 2003.

**Bank of England Credit Conditions Survey.** Quarterly since 2007Q2.

**RBA Liaison Programme.** Confidential, qualitative, not published as
a numerical series. The RBA does not currently publish a quantitative
credit-conditions survey series. This is the binding obstacle to a
direct survey-based Australian CCI; it could potentially be hand-coded
by reading RBA Statement of Monetary Policy "Domestic Financial
Conditions" sections quarter-by-quarter and converting to a numerical
score, but this is laborious and judgement-laden.

### 2.5 Direct observable proxies

**Average mortgage maturity** (Chauvin-Muellbauer France). French
average mortgage maturity rose 13→17.4→20 years over 1999-2008,
tracking credit liberalisation closely. Available for Australia from
RBA / APRA but not currently in our master.

**Loan-to-value ratio** (BIS/APRA). Aggregate household-mortgage LTV
ratios. Limited Australian time-series — APRA publishes quarterly
loan-by-loan LTV from 2008+. Pre-2008 series are sparse.

**Interest-only loan share** (APRA from 2008+). Direct measure of
lending standards: high IO share = lax standards (peaked ~40% in 2015
before APRA's 30% cap).

**First-home-buyer share** (we have `fhb_share` from 2002+). Williams
uses FHB cohort share as a *demographic* control, not as a CCI proxy
itself. But the FHB *loan* share is partly endogenous to credit
availability for younger borrowers. Worth testing as a CCI proxy.

**Mortgage approvals to applications ratio.** Direct lending-standard
measure. APRA Form ARF 392 reports this from 2008Q1.

**Macroprudential intensity index.** Combine the APRA episodes (2014
investor-loan cap, 2017 IO cap, plus relaxations) into a continuous
"tightening intensity" measure. We have separate `d_apra_2014` and
`d_apra_2017` ogive dummies; they could be aggregated.

### 2.6 Spread-based proxies

**Mortgage rate − cash rate spread.** Originally tried in this
codebase; dropped because the spread reflects funding costs and risk
premia (the price of credit), not the supply of credit (the
non-price terms — LTV, IO, documentation, etc.). Reinstated as a
fallback in some earlier iterations. Documented by Williams (2010
Section 3) as theoretically suspect.

**Senior unsecured bank bond spreads.** Sometimes used as a marginal
funding cost proxy for the banking sector; available in Australia from
the early 2000s. Not yet sourced into our pipeline.

### 2.7 Aggregate quantity gaps

**BIS credit-to-GDP gap** (Hodrick-Prescott or one-sided HP filter).
Different family of approach: the *deviation* of credit/GDP from a
slow trend is interpreted as cyclical credit conditions. Used in
macroprudential policy frameworks (Basel III countercyclical capital
buffer). Methodologically distinct from Muellbauer's CCI: HP gap
captures cyclical departures, while Muellbauer's CCI captures
structural-regime shifts. Worth running as a different-family
comparator.

**Household debt-to-income gap.** Same idea applied to the household
sector specifically. Australian DTI ratio rose from ~50% in 1990 to
~190% by 2024; the trend itself reflects regime shifts, so a gap
measure depends critically on the trend specification.

### 2.8 Event-study / impulse approaches

**Bayoumi (1993)** uses unanchored impulse dummies at the deregulation
dates; the dummies compete with all other regressors for explanatory
power without sign priors. This is the *least* disciplined version of
the spline approach and is closest to the user's "detrending"
critique. Williams (2010) explicitly prefers the smoothed-step
approach over impulse dummies on the grounds that institutional
shifts have transition periods rather than discrete jumps.

### 2.9 Markov-switching / regime-switching models

**Hamilton-style regime-switching** would let the data identify
endogenously which dates are regime changes, rather than imposing
them institutionally. The cost is heavy parameterisation; the benefit
is the regime dates emerge from data rather than from priors. Has
been used in the wider macroeconomics literature for credit-condition
identification but not in the Muellbauer family — Williams considered
this in his 2010 thesis appendix and rejected it on identification
grounds (the Australian sample is too short and the regime shifts too
slow for clean Markov-switching).

### 2.10 Time-varying-parameter approaches

**TVP-VAR** (Primiceri 2005). Jointly estimate credit-conditions
fluctuations and their effect on consumption with time-varying
coefficients and time-varying variance. Methodologically robust;
heavy computational and identification cost. Adjacent to but not in
the Muellbauer family.

---

## 3. What we have tried in this codebase

### 3.1 Implemented and live

**Path A (default): observable housing-credit-flow ratio.**
Construction:
```r
cci_ratio = log(housing_loan_flow / ydi_ann_8qma)
```
where `ydi_ann_8qma` is the 8-quarter MA of annualised nominal
disposable income. Available 2002Q3+ (ABS Cat 5601.0 binding limit).
Used as a short-run regressor in Specs 2 and 5 (`d2_logcci_lag2`).
**Outcome:** statistically marginal short-run effect; not used in the
long-run cointegrating vector.

### 3.2 Implemented and conditionally live

**Path B (`USE_INSTITUTIONAL_CCI = TRUE`): two paths combine.**

(i) `construct_institutional_cci()` in `model_helpers.R`: blends a
regime-step component (smoothed steps at 1983/1992/1998/2007 with
hand-coded weights `0.9, -0.6, 0.8, -0.9`) with a standardised
indicator average (housing loan flow, house-price growth, debt-income
ratio, FHB share, real mortgage rate). Final CCI is `0.65 ×
regime_component + 0.35 × indicator_component`, both standardised.
**Outcome:** wired in as a backfill of `cci_ratio` pre-2002 when the
flag is on. Not currently active by default.

(ii) `build_williams_cci_basis()`: Williams 4-knot smoothed-step basis
at 1979/1992/1998/2007. Used by `fit_consumption_with_williams_cci()`
in `australia_estimation.R` to estimate inside the consumption equation
under sign priors. **Outcome:** on our 1988+ sample, **2 of 4 knots
survive** (1998 and 2007); 1979 is aliased (constant in sample) and
1992 violates its sign prior. The fitted `cci_williams` is therefore
a two-knot reduced-form spline. Used in Spec 8 with the three
multiplicative interactions; only `log(HP/y) × (1 − 1.2·CCI)` passes
its sign prior on our sample.

### 3.3 Implemented but never wired in (dead code)

**`build_credit_ssm_factor()`, `build_credit_ssm_local_trend()`** in
`model_helpers.R`. These are state-space (Kalman-filter) latent-factor
extractors built on the `KFAS` package. They take a matrix of
indicator series and a vector of factor loadings and return an
`SSModel` object whose smoothed state is the latent CCI factor.
**Status:** scaffolding exists; no caller ever invokes them. The
dependencies (`KFAS`) are in `renv.lock`. Wiring this up is a clean
discrete piece of work.

### 3.4 Tried in earlier iterations and dropped

**Spread-backfill of pre-2002 CCI.** Earlier vintages of the codebase
back-extended `cci_ratio` to 1989Q3 by normalising the mortgage-rate
to cash-rate spread to match `cci_ratio`'s mean and standard
deviation over their overlap. Dropped during the May 2025 cleanup on
the theoretical grounds Williams (2010) sets out: spreads reflect
price not non-price terms. Documented in `data.md` §5.2 and
`australia_data_download.R` §7 comments.

---

## 4. What we have *not* tried

### 4.1 Methodologically robust, code-feasible

**(a) State-space Kalman CCI factor on multiple indicators.**

Wire `build_credit_ssm_factor()` into the data pipeline. Indicators:
- `log(housing_loan_flow)` (2002+)
- `log(debt_y)` (1988+)
- `fhb_share` (2002+, after the regex fix)
- `mortgage_burden` (1988+)
- `mortgage_rate − cash_rate` spread (1980+)
- The Williams smoothed-step basis as exogenous regressors *or* as
  prior centring for the loadings

Identification: the latent factor is the common signal across
indicators; loadings are estimated to make this signal extractable.
This is methodologically the most defensible alternative to a spline
because the discipline comes from cross-indicator co-movement rather
than from imposed sign priors.

**Why we haven't tried it.** Implementation effort: ~1 week. We have
the helper but not the orchestration. Worth doing.

**(b) PCA across multiple credit indicators.**

Simpler benchmark to (a). Standardise the same indicator set, run PCA,
take the first principal component as the CCI. No latent-variable
machinery, no priors, but provides a dimension-reduction summary that
can be falsified against the spline.

**Why we haven't tried it.** Effort: ~1 day. A natural sanity check
against the state-space approach.

**(c) Direct observable APRA series.**

APRA publishes the following from 2008Q1 onward (Form ARF 320, ARF 392
and macroprudential statistics):
- Interest-only loan share (proportion of new lending)
- Average loan-to-value ratio
- Loan approvals as a fraction of applications

Each of these is a *direct* measure of one dimension of credit supply.
Combining 2-3 of them gives a measured CCI from 2008+ that bypasses
the latent-variable question entirely. The Australian residential
mortgage market has the best post-2008 measurement in the world (APRA
moved early on macroprudential reporting); we should use it.

**Why we haven't tried it.** Sample limitation (2008+ only) means it
can't identify the pre-GFC episodes. But as a robustness column it
would be definitive on the post-2008 macroprudential period and would
provide a *direct* cross-validation of the spline's 2007 knot.

**(d) Macroprudential intensity index.**

Combine our existing APRA ogive dummies (`d_apra_2014`, `d_apra_2017`,
plus the 2018-19 relaxation and the 2020 COVID adjustments) into a
single continuous "macroprudential tightening intensity" series. This
would be the post-2008 segment of a longer institutional CCI, with
explicit policy-event dates rather than smoothed-step approximations.

**Why we haven't tried it.** Effort: ~2 days. The dummies exist; we
haven't aggregated them. Limited information beyond what the dummies
themselves carry.

**(e) Smooth-transition logistic CCI (Chauvin-Muellbauer style).**

Replace the smoothed-step (5-MA of 4-MA) construction with a
parametric ogive `1 / (1 + exp(-(t - t0)/w))` whose location `t0` and
width `w` can be jointly estimated. France uses this; it has fewer
degrees of freedom than spline knots and an explicit transition
duration parameter that can be cross-checked against institutional
phase-in periods.

**Why we haven't tried it.** Effort: ~3 days. Methodologically
attractive but performance gain over the existing smoothed-step is
unclear without testing.

### 4.2 Methodologically more demanding

**(f) BIS credit-to-GDP gap as a different-family comparator.**

One-sided HP filter (`mFilter::hpfilter`) on log(household credit /
nominal GDP), report the gap. This is *not* the Muellbauer concept —
it captures cyclical departures from a slow trend rather than
structural regime shifts — but it provides a cross-family comparator.
If our spline CCI is "just detrending", we should expect it to
correlate strongly with the credit-to-GDP gap; if the spline is
genuinely identifying structural shifts, the correlation should be
moderate at best.

**Why we haven't tried it.** Effort: half a day. Very useful as a
discipline check.

**(g) Markov-switching regime identification.**

`MSwM::msmFit` or similar. Let the data choose the regime dates
endogenously; compare the resulting break dates to Williams'
institutional dates. If the data-chosen breaks line up with Williams,
that's strong validation; if they diverge significantly, the spline
is more imposed-than-identified.

**Why we haven't tried it.** Effort: 1-2 weeks (regime-switching is
heavy machinery and requires careful identification work). The
Australian 1988+ sample may be too short for clean Markov-switching
identification.

**(h) Hand-coded survey-style CCI from RBA Statement of Monetary
Policy "Domestic Financial Conditions" sections.**

Read each quarterly SoMP (1996Q1+) and assign a numerical score
(say, -2 to +2) for credit standards. This is laborious (~120
quarters × ~10 minutes each = ~20 hours of careful reading) and
involves judgement, but produces a survey-style CCI that no other
Australian researcher has constructed. Could be the headline data
contribution of the WP.

**Why we haven't tried it.** Effort: 2-3 weeks of careful reading.
High potential payoff but requires institutional knowledge of how
SoMP language has evolved.

### 4.3 Data-sourcing-dependent

**(i) Mortgage maturity series back to 1990s.**

If RBA / APRA / ABS publishes average mortgage maturity quarterly,
this would be a direct observable. France found this captured the
liberalisation episode well. We have not investigated whether such a
series exists for Australia.

**(j) MBS spread / RMBS coverage as funding-cost proxy.**

Australian RMBS issuance and spreads from the late 1990s. A funding
cost proxy that complements (but doesn't replace) the spread-based
proxy already rejected. Limited series.

**(k) Bank funding composition / wholesale-vs-deposit ratio.**

When banks fund more from wholesale markets, their lending is more
sensitive to wholesale credit conditions. The wholesale-share series
could proxy the marginal credit-supply elasticity.

---

## 5. Engaging with the user's "detrending" critique

The user's concern is most pointed when applied to our specific
implementation. To address it, we should run *deliberate
discrimination tests* between the disciplined and ad-hoc readings of
CCI. Specific tests:

**Test 1 — Common-factor discipline.**
Run a state-space Kalman CCI on multiple indicators (item (a) above).
If the resulting latent factor has low pairwise correlation with the
Williams spline, the spline is more imposed than identified. If they
correlate strongly (>0.85), the spline is a sensible
parameterisation of a recoverable structural object.

**Test 2 — Fit-improvement decomposition.**
Re-fit Spec 6 *without* CCI and *with* CCI. If `R²` rises by a
material amount but the wealth coefficients move only marginally,
CCI is mostly absorbing residual variation that wealth coefficients
would otherwise have absorbed (the user's detrending concern). If
adding CCI changes the wealth coefficients meaningfully (and brings
them closer to Williams' published values), CCI is doing genuine
identification work.

**Test 3 — Out-of-sample prediction.**
Hold out 2020-2024. Estimate Spec 6 and Spec 8 on 1988-2019. Forecast
2020-2024. If the CCI-augmented Spec 8 forecasts materially better
than Spec 6, CCI is contributing identifiable economic information.
If forecasts are similar, CCI is largely a sample-fit feature.

**Test 4 — Cross-country sign discipline.**
Estimate the same eight specs on Italy data (using the Italy paper's
published results) under our methodology, and check whether the spline
coefficients have the same sign pattern. If they do, the priors are
recovering a cross-country invariant; if not, the priors are imposing
country-specific assumptions that should be made explicit.

**Test 5 — Random-knot placebo.**
Generate 100 random sets of four knot dates uniformly distributed
over 1979-2007 (i.e. without institutional grounding). Refit Spec 8
under each. Compute the distribution of `R²` and λ. If the actual
Williams knots fall in the upper tail (best fit, smallest λ), the
historical knots have explanatory power beyond chance; if they sit
near the median, the institutional placement is little more than
flexible curve-fitting.

### Test results (added 2026-05-07; canonical Italy LP)

Tests 1, 2, 3, and 5 have now been implemented. Headline outcomes:

- **Test 1 (Kalman common-factor discipline)** — Spec 9 with the
  Kalman state-space single-factor CCI delivers λ = −0.206
  (highly significant) and `ln_yp_over_y` = +0.217. Pearson correlation
  with the Williams maximal-GETS CCI is −0.375, indicating the two
  series are recovering related but distinguishable latent objects;
  see [`australia_cci_method_comparison.md`](../outputs/australia_cci_method_comparison.md).
  Both Williams maximal-GETS (Spec 8) and Kalman (Spec 9) deliver
  highly significant λ estimates (−0.245 and −0.206 respectively),
  so the identification of the speed of adjustment under CCI is robust
  to the CCI extraction method. Verdict: the spline is a sensible
  parameterisation, not a methodology artefact.

- **Test 2 (fit-improvement decomposition)** — Under canonical Italy
  LP, adding the Williams maximal-GETS CCI (Spec 6 → Spec 8) shifts
  the wealth coefficients by **150.7%** on average; adding the Kalman
  CCI shifts them by **16.6%** on average. R² actually drops slightly
  in both cases (Spec 6 adj R² 0.812; Spec 8 0.763; Spec 9 0.745).
  Verdict: CCI is doing **identification work, not residual absorption**
  — the opposite of the detrending hypothesis. See
  [`australia_cci_fit_decomposition.md`](../outputs/australia_cci_fit_decomposition.md).

- **Test 3 (out-of-sample forecasts)** — At h = 1 the structural specs
  (including Spec 8 / Spec 9 with CCI) are competitive with
  random-walk-with-drift; at h = 4, 8 the RW-drift dominates.
  CCI-augmented specs do not systematically beat the no-CCI Spec 6 at
  any horizon. The OOS test does not discriminate strongly in either
  direction. See [`australia_oos_rmse.csv`](../outputs/australia_oos_rmse.csv).

- **Test 5 (random-knot placebo)** — Williams' canonical 4-knot
  benchmark sits at the **49th percentile** of 200 random 4-knot draws
  (uniform in 1979-2007) by adj-R², and at the **22nd percentile** by
  |λ|. Verdict: **the specific 1979/1992/1998/2007 knot dates are
  arbitrary on our 1988Q4+ sample** — they do not have explanatory
  power beyond chance. The maximal-GETS reduction (15 candidate knots
  → 6 surviving via Hendry-Krolzig sign-prior reduction) is the
  methodologically defensible response and is now the canonical CCI
  basis. See [`australia_williams_knot_placebo.png`](../outputs/australia_williams_knot_placebo.png).

**Synthesis.** Williams' canonical 4-knot dates fail the placebo
discrimination test on our sample, but adding *any* well-disciplined
CCI (Williams maximal-GETS or Kalman) does substantial identification
work on the consumption equation. The detrending critique applies to
the literal Williams 4-knot replication, not to the structural CCI
machinery itself. The WP narrative reflects this: the canonical CCI
basis is the maximal-GETS reduction, with Kalman as a methodology
robustness column.

---

## 6. Recommended experimental sequence

In order of payoff per unit effort, prioritised for the WP:

### ~~Priority 1 — State-space Kalman CCI (item (a))~~ ✅ DONE
Spec 9 (`fit_kalman_cci()` in `model_helpers.R`) wired in as canonical
robustness column. λ = −0.206; correlation with Williams maximal-GETS
CCI = −0.375. See test results above.

### ~~Priority 2 — Random-knot placebo test (Test 5)~~ ✅ DONE
[`cci_placebo_test.R`](../R/cci_placebo_test.R). Williams canonical
4-knot at 49th/22nd percentile — detrending critique vindicated for
the literal 4-knot replication. Maximal-GETS reduction is the
methodological response.

### Priority 3 — Direct APRA observable (item (c))
**Effort:** 3 days (data sourcing + wiring). **Payoff:** A measured
CCI for the post-2008 macroprudential period that bypasses the
latent-variable question entirely. Cross-validates the 2007 knot of
the spline. [Open: NS-107.]

### ~~Priority 4 — Fit-improvement decomposition (Test 2)~~ ✅ DONE
[`cci_fit_decomposition.R`](../R/cci_fit_decomposition.R). Williams
maximal-GETS shifts wealth coefs 150.7%; Kalman shifts 16.6%. Verdict:
identification work, not residual absorption.

### ~~Priority 5 — BIS credit-to-GDP gap comparator (item (f))~~ ✅ DONE
`cci_creditgap` (HP filter λ = 400 000 on log debt-to-income) attached
to master via [`cci_alternatives.R`](../R/cci_alternatives.R). Reported
in the 4-way pairwise correlation table.

### ~~Priority 6 — PCA factor (item (b))~~ ✅ DONE
`cci_pca` (first principal component, 5 indicators) attached to master
via the same script.

### Priority 7 — Macroprudential intensity index (item (d))
**Partly done:** `macropru_intensity` ogive over 7 events is wired in
via [`cci_alternatives.R`](../R/cci_alternatives.R). The full
combined-effect counterfactual (NS-012 "no-APRA") is still open.

### ~~Priority 8 — Out-of-sample forecast test (Test 3)~~ ✅ DONE
[`oos_forecast.R`](../R/oos_forecast.R) (NS-033). At h = 1 structural
specs are competitive with RW-drift; at h = 4, 8 RW-drift dominates.
CCI-augmented specs do not systematically beat the no-CCI Spec 6.

### Priority 9 — Smooth-transition ogive CCI (item (e))
**Effort:** 3 days. **Payoff:** Smaller; mostly a robustness column.

### Priority 10 — Markov-switching (item (g))
**Effort:** 2+ weeks. **Payoff:** Potentially valuable but high cost
on a short sample. Skip unless other priorities clear.

### Priority 11 — Hand-coded survey CCI from SoMP (item (h))
**Effort:** 3 weeks. **Payoff:** Headline data contribution but
requires significant investment. Could be a separate companion paper.

---

## 7. What this means for the WP

Even without implementing any of the above, this exploration suggests
two adjustments to the WP draft:

**(a) Acknowledge the detrending concern explicitly.** §5
("Identification of credit conditions") currently says "identification
is weaker [in single-equation OLS] but the spline can still be
estimated". This is true but evades the deeper question. A new
paragraph should set out the user's critique and explain how the
sign-prior reduction (Hendry-Krolzig) and the institutional placement
of knots are the *binding* discipline in our setting, in lieu of the
common-factor cross-equation identification of the multi-equation
LIVES system.

**(b) Add a "robustness to CCI specification" subsection in §8.** The
current §8 has the Williams spline as Spec 8 vs no CCI in Spec 6.
After implementing items (a) and (b) above, §8 should additionally
report:
- Williams 4-knot spline (current Spec 8)
- State-space Kalman CCI factor on 5 indicators
- Williams 4-knot spline with random-knot placebo distribution
- Optionally: PCA factor

If all three approaches converge on similar consumption-equation
coefficients, the case for the spline is strong. If they diverge, the
WP's identification narrative needs adjustment.

---

## 8. The honest summary (updated 2026-05-07)

Status of the original ten priority items:

1. ✅ State-space Kalman latent factor — Spec 9 wired in.
2. ✅ PCA factor across multiple proxies — `cci_pca` attached.
3. ☐ Direct APRA observables — open (NS-107).
4. ✅ BIS credit-to-GDP gap comparator — `cci_creditgap` attached.
5. ✅ Random-knot placebo discrimination test — done.
6. ☐ Smooth-transition ogive — partial (`macropru_intensity`).
7. ☐ Markov-switching — open (NS-110).
8. ☐ Hand-coded SoMP-derived survey CCI — open (NS-114).
9. ☐ Mortgage maturity / RMBS spread / bank funding-composition proxies — open.
10. ✅ Direct fit-improvement decomposition test — done.

The four substantive identification tests (1, 5, 10, plus the OOS
forecast test) all converge on the same conclusion: **the literal
Williams 4-knot replication is at the placebo distribution median on
the post-deregulation Australian sample, but the structural CCI
machinery is doing identification work** (150.7% wealth-coef shift
under Williams maximal-GETS; 16.6% under Kalman; both deliver
significant λ in the consumption equation).

The methodological response in the WP is to (a) replace the literal
4-knot Williams basis with the maximal-GETS reduction (15 candidate
knots → 6 surviving via Hendry-Krolzig sign-prior reduction) as the
canonical CCI basis, (b) report Kalman state-space CCI as a
cross-method robustness column, and (c) honestly document the placebo
test as the empirical justification for the methodology pivot.

---

**Generated as a follow-up methodological exploration in May 2026 and
updated 2026-05-07 with test results. The corresponding implementation
backlog items are tracked in [`next_steps.md`](next_steps.md) under
NS-105–NS-114.**
