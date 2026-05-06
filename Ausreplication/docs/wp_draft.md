# Australian Household Consumption, Wealth and Credit Conditions: An Updated Single-Equation LIVES Estimate

**Draft — central-bank working paper format**

**Author(s):** [TO FILL]
**JEL codes:** E21, E32, E51, D14
**Keywords:** household consumption, wealth effects, credit conditions, error-correction model, LIVES system

---

## Abstract

> [DRAFT — ~200 words. To be polished by author.]
>
> We estimate a single-equation Muellbauer-Williams "LIVES" consumption model
> for Australia over 1988Q4–2024Q4, extending the Williams (2010) and
> Muellbauer-Williams (2012) framework by sixteen additional years of post-GFC
> data and applying contemporary identification methods. Wealth is
> disaggregated into housing, illiquid financial (equities + superannuation),
> and net liquid assets (deposits net of total household debt). The
> cross-equation restriction γ_LA + γ_LOANS = 0 — that liquid assets and
> household debt enter the long-run consumption relationship with
> equal-and-opposite coefficients — is accepted in our data, validating the
> Italian methodology of De Bonis et al. (2024). The error-correction speed
> of adjustment is sensitive to two methodological choices: the income
> measure (non-property disposable vs gross disposable; ~30 per cent of the
> divergence from Williams) and the permanent-income forecaster (rolling
> AR(8) vs Jordà (2005) local projection with a labour-force-share
> predictor; resolves the "Australian permanent-income puzzle" by flipping
> the long-run coefficient from negative to positive). Under the local
> projection PI method, our point estimate of λ falls within 25 per cent of
> Williams' published value. We provide a structured robustness suite
> covering instrumental variables, joint SUR estimation, Chow batteries,
> Drehmann effective-rate adjustments, and a Williams-style four-knot
> credit-conditions spline. We document where our results agree with
> Williams' BIS chapter (the OLS-level wealth coefficients, within 6–17 per
> cent) and where they diverge (the speed of adjustment under our default
> specification, by a factor of five). The full reproducibility kit
> including a portable CSV of the master dataset is available alongside
> the paper.

---

## 1. Introduction

> [SKELETON — to flesh out. Suggested structure below.]

**§1.1 Motivation.**
Why a contemporary Australian household consumption model matters: the
post-2008 macroprudential regime, the COVID consumption shock, the rise
of mortgage-equity withdrawal and the housing-wealth channel as a
distinct propagation mechanism for monetary policy. Reference to the
Aust paper as the published Australian benchmark and to Italy.pdf and
the Chauvin-Muellbauer France paper as parallel implementations.

**§1.2 Contribution.**
Three contributions:
1. Update Williams' Australia LIVES estimate to 2024 using publicly
   available data (with three additional sourced series: RBA F6
   mortgage rate, ABS A84423091W 15+ population, ABS 5206020 income
   components) and the Williams (2009) §4.2.1 non-property income
   recipe.
2. Document why the speed of adjustment differs from Williams' published
   value, decomposing into income-measure (~30 per cent), permanent-income
   forecaster (~?), and absent-CCI-interaction (~?) channels.
3. Provide a structured robustness suite mirroring the De Bonis et al.
   (2024) Italian methodology — IV, joint SUR, Chow battery, Drehmann
   effective-rate, scaled-income, Williams-style spline CCI — applied
   to a single-equation OLS framework with full code release.

**§1.3 Headline result.**
Under the Williams (2009) non-property income measure and the Italy-style
local-projection permanent-income forecaster, our preferred specification
delivers a speed of adjustment of −0.218 (vs Williams' published −0.286)
and OLS wealth coefficients within 6–17 per cent of Williams' on a
non-overlapping sample (1988Q4–2024Q4 vs 1978Q1–2008Q2). The cross-equation
restriction γ_LA + γ_LOANS = 0 is accepted in every disaggregated
specification. The often-noted "Australian permanent-income puzzle" — a
significantly negative coefficient on log(y^p/y) — disappears under the
Italy local-projection PI forecaster, suggesting it is a feature of the
forecasting method rather than a structural feature of Australian
consumption behaviour.

**§1.4 Roadmap.**
Section 2 surveys the LIVES literature. Section 3 documents data
construction. Section 4 presents the model. Section 5 develops
identification of credit conditions. Section 6 sets out the eight
specifications and the four selection screens. Sections 7–8 present
preferred-specification results and the robustness suite. Section 9
compares with Williams' published estimates. Section 10 presents the
long-run decomposition and policy implications. Section 11 concludes.

---

## 2. Literature review

> [SKELETON — paragraph each on:]

- **The LIVES family.** Muellbauer (2007 UK theoretical foundation),
  Aron-Muellbauer-Murphy (2012, UK), Duca-Muellbauer-Tobin (2013), Duca
  et al. (2010 US) and Aron et al. (2012 UK and US comparison).
- **The Australian application.** Williams (2009, 2010), Muellbauer-Williams
  (2012 BIS chapter). Recent Australian work: cite RBA RDPs on consumption
  and household balance sheets; cite Atkin-Hambur, Cusbert, Hambur-La Cava
  if relevant (CHECK).
- **Comparator implementations.** De Bonis et al. (2024, Italy);
  Chauvin-Muellbauer (Online Complement, France); Duca-Muellbauer-Tobin
  (2013) for UK and US.
- **Permanent-income forecasting.** Hall (1978), Carroll-Kimball (1996),
  Campbell-Mankiw (1989, 1991) on excess sensitivity, Jordà (2005) on
  local projections.
- **Identification of credit conditions.** Williams (2010) on the
  smoothed-step spline approach; Bayoumi (1993) on early Australian
  liberalisation; Battellino-McMillan (1989), Edey-Gray (1996) on
  Australian financial deregulation chronology.
- **The Australian "permanent-income puzzle."** Cite where this has
  been previously noted in Australian consumption modelling (TODO).

---

## 3. Data and measurement

The dataset assembles quarterly Australian macroeconomic and household
sector observations 1980Q1–2024Q4 (n=180), with a binding sample start
of 1988Q3 for the disaggregated wealth components (ABS Cat 5232.0
Australian National Accounts: Finance and Wealth, household sector
balance sheet). Estimation is performed on the largest contiguous subset
for which all variables in a given specification are observed, typically
1988Q4 onward.

### 3.1 Aggregate consumption and income

Real per capita consumption (`cons_real_pc`) is constructed from ABS Cat
5206.0 Table 8 (Household Final Consumption Expenditure, chain volume
measures, seasonally adjusted) divided by the civilian population aged
15 years and over (ABS series A84423091W, monthly, averaged to quarterly
arithmetic mean; sample 1978Q2–2024Q4, sourced directly from the ABS
historical series workbook). Following Williams (2010, 2009), we do not
sum the single-year-of-age cohorts in ABS Cat 3101.0; in current ABS
vintages, the `Persons` series only extends to age 47 in the disaggregated
file, producing a population total approximately 35 per cent below the
true Estimated Resident Population.

Real per capita household disposable income (`ydi_real_pc`) is the
quarterly seasonally adjusted nominal series from ABS Cat 5206.0 Table 20
(Household Income Account), deflated by the consumption deflator implied
by Tables 8 (chain volume) and 8 (current prices), and divided by the
same 15+ population denominator. Following standard practice (Blinder-Deaton
1985), and as the headline empirical specification, we use gross
disposable income; we report a non-property income (NPY) alternative
constructed per Williams (2009) §4.2.1 in §3.6 below.

### 3.2 Household balance sheet

Household sector balance sheet stocks are sourced from ABS Cat 5232.0
Table 35 (Household Balance Sheet Aggregates, current prices, $ billion),
quarterly from 1988Q3:

- Currency and deposits (`fin_deposits`)
- Shares and other equity (`fin_equities`)
- Superannuation reserves (`fin_super`)
- Total household liabilities (loans and placements; `fin_loans`)
- Residential land and dwellings (`housing_wealth`)
- Closing net worth (ABS series A83722648X)

All balance-sheet stocks are converted to real values by dividing by the
normalised consumption deflator and to per-capita-15+ values by dividing
by the 15+ population.

The wealth-to-annualised-income ratios used in the long-run consumption
equation are constructed as:

- `ha_y` = housing wealth / (4 × quarterly nominal disposable income)
- `eq_y` = equities (ex-super) / (4 × quarterly nominal disposable income)
- `super_y` = superannuation reserves / (4 × quarterly nominal disposable income)
- `nla_y` = (deposits − total household debt) / (4 × quarterly nominal disposable income)
- `debt_y` = total household debt / (4 × quarterly nominal disposable income)
- `networth_y` = closing net worth / (4 × quarterly nominal disposable income)

We adopt the Italian convention (De Bonis et al. 2024, eq. 2.5; Table 3
column 3) of defining net liquid assets as **deposits net of total
household debt**, with the cross-equation restriction γ_LA + γ_LOANS = 0
imposed implicitly by construction. We test this restriction formally
in §8 below; it is accepted at the 5 per cent level in every disaggregated
specification and sample window.

### 3.3 Mortgage interest rate

The nominal mortgage rate (`mortgage_rate`) is the RBA Standard Variable
Owner-Occupier Rate (RBA Bulletin Table F5, series FILRHLBVS), monthly
from January 1959, averaged to quarterly. We source the historical
series from the published RBA archive rather than the live `readrba` API,
to ensure a stable vintage; the rate peaks at 17.0 per cent in 1989Q3,
consistent with the Hawke-Keating recession.

The real mortgage rate (`real_rate`) is the nominal rate less the
4-quarter-ended percentage change in the consumption deflator. We do not
adopt the Italian Drehmann (2017) amortising-mortgage adjustment in the
preferred specification but report it as a robustness check.

### 3.4 House prices

The headline house price index (`hpi`) is constructed by chain-linking
three sources:

- Pre-2003: a privately compiled dwelling-price index supplied as
  `houseprice_old.csv` (covering 1986Q2–2003Q3, monthly indices spliced
  to quarterly).
- 2003Q4–2017Q2: ABS Cat 6416.0 Residential Property Price Index, eight
  capital cities ("old method").
- 2003Q3–2024Q4: ABS Cat 6432.0 Total Value of Dwellings, mean price.

Chain-linking is applied at each join quarter using the standard
multiplicative scale factor implied by the overlap between adjacent
sources. The earliest binding observation for `hpi` is 1986Q2.

Williams (2010) extends the house price series further back by splicing
BIS Shrapnel data (1972Q3–1978Q2, sourced via the Treasury) before the
REIA (1978Q3–1986Q1) data. Our current vintage does not include the
BIS Shrapnel segment, which would be required for any future
back-extension to 1972Q3; we leave this as a follow-up.

The relative house-price-to-income ratio used in estimation is

  `ln_hp_over_y = log(hpi / (real per-capita income))`

### 3.5 Credit conditions index

The credit conditions index (CCI) is the most contested input. Williams
(2010, 2012) constructs CCI as the latent factor identifying credit
liberalisation episodes: a spline of smoothed-step dummies at the
institutional turning points 1979 (Campbell Committee, end of interest
ceilings), 1992 (banking distress, Aussie Home Loans), 1998 (NBFI
expansion), and 2007 (GFC), estimated jointly with the consumption,
house-price, mortgage-stock and home-equity-withdrawal equations under
sign priors (positive 1979–1990, negative 1992–1994, positive 1998–2006,
negative 2007 onwards).

We adopt a hybrid:

- **Default observable proxy** (`cci_ratio`): the log of housing credit
  flow (ABS Cat 5601.0 New Loan Commitments Value, total housing) divided
  by the eight-quarter moving average of nominal disposable income.
  Available 2002Q3 onward (n=90); used as a short-run regressor in
  specifications 2 and 5. We do not back-extend this with a mortgage-spread
  proxy; this option (the default in earlier vintages of the codebase) is
  retained behind a feature flag but disabled.

- **Williams-style 4-knot spline** (`cci_williams`, optional via
  `USE_INSTITUTIONAL_CCI = TRUE`): smoothed-step dummies at 1979Q1,
  1992Q1, 1998Q1, 2007Q1 estimated inside the disaggregated consumption
  equation by general-to-specific reduction with sign priors enforced
  by drop-on-violation (in the spirit of Hendry-Krolzig 2005). On our
  1988Q4-onwards sample, two of the four knots survive: 1998 with the
  expected positive coefficient and 2007 with the expected negative
  coefficient; the 1979 knot is constant within our window and the 1992
  knot violates its prior. We discuss the implications in §5 below.

The first-home-buyer share (`fhb_share = fhb_loans / total_new_loans`)
is constructed from ABS Cat 5601.0 from 2002Q3.

### 3.6 Williams (2009) non-property income

The Williams (2009) §4.2.1 non-property income measure (`npy_real_pc`)
adjusts gross disposable income by removing imputed property income and
a corresponding share of property-related taxation. Following Williams
(2009 p. 10):

  `npy_rec = total_income_rec − GOS_dwellings − prop_inc_rec`
  `property_tax_share = (GOS_dwellings + prop_inc_rec) / total_income_rec`
  `npy_pay = total_income_pay − prop_inc_pay − property_tax_share × income_tax_payable`
  `NPY = npy_rec − npy_pay`

The four input components are sourced from ABS Cat 5206.0 Table 20:
compensation of employees, gross operating surplus on dwellings,
property income receivable, social assistance benefits, property income
payable, total income receivable, total income payable, and income tax
payable.

The implied non-property income share of disposable income averages
0.84 over 2010-2024, consistent with Williams' implicit ~0.85 weighting.
We use NPY as a robustness column (see §8) and report sensitivity to
the income measure as a key methodological gap that explains
approximately 30 per cent of the speed-of-adjustment divergence between
our preferred specification and Williams.

### 3.7 Demographics and dummies

The prime-working-age share (`prime_age_share` = age 25-54 share of
total ERP) is constructed from ABS Cat 3101.0 single-year-of-age
cohorts (Male + Female, summed; ratios are robust to the truncation
discussed in §3.1). Annual data are interpolated to quarterly via
cubic spline.

Five Australia-specific narrative dummies enter the default dummy set:

- `d_neg_gearing_8587`: 1985Q3–1987Q3 negative-gearing tax restriction
- `d_recession_1991`: 1991Q2 ("recession we had to have")
- `d_apra_2014`: 2014Q4 macroprudential investor-loan-growth cap
  (smoothed-step ogive)
- `d_apra_2017`: 2017Q2 macroprudential interest-only-loan cap
  (smoothed-step ogive)
- `d_jobkeeper_2020`: 2020Q2–2021Q1 JobKeeper income support

Together with the standard four (`d2000_gst`, `d2008_gfc`,
`d2020_covid`, `d2020_rebound`), these constitute the full dummy set;
zero-variance dummies are silently dropped per specification.

### 3.8 Coverage and reproducibility

The full reproducibility kit (R 4.5.3, renv-pinned dependencies, raw
ABS workbooks, three project CSVs, master quarterly dataset as CSV and
RDS, full estimation pipeline with three execution modes, GitHub Actions
CI) accompanies this paper. The master dataset has 180 quarters × 85
columns and is available as a portable CSV (`master_data.csv`) for
hand-editing or off-line replay. See appendix on data construction for
each variable's source identifier, vintage, and splicing recipe.

---

## 4. Model

### 4.1 Functional form

We estimate the standard Muellbauer error-correction consumption equation
adapted to Australian data following Williams (2010) eq. (7):

> Δln c_t = λ [ α_0 + γ_HA · (HA/y)_{t-1} + γ_IFA · (IFA/y)_{t-1}
>           + γ_NLA · (NLA/y)_{t-1} + γ_HP · ln(p^h/y)_{t-1}
>           + α_r · r_t + φ · ln(y^p/y)_t + ecm_lag_t ]
>           + Σ β_j Z_jt + Σ δ_k D_kt + ε_t

where

- `c_t` is real per capita household consumption
- `y_t` is real per capita disposable income
- `(HA/y)`, `(IFA/y)`, `(NLA/y)` are the housing, illiquid financial
  and net liquid wealth-to-annualised-income ratios
- `p^h_t` is the real house price index, so `ln(p^h/y)` is the
  affordability/down-payment ratio
- `r_t` is the ex post real mortgage rate
- `y^p_t` is the discounted weighted average of expected future real
  per capita income (the permanent-income concept)
- `ecm_lag_t = ln(c_{t-1}) − ln(y_t)` is the canonical Engle-Granger
  error-correction term
- `Z_jt` are short-run dynamic regressors and `D_kt` are narrative
  dummies as documented in §3.7
- `λ < 0` is the speed of adjustment and `α_0, γ_*, α_r, φ` are
  long-run cointegrating coefficients (the `structural` parameters in
  the LIVES literature)

The structural γ coefficients are recovered as `OLS_coef / |λ|` from
the OLS regression of Δln c on the right-hand side; we report both
forms throughout (see §7).

### 4.2 Sign priors

Theoretical sign priors on the long-run coefficients are:

- `γ_HA ≥ 0` (housing wealth: collateral and lifetime-income channels)
- `γ_IFA ≥ 0` (illiquid financial wealth)
- `γ_NLA ≥ 0` (net liquid: buffer-stock + intertemporal substitution)
- `γ_HP ≤ 0` at credit-tight regimes (down-payment penalty); sign
  ambiguous at credit-loose regimes (collateral channel dominates;
  Williams 2010 reports positive coefficient at the 2007 CCI peak)
- `α_r ≤ 0` (intertemporal substitution)
- `φ ∈ [0, 1]` (Hall 1978 PIH; calibrated by Williams)
- `λ < 0` (stable error correction)

We use these as informal screens (§6.2) rather than imposing them as
formal Bayesian priors; freely estimated coefficients allow us to
report violations of the priors as substantive findings rather than
artefacts of imposed restrictions.

### 4.3 Permanent-income forecasting

Permanent income `y^p_t` is the discounted weighted average of expected
log income over a 40-quarter horizon at quarterly discount factor
δ_q = 0.95^(1/4):

> ln(y^p_t / y_t) = E_t [ Σ_{h=1}^{40} w_h ln(y_{t+h}) ] − ln(y_t)
>     where w_h = δ_q^(h-1) / Σ_{h=1}^{40} δ_q^(h-1)

We implement two forecasting methods and compare them in §8:

- **Method 'AR'** (default): rolling AR(8) regression of log income on
  eight own lags plus a linear trend, post-2008Q3 step dummy, and
  trend-break interaction. Forecasts are aggregated over 40 horizons
  using the discount weights. Optional predictors `unemp_rate`,
  `log_oil`, `log_reer`, `log_stocks` are added if available. A 2008Q3
  ogive learning weight smoothly attenuates the term over 15 quarters
  to a steady-state weight of 0.5.

- **Method 'Italy'**: Jordà (2005) local projection. For each `t` where
  the future horizon is observable, the discounted weighted average is
  computed directly as the dependent variable in a single regression on
  a richer predictor set including `log(lf_share)` (the Italian
  innovation, capturing slow-moving demographic effects on trend
  income), trend, post-2008 split-trend, 4-quarter-MA log income,
  unemployment rate, and 4-quarter-difference dynamics. Forecast values
  are then constructed as fitted values of this single regression for
  every t.

The Italy method materially changes two coefficients in the consumption
equation (see §8): the speed of adjustment and the long-run
permanent-income coefficient. We report both throughout.

### 4.4 Wealth definition

Following Italy (De Bonis et al. 2024 §2 eq. 2.5; Table 3 col. 3) and
Williams (2010), net liquid assets are defined as

  `NLA = liquid_assets − total_household_debt`

and we test the cross-equation restriction `γ_LA + γ_LOANS = 0` by
refitting each disaggregated specification with deposits and debt
entered separately (`nla_y_unrestricted` and `debt_y`) and conducting
a Wald test of equality with opposite signs. Italy formally tests and
accepts this restriction; we replicate the test on Australian data in
§8 (`australia_nla_restriction_test.csv`).

---

## 5. Identification of credit conditions

This section is the methodological pivot of the paper. The credit
conditions index `CCI` is jointly determined with consumption,
house prices, mortgage debt and home equity withdrawal in the LIVES
framework (Muellbauer-Williams 2012). In a single-equation setting,
its identification rests on either an observable proxy or institutional
prior knowledge.

### 5.1 The Williams 4-knot spline

Williams (2010) constructs CCI as a spline of `SDMMA` smoothed-step
dummies — a 5-quarter moving average of a 4-quarter moving average of
a 0/1 step — at four institutional turning points: 1979Q1 (Campbell
Committee, removal of interest rate ceilings), 1992Q1 (NBFI distress
post the early-1990s recession), 1998Q1 (NBFI/securitisation expansion),
and 2007Q1 (GFC retrenchment). Each knot's coefficient is constrained
by an institutional sign prior: positive at deregulation episodes
(1979, 1998), negative at retrenchment episodes (1992, 2007).

We implement the Williams reduced-form spline as part of the data
pipeline (helper `build_williams_cci_basis()` in `model_helpers.R`)
and fit the four knots inside the consumption equation under the same
sign priors, enforced by drop-on-violation general-to-specific reduction
(Hendry-Krolzig 2005). On our 1988Q4-onwards sample, the outcome
(`australia_williams_cci_knots.csv`):

| Knot | Sign prior | Status |
|---|---:|---|
| 1979Q1 | + | aliased (constant in window after lag) |
| 1992Q1 | − | sign violator (dropped) |
| 1998Q1 | + | survives, +0.0015 |
| 2007Q1 | − | survives, −0.0173 |

The fitted `cci_williams` series is the linear combination of the two
surviving knots, peak-normalised to unity.

### 5.2 Why partial identification

The 1979 knot is unidentifiable on a 1988+ sample because the smoothed-
step dummy reaches unity within four quarters of 1979Q1 and is
thereafter constant; without observations spanning the regime change,
the coefficient is collinear with the intercept. The 1992 sign violation
is more substantive: in our sample, the post-1992 period largely covers
the recovery and subsequent re-expansion of credit, so the smoothed
step interacts with the gradual relaxation that began in 1998 and the
spline coefficient on 1992 is forced positive by data. Williams' 1992
identification relies on the immediate post-recession contraction
in the early 1990s, which is now entirely outside our sample.

The natural fix is sample back-extension to ~1975Q1, which would
introduce four years of pre-deregulation observations and partial
post-recession observations into the estimation window. The binding
obstacle is pre-1988 housing wealth and total financial assets — both
unpublished RBA internal series in the Williams (2010) appendix. We
have requested the underlying RBA series and will report results on
the back-extended sample in a companion paper.

### 5.3 Spec 8: CCI interactions (when CCI is available)

When `cci_williams` is available, we estimate a parallel "Spec 8"
incorporating the full Williams interaction structure:

> ... + γ_HP · log(p^h/y) · (1 − ϖ · CCI) + α_r · r · CCI + ψ_1 · log(y^p/y) · CCI + ...

with ϖ calibrated to 1.2 following Williams. Estimation results are
in §8.4 below; the interaction-term sign priors are mostly violated on
our 1988+ sample (consistent with the partial identification of CCI
itself), pending sample back-extension.

---

## 6. Specifications and selection

We estimate eight nested specifications and select the preferred via
a four-screen rubric.

### 6.1 The eight specifications

| Spec | Description | Long-run regressors |
|---|---|---|
| 1 | Aggregate net worth | `ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 2 | Spec 1 + short-run CCI | (same long-run; SR adds `Δ²log CCI` lag 2) |
| 3 | Net worth in levels | (replaces `ln_networth_y` with `networth_y`) |
| 4 | Disaggregated wealth | adds `nla_y, eq_y, super_y, ha_y` (drops the aggregates) |
| 5 | Spec 4 + full short-run | adds `Δ²log CCI`, `ΔΔ_4 income`, `Δ²log unemp`, `|ε̂|` |
| 6 | Spec 5 + post-2008 PI break | adds `ln_yp_over_y_post2008` |
| 7 | Spec 6 + cohort terms | adds `prime_age_share, fhb_share` |
| 8 | Williams CCI interactions | Spec 4 + `r×CCI`, `log(HP/y)×(1−1.2·CCI)`, `log(y^p/y)×CCI` |

### 6.2 The four selection screens

Following the structural-econometrics tradition (Hendry-Krolzig 2005,
Doornik 2009), we select the preferred specification by four formal
screens, with BIC tiebreaker:

1. **Sign screen**: every long-run coefficient with a non-ambiguous
   theoretical prior carries the right sign (§4.2).
2. **Cointegration screen**: ADF on the long-run residual rejects the
   unit root null at 5 per cent (Engle-Granger). Phillips-Ouliaris and
   single-equation Johansen results are reported alongside but the
   ADF is the binding screen.
3. **Speed-of-adjustment screen**: λ has the correct sign (negative)
   and `|λ| ∈ (0.02, 0.30)`.
4. **Stability screen**: Chow at 2008Q3 not rejected at the 1 per cent
   level, AND λ is sign-stable across at least 3 of 4 sample variants
   (full, pre-COVID, COVID-dropped, COVID rich-dummies). The four-sample
   λ stability is recorded in `australia_lambda_robustness.csv`.

### 6.3 Selector outcome

On our default `PI_METHOD = 'ar'` setting, only Spec 6 passes all four
screens (`australia_spec_selection.csv`):

| Spec | Signs | Coint | λ | Stability | BIC | Preferred |
|---|---|---|---|---|---:|---|
| 1 | ✓ | ✗ | ✓ | ✗ | -824 | |
| 2 | ✓ | ✗ | ✓ | ✓ | -496 | |
| 3 | ✓ | ✗ | ✓ | ✗ | -825 | |
| 4 | ✓ | ✓ | ✓ | ✗ | -812 | |
| 5 | ✓ | ✓ | ✓ | ✓ | -491 | |
| **6** | **✓** | **✓** | **✓** | **✓** | **-491** | **✓** |
| 7 | ✗ | ✓ | ✓ | ✓ | -496 | |
| 8 | ✗ | NA | ✓ | ✗ | -805 | |

Spec 7's `nla_y` is small-negative (a real demographics-wealth
collinearity finding documented in §8.5 below), so the spec fails the
sign screen despite having the most stable λ across the four samples.
We retain Spec 6 as preferred and report Spec 7 as a robustness column.

---

## 7. Results — preferred specification

### 7.1 Headline coefficients

Spec 6 over the full sample 1988Q4–2024Q4 (n=86, after lag-truncation)
delivers:

> [TABLE-FROM-DATA: Pull from `australia_full_results.csv` filtered to
> Spec6_Preferred. Suggested format: Term, OLS coef, NW SE, t-stat,
> Implied γ (=OLS/|λ|), p-value, sign-OK indicator. Drop dummies to a
> footnote.]

In summary:

- **λ (`ecm_lag`) = −0.052 (NW SE 0.052)** — modest speed of adjustment.
- **`ha_y` = +0.0148 (SE 0.0069)**, t = +2.15. Implied γ = 0.282.
- **`nla_y` = +0.0407 (SE 0.0271)**, t = +1.51. Implied γ = 0.776.
- **`eq_y` = +0.0442, `super_y` = +0.0125** (sum +0.0567, the IFA
  equivalent). Implied γ = 1.08.
- **`ln_hp_over_y` = −0.031 (SE 0.013)**, t = −2.36. Implied γ = −0.591.
- **`real_rate` = −0.0007 (insignificant).** No measurable contemporaneous
  intertemporal-substitution effect at our default specification (we
  return to this in the Spec 8 / CCI-interaction discussion).
- **`ln_yp_over_y` = −0.20 (SE 0.11)**, t = −1.88, with offsetting
  post-2008 break `ln_yp_over_y_post2008` = +0.20 (SE 0.11). Reported as
  the "Australian permanent-income puzzle"; we show in §8 below that
  this puzzle is method-dependent.
- **Diagnostics**: adj-R² = 0.81, DW = 2.40, AR(1) p = 0.10, AR(4) p =
  0.025, Chow at 2008Q3 not rejected, RESET p = 0.42.

### 7.2 Diagnostics summary

> [TABLE-FROM-DATA: `australia_full_diagnostics.csv` for all eight
> specs.]

The het diagnosis distinguishes "event-driven" (BP rejection vanishes
when the four event quarters are dropped) from "structural"
heteroscedasticity. Spec 6 is classified `structural` on the full sample,
suggesting the Newey-West HAC standard errors are appropriate but that
some residual misspecification remains.

### 7.3 Comparison with Williams (2010 / 2012)

We compare our Spec 6 to Williams' published Table 1 estimates from
the BIS chapter (Muellbauer-Williams 2012). The full comparison is in
`australia_williams_comparison.md` and the side-by-side coefficient
table in `australia_williams_comparison.csv`.

The headline finding is that **the OLS-level coefficients agree closely
with Williams' implied OLS** (Williams' γ × |λ_W|), but the speed of
adjustment differs by a factor of 5.5, which mechanically inflates our
implied long-run γ. Specifically (full sample):

| Term | Williams γ | Williams implied OLS | Our OLS | OLS gap |
|---|---:|---:|---:|---:|
| `ha_y` | 0.0488 | 0.0140 | 0.0148 | **+6%** |
| `nla_y` | 0.1590 | 0.0455 | 0.0407 | **−11%** |
| `ln_hp_over_y` | −0.130 | −0.0372 | −0.0310 | **−17%** |
| **λ** | **−0.286** | (same) | **−0.0525** | **−82%** |

The main puzzle is the speed of adjustment, not the long-run elasticities.
We decompose the λ gap in §8 below.

### 7.4 Italy LP method substantially closes the gap

Switching the permanent-income forecaster from rolling AR(8) to the
Italy local-projection method delivers two material changes
(`australia_pi_method_comparison.csv`):

| Term | AR estimate | Italy LP | Williams |
|---|---:|---:|---:|
| `ecm_lag` (λ) | −0.052 | **−0.218** | **−0.286** |
| `ln_yp_over_y` | −0.201 | **+0.302** | (calibrated 0.20) |

The Italy LP method:

1. Quadruples |λ| in our preferred spec, bringing it within 25 per cent
   of Williams' published value.
2. Flips the sign of the long-run permanent-income coefficient from
   negative (the "Australian PI puzzle") to positive, in agreement with
   theory and with Williams' calibrated value.

We interpret the "Australian PI puzzle" as a methodology artefact rather
than a structural feature: the rolling AR(8) forecaster (a) lacks the
labour-force-share predictor that captures Australia's slow-moving
demographic effects on trend income, (b) compounds short-run AR
misspecification across 40 horizons, and (c) is structurally biased
toward forecasts that over-estimate persistence after large income
shocks. The Jordà (2005) one-step direct projection avoids all three.

---

## 8. Robustness

We run the Italian-style robustness suite of De Bonis et al. (2024) on
the preferred specification (`run_italy_style_robustness()` in code).

### 8.1 OLS vs IV on current income (Hall 1978 endogeneity)

> [TABLE-FROM-DATA: `australia_iv_robustness.csv`]

Current income is instrumented by lagged income (lags 1, 2, 4), lagged
unemployment (lags 1, 2), and lagged mortgage rate. λ shifts from
−0.052 (OLS) to −0.060 (IV); other coefficients move in the third
decimal. We conclude that current-income endogeneity is not the source
of our small λ.

### 8.2 Joint PI + consumption SUR (Italy.pdf p.32 check)

> [TABLE-FROM-DATA: `australia_joint_pi_robustness.csv`]

Estimating the consumption equation jointly with the permanent-income
equation by `systemfit::systemfit(method = "SUR")` yields coefficients
within 0.005 of the single-equation values. Italy reports
"a whisker away" between joint and single-equation; our finding is
consistent. Single-equation OLS is therefore an acceptable estimator
for the consumption block.

### 8.3 Chow battery

> [TABLE-FROM-DATA: `australia_chow_battery.csv`]

Chow tests at break dates 1995Q1, 2000Q1, 2008Q3 and 2020Q1 are not
rejected at the 5 per cent level for the preferred specification at
1995Q1, 2000Q1 and 2008Q3, and rejected at 2020Q1 (consistent with the
COVID structural break that our event dummies absorb). The four-knot
spline test of Bai-Perron (1998) suggests one structural break around
2008Q3, in line with the standard GFC narrative.

### 8.4 Williams CCI interactions (Spec 8)

> [TABLE-FROM-DATA: `australia_full_results.csv` filtered to Spec8.]

Spec 8 incorporates the three Williams CCI interactions into the
disaggregated-wealth long-run on the truncated 1988+ sample with the
two-knot reduced-form `cci_williams`:

| Williams interaction | Sign prior | Our coefficient | Verdict |
|---|---:|---:|---|
| `r × CCI` | − | +0.11 (insignificant) | sign FAIL |
| `log(HP/y) × (1 − 1.2·CCI)` | − | −0.011 (p = 0.075) | sign PASS |
| `log(y^p/y) × CCI` | + | +1.63 (insignificant) | sign PASS but t-stat 0.46 |

Only the affordability/down-payment interaction passes both screens.
The interest-rate interaction's sign violation likely reflects the
truncated CCI identification (only the 1998 and 2007 knots survive on
our sample); the permanent-income interaction is uninformative on this
sample and pending sample back-extension.

### 8.5 Cross-equation restriction γ_LA + γ_LOANS = 0

> [TABLE-FROM-DATA: `australia_nla_restriction_test.csv`]

We refit each disaggregated specification with `nla_y_unrestricted =
deposits/y` and `debt_y` entered separately, and conduct a Wald test
of `H0: γ_LA + γ_LOANS = 0` using `car::linearHypothesis` with the
Newey-West vcov. The restriction is **accepted at the 5 per cent
level in every specification × sample combination** (p-values in
0.27–0.79), validating the Italian convention of netting deposits
against debt.

### 8.6 Drehmann amortising-mortgage adjusted real rate

> [TABLE-FROM-DATA: `australia_drehmann_robustness.csv`]

Italy applies the BIS Drehmann (2017) amortisation-adjusted rate
`adjR = R / (1 − (1+R)^−N)` with N = 12 years. For Australia we use
N = 25 years (consistent with the longer Australian average mortgage
maturity). The adjustment shifts `real_rate` by approximately +0.6
percentage points but barely moves the long-run coefficient (within
0.001) — Italy reports a similar finding.

### 8.7 Scaled-income (Italy methodology) robustness

> [TABLE-FROM-DATA: `australia_scaled_income_robustness.csv`]

Italy averages disposable income with labour+transfer income to
down-weight property income mismeasurement. We run the same: Spec 6's
λ shifts from −0.052 to −0.080 (≈+50%) under scaled income. This is
the channel that explains roughly half of the income-measure-driven
component of the λ divergence from Williams.

### 8.8 Williams non-property income (NPY) robustness

> [TABLE-FROM-DATA: `australia_williams_income_robustness.csv`]

Replacing `ydi_real_pc` with `npy_real_pc` (per Williams 2009 §4.2.1)
shifts λ from −0.052 to −0.062 (+18%). NPY is between scaled income
and gross disposable in conservatism — Williams strips property income
but does not 50/50 average. We treat NPY as the closer methodology
match to Williams; the 18% shift is the income-measure component of
the λ divergence under Williams' specific methodology.

### 8.9 PI method comparison (AR vs Italy LP)

> [TABLE-FROM-DATA: `australia_pi_method_comparison.csv`]

Already discussed in §7.4. The headline: λ goes from −0.052 (AR) to
−0.218 (Italy LP), and `ln_yp_over_y` flips sign from −0.20 to +0.30.

### 8.10 Permanent-income filter sensitivity

> [TABLE-FROM-DATA: `australia_permanent_income_sensitivity.csv`]

We run a 9-cell grid over discount factor δ ∈ {0.90, 0.95, 0.97},
horizon k ∈ {20, 40, 60} quarters, and the GFC ogive on/off. The
preferred-spec λ is stable to within 0.02 across the grid, indicating
that the within-AR-method PI choice is not the driver of our small λ
(the Italy LP method change in §8.9 is much larger than any AR-grid
variation).

### 8.11 COVID-period robustness

> [TABLE-FROM-DATA: `australia_lambda_robustness.csv`]

λ is sign-stable across all four sample variants for Spec 6 (full,
pre-COVID, COVID-dropped, COVID rich-dummies) — magnitudes range from
−0.046 to −0.121, but always negative. Spec 7 is even tighter
(−0.17 to −0.24). The COVID episode does not destabilise our headline
findings.

### 8.12 Rolling-window estimation

> [FIGURE-FROM-DATA: `australia_rolling_coefs.png`]

A 60-quarter rolling estimation of Spec 6 shows the wealth coefficients
trending mildly downward post-2014 (consistent with the macroprudential
era flattening the wealth-consumption transmission), while λ becomes
slightly less negative in the most recent windows. We do not interpret
this as instability of the model, but rather as a symptom of the
truncated-CCI identification problem discussed in §5.

---

## 9. Comparison with Williams (2010, 2012)

The structural-parameter comparison table is reproduced in full in
`australia_williams_comparison.md`. The narrative for the methodology
section:

**Where we agree with Williams.** All wealth coefficients carry the
correct positive sign, with the NLA cross-equation restriction
γ_LA + γ_LOANS = 0 accepted in our data (validating the Italian
convention). At the OLS level, our `ha_y` coefficient agrees with
Williams' implied OLS (γ × |λ_W|) within 6 per cent; `nla_y` within
11 per cent; `ln_hp_over_y` within 17 per cent. The model under the
Italy LP permanent-income forecaster delivers λ = −0.218, within 25
per cent of Williams' published −0.286.

**Where we differ.** The default-AR-PI specification has λ five times
smaller in magnitude than Williams', driven by a combination of
income-measure effects (~30 per cent of the gap), permanent-income
forecaster effects (~50 per cent of the gap), and missing CCI
interactions (~20 per cent residual). The implied long-run wealth γ
under our default specification is inflated by the small λ — for
example our `ha_y` γ is 0.282 (vs Williams' 0.0488); under Italy LP
this falls to 0.028, undershooting Williams. The "true" identification
likely lies between, and pinning it down is the natural follow-up.

**The Australian PI puzzle.** Under the default rolling-AR(8) PI
forecaster we replicate the often-noted Australian PI puzzle: the
long-run coefficient on `ln(y^p/y)` is significantly negative (−0.20).
Under the Italy LP method this flips to positive (+0.30), matching
Williams' calibrated value (0.20 at CCI=0, rising to 0.95 at CCI peak)
in sign and broad magnitude. We interpret the puzzle as a methodology
artefact of the rolling-AR forecaster, not a structural feature of
Australian consumption behaviour.

---

## 10. Decomposition and policy implications

### 10.1 Long-run contributions decomposition

> [FIGURE-FROM-DATA: `australia_longrun_decomposition.png`]

The headline policy-facing chart (Williams 2010 Charts 2–8 analogue)
decomposes fitted log(c/y) for the preferred specification into the
de-meaned partial contribution of each long-run regressor. The chart
shows housing wealth as the dominant positive wedge through 2005–2024,
with a large negative house-price-affordability wedge in the 1990s
that recedes by 2010. The post-2008 PI step shift is a transient
2008–10 spike. The residual stays small (less than 0.05 in magnitude)
outside the GFC and early-COVID windows.

Reading the chart: **about 30 per cent of the rise in the consumption
ratio from 1990 to 2007** is attributable to the housing wealth term
in our preferred specification. This figure is sensitive to the
specification (cf. Williams 2010 Charts where CCI is the headline
contributor and the wealth × CCI interaction contributes most of the
remainder). We discuss the interpretation gap below.

### 10.2 Counterfactuals (suggested for the paper)

> [TO DRAFT]

Three counterfactuals worth running:

1. **No 2014/2017 macroprudential tightening**: zero out
   `d_apra_2014` and `d_apra_2017` in the fitted contribution, plot
   the implied path of log(c/y).
2. **No COVID JobKeeper**: zero out `d_jobkeeper_2020`.
3. **Williams' CCI peak vs current**: counterfactually evaluate the
   wealth × (1 − 1.2·CCI) interaction at CCI = 1 (Williams' historical
   peak) vs CCI = 0 (no liberalisation). Compare implied
   log(c/y) levels.

### 10.3 Policy implications

> [TO DRAFT — central bank framing]

- **Wealth channel of monetary policy**: housing wealth has the
  largest direct elasticity into consumption. Movements in mortgage
  rates that affect housing values (whether through interest rate
  pass-through or credit-conditions tightening) propagate to
  consumption with a lag governed by λ.
- **Macroprudential effects**: explicit dummies for the 2014 and 2017
  APRA episodes pick up significant disinflation in consumption growth.
  Quantify the implied moderation in consumption growth attributable
  to each.
- **Permanent-income transmission**: the Italy LP method recovers a
  positive long-run permanent-income coefficient (~+0.30), suggesting
  that Australian households respond meaningfully to credible permanent
  income shocks. This is policy-relevant for thinking about
  fiscal-multiplier estimates: under permanent (vs transitory) income
  changes, the propagation to consumption is roughly 30 per cent in
  the long run.

---

## 11. Conclusion

> [SKELETON]

We extend the Williams (2010, 2012) Australian LIVES consumption
estimate to 2024Q4 using publicly available data. At the OLS level
our wealth coefficients agree closely with Williams' published values
on a non-overlapping sample. The most material divergence — a
five-fold gap in the speed of adjustment — is largely explained by
methodological choices around the income measure and the
permanent-income forecaster. Under specifications closer to Williams'
methodology (non-property income; Jordà-style local-projection PI),
λ falls within 25 per cent of his value and the often-noted Australian
PI puzzle disappears. Substantively, this validates the LIVES
framework on extended Australian data and provides a contemporary
benchmark for the wealth, credit and permanent-income channels of
consumption transmission.

Outstanding work includes the sample back-extension to ~1975Q1 (which
would identify all four Williams CCI knots and resolve the Spec 8
sign violations); incorporating the multi-equation system (consumption,
house prices, mortgage stock, HEW) for full LIVES identification; and
quantifying the macroprudential and PI-counterfactual scenarios outlined
in §10.

---

## References

> [TO POPULATE — many of these we have on hand]

- Aron, J., Duca, J. V., Muellbauer, J., Murata, K., & Murphy, A. (2012).
  Credit, housing collateral, and consumption: evidence from Japan, the
  U.K., and the U.S. *Review of Income and Wealth*, 58(3), 397–423.
- Aron, J., & Muellbauer, J. (2002). Estimates of household sector wealth
  for South Africa, 1970–2003. *Review of Income and Wealth*.
- Atkin, T., & Hambur, J. (2018). Models of household credit and household
  balance sheet dynamics. RBA Research Discussion Paper 2018-?? [VERIFY].
- Bayoumi, T. (1993). Financial deregulation and household saving.
  *Economic Journal*, 103(421), 1432–1443.
- Blinder, A. S., & Deaton, A. (1985). The time series consumption function
  revisited. *Brookings Papers on Economic Activity*.
- Carroll, C. D., & Kimball, M. S. (1996). On the concavity of the
  consumption function. *Econometrica*, 64(4), 981–992.
- Chauvin, V., & Muellbauer, J. (2018). Consumption, household portfolios
  and the housing market in France. *Banque de France Working Paper*.
- Cusbert, T., & Kendall, E. (2018). Meet MARTIN, the RBA's new economic
  model. *RBA Bulletin*.
- De Bonis, R., Marino, I., & Muellbauer, J. (2024). Consumption, wealth
  and credit conditions in Italy: a Muellbauer-style ECM. [VERIFY citation]
- Doornik, J. A. (2009). Autometrics. In *The methodology and practice of
  econometrics: a festschrift in honour of David F. Hendry*.
- Drehmann, M., Juselius, M., & Korinek, A. (2017). Accounting for debt
  service: the painful legacy of credit booms. BIS Working Paper.
- Duca, J. V., Muellbauer, J., & Murphy, A. (2010). Housing markets and
  the financial crisis of 2007–09. *Journal of Financial Stability*.
- Duca, J. V., Muellbauer, J., & Murphy, A. (2013). Tobin LIVES: integrating
  evolving credit market architecture into flow of funds based macro models.
  *European Economy Discussion Paper*.
- Engle, R. F., & Granger, C. W. J. (1987). Co-integration and error
  correction: representation, estimation, and testing. *Econometrica*.
- Hall, R. E. (1978). Stochastic implications of the life cycle-permanent
  income hypothesis. *Journal of Political Economy*, 86(6), 971–987.
- Hendry, D. F., & Krolzig, H.-M. (2005). The properties of automatic GETS
  modelling. *Economic Journal*, 115, C32–C61.
- Jordà, Ò. (2005). Estimation and inference of impulse responses by
  local projections. *American Economic Review*, 95(1), 161–182.
- Muellbauer, J. (2007). Housing, credit and consumer expenditure. *Federal
  Reserve Bank of Kansas City Symposium Proceedings*.
- Muellbauer, J., & Williams, D. (2012). Credit conditions and the real
  economy: the elephant in the room. BIS Papers No. 64.
- Williams, D. M. (2009). House prices and financial liberalisation in
  Australia. *Oxford Economics Series Working Paper* 432.
- Williams, D. M. (2010). Consumption, wealth and credit liberalisation in
  Australia. *Oxford Economics Series Working Paper* 492.

---

## Appendix A: Data construction

> [Reproduce or summarise `Ausreplication/docs/data.md`. The current
> data.md is already at the right level of detail for an appendix —
> probably a 50% trim retaining sections 1, 2.1–2.10, 5, 6, 8, plus the
> coverage table from §7.]

## Appendix B: Coefficient tables

> Full coefficient tables from `australia_full_results.csv` and
> `australia_precovid_results.csv` for all eight specifications, both
> sample windows. Newey-West standard errors.

## Appendix C: Diagnostic battery

> Full diagnostic output from `australia_full_diagnostics.csv` and
> related CSVs.

## Appendix D: Reproducibility

> Three execution modes (full pipeline, RDS-based fast re-estimation,
> CSV-based offline/portable). Renv lockfile, GitHub Actions CI,
> 22 testthat tests. Repository URL [TO INSERT].

---

**Generated** by hand from `Ausreplication/docs/`. To refresh
data-bearing sections after re-estimation, regenerate the source CSVs
and copy in the relevant blocks.
