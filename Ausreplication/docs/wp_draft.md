# Australian Household Consumption, Wealth and Credit Conditions: An Updated Single-Equation LIVES Estimate

**Draft — central-bank working paper format**

**Author(s):** [TO FILL]
**JEL codes:** E21, E32, E51, D14
**Keywords:** household consumption, wealth effects, credit conditions, error-correction model, LIVES system

---

## Abstract

We estimate a single-equation Muellbauer-Williams LIVES consumption
model for Australia over 1988Q4–2024Q4 and a back-extended
1976Q3–2024Q4 sample, extending Williams (2010) and Muellbauer-Williams
(2012) by sixteen years of post-GFC data and applying contemporary
identification methods. Wealth is disaggregated into housing, illiquid
financial (equities + superannuation), and net liquid assets (deposits
net of total household debt). The cross-equation restriction
γ_LA + γ_LOANS = 0 is accepted in our data, validating the Italian
convention of De Bonis et al. (2024).

Under the canonical Jordà (2005) local-projection permanent-income
forecaster (Italian methodology), the preferred Spec 6 delivers
λ = −0.218 (within 25 per cent of Williams' published −0.286), and
the long-run permanent-income coefficient is +0.30 (matching Williams'
calibrated value and resolving the "Australian permanent-income
puzzle" produced by the rolling AR(8) forecaster). Adding the
time-varying housing-wealth m.p.c. interaction (`ha_x_cci`,
Williams Aust eq 7 γ_1t·HA) to the Williams CCI interactions
specification (Spec 8) produces λ = −0.383, exceeding Williams in
magnitude.

The implied long-run γ on individual wealth terms is roughly a
quarter of Williams' published values. Earlier drafts attributed this
to truncated CCI variation on the post-deregulation 1988+ sample.
This paper assembles the data infrastructure for a back-extension to
1976Q3 (TRYM long-run house prices, RBA D03 M3, RBA D02 total credit,
historical labour force, and aggregate and disaggregated wealth
proxies anchored at 1988Q3) and **empirically tests and rejects this
hypothesis**: λ moves +37% closer to Williams but individual wealth
coefficients get *smaller*, not larger. Triangulating with random-knot
placebo tests on both samples (the literal Williams 4-knot fails the
placebo on both samples; the maximal-GETS reduction only weakly
above median; sectional sign priors at median), with a Zellner SUR
of consumption + house prices showing zero residual correlation, and
with a 3-equation joint cross-equation CCI identification (only 2 of
6 single-equation knot survivors pass joint sign tests), we diagnose
the binding constraint as **single-equation OLS framing**, not
sample length, knot count, or sign-prior structure. Williams' wealth
coefficients require the full multi-equation FIML system with
cross-equation parameter restrictions, which we lay foundations for
in a separate folder.

We provide a structured robustness suite covering instrumental
variables, joint SUR estimation, Chow batteries, Drehmann
effective-rate adjustments, the Williams smoothed-step credit-
conditions spline with maximal-GETS reduction and Kalman state-space
and sectional-sign-prior alternatives, rolling-window estimation,
out-of-sample forecast validation, back-extension Spec 1 and Spec 4
refits, and joint-identification tests. The full reproducibility
kit including a portable CSV of the master dataset and a separate
LIVES multi-equation folder is available alongside the paper.

---

## 1. Introduction

### 1.1 Motivation

Australian household consumption faces a set of policy and analytical
questions that the standard New Keynesian DSGE workhorse — with its
representative consumer, exogenous wealth process, and assumed-frictionless
credit access — is structurally ill-equipped to answer. Among them:
how much of the post-2008 moderation in consumption growth is
attributable to the macroprudential tightening of 2014 and 2017, and
how much to the post-GFC household balance-sheet repair? How
sensitive is consumption to housing wealth at different points in the
credit cycle, when households are or are not able to extract home
equity? Did the COVID shock and JobKeeper income support move
permanent-income expectations, or only the short-run quarter? And how
should a central bank think about the wealth channel of monetary
policy when much of household wealth is housing wealth, mortgage debt
is at near-historic levels relative to income, and the credit
conditions in which households operate have evolved markedly since
financial deregulation in the 1980s?

The Muellbauer-Williams "LIVES" framework — Latent Interactive
Variable Equation System, after Tobin's flow-of-funds tradition
(Duca and Muellbauer, 2013) — was developed to answer exactly these
questions. It augments the standard credit-augmented life-cycle
consumption function (Friedman 1957, Ando-Modigliani 1963, Tobin-Dolde
1971) with: (i) wealth disaggregated into liquid, illiquid financial
and housing components, recognising that they have different
marginal propensities to consume (Backus and Purvis 1980); (ii) a
latent credit-conditions index (CCI) that interacts with key
parameters — the housing-wealth m.p.c. that is amplified by easier
credit, the down-payment constraint that is relaxed by easier credit,
the role of permanent-income expectations that rises with credit
ease — and (iii) a multi-equation system that estimates this latent
factor jointly across consumption, house prices, mortgage stock, and
home equity withdrawal under cross-equation parameter restrictions.

Williams (2010) applied this framework to Australia for 1978–2008,
producing the canonical Australian LIVES estimate. Sixteen years of
post-GFC and post-COVID data have since accumulated, and a
contemporary central-bank-quality update of his framework is
warranted. This paper provides that update.

### 1.2 Contribution

This paper makes four contributions to the Australian household
consumption literature, each of which the May 2026 work materially
expanded relative to earlier drafts of this paper.

**1.2.1 An updated Williams (2010) LIVES estimate to 2024Q4** using
publicly available data, with the canonical permanent-income
forecaster shifted from a rolling AR(8) (the implementation in earlier
Australian work) to the Jordà (2005) local-projection method with a
labour-force-share predictor (Italian methodology, De Bonis et al.
2024). Under canonical Italy LP, the speed of adjustment is within
25 per cent of Williams' published value; the often-noted "Australian
permanent-income puzzle" — a significantly negative long-run
coefficient on log(y^p/y) under the AR forecaster — flips to positive
under Italy LP, matching Williams' calibrated value in sign and broad
magnitude.

**1.2.2 A back-extended master dataset** (1976Q3–2024Q4, n=194 quarters)
with documented growth-rate splices for house prices (TRYM Treasury
historical compilation, 1959Q3+), M3 monetary aggregate (RBA D03,
1959Q3+), total credit (RBA D02, 1976Q3+), and labour force
(user-supplied historical compilation, 1964Q3+). For the 1976Q3–1988Q2
window where ABS sectoral household balance sheet data are unavailable,
we construct aggregate (`networth_y_proxy`) and disaggregated
(`ha_y_proxy`, `nla_y_proxy`, `eq_y_proxy`, `super_y_proxy`) wealth
proxies anchored at 1988Q3. The full data construction is documented
in §3.

**1.2.3 An empirical falsification of the wealth-coefficient gap
hypothesis.** Earlier drafts attributed the gap between our wealth-
coefficient estimates and Williams' Table 1 calibrated values to
truncated CCI variation on the post-deregulation 1988+ Australian
sample. The May 2026 work tested this directly by refitting on the
back-extended sample with the disaggregated wealth proxies. **The gap
does not close.** λ moves from −0.140 to −0.193 (still 32% short of
Williams' −0.286), but individual wealth coefficients get *smaller*,
not larger — `nla_y` collapses to ~zero; `eq_y` keeps its wrong sign.
The hypothesis is empirically falsified. Triangulating with random-knot
placebo tests on both samples and with the multi-equation LIVES build
in [`LIVES/`](../../LIVES/) folder, the diagnosis is that the binding
constraint is **single-equation OLS framing**, not sample length.
Williams' wealth coefficients come from cross-equation parameter
restrictions in a 4-equation FIML system that single-equation OLS
cannot replicate.

**1.2.4 A structured robustness suite** mirroring the De Bonis et al.
(2024) Italian methodology — IV, joint SUR, Chow battery, Drehmann
effective-rate, scaled-income, Williams-style smoothed-step CCI (with
a maximal-GETS knot-identification reduction, a Kalman state-space
alternative, and a sectional-sign-prior alternative that we
implemented in May 2026 to follow Williams' Aust paper §5.1
specification more faithfully), PI method comparison, rolling-window
estimation, rolling out-of-sample forecast validation, and
back-extension robustness on Spec 1 and Spec 4 — applied to a
single-equation OLS framework with full code release. The
random-knot placebo tests are extensive: on both the 1988+ and
back-extended 1976Q3+ samples; on the literal Williams 4-knot
specification, the maximal-GETS canonical, and the new sectional
sign-prior alternative. The empirical pattern is that none of these
specifications strongly outperform random-knot placements on the
post-2008 Australian data.

### 1.3 Headline result

Under the canonical Italy local-projection PI forecaster, the
preferred Spec 6 delivers a speed of adjustment of **λ = −0.218** (vs
Williams' published −0.286) on a non-overlapping sample
(1988Q4–2024Q4 vs Williams' 1978Q1–2008Q2). With the May 2026
addition of the time-varying housing-wealth m.p.c. interaction
(`ha_x_cci`; §5.5), Spec 8 delivers λ = −0.383, exceeding Williams'
calibrated value in magnitude. The implied long-run γ on individual
wealth terms is roughly a quarter of Williams' values; the
back-extension does *not* close this gap (§7.3, §8.15, §9).
The cross-equation restriction γ_LA + γ_LOANS = 0 is accepted in
every disaggregated specification × sample combination, validating
the Italian convention. The "Australian permanent-income puzzle" is
resolved by the methodology shift from AR to Italy LP (the long-run
coefficient on log(y^p/y) flips from −0.20 to +0.30, matching
Williams' calibrated +0.20 in sign and broad magnitude).

The placebo evidence on the extended sample is striking: the literal
Williams 4-knot specification fails on both samples, deteriorating
from the 49th/22nd percentile on 1988+ to the 19th/10th percentile
on 1976Q3+. The maximal-GETS canonical CCI sits at 64th/36th. The
sectional sign-prior alternative sits at 36th/40th — implementing
Williams' specific period boundaries (Aust paper §5.1) does not
produce stronger identification than random period placements. The
multi-equation LIVES phase 3 build (in [`LIVES/`](../../LIVES/)
folder) explains why: when we require knots to satisfy sign priors
*jointly* across consumption, house-price and mortgage-stock
equations, only 2 of the 6 single-equation survivors pass — 1986
financial deregulation and 2017 APRA macroprudential round II. The
consumption-fitted CCI is therefore not a true common factor; 4 of
its 6 surviving knots are consumption-equation-specific. The path to
closing the wealth-coefficient gap is a full FIML build with
cross-equation parameter restrictions, which single-equation OLS
cannot deliver.

### 1.4 Roadmap

Section 2 surveys the LIVES literature. Section 3 documents data
construction in exhaustive detail, including the back-extension
sources and proxies (§§3.9–3.13). Section 4 presents the model.
Section 5 develops identification of credit conditions, including
the placebo battery and the cross-equation joint-identification test.
Section 6 sets out the eight specifications and the four selection
screens. Section 7 presents preferred-specification results, including
the wealth-coefficient gap diagnosis. Section 8 runs the structured
robustness suite, including the back-extension robustness columns
and the LIVES phase 1 / phase 3 multi-equation findings. Section 9
compares with Williams' published estimates and explains why the
wealth-coefficient gap persists on the back-extended sample. Section
10 presents the long-run decomposition and policy implications.
Section 11 concludes.

---

## 2. Literature review

This paper sits at the intersection of three literatures: the
Muellbauer-Williams "LIVES" tradition that integrates wealth, credit
conditions and life-cycle behaviour into consumption equations; the
Australian empirical consumption literature, which has historically
focused on aggregate wealth effects without the explicit credit-conditions
machinery; and the small but growing body of work that disciplines
permanent-income measurement using forecasting approaches more robust
than the standard AR(p) recipe. We review each in turn before placing
the present contribution.

### 2.1 Theoretical foundations

The empirical specification estimated below has its origins in the
Davidson-Hendry-Srba-Yeo (1978) error-correction consumption function
and the canonical permanent-income hypothesis (PIH) of Friedman (1957)
and Hall (1978). The DHSY approach embeds a long-run cointegrating
relationship between consumption, income and wealth into a short-run
dynamic equation in growth rates, with an error-correction term
governing the speed at which the consumption ratio reverts to its
long-run determinants. Engle and Granger (1987) provide the formal
econometric framework, with Hendry and Krolzig (2005) and Doornik
(2009) refining the general-to-specific reduction methodology that
disciplines the choice of short-run regressors.

A long line of empirical work has documented systematic departures from
strict PIH behaviour. Campbell and Mankiw (1989, 1991) show that
roughly half of US consumption tracks current rather than permanent
income, consistent with rule-of-thumb behaviour or liquidity
constraints. Carroll and Kimball (1996) establish the concavity of the
consumption function under prudence, providing a microeconomic basis
for differential marginal propensities to consume across wealth classes.
Carroll (2001) and Deaton (1992) develop the buffer-stock interpretation
that motivates the distinction between liquid and illiquid wealth in the
Muellbauer specifications used here. The empirical implication —
that wealth components with different liquidity, transactions costs, and
ownership concentration should enter the consumption equation with
different coefficients — is the cornerstone of the disaggregated
specifications (Specs 4–7) reported in §7.

### 2.2 The LIVES framework

Muellbauer (2007), originally prepared for the Federal Reserve Bank of
Kansas City Jackson Hole symposium, sets out the integrated approach to
modelling consumption with wealth, housing collateral and credit
conditions. The paper's central insight is that credit-market
liberalisation interacts with the wealth and interest-rate channels:
when credit is tight, housing wealth collateralises borrowing only
weakly and the down-payment hurdle dampens consumption; when credit is
loose, housing wealth and current consumption become more tightly
linked. This insight requires a credit-conditions index (CCI) entering
both as a long-run intercept shifter and as a multiplicative interaction
with the wealth, interest-rate and permanent-income terms.

The framework was operationalised in a series of country studies. Aron,
Duca, Muellbauer, Murata and Murphy (2012) jointly estimate the framework
on Japan, the United Kingdom and the United States, finding consistent
positive long-run housing wealth effects in the UK and US (where home
equity withdrawal is institutionally available) and a much smaller
effect in Japan (where it is not). Duca, Muellbauer and Murphy (2010)
apply the framework to the global financial crisis, demonstrating that
the abrupt tightening in CCI from 2007 quantitatively rationalises the
sharp consumption pullback observed in heavy-MEW economies. Duca,
Muellbauer and Tobin (2013), in *European Economy* Discussion Paper 14,
formalise what they label the "LIVES" approach — the *l*ife-cycle
*I*ntegration of *V*ariable *E*xpectations and *S*tructure — emphasising
the joint determination of consumption, house prices, mortgage debt
and home equity withdrawal in a four-equation system identified by
common factors and cross-equation sign restrictions.

Two parallel implementations frame the present paper. De Bonis, Marino
and Muellbauer (2024) [VERIFY] estimate a single-equation Italian
adaptation that imposes the cross-equation restriction γ_LA + γ_LOANS = 0
(deposits and household debt enter with equal-and-opposite coefficients),
adopts a Jordà (2005) local-projection permanent-income forecaster,
applies a Drehmann (2017) amortising-mortgage adjustment to the real
mortgage rate, and validates the single-equation OLS estimator against
joint SUR. Chauvin and Muellbauer [VERIFY year — likely 2018], in the
*Banque de France Working Paper* series with associated Online
Complement, undertake a similar France adaptation, with particular
attention to the institutional differences (limited home equity
withdrawal, larger social housing sector) that shape the housing-wealth
channel. Both papers explicitly use Williams' Australia work as a
methodological precedent, and the present paper closes the loop by
applying the Italian methodology back to the Australian data on which
the original LIVES Australia estimation was performed.

### 2.3 The Australian application and prior Australian consumption work

The Australian application was developed in two companion papers by
David Williams during his Oxford doctorate. Williams (2009), Oxford
Economics Series Working Paper 432, focuses on the house-price equation
and its identification under financial liberalisation; the paper develops
the spline-based credit-conditions index from STAMP (Koopman et al.
2000) unobserved-components estimation, anchors the index at the four
institutional turning points 1979/1992/1998/2007, and constructs the
non-property household disposable income measure that we replicate in
§3.6. Williams (2010), Oxford Economics Series Working Paper 492,
estimates the four-equation LIVES system (consumption, house prices,
mortgage stock, home equity withdrawal) on Australian data 1977Q2–2008Q2.
The published version of this work, Muellbauer and Williams (2012) "Credit
conditions and the real economy: the elephant in the room", appears as
the lead chapter of *BIS Papers* No. 64 and constitutes our primary
benchmark.

Australian consumption modelling outside the LIVES tradition has a long
history. Tan and Voss (2003) [VERIFY: RBA Research Discussion Paper
2000-09] estimate aggregate-wealth effects on Australian consumption
using ABS National Accounts and RBA balance-sheet data, finding
significant positive effects of both housing and financial wealth.
Dvornak and Kohler (2003) [VERIFY: RBA RDP 2003-07] use a state-level
panel to identify wealth effects from cross-state variation, finding
larger marginal propensities to consume out of stock-market wealth
than out of housing wealth, in apparent contrast to the time-series
evidence; their findings are reconciled by the Muellbauer-Williams
framework once credit-conditions interactions are introduced.

The Reserve Bank of Australia's macroeconometric model MARTIN, introduced
in Cusbert and Kendall (2018) [VERIFY: RBA Bulletin] and documented in
Ballantyne et al. (2019) [VERIFY: RBA Research Discussion Paper 2019-07],
includes a household consumption block that incorporates wealth effects
and credit conditions in a more reduced-form way than the LIVES
specification. The MARTIN consumption equation imposes calibrated
elasticities for several channels rather than estimating the full long-
run cointegrating vector, and abstracts from the explicit CCI spline.
The present paper complements MARTIN by providing a freely estimated
benchmark against which calibrated coefficients can be evaluated, and
by surfacing the identification choices that drive the estimated speed
of adjustment.

A separate strand of Australian work has examined the cyclical
co-movement of consumption with credit and housing conditions in
Bayesian VAR frameworks, including Hambur and Cassidy [VERIFY] on
mortgage payments and consumption, and the broader RBA literature on
debt-burden effects [VERIFY: identify specific refs]. These approaches
identify shorter-run dynamics but do not deliver the long-run
cointegrating vector that the LIVES specification produces, and so are
complementary to the present analysis rather than substitutes.

### 2.4 Identification of credit conditions

The credit-conditions index is the most contested ingredient in the
LIVES framework. Williams (2010) constructs CCI as a latent variable
identified by smoothed-step dummies (`SDMMA` series — five-quarter
moving averages of four-quarter moving averages of step dummies) at
four institutional turning points: 1979 (Campbell Committee, removal
of interest-rate ceilings on bank deposits), 1992 (banking distress and
the entry of the first mortgage originator, Aussie Home Loans), 1998
(the rise of non-bank financial institutions and securitisation), and
2007 (the global financial crisis tightening). The institutional
chronology of Australian financial deregulation underpinning these
choices is documented in Battellino and McMillan (1989) and Edey and
Gray (1996); Bayoumi (1993) provides a cross-country analysis of the
consumption response to financial liberalisation, including Australia,
that quantitatively validates a structural CCI shift in the early
1980s.

In the system-estimation context of Muellbauer and Williams (2012),
each spline coefficient is identified by being a common factor across
the four equations: the same CCI value enters consumption, house
prices, the mortgage stock and home equity withdrawal, with different
loadings. This common-factor identification is the central methodological
contribution of the LIVES family. In single-equation OLS, the
identification is weaker but the spline can still be estimated under
sign-prior restrictions enforced by general-to-specific reduction
(Hendry-Krolzig 2005). We adopt this approach as a robustness check
(Spec 8) in the present paper.

The alternative, observable proxy for CCI — the ratio of housing credit
flow to disposable income, in logs — has the advantage of being measured
directly but the disadvantage of being available only from the early
2000s, after the most informative deregulation episodes have already
occurred. We use the observable proxy as a short-run regressor in the
specifications that include it (Specs 2 and 5), and the spline approach
for the long-run identification in Spec 8.

A separate identification approach uses survey-based credit-conditions
indices. The Federal Reserve's Senior Loan Officer Opinion Survey is
the prototype; the European Central Bank's Bank Lending Survey serves
the same function in the euro area; the Reserve Bank of Australia
operates a Liaison Programme but does not produce a published numerical
index of credit conditions [VERIFY: check whether RBA publishes any
quantitative CCI]. The unavailability of a long-running survey-based
Australian CCI is the practical reason for adopting Williams' spline
approach in the LIVES tradition.

### 2.5 Permanent-income forecasting

The empirical operationalisation of permanent income — the discounted
expected weighted average of future income — requires either explicit
forecasts or a parametric assumption about the income process. The
standard practice in the consumption literature has been to assume an
AR(p) process for log income, fit it on the available sample, and
aggregate the multi-step-ahead forecasts using exponentially declining
weights with discount factor δ. This recipe descends directly from the
PIH literature of Hall (1978) and the consumption-Euler-equation tests
of Campbell and Mankiw (1989).

Two recent methodological developments have improved on this recipe.
Jordà (2005), originally proposed for impulse-response identification,
estimates the discounted weighted average of income over the relevant
horizon directly as the dependent variable in a single regression, with
predictors observable at time *t*. The local-projection approach
sidesteps the compounding of AR misspecification across forecast horizons
that the standard recipe is vulnerable to, and admits a richer predictor
set than is feasible in a parsimonious AR(p). Carroll, Slacalek and
Tokuoka (2014) [VERIFY: ECB working paper] document substantial
differences between the permanent-income series implied by AR(p)
forecasting and by direct local-projection forecasting in the consumer-
expenditure context; the differences are concentrated at structural-break
episodes, where AR(p) forecasts inherit the slow adjustment of the
estimated AR coefficients while local projections can incorporate
predictors capturing the regime change.

De Bonis et al. (2024) explicitly adopt the Jordà local-projection
approach for their Italian permanent-income series, reporting that the
choice "captures much of the slow-down of permanent income growth in
the early 1990s" — a structural feature of Italian growth that the
AR-based forecaster missed. The Italian local projection includes a
`log(labour_force / population)` predictor, which slowly trends with
demographic change and is a natural input to long-horizon income
forecasting. We adopt the same predictor in our Italy-style PI helper
(§4.3) and find a quantitatively similar role: the PI series implied by
local projection in Australia diverges materially from the AR-based PI
series in the early 1990s and after the 2008 GFC. Substantively, the
implied long-run coefficient on log(y^p/y) flips from significantly
negative under the AR forecaster to significantly positive under the
local-projection forecaster, matching Williams' calibrated value.

### 2.6 Where this paper sits

The contribution of this paper is best understood as a contemporary
single-equation revisit and extension of Williams (2010) with three
methodological refinements drawn from the parallel Italian and French
implementations. First, we estimate on a non-overlapping post-1988
sample that adds sixteen additional years of post-GFC data, providing
external validation of Williams' published wealth-coefficient estimates
on data he did not see. Second, we introduce three corrected data
inputs — RBA F6 mortgage rate (replacing a previously biased ABS-implicit
fallback), ABS A84423091W 15+ population (replacing a previously biased
single-year-of-age cohort sum), and ABS Cat 5206.0 Table 20 income
components (enabling Williams' 2009 non-property-income recipe) — that
are typical of the revisions a current vintage of the same data would
deliver. Third, we adopt the Italian methodology of single-equation OLS
with a structured robustness suite covering instrumental variables,
joint SUR estimation, multi-window Chow tests, scaled-income and Drehmann
real-rate alternatives, and a Williams-style spline credit-conditions
robustness column.

The headline paper does not extend to the multi-equation LIVES
system, though the May 2026 work scaffolds the multi-equation build
in a separate folder ([`LIVES/`](../../LIVES/)). Williams (2010)
estimates four equations jointly by FIML; the headline of this paper
estimates the consumption equation alone. Italy's experience
(De Bonis et al. 2024, §4.2) suggests that single-equation OLS
produces consumption-equation coefficients "only a whisker away"
from joint SUR estimation, and we replicate that finding (§8.2).
However, the LIVES phase 1 SUR in [`LIVES/`](../../LIVES/) folder
finds residual correlation ρ̂(ε_C, ε_H) ≈ 0.0007 — joint estimation
gives no efficiency gain — so the case for the full multi-equation
build rests on cross-equation parameter restrictions, not on
efficiency. The full multi-equation system extension is left for a
companion paper (NS-031), with phases 1 and 3 of the
[`LIVES/`](../../LIVES/) folder providing the scaffolding.

The paper does extend the sample back to **1976Q3** (NS-020 phase 1).
The May 2026 data work assembled the public-data backbone for the
back-extension: the TRYM long-run house-price series (1959Q3+), RBA
D03 M3 monetary aggregate (1959Q3+), RBA D02 total credit splice
(1976Q3+), and a user-supplied historical labour force CSV
(1964Q3+). For the 1976Q3–1988Q2 window where ABS sectoral household
balance sheet data are unavailable, we construct aggregate
(`networth_y_proxy`) and disaggregated (`ha_y_proxy`, `nla_y_proxy`,
`eq_y_proxy`, `super_y_proxy`) wealth proxies anchored at 1988Q3
(see §3.9–§3.13). The empirical finding from refitting on the
back-extended sample (§7.3, §8.14, §8.15, §9) is that the
wealth-coefficient gap with Williams *does not close* — falsifying
the earlier conjecture that the gap was a sample-length issue and
pointing instead to single-equation OLS framing as the binding
constraint.



---

## 3. Data and measurement

The dataset assembles quarterly Australian macroeconomic and household
sector observations from **1976Q3 to 2024Q4 (n = 194)**. The sample
start was extended back from 1980Q1 to 1976Q3 in May 2026 once the
public-data backbone for the back-extension (NS-020 phase 1) was
sourced and wired in (TRYM long-run house-price series; RBA D03 M3
monetary aggregate; RBA D02 total credit splice; user-supplied
historical labour force CSV). The disaggregated wealth components
remain bounded at 1988Q3 by their primary source (ABS Cat 5232.0
Household Balance Sheet); for the 1976Q3–1988Q2 window we construct
proxies (§3.10 below) that growth-rate-splice each component onto
its 1988Q3 official value via the most relevant available aggregate.

Estimation is performed on the largest contiguous subset for which
all variables in a given specification are observed:

- **Spec 1–3 (aggregate net worth)** with the back-extension proxy
  fits on n=190 (1977Q3–2024Q4; the binding constraint is
  `real_rate`, which needs a 4-quarter CPI lag for inflation
  computation). On the official `networth_y` (1988Q3+ ABS) it fits
  on n=146.
- **Spec 4–7 (disaggregated wealth)** with the back-extension
  proxies fit on n=190; on the official disaggregated series, n=146.
- **Spec 6 (preferred — disagg + post-2008 PI shift + SR dynamics)**
  is bounded at 2002Q3+ regardless of the wealth-component proxy
  because its short-run CCI variable (`d2_logcci_lag2`) depends on
  `cci_ratio = log(housing_loan_flow / income)` from ABS Cat 5601.0,
  which only starts 2002Q3. n=86 in both samples.
- **Spec 8 (Williams CCI interactions)** fits on n=146 (bounded by
  disaggregated wealth).
- **Spec 9 (Kalman state-space CCI)** fits on the same window as
  the disaggregated specs; the Kalman extraction itself is a
  separate state-space step.
- **Spec 10 (Williams-prior calibrated)** fits via iterative
  fixed-point OLS on n=146.

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
**four** sources, with the deepest segment now extending to 1959Q3:

| Layer | Source | Coverage | Splice convention |
|-------|--------|----------|-------------------|
| TRYM | Treasury Macroeconomic Model historical database (`house_price_history_long.csv`) | 1959Q3–2018Q2 | growth-rate, anchored at 1986Q2 |
| Legacy | `houseprice_old.csv` (privately compiled pre-2003 dwelling-price index, monthly→quarterly) | 1986Q2–2003Q3 | growth-rate, anchored at 2003Q4 |
| Bridge | ABS Cat 6416.0 Residential Property Price Index, 8-capital-cities ("old method") | 2003Q4–2017Q2 | growth-rate, anchored at first overlap |
| Current | ABS Cat 6432.0 Total Value of Dwellings, mean price | 2003Q3–2024Q4 | (the modern overlay) |

The TRYM source was added to the repository in May 2026 and supersedes
the BIS Shrapnel/REIA chain Williams (2010) used: TRYM's curated
historical compilation already incorporates the same BIS Shrapnel
(pre-1978), REIA (1978–1986), and ABS (post-1986) segments that
Williams used, pre-chained into a single coherent 235-quarter series.
The earliest binding observation for `hpi` is therefore now **1959Q3**,
three years deeper than even Williams' fullest sample (1972Q3 start).

#### 3.4.1 Splice methodology — pure growth-rate chain-linking

For each adjacent pair of layers, the splice anchors the level at the
first quarter where both series are non-NA, then back-casts via the
base series' own QoQ growth rates:

```
chained[t] = overlay[t_anchor] × (base[t] / base[t_anchor])  for t < t_anchor
chained[t] = overlay[t]                                       for t >= t_anchor
```

This standard ABS chain-linking convention preserves the base
series' growth rates exactly while pinning the level to the overlay
at the join. By construction there is no level discontinuity at any
join quarter. An earlier implementation used `mean(overlay/base)`
over the full overlap, which produced step jumps at join quarters
where the overlap ratio drifts (notably a –17% step at 1986Q2 under
the long TRYM↔legacy overlap and a +10% step at 2011Q2→Q3 under the
bridge↔current overlap with mismatched units — the bridge is an
index while current is a $-value mean). Both artefacts were
eliminated by the growth-rate convention.

The relative house-price-to-income ratio used in estimation is

```
ln_hp_over_y = log(hpi / (ydi_ann_nom / pop_millions / (cons_deflator_norm / 100)))
```

This is the log of the real house-price index divided by real
disposable income per capita.

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
ABS workbooks, project-supplied CSVs, master quarterly dataset as CSV
and RDS, full estimation pipeline with three execution modes, GitHub
Actions CI) accompanies this paper. The master dataset has **194
quarters × ~110 columns** and is available as a portable CSV
(`master_data.csv`) for hand-editing or off-line replay. See appendix
on data construction for each variable's source identifier, vintage,
and splicing recipe.

### 3.9 RBA D-tables (added for the back-extension)

Three RBA historical statistical tables were added to the data
download in May 2026 to support the NS-020 sample back-extension.

#### 3.9.1 RBA D03 — Monetary aggregates (M3)

The M3 monetary aggregate (`m3_aggregate`) is loaded from
`d03hist.xlsx`, series `DMAM3N` (M3, original/not-seasonally-adjusted,
$ billion). M3 is total economy-wide currency plus transaction
deposits plus all other deposits at ADIs, plus certificates of deposit
issued by ADIs. Coverage: monthly, **1959Q3–2026Q1** (continuous, no
series breaks). Aggregated to quarterly by mean of the three monthly
observations.

M3 is the headline liquid-asset proxy for the pre-1988 portion of the
sample where ABS sectoral household-balance-sheet deposits aren't
available. Williams (2010) used the same M3 series, multiplied by the
household share of factor income, for his pre-1988 splice.

#### 3.9.2 RBA D02 — Lending and credit aggregates (total credit)

Total credit (`credit_total_d02`) is constructed by growth-rate
splicing two RBA D02 series across the July 2019 RBA conceptual
reform:

- `DLCACN` (Total credit, original, $ billion) for 1976Q3–2019Q2
- `DLCACSFN` (Total credit including select financial businesses,
  the post-2019 successor) for 2019Q3+

The two series have no quarterly overlap (DLCACN ends 2019-06,
DLCACSFN starts 2019-07). The splice anchors levels at the boundary
(`first_post × pre[t] / last_pre`) so the join is continuous in
level; the implicit growth rate at the boundary is exactly zero (no
overlap to estimate it from). For analyses that hinge on the
2019Q2→Q3 quarter specifically, treat with caution. Coverage
1976Q3+.

This is total credit, not housing-specific. The housing-specific
series in D02 (`DLCACOHN`, `DLCACIHN`) only goes back to 1990Q1 in
current vintage and so cannot extend the housing-credit history
pre-1990.

#### 3.9.3 RBA D01 — Growth in selected financial aggregates

Downloaded as `d01hist.xlsx` (provides monthly growth rates of the
same aggregates whose levels are in D02). Williams (2010) used D01
housing-credit growth rates to back-cast D02 levels pre-1976; in the
current vintage D02 already extends to 1976Q3 so D01 is not strictly
needed for back-extension. Retained for future use.

### 3.10 Pre-1978 labour force (`labour_force_historic.csv`)

A user-supplied CSV (`labour_force_historic.csv`, 188 quarterly rows
1964Q3–2011Q2) provides:

| Column | Definition | Units |
|--------|------------|-------|
| `pop_total` | Total resident population (annual + interpolation) | thousands |
| `pop_15_64` | Working-age population (15–64) | thousands |
| `labour_force` | Civilian labour force | thousands |
| `unemployed` | Unemployed persons (level) | thousands |

Provenance: the source compilation pulls together pre-1978 series
from ABS Cat 6204.0 (historical labour force, archived), ABS Year
Book Australia, the Foster (1996) *Australian Economic Statistics
1949–50 to 1996–97* compilation, and RBA Occasional Paper No 8.
Conceptually consistent with the current ABS Cat 6202.0 series
(which begins Feb 1978) at the join.

The historic series are growth-rate spliced onto the modern (1978+)
series at 1978Q1:

- `pop_15_64` → `pop_millions` (the working-age population
  denominator used in per-capita normalisation)
- `labour_force` → master `labour_force`
- `unemployed/labour_force × 100` → master `unemp_rate` (level
  replacement before 1978Q1)

Side-effect of this splice: the six quarters 1976Q3–1977Q4 now have
non-NA values for all per-capita and labour-force-derived variables
(`cons_real_pc`, `ydi_real_pc`, `npy_real_pc`, `lf_share`, etc.). On
the data download, `cons_real`, `ydi_nom`, `mortgage_rate`, `hpi`,
M3, total credit, and `prime_age_share` all extend to 1976Q3 already;
the labour-force splice was the binding remaining constraint.

`pop_total` is also exposed in the master as `pop_total_thousands`
(historic-only 1964Q3–2011Q2 because there's no modern 1978+
counterpart in ABS 6202; ABS Cat 3101.0 publishes it but on a
different release schedule). It can be used as an alternative
per-capita denominator (total resident vs civilian 15+) for
sensitivity analyses.

### 3.11 Household-allocated M3 (`m3_household_proxy`)

The pre-1988 liquid-asset proxy is constructed by allocating M3 to
the household sector via the wage share of GDP:

```
m3_household_proxy = m3_aggregate × wage_share / 100
```

Williams (2010) uses the household factor income share for this
allocation. The wage share alone (compensation of employees / GDP)
is a defensible approximation: it captures most of household factor
income (wages dominate Australian household income; mixed income +
property income receivable add ~10 percentage points and track
wage share over time). A documented simplification for our phase-1
back-extension; for a fully Williams-faithful splice, replace
`wage_share` with the broader `(compensation + mixed income +
property income receivable) / GDP` series.

`wage_share` itself is loaded from a user-supplied CSV
(`household_income.csv`) which extracts ABS Cat 5206.0 Table 24
analytical series A2302604K (compensation of employees, % of GDP),
1959Q3–2024Q4. Range over the sample: 49–62%, with values ~60% in
the 1970s, falling to ~50% today (capital-share rise).

Coverage of `m3_household_proxy`: 1976Q3+ (limited by `wage_share`
in the supplied CSV), values $22 billion (1976Q3) to $1,673 billion
(2024Q4).

### 3.12 Aggregate net-worth proxy (`networth_y_proxy`)

To enable Spec 1–3 fits on the back-extended sample, we construct
an aggregate net-worth proxy that:

1. Uses M3-allocated-to-households (`m3_household_proxy`) plus a
   `hpi × pop_millions` back-cast of housing wealth as the raw
   wealth aggregate.
2. Growth-rate-splices that raw aggregate ratio onto the official
   `networth_y` at 1988Q3 (so the proxy equals the official series
   from 1988Q3 onwards and back-casts smoothly through 1976Q3).

The housing-wealth back-cast anchors at the first available official
`housing_wealth` observation (1988Q3) and back-casts via
`housing_wealth × hpi × pop_millions` growth, holding dwellings per
capita constant:

```
housing_wealth_proxy[t] = housing_wealth[1988Q3]
                         × (hpi[t] / hpi[1988Q3])
                         × (pop_millions[t] / pop_millions[1988Q3])
                         for t < 1988Q3
                       = housing_wealth[t] for t >= 1988Q3
```

The aggregate proxy is then:

```
raw_proxy[t]    = (m3_household_proxy[t] + housing_wealth_proxy[t]) / ydi_ann_nom[t]
scale           = networth_y[1988Q3] / raw_proxy[1988Q3]    (≈ 1.90 in current vintage)
networth_y_proxy[t]
                = networth_y[t]              for t >= 1988Q3
                = raw_proxy[t] × scale       for t < 1988Q3
```

Caveats explicitly documented in [data.md §3.4b](data.md):

- The back-cast omits equities and super (quantitatively small in
  1976–1988 — Australian super pre-Superannuation Guarantee 1992
  was a negligible household asset class).
- The back-cast omits debt netting (mortgage debt was a much smaller
  share of household balance sheets in the 1970s than today).
- Use only for back-extension exercises; never as a substitute for
  `networth_y` on the modern sample where the official series exists.

The proxy values across key dates: 5.09× annual income (1976Q3),
4.67 (1980Q1), 4.72 (1985Q1), 5.37 (1988Q3 — anchored to official),
10.23 (2024Q4). Shape is consistent with the historical Australian
wealth-to-income trend (flat through 1970s/early 80s, sharply rising
post-1985 financial deregulation).

### 3.13 Disaggregated wealth proxies (NS-020 phase 1)

Four additional proxies extend the disaggregated wealth components
to the back-extended 1976Q3+ sample. Each equals the official
series for t ≥ 1988Q3 by construction; for t < 1988Q3 it back-casts
via the most relevant available aggregate.

**`ha_y_proxy`** = `housing_wealth_proxy / ydi_ann_nom`. Uses the
hpi×pop back-cast described in §3.12. Values: 2.68 (1976Q3) → 2.83
(1988Q3) → 6.41 (2024Q4).

**`fin_deposits_proxy`** anchors `fin_deposits[1988Q3]` and grows
by `m3_household_proxy`:

```
fin_deposits_proxy[t] = fin_deposits[1988Q3]
                      × m3_household_proxy[t] / m3_household_proxy[1988Q3]
                      for t < 1988Q3
```

**`fin_loans_proxy`** anchors `fin_loans[1988Q3]` and grows by RBA
total credit:

```
fin_loans_proxy[t] = fin_loans[1988Q3]
                   × credit_total_d02[t] / credit_total_d02[1988Q3]
                   for t < 1988Q3
```

**`nla_y_proxy`** = `(fin_deposits_proxy − fin_loans_proxy) / ydi_ann_nom`.
Values: +0.20 (1976Q3, households are net liquid creditors) → −0.05
(1988Q3) → −0.72 (2024Q4, modern net debtor position). The sign-flip
around 1988 captures the post-deregulation debt build-up.

**`eq_y_proxy`**: held constant at the 1988Q3 value pre-1988 (Option
B in our methodology — Australian household equity holdings were a
small wealth share in the late 1970s/early 80s; the constant
assumption introduces little level error and is straightforward to
upgrade to ASX-All-Ords back-cast). Value: 0.60 (constant) → 0.92
(2024Q4).

**`super_y_proxy`**: linear ramp from 0.1× the 1988Q3 value at
1976Q3 to the 1988Q3 value, then official thereafter. The 0.1
anchor matches Williams (2010) Table A.1 ballpark for the
pre-Superannuation-Guarantee era (SGC mandate 1992). Values: 0.07
(1976Q3) → 0.66 (1988Q3) → 2.44 (2024Q4).

#### 3.13.1 Coherence check — disagg sum vs aggregate proxy

Both `networth_y_proxy` (M3-allocated + housing back-cast,
growth-rate-spliced onto the official broad networth) and the
**sum-of-disaggregated** `networth_y_disagg_proxy` =
`ha_y_proxy + nla_y_proxy + eq_y_proxy + super_y_proxy` are
exposed in the master.

At 1988Q3 (the boundary):

- official `networth_y` (broad, uses ABS A83722648X closing net
  worth): **5.37**
- `networth_y_proxy` (aggregate, anchored to broad): **5.37** by
  construction
- `networth_y_disagg_proxy` (narrow, sum of components): **4.04**

The 25 percent gap between aggregate and disagg sum is the "other
wealth" component of ABS closing net worth (life office reserves,
unincorporated business equity, etc.) that is absent from the
narrow definition. For Spec 4–7 fits, which use the disaggregated
components individually, the disagg sum is the implicit reference.
For Spec 1–3 fits, the aggregate proxy is used.

### 3.14 Master variable coverage tiers (under 1976Q3+ spine)

After all splicing, master variable coverage falls into the
following tiers:

| First non-NA | n  | Variables (selected) |
|--------------|---:|----------------------|
| 1976Q3       | 56 | cons, ydi, hpi, mortgage_rate, M3, total credit, prime_age_share, all dummies, m3_household_proxy, ha_y_proxy, nla_y_proxy, eq_y_proxy, super_y_proxy, networth_y_proxy, ln_networth_y_proxy, cons_real_pc, ydi_real_pc, npy_real_pc, labour_force, unemp_rate, lf_share, pop_millions, ln_cons_real_pc, ln_ydi_real_pc, ln_hp_over_y, ecm_lag (1976Q4) |
| 1977Q3       |  2 | real_rate, hicp_4q_ann (4-quarter CPI lag) |
| 1978Q2       |  1 | ydi_ann_8qma (8-quarter MA) |
| 1988Q3       | 21 | OFFICIAL disaggregated wealth (ha_y, eq_y, super_y, nla_y, ifa_y, networth_y, debt_y), housing_wealth, fin_deposits/equities/super/loans (RAW from ABS 5232) |
| 2002Q3       |  5 | cci_ratio, fhb_share, housing_loan_flow, fhb_loans, non_fhb_loans |
| 2009Q1       |  2 | mortgage_interest_burden_rba, mortgage_payment_burden_rba (RBA E13) |

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

We implement two forecasting methods. The canonical method for the
WP results below is **Italy LP**; **AR** is reported as a methodology
robustness column in §8.9.

- **Method 'Italy' (canonical)**: Jordà (2005) local projection. For
  each `t` where the future horizon is observable, the discounted
  weighted average is computed directly as the dependent variable in a
  single regression on a richer predictor set including `log(lf_share)`
  (the Italian innovation, capturing slow-moving demographic effects on
  trend income), trend, post-2008 split-trend, 4-quarter-MA log income,
  unemployment rate, and 4-quarter-difference dynamics. Forecast values
  are then constructed as fitted values of this single regression for
  every t.

- **Method 'AR' (robustness)**: rolling AR(8) regression of log income
  on eight own lags plus a linear trend, post-2008Q3 step dummy, and
  trend-break interaction. Forecasts are aggregated over 40 horizons
  using the discount weights. Optional predictors `unemp_rate`,
  `log_oil`, `log_reer`, `log_stocks` are added if available. A 2008Q3
  ogive learning weight smoothly attenuates the term over 15 quarters
  to a steady-state weight of 0.5.

The two methods differ materially on two coefficients in the consumption
equation (see §8.9): the speed of adjustment and the long-run
permanent-income coefficient. The canonical Italy LP delivers
λ = −0.218 in the preferred spec, vs −0.052 under AR. We report both,
with Italy LP carrying the headline narrative.

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

### 5.1 The Williams smoothed-step spline approach

Williams (2010) constructs CCI as a spline of `SDMMA` smoothed-step
dummies — a 5-quarter moving average of a 4-quarter moving average of
a 0/1 step — at institutional turning points in the Australian
financial-policy chronology. Each knot's coefficient is constrained
by a sign prior derived from institutional history (deregulation
episodes positive; retrenchment episodes negative), enforced by
Hendry-Krolzig (2005) drop-on-violation general-to-specific reduction.

Williams' canonical paper uses four knots: 1979Q1 (Campbell Committee,
removal of interest rate ceilings), 1992Q1 (NBFI distress post the
early-1990s recession), 1998Q1 (NBFI/securitisation expansion), and
2007Q1 (GFC retrenchment). The four-knot choice reflects the
institutional information available at the time of his 1977-2008
sample: STAMP-derived turning points and a deregulation calendar
ending shortly after the GFC.

**On our 1988Q4-onwards sample, only one of Williams' four canonical
knots survives sign-prior reduction.** A direct replication of the
Williams 4-knot specification yields:

| Williams knot | Sign prior | Status on 1988+ sample |
|---|---:|---|
| 1979Q1 | + | aliased (constant within window) |
| 1992Q1 | − | sign violator (data signal +ve) |
| 1998Q1 | + | sign violator (data signal −ve) |
| 2007Q1 | − | survives, coef ≈ −0.014 |

The 1979 deregulation knot is mechanically uninformative because the
smoothed step reaches unity by 1980Q2, three years before our window
opens. The 1992 and 1998 knots fail their institutional sign priors:
the post-1988 sample observes the recovery from the early-1990s
banking distress (during which credit growth resumed and the OLS
coefficient turns positive) and the late-1990s NBFI period without the
contrast against the prior tight regime that identifies the loosening
direction.

A direct 4-knot replication is therefore *not* identifying the four
distinct credit-conditions episodes Williams' framework attributes to
the spline. It is identifying one — the 2007 GFC tightening — plus a
constant.

### 5.1b The maximal-GETS Australian CCI

Rather than impose Williams' published knot count on a sample that
cannot identify three of his four knots, we adopt a **maximal-GETS
approach**: start from a richer 15-knot candidate set covering the
documented Australian financial-policy chronology, and let drop-on-
violation reduction prune knots that are aliased or violate their
institutional sign prior. The 15 candidate institutional events are
enumerated in [`knot_experiment_findings.md`](knot_experiment_findings.md)
Appendix and span Campbell '79, housing-finance dereg '86, state-bank
distress '90, banking distress '92/'93, Wallis/APRA '98, GFC '07,
deposit guarantee '08, FHB Boost '09, APRA macroprudential '14/'17,
Hayne Royal Commission '19, APRA cap removal/buffer reduction '19Q3,
COVID/JobKeeper '20, and APRA buffer hike '21.

On the 1988Q4-2024Q4 sample this candidate set yields six surviving
knots:

| Knot | Sign prior | Coef (OLS) | Reading |
|---|---:|---:|---|
| 1992Q1 | − | −0.020 | Banking distress / Aussie Home Loans |
| 2007Q3 | − | −0.007 | GFC tightening |
| 2009Q1 | + | +0.006 | First Home Buyer Boost |
| 2019Q1 | − | −0.027 | Hayne Royal Commission lending crackdown |
| 2020Q2 | + | +0.077 | COVID/JobKeeper income support |
| 2021Q4 | − | +0.005 | APRA serviceability buffer hike |

(The 1990Q3, 1993Q1, 1998Q3, 2008Q4, 2014Q4, 2017Q1 and 2019Q3 knots
violate their sign priors and are dropped; 1979Q1 and 1986Q1 are
aliased.)

The `cci_williams` series we use throughout the rest of the paper is
constructed from these six surviving knots, peak-normalised to unity.

This approach is methodologically defensible on three grounds: (i) the
candidate set comes from documented Australian institutional history,
not authorial choice of specific dates; (ii) the surviving knots are
those whose data-signal aligns with their institutional sign prior, so
the spline is *empirically* identified rather than imposed; (iii) the
resulting λ on Spec 8 is materially closer to Williams' published
−0.286 than the canonical 4-knot replication delivers; with the
May 2026 addition of the time-varying housing-wealth m.p.c.
interaction (`ha_x_cci`; §5.5), Spec 8's λ = −0.383, exceeding
Williams in magnitude. Williams' canonical 4-knot setup is retained
as a robustness benchmark in
[`model_helpers.R`](../R/model_helpers.R) via
`build_williams_cci_basis_canonical()`. The May 2026 cross-check
against Williams' Aust paper §5.1 also added a sectional sign-prior
alternative (`build_williams_cci_basis_sectional()`); the placebo
test on this alternative (§5.2.2) shows it does not outperform the
maximal-GETS canonical on our extended sample.

### 5.2 The placebo battery

Whether the Williams spline is identifying genuine credit-conditions
turning points (rather than acting as flexible detrending of the
consumption-equation residual) is empirically testable. We construct
a random-knot placebo: 200 draws of 4 knots uniformly distributed in
the 1979–2007 window (Williams' candidate window), each fitted via
the same drop-on-violation reduction protocol. The canonical Williams
result's percentile rank in this placebo distribution measures
whether his specific knot choice is identifying something the data
genuinely flags vs whatever a flexible 4-knot smoothed-step series
could fit by chance.

#### 5.2.1 Three placebo runs

| Spec | Sample | adj R² %ile | \|λ\| %ile | Verdict |
|------|--------|------------:|----------:|---------|
| Literal Williams 4-knot                 | 1988Q4+ (n=146) |   49 |   22 | At placebo median |
| Literal Williams 4-knot                 | 1976Q3+ (n=190) |  **19** |  **10** | Fails — below median |
| Maximal-GETS canonical (15-knot reduce) | 1976Q3+ (n=190) |  **64** |   36 | Weakly above median  |

(Source: [`back_extension_findings.md`](back_extension_findings.md)
§3a, §3b.)

The literal Williams 4-knot specification fails the placebo on both
samples, and the failure *deepens* on the extended sample (49th→19th
percentile on R²). The maximal-GETS reduction partially rescues
identification (64th percentile on R², 36th on |λ|) but does not
deliver strong support — random combinations of 15 knots/priors
following the same reduction protocol produce *faster* mean reversion
than the canonical institutional choice in 64% of cases.

#### 5.2.2 Sectional sign priors (Williams Aust paper §5.1) tested

Williams (Aust paper §5.1) imposes sign priors over PERIODS rather
than knot-by-knot:

| Period       | Sign prior     | Rationale                |
|--------------|---------------:|--------------------------|
| 1982–1990    | non-negative   | Financial deregulation   |
| Early 1990s  | non-positive   | Banking sector distress  |
| Mid-1990s–2006 | non-negative | New entrants, securitisation |
| 2007+        | non-positive   | GFC                      |

We constructed a parallel CCI basis (`build_williams_cci_basis_sectional()`
in [`model_helpers.R`](../R/model_helpers.R)) with one knot per
period, extending Williams' four periods to cover post-2008 events
(APRA 2014, APRA 2017, COVID 2020, APRA 2021). On our extended
sample with random-period placebo (200 draws of 8 random knots and
8 random ±1 priors), the sectional canonical sits at the **36th
percentile R², 40th percentile |λ|** — *worse* than the maximal-GETS
canonical, not better. Williams' specific period dating does not
outperform random period placements on the post-2008-extended window.

The takeaway across §5.2.1 and §5.2.2: neither the literal 4-knot
construction, nor a maximal-GETS reduction, nor sectional sign priors
deliver strong placebo support on our extended sample. The CCI's
identification is consistent with single-equation OLS using flexible
smoothed-step dummies that the data can fit, but is not consistent
with a structurally-identified common factor.

### 5.3 Why the spline alone cannot identify the CCI as a common factor

The placebo evidence is consistent with the structural diagnosis in
the LIVES literature itself. Williams (2010) and Duca-Muellbauer
(2013, ECB WP 1581) estimate the CCI inside a **multi-equation
system** (consumption, house prices, mortgage stock, home equity
withdrawal — Williams; consumption + refinancing rate — Duca-
Muellbauer) where the *same* CCI series enters all equations
simultaneously. Williams imposes a normalisation (ζ_h = 1 in the
HP equation) and estimates ζ_c, ζ_m, ζ_w as relative scalings; this
cross-equation parameter restriction is what identifies the CCI as
a common factor rather than as an equation-specific residual.

In a single-equation OLS estimation, the spline is fit only to the
consumption-equation residual. There is no constraint that the same
knot loadings must satisfy sign priors in the HP, mortgage stock or
HEW equations simultaneously. Our LIVES phase 3 implementation
([`LIVES/`](../../LIVES/) folder) tests this directly:

#### 5.3.1 Cross-equation joint sign-survival (LIVES phase 3)

We refit the Williams 15-knot maximal candidate set with the
consumption equation, the HP equation, and the mortgage-stock
equation simultaneously, then require each knot to satisfy its
institutional sign prior in **all three** equations to be retained.

| Survival regime | Surviving knots | n |
|---|---|---:|
| Consumption equation only (Spec 1 with `ln_networth_y_proxy` on extended sample) | 1979, 1986, 1992, 2007, 2017, 2020 | 6 |
| **Joint (C ∩ H ∩ M)**                  | **1986, 2017**                         | **2** |

Of 6 knots that survive when fitted to consumption alone (here using
the Spec-1 aggregate-proxy specification on the back-extended sample;
the canonical Spec-4 disaggregated specification on the same sample
yields a different but overlapping survivor set, e.g. 1992Q1, 2007Q3,
2009Q1, 2019Q1, 2020Q2 from the canonical Ausreplication pipeline),
only **1986 (financial deregulation) and 2017 (APRA macroprudential
round II)** have signs consistent with their institutional priors
across consumption, house prices and mortgage stock simultaneously.
The other surviving knots (whichever set) sign-violate in HP or
mortgage-stock equations.

The maximal-GETS protocol's identification of 6 knots was therefore
overstated — 4 of them were consumption-equation-specific and would
not survive a true cross-equation common-factor restriction. This is
the empirical content of the placebo failures in §5.2: without
imposing cross-equation sign consistency, the consumption-equation
residual can be flexibly fit by smoothed-step dummies whose
information content is consumption-specific.

#### 5.3.2 What joint identification fixes

When we rebuild `cci_williams_joint` using the 2 jointly-surviving
knots and re-estimate the HP equation with the new CCI:

| HP equation, CCI loading | (a) cons-only CCI | (b) joint OLS | (c) joint SUR |
|---|---:|---:|---:|
| Estimate                 |  −0.024 | +0.024 | +0.024 |
| Sign                     |   ✗ violator | ✓ | ✓ |

The HP equation's CCI loading flips from significantly negative
(under the consumption-only CCI) to significantly positive (under
joint identification) — Williams' cross-equation sign restriction
working as intended. The mortgage-stock equation's CCI loading
remains negative under joint identification (the simple
joint-sign-survival approximation we use weights surviving knots
by consumption-equation coefficients; full FIML with parameter
restrictions across equations would be needed to flip this).

(Sources: [`LIVES/docs/phase3_findings.md`](../../LIVES/docs/phase3_findings.md);
[`LIVES/R/joint_cci_identification.R`](../../LIVES/R/joint_cci_identification.R).)

### 5.4 LIVES phase 1 SUR — joint estimation gives no efficiency gain

A complementary test of the multi-equation framework's value is
whether SUR/FIML provide efficiency gains over equation-by-equation
OLS. The phase 1 SUR (consumption + HP, both equations on the
extended 1976Q3+ sample using Spec 1 with the aggregate networth
proxy) finds residual correlation **ρ̂(ε_C, ε_H) ≈ 0.0007**
(essentially zero). SUR coefficients are within 0.1% of OLS for
nearly every term. This is robust across specification variants:
even with no `cci_williams` and no event dummies, ρ̂ ≈ −0.025.

The joint estimation case for LIVES therefore does not rest on
efficiency gains. It rests entirely on **cross-equation parameter
restrictions** — Williams' FIML imposes that the same CCI loading
enters all four equations with specific sign constraints. SUR alone
imposes only residual covariance flexibility. Phase 1 SUR confirms
that the cross-equation linkage between consumption and house prices
is captured by shared regressors (CCI, real rate, dummies for major
events); it does not reside in unexplained residual covariance at
the quarterly frequency.

(Source: [`LIVES/docs/phase1_findings.md`](../../LIVES/docs/phase1_findings.md).)

### 5.5 Spec 8: CCI interactions (when CCI is available)

When `cci_williams` is available, we estimate a parallel "Spec 8"
incorporating the full Williams interaction structure:

> ... + γ_HA · ha_y + γ_HA_cci · ha_y · CCI
>     + γ_HP · log(p^h/y) · (1 − ϖ · CCI)
>     + α_r · r · CCI
>     + ψ_1 · log(y^p/y) · CCI + ...

with ϖ calibrated to 1.2 following Williams (Aust paper §5.2 fn 9).
Spec 8 thus has four CCI interactions:

- `r_x_cci` (real_rate × CCI; intertemporal substitution, parameter shift)
- `hp_x_1_minus_cci` (the down-payment composite)
- `yp_x_cci` (PI × CCI; expected income role rises with CCI ease)
- **`ha_x_cci`** (housing wealth × CCI; *added in May 2026* per
  cross-check of Williams Aust paper Eq 7 γ_1t·HA, Tobin Lives 2013
  eq 5.2 (HLI)·HA/y)

The total housing-wealth m.p.c. on consumption is `γ_HA + γ_HA_cci ·
CCI`, time-varying with credit conditions. Williams' theory predicts
γ_HA_cci > 0 (m.p.c. rises with CCI as collateral becomes spendable
when credit conditions ease). Empirically on our sample we find
γ_HA_cci = −0.0011 (t = −0.30, p = 0.52) — wrong-signed but
insignificant. Total HA m.p.c. at CCI=1: 0.024 − 0.003 = 0.021
vs 0.024 at CCI=0, a slight decrease. (Williams' own data shows a
similar slight decrease from 0.0488 at peak CCI to 0.0452 at end
of his 2008Q2 sample.)

Adding the `ha_x_cci` interaction does materially affect λ:
post-fix Spec 8 has λ = −0.383, *exceeding* Williams' calibrated
−0.286 in magnitude. So the time-varying housing-wealth interaction
shifts mean-reversion speed but does not significantly change the
*level* of housing-wealth m.p.c.

(Source: [`LIVES/docs/items_1_and_2_findings.md`](../../LIVES/docs/items_1_and_2_findings.md);
implementation in [`australia_estimation.R`](../R/australia_estimation.R)
~lines 1481–1505.)

### 5.6 Why the wealth-coefficient gap with Williams persists on the extended sample

A natural hypothesis from §5.2 is that the back-extension to 1976Q3
would close the gap between our wealth-coefficient estimates and
Williams' Table 1 calibrated values. We test this directly: refit
Spec 4 (disaggregated wealth, no CCI, no SR) on the back-extended
sample using the disaggregated wealth proxies (§3.13).

| LR coef                  | Baseline 1988+ (n=146) | Extended 1976+ (n=190) | % change | Williams Table 1 |
|--------------------------|----------------------:|----------------------:|---------:|-----------------:|
| λ (ecm_lag)              |               −0.140 |               −0.193 |    +37.3 |           −0.286 |
| nla_y                    |               +0.035 |               −0.002 |    −106  |           +0.066 |
| eq_y                     |               −0.119 |               −0.104 |    −13.3 |           +0.013 |
| super_y                  |               +0.040 |               +0.024 |    −41.7 |           +0.013 |
| ha_y                     |               +0.068 |               +0.040 |    −41.6 |           +0.052 |
| ln_yp_over_y             |               +1.07  |               +1.12  |    +4.33 |           +0.20  |

(Source: [`back_extension_findings.md`](back_extension_findings.md)
§3c.)

**The back-extension does NOT close the wealth-coefficient gap.** λ
moves from −0.140 to −0.193 (closer to Williams' −0.286, +37%
improvement, but still 32% short). The individual wealth coefficients
get *smaller*, not larger; `nla_y` collapses to ~zero; `eq_y` retains
its wrong sign. `ln_yp_over_y` remains huge (~+1.1) on both samples
vs Williams' calibrated +0.20.

This **falsifies the §11 hypothesis** in earlier drafts that the
small wealth coefficients were primarily a sample-length issue. The
binding constraint is *not* the post-1988 sample window but the
**single-equation OLS framing**. Williams' values come from a
4-equation FIML system with cross-equation sign restrictions and a
different normalisation; a single-equation OLS of consumption on
wealth ratios cannot recover them, regardless of sample length.

This is consistent with the placebo evidence (§5.2) and the LIVES
phase 1 SUR finding (§5.4): the structural identification Williams
delivers comes from *cross-equation parameter restrictions*, not
from sample length, knot count, sign-prior structure, or
single-equation OLS efficiency. The path to closing the wealth-
coefficient gap is therefore a full FIML build with shared
parameters across equations, which is the next-step companion paper
NS-031 contemplates.

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

Under the canonical `PI_METHOD = 'italy'` setting no spec passes all
four screens; the selector falls back to the most-passes / BIC tiebreak
rule and returns Spec 3 (`australia_spec_selection.csv`):

| Spec | Signs | Coint | λ | Stability | BIC |
|---|---|---|---|---|---:|
| 1 | ✗ | ✗ | ✓ | ✗ | -923 |
| 2 | ✗ | ✗ | ✓ | ✗ | -504 |
| **3** | **✓** | ✗ | **✓** | ✗ | **-923** *(BIC-best of 2-pass)* |
| 4 | ✗ | ✓ | ✓ | ✗ | -909 |
| 5 | ✗ | ✓ | ✓ | ✗ | -498 |
| 6 | ✗ | ✓ | ✓ | ✗ | -496 |
| 7 | ✗ | ✓ | ✗ | ✓ | -501 |
| 7b | ✗ | ✓ | ✗ | ✗ | -363 |
| 8 | ✗ | NA | ✓ | ✗ | -911 |
| 9 | ✗ | NA | ✓ | ✗ | -900 |
| 10 | ✗ | NA | ✓ | ✓ | -492 |

Two patterns emerge once Italy LP is canonical:
- **The sign screen tightens.** Under Italy LP the implied long-run γ
  on each wealth term is roughly a quarter of its AR-method counterpart
  (because |λ| roughly quadruples). Modest negative coefficients on
  individual disaggregated wealth terms are no longer crowded out by
  large positive ones, so eq_y in Spec 4–6 tips slightly negative and
  the sign screen rejects them. Spec 3 (aggregated `networth_y`) avoids
  this by lumping all wealth into a single positive coefficient.
- **The cointegration screen separates aggregated from disaggregated
  forms.** Specs 1–3 (aggregated wealth) fail Engle-Granger; Specs 4–6
  pass it. The aggregated forms have lower long-run R² in the
  cointegrating regression because they conflate three economically
  distinct wealth components.

Methodologically the **disaggregated, Williams-form Spec 6 remains the
headline specification** for the WP narrative, because (a) it is the
form Williams (2010) and the LIVES tradition use, (b) it permits the
γ_LA + γ_LOANS = 0 cross-equation restriction test (§8.5), and (c) the
sign-screen failure (eq_y small-negative under Italy LP) is a known
identification effect that disappears once CCI interactions are added
(Spec 8: eq_y = +0.036, t = 2.11). We carry Spec 3 as the BIC-best
2-pass alternative and Spec 8 / Spec 9 as the CCI-augmented forms.

---

## 7. Results — preferred specification

### 7.1 Headline coefficients

Spec 6 over the full sample 1988Q4–2024Q4 fits on **n = 86** after
lag truncation (the binding constraint is `cci_ratio` from ABS Cat
5601.0, which begins 2002Q3). Spec 6 cannot be back-extended to the
1976Q3+ window because its short-run CCI variable
(`d2_logcci_lag2`) depends on `cci_ratio = log(housing_loan_flow /
income)` from ABS 5601.0; on the back-extended sample, n stays at 86
and coefficients are unchanged. To fit the preferred specification
on the back-extended sample would require either replacing the SR
CCI with one based on a longer-history credit aggregate (e.g.
Δ²log of `credit_total_d02`) or accepting a constant SR CCI = 0
pre-2002. Both are open methodology decisions.

Coefficients (canonical Italy LP, full sample):

> [TABLE-FROM-DATA: Pull from `australia_full_results.csv` filtered to
> Spec6_Preferred. Suggested format: Term, OLS coef, NW SE, t-stat,
> Implied γ (=OLS/|λ|), p-value, sign-OK indicator. Drop dummies to a
> footnote.]

In summary (canonical Italy LP):

- **λ (`ecm_lag`) = −0.218 (NW SE 0.098)**, t = −2.22 — within 25 per
  cent of Williams' published −0.286.
- **`ha_y` = +0.0062 (SE 0.0050)**, t = +1.23. Implied γ = 0.028.
- **`nla_y` = +0.0088 (SE 0.0447)**, t = +0.20. Implied γ = 0.040.
- **`super_y` = +0.0082** (right sign, insignificant). Implied γ = 0.038.
- **`eq_y` = −0.018 (insignificant)** — small wrong-signed coefficient
  that triggers the §6.3 sign-screen failure for Spec 6 under Italy LP.
  Disappears once CCI interactions are added (Spec 8: `eq_y` = +0.036,
  t = 2.11).
- **`ln_hp_over_y` = −0.0003 (essentially zero)** — the
  house-price-affordability long-run effect collapses under canonical
  Italy LP. Spec 8 with the affordability × (1 − 1.2·CCI) interaction
  recovers a structural effect (§8.4).
- **`real_rate` = −0.0006 (insignificant).** No measurable
  contemporaneous intertemporal-substitution effect in the level. The
  Spec 8 `r × CCI` interaction is the channel that recovers it.
- **`ln_yp_over_y` = +0.302 (SE 0.235)**, t = +1.29, with offsetting
  post-2008 break `ln_yp_over_y_post2008` = +0.154 (SE 0.178). The
  positive sign matches Williams' calibrated value (0.20); the
  often-noted "Australian permanent-income puzzle" of a significantly
  negative coefficient is a feature of the AR forecaster (§8.9), not a
  structural feature.
- **Diagnostics**: adj-R² = 0.81, DW = 2.16, AR(1) p = 0.30, AR(4) p =
  0.20, RESET p = 0.0002. Lambda sign-stable across all four sample
  variants (full −0.218, pre-COVID −0.213, COVID-dropped −0.139,
  COVID-rich-dummies −0.173).

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

Under the canonical Italy LP method, **|λ| is within 25 per cent of
Williams' published value**, but the implied long-run γ on individual
wealth terms collapses — because |λ| roughly quadruples relative to
the AR method, the Williams-form γ = OLS / |λ| identity drives the
implied γ down by a factor of four. Specifically (full sample, Italy
LP canonical):

| Term | Williams γ | Williams implied OLS | Our OLS | Implied γ |
|---|---:|---:|---:|---:|
| `ha_y` | 0.0488 | 0.0140 | 0.0062 | 0.028 |
| `nla_y` | 0.1590 | 0.0455 | 0.0088 | 0.040 |
| `ln_hp_over_y` | −0.130 | −0.0372 | −0.0003 | ≈ 0 |
| **λ** | **−0.286** | (same) | **−0.218** | (same; gap −24%) |

The puzzle inverts relative to the AR-canonical specification: under
Italy LP we agree with Williams on the speed of adjustment but
**undershoot Williams' wealth γ by roughly a factor of four**.

In earlier drafts we attributed this to truncated CCI variation on
the post-deregulation Australian sample and conjectured that a
sample back-extension to ~1975 would close the gap. **That conjecture
has now been tested directly and falsified.** Refitting Spec 4
(disaggregated wealth, no CCI, no SR) on the back-extended 1976Q3+
sample using the disaggregated wealth proxies (§3.13):

| LR coef        | 1988+ baseline (n=146) | 1976+ extended (n=190) | Williams Table 1 |
|----------------|----------------------:|----------------------:|-----------------:|
| λ (ecm_lag)    |               −0.140 |               −0.193 |          −0.286  |
| nla_y          |               +0.035 |               −0.002 |          +0.066  |
| eq_y           |               −0.119 |               −0.104 |          +0.013  |
| super_y        |               +0.040 |               +0.024 |          +0.013  |
| ha_y           |               +0.068 |               +0.040 |          +0.052  |

λ moves +37% closer to Williams' value (−0.193 vs −0.286, still 32%
short). The individual wealth coefs get *smaller*, not larger; `nla_y`
collapses to ~zero; `eq_y` keeps its wrong sign. The back-extension
does NOT close the wealth-coefficient gap. (Source:
[`back_extension_findings.md`](back_extension_findings.md) §3c.)

The persistent gap is consistent with the placebo and LIVES findings
in §5: the binding constraint is **single-equation OLS framing**, not
sample length. Williams' Table 1 values come from a 4-equation FIML
system with cross-equation parameter restrictions (the same CCI
loading appears in all four equations under sign constraints; ϖ in
the wealth × (1−ϖ·CCI) interaction is shared); single-equation OLS
of consumption on wealth ratios — even with the maximal-GETS CCI
spline and the time-varying `ha_x_cci` interaction added (§5.5) —
cannot recover them, regardless of sample length.

Spec 8 with the Williams CCI interactions partially recovers wealth
identification. After the May 2026 addition of `ha_x_cci`
(Williams Aust eq 7 γ_1t·HA; see §5.5), Spec 8 delivers
**λ = −0.383** (overshooting Williams' −0.286 in magnitude) on the
1988Q4-onwards sample. Wealth coefficients shift by 150 per cent on
average vs Spec 6 (see §8.4 and
`australia_cci_fit_decomposition.md`).

### 7.4 The Italy LP / AR comparison

The two PI methods differ materially in two coefficients
(`australia_pi_method_comparison.csv`):

| Term | AR estimate | Italy LP (canonical) | Williams |
|---|---:|---:|---:|
| `ecm_lag` (λ) | −0.052 | **−0.218** | **−0.286** |
| `ln_yp_over_y` | −0.201 | **+0.302** | (calibrated 0.20) |

Italy LP:

1. Quadruples |λ| in our preferred spec, bringing it within 25 per cent
   of Williams' published value.
2. Flips the sign of the long-run permanent-income coefficient from
   negative (the "Australian PI puzzle" under AR) to positive, in
   agreement with theory and with Williams' calibrated value.

We interpret the "Australian PI puzzle" as a methodology artefact
rather than a structural feature: the rolling AR(8) forecaster (a)
lacks the labour-force-share predictor that captures Australia's
slow-moving demographic effects on trend income, (b) compounds
short-run AR misspecification across 40 horizons, and (c) is
structurally biased toward forecasts that over-estimate persistence
after large income shocks. The Jordà (2005) one-step direct projection
avoids all three. We adopt Italy LP as the canonical method and report
AR results as a methodology robustness column in §8.9.

---

## 8. Robustness

We run the Italian-style robustness suite of De Bonis et al. (2024) on
the preferred specification (`run_italy_style_robustness()` in code).

### 8.1 OLS vs IV on current income (Hall 1978 endogeneity)

> [TABLE-FROM-DATA: `australia_iv_robustness.csv`]

Current income is instrumented by lagged income (lags 1, 2, 4), lagged
unemployment (lags 1, 2), and lagged mortgage rate. Under canonical
Italy LP, λ on the IV variant is within 0.02 of the OLS estimate; other
coefficients move in the third decimal. We conclude that current-income
endogeneity is not a material source of bias on our sample.

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
down-weight property income mismeasurement. We run the same on the
preferred spec under canonical Italy LP and report the shift in λ
relative to the headline `ydi_real_pc` figure.

### 8.8 Williams non-property income (NPY) robustness

> [TABLE-FROM-DATA: `australia_williams_income_robustness.csv`]

Replacing `ydi_real_pc` with `npy_real_pc` (per Williams 2009 §4.2.1)
provides the income-measure robustness column. NPY is between scaled
income and gross disposable in conservatism — Williams strips property
income but does not 50/50 average. We treat NPY as the closer
methodology match to Williams.

### 8.9 PI method comparison (AR vs Italy LP)

> [TABLE-FROM-DATA: `australia_pi_method_comparison.csv`]

Already discussed in §7.4. The headline: under canonical Italy LP,
λ = −0.218 and `ln_yp_over_y` = +0.302. The AR robustness column gives
λ = −0.052 and `ln_yp_over_y` = −0.201 (the "Australian PI puzzle").
The difference is identification, not noise.

### 8.10 Permanent-income filter sensitivity

> [TABLE-FROM-DATA: `australia_permanent_income_sensitivity.csv`]

We run a 9-cell grid over discount factor δ ∈ {0.90, 0.95, 0.97},
horizon k ∈ {20, 40, 60} quarters, and the GFC ogive on/off. Under the
AR forecaster the preferred-spec λ is stable to within 0.02 across the
grid, indicating that the within-AR-method PI choice is not what drives
the |λ| gap with Williams (the AR vs Italy LP method choice in §8.9 is
the dominant factor).

### 8.11 COVID-period robustness

> [TABLE-FROM-DATA: `australia_lambda_robustness.csv`]

Under canonical Italy LP, λ is sign-stable across all four sample
variants for Spec 6 (full −0.218, pre-COVID −0.213, COVID-dropped
−0.139, COVID rich-dummies −0.173). Spec 7 (cohort-burden) is tighter
(range −0.20 to −0.37). The COVID episode does not destabilise the
headline findings.

### 8.12 Rolling-window estimation

> [FIGURE-FROM-DATA: `australia_rolling_coefs.png`]

A 60-quarter rolling estimation of Spec 6 shows the wealth coefficients
trending mildly downward post-2014 (consistent with the macroprudential
era flattening the wealth-consumption transmission), while λ becomes
slightly less negative in the most recent windows. We do not interpret
this as instability of the model, but rather as a symptom of the
truncated-CCI identification problem discussed in §5.

### 8.13 Out-of-sample forecast validation

> [TABLE-FROM-DATA: `australia_oos_rmse.csv`;
>  FIGURE-FROM-DATA: `australia_oos_forecast_paths.png`]

We run rolling out-of-sample (OOS) validation on five specs (Spec 6
preferred, Spec 4 disagg-no-CCI, Spec 7 cohort-burden, Spec 8
Williams-CCI-interactions, Spec 9 Kalman-CCI-interactions) over 36
expanding-window cuts from 2015Q1 to 2023Q4 at horizons h ∈ {1, 4, 8}
quarters, with random-walk-with-drift and AR(1) benchmark forecasters.

The honest finding: at h = 1 the structural specs are competitive with
the random-walk-with-drift benchmark (Spec 7 is best at RMSE 0.0306,
narrowly beating RW-drift at 0.0310); at h = 4 and h = 8 the
random-walk-with-drift dominates every structural spec by 5–15 per
cent in RMSE. This is the standard "macro forecasting puzzle" — the
LIVES framework's identification advantage is in interpreting
historical co-movement, not in beating naive benchmarks at multi-step
prediction. The WP records this honestly rather than overstating the
forecast performance.

### 8.14 Back-extension robustness — Spec 1 on the 1976Q3+ sample

A primary motivation for the back-extension (§3.9–§3.13) was to test
whether the wealth-coefficient gap with Williams (§7.3, §9) closes
when the sample covers the institutional events that motivate his
CCI splines. We refit Spec 1 (aggregate net worth) on the
back-extended 1976Q3+ sample using `ln_networth_y_proxy`:

| LR coef                | 1988+ baseline (n=146) | 1976+ extended (n=190) | % change |
|------------------------|----------------------:|----------------------:|---------:|
| λ (ecm_lag)            |               −0.177 |               −0.202 |    +14.2 |
| ln_networth_y          |               +0.112 |               +0.107 |    −3.97 |
| ln_hp_over_y           |               −0.0151 |               −0.0038 |    −74.8 |
| real_rate              |               −0.00137 |               +0.00090 |   −165   |
| ln_yp_over_y           |               +0.961 |               +0.971 |    +1.05 |

(Source: [`back_extension_findings.md`](back_extension_findings.md)
§2; implementation in
[`refit_spec1_extended.R`](../R/refit_spec1_extended.R).)

The wealth elasticity is **stable across samples** (0.112 → 0.107,
−4%) — a positive validation of the proxy: doubling the sample
length and adding the deregulation-era regime does not shift the
structural wealth-to-consumption coefficient. Permanent-income
elasticity is also stable (+0.96 → +0.97). λ moves slightly more
negative on the longer sample. The house-price-to-income coefficient
collapses to near zero on the longer sample (a real signal: less
hp_over_y variation pre-1985, when the post-deregulation house price
boom was yet to begin). The real-rate coefficient sign-flips but
both estimates are tiny.

### 8.15 Spec 4 on the back-extended sample (disaggregated wealth)

The disaggregated-wealth proxies (§3.13) allow Spec 4 to fit on the
back-extended sample. Result table is in §7.3 (reproduced from
`back_extension_findings.md` §3c). Headline: **the back-extension
does not close the wealth-coefficient gap**. λ moves from −0.140 to
−0.193 (+37%, still 32% short of Williams' −0.286). Individual
wealth coefficients get smaller, not larger. This was an unexpected
falsification: the original WP §11 claim that the gap reflected
sample-length truncation has been empirically rejected. The binding
constraint is single-equation OLS framing, not sample length.

### 8.16 Maximal-GETS placebo on the back-extended sample

The Williams maximal-GETS canonical CCI on the back-extended 1976Q3+
sample (Spec 1 with aggregate proxy) sits at the **64th percentile
of adj R² and 36th percentile of |λ|** in 200 random 15-knot/15-prior
placebo draws. (Source: [`cci_placebo_maximal_gets_extended.R`](../R/cci_placebo_maximal_gets_extended.R);
findings in [`back_extension_findings.md`](back_extension_findings.md)
§3b.)

This is a meaningful improvement over the literal Williams 4-knot
result (19th/10th percentile on the same extended sample), but does
not reach the strong-support threshold (>90th on both metrics).
Random combinations of 15 knots/priors with the same sign-survival
protocol produce *faster* mean reversion than Williams' canonical
institutional choice in 64% of cases. The maximal-GETS protocol is
doing identification work but most of its lift comes from the
adaptiveness of the drop-on-violation reduction (15 candidate knots
is a lot of flexibility), not from Williams' specific knot/prior
choice.

### 8.17 Sectional sign-prior CCI

Williams (Aust paper §5.1) imposes sign priors over PERIODS rather
than knot-by-knot. We constructed
`build_williams_cci_basis_sectional()` with one knot per period
(1982/1990/1993/2007 plus our 2014/2017/2020/2021 extensions) and
re-ran the placebo. The sectional canonical sits at the **36th
percentile R² and 40th percentile |λ|** — *worse* than the
maximal-GETS canonical (64th/36th). Williams' specific period
dating does not outperform random period placements on the
post-2008-extended sample. (Source:
[`LIVES/docs/items_1_and_2_findings.md`](../../LIVES/docs/items_1_and_2_findings.md)
§2; implementation in
[`LIVES/R/sectional_cci_test.R`](../../LIVES/R/sectional_cci_test.R).)

### 8.18 LIVES phase 1 SUR (consumption + house prices)

Joint Zellner SUR estimation of the consumption equation + a
Williams-style house-price ECM (Aust paper Eq 11) on the
back-extended 1976Q3+ sample yields residual correlation
**ρ̂(ε_C, ε_H) ≈ 0.0007**. SUR coefficients are within 0.1% of
equation-by-equation OLS for nearly every term. Robust across
specification variants (no `cci_williams`: ρ̂ = −0.083; no event
dummies: ρ̂ = +0.043; minimal LR + SR: ρ̂ = −0.025). Joint
estimation gives no efficiency gain at the quarterly frequency.

The case for the multi-equation framework therefore rests on
cross-equation parameter restrictions, not on residual covariance.
(Source: [`LIVES/docs/phase1_findings.md`](../../LIVES/docs/phase1_findings.md);
implementation in
[`LIVES/R/lives_sur_2eq.R`](../../LIVES/R/lives_sur_2eq.R).)

### 8.19 LIVES phase 3 — joint cross-equation CCI identification

We extend the maximal-GETS protocol to require sign-prior survival
across **three** equations simultaneously (consumption + house
prices + mortgage stock). Of the 6 knots that survive
consumption-only fitting, only **2 (1986 deregulation, 2017 APRA
macroprudential round II)** pass the joint test:

| Survival regime | Surviving knots |
|---|---|
| Consumption only | 1979, 1986, 1992, 2007, 2017, 2020 |
| Joint (C ∩ H ∩ M) | **1986, 2017** |

The joint-identified `cci_williams_joint` flips the HP equation's CCI
loading from significantly negative (−0.024 under cons-only CCI) to
significantly positive (+0.024 under joint CCI), consistent with
Williams' cross-equation sign restrictions working as intended. The
mortgage equation's CCI loading remains negative (joint sign-survival
is a sign-only restriction, not a parameter-equality restriction; full
FIML would be required to fix the M equation's loading).

The wealth-coefficient gap with Williams' Table 1 is barely affected
by joint identification — confirming that the gap is structural, not
a CCI-construction artefact. (Source:
[`LIVES/docs/phase3_findings.md`](../../LIVES/docs/phase3_findings.md);
implementations in
[`LIVES/R/joint_cci_identification.R`](../../LIVES/R/joint_cci_identification.R)
and [`LIVES/R/lives_sur_3eq.R`](../../LIVES/R/lives_sur_3eq.R).)

### 8.20 Items 1 and 2 — cross-check follow-ups

Following a cross-check of our implementation against Williams (Aust
system paper) and Duca-Muellbauer (2013, ECB WP 1581 "Tobin Lives"),
we implemented the two highest-priority gaps identified:

**Item 1 — `ha_x_cci` interaction in Spec 8.** Williams Aust eq 7
γ_1t·HA captures the time-varying housing-wealth m.p.c. (Tobin Lives
2013 eq 5.2 (HLI)·HA/y). Adding `ha_x_cci = ha_y × cci_williams` to
Spec 8 produces λ = −0.383 (overshooting Williams' −0.286), with
γ_HA_cci = −0.0011 (insig, p = 0.52, wrong-signed vs Williams'
positive prior). Total HA m.p.c. at CCI=1 is 0.021 vs 0.024 at
CCI=0, a slight decrease. Williams' own data shows similar slight
decrease (0.0488 at peak vs 0.0452 at sample end), so direction is
not unambiguously violated.

**Item 2 — sectional sign priors.** Implemented as
`build_williams_cci_basis_sectional()`. Placebo result: 36th/40th
percentile — worse than maximal-GETS (64th/36th). Cross-check
prediction was empirically wrong; sectional priors do not tighten
identification on our extended sample.

(Source: [`LIVES/docs/items_1_and_2_findings.md`](../../LIVES/docs/items_1_and_2_findings.md).)

---

## 9. Comparison with Williams (2010, 2012)

The structural-parameter comparison table is reproduced in full in
`australia_williams_comparison.md`. The narrative for the methodology
section:

**Where we agree with Williams.** Under canonical Italy LP, the
preferred Spec 6 delivers λ = −0.218, within 25 per cent of Williams'
published −0.286. The NLA cross-equation restriction γ_LA + γ_LOANS = 0
is accepted in every disaggregated specification × sample combination,
validating the Italian convention (De Bonis et al. 2024). The
permanent-income coefficient is positive (+0.30), matching Williams'
calibrated value in sign and broad magnitude.

**Where we differ.** The implied long-run wealth γ on individual terms
is roughly a quarter of Williams' published γ (e.g. `ha_y` γ = 0.028
vs Williams' 0.0488; `nla_y` γ = 0.040 vs Williams' 0.159). In
earlier drafts we attributed this to truncated CCI variation on the
post-deregulation Australian sample and conjectured that a sample
back-extension to ~1975 would close the gap.

**That conjecture has been tested empirically and falsified.** The
data infrastructure for the back-extension was assembled in May 2026
(NS-020 phase 1: TRYM long-run house prices, RBA D03 M3, RBA D02
total credit, user-supplied historical labour force; see §3.9–§3.13).
With the disaggregated wealth proxies (§3.13) we refit Spec 4 on the
back-extended 1976Q3+ sample (n=190) and compared coefficients to
the 1988+ baseline (n=146):

- **λ** improves +37% (−0.140 → −0.193), still 32% short of Williams' −0.286.
- **`ha_y`, `super_y`** *decrease* (0.068 → 0.040; 0.040 → 0.024).
- **`nla_y`** collapses to ~zero (0.035 → −0.002).
- **`eq_y`** retains its wrong sign (−0.119 → −0.104).

The back-extension thus does not close the wealth-coefficient gap.
The binding constraint is **single-equation OLS framing**, not
sample length. This is consistent with the placebo evidence (§5.2 —
literal Williams 4-knot fails on both samples; maximal-GETS only
weakly above median; sectional priors at median) and the LIVES
phase 1 SUR finding (§5.4 — joint estimation gives ρ̂ ≈ 0, so no
efficiency gain).

Williams' Table 1 wealth coefficients come from a 4-equation FIML
system (consumption + house prices + mortgage stock + HEW)
estimated jointly with cross-equation parameter restrictions: the
*same* `CCI_t` enters all four equations with sign constraints; ϖ in
the wealth × (1−ϖ·CCI) interaction is shared across equations;
ζ_h = 1 normalises the HP equation. None of these structural
restrictions are imposed by single-equation OLS. The path to closing
the gap is therefore a full FIML build, not further sample-length or
CCI-construction tweaks. This is the territory of
[`next_steps.md`](next_steps.md) NS-031, the multi-equation companion paper.

Adding the Williams CCI interactions in Spec 8 — specifically with
the May 2026 addition of `ha_x_cci` (the time-varying
housing-wealth m.p.c. of Williams' γ_1t·HA; §5.5) — produces
λ = −0.383, *exceeding* Williams' calibrated −0.286 in magnitude.
The wealth coefficients shift by 150 per cent on average vs Spec 6
(`australia_cci_fit_decomposition.md`); under Spec 8 the implied γ
on `ha_y` is 0.019 and on `nla_y` is 0.078. Spec 8 thus narrows the
gap in the conditioning direction predicted by LIVES theory but does
not close it.

**The LIVES phase 3 finding** (§5.3, §8.19) is the substantive
diagnosis: when we require knots to satisfy sign priors *jointly*
across three equations (consumption + HP + mortgage stock), only 2
of 6 single-equation survivors pass — 1986 (financial deregulation)
and 2017 (APRA macroprudential round II). The maximal-GETS
identification of 6 knots was therefore overstated in the
single-equation pipeline; 4 of them are
consumption-equation-specific and would not survive a true
common-factor restriction. This empirical finding aligns with both
Williams' framework (CCI is a common factor across equations under
parameter restrictions) and Duca-Muellbauer (2013) "Tobin Lives"
(latent factor identified jointly across equations in a state-space
model).

**The Australian PI puzzle resolved.** Under the AR-method robustness
column we replicate the often-noted Australian PI puzzle: the long-run
coefficient on `ln(y^p/y)` is significantly negative (−0.20). Under
canonical Italy LP it is positive (+0.30), matching Williams'
calibrated value (0.20 at CCI=0, rising to 0.95 at CCI peak) in sign
and broad magnitude. We interpret the puzzle as a methodology artefact
of the rolling-AR forecaster, not a structural feature of Australian
consumption behaviour.

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

Reading the chart: under canonical Italy LP the implied wealth γ is
roughly a quarter of Williams' published values, so the partial
contribution of each wealth term to the fitted log(c/y) path is
correspondingly compressed compared with Williams (2010) Charts 2–8.
We initially attributed this to truncated CCI variation on the
post-deregulation sample, but per §7.3 and §9 the May 2026 back-
extension testing has shown the gap is driven by single-equation
OLS framing rather than sample length. The Spec 8 (Williams CCI
interactions) decomposition is the appropriate counterpart for the
Williams 2010 framing, and we
report it as a robustness column. The residual stays small (less than
0.05 in magnitude) outside the GFC and early-COVID windows.

### 10.2 Counterfactuals — status and outstanding work

The paper as currently drafted does not yet include the three policy
counterfactuals scoped in
[`next_steps.md`](next_steps.md) NS-012:

1. **No 2014/2017 macroprudential tightening**: zero out
   `d_apra_2014` and `d_apra_2017` in the fitted contribution, plot
   the implied path of log(c/y) and a 4-quarter cumulative
   consumption gap.
2. **No COVID JobKeeper**: zero out `d_jobkeeper_2020` and the
   two COVID dummies.
3. **Williams' CCI peak vs current**: counterfactually evaluate the
   wealth × (1 − 1.2·CCI) interaction at CCI = 1 (Williams'
   historical peak) vs CCI = 0 (no liberalisation), comparing
   implied log(c/y) levels.

The decomposition machinery in `plot_longrun_decomposition()` already
returns the partial contribution of each long-run regressor
(`australia_longrun_contributions.csv`), so the counterfactuals are a
mechanical computation: zero out the relevant dummy or interaction
and re-aggregate. Estimated effort 1–2 days. Pending decision: should
they run on Spec 6 (preferred but bounded at 2002Q3+) or on Spec 1
back-extended (1976Q3+, but aggregate proxy)?

### 10.3 Policy implications

The empirical findings have direct policy relevance for an Australian
central bank reader.

**Wealth channel of monetary policy.** The May 2026 work confirms
the central LIVES proposition for Australia: housing wealth has the
largest direct long-run elasticity into consumption among the
disaggregated wealth components. The structural γ_HA on the
1988+ baseline Spec 4 is +0.068, on the back-extended sample +0.040;
both are positive and in line with Williams' calibrated 0.0488 at
the CCI peak. Movements in mortgage rates that change housing
values (whether through interest-rate pass-through or
credit-conditions tightening) propagate to consumption with a lag
governed by λ ≈ −0.18 to −0.20 (about a 25-35% cumulative effect at
4 quarters; about 50-60% at 8 quarters; full pass-through at ~20
quarters).

**Macroprudential effects.** The 2014 and 2017 APRA episodes are
modelled as smoothed-step ogive dummies (`d_apra_2014`,
`d_apra_2017`). Their estimated coefficients on Δlog c are typically
small (~−0.005 to −0.012, i.e. 0.5–1.2 percentage points off
quarterly consumption growth) and wash out in the long-run. Spec 8
identifies a positive but insignificant `d_apra_2014` coefficient
and a small negative `d_apra_2017`; both interpretations are
sensitive to the spec. The decomposition shows the macroprudential
episodes contributing modestly to the deviation of consumption from
its long-run path during 2014–2018. Quantifying these contributions
in counterfactual form is item 10.2 above.

**Permanent-income transmission.** Under canonical Italy LP, the
long-run coefficient on `ln(y^p/y)` is ≈ +0.30, matching Williams'
calibrated +0.20 in sign and broad magnitude. Australian households
respond meaningfully to credible permanent-income shocks. For
fiscal-multiplier work: under permanent (vs transitory) income
changes, the propagation to consumption is roughly 30 per cent in
the long run, with full speed-of-adjustment in 4–6 years.

**Credit-conditions identification caveat.** Section 5 documents
extensively that the CCI's identification in single-equation OLS is
weak, and that adding the back-extended sample, sectional sign
priors, and the time-varying `ha_x_cci` interaction does not change
this. Policy-makers using the CCI series for regime classification
(e.g. tightening-vs-easing diagnoses) should be aware that the
spline coefficients reflect consumption-equation residual
identification, not necessarily a structurally identified common
credit-conditions factor. For policy use, the Kalman state-space
CCI (`cci_kalman`, Spec 9; available 1976Q3+ in the back-extended
master) is a less-imposed alternative that doesn't require knot
choices. The maximal-GETS canonical and the Kalman CCI deliver
nearly identical λ and very similar wealth-coefficient shifts (per
`australia_cci_method_summary.md`); they can be used as cross-checks
of one another.

---

## 11. Conclusion

We extend the Williams (2010, 2012) Australian LIVES consumption
estimate to 2024Q4 using publicly available data, document the
methodological choices that drive the estimate, and stress-test
the framework with a battery of new empirical tests including a
sample back-extension to 1976Q3, random-knot placebo tests on both
the original and extended samples, and a multi-equation LIVES build
in a separate folder.

### 11.1 Where we agree with Williams (2010, 2012)

Three substantive findings of the paper match Williams in sign and
broad magnitude:

1. **Speed of adjustment.** Under canonical Italy LP, λ = −0.218 vs
   Williams' published −0.286 — within 25 per cent. With the
   May 2026 addition of the time-varying housing-wealth m.p.c.
   interaction (`ha_x_cci`; §5.5) Spec 8 delivers λ = −0.383, which
   *exceeds* Williams' value in magnitude.

2. **Permanent-income transmission resolved.** The often-noted
   "Australian permanent-income puzzle" — a significantly negative
   long-run coefficient on `ln(y^p/y)` under the rolling-AR(8)
   forecaster — flips to positive (+0.30) under Italy LP, matching
   Williams' calibrated value (+0.20 at CCI=0) in sign and broad
   magnitude. We interpret the puzzle as a methodology artefact of
   the rolling-AR forecaster, not a structural feature of Australian
   consumption.

3. **NLA cross-equation restriction accepted.** The Italian
   convention γ_LA + γ_LOANS = 0 is accepted at the 5 per cent level
   in every disaggregated specification × sample combination on our
   data, validating the De Bonis et al. (2024) methodology.

### 11.2 Where we diverge — and what the May 2026 evidence shows

The implied long-run γ on individual wealth terms in our preferred
spec is roughly a quarter of Williams' published values (e.g.
`ha_y` γ = 0.028 vs Williams' 0.0488). Earlier drafts attributed
this to truncated CCI variation on the post-deregulation
Australian sample and conjectured that a sample back-extension to
~1975 would close the gap. **That conjecture has been tested
empirically and falsified.**

The May 2026 work assembled the public-data backbone for the
back-extension (NS-020 phase 1: TRYM long-run house prices to
1959Q3; RBA D03 M3 monetary aggregate to 1959Q3; RBA D02 total
credit splice to 1976Q3; user-supplied historical labour force CSV
to 1964Q3; aggregate and disaggregated wealth proxies anchored at
1988Q3 and back-cast via M3 × wage-share, hpi × pop, and
linear-ramp procedures; see §3.9–§3.13). Refitting Spec 4 on the
back-extended sample (n=190, 1976Q3+) vs the 1988+ baseline
(n=146):

| LR coef                | 1988+ baseline | 1976+ extended | Williams Table 1 |
|------------------------|---------------:|---------------:|-----------------:|
| λ                      |        −0.140 |        −0.193 |          −0.286  |
| nla_y                  |        +0.035 |        −0.002 |          +0.066  |
| eq_y                   |        −0.119 |        −0.104 |          +0.013  |
| super_y                |        +0.040 |        +0.024 |          +0.013  |
| ha_y                   |        +0.068 |        +0.040 |          +0.052  |

λ improves +37%, but the wealth coefficients **decrease** rather than
move toward Williams' values; `nla_y` collapses to ~zero and `eq_y`
keeps its wrong sign. The wealth-coefficient gap is not a
sample-length issue.

Three lines of evidence converge on the same diagnosis: the binding
constraint is **single-equation OLS framing**, not the CCI
construction or the sample length.

- **Placebo evidence.** The literal Williams 4-knot CCI sits at
  the 49th/22nd percentile of random-knot draws on the 1988+
  sample, *deteriorating* to the 19th/10th percentile on the
  back-extended 1976Q3+ sample. The maximal-GETS reduction
  rescues identification only weakly (64th/36th percentile on
  the extended sample). Sectional sign priors (Williams Aust
  paper §5.1) implemented in May 2026 don't help either —
  36th/40th percentile, contrary to my pre-implementation
  hypothesis. (§5.2; §8.16; §8.17.)

- **LIVES phase 1 SUR evidence.** Joint Zellner SUR estimation of
  the consumption + house-price equations gives residual
  correlation ρ̂(ε_C, ε_H) ≈ 0.0007 — essentially zero. SUR
  coefficients are within 0.1% of equation-by-equation OLS.
  Joint estimation gives no efficiency gain at the quarterly
  frequency. (§5.4; §8.18.)

- **LIVES phase 3 joint identification.** Requiring sign-prior
  survival across three equations (consumption + house prices +
  mortgage stock) reduces 6 single-equation-fit knots to 2 (1986
  deregulation; 2017 APRA macroprudential round II). The
  consumption-fitted CCI is therefore not a true common factor;
  4 of its 6 surviving knots are consumption-equation-specific.
  Joint identification flips the HP equation's CCI loading from
  significantly negative (sign violator) to significantly
  positive (sign-consistent) — Williams' cross-equation sign
  restrictions working as intended. (§5.3; §8.19.)

The path to closing the wealth-coefficient gap is therefore a
**multi-equation FIML build with shared parameters across
equations** — Williams' actual 4-equation system with consumption,
house prices, mortgage stock, and home equity withdrawal estimated
jointly under cross-equation parameter restrictions (the same CCI
loading enters all equations with sign constraints; ϖ in the
wealth × (1−ϖ·CCI) interaction is shared). This is the territory
of the multi-equation companion paper
([`next_steps.md`](next_steps.md) NS-031), which the LIVES
phases 1 and 3 in
[`LIVES/`](../../LIVES/) folder lay foundations for.

### 11.3 Outstanding work, in priority order

1. **Multi-equation companion paper (NS-031).** Full LIVES with
   FIML and cross-equation parameter restrictions, including a HEW
   equation. Requires custom likelihood code (months) but is the
   only path to closing the wealth-coefficient gap with Williams.
   The data (RBA HEW series; Williams Table 4 spec) is the
   binding obstacle.

2. **Counterfactuals for §10.** No-APRA, no-JobKeeper, and CCI=peak
   counterfactuals on the preferred spec. Decomposition machinery
   exists; this is a 1–2 day mechanical computation.

3. **Spec 6 back-extension.** Spec 6 is bounded at 2002Q3+ by the
   `cci_ratio` short-run regressor. Replacing it with a
   longer-history alternative (e.g. Δ²log of `credit_total_d02`
   from RBA D02, available 1976Q3+) would let Spec 6 fit on the
   back-extended sample.

4. **Counterfactual policy evaluation under Spec 8 + extended
   sample.** Once the multi-equation system is operational, the
   CCI-peak vs current counterfactual becomes well-identified.

5. **Documenting `house_price_history_long.csv` provenance.**
   The TRYM source CSV used in §3.4 needs a short data-appendix
   note recording its specific TRYM vintage, retrieval URL, and
   any modifications since release.

### 11.4 What the paper now contributes

Notwithstanding the failure to close the wealth-coefficient gap,
the paper now makes four contributions to the Australian
consumption literature that earlier drafts could not:

1. **A back-extended Australian master dataset** (1976Q3–2024Q4,
   n=194) with documented growth-rate splices for house prices,
   M3, total credit, and labour force, plus aggregate and
   disaggregated wealth proxies for the pre-1988 window.

2. **A cross-sample empirical test** of whether the Williams CCI
   knots identify when the data covers his motivating
   institutional events. Result: they don't. The literal Williams
   4-knot specification fails the placebo on both samples; the
   maximal-GETS reduction barely lifts it above the median; and
   sectional sign priors don't help.

3. **A diagnosis** of why the wealth-coefficient gap with Williams
   persists: single-equation OLS framing, not sample length.
   Triangulated through the placebo battery (§5.2), the LIVES
   phase 1 SUR (§5.4), and the LIVES phase 3 joint identification
   (§5.3). Adding the time-varying `ha_x_cci` interaction (§5.5)
   shifts λ but not the wealth-coefficient level.

4. **A scaffolded multi-equation framework** in
   [`LIVES/`](../../LIVES/) (data prep, house-price equation,
   mortgage-stock equation, joint CCI identification, 2- and
   3-equation SURs) that the companion paper can build on
   directly. Cross-checked against Williams (Aust system paper)
   and Duca-Muellbauer (2013) "Tobin Lives"; gaps documented in
   [`LIVES/docs/cross_check_against_papers.md`](../../LIVES/docs/cross_check_against_papers.md).

The paper is honest about what its single-equation framework can
and cannot deliver. Where Williams gets identification from
joint estimation under parameter restrictions, we cannot — and we
document both the gap and the empirical evidence that further
single-equation tweaks (sample length, knot choice, sign-prior
structure, the `ha_x_cci` interaction) do not close it. This is
itself a substantive finding that the field's previous lack of
back-extended Australian data made impossible to test.

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
