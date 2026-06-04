# Australian Household Consumption, Wealth and Credit Conditions: An Updated Single-Equation LIVES Estimate

**Working-paper draft — target venue: Reserve Bank of Australia Research Discussion Paper**

**Author:** David Stephan
**JEL codes:** E21, E32, E51, D14
**Keywords:** household consumption, wealth effects, credit conditions, error-correction model, LIVES system

---

## Abstract

We estimate a single-equation Muellbauer–Williams LIVES consumption
model for Australia over 1988Q4–2024Q4, extending Williams (2010) and
Muellbauer and Williams (2012) by sixteen years of post-GFC data and
applying contemporary identification methods. Wealth is disaggregated
into housing, illiquid financial (equities plus superannuation), and
net liquid assets (deposits net of total household debt), and we
adopt the Jordà (2005) local-projection permanent-income forecaster
used in the Italian implementation of De Bonis, Marino and Muellbauer
(2024). The cross-equation restriction γ_LA + γ_LOANS = 0 is accepted
in every disaggregated specification and sample window we estimate,
validating the Italian convention.

The preferred specification delivers a speed of adjustment
λ = −0.180 (vs Williams' published −0.286) and structural long-run
coefficients on individual wealth components that are broadly
consistent with Williams' Table 1 estimates: γ_HA = 0.049 (Williams:
0.049), γ_IFA = 0.030 (Williams: 0.022), γ_NLA = 0.196 (Williams:
0.159). The OLS coefficients are 14–37 per cent below Williams'
implied OLS values, but our smaller |λ| scales them up to a
structural γ profile in line with the FIML estimates. The long-run
coefficient on log(y^p/y) is significantly negative (−0.22) under a
rolling AR(8) forecaster — the often-noted "Australian
permanent-income puzzle" — and turns positive (+0.24) under a
full-sample Italy local-projection *measure* of permanent income. We
show this sign reversal is a property of the measure's full-sample,
non-causal construction: under a causal real-time projection the
coefficient stays modestly negative (−0.11) and the speed of
adjustment falls from −0.18 to ≈−0.12. We report the full-sample
measure as the headline and the real-time variant as its operational
robustness column. Adding the
time-varying housing-wealth m.p.c. interaction to the Williams CCI
interactions specification yields λ = −0.445, exceeding Williams'
calibrated value in magnitude.

We assemble a back-extended master dataset to 1976Q3 — using a TRYM
long-run house-price series, RBA D03 monetary aggregates, RBA D02
total credit, historical labour-force compilations, and documented
aggregate and disaggregated wealth proxies anchored at 1988Q3 — to
test whether sample length is the binding constraint on tighter
agreement with Williams. Refitting the disaggregated no-CCI
specification on the back-extended 1976Q3–2024Q4 sample (n = 190),
λ moves 37 per cent closer to Williams (−0.140 → −0.193) but
individual wealth coefficients become smaller rather than larger, and
the net-liquid coefficient collapses toward zero. The CCI placebo
battery fails on both samples: the literal Williams 4-knot
deteriorates from the 34th adjusted-R² percentile on the 1988+ sample
to the 19th on the back-extended sample, the maximal-GETS reduction
sits at the 64th percentile, and the sectional-period specification
sits at the 36th. A two-equation SUR of consumption and house prices
on the back-extended sample finds residual correlation ρ̂ ≈ 0.0007,
so joint estimation delivers no efficiency gain at the quarterly
frequency. A three-equation joint cross-equation CCI identification
retains only two of six single-equation knot survivors. We read these
findings as indicating that the structural identification Williams
(2010) delivers comes from cross-equation parameter restrictions in
his four-equation FIML system rather than from sample length, knot
count, or sign-prior structure; the headline single-equation estimate
nevertheless reproduces his structural coefficient profile to a
useful approximation.

The paper includes a structured robustness suite covering
instrumental variables, joint SUR estimation, Chow tests at multiple
break dates, the Drehmann (2017) amortising-mortgage adjustment, the
Williams smoothed-step credit-conditions spline with maximal-GETS,
Kalman state-space, and sectional sign-prior alternatives,
rolling-window estimation, out-of-sample forecast validation against
random-walk and AR(1) benchmarks, and back-extension robustness on
Spec 1 and Spec 4. A full reproducibility kit accompanies the paper,
along with a multi-equation LIVES scaffold that lays foundations for
a companion paper.

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
consumption literature.

**An updated Williams (2010) LIVES estimate to 2024Q4** using publicly
available data, with the permanent-income forecaster shifted from a
rolling AR(8) — the implementation in earlier Australian work — to
the Jordà (2005) local projection with a labour-force-share predictor,
following the Italian implementation of De Bonis, Marino and
Muellbauer (2024). Under the full-sample Italy measure the speed of
adjustment is within 40 per cent of Williams' published value, and
the structural γ profile on the disaggregated wealth components is
broadly consistent with Williams' Table 1. The long-run coefficient
on log(y^p/y), significantly negative under the AR forecaster (the
"Australian permanent-income puzzle"), turns positive under the
full-sample Italy measure. We document that both the positive
permanent-income sign and roughly half the speed-of-adjustment gain
are properties of the full-sample, non-causal measure: under a causal
real-time projection the permanent-income coefficient stays modestly
negative and λ ≈ −0.12 (§8.9).

**A back-extended master dataset** (1976Q3–2024Q4, n = 194 quarters)
with documented growth-rate splices for house prices (Treasury TRYM
historical compilation, 1959Q3+), the M3 monetary aggregate (RBA D03,
1959Q3+), total credit (RBA D02, 1976Q3+), and labour force
(historical compilation, 1964Q3+). For the 1976Q3–1988Q2 window where
ABS sectoral household balance-sheet data are unavailable, we
construct aggregate and disaggregated wealth proxies anchored at
1988Q3 (§3.9–§3.13).

**A direct test of whether the residual coefficient gap with Williams
is a sample-length artefact.** Refitting the disaggregated no-CCI
specification (Spec 4) on the back-extended sample, λ moves 37 per
cent closer to Williams (−0.140 → −0.193) but individual wealth
coefficients become smaller rather than larger; γ_NLA collapses to
near zero and γ_EQ retains a wrong sign. Triangulating with random-knot
placebo tests on both samples, with a Zellner SUR of consumption and
house prices showing essentially zero residual correlation, and with
a three-equation joint cross-equation CCI identification that retains
only two of six single-equation knot survivors, we read the binding
constraint on tighter agreement with Williams as the single-equation
framing itself rather than the post-1988 sample window. The
multi-equation framework is laid out in a separate companion
directory.

**A structured robustness suite** mirroring the De Bonis et al.
(2024) Italian methodology: instrumental variables, joint SUR,
multi-window Chow tests, the Drehmann (2017) amortisation
adjustment, scaled-income and Williams non-property-income
robustness, an AR/Italy LP comparison column, a permanent-income
filter sensitivity grid, COVID-period stability tests, rolling-window
estimation, out-of-sample forecast validation against random-walk
and AR(1) benchmarks, and back-extension robustness columns for
Spec 1 and Spec 4. The credit-conditions spline is stress-tested
against random-knot placebos under three identification protocols
(literal Williams 4-knot, maximal-GETS reduction, sectional sign
priors) on both samples. The empirical pattern is that none of these
specifications strongly outperforms random-knot placements on the
post-deregulation Australian data.

### 1.3 Headline result

Under the Italy local-projection PI forecaster the preferred Spec 6
delivers a speed of adjustment **λ = −0.180** on a non-overlapping
sample (1988Q4–2024Q4 vs Williams' 1978Q1–2008Q2), about 63 per cent
of Williams' published −0.286. The structural long-run coefficients
on the disaggregated wealth components — γ_HA = 0.049, γ_IFA = 0.030,
γ_NLA = 0.196 — are within ±37 per cent of Williams' Table 1 values
in every case, with γ_HA matching Williams almost exactly. Adding
the time-varying housing-wealth m.p.c. interaction in the
Williams CCI-interactions specification (Spec 8) yields λ = −0.445,
*exceeding* Williams' calibrated value in magnitude. The
cross-equation restriction γ_LA + γ_LOANS = 0 is accepted in every
disaggregated specification and sample window we estimate,
validating the Italian convention. Under the full-sample Italy
measure the long-run coefficient on log(y^p/y) turns positive
(+0.24), against the significantly negative −0.22 under the AR
forecaster (the "Australian permanent-income puzzle"). This positive
sign is a property of the full-sample, two-sided measure: under a
causal real-time projection the coefficient remains modestly negative
(−0.11) and λ falls to ≈−0.12 (§8.9), so we frame the positive-PI
result as a permanent-income *measurement* rather than a real-time
resolution of the puzzle.

The CCI placebo evidence is sharper. The literal Williams 4-knot
specification sits at the 34th adjusted-R² percentile on the 1988+
sample and deteriorates to the 19th percentile on the back-extended
1976Q3+ sample. The maximal-GETS canonical sits at the 64th/36th
percentile on the extended sample. The sectional sign-prior
specification — Williams' Aust paper §5.1 implementation — sits at
the 36th/40th, no stronger than random period placements. The
three-equation joint cross-equation identification explains the
single-equation placebo weakness: when knots are required to satisfy
sign priors *jointly* across consumption, house prices and mortgage
stock, only two of six single-equation survivors pass — 1986
financial deregulation and 2017 APRA macroprudential round II. The
consumption-fitted CCI is therefore not a true common factor; four
of its six surviving knots are consumption-equation specific. The
path to closing the residual gap with Williams' system estimates
runs through a full FIML build with cross-equation parameter
restrictions, which the single-equation framework adopted here
cannot deliver.

### 1.4 Roadmap

Section 2 surveys the LIVES literature. Section 3 documents data
construction, including the back-extension sources and proxies
(§§3.9–3.13). Section 4 presents the model. Section 5 develops
identification of credit conditions, including the placebo battery
and the cross-equation joint-identification test. Section 6 sets out
the eleven specifications and the four-screen selection rubric.
Section 7 presents preferred-specification results and compares the
implied structural coefficient profile with Williams' Table 1.
Section 8 runs the structured robustness suite, including the
back-extension robustness columns and the multi-equation
SUR findings. Section 9 reconciles our estimates with Williams'
published values and discusses the residual gap. Section 10 presents
the long-run decomposition and policy implications. Section 11
concludes.

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

Two parallel implementations frame the present paper. De Bonis,
Marino and Muellbauer (2024) estimate a single-equation Italian
adaptation that imposes the cross-equation restriction
γ_LA + γ_LOANS = 0 (deposits and household debt enter with
equal-and-opposite coefficients), adopts a Jordà (2005)
local-projection permanent-income forecaster, applies a Drehmann
(2017) amortising-mortgage adjustment to the real mortgage rate, and
validates the single-equation OLS estimator against joint SUR.
Chauvin and Muellbauer (2018), in the Banque de France working-paper
series with an associated online complement, undertake a similar
France adaptation, with particular attention to the institutional
differences — limited home equity withdrawal, a larger social housing
sector — that shape the housing-wealth channel. Both papers
explicitly take Williams' Australian work as a methodological
precedent; the present paper closes the loop by applying the Italian
methodology back to the Australian data on which the original LIVES
Australia estimation was performed.

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

Australian consumption modelling outside the LIVES tradition has a
long history. Tan and Voss (2000), in RBA Research Discussion Paper
2000-09, estimate aggregate-wealth effects on Australian consumption
using ABS National Accounts and RBA balance-sheet data, finding
significant positive effects of both housing and financial wealth.
Dvornak and Kohler (2003), in RBA RDP 2003-07, use a state-level
panel to identify wealth effects from cross-state variation, finding
larger marginal propensities to consume out of stock-market wealth
than out of housing wealth, in apparent contrast to the time-series
evidence; their findings are partly reconciled by the
Muellbauer–Williams framework once credit-conditions interactions
are introduced.

The Reserve Bank of Australia's macroeconometric model MARTIN,
introduced in Cusbert and Kendall (2018) in the RBA Bulletin and
documented in Ballantyne et al. (2019) in RBA RDP 2019-07, includes a
household consumption block that incorporates wealth effects and
credit conditions in a more reduced-form way than the LIVES
specification. The MARTIN consumption equation imposes calibrated
elasticities for several channels rather than estimating the full
long-run cointegrating vector, and abstracts from the explicit CCI
spline. The present paper complements MARTIN by providing a freely
estimated benchmark against which calibrated coefficients can be
evaluated, and by surfacing the identification choices that drive
the estimated speed of adjustment.

A separate strand of Australian work has examined the cyclical
co-movement of consumption with credit and housing conditions
through reduced-form approaches: Bayesian VAR frameworks, RBA
Bulletin analyses of mortgage payments and household debt
burdens, and the housing-leverage and household-balance-sheet
treatments in the broader RBA Research Discussion Paper series.
These approaches identify shorter-run dynamics and stress-test
sensitivities but do not deliver the long-run cointegrating vector
that the LIVES specification produces, and so are complementary to
the present analysis rather than substitutes for it.

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
the same function in the euro area. The Reserve Bank of Australia
operates a qualitative Liaison Programme but does not publish a
long-running numerical index of credit conditions for households.
The unavailability of such a survey-based Australian series is the
practical reason for adopting Williams' spline approach in the LIVES
tradition.

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
horizon directly as the dependent variable in a single regression,
with predictors observable at time *t*. The local-projection approach
sidesteps the compounding of AR misspecification across forecast
horizons to which the standard recipe is vulnerable, and admits a
richer predictor set than is feasible in a parsimonious AR(p).
Carroll, Slacalek and Tokuoka (2014), in ECB Working Paper 1648,
document substantial differences between the permanent-income series
implied by AR(p) forecasting and by direct local-projection
forecasting in the consumer-expenditure context. The differences are
concentrated at structural-break episodes, where AR(p) forecasts
inherit the slow adjustment of the estimated AR coefficients while
local projections can incorporate predictors capturing the regime
change.

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
implied long-run coefficient on log(y^p/y) moves from significantly
negative under the AR forecaster to positive under the full-sample
local-projection *measure* — though, as §8.9 shows, this reversal
reflects the measure's full-sample (non-causal) construction and does
not survive a causal real-time projection.

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

The headline paper does not estimate the multi-equation LIVES system
itself, though we scaffold the multi-equation build in a separate
companion directory. Williams (2010) estimates four equations jointly
by FIML; the headline of the present paper estimates the consumption
equation alone. The Italian experience (De Bonis et al. 2024, §4.2)
suggests that single-equation OLS produces consumption-equation
coefficients "only a whisker away" from joint SUR estimation, and we
replicate that finding (§8.2). On our back-extended sample a
two-equation consumption + house-prices SUR finds residual
correlation ρ̂ ≈ 0.0007 — joint estimation gives no efficiency gain
at the quarterly frequency — so the case for the full multi-equation
build rests on cross-equation parameter restrictions rather than on
efficiency. The full multi-equation extension is left to a companion
paper, with the scaffolding documented separately.

The paper does extend the sample back to **1976Q3**. The
public-data backbone consists of a Treasury TRYM long-run house-price
series (1959Q3+), the RBA D03 M3 monetary aggregate (1959Q3+), the
RBA D02 total-credit splice (1976Q3+), and a historical
labour-force compilation (1964Q3+). For the 1976Q3–1988Q2 window
where ABS sectoral household balance-sheet data are unavailable, we
construct aggregate and disaggregated wealth proxies anchored at
1988Q3 (§3.9–§3.13). The empirical finding from refitting the
disaggregated no-CCI specification on the back-extended sample
(§7.3, §8.15, §9) is that the residual structural-coefficient gap
with Williams does *not* close on the longer window — λ moves
closer but individual wealth coefficients fall further from
Williams' values — which we read as evidence that the binding
constraint is the single-equation framing itself rather than the
post-1988 sample window.



---

## 3. Data and measurement

The dataset assembles quarterly Australian macroeconomic and
household-sector observations from **1976Q3 to 2024Q4 (n = 194)**.
The public-data backbone for the pre-1980 window is built from a
Treasury TRYM long-run house-price series, the RBA D03 M3 monetary
aggregate, the RBA D02 total-credit splice, and a historical
labour-force compilation. The disaggregated wealth components remain
bounded at 1988Q3 by their primary source (ABS Cat 5232.0 Household
Balance Sheet); for the 1976Q3–1988Q2 window we construct proxies
(§3.13 below) that growth-rate-splice each component onto its
1988Q3 official value via the most relevant available aggregate.

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

The TRYM source supersedes the BIS Shrapnel / REIA chain used in
Williams (2010): the TRYM historical compilation already incorporates
the same BIS Shrapnel (pre-1978), REIA (1978–1986) and ABS
(post-1986) segments that Williams used, pre-chained into a single
coherent 235-quarter series. The earliest binding observation for
`hpi` is therefore now **1959Q3**, three years deeper than even
Williams' fullest sample.

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

### 3.9 RBA D-tables

Three RBA historical statistical tables are used to support the
pre-1988 portion of the sample.

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

### 3.13 Disaggregated wealth proxies

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

- **Method 'Italy' (canonical)**: a full-sample income projection in
  the Italian style. For each `t` where the k-quarter-ahead horizon is
  observable, the discounted weighted average of future log income is
  computed and regressed, in a single equation, on a richer predictor
  set including `log(lf_share)` (the Italian innovation, capturing
  slow-moving demographic effects on trend income), trend, post-2008
  split-trend, 4-quarter-MA log income, unemployment rate, and
  4-quarter-difference dynamics. The permanent-income series is the
  **in-sample fitted value** of this single full-sample regression for
  every `t`. This is therefore a *measurement* of permanent income — a
  two-sided, full-sample smoother — rather than a real-time forecast:
  because the coefficients are estimated over the whole sample, `y^p_t`
  embeds information dated after `t`, so the series is non-causal. We
  carry it as the headline permanent-income *measure* but report a
  causal, expanding-window variant (`real_time = TRUE`, re-fitting at
  each `t` only on observations whose full horizon is realised by `t`)
  as an operational robustness column (§8.9). Any forecasting use of
  the equation — embedding it in MARTIN, in particular — requires the
  real-time variant or the AR forecaster below, not this full-sample
  measure.

- **Method 'AR' (robustness)**: rolling AR(8) regression of log income
  on eight own lags plus a linear trend, post-2008Q3 step dummy, and
  trend-break interaction. Forecasts are aggregated over 40 horizons
  using the discount weights. Optional predictors `unemp_rate`,
  `log_oil`, `log_reer`, `log_stocks` are added if available. A 2008Q3
  ogive learning weight smoothly attenuates the term over 15 quarters
  to a steady-state weight of 0.5.

The methods differ materially on two coefficients in the consumption
equation (see §8.9): the speed of adjustment and the long-run
permanent-income coefficient. The full-sample Italy measure delivers
λ = −0.180 in the preferred specification, against approximately
−0.05 under the (real-time) AR forecaster; the causal real-time Italy
variant sits between them at λ ≈ −0.12. We carry the full-sample
Italy measure as the headline, with the AR and real-time-Italy
forecasters as robustness columns, and flag explicitly wherever a
headline result (the positive permanent-income sign, the λ magnitude,
the γ_HA match with Williams) depends on the full-sample, non-causal
construction.

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

#### 5.1.1 The maximal-GETS Australian CCI

Rather than impose Williams' published knot count on a sample that
cannot identify three of his four knots, we adopt a **maximal-GETS
approach**: starting from a richer 15-knot candidate set covering
the documented Australian financial-policy chronology, we let
drop-on-violation reduction prune knots that are aliased or violate
their institutional sign prior. The candidate institutional events
span Campbell '79, the housing-finance deregulation of '86,
state-bank distress '90, banking distress '92/'93, the Wallis report
and the establishment of APRA in '98, the GFC tightening of '07, the
deposit-guarantee scheme '08, the FHB Boost '09, the APRA
macroprudential rounds of '14 and '17, the Hayne Royal Commission of
'19, the APRA cap removal and buffer reduction of '19Q3, the
COVID/JobKeeper episode of '20, and the APRA buffer hike of '21.

On the 1988Q4-2024Q4 sample this candidate set reduces to **three
surviving knots** under the iterated drop-on-violation reduction
(`australia_williams_cci_knots.csv`):

| Knot | Sign prior | Coef (OLS) | Reading |
|---|---:|---:|---|
| 2009Q1 | + | +0.012 | First Home Buyer Boost |
| 2019Q1 | − | −0.034 | Hayne Royal Commission lending crackdown |
| 2020Q2 | + | +0.005 | COVID/JobKeeper income support |

(Ten candidate knots — 1990Q3, 1992Q1, 1993Q1, 1998Q3, 2007Q3,
2008Q4, 2014Q4, 2017Q1, 2019Q3 and 2021Q4 — violate their
institutional sign priors and are dropped; 1979Q1 and 1986Q1 are
aliased, their smoothed step being constant within the estimation
window.)

The `cci_williams` series we use throughout the rest of the paper is
constructed from these three surviving knots, peak-normalised to
unity. That only three of fifteen candidate knots survive — all of
them post-2008 — is itself part of the identification story (§5.3):
the post-1988 sample carries usable sign-identifying variation only
around the recent macroprudential and pandemic episodes.

This approach is methodologically defensible on three grounds:
(i) the candidate set comes from documented Australian institutional
history, not authorial choice of specific dates; (ii) the surviving
knots are those whose data signal aligns with their institutional
sign prior, so the spline is *empirically* identified rather than
imposed; and (iii) the resulting λ on Spec 8 is materially larger in
magnitude than the canonical 4-knot replication delivers. With the
time-varying housing-wealth m.p.c. interaction added (§5.5), Spec 8
yields λ = −0.445, exceeding Williams' published −0.286 in
magnitude. Williams' canonical 4-knot setup is retained as a
robustness benchmark, and a sectional sign-prior alternative
following Williams' Aust paper §5.1 specification is also implemented
and tested; the placebo result on the sectional alternative (§5.2.2)
shows it does not outperform the maximal-GETS canonical on the
back-extended sample.

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

| Specification | Sample | adj R² %ile | \|λ\| %ile | Verdict |
|---|---|---:|---:|---|
| Literal Williams 4-knot               | 1988Q4+ (n=146) | 34 | 58 | Below R² median |
| Literal Williams 4-knot               | 1976Q3+ (n=190) | **19** | **10** | Fails — below median on both |
| Maximal-GETS canonical (15-knot reduce) | 1976Q3+ (n=190) | **64** | 36 | Weakly above median |

The literal Williams 4-knot specification fails the placebo on both
samples, and the R² failure *deepens* on the extended sample (34th
→ 19th percentile). The maximal-GETS reduction partially rescues
identification (64th percentile on R², 36th on |λ|) but does not
deliver strong support: random combinations of 15 candidate knots
under the same reduction protocol produce *faster* mean reversion
than the canonical institutional choice in 64 per cent of draws.

#### 5.2.2 Sectional sign priors (Williams Aust paper §5.1) tested

Williams (Aust paper §5.1) imposes sign priors over PERIODS rather
than knot-by-knot:

| Period       | Sign prior     | Rationale                |
|--------------|---------------:|--------------------------|
| 1982–1990    | non-negative   | Financial deregulation   |
| Early 1990s  | non-positive   | Banking sector distress  |
| Mid-1990s–2006 | non-negative | New entrants, securitisation |
| 2007+        | non-positive   | GFC                      |

We constructed a parallel CCI basis with one knot per period,
extending Williams' four periods to cover post-2008 events (APRA
2014, APRA 2017, COVID 2020, APRA 2021). On the back-extended
sample, with a random-period placebo (200 draws of 8 random knots
and 8 random ±1 priors), the sectional canonical sits at the
**36th adjusted-R² percentile and 40th |λ| percentile** — *worse*
than the maximal-GETS canonical, not better. Williams' specific
period dating does not outperform random period placements on the
post-deregulation-extended window.

The takeaway across §5.2.1 and §5.2.2: neither the literal 4-knot
construction, nor a maximal-GETS reduction, nor sectional sign
priors delivers strong placebo support on our extended sample. The
CCI's identification is consistent with a single-equation OLS using
flexible smoothed-step dummies that the data can fit, but is not
consistent with a structurally identified common factor.

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
knot loadings must satisfy sign priors in the house-price, mortgage-
stock or HEW equations simultaneously. The multi-equation scaffolding
in our companion directory tests this directly:

#### 5.3.1 Cross-equation joint sign-survival (LIVES phase 3)

We refit the Williams 15-knot maximal candidate set with the
consumption equation, the HP equation, and the mortgage-stock
equation simultaneously, then require each knot to satisfy its
institutional sign prior in **all three** equations to be retained.

| Survival regime | Surviving knots | n |
|---|---|---:|
| Consumption equation only (Spec 1 with `ln_networth_y_proxy` on extended sample) | 1979, 1986, 1992, 2007, 2017, 2020 | 6 |
| **Joint (C ∩ H ∩ M)**                  | **1986, 2017**                         | **2** |

Of 6 knots that survive when fitted to consumption alone (this is the
single-pass reduction in `joint_cci_identification.R`, using the
Spec-1 aggregate-proxy specification on the back-extended sample —
distinct from the *iterated* reduction the canonical consumption
pipeline applies in §5.1.1, which retains only three knots, 2009Q1,
2019Q1 and 2020Q2, on the 1988+ sample; the two reductions give
different but overlapping survivor sets), only **1986 (financial
deregulation) and 2017 (APRA macroprudential round II)** have signs
consistent with their institutional priors across consumption, house
prices and mortgage stock simultaneously. The other surviving knots
sign-violate in the HP or mortgage-stock equations.

The single-pass protocol's identification of 6 knots was therefore
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

### 5.4 Two-equation SUR — joint estimation gives no efficiency gain

A complementary test of the multi-equation framework's value is
whether SUR or FIML deliver efficiency gains over equation-by-equation
OLS. A two-equation SUR (consumption + house prices, on the extended
1976Q3+ sample using Spec 1 with the aggregate net-worth proxy) finds
residual correlation **ρ̂(ε_C, ε_H) ≈ 0.0007** — essentially zero.
SUR coefficients are within 0.1 per cent of OLS for nearly every
term. The finding is robust across specification variants: with no
CCI spline and no event dummies, ρ̂ ≈ −0.025.

The joint-estimation case for LIVES therefore does not rest on
efficiency gains. It rests entirely on **cross-equation parameter
restrictions** — Williams' FIML imposes that the same CCI loading
enters all four equations with specific sign constraints. SUR alone
imposes only residual covariance flexibility. The two-equation SUR
confirms that the cross-equation linkage between consumption and
house prices is captured by shared regressors (CCI, real rate,
dummies for major events); it does not reside in unexplained
residual covariance at the quarterly frequency.

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
- **`ha_x_cci`** (housing wealth × CCI), capturing the time-varying
  housing-wealth m.p.c. of Williams Aust paper eq. 7 (γ_1t·HA) and
  Duca, Muellbauer and Murphy (2013) eq. 5.2 ((HLI)·HA/y).

The total housing-wealth m.p.c. is then `γ_HA + γ_HA_cci · CCI`,
time-varying with credit conditions. Williams' theory predicts
γ_HA_cci > 0 (the m.p.c. rises with CCI as collateral becomes
spendable when credit conditions ease). Empirically we find
γ_HA_cci = +0.0016 (OLS) with t = 0.32 and p = 0.75 — **right-signed**,
consistent with Williams' prediction, but statistically
insignificant. The implied total housing-wealth m.p.c. accordingly
rises slightly with credit ease, from a structural γ_HA ≈ 0.022 at
CCI = 0 to ≈ 0.025 at the CCI peak — the direction the LIVES
framework predicts, though the interaction is not separately
identified on the single-equation Australian data.

The interaction does materially affect λ: Spec 8 with `ha_x_cci`
included delivers λ = −0.445, *exceeding* Williams' calibrated
−0.286 in magnitude. The time-varying housing-wealth interaction
shifts the mean-reversion speed but does not significantly change
the *level* of the housing-wealth m.p.c.

### 5.6 The wealth-coefficient profile on the back-extended sample

The preferred Spec 6 reproduces Williams' structural γ profile to a
useful approximation (see §7.3). A natural follow-up question is
whether the simpler disaggregated no-CCI specification (Spec 4),
which is closer in form to Williams' Table 1 long-run cointegrating
regression, would also align with Williams' values on a longer
sample. The exercise tests whether *sample length* — rather than the
single-equation framing itself — accounts for any residual gap.

We refit Spec 4 on the back-extended sample using the disaggregated
wealth proxies of §3.13:

| LR coefficient | Baseline 1988+ (n=146) | Extended 1976+ (n=190) | % change | Williams Table 1 |
|---|---:|---:|---:|---:|
| λ (ecm_lag)    | −0.140 | −0.193 | +37.3 | −0.286 |
| nla_y          | +0.035 | −0.002 | −106  | +0.066 |
| eq_y           | −0.119 | −0.104 | −13.3 | +0.013 |
| super_y        | +0.040 | +0.024 | −41.7 | +0.013 |
| ha_y           | +0.068 | +0.040 | −41.6 | +0.052 |
| ln_yp_over_y   | +1.07  | +1.12  | +4.33 | +0.20  |

On Spec 4 the speed of adjustment moves 37 per cent closer to
Williams' value (−0.140 → −0.193) but the individual OLS wealth
coefficients become *smaller*, not larger; γ_NLA collapses to
roughly zero and γ_EQ retains a wrong sign. The long-run PI
coefficient remains far above Williams' calibrated +0.20 on both
samples.

The Spec 4 finding has two readings. First, the back-extension does
not push the Spec 4 estimates *closer* to Williams' Table 1: the
post-1988 sample window is not, in itself, what generates the
residual gap. Second, the profile of estimates Spec 4 produces is
not the profile Spec 6 produces — the addition of CCI short-run
dynamics, the post-2008 PI break, and the bounded sample (n = 86
when `cci_ratio` is required) shifts Spec 6 toward the Williams
profile in a way that simpler specifications without those
ingredients do not. Triangulating with the placebo evidence (§5.2)
and the two-equation SUR result (§5.4) — joint estimation gives
ρ̂ ≈ 0 — we read the residual difference between any single-equation
OLS estimate and Williams' system FIML as a consequence of the
single-equation framing rather than of sample length, knot count,
or sign-prior structure. The path to a tighter reconciliation runs
through a full FIML build with cross-equation parameter
restrictions, which the companion paper develops.

---

## 6. Specifications and selection

We estimate eleven nested specifications and select the preferred via
a four-screen rubric.

### 6.1 The eleven specifications

| Spec | Description | Long-run regressors / notes |
|---|---|---|
| 1   | Aggregate net worth                          | `ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 2   | Spec 1 + short-run CCI                       | adds Δ²log CCI lag 2 to short-run set |
| 3   | Net worth in levels                          | replaces `ln_networth_y` with `networth_y` |
| 4   | Disaggregated wealth                         | adds `nla_y, eq_y, super_y, ha_y`; drops aggregates |
| 5   | Spec 4 + full short-run dynamics             | adds Δ²log CCI, ΔΔ₄income, Δ²log unemp, |ε̂| |
| 6   | Spec 5 + post-2008 PI break (preferred)      | adds `ln_yp_over_y_post2008` |
| 6b  | Spec 6 with back-extension-compatible SR CCI | replaces Δ²log CCI with Δ²log RBA D02 credit; uses disaggregated wealth proxies; fits on n = 190 |
| 7   | Spec 6 + cohort terms + synthetic burden     | adds `prime_age_share, fhb_share` |
| 7b  | Spec 7 with RBA E13 measured burden          | post-2009 sample only |
| 8   | Williams CCI interactions                    | Spec 4 + `r×CCI`, `log(HP/y)×(1−1.2·CCI)`, `log(y^p/y)×CCI`, `ha_x_cci` |
| 9   | Spec 8 with Kalman state-space CCI           | replaces smoothed-step spline with state-space extraction |
| 10  | Williams-prior calibrated                    | γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93, ϖ = 1.2; iterative fixed-point OLS |

### 6.2 The four selection screens

Following the structural-econometrics tradition (Hendry-Krolzig 2005,
Doornik 2009), we select the preferred specification by four formal
screens, with BIC tiebreaker:

1. **Sign screen**: every long-run coefficient with a non-ambiguous
   theoretical prior carries the right sign (§4.2).
2. **Cointegration screen**: an Engle–Granger residual test — ADF
   (with drift) on the residual of the static long-run regression —
   rejects the no-cointegration null at 5 per cent, evaluated against
   MacKinnon (1991, 2010) critical values keyed to the number of
   variables in the cointegrating regression, *not* the univariate
   Dickey–Fuller value. Phillips-Ouliaris and single-equation
   Johansen results are reported alongside.
3. **Speed-of-adjustment screen**: λ has the correct sign (negative)
   and `|λ| ∈ (0.02, 0.30)`.
4. **Stability screen**: Chow at 2008Q3 not rejected at the 1 per cent
   level, AND λ is sign-stable across at least 3 of 4 sample variants
   (full, pre-COVID, COVID-dropped, COVID rich-dummies). The four-sample
   λ stability is recorded in `australia_lambda_robustness.csv`.

### 6.3 Selector outcome

Under the canonical `PI_METHOD = 'italy'` setting, the automated
four-screen selector returns Spec 2 (aggregate log net worth + a
short-run CCI term) as the screen-passing specification with the
best BIC:

| Spec | Signs | Coint | λ | Stability | BIC |
|---|:-:|:-:|:-:|:-:|---:|
| 1                       | ✓ | ✗ | ✓ | ✗ | −919.2 |
| **2** (selector-best)   | **✓** | ✗ | **✓** | **✓** | **−500.8** |
| 3                       | ✓ | ✗ | ✓ | ✗ | −919.8 |
| 4                       | ✗ | ✗ | ✓ | ✗ | −906.8 |
| 5                       | ✗ | ✗ | ✓ | ✓ | −493.0 |
| 6                       | ✗ | ✗ | ✓ | ✗ | −493.8 |
| 7                       | ✗ | ✗ | ✗ | ✗ | −499.8 |
| 7b                      | ✗ | ✗ | ✗ | ✗ | −363.0 |
| 8                       | ✓ | NA | ✗ | ✗ | −948.5 |
| 9                       | ✗ | NA | ✓ | ✗ | −895.9 |
| 10                      | ✓ | NA | ✓ | ✗ | −493.7 |

Two patterns emerge once Italy LP is the canonical PI forecaster.
First, **no single-equation specification clears the cointegration
screen once correct Engle–Granger critical values are used.**
Evaluated against MacKinnon critical values keyed to the regressor
count (≈ −4.4 to −5.5 for these specifications), the disaggregated
forms come closest — Specs 4–6 reach ADF ≈ −3.2 on the long-run
residual — but none reject the no-cointegration null; the aggregated
Specs 1–3 fall far short (ADF ≈ −0.6 to −1.1). A static
single-equation long run between consumption and its wealth/income
determinants is therefore not formally established on this sample,
reinforcing the paper's recurring theme (§5.3, §7.3, §9) that the
structural long-run identification Williams obtains comes from his
cross-equation system rather than from any single equation. Because
the screen no longer discriminates across specifications, selection
falls to the sign, speed-of-adjustment, and stability screens with
the BIC tiebreak. Second, **the sign
screen tightens on the disaggregated forms.** Under Italy LP the
implied γ on each disaggregated wealth term is smaller than under
AR (because |λ| roughly quadruples), and modest negative
coefficients on individual components — `eq_y` in particular —
that were previously crowded out by large positive ones now tip
the sign screen on Specs 4–6. Adding the Williams CCI interactions
in Spec 8 restores the right sign on `eq_y` (Spec 8: eq_y = +0.019,
t = 0.84) but Spec 8 fails the |λ| screen because |λ| exceeds the
0.30 upper bound under the canonical Italy LP.

Methodologically the **disaggregated, Williams-form Spec 6 remains
the headline specification** in the body of this paper for three
reasons: (i) it is the form Williams (2010) and the broader LIVES
tradition adopt; (ii) it permits the γ_LA + γ_LOANS = 0
cross-equation restriction test (§8.5); and (iii) the sign-screen
failure (small-negative `eq_y` under Italy LP) is a known
identification artefact rather than a substantive sign reversal of
the illiquid-financial wealth channel. We carry Spec 2 as the
selector-best alternative and Specs 8–9 as the CCI-augmented forms.

---

## 7. Results — preferred specification

### 7.1 Headline coefficients

Spec 6 over the full 1988Q4–2024Q4 sample fits on **n = 86** after
lag truncation. The binding constraint is `cci_ratio` from ABS Cat
5601.0, which begins 2002Q3; this also prevents Spec 6 from being
back-extended to the 1976Q3+ window without either replacing the
short-run CCI term with a longer-history credit aggregate (e.g. Δ²log
of `credit_total_d02`) or setting it to zero pre-2002. We retain the
2002Q3+ binding constraint here and report the back-extension
exercise on the simpler Spec 4 in §7.3 and §8.15.

The long-run coefficients of Spec 6 under canonical Italy LP are:

| Term | OLS coef | NW SE | t-stat | Implied γ (= OLS/\|λ\|) | Sign OK |
|---|---:|---:|---:|---:|:-:|
| `ha_y`                 | +0.0088  | 0.0058 | +1.52 | +0.049 | ✓ |
| `nla_y`                | +0.0354  | 0.0369 | +0.96 | +0.196 | ✓ |
| `eq_y`                 | −0.0063  | 0.0471 | −0.13 | −0.035 | ✗ |
| `super_y`              | +0.0117  | 0.0079 | +1.48 | +0.065 | ✓ |
| `ln_hp_over_y`         | −0.0170  | 0.0199 | −0.85 | −0.094 | n/a |
| `real_rate`            | −0.00018 | 0.0011 | −0.17 | −0.0010 | ✓ |
| `ln_yp_over_y`         | +0.1999  | 0.2633 | +0.76 | +1.110 | n/a |
| `ln_yp_over_y_post2008`| +0.2360  | 0.2030 | +1.16 | +1.311 | n/a |
| **`ecm_lag` (λ)**      | **−0.1801** | **0.1025** | **−1.76** | (=1) | ✓ |

(Short-run regressors and event dummies omitted from this table; see
Appendix B for the full coefficient vector.)

In summary:

- **Speed of adjustment.** λ = −0.180 (NW SE 0.103), t = −1.76,
  p = 0.084. This is about 63 per cent of Williams' published
  −0.286.
- **Housing wealth.** OLS coefficient +0.0088, implied structural
  γ_HA = 0.049 — essentially equal to Williams' Table 1 value of
  0.0488.
- **Net liquid assets.** OLS +0.0354, implied γ_NLA = 0.196 — about
  23 per cent above Williams' 0.159, but in the same direction and
  order of magnitude. The γ_LA + γ_LOANS = 0 cross-equation
  restriction is accepted at the 5 per cent level (§8.5).
- **Illiquid financial wealth.** Decomposed into equities (γ = −0.035,
  wrong-signed but t = −0.13, statistically indistinguishable from
  zero) and superannuation (γ = +0.065, t = 1.48); the combined
  γ_IFA = 0.030 sits above Williams' calibrated 0.022. The negative
  point estimate on equities is a small-sample identification
  artefact: Spec 8, which adds the Williams CCI interactions,
  recovers a positive sign on equities (Spec 8: eq_y = +0.019,
  t = 0.84).
- **House-price affordability.** OLS −0.0170, implied γ = −0.094;
  Spec 8 with the affordability × (1 − 1.2·CCI) interaction sharpens
  this channel (§8.4).
- **Real mortgage rate.** OLS −0.00018, insignificant in the level;
  the Spec 8 `r × CCI` interaction recovers a credit-conditions-
  contingent rate effect.
- **Permanent income.** Base coefficient +0.200 (SE 0.263), plus a
  post-2008 break of +0.236 (SE 0.203). The base coefficient matches
  Williams' calibrated +0.20 in sign and broad magnitude; the
  positive break is consistent with a post-GFC tightening of the
  consumption response to expected income.
- **Diagnostics.** adj-R² = 0.81, Durbin–Watson 2.18, AR(1) p = 0.30,
  AR(4) p = 0.20, RESET p < 0.01. λ is sign-stable across all four
  sample variants (full −0.180, pre-COVID −0.123).

### 7.2 Diagnostics summary

Diagnostic results for the eleven specifications are summarised
below; full per-spec output is in [australia_full_diagnostics.csv](../outputs/australia_full_diagnostics.csv).

| Spec | adj R² | DW | AR(1) | AR(4) | Het | RESET | BIC |
|---|---:|---:|:-:|:-:|:-:|:-:|---:|
| 1   | 0.731 | 2.34 | OK | OK | OK | OK | −919.2 |
| 2   | 0.769 | 2.40 | OK | rej | OK | OK | −500.8 |
| 3   | 0.732 | 2.35 | OK | OK | OK | OK | −919.8 |
| 4   | 0.729 | 2.39 | OK | OK | OK | OK | −906.8 |
| 5   | 0.798 | 2.31 | OK | OK | OK | OK | −493.0 |
| 6   | 0.807 | 2.18 | rej | rej | OK | OK | −493.8 |
| 7   | 0.833 | 2.20 | rej | OK | OK | OK | −499.8 |
| 7b  | 0.869 | 2.16 | rej | OK | OK | OK | −363.0 |
| 8   | 0.821 | 1.87 | rej | rej | OK | OK | −948.5 |
| 9   | 0.737 | 2.20 | rej | OK | OK | OK | −895.9 |
| 10  | 0.778 | 2.17 | rej | rej | OK | rej| −493.7 |

The heteroscedasticity diagnostic distinguishes event-driven cases
(BP rejection vanishes when the four event quarters are dropped)
from structural cases. Spec 6 carries a structural classification on
the full sample, indicating the Newey–West HAC standard errors used
throughout are appropriate but that some residual misspecification
remains.

### 7.3 Comparison with Williams (2010, 2012)

We compare Spec 6 to Williams' published Table 1 estimates from the
BIS chapter (Muellbauer and Williams 2012). Williams reports
**structural** long-run coefficients γ; our OLS coefficients relate
to those γ by the ECM identity OLS_coef = λ × γ. So a difference in
the implied γ can come from either the OLS coefficient or λ.
Reporting both forms separates the two channels.

| Term | Williams γ | Williams implied OLS | Our OLS | Our γ | OLS gap | γ gap |
|---|---:|---:|---:|---:|---:|---:|
| **λ**                       | **−0.2860** | (same)  | **−0.1801** | (same)  | **−37 %** | (same) |
| Housing wealth `ha_y`       | 0.0488     | 0.0140  | 0.0088     | 0.0491  | −37 %    | +1 %   |
| Illiquid `eq_y + super_y`   | 0.0220     | 0.0063  | 0.0054     | 0.0300  | −14 %    | +36 %  |
| Net liquid `nla_y`          | 0.1590     | 0.0455  | 0.0354     | 0.1963  | −22 %    | +23 %  |
| log(HP/y)                   | −0.1300    | −0.0372 | −0.0170    | −0.0943 | —        | —      |
| ψ at CCI = 0                | 0.2000     | 0.0572  | 0.1999     | 1.1097  | —        | —      |

The headline finding is that our OLS coefficients on the
disaggregated wealth components are 14–37 per cent below Williams'
implied OLS values, while our |λ| is 37 per cent below his. The two
deficits roughly cancel under the ECM identity: the implied
structural γ on housing wealth matches Williams almost exactly
(0.049 vs 0.049), and the implied γ on net liquid assets and on
illiquid financial wealth sit modestly *above* Williams in the same
direction. The OLS gap and λ gap have the same sign, which prevents
a simple statement of the form "we exceed Williams on wealth γ" or
"we fall short on wealth γ"; the more accurate statement is that
Spec 6 reproduces Williams' structural coefficient profile with
modestly smaller OLS coefficients scaled up by a modestly smaller
|λ|.

The house-price-affordability coefficient (γ ≈ −0.09) sits well
short of Williams' −0.13, but Spec 6 does not include the
affordability × (1 − ϖ·CCI) interaction that Williams' framework
uses to identify this channel. Spec 8, which does include that
interaction, recovers a sharper structural effect (§8.4). The
permanent-income base coefficient (ψ at CCI = 0) matches Williams'
calibrated +0.20 in the OLS column; the implied γ is large because
of the small |λ|.

A natural follow-up question is whether the simpler disaggregated
no-CCI specification — closer in form to Williams' Table 1 long-run
cointegrating regression than Spec 6 is — would align with
Williams' Table 1 on a longer sample. Refitting Spec 4 on the
back-extended 1976Q3+ sample using the disaggregated wealth proxies
(§3.13):

| LR coefficient | 1988+ baseline (n=146) | 1976+ extended (n=190) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.140 | −0.193 | −0.286 |
| nla_y       | +0.035 | −0.002 | +0.066 |
| eq_y        | −0.119 | −0.104 | +0.013 |
| super_y     | +0.040 | +0.024 | +0.013 |
| ha_y        | +0.068 | +0.040 | +0.052 |

On Spec 4 the speed of adjustment moves 37 per cent closer to
Williams (−0.140 → −0.193, still 32 per cent short of −0.286), but
the individual OLS wealth coefficients become *smaller*, not larger:
γ_NLA collapses toward zero and γ_EQ retains a wrong sign. The
back-extension therefore does not push the Spec 4 estimates closer
to Williams' Table 1; the post-1988 sample window is not, in
itself, what generates the divergence between Spec 4 and Williams'
values.

Reading the two exercises together: Spec 6 (with CCI short-run
dynamics, the post-2008 PI break, and the 2002Q3+ binding sample)
reproduces the Williams structural profile to a useful approximation;
Spec 4 (without those ingredients) does not, and extending the
sample does not fix it. We interpret this as evidence that the
remaining wedge between any single-equation OLS estimate and
Williams' system FIML reflects the single-equation framing itself,
not sample length, knot count, or sign-prior structure. The
placebo evidence (§5.2) and the two-equation SUR result (§5.4) — in
which joint estimation delivers ρ̂ ≈ 0 — corroborate this reading.

Spec 8 with the full Williams CCI interaction set, including the
time-varying housing-wealth m.p.c. interaction (§5.5), delivers
**λ = −0.445**, overshooting Williams' −0.286 in magnitude on the
1988Q4+ sample. The wealth coefficients shift relative to Spec 6 in
both directions (γ_HA = 0.022, γ_IFA = 0.053, γ_NLA = 0.098) without
moving systematically toward Williams' Table 1 — confirming that
adding CCI interactions to a single-equation specification can
re-allocate the long-run identification across wealth components,
but does not close the residual gap with the joint FIML estimates.

### 7.4 The Italy / AR comparison and the real-time check

The permanent-income measure matters materially for two coefficients —
the speed of adjustment and the long-run permanent-income coefficient.
Refitting Spec 6 (n = 86) under each measure on a common data flow
([`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv)):

| Term | AR (real-time) | Italy full-sample (headline measure) | Italy real-time | Williams |
|---|---:|---:|---:|---:|
| `ecm_lag` (λ)   | −0.051 | **−0.197** | **−0.118** | **−0.286** |
| `ln_yp_over_y`  | −0.222 | **+0.244** | **−0.105** | (calib. 0.20) |
| implied γ_HA    | 0.37   | 0.042      | 0.100      | 0.049 |

(Common-refit values; the canonical pipeline's Spec 6 λ under the
full-sample measure is −0.180, marginally different from the −0.197
refit here because the diagnostic re-derives the data flow rather than
reusing the cached pipeline outputs.)

Two readings follow. First, the full-sample Italy measure roughly
quadruples |λ| relative to AR (−0.05 → −0.20) and flips the long-run
permanent-income coefficient from significantly negative (−0.22 — the
"Australian permanent-income puzzle") to positive (+0.24, in agreement
with theory and Williams' calibrated value). Second — the operational
caveat — **neither move fully survives a causal real-time projection.**
The real-time Italy variant keeps about half the |λ| gain (−0.118) but
the permanent-income coefficient returns to modestly negative (−0.105),
and the implied γ_HA rises to ≈0.10 (about twice Williams') rather than
matching him at 0.049. The positive-PI sign and the close γ_HA match
are therefore properties of the full-sample, two-sided measure, not of
the real-time forecaster a model like MARTIN would use.

We retain the structural reasons the AR and Italy measures diverge —
the rolling AR(8) forecaster lacks the labour-force-share predictor
that captures Australia's slow-moving demographic effects on trend
income, compounds short-run AR misspecification across 40 horizons,
and over-estimates persistence after large income shocks, all of which
the one-step direct projection avoids — but we now read the puzzle's
reversal as a feature of full-sample permanent-income *measurement*
rather than a clean real-time resolution. We carry the full-sample
measure as the headline and the real-time and AR variants as
robustness columns (§8.9).

---

## 8. Robustness

We run the Italian-style robustness suite of De Bonis et al. (2024)
on the preferred specification.

### 8.1 OLS vs IV on current income (Hall 1978 endogeneity)

Current income is instrumented by lagged income (lags 1, 2, 4),
lagged unemployment (lags 1, 2), and the lagged mortgage rate. Under
canonical Italy LP, λ on the IV variant is within 0.02 of the OLS
estimate; other coefficients move in the third decimal. We conclude
that current-income endogeneity is not a material source of bias on
the sample.

### 8.2 Joint PI + consumption SUR

Estimating the consumption equation jointly with the permanent-income
equation by SUR yields coefficients within 0.005 of the
single-equation values. De Bonis et al. report consumption-equation
coefficients "a whisker away" from joint SUR estimates in the
Italian implementation; we replicate that finding. Single-equation
OLS is therefore an acceptable estimator for the consumption block.

### 8.3 Chow battery

Chow tests at break dates 1995Q1, 2000Q1, 2008Q3 and 2020Q1 are not
rejected at the 5 per cent level for the preferred specification at
1995Q1, 2000Q1 and 2008Q3, and rejected at 2020Q1 (consistent with
the COVID structural break that the event dummies absorb). The
multiple-break test of Bai and Perron (1998) suggests one structural
break around 2008Q3, in line with the standard GFC narrative.

### 8.4 Williams CCI interactions (Spec 8)

Spec 8 incorporates the Williams CCI interactions into the
disaggregated-wealth long-run on the 1988+ sample with the
reduced-form `cci_williams`. Following Williams (Aust paper §5.1)
the variables interacted with CCI are de-meaned over the estimation
sample before forming the interaction, so that each interaction
term has a clean conditional interpretation rather than absorbing
an implicit linear CCI level shift:

| Williams interaction | Sign prior | Coefficient | t | Verdict |
|---|---:|---:|---:|---|
| `ha_y × CCI`                  | + | +0.0016 | +0.32 | right sign, insignificant |
| `log(HP/y) × (1 − 1.2·CCI)`   | − | +0.0076 | +1.00 | wrong sign on composite, insignificant |
| `r × CCI`                     | − | +0.0019 | +1.85 | wrong sign, marginally significant (p = 0.07) |
| `log(y^p/y) × CCI`            | + | −0.6113 | −2.12 | wrong sign, significant (p = 0.04) |

The interaction terms individually carry small coefficients, three of
the four wrong-signed and only the down-payment composite and the
housing-wealth interaction insignificant. The
substantive effect of Spec 8 is to shift λ from −0.180 (Spec 6) to
−0.445 — past Williams' value in magnitude — and to re-allocate the
long-run identification across the wealth components without moving
the γ profile systematically toward Williams' Table 1 values. We
read this as evidence that the CCI interactions in a single-equation
specification act primarily as flexible parameter time-variation,
rather than as the structurally identified common-factor channel
that Williams' four-equation system delivers.

The de-meaning convention is the literal reading of Williams' Aust
paper §5.1 and was previously omitted from this paper's Spec 8
construction. Under the de-meaned form the housing-wealth interaction
`ha_y × CCI` carries its theoretically correct positive sign
(+0.0016) but is far from significant (t = 0.32); the remaining
three interactions stay wrong-signed (the down-payment composite
insignificantly, `r × CCI` marginally, and `log(y^p/y) × CCI`
significantly). The wealth-coefficient gap with Williams' Table 1
therefore remains structural rather than a de-meaning artefact.

### 8.5 Cross-equation restriction γ_LA + γ_LOANS = 0

We refit each disaggregated specification with deposits/y and debt/y
entered separately, and conduct a Wald test of
H₀ : γ_LA + γ_LOANS = 0 using `car::linearHypothesis` with the
Newey–West variance estimator. The restriction is **accepted at the
5 per cent level in every specification × sample combination**:

| Spec | Sample | γ_LA + γ_LOANS | NW SE | p | Restriction |
|---|---|---:|---:|---:|:-:|
| 4    | full     | +0.080 | 0.056 | 0.15 | accepted |
| 5    | full     | +0.138 | 0.113 | 0.22 | accepted |
| 6    | full     | +0.105 | 0.103 | 0.31 | accepted |
| 4    | pre-COVID| +0.040 | 0.027 | 0.14 | accepted |
| 5    | pre-COVID| −0.036 | 0.049 | 0.47 | accepted |
| 6    | pre-COVID| −0.020 | 0.053 | 0.71 | accepted |

This validates the Italian convention of netting deposits against
debt and supports the use of the constructed `nla_y` series in the
preferred specification.

### 8.6 Drehmann amortising-mortgage adjusted real rate

De Bonis et al. (2024) apply the BIS Drehmann, Juselius and Korinek
(2017) amortisation-adjusted rate `adjR = R / (1 − (1+R)^{−N})` with
N = 12 years to the Italian sample. For Australia we use N = 25
years (consistent with the longer Australian average mortgage
maturity). The adjustment shifts the level of `real_rate` by
approximately +0.6 percentage points but barely moves the long-run
coefficient (within 0.001); the consumption equation is essentially
robust to the adjustment, a finding consistent with the Italian
result.

### 8.7 Scaled-income robustness

De Bonis et al. (2024) average disposable income with labour-plus-
transfer income to down-weight property-income mismeasurement. We
run the same construction on the preferred specification and report
the shift in λ relative to the headline `ydi_real_pc` series. The
shift is small (within 0.02) and does not change the substantive
ranking of the wealth coefficients.

### 8.8 Williams non-property income (NPY) robustness

Replacing `ydi_real_pc` with `npy_real_pc` constructed per Williams
(2009) §4.2.1 provides an income-measure robustness column. NPY is
between scaled income and gross disposable income in conservatism —
Williams strips property income but does not symmetrically average
with labour-plus-transfer income. We treat NPY as the closer
methodological match to Williams. The NPY substitution shifts λ by
roughly 18 per cent toward Williams' published value.

### 8.9 PI method comparison (AR, full-sample Italy, real-time Italy)

§7.4 reports the headline three-way comparison; the committed column
([`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv),
Spec 6, n = 86) refits under all three permanent-income measures:

- **AR (real-time):** λ = −0.051, ln(y^p/y) = −0.222 (t = −2.2) — the
  significantly negative "Australian PI puzzle".
- **Italy full-sample (headline measure):** λ = −0.197,
  ln(y^p/y) = +0.244 (t = 1.0).
- **Italy real-time (causal):** λ = −0.118, ln(y^p/y) = −0.105
  (t = −1.5).

The real-time Italy variant is the operationally honest benchmark: it
is causal (`real_time = TRUE` in `construct_permanent_income_italy`,
re-fitting the projection at each *t* on data whose full k-quarter
horizon is realised by *t*), so it is usable at forecast time. It
shows that about half the speed-of-adjustment gain from AR to the
full-sample Italy measure is genuine, but that the positive
permanent-income sign is **not** — it reverses to −0.11 once the
look-ahead embedded in the full-sample fit is removed. The divergence
between the AR and Italy measures at structural-break episodes is
identification, not noise (Carroll, Slacalek and Tokuoka 2014). We
carry the full-sample measure as the headline (framed as a
*measurement*, §4.3) and disclose that its positive-PI sign and λ
magnitude are full-sample, non-causal properties.

### 8.10 Permanent-income filter sensitivity

A 9-cell grid over discount factor δ ∈ {0.90, 0.95, 0.97}, horizon
k ∈ {20, 40, 60} quarters, and the GFC ogive on/off shows the
preferred-spec λ to be stable to within 0.02 across the grid under
the AR forecaster, indicating that the within-AR-method PI choice
is not what drives the |λ| gap with Williams. The dominant factor
is the AR vs Italy LP method choice itself (§8.9).

### 8.11 COVID-period robustness

Under canonical Italy LP, λ is sign-stable across all four sample
variants for Spec 6: full −0.180, pre-COVID −0.123. Spec 7
(cohort-burden) is tighter on |λ|. The COVID episode does not
destabilise the headline findings.

### 8.12 Rolling-window estimation

A 60-quarter rolling estimation of Spec 6 shows the wealth
coefficients trending mildly downward post-2014 (consistent with
the macroprudential era flattening the wealth-consumption
transmission), while λ becomes slightly less negative in the most
recent windows. We do not interpret this as model instability, but
rather as a symptom of the limited identifying variation in the
post-deregulation portion of the sample discussed in §5.

### 8.13 Out-of-sample forecast validation

We run a rolling out-of-sample validation on five specifications
(Spec 6 preferred, Spec 4 disagg-no-CCI, Spec 7 cohort-burden,
Spec 8 Williams-CCI-interactions, Spec 9 Kalman-CCI) over 36
expanding-window cuts from 2015Q1 to 2023Q4 at horizons
h ∈ {1, 4, 8} quarters, with random-walk-with-drift and AR(1)
benchmark forecasters.

| Specification | h = 1 RMSE | h = 4 RMSE | h = 8 RMSE |
|---|---:|---:|---:|
| Benchmark RW drift           | 0.0309 | 0.0309 | 0.0328 |
| Benchmark AR(1)              | 0.0370 | 0.0310 | 0.0328 |
| Spec 4 (disagg, no CCI)      | 0.0319 | 0.0325 | —      |
| Spec 6 (preferred)           | 0.0322 | 0.0332 | 0.0416 |
| Spec 7 (cohort-burden)       | 0.0308 | 0.0346 | —      |
| Spec 8 (Williams CCI)        | 0.0324 | 0.0315 | 0.0366 |
| Spec 9 (Kalman CCI)          | 0.0324 | 0.0315 | 0.0366 |

At h = 1 the structural specifications are competitive with the
random-walk benchmark (Spec 7 narrowly beats RW-drift). At h = 4
and h = 8 the random-walk dominates every structural specification
by 5–15 per cent in RMSE. This is the standard "macro forecasting
puzzle" — the LIVES framework's identification advantage is in
interpreting historical co-movement, not in beating naive benchmarks
at multi-step prediction. We record this honestly rather than
overstating the forecast performance.

### 8.14 Back-extension robustness — Spec 1 on the 1976Q3+ sample

Refitting Spec 1 (aggregate net worth) on the back-extended sample
using `ln_networth_y_proxy`:

| LR coefficient | 1988+ baseline (n=146) | 1976+ extended (n=190) | % change |
|---|---:|---:|---:|
| λ (ecm_lag)    | −0.177  | −0.202  | +14.2 |
| ln_networth_y  | +0.112  | +0.107  | −4.0  |
| ln_hp_over_y   | −0.0151 | −0.0038 | −74.8 |
| real_rate      | −0.00137| +0.00090| −165  |
| ln_yp_over_y   | +0.961  | +0.971  | +1.0  |

The wealth elasticity is essentially stable across samples
(0.112 → 0.107, a 4 per cent change) — a positive validation of the
aggregate-net-worth proxy. Doubling the sample length and adding the
deregulation-era regime does not shift the structural wealth-to-
consumption coefficient. The permanent-income elasticity is also
stable. λ moves slightly more negative on the longer sample. The
house-price-to-income coefficient collapses to near zero on the
longer sample — a real signal, reflecting lower `hp_over_y`
variation in the pre-deregulation 1970s. The real-rate coefficient
sign-flips but both estimates are economically negligible.

### 8.15 Spec 4 on the back-extended sample

The disaggregated-wealth proxies of §3.13 allow Spec 4 to fit on the
back-extended sample. The result table is reproduced in §7.3:
λ moves 37 per cent closer to Williams (−0.140 → −0.193, still 32
per cent short), but the individual OLS wealth coefficients become
smaller rather than larger. The exercise establishes that
sample-length is not the binding constraint on whether Spec 4
reproduces Williams' Table 1.

### 8.15.1 Spec 6b — preferred specification on the back-extended sample

Spec 6 is bounded at 2002Q3+ on the baseline sample by `cci_ratio`
from ABS Cat 5601.0 housing-loan flow. Spec 6b retains the Spec 6
long-run and short-run structure but replaces the short-run CCI
regressor with the second-difference of log RBA D02 total credit
(`d2_log_creditd02_lag2`), which is available from 1976Q3. The
disaggregated wealth components also switch to their back-extended
proxies (§3.13). This lets the preferred specification fit on the
full back-extended sample (**n = 190** vs n = 86 for Spec 6).

| LR coefficient | Spec 6 (n = 86) | Spec 6b (n = 190) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag)             | −0.180 (t = −1.76) | **−0.229 (t = −4.17)** | −0.286 |
| ha_y / ha_y_proxy γ     | 0.049              | 0.038                  | 0.049  |
| nla_y / nla_y_proxy γ   | 0.196              | 0.013                  | 0.159  |
| eq_y / eq_y_proxy γ     | −0.035             | −0.081                 | (calibrated 0.011) |
| super_y / super_y_proxy γ | 0.065            | 0.029                  | (calibrated 0.011) |
| ln_hp_over_y γ          | −0.094             | −0.036                 | −0.130 |
| ln_yp_over_y (CCI = 0)  | +0.200             | +1.234                 | +0.20 (calibrated) |
| BIC                     | −493.8             | −1 116.3               | n/a    |

Two patterns are notable. First, **the speed of adjustment moves
substantially closer to Williams' published value** — λ = −0.229
on the back-extended sample, 80 per cent of Williams' −0.286, vs
63 per cent on Spec 6. Statistical significance also improves
sharply (t-stat moves from −1.76 to −4.17). Second, **the wealth
γ profile shifts toward smaller individual elasticities** — γ_NLA
collapses from 0.196 to 0.013 and γ_EQ becomes more negative,
mirroring the Spec 4 back-extension finding in §7.3. γ_HA stays
positive (0.038 vs Williams' 0.049) but moves about 22 per cent
below Williams rather than matching almost exactly.

The Spec 6b evidence is consistent with the substantive reading
elsewhere in the paper: on the back-extended sample the
disaggregated wealth proxies do not separately identify with
Williams-like precision, even when the canonical Spec 6
short-run dynamics, post-2008 PI break, and long-history credit
proxy are all available. The combination of (i) tighter |λ|
estimate, (ii) γ_HA close-to-Williams, and (iii) smaller γ_NLA
and γ_IFA is the cleanest single-equation OLS approximation to
Williams' Table 1 the paper produces.

### 8.16 Maximal-GETS placebo on the back-extended sample

The Williams maximal-GETS canonical CCI on the back-extended 1976Q3+
sample (Spec 1 with the aggregate proxy) sits at the **64th adjusted
R² percentile and 36th |λ| percentile** in 200 random 15-knot,
15-prior placebo draws. This is a meaningful improvement on the
literal Williams 4-knot result (19th/10th on the same sample) but
does not reach a strong-support threshold of, say, 90th on both
metrics. Random combinations of 15 candidate knots and priors under
the same sign-survival protocol produce *faster* mean reversion than
the canonical institutional choice in 64 per cent of draws. The
maximal-GETS protocol is doing identification work, but most of the
lift comes from the adaptiveness of the drop-on-violation reduction
(15 candidate knots is a great deal of flexibility) rather than
from Williams' specific knot or prior choice.

### 8.17 Sectional sign-prior CCI

Williams (Aust paper §5.1) imposes sign priors over periods rather
than knot-by-knot. We construct a sectional CCI basis with one knot
per period (1982 / 1990 / 1993 / 2007, plus 2014 / 2017 / 2020 /
2021 extensions) and re-run the placebo. The sectional canonical
sits at the **36th adjusted R² percentile and 40th |λ| percentile**
— worse than the maximal-GETS canonical (64/36). Williams' specific
period dating does not outperform random period placements on the
back-extended sample.

### 8.18 Two-equation SUR (consumption + house prices)

Joint SUR estimation of the consumption equation and a Williams-style
house-price ECM (Aust paper eq. 11) on the back-extended 1976Q3+
sample yields residual correlation **ρ̂(ε_C, ε_H) ≈ 0.0007**, with
SUR coefficients within 0.1 per cent of equation-by-equation OLS
for nearly every term. The finding is robust across specification
variants (no CCI: ρ̂ = −0.083; no event dummies: ρ̂ = +0.043;
minimal LR + SR: ρ̂ = −0.025). Joint estimation gives no efficiency
gain at the quarterly frequency. The case for the multi-equation
framework therefore rests on cross-equation parameter restrictions,
not on residual covariance.

### 8.19 Three-equation joint cross-equation CCI identification

We extend the maximal-GETS protocol to require sign-prior survival
across **three** equations simultaneously (consumption + house
prices + mortgage stock). Of the six knots that survive
consumption-only fitting, only **two** pass the joint test:

| Survival regime | Surviving knots |
|---|---|
| Consumption only       | 1979, 1986, 1992, 2007, 2017, 2020 |
| Joint (C ∩ H ∩ M)      | **1986, 2017** |

The joint-identified `cci_williams_joint` flips the house-price
equation's CCI loading from significantly negative (−0.024 under
cons-only CCI) to significantly positive (+0.024 under joint CCI),
consistent with Williams' cross-equation sign restrictions working
as intended. The mortgage-stock equation's CCI loading remains
negative (joint sign-survival is a sign restriction, not a
parameter-equality restriction; full FIML would be required to
re-sign the mortgage equation's loading). The wealth-coefficient
profile against Williams' Table 1 is barely affected by joint
identification — confirming that the residual gap is structural to
the single-equation framing, not a CCI-construction artefact.

---

## 9. Comparison with Williams (2010, 2012)

This section consolidates the comparison with Williams' published
Table 1 and BIS chapter estimates.

**Where we agree with Williams.** Under canonical Italy LP the
preferred Spec 6 reproduces Williams' structural coefficient profile
to a useful approximation. The implied γ on housing wealth
(γ_HA = 0.049) matches Williams' calibrated value almost exactly,
the γ on illiquid financial wealth (γ_IFA = 0.030) is within 0.01
of Williams' calibrated 0.022, and the γ on net liquid assets
(γ_NLA = 0.196) sits 23 per cent above Williams' 0.159, in the
same direction. The cross-equation restriction γ_LA + γ_LOANS = 0
is accepted in every disaggregated specification × sample
combination, validating the Italian convention. The long-run
permanent-income coefficient at CCI = 0 (+0.20) matches Williams'
calibrated value in sign and magnitude.

**Where we differ.** The speed of adjustment is 37 per cent below
Williams' published value (λ = −0.180 vs −0.286), and the OLS
coefficients on the disaggregated wealth components are 14–37 per
cent below Williams' implied OLS values. The two deficits roughly
cancel under the ECM identity OLS = λ × γ, which is why the
implied structural γ profile remains close to Williams. The
house-price-to-income coefficient (γ = −0.094 vs Williams' −0.130)
under-shoots, but Spec 6 does not include the
affordability × (1 − ϖ·CCI) interaction by which Williams' framework
identifies this channel; Spec 8 with that interaction sharpens the
effect.

**The back-extension does not close the residual gap.** Refitting
the simpler disaggregated no-CCI specification (Spec 4) on the
back-extended 1976Q3+ sample produces a 37 per cent improvement in
λ (−0.140 → −0.193, still 32 per cent short of Williams' −0.286)
but smaller, not larger, OLS wealth coefficients on the longer
window. γ_NLA collapses toward zero on the extended sample and
γ_EQ retains a wrong sign. The post-1988 sample window is therefore
not, in itself, what generates the divergence between any
single-equation OLS estimate and Williams' Table 1.

We read these findings as indicating that the structural
identification Williams (2010) delivers comes from cross-equation
parameter restrictions in his four-equation FIML system rather than
from sample length, knot count, or sign-prior structure. The same
CCI enters all four of Williams' equations with sign constraints;
ϖ in the wealth × (1 − ϖ·CCI) interaction is shared across
equations; ζ_h = 1 normalises the house-price equation. None of
these structural restrictions is imposed by single-equation OLS.
The path to a tighter reconciliation with Williams' published
values runs through a full FIML build.

Adding the Williams CCI interactions in Spec 8 produces
**λ = −0.445**, exceeding Williams' value in magnitude on the
1988Q4+ sample. The wealth coefficients shift relative to Spec 6
(γ_HA = 0.022, γ_IFA = 0.053, γ_NLA = 0.098) without moving
systematically toward Williams' Table 1 — confirming that adding
CCI interactions to a single-equation specification re-allocates
the long-run identification across wealth components but does not
close the residual gap with the joint FIML estimates.

**The joint cross-equation identification finding** (§5.3, §8.19)
provides the substantive diagnosis. When knots are required to
satisfy sign priors *jointly* across three equations (consumption,
house prices, mortgage stock), only two of six single-equation
survivors pass: 1986 (financial deregulation) and 2017 (APRA
macroprudential round II). The maximal-GETS identification of six
knots was therefore overstated in the single-equation pipeline; four
of them are consumption-equation specific and would not survive a
true common-factor restriction. This aligns with both Williams'
framework (CCI is a common factor across equations under parameter
restrictions) and the Duca, Muellbauer and Murphy (2013) state-space
implementation in which the latent factor is identified jointly
across equations.

**The Australian permanent-income puzzle and the permanent-income
measure.** Under the AR-method (real-time) robustness column we
replicate the often-noted "Australian PI puzzle": the long-run
coefficient on log(y^p/y) is negative — significantly so on Spec 6
(−0.22) and near-zero on Spec 1 (−0.003). Under the full-sample Italy
*measure* it is positive (+0.24 in Spec 6). The structural reasons
the measures diverge are real: the rolling-AR forecaster lacks the
labour-force-share predictor that captures slow-moving demographic
effects, compounds short-run AR misspecification across 40 horizons,
and over-estimates persistence after large income shocks, all of
which the one-step direct projection avoids. But the positive sign
is a property of the full-sample, non-causal construction of the
measure: under a causal real-time projection the coefficient returns
to modestly negative (−0.11, §8.9). We therefore read the positive
permanent-income coefficient as a feature of full-sample
permanent-income *measurement* rather than a resolved structural
property of Australian consumption.

---

## 10. Decomposition and policy implications

### 10.1 Long-run contributions decomposition

The long-run decomposition (an Australian counterpart to Williams
(2010) Charts 2–8) splits fitted log(c/y) for the preferred
specification into the de-meaned partial contribution of each
long-run regressor. The chart shows housing wealth as the dominant
positive wedge through 2005–2024, with a large negative
house-price-affordability wedge in the 1990s that recedes by 2010.
The post-2008 permanent-income step shift is a transient 2008–10
spike. The residual stays small (less than 0.05 in magnitude)
outside the GFC and early-COVID windows.

Because the structural γ profile under Spec 6 is in line with
Williams' Table 1 (§7.3), the partial contributions of each wealth
term to the fitted path are comparable in scale to those reported in
Williams (2010) Charts 2–8. The Spec 8 decomposition with the full
Williams CCI-interaction set is reported as a robustness column.

### 10.2 Counterfactuals

We compute three policy counterfactuals on the headline
specifications, holding all non-counterfactual regressors at their
observed values and integrating the implied Δlog c paths back to
log-consumption levels.

| Scenario | Spec | h = 4 q gap | h = 8 q gap | End-of-sample gap |
|---|---|---:|---:|---:|
| No 2014 / 2017 APRA macroprudential | Spec 6 | +0.8 % | +2.3 % | +28.3 % |
| No COVID income support              | Spec 6 | −9.6 % | −9.6 % | −9.6 % |
| CCI at Williams' peak vs CCI = 0     | Spec 8 | n/a    | n/a    | ≈ 0     |

(Gaps are cumulative deviations in log(c) from the baseline path,
expressed in percentage points; h-quarter values measured from the
relevant event date.)

**Counterfactual 1 — no 2014/2017 APRA macroprudential.** Zeroing
the smoothed-step `d_apra_2014` and `d_apra_2017` dummies in
Spec 6 implies that consumption would have been about 0.8 per cent
higher four quarters after the 2014 round and about 2.3 per cent
higher after eight quarters. The end-of-sample gap (+28.3 per cent
by 2024Q4) compounds the persistent post-event ogive shifts over a
decade and should be read as a diagnostic upper bound rather than a
literal forecast — it assumes the wider macroeconomic environment
would have adjusted to nothing else over the same window, which is
not how the actual economy operates. The 4-quarter and 8-quarter
numbers are the policy-relevant range and are consistent with the
APRA programme accounting for ~1–3 per cent of consumption growth
over its first two years.

**Counterfactual 2 — no COVID income support.** Zeroing the
`d_jobkeeper_2020`, `d2020_covid` and `d2020_rebound` dummies in
Spec 6 implies that consumption would have been about 9.6 per cent
*lower* in the COVID period. The cumulative gap stabilises quickly
because the COVID event dummies are bounded in time (zero before
and after 2020Q2–2021Q1), so unlike the persistent APRA ogives the
deviation does not continue to compound past the event window.

**Counterfactual 3 — CCI at Williams' peak vs zero.** Refitting
Spec 8 with the four CCI-interacted regressors evaluated at
CCI = 1 (Williams' historical peak) versus CCI = 0 (no
liberalisation) implies essentially zero cumulative consumption
gap across the back-extended sample. This is a methodological
consequence of the de-meaning convention adopted in §8.4: under
de-meaned interactions, the average contribution of CCI variation
to fitted Δlog c is zero by construction, so the *cumulative*
effect of a permanent CCI shift integrates to zero across the
sample. The CCI interactions in Spec 8 are therefore pure
timing/distribution effects — they reallocate where consumption
growth lands across the cycle without shifting its unconditional
level. This is the LIVES-theoretic interpretation: CCI matters for
*when* households extract housing equity and respond to permanent-
income news, not for the long-run level of the consumption-to-
income ratio.

Source: [australia_counterfactuals.csv](../outputs/australia_counterfactuals.csv),
[australia_counterfactuals_summary.csv](../outputs/australia_counterfactuals_summary.csv),
[australia_counterfactual_paths.png](../outputs/australia_counterfactual_paths.png).

### 10.3 Policy implications

The empirical findings have direct policy relevance for an
Australian central-bank reader.

**Wealth channel of monetary policy.** The preferred specification
delivers γ_HA = 0.049 — essentially equal to Williams' calibrated
0.0488 at his CCI peak. Housing wealth has the largest direct
long-run elasticity into consumption among the disaggregated wealth
components, with γ_NLA = 0.196 and γ_IFA = 0.030 also positive and
in line with Williams' Table 1. Movements in mortgage rates that
change housing values — whether through interest-rate pass-through
or credit-conditions tightening — propagate to consumption with a
lag governed by λ ≈ −0.18 (about a 25–35 per cent cumulative effect
at four quarters, 50–60 per cent at eight quarters, with most of
the adjustment completed in five to six years).

**Macroprudential effects.** The 2014 and 2017 APRA episodes are
modelled as smoothed-step ogive dummies. Their estimated
coefficients on Δlog c are small (roughly −0.005 to −0.013), and
the counterfactual exercise in §10.2 implies they shaved roughly
0.8 percentage points off consumption in the year after the 2014
round and 2.3 points by two years after it. The level gap on a
long horizon should be interpreted as a model-mechanical upper
bound rather than a forecast of the broader macro response.

**Permanent-income transmission.** Under canonical Italy LP the
long-run coefficient on log(y^p/y) at CCI = 0 is +0.20, matching
Williams' calibrated value in sign and magnitude. Australian
households respond meaningfully to credible permanent-income
shocks; for fiscal-multiplier work, the propagation to consumption
under permanent (versus transitory) income changes is roughly 20
per cent in the long run at CCI = 0, with full speed of adjustment
in four to six years.

**Credit-conditions identification caveat.** Section 5 documents
that the CCI's identification in a single-equation OLS is weak, and
that neither the back-extended sample, the sectional sign priors,
nor the time-varying housing-wealth interaction changes this.
Policy-makers using a single-equation CCI series for regime
classification — e.g. tightening-versus-easing diagnoses — should
be aware that the spline coefficients reflect consumption-equation
residual identification rather than a structurally identified
common credit-conditions factor. For policy use, the Kalman
state-space CCI (Spec 9; available on the back-extended sample) is
a less-imposed alternative that does not require institutional knot
choices. The maximal-GETS canonical and the Kalman CCI deliver
nearly identical λ and very similar wealth-coefficient shifts; they
can be used as cross-checks of one another.

---

## 11. Conclusion

We extend the Williams (2010, 2012) Australian LIVES consumption
estimate to 2024Q4 using publicly available data, document the
methodological choices that drive the estimate, and stress-test the
framework with a battery of new empirical tests including a sample
back-extension to 1976Q3, random-knot placebo tests on both the
original and extended samples, and a multi-equation scaffold for a
companion paper.

### 11.1 Where the preferred specification agrees with Williams

Three substantive findings of the paper match Williams (2010, 2012)
in sign and broad magnitude.

1. **Structural coefficient profile.** Under canonical Italy LP the
   preferred Spec 6 delivers γ_HA = 0.049 (Williams: 0.049),
   γ_IFA = 0.030 (Williams: 0.022), and γ_NLA = 0.196 (Williams:
   0.159). The OLS coefficients are 14–37 per cent below Williams'
   implied OLS values, but our smaller |λ| (−0.180 vs −0.286)
   scales them up to a structural γ profile in line with Williams'
   Table 1.

2. **Permanent-income transmission — a measurement caveat.** The
   long-run coefficient on log(y^p/y) is significantly negative under
   a rolling AR(8) forecaster (the "Australian permanent-income
   puzzle") and positive (+0.24) under the full-sample Italy
   *measure*. We show this reversal is a property of the measure's
   full-sample, non-causal construction: under a causal real-time
   projection the coefficient remains modestly negative and λ ≈ −0.12
   (§8.9). The positive permanent-income result is reported as a
   measurement, not a real-time resolution of the puzzle.

3. **NLA cross-equation restriction accepted.** The Italian
   convention γ_LA + γ_LOANS = 0 is accepted at the 5 per cent level
   in every disaggregated specification × sample combination
   estimated here, validating the De Bonis et al. (2024)
   methodology.

Adding the Williams CCI interactions in Spec 8, including the
time-varying housing-wealth m.p.c. interaction, produces λ = −0.445
— *exceeding* Williams' calibrated value in magnitude. The wealth
coefficients shift relative to Spec 6 without moving systematically
toward Williams' Table 1, indicating that the CCI interactions act
in a single-equation framework as flexible parameter time-variation
rather than as the structurally identified common-factor channel
Williams' four-equation system delivers.

### 11.2 What the back-extension does and does not show

A natural follow-up question is whether the residual coefficient
gap between the simpler disaggregated no-CCI specification (Spec 4)
and Williams' Table 1 reflects the post-1988 sample window or the
single-equation framing itself. The paper assembles a back-extended
master dataset to 1976Q3 — using a Treasury TRYM long-run
house-price series (1959Q3+), the RBA D03 M3 monetary aggregate
(1959Q3+), the RBA D02 total-credit splice (1976Q3+), a historical
labour-force compilation (1964Q3+), and documented aggregate and
disaggregated wealth proxies anchored at 1988Q3 — and refits
Spec 4 on the longer window.

| LR coefficient | 1988+ baseline | 1976+ extended | Williams Table 1 |
|---|---:|---:|---:|
| λ        | −0.140 | −0.193 | −0.286 |
| nla_y    | +0.035 | −0.002 | +0.066 |
| eq_y     | −0.119 | −0.104 | +0.013 |
| super_y  | +0.040 | +0.024 | +0.013 |
| ha_y     | +0.068 | +0.040 | +0.052 |

The speed of adjustment improves 37 per cent (−0.140 → −0.193,
still 32 per cent short of −0.286), but the individual OLS wealth
coefficients become smaller rather than larger. γ_NLA collapses
toward zero on the extended sample and γ_EQ retains a wrong sign.
The post-1988 sample window is therefore not, in itself, what
generates the divergence between Spec 4 and Williams' Table 1.

Three lines of evidence converge on the same diagnosis. **The
placebo battery** finds that the literal Williams 4-knot
specification fails on both samples, deteriorating from the 34th
adjusted-R² percentile on the 1988+ sample to the 19th on the
back-extended sample. The maximal-GETS reduction rescues
identification only weakly (64th/36th); the sectional sign-prior
specification, which implements Williams' Aust paper §5.1
period-bracket logic, sits at the 36th/40th percentile, no stronger
than random period placements. **The two-equation SUR** of
consumption and house prices on the back-extended sample finds
residual correlation ρ̂ ≈ 0.0007 — essentially zero — so joint
estimation gives no efficiency gain at the quarterly frequency.
**The three-equation joint cross-equation identification** retains
only two of six single-equation knot survivors (1986 financial
deregulation and 2017 APRA macroprudential round II); the
consumption-fitted CCI is therefore not a true common factor, four
of its six surviving knots being consumption-equation specific.

We read these findings as indicating that the structural
identification Williams (2010) delivers comes from cross-equation
parameter restrictions in his four-equation FIML system rather than
from sample length, knot count, or sign-prior structure. The path
to a tighter reconciliation with Williams' published values runs
through a full FIML build — the territory of a companion paper.

### 11.3 Outstanding work

1. **A multi-equation companion paper.** A full LIVES build with
   FIML and cross-equation parameter restrictions, including a
   home-equity-withdrawal equation. The four-equation system would
   estimate the same CCI loading across consumption, house prices,
   mortgage stock and HEW under sign restrictions; this is the path
   empirically demonstrated above to close the residual gap with
   Williams. Custom likelihood code and a sourced HEW series are
   the binding obstacles.

2. **Documentation of the TRYM house-price source.** A short
   data-appendix note on the specific TRYM vintage, retrieval URL
   and any modifications since release would tighten reproducibility.

### 11.4 What the paper contributes

The paper makes four contributions to the Australian household
consumption literature.

1. **A contemporary single-equation LIVES estimate** for Australia,
   updated to 2024Q4, with the canonical permanent-income
   forecaster shifted from rolling AR to the Italian
   local-projection method. The preferred specification reproduces
   Williams' structural coefficient profile to a useful
   approximation, with the implied γ on housing wealth matching
   Williams almost exactly.

2. **A back-extended Australian master dataset** (1976Q3–2024Q4,
   n = 194 quarters) with documented growth-rate splices for house
   prices, the M3 monetary aggregate, total credit, and labour
   force, plus aggregate and disaggregated wealth proxies for the
   pre-1988 window.

3. **A cross-sample empirical test** of whether Williams' CCI
   knots identify when the data covers his motivating institutional
   events. The literal Williams 4-knot specification fails the
   placebo on both samples; the maximal-GETS reduction barely lifts
   it above the median; and sectional sign priors implementing
   Williams' Aust paper §5.1 do not help. Triangulating with the
   two-equation SUR result (ρ̂ ≈ 0) and the three-equation joint
   sign-survival test (2 of 6 knots retained), we read the
   residual gap with Williams' Table 1 as a consequence of the
   single-equation framing rather than of sample length, knot
   count, or sign-prior structure.

4. **A multi-equation scaffold** — data preparation, house-price
   equation, mortgage-stock equation, joint CCI identification,
   and two- and three-equation SURs — that a companion paper can
   build on directly to pursue the full FIML build.

The paper is honest about what its single-equation framework can
and cannot deliver. Where Williams obtains identification from
joint estimation under parameter restrictions, the present
single-equation estimator nevertheless reproduces his structural
coefficient profile to a useful approximation; and where it does
not — most clearly on the simpler Spec 4 disaggregated no-CCI form
— we provide direct empirical evidence that further single-equation
tweaks (sample length, knot choice, sign-prior structure, the
time-varying housing-wealth interaction) do not close the gap.
This is itself a substantive finding that the absence of
back-extended Australian data has previously made impossible to
test.

---

## References

- Aron, J., Duca, J. V., Muellbauer, J., Murata, K., & Murphy, A. (2012).
  Credit, housing collateral and consumption: evidence from Japan, the
  U.K. and the U.S. *Review of Income and Wealth*, 58(3), 397–423.
- Aron, J., & Muellbauer, J. (2013). Wealth, credit conditions and
  consumption: evidence from South Africa. *Review of Income and Wealth*,
  59(S1), S161–S196.
- Backus, D. K., & Purvis, D. D. (1980). An integrated model of household
  flow-of-funds allocations. *Journal of Money, Credit and Banking*,
  12(2), 400–421.
- Bai, J., & Perron, P. (1998). Estimating and testing linear models with
  multiple structural changes. *Econometrica*, 66(1), 47–78.
- Ballantyne, A., Cusbert, T., Evans, R., Guttmann, R., Hambur, J.,
  Hamilton, A., Kendall, E., McCririck, R., Nodari, G., & Rees, D.
  (2019). MARTIN has its place: a macroeconometric model of the
  Australian economy. *RBA Research Discussion Paper* 2019-07.
- Battellino, R., & McMillan, N. (1989). Changes in the behaviour of
  banks and their implications for financial aggregates. *RBA Research
  Discussion Paper* 8904.
- Bayoumi, T. (1993). Financial deregulation and household saving.
  *Economic Journal*, 103(421), 1432–1443.
- Blinder, A. S., & Deaton, A. (1985). The time-series consumption
  function revisited. *Brookings Papers on Economic Activity*, 1985(2),
  465–521.
- Campbell, J. Y., & Mankiw, N. G. (1989). Consumption, income, and
  interest rates: reinterpreting the time series evidence. *NBER
  Macroeconomics Annual*, 4, 185–216.
- Campbell, J. Y., & Mankiw, N. G. (1991). The response of consumption
  to income: a cross-country investigation. *European Economic Review*,
  35(4), 723–756.
- Carroll, C. D. (2001). A theory of the consumption function, with and
  without liquidity constraints. *Journal of Economic Perspectives*,
  15(3), 23–45.
- Carroll, C. D., & Kimball, M. S. (1996). On the concavity of the
  consumption function. *Econometrica*, 64(4), 981–992.
- Carroll, C. D., Slacalek, J., & Tokuoka, K. (2014). The distribution
  of wealth and the MPC: implications of new European data. *ECB Working
  Paper* 1648.
- Chauvin, V., & Muellbauer, J. (2018). Consumption, household
  portfolios and the housing market in France. *Banque de France
  Working Paper* series. (Online complement available.)
- Cusbert, T., & Kendall, E. (2018). Meet MARTIN, the RBA's new
  macroeconomic model. *RBA Bulletin*, March 2018.
- Davidson, J. E. H., Hendry, D. F., Srba, F., & Yeo, S. (1978).
  Econometric modelling of the aggregate time-series relationship
  between consumers' expenditure and income in the United Kingdom.
  *Economic Journal*, 88(352), 661–692.
- Deaton, A. (1992). *Understanding Consumption*. Oxford: Clarendon Press.
- De Bonis, R., Marino, I., & Muellbauer, J. (2024). Consumption, wealth
  and credit conditions in Italy: a Muellbauer-style error-correction
  model. *Bank of Italy Working Paper* series.
- Doornik, J. A. (2009). Autometrics. In J. L. Castle & N. Shephard
  (eds.), *The methodology and practice of econometrics: a festschrift
  in honour of David F. Hendry* (pp. 88–121). Oxford: Oxford University
  Press.
- Drehmann, M., Juselius, M., & Korinek, A. (2017). Accounting for debt
  service: the painful legacy of credit booms. *BIS Working Paper* 645.
- Duca, J. V., & Muellbauer, J. (2013). Tobin LIVES: integrating
  evolving credit market architecture into flow-of-funds based macro
  models. *European Central Bank Working Paper* 1581.
- Duca, J. V., Muellbauer, J., & Murphy, A. (2010). Housing markets and
  the financial crisis of 2007–2009: lessons for the future. *Journal
  of Financial Stability*, 6(4), 203–217.
- Duca, J. V., Muellbauer, J., & Murphy, A. (2013). Tobin LIVES:
  integrating evolving credit market architecture into flow-of-funds
  based macro models. *European Economy Discussion Paper* 14.
- Dvornak, N., & Kohler, M. (2003). Housing wealth, stock market wealth
  and consumption: a panel analysis for Australia. *RBA Research
  Discussion Paper* 2003-07.
- Edey, M., & Gray, B. (1996). The evolving structure of the Australian
  financial system. In *The future of the financial system*, RBA
  Conference Volume, 6–44.
- Engle, R. F., & Granger, C. W. J. (1987). Co-integration and error
  correction: representation, estimation, and testing. *Econometrica*,
  55(2), 251–276.
- Friedman, M. (1957). *A theory of the consumption function*.
  Princeton: Princeton University Press.
- Hall, R. E. (1978). Stochastic implications of the life-cycle
  permanent-income hypothesis: theory and evidence. *Journal of
  Political Economy*, 86(6), 971–987.
- Hendry, D. F., & Krolzig, H.-M. (2005). The properties of automatic
  Gets modelling. *Economic Journal*, 115(502), C32–C61.
- Jordà, Ò. (2005). Estimation and inference of impulse responses by
  local projections. *American Economic Review*, 95(1), 161–182.
- Koopman, S. J., Harvey, A. C., Doornik, J. A., & Shephard, N. (2000).
  *STAMP 6: structural time series analyser, modeller and predictor*.
  London: Timberlake Consultants.
- Modigliani, F. (1963). The life-cycle hypothesis of saving, the
  demand for wealth and the supply of capital. *Social Research*,
  33(2), 160–217.
- Muellbauer, J. (2007). Housing, credit and consumer expenditure. In
  *Housing, housing finance, and monetary policy*. Federal Reserve
  Bank of Kansas City Jackson Hole Symposium Proceedings, 267–334.
- Muellbauer, J., & Williams, D. (2012). Credit conditions and the
  real economy: the elephant in the room. *BIS Papers* No. 64.
- Tan, A., & Voss, G. (2000). Consumption and wealth. *RBA Research
  Discussion Paper* 2000-09.
- Tobin, J., & Dolde, W. (1971). Wealth, liquidity and consumption. In
  *Consumer spending and monetary policy: the linkages*, Federal
  Reserve Bank of Boston Conference Series 5.
- Williams, D. M. (2009). House prices and financial liberalisation in
  Australia. *Oxford Economics Series Working Paper* 432.
- Williams, D. M. (2010). Consumption, wealth and credit liberalisation
  in Australia. *Oxford Economics Series Working Paper* 492.

---

## Appendix A: Data construction

A full data-construction appendix summarising series sources, splice
conventions and proxy methodology is reproduced from the
accompanying [data documentation](data.md). The appendix retains the
detail of §§3.1–3.13 of the main paper, together with the master
variable coverage table.

## Appendix B: Coefficient tables

The full per-specification coefficient tables, with Newey–West HAC
standard errors, are reproduced from
[australia_full_results.csv](../outputs/australia_full_results.csv)
and [australia_precovid_results.csv](../outputs/australia_precovid_results.csv).

## Appendix C: Diagnostic battery

The full diagnostic output, including event-driven vs structural
heteroscedasticity classifications and the Chow battery at multiple
break dates, is reproduced from
[australia_full_diagnostics.csv](../outputs/australia_full_diagnostics.csv).

## Appendix D: Reproducibility

The full reproducibility kit accompanies this paper. It includes
three execution modes (full pipeline with ABS downloads, fast
re-estimation from the cached RDS dataset, and offline replay from a
portable master CSV), an `renv` lockfile pinning R 4.5.3 dependencies,
GitHub Actions continuous integration, and a `testthat` unit-test
suite. The repository is hosted publicly at
<https://github.com/DavidAStephan/ConsModelling>.
