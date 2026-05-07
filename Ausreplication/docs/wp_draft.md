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
> Drehmann effective-rate adjustments, a Williams-style smoothed-step
> credit-conditions spline with a maximal-GETS knot-identification
> reduction and a Kalman state-space alternative, and rolling
> out-of-sample forecast validation. Under canonical Italy LP, the
> speed of adjustment is within 25 per cent of Williams' published value
> (λ = −0.218 vs −0.286), but the implied long-run γ on individual
> wealth terms is roughly a quarter of Williams' — a finding we attribute
> to truncated CCI variation on the post-deregulation Australian sample.
> The full reproducibility kit
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
1. Update Williams' Australia LIVES estimate to 2024Q4 using publicly
   available data (with three additional sourced series: RBA F6
   mortgage rate, ABS A84423091W 15+ population, ABS 5206020 income
   components) and the Williams (2009) §4.2.1 non-property income
   recipe.
2. Show that the speed of adjustment matches Williams' published value
   to within 25 per cent under a Jordà (2005) local-projection
   permanent-income forecaster with a labour-force-share predictor,
   while the rolling AR(8) forecaster commonly used in the literature
   produces an order-of-magnitude smaller |λ| and a wrong-signed
   long-run permanent-income coefficient. We treat the latter as a
   methodology artefact rather than a structural Australian feature.
3. Provide a structured robustness suite mirroring the De Bonis et al.
   (2024) Italian methodology — IV, joint SUR, Chow battery, Drehmann
   effective-rate, scaled-income, Williams-style smoothed-step CCI
   (with a maximal-GETS knot-identification reduction; a Kalman
   state-space alternative; a placebo test against random knot draws),
   PI method comparison, rolling-window estimation, and rolling
   out-of-sample forecast validation — applied to a single-equation
   OLS framework with full code release.

**§1.3 Headline result.**
Under the canonical Italy local-projection PI forecaster, the
preferred Spec 6 delivers a speed of adjustment of −0.218 (vs
Williams' published −0.286) on a non-overlapping sample
(1988Q4–2024Q4 vs Williams' 1978Q1–2008Q2). The implied long-run γ on
individual wealth terms is roughly a quarter of Williams' values — a
finding we attribute to truncated CCI variation on the
post-deregulation Australian sample (only one of Williams' four
canonical CCI knots survives a maximal-GETS reduction on our window;
see §5). The cross-equation restriction γ_LA + γ_LOANS = 0 is accepted
in every disaggregated specification × sample combination, validating
the Italian convention. The often-noted "Australian permanent-income
puzzle" — a significantly negative long-run coefficient on log(y^p/y)
under the rolling AR(8) forecaster — flips to positive (+0.30) under
canonical Italy LP, matching Williams' calibrated value (0.20) in sign
and broad magnitude.

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

The paper does not extend to the multi-equation LIVES system. Williams
(2010) estimates four equations jointly by FIML; we estimate the
consumption equation alone. Italy's experience (De Bonis et al. 2024,
§4.2) suggests that single-equation OLS produces consumption-equation
coefficients "only a whisker away" from joint SUR estimation, and we
replicate that finding (§8.2). The full multi-equation system extension,
which would estimate house-price, mortgage-stock and home-equity-
withdrawal equations jointly with consumption and identify the credit-
conditions spline as a common factor across equations, is left for a
companion paper.

The paper also does not — at present — extend the sample back to
~1975Q1, which would be required for full identification of the
Williams CCI 1979 deregulation knot. The binding obstacle is access to
the unpublished RBA pre-1988 housing wealth and total financial assets
series Williams used. We have requested these data and will report
back-extended results in a companion paper.



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
resulting λ (−0.121 in Spec 8) is materially closer to Williams'
published −0.286 than the canonical 4-knot replication delivers
(−0.076 in Williams-2010 spec on our sample). Williams' canonical
4-knot setup is retained as a robustness benchmark in
[`model_helpers.R`](../R/model_helpers.R) via
`build_williams_cci_basis_canonical()`.

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

Spec 6 over the full sample 1988Q4–2024Q4 (n=86, after lag-truncation)
delivers:

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
**undershoot Williams' wealth γ by roughly a factor of four**. Two
candidate reasons: (a) the Australian sample (1988Q4–2024Q4) is
post-financial-deregulation and post-mandatory-superannuation, so the
historical level of credit conditions across our sample is uniformly
high (the placebo test in `australia_williams_knot_placebo.png` shows
Williams' 1979/1992/1998 knots are all aliased on this sample), which
reduces the cross-sectional variation that identifies γ in the LIVES
framework; (b) the absence of the CCI interactions in Spec 6 means the
preferred spec averages low-CCI and high-CCI regimes and finds a small
average effect. Spec 8 with the Williams CCI interactions partially
recovers this — wealth coefficients shift by 150 per cent on average
(see §8.4 and `australia_cci_fit_decomposition.md`).

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
vs Williams' 0.0488; `nla_y` γ = 0.040 vs Williams' 0.159). Two
candidate reasons: (a) the Australian sample (1988Q4–2024Q4) is post-
financial-deregulation and post-mandatory-superannuation, which
truncates the effective CCI variation needed to identify γ in the
LIVES framework — the placebo test in `australia_williams_knot_placebo.png`
shows Williams' 1979/1992/1998 knots are aliased on this sample;
(b) Spec 6 averages low-CCI and high-CCI regimes without the CCI
interactions. Adding the Williams CCI interactions in Spec 8 shifts
the wealth coefficients by 150 per cent on average (see §8.4 and
`australia_cci_fit_decomposition.md`); under Spec 8 the implied γ on
`ha_y` rises to 0.019 and on `nla_y` to 0.078, narrowing the gap with
Williams in the conditioning direction predicted by the LIVES theory.

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
We attribute this to truncated CCI variation on the post-deregulation
sample rather than a structural difference in the wealth-consumption
transmission; the Spec 8 (Williams CCI interactions) decomposition is
the appropriate counterpart for the Williams 2010 framing, and we
report it as a robustness column. The residual stays small (less than
0.05 in magnitude) outside the GFC and early-COVID windows.

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
estimate to 2024Q4 using publicly available data. Under the canonical
Italy local-projection permanent-income forecaster (Jordà 2005, with
labour-force-share predictor), the speed of adjustment falls within 25
per cent of Williams' published value (λ = −0.218 vs −0.286), the
often-noted "Australian permanent-income puzzle" disappears (the
long-run coefficient on log(y^p/y) flips from −0.20 under AR to +0.30
under Italy LP, matching Williams' calibrated value in sign and broad
magnitude), and the NLA cross-equation restriction γ_LA + γ_LOANS = 0
is accepted in every disaggregated specification × sample combination
— validating the Italian convention of De Bonis et al. (2024). Where
we *under*shoot Williams is in the implied long-run γ on individual
wealth terms (roughly a quarter of Williams' values); we attribute
this to truncated CCI variation on the post-deregulation Australian
sample, which a sample back-extension to ~1975 would resolve.
Substantively, the result validates the LIVES framework on extended
Australian data and provides a contemporary benchmark for the wealth,
credit and permanent-income channels of consumption transmission.

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
