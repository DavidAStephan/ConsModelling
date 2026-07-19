# Australian Household Consumption, Wealth and Credit Conditions: An Updated Single-Equation LIVES Estimate

**Working-paper draft — target venue: Reserve Bank of Australia Research Discussion Paper**

**Author:** David Stephan
**JEL codes:** E21, E32, E51, D14
**Keywords:** household consumption, wealth effects, credit conditions, error-correction model, LIVES system

---

## Abstract

We estimate a single-equation Muellbauer–Williams LIVES consumption
model for Australia over 1988Q3–2024Q4, extending Williams (2010) and
Muellbauer and Williams (2012) by sixteen years of post-GFC data and
applying contemporary identification methods. Wealth is disaggregated
into housing, illiquid financial (equities plus superannuation), and
net liquid assets (deposits net of total household debt), each entered
as an asset/annualised-income ratio, and we adopt the direct
(single-regression) forecast of the discounted future-income aggregate
used in the Italian implementation of De Bonis, Liberati, Muellbauer
and Rondinelli (2020). The cross-equation restriction
γ_LA + γ_LOANS = 0 is accepted in every disaggregated specification
and sample window we estimate, validating the net-liquid aggregation
used in that Italian convention.

Our central methodological finding is that the *form* of the equation
is decisive. A conventional disaggregated error-correction model that
enters wealth as constant-marginal-propensity levels — the
specification most of this literature, and an earlier draft of this
paper, treated as the LIVES equation — is not in fact the LIVES
equation. In Williams' framework credit conditions *scale* the
housing-wealth effect: there is no classical housing wealth effect,
and the marginal propensity is unlocked only as the credit-conditions
index (CCI) rises. Estimating a standalone housing-wealth coefficient
therefore tests a parameter the theory predicts to be approximately
zero; reading its insignificance as a failed housing-wealth effect is
a category error. When the equation is instead specified faithfully —
housing wealth entering only through its CCI interaction, the
autonomous-consumption CCI intercept restored, and illiquid financial
assets combined — the error-correction and core wealth structure come
alive (the faithful LIVES specification, Spec 11), estimated on
n = 146 rather than the n = 86 to which the conventional credit term
binds the model. We anchor the headline on the COVID-controlled
estimates: the speed of adjustment is λ ≈ −0.25 and tightly clustered
across the pre-COVID, COVID-excluded and COVID-rich-dummy variants
(−0.266, −0.248 and −0.242, with t-ratios of −4.8 to −6.7;
australia_spec11_variants.csv), within about 13 per cent of Williams'
published −0.286; the full-sample λ = −0.448 (t = −3.6) is inflated by
the COVID quarters and we do not headline it. The credit-invariant
wealth structure is right-signed and significant at 5 per cent in the
full-sample and COVID-dummy variants, weakening to 10 per cent in the
pre-COVID subsample: the structural net-liquid MPC is 0.060 (95 per
cent CI [0.022, 0.098]) and the illiquid financial MPC 0.035
([0.012, 0.057]), the latter spanning Williams' calibrated 0.022. The
estimates are now precise enough to *reject* parts of Williams'
profile rather than merely fail to distinguish them: his net-liquid
MPC of 0.159 and his peak housing-collateral MPC of 0.0488 lie outside
our intervals, while the housing-collateral channel itself is
right-signed but insignificant (structural 0.0055, CI [−0.010,
0.021]). The permanent-income response is strong (OLS +0.30 to +0.46,
t ≈ 4–6), but its structural value of 1.0–1.1 exceeds the
theoretical admissibility bound ψ ≤ 1 − η ≈ 0.95, a tension we
disclose and show is not an artefact of the GFC learning-weight
applied to the series. The conventional constant-MPC specification
(Spec 6) delivers λ = −0.239 (t = −2.6) on a sample less than
two-thirds the length. Much of the apparent weakness of
single-equation LIVES estimates for Australia is thus a specification
artefact, not an economic result. (Permanent income is the full-sample
Italian direct-forecast measure; its non-causal construction and a
causal real-time variant — λ = −0.159, with the permanent-income
coefficient reversing sign — are discussed in §7.4.)

The LIVES *structure* transfers to Australia, but Williams' Australian
*calibrations* do not. Imposing his calibrated permanent-income
gearing (ψ₀ = 0.20, ψ₁ = 0.93) collapses the error-correction to
λ = −0.030 (t = −0.7; the Williams-calibration-imposed specification,
Spec 12, which flips sign pre-COVID), independently reproduced by
Spec 10 (λ = −0.048): the Australian data freely estimate a
permanent-income gearing several times Williams', and forcing his
smaller value destroys the equilibrium. This reconciles a puzzle in
our companion work, where a Wald test fails to reject Williams' joint
calibration (χ²(6) = 7.55, p = 0.27) — the free estimates are too
imprecise to reject his values jointly, yet imposing them still ruins
the fit; low power is not the same as good fit. The credit-conditions
*interactions* are themselves only weakly identified: the CCI-scaled
regressors are 0.66–0.97 mutually correlated in absolute value on this
sample (australia_cci_interaction_corr.csv) because each is
approximately proportional to the latent index, so they cannot be
separately estimated from a single equation. This is exactly why
Williams calibrates and estimates a four-equation FIML system; on
contemporary Australian data the single-equation calibration shortcut
is empirically closed, leaving joint estimation and pre-1988
back-extension as the only routes to sharpen the credit channels.

The deployed CCI itself is disclosed in full rather than implied: only
four of fifteen candidate knots survive the sign-prior reduction, all
post-2007 (2007Q3, 2009Q1, 2019Q1, 2020Q2), so the index is identically
zero from 1988 to mid-2007, plateaus at its peak over 2010–2018, and
turns negative after 2019 (range −2.1 to 1;
australia_cci_williams_path.png). The credit channels are therefore
identified off roughly seventy post-2007 quarters, not the full
n = 146, and the housing-collateral MPC implied by γ₁·CCI is negative
in the post-2019 regime — a property Williams' [0, 0.8] index cannot
produce and a caveat on any structural reading of the interactions.
Placebo evidence is split and reported both ways: the literal Williams
4-knot spline sits at the placebo median (45th adjusted-R² percentile
over 200 random-knot draws), but the *deployed* construction — the
maximal candidate set under the same iterated sign-prior reduction used
in estimation — beats 84 per cent of random draws
(australia_williams_knot_placebo_deployed_verdict.csv), moderate
support for institutional knot placement. A two-equation SUR of
consumption and house prices delivers negligible cross-equation
residual correlation (ρ̂ = −0.013) and essentially no efficiency gain,
and a joint cross-equation CCI identification retains only one knot
across all four equations. We read these findings as indicating that
the structural identification Williams (2010) delivers comes from
cross-equation parameter restrictions in his four-equation FIML system
rather than from sample length, knot count, or sign-prior structure.

We assemble a back-extended master dataset to 1976Q3 — using a TRYM
long-run house-price series, RBA D03 monetary aggregates, RBA D02
total credit, historical labour-force compilations, and documented
aggregate and disaggregated wealth proxies anchored at 1988Q3, when
ABS sectoral balance-sheet data begin — to test whether sample length
is the binding constraint on tighter agreement with Williams.
Refitting the disaggregated no-CCI specification (Spec 4) on the
back-extended sample moves λ about 12 per cent toward Williams
(−0.182 → −0.203) but the net-liquid coefficient collapses toward
zero — sample length is not the binding constraint. The remaining
honest negatives are reported as substantive findings: the structural
specifications beat a random walk with drift out of sample at the
one-quarter horizon but lose at four and eight quarters; the automated
selection screen (which caps |λ| at 0.30) prefers a net-worth
specification while the Bayesian information criterion and LIVES
theory now both point to Spec 11; no specification passes the
Engle–Granger cointegration screen at MacKinnon critical values; and
the full-sample permanent-income measure partially reverses under a
causal real-time forecaster. The paper includes a structured
robustness suite covering instrumental variables with first-stage
diagnostics, joint SUR estimation, Chow tests at multiple break dates,
the Drehmann (2017) amortising-mortgage adjustment, the Williams
smoothed-step credit-conditions spline with maximal-GETS reduction, a
Kalman state-space credit factor, sectional sign-prior alternatives,
rolling-window estimation, out-of-sample forecast validation against
random-walk and AR(1) benchmarks, and back-extension robustness on
Spec 1 and Spec 4 — with the headline Spec 11 included in every
exercise. A full reproducibility kit accompanies the paper, along with
a multi-equation LIVES scaffold that lays the foundations for a
companion paper.

---


## 1. Introduction

### 1.1 Motivation

Australian household consumption poses a set of policy and analytical
questions that the standard New Keynesian DSGE workhorse — with its
representative consumer, exogenous wealth process, and assumed-frictionless
credit access — is structurally ill-equipped to answer. Among them: how
much of the post-2008 moderation in consumption growth is attributable to
the macroprudential tightening of 2014 and 2017, and how much to post-GFC
household balance-sheet repair? How sensitive is consumption to housing
wealth at different points in the credit cycle, when households are or are
not able to extract home equity? Did the COVID shock and JobKeeper income
support move permanent-income expectations, or only the short-run quarter?
And how should a central bank think about the wealth channel of monetary
policy when much of household wealth is housing wealth, mortgage debt is at
near-historic levels relative to income, and the credit conditions in which
households operate have evolved markedly since financial deregulation in the
1980s?

The Muellbauer–Williams "LIVES" framework — Latent Interactive Variable
Equation System, in the flow-of-funds tradition of Tobin and of Duca
and Muellbauer (2013) — was developed to answer exactly these
questions. It augments the standard credit-augmented life-cycle consumption
function (Friedman 1957; Ando and Modigliani 1963; Tobin and Dolde 1971)
with three features. First, wealth is disaggregated into net liquid assets
(NLA, liquid assets less total household debt), illiquid financial assets
(IFA, equities plus superannuation) and housing assets (HA), each entered as
a ratio to annualised income and each carrying a different marginal
propensity to consume (Backus and Purvis 1980). Second, a latent
credit-conditions index (CCI) interacts with the long-run equilibrium so
that key channels are switched on only as credit eases. Third — and central
to Williams' (2010) Australian estimate — the latent CCI is identified
*jointly* across a four-equation system (consumption, house prices, mortgage
stock and home-equity withdrawal) under cross-equation parameter
restrictions estimated by full-information maximum likelihood (FIML).

The theoretically decisive feature for this paper is how housing enters.
In the canonical LIVES equation (Williams' Eq (7); his Table 1, column 1)
**there is no classical housing-wealth effect**. Housing wealth appears
*only* through its interaction with credit conditions, as
γ₁·CCI·(HA/4y). The housing marginal propensity to consume is therefore
zero when credit is fully constrained (CCI = 0) and is unlocked as credit
eases, reaching its peak when CCI is at its maximum. By contrast, illiquid
financial and net liquid assets enter as plain, credit-invariant marginal
propensities (γ₂ and γ₃). This functional form has an immediate
implication for empirical work: a specification that enters a standalone
housing-wealth ratio (a credit-invariant ha_y term) is *not* the LIVES
equation, and reading an insignificant standalone housing coefficient as
evidence of a failed housing-wealth effect is a **category error** — the
theory predicts that coefficient to be approximately zero in the absence of
the CCI interaction.

Williams (2010) applied the LIVES framework to Australia for 1978–2008,
producing the canonical Australian estimate. Sixteen years of post-GFC and
post-COVID data have since accumulated, and a contemporary
central-bank-quality update is warranted. This paper provides that update.
Its pivot, set out below, is that an earlier reading of single-equation
Australian LIVES estimates as "weak" was in substantial part a
specification artefact: when the equation is estimated in its *faithful*
form — housing entered only through the credit interaction, the autonomous
CCI intercept restored, and illiquid financial assets combined — the
error-correction and core wealth structure come alive in a way the
conventional constant-MPC disaggregated specification cannot deliver.

### 1.2 Contribution

This paper makes five contributions to the Australian household consumption
literature.

**(i) A faithful single-equation LIVES estimate, with the form correction
as the lead methodological result.** We show that the functional *form* of
the LIVES equation, not the fit of any one regressor, is what identifies it.
Estimating the faithful form — housing entered only through ha_x_cci, the
de-meaned CCI·(HA/4y) interaction; the autonomous-consumption CCI intercept
(cci_williams, the zeta_c loading) restored; and illiquid financial assets
combined into a single ilfa_y = eq_y + super_y term — yields our headline
specification, Spec 11. It recovers a correctly signed and significant
error-correction speed and a right-signed wealth structure (§1.3). The same
data fitted to the conventional constant-MPC disaggregated error-correction
model (Spec 6), which carries plain ha_y / eq_y / super_y / nla_y terms with
no credit scaling and admits CCI only as a short-run regressor, delivers an
insignificant speed of adjustment. The contrast is one of theoretical form,
not of estimation luck.

**(ii) The finding that the LIVES structure transfers to Australia but
Williams' Australian calibrations do not.** Imposing Williams' Australian
permanent-income gearing (psi_0 = 0.20, psi_1 = 0.93) and his illiquid
financial MPC (gamma_IFA = 0.022) — the Williams-calibration-imposed
specification, Spec 12 — *collapses* the equilibrium: the speed of
adjustment falls to lambda = −0.030 (t = −0.74, full sample), statistically
indistinguishable from zero, and flips sign pre-COVID. An independent
calibration route (Spec 10, Williams-prior) reproduces the collapse
(lambda = −0.048, t = −0.78). The reason is that Australia freely
estimates a structural permanent-income gearing of order one — several
times Williams' 0.20 — so forcing his value breaks the long-run fixed
point. This reconciles the companion paper's Wald non-rejection of
Williams' joint calibration (chi-squared(6) = 7.55, p = 0.27): the free
Spec 6 estimates are too imprecise to *reject* Williams' values
jointly — although the sharper Spec 11 intervals now reject his
net-liquid and peak-housing magnitudes individually (§7.3.1) — yet
imposing his calibration nonetheless wrecks the fit. Low power is not
the same as good fit.

**(iii) An interaction-collinearity diagnosis explaining why Williams needs
FIML.** The CCI-interacted regressors that constitute the credit
channels have absolute pairwise correlations between roughly 0.66 and
0.97 on the post-1988 Australian sample
(australia_cci_interaction_corr.csv), because each is approximately
proportional to the same latent CCI — which itself has no variation
before 2007Q3 (§5.1.1), so the credit channels are identified off
roughly seventy quarters. They cannot, therefore, be separately free-estimated
off a single equation. This is the structural reason Williams identifies the
credit channels through cross-equation FIML restrictions rather than within
the consumption equation alone. The corollary is that single-equation
calibration is empirically closed: sharpening the credit channels requires
either the four-equation FIML system or a longer (pre-1988) sample that
spans the financial-liberalisation episode.

**(iv) A back-extended master dataset (1976Q3–2024Q4, n = 194 quarters) and
a direct sample-length test.** We construct documented growth-rate splices
for house prices (Treasury TRYM historical compilation), the M3 monetary
aggregate (RBA D03), total credit (RBA D02) and labour force, with
disaggregated wealth proxies anchored at 1988Q3 — the quarter in which ABS
sectoral household balance-sheet data begin. Because the
financial-liberalisation episode that identifies the credit channels largely
predates 1988Q3, this back-extension is the natural route to longer credit
variation. Refitting the disaggregated no-CCI specification (Spec 4) on the
extended sample moves lambda about 12 per cent toward Williams (−0.182
to −0.203), but individual wealth coefficients shrink rather than
strengthen and the net liquid MPC collapses — so sample length is *not*
the binding constraint on tighter agreement with Williams.

**(v) A structured robustness and placebo suite, and a set of honest
negative results reported as substantive findings.** Mirroring the De Bonis,
Liberati, Muellbauer and Rondinelli (2020) Italian methodology, we run instrumental
variables, a Zellner SUR, multi-window Chow and Bai–Perron break tests, the
Drehmann amortisation adjustment, an AR/Italy direct-forecast
permanent-income comparison with a real-time column, a permanent-income
filter sensitivity grid, rolling-window estimation, and out-of-sample
validation against random-walk and AR(1) benchmarks. We retain — and report
prominently, without apology — the negative results that a freely estimated
single-equation framework permits: the literal Williams 4-knot spline
sits at the placebo median, although the *deployed* maximal-GETS
construction beats 84 per cent of random draws under its own iterated
reduction protocol; the SUR delivers essentially no efficiency gain
(negligible cross-equation residual correlation); the structural
specifications beat a random walk with drift out of sample at one
quarter but lose at four and eight; the automated selection screen
prefers a net-worth specification while the BIC and LIVES theory now
both point to the faithful Spec 11; no specification passes the
Engle–Granger cointegration screen; and the full-sample
permanent-income measure is partly reversed under a causal real-time
projection. Each negative is diagnostic: it points to why FIML and
back-extension are the routes forward.

### 1.3 Headline result

The faithful LIVES specification (Spec 11) is the headline, and we
anchor it on the COVID-controlled estimates. The speed of adjustment is
tightly clustered at **lambda ≈ −0.25** across the three COVID-robust
variants — −0.266 (t = −4.85) on the pre-COVID sample (1988Q3–2019Q4,
n = 126), −0.248 (t = −6.66) dropping 2020Q1–2021Q4, and −0.242
(t = −6.25) with quarterly 2020–21 dummies — within about 13 per cent of
Williams' (2010) published −0.286 (his phi_c). The full-sample estimate
(1988Q3–2024Q4, n = 146) is **lambda = −0.448** (Newey–West t = −3.57);
it is inflated by the COVID quarters — the three pulse dummies the
full-sample specification carries are not sufficient — and it fails the
upper-bound screen on |lambda| in the selection rubric (§6), so we
report it but do not headline it. The full Spec 11 coefficient vector
under all four sample treatments is committed
(australia_spec11_variants.csv) so this choice is checkable. [Source:
australia_spec11_variants.csv; australia_lambda_robustness.csv.]

The credit-invariant wealth structure is right-signed and significant
at 5 per cent in the full-sample and COVID-dummy variants, weakening to
10 per cent in the pre-COVID subsample. The
net liquid MPC is **nla_y = +0.027** (t = 3.75) on the full sample,
+0.017 (t = 2.11) with the COVID quarters dropped and +0.014 (t = 3.10)
under quarterly COVID dummies, implying a structural MPC
gamma_3 = OLS/|lambda| of about **0.060** with a 95 per cent
delta-method interval of [0.022, 0.098]. The illiquid financial MPC is
**ilfa_y = +0.015** (t = 3.09) full-sample and significant at 5 per cent
under the COVID-dummy variants, weakening to 10 per cent on the
pre-COVID subsample (t = 1.74), implying structural
gamma_2 ≈ **0.035** [0.012, 0.057] — an interval that spans Williams'
calibrated 0.022. The housing-collateral channel enters with the right
sign but is not significant in any variant: **ha_x_cci = +0.0025**
(t = 0.71 full sample), implying a structural peak housing-collateral
MPC of gamma_1 ≈ 0.0055 [−0.010, 0.021]. [Source:
australia_spec11_variants.csv; australia_gamma_inference.csv.]

The gamma-inference exercise now cuts both ways, which is a substantive
upgrade on an earlier draft in which every comparison was vacuous.
Spec 11's intervals are tight enough to *reject* parts of Williams'
profile: his net-liquid MPC (0.159) lies outside [0.022, 0.098], and his
peak housing MPC (0.0488) lies outside [−0.010, 0.021]; his illiquid
financial MPC (0.022) is comfortably inside ours. The honest reading is
agreement on the *form* and on the illiquid-financial channel,
disagreement on magnitudes for the net-liquid channel, and an
underpowered housing-collateral channel whose interval contains zero.
[Source: australia_gamma_inference.csv.]

Permanent income enters strongly: **ln_yp_over_y = +0.459** (t = 4.04)
on the full sample and +0.298 (t = 5.81) pre-COVID. Applying the
paper's own structural-recovery rule (§4.2), however, the implied
gearing is OLS/|lambda| ≈ **1.02 full-sample and 1.12–1.13 in the
COVID-controlled variants — above the theoretical admissibility bound
psi ≤ 1 − η ≈ 0.95**. We flag this openly rather than re-scale it away:
the breach is not an artefact of the GFC learning-weight applied to the
permanent-income series (re-estimating without the ogive gives
lambda = −0.574 with structural gearing ≈ 1.05;
australia_spec11_ogive_robustness.csv), and §7.0 discusses the
candidate explanations, including remaining COVID leverage and the
non-causal construction of the measure itself. Comparisons with
Williams' psi_0 = 0.20 must be made on a consistent scale: the correct
statements are "structural ≈ 1.0–1.1 versus 0.20" or "OLS +0.46 versus
phi·psi_0 ≈ 0.057", not the OLS-to-structural mix an earlier draft
used. [Source: australia_spec11_variants.csv;
australia_spec11_ogive_robustness.csv.]

The contrast with the calibration route is sharp. Imposing Williams'
Australian calibrations (Spec 12: psi_0 = 0.20, psi_1 = 0.93,
gamma_IFA = 0.022) collapses the error-correction term to
lambda = −0.030 (t = −0.74) on the full sample and flips its sign on
the pre-COVID sample (lambda = +0.041, t = 2.03, significant at 5 per cent); the
independent Williams-prior route (Spec 10) reproduces the collapse at
lambda = −0.048 (t = −0.78). The LIVES *structure* transfers to
Australia; Williams' Australian *calibrations* do not. Williams' rate,
affordability and autonomous-consumption loadings cannot even be
imposed at their published magnitudes — his raw rate loading
(alpha_r = −0.871) is roughly thirty times too large on the
repository's percent real-rate by unit-normalised-CCI scaling, and
diverges the long-run fixed point. [Source: australia_all_results.csv;
australia_lambda_robustness.csv.]

By contrast, the conventional constant-MPC disaggregated ECM
(Spec 6) — the specification an earlier draft, and prior Australian
work, treated as the LIVES equation — delivers lambda = −0.239
(t = −2.55) on the n = 86 sample to which its credit term binds it,
with an insignificant standalone housing coefficient (ha_y = +0.0022,
t = 0.30). We retain Spec 6 as the conventional baseline against which
the faithful form is the alternative, but we no longer read its
insignificant standalone ha_y as a failed housing-wealth effect: under
LIVES theory that coefficient is expected to be approximately zero
absent the credit interaction. [Source: australia_all_results.csv.]

Finally, the credit-conditions identification limits are reported as
findings rather than hidden, starting with the deployed index itself:
only four of fifteen candidate knots survive, all post-2007, so the
CCI is identically zero from 1988 to mid-2007 and negative after 2019
(§5.1.1 and australia_cci_williams_path.png) — the credit channels are
identified off roughly seventy quarters, not the nominal n = 146. The
placebo evidence is split: the literal Williams 4-knot sits at the
placebo median (45th adjusted-R² percentile), while the deployed
maximal-GETS construction under its own iterated reduction protocol
beats 84 per cent of random draws — moderate support, short of strong
identification. The CCI-scaled regressors are mutually collinear
(|rho| 0.66–0.97; australia_cci_interaction_corr.csv) and so cannot be
separately identified off one equation; the SUR of consumption and
house prices shows negligible residual correlation (ρ̂ = −0.013) and no
efficiency gain; and the structural specifications beat a random walk
with drift out of sample at one quarter but lose at four and eight. We
do not promote the free-interaction specification (Spec 8,
lambda = −0.458) as a credit-conditions success: it re-allocates
identification across regressors rather than closing the gap with
Williams. [Source: australia_williams_knot_placebo_verdict.csv;
australia_williams_knot_placebo_deployed_verdict.csv;
australia_oos_rmse.csv; australia_all_results.csv.]

### 1.4 Roadmap

Section 2 surveys the LIVES literature, the Australian aggregate-wealth-effect
literature and permanent-income measurement, and frames the LIVES claim that
there is no classical housing-wealth effect — so that standalone-wealth
specifications mis-test the theory. Section 3 documents data construction,
including the disaggregated wealth ratios, the Italian direct-forecast
permanent-income measure with its look-ahead versus real-time caveat, and
the 1976Q3 back-extension sources and proxies. Section 4 presents the
canonical LIVES equation (Eq (7)) and distinguishes the faithful form
(Spec 11) from the conventional constant-MPC ECM (Spec 6) as a matter of
theory. Section 5 develops the identification of credit conditions,
including the interaction-collinearity result, the placebo battery, the
near-zero SUR residual correlation and the joint knot-survival test. Section 6
sets out the full specification ladder (Specs 1–12, now including the
faithful Spec 11 and the calibration-imposed Spec 12) and the selection
rubric, and reports the selector divergence honestly. Section 7 presents the
faithful-LIVES headline results and the calibration-collapse finding, with
Spec 6 reframed as the conventional baseline. Section 8 runs the structured
robustness suite, preserving the honest negatives. Section 9 reconciles our
estimates with Williams (2010, 2012) around the structure-transfers /
calibrations-don't thesis. Section 10 presents the long-run decomposition and
policy implications, based on the faithful Spec 11 channels. Section 11
concludes.

---


## 2. Literature review

This paper sits at the intersection of three literatures: the
Muellbauer-Williams "LIVES" tradition that integrates wealth, credit
conditions and life-cycle behaviour into consumption equations; the
Australian empirical consumption literature, which has historically
focused on aggregate or constant-marginal-propensity wealth effects
without the explicit credit-conditions machinery; and the small but
growing body of work that disciplines permanent-income measurement
using forecasting approaches more robust than the standard AR(p)
recipe. We review each in turn before placing the present contribution.

The thread that runs through the review, and that anticipates the
paper's central finding, is a point about functional form. The LIVES
framework makes a specific theoretical claim — that there is *no
classical housing wealth effect*: the marginal propensity to consume
out of housing wealth is zero when credit conditions are tight and is
unlocked only as credit conditions ease. A consumption equation that
enters housing wealth as a standalone level term, with a constant
marginal propensity, does not test this claim — it tests a different,
incompatible one. Reading an insignificant standalone housing-wealth
coefficient as evidence of "no housing wealth effect" is therefore a
*category error*: theory predicts that coefficient to be near zero in
the absence of the credit-conditions interaction. This distinction —
between the *faithful* LIVES form, in which housing enters only through
its interaction with the credit-conditions index, and the *conventional
constant-MPC* disaggregated error-correction model that prior work and
an earlier draft of this paper treated as the LIVES equation — is the
organising idea of the literature that follows and the lead result of
the paper (§7.0).

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
different marginal propensities — is the cornerstone of the
disaggregated specifications reported in §7. In the faithful LIVES
form, this implication is sharpened further: liquidity matters not only
through a level distinction but through how each component interacts
with the prevailing state of credit. Net liquid assets and illiquid
financial assets enter as plain marginal propensities, but housing
wealth is collateral whose consumability depends on the borrowing
technology — and so enters only through its credit-conditions
interaction.

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
the long-run relationship not as a single additive term but as a
multiplicative shifter across several channels simultaneously.

The functional form is decisive, and worth stating precisely because it
is what distinguishes the framework from a conventional wealth
error-correction model. In the canonical Williams (2010) consumption
equation — his Eq (7), reproduced in §4 — the credit-conditions index
multiplies *six* channels jointly inside the long-run bracket: the
autonomous-consumption intercept (ζ_c·CCI), the real-interest-rate term
(α_1·r·CCI), the housing-collateral term (γ_1·CCI·(HA/4y)), the
permanent-income gearing (ψ(CCI)·log(y^p/y), with ψ(CCI) = ψ_0 +
ψ_1·CCI), and the affordability term (α_4·(1−ϖ·CCI)·log(p^h/y), with
the affordability multiplier ϖ = 1.2). Critically, housing wealth
enters *only* through γ_1·CCI·(HA/4y) — there is no standalone housing
wealth level in the long-run relationship. The housing marginal
propensity is therefore zero at CCI = 0 and rises as credit conditions
ease, reaching Williams' implied peak housing MPC of 0.0488 — his
derived long-run figure, not the raw Table 1 coefficient (γ₁ = 0.0606);
see williams_comparison.csv. The illiquid financial asset term
(IFA = equities + superannuation) and the net liquid asset term
(NLA = liquid assets − total household debt) enter as plain marginal
propensities (Williams' calibrated γ_IFA = 0.022 and estimated
γ_NLA = 0.159 respectively). Income enters with a *unit* coefficient,
so that the error-correction term is the (log) consumption-to-income
ratio, and all wealth terms enter as asset/annualised-income ratios,
x_{t−1}/4y.

It follows that a specification entering housing wealth as a standalone
constant-MPC level term — alongside equities, superannuation and net
liquid assets, with credit conditions relegated to a short-run dynamic
regressor — is not the LIVES equation at all, but a generic wealth
error-correction model. Under the LIVES theory, the standalone housing
coefficient in such a model is expected to be near zero, because the
operative channel (the CCI interaction) has been omitted. This is the
sense in which reading an insignificant standalone housing coefficient
as a failed housing wealth effect is a category error, and it is the
reason §7 distinguishes the *faithful* LIVES specification from the
*conventional constant-MPC* baseline as a matter of theory, not fit.

The framework was operationalised in a series of country studies. Aron,
Duca, Muellbauer, Murata and Murphy (2012) jointly estimate the framework
on Japan, the United Kingdom and the United States, finding consistent
positive long-run housing wealth effects in the UK and US (where home
equity withdrawal is institutionally available) and a much smaller
effect in Japan (where it is not) — precisely the cross-country pattern
that the credit-interaction form predicts, since the housing channel
should be muted where the collateral-borrowing technology is weak. Duca,
Muellbauer and Murphy (2010) apply the framework to the global financial
crisis, demonstrating that the abrupt tightening in CCI from 2007
quantitatively rationalises the sharp consumption pullback observed in
heavy-MEW economies. Duca and Muellbauer (2013), in
European Central Bank Working Paper 1581 ("Tobin LIVES"), formalise
what they label the "LIVES" approach — the *L*atent *I*nteractive
*V*ariable *E*quation *S*ystem — emphasising the joint determination of
consumption, house prices, mortgage debt and home equity withdrawal in
a four-equation system identified by common factors and cross-equation
sign restrictions.

Two parallel implementations frame the present paper. De Bonis,
Liberati, Muellbauer and Rondinelli (2020) estimate a single-equation Italian
adaptation that imposes the cross-equation restriction
γ_LA + γ_LOANS = 0 (deposits and household debt enter with
equal-and-opposite coefficients, so that net liquid assets is the
operative quantity), adopts a direct single-regression forecast of the
discounted future-income aggregate as its permanent-income measure
(their Appendix A.2), applies a Drehmann (2017)
amortising-mortgage adjustment to the real mortgage rate, and
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
§3. Williams (2010), Oxford Economics Series Working Paper 492,
estimates the four-equation LIVES system (consumption, house prices,
mortgage stock, home equity withdrawal) jointly by full-information
maximum likelihood (FIML) on Australian data 1978Q1–2008Q2, with the
house-price loading on credit conditions normalised (ζ_h = 1) to fix
the scale of the latent index. The published version of this work,
Muellbauer and Williams (2012) "Credit conditions and the real economy:
the elephant in the room", appears as the lead chapter of *BIS Papers*
No. 64 and constitutes our primary benchmark.

Australian consumption modelling outside the LIVES tradition has a
long history, almost all of it in the standalone-wealth-effect form that
the LIVES theory regards as a mis-test of the housing channel. Tan and
Voss (2000), in RBA Research Discussion Paper 2000-09, estimate
aggregate-wealth effects on Australian consumption using ABS National
Accounts and RBA balance-sheet data, finding significant positive
effects of both housing and financial wealth. Dvornak and Kohler (2003),
in RBA RDP 2003-07, use a state-level panel to identify wealth effects
from cross-state variation, finding larger marginal propensities to
consume out of stock-market wealth than out of housing wealth, in
apparent contrast to the time-series evidence; their findings are partly
reconciled by the Muellbauer-Williams framework once credit-conditions
interactions are introduced, since the unconditional housing MPC
averages over tight- and loose-credit regimes and so understates the
loose-credit collateral channel. The methodological point is that none
of these specifications interacts housing wealth with credit conditions,
and so each estimates an unconditional housing MPC whose magnitude the
LIVES form predicts will be small and unstable — a property to keep in
mind when comparing coefficient magnitudes across studies.

The Reserve Bank of Australia's macroeconometric model MARTIN,
introduced in Cusbert and Kendall (2018) in the RBA Bulletin and
documented in Ballantyne et al. (2019) in RBA RDP 2019-07, includes a
household consumption block that incorporates wealth effects and
credit conditions in a more reduced-form way than the LIVES
specification. The MARTIN consumption equation imposes calibrated
elasticities for several channels rather than estimating the full
long-run cointegrating vector, and abstracts from the explicit CCI
spline; its calibrated net-wealth elasticity is 0.17
(martin_nesting.csv). The present paper complements MARTIN by providing
a freely estimated benchmark against which calibrated coefficients can be
evaluated — our unrestricted net-wealth elasticity is 0.1155
(martin_nesting.csv), with long-run homogeneity rejected
(χ² = 16.41, p = 0.0001) — and by surfacing the identification choices
that drive the estimated speed of adjustment. As §10 discusses, our
wealth-elasticity estimate is too imprecise to discipline MARTIN's
calibration; the appropriate reading is to import the qualitative
structure rather than the point estimates.

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
LIVES framework, and the heart of the identification problem this paper
documents. Muellbauer and Williams (2012) construct CCI as a latent
variable identified by a spline of smoothed-step dummies (`SDMMA`
series — five-quarter moving averages of four-quarter moving averages
of step dummies), Σ a_s·SDMMA_s, at four institutional turning points:
1979
(Campbell Committee, removal of interest-rate ceilings on bank
deposits), 1992 (banking distress and the entry of the first mortgage
originator, Aussie Home Loans), 1998 (the rise of non-bank financial
institutions and securitisation), and 2007 (the global financial crisis
tightening). The institutional chronology of Australian financial
deregulation underpinning these choices is documented in Battellino and
McMillan (1989) and Edey and Gray (1996); Bayoumi (1993) provides a
cross-country analysis of the consumption response to financial
liberalisation, including Australia, that quantitatively validates a
structural CCI shift in the early 1980s. Williams' earlier
single-author papers used a simpler precursor: Williams (2009)
identifies the same institutional turning points via STAMP
unobserved-components analysis (Koopman et al. 2000) but represents
credit conditions as smoothed *linear split trends* rather than
smoothed step dummies, and Williams (2010) carries this simpler
measure (denoted `CCIH`) into the single-equation consumption model;
the step-dummy `SDMMA` spline used throughout this paper is the
refinement introduced in the joint four-equation system.

The decisive feature of Williams' identification is that it is *joint*.
In the system-estimation context of Muellbauer and Williams (2012),
each spline coefficient is identified by being a *common factor* across
the four equations: the same CCI value enters consumption, house
prices, the mortgage stock and home equity withdrawal, with different
loadings, and the house-price loading is normalised (ζ_h = 1) to fix
the index scale. This common-factor, FIML identification is the central
methodological contribution of the LIVES family — and, as this paper
argues, a structural necessity rather than a stylistic preference. The
reason is collinearity. The six CCI-interacted regressors are each
approximately proportional to CCI, and on the post-1988 Australian
sample they are 0.66–0.97 mutually correlated in absolute value
(`australia_cci_interaction_corr.csv`; §5.5). Six near-collinear
interaction regressors cannot be separately freely estimated off a
single equation; in single-equation OLS the spline can be proxied or
calibrated, but the individual credit channels cannot be jointly
identified. This is the structural reason Williams uses FIML, and it is
why this paper treats four-equation FIML and pre-1988 back-extension —
not single-equation refinement — as the only routes to sharpen the
credit channels. We adopt the spline approach under sign-prior
restrictions enforced by general-to-specific reduction (Hendry-Krolzig
2005) as a single-equation robustness exercise (§5), but with the
understanding that single-equation calibration is empirically closed.

The alternative, observable proxy for CCI — the ratio of housing credit
flow to disposable income, in logs — has the advantage of being measured
directly but the disadvantage of being available only from the early
2000s (the ABS 5601.0 lending series begins 2002Q3), after the most
informative deregulation episodes have already occurred. This timing
problem is fundamental rather than incidental: ABS sectoral
balance-sheet data begin only in 1988Q3, so the financial-liberalisation
episode that identifies the credit channels largely predates the data on
which the consumption equation can be estimated. We use the observable
proxy as a short-run regressor in the conventional-baseline
specification, and the spline approach for the long-run identification
in the credit-interaction robustness specification.

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
expected weighted average of expected future income — requires either
explicit forecasts or a parametric assumption about the income process.
The standard practice in the consumption literature has been to assume
an AR(p) process for log income, fit it on the available sample, and
aggregate the multi-step-ahead forecasts using exponentially declining
weights with discount factor δ. This recipe descends directly from the
PIH literature of Hall (1978) and the consumption-Euler-equation tests
of Campbell and Mankiw (1989). In the LIVES form, permanent income
enters with a credit-conditions-dependent gearing, ψ(CCI) = ψ_0 +
ψ_1·CCI, which Williams calibrates rather than estimates (ψ_0 = 0.20,
ψ_1 = 0.93); §4 and §7 take up whether that calibration transfers to
Australia.

An alternative to the AR(p) recipe is to forecast the discounted
aggregate *directly*: compute, for each quarter whose forecast horizon
is observable, the discounted weighted average of realised future log
income, and regress that pre-aggregated target on predictors observable
at time *t* in a single equation. This *direct forecast* of the
horizon-aggregate — the method of De Bonis, Liberati, Muellbauer and
Rondinelli (2020, Appendix A.2), and related in spirit to (though
distinct from) Jordà's (2005) direct multi-horizon projections, which
fit one regression per horizon — sidesteps the compounding of AR
misspecification across forecast horizons to which the standard recipe
is vulnerable, and admits a richer predictor set than is feasible in a
parsimonious AR(p). (An earlier draft of this paper labelled the method
a "Jordà local projection"; we retain the De Bonis et al. attribution
throughout, since the implementation is a single full-sample regression
of the pre-aggregated discounted target, not a per-horizon projection.)

De Bonis et al. (2020) adopt this direct-forecast approach for their
Italian permanent-income series, reporting that the
choice "captures much of the slow-down of permanent income growth in
the early 1990s" — a structural feature of Italian growth that the
AR-based forecaster missed. The Italian forecasting regression includes
a `log(labour_force / population)` predictor, which slowly trends with
demographic change and is a natural input to long-horizon income
forecasting. We adopt the same predictor in our Italy-style
permanent-income helper, with a discounted (η = 0.05, k = 40 quarters)
weighted average of expected future income (§3, §4), and find a
quantitatively similar role: the permanent-income series implied by
the direct forecast in Australia diverges materially from the AR-based
series in the early 1990s and after the 2008 GFC. Substantively, the
implied long-run coefficient on log(y^p/y) moves from slightly negative
and economically negligible under the AR forecaster — the "Australian
permanent-income puzzle" — to positive under the full-sample
direct-forecast *measure*.
As §8 documents, this reversal reflects the measure's full-sample
(non-causal, look-ahead) construction and does not survive a causal
real-time projection, which sharply shrinks the speed of adjustment
(see §7.4 and australia_pi_realtime_robustness.csv). The look-ahead
versus real-time distinction is therefore flagged as a measurement
caveat throughout: the full-sample permanent-income measure is the
headline input, with the real-time variant reported as the operational
robustness column.

### 2.6 Where this paper sits

The contribution of this paper is best understood as a faithful
single-equation revisit and extension of Williams (2010), and it has
five distinct components.

**(i) The faithful single-equation LIVES estimate, and the form
correction as the lead methodological result.** The paper's central
finding is that the functional *form* of the LIVES equation is what
identifies it. When housing wealth is entered faithfully — only through
its credit-conditions interaction (ha_x_cci, the de-meaned CCI·(HA/4y)
term), with no standalone housing-wealth level — the
autonomous-consumption CCI intercept restored (ζ_c·CCI), and illiquid
financial assets combined (ilfa_y = equities + superannuation), the
error-correction and core wealth structure come alive. The faithful
specification (the headline of §7.0) delivers a COVID-controlled speed
of adjustment of λ ≈ −0.25 (pre-COVID −0.266, t = −4.85), within about
13 per cent of Williams' −0.286, on a sample of n = 146 (full) / 126
(pre-COVID); correctly signed and significant net-liquid and
illiquid-financial marginal propensities (structural MPCs of 0.060 and
0.035 respectively, full sample, relative to |λ| = 0.448); a strong
permanent-income coefficient (+0.46, t = 4.0); and a housing-collateral
coefficient (γ_1) that is right-signed but insignificant (implied
structural MPC 0.0055, t = 0.71, against Williams' peak 0.0488). By
contrast, the conventional constant-MPC disaggregated error-correction
model that prior work and an earlier draft of this paper treated as the
LIVES equation is *not* the LIVES equation and delivers a weaker,
sample-fragile equilibrium (λ = −0.239, t = −2.55, on its n = 86
sample, collapsing to −0.087 pre-COVID).
Reading its insignificant standalone housing coefficient as a failed
housing wealth effect is the category error described above; much of
the apparent weakness of Australian single-equation LIVES estimates is
a specification artefact.

**(ii) The structure-transfers-but-calibrations-do-not finding.** The
LIVES *structure* transfers to Australia, but Williams' Australian
*calibrations* do not. Imposing his permanent-income gearing
(ψ_0 = 0.20, ψ_1 = 0.93) and illiquid-financial MPC (γ_IFA = 0.022)
collapses the equilibrium to λ ≈ −0.030 (t = −0.74), independently
reproduced by a Williams-prior calibrated specification (λ = −0.048),
because Australia freely estimates a structural permanent-income
gearing of order one (ψ̂ = OLS/|λ| ≈ 1.0–1.1) — roughly five times
Williams' 0.20 on the consistent structural scale (equivalently, the
OLS coefficient +0.46 against his implied φ·ψ_0 ≈ 0.057). This
reconciles the companion paper's Wald non-rejection of the joint
calibration (χ²(6) = 7.55, p = 0.27): the free estimates are too
imprecise to reject Williams' values, but imposing those values wrecks
the fit — low power is not the same as good fit. Williams' rate and
affordability loadings cannot even be imposed at published magnitudes:
his raw α_r = −0.871 is roughly thirty times too large on the repository's
percent real-rate × unit-normalised CCI scaling, and diverges the
fixed point.

**(iii) The interaction-collinearity diagnosis.** The six
CCI-interacted regressors are 0.66–0.97 mutually correlated in absolute
value on this sample (australia_cci_interaction_corr.csv) because each
is approximately proportional to CCI, and so cannot
be separately freely estimated off a single equation. This is the
structural reason Williams uses FIML, and it explains why single-equation
calibration is empirically closed and why four-equation FIML plus
pre-1988 back-extension are the only routes to sharpen the credit
channels (§5).

**(iv) The 1976Q3 back-extended master dataset and a direct
sample-length test.** The paper extends the sample back to **1976Q3**.
The public-data backbone consists of a Treasury TRYM long-run
house-price series (1959Q3+), the RBA D03 M3 monetary aggregate
(1959Q3+), the RBA D02 total-credit splice (1976Q3+), and a historical
labour-force compilation (1964Q3+). For the 1976Q3–1988Q2 window where
ABS sectoral household balance-sheet data are unavailable, we construct
aggregate and disaggregated wealth proxies anchored at 1988Q3 (§3).
Refitting the disaggregated no-CCI specification on the back-extended
sample moves the speed of adjustment about 12% toward Williams (λ from
−0.182 to −0.203; lambda_robustness.csv / spec46_extended_comparison.csv),
but the individual wealth coefficients shrink and the net-liquid-asset
coefficient collapses (−95%) — which we read as evidence
that the binding constraint is the single-equation framing itself, not
the post-1988 sample window. Sample length is not the binding
constraint.

**(v) The structured robustness and placebo suite, and the honest
negative results.** Adopting the Italian methodology of single-equation
OLS with Newey-West HAC standard errors, the paper runs a structured
robustness suite — instrumental variables, joint SUR, multi-window
Chow tests, scaled-income and Drehmann real-rate alternatives, a
Kalman state-space CCI, and a Williams-style spline credit-conditions
column. The paper is honest throughout about the limitations of
single-equation LIVES estimation on post-deregulation Australian data,
and reports the negative results prominently as substantive findings
that a freely-estimated single-equation framework permits (where imposed
restrictions would hide them): the knot-placebo evidence on the
credit-conditions index is split — the literal Williams four-knot
construction sits at the placebo median (45th adjusted-R² percentile)
and below it on the extended variants (36th–48th), while the deployed
maximal-GETS protocol beats 84 per cent of random draws; a two-equation
consumption + house-price SUR finds negligible cross-equation residual
correlation and so no efficiency gain (SUR ρ̂ = −0.013 against OLS
−0.011; lives_sur_2eq_resid_corr.csv); out-of-sample, the random walk
with drift beats every structural specification at horizons h = 4 and
h = 8 (though Specs 8 and 11 beat it at h = 1); the automated
specification selector falls back to a non-LIVES net-worth form
(Spec 3) even though BIC and theory agree on Spec 11; and the
look-ahead permanent-income reversal does not survive real-time
construction. Each negative is read diagnostically — it points to why
FIML and back-extension, rather than further single-equation
refinement, are the way forward.

The headline paper does not estimate the multi-equation LIVES system
itself, though we scaffold the multi-equation build in a separate
companion directory. Williams (2010) estimates four equations jointly
by FIML; the headline of the present paper estimates the consumption
equation alone. The Italian experience (De Bonis et al. 2020)
suggests that single-equation OLS produces consumption-equation
coefficients close to joint SUR estimation, and we replicate that
finding (§8): on our back-extended sample a two-equation
consumption + house-prices SUR finds negligible residual correlation,
so joint estimation gives no efficiency gain at the quarterly frequency.
The case for the full multi-equation build therefore rests on
cross-equation parameter restrictions — the common-factor identification
of the credit channels — rather than on efficiency. The full
multi-equation extension is left to a companion paper, with the
scaffolding documented separately.



---



## 3. Data and measurement

The dataset assembles quarterly Australian macroeconomic and
household-sector observations from **1976Q3 to 2024Q4 (n = 194)**.
The public-data backbone for the pre-1980 window is built from a
Treasury TRYM long-run house-price series, the RBA D03 M3 monetary
aggregate, the RBA D02 total-credit splice, and a historical
labour-force compilation. The disaggregated wealth components — the
housing, illiquid-financial and net-liquid ratios that carry the
faithful LIVES specification (Spec 11) — remain bounded at 1988Q3 by
their primary source (ABS Cat 5232.0 Household Balance Sheet); for the
1976Q3–1988Q2 window we construct proxies (§3.13 below) that
growth-rate-splice each component onto its 1988Q3 official value via the
most relevant available aggregate. It bears emphasis at the outset that
1988Q3 is when the ABS sectoral balance sheet begins, so the
financial-liberalisation episode that identifies Williams' credit
channels — the 1980s deregulation — largely *predates* the modern
disaggregated data. This timing, rather than estimator choice, is the
deepest single-equation constraint on identifying the credit-conditions
interactions, and motivates the back-extension to 1976Q3 documented
below (and, ultimately, the four-equation FIML route discussed in §5
and §9).

Estimation is performed on the largest contiguous subset for which all
variables in a given specification are observed:

- **Spec 1–3 (aggregate net worth)** with the back-extension proxy fit
  on n=190 (1977Q3–2024Q4; the binding constraint is `real_rate`, which
  needs a 4-quarter CPI lag for inflation computation). On the official
  `networth_y` (1988Q3+ ABS), Specs 1 and 3 fit on n=146; Spec 2 carries
  the short-run CCI term and so binds at the 2002Q3 loan-flow constraint
  (n=86 full / n=66 pre-COVID).
- **Spec 4 (disaggregated, no CCI) and the faithful LIVES specification
  (Spec 11)** use the official disaggregated series and fit on **n=146**
  (1988Q3–2024Q4), with a **pre-COVID** sub-sample of **n=126**
  (1988Q3–2019Q4) treated as the identified window throughout the paper.
- **Spec 5 (full disaggregated) and Spec 6 (conventional constant-MPC
  disaggregated ECM — the baseline, not the headline)** are both bounded
  at 2002Q3+ because each carries the short-run CCI variable
  (`d2_logcci_lag2`), which depends on `cci_ratio = log(housing_loan_flow
  / income)` from ABS Cat 5601.0; that loan-flow series only starts
  2002Q3. Both bind at **n=86** (full) / **n=66** (pre-COVID) in both
  samples.
- **Spec 8 (free CCI interactions)** and the **Williams-calibrated
  Spec 12** fit on n=146, bounded by the disaggregated wealth and the
  `cci_williams` smoothed-step spline (§3.5), which is constructed across
  the full 1988Q3+ window and does not require the 2002Q3 loan-flow
  series. **Spec 10 (Williams-prior reproduction)**, by contrast, carries
  the short-run `d2_logcci_lag2` term and so binds at the 2002Q3 loan-flow
  constraint, fitting on **n=86** (full) / **n=66** (pre-COVID).
- **Spec 9 (Kalman state-space CCI)** fits on the same n=146 window as
  the disaggregated specs; the Kalman extraction itself is a separate
  state-space step.

The asymmetry in sample length is itself substantive. The faithful
LIVES specification (Spec 11) is estimable on n=146 precisely because it
proxies the credit channels through the `cci_williams` spline rather
than the 2002Q3-bounded loan-flow ratio, whereas the conventional Spec 6
baseline binds at n=86. Comparisons between the two should keep this
sample difference in view; the larger n=146 window is one reason the
faithful form delivers a sharper error-correction estimate (§7).

### 3.1 Aggregate consumption and income

Real per capita consumption (`cons_real_pc`) is constructed from ABS Cat
5206.0 Table 8 (Household Final Consumption Expenditure, chain volume
measures, seasonally adjusted) divided by the civilian population aged
15 years and over (ABS series A84423091W, monthly, averaged to quarterly
arithmetic mean; sample 1978Q2–2024Q4, sourced directly from the ABS
historical series workbook). Following Williams (2010, 2009), we do not
sum the single-year-of-age cohorts in ABS Cat 3101.0; in current ABS
vintages, the `Persons` series only extends to age 47 in the
disaggregated file, producing a population total approximately 35 per
cent below the true Estimated Resident Population.

Real per capita household disposable income (`ydi_real_pc`) is the
quarterly seasonally adjusted nominal series from ABS Cat 5206.0 Table 20
(Household Income Account), deflated by the consumption deflator implied
by Table 8 (chain volume and current prices), and divided by the same
15+ population denominator. Following standard practice
(Blinder–Deaton 1985), and as the headline empirical specification, we
use gross disposable income; we report a non-property income (NPY)
alternative constructed per Williams (2009) §4.2.1 in §3.6 below.

In the canonical LIVES equation (§4) income enters the long run with a
**unit coefficient** — the error-correction term is the
consumption-to-income ratio `log y − log c_{t-1}`, not a freely
estimated income elasticity. The disposable-income series above is
therefore load-bearing for the *level* of the equilibrium, not merely as
a scaling variable, and the choice of income measure (gross vs
non-property) is one of the channels through which our estimates and
Williams' diverge (§3.6, §9).

### 3.2 Household balance sheet

Household-sector balance sheet stocks are sourced from ABS Cat 5232.0
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

Following Williams' Eq (7), wealth enters the long run as
**asset/annualised-income ratios**. In our implementation the ratio is
*contemporaneous*: the end-of-quarter (closing) stock divided by four
times current quarterly income, `x_t/4y`. The
wealth-to-annualised-income ratios used in the long-run consumption
equation are constructed as:

- `ha_y` = housing wealth / (4 × quarterly nominal disposable income)
- `eq_y` = equities (ex-super) / (4 × quarterly nominal disposable income)
- `super_y` = superannuation reserves / (4 × quarterly nominal disposable income)
- `ilfa_y` = (equities + superannuation) / (4 × quarterly nominal disposable income)
- `nla_y` = (deposits − total household debt) / (4 × quarterly nominal disposable income)
- `debt_y` = total household debt / (4 × quarterly nominal disposable income)
- `networth_y` = closing net worth / (4 × quarterly nominal disposable income)

Williams' bracket dates the stock at `t−1` (`x_{t-1}/4y`), and an
earlier draft described our ratios that way; the implementation in fact
uses the current quarter's *closing* stocks throughout, `x_t/4y`.
Because ABS 5232 stocks are closing values, the contemporaneous ratio
embeds within-quarter revaluations, which weakens the predeterminedness
defence of OLS for the wealth terms; §4.6 discloses this timing
convention and §8.1 reports the corresponding IV robustness check.

#### 3.2.1 Illiquid financial assets are combined in the faithful LIVES specification

The LIVES theory distinguishes three wealth classes by their marginal
propensity to consume: housing (a collateral asset, with an MPC that is
*zero* at zero credit conditions and unlocked only as CCI rises),
**illiquid financial assets** (IFA = equities plus superannuation, with a
low plain MPC), and net liquid assets (NLA = deposits net of debt, with
the highest plain MPC). Williams (2010) and the Italian implementation
(De Bonis, Liberati, Muellbauer & Rondinelli 2020) both enter equities
and superannuation as a single illiquid-financial aggregate, calibrating
γ\_IFA = 0.022.

The faithful LIVES specification (Spec 11) follows this convention: it
uses the **combined** `ilfa_y = eq_y + super_y` rather than entering
equities and superannuation separately. This is not a cosmetic
aggregation. Separating the two financial classes (as the conventional
Spec 6 baseline does, with plain `eq_y` and `super_y`) repeatedly
delivers a wrong-signed, insignificant equities coefficient on
post-deregulation Australian data (e.g. Spec 6 full-sample `eq_y` =
−0.016, t = −0.30; Spec 9 `eq_y` = −0.010, ns), reflecting the high
collinearity between the two series and the short modern sample rather
than a negative MPC on equities. Combining them into `ilfa_y` recovers a
correctly signed and significant illiquid-financial loading in the
faithful form (Spec 11 full-sample `ilfa_y` = +0.0155, t = 3.09, ***;
implied structural MPC = 0.035), the same order of magnitude as
Williams' calibrated 0.022 though somewhat larger. The aggregation is
therefore both theoretically faithful and empirically necessary.

#### 3.2.2 Net liquid assets: deposits net of debt

We adopt the convention of the Italian implementation (De Bonis,
Liberati, Muellbauer & Rondinelli 2020, eq. 2.5; Table 3 column 3) of
defining **net liquid assets as deposits net of total household debt**:

```
nla_y = (fin_deposits − fin_loans) / (4 × quarterly nominal disposable income)
```

This embeds the cross-equation restriction γ\_LA + γ\_LOANS = 0 (equal
and opposite MPCs on liquid assets and on debt) implicitly by
construction, in place of estimating a separate liquid-asset and a
separate debt MPC. We test this restriction formally in §8: the netting
restriction is **accepted at the 5 per cent level in every disaggregated
specification and sample window** (australia\_nla\_restriction\_test.csv;
e.g. Spec 4 full sum = +0.032, t = 0.73, p = 0.46; Spec 6 full sum =
+0.020, t = 0.20, p = 0.84). The honest reading is that the modern
sample cannot statistically distinguish separate liquid-asset and debt
MPCs, which is consistent with — but does not by itself confirm — the
NLA aggregation; netting is a defensible economy of parameters rather
than a sharply identified result.

In the faithful LIVES specification, NLA carries the largest and most
robust wealth loading: Spec 11 full-sample `nla_y` = +0.0269 (t = 3.75,
***), an implied structural MPC of 0.060, against Williams' calibrated
0.159.

### 3.3 Mortgage interest rate

The nominal mortgage rate (`mortgage_rate`) is the RBA Standard Variable
Owner-Occupier Rate (RBA Bulletin Table F6 housing lending rates,
series FILRHLBVS), monthly from January 1959, averaged to quarterly. We source the historical
series from the published RBA archive rather than the live `readrba`
API, to ensure a stable vintage; the rate peaks at 17.0 per cent in
1989Q3, consistent with the Hawke–Keating recession.

The real mortgage rate (`real_rate`) is the nominal rate less the
4-quarter-ended percentage change in the consumption deflator. In the
faithful LIVES specification the rate enters only through its credit
interaction, `r_x_cci = real_rate × CCI` (Williams' α₁ channel). We note
here a measurement point that becomes load-bearing in §9: our `real_rate`
is expressed in *percentage* units (e.g. 3.0 for 3 per cent), and CCI is
*unit-normalised*, so Williams' published raw rate loading (α\_r =
−0.871) is roughly thirty times too large to impose on this scaling and
diverges the iterative fixed point — one of the calibrations that, by
construction, cannot transfer. We do not adopt the Italian Drehmann
(2017) amortising-mortgage adjustment in the headline specifications but
report it as a robustness check (§8).

### 3.4 House prices

The headline house price index (`hpi`) is constructed by chain-linking
**four** sources, with the deepest segment extending to 1959Q3:

| Layer | Source | Coverage | Splice convention |
|-------|--------|----------|-------------------|
| TRYM | Treasury Macroeconomic Model historical database (`house_price_history_long.csv`) | 1959Q3–2018Q2 | growth-rate, anchored at 1986Q2 (first overlap with the legacy layer) |
| Legacy | `houseprice_old.csv` (privately compiled dwelling-price index, quarterly) | 1986Q2–2005Q2 | growth-rate, anchored at 2003Q3 (first overlap with the bridge) |
| Bridge | ABS Cat 6416.0 Residential Property Price Index, 8-capital-cities ("old method") | 2003Q3–2021Q4 | growth-rate, anchored at 2011Q3 (first overlap with the current layer) |
| Current | ABS Cat 6432.0 Total Value of Dwellings, mean price | 2011Q3–2024Q4 (sample end; the raw release extends beyond) | (the modern overlay) |

The TRYM source supersedes the BIS Shrapnel / REIA chain used in
Williams (2010): the TRYM historical compilation already incorporates
the same BIS Shrapnel (pre-1978), REIA (1978–1986) and ABS (post-1986)
segments that Williams used, pre-chained into a single coherent
236-quarter series (1959Q3–2018Q2). The earliest binding observation for `hpi` is
therefore **1959Q3**, three years deeper than even Williams' fullest
sample.

In the faithful LIVES specification (§4), the house price enters the
long run *only* through the affordability interaction `hp_x_1_minus_cci
= (1 − 1.2·CCI)·log(ph/y)` (Williams' α₄ channel, with affordability
multiplier ϖ = 1.2), not as a standalone level. There is no separate
classical house-price level term in the faithful form.

#### 3.4.1 Splice methodology — pure growth-rate chain-linking

For each adjacent pair of layers, the splice anchors the level at the
first quarter where both series are non-NA, then back-casts via the
base series' own QoQ growth rates:

```
chained[t] = overlay[t_anchor] × (base[t] / base[t_anchor])  for t < t_anchor
chained[t] = overlay[t]                                       for t >= t_anchor
```

This standard ABS chain-linking convention preserves the base series'
growth rates exactly while pinning the level to the overlay at the join.
By construction there is no level discontinuity at any join quarter. An
earlier implementation used `mean(overlay/base)` over the full overlap,
which produced step jumps at join quarters where the overlap ratio
drifts (notably a −17% step at 1986Q2 under the long TRYM↔legacy overlap
and a +10% step at 2011Q2→Q3 under the bridge↔current overlap with
mismatched units — the bridge is an index while current is a $-value
mean). Both artefacts were eliminated by the growth-rate convention.

The relative house-price-to-income ratio used in estimation is

```
ln_hp_over_y = log(hpi × pop_millions / ydi_ann_nom)
```

i.e. the log of the nominal house-price index divided by nominal
annualised disposable income per capita. Because numerator and
denominator are both nominal, the consumption deflator cancels exactly
and the ratio is identical to the real house price over real income per
capita: `(hpi/defl) / (ydi_ann_nom/pop/defl) = hpi·pop_millions/ydi_ann_nom`
(`australia_data_download.R`).

We disclose a correction here. An earlier draft of this paper divided
the *nominal* `hpi` by *real* income per capita, leaving the
economy-wide price level inside the ratio — the contaminated variable
had a correlation of 0.98 with the consumption deflator and was
measuring inflation, not relative house-price pressure. Correcting the
ratio to the consistent nominal/nominal (equivalently real/real) form
changed the surviving CCI knot set (§3.5, §5.1.1) and, with it, every
coefficient estimate in the paper; all numbers reported in this draft
are from the corrected pipeline.

### 3.5 Credit conditions index

The credit conditions index (CCI) is the most contested input. Williams
(2010, 2012) constructs CCI as the latent factor identifying credit
liberalisation episodes: a spline ∑ a\_s · SDMMA\_s of smoothed-step
dummies at the institutional turning points 1979 (Campbell Committee,
end of interest ceilings), 1992 (banking distress, Aussie Home Loans),
1998 (NBFI expansion), and 2007 (GFC), estimated **jointly** across the
four-equation FIML system (consumption, house-price, mortgage-stock and
home-equity-withdrawal equations) under sign priors, with the
house-price-equation loading ζ\_h = 1 normalised. The structural reason
for the joint estimation is identification: CCI enters six channels of
the consumption equation, and (as §5 develops) the five CCI-carrying
regressors that implement those channels are between 0.66 and 0.97
mutually correlated in absolute value on this sample
(`australia_cci_interaction_corr.csv`) — each is approximately
proportional to CCI — so they cannot be separately identified off the
consumption equation alone.

A single equation cannot reproduce the FIML identification, so we proxy
or calibrate CCI by two routes:

- **Default observable proxy** (`cci_ratio`): the log of housing-credit
  flow (ABS Cat 5601.0 New Loan Commitments Value, total housing)
  divided by the eight-quarter moving average of nominal disposable
  income. Available 2002Q3 onward (n=90); used only as a *short-run*
  regressor (`d2_logcci_lag2`) in the conventional Spec 6 baseline and
  Spec 2. This is the binding 2002Q3 constraint on Spec 6 (§3 opening).
  We do not back-extend it with a mortgage-spread proxy; that option is
  retained behind a feature flag but disabled.

- **Williams-style smoothed-step spline** (`cci_williams`): smoothed-step
  (SDMMA) dummies at the Australian institutional turning points,
  reduced by general-to-specific search with sign priors enforced by
  drop-on-violation (in the spirit of Hendry–Krolzig 2005). This is the
  CCI proxy used in the faithful LIVES specification (Spec 11), where it
  also restores the autonomous-consumption loading ζ\_c · CCI
  (`cci_williams`) inside the bracket. On our 1988Q3-onward sample,
  **four** of fifteen candidate knots survive the iterated sign-prior
  reduction (australia\_williams\_cci\_knots.csv): `sdmma_2007_09` (−),
  `sdmma_2009_01` (+), `sdmma_2019_01` (−) and `sdmma_2020_04` (+); the
  1979 and 1986 knots are aliased/constant within the window and the
  remaining nine candidates violate their priors. Only one of Williams'
  four canonical knots (2007) survives on a post-1988 sample. The
  fitted series is committed (`australia_cci_williams_series.csv`; path
  figure `australia_cci_williams_path.png`) and its path is disclosed
  in §5.1.1: identically zero before 2007Q4, a plateau at its peak of 1
  over 2010Q4–2018Q4, and strongly negative (trough −2.12, settling at
  ≈ −1.63) after 2019. We report a full placebo battery on the spline
  in §5.2: the literal Williams 4-knot construction sits at the 45th
  adjusted-R² percentile of random knot draws — the detrending critique
  is vindicated for that construction — while the deployed
  iterated-reduction protocol reaches the 84th percentile, moderate
  support that we report with its protocol dependence disclosed.

The first-home-buyer share (`fhb_share = fhb_loans / total_new_loans`)
is also constructed from ABS Cat 5601.0 from 2002Q3, and enters Spec 7.

### 3.6 Williams (2009) non-property income

The Williams (2009) §4.2.1 non-property income measure (`npy_real_pc`)
adjusts gross disposable income by removing imputed property income and
a corresponding share of property-related taxation. Following Williams
(2009 p. 10):

  `npy_rec = total_income_rec − GOS_dwellings − prop_inc_rec`
  `property_tax_share = (GOS_dwellings + prop_inc_rec) / total_income_rec`
  `npy_pay = total_income_pay − prop_inc_pay − property_tax_share × income_tax_payable`
  `NPY = npy_rec − npy_pay`

The input components are sourced from ABS Cat 5206.0 Table 20:
compensation of employees, gross operating surplus on dwellings,
property income receivable, social assistance benefits, property income
payable, total income receivable, total income payable, and income tax
payable.

The implied non-property income share of disposable income averages 0.84
over 2010–2024, consistent with Williams' implicit ~0.85 weighting. We
use NPY as a robustness column (§8): in the current vintage,
substituting NPY for gross income leaves the estimated speed of
adjustment essentially unchanged (Spec 3 λ = −0.191 → −0.186;
australia\_williams\_income\_robustness.csv), with the long-run
permanent-income loading shifting modestly (+0.196 → +0.164). The income
measure remains one of the channels through which our estimates and
Williams' can diverge, and we treat it as a methodological caveat rather
than a resolved choice.

### 3.7 Permanent income

Permanent income (`yp`) is the forward-looking object in Williams' ratio
`log(yp/y)`. We construct it as a **discounted weighted average of
expected future income**, following the Italian implementation's direct
(single-regression) forecast of the discounted future-income aggregate
(De Bonis, Liberati, Muellbauer & Rondinelli 2020, Appendix A.2). The
baseline uses discount η = 0.05 (an annual discount factor δ = 0.95)
over a horizon k = 40 quarters.

Three properties of the headline measure, set out in full in §4.4, are
flagged here because they are measurement issues:

1. **Look-ahead and tail extrapolation.** The forecasting regression is
   estimated once on the full sample, so `yp_t` embeds information dated
   after `t` and is non-causal. Moreover, the realised 40-quarter-ahead
   target is computable only up to **2014Q4**, so the training sample
   ends there and the final forty quarters of `yp` — about 27 per cent
   of the estimation sample, including the entire COVID period — are
   out-of-training extrapolations from 2014-vintage coefficients. The
   real-time variant (re-estimating the forecaster on an expanding
   window) shrinks the speed of adjustment materially and reverses the
   `log(yp/y)` coefficient
   (australia\_pi\_realtime\_robustness.csv; in the Spec 6 frame,
   λ = −0.239 under the full-sample Italy measure against −0.159
   real-time Italy and −0.095 real-time AR). We flag the full-sample
   measure as the headline and the real-time column as the operational
   robustness check; headline results that depend on the look-ahead
   measure are identified as such in §7.

2. **GFC learning ogive.** The headline series is multiplied by an
   ogive that declines from 1 to 0.5 over 2008Q3–2012Q2, so the
   regressor is *half* the raw discounted-gap measure over the
   post-2012 two-thirds of the sample. A no-ogive re-estimate of the
   headline specification
   (australia\_spec11\_ogive\_robustness.csv) leaves the structural
   conclusions unchanged (§4.4).

3. **Forecaster choice and the Australian permanent-income puzzle.**
   Under an AR forecaster the `log(yp/y)` coefficient is *negative*
   (the "Australian permanent-income puzzle"); under the Italy-style
   direct forecaster it is positive, and the Italy forecaster fits
   better (australia\_pi\_method\_meta.csv: AR adj-R² = 0.696, Italy
   adj-R² = 0.731). The faithful LIVES specification uses the
   Italy-style measure and recovers a strong, correctly signed
   permanent-income loading (Spec 11 full-sample `ln_yp_over_y` =
   +0.459, t = 4.04, ***).

On the discount and horizon settings, the δ/k sensitivity grid
(australia\_permanent\_income\_sensitivity.csv; δ ∈ {0.90, 0.95, 0.97},
k ∈ {20, 40, 60}, ogive on/off) is run under the **AR constructor** in
the Spec 2 frame, where λ moves only at the third decimal. The grid
does **not** cover the headline Italy-style method; the
headline-method sensitivities are the no-ogive column and the
real-time variant of point 1 above. The forecaster *method* (Italy vs
AR, full-sample vs real-time), not the discount calibration, is the
material choice.

In the canonical LIVES bracket the permanent-income weight is itself
credit-dependent, ψ(CCI) = ψ₀ + ψ₁·CCI with ψ₀ = 0.20 and ψ₁ = 0.93
**calibrated** (not freely estimated) by Williams. As §7 and §9 develop,
this is the calibration that most clearly does not transfer: the freely
estimated Australian gearing is far above Williams' ψ₀ = 0.20 (at or
above unity in structural terms, §4.4), and imposing Williams' values
collapses the error-correction term. The data section flags only that
ψ(CCI) is calibrated in Williams' original — the estimation
consequences are reported in the results.

### 3.8 Demographics and dummies

The prime-working-age share (`prime_age_share` = age 25–54 share of
total ERP) is constructed from ABS Cat 3101.0 single-year-of-age
cohorts (Male + Female, summed; ratios are robust to the truncation
discussed in §3.1). Annual data are interpolated to quarterly via cubic
spline.

The canonical LIVES equation also contains two demographic terms in the
long-run bracket — D4 DEMFTB (the first-home-buyer / demographic cohort)
and D4 WAPOP (working-age population growth) — for which Williams reports
α₂ = −0.138 and α₃ = −0.069. These are not directly constructed in the
single-equation specifications; `prime_age_share` (Spec 7) and the
first-home-buyer share (`fhb_share`, Spec 7) stand in for the cohort
channel, and we treat the full demographic block as a target for the
FIML system rather than the single equation.

Australia-specific narrative dummies enter the default dummy set:

- `d_neg_gearing_8587`: 1985Q3–1987Q3 negative-gearing tax restriction
- `d_recession_1991`: 1991Q2 ("recession we had to have")
- `d_apra_2014`: 2014Q4 macroprudential investor-loan-growth cap
  (smoothed-step ogive)
- `d_apra_2017`: 2017Q2 macroprudential interest-only-loan cap
  (smoothed-step ogive)
- `d_jobkeeper_2020`: 2020Q2–2021Q1 JobKeeper income support

Together with the standard set (`d2000_gst`, `d2008_gfc`, `d2020_covid`,
`d2020_rebound`), these constitute the full dummy set; zero-variance
dummies are silently dropped per specification. The COVID quarters carry
a very large dummy (`d2020_covid` ≈ −0.155 in Spec 11), and the
COVID-inflated full sample fails the upper-bound λ screen; we therefore
treat the **pre-COVID** sub-sample (n=126) as the identified window for
the speed of adjustment (§7).

### 3.9 Coverage and reproducibility

The full reproducibility kit (R 4.5.3, renv-pinned dependencies, raw ABS
workbooks, project-supplied CSVs, master quarterly dataset as CSV and
RDS, full estimation pipeline with three execution modes, GitHub Actions
CI, testthat suite) accompanies this paper. The master dataset has **194
quarters × ~120 columns** and is available as a portable CSV for
hand-editing or off-line replay. See Appendix A for each variable's
source identifier, vintage, and splicing recipe.

### 3.10 RBA D-tables

Three RBA historical statistical tables support the pre-1988 portion of
the sample.

#### 3.10.1 RBA D03 — Monetary aggregates (M3)

The M3 monetary aggregate (`m3_aggregate`) is loaded from `d03hist.xlsx`,
series `DMAM3N` (M3, original/not-seasonally-adjusted, $ billion). M3 is
total economy-wide currency plus transaction deposits plus all other
deposits at ADIs, plus certificates of deposit issued by ADIs. Coverage:
monthly, **1959Q3–2026Q1**, aggregated to quarterly by mean of the three
monthly observations.

An earlier draft described the series as continuous with no breaks. That
is **incorrect**: `DMAM3N` carries a definitional series break at
**August 1976**, when the level jumps by +14.25 log per cent
month-on-month (computed directly from `d03hist.xlsx`, against a mean
monthly log change of 0.8 per cent over the full history). The break
falls inside the opening quarters of the 1976Q3+ spine: it makes the
1976Q4 quarterly log-difference of M3 (+9.5 log per cent) an outlier of
roughly 4.8 standard deviations of the series' quarterly growth
distribution over the spine window, and the outlier propagates into
every M3-based back-extension proxy (`m3_household_proxy`,
`fin_deposits_proxy`, `nla_y_proxy`, `networth_y_proxy`) at exactly the
deepest end of the back-extended sample. Back-extension results that
lean on the first spine quarters should be read with this break in
mind.

M3 is the headline liquid-asset proxy for the pre-1988 portion of the
sample where ABS sectoral household-balance-sheet deposits are not
available. Williams (2010) used the same M3 series, multiplied by the
household share of factor income, for his pre-1988 splice.

#### 3.10.2 RBA D02 — Lending and credit aggregates (total credit)

Total credit (`credit_total_d02`) is constructed by growth-rate-splicing
two RBA D02 series across the July 2019 RBA conceptual reform:

- `DLCACN` (Total credit, original, $ billion) for 1976Q3–2019Q2
- `DLCACSFN` (Total credit including select financial businesses, the
  post-2019 successor) for 2019Q3+

The two series have no quarterly overlap (DLCACN ends 2019-06, DLCACSFN
starts 2019-07). The splice anchors levels at the boundary
(`first_post × pre[t] / last_pre`) so the join is continuous in level;
the implicit growth rate at the boundary is exactly zero (no overlap to
estimate it from). For analyses that hinge on the 2019Q2→Q3 quarter
specifically, treat with caution. Coverage 1976Q3+.

This is total credit, not housing-specific. The housing-specific series
in D02 (`DLCACOHN`, `DLCACIHN`) only goes back to 1990Q1 in the current
vintage and so cannot extend the housing-credit history pre-1990.

#### 3.10.3 RBA D01 — Growth in selected financial aggregates

Downloaded as `d01hist.xlsx` (monthly growth rates of the same
aggregates whose levels are in D02). Williams (2010) used D01
housing-credit growth rates to back-cast D02 levels pre-1976; in the
current vintage D02 already extends to 1976Q3, so D01 is not strictly
needed for back-extension. Retained for future use.

### 3.11 Pre-1978 labour force (`labour_force_historic.csv`)

A user-supplied CSV (`labour_force_historic.csv`, 188 quarterly rows
1964Q3–2011Q2) provides:

| Column | Definition | Units |
|--------|------------|-------|
| `pop_total` | Total resident population (annual + interpolation) | thousands |
| `pop_15_64` | Working-age population (15–64) | thousands |
| `labour_force` | Civilian labour force | thousands |
| `unemployed` | Unemployed persons (level) | thousands |

Provenance: the source compilation pulls together pre-1978 series from
ABS Cat 6204.0 (historical labour force, archived), ABS Year Book
Australia, the Foster (1996) *Australian Economic Statistics 1949–50 to
1996–97* compilation, and RBA Occasional Paper No 8. Conceptually
consistent with the current ABS Cat 6202.0 series (which begins
Feb 1978) at the join.

The historic series are growth-rate-spliced onto the modern (1978+)
series at 1978Q1:

- `pop_15_64` → `pop_millions` (the working-age population denominator
  used in per-capita normalisation)
- `labour_force` → master `labour_force`
- `unemployed/labour_force × 100` → master `unemp_rate` (level
  replacement before 1978Q1)

A side-effect of this splice is that the six quarters 1976Q3–1977Q4 now
have non-NA values for all per-capita and labour-force-derived variables
(`cons_real_pc`, `ydi_real_pc`, `npy_real_pc`, `lf_share`, etc.). On the
data download, `cons_real`, `ydi_nom`, `mortgage_rate`, `hpi`, M3, total
credit, and `prime_age_share` all extend to 1976Q3 already; the
labour-force splice was the binding remaining constraint. The 1976Q3
back-extension — enabling the direct sample-length test reported in §8 —
is one of the contributions of this paper.

`pop_total` is also exposed in the master as `pop_total_thousands`
(historic-only 1964Q3–2011Q2 because there is no modern 1978+ counterpart
in ABS 6202). It can be used as an alternative per-capita denominator
(total resident vs civilian 15+) for sensitivity analyses.

### 3.12 Household-allocated M3 (`m3_household_proxy`)

The pre-1988 liquid-asset proxy is constructed by allocating M3 to the
household sector via the wage share of GDP:

```
m3_household_proxy = m3_aggregate × wage_share / 100
```

Williams (2010) uses the household factor-income share for this
allocation. The wage share alone (compensation of employees / GDP) is a
defensible approximation: it captures most of household factor income
(wages dominate Australian household income; mixed income + property
income receivable add ~10 percentage points and track wage share over
time). **This is a documented simplification for our phase-1
back-extension**; for a fully Williams-faithful splice, replace
`wage_share` with the broader `(compensation + mixed income + property
income receivable) / GDP` series. We flag it explicitly here because the
back-extension proxies should be read as approximations adequate for the
direct sample-length test in §8, not as substitutes for the official
1988Q3+ series.

`wage_share` itself is loaded from a user-supplied CSV
(`household_income.csv`) which extracts ABS Cat 5206.0 Table 24
analytical series A2302604K (compensation of employees, % of GDP),
1959Q3–2024Q4. Range over the sample: 49–62%, with values ~60% in the
1970s falling to ~50% today (capital-share rise).

Coverage of `m3_household_proxy`: 1976Q3+ (limited by `wage_share` in the
supplied CSV), values $22 billion (1976Q3) to $1,673 billion (2024Q4).

### 3.13 Aggregate net-worth proxy (`networth_y_proxy`)

To enable Spec 1–3 fits on the back-extended sample, we construct an
aggregate net-worth proxy that:

1. Uses M3-allocated-to-households (`m3_household_proxy`) plus a
   `hpi × pop_millions` back-cast of housing wealth as the raw wealth
   aggregate.
2. Growth-rate-splices that raw aggregate ratio onto the official
   `networth_y` at 1988Q3 (so the proxy equals the official series from
   1988Q3 onwards and back-casts smoothly through 1976Q3).

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
raw_proxy[t]    = (m3_household_proxy[t] × 1000 + housing_wealth_proxy[t]) / ydi_ann_nom[t]
scale           = networth_y[1988Q3] / raw_proxy[1988Q3]
networth_y_proxy[t]
                = networth_y[t]              for t >= 1988Q3
                = raw_proxy[t] × scale       for t < 1988Q3
```

The `× 1000` term is a units conversion the code applies before the sum:
`m3_household_proxy` is carried in $ billion (RBA D03 units, never
rescaled), whereas `housing_wealth_proxy` and `ydi_ann_nom` are in
$ million; without the conversion the M3 term would contribute ~0.01 per
cent of the numerator instead of its intended ~12 per cent and the
aggregate proxy would collapse to a housing-only back-cast.

Caveats explicitly documented in the data appendix:

- The back-cast omits equities and super (quantitatively small in
  1976–1988 — Australian super pre-Superannuation Guarantee 1992 was a
  negligible household asset class).
- The back-cast omits debt netting (mortgage debt was a much smaller
  share of household balance sheets in the 1970s than today).
- Use only for back-extension exercises; never as a substitute for
  `networth_y` on the modern sample where the official series exists.

The proxy values across key dates: 5.05× annual income (1976Q3), 4.71
(1980Q1), 4.78 (1985Q1), 5.37 (1988Q3 — anchored to official), 10.19
(2024Q4). The shape is consistent with the historical Australian
wealth-to-income trend (flat through the 1970s/early 80s, sharply rising
post-1985 financial deregulation).

### 3.14 Disaggregated wealth proxies

Four additional proxies extend the disaggregated wealth components to the
back-extended 1976Q3+ sample. Each equals the official series for
t ≥ 1988Q3 by construction; for t < 1988Q3 it back-casts via the most
relevant available aggregate. These proxies enable the direct
sample-length test of §8 (back-extension on Spec 1/4), where the honest
finding is that lengthening the sample moves λ partway towards Williams
but does *not* sharpen the individual wealth channels — sample length is
not the binding constraint; the binding constraints are the
post-deregulation data window and the interaction collinearity.

**`ha_y_proxy`** = `housing_wealth_proxy / ydi_ann_nom`. Uses the
hpi×pop back-cast described in §3.13. Values: 2.68 (1976Q3) → 2.83
(1988Q3) → 6.41 (2024Q4).

**`fin_deposits_proxy`** anchors `fin_deposits[1988Q3]` and grows by
`m3_household_proxy`:

```
fin_deposits_proxy[t] = fin_deposits[1988Q3]
                      × m3_household_proxy[t] / m3_household_proxy[1988Q3]
                      for t < 1988Q3
```

**`fin_loans_proxy`** anchors `fin_loans[1988Q3]` and grows by RBA total
credit:

```
fin_loans_proxy[t] = fin_loans[1988Q3]
                   × credit_total_d02[t] / credit_total_d02[1988Q3]
                   for t < 1988Q3
```

**`nla_y_proxy`** = `(fin_deposits_proxy − fin_loans_proxy) / ydi_ann_nom`,
again on the deposits-net-of-debt convention of §3.2.2. Values: +0.20
(1976Q3, households are net liquid creditors) → −0.05 (1988Q3) → −0.72
(2024Q4, modern net-debtor position). The sign-flip around 1988 captures
the post-deregulation debt build-up — and is precisely the variation that
the credit-conditions channel is meant to explain but which the modern
data window mostly excludes.

**`eq_y_proxy`**: held constant at the 1988Q3 value pre-1988 (Option B
in our methodology — Australian household equity holdings were a small
wealth share in the late 1970s/early 80s; the constant assumption
introduces little level error and is straightforward to upgrade to an
ASX All-Ordinaries back-cast). Value: 0.60 (constant) → 0.92 (2024Q4).

**`super_y_proxy`**: linear ramp from 0.1× the 1988Q3 value at 1976Q3 to
the 1988Q3 value, then official thereafter. The 0.1 anchor matches
Williams (2010) Table A.1 ballpark for the pre-Superannuation-Guarantee
era (SGC mandate 1992). Values: 0.07 (1976Q3) → 0.66 (1988Q3) → 2.44
(2024Q4).

We flag these back-extension proxies honestly: the equities proxy is a
constant-share assumption, the superannuation proxy is a linear ramp, and
both the deposits and debt proxies are aggregate-growth back-casts rather
than household-sector measurements. They are adequate for testing whether
sample length is the binding constraint (it is not — §8) but they should
not be over-interpreted as reconstructions of the 1970s household balance
sheet. For the faithful LIVES specification (Spec 11) and all headline
results we use the **official** 1988Q3+ disaggregated series and the
n=146 / n=126 windows; the proxies are confined to the back-extension
robustness exercises.

#### 3.14.1 Coherence check — disagg sum vs aggregate proxy

Both `networth_y_proxy` (M3-allocated + housing back-cast,
growth-rate-spliced onto the official broad networth) and the
**sum-of-disaggregated** `networth_y_disagg_proxy` =
`ha_y_proxy + nla_y_proxy + eq_y_proxy + super_y_proxy` are exposed in
the master.

At 1988Q3 (the boundary):

- official `networth_y` (broad, ABS A83722648X closing net worth):
  **5.37**
- `networth_y_proxy` (aggregate, anchored to broad): **5.37** by
  construction
- `networth_y_disagg_proxy` (narrow, sum of components): **4.04**

The roughly 25 per cent gap between the aggregate and the disaggregated
sum is the "other wealth" component of ABS closing net worth (life-office
reserves, unincorporated business equity, etc.) absent from the narrow
definition. For Spec 4–7 and the faithful LIVES specification (Spec 11),
which use the disaggregated components individually, the disagg sum is
the implicit reference; for Spec 1–3 the aggregate proxy is used.

### 3.15 Master variable coverage tiers (under 1976Q3+ spine)

After all splicing, master variable coverage falls into the following
tiers (australia\_model\_dataset.csv):

| First non-NA | n  | Variables (selected) |
|--------------|---:|----------------------|
| 1976Q3       | ~56 | cons, ydi, hpi, mortgage_rate, M3, total credit, prime_age_share, all dummies, m3_household_proxy, ha_y_proxy, nla_y_proxy, eq_y_proxy, super_y_proxy, networth_y_proxy, ln_networth_y_proxy, cci_kalman, cons_real_pc, ydi_real_pc, npy_real_pc, labour_force, unemp_rate, lf_share, pop_millions, ln_cons_real_pc, ln_ydi_real_pc, ln_hp_over_y, ecm_lag (1976Q4) |
| 1977Q3       |  2 | real_rate, hicp_4q_ann (4-quarter CPI lag) |
| 1978Q2       |  1 | ydi_ann_8qma (8-quarter MA) |
| 1988Q3       | ~21 | OFFICIAL disaggregated wealth (ha_y, eq_y, super_y, **ilfa_y = eq_y + super_y**, nla_y, networth_y, debt_y), housing_wealth, fin_deposits/equities/super/loans (RAW from ABS 5232) — n=146 |
| 2002Q3       |  5 | cci_ratio, fhb_share, housing_loan_flow, fhb_loans, non_fhb_loans — n=90 |
| 2009Q1       |  2 | mortgage_interest_burden_rba, mortgage_payment_burden_rba (RBA E13) — n=64 |

The 1988Q3 tier is the binding tier for the faithful LIVES specification
(n=146 full; n=126 pre-COVID), and the 2002Q3 tier binds the conventional
Spec 6 baseline (n=86 full; n=66 pre-COVID). The combined illiquid
financial ratio `ilfa_y` first becomes available at 1988Q3, alongside its
`eq_y` and `super_y` constituents.


## 4. Model

### 4.1 The canonical LIVES consumption equation

The decisive contribution of this paper is that the *functional form* of
the Muellbauer-Williams LIVES ("Latent Interactive Variable Equation
System") equation is what identifies it. We therefore present the
faithful form of Williams' (2010) eq. (7) as the model, and present the
generic constant-MPC error-correction model that an earlier draft and
much of the Australian literature treat as "the LIVES equation" as a
nested baseline (Spec 6, §4.5) that the theory predicts should *not*
behave like LIVES.

In its canonical form the LIVES consumption equation is

> Δln c_t = φ · [ ζ_c · CCI_t
>         + α_1 · r_t · CCI_t
>         + γ_1 · CCI_t · (HA/4y)_{t-1}
>         + γ_2 · (IFA/4y)_{t-1}
>         + γ_3 · (NLA/4y)_{t-1}
>         + ψ(CCI_t) · ln(y^p_t / y_t)
>         + α_2 · Δ_4 DEMFTB_t
>         + α_3 · Δ_4 WAPOP_t
>         + α_4 · (1 − ϖ·CCI_t) · ln(p^h/y)_{t-1}
>         + ln y_t − ln c_{t-1} ]
>     + β_1 · DSRISK_t
>     + β_2 · (1 − ϖ·CCI_t) · Δ_8 ln ue_t
>     + β_3 · Δ_4 ln c_{t-1}
>     + Σ_k δ_k D_kt + ε_t

where

- `c_t` is real per capita household consumption and `y_t` is real per
  capita household disposable income;
- `CCI_t` is the credit conditions index (a spline of smoothed-step
  dummies under sign-prior GETS reduction, identified jointly across the
  four-equation FIML system; see §5);
- `(HA/4y)`, `(IFA/4y)`, `(NLA/4y)` are housing, illiquid financial and
  net liquid wealth, each scaled by *annualised* income (the prior
  quarter's stock divided by `4·y`; see §4.6);
- `r_t` is the ex post real mortgage rate;
- `p^h_t` is the real house price index, so `ln(p^h/y)` is the
  affordability / down-payment ratio;
- `y^p_t` is permanent income — the discounted, expected-future-income
  measure of §4.4 — so `ln(y^p/y)` is the permanent-to-current income
  gap;
- `DEMFTB`, `WAPOP`, `DSRISK` and `Δ_4 ln c_{t-1}` are the demographic
  first-home-buyer cohort, working-age population, downside-risk and
  durables-habit terms of Williams' eq. (7);
- `φ < 0` is the speed of adjustment (the coefficient `λ` in our
  reporting convention, §4.3), and `ζ_c, α_1, γ_1, γ_2, γ_3, α_2, α_3,
  α_4` and the function `ψ(·)` are the long-run "structural" parameters;
- `D_kt` are the narrative dummies of §3.7.

Three features of this form are load-bearing and distinguish it sharply
from the conventional wealth-effect ECM.

**(i) Credit conditions multiply six channels jointly.** `CCI` enters
the long-run bracket six times — as the autonomous-consumption loading
`ζ_c·CCI`, the rate interaction `α_1·r·CCI`, the housing-collateral
interaction `γ_1·CCI·(HA/4y)`, inside the permanent-income gearing
`ψ(CCI)`, and inside the affordability term through `(1−ϖ·CCI)`. The
credit channel is therefore not an additive shifter but a set of
*interactions* that switch the long-run wealth and income channels on
and off as credit conditions ease or tighten.

**(ii) There is no classical housing wealth effect.** Housing wealth
enters *only* through `γ_1·CCI·(HA/4y)`. The marginal propensity to
consume out of housing wealth is identically zero when `CCI = 0` and is
unlocked as credit conditions ease: it is a collateral / housing-equity-
withdrawal channel, not a pure wealth effect. This is the theoretical
core of the form-is-decisive result. A specification that enters a
standalone `γ_HA·(HA/4y)` level term and reads its coefficient as the
housing wealth effect is mis-testing the theory: theory predicts that
standalone coefficient is approximately zero in the absence of the `CCI`
interaction, so reading an insignificant standalone `ha_y` as a *failed*
housing wealth effect is a category error. Williams' own peak housing
MPC (`γ_1` evaluated at the credit-loose regime) is 0.0488 — a number
that can only be recovered through the interaction, never through a level
term.

**(iii) Illiquid financial and net liquid wealth enter as plain MPCs;
income enters with a unit coefficient.** Illiquid financial wealth
(`IFA`, equities plus superannuation) and net liquid wealth
(`NLA`, liquid assets minus total household debt) enter the long-run
bracket as *un-interacted* marginal propensities `γ_2` and `γ_3`
(Williams' calibrated 0.022 and estimated 0.159 respectively). Current
income `ln y_t` enters the bracket with a coefficient restricted to
unity, against `−ln c_{t-1}`, so the error-correction term is the
log consumption-to-income ratio `ln(c_{t-1}/y_t)` (equivalently
`ln(c_{t-1}) − ln(y_t)`). This unit restriction is what makes the
equilibrium a stationary consumption-to-income ratio in the LIVES
tradition, rather than a free cointegrating vector.

The permanent-income gearing is itself credit-dependent:

> ψ(CCI_t) = ψ_0 + ψ_1 · CCI_t

so that the weight on the permanent-to-current income gap rises with
credit availability. Williams *calibrates* `ψ_0 = 0.20` and `ψ_1 = 0.93`
(the theoretical ceiling `ψ(CCI) ≤ 0.95` motivates these values — freely
estimated, ψ would exceed it — though his own CCI series empirically
peaks below its theoretical maximum, so the ceiling is not actually
reached within his estimation sample); these are
not free parameters in his system. The affordability multiplier is
`ϖ = 1.2`, also fixed. We treat the distinction between *structure*
(the interactions and the unit income restriction) and *calibration*
(the specific values `ψ_0`, `ψ_1`, `γ_2`, `α_1`, `ϖ`) as central, and
test it directly in §4.5 and the results.

### 4.2 Recovery of structural coefficients

We estimate the equation by OLS — Δln c on the contemporaneous and
lagged right-hand-side terms — with Newey-West HAC standard errors
throughout (heteroskedasticity is structural in every full-sample
specification; see §7.2). Because the entire long-run bracket is
multiplied by `φ`, the OLS coefficient on each long-run regressor is
`φ` times its structural value. We therefore recover each structural
parameter as

> structural γ_i = OLS coefficient on regressor i / |λ|

and report both forms throughout, so that the speed-of-adjustment
channel (`λ`) and the long-run-coefficient channel are separable. For
example, the faithful specification's full-sample net-liquid OLS
coefficient `+0.0269` and `λ = −0.448` imply a structural NLA marginal
propensity of `0.0269 / 0.448 = 0.060` (§7.0). The same rule applies to
the permanent-income term — a discipline §7.0 enforces when comparing
the gearing with Williams' calibrated `ψ_0`. The structural-recovery
identity also makes precise *why* imposing Williams' calibrations can
collapse the equation: fixing several `γ_i` while iterating to the
fixed-point implied by the unit income restriction over-determines the
bracket, and the only free margin left — `λ` — adjusts toward zero
(§4.5, §7.0.1).

### 4.3 Reporting and sign conventions

Throughout, `λ` denotes the `ecm_lag` coefficient (the OLS coefficient
on `ln(c_{t-1}) − ln y_t`), with `λ < 0` indicating stable error
correction. We report the faithful specification's speed of adjustment
as `λ = −0.448` (t = −3.57) on the full sample and `λ = −0.266`
(t = −4.85) on the pre-COVID sample (`australia_all_results.csv`). We
treat the COVID-controlled value of about `−0.25` as the *identified*
speed of adjustment: it is nearly invariant to the COVID treatment
(−0.266 pre-COVID, −0.248 excluding 2020Q1–2021Q4, −0.242 under
quarterly 2020–21 dummies; `australia_spec11_variants.csv`), whereas
the full-sample estimate is inflated by the COVID quarters — the
three pulse dummies the full-sample specification carries are
demonstrably insufficient, since replacing them with quarterly dummies
halves `|λ|` — and fails the upper-bound `|λ|` screen of §6.2. The
identified `−0.25` sits close to Williams' (2010) estimated speed of
`φ_c = −0.286`. Where a headline number depends on the full-sample,
non-causal permanent-income measure (§4.4) we say so explicitly and
point to the real-time robustness column (§8).

### 4.4 Permanent-income forecasting

Permanent income `y^p_t` is the discounted weighted average of *expected*
future log income over a 40-quarter horizon (`k = 40q`) at quarterly
discount factor `δ_q = 0.95^(1/4)` — i.e. an annual discount factor
`δ = 0.95`, equivalently an annual discount rate `η = 0.05`:

> ln(y^p_t / y_t) = E_t [ Σ_{h=1}^{40} w_h ln(y_{t+h}) ] − ln(y_t),
>     with w_h = δ_q^(h-1) / Σ_{h=1}^{40} δ_q^(h-1).

We implement two forecasters. The headline measure is the Italy-style
direct forecast of the discounted aggregate (De Bonis, Liberati,
Muellbauer and Rondinelli 2020, Appendix A.2); the rolling AR(8)
forecaster is reported as a methodology-robustness column (§8).

- **Italy direct forecast (headline measure).** For each `t` whose
  40-quarter-ahead horizon is observable, the discounted weighted
  average of future log income is computed and regressed, in a single
  full-sample equation, on a richer predictor set including
  `log(lf_share)` (the labour-force-participation term that captures
  slow-moving demographic effects on trend income), a trend, a
  post-2008 split trend, the four-quarter moving average of log income,
  the unemployment rate, and four-quarter-difference dynamics. The
  permanent-income series is the fitted value of this single
  regression. Three properties of the construction are disclosed
  explicitly. *(i) Look-ahead:* because the coefficients are estimated
  over the whole sample, `y^p_t` embeds information dated after `t` and
  is non-causal — a two-sided *measurement* of permanent income rather
  than a real-time forecast. *(ii) Tail extrapolation:* the realised
  40-quarter target is computable only up to 2014Q4, so the training
  sample ends there and the final forty quarters of `y^p` — about 27
  per cent of the estimation sample, including the entire COVID
  period — are out-of-training predictions from 2014-vintage
  coefficients, driven mainly by the deterministic trend terms.
  *(iii) GFC learning weight:* the series is multiplied by an ogive
  that declines from 1 to 0.5 over 2008Q3–2012Q2 (mirroring the
  treatment in the AR forecaster), so the headline regressor is HALF
  the raw discounted-gap measure over the post-2012 two-thirds of the
  sample; a no-ogive re-estimate of the headline specification is
  reported as a robustness column
  (`australia_spec11_ogive_robustness.csv`) and leaves the structural
  conclusions unchanged. We carry the full-sample measure as the
  headline but report a causal, expanding-window variant (re-fitting at
  each `t` only on observations whose full horizon is realised by `t`)
  as the operational robustness column. The real-time variant shrinks
  `|λ|` materially and reverses the permanent-income coefficient (§7.4):
  in the Spec 6 frame the full-sample Italy measure delivers
  `λ = −0.239` against `λ = −0.159` real-time and `λ = −0.095` under
  the real-time AR forecaster
  (`australia_pi_realtime_robustness.csv`). Any forecasting use of the
  equation — embedding it in MARTIN, in particular — requires the
  real-time variant or the AR forecaster, not the full-sample measure.

- **Rolling AR(8) (robustness).** A rolling AR(8) regression of log
  income on eight own lags plus a linear trend, a post-2008Q3 step
  dummy, and a trend-break interaction; forecasts are aggregated over the
  40 horizons using the discount weights, with optional `unemp_rate`,
  `log_oil`, `log_reer`, `log_stocks` predictors and the same 2008Q3
  ogive learning weight. Under this forecaster the long-run
  permanent-income coefficient turns negative — the "Australian
  permanent-income puzzle" of §8 — a sign reversal absent under the
  Italy measure.

The permanent-income discount and horizon settings are not load-bearing
for the speed of adjustment: across `δ ∈ {0.90, 0.95, 0.97}`,
`k ∈ {20, 40, 60}` and the ogive on/off toggle, `λ` in the AR/Spec 2
frame moves only at the third decimal
(`australia_permanent_income_sensitivity.csv`; the grid is run under
the AR constructor — the headline-method sensitivities are the ogive
column above and the real-time column of §7.4). The forecaster *method*
(Italy vs AR, full-sample vs real-time), not the discount calibration,
is the material choice.

In the faithful specification the permanent-income channel is strong
and correctly signed: `ln(y^p/y)` enters with OLS level coefficient
`+0.459` (t = 4.04) on the full sample and `+0.298` (t = 5.81)
pre-COVID (`australia_all_results.csv`). Two scale points discipline
the comparison with Williams. First, by the structural-recovery rule of
§4.2 the implied gearing is OLS/|λ| ≈ `1.02` (full) to `1.13`
(COVID-controlled) — **above the theoretical admissibility bound
`ψ ≤ 1 − η ≈ 0.95`**. We do not hide this: it survives removing the
ogive (structural ≈ 1.05; `australia_spec11_ogive_robustness.csv`), and
§7.0 discusses candidate explanations, including the measure's tail
extrapolation and the unit-income restriction forcing the
permanent-income gap to absorb low-frequency consumption-to-income
drift. Second, any comparison with Williams' calibrated `ψ_0 = 0.20`
must be scale-consistent: structural-to-structural the Australian
gearing is roughly five times his; OLS-to-OLS-equivalent the comparison
is +0.46 against `φ·ψ_0 ≈ 0.057`. Spec 11 does not separately estimate
the `ψ_0`/`ψ_1` split: the credit-geared component enters through a
separate `yp_x_cci` interaction whose full-sample coefficient
(`−0.510`, t = −1.51) is insignificant and wrong-signed relative to
Williams' calibrated `ψ_1 = +0.93` — though it turns right-signed
(+0.19, t = 1.80) on the pre-COVID sample — so the data identify the
level of the permanent-income gearing but not its credit slope on a
single equation. We return to this in §7.0.1: imposing Williams' `ψ_0`,
`ψ_1` and `γ_2` calibrations is what collapses the equilibrium.

### 4.5 The conventional constant-MPC ECM as nested baseline (Spec 6)

The conventional disaggregated wealth-effect error-correction model —
which prior Australian work and an earlier draft of this paper treated as
the LIVES equation — replaces the credit interactions with plain,
constant marginal propensities:

> Δln c_t = λ · [ α_0 + γ_HA · (HA/4y)_{t-1} + γ_eq · (eq/4y)_{t-1}
>           + γ_super · (super/4y)_{t-1} + γ_NLA · (NLA/4y)_{t-1}
>           + γ_HP · ln(p^h/y)_{t-1} + α_r · r_t
>           + ψ · ln(y^p/y)_t + ln y_t − ln c_{t-1} ]
>       + θ · Δ_2 ln CCI_{t-2} + Σ_j β_j Z_jt + Σ_k δ_k D_kt + ε_t

Here every wealth term enters as a standalone level with a *constant*
MPC and *no* credit scaling; credit conditions appear only through the
short-run term `Δ_2 ln CCI_{t-2}` (`d2_logcci_lag2`). This is **not** the
LIVES equation — it is a generic constant-MPC wealth ECM, and we label it
the *conventional baseline* (Spec 6), not the preferred or headline
specification. It is theoretically nested in the canonical form only in
the degenerate sense that `γ_1·CCI·(HA/4y)` would reduce to a constant
MPC if `CCI` were constant; on post-deregulation Australian data, where
the credit liberalisation that identifies the channels largely predates
the 1988Q3 start of ABS sectoral balance-sheet data (§3), that
reduction is precisely what kills the housing-collateral channel.

The implications follow directly. The conventional baseline delivers a
speed of adjustment of `λ = −0.239` (t = −2.55, full sample, n = 86;
`australia_all_results.csv`) — significant, but estimated on a sample
less than two-thirds the faithful specification's, to which its
short-run credit term binds it — and an insignificant standalone
housing coefficient (`ha_y = +0.0022`, t = 0.30). Reading that insignificant
`ha_y` as a *failed housing wealth effect* is the category error of §4.1:
the LIVES form predicts that very coefficient is approximately zero
absent the `CCI` interaction. The faithful specification, by contrast,
re-routes housing through `γ_1·CCI·(HA/4y)` alone, restores the
autonomous-consumption loading `ζ_c·CCI`, and combines illiquid
financial wealth (`IFA = eq + super`); the error-correction and core
wealth structure then come alive (§7.0). We retain Spec 6 throughout as
the conventional comparator against which the faithful form is the
correctly-specified alternative — a matter of theory, not of fit.

### 4.6 Wealth definitions

Wealth enters as asset-to-annualised-income *ratios* — the
end-of-quarter stock divided by four times current quarterly income,
`x_t / (4·y_t)`. This is *contemporaneous* (closing-balance-sheet)
timing: Williams' canonical equation dates the stock at `t−1`, and an
earlier draft of this paper described our ratios that way, but the
implementation uses the current quarter's closing stocks throughout.
Because ABS 5232 stocks are closing values, the contemporaneous ratio
embeds within-quarter revaluations, which weakens the predeterminedness
defence of OLS for the wealth terms; the IV exercise of §8.1 is the
corresponding robustness check. The three components are

- **Housing wealth (HA):** the value of the household-sector dwelling
  stock; it enters the faithful specification only through
  `γ_1·CCI·(HA/4y)`.
- **Illiquid financial wealth (IFA):** equities plus superannuation; it
  enters as a plain MPC `γ_2` (combined as `ilfa_y = eq_y + super_y` in
  the faithful specification; entered separately as `eq_y` and `super_y`
  in the conventional baseline).
- **Net liquid wealth (NLA):** liquid assets minus total household debt,
  entered as a plain MPC `γ_3`.

For NLA we follow the Italian implementation (De Bonis et al. 2020 §2;
Williams 2010) in netting debt against liquid assets, and we test the
underlying restriction `γ_LA + γ_LOANS = 0` directly by refitting the
disaggregated specifications with liquid assets and debt entered
separately and conducting a Wald test of equal-and-opposite
coefficients (`australia_nla_restriction_test.csv`). The restriction is
not rejected in any specification or sample: in the conventional
baseline the sum is `0.105` (SE 0.103, t = 1.02, p = 0.31) on the full
sample and `−0.020` (t = −0.38, p = 0.71) pre-COVID, and Spec 4 and
Spec 5 likewise accept (all six rows `restriction_accepted = TRUE`). The
honest reading is that the data cannot distinguish separate liquid-asset
and debt MPCs — non-rejection by imprecision rather than confirmation of
exact netting — which supports the NLA aggregation as a parsimonious
choice rather than as a tested structural equality.

### 4.7 The de-meaning convention for the interactions

Following Williams' own convention, it is the *interacted economic
variable*, not the CCI, that is de-meaned. The housing-collateral
regressor is `ha_x_cci = (HA/4y − mean(HA/4y))·CCI`, with the mean
taken over the 1980Q1-onwards estimation window on which the CCI is
observed; the rate and permanent-income interactions are
`r_x_cci = (r − r̄)·CCI` and `yp_x_cci = (ln(y^p/y) − m̄)·CCI`, and the
affordability term is
`hp_x_1_minus_cci = (ln(p^h/y) − m̄)·(1 − 1.2·CCI)`. The CCI level
`cci_williams` enters Spec 11 *raw* (peak-normalised, not de-meaned) as
the autonomous-consumption loading. (An earlier draft described the
convention backwards — de-meaned CCI times the raw variable — which is
a materially different regression for the housing and rate channels,
whose levels Spec 11 deliberately excludes; the description here
matches the code.) Under this convention the interaction coefficients
are the credit loadings evaluated at the sample-mean value of the
interacted variable, and — because the deployed CCI is zero before
2007Q3 (§5.1.1) — the un-interacted coefficients (`γ_2`, `γ_3`, the
intercept, `ψ̂_0`) are marginal propensities and gearings *at CCI = 0*,
i.e. in the pre-2007 credit regime. Two consequences matter for
interpretation. First, a pure re-centring of the credit regime is
absorbed by the included CCI level, so the housing-collateral
coefficient is invariant to the de-meaning constant; this is not true
of specifications that omit the level. (The near-zero CCI
counterfactual of §10 is run on the conventional baseline (Spec 6),
where `CCI` enters only the short run and so has no long-run channel to
integrate — confirming Spec 6 has no operative housing-collateral
channel.) Second, de-meaning does not break the near-collinearity of
the interactions — each remains approximately proportional to `CCI`
and their absolute pairwise correlations are 0.66–0.97 on this sample
(`australia_cci_interaction_corr.csv`; §5) — which is the structural
reason the credit channels cannot be separately free-estimated off a
single equation, and why Williams identifies them jointly through
FIML.


### 4.8 Sign priors

Theoretical sign priors on the long-run coefficients are

- `γ_1 ≥ 0` (housing collateral, unlocked by `CCI`);
- `γ_2 ≥ 0` (illiquid financial wealth);
- `γ_3 ≥ 0` (net liquid wealth: buffer-stock plus inter-temporal
  substitution);
- `α_4 ≤ 0` on `ln(p^h/y)` at credit-tight regimes (down-payment
  penalty), with the sign reversing toward positive at credit-loose
  regimes through the `(1 − ϖ·CCI)` term;
- `α_1 ≤ 0` on `r·CCI` (inter-temporal substitution);
- `ζ_c` positive (autonomous-consumption loading on credit);
- `ψ_0, ψ_1 ≥ 0` (permanent-income gearing rising in credit);
- `λ < 0` (stable error correction).

We use these as informal screens (§6.2) rather than imposing them as
formal restrictions. Freely estimating the coefficients lets us report
violations of the priors as substantive findings — for example the
sign failures on `r_x_cci` and `yp_x_cci` in the faithful specification
(§7.0), and the sign-prior GETS verdicts on the CCI knots (§5) — rather
than burying them inside imposed constraints. This is the methodological
discipline that distinguishes our single-equation estimates from
Williams' FIML system: where his cross-equation restrictions deliver
identification by imposition, our free estimates expose, rather than
hide, the weak identification of the credit channels. The cost is power;
the benefit is that the negative results (placebo failure, near-zero SUR
efficiency gain, out-of-sample loss to a random walk, the calibration
collapse, and the interaction collinearity) are visible as diagnostics
pointing to why FIML and the pre-1988 back-extension are the routes
forward (§8–§11).

---


## 5. Identification of credit conditions

This section is the methodological pivot of the paper. In the LIVES
framework (Williams 2010; Muellbauer and Williams 2012) the credit
conditions index, `CCI`, is not an observed series. It is a latent
common factor that enters the consumption, house-price, mortgage-stock
and home-equity-withdrawal equations simultaneously, and it is the
object that interacts with the six channels inside the long-run bracket
of equation (7). In Williams' setting it is identified by being shared
across equations under cross-equation parameter restrictions estimated
by full-information maximum likelihood (FIML). In a single-equation
setting that joint identification is unavailable, and `CCI` can only be
*proxied* — by an institutionally dated smoothed-step spline, a
state-space factor, a principal component, or a credit-to-income gap —
or *calibrated* to Williams' published values (the calibration route is
developed in §7.0.1 and shown to collapse the equilibrium).

The central new result of this section is an identification diagnosis,
not a CCI success. We document directly why the credit channels cannot
be separately recovered from a single equation: the five CCI-carrying
regressors that implement the credit channels are between 0.66 and 0.97
mutually correlated in absolute value on this sample
(`australia_cci_interaction_corr.csv`) because each is approximately
proportional to `CCI` itself (§5.5). A single equation therefore cannot
tell them apart. The placebo battery (§5.2) returns a split verdict on
the spline itself: the literal Williams 4-knot construction, and the
institutional knot choices under single-pass reduction, sit at or below
the median of random smoothed-step draws — the detrending critique is
vindicated for those constructions — while the *deployed* construction
(the iterated reduction over the 15-knot basis) beats roughly four
fifths of random draws, moderate support that remains
protocol-dependent. This is the structural reason Williams identifies
the credit channels through FIML, and it is why we treat four-equation
FIML and pre-1988 back-extension, not further single-equation search,
as the routes that could sharpen the credit channels. Throughout, the
honest negatives — placebo failures for the literal construction,
near-zero SUR residual correlation, selector divergence — are reported
as substantive diagnostic findings, not embarrassments.

### 5.1 The Muellbauer-Williams smoothed-step spline approach

Muellbauer and Williams (2012) construct `CCI` as a spline of `SDMMA`
smoothed-step dummies — a 5-quarter moving average of a 4-quarter
moving average of a 0/1 step — at institutional turning points in the
Australian financial-policy chronology (§2.4 above; the earlier,
single-author Williams 2009/2010 papers use a simpler smoothed
linear-split-trend measure, `CCIH`). Each knot's coefficient is
constrained by a sign prior derived from institutional history
(deregulation episodes positive; retrenchment episodes negative),
enforced by Hendry-Krolzig (2005) drop-on-violation
general-to-specific reduction.

Williams' canonical paper uses four knots: 1979Q1 (Campbell Committee,
removal of interest-rate ceilings), 1992Q1 (NBFI distress after the
early-1990s recession), 1998Q1 (NBFI/securitisation expansion), and
2007Q1 (GFC retrenchment). The four-knot choice reflects the
institutional information available over his 1977-2008 sample: STAMP-
derived turning points and a deregulation calendar ending shortly after
the GFC.

**On our 1988Q3-onwards sample, only one of Williams' four canonical
knots survives sign-prior reduction.** A direct replication of the
Williams 4-knot specification yields
(`australia_knot_experiment_estimates.csv`):

| Williams knot | Sign prior | Status on 1988+ sample |
|---|---:|---|
| 1979Q1 | + | aliased (constant within window) |
| 1992Q1 | − | sign violator (data signal +ve) |
| 1998Q1 | + | sign violator (data signal −ve) |
| 2007Q1 | − | survives, coef ≈ −0.010 |

The 1979 deregulation knot is mechanically uninformative because the
smoothed step reaches unity by 1980Q2, three years before our window
opens. The 1992 and 1998 knots fail their institutional sign priors:
the post-1988 sample observes the recovery from the early-1990s banking
distress (during which credit growth resumed and the OLS coefficient
turns positive) and the late-1990s NBFI period without the contrast
against the prior tight regime that would identify the loosening
direction. This is a direct consequence of the data window. As noted in
§3, ABS sectoral balance-sheet data begin only in 1988Q3, so the
financial-liberalisation episode that most cleanly identifies the credit
channels (Campbell, the early-1980s deregulation, the NBFI cycle)
largely predates the sample on which the single-equation model can be
estimated.

A direct 4-knot replication is therefore *not* identifying the four
distinct credit-conditions episodes Williams' framework attributes to
the spline. It is identifying one — the 2007 GFC tightening — plus a
constant.

#### 5.1.1 The maximal-GETS Australian CCI

Rather than impose Williams' published knot count on a sample that
cannot identify three of his four knots, we adopt a **maximal-GETS
approach**: starting from a richer 15-knot candidate set covering the
documented Australian financial-policy chronology, we let
drop-on-violation reduction prune knots that are aliased or violate
their institutional sign prior. The candidate institutional events span
Campbell '79, the housing-finance deregulation of '86, state-bank
distress '90, banking distress '92/'93, the Wallis report and the
establishment of APRA in '98, the GFC tightening of '07, the
deposit-guarantee scheme '08, the FHB Boost '09, the APRA
macroprudential rounds of '14 and '17, the Hayne Royal Commission of
'19, the APRA cap removal and buffer reduction of '19Q3, the
COVID/JobKeeper episode of '20, and the APRA buffer hike of '21.

On the 1988Q3-2024Q4 sample this candidate set reduces to **four
surviving knots** under the iterated drop-on-violation reduction
(`australia_williams_cci_knots.csv`):

| Knot | Sign prior | Coef (OLS) | Reading |
|---|---:|---:|---|
| 2007Q3 | − | −0.0022 | GFC tightening onset |
| 2009Q1 | + | +0.0123 | First Home Buyer Boost (but see below) |
| 2019Q1 | − | −0.0338 | Hayne Royal Commission lending crackdown |
| 2020Q2 | + | +0.0071 | COVID/JobKeeper income support |

(Nine candidate knots — 1990Q3, 1992Q1, 1993Q1, 1998Q3, 2008Q4, 2014Q4,
2017Q1, 2019Q3 and 2021Q4 — violate their institutional sign priors and
are dropped; 1979Q1 and 1986Q1 are aliased, their smoothed step being
constant within the estimation window.)

The `cci_williams` series used throughout the rest of the paper is
constructed from these four surviving knots, peak-normalised to unity,
and is committed in full (`australia_cci_williams_series.csv`; path
figure `australia_cci_williams_path.png`). Its path should be read
before any credit interpretation is placed on it: the index is
**identically zero from 1976 until 2007Q3**, dips slightly negative over
the GFC ramp (≈ −0.15 by late 2008), rises to its peak of 1 by 2010Q4,
plateaus at 1 through 2018Q4, then falls steeply after 2019Q1 to a
trough of −2.12 in 2020Q4 and settles at ≈ −1.63 from 2022 onwards
(range −2.12 to +1.00). Four implications follow. First, every credit
channel in Spec 8 and Spec 11 is identified off roughly **70 post-2007
quarters**: before 2007Q4 each CCI interaction is exactly zero, so the
pre-GFC half of the sample contributes nothing to the credit
coefficients. Second, that all four surviving knots are post-2007 is
itself part of the identification story (§5.3): the post-1988 sample
carries usable sign-identifying variation only around the GFC,
macroprudential and pandemic episodes. Third, the institutional reading
of the surviving 2009Q1 "+" knot is contestable: the First Home Buyer
Boost was a *fiscal* stimulus rather than a lending-standards easing,
and the RBA's Financial Stability Reviews record lending standards
*tightening* through 2009, so the "+" prior records a credit-demand
event under a credit-supply label. Fourth, the candidate basis embeds a
documented sign-prior conflict at 1993Q1: the maximal basis codes it −1
(the tail of the early-1990s bank retrenchment) while the sectional
basis of §5.2.2 codes the same date +1 (mortgage-originator entry,
Aussie Home Loans); both institutional readings are defensible for
different events at the same date, and we disclose rather than silently
resolve the conflict (`model_helpers.R`).

The reduction protocol also requires precise statement. At each pass the
consumption ECM is re-fitted with the currently surviving candidate set
and **all** knots whose coefficient violates its sign prior are dropped
simultaneously; the loop iterates to a fixed point
(`fit_consumption_with_williams_cci()`). This *differs* from Williams'
one-at-a-time, strongest-violator-first reduction, and the survivor set
is protocol-dependent: a single-pass reduction over the same 15-knot
basis retains a *different* set of five knots — 1992Q1, 2007Q3, 2017Q1,
2019Q1 and 2020Q2, with 2009Q1 aliased rather than surviving — and a
different λ (`australia_knot_experiment.csv`, `maximal_gets` row). We deploy the
iterated protocol and placebo-test exactly that protocol in §5.2.

Finally, the construction is **two-step, with pre-test re-use of the
dependent variable**. The knots are first selected as plain *additive*
long-run regressors inside a constant-MPC Spec-4-style consumption
equation estimated on the same `Δln c` series; the surviving
combination, peak-normalised, is then re-deployed *multiplicatively*
(as `ha_x_cci`, `r_x_cci`, `yp_x_cci`, `hp_x_1_minus_cci` and the
`cci_williams` level) in Spec 8 and Spec 11. Spec 11's fit statistics
are therefore conditional on a CCI that was pre-fitted, under sign
priors, to the same dependent variable — a pre-test problem that the
placebo battery quantifies but does not remove.

The maximal-GETS construction remains defensible on two grounds: (i) the
candidate set comes from documented Australian institutional history,
not authorial choice of specific dates; and (ii) the surviving knots are
those whose data signal aligns with their institutional sign prior, so
the spline is *empirically* selected rather than imposed. We do **not**,
however, claim that this delivers a structurally identified
credit-conditions factor. The placebo battery (§5.2) gives the deployed
protocol moderate — not strong — support against random smoothed-step
constructions, and §5.5 documents the underlying obstacle: the credit
channels the spline is meant to carry are near-collinear and cannot be
separated off one equation. Williams' canonical 4-knot setup is retained
as a robustness benchmark, and a sectional sign-prior alternative
following Williams' (Aust paper §5.1) specification is also implemented
and placebo-tested (§5.2.2).

### 5.2 The placebo battery

Whether the spline is identifying genuine credit-conditions turning
points — rather than acting as flexible detrending of the
consumption-equation residual — is empirically testable. We construct
random-knot placebos: 200 draws of knot dates, compared like-for-like
with the institutional construction under the *same* protocol. For the
literal-Williams comparison all four drawn knots are entered
**unconditionally** — testing pure fit, with no sign-prior reduction —
so the canonical 4-knot series is compared against random 4-knot
series. For the maximal-GETS comparison each draw of 15 candidate knots
is passed through a single sign-prior reduction pass. And — new in this
draft — a **deployed-protocol placebo** replaces the 15 institutional
knot dates with random dates carrying the same sign-prior pattern and
runs exactly the iterated drop-on-violation reduction of §5.1.1, so the
deployed `cci_williams` construction is placebo-tested as deployed,
pre-test step and all. The institutional result's percentile rank in
each placebo distribution measures whether the specific knot choice
identifies something the data genuinely flags, versus whatever a
flexible smoothed-step series could fit by chance.

#### 5.2.1 Four placebo runs

| Construction | Protocol | Sample | adj R² %ile | \|λ\| %ile | Verdict |
|---|---|---|---:|---:|---|
| Literal Williams 4-knot   | unconditional 4-knot | 1988Q3+ (n=146) | 45th | 56th | Below R² median — detrending critique vindicated |
| Literal Williams 4-knot   | unconditional 4-knot | 1976Q3+ (n=190) | 36th | 26th | Below median on both — critique persists |
| Maximal-GETS canonical    | single-pass reduce   | 1976Q3+ (n=190) | 48th | 70th | Below R² median — critique persists |
| **Deployed `cci_williams`** | **iterated reduce** | 1988Q3+ (n=146) | **84th** | **80th** | **Moderate support** |

Sources: `australia_williams_knot_placebo_verdict.csv` (literal,
1988Q3+; Williams adj-R² = 0.7252, |λ| = 0.1890; 200 draws; committed
verdict "DETRENDING CRITIQUE VINDICATED — Williams below median");
`australia_williams_knot_placebo_extended_summary.csv` (literal,
1976Q3+; Williams adj-R² = 0.6801 versus a placebo median of 0.6815,
|λ| = 0.2023 versus a placebo median of 0.2079);
`australia_williams_knot_placebo_maximal_extended_summary.csv`
(maximal-GETS, 1976Q3+; canonical adj-R² = 0.6836 versus a placebo
median of 0.6846, |λ| = 0.2563 versus a placebo median of 0.2329,
canonical surviving knots 7 versus a placebo median of 8);
`australia_williams_knot_placebo_deployed_verdict.csv` (deployed
protocol, 1988Q3+; deployed adj-R² = 0.7540 at the 84th percentile,
|λ| = 0.2461 at the 80th, 4 surviving knots versus a placebo median of
5; committed verdict "MODERATE SUPPORT — deployed CCI beats most random
draws"; full 200-draw table in
`australia_williams_knot_placebo_deployed.csv`).

The verdict is split, and we report both halves. The **literal Williams
4-knot construction sits at or below the placebo median on both
samples** (45th adjusted-R² percentile on the modern sample, 36th/26th
on the extended sample): Williams' specific published knot dates,
entered as published, do not outperform random dates, and the
single-pass maximal-GETS reduction on the extended sample does no
better (48th R² percentile, with the institutional choice retaining
*fewer* knots than the random median). For these constructions the
detrending critique is vindicated, and the committed verdict labels say
so. The **deployed construction fares better**: under the iterated
reduction actually used to build `cci_williams`, the institutional
knot dates beat 84 per cent of random draws on adjusted R² and 80 per
cent on |λ|, while retaining fewer knots (4) than the placebo median
(5) — i.e. the deployed CCI achieves more fit with less flexibility
than typical random constructions. We read this as moderate — not
strong — support: one in six random draws still matches the deployed
fit, the percentile is specific to the iterated protocol (§5.1.1), and
the construction re-uses the dependent variable. The standalone spline
remains, at best, weakly distinguished from flexible detrending; it is
not a structurally identified common factor.

#### 5.2.2 Sectional sign priors (Williams Aust paper §5.1) tested

Williams (Aust paper §5.1) imposes sign priors over PERIODS rather than
knot-by-knot:

| Period         | Sign prior     | Rationale                    |
|----------------|---------------:|------------------------------|
| 1982–1990      | non-negative   | Financial deregulation       |
| Early 1990s    | non-positive   | Banking sector distress      |
| Mid-1990s–2006 | non-negative   | New entrants, securitisation |
| 2007+          | non-positive   | GFC                          |

We constructed a parallel CCI basis with one knot per period, extending
Williams' four periods to cover post-2008 events (APRA 2014, APRA 2017,
COVID 2020, APRA 2021). On the back-extended sample, with a
random-period placebo (200 draws of 8 random knots and 8 random ±1
priors), the sectional canonical sits at the **37th adjusted-R²
percentile and 60th |λ| percentile**
(`sectional_placebo_summary.csv`; committed verdict "DETRENDING
CRITIQUE PERSISTS — sectional below random median"). Williams' specific
period dating does not outperform random period placements on the
post-deregulation-extended window. In the committed side-by-side
coefficient comparison (`sectional_cci_comparison.csv`) the sectional
basis retains only two survivors against the maximal basis's four, with
a weaker fit (adj-R² 0.726 vs 0.754; λ = −0.203 vs −0.246) and a 0.69
correlation between the two indices — so the period-prior construction
delivers a related but coarser credit signal than the maximal-GETS
reduction. (An earlier draft reported the two fits as identical; that
was a script bug — the sectional comparison had silently re-used the
maximal basis — fixed in this revision.)

The takeaway across §5.2.1 and §5.2.2: neither the literal 4-knot
construction, nor a single-pass maximal-GETS reduction, nor sectional
sign priors delivers placebo support on the extended sample — the
*institutionally dated* knots per se do not beat random dates. The one
construction that does beat most random draws is the deployed iterated
protocol on the modern sample, and even that support is moderate and
conditional on the protocol. The CCI's standalone identification
therefore remains closer to a single-equation OLS using flexible
smoothed-step dummies than to a structurally identified common factor.
The next two subsections set out *why* this is the expected outcome of
single-equation estimation, and §5.5 supplies the direct mechanical
reason.

### 5.3 Why the spline alone cannot identify the CCI as a common factor

The placebo evidence is consistent with the structural diagnosis in the
LIVES literature itself. Williams (2010) and Duca and Muellbauer
(2013, ECB WP 1581) estimate the CCI inside a **multi-equation system**
(consumption, house prices, mortgage stock, home equity withdrawal —
Williams; consumption + mortgage-refinancing rate, via a Kalman-filter
state-space model — Duca and Muellbauer) where the *same* latent credit
variable enters all equations simultaneously. Williams
imposes a normalisation (ζ_h = 1 in the house-price equation) and
estimates ζ_c, ζ_m, ζ_w as relative scalings; this cross-equation
parameter restriction is what identifies the CCI as a common factor
rather than as an equation-specific residual.

In a single-equation OLS estimation the spline is fit only to the
consumption-equation residual. There is no constraint that the same knot
loadings satisfy sign priors in the house-price, mortgage-stock or HEW
equations simultaneously. The multi-equation scaffolding in our companion
directory tests this directly.

#### 5.3.1 Cross-equation joint sign-survival (LIVES phase 3)

We refit the Williams 15-knot maximal candidate set with the consumption
equation, the house-price equation, the mortgage-stock equation and a
home-equity-withdrawal proxy equation simultaneously, then require each
knot to satisfy its institutional sign prior in every equation in which
it is estimable (`LIVES/outputs/lives_joint_cci_survival.csv`).

| Survival regime | Surviving knots | n |
|---|---|---:|
| Consumption equation only (Spec 1 with `ln_networth_y_proxy`, extended sample) | 1979, 1986, 1992, 2007Q3, 2017Q1, 2020Q2 | 6 |
| Joint 3-equation (C ∩ H ∩ M)            | 1986, 2017Q1 | 2 |
| **Joint 4-equation (C ∩ H ∩ M ∩ HEW)**  | **1986**     | **1** |

Of the six knots that pass their sign prior when fitted to consumption
alone (this is the single-pass reduction in
`joint_cci_identification.R`, using the Spec-1 aggregate-proxy
specification on the back-extended sample — distinct from the *iterated*
reduction the canonical consumption pipeline applies in §5.1.1, which
retains four knots, 2007Q3, 2009Q1, 2019Q1 and 2020Q2, on the 1988+
sample; the two reductions give different and only partially overlapping
survivor sets, and 2009Q1 is aliased in the joint system), only **1986
(financial deregulation) and 2017Q1 (APRA macroprudential round II)**
have signs consistent with their institutional priors across
consumption, house prices and mortgage stock simultaneously, and only
**1986** also survives the home-equity-withdrawal equation.

The consumption-only identification is therefore overstated: four of
the six consumption-passing knots are consumption-equation-specific and
do not survive a cross-equation common-factor restriction, and only one
knot survives all four equations. This is the empirical content of the
placebo results in §5.2: without imposing cross-equation sign
consistency, the consumption-equation residual can be flexibly fit by
smoothed-step dummies whose information content is
consumption-specific.

#### 5.3.2 What joint identification fixes

When we rebuild `cci_williams_joint` using the two knots that survive
the three-equation test and re-estimate the house-price equation with
the new CCI (`LIVES/outputs/lives_phase3_comparison.csv`):

| HP equation, CCI loading | (a) cons-only CCI | (b) joint OLS | (c) joint SUR |
|---|---:|---:|---:|
| Estimate                 |  −0.016 | +0.024 | +0.024 |
| Sign                     |   ✗ violator | ✓ | ✓ |

The house-price equation's CCI loading flips from negative (under the
consumption-only CCI) to positive (under joint identification) —
Williams' cross-equation sign restriction working as intended. The mortgage-stock equation's CCI loading remains negative
under this joint-sign-survival approximation, which weights surviving
knots by consumption-equation coefficients; full FIML with parameter
restrictions across all four equations would be needed to flip it. The
exercise is therefore illustrative of the mechanism, not a substitute for
the FIML estimate.

### 5.4 Two-equation SUR — joint estimation gives no efficiency gain

A complementary test of the multi-equation framework's value is whether
SUR or FIML deliver efficiency gains over equation-by-equation OLS. A
two-equation SUR (consumption + house prices, on the extended 1976Q3+
sample using Spec 1 with the aggregate net-worth proxy) finds an
essentially negligible cross-equation residual correlation: the
committed estimates are ρ̂(ε_C, ε_H) = −0.0109 under equation-by-equation
OLS and −0.0133 under SUR
(`LIVES/outputs/lives_sur_2eq_resid_corr.csv`, committed verdict
"NEGLIGIBLE cross-equation linkage — single-equation OLS approximately
efficient"). The corresponding coefficient comparison is in
`australia_joint_pi_robustness.csv`: moving from single-equation OLS to
SUR_joint on the Spec-3 consumption equation shifts coefficients only
within sampling noise — the net-worth loading moves from +0.0011 to
+0.0024 (well under half a standard error, SE 0.0028; insignificant
under both), the speed of adjustment from −0.191 to −0.204 (raw-units
ecm_lag), and the permanent-income loading from +0.196 to +0.187. No
coefficient changes sign in a way that alters the economic reading, and
the standard errors are barely tightened.

The joint-estimation case for LIVES therefore does **not** rest on
efficiency gains. It rests entirely on **cross-equation parameter
restrictions** — Williams' FIML imposes that the same CCI loading enters
all four equations with specific sign constraints. SUR alone imposes only
residual-covariance flexibility, and at the quarterly frequency that
covariance is negligible (§5.3.1). The two-equation SUR confirms that the
cross-equation linkage between consumption and house prices is captured by
shared regressors (CCI, the real rate, dummies for major events); it does
not reside in unexplained residual covariance. We report the near-zero
SUR residual correlation as another honest negative: it tells us that the
value of the system is in its sign restrictions, not its error structure,
and so single-equation SUR cannot be a shortcut to system identification.

### 5.5 Interaction collinearity — why the credit channels cannot be separated off one equation

The placebo failures (§5.2), the cross-equation sign-survival collapse
(§5.3.1) and the absence of an SUR efficiency gain (§5.4) all point to the
same underlying obstacle, which we now state directly. In the faithful
LIVES form, `CCI` does not enter once: it multiplies six channels jointly
inside the long-run bracket of equation (7) — the autonomous-consumption
loading (ζ_c·CCI), the rate channel (α_1·r·CCI), the housing-collateral
channel (γ_1·CCI·(HA/4y)), the down-payment/affordability composite
(α_4·(1−ϖ·CCI)·log(p^h/y)), the permanent-income gearing (ψ(CCI)·log(y^p/y)
with ψ(CCI) = ψ_0 + ψ_1·CCI), and so the regressors that carry these
channels are each approximately *proportional to* `CCI` over the sample.

The empirical consequence is that the five CCI-carrying regressors —
`cci_williams`, `ha_x_cci`, `hp_x_1_minus_cci`, `r_x_cci` and
`yp_x_cci` — have absolute pairwise correlations between **0.66 and
0.97** on this sample. The full correlation matrix is committed
(`australia_cci_interaction_corr.csv`); the extremes are
ρ(`cci_williams`, `yp_x_cci`) = −0.967 and
ρ(`cci_williams`, `ha_x_cci`) = +0.890, with the weakest pair
(`cci_williams`, `hp_x_1_minus_cci`) still at −0.681. Five
near-collinear regressors carrying distinct structural meanings cannot
be separately free-estimated off a single equation: ordinary least
squares will allocate a near-singular design among them more or less
arbitrarily, producing wrong-signed and insignificant individual
loadings even when the joint contribution of the credit block is real.

The committed, load-bearing evidence for this collinearity is threefold,
and each piece is itself an honest negative:

1. **Sign failures and insignificance when the interactions are freed
   (Spec 8, §5.6 and §7).** When the credit interactions are estimated
   freely, the housing-collateral interaction is right-signed but
   insignificant, while three of the remaining interactions fail their
   institutional sign priors (`australia_spec8_sign_prior_verdicts.csv`).
   This is exactly the pattern a near-singular design produces.

2. **The identification-versus-detrending decomposition
   (`australia_cci_fit_decomposition.csv`).** Adding the Williams
   maximal-GETS CCI to the conventional baseline (Spec 6 → Spec 8) does
   *not* merely detrend: it shifts the permanent-income loading by +51
   per cent (from +0.33, t = 1.50, to +0.49, t = 3.42) and the speed of
   adjustment by −92 per cent (from −0.239 to −0.458), with the
   net-liquid loading shifting +303 per cent and the superannuation
   loading +258 per cent. By the decomposition's own classification
   these are "IDENTIFICATION" (> 30 per cent) rather than "DETRENDING"
   (< 5 per cent) shifts. The credit interactions re-allocate
   identification across the income, liquid-wealth and
   speed-of-adjustment channels — which is precisely the symptom of
   collinearity (one near-singular block redistributing fit), not
   evidence of clean separate identification of each channel.

3. **The calibration collapse (Spec 10 and Spec 12, §7.0.1).** When
   Williams' calibrations are imposed instead of freed, the
   error-correction term collapses to λ = −0.030 (t = −0.74) in Spec 12,
   independently reproduced at λ = −0.048 (t = −0.78) by Spec 10. A block
   of regressors that can be jointly identified would not behave this
   way; a near-collinear block, whose joint mapping into the data is
   sharp but whose internal split is not, will.

Single-equation calibration of the credit channels is therefore
empirically closed: the data pin down the *block* but not its
*components*. This is the structural reason Williams uses FIML — the
cross-equation sign restrictions break the within-equation collinearity by
requiring the *same* loadings to be consistent across consumption, house
prices, mortgage stock and HEW. Two routes can sharpen the credit channels
on Australian data: the four-equation FIML system (which the companion
paper develops), and the pre-1988 back-extension, which adds the
financial-liberalisation variation that most cleanly identifies `CCI` but
which the post-1988 balance-sheet data exclude. Further single-equation
search cannot.

### 5.6 Spec 8: the freely estimated CCI interactions (the collinearity demonstration)

When `cci_williams` is available, we estimate Spec 8, which incorporates
the Williams interaction structure with the credit channels entered as
free regressors (the autonomous-consumption CCI level is the one channel
Spec 8 omits; Spec 11 restores it):

> ... + γ_HA · ha_y + γ_HA_cci · ha_y · CCI
>     + γ_HP · log(p^h/y) · (1 − ϖ · CCI)
>     + α_r · r · CCI
>     + ψ_1 · log(y^p/y) · CCI + ...

with ϖ calibrated to 1.2 following Williams (Aust paper §5.2 fn 9). We
present Spec 8 not as a credit-conditions success but as the empirical
*demonstration* of the collinearity diagnosed in §5.5. It is the
free-estimation counterpart of the calibration-collapse specifications
(Spec 10/12): freeing the block over-fits the within-equation split and
re-allocates identification, whereas imposing the block collapses the
equilibrium. Neither separately identifies the credit channels.

The freely estimated interaction loadings, with their institutional sign
priors (`australia_spec8_sign_prior_verdicts.csv`,
`australia_williams_spec8_comparison.csv`), are:

| Interaction | Williams reference | Sign prior | OLS (Spec 8, full) | t | Verdict |
|---|---|---:|---:|---:|---|
| `ha_x_cci` (housing × CCI; γ_1)        | +0.0488 (peak MPC) | + | +0.0043 | 0.87 | right-signed, ns |
| `r_x_cci` (real rate × CCI; α_1)       | −0.871 (at CCI=1)  | − | +0.0028 | 2.05 | sign FAIL |
| `hp_x_1_minus_cci` (down-payment; α_4) | −0.13 (at CCI=0)   | − | +0.0299 | 1.38 | sign FAIL |
| `yp_x_cci` (PI × CCI; ψ_1)             | +0.93 (calibrated) | + | −0.5046 | −2.43 | sign FAIL |

The pattern is diagnostic of the near-singular design, not of structure.
The housing-collateral interaction is **right-signed** — consistent with
Williams' prediction that the housing MPC rises with credit ease as
collateral becomes spendable — but statistically insignificant (t = 0.87,
p = 0.38). Of the remaining three interactions, all fail their
institutional sign priors: the rate interaction is wrong-signed (and
significant at 5 per cent, p = 0.042), the down-payment composite is
wrong-signed and insignificant, and the permanent-income interaction is
strongly wrong-signed (the free estimate is −0.50, t = −2.43, against
Williams' calibrated +0.93). The one term that survives its prior is the
*non*-interacted permanent-income level (`ln_yp_over_y` = +0.49, t = 3.42,
PASS). In other words, when the credit block is freed the equation
re-allocates almost all of its identification onto the income and
speed-of-adjustment channels and away from the individual interactions —
exactly the identification re-allocation §5.5 documents.

It is true that Spec 8 with the interactions included delivers a
numerically larger speed of adjustment, λ = −0.458 (t = −3.52), exceeding
Williams' calibrated −0.286 in magnitude, and a BIC of −952.8 — second
only to the faithful Spec 11 (−954.8) among the n = 146 specifications.
We do **not** read this as Spec 8
"closing the gap" with Williams or "exceeding" him in any substantive
sense. The larger |λ| and the low BIC reflect the re-allocation of fit
that a near-collinear block produces, not separate identification of the
credit channels: three of the four interactions are wrong-signed, and the
one right-signed interaction is insignificant. The honest reading is that
freeing the block buys fit at the cost of structurally meaningless
component loadings. The faithful LIVES specification (Spec 11, §7.0), by
contrast, enters housing *only* through its CCI interaction
(`ha_x_cci`, de-meaned, +0.0025, t = 0.71 — right-signed, insignificant)
and restores the autonomous-consumption CCI loading, and it is the
narrative-preferred headline on theoretical-form grounds; Spec 8 is
retained as the collinearity demonstration and is not promoted as a CCI
result.

### 5.7 CCI measure cross-checks

To confirm that the identification difficulty is a property of
single-equation Australian data rather than of the smoothed-step
construction in particular, we cross-check the Williams maximal-GETS
spline against three alternative latent-CCI measures
(`australia_cci_methods_summary.csv`): a Kalman state-space common factor
estimated by ML on five credit indicators with a housing-loan-flow anchor
(n = 194); the first principal component across the same five standardised
indicators (n = 146); and a BIS-style credit-to-income HP-filter gap
(λ = 400,000) on log household debt-to-income (n = 146). The pairwise
correlations between the four measures (`australia_cci_method_4way.csv`)
are low except between the two purely statistical factors:

| Pair | ρ | n |
|---|---:|---:|
| Kalman vs Williams maximal-GETS    | −0.004 | 194 |
| PCA vs Williams maximal-GETS       | −0.218 | 146 |
| Credit/income gap vs Williams      | 0.335  | 146 |
| Credit/income gap vs Kalman        | 0.304  | 146 |
| Kalman vs PCA                      | **0.764** | 146 |
| Credit/income gap vs PCA           | 0.238  | 146 |

The institutional spline is essentially uncorrelated with the Kalman
factor (ρ = −0.004) and weakly negatively correlated with the PCA factor
(ρ = −0.22); only the two data-driven statistical factors agree strongly
with one another (Kalman versus PCA, ρ = 0.76). The four candidate
measures do not converge on a common Australian credit-conditions series.
When the Kalman factor is used in place of the spline (Spec 9, §7), the
liquid, illiquid-financial and permanent-income channels shift by 32–66
per cent relative to the no-CCI baseline
(`australia_cci_fit_decomposition.csv`), again re-allocating
identification rather than sharpening it. The measure
choice is therefore not innocuous, and no single-equation CCI proxy
resolves the underlying weak identification — reinforcing that regime
classification should rely on the joint system rather than on any one
single-equation proxy (§10).

### 5.8 The wealth-coefficient profile on the back-extended sample

A natural follow-up question is whether *sample length*, rather than the
single-equation framing itself, accounts for the residual gap between our
estimates and Williams' Table 1. The cleanest test runs on the simpler
disaggregated no-CCI specification (Spec 4), which is closest in form to
Williams' long-run cointegrating regression. We refit Spec 4 on the
back-extended 1976Q3+ sample using the disaggregated wealth proxies of §3:

| LR coefficient | Baseline 1988+ (n=146) | Extended 1976+ (n=190) | % change | Williams Table 1 |
|---|---:|---:|---:|---:|
| λ (ecm_lag)    | −0.182 | −0.203 | +11.5 | −0.286 |
| nla_y          | +0.0245 | +0.0013 | −94.6 | +0.159 |
| eq_y           | −0.043 | −0.041 | −6.5 | +0.022‡ |
| super_y        | +0.0137 | −0.0052 | −138 | (incl.)‡ |
| ha_y           | +0.0193 | +0.0177 | −8.1 | +0.0488 |
| ln_yp_over_y   | +1.11  | +1.04  | −6.5 | +0.20  |

Source: `spec46_extended_comparison.csv` (structural long-run
coefficients, γ = OLS/|λ|, per §4.2; the λ row is the OLS `ecm_lag`).
‡ Williams reports a single illiquid-financial-asset MPC (γ_IFA = 0.022), shown here against the combined eq_y + super_y. The Williams structural MPCs used throughout the paper (γ_HA = 0.0488, γ_NLA = 0.159, γ_IFA = 0.022) are as reported in Muellbauer and Williams (2012): γ_NLA and γ_IFA are Table 1's raw coefficients γ₃ and γ₂ (0.1588 and 0.0219 respectively); γ_HA = 0.0488 is the paper's derived long-run peak housing MPC reported in the text, not the raw Table 1 coefficient (Table 1's γ₁ = 0.0606). All values are recorded in [`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv) — the same authoritative source as §7.3 and §11.4.

On Spec 4 the speed of adjustment moves about a fifth of the remaining
distance towards Williams' value (−0.182 → −0.203, against −0.286), but
the individual structural wealth coefficients become *smaller*, not
larger; γ_NLA collapses to roughly zero (+0.0245 → +0.0013), γ_SUPER
flips sign,
and γ_EQ retains a wrong sign on both samples. The long-run
permanent-income coefficient remains far above Williams' calibrated
+0.20 on both samples.

The reading is consistent with the rest of this section. First, the
back-extension does not push the Spec 4 estimates *closer* to Williams'
Table 1 in the cross-section of wealth loadings — the post-1988 sample
window is not, in itself, what generates the residual gap; sample length
is not the binding constraint. Second, triangulating with the placebo
evidence (§5.2), the cross-equation sign-survival collapse (§5.3.1), the
near-zero SUR residual correlation (§5.4) and the interaction
collinearity (§5.5), we read the residual difference between any
single-equation OLS estimate and Williams' system FIML as a consequence of
the single-equation framing rather than of sample length, knot count, or
sign-prior structure. The path to a tighter reconciliation runs through a
full FIML build with cross-equation parameter restrictions and the
pre-1988 financial-liberalisation variation — the two routes §5.5
identifies — which the companion paper develops.

---


## 6. Specifications and selection

We estimate fourteen specifications — a numbered ladder Spec 1 through
Spec 12, plus two variants, the long-history Spec 6b and the
measured-burden Spec 7b — that range from a single aggregate-net-worth
error-correction model to the faithful Muellbauer–Williams LIVES form
and its calibration-imposed counterpart.
The ladder is then passed through a four-screen selection rubric. We
report the automated selector outcome in full and honestly, but we
state at the outset that the headline of this paper is chosen on
theoretical-form grounds, not by the screen: the **faithful LIVES
specification (Spec 11)** is the narrative lead, the **conventional
constant-MPC disaggregated ECM (Spec 6)** is retained as the baseline
against which it is contrasted, and the **Williams-calibration-imposed
specification (Spec 12)** is the negative control that shows what fails
to transfer. In the current vintage, BIC and LIVES theory *agree*:
Spec 11 carries the best BIC of all fourteen specifications (−954.8).
The automated rubric nevertheless returns a third, non-LIVES
specification (Spec 3, aggregate net worth in levels), because no
specification clears the cointegration screen and the conservative
speed-of-adjustment ceiling (|λ| < 0.30) and the 2008Q3 Chow screen
bind against Spec 11's COVID-inflated full-sample estimate. We treat
that divergence not as a defect to be smoothed over but as a documented
result (§6.3): it is diagnostic of how weakly a single equation can pin
down the LIVES long run on post-deregulation Australian data, and of
how much the conservative screens are doing.

### 6.1 The fourteen specifications

The ladder is organised so that each step adds one structural element.
Specs 1–3 are aggregate-net-worth ECMs; Specs 4–7b disaggregate wealth
and add cohort/burden terms; Specs 8–9 introduce the Williams CCI
interactions (spline and Kalman extractions); and Specs 10–12 are the
explicitly Williams-aligned forms — the calibrated-prior fixed point
(Spec 10), the faithful free LIVES estimate (Spec 11, the headline) and
the Williams-calibration-imposed LIVES (Spec 12). Long-run regressors
follow the notation of §4; `ha_x_cci` is the de-meaned credit-scaled
housing term CCI·(HA/4y), `ilfa_y` is the combined illiquid-financial
ratio (equities + superannuation), `cci_williams` is the autonomous-
consumption CCI loading ζ_c, and `yp_x_cci` is the credit-scaled
permanent-income term.

| Spec | Description | Long-run regressors / notes |
|---|---|---|
| 1   | Aggregate net worth                          | `ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, ecm_lag` |
| 2   | Spec 1 + short-run CCI                        | adds Δ²log CCI lag 2 to short-run set |
| 3   | Net worth in levels                          | replaces `ln_networth_y` with `networth_y` |
| 4   | Disaggregated wealth                          | adds `nla_y, eq_y, super_y, ha_y`; drops aggregates |
| 5   | Spec 4 + full short-run dynamics             | adds Δ²log CCI, ΔΔ₄income, Δ²log unemp, \|ε̂\| |
| 6   | Conventional constant-MPC disaggregated ECM (baseline) | Spec 5 + post-2008 PI break `ln_yp_over_y_post2008`; plain `ha_y/eq_y/super_y/nla_y`, CCI short-run only |
| 6b  | Spec 6 with back-extension-compatible SR CCI | replaces Δ²log CCI with Δ²log RBA D02 credit; disaggregated wealth proxies; fits on n = 180 (1977Q3+ window less short-run lags) |
| 7   | Spec 6 + cohort terms + synthetic burden     | adds `prime_age_share, fhb_share` |
| 7b  | Spec 7 with RBA E13 measured burden          | post-2009 sample only |
| 8   | Williams CCI interactions (free)             | Spec 4 with the plain `real_rate` and `ln_hp_over_y` levels replaced by `r×CCI` and `log(HP/y)×(1−1.2·CCI)`, plus `ha_x_cci` and `log(y^p/y)×CCI`; plain `ha_y` retained |
| 9   | Spec 8 with Kalman state-space CCI           | mirrors Spec 8's structure exactly (same de-meaned interaction set incl. `ha_x_cci_k`; plain `ha_y` retained) with `cci_kalman` in place of the spline, isolating the CCI *series* |
| 10  | Williams-prior calibrated                    | γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93, ϖ = 1.2; iterative fixed-point OLS |
| **11**  | **Faithful LIVES (free, headline)**      | housing via `ha_x_cci` only; ζ_c·CCI intercept `cci_williams` restored; IFA combined (`ilfa_y`); `nla_y`, `hp_x_1_minus_cci`, `r_x_cci`, `ln_yp_over_y`, `yp_x_cci` |
| 12  | Williams-calibration-imposed LIVES           | Spec 11 form with ψ₀ = 0.20, ψ₁ = 0.93, γ_IFA = 0.022 imposed via iterative fixed-point |

The crucial structural difference is between Spec 6 and Spec 11. Spec 6
carries the wealth components as plain, constant marginal propensities
(`ha_y`, `eq_y`, `super_y`, `nla_y`) and lets the credit-conditions
index enter only through a short-run term, Δ²log CCI at lag 2. It is the
form that the broader empirical literature, and an earlier draft of this
work, treated as "the" LIVES equation. It is not. As §4 sets out, the
LIVES theory predicts no classical housing-wealth effect: the
housing marginal propensity is zero at CCI = 0 and is unlocked only as
credit conditions ease, so housing must enter through the interaction
term `ha_x_cci` and not as a standalone level. Spec 11 imposes exactly
that form — housing enters only via `ha_x_cci`, the autonomous-
consumption CCI loading ζ_c (`cci_williams`) is restored, and the two
illiquid-financial components are combined into `ilfa_y` as the theory
specifies. The contrast between the two is therefore a matter of
functional form dictated by theory, not of fit; §7 shows that the
form is decisive for whether the error-correction and wealth structure
come alive.

### 6.2 The four selection screens

Following the structural-econometrics tradition (Hendry and Krolzig
2005; Doornik 2009), we screen each estimable specification through four
formal tests, with BIC as a tiebreak. The screens and their per-
specification outcomes are recorded in `australia_spec_selection.csv`;
the four-sample λ stability that feeds the stability screen is in
`australia_lambda_robustness.csv`; the cointegration statistics are in
`australia_cointegration.csv`.

1. **Sign screen** — every long-run coefficient carrying a
   non-ambiguous theoretical prior (§4.2) has the correct sign.
2. **Cointegration screen** — an Engle–Granger residual test: an ADF
   (with drift) on the residual of the static long-run regression
   rejects the no-cointegration null at 5 per cent, evaluated against
   MacKinnon (1991, 2010) critical values keyed to the number of
   regressors in the cointegrating regression, *not* the univariate
   Dickey–Fuller value. Phillips–Ouliaris results are reported
   alongside for Specs 1–3. A Johansen trace statistic is also
   reported, but it should be read for what it is: **one fixed
   trivariate subsystem per specification** — log consumption, log
   income, and either `ln_networth_y` (Specs 1–3) or `ha_y` (all other
   specs) — estimated with K = 2 lags and a restricted constant,
   testing only the r = 0 null. It is *not* a per-specification test of
   the specification's own long run, and its uniform rejection of r = 0
   (`johansen_r1_pass = TRUE` for every estimable form in
   `australia_cointegration.csv`) says only that the small common
   subsystem cointegrates.
3. **Speed-of-adjustment screen** — λ has the correct (negative) sign
   and lies in the interval (0.02, 0.30).
4. **Stability screen** — Chow at 2008Q3 is not rejected at the 1 per
   cent level, *and* λ is sign-stable across at least 3 of the 4 sample
   variants (full, pre-COVID, COVID-dropped, COVID rich-dummies). The
   Chow statistic uses `strucchange::sctest` where computable; when the
   subsample design is singular (e.g. dummies that are all-zero on one
   side of the break) the pipeline falls back to a manual
   common-coefficient Chow F-test, and the method actually used per
   specification is recorded in the `chow_method` column of
   `australia_full_diagnostics.csv`. A Chow that is incomputable even
   after the fallback is treated as *neutral*, not as a failure — an
   earlier vintage treated incomputability as failure, which spuriously
   "failed" 12 of the 14 specifications.

Two screens warrant comment for the LIVES interaction specifications
(Specs 8–12). The cointegration battery now covers Spec 8 and Spec 11
directly: both fail, like every other specification (Spec 11: EG ADF
−3.13 against a MacKinnon 5 per cent critical value of −5.47 for nine
regressors; Spec 8: −3.40 against −5.70 for ten). Three forms are
skipped with an explanatory `note` in `australia_cointegration.csv`:
Spec 9, because its de-meaned Kalman interactions are constructed
locally inside the estimation step and are not available to the static
long-run regression, and Specs 10 and 12, because their long run is a
calibrated offset rather than a freely estimated static regression, so
an Engle–Granger residual test is not applicable. And the upper bound
of the speed-of-adjustment screen (|λ| < 0.30) is binding for the
strongest-adjusting forms: Specs 8 and 11 both adjust faster than the
0.30 ceiling on the full sample (|λ| = 0.458 and 0.448), and so are
flagged as failing the |λ| screen even though their λ is correctly
signed, strongly significant, and sign-stable across all four sample
variants. We return to this in §6.3 and §7: the full-sample λ for the
faithful LIVES form is inflated by the COVID quarters, and the
identified value is the pre-COVID estimate, λ = −0.266 (t = −4.85),
which lies comfortably inside the screen interval and within roughly 7
per cent of Williams' −0.286.

### 6.3 Selector outcome

Under the canonical `PI_METHOD = 'italy'` setting, **no specification
passes all four screens** — the cointegration screen fails wherever it
is computable — so the selector falls back to its documented
most-passes rule (most screens passed, BIC tiebreak). Spec 1 and Spec 3
each pass three screens (signs, λ, stability), and the BIC tiebreak
returns **Spec 3** (aggregate net worth in levels, −919.4 against
Spec 1's −919.3) as the automated pick — a conservative, non-LIVES
form. The full screen card, taken directly from
`australia_spec_selection.csv`, is:

| Spec | Signs | Coint | λ | Stability | BIC |
|---|:-:|:-:|:-:|:-:|---:|
| 1                            | ✓ | ✗ | ✓ | ✓ | −919.3 |
| 2                            | ✗ | ✗ | ✓ | ✓ | −501.7 |
| **3** (selector-preferred)   | **✓** | ✗ | **✓** | **✓** | **−919.4** |
| 4                            | ✗ | ✗ | ✓ | ✓ | −905.3 |
| 5                            | ✗ | ✗ | ✓ | ✓ | −494.6 |
| 6 (conventional baseline)    | ✗ | ✗ | ✓ | ✓ | −492.5 |
| 6b                           | ✓ | ✗ | ✓ | ✗ | −1114.0 |
| 7                            | ✗ | ✗ | ✗ | ✓ | −500.6 |
| 7b                           | ✗ | ✗ | ✗ | ✓ | −364.5 |
| 8                            | ✗ | ✗ | ✗ | ✓ | −952.8 |
| 9                            | ✗ | NA | ✓ | ✗ | −890.6 |
| 10                           | ✓ | NA | ✓ | ✗ | −493.2 |
| **11** (faithful LIVES, headline) | **✓** | ✗ | ✗ | ✗ | **−954.8** |
| 12                           | ✓ | NA | ✓ | ✗ | −893.8 |

Source: `australia_spec_selection.csv` (the BIC column is the Schwarz
criterion recorded there and in `australia_full_diagnostics.csv`).
Spec 6b carries the lowest (best) raw BIC only because it is fitted on
the longest, n = 190 back-extended sample and so is not directly
comparable to the n = 146 or n = 86 forms.

Several patterns emerge, and we read them as a coherent statement about
the limits of single-equation identification rather than as a clean
verdict.

**No specification clears the cointegration screen.** Evaluated against
MacKinnon critical values keyed to the regressor count
(`coint_adf_5pct_cv` ranging from −4.42 to −6.13 across the estimable
forms), no single-equation specification rejects the no-cointegration
null. The richer forms come closest — Specs 4/5 reach ADF −3.12 against
−5.23, Spec 6 −3.12 against −5.47, the faithful Spec 11 −3.13 against
−5.47, Spec 8 −3.40 against −5.70, and the back-extended Spec 6b −3.47
against −5.47 — but none crosses its critical value. The aggregated
Specs 1–3 fall far short (ADF −1.75 to −1.95 against −4.42).
Phillips–Ouliaris likewise fails for Specs 1–3. The Johansen column
rejects r = 0 for every estimable form, but — as §6.2 cautions — it
tests one fixed trivariate subsystem (consumption, income, and one
wealth ratio; K = 2, r = 0 only), not each specification's own long
run, so it cannot rescue the screen. A static single-equation long run
between consumption and its wealth/income determinants is therefore not
formally established on this sample. This is one of the paper's
recurring themes (§5, §7.3, §9): the long-run identification Williams
obtains comes from his cross-equation FIML system, not from any single
equation. Because the screen never passes, selection falls to the
remaining three screens with the BIC tiebreak.

**The sign screen passes for the faithful LIVES form — and now fails
for Spec 8.** Spec 11 passes the sign screen: its long-run coefficients
with unambiguous priors are all correctly signed, with
`nla_y = +0.0269` (t = 3.75), `ilfa_y = +0.0155` (t = 3.09) and
`ln_yp_over_y = +0.459` (t = 4.04) significant and right-signed on the
full sample, and `ha_x_cci = +0.0025` (t = 0.71) right-signed though
insignificant. Spec 8, the free-interaction form, *fails* the sign
screen in the current vintage because its separately entered equities
component is wrong-signed (`eq_y = −0.0014`, t = −0.11). The
disaggregated Specs 4–6 fail for the same reason — small negative
coefficients on `eq_y` (Spec 6 full: −0.016, t = −0.30) tip the screen.
We read this as an identification artefact of the constant-MPC
disaggregation, not as a substantive reversal of the
illiquid-financial channel, and it is precisely the artefact that the
faithful LIVES form removes by combining the illiquid components into
`ilfa_y` (which is then correctly signed and significant).

**The faithful LIVES form fails the |λ| ceiling and the 2008Q3 Chow on
the full sample.** Spec 11's full-sample λ = −0.448 (t = −3.57) exceeds
the 0.30 ceiling, as does Spec 8's λ = −0.458 (t = −3.52). Both are
correctly signed, strongly significant and sign-stable across all four
sample variants; the |λ| failure is not a sign or significance failure
but reflects the full-sample λ being inflated by the COVID quarters.
Spec 11 additionally fails the stability screen through its 2008Q3
Chow (p = 0.0066 under the manual common-coefficient fallback,
rejected at the 1 per cent threshold) — an honest flag that the
faithful form's coefficients shift across the GFC boundary on the full
COVID-inclusive sample. On the pre-COVID sample Spec 11's λ = −0.266
(t = −4.85) lies inside the screen interval and within roughly 7 per
cent of Williams' −0.286 — which is why §7 treats the pre-COVID
estimate as the identified value.

**The calibration-imposed forms pass the |λ| screen for the wrong
reason.** Specs 10 and 12 pass the speed-of-adjustment screen on the
full sample, but only because their λ has *collapsed* toward zero
(Spec 10: −0.048, t = −0.78; Spec 12: −0.030, t = −0.74), placing it
just inside the lower edge of the (0.02, 0.30) interval. Both fail the
stability screen — their λ is not sign-stable across samples
(`lambda_sign_stable_across_samples = FALSE` in
`australia_lambda_robustness.csv`; Spec 12's pre-COVID λ flips to
+0.041, Spec 10's COVID-dropped λ flips to +0.009). A screen that is
satisfied by a near-zero, sign-unstable adjustment speed is exactly the
case the stability screen exists to catch. The economic content of this
collapse — that imposing Williams' Australian calibrations wrecks the
equilibrium — is developed in §7.0.1 and §9.

**BIC and theory now agree on the headline; only the conservative
screens dissent.** In the current vintage the faithful Spec 11 carries
the best BIC of all fourteen specifications (−954.8), ahead of Spec 8
(−952.8), so the fit criterion and the LIVES theoretical form point to
the *same* specification. What stands between Spec 11 and the automated
pick is the conservative screen pair: the |λ| < 0.30 ceiling, which the
COVID-inflated full-sample estimate (−0.448) breaches, and the 2008Q3
Chow. The selector therefore returns Spec 3 — correctly, by its own
documented rules — while the narrative leads with Spec 11. This is a
much narrower divergence than in earlier vintages (where selector, BIC
and theory pointed to three different specifications), but we still
treat it **as a documented limitation, not a result to be argued
away**: the long run does not cointegrate in any single equation, the
five CCI-carrying regressors are 0.66–0.97 mutually collinear (§5),
and a mechanical rule built around a 0.30 adjustment ceiling cannot
endorse a form whose full-sample speed is inflated by the pandemic
quarters. We do *not* promote Spec 8's fast adjustment (λ = −0.458) as
a CCI success; its interaction coefficients are wrong-signed or
insignificant (§7) and it re-allocates identification across the
interaction block rather than sharpening any single channel.

Accordingly, the body of this paper leads with the **faithful LIVES
specification (Spec 11)** on theoretical-form grounds — it is the form
Williams (2010) and the LIVES tradition adopt, it passes the sign
screen, it carries the best BIC, and (on the identified pre-COVID
sample) it recovers Williams' error-correction speed. We retain
**Spec 6 as the conventional constant-MPC baseline** — not the headline
or the preferred specification — because it is the form prior work
treated as LIVES and because it permits the γ_LA + γ_LOANS = 0
net-liquid restriction test (§8). We carry **Spec 12 (and its
independent reproduction Spec 10) as the negative control** that shows
the structure transfers but the calibrations do not, and **Spec 3 as
the automated selector-best** comparator. The full per-specification
coefficient vectors, with Newey–West HAC standard errors and now
including the Spec 11 and Spec 12 columns, are in Appendix B.

---


## 7. Results

### 7.0 The LIVES headline specification (Spec 11)

The specification we lead with, Spec 11, is the faithful Muellbauer–Williams
Eq (7) form. It differs from the conventional disaggregated ECM (Spec 6,
§7.1 below) in three structural respects, each dictated by the theory rather
than by fit: (i) housing wealth enters **only** through its credit-conditions
interaction `CCI·(HA/4y)` — there is no standalone `ha_y` level, because the
Williams housing m.p.c. is zero at CCI = 0; (ii) the autonomous-consumption
intercept `ζ_c·CCI` is restored; and (iii) illiquid financial assets are
combined into a single ratio (equities + superannuation) rather than split,
which removes a collinear, wrong-signed equities coefficient. The CCI
interactions de-mean the interacted variable over the 1980Q1+ window,
following Williams (§4.7). Spec 11 omits Williams' demographic Δ₄DEMFTB /
Δ₄WAPOP terms, his DSRISK downside-risk index and the Δ₄ln c habit term
(none constructible on our data, §3.8), and enters the unemployment
uncertainty term un-interacted — so it is faithful to Eq (7)'s *credit
architecture*, not a term-for-term replication.

Full-sample (1988Q3–2024Q4, n = 146, adj-R² = 0.82) long-run coefficients
([`australia_all_results.csv`](../outputs/australia_all_results.csv), Spec 11
rows; the full coefficient vector under all four sample treatments is in
[`australia_spec11_variants.csv`](../outputs/australia_spec11_variants.csv)):

| Term | OLS coef | t-stat | Implied γ (= OLS/\|λ\|) | Williams |
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

And the headline speed of adjustment and credit-invariant wealth
coefficients across the four sample treatments
([`australia_spec11_variants.csv`](../outputs/australia_spec11_variants.csv)):

| Variant | n | λ (t) | `nla_y` (t) | `ilfa_y` (t) | `ha_x_cci` (t) | `ln_yp_over_y` (t) |
|---|---:|---:|---:|---:|---:|---:|
| Full sample | 146 | −0.448 (−3.6) | +0.0269 (3.8) | +0.0155 (3.1) | +0.0025 (0.7) | +0.459 (4.0) |
| Pre-COVID (to 2019Q4) | 126 | −0.266 (−4.8) | +0.0159 (1.8) | +0.0093 (1.7) | +0.0019 (0.9) | +0.298 (5.8) |
| COVID quarters dropped | 138 | −0.248 (−6.7) | +0.0170 (2.1) | +0.0098 (2.0) | +0.0023 (1.1) | +0.281 (8.7) |
| Quarterly COVID dummies | 146 | −0.242 (−6.2) | +0.0141 (3.1) | +0.0084 (3.3) | +0.0016 (0.8) | +0.278 (8.2) |

The contrast with Spec 6 (§7.1) is stark, and — unlike in an earlier
draft — it does not rest on the COVID quarters. **The error-correction
and core wealth structure come alive.** The identified speed of
adjustment is λ ≈ −0.25, nearly invariant across the pre-COVID,
COVID-dropped and quarterly-dummy treatments (−0.266 / −0.248 / −0.242,
t-ratios −4.8 to −6.7) and close to Williams' −0.286; the full-sample
−0.448 is inflated by the COVID quarters (the three pulse dummies are
demonstrably insufficient — replacing them with quarterly dummies halves
\|λ\|) and fails the upper-bound \|λ\| screen, so we do not headline it.
The net-liquid and illiquid-financial m.p.c.s are correctly signed in
every variant and significant at 5 per cent in the full-sample,
COVID-dropped and quarterly-dummy treatments (pre-COVID they retain the
same magnitudes with t ≈ 1.7–1.8); the delta-method intervals on the
structural values are γ_NLA = 0.060 [0.022, 0.098] and
γ_IFA = 0.035 [0.012, 0.057]
([`australia_gamma_inference.csv`](../outputs/australia_gamma_inference.csv)).
The permanent-income response is strong in every variant (t = 4.0–8.7).
Because the CCI-spline interactions replace Spec 6's 2002Q3-binding
`cci_ratio` short-run term, the model estimates on n = 146 rather than
n = 86 — though the *credit channels* themselves have identifying
variation only on the ~70 post-2007 quarters where the deployed CCI
moves (§5.1.1). Reading a standalone, insignificant `ha_y` in Spec 6 as
a failed housing wealth effect was therefore a category error: the
theory predicts that coefficient to be ≈ 0, and the housing effect
lives in the credit interaction.

Three honest qualifiers temper the headline. First, **the
credit-conditions interactions themselves remain weakly identified.**
The housing-collateral term `ha_x_cci` (γ₁) is correctly signed but
insignificant in every variant (full-sample t = 0.71; implied peak
m.p.c. 0.0055 [−0.010, 0.021] against Williams' 0.049, which the
interval excludes); the `ζ_c` intercept is essentially zero on the full
sample though right-signed and significant in the COVID-controlled
variants (+0.0199, t = 2.77 pre-COVID; +0.0148, t = 4.32
COVID-dropped); the real-rate interaction is wrong-signed and
significant in every variant (α₁ should be negative); and the
affordability interaction is wrong-signed (positive) and significant on
the full sample. The permanent-income credit slope `yp_x_cci` is
wrong-signed full-sample (−0.51, t = −1.5) but flips to the correct
sign in the pre-COVID (+0.19, t = 1.8) and COVID-dropped (+0.10,
t = 1.7) variants. This is the signature of two compounding problems
documented in §5: the CCI-scaled regressors are 0.66–0.97 mutually
correlated in absolute value (each ≈ proportional to the latent index),
and the deployed CCI has no variation before 2007Q3, so the
liberalisation episode that identifies the credit channels in Williams'
1978–2008 sample is absent from ours.

Second, **the structural permanent-income gearing violates the theory
bound.** By the §4.2 recovery rule, ψ̂ = OLS/\|λ\| is 1.02 on the full
sample and 1.12–1.13 in the COVID-controlled variants — above the
admissibility bound ψ ≤ 1 − η ≈ 0.95 implied by the discounting that
defines y^p. The breach is not the GFC ogive (removing it gives
λ = −0.574 and ψ̂ ≈ 1.05;
[`australia_spec11_ogive_robustness.csv`](../outputs/australia_spec11_ogive_robustness.csv)),
and the delta-method interval [0.86, 1.18] does not exclude 0.95, so
the violation is not itself statistically decisive. Candidate
explanations are the unit-income restriction forcing ln(y^p/y) to
absorb low-frequency consumption-to-income drift, and the measure's
non-causal, post-2014 tail-extrapolated construction (§4.4); under the
causal real-time variant the coefficient reverses sign entirely (§7.4),
so the strong positive gearing is a property of the full-sample
*measurement*, not an operational forecasting relationship.

Third, the wealth-magnitude comparison with Williams now *rejects* as
well as accepts: γ_NLA's interval [0.022, 0.098] excludes his 0.159,
while γ_IFA's interval comfortably includes his 0.022 (§7.3.1). The
agreement is on form and on the illiquid-financial channel; the
net-liquid magnitude is genuinely smaller in Australia post-1988 than
in his 1978–2008 sample.

### 7.0.1 The calibration route does not transfer (Spec 12, Spec 10)

Because the interactions cannot be freely identified, the natural
single-equation response is Williams' own: calibrate the credit channels
and estimate only what the data can support. Spec 12 imposes Williams'
scale-robust calibrations (γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93) via an
iterative fixed-point offset and frees only the housing-collateral m.p.c.,
the net-liquid m.p.c. and λ. (His real-rate, affordability and intercept
loadings cannot be imposed at their published magnitudes: the repo's
percent-scaled real rate against a unit-normalised CCI makes α₁ = −0.871
roughly thirty times too large, diverging the fixed point. The CCI-support
problem cuts deeper than scale: on the deployed index, which ranges over
[−2.1, 1] rather than Williams' [0, 0.8], the imposed gearing
ψ(CCI) = 0.20 + 0.93·CCI is *negative* in the post-2019 regime and
breaches Williams' own ψ ≤ 0.95 cap at the peak — part of the collapse
below is therefore the calibration being evaluated on a support it was
never defined for, a caveat that applies to any structural reading of
the imposed-calibration specifications.)

The result is decisive and negative: **imposing Williams' permanent-income
calibration collapses the error-correction to λ ≈ 0** (Spec 12:
λ = −0.030, t = −0.74, flipping sign pre-COVID; independently reproduced
by the pre-existing Spec 10, λ = −0.048, t = −0.78, which keeps the rate
and affordability channels free). The mechanism is that the Australian
data freely estimate a structural permanent-income gearing of order one
(§7.0) — several times Williams' 0.20 — so forcing his value injects a
large, mis-signed contribution that destroys the equilibrium. The LIVES
*structure* transfers; Williams' Australian *calibrations* do not. This
sits with, and explains, the companion paper's Wald result that the
joint calibration is *not rejected* (χ²(6) = 7.55, p = 0.27;
`LIVES/outputs/williams_calibration_wald.csv`): the freely estimated
Spec 6 coefficients are too imprecise to reject Williams' values
jointly, but imposing them still ruins the fit — low power is not the
same as good fit. Sharpening the credit channels therefore requires
either the four-equation FIML system (whose cross-equation restrictions
supply the identifying variation the single equation lacks) or a
pre-1988 back-extension that recovers the financial-liberalisation
episode — not a single-equation calibration.


### 7.1 Conventional disaggregated specification (Spec 6) — baseline

Spec 6 is the constant-marginal-propensity disaggregated ECM. We retain it
here not as the preferred or headline result — that role belongs to the
faithful LIVES form of §7.0 — but as the **conventional baseline** against
which Spec 11 is the faithful alternative. It is the specification an earlier
draft, and much of the Australian aggregate-wealth-effect literature, would
treat as "the LIVES equation": it carries plain `ha_y`, `eq_y`, `super_y` and
`nla_y` with no credit scaling, and CCI enters only as a short-run term
(`d2_logcci_lag2`). It is therefore the right object against which to make the
form-is-decisive point concrete, not the equation we recommend.

Over the full 1988Q3–2024Q4 sample Spec 6 fits on **n = 86** after lag
truncation. The binding constraint is `cci_ratio` from ABS Cat 5601.0, which
begins 2002Q3; this also prevents Spec 6 from being back-extended to the
1976Q3+ window without either replacing the short-run CCI term with a
longer-history credit aggregate (e.g. Δ²log of `credit_total_d02`) or setting
it to zero pre-2002. We retain the 2002Q3+ binding constraint here and report
the back-extension exercise on the simpler Spec 4 in §7.3 and §8.15.

The long-run coefficients of Spec 6 under canonical Italy LP are
([`australia_full_results.csv`](../outputs/australia_full_results.csv), Spec 6
rows):

| Term | OLS coef | NW SE | t-stat | Implied γ (= OLS/\|λ\|) | Sign OK |
|---|---:|---:|---:|---:|:-:|
| `ha_y`                 | +0.0022  | 0.0076 | +0.30 | +0.009 | ✓ |
| `nla_y`                | +0.0083  | 0.0351 | +0.24 | +0.035 | ✓ |
| `eq_y`                 | −0.0156  | 0.0517 | −0.30 | −0.065 | ✗ |
| `super_y`              | +0.0060  | 0.0091 | +0.66 | +0.025 | ✓ |
| `ln_hp_over_y`         | +0.0102  | 0.0444 | +0.23 | +0.043 | n/a |
| `real_rate`            | −0.00053 | 0.0011 | −0.46 | −0.0022 | ✓ |
| `ln_yp_over_y`         | +0.3253  | 0.2173 | +1.50 | +1.363 | n/a |
| `ln_yp_over_y_post2008`| +0.1704  | 0.1965 | +0.87 | +0.714 | n/a |
| **`ecm_lag` (λ)**      | **−0.2386** | **0.0935** | **−2.55** | (=1) | ✓ |

(Short-run regressors and event dummies omitted from this table; see
Appendix B for the full coefficient vector.)

The contrast with Spec 11 is now sharper than a simple
significant-versus-insignificant split. In summary:

- **Speed of adjustment.** λ = −0.239 (NW SE 0.093), t = −2.55,
  p = 0.013 — significant at the 5 per cent level and about 83 per cent of
  Williams' published −0.286. But the significance leans on the COVID
  quarters: the pre-COVID estimate collapses to −0.087 (t = −0.79,
  [`australia_precovid_results.csv`](../outputs/australia_precovid_results.csv)),
  where Spec 11's pre-COVID λ is −0.266 with t = −4.85 (§7.0). The
  error-correction is identified in this form only when the COVID episode
  supplies the variation.
- **Housing wealth.** OLS coefficient +0.0022, t = 0.30 — statistically
  indistinguishable from zero as a *standalone* level. This is the
  coefficient the theory predicts to be ≈ 0 absent the CCI interaction;
  reading it as a failed housing wealth effect is the category error §7.0
  identifies. The implied structural γ_HA = 0.009 is now far below Williams'
  0.0488 (the numerical coincidence an earlier draft reported did not
  survive the deflator fix to `ln_hp_over_y` and the pipeline re-run; §7.3).
- **Net liquid assets.** OLS +0.0083 (t = 0.24, insignificant), implied
  γ_NLA = 0.035 — about a fifth of Williams' 0.159, though correctly signed.
  The γ_LA + γ_LOANS = 0 cross-equation restriction is accepted at the
  5 per cent level (§8.5).
- **Illiquid financial wealth.** Decomposed into equities (γ = −0.065,
  wrong-signed but t = −0.30, statistically indistinguishable from
  zero) and superannuation (γ = +0.025, t = 0.66); the combined
  γ_IFA = −0.040 is wrong-signed, dragged below zero by the equities
  component. The negative point estimate on equities is a small-sample
  identification artefact of the disaggregated split: combining the two
  illiquid components into a single `ilfa_y` ratio (as Spec 11 does, §7.0)
  restores a positive, significant coefficient (+0.0155, t = 3.09).
- **House-price affordability.** OLS +0.0102 (t = 0.23), implied γ = +0.043;
  Spec 6 does not include the affordability × (1 − ϖ·CCI) interaction that
  Williams' framework uses to identify this channel, so the level coefficient
  is not a like-for-like comparison with his −0.13.
- **Real mortgage rate.** OLS −0.00053, insignificant in the level; the
  credit-conditions-contingent rate effect Williams models enters only
  through the `r × CCI` interaction, which Spec 6 omits.
- **Permanent income.** Base coefficient +0.325 (SE 0.217, t = 1.50), plus a
  post-2008 break of +0.170 (SE 0.196, t = 0.87) — neither individually
  significant. The implied structural gearing at CCI = 0 is 1.36, well above
  Williams' calibrated 0.20 (§7.3.1).
- **Diagnostics.** adj-R² = 0.80, Durbin–Watson 2.15, AR(1) p = 0.34,
  AR(4) p = 0.30 (no serial correlation), RESET p = 0.0003
  (functional-form misspecification remains), heteroskedasticity
  structural (NW HAC SEs throughout;
  [`australia_full_diagnostics.csv`](../outputs/australia_full_diagnostics.csv)).
  λ is sign-stable across all four sample variants (full −0.239, pre-COVID
  −0.087, COVID-dropped −0.162, COVID-rich −0.177;
  [`australia_lambda_robustness.csv`](../outputs/australia_lambda_robustness.csv)).

The key reading is comparative, not absolute: Spec 6 now delivers a
significant full-sample error-correction, but its wealth channels are
individually unidentified (no wealth t-ratio exceeds 0.7), one of the four
is wrong-signed, and the λ identification evaporates pre-COVID. Spec 11
recovers the same theory on the same data with significant, correctly signed
core wealth channels and a λ that survives every COVID treatment — the
difference is the functional form, not the sample, the data vintage, or the
estimator.

### 7.2 Diagnostics summary

Diagnostic results for all fourteen specifications (full sample) are
summarised below; full per-spec output is in
[`australia_full_diagnostics.csv`](../outputs/australia_full_diagnostics.csv)
(pre-COVID counterparts in
[`australia_precovid_diagnostics.csv`](../outputs/australia_precovid_diagnostics.csv)).

| Spec | n | adj R² | DW | AR(1) p | AR(4) p | RESET p | Het p (diagnosis) | BIC |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 (LogNetWorth)        | 146 | 0.731 | 2.32 | 0.026 | 0.093 | <0.001 | <0.001 (struct) | −919.3 |
| 2 (LogNetWorth_CCI)    | 86  | 0.772 | 2.44 | 0.020 | 0.165 | 0.003  | <0.001 (struct) | −501.7 |
| 3 (LevelNetWorth)      | 146 | 0.731 | 2.32 | 0.025 | 0.088 | <0.001 | <0.001 (struct) | −919.4 |
| 4 (Disagg_NoCCI)       | 146 | 0.726 | 2.32 | 0.018 | 0.067 | <0.001 | <0.001 (struct) | −905.3 |
| 5 (FullDisagg)         | 86  | 0.802 | 2.25 | 0.107 | 0.087 | <0.001 | <0.001 (struct) | −494.6 |
| 6 (conventional baseline) | 86 | 0.804 | 2.15 | 0.339 | 0.301 | <0.001 | <0.001 (struct) | −492.5 |
| 6b (LongHistSRCCI)     | 180 | 0.703 | 2.12 | 0.275 | 0.134 | <0.001 | <0.001 (struct) | −1114.0 |
| 7 (CohortBurden)       | 86  | 0.834 | 2.21 | 0.203 | 0.050 | 0.018  | 0.002 (struct)  | −500.6 |
| 7b (RBABurden)         | 64  | 0.872 | 2.16 | 0.327 | 0.004 | 0.109  | 0.003 (struct)  | −364.5 |
| 8 (CCI_Interactions)   | 146 | 0.827 | 1.92 | 0.494 | 0.057 | 0.001  | <0.001 (struct) | −952.8 |
| 9 (KalmanCCI)          | 146 | 0.735 | 2.18 | 0.140 | 0.018 | <0.001 | <0.001 (struct) | −890.6 |
| 10 (WilliamsPrior)     | 86  | 0.778 | 2.10 | 0.510 | 0.230 | 0.085  | <0.001 (struct) | −493.2 |
| **11 (LIVES_Headline)**| 146 | 0.824 | 1.90 | 0.448 | 0.055 | 0.001  | <0.001 (struct) | **−954.8** |
| 12 (LIVES_Calibrated)  | 146 | 0.686 | 2.08 | 0.555 | 0.009 | 0.003  | <0.001 (struct) | −892.3 |

(Table regenerated from `australia_full_diagnostics.csv`; AR(1)/AR(4) are
Breusch–Godfrey p-values, "Het p" is the Breusch–Pagan-type LM p-value and
the parenthetical is the `het_diagnosis` field. The diagnostics file also
records a `chow_method` column for the 2008Q3 stability statistic: where
`strucchange::sctest` is singular — because event dummies are all-zero in
one subsample — the p-value comes from a manual split-sample Chow F-test on
the coefficients estimable in both subsamples, labelled
`manual_common_coef`; previously those specifications "failed" the
stability screen by incomputability rather than by evidence.)

Four diagnostic patterns are worth noting. First, **heteroskedasticity
is structural in every full-sample specification** — the LM
rejection survives dropping the event quarters in every case — so
the Newey–West HAC standard errors used throughout are necessary rather
than a precaution. Second, **RESET rejects in every specification bar
the Williams-prior Spec 10** (p = 0.085) **and the short-sample Spec 7b**
(p = 0.109), indicating functional-form misspecification that the
single-equation framing does not fully resolve
(consistent with §5.3/§7.3/§9); the faithful Spec 11 is not exempt
(RESET p = 0.001). Third, **low-order serial correlation is concentrated
in the aggregated and disaggregated-no-CCI forms**: the Breusch–Godfrey
AR(1) test rejects for Specs 1–4 but for none of the richer forms, so the
disaggregated and credit-spline dynamics absorb the serial dependence the
aggregated specifications leave in the residual; at the fourth order the
rejections sit instead in the burden, Kalman and calibrated forms (Specs 7,
7b, 9, 12), with Spec 8 (p = 0.057) and Spec 11 (p = 0.055) borderline.
Fourth, **the headline LIVES form (Spec 11) now carries the best BIC of any
n = 146 specification, including the over-parameterised Spec 8** (−954.8 vs
−952.8): the form correction is not bought at the cost of fit. The automated
selector nonetheless prefers Spec 3, because Spec 11 fails the λ-bound and
stability screens (§6.3) — the selector divergence is about admissibility
screens, not fit.

### 7.3 Comparison with Williams (2010, 2012): the conventional baseline

The faithful comparison with Williams — where the LIVES structure recovers
his error-correction speed and core wealth m.p.c.s — is in §7.0 and is taken
up systematically in §9. Here we present the comparison for the
*conventional baseline* Spec 6, because that comparison is what an earlier
draft (and much of the literature) would have offered as the headline.

We compare Spec 6 to Williams' published Table 1 estimates from the
full working-paper version of Muellbauer and Williams (2012) (CEPR
Discussion Paper 8386, revised 12 April 2012; the 7-page *BIS Papers*
No. 64 chapter is a condensed version that does not itself contain
Table 1). Williams reports
**structural** long-run coefficients γ; our OLS coefficients relate
to those γ by the ECM identity OLS_coef = λ × γ, so a difference in
the implied γ can come from either the OLS coefficient or λ.
Reporting both forms separates the two channels
([`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv)):

| Term | Williams γ | Williams implied OLS | Our OLS | Our γ | OLS gap | γ gap |
|---|---:|---:|---:|---:|---:|---:|
| **λ**                       | **−0.2860** | (same)  | **−0.2386** | (same)  | **−17 %** | (same) |
| Housing wealth `ha_y`       | 0.0488     | 0.0140  | 0.0022     | 0.0094  | −84 %    | −81 %  |
| Illiquid `eq_y + super_y`   | 0.0220     | 0.0063  | −0.0096    | −0.0403 | wrong sign | wrong sign |
| Net liquid `nla_y`          | 0.1590     | 0.0455  | 0.0083     | 0.0350  | −82 %    | −78 %  |
| log(HP/y)                   | −0.1300    | −0.0372 | +0.0102    | +0.0429 | wrong sign | wrong sign |
| ψ at CCI = 0                | 0.2000     | 0.0572  | 0.3253     | 1.3632  | —        | —      |

(The combined illiquid row sums the `eq_y` and `super_y` OLS coefficients
and their structural values from `australia_full_results.csv`; the negative
total is driven entirely by the wrong-signed equities split, §7.1.)

Mechanically, the picture has inverted relative to an earlier draft of this
paper (which was estimated on a deflator-contaminated `ln_hp_over_y`
regressor; Appendix D). The speed of adjustment is now the coefficient that
*agrees* with Williams — our −0.239 sits 17 per cent below his −0.286, well
inside one standard error — while the disaggregated wealth OLS coefficients
sit roughly 80 per cent below his implied OLS values. Because the λ gap is
now small, the ECM identity γ = OLS/|λ| no longer rescales the wealth
deficits away: γ_HA comes out at 0.0094 against Williams' 0.0488, γ_NLA at
0.035 against 0.159, and the combined illiquid γ is wrong-signed. There is
no offsetting-deficits arithmetic to report; the conventional baseline
matches Williams on speed and misses him on every wealth channel.

Two readings discipline this. First, none of the wealth gaps is sharply
estimated: §7.3.1 shows the Spec 6 delta-method intervals are wide enough
to contain Williams' value *and* zero for every wealth coefficient, so the
80 per cent shortfalls are imprecise, not rejections. Second, and more
fundamentally, Spec 6 is not the LIVES equation: its housing channel is a
standalone level that the theory predicts to be ≈ 0, so neither a match nor
a miss against Williams' credit-unlocked m.p.c. carries structural content.
The faithful Spec 11 makes the housing channel explicit
(γ₁ = 0.0055, t = 0.71) — right-signed, weakly identified, and below
Williams numerically — which is the honest read on what the single-equation
Australian data support. The §9 comparison is therefore built on Spec 11,
not on this baseline.

A natural follow-up question is whether the simpler disaggregated
no-CCI specification (Spec 4) — closer in form to Williams' Table 1 long-run
cointegrating regression than Spec 6 is — would align with Williams' Table 1
on a longer sample. Refitting Spec 4 on the back-extended 1976Q3+ sample
using the disaggregated wealth proxies (§3.13;
[`spec46_extended_comparison.csv`](../outputs/spec46_extended_comparison.csv);
structural long-run coefficients γ = OLS/|λ|):

| LR coefficient (γ) | 1988+ baseline (n=146) | 1976+ extended (n=190) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.182 | −0.203 | −0.286 |
| nla_y       | +0.025 | +0.001 | +0.159 |
| eq_y        | −0.043 | −0.041 | +0.022‡ |
| super_y     | +0.014 | −0.005 | (incl.)‡ |
| ha_y        | +0.019 | +0.018 | +0.0488 |

‡ Williams reports a single illiquid-financial-asset MPC (γ_IFA = 0.022), shown here against the combined eq_y + super_y. Williams' structural MPCs (γ_HA = 0.0488, γ_NLA = 0.159, γ_IFA = 0.022) are as reported in Muellbauer and Williams (2012): γ_NLA and γ_IFA are Table 1's raw coefficients; γ_HA = 0.0488 is the paper's derived peak housing MPC, not the raw Table 1 coefficient (γ₁ = 0.0606). Values recorded in [`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv), the same source used in §7.3's headline comparison and §11.4.

On Spec 4 the speed of adjustment moves 11.5 per cent toward
Williams (−0.182 → −0.203, still 29 per cent short of −0.286), but
the individual wealth coefficients become *smaller*, not larger:
γ_NLA collapses by 95 per cent toward zero, γ_SUPER flips sign, and
γ_EQ retains a wrong sign. The back-extension therefore does not push
the Spec 4 estimates closer to Williams' Table 1; the post-1988
sample window is not, in itself, what generates the divergence
between Spec 4 and Williams' values. **Sample length is not the
binding constraint.**

Reading the two exercises together points to the same conclusion as §7.0.1:
the wedge between any single-equation OLS estimate and Williams' system FIML
is the single-equation framing itself, not sample length, knot count, or
sign-prior structure. The placebo evidence (§5.2) and the two-equation SUR
result (§5.4) — in which joint estimation delivers a negligible
cross-equation residual correlation and no efficiency gain — corroborate
this reading.

We do *not* present Spec 8 (the free six-interaction form, λ = −0.458) as a
CCI success here. Adding the full Williams CCI interaction set re-allocates
the long-run identification across wealth components (γ_HA = 0.007,
γ_IFA = 0.044, γ_NLA = 0.073) without moving systematically toward Williams'
Table 1, and three of its four sign-priced interaction coefficients carry
the wrong sign against their priors (§5.5, §8.4). The magnitude of its λ
reflects the collinear interaction set absorbing variation, not a sharpening
of the credit channel.

### 7.3.1 How precisely is the structural profile identified?

The implied structural coefficients γ_i = β_i/|λ| are ratios of two
imprecisely estimated quantities, so they inherit sampling uncertainty
that the point-estimate comparison above conceals. We compute, for both
the conventional baseline (Spec 6, n = 86) and the faithful LIVES headline
(Spec 11, n = 146), delta-method standard errors from the Newey–West
covariance of (β_i, λ) — which carries the correlation between each
numerator and the speed of adjustment — and, as a cross-check, a seeded
moving-block residual bootstrap (block length 8, B = 1000). Both blocks
are committed in
[`australia_gamma_inference.csv`](../outputs/australia_gamma_inference.csv).

For **Spec 6** the intervals are wide enough to be almost uninformative:

| Term (Spec 6) | Implied γ | 95% CI (delta) | Williams Table 1 | Williams in CI? |
|---|---:|---:|---:|:-:|
| Housing `ha_y`           | 0.009  | [−0.056, 0.075] | 0.0488 | ✓ |
| Net liquid `nla_y`       | 0.035  | [−0.262, 0.332] | 0.159  | ✓ |
| Equities `eq_y`          | −0.065 | [−0.468, 0.337] | 0.011  | ✓ |
| Super `super_y`          | 0.025  | [−0.040, 0.091] | 0.011  | ✓ |
| log(HP/y)                | 0.043  | [−0.308, 0.393] | −0.130 | ✓ |
| Permanent income         | 1.363  | [0.285, 2.442]  | 0.200  | ✗ |
| **Wealth aggregate (Σ)** | **0.004** | **[−0.659, 0.668]** | 0.230 | ✓ |

Williams' Table 1 value lies inside the 95 per cent interval for every
Spec 6 coefficient except the permanent-income gearing (whose interval
[0.29, 2.44] excludes his calibrated 0.20 from above) — but the same
intervals also contain zero for every wealth channel and values far from
Williams. The bootstrap agrees (median γ_HA = 0.008, 95 per cent interval
[−0.049, 0.107]). On the conventional baseline, then, the data cannot
distinguish Williams' wealth profile from a broad range of alternatives,
including no wealth effect at all: **non-rejection driven by imprecision,
not confirmation**.

For **Spec 11** the intervals are several times tighter, and they become
informative in *both* directions:

| Term (Spec 11) | Implied γ | 95% CI (delta) | Williams Table 1 | Williams in CI? |
|---|---:|---:|---:|:-:|
| Housing × CCI `ha_x_cci` (γ₁) | 0.0055 | [−0.010, 0.021] | 0.0488 | ✗ |
| Net liquid `nla_y`            | 0.060  | [0.022, 0.098]  | 0.159  | ✗ |
| Illiquid `ilfa_y`             | 0.035  | [0.012, 0.057]  | 0.022  | ✓ |
| Affordability `hp_x_1_minus_cci` | 0.062 | [0.027, 0.098] | −0.130 | ✗ |
| Permanent income (ψ₀)         | 1.024  | [0.865, 1.184]  | 0.200  | ✗ |
| PI × CCI `yp_x_cci` (ψ₁)      | −1.138 | [−2.099, −0.177]| 0.930  | ✗ |
| **Wealth aggregate (Σ)**      | **0.100** | **[0.036, 0.164]** | 0.230 | ✗ |

The bootstrap intervals are again close to the delta-method ones (e.g.
γ_NLA [0.013, 0.111], γ_IFA [0.005, 0.065]). The Spec 11 read is therefore
no longer a blanket non-rejection. The data **accept** Williams' illiquid
financial m.p.c. (0.022 sits comfortably inside [0.012, 0.057]) but
**reject** his net-liquid magnitude (0.159 against an upper bound of
0.098), his credit-unlocked housing m.p.c. (0.0488 against [−0.010,
0.021]), the sign of his affordability loading, the magnitude of his
permanent-income gearing, and the sign of his permanent-income credit slope.
The agreement with Williams is on *form* — the error-correction speed and
the significance pattern of the wealth channels — not on the credit-channel
calibrations, which is the same verdict §7.0 and §9 reach from the point
estimates.

For use in a calibrated model (MARTIN, §10.3), the policy-relevant
summary is Spec 11's aggregate long-run wealth coefficient
γ_W = 0.100 (95% CI [0.036, 0.164]): positive and bounded away from zero,
but below both Williams' 0.230 and MARTIN's calibrated net-wealth
elasticity of ≈ 0.17 (the latter sits just outside the upper bound). The
Spec 6 aggregate (0.004, CI [−0.66, 0.67]) is too imprecise to discipline
anything.

(Caveat: both the delta method and the residual bootstrap hold the
right-hand side fixed, so they propagate sampling uncertainty in the
ECM coefficients but **not** the first-stage uncertainty in the
generated permanent-income and CCI regressors — the permanent-income
projection and the CCI are held at their full-sample values in both
methods; the true intervals are therefore at least this wide. The
real-time permanent-income sensitivity of §7.4 and §8.9 gives a partial
read on the first-stage component.)

### 7.4 The Italy / AR comparison and the real-time check

The permanent-income measure matters materially for two coefficients —
the speed of adjustment and the long-run permanent-income coefficient.
Refitting Spec 6 (n = 86) under each measure on a common data flow
([`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv)):

| Term | AR (real-time) | Italy full-sample (headline measure) | Italy real-time | Williams |
|---|---:|---:|---:|---:|
| `ecm_lag` (λ)   | −0.095 (t = −1.68) | **−0.239 (t = −2.55)** | **−0.159 (t = −2.39)** | **−0.286** |
| `ln_yp_over_y`  | −0.158 (t = −1.68) | **+0.325 (t = 1.50)** | **−0.145 (t = −2.23)** | (calib. 0.20) |
| `ha_y` (OLS)    | +0.0135 (t = 1.76) | +0.0022 (t = 0.30)    | +0.0024 (t = 0.30)    | (implied 0.014) |

(The refit now reproduces the canonical pipeline's Spec 6 estimates exactly
under the full-sample Italy measure — λ = −0.2386, matching
`australia_full_results.csv` — since the pipeline-isolation fix described
in §8.2. The `ha_y` row reports the OLS coefficient, which equals the
implied γ only after dividing by |λ|, consistent with the CSV.)

Two readings follow. First, the full-sample Italy measure multiplies |λ|
by roughly two and a half relative to AR (−0.095 → −0.239) and flips the
long-run permanent-income coefficient from negative (−0.158, t = −1.68 —
the "Australian permanent-income puzzle") to positive (+0.325, in sign
agreement with theory and Williams' calibrated value, though insignificant
at t = 1.50). Second — the operational caveat — **only part of either move
survives a causal real-time projection.** The real-time Italy variant keeps
roughly two-thirds of the full-sample |λ| (−0.159 against −0.239) and is
now itself significant (t = −2.39), so about a third of the headline |λ| is
attributable to look-ahead rather than to the Italy projection per se. The
permanent-income coefficient, by contrast, returns to significantly
negative under the causal measure (−0.145, t = −2.23). The positive-PI sign
is therefore a property of the full-sample, two-sided measure, not of the
real-time forecaster a model like MARTIN would use.

We retain the structural reasons the AR and Italy measures diverge —
the rolling AR(8) forecaster lacks the labour-force-share predictor
that captures Australia's slow-moving demographic effects on trend
income, compounds short-run AR misspecification across 40 horizons,
and over-estimates persistence after large income shocks, all of which
the one-step direct projection avoids — but we read the puzzle's
reversal as a feature of full-sample permanent-income *measurement*
rather than a clean real-time resolution. We carry the full-sample
measure as the headline and the real-time and AR variants as
robustness columns (§8.9), and we flag throughout that the strong
permanent-income response in the faithful Spec 11 (t = 4.0, §7.0)
inherits this look-ahead caveat: it is conditioned on the full-sample
permanent-income forecaster, and the real-time read is the operational
lower bound.

---


## 8. Robustness

We run the Italian-style robustness suite of De Bonis, Liberati,
Muellbauer and Rondinelli (2020) on the automated-selector
specification (Spec 3, §6.3), on the conventional constant-MPC baseline
(Spec 6) where the battery requires the disaggregated wealth split, and —
in parallel, with a `_spec11` suffix on the committed outputs — on the
faithful LIVES specification (Spec 11). The suite is deliberately
weighted toward exposing rather than concealing weakness: it spans
estimator choice (OLS, IV, SUR), break testing, the credit-conditions
construction, the permanent-income measure, the affordability
adjustment, sample length, and out-of-sample forecast accuracy. Several
of the batteries return honest negatives — the multi-step out-of-sample
loss to a random walk, the near-zero cross-equation residual
correlation, the below-median placebo percentiles for the literal
Williams construction, and the partial reversal under a causal
permanent-income measure. We report these prominently. They are
substantive findings: a freely-estimated single-equation framework
permits them to surface, whereas the imposed restrictions of a
four-equation FIML system would hide them, and each points to why FIML
and pre-1988 back-extension are the routes forward (§5, §9).

### 8.1 OLS vs IV on current income (Hall 1978 endogeneity)

Because current income enters both the error-correction term
(ecm_lag = ln c_{t−1} − ln y_t) and the permanent-income gap
(ln_yp_over_y), the exercise now instruments **both** regressors —
an upgrade on an earlier draft, which treated ecm_lag as exogenous and
instrumented only the PI gap. The instrument set is income lagged one,
two and four quarters, unemployment lagged one and two quarters, and
the mortgage rate lagged one quarter (six instruments); both the OLS
and IV columns carry Newey–West HAC standard errors
([`australia_iv_robustness.csv`](../outputs/australia_iv_robustness.csv),
Spec 3, n = 146).

Instrumenting moves the speed of adjustment *away* from zero:
ecm_lag = −0.191 (OLS) → −0.258 (IV, HAC SE 0.110), +35 per cent, and
the permanent-income coefficient from +0.196 to +0.280 (+43 per cent);
the small net-worth coefficient flips sign (+0.0011 → −0.0009) but is
statistically indistinguishable from zero either way. The IV
diagnostics
([`australia_iv_diagnostics.csv`](../outputs/australia_iv_diagnostics.csv))
say the exercise is well-posed but the endogeneity it corrects is
marginal: the weak-instrument F-statistics are 73.6 (ln_yp_over_y) and
28.9 (ecm_lag), both far above conventional thresholds; the Wu–Hausman
test does not reject OLS exogeneity at 5 per cent (p = 0.095); and the
Sargan over-identification test does not reject instrument validity at
5 per cent (p = 0.074), though both are close enough to the boundary
to keep honest.

Run on the faithful LIVES specification
([`australia_iv_robustness_spec11.csv`](../outputs/australia_iv_robustness_spec11.csv),
[`australia_iv_diagnostics_spec11.csv`](../outputs/australia_iv_diagnostics_spec11.csv),
n = 146), the exercise is more demanding and correspondingly less
conclusive, and we read it diagnostically rather than confirmatorily.
Correcting an error in an earlier run — which had let three of the CCI
interactions instrument themselves — the endogenous set is now the five
income- and wealth-carrying terms (ecm_lag, ln_yp_over_y, yp_x_cci,
ha_x_cci and hp_x_1_minus_cci) against the same six instruments, which
leaves only a single overidentifying restriction. The Wu–Hausman test
still rejects exogeneity (p = 0.002), so current-income endogeneity is a
live concern in the LIVES form; but the first stages are uneven — the
weak-instrument F-statistics are 126.0 (ln_yp_over_y), 55.5 (ha_x_cci),
32.5 (ecm_lag) and 15.2 (hp_x_1_minus_cci), yet only 6.5 for yp_x_cci,
below the conventional weak-instrument threshold. λ does move away from
zero (−0.448 → −0.503, +12 per cent), in the direction of a *faster*
error-correction; but the wealth-channel point estimates cannot bear
weight. nla_y attenuates to statistical insignificance
(0.0269 → 0.0040, HAC t ≈ 0.3), and the two housing interactions flip
sign relative to OLS (ha_x_cci +0.0025 → −0.013;
hp_x_1_minus_cci +0.028 → −0.015). With only one overidentifying
restriction (Sargan p = 0.251, a near-vacuous test at df = 1) and a weak
first stage on yp_x_cci, these movements are not reliable evidence for
or against the wealth channel in either direction — the IV design is
underpowered for Spec 11. What the exercise establishes is narrower than
the earlier draft claimed: that endogeneity matters here (Wu–Hausman),
not that the LIVES wealth m.p.c.s survive instrumentation.

### 8.2 Joint permanent-income + consumption SUR

Estimating the consumption equation jointly with the permanent-income
equation by SUR
([`australia_joint_pi_robustness.csv`](../outputs/australia_joint_pi_robustness.csv),
Spec 3, n = 146) leaves every coefficient within sampling noise of the
single-equation OLS values. The largest level shift among the long-run
terms is in the speed of adjustment (ecm_lag −0.191 → −0.204, +7.0 per
cent in relative terms but barely a hundredth in level), with the
permanent-income coefficient moving −4.5 per cent (0.196 → 0.187). On
the faithful LIVES specification
([`australia_joint_pi_robustness_spec11.csv`](../outputs/australia_joint_pi_robustness_spec11.csv),
n = 146) the stability is even tighter: λ −0.448 → −0.457 (+1.9 per
cent), nla_y −0.2 per cent, ilfa_y +2.6 per cent, ln_yp_over_y −1.4
per cent. Single-equation OLS is therefore an acceptable estimator for
the consumption block, and — consistent with the two-equation SUR
result in §8.18 — the case for the multi-equation framework rests on
cross-equation parameter restrictions, not on residual covariance or
efficiency gain.

Two honest disclosures attach to this battery. First, the second
equation is a *one-step-ahead* income-growth forecast, not the
k = 40-quarter discounted projection that defines the permanent-income
regressor, and the SUR does not propagate the first-stage uncertainty
of the generated permanent-income series into the consumption-equation
standard errors — it tests residual covariance, not
generated-regressor bias (the §7.3.1 caveat applies here too). Second,
the IV and SUR tables in a previous draft had been produced on data
contaminated by a pipeline-ordering bug: the diagnostic that builds
the AR permanent-income series had overwritten the canonical
Italy-method series in the shared model dataset before the robustness
batteries ran. The pipeline now isolates each sub-script's data flow,
and the committed tables here and in §8.1 are estimated on the
canonical data (which is why the §7.4 refit now matches
`australia_full_results.csv` exactly).

### 8.3 Chow battery and multiple-break tests

Chow tests on the selector specification
([`australia_chow_battery.csv`](../outputs/australia_chow_battery.csv),
Spec 3, n = 146) do not reject parameter stability at 1995Q1 (stat
0.380, p = 0.978; n_pre = 27, n_post = 119), 2000Q1 (0.740, p = 0.730)
or 2008Q3 (0.529, p = 0.912), while the 2020Q1 break is strongly
rejected (Chow stat 11.12, p = 8.9 × 10⁻¹⁶; n_pre = 127, n_post = 19)
— the COVID structural break that the event dummies absorb. On the
faithful LIVES specification
([`australia_chow_battery_spec11.csv`](../outputs/australia_chow_battery_spec11.csv))
1995Q1 (p = 0.844) and 2000Q1 (p = 0.152) are stable, but the 2008Q3
break is rejected at 5 per cent (stat 1.914, p = 0.017) — consistent
with the GFC being exactly where Spec 11's credit-interaction
structure starts to bind (the deployed CCI's first knot is 2007Q3,
§5.1.1) — and the 2020Q1 statistic is not computable (`sctest`
singular on the COVID-dummied subsample; the CSV records the failure).
The Bai–Perron / CUSUM battery
([`australia_breaks.csv`](../outputs/australia_breaks.csv), Spec 3)
returns a single dominant break: supF = 169.96 (p = 0) dated 2019Q1 —
the trimming convention places the date at the segment boundary
adjacent to the COVID episode — with a CUSUM p-value of 0.971
(recursive residuals stable away from that episode). For the selector
specification the break structure is therefore concentrated at COVID,
not at the GFC; for the LIVES form a 5 per cent GFC break is the
honest additional finding.

### 8.4 Williams CCI interactions (Spec 8) — reallocation, not identification

Spec 8 enters all six Williams CCI interactions freely on the 1988Q3+
sample with the reduced-form `cci_williams`. Following the §4.7
convention (Williams' own), it is the interacted economic *variable*
that is de-meaned — over the 1980Q1-onwards window on which the CCI is
observed — while the CCI itself enters the interaction raw, so that
each interaction term has a clean conditional interpretation rather
than absorbing an implicit linear CCI level shift. The sign-prior
verdicts
([`australia_spec8_sign_prior_verdicts.csv`](../outputs/australia_spec8_sign_prior_verdicts.csv);
full vector in
[`australia_full_results.csv`](../outputs/australia_full_results.csv),
n = 146) are:

| Williams interaction | Sign prior | OLS coef | t | p | Verdict |
|---|---:|---:|---:|---:|---|
| `r × CCI`                     | − | +0.00283 | +2.05 | 0.042 | wrong sign, significant — **FAIL** |
| `log(HP/y) × (1 − 1.2·CCI)`   | − | +0.0299  | +1.38 | 0.171 | wrong sign on composite, insignificant — **FAIL** |
| `log(y^p/y) × CCI`            | + | −0.5046  | −2.43 | 0.016 | wrong sign, significant — **FAIL** |
| `log(y^p/y)`                  | + | +0.4921  | +3.42 | 0.001 | right sign — **PASS** |
| `HA × CCI` (γ₁)               | + | +0.00426 | +0.87 | 0.384 | right sign, insignificant |

Three of the four sign-priced interactions fail. Of the free
interactions only the permanent-income level term (`ln_yp_over_y`,
+0.4921, t = 3.42) passes its prior cleanly; the housing-collateral
interaction `ha_x_cci` carries its theoretically correct positive
sign but is far from significant (+0.00426, t = 0.87). Against
Williams' Table 1 the raw-coefficient gaps remain structural
([`australia_williams_spec8_comparison.csv`](../outputs/australia_williams_spec8_comparison.csv)):
his α_c1 (r × CCI) = −0.871 against our structural +0.0062; his
α_c4 (HP/y × (1−1.2·CCI)) = −0.13 against our structural +0.065; his
calibrated ψ₁ = +0.93 against our freely-estimated structural −1.101.

The substantive effect of Spec 8 is to *re-allocate* the long-run
identification, not to recover Williams' channels. Standalone
non-housing wealth strengthens (nla_y +0.0337***, struct +0.073;
super_y +0.0215**, struct +0.047; ha_y stays small at +0.0030,
t = 0.48) and the speed of adjustment shifts from −0.239 (Spec 6) to
−0.458 (t = −3.52) on the full sample — past Williams' value in
magnitude. Spec 8 also attains the highest adjusted R² among the
n = 146 specifications (0.827) and the second-best BIC (−952.8,
behind Spec 11's −954.8). We do **not** read any of this as a
credit-conditions success. The pre-COVID estimates are revealing
([`australia_precovid_results.csv`](../outputs/australia_precovid_results.csv)):
there the standalone housing level is significant (ha_y +0.0154***,
t = 2.87) but the *interactions* turn negative — `ha_x_cci` at
−0.0069 (t = −1.42) and `hp_x_1_minus_cci` at −0.0292 (t = −1.54),
the former the wrong sign for the LIVES collateral channel — while λ
halves to −0.219 (t = −3.95). Read together with the calibration
collapse (§9, Spec 10/12) and the identification-vs-detrending
decomposition (§5), the honest reading is that the six CCI-interacted
regressors in a single equation act as flexible parameter
time-variation rather than as the structurally identified
common-factor channel that Williams' four-equation system delivers.
Their mutual collinearity (each is approximately proportional to CCI)
is the structural reason FIML is required; we discuss it as a
first-class identification result in §5.

### 8.5 Net-liquid-assets restriction γ_LA + γ_LOANS = 0

We refit each disaggregated specification with deposits/y and debt/y
entered separately and conduct a Wald test of
H₀ : γ_LA + γ_LOANS = 0 using `car::linearHypothesis` with the
Newey–West variance estimator. The restriction is **accepted at the
5 per cent level in every specification × sample combination**
([`australia_nla_restriction_test.csv`](../outputs/australia_nla_restriction_test.csv)):

| Spec | Sample | γ_LA + γ_LOANS | NW SE | t | p | Restriction |
|---|---|---:|---:|---:|---:|:-:|
| 4 | full      | +0.0325 | 0.0445 | 0.731 | 0.465 | accepted |
| 5 | full      | +0.0422 | 0.0999 | 0.422 | 0.673 | accepted |
| 6 | full      | +0.0202 | 0.0987 | 0.204 | 0.838 | accepted |
| 4 | pre-COVID | +0.0282 | 0.0285 | 0.989 | 0.322 | accepted |
| 5 | pre-COVID | −0.0222 | 0.0499 | −0.445 | 0.656 | accepted |
| 6 | pre-COVID | −0.0026 | 0.0509 | −0.051 | 0.959 | accepted |

The data cannot distinguish separate liquid-asset and debt
propensities. We read this as non-rejection-by-imprecision rather
than as positive confirmation of exact netting, but it validates the
Italian convention of netting deposits against debt and supports the
use of the constructed `nla_y` series — the net-liquid channel that
carries a significant, correctly-signed marginal propensity in the
faithful LIVES specification (Spec 11, nla_y +0.0269***, struct 0.060;
§7.0).

### 8.6 Drehmann amortising-mortgage adjusted real rate

De Bonis et al. (2020) apply the BIS Drehmann, Juselius and Korinek
(2017) amortisation-adjusted rate adjR = R / (1 − (1+R)⁻ᴺ). For
Australia we set N = 25 years (100 quarters), consistent with the
longer Australian average mortgage maturity
([`australia_drehmann_robustness.csv`](../outputs/australia_drehmann_robustness.csv),
Spec 3, n = 146). The substitution is now essentially inert, matching
the Italian invariance result: ecm_lag −0.1906 (base) → −0.1902
(Drehmann), the net-worth coefficient 0.00108 → 0.00129, the
permanent-income coefficient 0.1963 → 0.1959, and the real-rate
coefficient itself −0.00020 → −0.00015 (insignificant either way).
(The large Drehmann sensitivity an earlier draft reported was an
artefact of the contaminated data flow disclosed in §8.2.) The
exercise is undefined for the faithful LIVES specification: Spec 11
carries no plain `real_rate` term — its rate enters only through the
`r × CCI` interaction — so the Drehmann substitution cannot be
applied, and
[`australia_drehmann_robustness_spec11.csv`](../outputs/australia_drehmann_robustness_spec11.csv)
records the attempt as FAILED for that reason.

### 8.7 Scaled-income robustness

De Bonis et al. (2020) average disposable income with
labour-plus-transfer income to down-weight property-income
mismeasurement. Re-running this construction
([`australia_scaled_income_robustness.csv`](../outputs/australia_scaled_income_robustness.csv),
Spec 3, n = 146) shifts the speed of adjustment modestly from −0.1906
to −0.2111 and the permanent-income coefficient from 0.196 to 0.163,
with the net-worth coefficient roughly halving (0.00108 → 0.00061)
from an already negligible base. On the faithful LIVES specification
([`australia_scaled_income_robustness_spec11.csv`](../outputs/australia_scaled_income_robustness_spec11.csv))
the income-measure choice likewise moves λ from −0.448 to −0.525
while *strengthening* the wealth m.p.c.s (nla_y 0.0269 → 0.0344,
ilfa_y 0.0155 → 0.0175) and trimming the permanent-income coefficient
(0.459 → 0.376). The income-measure choice moves |λ| by 0.02–0.08 in
level — not negligible — but does not change the substantive ranking
or signs of the wealth coefficients in either frame.

### 8.8 Williams non-property income (NPY) robustness

Replacing the disposable-income series with `npy_real_pc` constructed
per Williams (2009) §4.2.1
([`australia_williams_income_robustness.csv`](../outputs/australia_williams_income_robustness.csv),
Spec 3, n = 146) provides the closest methodological match to
Williams' income concept (property income stripped, but not
symmetrically averaged with labour-plus-transfer income). The
substitution is now close to inert on the speed of adjustment —
ecm_lag −0.1906 → −0.1857, a 2.6 per cent change — with the
permanent-income coefficient easing from 0.196 to 0.164 and the
net-worth coefficient staying negligible (0.00108 → 0.00044). (An
earlier draft reported the NPY substitution roughly halving |λ|; that
result did not survive the §8.2 data fix.) On the faithful LIVES
specification
([`australia_williams_income_robustness_spec11.csv`](../outputs/australia_williams_income_robustness_spec11.csv))
the NPY measure trims λ from −0.448 to −0.395 while *raising* the
wealth m.p.c.s (nla_y 0.0269 → 0.0413, ilfa_y 0.0155 → 0.0203) and
easing the permanent-income coefficient (0.459 → 0.352). The income
concept is therefore not what separates our estimates from Williams':
under his own income measure the conventional λ is essentially
unchanged and the LIVES wealth channels strengthen.

### 8.9 Permanent-income method comparison (AR, full-sample Italy, real-time Italy)

§7.4 reports the headline three-way comparison in the Spec 6 frame;
the committed batteries span two specification frames, and we label
each row accordingly. The method comparison
([`australia_pi_method_comparison.csv`](../outputs/australia_pi_method_comparison.csv))
runs on the selector specification (Spec 3, n = 146 — its Italy-column
estimates match the Spec 3 rows of `australia_full_results.csv`
exactly), while the real-time battery
([`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv))
runs on the conventional baseline (Spec 6, n = 86):

| PI measure | Frame | λ (ecm_lag) | t | log(y^p/y) | t | adj-R² |
|---|---|---:|---:|---:|---:|---:|
| AR (expanding-window)      | Spec 3, n = 146 | −0.0479 | −1.12 | −0.0036 | −3.96 | 0.696 |
| Italy LP (full-sample)     | Spec 3, n = 146 | −0.1906 | −2.90 | +0.1963 | +3.41 | 0.731 |
| AR (expanding-window)      | Spec 6, n = 86  | −0.0948 | −1.68 | −0.1583 | −1.68 | — |
| Italy LP (full-sample)     | Spec 6, n = 86  | −0.2386 | −2.55 | +0.3253 | +1.50 | — |
| Italy LP (real-time)       | Spec 6, n = 86  | −0.1594 | −2.39 | −0.1451 | −2.23 | — |

(Adjusted R² from
[`australia_pi_method_meta.csv`](../outputs/australia_pi_method_meta.csv);
the Italy LP forecaster fits better in the Spec 3 frame, 0.731 vs
0.696. The AR forecaster is *always* expanding-window — it refits the
AR(8) on data through each forecast origin — so there is no separate
"full-sample AR" variant to report.)

The full-sample Italy LP measure is the headline measure (framed as a
*measurement*, §4.3), and the AR forecaster delivers the negative
"Australian permanent-income puzzle" coefficient in both frames
(significantly so in the Spec 3 frame, t = −3.96). The real-time
Italy LP variant is the operationally honest benchmark: it is causal,
re-fitting the projection at each *t* on data whose full k-quarter
horizon is realised by *t*, so it is usable at forecast time. It
shows that roughly two-thirds of the full-sample Italy |λ| is genuine
(−0.159 against −0.239, and now itself significant at t = −2.39) —
the look-ahead accounts for about a third — but that the positive
permanent-income sign is **not**: it reverses to −0.145 (t = −2.23)
under the causal measure. We carry the full-sample measure as the
headline and disclose explicitly that its positive-PI sign and part
of its λ magnitude are full-sample, non-causal properties. The same
look-ahead caveat attaches to the strong permanent-income coefficient
in the faithful LIVES specification (Spec 11, ln_yp_over_y +0.459,
t = 4.0); the real-time column is the operational robustness check.

### 8.10 Permanent-income filter sensitivity

A grid over discount factor δ ∈ {0.90, 0.95, 0.97} and horizon
k ∈ {20, 40, 60} quarters, with the GFC learning-weight ogive on and
off
([`australia_permanent_income_sensitivity.csv`](../outputs/australia_permanent_income_sensitivity.csv)),
shows the speed of adjustment to be extremely stable within the AR
method on the selector specification (Spec 3 — the grid's λ values
match the AR column of §8.9's Spec 3 frame): λ ranges only from
−0.0470 to −0.0482 across the eighteen δ × k × ogive cells (baseline
δ = 0.95, k = 40, ogive on: λ = −0.0479, structural PI weight −0.076,
structural net-worth weight +0.122). The GFC ogive toggle is now a
real switch — an earlier version of the grid carried it as a no-op,
so its on/off rows were identical; the fixed toggle moves λ by less
than 0.0003 and the structural PI weight by at most 0.03 within each
δ × k cell, so the original "no effect" conclusion survives the fix,
now for a demonstrated rather than vacuous reason. The PI weight is
more sensitive to the horizon (−0.21 at k = 20 down to −0.04 at
k = 60) but never changes sign within the AR method. Switching to an
HP-filter permanent income (λ = 1600) moves λ to −0.0820 and flips
the structural PI weight to +1.238. The within-AR-method PI tuning is
therefore not what drives the |λ| gap with Williams; the dominant
factor is the AR-versus-Italy-direct-forecast method choice itself (§8.9).

For the headline LIVES specification the corresponding sensitivities
are the GFC-ogive toggle on the Italy measure and the real-time
variant. Removing the ogive from the headline measure
([`australia_spec11_ogive_robustness.csv`](../outputs/australia_spec11_ogive_robustness.csv))
moves Spec 11's λ from −0.448 to −0.574 (t = −4.63) and the
permanent-income coefficient from +0.459 to +0.604, leaving the
structural gearing essentially unchanged (ψ̂ ≈ 1.05 against the
headline 1.02) — so the §7.0 bound violation is not an ogive
artefact. The real-time variant is §7.4/§8.9.

### 8.11 COVID-period robustness

All fourteen specifications now carry all **four** sample variants
(full, pre-COVID, COVID-dropped, COVID-rich) in
[`australia_lambda_robustness.csv`](../outputs/australia_lambda_robustness.csv)
— the COVID-rich variant was previously missing for Specs 7–12, so
this is the first complete battery. The speed of adjustment is
sign-stable across all four variants for every specification except
the two Williams-calibration-imposed ones. For the conventional
baseline (Spec 6) λ runs −0.239 (full) / −0.087 (pre-COVID) / −0.162
(COVID-dropped) / −0.177 (COVID-rich) — all correctly signed, though
the pre-COVID estimate is small and insignificant (§7.1). The
faithful LIVES specification (Spec 11) gives −0.448 (full) / −0.266
(pre-COVID) / −0.248 (COVID-dropped) / −0.242 (COVID-rich) —
sign-stable, with the full-sample value inflated by the COVID
quarters and the tightly clustered −0.24 to −0.27 of the three
COVID-controlled variants treated as the identified value (close to
Williams' −0.286; full coefficient vectors for all four variants in
[`australia_spec11_variants.csv`](../outputs/australia_spec11_variants.csv)).
Only Spec 10 (Williams-prior calibrated; sign-flips to +0.009
COVID-dropped and +0.051 COVID-rich) and Spec 12 (Williams
calibrations imposed; sign-flips to +0.041 pre-COVID and +0.015
COVID-rich) are not sign-stable across samples — an artefact of the
calibration collapse documented in §9, not of the COVID episode
itself.

### 8.12 Rolling-window estimation

A 60-quarter rolling estimation of the selector specification
(Spec 3; 87 windows ending 2003Q2–2024Q4,
[`australia_rolling_coefs.csv`](../outputs/australia_rolling_coefs.csv))
shows λ holding in a band of roughly −0.13 to −0.27 for the windows
ending before 2020, the net-worth coefficient trending down from
about +0.012 in the earliest windows to near zero through the
macroprudential era (consistent with that era flattening the
wealth-consumption transmission) and mildly negative in the latest
windows (−0.011 at 2024Q4, within one standard error of zero). The
COVID quarters destabilise the short windows: λ briefly flips to
+0.15 in the windows ending around 2021Q3 before settling at −0.44
(SE 0.19) in the final window, where the COVID quarters dominate a
60-quarter span. We do not interpret this as model instability but
as a symptom of the limited identifying variation in the
post-deregulation portion of the sample (§5): the
financial-liberalisation episode that would identify the credit
channels largely predates the 1988Q3 start of ABS sectoral
balance-sheet data.

### 8.13 Out-of-sample forecast validation

We run a rolling out-of-sample validation
([`australia_oos_rmse.csv`](../outputs/australia_oos_rmse.csv)) on
six structural specifications (Spec 4 disagg-no-CCI, Spec 6
conventional baseline, Spec 7 cohort-burden, Spec 8 Williams-CCI
interactions, Spec 9 Kalman-CCI, and now the faithful LIVES Spec 11)
over 36 expanding-window cuts at
horizons h ∈ {1, 4, 8} quarters (n = 36 at h = 1, 4; n = 32 at h = 8),
against random-walk-with-drift and AR(1) benchmarks:

| Specification | h = 1 RMSE | h = 4 RMSE | h = 8 RMSE |
|---|---:|---:|---:|
| Benchmark RW drift           | 0.03094 | 0.03094 | 0.03282 |
| Benchmark AR(1)              | 0.03703 | 0.03102 | 0.03283 |
| Spec 4 (disagg, no CCI)      | 0.03175 | 0.03182 | 0.03896 |
| Spec 6 (conventional baseline)| 0.03231 | 0.03293 | 0.04180 |
| Spec 7 (cohort-burden)       | 0.03247 | 0.03164 | 0.03540 |
| Spec 8 (Williams CCI)        | 0.02901 | 0.03323 | 0.04038 |
| Spec 9 (Kalman CCI)          | 0.03206 | 0.03299 | 0.03929 |
| **Spec 11 (LIVES headline)** | 0.02919 | 0.03517 | 0.06402 |

At h = 1 the two CCI-interaction forms now **beat** the random-walk
benchmark — Spec 8 (0.0290) and Spec 11 (0.0292) against RW-drift's
0.0309 — with the remaining structural specifications close behind;
this is a better one-step showing than an earlier draft reported. At
h = 4 and h = 8 the random walk with drift still dominates **every**
structural specification (best structural at h = 4 is Spec 7 at
0.0316 vs 0.0309; at h = 8, Spec 7 at 0.0354 vs 0.0328), and Spec 11
is the *worst* performer at h = 8 (0.0640) — plausibly the
full-sample permanent-income measure's extrapolated tail compounding
over long horizons. This is the standard macro-forecasting pattern:
the LIVES framework's identification advantage is in interpreting
historical co-movement, not in beating naive benchmarks at multi-step
prediction. We record it honestly rather than overstating forecast
performance.

A construction caveat applies to the whole exercise, and we correct
an earlier draft's claim here. As committed, the validator's
permanent-income input is the **full-sample Italy-method measure**,
not a real-time forecaster (the earlier statement that "the AR
permanent-income input is real-time" was wrong for the committed
numbers), and the credit-conditions series and its de-mean constants
are likewise full-sample objects. Both generated regressors therefore
embed information from beyond each expanding-window cut, so these
RMSEs measure **fit stability under re-estimation, not genuine
real-time forecast accuracy** — they are an upper bound on what a
fully real-time forecaster would deliver, and the h = 1 wins over the
random walk should be read in that light.

### 8.14 Back-extension robustness — Spec 1 on the 1976Q3+ sample

Refitting Spec 1 (aggregate net worth) on the back-extended sample
using `ln_networth_y_proxy`
([`spec1_extended_comparison.csv`](../outputs/spec1_extended_comparison.csv);
structural long-run coefficients γ = OLS/|λ|):

| LR coefficient (γ) | 1988+ baseline (n = 146) | 1976+ extended (n = 190) | % change |
|---|---:|---:|---:|
| λ (ecm_lag)    | −0.1934  | −0.2090  | +8.1  |
| ln_networth_y  | +0.0195  | +0.0467  | +140  |
| ln_hp_over_y   | +0.0443  | +0.0375  | −15.5 |
| real_rate      | −0.00115 | +0.00005 | sign flip |
| ln_yp_over_y   | +1.0379  | +0.9913  | −4.5  |

The speed of adjustment and the permanent-income elasticity are
essentially stable across samples (λ −0.193 → −0.209; γ_yp
1.04 → 0.99), so doubling the sample length and adding the
deregulation-era regime does not disturb the equilibrium structure.
The aggregate wealth elasticity, however, more than doubles
(0.019 → 0.047) — both values small and imprecisely estimated
(baseline t = 0.17 on the OLS coefficient,
`australia_full_results.csv`), so we read this as the
pre-deregulation regime adding identifying variation to a coefficient
the modern sample barely pins down, not as parameter instability that
can be dated. The house-price-to-income coefficient eases toward zero
on the longer sample — consistent with lower `hp_over_y` variation in
the pre-deregulation 1970s — and the real-rate coefficient
sign-flips, though both estimates are economically negligible. The
aggregate net-worth proxy correctly includes the household M3 liquid
component (the earlier `$bn`/`$m` unit defect that had made the M3
term numerically inert is fixed and propagated through a reproducible
cold rebuild; see Appendix D).

### 8.15 Spec 4 on the back-extended sample

The disaggregated-wealth proxies of §3 allow Spec 4 to fit on the
back-extended sample
([`spec46_extended_comparison.csv`](../outputs/spec46_extended_comparison.csv);
structural γ): λ moves 11.5 per cent toward Williams
(−0.1824 → −0.2034, still 29 per cent short of −0.286), but the
individual wealth coefficients become *smaller* rather than larger —
γ_NLA collapses by 95 per cent (+0.0245 → +0.0013), γ_SUPER flips
sign (+0.014 → −0.005), γ_HA eases (+0.019 → +0.018) and γ_EQ retains
its wrong sign (−0.043 → −0.041). The exercise establishes that
sample length is **not** the binding constraint on whether the
disaggregated single-equation form reproduces Williams' Table 1: the
longer sample sharpens the speed of adjustment but blunts, rather
than sharpens, the individual wealth channels.

### 8.15.1 Spec 6b — conventional baseline on the back-extended sample

Spec 6 binds at 2002Q3+ on the baseline sample because `cci_ratio`
(ABS Cat 5601.0 housing-loan flow) begins there. Spec 6b retains the
Spec 6 long-run and short-run structure but replaces the short-run
CCI regressor with the second difference of log RBA D02 total credit
(available from 1976Q3) and switches the wealth components to their
back-extended proxies. This lets the conventional baseline fit on the
full back-extended sample (n = 190 in the cointegration screen;
n = 180 full / 160 pre-COVID in the diagnostics)
([`australia_full_results.csv`](../outputs/australia_full_results.csv);
[`australia_full_diagnostics.csv`](../outputs/australia_full_diagnostics.csv);
structural γ):

| LR coefficient (γ) | Spec 6 (n = 86) | Spec 6b (n = 180) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag)              | −0.239 (t = −2.55) | **−0.248 (t = −3.96)** | −0.286 |
| ha_y / ha_y_proxy γ      | 0.009              | 0.012                  | 0.049  |
| nla_y / nla_y_proxy γ    | 0.035              | 0.015                  | 0.159  |
| eq_y / eq_y_proxy γ      | −0.065             | −0.008                 | (calibrated 0.011) |
| super_y / super_y_proxy γ| 0.025              | −0.001                 | (calibrated 0.011) |
| ln_hp_over_y γ           | +0.043             | +0.024                 | −0.130 |
| ln_yp_over_y (CCI = 0) γ | +1.363             | +1.113                 | +0.20 (calibrated) |
| BIC                      | −492.5             | −1 114.0               | n/a    |

Two patterns recur. First, the speed of adjustment moves modestly
closer to Williams' published value — λ = −0.248 on the back-extended
sample (87 per cent of Williams' −0.286, vs 83 per cent on Spec 6) —
and is far more sharply estimated, the t-statistic improving from
−2.55 to −3.96, with the pre-COVID estimate nearly identical at
−0.240 (t = −4.61,
[`australia_lambda_robustness.csv`](../outputs/australia_lambda_robustness.csv)):
unlike Spec 6, whose λ collapses without the COVID quarters, the
back-extended conventional baseline is identified in every sample
variant (COVID-dropped −0.240, COVID-rich −0.234). Second, the wealth
γ profile shifts toward still-smaller individual elasticities — γ_NLA
falls from 0.035 to 0.015 and γ_SUPER flips sign, mirroring the
Spec 4 back-extension finding — while γ_HA stays positive but at a
quarter of Williams' 0.049. This is consistent with the substantive
reading throughout the paper: on the back-extended sample the
disaggregated wealth proxies do not separately identify with
Williams-like precision, even when the canonical short-run dynamics,
the post-2008 permanent-income break, and a long-history credit proxy
are all available. Sample length sharpens |λ| but not the individual
wealth γ profile — the residual gap is structural to the
single-equation framing, not a sample-length artefact.

(The proxy caveats of §3.14 apply with full force: in particular
`eq_y_proxy` is held constant at its 1988Q3 value pre-1988, so the
equities coefficient is identified only off the modern subsample, and
the near-zero back-extended γ_EQ and γ_SUPER should be read as
proxy-limited rather than as estimates of a true pre-1988 propensity.)

### 8.16 Maximal-GETS placebo on the back-extended sample

We place the canonical knot constructions against 200 random draws
under the same sign-survival protocols on the back-extended 1976Q3+
sample. The committed summary files report two extended-sample
placebo variants
([`australia_williams_knot_placebo_extended_summary.csv`](../outputs/australia_williams_knot_placebo_extended_summary.csv),
[`australia_williams_knot_placebo_maximal_extended_summary.csv`](../outputs/australia_williams_knot_placebo_maximal_extended_summary.csv)):

| Placebo variant (1976Q3+) | Canonical adj-R² | adj-R² percentile | \|λ\| percentile | Verdict |
|---|---:|---:|---:|---|
| Extended (Williams literal 4-knot) | 0.6801 | 36th | 26th | "detrending critique persists — below median" |
| Maximal-GETS (canonical 15-knot reduction) | 0.6836 | 48th | 70th | "detrending critique persists — below median" |

On the back-extended sample the literal Williams 4-knot construction
sits below the placebo median on both metrics (adj-R² 0.6801 versus a
placebo median of 0.6815; |λ| 0.2023 versus 0.2079). The maximal-GETS
reduction does better on mean reversion — its |λ| of 0.2563 beats 70
per cent of random draws against a placebo median of 0.2329 — but
still sits just below the random median on fit (48th percentile,
0.6836 versus 0.6846), while retaining *fewer* knots (7) than the
placebo median (8), i.e. achieving near-median fit with less
flexibility. Neither extended-sample construction is the deployed
CCI: the deployed-protocol placebo — random knot dates run through
exactly the iterated drop-on-violation reduction used to build
`cci_williams`, on the 1988Q3+ estimation sample — is the §5.2
result, where the deployed construction reaches the **84th adjusted-R²
and 80th |λ| percentiles**
(`australia_williams_knot_placebo_deployed_verdict.csv`). The honest
reading is unchanged from §5.2: on the back-extended sample, where
the institutional knot dates predate the data's identifying
variation, the canonical constructions do not beat random
flexibility, and most of what lift there is comes from the
adaptiveness of the reduction protocol rather than from Williams'
specific knot or prior choice. We carry the below-median
extended-sample percentiles as a core negative result, not as an
embarrassment: they are the empirical expression of why
single-equation CCI is weakly identified and why the joint system is
needed for regime classification (§10).

### 8.17 Sectional sign-prior CCI

Williams (Aust paper §5.1) imposes sign priors over periods rather
than knot by knot. We construct a sectional CCI basis with one knot
per period (1982 / 1990 / 1993 / 2007, plus 2014 / 2017 / 2020 / 2021
extensions) and re-run the placebo on the back-extended sample
([`LIVES/outputs/sectional_placebo_summary.csv`](../../LIVES/outputs/sectional_placebo_summary.csv)).
The sectional canonical (adj-R² 0.6805, |λ| 0.2244) sits at the 37th
adjusted-R² percentile and 60th |λ| percentile — below the random
median on fit, like every extended-sample construction in §8.16, and
between the literal 4-knot and maximal-GETS constructions on mean
reversion. Williams' specific period dating therefore does not
outperform random period placements on the back-extended sample. On
the modern 1988Q3+ sample the sectional reduction retains two
survivors against the deployed protocol's four, fitting somewhat worse
(adj-R² 0.726 vs 0.754; λ = −0.203 vs −0.246) with a 0.69 correlation
between the two indices
([`LIVES/outputs/sectional_cci_comparison.csv`](../../LIVES/outputs/sectional_cci_comparison.csv)) —
a related but coarser credit signal, consistent with the period priors
being a constrained subset of the maximal candidate set.

### 8.18 Two-equation SUR (consumption + house prices)

Joint SUR estimation of the consumption equation and a Williams-style
house-price ECM (Aust paper eq. 11) on the back-extended 1976Q3+
sample yields a residual correlation ρ̂(ε_C, ε_H) = −0.0109 under
equation-by-equation OLS and −0.0133 under SUR
([`LIVES/outputs/lives_sur_2eq_resid_corr.csv`](../../LIVES/outputs/lives_sur_2eq_resid_corr.csv);
committed verdict "NEGLIGIBLE cross-equation linkage —
single-equation OLS approximately efficient"). The SUR coefficients
move by under 5 per cent of themselves relative to OLS for every
substantive term (10 per cent on the near-zero intercept) — the
consumption equation's λ shifts from −0.2101 to −0.2118 (+0.8 per
cent) and its SUR standard errors are within 0.01 per cent of the OLS
ones
([`LIVES/outputs/lives_sur_2eq_compare.csv`](../../LIVES/outputs/lives_sur_2eq_compare.csv)).
Joint estimation gives no efficiency gain at the quarterly frequency.
The case for the multi-equation framework therefore rests on
cross-equation parameter restrictions, not on residual covariance —
the same conclusion as the consumption + PI SUR of §8.2.

### 8.19 Three-equation joint cross-equation CCI identification

We extend the maximal-GETS protocol to require sign-prior survival
across **three** equations simultaneously (consumption + house prices
+ mortgage stock), and then across **four** (adding the wealth
equation). Of the fifteen candidate knots, seven pass consumption-only
fitting; only **two** pass the three-equation joint test and only
**one** survives all four equations
([`LIVES/outputs/lives_joint_cci_survival.csv`](../../LIVES/outputs/lives_joint_cci_survival.csv)):

| Survival regime | Surviving knots |
|---|---|
| Consumption only      | 1979Q1, 1986Q1, 1992Q1, 2007Q3, 2009Q1, 2017Q1, 2020Q2 |
| Joint 3-eq (C ∩ H ∩ M) | **1986Q1, 2017Q1** |
| Joint 4-eq (C ∩ H ∩ M ∩ W) | **1986Q1** |

The joint-identified CCI flips the house-price equation's CCI loading
from significantly negative (−0.0156 under the consumption-only CCI)
to positive (+0.0240 under the 3-equation joint CCI; +0.0236 under
the 4-equation variant;
[`LIVES/outputs/lives_phase3_comparison.csv`](../../LIVES/outputs/lives_phase3_comparison.csv)),
consistent with Williams' cross-equation sign restrictions working as
intended. The mortgage-stock equation's loading moves the other way —
positive under consumption-only CCI (+0.0026), negative under the
joint variants (−0.0060 / −0.0069) — a reminder that joint
sign-survival is a sign restriction, not a parameter-equality
restriction; full FIML would be required to discipline all loadings
simultaneously. The consumption equation's error-correction is only
modestly affected — λ moves from −0.271 (consumption-only) to −0.213
(3-eq) / −0.227 (4-eq) — while its housing/net-worth long-run
coefficient sign-corrects under joint identification (−0.031
consumption-only → +0.037 / +0.017 joint), and the SUR residual
correlations remain modest (ρ̂(ε_C, ε_H) = −0.119,
ρ̂(ε_C, ε_M) = +0.102, ρ̂(ε_H, ε_M) = −0.244). Joint sign-survival
therefore does real, if limited, identification work — it re-signs
the house-price loading and the consumption wealth term — without
closing the magnitude gap against Williams' Table 1, confirming that
the residual gap is structural to the single-equation framing, not a
CCI-construction artefact, and that the route to sharpening the
credit channels runs through the four-equation FIML system, not
through further single-equation tuning.

---


## 9. Comparison with Williams (2010, 2012)

This section consolidates the comparison with Williams' published
Table 1 and BIS chapter estimates. The organising thesis is the one
that runs through the paper: **the LIVES *structure* transfers to
Australia, but Williams' Australian *calibrations* do not.** The
faithful single-equation LIVES form (Spec 11) recovers Williams'
error-correction speed and the correctly signed wealth-m.p.c.
structure; imposing his permanent-income gearing (Spec 12, Spec 10)
collapses the equilibrium; and the six CCI interactions through which
his framework identifies the credit channels are too mutually
collinear to be separated off a single equation. We deliberately
retire the earlier draft's "the preferred specification reproduces
Williams' profile almost exactly" framing: that reading rested on the
conventional constant-m.p.c. baseline (Spec 6), on an offsetting-deficit
coincidence, and on confidence intervals so wide that they contain both
Williams' values and zero. The honest ceiling is consistency, not
confirmation.

### 9.1 Where the structure transfers — the faithful LIVES form recovers Williams' speed and wealth structure

The decisive agreement is on the error-correction speed and the
*sign and significance* of the core wealth channels, and it appears
only once the equation is written in the faithful LIVES form.

On the COVID-controlled samples the faithful LIVES specification
(Spec 11) estimates a speed of adjustment tightly clustered at
**λ ≈ −0.25** — pre-COVID −0.266 (t = −4.85), COVID quarters dropped
−0.248 (t = −6.66), quarterly COVID dummies −0.242 (t = −6.25) —
against Williams' FIML estimate of **−0.286 (SE 0.083, t = −3.45)**:
about 13 per cent below his value, and tightly identified
(`australia_spec11_variants.csv`; `australia_lambda_robustness.csv`).
The full-sample estimate is λ = −0.448 (t = −3.57); it is inflated by
the COVID quarters and fails the |λ| upper-bound screen, so we read
the COVID-controlled cluster as the identified speed (§7.0). Either
way the contrast with the conventional baseline is sharp: Spec 6
returns λ = −0.239 (t = −2.55) on its n = 86 sample — about 17 per
cent below Williams — and collapses to an insignificant −0.087 once
the COVID quarters are excluded.

Within the faithful form the wealth structure is correctly signed and,
for two of the three components, individually significant on the full
sample (`australia_all_results.csv`; `australia_gamma_inference.csv`):

- net liquid assets (γ_NLA): OLS +0.0269 (t = 3.75, ***), implied
  structural m.p.c. **0.060** [0.022, 0.098], against Williams'
  calibrated 0.159 — same sign, but genuinely smaller: the interval
  *excludes* his value (§9.4);
- illiquid financial assets (γ_IFA, equities + superannuation
  combined): OLS +0.0155 (t = 3.09, ***), implied structural m.p.c.
  **0.035** [0.012, 0.057], against Williams' calibrated **0.022** —
  same sign, and the interval comfortably includes his value;
- housing-collateral (γ₁, the `CCI·(HA/4y)` interaction): OLS +0.0025
  (t = 0.71), implied structural m.p.c. **0.0055** [−0.010, 0.021],
  against Williams' peak housing m.p.c. of 0.0488 — correctly signed
  but insignificant, with an interval that excludes his peak value
  while containing zero.

The permanent-income response is strong and correctly signed
(`ln_yp_over_y` OLS +0.459, t = 4.04) — though its structural gearing
breaches the admissibility bound (§7.0, §9.8). And critically, because
the CCI-spline interactions replace Spec 6's 2002Q3-binding `cci_ratio`
short-run term, the faithful form estimates on **n = 146** rather than
n = 86 — a near-doubling of the estimation window relative to the
baseline against which the previous draft compared Williams (though the
credit channels themselves are identified only off the ~70 post-2007
quarters where the deployed CCI moves; §5.1.1).

The single most important interpretive point for the comparison is
that **the housing channel only appears when the form is faithful.**
In the LIVES theory there is no classical housing wealth effect: the
housing m.p.c. is zero at CCI = 0 and is unlocked as credit conditions
loosen. Reading Spec 6's insignificant standalone `ha_y` (OLS +0.0022,
t = 0.30) as a *failed* housing wealth effect was therefore a category
error — the theory predicts that coefficient to be ≈ 0, and the
housing effect lives in the credit interaction `ha_x_cci`, where it is
correctly signed (if statistically unproven).

### 9.2 Where the calibrations do not transfer — imposing Williams' gearing collapses the equilibrium

The opposite result holds for Williams' Australian *calibrations*. The
natural single-equation response to weakly identified interactions is
Williams' own — calibrate the credit channels and estimate only what
the data support — but the data reject that route decisively.

Spec 12 imposes Williams' scale-robust calibrations (γ_IFA = 0.022,
ψ₀ = 0.20, ψ₁ = 0.93) via an iterative fixed-point offset and frees
only the housing-collateral m.p.c., the net-liquid m.p.c. and λ. The
result is a near-total collapse of the error-correction mechanism:
**λ = −0.030 (t = −0.74)** on the full sample, and it flips to the
wrong sign (+0.041, t = 2.03, significant at 5 per cent) pre-COVID, so it is not even sign-stable across
samples (`australia_lambda_robustness.csv`). This is independently
reproduced by the pre-existing Williams-prior specification (Spec 10),
which keeps the rate and affordability channels free and still returns
λ = −0.048 (t = −0.78) full-sample and a still-insignificant −0.025
(t = −0.57) pre-COVID, flipping to the wrong sign once the COVID
quarters are dropped or dummied (`australia_lambda_robustness.csv`).
Two separate calibration routes therefore reach the same conclusion.

The mechanism is straightforward: the Australian data freely estimate
a structural permanent-income gearing of order one — by the §4.2
recovery rule, ψ̂ = OLS/|λ| is 1.02 on the full sample and 1.12–1.13
in the COVID-controlled variants, roughly five times Williams' 0.20 —
so forcing his much lower value injects a large, mis-signed
contribution that destroys the long-run equilibrium. (The comparison
must be made on a consistent scale: structural ≈ 1.0–1.1 against his
ψ₀ = 0.20, or OLS +0.459 against his implied φ·ψ₀ ≈ 0.057 — not an
OLS-to-structural mix. The freely estimated gearing itself breaches
the ψ ≤ 0.95 admissibility bound, which we flag openly in §7.0; the
ψ₀/ψ₁ split is in any case not sharply estimable on a single equation,
so we report the contrast as order-of-magnitude rather than as a
precise ratio.)

Williams' rate, affordability and autonomous-consumption loadings
cannot be imposed at all at their published magnitudes. His raw
real-rate loading α_r = −0.871 enters his FIML system on differently
scaled regressors; on this repository's *percent*-scaled real rate
against a *unit*-normalised CCI it is roughly thirty times too large,
and the iterative fixed point diverges. This is itself a finding: the
LIVES structure is portable, but its numerical calibrations are
specific to Williams' scaling conventions and sample, and cannot be
transplanted mechanically.

### 9.3 Reconciling the companion paper's Wald non-rejection

The companion analysis reports that a joint Wald test on the freely
estimated Spec 6 coefficients does **not reject** Williams'
calibration: χ²(6) = 7.55, p = 0.27 for all six coefficients jointly,
χ²(4) = 1.83, p = 0.77 for the four wealth coefficients, and no
individual coefficient rejects either
(`LIVES/outputs/williams_calibration_wald.csv`). Read alongside §9.2
this looks paradoxical: how can a calibration that *collapses* the
equilibrium when imposed also be *not rejected* by a test? The
resolution is power. The freely estimated single-equation coefficients
are so imprecise that they cannot statistically reject Williams'
values — but the same imprecision means the data also cannot reject
zero, or any number of other points. **Low power is not the same as
good fit.** A calibration can be non-rejected by an underpowered test
and still wreck the model when imposed; both facts hold here
simultaneously, and the gamma-inference confidence intervals (§9.4)
make the imprecision explicit: in the Spec 6 frame every Williams
*wealth* value lies inside our 95 per cent CI, and every such CI also
contains zero. The one place the free estimate is strong enough to
disagree is the permanent-income gearing: the Wald test on the OLS
scale does not reject (p = 0.20), but the delta-method interval on the
structural ratio γ = OLS/|λ|, [0.28, 2.44], excludes Williams' 0.20 —
and that is exactly the channel whose imposition produces the §9.2
collapse.

### 9.4 The structural-gamma comparison: the inference now cuts both ways

The earlier draft's headline — that the implied structural γ profile
"matches Williams almost exactly" — was a property of the conventional
baseline (Spec 6), and it does not survive the corrected data. Under
the ECM identity OLS = λ × γ, Spec 6's |λ| now runs about 17 per cent
below Williams' while its OLS wealth coefficients run roughly 80 per
cent below his implied OLS values (e.g. `ha_y` +0.0022 against the
0.0140 his γ = 0.0488 implies at λ = −0.286; `nla_y` +0.0083 against
0.0455), so the implied γ profile sits far *below* Williams' rather
than coincidentally close to it (γ_HA 0.0094 vs 0.0488; γ_NLA 0.035 vs
0.159). The earlier "offsetting deficits leave γ near Williams"
reading is therefore retired on the corrected data, not merely hedged.
What keeps Spec 6 *consistent* with Williams is imprecision: every
wealth-component interval contains both his value and zero
(`australia_gamma_inference.csv`, Spec 6 rows):

| Term (Spec 6, n = 86) | OLS | Implied γ | 95% CI | Bootstrap 95% CI | Williams | In CI? |
|---|---:|---:|---:|---:|---:|:-:|
| `ha_y` (housing) | +0.0022 | 0.009 | [−0.056, 0.075] | [−0.049, 0.107] | 0.0488 | ✓ |
| `nla_y` (net liquid) | +0.0083 | 0.035 | [−0.262, 0.332] | [−0.183, 0.361] | 0.159 | ✓ |
| `eq_y` (equities) | −0.0156 | −0.066 | [−0.468, 0.337] | [−0.303, 0.208] | 0.011 | ✓ |
| `super_y` (super) | +0.0060 | 0.025 | [−0.040, 0.091] | [−0.064, 0.141] | 0.011 | ✓ |
| `ln_hp_over_y` | +0.0102 | 0.043 | [−0.308, 0.393] | [−0.400, 0.325] | −0.130 | ✓ |
| `ln_yp_over_y` | +0.3253 | 1.363 | [0.285, 2.442] | [0.605, 2.192] | 0.20 | ✗ |
| `WEALTH_AGG` | +0.0010 | 0.004 | [−0.659, 0.668] | — | 0.230 | ✓ |

In the faithful frame the picture sharpens decisively, and in both
directions (`australia_gamma_inference.csv`, Spec 11 rows):

| Term (Spec 11, n = 146) | OLS | Implied γ | 95% CI | Williams | In CI? |
|---|---:|---:|---:|---:|:-:|
| `ha_x_cci` (γ₁, housing × CCI) | +0.0025 | 0.0055 | [−0.010, 0.021] | 0.0488 | ✗ |
| `nla_y` (γ_NLA) | +0.0269 | 0.060 | [0.022, 0.098] | 0.159 | ✗ |
| `ilfa_y` (γ_IFA) | +0.0155 | 0.035 | [0.012, 0.057] | 0.022 | ✓ |
| `hp_x_1_minus_cci` (α₄) | +0.0279 | 0.062 | [0.027, 0.098] | −0.130 | ✗ (wrong sign) |
| `ln_yp_over_y` (ψ₀) | +0.4591 | 1.024 | [0.865, 1.184] | 0.20 | ✗ |
| `yp_x_cci` (ψ₁) | −0.5101 | −1.138 | [−2.099, −0.177] | 0.93 | ✗ |
| `WEALTH_AGG` | +0.0448 | 0.100 | [0.036, 0.164] | 0.230 | ✗ |

The honest summary is therefore no longer "everything is inside a wide
interval". The gamma inference now **cuts both ways**: Spec 11's
intervals are tight enough to *reject* parts of Williams' profile —
his net-liquid m.p.c. (0.159) lies outside [0.022, 0.098], his peak
housing m.p.c. (0.0488) outside [−0.010, 0.021], and his affordability
loading has the wrong sign — while his illiquid-financial m.p.c.
(0.022) is comfortably *inside* [0.012, 0.057]. The Spec 6 frame
remains consistency-by-imprecision on a small n = 86 sample; the
Spec 11 frame upgrades the comparison to agreement on form and on the
illiquid-financial channel, disagreement on the net-liquid magnitude,
and an underpowered housing-collateral channel whose interval contains
zero.

### 9.5 Adding the CCI interactions does not close the gap (Spec 8)

Estimating the CCI interactions freely alongside the standalone wealth
ratios (Spec 8) raises the speed of adjustment to λ = −0.458
(t = −3.52) full-sample on n = 146, in magnitude above Williams'
−0.286, with the second-best BIC in the ladder (−952.8, behind
Spec 11's −954.8). We do not present this as a credit-channel success.
The wealth coefficients shift relative to the baseline (γ_NLA → 0.073,
γ_super → 0.047, standalone housing → 0.007) without moving
systematically toward Williams' Table 1, and three of the four
interaction sign priors fail outright
(`australia_spec8_sign_prior_verdicts.csv`):

- `r_x_cci` (Williams α_c1 = −0.871 at CCI = 1): prior negative, OLS
  +0.0028, p = 0.042 → **FAIL** (wrong sign, significant);
- `hp_x_1_minus_cci` (Williams α_c4 = −0.13 at CCI = 0): prior
  negative, OLS +0.0299, p = 0.171 → **FAIL**;
- `yp_x_cci` (Williams ψ₁ = +0.93): prior positive, OLS −0.505,
  p = 0.016 → **FAIL** (wrong sign, significant);
- `ln_yp_over_y` (Williams ψ₀ ≈ 0.20): prior small positive, OLS
  +0.492, p = 0.0008 → **PASS**.

The raw-coefficient comparison confirms that the interactions do not
land where Williams' framework predicts
(`australia_williams_spec8_comparison.csv`): his α_c1 = −0.871 against
our structural +0.0062 (t = 2.05), his α_c4 = −0.13 against our
+0.065 (t = 1.38), his calibrated ψ₁ = +0.93 against our freely
estimated −1.10 (t = −2.43). Adding the interactions therefore
**re-allocates** the long-run identification across components — and
raises λ — but does not close the residual gap with the joint FIML
estimates. The earlier draft's framing that Spec 8 "exceeds Williams"
is withdrawn.

### 9.6 The interactions are collinear: the structural reason Williams uses FIML

The reason single-equation estimation cannot deliver Williams' result
is, at root, an identification problem. The CCI-interacted regressors
are 0.66–0.97 mutually correlated in absolute value on this sample
(`australia_cci_interaction_corr.csv`; §5.5) because each is
approximately proportional to the latent CCI; they cannot be
separately identified off one equation. The committed evidence runs
in the same direction from three sides: the correlation matrix itself;
the wrong-signed Spec 8 interaction coefficients of §9.5; and the
Spec 10/Spec 12 collapse of §9.2.

This is precisely the structural reason Williams (2010) uses a
four-equation FIML system rather than single-equation OLS. The same
CCI enters all four of his equations with sign constraints; the
affordability multiplier ϖ in the wealth × (1 − ϖ·CCI) interaction is
shared across equations; and ζ_h = 1 normalises the house-price
equation. Those cross-equation restrictions supply the identifying
variation that lets the credit channels be separated — variation that
no single-equation specification has. The joint-survival result of §5
corroborates this from the other direction: of the fifteen candidate
knots, seven pass the consumption-equation sign prior, but only two of
those survive the three-equation requirement (1986 and 2017Q1) and
only one survives all four equations (1986)
(`LIVES/outputs/lives_joint_cci_survival.csv`), so the single-equation
identification was largely equation-specific. This aligns with both
Williams' framework (CCI as a common factor under parameter
restrictions) and the Duca and Muellbauer (2013) state-space
implementation in which the latent factor is identified jointly across
equations.

### 9.7 Sample length is not the binding constraint

A natural conjecture is that the divergence from Williams reflects
sample period — his 1978Q1–2008Q2 window spans the
financial-liberalisation episode that identifies the credit channels,
whereas our balance-sheet data begin only in 1988Q3, largely after
deregulation. The back-extension exercise tests this directly and
finds it is not the binding constraint.

Refitting the simpler disaggregated no-CCI specification (Spec 4) on
the back-extended 1976Q3+ sample moves λ about 12 per cent toward
Williams (−0.182 → −0.203, still 29 per cent short of −0.286;
`spec46_extended_comparison.csv`) — and the aggregate Spec 1 moves
similarly (−0.193 → −0.209; `spec1_extended_comparison.csv`) — but
the individual wealth coefficients *shrink* rather than grow on the
longer window: γ_NLA collapses by 95 per cent (+0.025 → +0.001),
γ_super flips sign, and γ_EQ retains a wrong sign (§8). Lengthening
the sample slightly sharpens the speed of adjustment but
not the wealth structure, so the post-1988 window is not, in itself,
what generates the divergence between any single-equation OLS estimate
and Williams' jointly identified Table 1. The back-extension is
retained as a contribution (it builds a 1976Q3-anchored master
dataset and permits the direct sample-length test), but it is not the
route to closing the gap with Williams.

### 9.8 The permanent-income measure and the Australian PI puzzle

A final point of comparison concerns the permanent-income channel and
the measure on which it depends. Under the AR-method forecaster we
replicate the often-noted "Australian permanent-income puzzle": the
long-run coefficient on log(yᵖ/y) is negative — near-zero on the
full-sample (look-ahead) AR variant (−0.004) and negative though
insignificant once the AR forecaster is run in real time (−0.158,
t = −1.68) (`australia_pi_method_comparison.csv`,
`australia_pi_realtime_robustness.csv`). Under the full-sample Italian
direct-forecast *measure* (De Bonis et al. 2020, Appendix A.2; §2.5)
it is strongly positive (+0.196 on the net-worth forecaster
regression; +0.325 in the Spec 6 frame; +0.459 in the faithful
Spec 11). The structural reasons the measures diverge are real — the
rolling-AR forecaster lacks the labour-force-share predictor that
captures slow demographic effects, compounds short-run AR
misspecification across 40 horizons, and over-estimates persistence
after large income shocks, all of which the one-step direct forecast
avoids. But the positive sign is a property of the full-sample,
non-causal construction of the measure: under a causal real-time
direct forecast the coefficient flips to significantly negative
(−0.145, t = −2.23), even though the error-correction term itself
survives the real-time treatment (λ = −0.159, t = −2.39, against the
AR real-time −0.095). The strong positive permanent-income response
that the faithful LIVES form recovers therefore rests on the
full-sample (look-ahead) permanent-income measure; we flag this
explicitly and direct readers to the real-time robustness column
rather than treating the puzzle as resolved.

### 9.9 Summary

The comparison with Williams resolves into a clean two-part statement,
with one honest refinement. The LIVES *structure* transfers: the
faithful single-equation form recovers his error-correction speed
(COVID-controlled λ ≈ −0.25 against −0.286, a gap of about 13 per
cent) and the correctly signed wealth-m.p.c. structure (net-liquid and
illiquid-financial m.p.c.s individually significant;
housing-collateral correctly signed). His Australian *calibrations* do
not transfer: imposing his permanent-income gearing collapses λ to ≈ 0
(Spec 12, Spec 10), because Australia freely estimates a structural
gearing of order one — roughly five times his 0.20 on the consistent
structural scale — and his rate loading cannot even be imposed at its
published scale. The refinement is that the magnitude comparison now
cuts both ways: the gamma inference rejects his net-liquid m.p.c.
(0.159 outside [0.022, 0.098]) and his peak housing m.p.c. (0.0488
outside [−0.010, 0.021]) while matching his illiquid-financial m.p.c.
(0.022 inside [0.012, 0.057]), so the agreement is on form and on the
IFA channel, not on every magnitude. The companion Wald non-rejection
(χ²(6) = 7.55, p = 0.27) is reconciled as low-power non-rejection in
the imprecise Spec 6 frame. The CCI interactions are 0.66–0.97
collinear in absolute value and so weakly identified off a single
equation, which is exactly why Williams' identification comes from
cross-equation FIML restrictions; the back-extension shows sample
length is not the binding constraint. The path to a tighter
reconciliation with Williams' published values therefore runs through
a full four-equation FIML build, not through any single-equation OLS
refinement.

---


## 10. Decomposition and policy implications

This section reads the estimated long run and the policy
counterfactuals through the lens of the faithful LIVES specification
(Spec 11) of §7.0, while being explicit about which committed
decompositions run on which long-run parameterisation. Two notes frame
everything below. First, the long-run contributions decomposition is
now committed in two parameterisations: the Spec 11 LIVES bracket
([`australia_longrun_contributions_spec11.csv`](../outputs/australia_longrun_contributions_spec11.csv)),
which is the lead decomposition of §10.1, and the selector-preferred
net-worth Spec 3
([`australia_longrun_contributions.csv`](../outputs/australia_longrun_contributions.csv)),
which we retain as a cross-check. Second, the counterfactuals
([`australia_counterfactuals_summary.csv`](../outputs/australia_counterfactuals_summary.csv))
were generated on the conventional baseline (Spec 6 dummies) and the
interaction spec (Spec 8); we report them as committed but read the
CCI counterfactual through the de-meaning convention rather than as a
structural housing-collateral experiment. The policy reading in
§10.3 is anchored on the Spec 11 channels.

### 10.1 Long-run contributions decomposition

The long-run decomposition (an Australian counterpart to Williams
(2010) Charts 2–8) splits fitted de-meaned log(c/y) into the partial
contribution of each long-run regressor over the 146 quarters from
1988Q3 to 2024Q4. The lead decomposition is on the Spec 11 LIVES
bracket: net liquid assets (`nla_y`), illiquid financial assets
(`ilfa_y`), the credit-scaled housing-collateral term (`ha_x_cci`),
the credit-scaled affordability term (`hp_x_1_minus_cci`), the rate
and permanent-income interactions (`r_x_cci`, `yp_x_cci`), the CCI
intercept (`cci_williams`) and the permanent-income ratio
(`ln_yp_over_y`); each term is de-meaned and so sums to approximately
zero by construction over the window, and the actual de-meaned path
and the residual are reported alongside
([`australia_longrun_contributions_spec11.csv`](../outputs/australia_longrun_contributions_spec11.csv)).

At the last observation (2024Q4) the dominant wedge is now the
credit-scaled affordability term at +0.098 — its largest value in the
sample, reflecting house prices elevated relative to income with the
CCI in its post-2022 tight regime (CCI ≈ −1.6, so the affordability
multiplier 1 − 1.2·CCI is large and positive) — offset by the two
permanent-income terms (−0.057 level, −0.088 interaction), with the
financial-wealth channels smaller (`ilfa_y` +0.039, `ha_x_cci` −0.017,
`nla_y` −0.004) against an actual de-meaned log(c/y) of −0.020 and a
residual of only +0.013. The headline of the corrected decomposition
is that the fitted long-run sum now tracks the actual path closely:
the correlation between fitted and actual de-meaned log(c/y) is +0.945
and the residual carries only 11 per cent of the variance of the
actual path, so the estimated drivers — not an unexplained residual —
account for most of the movement in the consumption-to-income ratio.

Three features of the path stand out. First, because the deployed CCI
is identically zero before 2007Q3, the rate, permanent-income and
housing-collateral interactions (`r_x_cci`, `yp_x_cci`, `ha_x_cci`)
are held flat over 1988–2007 and only the affordability term moves
(and only with house prices, since 1 − 1.2·CCI = 1 there); the pre-GFC
swings are therefore carried by permanent income and the
financial-wealth ratios. The early-2000s credit boom is a
permanent-income episode in this arithmetic — at 2003Q4 an actual
de-meaned log(c/y) of +0.062 is almost entirely the +0.063
contribution of `ln_yp_over_y` — and the 2009Q1 GFC dip (actual
−0.027) is carried by net liquid assets (−0.016) with permanent income
turning below trend (−0.009). Second, once the CCI is live the credit
terms do most of their work in the tightening episodes: through the
2010s the permanent-income level term runs persistently negative (PI
below realised income, e.g. −0.046 at 2015Q4) largely offset by rising
illiquid financial wealth (+0.020) and the permanent-income
interaction (+0.046); and in the post-2022 tightening regime the rate
interaction swings to +0.049 in 2022Q3 while the affordability wedge
widens steadily to its end-of-sample extreme of +0.098. Third — the
substantive reversal from the pre-fix decomposition — the residual is
now small and no longer carries the story. Across 2002–2007 it sits
within ±0.02 (against the +0.09 to +0.16 level the sign-inverted
decomposition had reported), and even the COVID collapse is largely
captured by the long-run bracket rather than left to the dynamic
dummies: at the trough (2020Q2, actual −0.213) the drivers still leave
a −0.107 residual, but by 2020Q3 the permanent-income interaction
(−0.158) and its level term (−0.077) absorb nearly all of the −0.170
dip, leaving a residual of only +0.017. The COVID quarters are where
the drivers explain the most, not the least.

The honest qualifier is that the credit-scaled contributions inherit
the weak identification of their coefficients (§5, §7.0): γ₁ is
statistically indistinguishable from zero (t = 0.71), and the
affordability and rate interactions are wrong-signed against Williams'
priors, so the Spec 11 attribution should be read as the model's
fitted arithmetic, not as well-identified structural channels. As a
cross-check, the selector-preferred net-worth decomposition (Spec 3;
[`australia_longrun_contributions.csv`](../outputs/australia_longrun_contributions.csv))
tells a consistent end-of-sample story with coarser channels: net
worth +0.018, house-price affordability +0.015, real rate ≈ 0.000 and
permanent income −0.057 at 2024Q4, with a residual of +0.004. Spec 3
tracks the actual path less tightly than Spec 11 (fitted–actual
correlation +0.81, residual variance 36 per cent of the actual) —
lacking the CCI interactions it cannot capture the COVID dip, leaving
a −0.103 residual at 2020Q3 — but both parameterisations agree that
permanent income is the single largest source of variation in the
fitted equilibrium, with the affordability/house-price wedge the
next-largest moving part. On the wealth side the two decompositions
also agree in the corrected sign: net worth (Spec 3, +0.018) and
illiquid financial assets (Spec 11, +0.039) both sit modestly above
their sample means at end-2024, a small positive support to the ratio
rather than the drag the sign-inverted decomposition had shown.

Source:
[australia_longrun_contributions_spec11.csv](../outputs/australia_longrun_contributions_spec11.csv),
[australia_longrun_decomposition_spec11.png](../outputs/australia_longrun_decomposition_spec11.png),
[australia_longrun_contributions.csv](../outputs/australia_longrun_contributions.csv),
[australia_longrun_decomposition.png](../outputs/australia_longrun_decomposition.png).

### 10.2 Counterfactuals

We report three policy counterfactuals, holding all
non-counterfactual regressors at their observed values and
integrating the implied Δlog c paths back to log-consumption levels
([`australia_counterfactuals_summary.csv`](../outputs/australia_counterfactuals_summary.csv)).
The APRA and COVID scenarios are computed on the conventional
baseline event dummies; the CCI scenario is computed on the
interaction spec. We retain them as committed evidence, with the
hedges noted below.

| Scenario | Event date | Basis | h = 4 q gap | h = 8 q gap | End-of-sample gap |
|---|---|---|---:|---:|---:|
| No 2014/2017 APRA macroprudential | 2014Q4 | Spec 6 dummies | −1.0 % | −2.3 % | +2.0 % |
| No COVID income support           | 2020Q1 | Spec 6 dummies | −8.3 % | −8.3 % | −8.3 % |
| CCI at peak vs CCI = 0            | 1988Q4 | Spec 8 interactions | n/a | n/a | ≈ 0 |

(Gaps are cumulative deviations in log(c) from the baseline path,
expressed in per cent; h-quarter values are measured from the
relevant event date.)

**Counterfactual 1 — no 2014/2017 APRA macroprudential.** On the
re-estimated Spec 6 the two APRA ogive dummies are small and
statistically weak, and they now disagree in sign (`d_apra_2014`
+0.0050, t = 0.57; `d_apra_2017` −0.0069, t = −1.43;
`australia_full_results.csv`) — note that the Spec 11 headline
estimates the opposite sign pattern (§10.3), so the profile is not
robust across specifications. Zeroing both dummies implies
consumption about 1.0 per cent *lower* four quarters after the 2014
round (−0.0103 in log points), 2.3 per cent lower after eight
quarters (−0.0232), and 2.0 per cent *higher* by end of sample
(+0.0197), as the negative 2017 dummy's removal eventually dominates.
The honest reading is that the corrected data do not support a large,
persistent APRA consumption drag through these event dummies: the
counterfactual gaps are of the order of 1–2 per cent and not robustly
signed, and an earlier draft's decade-horizon figure of ≈ +28 per
cent is withdrawn with the old estimates that produced it.

**Counterfactual 2 — no COVID income support.** Zeroing the
`d_jobkeeper_2020`, `d2020_covid` and `d2020_rebound` dummies implies
that consumption would have been about 8.3 per cent *lower* in the
COVID period (−0.0835 in log points). The cumulative gap is constant
across horizons because the COVID event dummies are bounded in time
(zero before and after the 2020–21 window), so unlike the persistent
APRA ogive the deviation does not continue to compound past the event
window; the scenario is modelled as a one-off.

**Counterfactual 3 — CCI at peak vs zero.** Evaluating the
interaction spec with the CCI-interacted regressors at CCI = 1
(historical peak) versus CCI = 0 (no liberalisation) implies an
essentially zero cumulative consumption gap across the
sample (−2.7 × 10⁻¹⁵, i.e. zero to numerical precision). This is a
mechanical consequence of the de-meaning convention: under de-meaned
interactions the average contribution of CCI variation to fitted
Δlog c is zero by construction, so the *cumulative* effect of a
permanent CCI shift integrates to zero across the sample. The CCI
interactions therefore operate as pure timing/distribution effects —
they reallocate where consumption growth lands across the cycle
without shifting its unconditional level. This is the
LIVES-theoretic reading: credit conditions matter for *when*
households extract housing equity and respond to permanent-income
news, not for the long-run level of the consumption-to-income ratio.
It is not a structural housing-collateral experiment, and it should
not be read as evidence on the magnitude of γ₁ (for which see §7.0,
where the credit-scaled collateral MPC is right-signed but
insignificant).

Source:
[australia_counterfactuals.csv](../outputs/australia_counterfactuals.csv),
[australia_counterfactuals_summary.csv](../outputs/australia_counterfactuals_summary.csv),
[australia_counterfactual_paths.png](../outputs/australia_counterfactual_paths.png).

### 10.3 Policy implications

We read the policy implications off the faithful LIVES specification
(Spec 11), distinguishing the channels that come through cleanly from
those that remain weakly identified.

**Wealth channel of monetary policy.** Under Spec 11 the two
plain-MPC wealth channels are significant and correctly signed: the
net-liquid-asset MPC is γ₃ = 0.060 [0.022, 0.098] (OLS coefficient
+0.027, t = 3.75) and the illiquid-financial-asset MPC is
γ₂ = 0.035 [0.012, 0.057] (OLS +0.015, t = 3.09), both as structural
propensities recovered as OLS/|λ| on the full sample (|λ| = 0.448),
both significant at 5 per cent in the full-sample and COVID-dummy
variants, weakening to 10 per cent in the pre-COVID subsample. Housing wealth, by
contrast, enters *only* through the credit-scaled collateral channel:
the implied structural MPC is γ₁ = 0.0055 — right-signed but
statistically insignificant (OLS +0.0025, t = 0.71), against
Williams' calibrated peak of 0.0488, which the interval excludes. The
policy reading is therefore asymmetric across asset classes: liquid
and illiquid financial wealth transmit to consumption with
well-identified marginal propensities, whereas the housing-collateral
channel that LIVES theory makes conditional on credit conditions is,
on Australian single-equation post-deregulation data, of the
predicted sign but unproven. Movements in mortgage rates that change
housing values propagate to consumption with a speed of adjustment we
identify on the COVID-controlled samples at λ ≈ −0.25 (pre-COVID
−0.266, t = −4.85) — close to Williams' −0.286 — implying about a
25 per cent closing of any equilibrium gap in the first quarter and,
since (1 − 0.25)⁸ ≈ 0.10, roughly 90 per cent of the adjustment
completed within two years. The full-sample λ of −0.448 is inflated
by the COVID quarters and fails the |λ| upper-bound screen (§6); the
COVID-controlled value is the policy-relevant speed.

**Housing wealth is not a free-standing channel.** The central
form-is-decisive result of the paper carries a direct policy
corollary: reading an insignificant standalone housing-wealth
coefficient as evidence of "no housing wealth effect" is a category
error. LIVES predicts that coefficient to be approximately zero
absent the credit-conditions interaction; the housing MPC is unlocked
only as credit conditions ease. For macroprudential and
financial-stability analysis this means the consumption response to
house prices is regime-dependent — larger when credit is loose,
muted when it is tight — rather than a fixed elasticity. The
qualitative structure is the policy-relevant object even where the
point estimate of γ₁ is imprecise.

**Macroprudential effects.** The 2014 and 2017 APRA episodes enter
as smoothed-step ogive dummies with small Δlog c coefficients. On the
Spec 11 headline only the 2014 round is materially negative and
marginally significant (`d_apra_2014` = −0.0108, t = −1.92); the 2017
dummy is positive and insignificant (`d_apra_2017` = +0.0099,
t = 1.61), so any macroprudential drag is carried by the first round.
The Spec 6-based counterfactual in §10.2 estimates the opposite sign
pattern on its shorter sample and implies cumulative gaps of only
1–2 per cent that are not robustly signed across horizons. The honest
policy statement is that these event dummies detect, at most, a
small and specification-dependent consumption effect of the APRA
rounds — not the large persistent drag an earlier draft reported.

**Permanent-income transmission.** Permanent income is the strongest
channel in the faithful form: the Spec 11 coefficient on log(yᵖ/y) is
+0.46 (t = 4.04) on the full sample and +0.30 (t = 5.81) pre-COVID,
both highly significant — though the implied structural gearing
ψ̂ = OLS/|λ| of 1.02–1.13 sits *above* the theoretical admissibility
bound ψ ≤ 0.95, a breach we disclose as an open puzzle rather than
re-scale away (§7.0). For fiscal-multiplier work this implies
Australian households respond meaningfully and durably to credible
permanent-income shocks. Two caveats apply. The headline estimate
uses the full-sample (look-ahead) Italian direct-forecast
permanent-income measure; under the real-time, no-look-ahead variant
the speed of adjustment shrinks (Italy real-time λ = −0.159, t = −2.39,
versus full-sample −0.239 in the Spec 6 frame) and the
permanent-income coefficient flips sign (+0.325 → −0.145; §7.4, §8),
so the real-time column is the operationally-relevant version for any
forward-looking application. And the freely-estimated structural
gearing of order one is roughly five times Williams' calibrated
ψ₀ = 0.20; imposing his value collapses the equilibrium (§7.0.1,
§9), so the Australian permanent-income gearing is a domestic
estimate, not a transferred calibration.

**Credit-conditions identification caveat.** Section 5 documents that
the CCI's identification in a single-equation OLS is weak, and that
neither the back-extended sample, the sectional sign priors, nor the
time-varying housing-wealth interaction changes this. The deployed
index itself is degenerate over much of the sample — identically zero
before 2007Q3, so the credit channels are identified off roughly
seventy quarters — and the CCI-interacted regressors are very highly
mutually correlated (pairwise |ρ| of 0.66–0.97, each approximately
proportional to CCI; `australia_cci_interaction_corr.csv`), so they
cannot be separately identified off one equation — the structural
reason Williams uses four-equation FIML. Policymakers using a
single-equation CCI series for regime classification (tightening
versus easing diagnoses) should treat the spline coefficients as
consumption-equation residual identification rather than a
structurally identified common credit-conditions factor. For policy
use, regime classification should rely on the joint system, with the
Kalman state-space CCI (Spec 9; available on the back-extended
sample) as a less-imposed cross-check that does not require
institutional knot choices.

### 10.4 Nesting in MARTIN

A natural use of a freely-estimated equation is to discipline the
calibrated consumption block of the RBA's MARTIN model (Ballantyne et
al. 2019), whose long run is homogeneous of degree one in real income
and real net wealth — log c = w·log y + (1 − w)·log NW + (rate term)
— with a calibrated net-wealth elasticity (1 − w) ≈ 0.17. We test
that structure directly
([`australia_martin_nesting.csv`](../outputs/australia_martin_nesting.csv))
by fitting the static long run
log c = β₀ + β_y·log y + β_NW·log(NW/y) + β_r·r. Because
log(NW_real_pc) = log(NW/y) + log y + log 4, the coefficient on log
income equals the sum of MARTIN's income and wealth weights (which
homogeneity sets to one) and the coefficient on the net-worth ratio
is the net-wealth elasticity:

| Quantity | Estimate | MARTIN |
|---|---:|---:|
| Net-wealth elasticity (β_NW)    | 0.1155 | 0.17 |
| Income + wealth weight (β_y)    | 0.7198 | 1.00 (imposed) |
| Real-rate semi-elasticity (β_r) | −0.0086 | small, calibrated |

Three findings bear on integration. First, the unrestricted
net-wealth elasticity, 0.1155, is below MARTIN's calibrated 0.17 but
of the same order. Second, **MARTIN's long-run homogeneity
restriction is rejected** on the Australian data: the
income-plus-wealth weight is 0.7198, and the test β_y = 1 gives
χ²(1) = 16.41 (p = 0.0001). Imposing homogeneity by force does not
help — the restricted net-wealth elasticity flips to −0.0832
(restricted real-rate −0.0038), so the data resist the
balanced-growth parameterisation in this simple form. Third, the
static long run does not cointegrate under Engle–Granger MacKinnon
critical values (ADF −2.3828 vs 5 per cent CV for four variables
−4.10), echoing the cointegration-screen result of §6 — and this
caveat bites on the second finding too: the homogeneity Wald test
sits inside a non-cointegrated static levels regression, where its
distribution is non-standard, so the rejection is indicative rather
than decisive.

The reading for MARTIN is that the freely-estimated single-equation
long run does **not** nest MARTIN's calibrated balanced-growth block:
homogeneity would have to be imposed rather than tested-and-accepted
(with the non-cointegration caveat above), and the comparable
unrestricted wealth elasticity (0.12) sits below MARTIN's 0.17
without a committed standard error to adjudicate the gap. The
disaggregated evidence is now more informative than in an earlier
draft — Spec 11's aggregate-wealth structural MPC is 0.100 with a
95 per cent CI of [0.036, 0.164] (`australia_gamma_inference.csv`),
significantly positive rather than zero-spanning — but an MPC on the
wealth-to-income ratio is not the same object as MARTIN's log-wealth
elasticity, so it corroborates a positive wealth channel of the right
order rather than recalibrating MARTIN's parameter. The honest
position remains that this equation is *not yet MARTIN-ready as a
source of point estimates*. The MARTIN-operational version of the
permanent-income measure is the real-time Italian direct forecaster
(§7.4), not the full-sample look-ahead measure used for the headline;
any integration must use the real-time variant, under which the speed
of adjustment is materially weaker. A productive integration path is
therefore to import the *qualitative* structure — disaggregated
wealth channels and a credit-conditions interaction, with the signs
and rough magnitudes of the channels — while retaining MARTIN's
calibrated long-run weights, using this paper as a freely-estimated
benchmark the calibration is consistent with rather than as a source
of point estimates precise enough to replace it.

Source:
[australia_martin_nesting.csv](../outputs/australia_martin_nesting.csv),
[australia_gamma_inference.csv](../outputs/australia_gamma_inference.csv).

---


## 11. Conclusion

This paper re-estimates the Williams (2010, 2012) Australian LIVES
consumption equation on a sample extended to 2024Q4, and in doing so
revisits what an earlier draft and much of the prior Australian
literature had treated as a *weak* single-equation result. The
central finding is methodological: the functional **form** of the
LIVES equation is what identifies it. When housing wealth is entered
faithfully through its credit-conditions interaction alone, the
autonomous-consumption CCI intercept is restored, and illiquid
financial assets are combined, the error-correction mechanism and the
core wealth structure come alive in a single equation — and they stay
alive under every COVID treatment. When housing instead enters as a
plain, constant-MPC level term — as in the conventional disaggregated
error-correction model that prior work implicitly tested — the
equilibrium is weaker and sample-fragile and the standalone housing
coefficient is insignificant. Reading that insignificant standalone
coefficient as a failed housing-wealth effect is a category error:
LIVES theory predicts that coefficient is approximately zero absent
the credit-conditions interaction, because there is no classical
housing-wealth channel in the model.

A second, equally central finding is that the LIVES *structure*
transfers to Australia while Williams' Australian *calibrations* do
not — and the corrected data now grade the transfer channel by
channel. The faithful specification recovers Williams' speed of
adjustment to within about 13 per cent and matches his
illiquid-financial m.p.c., but its intervals are tight enough to
reject his net-liquid magnitude; imposing his calibrated
permanent-income gearing collapses the equilibrium. We document why —
the credit-conditions interactions are mutually collinear in the
post-deregulation sample and the deployed index has no variation
before 2007Q3 — and we are honest throughout about the limitations of
single-equation LIVES estimation: a split placebo verdict on the
credit-conditions index (the literal Williams construction sits at
the placebo median, the deployed protocol earns only moderate
support), a structural permanent-income gearing above its theoretical
admissibility bound, no efficiency gain from joint estimation at the
quarterly frequency, an out-of-sample loss to a random walk beyond
one quarter, and a look-ahead permanent-income measure whose
coefficient reverses under a real-time construction.

### 11.1 Form is decisive: the faithful LIVES specification

The headline result is the faithful LIVES specification (Spec 11),
in which housing wealth enters *only* through the de-meaned
credit-conditions interaction `ha_x_cci` = CCI × (HA/4y), the
autonomous-consumption loading ζ_c·CCI (`cci_williams`) is restored,
and illiquid financial assets are combined into a single ratio
(`ilfa_y` = equities + superannuation). On this form the
error-correction and core wealth structure are recovered
(australia_spec11_variants.csv; australia_lambda_robustness.csv):

| Quantity | Full sample (n = 146) | Pre-COVID (n = 126) | COVID dropped (n = 138) | Williams (2010) |
|---|---:|---:|---:|---:|
| λ (ecm_lag) | −0.448 (t = −3.57) | −0.266 (t = −4.85) | −0.248 (t = −6.66) | −0.286 |
| NLA m.p.c. (`nla_y`) | +0.027 (t = 3.75)*** | +0.016 (t = 1.81)* | +0.017 (t = 2.11)** | 0.159 |
| IFA m.p.c. (`ilfa_y`) | +0.015 (t = 3.09)*** | +0.009 (t = 1.74)* | +0.010 (t = 2.00)** | 0.022 |
| housing-collateral (`ha_x_cci`, γ₁) | +0.0025 (t = 0.71) | +0.0019 (t = 0.86) | +0.0023 (t = 1.05) | 0.0488 |
| permanent income (`ln_yp_over_y`) | +0.459 (t = 4.04)*** | +0.298 (t = 5.81)*** | +0.281 (t = 8.68)*** | 0.20–0.95 |

The COVID-controlled speed of adjustment is tightly clustered at
λ ≈ −0.25 (−0.266 / −0.248 / −0.242 across the pre-COVID,
COVID-dropped and quarterly-dummy treatments), about 13 per cent below
Williams' (2010) −0.286, and is the value we treat as identified: the
full-sample λ = −0.448 is inflated by the COVID quarters and fails
the upper-bound speed screen. The net-liquid and illiquid-financial
marginal propensities are correctly signed in every variant and
significant at 5 per cent in the full-sample, COVID-dropped and
quarterly-dummy treatments, and permanent income enters strongly
everywhere (t = 4.0–8.7). The housing-collateral term γ₁ is
right-signed but insignificant in every variant (`ha_x_cci` +0.0025,
t = 0.71 full sample). Scaled by |λ| = 0.448, the implied structural
marginal propensities are NLA 0.060 [0.022, 0.098], IFA 0.035
[0.012, 0.057], and housing-collateral γ₁ 0.0055 (right-signed,
insignificant, versus Williams' 0.0488, which the interval excludes).
The one theory casualty is the permanent-income gearing: ψ̂ = OLS/|λ|
is 1.02–1.13, above the admissibility bound ψ ≤ 0.95, a breach that
survives removal of the GFC ogive and that we disclose as an open
puzzle (§7.0).

By contrast the conventional constant-MPC disaggregated ECM (Spec 6),
which carries plain `ha_y`/`eq_y`/`super_y`/`nla_y` with no credit
scaling and a 2002Q3-binding short-run credit term, delivers a
sample-fragile equilibrium on n = 86 (λ = −0.239, t = −2.55,
collapsing to −0.087 pre-COVID) and an insignificant standalone
housing coefficient (`ha_y` +0.0022,
t = 0.30). This is the conventional baseline, not the preferred
specification. The contrast between Spec 11 and Spec 6 is a matter of
theory rather than of fit: the two forms test different equations, and
only Spec 11 is the LIVES equation. Much of the apparent weakness of
single-equation Australian LIVES estimates is, on this reading, a
specification artefact.

### 11.2 Structure transfers; Williams' calibrations do not

The faithful form recovers Williams' structure when freely estimated.
It does *not* survive imposition of his Australian calibrations.
Specification 12 imposes Williams' permanent-income gearing
(ψ₀ = 0.20, ψ₁ = 0.93) and his illiquid-financial m.p.c.
(γ_IFA = 0.022) via an iterative fixed point; the error-correction
mechanism collapses to λ = −0.030 (t = −0.74) and flips sign
pre-COVID (+0.041, t = 2.03), with the net-liquid coefficient turning
wrong-signed (australia_all_results.csv;
australia_lambda_robustness.csv). Specification 10 (Williams-prior
calibrated) independently reproduces the collapse (λ = −0.048,
t = −0.78). The mechanism is that Australia freely estimates a
structural permanent-income gearing of order one (ψ̂ = 1.02–1.13) —
roughly five times Williams' calibrated ψ₀ = 0.20 on the consistent
structural scale; forcing his much lower value removes the
equilibrium. Williams' rate, affordability and autonomous-consumption
loadings cannot even be imposed at their published magnitudes: his
raw α_r = −0.871 is some thirty times too large on the repository's
percent real-rate × unit-normalised CCI scaling, and diverges the
fixed point.

This reconciles the companion Wald non-rejection of the joint
Williams calibration (χ²(6) = 7.55, p = 0.27;
LIVES/outputs/williams_calibration_wald.csv). The non-rejection is
driven by imprecision, not by good fit: in the Spec 6 frame every
Williams wealth value lies inside a wide structural confidence
interval that also contains zero
(australia_gamma_inference.csv). The free Spec 6 estimates are too
imprecise to reject Williams' values, yet imposing those values
wrecks the fit. Low power is not the same as good fit. In the
faithful Spec 11 frame, by contrast, the inference cuts both ways:
the intervals reject his net-liquid m.p.c. (0.159 outside
[0.022, 0.098]) and his peak housing m.p.c. (0.0488 outside
[−0.010, 0.021]) while matching his illiquid-financial m.p.c. (0.022
inside [0.012, 0.057]) — agreement on form and on the IFA channel,
genuine disagreement on the net-liquid magnitude.

### 11.3 Why the credit channels are weakly identified in one equation

The reason the credit-conditions calibrations cannot be sharpened off
a single equation is identification, not sample length. The deployed
CCI is degenerate over much of the sample — only four of fifteen
candidate knots survive, all post-2007, so the index is identically
zero from 1988 to mid-2007 and the credit channels are identified off
roughly seventy quarters — and the CCI-interacted regressors are each
approximately proportional to the index and therefore 0.66–0.97
mutually correlated in absolute value
(australia_cci_interaction_corr.csv). They cannot be separately
free-estimated from one equation — the structural reason Williams uses
four-equation FIML. The evidence converges from three directions.
First, when the interactions are entered freely (Spec 8), the
individual interaction coefficients are wrong-signed or insignificant
against Williams' priors even though the fit is strong (BIC −952.8,
second only to Spec 11's −954.8), and the credit channels do not move
toward Williams' Table 1. Specification 8 reallocates identification;
it does not close the gap, and we do not present its λ = −0.458 as a
credit-conditions success. Second, the calibration collapse of
Specs 10 and 12 (§11.2) shows the channels are not jointly
recoverable by imposition either. Third, the credit-conditions
placebo battery returns a split verdict rather than a clean failure:
the literal Williams four-knot specification sits at the placebo
median (45th adjusted-R² percentile) and below it on the extended
samples (36th literal, 48th maximal, 37th sectional), while the
deployed maximal-GETS construction, evaluated under its own iterated
reduction protocol, beats 84 per cent of random draws — moderate
support, short of strong identification
(australia_williams_knot_placebo_verdict.csv;
australia_williams_knot_placebo_deployed_verdict.csv; extended and
maximal summaries). These are honest mixed-to-negative results, and
they are diagnostic: single-equation calibration of the credit
channels is empirically closed, and a four-equation FIML build is the
route to sharpen them.

### 11.4 The back-extension: sample length is not the binding constraint

A natural alternative hypothesis is that the residual gap between the
freely estimated Australian channels and Williams' Table 1 reflects
the post-1988 sample window, which begins only after the
financial-liberalisation episode that identifies the credit channels.
To test this we assembled a back-extended master dataset to 1976Q3
(n = 190–194 quarters) — using a Treasury TRYM long-run house-price
series, the RBA D03 M3 monetary aggregate, the RBA D02 total-credit
splice, a historical labour-force compilation, and aggregate and
disaggregated wealth proxies anchored at 1988Q3 — and refit the
simpler disaggregated no-CCI specification (Spec 4) on the longer
window (spec46_extended_comparison.csv):

| Structural coefficient | 1988+ baseline | 1976+ extended | Williams (2010) |
|---|---:|---:|---:|
| λ | −0.182 | −0.203 | −0.286 |
| NLA (γ_NLA) | +0.025 | +0.001 | +0.159 |
| IFA equities (γ_EQ) | −0.043 | −0.041 | — |
| IFA super (γ_super) | +0.014 | −0.005 | — |
| IFA combined (γ_IFA) | −0.030 | −0.046 | +0.022 |
| housing (γ_HA) | +0.019 | +0.018 | +0.049 |

The Williams (2010) reference values are his housing wealth m.p.c.
(0.0488 at the CCI peak), net-liquid m.p.c. (0.159) and calibrated
illiquid-financial m.p.c. (0.022, not split between equities and
superannuation); the back-extension figures are the Spec 4
disaggregated structural coefficients
(spec46_extended_comparison.csv). The speed of adjustment improves by
about 12 per cent (−0.182 → −0.203, still 29 per cent short of
−0.286; the aggregate Spec 1 moves similarly, −0.193 → −0.209,
spec1_extended_comparison.csv), but the individual wealth
coefficients shrink rather than converging on Williams' values: the
net-liquid m.p.c. collapses by 95 per cent toward zero, the equities
coefficient retains a wrong sign, and superannuation flips sign. Sample
length is therefore not, in itself, what generates the divergence; the
diagnosis remains the single-equation
framing and the collinearity of the credit channels. The
back-extension stands as a contribution in its own right — it makes
this test possible for the first time — and supports the conclusion
that the route forward is joint estimation rather than a longer single
equation.

### 11.5 Honest negative results

The paper reports its negative results prominently, because a freely
estimated single-equation framework permits diagnostics that imposed
restrictions would hide. Each points to why FIML and back-extension
are the natural next steps rather than further single-equation tuning.

- **Credit-conditions placebo: split verdict.** The literal Williams
  four-knot construction sits at the placebo median (45th adjusted-R²
  percentile) and below it on the extended variants (36th–48th;
  sectional 37th), sustaining the detrending critique; the deployed
  maximal-GETS protocol beats 84 per cent of random draws — moderate
  support, not strong identification
  (australia_williams_knot_placebo_verdict.csv;
  australia_williams_knot_placebo_deployed_verdict.csv).
- **No system-efficiency gain.** A two-equation SUR of consumption
  and house prices yields negligible cross-equation residual
  correlation (ρ̂ = −0.0133 against −0.0109 OLS;
  LIVES/outputs/lives_sur_2eq_resid_corr.csv), and the SUR coefficient
  estimates are within sampling noise of single-equation OLS
  (australia_joint_pi_robustness.csv): joint estimation buys no
  efficiency at the quarterly frequency in this sample.
- **Out-of-sample: a one-quarter win, multi-step losses.** At h = 1
  Spec 8 (RMSE 0.0290) and Spec 11 (0.0292) beat the random walk with
  drift (0.0309), but at h = 4 and h = 8 the random walk beats every
  structural specification, and Spec 11 is the worst performer at
  h = 8 (0.0640) (australia_oos_rmse.csv). The exercise uses
  full-sample permanent-income and CCI inputs, so it measures fit
  stability under re-estimation, not real-time forecast accuracy
  (§8.13). The framework's value is structural interpretation, not
  point forecasting.
- **Selector divergence, now narrower.** The automated screen falls
  back to a most-passes rule (no specification clears the
  Engle–Granger cointegration screen at the correct MacKinnon
  critical values; australia_cointegration.csv) and returns the
  net-worth Spec 3, while BIC and LIVES theory now *agree* on Spec 11
  (BIC −954.8; australia_spec_selection.csv). We headline Spec 11 on
  joint BIC-and-theory grounds and report the rubric fallback's
  divergence — and the universal cointegration failure — as documented
  limitations.
- **Look-ahead permanent income.** The headline permanent-income
  result uses the full-sample (non-causal) Italian direct forecaster.
  Under a causal real-time construction the speed of adjustment
  shrinks but survives (λ = −0.159, t = −2.39, against −0.239
  full-sample) while the permanent-income coefficient flips sign
  (+0.325 → −0.145; australia_pi_realtime_robustness.csv): the
  positive permanent-income loading is reported as a property of the
  full-sample measure, not a real-time resolution of the Australian
  permanent-income puzzle.
- **The ψ admissibility bound is breached.** The freely estimated
  structural gearing ψ̂ = 1.02–1.13 exceeds the theoretical ψ ≤ 0.95
  bound, and the breach is not an artefact of the GFC ogive
  (australia_spec11_ogive_robustness.csv). We disclose it as an open
  puzzle (§7.0) rather than re-scaling it away.

### 11.6 MARTIN nesting and the wealth elasticity

Nesting the long run against the RBA's MARTIN consumption block, the
unrestricted net-wealth elasticity is 0.1155, against MARTIN's
calibrated 0.17 (australia_martin_nesting.csv). Long-run homogeneity
of the combined income-and-wealth term is rejected (χ²(1) = 16.41,
p = 0.0001) — though the test sits inside a static levels regression
that does not cointegrate (EG ADF −2.38 against a −4.10 critical
value), so the rejection is indicative rather than decisive — and the
wealth elasticity is not robustly identified: imposing homogeneity
drives the restricted net-wealth elasticity to −0.083, so the
estimate is too sensitive to the restriction to discipline MARTIN's
calibration. The appropriate reading is qualitative: the
LIVES structure can inform MARTIN's functional form, and Spec 11's
aggregate-wealth structural MPC of 0.100 [0.036, 0.164] corroborates
a positive wealth channel of the right order, but the single-equation
point estimates are not precise enough — nor on the right scale — to
recalibrate MARTIN's parameters.

### 11.7 Contributions

The paper makes five contributions to the Australian household
consumption literature.

1. **The faithful single-equation LIVES estimate (Spec 11) with the
   form correction as the lead methodological result.** Entering
   housing only through its credit-conditions interaction, restoring
   the autonomous-consumption CCI intercept, and combining illiquid
   financial assets recovers a Williams-consistent, COVID-robust
   error-correction speed (λ ≈ −0.25 across the COVID-controlled
   treatments, about 13 per cent below Williams' −0.286) and
   right-signed wealth structure (NLA +0.027***, IFA +0.015***),
   where the conventional constant-MPC ECM is weaker and
   sample-fragile. Reading the conventional form's insignificant
   standalone housing coefficient as a failed wealth effect is shown
   to be a category error. The structural intervals are tight enough
   to grade the transfer: the IFA m.p.c. matches Williams' 0.022, the
   NLA m.p.c. is genuinely smaller than his 0.159, and the housing
   channel is unproven.

2. **The structure-transfers-but-calibrations-don't finding.**
   Imposing Williams' permanent-income gearing collapses the
   equilibrium (Spec 12: λ = −0.030, flipping sign pre-COVID;
   independently reproduced by Spec 10: λ = −0.048), because
   Australia freely estimates a structural gearing of order one —
   roughly five times Williams' 0.20; this reconciles the companion
   low-power Wald non-rejection (χ²(6) = 7.55, p = 0.27) as
   imprecision, not good fit.

3. **The interaction-collinearity diagnosis.** The CCI-interacted
   regressors are 0.66–0.97 mutually collinear in absolute value
   (australia_cci_interaction_corr.csv), and the deployed index has no
   variation before 2007Q3, explaining why the credit channels cannot
   be separately identified off a single equation and why Williams'
   identification requires four-equation FIML — corroborated by the
   Spec 8 reallocation, the calibration collapse, and the placebo
   battery.

4. **A back-extended Australian master dataset to 1976Q3**
   (n = 190–194 quarters) with documented growth-rate splices for
   house prices, M3, total credit and labour force, plus aggregate and
   disaggregated wealth proxies — and the direct test it permits,
   showing that sample length is not the binding constraint.

5. **A structured robustness and placebo suite with honest negative
   results** — the split credit-conditions placebo verdict, the
   negligible SUR efficiency gain, the out-of-sample losses beyond one
   quarter, the universal cointegration-screen failure, the
   ψ-bound breach, and the look-ahead permanent-income caveat —
   reported as substantive findings, alongside a multi-equation
   scaffold (data preparation, house-price and mortgage-stock
   equations, joint CCI identification, SURs) on which a companion
   FIML paper can build directly.

### 11.8 Outstanding work

Two structural routes follow directly from the diagnosis above, and
four narrower investigations sit alongside them.

1. **A multi-equation companion paper.** A full LIVES build with FIML
   and cross-equation parameter restrictions — consumption, house
   prices, the mortgage stock and home-equity withdrawal — would
   estimate a single CCI loading across equations under sign
   restrictions, the route the collinearity diagnosis and the placebo
   and SUR results identify as the only way to sharpen the credit
   channels. Custom likelihood code and a sourced HEW series remain
   the binding obstacles.

2. **Pre-1988 extension of the credit channels, and richer credit
   data.** Because the financial-liberalisation episode that
   identifies the credit-conditions index largely predates the 1988Q3
   start of ABS sectoral balance-sheet data, extending the
   disaggregated credit-interaction regressors back into the
   deregulation window — building on the 1976Q3 back-extension — and
   bringing in richer direct credit-conditions data (lending
   standards, securitisation, loan-level series) would, in
   combination with FIML, give the credit channels the variation they
   need.

Narrower items: (i) **the ψ-bound breach** — investigating why the
structural permanent-income gearing estimates at 1.02–1.13 against an
admissibility bound of 0.95, including the role of the unit-income
restriction and the measure's non-causal tail; (ii) **a
lagged-wealth-timing variant**, entering the wealth ratios at t−1 to
test whether within-quarter timing inflates the wealth and ECM
loadings; (iii) **a real home-equity-withdrawal series**, which both
completes the four-equation system and supplies a direct observable
for the collateral channel; and (iv) **the FIML estimator itself**,
whose cross-equation restrictions are the identification technology
everything above points to.

The single-equation framework cannot, on its own, deliver
identification of the credit channels that Williams (2010) obtains
from joint estimation under parameter restrictions. What it can do —
and what this paper establishes — is that the faithful LIVES *form*
recovers his error-correction and wealth structure where the
conventional constant-MPC ECM is weak, that the recovery survives
every COVID treatment, that his Australian *calibrations* do not
transfer, and that the credit interactions are collinear and weakly
identified off a single equation. Reported honestly, these results
point unambiguously to the multi-equation, back-extended programme as
the way forward.


## References

- Ando, A., & Modigliani, F. (1963). The "life cycle" hypothesis of
  saving: aggregate implications and tests. *American Economic Review*,
  53(1), 55–84.
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
  portfolios and the housing market in France. *Économie et
  Statistique*, Nos. 500-501-502. (Online complement available.)
- Cusbert, T., & Kendall, E. (2018). Meet MARTIN, the RBA's new
  macroeconomic model. *RBA Bulletin*, March 2018.
- Davidson, J. E. H., Hendry, D. F., Srba, F., & Yeo, S. (1978).
  Econometric modelling of the aggregate time-series relationship
  between consumers' expenditure and income in the United Kingdom.
  *Economic Journal*, 88(352), 661–692.
- Deaton, A. (1992). *Understanding Consumption*. Oxford: Clarendon Press.
- De Bonis, R., Liberati, D., Muellbauer, J., & Rondinelli, C. (2020). Consumption and wealth: new
  evidence from Italy. *Banca d'Italia Temi di Discussione (Working Paper)
  No. 1304* (November 2020). (Cited throughout as the
  Italian implementation / companion single-equation study.)
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

A full data-construction appendix — series sources, splice conventions,
deflation, the asset/annualised-income ratio transform, the
disaggregated wealth definitions and the 1976Q3 back-extension — is
reproduced from the accompanying
[data documentation](data.md). It retains the detail of §§3.1–3.13 of
the main paper, together with the master variable coverage table.

The three load-bearing constructions for the faithful LIVES specification
(Spec 11) and the conventional baseline (Spec 6) are summarised here for
the reader's convenience; the full provenance is in the data appendix.

**A.1 Disaggregated wealth (ratios to annualised income).** All wealth
terms enter as the current quarter's end-of-quarter (closing) stock
divided by annualised disposable income, $x_t/4y$ — *contemporaneous*
timing, not the $x_{t-1}/4y$ dating of Williams' bracket; the
implications for predeterminedness are disclosed in §3.2 and §4.6, with
the IV check in §8.1:

- **HA** — housing assets (ABS Balance Sheet 5232035). Enters the faithful
  LIVES form *only* through the credit interaction `ha_x_cci`
  (de-meaned $\mathrm{CCI}\times(\mathrm{HA}/4y)$); there is no standalone
  housing-wealth level term, consistent with the theory that the housing
  marginal propensity to consume is zero at $\mathrm{CCI}=0$ and is
  unlocked as credit conditions ease.
- **IFA** — illiquid financial assets, equities plus superannuation
  (`ilfa_y = eq_y + super_y`). Enters as a plain MPC.
- **NLA** — net liquid assets, liquid assets minus total household debt.
  Enters as a plain MPC. The net-liquid aggregation (constraining the
  liquid-asset and debt MPCs to be equal and opposite,
  $\gamma_{LA}+\gamma_{LOANS}=0$) is *not rejected* in any specification or
  sample (Appendix C; `australia_nla_restriction_test.csv`).

**A.2 Permanent income.** The permanent-income term $\log(y^p/y)$ is a
discounted ($\eta=0.05$, $k=40$ quarters) weighted average of *expected*
future income from the Italy-style direct (single-regression) forecast
of the discounted future-income aggregate (De Bonis et al. 2020,
Appendix A.2). The headline measure uses the full-sample (in-sample,
hence look-ahead) forecaster, whose training sample necessarily ends at
2014Q4 (the last quarter with a fully realised 40-quarter horizon), so
the final forty quarters — about 27 per cent of the estimation sample —
are out-of-training extrapolations; the series also carries a GFC
learning ogive that halves it from 2012Q2 onwards (no-ogive robustness:
`australia_spec11_ogive_robustness.csv`). The real-time variant is
reported as an operational robustness column (§7.4;
`australia_pi_realtime_robustness.csv`). This look-ahead-versus-real-time
distinction is flagged wherever a headline number depends on the
non-causal measure.

**A.3 The 1976Q3 back-extension.** Sectoral balance-sheet data from the ABS
begin in 1988Q3, which is why the financial-liberalisation episode that
identifies the credit channels largely *predates* the estimation sample.
A back-extended master dataset to 1976Q3 splices in TRYM house prices,
RBA D03 M3 and RBA D02 credit aggregates, historical labour-force series
and wealth proxies anchored to 1988Q3 levels. One caveat: the RBA D03
M3 series carries a +14.25 log per cent definitional break at
August 1976, inside the opening spine quarters, which makes 1976Q4 a
roughly 4.8σ growth outlier in every M3-based proxy (§3.10.1). The
back-extended dataset supports the long-history specification (Spec 6b)
and the direct sample-length test (§8): back-extending Spec 4 moves
$\lambda$ about a fifth of the way towards Williams' value (from
$-0.182$ to $-0.203$, against $-0.286$;
`spec46_extended_comparison.csv`) but individual wealth coefficients
shrink and the NLA term collapses — sample length is not the binding
constraint on identifying the credit channels.

---

## Appendix B: Coefficient tables

The full per-specification coefficient vectors, with Newey–West HAC
standard errors, $t$-statistics, $p$-values, the implied error-correction
speed $\lambda$ (the `ecm_lag` coefficient) and the implied structural
parameter ($\gamma = \mathrm{OLS}/|\lambda|$), are reproduced in machine
form from the committed results files. The current draft regenerates these
tables from the committed per-window results files,
[australia_full_results.csv](../outputs/australia_full_results.csv) and
[australia_precovid_results.csv](../outputs/australia_precovid_results.csv)
(consolidated in
[australia_all_results.csv](../outputs/australia_all_results.csv), with the
matched diagnostics in
[australia_full_diagnostics.csv](../outputs/australia_full_diagnostics.csv)
and
[australia_precovid_diagnostics.csv](../outputs/australia_precovid_diagnostics.csv)
and the four-sample λ vector in
[australia_lambda_robustness.csv](../outputs/australia_lambda_robustness.csv)),
which span all fourteen specifications — Spec 1 through Spec 12, plus
the long-history Spec 6b and the RBA-burden Spec 7b — for both the full
($n=146$; 1988Q3–2024Q4) and pre-COVID ($n=126$; 1988Q3–2019Q4) windows.
In particular they include the faithful LIVES headline (Spec 11) and the
Williams-calibration-imposed specification (Spec 12), which were absent
from the earlier draft's tables.

Rather than retype the full coefficient matrix, Table B.1 reports the two
specifications that carry the paper's central message — the faithful LIVES
headline (Spec 11) and the Williams-calibration-imposed specification
(Spec 12) — across both samples. The remaining specifications are tabulated
in §6 (the specification ladder) and §7 (results), and in full in Tables B.2–B.4 below and the
committed CSVs.

**Table B.1 — Faithful LIVES (Spec 11) vs Williams-calibration-imposed
(Spec 12): selected coefficients (Newey–West HAC).** Source:
`australia_full_results.csv`, `australia_precovid_results.csv`,
`australia_full_diagnostics.csv`, `australia_precovid_diagnostics.csv`.
Convention: $\lambda=$ `ecm_lag` (negative = error-correction); structural
$\gamma=\mathrm{OLS}/|\lambda|$. *** sig 1%, ** sig 5%, * sig 10%.

| Term | Spec 11 full ($n=146$) | Spec 11 pre-COVID ($n=126$) | Spec 12 full ($n=146$) | Spec 12 pre-COVID ($n=126$) |
|---|---|---|---|---|
| $\lambda$ (`ecm_lag`) | −0.448 (t=−3.57)*** | −0.266 (t=−4.85)*** | −0.030 (t=−0.74) | +0.041 (t=2.03)**, sign flip |
| `nla_y` | +0.0269 (t=3.75)*** | +0.0159 (t=1.81)* | +0.0011 (t=0.39) | −0.0002 (t=−0.09, wrong sign) |
| `ilfa_y` (=eq+super) | +0.0155 (t=3.09)*** | +0.0093 (t=1.74)* | imposed (γ=0.022) | imposed (γ=0.022) |
| `ha_x_cci` ($\gamma_1$) | +0.0025 (t=0.71) | +0.0019 (t=0.86) | −0.0012 (t=−0.88) | +0.0018 (t=1.52) |
| `hp_x_1_minus_cci` | +0.0279 (t=3.08)*** | +0.0137 (t=1.10) | — | — |
| `r_x_cci` | +0.0028 (t=4.55)*** | +0.0022 (t=3.47)*** | — | — |
| `cci_williams` ($\zeta_c$) | +0.0001 (t=0.01) | +0.0199 (t=2.77)*** | — | — |
| `ln_yp_over_y` | +0.4591 (t=4.04)*** | +0.2982 (t=5.81)*** | imposed ($\psi_0$=0.20) | imposed ($\psi_0$=0.20) |
| `yp_x_cci` | −0.5101 (t=−1.51) | +0.1869 (t=1.80)* | imposed ($\psi_1$=0.93) | imposed ($\psi_1$=0.93) |
| Intercept | −0.0139 (t=−2.13)** | −0.0099 (t=−1.54) | +0.0078 (t=1.81)* | +0.0048 (t=2.13)** |
| adj-$R^2$ | 0.824 | 0.239 | 0.687 | 0.073 |
| SE (%) | 0.683 | 0.510 | 0.911 | 0.565 |
| BIC (Schwarz) | −954.75 | −900.15 | −893.79 | −896.61 |

Implied structural MPCs for Spec 11 (full sample, $/|\lambda|=0.448$):
NLA 0.060, IFA 0.035, housing-collateral $\gamma_1$ 0.0055 (right-signed,
insignificant, $t=0.71$; cf. Williams' calibrated housing MPC 0.0488). The
COVID-controlled cluster $\lambda\approx-0.25$ (pre-COVID $-0.266$,
$t=-4.85$; Table B.4) is treated as the identified speed of adjustment —
about 13 per cent below Williams' $-0.286$ — because the full-sample
value is inflated by the COVID quarters and fails the $|\lambda|$
upper-bound screen. The headline numbers depend on the full-sample
(look-ahead) permanent-income measure; see §7.4 and the real-time
robustness column.

**Table B.2 — Speed of adjustment $\lambda$ (the `ecm_lag` coefficient) across all fourteen specifications, full and pre-COVID samples (Newey–West HAC).** Source: `australia_full_results.csv`, `australia_precovid_results.csv` (point estimates also in `australia_lambda_robustness.csv`). *** sig 1%, ** sig 5%, * sig 10%.

| Specification | $\lambda$ full ($n=146$) | $\lambda$ pre-COVID ($n=126$) |
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

(Several specifications estimate on different windows from the nominal
$n=146$/$n=126$: Specs 2, 5, 6, 7 and 10 on $n=86$ because a
2002Q3-binding credit term shortens their sample, Spec 7b on $n=64$,
and Spec 6b on the $n=180$ back-extended sample — see §6.1 and
`australia_full_diagnostics.csv`.)

**Table B.3 — Long-run coefficient matrix for the disaggregated specifications (full sample): OLS estimate ($t$-statistic).** Source: `australia_full_results.csv`. "—" = term not in that specification; the implied structural parameter is $\gamma=\mathrm{OLS}/|\lambda|$ (Table B.2). *** sig 1%, ** sig 5%, * sig 10%.

| Term | Spec 4 | Spec 5 | Spec 6 | Spec 8 | Spec 11 |
|---|---|---|---|---|---|
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

**Table B.4 — Spec 11 across the four sample treatments: the COVID-controlled headline.** Source: `australia_spec11_variants.csv` (Newey–West HAC $t$-statistics in parentheses).

| Variant | n | $\lambda$ | `nla_y` | `ilfa_y` | `ha_x_cci` | `ln_yp_over_y` |
|---|---:|---:|---:|---:|---:|---:|
| Full sample | 146 | −0.448 (−3.57) | +0.0269 (3.75) | +0.0155 (3.09) | +0.0025 (0.71) | +0.459 (4.04) |
| Pre-COVID (to 2019Q4) | 126 | −0.266 (−4.85) | +0.0159 (1.81) | +0.0093 (1.74) | +0.0019 (0.86) | +0.298 (5.81) |
| COVID quarters dropped | 138 | −0.248 (−6.66) | +0.0170 (2.11) | +0.0098 (2.00) | +0.0023 (1.05) | +0.281 (8.68) |
| Quarterly COVID dummies | 146 | −0.242 (−6.25) | +0.0141 (3.10) | +0.0084 (3.28) | +0.0016 (0.83) | +0.278 (8.18) |

The contrast between the Spec 11 and Spec 12 columns of Table B.1 is
the structure-transfers-but-calibrations-do-not result: imposing
Williams' Australian calibrations
($\psi_0=0.20$, $\psi_1=0.93$, $\gamma_{\mathrm{IFA}}=0.022$) via the
iterative fixed point collapses the error-correction speed from $-0.448$
to a statistically insignificant $-0.030$, and flips it to the wrong sign
and statistically significant $+0.041$ ($t=2.03$) pre-COVID. Spec 10 (Williams-prior calibrated;
`australia_full_results.csv`) independently reproduces the collapse
($\lambda=-0.048$, $t=-0.78$).

The structural-parameter inference (the implied $\gamma=\mathrm{OLS}/|\lambda|$
with delta-method and bootstrap confidence intervals, for both Spec 6 and
Spec 11) is in
[australia_gamma_inference.csv](../outputs/australia_gamma_inference.csv).
In the Spec 6 frame every Williams wealth value lies inside the (wide)
95 per cent interval, and so does zero — consistency by imprecision,
the low-power reading of the companion Wald non-rejection
($\chi^2(6)=7.55$, $p=0.27$). In the Spec 11 frame the inference cuts
both ways: the intervals exclude Williams' net-liquid m.p.c. (0.159)
and peak housing m.p.c. (0.0488) while including his
illiquid-financial m.p.c. (0.022); see §9.4.

---

## Appendix C: Diagnostic battery

The full diagnostic output is reproduced from
[australia_full_diagnostics.csv](../outputs/australia_full_diagnostics.csv)
and
[australia_precovid_diagnostics.csv](../outputs/australia_precovid_diagnostics.csv)
(consolidated in
[australia_all_diagnostics.csv](../outputs/australia_all_diagnostics.csv)).
For each specification and sample these report the number of observations,
the residual standard error (per cent), adjusted $R^2$, Durbin–Watson,
Breusch–Godfrey serial-correlation tests at lags 1 and 4, the
heteroscedasticity test in two forms (full and events-excluded), the
Chow break test together with a `chow_method` column recording which
test variant produced it (`sctest` where the standard implementation is
estimable, `manual_common_coef` where the sub-sample design matrices are
rank-deficient and a common-coefficient F-test is substituted, NA where
neither applies), the RESET functional-form test, the Schwarz/BIC and
the log-likelihood.

**C.1 Heteroscedasticity is structural, not event-driven.** Every
full-sample specification flags `het_diagnosis = "structural"`: the
heteroscedasticity test rejects homoscedasticity even after the COVID, GFC
and policy-event dummies are excluded (for Spec 11 the full-sample
heteroscedasticity $p\approx 1.7\times10^{-8}$ and the events-excluded
$p\approx 2.0\times10^{-9}$). This is why Newey–West HAC standard errors
are used for every reported $t$-statistic throughout the paper. The
pre-COVID Spec 11 residuals are, by contrast, well behaved (heteroscedasticity
$p=0.52$; no serial correlation, $\mathrm{AR}(1)\ p=0.32$,
$\mathrm{AR}(4)\ p=0.47$; RESET $p=0.75$).

**C.2 The multi-break Chow battery** is reported in
[australia_chow_battery.csv](../outputs/australia_chow_battery.csv) and
[australia_chow_battery_spec11.csv](../outputs/australia_chow_battery_spec11.csv),
with the structural-break tests in
[australia_breaks.csv](../outputs/australia_breaks.csv). On the
selector-preferred Spec 3, the 1995Q1, 2000Q1 and 2008Q3 breaks are not
rejected (Chow $p=0.98$, $0.73$ and $0.91$) while the 2020Q1 break is
strongly rejected ($p\approx 8.9\times10^{-16}$ — the COVID break). On
the faithful Spec 11 the 1995Q1 and 2000Q1 breaks are stable
($p=0.84$, $0.15$), the 2008Q3 break is rejected at 5 per cent
($p=0.017$), and the 2020Q1 test is not estimable in its standard form
(the `sctest failed` note in the CSV). The Bai–Perron supF statistic
(169.96, $p=0$, breakpoint dated 2019Q1 — the algorithm placing the
single permitted break just ahead of the COVID collapse) and the
recursive-residual CUSUM ($p=0.97$, stable) together locate the
dominant instability at the COVID episode. This break, and not a
deeper instability, is what inflates the full-sample $\lambda$
relative to the COVID-controlled value.

**C.3 The net-liquid restriction test**
([australia_nla_restriction_test.csv](../outputs/australia_nla_restriction_test.csv))
records `restriction_accepted = TRUE` in all six rows (Specs 4, 5, 6 across
full and pre-COVID samples): the data cannot distinguish separate liquid-
asset and debt MPCs, which supports the NLA aggregation used in the
faithful LIVES form.

**C.4 Cointegration screen**
([australia_cointegration.csv](../outputs/australia_cointegration.csv)).
The Engle–Granger ADF residual test fails to reject the null of no
cointegration for *every* estimable specification at the correct MacKinnon
critical values (e.g. Spec 11 ADF $-3.13$ against a $-5.47$ critical
value), while the Johansen rank-1 trace test passes for all estimable
forms. No single-equation form clears the Engle–Granger screen. Three
specifications are not run through it (`coint` reported NA): Spec 9,
whose Kalman-CCI interaction columns are not committed to the static
regression frame, and Specs 10 and 12, whose calibrated-offset long
runs make the static EG regression inapplicable.

---

## Appendix D: Reproducibility

The full reproducibility kit accompanies this paper. The estimation
pipeline can be run in three execution modes, all driven from the project
root (`/Users/davidstephan/Documents/consmodelling`):

1. **Cold rebuild (downloads + estimates).**
   ```
   Rscript Australia/R/australia_consumption_model.R
   ```
   Reads the cached ABS workbooks, fetches the RBA series live, rebuilds
   `master`, saves `australia_model_dataset.rds`, and then runs estimation.
   Required when the `data_raw/` workbooks change.

2. **Fast re-estimate from the cached RDS (no data work; bit-identical).**
   ```
   Rscript Australia/R/run_estimation_from_rds.R
   ```
   Loads the pre-built RDS and runs only the estimation script. This is the
   canonical reproduction command — it is the path exercised by continuous
   integration and produces bit-identical results.

3. **Offline replay from a portable master CSV (no downloads,
   hand-editable).**
   ```
   Rscript Australia/R/load_master_from_csv.R
   ```
   Loads `data_raw/master_data.csv`, reconstructs `master`, and runs
   estimation. Useful without internet access, for a frozen
   version-controllable snapshot, or to patch a known data error by hand.
   The portable CSV has been regenerated alongside the current pipeline
   run (194 rows × 120 columns, with the earlier vintage's +10 per cent
   house-price splice step at 2011Q3 eliminated — the boundary now shows
   a clean −1.78 per cent QoQ log change) and is consistent with the RDS
   path; an earlier draft's caveat about a stale CSV snapshot no longer
   applies.

The estimation script writes all fourteen specifications (Spec 1–12, plus
the long-history Spec 6b and the RBA-burden Spec 7b) to
[australia_full_results.csv](../outputs/australia_full_results.csv) /
[australia_precovid_results.csv](../outputs/australia_precovid_results.csv)
(consolidated in
[australia_all_results.csv](../outputs/australia_all_results.csv)) and the
matching diagnostics files, including the faithful LIVES headline
(Spec 11) and the Williams-calibration-imposed specification (Spec 12);
all robustness, placebo, counterfactual and comparison outputs cited in
the paper are written to the matching CSVs in `outputs/`.

The current pipeline run adds the following committed artefacts, all
cited in the text: the Spec 11 sample-treatment vector
(`australia_spec11_variants.csv`) and ogive-robustness run
(`australia_spec11_ogive_robustness.csv`); the deployed CCI path and
its interaction-correlation matrix (`australia_cci_williams_series.csv`,
`australia_cci_interaction_corr.csv`,
`australia_cci_williams_path.png`); the deployed-protocol knot placebo
(`australia_williams_knot_placebo_deployed.csv`,
`australia_williams_knot_placebo_deployed_verdict.csv`); the IV
diagnostics (`australia_iv_diagnostics.csv`,
`australia_iv_diagnostics_spec11.csv`) alongside the Spec 11 variants
of the robustness battery (`australia_iv_robustness_spec11.csv`,
`australia_chow_battery_spec11.csv`,
`australia_drehmann_robustness_spec11.csv`,
`australia_joint_pi_robustness_spec11.csv`,
`australia_scaled_income_robustness_spec11.csv`,
`australia_williams_income_robustness_spec11.csv`); the Spec 11
long-run decomposition (`australia_longrun_contributions_spec11.csv`,
`australia_longrun_decomposition_spec11.png`); and the two-specification
structural-gamma inference (`australia_gamma_inference.csv`, Spec 6 and
Spec 11 rows). The companion LIVES outputs cited here — the joint
calibration Wald test (`williams_calibration_wald.csv`), the SUR
residual correlations (`lives_sur_2eq_resid_corr.csv`) and the joint
knot-survival table (`lives_joint_cci_survival.csv`) — are committed
under `LIVES/outputs/`.

The environment is pinned with `renv` (R 4.5.3); the dependency manifest is
in `DESCRIPTION` and the full transitive closure — including the `car` and
`systemfit` packages used for the Wald and SUR tests — is locked in
`renv.lock`. Restore it with `renv::restore()`. The kit also ships GitHub
Actions continuous integration and a `testthat` unit-test suite. The
repository is hosted publicly at
<https://github.com/DavidAStephan/ConsModelling>.


