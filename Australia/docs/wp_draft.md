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
as an asset/annualised-income ratio, and we adopt the Jordà (2005)
local-projection permanent-income forecaster used in the Italian
implementation of De Bonis, Liberati, Muellbauer and Rondinelli (2020). The
cross-equation restriction γ_LA + γ_LOANS = 0 is accepted in every
disaggregated specification and sample window we estimate, validating
the net-liquid aggregation used in that Italian convention.

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
zero, and duly finds it insignificant; reading that as a failed
housing-wealth effect is a category error. When the equation is
instead specified faithfully — housing wealth entering only through its
CCI interaction, the autonomous-consumption CCI intercept restored,
and illiquid financial assets combined — the error-correction and core
wealth structure come alive (the faithful LIVES specification, Spec
11), estimated on n = 146 rather than the n = 86 to which the
conventional credit term binds the model. On the pre-COVID sample the
speed of adjustment is λ = −0.245 (t = −4.8;
australia_lambda_robustness.csv), within reach of Williams' published
−0.286 (about 14 per cent below it). On the full sample it carries
significant, correctly signed marginal propensities on net liquid
(implied 0.077; OLS +0.037, t = 3.4) and illiquid financial (implied
0.037; OLS +0.018, t = 3.0) wealth and a strong permanent-income
response (OLS +0.50, t = 4.0). The implied housing-collateral
propensity γ₁ is right-signed but insignificant (implied 0.010,
t = 1.0), consistent
with — but not confirmation of — Williams' calibrated 0.0488. The
conventional constant-MPC specification (Spec 6), by contrast,
delivers an insignificant λ = −0.18 (t = −1.8). Much of the apparent
weakness of single-equation LIVES estimates for Australia is thus a
specification artefact, not an economic result. (Permanent income is
the full-sample Italy local-projection measure; its non-causal
construction and a causal real-time robustness variant, which shrinks
λ further, are discussed in §7.4.)

The LIVES *structure* transfers to Australia, but Williams' Australian
*calibrations* do not. Imposing his calibrated permanent-income
gearing (ψ₀ = 0.20, ψ₁ = 0.93) collapses the error-correction to
λ = −0.029 (t = −0.7; the Williams-calibration-imposed specification,
Spec 12), independently reproduced by Spec 10 (λ = −0.048): the
Australian data freely estimates a permanent-income response roughly
two-and-a-half times Williams', and forcing his smaller value destroys
the equilibrium. This reconciles a puzzle in our companion work, where
a Wald test fails to reject Williams' joint calibration (χ² = 2.24) —
the free estimates are too imprecise to reject his values, yet imposing
them still ruins the fit; low power is not the same as good fit. The
credit-conditions *interactions* are themselves only weakly identified:
the six CCI-interacted regressors are 0.74–0.97 mutually correlated on
this sample because each is approximately proportional to the latent
index, so they cannot be separately estimated from a single equation.
This is exactly why Williams calibrates and estimates a four-equation
FIML system; on contemporary Australian data the single-equation
calibration shortcut is empirically closed, leaving joint estimation
and pre-1988 back-extension as the only routes to sharpen the credit
channels.

We assemble a back-extended master dataset to 1976Q3 — using a TRYM
long-run house-price series, RBA D03 monetary aggregates, RBA D02
total credit, historical labour-force compilations, and documented
aggregate and disaggregated wealth proxies anchored at 1988Q3, when
ABS sectoral balance-sheet data begin — to test whether sample length
is the binding constraint on tighter agreement with Williams.
Refitting the disaggregated no-CCI specification (Spec 4) on the
back-extended 1976Q3–2024Q4 sample (n = 190 in the cointegration
window), λ moves about 37 per cent closer to Williams (−0.140 →
−0.193) but individual wealth coefficients become smaller rather than
larger, and the net-liquid coefficient collapses toward zero — sample
length is not the binding constraint. The CCI placebo battery is weak
on both samples: the literal Williams 4-knot deteriorates from the
34th adjusted-R² percentile on the 1988+ sample to the 18th on the
back-extended sample (200 placebo draws;
australia_williams_knot_placebo_verdict.csv and
…_extended_summary.csv), the maximal-GETS reduction sits at the 64th
percentile and the sectional-period variant near the median. A
two-equation SUR of consumption and house prices delivers negligible
cross-equation residual correlation and essentially no efficiency
gain — the SUR and single-equation coefficients differ only within
sampling noise (australia_joint_pi_robustness.csv). A joint
cross-equation CCI identification retains only two of seven
single-equation knot survivors. We read these findings as indicating
that the structural identification Williams (2010) delivers comes from
cross-equation parameter restrictions in his four-equation FIML system
rather than from sample length, knot count, or sign-prior structure.
The calibration-collapse and interaction-collinearity results
independently confirm this: the faithful single-equation LIVES form
recovers Williams' error-correction speed and wealth structure, but
the separate identification of the credit interactions — and the
transferability of his credit-channel calibrations — require his joint
system.

We report the honest negative results throughout, not as caveats but
as substantive findings that a freely-estimated single-equation
framework permits and an imposed-restriction framework would hide: the
CCI placebo failure, the negligible SUR efficiency gain, the loss to a
random-walk-with-drift benchmark out of sample at horizons of four and
eight quarters, the divergence of model-selection criteria (the
automated screen favours a net-worth specification, the
Bayesian-information criterion the free-interaction specification, and
LIVES theory the faithful Spec 11), and the partial reversal of the
permanent-income response under a real-time, causal forecaster. Each
points to the same conclusion as the calibration and collinearity
diagnostics: four-equation FIML and pre-1988 back-extension are the
routes needed to sharpen the credit channels. The paper includes a
structured robustness suite covering instrumental variables, joint SUR
estimation, Chow tests at multiple break dates, the Drehmann (2017)
amortising-mortgage adjustment, the Williams smoothed-step
credit-conditions spline with maximal-GETS reduction, a Kalman
state-space credit factor, sectional sign-prior alternatives,
rolling-window estimation, out-of-sample forecast validation against
random-walk and AR(1) benchmarks, and back-extension robustness on
Spec 1 and Spec 4. A full reproducibility kit accompanies the paper,
along with a multi-equation LIVES scaffold that lays the foundations
for a companion paper.

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
Equation System, in the flow-of-funds tradition of Tobin and of Duca,
Muellbauer and Murphy (2013) — was developed to answer exactly these
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
adjustment falls to lambda = −0.029 (t = −0.68, full sample), statistically
indistinguishable from zero. An independent calibration route (Spec 10,
Williams-prior) reproduces the collapse (lambda = −0.048, t = −0.76). The
reason is that Australia freely estimates a permanent-income weight of order
0.50, around two and a half times Williams' 0.20, so forcing his value
breaks the long-run fixed point. This reconciles the companion paper's Wald
non-rejection of Williams' joint calibration: the free single-equation
estimates are too imprecise to *reject* Williams' values — every one of his
Table 1 values lies inside our gamma-inference 95 per cent intervals, but so
does zero in every case — yet imposing those values nonetheless wrecks the
fit. Low power is not the same as good fit.

**(iii) An interaction-collinearity diagnosis explaining why Williams needs
FIML.** The six CCI-interacted regressors that constitute the credit
channels are between roughly 0.74 and 0.97 mutually correlated on the
post-1988 Australian sample, because each is approximately proportional to
the same latent CCI. They cannot, therefore, be separately free-estimated
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
extended sample moves lambda about 37 per cent closer to Williams (−0.140 to
−0.193), but individual wealth coefficients shrink rather than strengthen
and the net liquid MPC collapses — so sample length is *not* the binding
constraint on tighter agreement with Williams.

**(v) A structured robustness and placebo suite, and a set of honest
negative results reported as substantive findings.** Mirroring the De Bonis,
Liberati, Muellbauer and Rondinelli (2020) Italian methodology, we run instrumental
variables, a Zellner SUR, multi-window Chow and Bai–Perron break tests, the
Drehmann amortisation adjustment, an AR/Italy local-projection
permanent-income comparison with a real-time column, a permanent-income
filter sensitivity grid, rolling-window estimation, and out-of-sample
validation against random-walk and AR(1) benchmarks. We retain — and report
prominently, without apology — the negative results that a freely estimated
single-equation framework permits: the credit-conditions spline does not
strongly outperform random-knot placebos (it sits in the 18th–34th
adjusted-R² percentile band across placebo variants); the SUR delivers
essentially no efficiency gain (negligible cross-equation residual
correlation); the structural specifications lose to a random walk with drift
out of sample at horizons of four and eight quarters; the automated selector,
the BIC and LIVES theory each point to a different specification; and the
full-sample permanent-income measure is partly reversed under a causal
real-time projection. Each negative is diagnostic: it points to why FIML and
back-extension are the routes forward.

### 1.3 Headline result

The faithful LIVES specification (Spec 11) is the headline. On the
full sample (1988Q3–2024Q4, n = 146) it delivers a speed of adjustment of
**lambda = −0.480** (Newey–West t = −3.59), and on the pre-COVID sample
(1988Q3–2019Q4, n = 126) **lambda = −0.245** (t = −4.80), close to Williams'
(2010) published −0.286 (his phi_c). We treat the pre-COVID estimate as the
identified value: the full-sample magnitude is inflated by the COVID
quarters and fails the upper-bound screen on |lambda| in the selection
rubric (§6), while the pre-COVID estimate is both correctly signed and
sharply determined. [Source: australia_all_results.csv;
australia_lambda_robustness.csv.]

The wealth structure is right-signed and, for the credit-invariant
components, significant. On the full sample the net liquid MPC is
**nla_y = +0.037** (t = 3.40, significant at 1 per cent; implied structural
MPC gamma_3 = OLS/|lambda| = 0.077) and the illiquid financial MPC is
**ilfa_y = +0.018** (t = 3.03, significant at 1 per cent; implied structural
gamma_2 = 0.037). The housing-collateral channel enters with the right sign
but is not significant: **ha_x_cci = +0.0049** (t = 1.03), implying a
structural housing-collateral MPC of gamma_1 = 0.010, against Williams'
peak housing MPC of 0.0488. Permanent income enters strongly:
**ln_yp_over_y = +0.504** (t = 4.00) on the full sample and +0.279
(t = 4.75) pre-COVID, consistent with a freely estimated permanent-income
weight of order 0.50. [Source: australia_all_results.csv.]

The honest qualifier on the housing channel is that it is *right-signed but
insignificant*: gamma_1 = 0.010 (t = 1.03) is consistent with Williams'
0.0488 only in the weak sense that the latter lies inside a wide confidence
interval that also contains zero. This is consistency, not confirmation — a
non-rejection driven by imprecision rather than by point-estimate agreement.
The accompanying gamma-inference exercise makes the point general: every one
of Williams' Table 1 values lies inside our 95 per cent interval, but so
does zero in every case. [Source: australia_gamma_inference.csv.]

The contrast with the calibration route is sharp. Imposing Williams'
Australian calibrations (Spec 12: psi_0 = 0.20, psi_1 = 0.93,
gamma_IFA = 0.022) collapses the error-correction term to lambda = −0.029
(t = −0.68) on the full sample and flips its sign on the pre-COVID sample
(lambda = +0.030, not significant); the independent Williams-prior route
(Spec 10) reproduces the collapse at lambda = −0.048 (t = −0.76). The LIVES
*structure* transfers to Australia; Williams' Australian *calibrations* do
not. Williams' rate, affordability and autonomous-consumption loadings
cannot even be imposed at their published magnitudes — his raw rate loading
(alpha_r = −0.871) is roughly thirty times too large on the repository's
percent real-rate by unit-normalised-CCI scaling, and diverges the long-run
fixed point. [Source: australia_all_results.csv;
australia_lambda_robustness.csv.]

By contrast, the conventional constant-MPC disaggregated ECM (Spec 6) — the
specification an earlier draft, and prior Australian work, treated as the
LIVES equation — delivers lambda = −0.180 (t = −1.76, full sample, n = 86),
significant only at the 10 per cent level, with an insignificant standalone
housing coefficient (ha_y = +0.0088, t = 1.52). We retain Spec 6 as the
conventional baseline against which the faithful form is the alternative,
but we no longer read its insignificant standalone ha_y as a failed
housing-wealth effect: under LIVES theory that coefficient is expected to be
approximately zero absent the credit interaction. [Source:
australia_all_results.csv.]

Finally, the credit-conditions identification limits are reported as
findings rather than hidden. The deployed CCI spline sits in the
18th–34th adjusted-R² percentile band across random-knot placebo variants;
the six CCI interactions are mutually collinear (roughly 0.74–0.97) and so
cannot be separately identified off one equation; the SUR of consumption and
house prices shows negligible residual correlation and no efficiency gain;
and the structural specifications lose to a random walk with drift out of
sample at the four- and eight-quarter horizons. We do not promote the
CCI-interactions specification (Spec 8, lambda = −0.445) as a
credit-conditions success: it re-allocates identification across regressors
rather than closing the gap with Williams, and its individual interaction
coefficients are wrong-signed or insignificant. [Source:
australia_williams_knot_placebo_verdict.csv; australia_oos_rmse.csv;
australia_all_results.csv.]

### 1.4 Roadmap

Section 2 surveys the LIVES literature, the Australian aggregate-wealth-effect
literature and permanent-income measurement, and frames the LIVES claim that
there is no classical housing-wealth effect — so that standalone-wealth
specifications mis-test the theory. Section 3 documents data construction,
including the disaggregated wealth ratios, the Italy local-projection
permanent-income forecaster with its look-ahead versus real-time caveat, and
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
ease, reaching Williams' implied peak housing MPC of 0.0488 (Williams'
Table 1; williams_comparison.csv). The illiquid financial asset term
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
*European Economy* Discussion Paper 14, formalise what they label the
"LIVES" approach — the *L*atent *I*nteractive *V*ariable
*E*quation *S*ystem — emphasising the joint determination of
consumption, house prices, mortgage debt and home equity withdrawal in
a four-equation system identified by common factors and cross-equation
sign restrictions.

Two parallel implementations frame the present paper. De Bonis,
Liberati, Muellbauer and Rondinelli (2020) estimate a single-equation Italian
adaptation that imposes the cross-equation restriction
γ_LA + γ_LOANS = 0 (deposits and household debt enter with
equal-and-opposite coefficients, so that net liquid assets is the
operative quantity), adopts a Jordà (2005) local-projection
permanent-income forecaster, applies a Drehmann (2017)
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
documents. Williams (2010) constructs CCI as a latent variable
identified by a spline of smoothed-step dummies (`SDMMA` series —
five-quarter moving averages of four-quarter moving averages of step
dummies), Σ a_s·SDMMA_s, at four institutional turning points: 1979
(Campbell Committee, removal of interest-rate ceilings on bank
deposits), 1992 (banking distress and the entry of the first mortgage
originator, Aussie Home Loans), 1998 (the rise of non-bank financial
institutions and securitisation), and 2007 (the global financial crisis
tightening). The institutional chronology of Australian financial
deregulation underpinning these choices is documented in Battellino and
McMillan (1989) and Edey and Gray (1996); Bayoumi (1993) provides a
cross-country analysis of the consumption response to financial
liberalisation, including Australia, that quantitatively validates a
structural CCI shift in the early 1980s.

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
sample they are 0.74–0.97 mutually correlated (a property of the
estimation sample; the closest committed evidence is the
identification-versus-detrending decomposition in §5 and the
sign-failures and collapse documented in §7). Six near-collinear
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
specification (the headline of §7.0) delivers a pre-COVID speed of
adjustment of λ = −0.245 (t = −4.80), almost exactly Williams' −0.286,
on a sample of n = 146 (full) / 126 (pre-COVID); correctly signed and
significant net-liquid and illiquid-financial marginal propensities
(structural MPCs of 0.077 and 0.037 respectively, full sample,
relative to |λ| = 0.480); a strong permanent-income coefficient
(+0.50, t = 4.0); and a housing-collateral coefficient (γ_1) that is
right-signed but insignificant (implied structural MPC 0.010, t = 1.03,
against Williams' peak 0.0488). By contrast, the conventional
constant-MPC disaggregated error-correction model that prior work and
an earlier draft of this paper treated as the LIVES equation is *not*
the LIVES equation and delivers an insignificant speed of adjustment.
Reading its insignificant standalone housing coefficient as a failed
housing wealth effect is the category error described above; much of
the apparent weakness of Australian single-equation LIVES estimates is
a specification artefact.

**(ii) The structure-transfers-but-calibrations-do-not finding.** The
LIVES *structure* transfers to Australia, but Williams' Australian
*calibrations* do not. Imposing his permanent-income gearing
(ψ_0 = 0.20, ψ_1 = 0.93) and illiquid-financial MPC (γ_IFA = 0.022)
collapses the equilibrium to λ ≈ −0.029 (t = −0.68), independently
reproduced by a Williams-prior calibrated specification (λ = −0.048),
because Australia freely estimates ψ at roughly 0.50 — about 2.5 times
Williams' 0.20 (a framing we hedge, since the headline ln(y^p/y)
coefficient is +0.50 and the ψ_0/ψ_1 split is not separately
identified). This reconciles the companion paper's Wald non-rejection
of the joint calibration (χ² = 2.24): the free estimates are too
imprecise to reject Williams' values, but imposing those values wrecks
the fit — low power is not the same as good fit. Williams' rate and
affordability loadings cannot even be imposed at published magnitudes:
his raw α_r = −0.871 is roughly thirty times too large on the repository's
percent real-rate × unit-normalised CCI scaling, and diverges the
fixed point.

**(iii) The interaction-collinearity diagnosis.** The six
CCI-interacted regressors are 0.74–0.97 mutually correlated on this
sample because each is approximately proportional to CCI, and so cannot
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
sample moves the speed of adjustment 37% closer to Williams (λ from
−0.140 to −0.193; lambda_robustness.csv / spec46_extended_comparison.csv),
but the individual wealth coefficients shrink and the net-liquid-asset
coefficient collapses (a sign flip, −106%) — which we read as evidence
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
restrictions would hide them): the deployed credit-conditions index
sits only in the 18th–34th adjusted-R² percentile band of a knot
placebo across variants; a two-equation consumption + house-price SUR
finds negligible cross-equation residual correlation and so no
efficiency gain (the literal value ≈ −0.004 cited in the companion
work is a narrative figure; the committed evidence is the OLS-versus-SUR
coefficient stability in joint_pi_robustness.csv); out-of-sample, the
random walk with drift beats every structural specification at horizons
h = 4 and h = 8; the automated specification selector diverges from
both the BIC-minimising and the theory-preferred specification; and the
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
  `networth_y` (1988Q3+ ABS) they fit on n=146.
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
**asset/annualised-income ratios**, `x_{t-1}/4y`. The
wealth-to-annualised-income ratios used in the long-run consumption
equation are constructed as:

- `ha_y` = housing wealth / (4 × quarterly nominal disposable income)
- `eq_y` = equities (ex-super) / (4 × quarterly nominal disposable income)
- `super_y` = superannuation reserves / (4 × quarterly nominal disposable income)
- `ilfa_y` = (equities + superannuation) / (4 × quarterly nominal disposable income)
- `nla_y` = (deposits − total household debt) / (4 × quarterly nominal disposable income)
- `debt_y` = total household debt / (4 × quarterly nominal disposable income)
- `networth_y` = closing net worth / (4 × quarterly nominal disposable income)

Each ratio is dated `x_{t-1}/4y` (beginning-of-period stock over
annualised current income), consistent with the timing convention in
Williams' bracket.

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
−0.006, t = −0.13; Spec 9 `eq_y` = −0.013, ns), reflecting the high
collinearity between the two series and the short modern sample rather
than a negative MPC on equities. Combining them into `ilfa_y` recovers a
correctly signed and significant illiquid-financial loading in the
faithful form (Spec 11 full-sample `ilfa_y` = +0.0178, t = 3.03, ***;
implied structural MPC = 0.037), the same order of magnitude as
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
e.g. Spec 4 full sum = +0.0800, t = 1.43, p = 0.15; Spec 6 full sum =
+0.105, t = 1.02, p = 0.31). The honest reading is that the modern
sample cannot statistically distinguish separate liquid-asset and debt
MPCs, which is consistent with — but does not by itself confirm — the
NLA aggregation; netting is a defensible economy of parameters rather
than a sharply identified result.

In the faithful LIVES specification, NLA carries the largest and most
robust wealth loading: Spec 11 full-sample `nla_y` = +0.037 (t = 3.40,
***), an implied structural MPC of 0.077, against Williams' calibrated
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
| TRYM | Treasury Macroeconomic Model historical database (`house_price_history_long.csv`) | 1959Q3–2018Q2 | growth-rate, anchored at 1986Q2 |
| Legacy | `houseprice_old.csv` (privately compiled pre-2003 dwelling-price index, monthly→quarterly) | 1986Q2–2003Q3 | growth-rate, anchored at 2003Q4 |
| Bridge | ABS Cat 6416.0 Residential Property Price Index, 8-capital-cities ("old method") | 2003Q4–2017Q2 | growth-rate, anchored at first overlap |
| Current | ABS Cat 6432.0 Total Value of Dwellings, mean price | 2003Q3–2024Q4 | (the modern overlay) |

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
ln_hp_over_y = log(hpi / (ydi_ann_nom / pop_millions / (cons_deflator_norm / 100)))
```

i.e. the log of the real house-price index divided by real disposable
income per capita.

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
the consumption equation, and (as §5 develops) the six CCI-interacted
regressors are between 0.74 and 0.97 mutually correlated on this sample —
each is approximately proportional to CCI — so they cannot be separately
identified off the consumption equation alone.

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
  **only three** of fifteen candidate knots survive the sign-prior
  reduction (australia\_williams\_cci\_knots.csv): `sdmma_2009_01` (+),
  `sdmma_2019_01` (−) and `sdmma_2020_04` (+); the 1979 and 1986 knots
  are aliased/constant within the window and the remaining candidates
  violate their priors. Only one of Williams' four canonical knots
  (2007) survives on a post-1988 sample. We report a full placebo
  battery on the surviving spline in §5: the deployed CCI sits in the
  18th–34th adjusted-R² percentile band across placebo variants, a
  result we preserve as an honest negative.

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
use NPY as a robustness column (§8): substituting NPY for gross income
roughly halves the estimated speed of adjustment (Spec 2 λ = −0.193 →
−0.094; australia\_williams\_income\_robustness.csv). The income measure is one
of several channels through which our estimates and Williams' diverge,
and we treat it as a methodological caveat rather than a resolved choice.

### 3.7 Permanent income

Permanent income (`yp`) is the forward-looking object in Williams' ratio
`log(yp/y)`. We construct it as a **discounted weighted average of
expected future income** from a forecasting model, following the Italian
implementation's Jordà (2005) local-projection approach. The baseline
uses discount η = 0.05 (equivalently a per-quarter weight δ = 0.95) over
a horizon k = 40 quarters
(australia\_permanent\_income\_sensitivity.csv, baseline row δ = 0.95,
k = 40). The estimated speed of adjustment is very stable across
δ ∈ {0.90, 0.95, 0.97} and
k ∈ {20, 40, 60}.

Two important measurement caveats are preserved throughout the paper:

1. **Look-ahead vs real-time.** The headline permanent-income measure is
   estimated on the full sample and therefore uses information not
   available in real time. The real-time variant (re-estimating the
   forecaster recursively) shrinks the speed of adjustment sharply and
   flips or shrinks the `log(yp/y)` coefficient
   (australia\_pi\_realtime\_robustness.csv; e.g. AR real-time λ =
   −0.051, ns; Italy real-time λ = −0.118 vs Italy full-sample −0.197).
   We flag the full-sample measure as the headline and the real-time
   column as the operational robustness check; headline results that
   depend on the look-ahead measure are identified as such in §7.

2. **Forecaster choice and the Australian permanent-income puzzle.**
   Under an AR forecaster the `log(yp/y)` coefficient is *negative*
   (the "Australian permanent-income puzzle"); under the Italy-style
   local-projection forecaster it is positive and the Italian forecaster
   fits marginally better (australia\_pi\_method\_meta.csv: AR
   adj-R² = 0.758, Italy adj-R² = 0.769). The faithful LIVES
   specification uses the Italy-style measure and recovers a strong,
   correctly signed permanent-income loading (Spec 11 full-sample
   `ln_yp_over_y` = +0.504, t = 4.0, ***).

In the canonical LIVES bracket the permanent-income weight is itself
credit-dependent, ψ(CCI) = ψ₀ + ψ₁·CCI with ψ₀ = 0.20 and ψ₁ = 0.93
**calibrated** (not freely estimated) by Williams. As §7 and §9 develop,
this is the calibration that most clearly does not transfer: Australia
freely estimates ψ on the order of 0.50, and imposing Williams' value
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
a very large dummy (`d2020_covid` ≈ −0.157 in Spec 11), and the
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
monthly, **1959Q3–2026Q1** (continuous, no series breaks). Aggregated to
quarterly by mean of the three monthly observations.

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
raw_proxy[t]    = (m3_household_proxy[t] + housing_wealth_proxy[t]) / ydi_ann_nom[t]
scale           = networth_y[1988Q3] / raw_proxy[1988Q3]    (≈ 1.68 in current vintage)
networth_y_proxy[t]
                = networth_y[t]              for t >= 1988Q3
                = raw_proxy[t] × scale       for t < 1988Q3
```

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
the Muellbauer-Williams LIVES (life-cycle / inter-temporal / credit-
conditions) equation is what identifies it. We therefore present the
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
(the cap `ψ(CCI) ≤ 0.95` binds at the credit-loose extreme); these are
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
coefficient `+0.0370` and `λ = −0.480` imply a structural NLA marginal
propensity of `0.0370 / 0.480 = 0.077` (§7.0). The structural-recovery
identity also makes precise *why* imposing Williams' calibrations can
collapse the equation: fixing several `γ_i` while iterating to the
fixed-point implied by the unit income restriction over-determines the
bracket, and the only free margin left — `λ` — adjusts toward zero
(§4.5, §7.0.1).

### 4.3 Reporting and sign conventions

Throughout, `λ` denotes the `ecm_lag` coefficient (the OLS coefficient
on `ln(c_{t-1}) − ln y_t`), with `λ < 0` indicating stable error
correction. We report the faithful specification's speed of adjustment
as `λ = −0.480` (t = −3.59) on the full sample and `λ = −0.245`
(t = −4.80) on the pre-COVID sample (`australia_all_results.csv`). We treat the pre-COVID value as the
*identified* speed of adjustment: the full-sample estimate is inflated
by the extreme COVID quarters and fails the upper-bound `|λ|` screen of
§6.2, whereas the pre-COVID `−0.245` sits close to Williams' (2010)
estimated speed of `φ_c = −0.286`. Where a headline number depends on
the full-sample, non-causal permanent-income measure (§4.4) we say so
explicitly and point to the real-time robustness column (§8).

### 4.4 Permanent-income forecasting

Permanent income `y^p_t` is the discounted weighted average of *expected*
future log income over a 40-quarter horizon (`k = 40q`) at quarterly
discount factor `δ_q = 0.95^(1/4)` — i.e. an annual discount factor
`δ = 0.95`, equivalently an annual discount rate `η = 0.05`:

> ln(y^p_t / y_t) = E_t [ Σ_{h=1}^{40} w_h ln(y_{t+h}) ] − ln(y_t),
>     with w_h = δ_q^(h-1) / Σ_{h=1}^{40} δ_q^(h-1).

We implement two forecasters. The headline measure is the Italy-style
local projection (Jordà 2005; De Bonis, Liberati, Muellbauer and Rondinelli 2020);
the rolling AR(8) forecaster is reported as a methodology-robustness
column (§8).

- **Italy local projection (headline measure).** For each `t` whose
  40-quarter-ahead horizon is observable, the discounted weighted
  average of future log income is computed and regressed, in a single
  full-sample equation, on a richer predictor set including
  `log(lf_share)` (the labour-force-share term that captures slow-moving
  demographic effects on trend income), a trend, a post-2008 split
  trend, the four-quarter moving average of log income, the
  unemployment rate, and four-quarter-difference dynamics. The
  permanent-income series is the in-sample fitted value of this single
  regression. It is therefore a two-sided, full-sample *measurement* of
  permanent income rather than a real-time forecast: because the
  coefficients are estimated over the whole sample, `y^p_t` embeds
  information dated after `t` and is non-causal. We carry it as the
  headline measure but flag this look-ahead property explicitly, and
  report a causal, expanding-window variant (re-fitting at each `t` only
  on observations whose full horizon is realised by `t`) as an
  operational robustness column. The real-time variant shrinks `|λ|`
  materially (§8): in the Spec 2 net-worth frame the full-sample Italy
  measure delivers `λ = −0.197` against `λ = −0.118` real-time and
  `λ ≈ −0.051` under the real-time AR forecaster
  (`australia_pi_realtime_robustness.csv`). Any forecasting use of the
  equation — embedding it in MARTIN, in particular — requires the
  real-time variant or the AR forecaster, not the full-sample measure.

- **Rolling AR(8) (robustness).** A rolling AR(8) regression of log
  income on eight own lags plus a linear trend, a post-2008Q3 step
  dummy, and a trend-break interaction; forecasts are aggregated over the
  40 horizons using the discount weights, with optional `unemp_rate`,
  `log_oil`, `log_reer`, `log_stocks` predictors and a 2008Q3 ogive
  learning weight that attenuates the term to a steady-state weight of
  0.5 over 15 quarters. Under this forecaster the long-run
  permanent-income coefficient turns negative — the "Australian
  permanent-income puzzle" of §8 — a sign reversal absent under the
  Italy measure.

The permanent-income discount and horizon settings are not load-bearing
for the speed of adjustment: across `δ ∈ {0.90, 0.95, 0.97}` and
`k ∈ {20, 40, 60}`, `λ` in the net-worth frame ranges only over
`−0.0824` to `−0.0830` (`australia_permanent_income_sensitivity.csv`),
and the GFC learning weight has no effect on these summaries. The
forecaster *method* (Italy vs AR, full-sample vs real-time), not the
discount calibration, is the material choice.

In the faithful specification the permanent-income channel is strong and
correctly signed: `ln(y^p/y)` enters with OLS level coefficient `+0.504`
(t = 4.0) on the full sample and `+0.279` (t = 4.75) pre-COVID
(`australia_all_results.csv`). This
level coefficient is of order one half — roughly two to three times
Williams' calibrated `ψ_0 = 0.20` — although Spec 11 does not separately
estimate the `ψ_0` / `ψ_1` split, so we read it as the gearing on the
permanent-income gap rather than as a sharp estimate of `ψ_0` alone. The
credit-geared component `ψ_1·CCI` is entered through a separate `yp_x_cci`
interaction; its full-sample coefficient (`−0.590`, t = −1.58) is
insignificant and wrong-signed relative to Williams' calibrated
`ψ_1 = +0.93`, so the data identify the level of the permanent-income
gearing but not its credit slope on a single equation. We return to this
in §7.0.1: imposing Williams' `ψ_0`, `ψ_1` and `γ_2` calibrations is
what collapses the equilibrium.

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

The implications follow directly. The conventional baseline delivers an
insignificant speed of adjustment (`λ = −0.180`, t = −1.76, full sample;
`australia_all_results.csv`) and an insignificant standalone housing
coefficient (`ha_y = +0.0088`, t = 1.52). Reading that insignificant
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

Wealth enters as asset-to-annualised-income *ratios*, `x_{t-1} / (4·y_t)`
— the prior-quarter stock divided by four times current quarterly
income — consistent with the LIVES treatment. The three components are

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

The credit interactions are entered in *de-meaned* form. The
housing-collateral regressor is `ha_x_cci`, the de-meaned product
`(CCI − CCĪ)·(HA/4y)`, and the autonomous-consumption loading
`cci_williams` is likewise de-meaned; the affordability and rate
interactions enter as `hp_x_1_minus_cci` and `r_x_cci`. De-meaning
centres each interaction at the sample-average credit regime, so that
the un-interacted coefficients (`γ_2`, `γ_3`, the intercept) retain
their interpretation as marginal propensities evaluated at the average
`CCI`, and the interaction coefficients measure the *additional* effect
of moving `CCI` away from its mean. Two consequences matter for
interpretation. First, under de-meaning a pure re-centring of the credit
regime is a timing-and-distribution effect, not a level effect, on
cumulative consumption. (This is distinct from the near-zero CCI
counterfactual of §10, which is run on the conventional baseline (Spec 6)
where `CCI` enters only the short run and so has no long-run channel to
integrate — confirming Spec 6 has no operative housing-collateral
channel.) Second, de-meaning does not break the
near-collinearity of the interactions — each remains approximately
proportional to `CCI` and they are 0.74–0.97 mutually correlated on this
sample (§5) — which is the structural reason the six interactions cannot
be separately free-estimated off a single equation, and why Williams
identifies them jointly through FIML.

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
be separately recovered from a single equation: the six CCI-interacted
regressors that carry the credit channels are between roughly 0.74 and
0.97 mutually correlated on this sample because each is approximately
proportional to `CCI` itself (§5.5). A single equation therefore cannot
tell them apart, and the spline, when fitted standalone to the
consumption-equation residual, behaves no better than a flexible
detrending series — it sits in the placebo distribution rather than
above it (§5.2). This is the structural reason Williams identifies the
credit channels through FIML, and it is why we treat four-equation FIML
and pre-1988 back-extension, not further single-equation search, as the
routes that could sharpen the credit channels. Throughout, the honest
negatives — placebo failure, near-zero SUR residual correlation,
selector divergence — are reported as substantive diagnostic findings,
not embarrassments.

### 5.1 The Williams smoothed-step spline approach

Williams (2010) constructs `CCI` as a spline of `SDMMA` smoothed-step
dummies — a 5-quarter moving average of a 4-quarter moving average of
a 0/1 step — at institutional turning points in the Australian
financial-policy chronology. Each knot's coefficient is constrained
by a sign prior derived from institutional history (deregulation
episodes positive; retrenchment episodes negative), enforced by
Hendry-Krolzig (2005) drop-on-violation general-to-specific reduction.

Williams' canonical paper uses four knots: 1979Q1 (Campbell Committee,
removal of interest-rate ceilings), 1992Q1 (NBFI distress after the
early-1990s recession), 1998Q1 (NBFI/securitisation expansion), and
2007Q1 (GFC retrenchment). The four-knot choice reflects the
institutional information available over his 1977-2008 sample: STAMP-
derived turning points and a deregulation calendar ending shortly after
the GFC.

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

On the 1988Q4-2024Q4 sample this candidate set reduces to **three
surviving knots** under the iterated drop-on-violation reduction
(`australia_williams_cci_knots.csv`):

| Knot | Sign prior | Coef (OLS) | Reading |
|---|---:|---:|---|
| 2009Q1 | + | +0.012 | First Home Buyer Boost |
| 2019Q1 | − | −0.034 | Hayne Royal Commission lending crackdown |
| 2020Q2 | + | +0.005 | COVID/JobKeeper income support |

(Ten candidate knots — 1990Q3, 1992Q1, 1993Q1, 1998Q3, 2007Q3, 2008Q4,
2014Q4, 2017Q1, 2019Q3 and 2021Q4 — violate their institutional sign
priors and are dropped; 1979Q1 and 1986Q1 are aliased, their smoothed
step being constant within the estimation window.)

The `cci_williams` series used throughout the rest of the paper is
constructed from these three surviving knots, peak-normalised to unity.
That only three of fifteen candidate knots survive — all of them
post-2008 — is itself part of the identification story (§5.3): the
post-1988 sample carries usable sign-identifying variation only around
the recent macroprudential and pandemic episodes.

The maximal-GETS construction is defensible on two grounds: (i) the
candidate set comes from documented Australian institutional history,
not authorial choice of specific dates; and (ii) the surviving knots are
those whose data signal aligns with their institutional sign prior, so
the spline is *empirically* selected rather than imposed. We do **not**,
however, claim that this delivers a structurally identified
credit-conditions factor. The placebo battery (§5.2) shows the deployed
spline does not robustly outperform random smoothed-step constructions,
and §5.5 documents why: the credit channels it is meant to carry are
near-collinear and cannot be separated off one equation. Williams'
canonical 4-knot setup is retained as a robustness benchmark, and a
sectional sign-prior alternative following Williams' (Aust paper §5.1)
specification is also implemented and placebo-tested; on the
back-extended sample it does not outperform the maximal-GETS canonical
(§5.2.2).

### 5.2 The placebo battery

Whether the spline is identifying genuine credit-conditions turning
points — rather than acting as flexible detrending of the
consumption-equation residual — is empirically testable. We construct a
random-knot placebo: 200 draws of knots in Williams' candidate window.
For the literal-Williams comparison all four drawn knots are entered
**unconditionally** — testing pure fit, with no sign-prior reduction —
so the canonical 4-knot series is compared like-for-like against random
4-knot series. For the maximal-GETS comparison each draw of 15 candidate
knots is passed through a single sign-prior reduction pass. (The
*deployed* `cci_williams` series uses the iterated reduction of §5.1.1,
which retains three knots; the maximal-GETS placebo applies a single
reduction pass, so its percentile benchmark is indicative of the
canonical procedure rather than an exact match to the deployed series.)
The canonical result's percentile rank in this placebo distribution
measures whether the specific knot choice identifies something the data
genuinely flags, versus whatever a flexible smoothed-step series could
fit by chance.

#### 5.2.1 Three placebo runs

| Specification | Sample | adj R² %ile | \|λ\| %ile | Verdict |
|---|---|---:|---:|---|
| Literal Williams 4-knot                 | 1988Q4+ (n=146) | 34th | 58th | Below R² median |
| Literal Williams 4-knot                 | 1976Q3+ (n=190) | **18th** | **10th** | Fails — below median on both |
| Maximal-GETS canonical (15-knot reduce) | 1976Q3+ (n=190) | **64th** | 37th | Weakly above median |

Sources: `australia_williams_knot_placebo_verdict.csv` (literal,
1988Q4+; Williams adj-R² = 0.7209, |λ| = 0.0799; n_placebo = 200);
`australia_williams_knot_placebo_extended_summary.csv` (literal,
1976Q3+; Williams adj-R² = 0.6789 versus a placebo median of 0.6816,
Williams |λ| = 0.1934 versus a placebo median of 0.2022);
`australia_williams_knot_placebo_maximal_extended_summary.csv`
(maximal-GETS, 1976Q3+; canonical adj-R² = 0.6852 versus a placebo
median of 0.6832, canonical |λ| = 0.2155 versus a placebo median of
0.2226, canonical surviving knots 7 versus a placebo median of 8).

The literal Williams 4-knot specification fails the placebo on both
samples, and the R² failure *deepens* on the extended sample (34th →
18th percentile). The maximal-GETS reduction partially rescues
identification — 64th percentile on adjusted R², 37th on |λ| — but does
not deliver strong support: random combinations of 15 candidate knots
under the same reduction protocol produce *faster* mean reversion than
the canonical institutional choice in 63 per cent of draws, and a higher
adjusted R² in 36 per cent. The committed verdict label is "WEAK
SUPPORT — institutional choice above median but not far". Read across
the variants, the deployed/institutional CCI sits in an **18th–34th
adjusted-R² percentile band**. We report this prominently as an honest
negative: the spline's standalone fit is consistent with that of a
flexible detrending series, not with a structurally identified common
factor.

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
priors), the sectional canonical sits at the **36th adjusted-R²
percentile and 40th |λ| percentile** — *worse* than the maximal-GETS
canonical, not better. Williams' specific period dating does not
outperform random period placements on the post-deregulation-extended
window.

The takeaway across §5.2.1 and §5.2.2 is uniform: neither the literal
4-knot construction, nor a maximal-GETS reduction, nor sectional sign
priors delivers strong placebo support on our extended sample. The CCI's
standalone identification is consistent with a single-equation OLS using
flexible smoothed-step dummies that the data can fit, and is not
consistent with a structurally identified common factor. The next two
subsections set out *why* this is the expected outcome of single-equation
estimation, and §5.5 supplies the direct mechanical reason.

### 5.3 Why the spline alone cannot identify the CCI as a common factor

The placebo evidence is consistent with the structural diagnosis in the
LIVES literature itself. Williams (2010) and Duca, Muellbauer and Murphy
(2013, ECB WP 1581) estimate the CCI inside a **multi-equation system**
(consumption, house prices, mortgage stock, home equity withdrawal —
Williams; consumption + refinancing rate — Duca, Muellbauer and Murphy)
where the *same* CCI series enters all equations simultaneously. Williams
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
equation, the house-price equation, and the mortgage-stock equation
simultaneously, then require each knot to satisfy its institutional sign
prior in **all three** equations to be retained.

| Survival regime | Surviving knots | n |
|---|---|---:|
| Consumption equation only (Spec 1 with `ln_networth_y_proxy`, extended sample) | 1979, 1986, 1992, 2007, 2009, 2017, 2020 | 7 |
| **Joint (C ∩ H ∩ M)**                  | **1986, 2017**                         | **2** |

Of the 7 knots that survive when fitted to consumption alone (this is the
single-pass reduction in `joint_cci_identification.R`, using the Spec-1
aggregate-proxy specification on the back-extended sample — distinct from
the *iterated* reduction the canonical consumption pipeline applies in
§5.1.1, which retains only three knots, 2009Q1, 2019Q1 and 2020Q2, on the
1988+ sample; the two reductions give different but overlapping survivor
sets), only **1986 (financial deregulation) and 2017 (APRA
macroprudential round II)** have signs consistent with their institutional
priors across consumption, house prices and mortgage stock simultaneously.
The other surviving knots sign-violate in the house-price or
mortgage-stock equations.

The single-pass protocol's identification of 7 knots was therefore
overstated: 5 of them were consumption-equation-specific and would not
survive a true cross-equation common-factor restriction. This is the
empirical content of the placebo failures in §5.2: without imposing
cross-equation sign consistency, the consumption-equation residual can be
flexibly fit by smoothed-step dummies whose information content is
consumption-specific.

#### 5.3.2 What joint identification fixes

When we rebuild `cci_williams_joint` using the 2 jointly-surviving knots
and re-estimate the house-price equation with the new CCI:

| HP equation, CCI loading | (a) cons-only CCI | (b) joint OLS | (c) joint SUR |
|---|---:|---:|---:|
| Estimate                 |  −0.024 | +0.024 | +0.024 |
| Sign                     |   ✗ violator | ✓ | ✓ |

The house-price equation's CCI loading flips from significantly negative
(under the consumption-only CCI) to significantly positive (under joint
identification) — Williams' cross-equation sign restriction working as
intended. The mortgage-stock equation's CCI loading remains negative
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
essentially negligible cross-equation residual correlation; with no CCI
spline and no event dummies it remains small and negative. The narrative
figure for the headline configuration is ρ̂(ε_C, ε_H) ≈ −0.004 — we flag
that the exact scalar is a reported summary, not a separately committed
CSV value, and the load-bearing committed evidence is the coefficient
comparison in `australia_joint_pi_robustness.csv`. There, moving from
single-equation OLS to SUR_joint on the Spec-2 consumption equation
shifts coefficients only within sampling noise: the net-worth loading
moves from +0.0615 to +0.0695 (+13.1 per cent, both highly significant),
the speed of adjustment from −0.0829 to −0.1273 (raw-units ecm_lag), and
the real-rate and permanent-income terms remain near zero under both. No
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

The empirical consequence is that the six CCI-interacted regressors are
between roughly **0.74 and 0.97 mutually correlated** on this sample. (We
report this as a property of the estimation sample established in the
analysis narrative; the specific pairwise correlation range is not itself
a standalone committed results CSV, and a drafter checking the exact
figures should compute the interaction correlation matrix from
`australia_model_dataset.csv`. The closest committed cross-check —
`australia_cci_method_4way.csv` — concerns correlations *between CCI
measures*, e.g. Kalman versus PCA ρ = 0.764, not between the six
interaction regressors, and so should not be substituted for the
interaction-collinearity figure.) Six near-collinear regressors carrying
distinct structural meanings cannot be separately free-estimated off a
single equation: ordinary least squares will allocate a near-singular
design among them more or less arbitrarily, producing wrong-signed and
insignificant individual loadings even when the joint contribution of the
credit block is real.

The committed, load-bearing evidence for this collinearity is threefold,
and each piece is itself an honest negative:

1. **Sign failures and insignificance when the interactions are freed
   (Spec 8, §5.6 and §7).** When the six interactions are estimated
   freely, the housing-collateral interaction is right-signed but
   insignificant, while three of the remaining interactions fail their
   institutional sign priors (`australia_spec8_sign_prior_verdicts.csv`).
   This is exactly the pattern a near-singular design produces.

2. **The identification-versus-detrending decomposition
   (`australia_cci_fit_decomposition.csv`).** Adding the Williams
   maximal-GETS CCI to the conventional baseline (Spec 6 → Spec 8) does
   *not* merely detrend: it shifts the permanent-income loading by +134
   per cent (from +0.20, t = 0.76, to +0.47, t = 3.44) and the speed of
   adjustment by −147 per cent (from −0.180 to −0.445), with the
   equities loading shifting +254 per cent. By the decomposition's own
   classification these are "IDENTIFICATION" (> 30 per cent) rather than
   "DETRENDING" (< 5 per cent) shifts. The credit interactions re-allocate
   identification across the income and speed-of-adjustment channels —
   which is precisely the symptom of collinearity (one near-singular
   block redistributing fit), not evidence of clean separate
   identification of each channel.

3. **The calibration collapse (Spec 10 and Spec 12, §7.0.1).** When
   Williams' calibrations are imposed instead of freed, the
   error-correction term collapses to λ = −0.029 (t = −0.68) in Spec 12,
   independently reproduced at λ = −0.048 (t = −0.76) by Spec 10. A block
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
the Williams interaction structure with the six channels entered as free
regressors:

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
| `ha_x_cci` (housing × CCI; γ_1)        | +0.0488 (peak MPC) | + | +0.0016 | 0.32 | right-signed, ns |
| `r_x_cci` (real rate × CCI; α_1)       | −0.871 (at CCI=1)  | − | +0.0019 | 1.85 | sign FAIL |
| `hp_x_1_minus_cci` (down-payment; α_4) | −0.13 (at CCI=0)   | − | +0.0076 | 1.00 | sign FAIL |
| `yp_x_cci` (PI × CCI; ψ_1)             | +0.93 (calibrated) | + | −0.6113 | −2.12 | sign FAIL |

The pattern is diagnostic of the near-singular design, not of structure.
The housing-collateral interaction is **right-signed** — consistent with
Williams' prediction that the housing MPC rises with credit ease as
collateral becomes spendable — but statistically insignificant (t = 0.32,
p = 0.75). Of the remaining three interactions, all fail their
institutional sign priors: the rate interaction is wrong-signed (and only
marginally significant, p = 0.066), the down-payment composite is
wrong-signed and insignificant, and the permanent-income interaction is
strongly wrong-signed (the free estimate is −0.61, t = −2.12, against
Williams' calibrated +0.93). The one term that survives its prior is the
*non*-interacted permanent-income level (`ln_yp_over_y` = +0.47, t = 3.44,
PASS). In other words, when the credit block is freed the equation
re-allocates almost all of its identification onto the income and
speed-of-adjustment channels and away from the individual interactions —
exactly the identification re-allocation §5.5 documents.

It is true that Spec 8 with the interactions included delivers a
numerically larger speed of adjustment, λ = −0.445 (t = −3.30), exceeding
Williams' calibrated −0.286 in magnitude, and the lowest BIC among the
n = 146 specifications (−948.476). We do **not** read this as Spec 8
"closing the gap" with Williams or "exceeding" him in any substantive
sense. The larger |λ| and the lower BIC reflect the re-allocation of fit
that a near-collinear block produces, not separate identification of the
credit channels: three of the four interactions are wrong-signed, and the
one right-signed interaction is insignificant. The honest reading is that
freeing the block buys fit at the cost of structurally meaningless
component loadings. The faithful LIVES specification (Spec 11, §7.0), by
contrast, enters housing *only* through its CCI interaction
(`ha_x_cci`, de-meaned, +0.0049, t = 1.03 — right-signed, insignificant)
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
| Kalman vs Williams maximal-GETS    | 0.050  | 194 |
| PCA vs Williams maximal-GETS       | −0.190 | 146 |
| Credit/income gap vs Williams      | 0.332  | 146 |
| Credit/income gap vs Kalman        | 0.304  | 146 |
| Kalman vs PCA                      | **0.764** | 146 |
| Credit/income gap vs PCA           | 0.238  | 146 |

The institutional spline is essentially uncorrelated with the Kalman
factor (ρ = 0.05) and weakly negatively correlated with the PCA factor
(ρ = −0.19); only the two data-driven statistical factors agree strongly
with one another (Kalman versus PCA, ρ = 0.76). The four candidate
measures do not converge on a common Australian credit-conditions series.
When the Kalman factor is used in place of the spline (Spec 9, §7), the
wealth and speed channels shift by 41–101 per cent relative to the
no-CCI baseline (`australia_cci_fit_decomposition.csv`), again
re-allocating identification rather than sharpening it. The measure
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
| λ (ecm_lag)    | −0.140 | −0.193 | +37.3 | −0.286 |
| nla_y          | +0.035 | −0.002 | −106  | +0.159 |
| eq_y           | −0.119 | −0.104 | −13.3 | +0.022‡ |
| super_y        | +0.040 | +0.024 | −41.7 | (incl.)‡ |
| ha_y           | +0.068 | +0.040 | −41.6 | +0.0488 |
| ln_yp_over_y   | +1.07  | +1.12  | +4.33 | +0.20  |

‡ Williams reports a single illiquid-financial-asset MPC (γ_IFA = 0.022), shown here against the combined eq_y + super_y. The Williams structural MPCs used throughout the paper (γ_HA = 0.0488, γ_NLA = 0.159, γ_IFA = 0.022) are the BIS-chapter values recorded in [`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv) — the same authoritative source as §7.3 and §11.4.

On Spec 4 the speed of adjustment moves 37 per cent closer to Williams'
value (−0.140 → −0.193), but the individual OLS wealth coefficients become
*smaller*, not larger; γ_NLA collapses to roughly zero and γ_EQ retains a
wrong sign. The long-run permanent-income coefficient remains far above
Williams' calibrated +0.20 on both samples.

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
to transfer. The automated rubric, applied mechanically, returns a
third, non-LIVES specification (Spec 2). We treat that divergence not
as a defect to be smoothed over but as a documented result (§6.3): no
single screen, BIC, or theory criterion agrees on a single
specification, which is itself diagnostic of how weakly a single
equation can pin down the LIVES long run on post-deregulation
Australian data.

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
| 6b  | Spec 6 with back-extension-compatible SR CCI | replaces Δ²log CCI with Δ²log RBA D02 credit; disaggregated wealth proxies; fits on n = 190 |
| 7   | Spec 6 + cohort terms + synthetic burden     | adds `prime_age_share, fhb_share` |
| 7b  | Spec 7 with RBA E13 measured burden          | post-2009 sample only |
| 8   | Williams CCI interactions (six, free)        | Spec 4 + `r×CCI`, `log(HP/y)×(1−1.2·CCI)`, `log(y^p/y)×CCI`, `ha_x_cci` |
| 9   | Spec 8 with Kalman state-space CCI           | replaces smoothed-step spline with state-space extraction |
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
   Dickey–Fuller value. Phillips–Ouliaris and single-equation Johansen
   results are reported alongside.
3. **Speed-of-adjustment screen** — λ has the correct (negative) sign
   and lies in the interval (0.02, 0.30).
4. **Stability screen** — Chow at 2008Q3 is not rejected at the 1 per
   cent level, *and* λ is sign-stable across at least 3 of the 4 sample
   variants (full, pre-COVID, COVID-dropped, COVID rich-dummies).

Two screens warrant comment for the LIVES interaction specifications
(Specs 8–12). The cointegration screen is reported as "NA" for these
forms in `australia_cointegration.csv`: with up to ten regressors
(`coint_n_vars` of 10, 9, 5, 9 and 3 for Specs 8–12 respectively) the
static long-run regression that the Engle–Granger test requires is not
well posed, and the test is not run. And the upper bound of the
speed-of-adjustment screen (|λ| < 0.30) is binding for the
strongest-adjusting forms: Specs 8 and 11 both adjust faster than the
0.30 ceiling on the full sample (|λ| = 0.445 and 0.480), and so are
flagged as failing the |λ| screen even though their λ is correctly
signed, strongly significant, and sign-stable across all four sample
variants. We return to this in §6.3 and §7: the full-sample λ for the
faithful LIVES form is inflated by the COVID quarters, and the
identified value is the pre-COVID estimate, λ = −0.245 (t = −4.80),
which lies comfortably inside the screen interval and within roughly 15
per cent of Williams' −0.286.

### 6.3 Selector outcome

Under the canonical `PI_METHOD = 'italy'` setting, the automated
four-screen selector returns **Spec 2** (aggregate log net worth plus a
short-run CCI term) as the only specification that passes the sign,
speed-of-adjustment and stability screens with a finite BIC — and it is
not the LIVES form. The full screen card, taken directly from
`australia_spec_selection.csv`, is:

| Spec | Signs | Coint | λ | Stability | BIC |
|---|:-:|:-:|:-:|:-:|---:|
| 1                            | ✓ | ✗ | ✓ | ✗ | −919.2 |
| **2** (selector-best)        | **✓** | ✗ | **✓** | **✓** | **−500.8** |
| 3                            | ✓ | ✗ | ✓ | ✗ | −919.8 |
| 4                            | ✗ | ✗ | ✓ | ✗ | −906.8 |
| 5                            | ✗ | ✗ | ✓ | ✓ | −493.0 |
| 6 (conventional baseline)    | ✗ | ✗ | ✓ | ✗ | −493.8 |
| 6b                           | ✗ | ✗ | ✓ | ✗ | −1116.3 |
| 7                            | ✗ | ✗ | ✗ | ✗ | −499.8 |
| 7b                           | ✗ | ✗ | ✗ | ✗ | −363.0 |
| 8                            | ✓ | NA | ✗ | ✗ | −948.5 |
| 9                            | ✗ | NA | ✓ | ✗ | −895.9 |
| 10                           | ✓ | NA | ✓ | ✗ | −493.7 |
| **11** (faithful LIVES, headline) | **✓** | NA | ✗ | ✗ | **−945.0** |
| 12                           | ✗ | NA | ✓ | ✗ | −892.5 |

Source: `australia_spec_selection.csv`; BIC values are Schwarz from
`australia_all_diagnostics.csv`. Spec 6b carries the lowest (best) BIC
only because it is fitted on the longest, n = 190 back-extended sample
and so is not directly comparable to the n = 146 or n = 86 forms.

Several patterns emerge, and we read them as a coherent statement about
the limits of single-equation identification rather than as a clean
verdict.

**No specification clears the cointegration screen.** Evaluated against
MacKinnon critical values keyed to the regressor count
(`coint_adf_5pct_cv` ranging from −4.42 to −6.13 across the estimable
forms), no single-equation specification rejects the no-cointegration
null. The disaggregated forms come closest — Specs 4–6 reach
ADF ≈ −3.2 on the long-run residual (Spec 4/5: −3.23 against −5.23;
Spec 6: −3.22 against −5.47) and the back-extended Spec 6b reaches
−3.77 against −5.47 — but none crosses its critical value. The
aggregated Specs 1–3 fall far short (ADF ≈ −0.56 to −1.13). Phillips–
Ouliaris likewise fails for Specs 1–3, while single-equation Johansen
returns r = 1 for every estimable form. A static single-equation long
run between consumption and its wealth/income determinants is therefore
not formally established on this sample. This is one of the paper's
recurring themes (§5, §7.3, §9): the long-run identification Williams
obtains comes from his cross-equation FIML system, not from any single
equation. Because the screen no longer discriminates, selection falls
to the remaining three screens with the BIC tiebreak.

**The sign screen passes for both the faithful LIVES form and Spec 8.**
Spec 11 passes the sign screen — its long-run coefficients with
unambiguous priors (`nla_y`, `ilfa_y`, `ha_x_cci`, `ln_yp_over_y`) are
all correctly signed, with `nla_y = +0.037` (t = 3.40), `ilfa_y =
+0.018` (t = 3.03) and `ln_yp_over_y = +0.504` (t = 4.00) significant
and right-signed on the full sample, and `ha_x_cci = +0.0049`
(t = 1.03) right-signed though insignificant. Spec 8, the six-interaction
free form, also passes the sign screen at the aggregate level. By
contrast the disaggregated Specs 4–6 *fail* the sign screen under the
canonical Italy LP forecaster: because |λ| rises markedly relative
to the AR forecaster, the implied γ on each wealth component shrinks and
small negative coefficients on individual components — `eq_y` in
particular (Spec 6 full: `eq_y = −0.0063`, wrong-signed) — tip the
sign screen. We read this as an identification artefact of the
constant-MPC disaggregation, not as a substantive reversal of the
illiquid-financial channel, and it is precisely the artefact that the
faithful LIVES form removes by combining the illiquid components into
`ilfa_y` (which is then correctly signed and significant).

**The faithful LIVES form fails the |λ| upper-bound screen on the full
sample.** Spec 11's full-sample λ = −0.480 (t = −3.59) exceeds the 0.30
ceiling, as does Spec 8's λ = −0.445 (t = −3.30). Both are correctly
signed, strongly significant and sign-stable across all four sample
variants. The mechanical screen failure is therefore not a sign or
significance failure; it reflects the full-sample λ being inflated by
the COVID quarters. On the pre-COVID sample Spec 11's λ = −0.245
(t = −4.80) lies inside the screen interval and within roughly 15 per
cent of Williams' −0.286 — which is why §7 treats the pre-COVID estimate
as the identified value.

**The calibration-imposed forms pass the |λ| screen for the wrong
reason.** Specs 10 and 12 pass the speed-of-adjustment screen on the
full sample, but only because their λ has *collapsed* toward zero
(Spec 10: −0.048, t = −0.76; Spec 12: −0.029, t = −0.68), placing it
just inside the lower edge of the (0.02, 0.30) interval. Both fail the
stability screen — their λ is not sign-stable across samples
(`lambda_sign_stable_across_samples = FALSE` in
`australia_lambda_robustness.csv`; Spec 12's pre-COVID λ flips to
+0.030, Spec 10's COVID-dropped λ flips to +0.005). A screen that is
satisfied by a near-zero, sign-unstable adjustment speed is exactly the
case the stability screen exists to catch. The economic content of this
collapse — that imposing Williams' Australian calibrations wrecks the
equilibrium — is developed in §7.0.1 and §9.

**No single criterion selects the headline.** The automated screen
returns Spec 2, a non-LIVES aggregate-net-worth form; BIC over the
comparable n = 146 specifications favours Spec 8 (−948.5, the best
finite-sample BIC), with the faithful Spec 11 close behind (−945.0);
and LIVES theory favours Spec 11. These three criteria do not agree.
We treat this **selector divergence as a documented limitation, not a
result to be argued away.** It is the single-equation counterpart of
the identification problem that runs through the paper: the six CCI
interactions are 0.74–0.97 mutually collinear (§5), the long run does
not cointegrate in a single equation, and the data are too imprecise
for any mechanical rule to isolate the structural LIVES form. We do
*not* promote Spec 8's fast adjustment (λ = −0.445) as a CCI success;
its interaction coefficients are wrong-signed or insignificant (§7) and
it re-allocates identification across the interaction block rather than
sharpening any single channel.

Accordingly, the body of this paper leads with the **faithful LIVES
specification (Spec 11)** on theoretical-form grounds — it is the form
Williams (2010) and the LIVES tradition adopt, it passes the sign
screen, and (on the identified pre-COVID sample) it recovers Williams'
error-correction speed. We retain **Spec 6 as the conventional
constant-MPC baseline** — not the headline or the preferred
specification — because it is the form prior work treated as LIVES and
because it permits the γ_LA + γ_LOANS = 0 net-liquid restriction test
(§8). We carry **Spec 12 (and its independent reproduction Spec 10) as
the negative control** that shows the structure transfers but the
calibrations do not, and **Spec 2 as the automated selector-best**
comparator. The full per-specification coefficient vectors, with
Newey–West HAC standard errors and now including the Spec 11 and
Spec 12 columns, are in Appendix B.

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
interactions are de-meaned over the post-1979 sample, following Williams.

Full-sample (1988Q4–2024Q4, n = 146, adj-R² = 0.81) long-run coefficients
([`australia_all_results.csv`](../outputs/australia_all_results.csv), Spec 11
rows; diagnostics from
[`australia_all_diagnostics.csv`](../outputs/australia_all_diagnostics.csv)):

| Term | OLS coef | t-stat | Implied γ (= OLS/\|λ\|) | Williams |
|---|---:|---:|---:|---:|
| `ha_x_cci` (γ₁, housing × CCI) | +0.0049 | +1.03 | 0.010 | 0.049 |
| `nla_y` (γ_NLA) | +0.0370 | +3.39 | 0.077 | 0.159 |
| `ilfa_y` (γ_IFA) | +0.0178 | +3.03 | 0.037 | 0.022 |
| `cci_williams` (ζ_c) | +0.0046 | +0.47 | — | 0.190 |
| `hp_x_1_minus_cci` (α₄) | +0.0142 | +2.69 | — | −0.130 |
| `r_x_cci` (α₁) | +0.0027 | +3.57 | — | −0.871 |
| `ln_yp_over_y` (ψ₀) | +0.504 | +4.00 | — | 0.20 |
| `yp_x_cci` (ψ₁) | −0.590 | −1.58 | — | 0.93 |
| **`ecm_lag` (λ)** | **−0.480** | **−3.59** | (= 1) | −0.286 |

The contrast with Spec 6 (§7.1) is stark. **The error-correction and core
wealth structure come alive.** The speed of adjustment is λ = −0.480
(t = −3.59) on the full sample and **λ = −0.245 (t = −4.8) pre-COVID —
almost exactly Williams' −0.286** — against Spec 6's insignificant −0.180
(t = −1.76); the full-sample value is inflated by the COVID quarters and
fails the upper-bound |λ| screen, so we read the pre-COVID estimate as the
identified one. The net-liquid and illiquid-financial m.p.c.s are now
individually significant and correctly signed on the full sample (implied γ_NLA = 0.077,
γ_IFA = 0.037, the latter close to Williams' 0.022; both become individually insignificant on the smaller pre-COVID
sample, Table 11.1), and the permanent-income response is strong (t = 4.0). Because the CCI-spline
interactions replace Spec 6's 2002Q3-binding `cci_ratio` short-run term,
the model estimates on n = 146 rather than n = 86 — a near-doubling of the
identification window. Reading a standalone, insignificant `ha_y` in Spec 6
as a failed housing wealth effect was therefore a category error: the
theory predicts that coefficient to be ≈ 0, and the housing effect lives in
the credit interaction.

The honest qualifier is that **the credit-conditions interactions
themselves remain weakly identified.** The housing-collateral term
`ha_x_cci` (γ₁) is correctly signed but insignificant (t = 1.03; implied
m.p.c. 0.010 against Williams' 0.049), the `ζ_c` intercept is correctly
signed but insignificant, and the real-rate and affordability interactions
carry the wrong sign and significance on the full sample but flip to the
correct sign pre-COVID (e.g. ψ₁ on `yp_x_cci` is −0.59 full-sample but
+0.13 pre-COVID). This is the signature of two compounding problems
documented in §5 and below: the six CCI-interacted regressors are 0.74–0.97
mutually correlated (each ≈ proportional to the latent index), and the
1980s–1990s financial-liberalisation episode that identifies the credit
channels largely predates the 1988Q3 household-balance-sheet data.

### 7.0.1 The calibration route does not transfer (Spec 12, Spec 10)

Because the interactions cannot be freely identified, the natural
single-equation response is Williams' own: calibrate the credit channels
and estimate only what the data can support. Spec 12 imposes Williams'
scale-robust calibrations (γ_IFA = 0.022, ψ₀ = 0.20, ψ₁ = 0.93) via an
iterative fixed-point offset and frees only the housing-collateral m.p.c.,
the net-liquid m.p.c. and λ. (His real-rate, affordability and intercept
loadings cannot be imposed at their published magnitudes: the repo's
percent-scaled real rate against a unit-normalised CCI makes α₁ = −0.871
roughly thirty times too large, diverging the fixed point.)

The result is decisive and negative: **imposing Williams' permanent-income
calibration collapses the error-correction to λ ≈ 0** (Spec 12:
λ = −0.029, t = −0.68; independently reproduced by the pre-existing Spec 10,
λ = −0.048, which keeps the rate and affordability channels free). The
mechanism is that the Australian data freely estimates ψ₀ ≈ 0.50 — about
two-and-a-half times Williams' 0.20 — so forcing his value injects a large,
mis-signed contribution that destroys the equilibrium. The LIVES *structure*
transfers; Williams' Australian *calibrations* do not. This sits with, and
explains, the companion paper's Wald result that the joint calibration is
*not rejected* (χ² = 2.24, p = 0.90): the freely estimated coefficients are
too imprecise to reject Williams' values, but imposing them still ruins the
fit — low power is not the same as good fit. Sharpening the credit channels
therefore requires either the four-equation FIML system (whose
cross-equation restrictions supply the identifying variation the single
equation lacks) or a pre-1988 back-extension that recovers the
financial-liberalisation episode — not a single-equation calibration.

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

Over the full 1988Q4–2024Q4 sample Spec 6 fits on **n = 86** after lag
truncation. The binding constraint is `cci_ratio` from ABS Cat 5601.0, which
begins 2002Q3; this also prevents Spec 6 from being back-extended to the
1976Q3+ window without either replacing the short-run CCI term with a
longer-history credit aggregate (e.g. Δ²log of `credit_total_d02`) or setting
it to zero pre-2002. We retain the 2002Q3+ binding constraint here and report
the back-extension exercise on the simpler Spec 4 in §7.3 and §8.15.

The long-run coefficients of Spec 6 under canonical Italy LP are
([`australia_all_results.csv`](../outputs/australia_all_results.csv), Spec 6
rows):

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

The picture is the mirror image of Spec 11. In summary:

- **Speed of adjustment.** λ = −0.180 (NW SE 0.103), t = −1.76,
  p = 0.084 — only borderline significant, about 63 per cent of Williams'
  published −0.286. The error-correction does not come decisively to life in
  this form.
- **Housing wealth.** OLS coefficient +0.0088, t = 1.52 — statistically
  indistinguishable from zero as a *standalone* level. This is the
  coefficient the theory predicts to be ≈ 0 absent the CCI interaction;
  reading it as a failed housing wealth effect is the category error §7.0
  identifies. The implied structural γ_HA = 0.049 is numerically close to
  Williams' 0.0488, but §7.3.1 shows that closeness is a non-rejection driven
  by imprecision, not confirmation.
- **Net liquid assets.** OLS +0.0354 (t = 0.96, insignificant), implied
  γ_NLA = 0.196 — about 23 per cent above Williams' 0.159, in the same
  direction and order of magnitude. The γ_LA + γ_LOANS = 0 cross-equation
  restriction is accepted at the 5 per cent level (§8.5).
- **Illiquid financial wealth.** Decomposed into equities (γ = −0.035,
  wrong-signed but t = −0.13, statistically indistinguishable from
  zero) and superannuation (γ = +0.065, t = 1.48); the combined
  γ_IFA = 0.030 sits above Williams' calibrated 0.022. The negative
  point estimate on equities is a small-sample identification
  artefact of the disaggregated split: combining the two illiquid components
  into a single `ilfa_y` ratio (as Spec 11 does, §7.0) restores a positive,
  significant coefficient (+0.018, t = 3.03).
- **House-price affordability.** OLS −0.0170, implied γ = −0.094;
  Spec 6 does not include the affordability × (1 − ϖ·CCI) interaction that
  Williams' framework uses to identify this channel, so the level coefficient
  is not a like-for-like comparison.
- **Real mortgage rate.** OLS −0.00018, insignificant in the level; the
  credit-conditions-contingent rate effect Williams models enters only
  through the `r × CCI` interaction, which Spec 6 omits.
- **Permanent income.** Base coefficient +0.200 (SE 0.263, t = 0.76), plus a
  post-2008 break of +0.236 (SE 0.203, t = 1.16) — neither individually
  significant. The base coefficient is close to Williams' calibrated +0.20 in
  sign and broad magnitude.
- **Diagnostics.** adj-R² = 0.81, Durbin–Watson 2.18, AR(1) p = 0.26,
  AR(4) p = 0.14 (no serial correlation), RESET p < 0.01
  (functional-form misspecification remains), heteroskedasticity
  structural (NW HAC SEs throughout). λ is sign-stable across all four
  sample variants (full −0.180, pre-COVID −0.123).

The key reading is comparative, not absolute: Spec 6 reproduces a *plausible
profile of point estimates* but at conventional significance levels its
error-correction and wealth channels are not individually identified. Spec 11
recovers the same theory on the same data with significant, correctly signed
core channels — the difference is the functional form, not the sample, the
data vintage, or the estimator.

### 7.2 Diagnostics summary

Diagnostic results for all fourteen specifications (full sample) are
summarised below; full per-spec output is in
[`australia_all_diagnostics.csv`](../outputs/australia_all_diagnostics.csv).

| Spec | n | adj R² | DW | AR(1) | AR(4) | Het | RESET | BIC |
|---|---:|---:|---:|:-:|:-:|:-:|:-:|---:|
| 1 (LogNetWorth)        | 146 | 0.731 | 2.34 | rej | rej | struct | rej | −919.2 |
| 2 (LogNetWorth_CCI)    | 86  | 0.769 | 2.40 | rej | OK  | struct | rej | −500.8 |
| 3 (LevelNetWorth)      | 146 | 0.732 | 2.35 | rej | rej | struct | rej | −919.8 |
| 4 (Disagg_NoCCI)       | 146 | 0.729 | 2.39 | rej | rej | struct | rej | −906.8 |
| 5 (FullDisagg)         | 86  | 0.798 | 2.31 | rej | rej | struct | rej | −493.0 |
| 6 (Preferred/baseline) | 86  | 0.807 | 2.18 | OK  | OK  | struct | rej | −493.8 |
| 6b (LongHistSRCCI)     | 180 | 0.707 | 2.11 | OK  | OK  | struct | rej | −1116.3 |
| 7 (CohortBurden)       | 86  | 0.833 | 2.20 | OK  | rej | struct | rej | −499.8 |
| 7b (RBABurden)         | 64  | 0.869 | 2.16 | OK  | rej | struct | rej | −363.0 |
| 8 (CCI_Interactions)   | 146 | 0.821 | 1.87 | OK  | rej | struct | rej | −948.5 |
| 9 (KalmanCCI)          | 146 | 0.737 | 2.20 | OK  | rej | struct | rej | −895.9 |
| 10 (WilliamsPrior)     | 86  | 0.778 | 2.17 | OK  | OK  | struct | OK  | −493.7 |
| **11 (LIVES_Headline)**| 146 | 0.812 | 1.80 | OK  | OK  | struct | rej | −945.0 |
| 12 (LIVES_Calibrated)  | 146 | 0.686 | 2.09 | OK  | rej | struct | rej | −892.5 |

(Table regenerated from `australia_all_diagnostics.csv`; "OK"/"rej"
are the 5 per cent verdicts of the Breusch–Godfrey AR(1)/AR(4) and
RESET tests, and "Het" is the `het_diagnosis` field. AR(4) is borderline for
Spec 11, p = 0.055.)

Four diagnostic patterns are worth noting. First, **heteroskedasticity
is structural in every full-sample specification** — the Breusch–Pagan
rejection survives dropping the four event quarters in every case — so
the Newey–West HAC standard errors used throughout are necessary rather
than a precaution. Second, **RESET rejects in every specification bar
the Williams-prior Spec 10** (p = 0.054), indicating functional-form
misspecification that the single-equation framing does not fully resolve
(consistent with §5.3/§7.3/§9); the faithful Spec 11 is not exempt
(RESET p = 0.006). Third, **low-order serial correlation is concentrated
in the aggregated and disaggregated-no-CCI forms**: the Breusch–Godfrey
AR(1) test rejects for Specs 1–5 but not for the baseline Spec 6, the
faithful Spec 11, or the CCI-augmented Specs 6b–10/12, so the credit-spline
and disaggregated dynamics absorb the serial dependence the aggregated
specifications leave in the residual. Fourth, **the headline LIVES form
(Spec 11) carries the best BIC among the n = 146 specifications bar Spec 8**
(−945.0 vs −948.5): the form correction is not bought at the cost of fit, but
neither does it dominate the BIC screen, which favours the over-parameterised
Spec 8 — one strand of the selector divergence documented in §6.

### 7.3 Comparison with Williams (2010, 2012): the conventional baseline

The faithful comparison with Williams — where the LIVES structure recovers
his error-correction speed and core wealth m.p.c.s — is in §7.0 and is taken
up systematically in §9. Here we present the comparison for the
*conventional baseline* Spec 6, because that comparison is what an earlier
draft (and much of the literature) would have offered as the headline, and
because it exhibits a numerical coincidence that the form-is-decisive thesis
must explain rather than celebrate.

We compare Spec 6 to Williams' published Table 1 estimates from the
BIS chapter (Muellbauer and Williams 2012). Williams reports
**structural** long-run coefficients γ; our OLS coefficients relate
to those γ by the ECM identity OLS_coef = λ × γ, so a difference in
the implied γ can come from either the OLS coefficient or λ.
Reporting both forms separates the two channels
([`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv)):

| Term | Williams γ | Williams implied OLS | Our OLS | Our γ | OLS gap | γ gap |
|---|---:|---:|---:|---:|---:|---:|
| **λ**                       | **−0.2860** | (same)  | **−0.1801** | (same)  | **−37 %** | (same) |
| Housing wealth `ha_y`       | 0.0488     | 0.0140  | 0.0088     | 0.0491  | −37 %    | +1 %   |
| Illiquid `eq_y + super_y`   | 0.0220     | 0.0063  | 0.0054     | 0.0300  | −14 %    | +36 %  |
| Net liquid `nla_y`          | 0.1590     | 0.0455  | 0.0354     | 0.1963  | −22 %    | +23 %  |
| log(HP/y)                   | −0.1300    | −0.0372 | −0.0170    | −0.0943 | —        | —      |
| ψ at CCI = 0                | 0.2000     | 0.0572  | 0.1999     | 1.1097  | —        | —      |

Mechanically, our OLS coefficients on the disaggregated wealth components are
14–37 per cent below Williams' implied OLS values, while our |λ| is 37 per
cent below his. Because the OLS gap and the λ gap have the same sign, the two
deficits **partly cancel under the ECM identity** γ = OLS/|λ|: the implied
structural γ on housing wealth comes out at 0.049, numerically all but equal
to Williams' 0.049, and the implied γ on net liquid and illiquid financial
wealth sit modestly *above* Williams in the same direction.

We treat this cancellation as a **property of the conventional baseline, not
as evidence**, for two reasons. First, the closeness is fragile: it is a
ratio of two estimates each measured imprecisely on n = 86, and §7.3.1 shows
the confidence interval around γ_HA spans [−0.05, 0.13] — wide enough to
contain both Williams' value *and* zero. Second, and more fundamentally,
Spec 6 is not the LIVES equation: its housing channel is a standalone level
that the theory predicts to be ≈ 0, so a coincidental match between an
artefactual γ_HA and Williams' credit-unlocked m.p.c. carries no structural
content. The faithful Spec 11 makes the same housing channel explicit
(γ₁ = 0.010, t = 1.03) — right-signed, weakly identified, and *not* matching
Williams numerically — which is the honest read on what the single-equation
Australian data support. The §9 comparison is therefore built on Spec 11, not
on this baseline coincidence.

A natural follow-up question is whether the simpler disaggregated
no-CCI specification (Spec 4) — closer in form to Williams' Table 1 long-run
cointegrating regression than Spec 6 is — would align with Williams' Table 1
on a longer sample. Refitting Spec 4 on the back-extended 1976Q3+ sample
using the disaggregated wealth proxies (§3.13;
[`spec46_extended_comparison.csv`](../outputs/spec46_extended_comparison.csv)):

| LR coefficient | 1988+ baseline (n=146) | 1976+ extended (n=190) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.140 | −0.193 | −0.286 |
| nla_y       | +0.035 | −0.002 | +0.159 |
| eq_y        | −0.119 | −0.104 | +0.022‡ |
| super_y     | +0.040 | +0.024 | (incl.)‡ |
| ha_y        | +0.068 | +0.040 | +0.0488 |

‡ Williams reports a single illiquid-financial-asset MPC (γ_IFA = 0.022), shown here against the combined eq_y + super_y. Williams' structural MPCs (γ_HA = 0.0488, γ_NLA = 0.159, γ_IFA = 0.022) are the BIS-chapter values in [`australia_williams_comparison.csv`](../outputs/australia_williams_comparison.csv), the same source used in §7.3's headline comparison and §11.4.

On Spec 4 the speed of adjustment moves 37 per cent closer to
Williams (−0.140 → −0.193, still 32 per cent short of −0.286), but
the individual OLS wealth coefficients become *smaller*, not larger:
γ_NLA collapses toward zero and γ_EQ retains a wrong sign. The
back-extension therefore does not push the Spec 4 estimates closer
to Williams' Table 1; the post-1988 sample window is not, in
itself, what generates the divergence between Spec 4 and Williams'
values. **Sample length is not the binding constraint.**

Reading the two exercises together points to the same conclusion as §7.0.1:
the wedge between any single-equation OLS estimate and Williams' system FIML
is the single-equation framing itself, not sample length, knot count, or
sign-prior structure. The placebo evidence (§5.2) and the two-equation SUR
result (§5.4) — in which joint estimation delivers a negligible
cross-equation residual correlation and no efficiency gain — corroborate
this reading.

We do *not* present Spec 8 (the free six-interaction form, λ = −0.445) as a
CCI success here. Adding the full Williams CCI interaction set re-allocates
the long-run identification across wealth components (γ_HA = 0.022,
γ_IFA = 0.053, γ_NLA = 0.098) without moving systematically toward Williams'
Table 1, and three of its six interaction coefficients carry the wrong sign
against their priors (§5.5, §8.4). The magnitude of its λ reflects the
collinear interaction set absorbing variation, not a sharpening of the credit
channel.

### 7.3.1 How precisely is the structural profile identified?

The implied structural coefficients γ_i = β_i/|λ| are ratios of two
imprecisely estimated quantities on n = 86, so they inherit wide
sampling uncertainty that the point-estimate comparison above conceals.
Computing delta-method standard errors from the Newey–West covariance
of (β_i, λ) — which carries the correlation between each numerator and
the speed of adjustment — gives
([`australia_gamma_inference.csv`](../outputs/australia_gamma_inference.csv)):

| Term | Implied γ | 95% CI (delta) | Williams Table 1 | Williams in CI? |
|---|---:|---:|---:|:-:|
| Housing `ha_y`           | 0.042  | [−0.050, 0.133] | 0.0488 | ✓ |
| Net liquid `nla_y`       | 0.150  | [−0.317, 0.617] | 0.159  | ✓ |
| Equities `eq_y`          | −0.044 | [−0.506, 0.419] | 0.011  | ✓ |
| Super `super_y`          | 0.047  | [−0.027, 0.121] | 0.011  | ✓ |
| log(HP/y)                | −0.091 | [−0.447, 0.265] | −0.130 | ✓ |
| Permanent income         | 1.237  | [−0.337, 2.811] | 0.200  | ✓ |
| **Wealth aggregate (Σ)** | **0.194** | **[−0.768, 1.157]** | 0.230 | ✓ |

A moving-block residual bootstrap (block length 8, B = 1000) gives a
near-identical picture, with the bootstrap median γ_HA at 0.038 and a 95 per
cent interval [−0.45, 0.26] that includes zero (consistent with the standalone
`ha_y` being statistically indistinguishable from zero in Spec 6).

Two things follow, and they discipline the §7.3 reading. First, **Williams'
Table 1 value lies inside the 95 per cent confidence interval for every
coefficient** — so the data are statistically consistent with his
calibrations, the same conclusion the formal Wald test reaches (§9;
companion χ² = 2.24, p = 0.90). Second, and equally true, the intervals are
so wide that they also contain zero (for the aggregate wealth effect, for
equities, and for housing at the lower bound) and values far from Williams.
The close numerical agreement of the baseline point estimates is therefore a
**non-rejection driven by imprecision, not positive evidence** that the
Australian structural coefficients *equal* Williams': on this sample the
single-equation estimates cannot distinguish his values from a broad range of
alternatives, including zero. This is the honest ceiling on the §7.3
comparison — consistency, not confirmation — and it is exactly why §7.0
leads with the faithful Spec 11 form (where the *significant* channels are
NLA and IFA, not the credit-scaled housing m.p.c.) rather than with the
baseline γ-profile coincidence.

For use in a calibrated model (MARTIN, §10.3), the policy-relevant
summary is the aggregate long-run wealth elasticity γ_W = 0.19
(95% CI [−0.77, 1.16]); the point estimate is close to MARTIN's
calibrated net-wealth elasticity of ≈0.17, but the interval is too wide
for the free estimate to discipline the calibration — it neither
confirms nor rejects it. The disaggregated split into four components
is even less precisely identified and should be treated as indicative.

(Caveat: both the delta method and the residual bootstrap hold the
right-hand side fixed, so they propagate sampling uncertainty in the
ECM coefficients but **not** the first-stage uncertainty in the
generated permanent-income and CCI regressors; the true intervals are
therefore at least this wide. The real-time permanent-income
sensitivity of §7.4 and §8.9 gives a partial read on the first-stage
component.)

### 7.4 The Italy / AR comparison and the real-time check

The permanent-income measure matters materially for two coefficients —
the speed of adjustment and the long-run permanent-income coefficient.
Refitting Spec 6 (n = 86) under each measure on a common data flow
([`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv)):

| Term | AR (real-time) | Italy full-sample (headline measure) | Italy real-time | Williams |
|---|---:|---:|---:|---:|
| `ecm_lag` (λ)   | −0.051 | **−0.197** | **−0.118** | **−0.286** |
| `ln_yp_over_y`  | −0.222 | **+0.244** | **−0.105** | (calib. 0.20) |
| implied γ_HA    | 0.019  | 0.0082     | 0.0118     | 0.049 |

(Common-refit values from `australia_pi_realtime_robustness.csv`; the
canonical pipeline's Spec 6 λ under the full-sample Italy measure is −0.180,
marginally different from the −0.197 refit here because the diagnostic
re-derives the data flow rather than reusing the cached pipeline outputs. The
`ha_y` row reports the OLS coefficient, which equals the implied γ only after
dividing by |λ|; we report the OLS coefficient here, consistent with the CSV.)

Two readings follow. First, the full-sample Italy measure roughly
quadruples |λ| relative to AR (−0.05 → −0.20) and flips the long-run
permanent-income coefficient from significantly negative (−0.22, t = −2.23 —
the "Australian permanent-income puzzle") to positive (+0.24, in sign
agreement with theory and Williams' calibrated value, though insignificant at
t = 0.95). Second — the operational caveat — **neither move fully survives a
causal real-time projection.** The real-time Italy variant keeps about half
the |λ| gain (−0.118) but the permanent-income coefficient returns to
modestly negative (−0.105, t = −1.53), and the `ha_y` coefficient sits at
+0.012 (t = 1.86) rather than at the full-sample +0.008. The positive-PI sign
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
Muellbauer and Rondinelli (2020) on the conventional constant-MPC baseline (Spec 6)
and, where the committed batteries permit, on the automated-selector
specification (Spec 2) and the faithful LIVES specification (Spec 11).
The suite is deliberately weighted toward exposing rather than
concealing weakness: it spans estimator choice (OLS, IV, SUR), break
testing, the credit-conditions construction, the permanent-income
measure, the affordability adjustment, sample length, and
out-of-sample forecast accuracy. Several of the batteries return
honest negatives — the out-of-sample loss to a random walk, the
near-zero cross-equation residual correlation, the placebo failure,
and the partial reversal under a causal permanent-income measure.
We report these prominently. They are substantive findings: a
freely-estimated single-equation framework permits them to surface,
whereas the imposed restrictions of a four-equation FIML system would
hide them, and each points to why FIML and pre-1988 back-extension
are the routes forward (§5, §9).

### 8.1 OLS vs IV on current income (Hall 1978 endogeneity)

Current income is instrumented by lagged income, lagged unemployment
and the lagged mortgage rate (six instruments;
[`australia_iv_robustness.csv`](../outputs/australia_iv_robustness.csv),
Spec 2, n = 86). The speed of adjustment is essentially unmoved:
ecm_lag = −0.0829 (OLS) → −0.0852 (IV), a change of −0.0023 (+2.82
per cent). The net-worth elasticity shifts by −4.81 per cent
(0.0615 → 0.0585) and the permanent-income coefficient by +85 per
cent (−0.0031 → −0.0057), but both are economically negligible in
level terms. Current-income endogeneity is therefore not a material
source of bias for the speed of adjustment on this sample.

### 8.2 Joint permanent-income + consumption SUR

Estimating the consumption equation jointly with the permanent-income
equation by SUR
([`australia_joint_pi_robustness.csv`](../outputs/australia_joint_pi_robustness.csv),
Spec 2, n = 86) leaves every coefficient within sampling noise of the
single-equation OLS values. The largest level shift is in the speed
of adjustment (ecm_lag −0.0829 → −0.1273, +53.6 per cent in relative
but a third of a percentage point in level), and the net-worth
elasticity moves +13.1 per cent (0.0615 → 0.0695). The
cross-equation residual correlation is negligible; the SUR-versus-OLS
coefficient stability is the load-bearing evidence (the literal
ρ̂ ≈ −0.004 figure cited in the system narrative is not a separately
committed scalar — we report the coefficient comparison rather than
the correlation). Single-equation OLS is therefore an acceptable
estimator for the consumption block, and — consistent with the
two-equation SUR result in §8.18 — the case for the multi-equation
framework rests on cross-equation parameter restrictions, not on
residual covariance or efficiency gain.

### 8.3 Chow battery and multiple-break tests

Chow tests
([`australia_chow_battery.csv`](../outputs/australia_chow_battery.csv),
Spec 2) at 2008Q3 do not reject parameter stability (Chow stat
1.248, p = 0.270; n_pre = 21, n_post = 65), while the 2020Q1 break is
strongly rejected (Chow stat 8.852, p = 9.9 × 10⁻¹⁰; n_pre = 67,
n_post = 19) — the COVID structural break that the event dummies
absorb. The 1995Q1 and 2000Q1 break dates fall too close to the
sample edge to estimate. The Bai–Perron / CUSUM battery
([`australia_breaks.csv`](../outputs/australia_breaks.csv)) returns a
single dominant break: supF = 164.20 (p = 0) at 2020Q4, with a CUSUM
p-value of 0.9963 (recursive residuals stable away from the COVID
episode). The break structure is therefore concentrated entirely at
COVID, not at the GFC.

### 8.4 Williams CCI interactions (Spec 8) — reallocation, not identification

Spec 8 enters all six Williams CCI interactions freely on the 1988Q4+
sample with the reduced-form `cci_williams`. Following Williams (Aust
paper §5.1) the variables interacted with CCI are de-meaned over the
estimation sample before forming the interaction, so that each
interaction term has a clean conditional interpretation rather than
absorbing an implicit linear CCI level shift. The sign-prior verdicts
([`australia_spec8_sign_prior_verdicts.csv`](../outputs/australia_spec8_sign_prior_verdicts.csv);
full vector in
[`australia_all_results.csv`](../outputs/australia_all_results.csv),
n = 146) are:

| Williams interaction | Sign prior | OLS coef | t | p | Verdict |
|---|---:|---:|---:|---:|---|
| `r × CCI`                     | − | +0.00192 | +1.85 | 0.066 | wrong sign, marginal — **FAIL** |
| `log(HP/y) × (1 − 1.2·CCI)`   | − | +0.00765 | +1.00 | 0.319 | wrong sign on composite, insignificant — **FAIL** |
| `log(y^p/y) × CCI`            | + | −0.6113  | −2.12 | 0.036 | wrong sign, significant — **FAIL** |
| `log(y^p/y)`                  | + | +0.4677  | +3.44 | 0.001 | right sign — **PASS** |
| `HA × CCI` (γ₁)               | + | +0.00159 | +0.32 | 0.748 | right sign, insignificant |

Three of the four sign-priced interactions fail. Of the six free
interactions only the permanent-income level term (`ln_yp_over_y`,
+0.4677, t = 3.44) passes its prior cleanly; the housing-collateral
interaction `ha_x_cci` carries its theoretically correct positive
sign but is far from significant (+0.00159, t = 0.32). Against
Williams' Table 1 the raw-coefficient gaps remain structural
([`australia_williams_spec8_comparison.csv`](../outputs/australia_williams_spec8_comparison.csv)):
his α_c1 (r × CCI) = −0.871 against our structural +0.0043; his
α_c4 (HP/y × (1−1.2·CCI)) = −0.13 against our structural +0.017; his
calibrated ψ₁ = +0.93 against our freely-estimated structural −1.374.

The substantive effect of Spec 8 is to *re-allocate* the long-run
identification, not to recover Williams' channels. Standalone wealth
strengthens (nla_y +0.0435***, struct +0.098; super_y +0.0139**,
struct +0.031; ha_y +0.0097**, struct +0.022) and the speed of
adjustment shifts from −0.180 (Spec 6) to −0.445 (t = −3.30) on the
full sample — past Williams' value in magnitude. Spec 8 also attains
the highest adjusted R² among the n = 146 specifications (0.8215) and
the best BIC overall (−948.5). We do **not** read any of this as a
credit-conditions success. The pre-COVID estimates are revealing:
there the wealth coefficients sign-correctly (ha_y +0.0139***) but
the *de-meaned interaction* `ha_x_cci` turns significantly negative
(−0.0114**, struct −0.046), the wrong sign for the LIVES collateral
channel, and `hp_x_1_minus_cci` is significantly negative
(−0.0194***). Read together with the calibration collapse (§9, Spec
10/12) and the identification-vs-detrending decomposition (§5), the
honest reading is that the six CCI-interacted regressors in a single
equation act as flexible parameter time-variation rather than as the
structurally identified common-factor channel that Williams'
four-equation system delivers. Their mutual collinearity (each is
approximately proportional to CCI) is the structural reason FIML is
required; we discuss it as a first-class identification result in §5.

### 8.5 Net-liquid-assets restriction γ_LA + γ_LOANS = 0

We refit each disaggregated specification with deposits/y and debt/y
entered separately and conduct a Wald test of
H₀ : γ_LA + γ_LOANS = 0 using `car::linearHypothesis` with the
Newey–West variance estimator. The restriction is **accepted at the
5 per cent level in every specification × sample combination**
([`australia_nla_restriction_test.csv`](../outputs/australia_nla_restriction_test.csv)):

| Spec | Sample | γ_LA + γ_LOANS | NW SE | t | p | Restriction |
|---|---|---:|---:|---:|---:|:-:|
| 4 | full      | +0.0800 | 0.0559 | 1.430 | 0.153 | accepted |
| 5 | full      | +0.1377 | 0.1134 | 1.214 | 0.225 | accepted |
| 6 | full      | +0.1045 | 0.1028 | 1.017 | 0.309 | accepted |
| 4 | pre-COVID | +0.0398 | 0.0269 | 1.480 | 0.139 | accepted |
| 5 | pre-COVID | −0.0356 | 0.0494 | −0.721 | 0.471 | accepted |
| 6 | pre-COVID | −0.0201 | 0.0532 | −0.378 | 0.706 | accepted |

The data cannot distinguish separate liquid-asset and debt
propensities. We read this as non-rejection-by-imprecision rather
than as positive confirmation of exact netting, but it validates the
Italian convention of netting deposits against debt and supports the
use of the constructed `nla_y` series — the net-liquid channel that
carries a significant, correctly-signed marginal propensity in the
faithful LIVES specification (Spec 11, nla_y +0.037***, struct 0.077;
§7).

### 8.6 Drehmann amortising-mortgage adjusted real rate

De Bonis et al. (2020) apply the BIS Drehmann, Juselius and Korinek
(2017) amortisation-adjusted rate adjR = R / (1 − (1+R)⁻ᴺ). For
Australia we set N = 25 years (100 quarters), consistent with the
longer Australian average mortgage maturity
([`australia_drehmann_robustness.csv`](../outputs/australia_drehmann_robustness.csv),
Spec 2, n = 86). The maturity choice materially changes the
estimated speed of adjustment: ecm_lag −0.1928 (base) → −0.0866
(Drehmann), with the net-worth elasticity moving 0.0013 → 0.0612 and
the permanent-income coefficient 0.2404 → −0.0032. Unlike the
Italian result, the consumption equation is *not* invariant to the
amortisation adjustment on the Australian sample; we flag the CCI/rate
maturity convention as a genuine source of sensitivity, consistent
with the more general finding that the credit-channel calibrations are
weakly pinned down off a single equation.

### 8.7 Scaled-income robustness

De Bonis et al. (2020) average disposable income with
labour-plus-transfer income to down-weight property-income
mismeasurement. Re-running this construction
([`australia_scaled_income_robustness.csv`](../outputs/australia_scaled_income_robustness.csv),
Spec 2, n = 86) shifts the speed of adjustment from −0.1928 to
−0.1117 (SE 0.0466) and lifts the net-worth elasticity from 0.0013 to
0.0569. The income-measure choice moves |λ| by roughly 0.08 — not
negligible — but does not change the substantive ranking of the
wealth coefficients.

### 8.8 Williams non-property income (NPY) robustness

Replacing the disposable-income series with `npy_real_pc` constructed
per Williams (2009) §4.2.1
([`australia_williams_income_robustness.csv`](../outputs/australia_williams_income_robustness.csv),
Spec 2, n = 86) provides the closest methodological match to
Williams' income concept (property income stripped, but not
symmetrically averaged with labour-plus-transfer income). The
substitution shifts ecm_lag from −0.1928 to −0.0938 (SE 0.0431) and
lifts the net-worth elasticity from 0.0013 to 0.0572. This moves |λ|
by roughly 0.10 under the Williams-NPY measure relative to the
baseline net-worth specification, in the same direction as — and
somewhat larger than — the scaled-income result.

### 8.9 Permanent-income method comparison (AR, full-sample Italy, real-time Italy)

§7.4 reports the headline three-way comparison; the committed columns
([`australia_pi_method_comparison.csv`](../outputs/australia_pi_method_comparison.csv)
and
[`australia_pi_realtime_robustness.csv`](../outputs/australia_pi_realtime_robustness.csv),
n = 86) refit under all three permanent-income measures:

| PI measure | λ (ecm_lag) | t | log(y^p/y) | t | adj-R² |
|---|---:|---:|---:|---:|---:|
| AR forecaster (full)       | −0.0829 | — | −0.0031 (puzzle) | — | 0.7578 |
| AR (real-time)             | −0.0511 | −0.89 | −0.2224 | −2.23 | — |
| Italy LP (full-sample)     | −0.1974 | −1.95 | +0.2442 | +0.95 | 0.7690 |
| Italy LP (real-time)       | −0.1182 | −1.86 | −0.1051 | −1.53 | — |

(adjusted R² from
[`australia_pi_method_meta.csv`](../outputs/australia_pi_method_meta.csv);
the Italy LP forecaster fits marginally better, 0.7690 vs 0.7578.)

The full-sample Italy LP measure is the headline measure (framed as a
*measurement*, §4.3), and the AR forecaster delivers the
significantly negative "Australian permanent-income puzzle"
coefficient (−0.0031 full, −0.2224 under the real-time AR variant).
The real-time Italy LP variant is the operationally honest benchmark:
it is causal, re-fitting the projection at each *t* on data whose
full k-quarter horizon is realised by *t*, so it is usable at
forecast time. It shows that about half the speed-of-adjustment gain
from AR to the full-sample Italy measure is genuine (λ falls from
−0.197 to −0.118 once look-ahead is removed), but that the positive
permanent-income sign is **not** — it reverses to −0.105 under the
causal measure. We carry the full-sample measure as the headline and
disclose explicitly that its positive-PI sign and λ magnitude are
full-sample, non-causal properties. The same look-ahead caveat
attaches to the strong permanent-income coefficient in the faithful
LIVES specification (Spec 11, ln_yp_over_y +0.504, t = 4.0); the
real-time column is the operational robustness check.

### 8.10 Permanent-income filter sensitivity

A nine-cell grid over discount factor δ ∈ {0.90, 0.95, 0.97} and
horizon k ∈ {20, 40, 60} quarters, with the GFC learning-weight on
and off
([`australia_permanent_income_sensitivity.csv`](../outputs/australia_permanent_income_sensitivity.csv)),
shows the speed of adjustment to be extremely stable within the AR
method: λ ranges only from −0.0824 to −0.0830 across the eighteen
δ × k × GFC combinations (baseline δ = 0.95, k = 40, the eta = 0.05
mapping of §4.3: λ = −0.0829, structural PI weight −0.037,
structural main-wealth +0.742). The GFC learning weight has no effect
on these summaries. Switching to an HP-filter permanent income
(λ = 1600) moves λ to −0.2006 and flips the structural PI weight to
+1.397. The within-AR-method PI tuning is therefore not what drives
the |λ| gap with Williams; the dominant factor is the AR-vs-Italy-LP
method choice itself (§8.9).

### 8.11 COVID-period robustness

Across four sample variants (full, pre-COVID, COVID-dropped,
COVID-rich) the speed of adjustment is sign-stable for every
estimable specification except the two Williams-calibration-imposed
specifications
([`australia_lambda_robustness.csv`](../outputs/australia_lambda_robustness.csv)).
For the conventional baseline (Spec 6) λ runs −0.180 (full) /
−0.123 (pre-COVID) / −0.100 (COVID-dropped) / −0.149 (COVID-rich) —
all correctly signed. The faithful LIVES specification (Spec 11)
gives −0.480 (full) / −0.245 (pre-COVID) / −0.256 (COVID-dropped) —
also sign-stable, with the full-sample value inflated by the COVID
quarters and the pre-COVID −0.245 treated as the identified value
(close to Williams' −0.286). Only Spec 10 (Williams-prior calibrated;
sign-flip to +0.005 COVID-dropped) and Spec 12 (Williams calibrations
imposed; sign-flip to +0.030 pre-COVID) are not sign-stable across
samples — an artefact of the calibration collapse documented in §9,
not of the COVID episode itself.

### 8.12 Rolling-window estimation

A 60-quarter rolling estimation of the baseline specification shows
the wealth coefficients trending mildly downward post-2014
(consistent with the macroprudential era flattening the
wealth-consumption transmission) and λ becoming slightly less
negative in the most recent windows. We do not interpret this as
model instability but as a symptom of the limited identifying
variation in the post-deregulation portion of the sample (§5): the
financial-liberalisation episode that would identify the credit
channels largely predates the 1988Q3 start of ABS sectoral
balance-sheet data.

### 8.13 Out-of-sample forecast validation

We run a rolling out-of-sample validation
([`australia_oos_rmse.csv`](../outputs/australia_oos_rmse.csv)) on
five structural specifications (Spec 4 disagg-no-CCI, Spec 6
conventional baseline, Spec 7 cohort-burden, Spec 8 Williams-CCI
interactions, Spec 9 Kalman-CCI) over 36 expanding-window cuts at
horizons h ∈ {1, 4, 8} quarters (n = 36 at h = 1, 4; n = 32 at h = 8),
against random-walk-with-drift and AR(1) benchmarks:

| Specification | h = 1 RMSE | h = 4 RMSE | h = 8 RMSE |
|---|---:|---:|---:|
| Benchmark RW drift           | 0.03094 | 0.03094 | 0.03282 |
| Benchmark AR(1)              | 0.03703 | 0.03102 | 0.03283 |
| Spec 4 (disagg, no CCI)      | 0.03191 | 0.03252 | 0.05025 |
| Spec 6 (conventional baseline)| 0.03218 | 0.03323 | 0.04163 |
| Spec 7 (cohort-burden)       | 0.03080 | 0.03461 | 0.04428 |
| Spec 8 (Williams CCI)        | 0.03242 | 0.03151 | 0.03657 |
| Spec 9 (Kalman CCI)          | 0.03203 | 0.03222 | 0.03854 |

At h = 1 the structural specifications are competitive with the
random-walk benchmark and Spec 7 narrowly beats RW-drift (0.03080 vs
0.03094); no other structural spec does. At h = 4 and h = 8 the
random walk with drift dominates **every** structural specification
(the best structural performer at h = 8 is Spec 8 at 0.03657 vs
RW-drift 0.03282). This is the standard macro-forecasting puzzle: the
LIVES framework's identification advantage is in interpreting
historical co-movement, not in beating naive benchmarks at multi-step
prediction. We record it honestly rather than overstating forecast
performance.

Two construction caveats attach to the CCI specifications (Spec 8,
Spec 9). The credit-conditions series and its de-mean constants are
full-sample objects, so the Spec 8/9 columns are conditional on a
full-sample-constructed CCI and are an upper bound on what a fully
real-time credit-conditions forecaster would deliver; the
permanent-income input to the validator is the leak-free rolling AR
forecaster.

### 8.14 Back-extension robustness — Spec 1 on the 1976Q3+ sample

Refitting Spec 1 (aggregate net worth) on the back-extended sample
using `ln_networth_y_proxy`
([`spec1_extended_comparison.csv`](../outputs/spec1_extended_comparison.csv)):

| LR coefficient | 1988+ baseline (n = 146) | 1976+ extended (n = 190) | % change |
|---|---:|---:|---:|
| λ (ecm_lag)    | −0.1772  | −0.2021  | +14.0 |
| ln_networth_y  | +0.1119  | +0.1062  | −5.1  |
| ln_hp_over_y   | −0.0151  | −0.0033  | −78.1 |
| real_rate      | −0.00137 | +0.00086 | −163  |
| ln_yp_over_y   | +0.9612  | +0.9732  | +1.2  |

The wealth elasticity is essentially stable across samples
(0.112 → 0.106, a 5 per cent change) — a positive validation of the
aggregate-net-worth proxy: doubling the sample length and adding the
deregulation-era regime does not shift the structural
wealth-to-consumption coefficient. The permanent-income elasticity is
also stable, and λ moves slightly more negative on the longer sample.
The house-price-to-income coefficient collapses to near zero on the
longer sample — a real signal reflecting lower `hp_over_y` variation
in the pre-deregulation 1970s — and the real-rate coefficient
sign-flips, though both estimates are economically negligible. The
aggregate net-worth proxy correctly includes the household M3 liquid
component (the earlier `$bn`/`$m` unit defect that had made the M3
term numerically inert is fixed and propagated through a reproducible
cold rebuild; see Appendix D).

### 8.15 Spec 4 on the back-extended sample

The disaggregated-wealth proxies of §3 allow Spec 4 to fit on the
back-extended sample
([`spec46_extended_comparison.csv`](../outputs/spec46_extended_comparison.csv)):
λ moves 37 per cent closer to Williams (−0.1404 → −0.1927, still
about a third short of −0.286), but the individual OLS wealth
coefficients become *smaller* rather than larger — nla_y collapses
(+0.035 → −0.002, a sign-flip), ha_y falls 41.6 per cent
(0.068 → 0.040) and super_y falls 41.7 per cent (0.040 → 0.024). The
exercise establishes that sample length is **not** the binding
constraint on whether the disaggregated single-equation form
reproduces Williams' Table 1: the longer sample sharpens the speed of
adjustment but blunts, rather than sharpens, the individual wealth
channels.

### 8.15.1 Spec 6b — conventional baseline on the back-extended sample

Spec 6 binds at 2002Q3+ on the baseline sample because `cci_ratio`
(ABS Cat 5601.0 housing-loan flow) begins there. Spec 6b retains the
Spec 6 long-run and short-run structure but replaces the short-run
CCI regressor with the second difference of log RBA D02 total credit
(available from 1976Q3) and switches the wealth components to their
back-extended proxies. This lets the conventional baseline fit on the
full back-extended sample (n = 190 in the cointegration screen;
n = 180 full / 160 pre-COVID in the diagnostics)
([`australia_all_results.csv`](../outputs/australia_all_results.csv);
[`australia_all_diagnostics.csv`](../outputs/australia_all_diagnostics.csv)):

| LR coefficient | Spec 6 (n = 86) | Spec 6b (n = 180) | Williams Table 1 |
|---|---:|---:|---:|
| λ (ecm_lag)              | −0.180 (t = −1.76) | **−0.229 (t = −4.18)** | −0.286 |
| ha_y / ha_y_proxy γ      | 0.049              | 0.038                  | 0.049  |
| nla_y / nla_y_proxy γ    | 0.196              | 0.013                  | 0.159  |
| eq_y / eq_y_proxy γ      | −0.035             | −0.081                 | (calibrated 0.011) |
| super_y / super_y_proxy γ| 0.065              | 0.029                  | (calibrated 0.011) |
| ln_hp_over_y γ           | −0.094             | −0.036                 | −0.130 |
| ln_yp_over_y (CCI = 0)   | +0.200             | +1.234                 | +0.20 (calibrated) |
| BIC                      | −493.8             | −1 116.3               | n/a    |

Two patterns recur. First, the speed of adjustment moves substantially
closer to Williams' published value — λ = −0.229 on the back-extended
sample (80 per cent of Williams' −0.286, vs 63 per cent on Spec 6),
with the t-statistic improving sharply from −1.76 to −4.18, and the
pre-COVID estimate similar at −0.240 (t = −4.75). Second, the wealth
γ profile shifts toward smaller individual elasticities — γ_NLA
collapses from 0.196 to 0.013 and γ_EQ becomes more negative,
mirroring the Spec 4 back-extension finding. γ_HA stays positive
(0.038 vs Williams' 0.049) but sits about 22 per cent below Williams.
This is consistent with the substantive reading throughout the paper:
on the back-extended sample the disaggregated wealth proxies do not
separately identify with Williams-like precision, even when the
canonical short-run dynamics, the post-2008 permanent-income break,
and a long-history credit proxy are all available. Sample length
sharpens |λ| but not the individual wealth γ profile — the residual
gap is structural to the single-equation framing, not a sample-length
artefact.

### 8.16 Maximal-GETS placebo on the back-extended sample

We place the deployed institutional CCI against 200 random 15-knot,
15-prior draws under the same sign-survival protocol on the
back-extended 1976Q3+ sample. The committed verdict files report
three nested placebo variants
([`australia_williams_knot_placebo_verdict.csv`](../outputs/australia_williams_knot_placebo_verdict.csv),
[`australia_williams_knot_placebo_extended_summary.csv`](../outputs/australia_williams_knot_placebo_extended_summary.csv),
[`australia_williams_knot_placebo_maximal_extended_summary.csv`](../outputs/australia_williams_knot_placebo_maximal_extended_summary.csv)):

| Placebo variant | Williams/canonical adj-R² | adj-R² percentile | \|λ\| percentile | Verdict |
|---|---:|---:|---:|---|
| Standard (Williams literal 4-knot)    | 0.7209 | 34th | 58th | "detrending critique vindicated — below median" |
| Extended (Williams-canonical)         | 0.6789 | 18th | 10th | "detrending critique persists — below median" |
| Maximal-GETS (deployed institutional) | 0.6852 | 64th | 37th | "weak support — above median but not far" |

The deployed/institutional CCI sits in the **18th–34th adjusted-R²
percentile** band across the literal and Williams-canonical variants,
and the maximal-GETS protocol lifts this to the 64th adjusted-R²
percentile but only the 37th |λ| percentile (placebo medians:
adj-R² 0.6832, |λ| 0.2226, surviving knots 8; canonical surviving
knots 7). Random combinations of 15 candidate knots and priors
produce faster mean reversion than the canonical institutional choice
in 64 per cent of draws. The honest reading is that the maximal-GETS
protocol does some identification work, but most of the lift comes
from the adaptiveness of the drop-on-violation reduction (15 candidate
knots is a great deal of flexibility) rather than from Williams'
specific knot or prior choice. We carry this placebo failure as a
core negative result, not as an embarrassment: it is the empirical
expression of why single-equation CCI is weakly identified and why
the joint system is needed for regime classification (§10).

### 8.17 Sectional sign-prior CCI

Williams (Aust paper §5.1) imposes sign priors over periods rather
than knot by knot. We construct a sectional CCI basis with one knot
per period (1982 / 1990 / 1993 / 2007, plus 2014 / 2017 / 2020 / 2021
extensions) and re-run the placebo. The sectional canonical sits at
the 36th adjusted-R² percentile and 40th |λ| percentile — worse than
the maximal-GETS canonical (64th/37th). Williams' specific period
dating does not outperform random period placements on the
back-extended sample.

### 8.18 Two-equation SUR (consumption + house prices)

Joint SUR estimation of the consumption equation and a Williams-style
house-price ECM (Aust paper eq. 11) on the back-extended 1976Q3+
sample yields a residual correlation ρ̂(ε_C, ε_H) ≈ −0.004, with SUR
coefficients within 0.1 per cent of equation-by-equation OLS for
nearly every term. The finding is robust across specification variants
(no CCI: ρ̂ ≈ −0.083; no event dummies: ρ̂ ≈ +0.043; minimal LR + SR:
ρ̂ ≈ −0.025). Joint estimation gives no efficiency gain at the
quarterly frequency. The case for the multi-equation framework
therefore rests on cross-equation parameter restrictions, not on
residual covariance — the same conclusion as the consumption + PI SUR
of §8.2.

### 8.19 Three-equation joint cross-equation CCI identification

We extend the maximal-GETS protocol to require sign-prior survival
across **three** equations simultaneously (consumption + house prices
+ mortgage stock). Of the seven knots that survive consumption-only
fitting, only **two** pass the joint test:

| Survival regime | Surviving knots |
|---|---|
| Consumption only  | 1979, 1986, 1992, 2007, 2009, 2017, 2020 |
| Joint (C ∩ H ∩ M) | **1986, 2017** |

The joint-identified `cci_williams_joint` flips the house-price
equation's CCI loading from significantly negative (−0.024 under
consumption-only CCI) to significantly positive (+0.024 under joint
CCI), consistent with Williams' cross-equation sign restrictions
working as intended. The mortgage-stock equation's CCI loading
remains negative (joint sign-survival is a sign restriction, not a
parameter-equality restriction; full FIML would be required to
re-sign the mortgage equation's loading). The wealth-coefficient
profile against Williams' Table 1 is barely affected by joint
identification, confirming that the residual gap is structural to the
single-equation framing, not a CCI-construction artefact — and that
the route to sharpening the credit channels runs through the
four-equation FIML system, not through further single-equation tuning.

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

On the pre-COVID sample the faithful LIVES specification (Spec 11)
estimates a speed of adjustment of **λ = −0.245 (t = −4.80)** against
Williams' FIML estimate of **−0.286 (SE 0.083, t = −3.45)** — close to
his value (about 86 per cent of it), and tightly identified
(`australia_lambda_robustness.csv`; `australia_all_results.csv`).
The full-sample estimate is λ = −0.480 (t = −3.59); it is inflated by
the COVID quarters and fails the |λ| upper-bound screen, so we read
the pre-COVID figure as the identified one (§7.0). Either way the
contrast with the conventional baseline is sharp: Spec 6 returns an
insignificant λ = −0.180 (t = −1.76), 37 per cent below Williams.

Within the faithful form the wealth structure is correctly signed and,
for two of the three components, individually significant on the full
sample (`australia_all_results.csv`):

- net liquid assets (γ_NLA): OLS +0.037 (t = 3.40, ***), implied
  structural m.p.c. **0.077**, against Williams' calibrated 0.159 —
  same sign, same order of magnitude, roughly half his value;
- illiquid financial assets (γ_IFA, equities + superannuation
  combined): OLS +0.018 (t = 3.03, ***), implied structural m.p.c.
  **0.037**, against Williams' calibrated **0.022** — same sign, within
  0.015;
- housing-collateral (γ₁, the `CCI·(HA/4y)` interaction): OLS +0.0049
  (t = 1.03), implied structural m.p.c. **0.010**, against Williams'
  peak housing m.p.c. of 0.0488 — correctly signed but insignificant.

The permanent-income response is strong and correctly signed
(`ln_yp_over_y` OLS +0.504, t = 4.0). And critically, because the
CCI-spline interactions replace Spec 6's 2002Q3-binding `cci_ratio`
short-run term, the faithful form estimates on **n = 146** rather than
n = 86 — a near-doubling of the identification window relative to the
baseline against which the previous draft compared Williams.

The single most important interpretive point for the comparison is
that **the housing channel only appears when the form is faithful.**
In the LIVES theory there is no classical housing wealth effect: the
housing m.p.c. is zero at CCI = 0 and is unlocked as credit conditions
loosen. Reading Spec 6's insignificant standalone `ha_y` (OLS +0.0088,
t = 1.52) as a *failed* housing wealth effect was therefore a category
error — the theory predicts that coefficient to be ≈ 0, and the
housing effect lives in the credit interaction `ha_x_cci`, where it is
correctly signed.

### 9.2 Where the calibrations do not transfer — imposing Williams' gearing collapses the equilibrium

The opposite result holds for Williams' Australian *calibrations*. The
natural single-equation response to weakly identified interactions is
Williams' own — calibrate the credit channels and estimate only what
the data support — but the data reject that route decisively.

Spec 12 imposes Williams' scale-robust calibrations (γ_IFA = 0.022,
ψ₀ = 0.20, ψ₁ = 0.93) via an iterative fixed-point offset and frees
only the housing-collateral m.p.c., the net-liquid m.p.c. and λ. The
result is a near-total collapse of the error-correction mechanism:
**λ = −0.029 (t = −0.68)** on the full sample, and it flips to the
wrong sign (+0.030) pre-COVID, so it is not even sign-stable across
samples (`australia_lambda_robustness.csv`). This is independently
reproduced by the pre-existing Williams-prior specification (Spec 10),
which keeps the rate and affordability channels free and still returns
λ = −0.048 (t = −0.76) full-sample and a still-insignificant −0.027
(t = −0.62) pre-COVID, flipping to the wrong sign once the COVID
quarters are dropped (`australia_lambda_robustness.csv`). Two separate
calibration routes therefore reach the same conclusion.

The mechanism is straightforward: the Australian data freely estimates
the permanent-income weight at ψ₀ ≈ 0.50 — roughly two-and-a-half
times Williams' 0.20 — so forcing his value injects a large,
mis-signed contribution that destroys the long-run equilibrium. (The
"≈ 2.5×" framing is a comparison of Spec 11's level coefficient
`ln_yp_over_y` = +0.504 with Williams' calibrated ψ₀ = 0.20; the exact
ψ₀/ψ₁ split is not separately estimable on a single equation because
`yp_x_cci` is insignificant, so we report it as an order-of-magnitude
contrast rather than a precise ratio.)

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

The companion paper reports that a joint Wald test does **not reject**
Williams' calibration (χ² = 2.24, p = 0.90). Read alongside §9.2 this
looks paradoxical: how can a calibration that *collapses* the
equilibrium when imposed also be *not rejected* by a test? The
resolution is power. The freely estimated single-equation coefficients
are so imprecise that they cannot statistically reject Williams'
values — but the same imprecision means the data also cannot reject
zero, or any number of other points. **Low power is not the same as
good fit.** A calibration can be non-rejected by an underpowered test
and still wreck the model when imposed; both facts hold here
simultaneously, and the gamma-inference confidence intervals (§9.4)
make the imprecision explicit.

(The χ² = 2.24 figure is reported in the companion paper; it is not a
value in this repository's committed results CSVs. The load-bearing
committed evidence for the non-rejection-by-imprecision reading is the
gamma-inference interval table `australia_gamma_inference.csv`, in
which every Williams value lies inside our 95 per cent CI *and* every
CI also contains zero.)

### 9.4 The structural-gamma comparison: consistency, not confirmation

The earlier draft's headline — that the implied structural γ profile
"matches Williams almost exactly" — was a property of the conventional
baseline (Spec 6), and it survives only as a much-weakened, hedged
statement. Two features demote it from confirmation to consistency.

First, the apparent agreement rests on offsetting deficits. Under the
ECM identity OLS = λ × γ, Spec 6's OLS wealth coefficients run
14–37 per cent below Williams' implied OLS values while its |λ| runs
37 per cent below Williams'; the two deficits roughly cancel, leaving
the implied γ close to Williams (`australia_williams_comparison.csv`).
That cancellation is a numerical coincidence of the baseline, not
evidence that the structure has been identified. We therefore no
longer present it as a positive result.

Second, and decisively, every component's confidence interval is wide
enough to contain *both* Williams' value and zero
(`australia_gamma_inference.csv`):

| Term | OLS | Implied γ | 95% CI | Bootstrap 95% CI | Williams | In CI? |
|---|---:|---:|---:|---:|---:|:-:|
| `ha_y` (housing) | +0.0082 | 0.042 | [−0.050, 0.133] | [−0.451, 0.255] | 0.0488 | ✓ |
| `nla_y` (net liquid) | +0.0295 | 0.150 | [−0.317, 0.617] | [−0.940, 0.656] | 0.159 | ✓ |
| `eq_y` (equities) | −0.0086 | −0.044 | [−0.506, 0.419] | [−0.821, 0.578] | 0.011 | ✓ |
| `super_y` (super) | +0.0092 | 0.047 | [−0.027, 0.121] | [−0.062, 0.210] | 0.011 | ✓ |
| `ln_hp_over_y` | −0.0179 | −0.091 | [−0.447, 0.265] | [−0.718, 1.220] | −0.130 | ✓ |
| `ln_yp_over_y` | +0.2442 | 1.237 | [−0.337, 2.811] | [−2.065, 4.813] | 0.20 | ✓ |
| `WEALTH_AGG` | +0.0384 | 0.194 | [−0.768, 1.157] | — | 0.230 | ✓ |

The honest summary is therefore: **every Williams value lies inside
our 95 per cent confidence interval — but so does zero.** This is
non-rejection driven by imprecision, the same low-power phenomenon
that produces the companion Wald result. The n = 86 baseline sample is
small and the intervals are correspondingly wide; point-estimate
proximity to Williams is consistency, not confirmation.

### 9.5 Adding the CCI interactions does not close the gap (Spec 8)

Estimating the six CCI interactions freely (Spec 8) raises the speed
of adjustment to λ = −0.445 (t = −3.30) full-sample on n = 146, in
magnitude above Williams' −0.286. We do not present this as a
credit-channel success. The wealth coefficients shift relative to the
baseline (γ_NLA → 0.098, γ_IFA → 0.053, housing-collateral → 0.004)
without moving systematically toward Williams' Table 1, and three of
the four interaction sign priors fail outright
(`australia_spec8_sign_prior_verdicts.csv`):

- `r_x_cci` (Williams α_c1 = −0.871 at CCI = 1): prior negative, OLS
  +0.0019, p = 0.066 → **FAIL** (wrong sign);
- `hp_x_1_minus_cci` (Williams α_c4 = −0.13 at CCI = 0): prior
  negative, OLS +0.0077, p = 0.319 → **FAIL**;
- `yp_x_cci` (Williams ψ₁ = +0.93): prior positive, OLS −0.611,
  p = 0.036 → **FAIL** (wrong sign);
- `ln_yp_over_y` (Williams ψ₀ ≈ 0.20): prior small positive, OLS
  +0.468, p = 0.0008 → **PASS**.

The raw-coefficient comparison confirms that the interactions do not
land where Williams' framework predicts
(`australia_williams_spec8_comparison.csv`): his α_c1 = −0.871 against
our structural +0.0043 (t = 1.85), his α_c4 = −0.13 against our
+0.017 (t = 1.00), his calibrated ψ₁ = +0.93 against our freely
estimated −1.37 (t = −2.12). Adding the interactions therefore
**re-allocates** the long-run identification across components — and
raises λ — but does not close the residual gap with the joint FIML
estimates. The earlier draft's framing that Spec 8 "exceeds Williams"
is withdrawn.

### 9.6 The interactions are collinear: the structural reason Williams uses FIML

The reason single-equation estimation cannot deliver Williams' result
is, at root, an identification problem. The six CCI-interacted
regressors are 0.74–0.97 mutually correlated on this sample because
each is approximately proportional to the latent CCI; they cannot be
separately identified off one equation. (The 0.74–0.97 range is a
property of the interaction-regressor matrix reported in §5 and the
analysis narrative; it is not itself a standalone committed CSV. The
load-bearing committed evidence is threefold: the wrong-signed,
insignificant Spec 8 interaction coefficients of §9.5; the
identification-versus-detrending shifts of §5; and the Spec 10/Spec 12
collapse of §9.2.)

This is precisely the structural reason Williams (2010) uses a
four-equation FIML system rather than single-equation OLS. The same
CCI enters all four of his equations with sign constraints; the
affordability multiplier ϖ in the wealth × (1 − ϖ·CCI) interaction is
shared across equations; and ζ_h = 1 normalises the house-price
equation. Those cross-equation restrictions supply the identifying
variation that lets the credit channels be separated — variation that
no single-equation specification has. The joint-survival result of §5
corroborates this from the other direction: when the CCI knots are
required to satisfy sign priors *jointly* across the consumption,
house-price and mortgage equations, only two of the seven
single-equation survivors pass, so the maximal-GETS single-equation
identification was partly equation-specific. This aligns with both
Williams' framework (CCI as a common factor under parameter
restrictions) and the Duca, Muellbauer and Murphy (2013) state-space
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
the back-extended 1976Q3+ sample moves λ 37 per cent closer to
Williams (−0.140 → −0.193, still 32 per cent short of −0.286) — but
the individual wealth coefficients *shrink* rather than grow on the
longer window: γ_NLA collapses toward zero and γ_EQ retains a wrong
sign (§8). Lengthening the sample sharpens the speed of adjustment but
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
full-sample (look-ahead) AR variant (−0.003) and significantly negative
once the AR forecaster is run in real time (−0.22, t = −2.23)
(`australia_pi_method_comparison.csv`,
`australia_pi_realtime_robustness.csv`). Under the full-sample Italy
Jordà local-projection *measure* it is positive (+0.24 on the
net-worth forecaster regression; +0.504 in the faithful Spec 11). The structural reasons the measures
diverge are real — the rolling-AR forecaster lacks the
labour-force-share predictor that captures slow demographic effects,
compounds short-run AR misspecification across 40 horizons, and
over-estimates persistence after large income shocks, all of which the
one-step direct projection avoids. But the positive sign is partly a
property of the full-sample, non-causal construction of the measure:
under a causal real-time projection the coefficient returns to
modestly negative (−0.11, §8). The strong positive permanent-income
response that the faithful LIVES form recovers therefore rests on the
full-sample (look-ahead) permanent-income measure; we flag this
explicitly and direct readers to the real-time robustness column
rather than treating the puzzle as resolved.

### 9.9 Summary

The comparison with Williams resolves into a clean two-part statement.
The LIVES *structure* transfers: the faithful single-equation form
recovers his error-correction speed (pre-COVID λ = −0.245 against
−0.286) and the correctly signed wealth-m.p.c. structure (net-liquid
and illiquid-financial m.p.c.s individually significant;
housing-collateral correctly signed). His Australian *calibrations* do
not transfer: imposing his permanent-income gearing collapses λ to
≈ 0 (Spec 12, Spec 10), because Australia freely estimates ψ₀ ≈ 0.50,
roughly 2.5× his 0.20, and his rate loading cannot even be imposed at
its published scale. The companion Wald non-rejection is reconciled as
low-power non-rejection — every Williams value sits inside a wide CI
that also contains zero, so the structural-γ "agreement" is
consistency, not confirmation. The six CCI interactions are
0.74–0.97 collinear and so weakly identified off a single equation,
which is exactly why Williams' identification comes from cross-equation
FIML restrictions; the back-extension shows sample length is not the
binding constraint. The path to a tighter reconciliation with Williams'
published values therefore runs through a full four-equation FIML build,
not through any single-equation OLS refinement.

---


## 10. Decomposition and policy implications

This section reads the estimated long run and the policy
counterfactuals through the lens of the faithful LIVES specification
(Spec 11) of §7.0, while being explicit about which committed
decompositions are available on which long-run parameterisation. Two
caveats frame everything below. First, the long-run contributions
decomposition committed to the repository
([`australia_longrun_contributions.csv`](../outputs/australia_longrun_contributions.csv))
is built on the *net-worth* (Spec 2-style) long run, not the
disaggregated LIVES bracket of Spec 11; we therefore use it to
characterise the shape of the fitted equilibrium path rather than to
attribute consumption to the credit-scaled housing-collateral channel.
Second, the counterfactuals
([`australia_counterfactuals_summary.csv`](../outputs/australia_counterfactuals_summary.csv))
were generated on the conventional baseline (Spec 6 dummies) and the
interaction spec (Spec 8); we report them as committed but read the
CCI counterfactual through the de-meaning convention rather than as a
structural housing-collateral experiment. The policy reading in
§10.3, by contrast, is anchored on the Spec 11 channels.

### 10.1 Long-run contributions decomposition

The long-run decomposition (an Australian counterpart to Williams
(2010) Charts 2–8) splits fitted de-meaned log(c/y) into the partial
contribution of each long-run regressor over the 86 quarters from
2003Q3 to 2024Q4, the window over which the underlying series
(including the Italy-LP permanent-income measure) are jointly
available. Because the committed decomposition rests on the
net-worth long run, the regressors are aggregate net worth
(`ln_networth_y`), house-price affordability (`ln_hp_over_y`), the
real mortgage rate (`real_rate`) and the permanent-income ratio
(`ln_yp_over_y`); each term is de-meaned and so sums to approximately
zero by construction over the window, and the actual de-meaned path
and the residual are reported alongside
([`australia_longrun_contributions.csv`](../outputs/australia_longrun_contributions.csv)).

At the last observation (2024Q4) the partial contributions are: net
worth −0.002, house-price affordability −0.041, real rate +0.002,
and permanent income +0.041, against an actual de-meaned log(c/y) of
−0.009 and a residual of −0.009. The two large, near-offsetting
wedges at end-of-sample are the negative affordability term
(elevated house prices relative to income depressing the equilibrium
consumption ratio) and the positive permanent-income term, with
aggregate net worth close to its sample mean. The residual is not
negligible: it carries a sizeable, slowly-evolving level — around
+0.10 to +0.18 in the mid-2000s, drifting through zero and turning
negative later in the sample — with the sharpest excursions around
the GFC and, especially, early COVID (−0.24 in 2020Q2). By 2024Q4 it
has returned close to zero (−0.009).

The honest qualifier is that this is a net-worth decomposition, not
the disaggregated LIVES bracket. Under the faithful form (Spec 11)
the housing contribution would enter *only* through the credit-scaled
collateral channel γ₁·CCI·(HA/4y), whose implied structural MPC is
0.010 (right-signed but insignificant, t = 1.03; §7.0), so a
LIVES-consistent decomposition would attribute far less of the
equilibrium path to housing than a standalone-wealth reading would
suggest. We do not present a separate Spec 11 path decomposition
because the credit interaction is weakly identified off a single
equation (§5, §10.3); attributing the fitted path to a γ₁ that is
statistically indistinguishable from zero would overstate the
precision of the channel. The committed net-worth decomposition is
therefore best read as descriptive of the equilibrium *shape* —
affordability and permanent income as the dominant moving wedges —
rather than as a structural attribution to the LIVES collateral
channel.

Source:
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
| No 2014/2017 APRA macroprudential | 2014Q4 | Spec 6 dummies | +0.8 % | +2.3 % | +28.3 % |
| No COVID income support           | 2020Q1 | Spec 6 dummies | −9.6 % | −9.6 % | −9.6 % |
| CCI at peak vs CCI = 0            | 1988Q4 | Spec 8 interactions | n/a | n/a | ≈ 0 |

(Gaps are cumulative deviations in log(c) from the baseline path,
expressed in per cent; h-quarter values are measured from the
relevant event date.)

**Counterfactual 1 — no 2014/2017 APRA macroprudential.** Zeroing
the smoothed-step `d_apra_2014` and `d_apra_2017` dummies implies
that consumption would have been about 0.8 per cent higher four
quarters after the 2014 round (+0.0078 in log points) and about 2.3
per cent higher after eight quarters (+0.0234). The end-of-sample
gap of +28.3 per cent (+0.2828 by 2024Q4) compounds the persistent
post-event ogive shift over a decade and must be read as a
diagnostic upper bound, not a forecast: it assumes nothing else in
the macroeconomic environment would have adjusted over the same
window, which is not how the economy operates. The four- and
eight-quarter figures are the policy-relevant range and are
consistent with the APRA programme accounting for roughly 1–3 per
cent of consumption over its first two years.

**Counterfactual 2 — no COVID income support.** Zeroing the
`d_jobkeeper_2020`, `d2020_covid` and `d2020_rebound` dummies implies
that consumption would have been about 9.6 per cent *lower* in the
COVID period (−0.0962 in log points). The cumulative gap is constant
across horizons because the COVID event dummies are bounded in time
(zero before and after the 2020–21 window), so unlike the persistent
APRA ogive the deviation does not continue to compound past the event
window; the scenario is modelled as a one-off.

**Counterfactual 3 — CCI at peak vs zero.** Evaluating the
interaction spec with the CCI-interacted regressors at CCI = 1
(historical peak) versus CCI = 0 (no liberalisation) implies an
essentially zero cumulative consumption gap across the back-extended
sample (−4.9 × 10⁻¹⁵, i.e. zero to numerical precision). This is a
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
net-liquid-asset MPC is γ₃ = 0.077 (OLS coefficient +0.037, t = 3.40)
and the illiquid-financial-asset MPC is γ₂ = 0.037 (OLS +0.018,
t = 3.03), both as structural propensities recovered as OLS/|λ| on
the full sample (|λ| = 0.480). Housing wealth, by contrast, enters
*only* through the credit-scaled collateral channel: the implied
structural MPC is γ₁ = 0.010 — right-signed but statistically
insignificant (OLS +0.0049, t = 1.03), against Williams' calibrated
peak of 0.0488. The policy reading is therefore asymmetric across
asset classes: liquid and illiquid financial wealth transmit to
consumption with well-identified marginal propensities, whereas the
housing-collateral channel that LIVES theory makes conditional on
credit conditions is, on Australian single-equation post-deregulation
data, of the predicted sign but too imprecise to quantify. Movements
in mortgage rates that change housing values propagate to consumption
with a speed of adjustment we identify on the pre-COVID sample at
λ = −0.245 (t = −4.80) — close to Williams' −0.286 — implying about a
25 per cent cumulative effect at one quarter and most of the
adjustment completed within five to six years. The full-sample λ of
−0.480 is inflated by the COVID quarters and fails the |λ|
upper-bound screen (§6); the pre-COVID value is the policy-relevant
speed.

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
marginally significant (`d_apra_2014` = −0.0143, t = −1.81); the 2017
dummy is small, positive and insignificant (`d_apra_2017` = +0.0079,
t = 1.32), so the macroprudential drag is carried by the first round.
The counterfactual in §10.2 implies they shaved about
0.8 percentage points off consumption in the year after the 2014
round and 2.3 points by two years out. The decade-horizon level gap
is a model-mechanical upper bound, not a forecast of the broader
macro response.

**Permanent-income transmission.** Permanent income is the strongest
channel in the faithful form: the Spec 11 coefficient on log(yᵖ/y) is
+0.50 (t = 4.0) on the full sample and +0.28 (t = 4.7) pre-COVID,
both highly significant and consistent with a structural
permanent-income weight ψ near 0.50. For fiscal-multiplier work this
implies Australian households respond meaningfully and durably to
credible permanent-income shocks. Two caveats apply. The headline
estimate uses the full-sample (look-ahead) Italy-LP permanent-income
measure; under the real-time, no-look-ahead variant the speed of
adjustment shrinks sharply (Italy real-time λ = −0.118 versus
full-sample −0.197) and the permanent-income coefficient flips or
weakens (§7.4, §8), so the real-time column is the
operationally-relevant version for any forward-looking application.
And the freely-estimated ψ ~ 0.50 is roughly 2.5× Williams' calibrated
ψ₀ = 0.20; imposing his value collapses the equilibrium (§7.0.1,
§9), so the Australian permanent-income gearing is a domestic
estimate, not a transferred calibration.

**Credit-conditions identification caveat.** Section 5 documents that
the CCI's identification in a single-equation OLS is weak, and that
neither the back-extended sample, the sectional sign priors, nor the
time-varying housing-wealth interaction changes this. The six
CCI-interacted regressors are very highly mutually correlated on this
sample (pairwise correlations of roughly 0.74–0.97, each
approximately proportional to CCI), so they cannot be separately
identified off one equation — the structural reason Williams uses
four-equation FIML. (This near-collinearity is a sample property
reported in the analysis narrative rather than a standalone committed
correlation matrix; the load-bearing committed evidence is the
wrong-signed and insignificant Spec 8 interaction coefficients and
the Spec 10/12 collapse documented in §7.) Policymakers using a
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
−4.10), echoing the cointegration-screen result of §6.

The reading for MARTIN is that the freely-estimated single-equation
long run does **not** nest MARTIN's calibrated balanced-growth block:
homogeneity would have to be imposed rather than tested-and-accepted,
and the unrestricted wealth elasticity (0.12 here, or 0.19 from the
disaggregated wealth aggregate of §7.3 with a 95 per cent CI of
[−0.77, 1.16]) is too imprecise to discipline MARTIN's 0.17
calibration. The honest position is that this equation is *not yet
MARTIN-ready as a source of point estimates*: the wealth elasticity
CI spans MARTIN's value but also spans zero, so it can corroborate
but not replace the calibration. The MARTIN-operational version of
the permanent-income measure is the real-time Italy-LP forecaster
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
core wealth structure come alive in a single equation. When housing
instead enters as a plain, constant-MPC level term — as in the
conventional disaggregated error-correction model that prior work
implicitly tested — the equilibrium is far weaker and the standalone
housing coefficient is insignificant. Reading that insignificant
standalone coefficient as a failed housing-wealth effect is a
category error: LIVES theory predicts that coefficient is
approximately zero absent the credit-conditions interaction, because
there is no classical housing-wealth channel in the model.

A second, equally central finding is that the LIVES *structure*
transfers to Australia while Williams' Australian *calibrations* do
not. The faithful specification recovers Williams' speed of
adjustment and right-signed wealth structure when its parameters are
freely estimated; imposing his calibrated permanent-income gearing
collapses the equilibrium. We document why — the six credit-conditions
interactions are mutually collinear in the post-deregulation sample —
and we are honest throughout about the limitations of single-equation
LIVES estimation: a credit-conditions placebo that the deployed index
fails, no efficiency gain from joint estimation at the quarterly
frequency, an out-of-sample loss to a random walk, and a look-ahead
permanent-income measure whose result partially reverses under a
real-time construction.

### 11.1 Form is decisive: the faithful LIVES specification

The headline result is the faithful LIVES specification (Spec 11),
in which housing wealth enters *only* through the de-meaned
credit-conditions interaction `ha_x_cci` = CCI × (HA/4y), the
autonomous-consumption loading ζ_c·CCI (`cci_williams`) is restored,
and illiquid financial assets are combined into a single ratio
(`ilfa_y` = equities + superannuation). On this form the
error-correction and core wealth structure are recovered
(australia_all_results.csv; australia_lambda_robustness.csv):

| Quantity | Full sample (n = 146) | Pre-COVID (n = 126) | Williams (2010) |
|---|---:|---:|---:|
| λ (ecm_lag) | −0.480 (t = −3.59) | −0.245 (t = −4.80) | −0.286 |
| NLA m.p.c. (`nla_y`) | +0.037 (t = 3.39)*** | +0.008 (ns) | 0.159 |
| IFA m.p.c. (`ilfa_y`) | +0.018 (t = 3.03)*** | +0.009 (ns) | 0.022 |
| housing-collateral (`ha_x_cci`, γ₁) | +0.0049 (t = 1.03) | +0.0008 (ns) | 0.0488 |
| permanent income (`ln_yp_over_y`) | +0.504 (t = 4.00)*** | +0.279 (t = 4.75)*** | 0.20–0.95 |

The pre-COVID speed of adjustment, λ = −0.245 (t = −4.80), is close
to Williams' (2010) −0.286 and is the value we treat as identified:
the full-sample λ = −0.480 is inflated by the COVID quarters and
fails the upper-bound speed screen. The net-liquid and
illiquid-financial marginal propensities are correctly signed and
significant on the full sample (`nla_y` +0.037, t = 3.39;
`ilfa_y` +0.018, t = 3.03), and permanent income enters strongly
(+0.504, t = 4.00). The housing-collateral term γ₁ is right-signed
but insignificant (`ha_x_cci` +0.0049, t = 1.03). Scaled by
|λ| = 0.480, the implied structural marginal propensities are NLA
0.077, IFA 0.037, and housing-collateral γ₁ 0.010 (right-signed,
insignificant, versus Williams' 0.0488).

By contrast the conventional constant-MPC disaggregated ECM (Spec 6),
which carries plain `ha_y`/`eq_y`/`super_y`/`nla_y` with no credit
scaling and CCI only as a short-run `d2_logcci_lag2` term, delivers a
weaker speed of adjustment (λ = −0.180, t = −1.76) and an
insignificant standalone housing coefficient (`ha_y` +0.0088,
t = 1.52). This is the conventional baseline, not the preferred
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
mechanism collapses to λ = −0.029 (t = −0.68), with the net-liquid
coefficient turning wrong-signed (australia_all_results.csv;
australia_lambda_robustness.csv). Specification 10 (Williams-prior
calibrated) independently reproduces the collapse (λ = −0.048,
t = −0.76). The mechanism is that Australia freely estimates a
permanent-income loading of roughly 0.50 — the Spec 11 full-sample
`ln_yp_over_y` coefficient is +0.504 — well above Williams'
calibrated ψ₀ = 0.20; forcing his lower value removes the equilibrium.
Williams' rate, affordability and autonomous-consumption loadings
cannot even be imposed at their published magnitudes: his raw
α_r = −0.871 is some thirty times too large on the repository's
percent real-rate × unit-normalised CCI scaling, and diverges the
fixed point.

This reconciles the companion paper's Wald non-rejection of the joint
Williams calibration (χ² = 2.24, p ≈ 0.90). The non-rejection is
driven by imprecision, not by good fit: every Williams value lies
inside our wide structural confidence intervals
(australia_gamma_inference.csv) — but so does zero. The free
estimates are too imprecise to reject Williams' values, yet imposing
those values wrecks the fit. Low power is not the same as good fit;
the honest reading is consistency, not confirmation.

### 11.3 Why the credit channels are weakly identified in one equation

The reason the credit-conditions calibrations cannot be sharpened off
a single equation is identification, not sample length. The six
CCI-interacted regressors are each approximately proportional to the
credit-conditions index and are therefore 0.74–0.97 mutually
correlated on the post-1988 sample. They cannot be separately
free-estimated from one equation — the structural reason Williams uses
four-equation FIML. The evidence converges from three directions.
First, when all six interactions are entered freely (Spec 8), the
individual interaction coefficients are wrong-signed or insignificant
against Williams' priors even though the overall fit is the best in
the ladder, and the credit channels do not move toward Williams'
Table 1. Specification 8 reallocates identification; it does not close
the gap, and we do not present its λ = −0.445 as a credit-conditions
success. Second, the calibration collapse of Specs 10 and 12 (§11.2)
shows the channels are not jointly recoverable by imposition either.
Third, the credit-conditions placebo battery shows the deployed index
does no better than randomly placed knots: the literal Williams
four-knot specification sits at the 34th adjusted-R² percentile on
the 1988+ sample and the 18th on the back-extended sample
(australia_williams_knot_placebo_verdict.csv;
australia_williams_knot_placebo_extended_summary.csv), and the
maximal-GETS reduction lifts it only weakly (64th/37th). These are
honest negative results, and they are diagnostic: single-equation
calibration of the credit channels is empirically closed, and a
four-equation FIML build is the route to sharpen them.

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
| λ | −0.140 | −0.193 | −0.286 |
| NLA (γ_NLA) | +0.035 | −0.002 | +0.159 |
| IFA equities (γ_EQ) | −0.119 | −0.104 | — |
| IFA super (γ_super) | +0.040 | +0.024 | — |
| IFA combined (γ_IFA) | −0.079 | −0.080 | +0.022 |
| housing (γ_HA) | +0.068 | +0.040 | +0.049 |

The Williams (2010) reference values are his housing wealth m.p.c.
(0.0488 at the CCI peak), net-liquid m.p.c. (0.159) and calibrated
illiquid-financial m.p.c. (0.022, not split between equities and
superannuation); the back-extension figures are the Spec 4
disaggregated structural coefficients (australia_williams_comparison.csv;
spec46_extended_comparison.csv). The speed of adjustment improves by
37 per cent (−0.140 → −0.193, still short of −0.286), but the
individual wealth coefficients shrink rather than converging on
Williams' values: the net-liquid m.p.c. collapses toward zero, the
equities coefficient retains a wrong sign, and housing falls. Sample
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

- **Credit-conditions placebo.** The deployed index sits in the
  18th–34th adjusted-R² percentile band across placebo variants
  (australia_williams_knot_placebo_verdict.csv; extended and maximal
  summaries) — no better than random knot placement on the relevant
  samples.
- **No system-efficiency gain.** A two-equation SUR of consumption
  and house prices yields negligible cross-equation residual
  correlation, and the SUR coefficient estimates are within sampling
  noise of single-equation OLS (australia_joint_pi_robustness.csv):
  joint estimation buys no efficiency at the quarterly frequency in
  this sample.
- **Out-of-sample loss to a random walk.** At horizons h = 4 and
  h = 8 the random walk with drift beats every structural
  specification (australia_oos_rmse.csv); only one specification
  narrowly beats it at h = 1. The framework's value is structural
  interpretation, not point forecasting.
- **Selector divergence.** No single criterion selects the same
  specification: the automated screen returns a net-worth form
  (Spec 2), BIC favours the free-interaction Spec 8, and LIVES theory
  favours Spec 11 (australia_spec_selection.csv). We retain Spec 11 as
  the narrative-preferred headline on theoretical-form grounds and
  report the divergence as a documented limitation. No single-equation
  form clears the Engle–Granger cointegration screen at the correct
  MacKinnon critical values (australia_cointegration.csv).
- **Look-ahead permanent income.** The headline permanent-income
  result uses the full-sample (non-causal) Italy local-projection
  forecaster. Under a causal real-time construction the speed of
  adjustment shrinks sharply and the permanent-income coefficient
  shrinks or flips (australia_pi_realtime_robustness.csv): the
  positive permanent-income loading is reported as a property of the
  full-sample measure, not a real-time resolution of the Australian
  permanent-income puzzle.

### 11.6 MARTIN nesting and the wealth elasticity

Nesting the long run against the RBA's MARTIN consumption block, the
unrestricted net-wealth elasticity is 0.1155, against MARTIN's
calibrated 0.17 (australia_martin_nesting.csv). Long-run homogeneity
of the combined income-and-wealth term is rejected (χ² = 16.41,
p = 0.0001), and the wealth elasticity is not robustly identified:
imposing homogeneity drives the restricted net-wealth elasticity to
−0.083, so the estimate is too sensitive to the restriction to
discipline MARTIN's calibration. The appropriate reading is qualitative: the
LIVES structure can inform MARTIN's functional form, but the
single-equation point estimates are not precise enough to recalibrate
its parameters.

### 11.7 Contributions

The paper makes five contributions to the Australian household
consumption literature.

1. **The faithful single-equation LIVES estimate (Spec 11) with the
   form correction as the lead methodological result.** Entering
   housing only through its credit-conditions interaction, restoring
   the autonomous-consumption CCI intercept, and combining illiquid
   financial assets recovers a Williams-consistent error-correction
   speed (pre-COVID λ = −0.245 ≈ Williams' −0.286) and right-signed
   wealth structure (NLA +0.037***, IFA +0.018***), where the
   conventional constant-MPC ECM does not. Reading the conventional
   form's insignificant standalone housing coefficient as a failed
   wealth effect is shown to be a category error.

2. **The structure-transfers-but-calibrations-don't finding.**
   Imposing Williams' permanent-income gearing collapses the
   equilibrium (Spec 12: λ = −0.029; independently reproduced by
   Spec 10: λ = −0.048), because Australia freely estimates a higher
   permanent-income loading; this reconciles the companion paper's
   low-power Wald non-rejection as consistency, not confirmation.

3. **The interaction-collinearity diagnosis.** The six CCI-interacted
   regressors are 0.74–0.97 mutually collinear, explaining why the
   credit channels cannot be separately identified off a single
   equation and why Williams' identification requires four-equation
   FIML — corroborated by the Spec 8 reallocation, the calibration
   collapse, and the placebo battery.

4. **A back-extended Australian master dataset to 1976Q3**
   (n = 190–194 quarters) with documented growth-rate splices for
   house prices, M3, total credit and labour force, plus aggregate and
   disaggregated wealth proxies — and the direct test it permits,
   showing that sample length is not the binding constraint.

5. **A structured robustness and placebo suite with honest negative
   results** — the credit-conditions placebo failure, the negligible
   SUR efficiency gain, the out-of-sample loss to a random walk, the
   selector divergence, and the look-ahead permanent-income caveat —
   reported as substantive findings, alongside a multi-equation
   scaffold (data preparation, house-price and mortgage-stock
   equations, joint CCI identification, SURs) on which a companion
   FIML paper can build directly.

### 11.8 Outstanding work

Two routes follow directly from the diagnosis above.

1. **A multi-equation companion paper.** A full LIVES build with FIML
   and cross-equation parameter restrictions — consumption, house
   prices, the mortgage stock and home-equity withdrawal — would
   estimate a single CCI loading across equations under sign
   restrictions, the route the collinearity diagnosis and the placebo
   and SUR results identify as the only way to sharpen the credit
   channels. Custom likelihood code and a sourced HEW series remain
   the binding obstacles.

2. **Pre-1988 extension of the credit channels.** Because the
   financial-liberalisation episode that identifies the
   credit-conditions index largely predates the 1988Q3 start of ABS
   sectoral balance-sheet data, extending the disaggregated
   credit-interaction regressors back into the deregulation window —
   building on the 1976Q3 back-extension — would, in combination with
   FIML, give the credit channels the variation they need.

The single-equation framework cannot, on its own, deliver
identification of the credit channels that Williams (2010) obtains
from joint estimation under parameter restrictions. What it can do —
and what this paper establishes — is that the faithful LIVES *form*
recovers his error-correction and wealth structure where the
conventional constant-MPC ECM cannot, that his Australian
*calibrations* do not transfer, and that the credit interactions are
collinear and weakly identified off a single equation. Reported
honestly, these results point unambiguously to the multi-equation,
back-extended programme as the way forward.


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

A full data-construction appendix — series sources, splice conventions,
deflation, the asset/annualised-income ratio transform ($x_{t-1}/4y$), the
disaggregated wealth definitions and the 1976Q3 back-extension — is
reproduced from the accompanying
[data documentation](../data.md). It retains the detail of §§3.1–3.13 of
the main paper, together with the master variable coverage table.

The three load-bearing constructions for the faithful LIVES specification
(Spec 11) and the conventional baseline (Spec 6) are summarised here for
the reader's convenience; the full provenance is in the data appendix.

**A.1 Disaggregated wealth (ratios to annualised income).** All wealth
terms enter as the prior-quarter stock divided by annualised disposable
income, $x_{t-1}/4y$:

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
future income from the Italy-style Jordà (2005) local-projection
forecaster. The headline measure uses the full-sample (in-sample, hence
look-ahead) forecaster; the real-time variant is reported as an
operational robustness column (§7.4; `australia_pi_realtime_robustness.csv`).
This look-ahead-versus-real-time distinction is flagged wherever a headline
number depends on the non-causal measure.

**A.3 The 1976Q3 back-extension.** Sectoral balance-sheet data from the ABS
begin in 1988Q3, which is why the financial-liberalisation episode that
identifies the credit channels largely *predates* the estimation sample.
A back-extended master dataset to 1976Q3 splices in TRYM house prices,
RBA D03 M3 and RBA D02 credit aggregates, historical labour-force series
and wealth proxies anchored to 1988Q3 levels. This dataset supports the
long-history specification (Spec 6b) and the direct sample-length test
(§8): back-extending Spec 4 moves $\lambda$ about 37 per cent closer to
Williams' value (from $-0.140$ to $-0.193$) but individual wealth
coefficients shrink and the NLA term collapses — sample length is not the
binding constraint on identifying the credit channels.

---

## Appendix B: Coefficient tables

The full per-specification coefficient vectors, with Newey–West HAC
standard errors, $t$-statistics, $p$-values, the implied error-correction
speed $\lambda$ (the `ecm_lag` coefficient) and the implied structural
parameter ($\gamma = \mathrm{OLS}/|\lambda|$), are reproduced in machine
form from the committed results files. The current draft regenerates these
tables from a single consolidated source,
[australia_all_results.csv](../../outputs/australia_all_results.csv) (with
the matched diagnostics in
[australia_all_diagnostics.csv](../../outputs/australia_all_diagnostics.csv)),
which now spans all fourteen specifications — Spec 1 through Spec 12, plus
the long-history Spec 6b and the RBA-burden Spec 7b — for both the full
($n=146$; 1988Q3–2024Q4) and pre-COVID ($n=126$; 1988Q3–2019Q4) windows.
In particular it includes the faithful LIVES headline (Spec 11) and the
Williams-calibration-imposed specification (Spec 12), which were absent
from the earlier draft's tables. The older
[australia_full_results.csv](../../outputs/australia_full_results.csv) and
[australia_precovid_results.csv](../../outputs/australia_precovid_results.csv)
files remain in the repository for the legacy spec set; the consolidated
file supersedes them for the present draft.

Rather than retype the full coefficient matrix, Table B.1 reports the two
specifications that carry the paper's central message — the faithful LIVES
headline (Spec 11) and the Williams-calibration-imposed specification
(Spec 12) — across both samples. The remaining specifications are tabulated
in §6 (the specification ladder) and §7 (results), and in full in Tables B.2–B.3 below and the
committed CSVs.

**Table B.1 — Faithful LIVES (Spec 11) vs Williams-calibration-imposed
(Spec 12): selected coefficients (Newey–West HAC).** Source:
`australia_all_results.csv`, `australia_all_diagnostics.csv`. Convention:
$\lambda=$ `ecm_lag` (negative = error-correction); structural
$\gamma=\mathrm{OLS}/|\lambda|$. *** sig 1%, ** sig 5%, * sig 10%.

| Term | Spec 11 full ($n=146$) | Spec 11 pre-COVID ($n=126$) | Spec 12 full ($n=146$) | Spec 12 pre-COVID ($n=126$) |
|---|---|---|---|---|
| $\lambda$ (`ecm_lag`) | −0.480 (t=−3.59)*** | −0.245 (t=−4.80)*** | −0.029 (t=−0.68) | +0.030 (t=1.49, sign flip) |
| `nla_y` | +0.0370 (t=3.40)*** | +0.0084 (t=0.60) | −0.0014 (t=−0.49, wrong sign) | +0.0024 |
| `ilfa_y` (=eq+super) | +0.0178 (t=3.03)*** | +0.0090 (t=1.40) | imposed (γ=0.022) | imposed (γ=0.022) |
| `ha_x_cci` ($\gamma_1$) | +0.0049 (t=1.03) | +0.0008 (t=0.22) | −0.0011 (t=−0.71) | +0.0012 |
| `hp_x_1_minus_cci` | +0.0142 (t=2.69)*** | −0.0020 (t=−0.29) | — | — |
| `r_x_cci` | +0.0027 (t=3.57)*** | +0.0020 (t=2.78)*** | — | — |
| `cci_williams` ($\zeta_c$) | +0.0046 (t=0.47) | +0.0127 (t=1.62) | — | — |
| `ln_yp_over_y` | +0.5043 (t=4.00)*** | +0.2788 (t=4.75)*** | imposed ($\psi_0$=0.20) | imposed ($\psi_0$=0.20) |
| `yp_x_cci` | −0.5897 (t=−1.58) | +0.1291 (t=1.17) | imposed ($\psi_1$=0.93) | imposed ($\psi_1$=0.93) |
| Intercept | −0.0152 (t=−2.32)** | −0.0126 (t=−1.77)* | +0.0101 (t=2.25)** | +0.0027 (t=1.21) |
| adj-$R^2$ | 0.812 | 0.210 | 0.686 | 0.096 |
| SE (%) | 0.706 | 0.520 | 0.915 | 0.560 |
| BIC (Schwarz) | −945.05 | −895.34 | −892.52 | −898.66 |

Implied structural MPCs for Spec 11 (full sample, $/|\lambda|=0.480$):
NLA 0.077, IFA 0.037, housing-collateral $\gamma_1$ 0.010 (right-signed,
insignificant, $t=1.03$; cf. Williams' calibrated housing MPC 0.0488). The
pre-COVID $\lambda=-0.245$ ($t=-4.80$) is treated as the identified speed
of adjustment — close to Williams' $-0.286$ — because the full-sample
value is inflated by the COVID quarters and fails the $|\lambda|$
upper-bound screen. The headline numbers depend on the full-sample
(look-ahead) permanent-income measure; see §7.4 and the real-time
robustness column.

**Table B.2 — Speed of adjustment $\lambda$ (the `ecm_lag` coefficient) across all fourteen specifications, full and pre-COVID samples (Newey–West HAC).** Source: `australia_all_results.csv`. *** sig 1%, ** sig 5%, * sig 10%.

| Specification | $\lambda$ full ($n=146$) | $\lambda$ pre-COVID ($n=126$) |
|---|---|---|
| Spec 1 | −0.177 (t=−2.34)** | −0.100 (t=−1.71)* |
| Spec 2 | −0.193 (t=−1.93)* | −0.085 (t=−0.84) |
| Spec 3 | −0.166 (t=−2.23)** | −0.101 (t=−1.70)* |
| Spec 4 | −0.140 (t=−2.05)** | −0.107 (t=−1.79)* |
| Spec 5 | −0.177 (t=−1.56) | −0.097 (t=−1.01) |
| Spec 6 | −0.180 (t=−1.76)* | −0.123 (t=−1.09) |
| Spec 6b | −0.229 (t=−4.18)*** | −0.240 (t=−4.75)*** |
| Spec 7 | −0.373 (t=−2.78)*** | −0.052 (t=−0.40) |
| Spec 7b | −0.375 (t=−2.40)** | −0.061 (t=−0.94) |
| Spec 8 | −0.445 (t=−3.30)*** | −0.247 (t=−5.03)*** |
| Spec 9 | −0.199 (t=−3.30)*** | −0.172 (t=−3.32)*** |
| Spec 10 | −0.048 (t=−0.76) | −0.027 (t=−0.62) |
| Spec 11 | −0.480 (t=−3.59)*** | −0.245 (t=−4.80)*** |
| Spec 12 | −0.029 (t=−0.68) | +0.030 (t=+1.49) |

**Table B.3 — Long-run coefficient matrix for the disaggregated specifications (full sample): OLS estimate ($t$-statistic).** Source: `australia_all_results.csv`. "—" = term not in that specification; the implied structural parameter is $\gamma=\mathrm{OLS}/|\lambda|$ (Table B.2). *** sig 1%, ** sig 5%, * sig 10%.

| Term | Spec 4 | Spec 5 | Spec 6 | Spec 8 | Spec 11 |
|---|---|---|---|---|---|
| `nla_y` | +0.0049 (+0.52) | +0.0084 (+0.22) | +0.0354 (+0.96) | +0.0435 (+3.74)*** | +0.0370 (+3.39)*** |
| `eq_y` | −0.0168 (−1.10) | −0.0268 (−0.51) | −0.0063 (−0.13) | +0.0097 (+0.69) | — |
| `super_y` | +0.0057 (+0.85) | +0.0045 (+0.48) | +0.0117 (+1.48) | +0.0139 (+2.15)** | — |
| `ilfa_y` | — | — | — | — | +0.0178 (+3.03)*** |
| `ha_y` | +0.0095 (+2.46)** | +0.0067 (+1.18) | +0.0088 (+1.52) | +0.0097 (+2.49)** | — |
| `ha_x_cci` | — | — | — | +0.0016 (+0.32) | +0.0049 (+1.03) |
| `ln_hp_over_y` | −0.0101 (−1.40) | −0.0024 (−0.11) | −0.0170 (−0.85) | — | — |
| `hp_x_1_minus_cci` | — | — | — | +0.0076 (+1.00) | +0.0142 (+2.69)*** |
| `real_rate` | −0.0004 (−0.62) | −0.0009 (−0.67) | −0.0002 (−0.17) | — | — |
| `r_x_cci` | — | — | — | +0.0019 (+1.85)* | +0.0027 (+3.57)*** |
| `cci_williams` | — | — | — | — | +0.0046 (+0.47) |
| `ln_yp_over_y` | +0.1509 (+1.71)* | +0.2484 (+0.80) | +0.1999 (+0.76) | +0.4677 (+3.44)*** | +0.5043 (+4.00)*** |
| `ln_yp_over_y_post2008` | — | — | +0.2360 (+1.16) | — | — |
| `yp_x_cci` | — | — | — | −0.6113 (−2.12)** | −0.5897 (−1.58) |
| `ecm_lag` | −0.1404 (−2.05)** | −0.1770 (−1.56) | −0.1801 (−1.76)* | −0.4449 (−3.30)*** | −0.4801 (−3.59)*** |


The contrast between the two columns is the structure-transfers-but-
calibrations-do-not result: imposing Williams' Australian calibrations
($\psi_0=0.20$, $\psi_1=0.93$, $\gamma_{\mathrm{IFA}}=0.022$) via the
iterative fixed point collapses the error-correction speed from $-0.480$
to a statistically insignificant $-0.029$, and flips it to the wrong sign
($+0.030$) pre-COVID. Spec 10 (Williams-prior calibrated;
`australia_all_results.csv`) independently reproduces the collapse
($\lambda=-0.048$, $t=-0.76$). This is a low-power-is-not-good-fit reading
of the structural-parameter inference (`australia_gamma_inference.csv`),
whose wide confidence intervals contain both Williams' values *and* zero:
the free estimates are too imprecise to reject Williams' calibrations, but
imposing them wrecks the fit.

The structural-parameter inference (the implied $\gamma=\mathrm{OLS}/|\lambda|$
with bootstrap confidence intervals) is in
[australia_gamma_inference.csv](../../outputs/australia_gamma_inference.csv);
every Williams value lies inside the (wide) 95 per cent interval, and so
does zero — consistency, not confirmation.

---

## Appendix C: Diagnostic battery

The full diagnostic output is reproduced from
[australia_all_diagnostics.csv](../../outputs/australia_all_diagnostics.csv)
(consolidated; supersedes the legacy
[australia_full_diagnostics.csv](../../outputs/australia_full_diagnostics.csv)
and
[australia_precovid_diagnostics.csv](../../outputs/australia_precovid_diagnostics.csv)).
For each specification and sample it reports the number of observations,
the residual standard error (per cent), adjusted $R^2$, Durbin–Watson,
Breusch–Godfrey serial-correlation tests at lags 1 and 4, the
heteroscedasticity test in two forms (full and events-excluded), the RESET
functional-form test, the Schwarz/BIC and the log-likelihood.

**C.1 Heteroscedasticity is structural, not event-driven.** Every
full-sample specification flags `het_diagnosis = "structural"`: the
heteroscedasticity test rejects homoscedasticity even after the COVID, GFC
and policy-event dummies are excluded (for Spec 11 the full-sample
heteroscedasticity $p\approx 7.8\times10^{-9}$ and the events-excluded
$p\approx 9.3\times10^{-10}$). This is why Newey–West HAC standard errors
are used for every reported $t$-statistic throughout the paper. The
pre-COVID Spec 11 residuals are, by contrast, well behaved (heteroscedasticity
$p=0.47$; no serial correlation, $\mathrm{AR}(1)\ p=0.37$,
$\mathrm{AR}(4)\ p=0.39$; RESET $p=0.54$).

**C.2 The multi-break Chow battery** is reported in
[australia_chow_battery.csv](../../outputs/australia_chow_battery.csv) and
the structural-break tests in
[australia_breaks.csv](../../outputs/australia_breaks.csv). The 2008Q3
break is not rejected (Chow $p=0.27$, stable); the 2020Q1 break is strongly
rejected (Chow $p\approx 9.9\times10^{-10}$ — the COVID break). The
Bai–Perron supF statistic (164.20, $p=0$) and the recursive-residual CUSUM
($p=0.9963$, stable) together locate a single break at the COVID episode
(2020Q4). This break, and not a deeper instability, is what inflates the
full-sample $\lambda$ relative to the pre-COVID value.

**C.3 The net-liquid restriction test**
([australia_nla_restriction_test.csv](../../outputs/australia_nla_restriction_test.csv))
records `restriction_accepted = TRUE` in all six rows (Specs 4, 5, 6 across
full and pre-COVID samples): the data cannot distinguish separate liquid-
asset and debt MPCs, which supports the NLA aggregation used in the
faithful LIVES form.

**C.4 Cointegration screen**
([australia_cointegration.csv](../../outputs/australia_cointegration.csv)).
The Engle–Granger ADF residual test fails to reject the null of no
cointegration for *every* estimable specification at the correct MacKinnon
critical values, while the Johansen rank-1 trace test passes for all. No
single-equation form clears the Engle–Granger screen; the interaction
specifications (Spec 8–12) are not run through the cointegration screen
(`coint` reported NA) because the long-run bracket is a non-linear
function of CCI.

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
   (A documented caveat: the CSV and RDS paths can diverge on
   Chow-borderline selector flags at machine precision; see the data
   appendix.)

The estimation script writes all fourteen specifications (Spec 1–12, plus
the long-history Spec 6b and the RBA-burden Spec 7b) to
[australia_all_results.csv](../../outputs/australia_all_results.csv) and
[australia_all_diagnostics.csv](../../outputs/australia_all_diagnostics.csv),
including the faithful LIVES headline (Spec 11) and the
Williams-calibration-imposed specification (Spec 12); all robustness,
placebo, counterfactual and comparison outputs cited in the paper are
written to the matching CSVs in `outputs/`.

The environment is pinned with `renv` (R 4.5.3); the dependency manifest is
in `DESCRIPTION` and the full transitive closure — including the `car` and
`systemfit` packages used for the Wald and SUR tests — is locked in
`renv.lock`. Restore it with `renv::restore()`. The kit also ships GitHub
Actions continuous integration and a `testthat` unit-test suite. The
repository is hosted publicly at
<https://github.com/DavidAStephan/ConsModelling>.


