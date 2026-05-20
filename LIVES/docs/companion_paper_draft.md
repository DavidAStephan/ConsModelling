# A Multi-Equation LIVES System for Australia: Joint Identification of Credit Conditions and Williams' Calibrations Tested

**Companion paper draft**

**Authors:** *to be added*
**JEL codes:** E21, E32, E51, D14
**Keywords:** household consumption, LIVES system, credit conditions, joint
identification, cross-equation restrictions

---

## Abstract

We extend the single-equation Australian LIVES consumption model in our
headline paper to a multi-equation system, following Williams' (2010,
2012) four-equation specification: consumption, house prices, mortgage
stock, and home equity withdrawal jointly estimated under
cross-equation sign restrictions. We implement the system in four
sequential stages: a two-equation SUR of consumption and house prices,
a three-equation joint sign-survival identification of the credit-
conditions index (CCI), a four-equation system that adds an HEW
equation, and a formal Wald test of Williams' Table 1 calibrations as
restrictions on the consumption equation.

The empirical results are sharper than the single-equation evidence
in the headline paper. **Joint sign-survival of CCI knots collapses
dramatically as more equations are added.** From the 5 knots that
survive the iterated consumption-equation maximal-GETS reduction
(2009Q1, 2019Q1, 2020Q2, and two others), only 2 survive a
three-equation joint test (1986 financial deregulation; 2017 APRA
macroprudential round II), and only **1** survives the four-equation
test once the HEW equation is added (1986 deregulation alone).
**Williams' ζ_h = 1 normalisation becomes inert with one surviving
knot** — the HP-weighted, M-weighted, and consumption-weighted CCI
variants are mathematically identical after peak-normalisation, so
the cross-equation rescaling has no traction.

**Williams' Table 1 wealth γ are formally rejected on the
contemporary Australian data.** A joint Wald test using the
Newey–West covariance gives χ²(4) = 10.03 (p = 0.040) for the four
wealth calibrations alone and χ²(6) = 29.10 (p < 0.001) for the
full Williams calibration set. The individual rejection is the
housing-wealth m.p.c. γ_HA = 0.0488 (χ²(1) = 7.18, p = 0.007),
even though the *implied* structural γ_HA from our Spec 6 estimate
is 0.049 — essentially equal to Williams' value. The reconciliation
runs through λ: the OLS coefficient on `ha_y` (0.0088) sits 37 per
cent below Williams' implied OLS at his published λ (0.0140), and
that gap is what generates the rejection of the calibration as a
parameter restriction.

The HEW equation we construct using `Δ(housing-secured debt) /
income` as a proxy (Williams' literal definition is unsourced for
contemporary Australia) yields a residual correlation of 0.83 with
the mortgage-stock equation — confirming that under the proxy
construction the two equations are not separately identifying
signals. A more discriminating HEW measure, with the
dwelling-investment subtraction Williams' definition includes, is
the binding constraint on further multi-equation work.

The Phase A multi-equation system therefore complements rather than
overturns the headline paper's positive finding: Spec 6 reproduces
Williams' structural γ profile to a useful approximation, and the
multi-equation extension formally tests *and rejects* the calibrated
values as a system of parameter restrictions. The four-equation
common-factor hypothesis — that a single CCI series enters all four
LIVES equations with sign-consistent loadings — is only weakly
supported on contemporary Australian data, with one institutional
event (1986 deregulation) carrying the joint identification.

---

## 1. Introduction

This paper is the companion to our headline single-equation paper on
the Australian LIVES consumption model. The headline paper estimates
Williams (2010, 2012) consumption equation on contemporary public
data and finds a structural γ profile broadly consistent with
Williams' Table 1: γ_HA = 0.049 (Williams: 0.049), γ_IFA = 0.030
(Williams: 0.022), γ_NLA = 0.196 (Williams: 0.159). The single-
equation framing is positive about Williams' calibration set in
aggregate, but cannot impose the cross-equation parameter
restrictions that the LIVES framework treats as the structural
identification of the credit-conditions index.

This paper builds the multi-equation system in stages. The objective
is to assess whether the cross-equation parameter restrictions in
the four-equation LIVES system are empirically supported on
contemporary Australian data, and whether they close the residual
identification questions left open by the single-equation paper.

### 1.1 Headline findings

1. **Cross-equation joint sign-survival of the CCI knots collapses
   as equations are added.** Three knots survive iterated
   consumption-equation fitting (2009Q1, 2019Q1, 2020Q2); two
   survive the three-equation joint test (1986, 2017); one
   survives the four-equation test once the HEW equation is added
   (1986 alone).

2. **ζ_h = 1 normalisation has no empirical traction with one
   surviving knot.** Williams' identification scheme — pinning the
   HP-equation CCI loading to 1 and estimating ζ_c, ζ_m, ζ_w as
   relative scalings — requires multiple surviving knots whose
   relative weights can shift across normalisation schemes. With
   one surviving knot the three weighted variants are identical
   after peak-normalisation.

3. **Williams' Table 1 calibrations are formally rejected.** A
   Wald test of the joint γ_HA = 0.0488, γ_IFA = 0.022,
   γ_NLA = 0.159, ψ_0 = 0.20, and ln(p^h/y) = −0.13 restrictions
   gives χ²(6) = 29.10, p < 0.001. The wealth-only subset rejects
   at 5 per cent, χ²(4) = 10.03, p = 0.040. The individual
   rejection is γ_HA — and yet the *implied* γ_HA from our
   Spec 6 estimate (0.049) is essentially equal to Williams'
   value, so the rejection runs through the speed-of-adjustment
   channel rather than the wealth-coefficient channel.

4. **The HEW residuals correlate 0.83 with mortgage-stock
   residuals** under the credit-flow proxy construction. The two
   equations are not separately identifying signals; a properly
   constructed HEW series with the dwelling-investment subtraction
   Williams' definition includes would be the next data
   investment.

### 1.2 Relationship to the headline paper

The headline single-equation paper finds that Spec 6 (the preferred
disaggregated specification with CCI short-run dynamics and the
post-2008 PI break) reproduces Williams' Table 1 γ profile to a
useful approximation. That finding stands: the present multi-
equation paper does not overturn it. But the multi-equation
analysis here clarifies *why* the positive single-equation reading
coexists with the formal rejection of Williams' calibrations as
parameter restrictions — the headline paper's γ are implied
quantities from the ECM identity, while the calibration restrictions
test the OLS coefficients directly, and our smaller |λ| breaks the
correspondence between the two test forms.

---

## 2. Methodology

### 2.1 The four-equation LIVES system

Williams (2010) estimates four equations jointly by full-information
maximum likelihood:

```
Δlog c_t   = θ_C  [ … + γ_HA·(HA/y)·(1 − ϖ·CCI) + ψ_t·log(y^p/y)
                    + ζ_c·CCI + λ_C·ecm_lag_C ]  + Σ β_C · SR_C  + ε_C,t
Δlog hp_t  = θ_H  [ … + α_R·r·(1 + κ·CCI) + ζ_h·CCI + λ_H·ecm_lag_H ]
                                                + Σ β_H · SR_H  + ε_H,t
Δlog m_t   = θ_M  [ … + η·CCI + λ_M·ecm_lag_M ] + Σ β_M · SR_M  + ε_M,t
hew_t      = θ_W  [ … + (HLI factor)·CCI + λ_W·ecm_lag_W ] · 1/(HA/y)
                                                + Σ β_W · SR_W  + ε_W,t
```

with **ζ_h = 1** as identification, ζ_c, ζ_m, ζ_w as relative
scalings, and **ϖ** restricted to the same value across the
consumption and HP equations.

### 2.2 Joint sign-survival as a tractable approximation to FIML

We do not implement full FIML in this paper. The structural
restriction at the heart of LIVES — that the *same* CCI series enters
all four equations with sign-consistent loadings — can be tested
approximately via **joint sign-survival**: fit each equation
separately with the full 15-knot smoothed-step CCI basis, and retain
only knots whose coefficient sign matches its institutional prior in
every equation simultaneously. The retained set defines the joint-
identified CCI; the relative loadings on this CCI across equations
are the empirical analogues of Williams' ζ_i.

This approximation imposes a sign restriction, not a parameter
equality restriction. True FIML imposes both. We document where the
two differ.

### 2.3 ζ_h = 1 normalisation

After joint sign-survival, we construct three CCI variants by
weighting the surviving knots with each equation's coefficients:

- `cci_williams_joint`     — consumption-equation weights
- `cci_williams_joint_h`   — house-price-equation weights (Williams' ζ_h = 1)
- `cci_williams_joint_m`   — mortgage-equation weights

Each variant is peak-normalised. The HP-weighted variant is the
literal implementation of Williams' ζ_h = 1 normalisation: the CCI
is the linear combination of surviving knots whose loading in the
HP equation is unity by construction. The other equations'
loadings on this rescaled series are the relative scalings ζ_c, ζ_m,
ζ_w. This identification matters when multiple knots survive: the
relative weights then differ across the variants and the CCI shapes
are distinct. With only one surviving knot the variants collapse to
the same series.

### 2.4 Williams' calibrations as Wald restrictions

Williams reports six calibrated long-run γ in Table 1:
γ_HA ∈ [0.0452, 0.0488] (housing wealth at CCI peak), γ_IFA = 0.022
(illiquid financial, calibrated), γ_NLA = 0.159 (net liquid), ψ_0 =
0.20 (PI weight at CCI = 0), the wealth-CCI interaction ϖ = 1.2, and
the speed of adjustment λ = −0.286.

The ECM identity OLS_coef_i = λ × γ_i lets us cast each calibration
as a linear restriction on the OLS coefficient vector of our Spec 6,
conditional on λ. We test these restrictions individually, in
groups, and jointly using `car::linearHypothesis` with the Newey–West
covariance.

---

## 3. Data and equations

We use the back-extended master dataset from the headline paper
(1976Q3–2024Q4, n = 194 quarters; see §3 of the headline paper for
construction). The four equations are estimated on the common
sample where all four LHS variables and all RHS regressors are
non-missing.

### 3.1 The HEW proxy

Williams (Aust paper §4.4) defines home equity withdrawal as

```
HEW_t = Δ(housing-secured debt)_t + housing-related grants_t − dwelling investment_t
```

The RBA publishes the constructed HEW series as an unpublished
internal time series in Williams' work; it is not available in the
current public data vintage. We construct a proxy that uses the
credit-flow dimension only:

```
hew_proxy_t = Δ(fin_loans_proxy)_t / ydi_ann_nom_t
```

with two caveats: (i) the dwelling-investment subtraction is omitted,
so our proxy over-states "true" HEW; (ii) `fin_loans_proxy` is total
household debt rather than housing-specific debt — on the modern
sample housing-specific debt is ~85 per cent of the total, so the
approximation is close, but the pre-1990 portion of the sample is
noisier.

The proxy is sufficient to demonstrate the multi-equation framework's
mechanics; quantitatively reliable HEW estimates would require
sourcing the RBA's unpublished series or constructing a properly
deflated dwelling-investment time series.

### 3.2 Equations estimated

| Eq | Response | Long-run regressors | ECM term |
|---|---|---|---|
| C | Δlog c          | ln_networth_y, ln_hp_over_y, real_rate, ln_yp_over_y, CCI | log(c_{t-1}/y_t) |
| H | Δlog hp_real    | lincome, log_credit_y, real_rate, prime_age_share, CCI    | log(hp_{t-1})    |
| M | Δlog M_real     | lincome, ln_hp_over_yd, real_rate, prime_age_share, CCI   | log(M_{t-1})     |
| W | hew_proxy       | lincome, ha_y_proxy, real_rate, prime_age_share, CCI      | hew_proxy_{t-1}  |

Sign priors on the CCI loading: positive in all four equations
(looser credit conditions raise consumption, house prices, mortgage
stock, and home-equity withdrawal).

---

## 4. Phase 1 — two-equation SUR

The two-equation SUR of consumption and house prices on the back-
extended sample (n = 189, 1977Q4–2024Q4) is the foundation result.
The residual correlation between the two equations is
ρ̂(ε_C, ε_H) = 0.0007 — essentially zero — under the full
specification with `cci_williams` and event dummies. The finding is
robust across variants: removing CCI gives ρ̂ = −0.083, removing
event dummies gives ρ̂ = +0.043, and the minimal LR + SR
specification gives ρ̂ = −0.025.

Joint estimation therefore delivers no efficiency gain at the
quarterly frequency. The case for the multi-equation framework
rests entirely on cross-equation parameter restrictions, not on
residual covariance.

A sign violation is documented on `cci_williams` in the house-price
equation: loading −0.024, t = −4.68, against an institutional prior
of positive. This is the structural failure that motivates the
joint-identified CCI of phases 3 and beyond.

---

## 5. Phase 3 — three-equation joint sign-survival

We refit all three equations (consumption, HP, mortgage stock) with
the full 15-knot Williams CCI basis on the back-extended common
sample (n = 189). A knot is **jointly surviving** if its coefficient
sign matches the institutional prior in every equation
simultaneously.

### 5.1 Survival results

| Survival regime                       | Surviving knots                                                   | n |
|---|---|---:|
| Consumption only (iterated)           | 1979Q1, 1986Q1, 1992Q1, 2007Q3, 2017Q1, 2020Q2                    | 6 |
| **Three-equation joint (C ∩ H ∩ M)**  | **1986Q1, 2017Q1**                                                | **2** |

Four of the six consumption-survivors fail joint sign tests in the
HP or mortgage equations. The retained set — 1986 financial
deregulation and 2017 APRA macroprudential round II — covers two
broad-spectrum institutional events with sectoral effects on all
three blocks.

### 5.2 Joint identification fixes the HP sign violation

Constructing `cci_williams_joint` from the two surviving knots,
weighted by consumption-equation coefficients, and re-running the HP
equation:

| HP-equation CCI loading       | (a) cons-only CCI | (b) joint CCI |
|---|---:|---:|
| Estimate                       | −0.024            | **+0.024**    |
| t-statistic                    | −4.68             | **+2.03**     |
| Sign vs prior (+)              | ✗ violator        | ✓             |

Joint identification flips the HP equation's CCI sign from
significantly wrong to significantly right. This is Williams'
cross-equation sign restriction working as intended.

### 5.3 Joint identification does NOT fix the M sign violation

The mortgage equation's CCI loading remains wrong-signed (loading
−0.0086, t = −1.43) under the joint-identified CCI. The two-knot
weighted construction is insufficient to pass sign tests in all
three equations simultaneously when the weights derive from one
equation's coefficients. True FIML imposes parameter equality
(magnitude as well as sign) across equations; the joint sign-survival
approximation imposes only sign equality.

---

## 6. Phase A — four-equation system with HEW

### 6.1 Adding the HEW equation collapses the joint survivor set

Refitting all four equations (consumption + HP + M + HEW) on the
common sample with the 15-knot CCI basis, and requiring sign-prior
survival in every equation, leaves **only one knot**:

| Survival regime                              | Surviving knots             | n |
|---|---|---:|
| Three-equation joint (C ∩ H ∩ M)             | 1986Q1, 2017Q1              | 2 |
| **Four-equation joint (C ∩ H ∩ M ∩ W)**       | **1986Q1**                  | **1** |

Sdmma_2017_03 sign-violates in the HEW equation (HEW responds
positively to APRA tightening in the data, against the institutional
prior of negative). Only the 1986 financial deregulation knot
survives the four-equation joint test.

### 6.2 ζ_h = 1 normalisation is inert with one surviving knot

With one surviving knot, the three weighted variants of
`cci_williams_joint` (consumption-weighted, HP-weighted,
mortgage-weighted) are mathematically identical after peak-
normalisation. The variant *correlations are all 1.0*. The HP-
equation CCI loading is identical across all three regimes, as is
the mortgage-equation loading and the HEW-equation loading. The
cross-equation rescaling of Williams' ζ_h = 1 normalisation has no
empirical traction.

This is not a coding artefact: it is a property of the underlying
data. With one surviving knot, the CCI is essentially a single
institutional shock (the 1986 deregulation smoothed-step). Any
positive multiplicative rescaling of this shock leaves its peak-
normalised form unchanged.

### 6.3 HEW residuals correlate 0.83 with mortgage residuals

The four-equation SUR residual correlation matrix (under the
joint-identified CCI):

```
         C       H       M       W
C    1.000  -0.107  +0.090  +0.042
H   -0.107   1.000  -0.194  -0.183
M   +0.090  -0.194   1.000  +0.832
W   +0.042  -0.183  +0.832   1.000
```

The mortgage and HEW residuals correlate at **+0.83**. This is a
structural feature of the proxy HEW construction: `hew_proxy = Δ
fin_loans_proxy / income` is the change-form of the M-equation
dependent variable `log M_real`. Under the proxy the two equations
are not separately identifying signals.

### 6.4 Coefficient profile across regimes

The 4-equation SUR coefficient profile is summarised in
[lives_phase_a_summary.csv](../outputs/lives_phase_a_summary.csv).
Headline λ values across the four equations under the joint CCI
regime:

| Equation     | λ_eq   |
|---|---:|
| Consumption  | −0.215 |
| House price  | −0.037 |
| Mortgage     | +0.002 (essentially zero) |
| HEW          | +0.067 (wrong sign) |

The HEW equation's ECM term is wrong-signed under the proxy
construction — `hew_proxy` does not mean-revert in our sample. A
properly constructed HEW measure with the dwelling-investment
subtraction would likely behave differently here.

---

## 7. Phase B — Williams' Table 1 calibrations as Wald restrictions

We refit Spec 6 (the preferred disaggregated specification from the
headline paper) on the canonical Italy-LP master and test Williams'
Table 1 calibrations as linear restrictions on the OLS coefficient
vector, conditional on the estimated λ̂.

### 7.1 Calibration grid

| Coefficient        | Williams γ | Implied OLS at λ̂ |
|---|---:|---:|
| ha_y                | 0.0488   | −0.0096   |
| eq_y                | 0.0110   | −0.0022   |
| super_y             | 0.0110   | −0.0022   |
| nla_y               | 0.1590   | −0.0314   |
| ln_hp_over_y        | −0.1300  | +0.0257   |
| ln_yp_over_y        | 0.2000   | −0.0395   |

The implied OLS values are computed at λ̂ = −0.197 (the version of
Spec 6 estimated by the test script; this is a slight refit of the
canonical −0.180 in the headline paper because the script
re-derives the data flow rather than reusing the cached pipeline
outputs).

### 7.2 Wald test results

| Restriction                          | χ²    | df | p-value | Reject at 5 % | Reject at 1 % |
|---|---:|---:|---:|:-:|:-:|
| ha_y = 0.0488                        | 7.18  | 1  | 0.007   | ✓ | ✓ |
| eq_y = 0.011                         | 0.02  | 1  | 0.896   | ✗ | ✗ |
| super_y = 0.011                      | 2.26  | 1  | 0.133   | ✗ | ✗ |
| nla_y = 0.159                        | 2.55  | 1  | 0.110   | ✗ | ✗ |
| ln_hp_over_y = −0.130                | 2.20  | 1  | 0.138   | ✗ | ✗ |
| ln_yp_over_y = 0.200                 | 1.23  | 1  | 0.268   | ✗ | ✗ |
| **Joint wealth (4 restrictions)**    | **10.03** | **4** | **0.040** | **✓** | ✗ |
| **Joint all (6 restrictions)**        | **29.10** | **6** | **<0.001** | **✓** | **✓** |

### 7.3 The reconciliation with the headline paper

The single rejection at 1 per cent is the housing-wealth m.p.c.
γ_HA = 0.0488. Yet the headline paper reports γ_HA = 0.049 as the
*implied* γ from Spec 6 — essentially equal to Williams' value.

The reconciliation runs through the ECM identity. Spec 6's
OLS coefficient on `ha_y` is 0.0088; the implied γ = 0.0088 / 0.180
= 0.049 in the headline paper. Williams' implied OLS at his λ is
0.0488 × 0.286 = 0.0140. Our OLS is therefore 37 per cent below
Williams' implied OLS, and our |λ| is also 37 per cent below
Williams'; the two ratios cancel in the implied γ comparison but
not in the Wald test of the OLS restriction.

The two tests answer different questions. The implied-γ comparison
asks "does our structural γ profile match Williams' Table 1?" — the
answer for housing wealth is yes, almost exactly. The Wald
restriction test asks "is the joint hypothesis (γ_HA = 0.0488 *and*
λ as estimated) consistent with our OLS coefficients?" — and the
answer is no for housing wealth because the OLS coefficient is too
small relative to what Williams' γ at our λ would imply.

For the four wealth coefficients jointly, the joint Wald test
rejects at 5 per cent (χ²(4) = 10.03, p = 0.040). For the full
calibration set including the affordability and PI terms, the joint
test rejects strongly (χ²(6) = 29.10, p < 0.001). Williams'
calibrations are not consistent with a single set of OLS estimates
under the Newey–West uncertainty bands.

---

## 8. Discussion and conclusion

### 8.1 What the multi-equation analysis adds

The headline single-equation paper finds that Spec 6 reproduces
Williams' structural γ profile to a useful approximation, and reads
the residual divergence as a consequence of the single-equation
framing rather than of sample length, knot count, or sign-prior
structure. The multi-equation analysis in this companion paper
reaches a complementary conclusion *from a different direction*:

- The cross-equation parameter restrictions Williams' framework
  treats as the structural identification are only weakly supported
  on contemporary Australian data. One institutional event (1986
  deregulation) carries the four-equation joint identification.
- Williams' Table 1 calibrations are formally rejected as a system
  of restrictions on the consumption equation, even though the
  implied γ_HA matches Williams almost exactly.
- The proxy HEW equation does not deliver an additional identifying
  signal; its residuals correlate 0.83 with the mortgage equation,
  so the four-equation system effectively has three independent
  blocks under the proxy construction.

### 8.2 Where this leaves the LIVES program for Australia

Two reasonable readings of the multi-equation evidence.

**Reading A.** The four-equation LIVES system, as Williams
formulated it, is genuinely not the right model for contemporary
Australia. The 1986 deregulation is a single institutional shock;
the apparently rich array of post-1990 events (1992 banking
distress, 1998 NBFI expansion, 2007 GFC, 2014/2017 APRA, 2020
COVID, 2021 APRA) does not produce sign-consistent loadings across
four blocks of household behaviour. The LIVES framework's claim of
a common credit-conditions factor is empirically thin once joint
sign-survival is required.

**Reading B.** The four-equation system *is* the right model, but
our proxy HEW equation is the binding constraint. A properly
constructed HEW series — with the dwelling-investment subtraction
Williams' definition includes — might restore additional knot
survival in the joint test, and the cross-equation parameter
restrictions might then become more discriminating. The 0.83
residual correlation between mortgage and HEW is consistent with
this reading: under a proper HEW the residuals would have a
different signal content.

Choosing between these readings requires the dwelling-investment
data. Sourcing the ABS National Accounts gross fixed capital
formation on residential dwellings (Cat 5206.0 Table 5) and
constructing a real per-capita series anchored to the same nominal
income denominator as our master is the obvious next-step data
investment. Until then the multi-equation evidence above is the
binding empirical constraint.

### 8.3 What does *not* change

The headline single-equation finding — that Spec 6 reproduces
Williams' structural γ profile to a useful approximation, with
γ_HA = 0.049 matching Williams almost exactly — stands. The
Phase A and Phase B multi-equation evidence does not overturn it.
What it adds is a more precise account of *what kind of statement*
the headline finding is: an implied-γ match, conditional on a
35 per cent shortfall in both the OLS coefficient and |λ|, which
cancels under the ECM identity but does not survive a joint Wald
restriction. The two statements are simultaneously true and the
distinction is methodological rather than empirical.

### 8.4 Path forward to full FIML

The four-equation SUR with joint-survival CCI is the natural pre-
FIML configuration. The remaining methodological gap — imposing
parameter equality across equations rather than only sign equality
— is the Phase B item B1 in the multi-equation plan. Custom
likelihood code with shared ϖ across the consumption and HP
equations is a 2–4 week implementation; the present paper provides
the data and SUR infrastructure that the FIML build would inherit.

Given the Phase A finding that the joint sign-survival CCI is a
single-knot series, the FIML build would not have many
cross-equation parameters to share initially. The case for
committing to the months-of-work FIML is therefore weaker than it
appeared before Phase A: the structural identification machinery
operates on a thin empirical basis. Expanding the candidate knot
set (e.g. with additional Australian institutional events such as
specific RBA policy episodes) is a logically prior step.

---

## References

(Shared bibliography with the headline single-equation paper; see
[`Australia/docs/wp_draft.md`](../../Australia/docs/wp_draft.md)
§References. Additional citations specific to this paper:)

- Duca, J. V., & Muellbauer, J. (2013). Tobin LIVES: Integrating
  evolving credit market architecture into flow-of-funds based
  macro models. *European Central Bank Working Paper* 1581.
- Muellbauer, J., & Williams, D. (2012). Credit conditions and the
  real economy: the elephant in the room. *BIS Papers* No. 64.
- Williams, D. M. (2010). Consumption, wealth and credit
  liberalisation in Australia. *Oxford Economics Series Working
  Paper* 492.

---

## Appendix A: Implementation

Phase 1 SUR: [`LIVES/R/lives_sur_2eq.R`](../R/lives_sur_2eq.R).

Phase 3 joint sign-survival and three-equation SUR:
[`LIVES/R/joint_cci_identification.R`](../R/joint_cci_identification.R),
[`LIVES/R/lives_sur_3eq.R`](../R/lives_sur_3eq.R).

Phase A four-equation SUR with HEW + ζ-normalisation variants:
[`LIVES/R/lives_sur_4eq.R`](../R/lives_sur_4eq.R).

Phase B Williams calibration Wald tests:
[`LIVES/R/williams_calibration_test.R`](../R/williams_calibration_test.R).

Data preparation: [`LIVES/R/lives_data_prep.R`](../R/lives_data_prep.R).
Iterated maximal-GETS CCI fitting in
[`Australia/R/australia_estimation.R`](../../Australia/R/australia_estimation.R)
function `fit_consumption_with_williams_cci()`.

## Appendix B: Outputs

- [lives_joint_cci_survival.csv](../outputs/lives_joint_cci_survival.csv)
- [lives_sur_2eq_coefs.csv](../outputs/lives_sur_2eq_coefs.csv),
  [lives_sur_2eq_compare.csv](../outputs/lives_sur_2eq_compare.csv),
  [lives_sur_2eq_resid_corr.csv](../outputs/lives_sur_2eq_resid_corr.csv)
- [lives_sur_3eq_coefs.csv](../outputs/lives_sur_3eq_coefs.csv),
  [lives_phase3_comparison.csv](../outputs/lives_phase3_comparison.csv)
- [lives_sur_4eq_coefs.csv](../outputs/lives_sur_4eq_coefs.csv),
  [lives_sur_4eq_residcorr.csv](../outputs/lives_sur_4eq_residcorr.csv),
  [lives_phase_a_summary.csv](../outputs/lives_phase_a_summary.csv)
- [williams_calibration_wald.csv](../outputs/williams_calibration_wald.csv)
