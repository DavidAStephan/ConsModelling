# Knot experiment — findings and interpretation

A substantive empirical answer to the question "why four knots?", from
the experiment in [`Ausreplication/R/knot_experiment.R`](../R/knot_experiment.R)
that tests seven variants of the Williams-style smoothed-step CCI
spline. The auto-generated tabular output is at
[`outputs/australia_knot_experiment.md`](../outputs/australia_knot_experiment.md);
this document is the interpretive companion that the pipeline does
not overwrite.

---

## What we tested

Seven variants of the Williams 4-knot smoothed-step CCI spline,
estimated on Spec 4 (disaggregated wealth, no short-run dynamics) over
the 1988Q4–2024Q4 sample with our standard nine narrative dummies. For
each variant: candidate knot dates, sign priors derived from
institutional history, then Hendry-Krolzig drop-on-violation reduction
to identify which knots survive.

| Variant | Source | n_candidates | Sign priors |
|---|---|---:|---|
| `williams_2009` | Williams (2009) Table 3 cols A/B | 3 | `+, −, +` |
| `williams_2010` | Aust system paper / current default | 4 | `+, −, +, −` |
| `six_with_macropru` | Williams 2010 + 2014/2017 macropru | 6 | `+, −, +, −, −, −` |
| `eight_full_australia` | + 1986 housing-finance dereg + 2019Q3 APRA relax | 8 | `+, +, −, +, −, −, −, +` |
| `australia_within_sample` | Drop pre-1988 aliased knots | 5 | `−, +, −, −, −` |
| `australia_within_sample_plus` | + 1990Q3 banking + 2019Q3 relax | 7 | `−, −, +, −, −, −, +` |
| `maximal_gets` | 15-knot maximal Australian candidate set | 15 | mixed |

The candidate knots in `maximal_gets` (Campbell '79, housing dereg '86,
state-bank crisis '90, banking distress '92/'93, Wallis/APRA '98, GFC
'07, deposit guarantee '08, FHB Boost '09, APRA investor cap '14, IO cap
'17, Hayne RC '19, APRA cap removal/buffer reduction '19Q3, COVID/
JobKeeper '20, buffer hike '21) cover the documented Australian
financial-policy chronology comprehensively.

## Headline findings

### 1. Only one of Williams' four canonical knots is empirically supported on the 1988+ sample.

Of Williams' (2010) four knots in `williams_2010`:

- **1979Q1** is **aliased**. The smoothed-step dummy reaches unity by
  1980Q2, three years before our window opens, so the column is
  constant in sample and `lm()` drops it as collinear with the
  intercept.
- **1992Q1** is a **sign-violator**. Its OLS coefficient is positive
  in the post-1988 sample, contradicting the institutional reading of
  1992 as a banking-distress retrenchment that *tightened* credit. The
  coefficient is dropped during sign-prior reduction.
- **1998Q1** is also a **sign-violator**. Its OLS coefficient is
  negative in the post-1988 sample, contradicting the institutional
  reading of 1998 as the start of the NBFI / securitisation expansion
  that *loosened* credit. Also dropped.
- **2007Q1** **survives**. Coefficient negative, prior negative, sign
  matches.

So only one of four canonical Williams knots is data-validated on our
sample. The implication is that **for our sample, the canonical
4-knot spec is not actually delivering 4 degrees of CCI variation**;
it is delivering one degree (a smoothed-step shift from 0 to a
negative number across 2007Q1–2009Q1) plus a constant.

### 2. The 2007 GFC knot is robust across all variants.

`2007_01` (or `2007_09` in the maximal-GETS variant) survives in 5 of
7 variants with a stable mean coefficient of −0.014. It is the only
knot whose institutional placement and sign prior are both validated
by the data on our sample. This is genuine identification: the GFC
episode generates variation that wealth and rate channels do not
absorb.

### 3. The 2014 macroprudential knot is consistently rejected.

APRA's December 2014 investor-loan-growth cap is well documented as a
credit tightening, so a negative sign prior is institutionally
reasonable. But the knot's OLS coefficient is positive in every
variant where it appears, and it is dropped as a sign-violator in
5 of 5 variants.

The most likely explanation is **collinearity with our existing
`d_apra_2014` ogive dummy**. The dummy is centred on 2014Q4 with
half-width 2.5 quarters and enters the spec as a separate regressor;
the knot is competing with the dummy for the same variation under
a sign-prior screen. The dummy wins because it is included by default.

This is a useful negative result. **A spline knot and a smooth-
transition dummy at the same date are near-substitutes**; one or the
other should be chosen, not both. We currently use both, and the dummy
captures the macropru effect while the knot is rejected.

### 4. The 2017 IO-cap knot is mixed.

Survives in 2 of 5 within-sample variants with mean coefficient
−0.003. Same collinearity story as the 2014 knot — `d_apra_2017`
captures the bulk of the IO-cap effect.

### 5. The maximal-GETS spec identifies a different set of 5 surviving knots.

Starting from a 15-knot maximal candidate set and applying drop-on-
violation reduction yields **5 surviving knots**:

| Surviving knot | Coefficient | Institutional reading |
|---|---:|---|
| 1992Q1 | −0.020 | Banking distress / Aussie Home Loans |
| 2007Q3 | −0.007 | GFC tightening (slightly later than Williams' 2007Q1) |
| 2019Q1 | −0.005 | Hayne Royal Commission findings, lending-standards crackdown |
| 2020Q2 | **+0.077** | COVID/JobKeeper income support (positive — *demand* not *supply*) |
| 2021Q4 | −0.004 | APRA serviceability buffer increase |

`λ` under this variant is −0.127 (vs −0.076 for Williams-2010), and
adjusted R² is 0.734 (vs 0.731). Both are improvements, though the R²
gap is small.

Two observations on this:

- **1992Q1 survives in `maximal_gets` but not in the parsimonious
  variants.** This reflects a well-known feature of spline estimation:
  knot coefficients depend on which other knots are simultaneously
  estimated. With a finer institutional resolution (1986 housing
  dereg, 1990Q3 state bank distress, 1992Q1, 1993Q1) the 1992 banking-
  distress story is data-supported; the parsimonious specs effectively
  force 1992 to absorb broader 1980s/early-1990s variation that doesn't
  share its sign.
- **2020Q2 is positive** (+0.077). The "knot" at 2020Q2 is not really
  a credit-conditions event; it is the JobKeeper income-support shock,
  which loosened *demand-side* constraints on consumption rather than
  supply-side credit conditions. Positive sign matches. This is a
  case where the spline is identifying a structural shift but in
  fiscal not credit policy.

### 6. The Williams 4-knot choice is conservative for our sample.

Two readings:

- **Charitable.** Williams (2010) sample is 1977Q2–2008Q2. He could
  not have known about post-2008 macroprudential events, and his
  pre-2008 deregulation calendar was correct for the institutional
  history available to him. The 4-knot choice is appropriate for that
  vintage. Our 1988+ sub-sample loses the 1979 deregulation episode
  *by construction* (the smoothed step is constant); the loss of three
  knots is an artefact of sample windowing.
- **Critical.** Even on Williams' published sample, the 4-knot
  spec's three pre-2007 knots may have been identified by short pre-
  knot windows that are themselves contestable. Without multi-equation
  common-factor identification (which Williams has and we don't), the
  spline has weaker discipline than the published narrative suggests.

The empirical evidence here favours the **critical reading** for
*our* implementation. Williams' sample includes ~5 quarters of pre-
1979 baseline; ours includes zero. Williams identifies the 1992
banking-distress knot from the immediate aftermath of the early-1990s
recession; we observe only the recovery and re-expansion phase. **The
loss of identification is structural, not just sample-size.**

## What this means for the WP

The CCI exploration in [`cci_exploration.md`](cci_exploration.md) §1
already concluded that, in our single-equation OLS implementation
without common-factor identification, the spline is closer to a
flexible structural-shift parameterisation than to disciplined latent-
factor identification. This experiment **partially vindicates and
partially refines that conclusion**:

**Partial vindication of the user's "detrending" critique.** Williams'
canonical 4-knot spec on our sample really does have only one degree
of identifiable CCI variation. The other three knots are either
mechanically uninformative (1979 aliased) or empirically rejected
(1992 and 1998 sign-violators). On the canonical spec, the spline
is doing approximately one thing — picking up the 2007 GFC shift —
which is what `d2008_gfc` could equivalently capture if we coarsened
the timing. The user's intuition that the spline is closer to flexible
detrending than to genuine structural identification has empirical
support.

**Refinement: the maximal-GETS spec rescues identification.** When we
let the data choose from a 15-knot candidate menu, we get 5 surviving
knots with different institutional contents (banking distress, GFC,
Hayne RC, COVID/JobKeeper, buffer hike). This is *more* identification
than Williams' canonical spec delivers on the same sample, and the
identification is *data-driven* rather than authorial-judgement-driven.
The detrending critique applies less forcefully when the knot set is
chosen by reduction rather than imposition.

**Recommended adjustment to the WP §5 narrative.**

The current draft asserts that the Williams 4-knot spline is the
identifying mechanism for the long-run CCI. Replace this with a more
honest account: report that the canonical 4-knot spec has only one
identifiable knot on the 1988+ sample (2007), document the maximal-GETS
variant as the methodologically defensible alternative, and report the
five surviving knots from that variant as the data-driven institutional
chronology. The §5 narrative should also acknowledge the
near-collinearity with the existing macroprudential dummies, which is
why the 2014 and 2017 knots fail in our spec (the dummies absorb their
identifiable variation).

## Resolved: canonical CCI basis

**Decision: Option C — maximal-GETS spec is canonical** (resolved
2026-04). `build_williams_cci_basis()` in `model_helpers.R` defaults to
the 15-knot maximal candidate set with sign-prior reduction;
`build_williams_cci_basis_canonical()` retains the original Williams
4-knot basis as a benchmark for replication.

The placebo test (200 random 4-knot draws over 1979–2007) provides
empirical justification for the choice: Williams' canonical 4-knot
benchmark sits at the **49th percentile** of the placebo distribution
by adj-R² and the **22nd percentile** by |λ|, indicating that the
specific 1979/1992/1998/2007 knot dates are arbitrary on our sample.
The maximal-GETS reduction lets the data choose the surviving knots
through Hendry-Krolzig drop-on-violation, recovering six that match
known institutional events.

The Kalman state-space CCI (Spec 9) is the **methodology robustness
column** — a model-based latent-factor extraction that delivers a
significant λ in the consumption equation (−0.206), confirming the
identification result is not an artefact of the smoothed-step spline
parameterisation.

---

## Appendix — knots tested and why

The 16 institutional-history candidate knots tested across variants:

| Date | Event | Sign prior | Notes |
|---|---|---:|---|
| 1979Q1 | Campbell Committee establishment | +1 | Williams' canonical knot; aliased on 1988+ sample |
| 1986Q1 | Removal of housing-finance interest-rate ceilings | +1 | Aliased; major housing-finance dereg |
| 1990Q3 | State Bank of Victoria collapse begins | −1 | Banking distress proxy |
| 1992Q1 | Banking-system distress trough; Aussie Home Loans launched | −1 | Williams' canonical knot; sign-violator in parsimonious specs |
| 1993Q1 | Peak banking provisions | −1 | Sub-event of 1992 episode |
| 1998Q1/Q3 | Wallis Inquiry → APRA/ASIC; NBFI expansion | +1 | Williams' canonical knot; sign-violator at 1998Q1 |
| 2007Q1/Q3 | GFC tightening begins | −1 | **Robust survivor** |
| 2008Q4 | Australian Government Guarantee on bank wholesale funding | −1 | Crisis-response policy |
| 2009Q1 | First Home Owner Boost (FHB Boost) | +1 | Aliased — too close to GFC for separate identification |
| 2014Q4 | APRA 10% investor-loan-growth cap | −1 | Sign-violator in all variants (collinear with `d_apra_2014`) |
| 2017Q1 | APRA 30% interest-only loan cap | −1 | Sometimes survives (2 of 5 variants) |
| 2019Q1 | Hayne Royal Commission Final Report | −1 | Survives in maximal-GETS |
| 2019Q3 | APRA macroprudential cap removal + serviceability buffer reduction | +1 | Sign-violator |
| 2020Q2 | COVID lockdowns + JobKeeper | +1 | Survives in maximal-GETS — but identifies fiscal not credit shock |
| 2021Q4 | APRA increases serviceability buffer 2.5% → 3% | −1 | Survives in maximal-GETS |

Sources: my institutional knowledge cross-referenced against Aust
system paper (Williams 2010) §3, Williams (2009) §5.1, RBA Statement
of Monetary Policy "Domestic Financial Conditions" sections, APRA
Information Paper "Macroprudential Policy" (2018+), and
Battellino-McMillan (1989) for the early-1980s deregulation
chronology.
