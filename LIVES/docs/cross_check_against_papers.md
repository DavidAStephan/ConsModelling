# LIVES — cross-check against the canonical papers

Cross-check of our implementation in `LIVES/` and `Australia/`
against:

1. **Williams (2010 / Aust system paper)** — *Credit conditions and the
   real economy: the elephant in the room*. The Australia LIVES
   application. The 4-equation system for AU.
2. **Duca and Muellbauer (2013) "Tobin Lives"** — ECB WP No 1581. The
   canonical LIVES methodology paper, with US application using two
   latent variables (CCI + HLI).
3. **Thompson (BIS Papers 64, 2012)** — discussant remarks on Williams
   from RBA's perspective.

Verdict: **we have the LIVES *spirit* right (ECM, wealth
disaggregation, smoothed-step CCI splines, sign-prior reduction,
multi-equation system) but several Williams-specific features are
missing or implemented differently**. Listed below in order of
empirical materiality.

---

## 1. ✅ What we have implemented correctly

### 1.1 Consumption equation form

Williams' Eq 7 (Aust paper):

```
Δlog c_t = φ(α_0t + α_1t·r_{t-1} + γ_1t·HA_{t-1}/4y_t + γ_2·IFA_{t-1}/4y_t
          + γ_3·NLA_{t-1}/4y_t + ψ_t·log(y^p/y)_t + α_2·Δ_4·DEMFTB
          + α_3·Δ_4·WAPOP_{t-1} + α_4t·log(p^h/y)_{t-1}
          + log y_t − log c_{t-1})
        + β_1·DSRISK_{t-1} + β_2t·Δ_8·log ue_{t-1} + β_3·Δ_4·log c_{t-1}
        + outliers + ε_t
```

Matches our Spec 6 / Spec 8 structure: ECM in Δlog c, three-fold
wealth disaggregation (NLA, IFA, HA), permanent income, real rate,
demographics, ECM term `(log y − log c_{t-1})`, short-run growth +
unemployment + uncertainty. ✓

### 1.2 Wealth disaggregation

Williams uses three buckets: **NLA** (net liquid assets minus debt),
**IFA** (illiquid financial assets), **HA** (housing assets). We use
four buckets: `nla_y`, `eq_y`, `super_y`, `ha_y` — splitting IFA into
equities and superannuation. **More disaggregated than Williams**;
defensible because Australian super (post-SGC 1992) is a distinct
asset class. Tobin Lives also uses 3-fold (NLA, IFA, HA). ✓

### 1.3 Smoothed-step (SDMMA) CCI construction

Williams uses `SDMMA_s(t)` = 5-quarter MA of 4-quarter MA of a 0/1
step at knot date s (Aust paper §4.5). Our `smoothed_step()` in
[model_helpers.R](../../Australia/R/model_helpers.R) implements
exactly this. ✓

### 1.4 Hendry-Krolzig sign-prior reduction

Williams' "general-to-specific model selection with strict sign
priors" — we implement it as drop-on-violation in
`fit_consumption_with_williams_cci()`. ✓

### 1.5 Williams-prior calibrated specification

Williams calibrates ϖ = 1.2 (Aust paper §5.2 fn 9), ψ_0 = 0.20,
ψ_1 = 0.93. Our Spec 10 (`fit_williams_prior_spec()`) imposes these
exact calibrations via iterative fixed-point OLS. ✓

### 1.6 Down-payment composite

Williams' down-payment effect: `[1 − ϖ·CCI_t] · log(p^h/y)_{t-1}`.
We have `hp_x_1_minus_cci = ln_hp_over_y * (1 - 1.2 * cci_williams)`
in Spec 8. ✓

### 1.7 Multi-equation framework with common CCI

Williams: 4 equations, same `CCI_t` in all (with different intercept
scalings ζ_i). Our LIVES phase 3 has 3 equations + `cci_williams_joint`
required to survive sign tests in all equations. **Spirit matches.** ✓

### 1.8 ECM speed-of-adjustment as fast as Williams (in HP), slower (in cons)

Williams reports φ_c = 0.286 (consumption), φ_h = 0.244 (HP),
φ_m = 0.045 (mortgage stock), φ_w = 0.786 (HEW). Our values:
0.18-0.21 (cons, ~25-30% short of Williams), 0.089 (HP, far short),
0.015 (M, far short), n/a (no HEW yet). **Direction matches; magnitudes
short, especially for HP and M.** ⚠️

---

## 2. ⚠️ Differences from Williams that are likely material

### 2.1 ★★★ MISSING: time-varying m.p.c. on housing wealth (γ_1t)

**This is the central LIVES innovation and we don't have it.**

Williams' γ_1t is time-varying (Aust paper §4.1, §5.2): the m.p.c.
on housing wealth `HA/4y` rises with CCI, capturing that housing
wealth becomes more spendable as collateral when credit conditions
ease. Williams calibrates γ_1t such that the m.p.c. is **0.0488 at
the CCI peak, dropping to 0.0452 in 2008Q2** (Aust paper §5.2).

In Tobin Lives this is `γ_3t = γ_3·HLI_t` (eq 4.4 footnote). The
estimated equation reports `+0.055·(HLI_{t-1})·HA_{t-1}/y_t` (eq 5.2)
— a CCI×HA interaction.

**We have CCI interactions on `ln_hp_over_y` and `ln_yp_over_y` in
Spec 8, but NOT on `ha_y`.** Spec 8 lr_vars in
[australia_estimation.R](../../Australia/R/australia_estimation.R):

```
lr_vars = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
            "real_rate", "ln_yp_over_y", "ecm_lag")
sr_vars = c(..., "yp_x_cci", "hp_x_1_minus_cci", "r_x_cci")
```

We have `r_x_cci` (real_rate × cci), `hp_x_1_minus_cci`, and `yp_x_cci`
— but NO `ha_x_cci` (housing wealth × cci). **This is the key gap that
likely accounts for some of our wealth-coefficient gap with Williams.**
Implementing it should be a one-line addition to Spec 8.

### 2.2 ★★★ MISSING: HEW equation (Williams' 4th equation)

Williams' HEW equation (Aust paper Eq 13) uses
`hew/y` (mortgage flow + dwelling investment vs income), with z = 1/(HA/y)
pre-multiplication for heteroskedasticity. RBA publishes an "unpublished
HEW time series" (Aust paper §4.4) — defined as `Δ(housing-secured
debt) + housing-related government grants − dwelling investment`.

**We don't have HEW.** Either source the RBA's HEW series (per Aust
paper §4.4 it's "unpublished"; would need to be requested) or
construct a proxy:

```
HEW ≈ Δ(fin_loans_proxy) − dwelling_investment
```

where dwelling_investment is from ABS National Accounts. The
back-extended `fin_loans_proxy` already exists in master.

### 2.3 ★★★ Cross-equation normalisation (ζ_h = 1) is not enforced

Williams (Aust paper §5.1): ζ_h = 1 in the HP equation as
**normalisation** — the CCI's intercept effect on house prices is
pinned to 1, and ζ_c, ζ_m, ζ_w in the other equations are estimated
**relative to** that.

Our `cci_williams_joint` allows the CCI loading to be free in each
equation. So our coefficients are not normalised to a common scale.
This is why the M-equation CCI loading came out negative in
[phase3_findings.md §2](phase3_findings.md): the
joint-survival requirement constrains *signs*, but not *relative
magnitudes*.

**Fix**: in the joint estimation, normalise `cci_williams_joint` so
that its loading in the HP equation equals 1 by construction, and
report `ζ_c/ζ_h, ζ_m/ζ_h, ζ_w/ζ_h` as relative scalings. This is
what Williams reports.

### 2.4 ★★★ Sectional vs point sign priors

Williams imposes **sectional monotonic sign priors** (Aust paper §5.1):

| Period       | Sign prior     | Rationale                |
|--------------|----------------|--------------------------|
| 1982–1990    | non-negative   | financial deregulation   |
| Early 1990s  | non-positive   | banking sector distress  |
| Mid-1990s–2006 | non-negative | new entrants, securitisation |
| 2007–        | negative       | GFC                      |

Our maximal-GETS (in `build_williams_cci_basis()`) imposes **point
sign priors at individual knot dates**. Different mathematical
constraint:
- Williams: the *cumulative* CCI must be non-decreasing over
  1982-1990, etc.
- Ours: the *individual coefficient* on each knot must have the
  signed-correct value, but cumulative cci_williams could move
  either way between knots.

**This may be why our maximal-GETS canonical only weakly survives
the placebo test (64th/36th percentile per
[Australia/docs/back_extension_findings.md §3b](../../Australia/docs/back_extension_findings.md)).
Williams' tighter sectional constraint would be harder for random
draws to satisfy.

### 2.5 ★★ Iterated estimation of CCI vs joint state-space

Williams (Aust paper §5.1): "estimate the system using quarterly
data ... credit conditions index (CCI) interacts with (de-meaned) key
variables ... after general-to-specific model selection with strict
sign priors". The CCI is constructed from the SDMMA splines'
estimated `a_s` coefficients; the system is then re-estimated
conditional on that CCI.

Tobin Lives (Duca-Muellbauer 2013 §5): HLI is estimated as a
**Kalman-filtered stochastic trend** in a state-space model
containing consumption + refinancing rate equations. Smooth spline
is mentioned as an alternative giving "broadly similar results".

**Our approach**: the spline (cci_williams from sign-survived knot
linear combination) — matches Williams. We also have a Kalman-CCI
(cci_kalman) — matches Tobin Lives. But our Kalman-CCI is constructed
in a single-equation state-space model only (housing-loan-flow as the
anchor indicator). Williams/Tobin do it jointly across equations.

### 2.6 ★★ Williams uses iterated estimation; we don't iterate

Williams (Aust paper §5.1): "we estimate the system ... credit
conditions index (CCI) interacts with (de-meaned) key variables as
determined by economic theory and after general-to-specific model
selection with strict sign priors. The intercept effect is scaled
by ζ_i". Implies an iterated procedure: fit → identify surviving
knots → rebuild CCI → refit → ... until convergence.

Our pipeline iterates **once**: `fit_consumption_with_williams_cci()`
fits, drops sign-violators, refits. No further iteration. This is
likely sub-optimal versus Williams' approach.

### 2.7 ★★ HP-equation specifics we lack

Williams' HP equation (Aust paper Eq 11):

- **Frenzy effect**: cubic of lagged real HP growth, capturing
  momentum/overshooting. We don't have this.
- **`DSRISK`**: aversion to negative housing returns, as a 4-quarter
  MA of negative-only `Δ_4 log p^h`. We don't have this.
- **FHOS dummy**: First Home Owners Subsidy step. We don't have this.
- **Inverse housing demand**: `κ·(log y_{t-1} − (1/τ)·log h_{t-1})`
  with `h_{t-1}` = lagged net dwelling capital stock. We use
  `lag(log_hp_real)` directly — Williams' formulation is more
  structural.

### 2.8 ★ Mortgage-stock equation differences

Williams' Eq 12: `Δlog m_t = φ_m·(α_m0 + ζ_m·CCI_t + α_m·Z_t −
log(m/p)_{t-1}) + β_m·ΔX_t + ε_mt`. The ECM target is `log(m/p)`
(real per capita mortgage stock). We use `lag(log_M_real)` and let
income enter as a separate LR regressor — same as our HP equation
fix. Probably OK.

We don't have:
- The MSMEAS measurement-error correction for pre-1990 (Williams
  found stock under-reported by 8.3% pre-1990; he models this as
  `b_88·(1−SDMMA_1988)` step dummy). We don't.

### 2.9 ★ Income forecasting method differs

Williams: an AR-based PI forecaster on a "sophisticated information
set" with iterated estimation (Aust paper §6, footnote 11). His
PI weight `ψ_t` rises from 0.20 in 1978 (CCI=0) to 0.95 at peak —
calibrated, not estimated.

Our canonical PI: Italy LP (Jordà 2005 local projection) with
labour-force-share predictor. Defensible alternative; gives
positive +0.30 LR coefficient on `log(y^p/y)`. Williams calibrates
his ψ_0 = 0.20 — close to ours in magnitude.

### 2.10 ★ Sample period

Williams: 1978(1)–2008(2). Our extended sample: 1976Q3–2024Q4. Our
sample is materially longer and includes the post-GFC era which
Williams couldn't see. Different result is expected.

---

## 3. Tobin Lives (Duca-Muellbauer 2013) — what's different

### 3.1 ★★ TWO latent variables (CCI + HLI), not one

Tobin Lives (eq 5.2):

```
log(c_t/y_t) ≈ 0.131 + 0.089·CCI_{t-1} − 0.0047·r_t
            + (0.49 + 0.35·HLI_t)·E_t·log(y^p/y)_t
            + 0.101·NLA/y + 0.017·IFA/y + 0.055·(HLI_{t-1})·HA/y
```

- **CCI** (Consumer Credit Conditions Index): from Senior Loan
  Officer Survey (SLOOS), unsecured consumer credit; enters as the
  intercept-shifter (scales α_0t).
- **HLI** (Housing Liquidity Index): a separate latent variable
  capturing varying spendability of housing wealth via HEW; enters
  in interactions with HA and PI.

Williams (Aust) and we use a SINGLE CCI conflating both effects. For
Australia this is defensible (RBA doesn't run a SLOOS-style survey;
no clean separate housing-liquidity series). But the conceptual
distinction matters for how to interpret CCI.

### 3.2 ★★ Joint state-space estimation of HLI

Duca-Muellbauer-Murphy (2012a) extract HLI as a Kalman-filtered
stochastic trend in a JOINT state-space model for consumption +
mortgage refinancing rate. We have a Kalman-CCI (`cci_kalman`) but it
uses housing-loan-flow as the anchor in a single-equation state-space.
Not joint.

### 3.3 ✓ "Pay-back effect" of credit liberalisation

Tobin Lives §5: "the build-up in debt ... depresses the
consumption-to-income ratio". Captured in our framework via `nla_y`
(deposits − loans) becoming negative as debt rises. ✓ matches.

---

## 4. Discussant comments (Thompson, RBA) — issues we should address

Thompson (BIS Papers 64, 2012) raises several Williams-specific
issues that are relevant to our build:

1. **Disinflation in 1990s lowering nominal rates may be confounded
   with CCI**: lower nominal rates relax repayment-ratio
   constraints (a different credit-relaxation channel). We have
   nominal rate as a regressor in the M equation but not as an
   interaction with debt-capacity proxies.

2. **HEW "no effect of housing wealth" puzzle**: Williams found no
   significant `HA/y` in HEW; RBA's 2005 Schwartz et al. survey
   contradicts this (most HEW associated with property transactions).
   Suggests adding `housing_turnover` if available. N/A for us
   currently (no HEW).

3. **Random-walk-with-drift alternative for CCI**: Thompson suggests
   testing this. Our `cci_kalman` is close but uses a custom
   indicator set; a clean RW-with-drift alternative would be a
   sensitivity check.

4. **Post-2008 sample**: Thompson notes Williams' sample ends 2008;
   "interesting to see model estimated over updated sample". We do
   this — sample to 2024Q4. But our results show wealth-coefficient
   gap doesn't close on the longer sample.

---

## 5. Consolidated punch list, in priority order

| # | Item | Effort | Materiality |
|---|------|--------|-------------|
| 1 | Add `ha_x_cci = ha_y * cci_williams` interaction to Spec 8 (and Spec 6) | 30 min | ★★★ likely the single biggest fix toward Williams' wealth coefs |
| 2 | Re-implement sectional sign priors (per period) instead of point priors per knot | 1-2 days | ★★★ tightens identification; may pass placebo |
| 3 | Build HEW equation: `hew = Δfin_loans_proxy − dwelling_investment / income` and add to LIVES system as 4th eq (with z=1/HA pre-mult per Williams) | 2-3 days | ★★ completes phase 3 → phase 3.5 |
| 4 | Cross-equation ζ_i normalisation: pin ζ_h = 1 in HP equation, estimate ζ_c, ζ_m, ζ_w as relative scalings | 1 day (after #3) | ★★ Williams' actual identification scheme |
| 5 | Iterate CCI estimation: refit → drop violators → rebuild CCI from surviving knots → refit → ... until convergence | half day | ★★ matches Williams' procedure |
| 6 | Add HP-equation specifics: frenzy term (cubic of lagged HP growth), DSRISK, log user-cost-of-capital, FHOS dummy | 1-2 days | ★ catches non-linear HP dynamics |
| 7 | Iterated joint Kalman state-space for CCI (across all 4 equations) instead of consumption-anchored only | 3-5 days | ★ matches Tobin Lives method |
| 8 | Add MSMEAS measurement-error correction for mortgage stock pre-1990 | 1 hour | ★ minor |
| 9 | Williams-style PI calibration (ψ_0 = 0.20 → 0.95 calibrated, not estimated) — already in Spec 10 | done | ✓ |

**Top recommendation**: items 1 and 2 first. Item 1 is a 30-minute
addition that addresses the most directly substantive missing
feature (housing wealth m.p.c. varies with CCI). Item 2 is what the
Thompson discussant predicts will tighten the placebo identification.

---

## 6. Re-grading of our existing findings

Given the cross-check, the empirical findings already in
[back_extension_findings.md](../../Australia/docs/back_extension_findings.md)
and [phase1_findings.md](phase1_findings.md) /
[phase3_findings.md](phase3_findings.md) need slight reframing:

- Our **wealth-coefficient gap with Williams** (Spec 4 wealth coefs
  ~quarter of Williams' values) is now diagnosable: we're missing
  the `γ_1t·HA·CCI` time-varying interaction that Williams uses to
  *amplify* the housing wealth m.p.c. when credit is loose. Our
  constant `γ_HA` averages across loose and tight regimes,
  attenuating it. Adding the interaction (item 1) should partly close
  the gap.
- Our **placebo failure on the maximal-GETS protocol** (64th/36th
  percentile) is partly because we use point sign priors per knot
  vs Williams' tighter sectional priors over periods. Item 2 may
  raise the percentile.
- Our **phase-1 SUR finding of ρ ≈ 0** and **phase-3 finding that
  joint identification doesn't close the wealth-coefficient gap**
  remain valid. Even if we add items 1-2, the structural
  identification SUR/FIML offers is not the efficiency story (which
  was zero), but the cross-equation parameter restrictions, which
  items 4-5 implement.

So the big-picture story stays the same: the wealth-coefficient gap
is *single-equation OLS framing* (now refined: specifically the
absence of the time-varying housing-wealth m.p.c. AND the
cross-equation normalisation). Items 1-5 are the path to closing it.

---

## References

- Muellbauer and Williams (2011), "Credit conditions and the real
  economy: the elephant in the room", paper presented to the
  RBA-MAS-BIS workshop, October 2011 (the *Aust system paper -
  revised.pdf* file).
- Duca, J.V. and Muellbauer, J. (2013), "Tobin Lives: Integrating
  evolving credit market architecture into flow of funds based macro
  models", ECB Working Paper No 1581, August 2013.
- Thompson, C. (2012), "Discussant remarks on John Muellbauer and
  David M Williams' paper", BIS Papers No 64, pp 103-106.
- Williams (2009): "House prices and the macroeconomy in Australia"
  (the Aust 2009 RDP — referenced in *Aust system paper*).
- Williams (2010 BIS WP) — the original Australia working paper
  version (BIS Papers 64).
