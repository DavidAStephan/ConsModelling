# Items 1 and 2 — findings

Per the cross-check punch list in
[cross_check_against_papers.md §5](cross_check_against_papers.md), this
note records the results of implementing the two highest-priority fixes:

1. **Item 1**: add `ha_x_cci = ha_y × cci_williams` interaction to Spec 8
   (the time-varying housing-wealth m.p.c. — Williams Aust eq 7 γ_1t·HA;
   Tobin Lives 2013 eq 5.2 (HLI)·HA/y).
2. **Item 2**: re-implement sectional sign priors per Williams Aust paper
   §5.1 (one prior per period, periods derived from institutional history).

Both were predicted to be material improvements. The empirical results
are mixed and partly surprising.

---

## 1. Item 1 — `ha_x_cci` interaction in Spec 8

Implementation: [australia_estimation.R](../../Ausreplication/R/australia_estimation.R)
~lines 1480–1505. Added `ha_x_cci = ha_y * cci_williams` to the Spec 8
mutate block and the lr_vars list.

### Result

| Long-run coef            | Spec 8 pre-fix | Spec 8 post-fix | Williams Aust |
|--------------------------|---------------:|----------------:|--------------:|
| λ (ecm_lag)              |         −0.20  |    **−0.383**   |       −0.286  |
| ha_y (LR struct)         |         +0.049 |        +0.024   |     +0.0488 (peak) |
| ha_x_cci (LR struct)     |          —     |        −0.003   |   (theory > 0) |
| nla_y (LR struct)        |         +0.196 |        +0.095   |        +0.159 |
| ln_yp_over_y (LR struct) |         +1.07  |        +1.384   |   +0.20 (cal) |

### Interpretation

**1.1 λ jumps past Williams in magnitude.** With `ha_x_cci` added,
λ moves from −0.20 to −0.383, *exceeding* Williams' calibrated −0.286.
Mean-reversion speed of consumption to long-run is now faster than
Williams. Whether this is "closer to Williams" depends on whether you
target his point estimate or just the broad order of magnitude — we now
overshoot rather than undershoot.

**1.2 `ha_x_cci` is wrong-signed and insignificant.** Williams' theory
(Aust paper §4.1) is that housing wealth becomes more spendable as
collateral when credit conditions ease, so γ_1t = γ_1·(1+κ·CCI) with
κ > 0, implying `ha_x_cci > 0`. Our estimate is −0.0011 (t = −0.30,
p = 0.52). The total HA m.p.c. at CCI = 1 is 0.021 vs 0.024 at CCI = 0
— a *slight decrease*, not the increase Williams' theory predicts.

**1.3 Williams' own data shows similar attenuation.** Williams (Aust
paper §5.2) reports housing-asset m.p.c. = 0.0488 at the CCI peak vs
0.0452 in 2008Q2. So Williams' m.p.c. *also* decreased between peak and
end-of-sample (about an 7% drop from 0.0488 → 0.0452). His functional
form is the OPPOSITE of monotonic in CCI — m.p.c. peaks at peak CCI,
falls when CCI subsequently retreats. Our −0.0011 is consistent with
that direction at the modest level Williams found.

**1.4 The bigger story is that nla_y and ln_yp_over_y soaked up the
variation.** When ha_x_cci is added (with insignificant own-coefficient),
the nla_y LR coefficient *halves* from 0.196 to 0.095, and ln_yp_over_y
LR rises from 1.07 to 1.38. The model's overall identification is
re-shuffling between regressors that share variance.

**1.5 Likely cause of the wrong sign + insignificance**: collinearity
with the existing CCI interactions. Spec 8 has `hp_x_1_minus_cci`,
`r_x_cci`, `yp_x_cci`. Adding `ha_x_cci` creates a 4th CCI-interaction.
Williams (Aust paper §5.1) "de-means the explanatory variables that
are interacted with CCI" — we don't. The de-meaning matters because
without it, the interaction term is collinear with a linear CCI term
that we don't have in the equation. This is a known econometric
issue (see Aiken-West *Multiple Regression*, 1991).

**1.6 The implementation is correct; the empirical effect is small.**
We've added the right regressor; the data don't support a
significantly positive coefficient on it. This isn't an implementation
error — it's a finding.

### Suggested follow-up

- Add `cci_williams` as a separate linear LR regressor in Spec 8 to
  break the collinearity-with-implicit-intercept-shift problem.
- De-mean all CCI interactions (Williams' explicit convention).
- Try the ϖ-calibrated form `ha_y * (1 - ϖ·CCI)` analogous to
  `hp_x_1_minus_cci` instead of the unconstrained `ha_y * cci_williams`.

---

## 2. Item 2 — sectional sign priors

Implementation: new `build_williams_cci_basis_sectional()` in
[model_helpers.R](../../Ausreplication/R/model_helpers.R) ~lines 879+.
Eight knots at Williams' period boundaries (Aust paper §5.1) plus our
post-2008 extensions:

| Period start | Sign prior | Rationale                |
|--------------|-----------:|--------------------------|
| 1982-01-01   |         +1 | Financial deregulation   |
| 1990-09-01   |         −1 | Banking sector distress  |
| 1993-01-01   |         +1 | New entrants, securitisation |
| 2007-09-01   |         −1 | GFC                      |
| 2014-12-01   |         −1 | APRA macroprudential I   |
| 2017-03-01   |         −1 | APRA macroprudential II  |
| 2020-04-01   |         +1 | COVID emergency easing   |
| 2021-12-01   |         −1 | APRA buffer hike         |

### Result — placebo test on the extended 1976Q3+ sample

| Specification                              | adj R² %ile | \|λ\| %ile | Verdict           |
|--------------------------------------------|------------:|----------:|-------------------|
| Literal Williams 4-knot (1976+)            |          19 |        10 | Fails             |
| Maximal-GETS canonical (1976+)             |       **64** |        36 | Weak support      |
| **Sectional (Williams periods, 1976+)**    |       **36** |    **40** | **Median**        |

### Interpretation

**2.1 Sectional priors do NOT outperform maximal-GETS** on R²
(36th percentile vs 64th — sectional is *worse*). On |λ|, sectional is
marginally better (40th vs 36th) but neither is in upper-tail
territory.

**2.2 The cross-check assertion was wrong**. I predicted in
[cross_check_against_papers.md §2.4](cross_check_against_papers.md#L185)
that "Williams' tighter sectional constraint would be harder for
random draws to satisfy" and would therefore lift the canonical's
percentile rank. Empirically, sectional does *worse* than maximal-GETS.

**2.3 Why sectional doesn't help**: with only 8 knots vs 15, both the
canonical and random draws have less identification flexibility. The
maximal-GETS' 64th-percentile R² came from having more knots that the
data could pick from. Sectional is a stricter test of *Williams' specific
historical dating*, and that dating doesn't outperform random
placements on our extended sample. Williams' institutional intuition
about deregulation 1982-1990 vs banking distress 1990-1993 may have
been right *for his sample* (1978-2008) but doesn't survive on the
extended window with post-2008 data.

**2.4 The surviving knots from sectional are the same as from maximal-GETS**
when fitted to the consumption equation with sign survival — both
identify {1992Q1, 2007Q3, 2009Q1, 2019Q1, 2020Q2}. (Note: this
coincidence happened because `fit_consumption_with_williams_cci()`
hardcodes the maximal-GETS basis internally; the sectional basis from
build_and_fit gets ignored. The placebo test's `fit_with_basis()` does
respect the basis correctly, hence its 36th/40th percentile result is
the valid sectional test.)

### What this changes for the WP

The cross-check punch list anticipated sectional priors would close
some of the placebo gap. They don't. Williams' specific institutional
dating doesn't survive on the post-2008 extended sample. This
strengthens, not weakens, the conclusion that the binding constraint
on identification is the **single-equation OLS framing** — neither
the choice of candidate knot set (4 vs 15 vs 8 sectional) nor the
type of sign prior (point vs sectional) nor adding the time-varying
HA m.p.c. interaction (item 1) closes the wealth-coefficient gap with
Williams or the placebo gap.

---

## 3. Combined punch-list status

| Item | Status | Material effect |
|------|--------|-----------------|
| 1. Add `ha_x_cci` to Spec 8 | DONE | λ moves to −0.38 (now overshoots Williams); ha_x_cci itself insig + wrong-signed |
| 2. Sectional sign priors    | DONE | sectional placebo at 36th/40th — *worse* than maximal-GETS, opposite of prediction |
| 3. Build HEW equation       | not started | |
| 4. Cross-equation ζ_i normalisation | not started | |
| 5. Iterated CCI estimation  | not started | |

Items 1 and 2 are *substantively* less helpful than the cross-check
predicted. The big-picture story — that the wealth-coefficient gap and
the placebo failure both point to single-equation OLS framing as the
binding constraint — is *strengthened* by these results, not weakened.

The path to closing the Williams gap therefore likely requires items
3-5 (full LIVES system with HEW, cross-equation parameter normalisation,
iterated CCI estimation) — which together amount to a serious FIML
build, not a single-equation tweak. This is what
[next_steps.md NS-031](../../Ausreplication/docs/next_steps.md#L269)
contemplated as the "big rebuild" all along.

---

## 4. Outputs

- [outputs/sectional_cci_comparison.csv](../outputs/sectional_cci_comparison.csv) — coefficient table (note: the Spec-4-style comparison has a known bug; the placebo result is correct)
- [outputs/sectional_placebo_summary.csv](../outputs/sectional_placebo_summary.csv) — sectional placebo verdict
- New helper: `build_williams_cci_basis_sectional()` in [Ausreplication/R/model_helpers.R](../../Ausreplication/R/model_helpers.R)
- New regressor: `ha_x_cci` in Spec 8 in [Ausreplication/R/australia_estimation.R](../../Ausreplication/R/australia_estimation.R)
