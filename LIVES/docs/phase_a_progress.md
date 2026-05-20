# Phase A — progress log

Progress against the multi-equation system estimation plan in
[multi_equation_plan.md](multi_equation_plan.md). Newest entries
first.

---

## A4 — De-meaned CCI interactions

**Status:** done.

### What changed in the code

`Australia/R/australia_estimation.R`, inside `run_all_specifications()`,
the Spec 8 mutate block now de-means each variable interacted with
`cci_williams` on the Spec 8 estimation sample before forming the
interaction:

```r
spec8_mask <- !is.na(model_data$cci_williams) &
              model_data$date >= as.Date("1980-01-01") &
              model_data$date <= sample_end
ha_mean <- mean(model_data$ha_y[spec8_mask],         na.rm = TRUE)
hp_mean <- mean(model_data$ln_hp_over_y[spec8_mask], na.rm = TRUE)
r_mean  <- mean(model_data$real_rate[spec8_mask],    na.rm = TRUE)
yp_mean <- mean(model_data$ln_yp_over_y[spec8_mask], na.rm = TRUE)

md8 <- model_data %>%
  mutate(
    r_x_cci          = (real_rate    - r_mean)  * cci_williams,
    hp_x_1_minus_cci = (ln_hp_over_y - hp_mean) * (1 - 1.2 * cci_williams),
    yp_x_cci         = (ln_yp_over_y - yp_mean) * cci_williams,
    ha_x_cci         = (ha_y        - ha_mean) * cci_williams
  )
```

The motivation is Williams' Aust paper §5.1 convention: "credit
conditions index (CCI) interacts with (de-meaned) key variables."
Without de-meaning, each `X · CCI` interaction is collinear with an
implicit linear `CCI` level term that Spec 8 does not include
separately. The de-meaned form makes the interaction coefficient
interpretable as the change in the slope of `X` on `Δlog c` when
CCI moves by 1 unit, evaluated at `X = X̄` in-sample.

### Headline result — the predicted sign flip does *not* happen

| Coefficient | Pre-demeaning | Post-demeaning | Williams sign prior |
|---|---:|---:|:-:|
| λ (`ecm_lag`)         | −0.383 | −0.377 | − |
| `ha_y`                | +0.0093 (t = 2.29) | +0.0104 (t = 2.21) | + |
| `ha_x_cci`            | −0.0011 (t = −0.65) | **−0.0020 (t = −0.29)** | **+** |
| `nla_y`               | +0.0365 (t = 4.20) | +0.0344 (t = 3.01) | + |
| `eq_y`                | +0.0187 (t = 0.84) | +0.0126 (t = 0.70) | + |
| `super_y`             | +0.0067 (t = 0.93) | +0.0076 (t = 1.13) | + |
| `hp_x_1_minus_cci`    | +0.0066 (t = 1.75) | **+0.0046 (t = 0.40)** | − (on composite) |
| `r_x_cci`             | +0.0018 (t = 1.33) | +0.0016 (t = 1.04) | − |
| `ln_yp_over_y`        | +0.530 (t = 2.46) | +0.535 (t = 2.62) | + |
| `yp_x_cci`            | −0.625 (t = −1.24) | −0.665 (t = −1.66) | + |
| adj R²                | 0.798 | 0.798 | |
| BIC                   | −930.6 | −930.3 | |

The hypothesis behind A4 — articulated in
[cross_check_against_papers.md §2.1](cross_check_against_papers.md)
and [items_1_and_2_findings.md §1.5](items_1_and_2_findings.md) —
was that the wrong-signed and insignificant `ha_x_cci` coefficient
was likely an artefact of the missing de-meaning convention, since
de-meaning would remove the implicit linear-CCI confound that biased
the interaction toward absorbing a level effect.

The empirical result does *not* support that hypothesis. `ha_x_cci`
stays wrong-signed (−0.0020 vs the prior + sign) and becomes *less*
significant, not more (t-statistic moves from −0.65 to −0.29). The
LIVES theoretical prediction — that the housing-wealth m.p.c. rises
with CCI — receives no additional empirical support from the
de-meaning correction.

### What did improve

The cleanest improvement is on the house-price composite. Pre-demean
`hp_x_1_minus_cci` was wrong-signed at marginal significance
(t = +1.75, p = 0.08); post-demean it is wrong-signed but not
distinguishable from zero (t = +0.40, p = 0.69). Mechanically this
is what the de-meaning is supposed to fix: the original positive
coefficient was partly absorbing a CCI level shift that disappears
once the interacted variable is centred.

`yp_x_cci` moves slightly further into wrong-sign territory
(t from −1.24 to −1.66), and `ln_yp_over_y` becomes more significant
in the right direction (t from 2.46 to 2.62). The two changes
together imply a redistribution of long-run permanent-income
identification toward the base coefficient and away from the
CCI-interaction.

`r_x_cci` is essentially unchanged.

### Structural γ profile

Implied γ values on the de-meaned Spec 8, computed as OLS / |λ| with
|λ| = 0.377:

- γ_HA      = 0.028
- γ_IFA     = γ_EQ + γ_SUPER = 0.054
- γ_NLA     = 0.091

These can be compared with Williams' Table 1 calibrated values
(γ_HA = 0.0488 at CCI peak; γ_IFA = 0.022; γ_NLA = 0.159) and with
the preferred Spec 6 of the headline paper (γ_HA = 0.049,
γ_IFA = 0.030, γ_NLA = 0.196). Spec 8 with de-meaning sits below
Williams on housing wealth and net liquid assets and above Williams
on illiquid financial assets — a mixed picture that does not
systematically move toward Williams.

### What this means for the multi-equation plan

A4 was the lowest-effort item in Phase A and was expected — per the
cross-check punch list — to be a candidate for the "single biggest
fix" toward closing the wealth-coefficient gap with Williams (the
plan in [multi_equation_plan.md §3](multi_equation_plan.md) flagged
it explicitly as such). The empirical result rules that hypothesis
out: de-meaning by itself does not deliver the predicted sign flips.

This strengthens (rather than weakens) the substantive case for
the more substantive Phase A items A2 (ζ_h = 1 normalisation) and
A1 (HEW equation), and ultimately Phase B (FIML with shared ϖ).
The residual gap with Williams' Table 1 is structural to the
single-equation framing in a way that the de-meaning convention
alone cannot fix.

### Working-paper impact

The headline single-equation paper §8.4 has been updated with the
post-demeaning Spec 8 numbers and a sentence explaining the
de-meaning convention. The substantive narrative — that Spec 8
re-allocates wealth identification across components without moving
the γ profile toward Williams' Table 1 — is unchanged. Specific
numbers updated in the WP: λ = −0.383 → −0.377; Spec 8 γ profile
(0.024, 0.066, 0.095) → (0.028, 0.054, 0.091); §8.4 interaction
coefficient table refreshed.

### Outputs

The full Spec 8 coefficient vector is in
[australia_full_results.csv](../../Australia/outputs/australia_full_results.csv)
filtered to `Spec8_CCI_Interactions`. The Williams comparison
remains anchored on Spec 6 in
[australia_williams_comparison.csv](../../Australia/outputs/australia_williams_comparison.csv).

---

## A3 — Iterated CCI estimation

**Status:** done.

### What changed

`fit_consumption_with_williams_cci()` in
[`Australia/R/australia_estimation.R`](../../Australia/R/australia_estimation.R)
now iterates fit → drop sign-violators → refit until the surviving
set is stable, up to a `max_iter = 10` cap. Previously the function
did a single drop-and-refit pass.

### Empirical result

On the 1988+ sample with 15 candidate knots:

- **Iteration 1** drops 8 sign-violators (sdmma_1990_09,
  sdmma_1993_01, sdmma_1998_09, sdmma_2008_12, sdmma_2014_12,
  sdmma_2017_03, sdmma_2019_09, sdmma_2021_12).
- **Iteration 2** drops 2 more (sdmma_1992_01, sdmma_2007_09)
  — these passed iter 1 but became sign-violators with the
  iter-2 coefficient estimates, after the iter-1 violators were
  removed.
- **Iteration 3** finds no new violators; loop exits.

Final surviving set: **3 knots — sdmma_2009_01, sdmma_2019_01,
sdmma_2020_04** (FHB Boost, Hayne Royal Commission, COVID/JobKeeper).

Previously the single-pass version retained 5 knots
(1992Q1, 2007Q3, 2009Q1, 2019Q1, 2020Q2). The iteration drops
1992Q1 and 2007Q3.

Downstream effect on Spec 8: λ moves from −0.377 (post-A4) to
**−0.445**, further past Williams' −0.286 in magnitude. Spec 6 is
unchanged (it does not use `cci_williams`).

---

## A1, A2, A6, A7 — Four-equation system with HEW, ζ-normalisation, joint survival

**Status:** done.

### What changed

- **A1.** New HEW proxy in [`LIVES/R/lives_data_prep.R`](../R/lives_data_prep.R):
  `hew_proxy = Δ(fin_loans_proxy) / ydi_ann_nom`. Williams' literal
  definition includes a dwelling-investment subtraction that is not
  in the current public-data master; the proxy uses the credit-flow
  dimension only and the caveat is documented.
- **A2.** [`LIVES/R/joint_cci_identification.R`](../R/joint_cci_identification.R)
  now constructs three CCI variants from joint-surviving knots:
  `cci_williams_joint` (consumption-weighted, legacy),
  `cci_williams_joint_h` (HP-weighted — Williams' ζ_h = 1), and
  `cci_williams_joint_m` (mortgage-weighted).
- **A6.** New [`LIVES/R/lives_sur_4eq.R`](../R/lives_sur_4eq.R)
  estimates the four-equation system under SUR with all three CCI
  variants plus a baseline (cci_williams).
- **A7.** Joint sign-survival in
  [`joint_cci_identification.R`](../R/joint_cci_identification.R)
  now requires survival across all four equations (C ∩ H ∩ M ∩ W);
  the 3-equation survivors are retained as a diagnostic.

### Empirical results

**Joint sign-survival collapses dramatically as equations are added:**

| Survival regime                          | Surviving knots                                                    | n |
|---|---|---:|
| Consumption only (iterated, this paper)  | 1979Q1, 1986Q1, 1992Q1, 2007Q3, 2017Q1, 2020Q2                     | 6 |
| Three-equation joint (C ∩ H ∩ M)          | 1986Q1, 2017Q1                                                     | 2 |
| **Four-equation joint (C ∩ H ∩ M ∩ W)**    | **1986Q1**                                                         | **1** |

Sdmma_2017_03 sign-violates in the HEW equation (HEW positively
loads APRA tightening, against the institutional prior of negative).
Only 1986 financial deregulation survives the four-equation test.

**A2 ζ_h = 1 normalisation becomes inert with one surviving knot.**
All three weighted variants (cons-, HP-, M-) are mathematically
identical after peak-normalisation when based on a single knot:

```
cor(cci_williams_joint, cci_williams_joint_h) = 1.0000
cor(cci_williams_joint, cci_williams_joint_m) = 1.0000
```

**4-eq SUR residual correlation under joint CCI:**

```
         C       H       M       W
C    1.000  -0.107  +0.090  +0.042
H   -0.107   1.000  -0.194  -0.183
M   +0.090  -0.194   1.000  +0.832
W   +0.042  -0.183  +0.832   1.000
```

The mortgage and HEW residuals correlate at **+0.83** — the two
equations are not separately identifying signals under the proxy
HEW construction.

**CCI sign behaviour across regimes (mortgage and HEW equations):**

| Regime                  | M-eq CCI (t)   | HEW-eq CCI (t) |
|---|---:|---:|
| (a) cci_williams         | +0.0003 (+0.13) | −0.0001 (−0.07) |
| (b–d) joint variants     | −0.0086 (−1.43) | −0.0067 (−1.33) |

Joint identification does not fix the M-equation sign violation
(remains wrong-signed). HEW-equation CCI loading is also wrong-
signed under joint identification.

---

## Phase B — Williams' Table 1 calibrations as Wald restrictions

**Status:** done.

### What changed

New [`LIVES/R/williams_calibration_test.R`](../R/williams_calibration_test.R)
refits Spec 6 on the canonical Italy-LP master and tests
Williams' six Table 1 calibrations as linear restrictions on the
OLS coefficient vector using `car::linearHypothesis` with the
Newey–West vcov.

### Headline result

**Williams' Table 1 calibrations are formally rejected** as
parameter restrictions on the contemporary Australian data:

| Restriction                       | χ²    | df | p-value | Reject 5% | Reject 1% |
|---|---:|---:|---:|:-:|:-:|
| ha_y = 0.0488                     | 7.18  | 1  | 0.007   | ✓ | ✓ |
| eq_y = 0.011                      | 0.02  | 1  | 0.896   | ✗ | ✗ |
| super_y = 0.011                   | 2.26  | 1  | 0.133   | ✗ | ✗ |
| nla_y = 0.159                     | 2.55  | 1  | 0.110   | ✗ | ✗ |
| ln_hp_over_y = −0.130             | 2.20  | 1  | 0.138   | ✗ | ✗ |
| ln_yp_over_y = 0.200              | 1.23  | 1  | 0.268   | ✗ | ✗ |
| **Joint wealth (4 restrictions)** | 10.03 | 4  | 0.040   | ✓ | ✗ |
| **Joint all (6 restrictions)**    | 29.10 | 6  | <0.001  | ✓ | ✓ |

The single individual rejection is the housing-wealth m.p.c.
γ_HA = 0.0488. Yet the *implied* γ_HA from Spec 6 is 0.049 —
essentially equal to Williams' value. The two statements are
simultaneously true because the implied-γ test divides OLS by |λ|
(and our OLS is 37 % short and our |λ| is also 37 % short, so they
cancel), while the Wald restriction tests the OLS coefficient
directly at the Williams γ × λ̂ point. See
[`companion_paper_draft.md` §7](companion_paper_draft.md) for the
reconciliation.

---

## Summary — Phase A and Phase B both complete

The full chain A1 → A2 → A3 → A4 → A6 → A7 → B2 is implemented and
documented. The substantive findings:

1. **A4 (de-meaning)** — does not flip ha_x_cci's wrong sign;
   reduces magnitude of wrong-signed `hp_x_1_minus_cci`.
2. **A3 (iterated CCI)** — reduces surviving knots from 5 to 3;
   Spec 8 λ moves to −0.445.
3. **A7 (4-eq joint survival)** — reduces survivors from 2 to 1
   knot (1986 alone).
4. **A2 (ζ-normalisation)** — inert with one surviving knot.
5. **A1 + A6 (HEW + 4-eq SUR)** — HEW residuals correlate 0.83
   with mortgage residuals; the proxy HEW is not a separately
   identifying signal.
6. **B2 (Williams calibration Wald)** — Williams' six Table 1
   calibrations are jointly rejected χ²(6) = 29.1, p < 0.001;
   wealth-only subset rejects at 5 % (χ²(4) = 10.03, p = 0.040).

The next-step decision is in §8.2 of the
[companion paper draft](companion_paper_draft.md): is the
four-equation LIVES system genuinely not the right model for
contemporary Australia, or is the proxy HEW the binding
constraint? Either reading is consistent with the evidence; the
discriminating step is sourcing a properly constructed HEW series
with dwelling-investment subtraction.

The path to full FIML (Phase B item B1 — custom likelihood with
shared ϖ) is conditional on that decision: with only one surviving
knot, FIML has very little cross-equation parameter space to
share, and the months-of-work commitment may not deliver
proportionate empirical leverage until the candidate knot set is
strengthened or the HEW equation is reconstructed on better data.
