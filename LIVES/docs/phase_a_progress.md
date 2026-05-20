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

## Next item

Per the recommended sequence in
[multi_equation_plan.md §4](multi_equation_plan.md), the next item
is **A3 — iterated CCI estimation**. With A4's finding that the
de-meaning convention does not move Spec 8 toward Williams, A3's
question becomes: does the iterated knot-selection procedure
identify a different surviving knot set, and does that change the
Spec 8 / phase 3 results?

After A3, the substantive question is whether A2 (ζ_h = 1
normalisation) can close the M-equation sign violation in phase 3,
which is the cleanest structural test of the multi-equation system
under SUR.
