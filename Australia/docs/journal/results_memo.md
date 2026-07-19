# Results memo — binding editorial decisions for the manuscript

**Date:** 2026-07-19 · **Author role:** econometric adjudicator (plan items B1–B5, C1–C3)
**Plan of record:** `review/journal_review_2026-07.md`
**Status of numbers below:** every figure was re-read from its named CSV in `Australia/outputs/` (the four agent reports were NOT trusted; all headline numbers reconciled exactly).

These decisions are **binding** for the manuscript revision. Where an earlier claim is retracted, the retraction is mandatory, not optional.

---

## (1) Long-run / equilibrium / structural-gamma language — what is permitted

**Source CSVs:** `australia_bounds_test.csv`, `australia_cy_stationarity.csv`.

**Two findings that must be stated together:**

- A PSS (Pesaran–Shin–Smith) bounds test **conclusively supports a level relationship** for both estimated specifications. Spec 6 (preferred): F = 5.591 and t = −4.931; Spec 11 (headline): F = 9.572 and t = −5.083 — both F statistics clear the I(1) 5% upper bound of 3.30 and both t statistics clear the I(1) 5% bound of −4.88 (`australia_bounds_test.csv`). Verdict: **cointegrated** for both.
- The paper's previously *asserted* fallback — that the imposed unit-income consumption/income ratio is itself a stationary equilibrium error — **is false and must be retracted.** ADF and KPSS agree unanimously that `ecm_lag = ln(cons_{t-1}) − ln(income_t)` is **nonstationary in every window**: ADF = −2.33 (full), −2.43 (Spec 6), −2.50 (Spec 11), all above their ~−2.88 5% critical values; KPSS = 0.997, 0.849, 0.783, all above 0.463 (`australia_cy_stationarity.csv`).

**Permitted framing (use language of this form, not stronger):**

> "A level (cointegrating) relationship between consumption and its long-run determinants is supported by a bounds test for both the preferred and the headline specifications (F = 5.6 and 9.6; t = −4.9 and −5.1, both beyond the I(1) upper bounds). This relationship is carried by the **full** long-run regressor set — wealth, housing, credit conditions and permanent income acting jointly — and **not** by the imposed unit-income consumption/income ratio, which is not stationary on its own (ADF and KPSS both reject in every window)."

**Consequences (binding):**
- The earlier "Engle–Granger fails everywhere ⇒ no long-run relation" statement was **too strong** and must be replaced by the bounds-test result above.
- The claim that the imposed unit-income vector delivers a stationary equilibrium error must be **deleted**; the equilibrium is empirical (bounds-supported), not definitional.
- **Spec 12 (calibrated) may NOT claim any bounds verdict.** Its long-run vector is partly hard-calibrated (γ_ifa = 0.022, ψ₀ = 0.20, ψ₁ = 0.93 enter as a fixed offset, not free coefficients), so the PSS UECM is not well-defined; the primary row is recorded **infeasible** and the free-regressors-only diagnostic is **inconclusive** (F = 4.245, between the 3.23/4.35 bounds) (`australia_bounds_test.csv`). Spec 12's long run must be described as **imposed/calibrated, not tested.**
- "Equilibrium" and "long-run" language is therefore **permitted for Spec 6 and Spec 11**. "**Structural-gamma**" magnitude language is governed separately by item (4) below and is largely **not** permitted.

---

## (2) Form vs sample — how much of "form is decisive" survives

**Source CSV:** `australia_spec11_ablation.csv` (baseline reproductions confirmed: Spec 6 λ = −0.2386, n = 86; Spec 11 λ = −0.4483, n = 146 — both MATCH the committed results).

**Decomposition of the adjustment speed λ (baseline Spec 6 = −0.239 → baseline Spec 11 = −0.448; total gap = −0.209):**
- **Form alone**, on Spec 6's own n = 86 window (Cell A): λ = −0.542 (t = −3.81). Shift of −0.303 — *larger in magnitude than the entire observed gap.*
- **Sample/CCI-series alone**, on Spec 6's own form (Cell B): λ = −0.262 (t = −2.49). Shift of only −0.023 — an order of magnitude smaller.

**Sentence the manuscript MAY print (for λ):**

> "Isolating functional form on the preferred specification's own 86-quarter window moves the estimated adjustment speed from −0.239 to −0.542 — a larger movement than the entire gap to the headline estimate of −0.448 — whereas holding the form fixed and only extending the sample and switching the credit-conditions series moves it to just −0.262. The change in adjustment speed is driven by functional form, not by the larger sample."

**Mandatory qualifications (must accompany the sentence):**
- For the **disaggregated wealth channels' significance** (`nla_y`, `ilfa_y`), form and sample are **complements, not substitutes**: form alone gets the sign and rough magnitude but does **not** reach conventional significance on n = 86 (Cell A t = 1.45 and 2.09) and only crosses thresholds once the sample is also extended (baseline Spec 11 t = 3.75 and 3.09). The manuscript may **not** claim "form alone delivers significant disaggregated wealth channels."
- Cell B is **form-approximate, not form-identical**: `cci_williams` spans negative values (a max-scaled spline index), so Spec 6's Δ²log(CCI) transform is undefined and a level second-difference was substituted (`australia_spec11_ablation.csv`, `note`). State this; do not present Cell B as an exact form-swap.
- Net permitted claim: **"form is decisive for the adjustment speed"** survives; **"form alone activates the wealth channels"** does not.

---

## (3) Credit-channel (CCI) identification — what may be claimed

**Source CSV:** `australia_cci_placebo_spec11_summary.csv` (198 finite draws of a random-knot placebo, deployed through the full multiplicative Spec-11 construction).

Real institutional Spec-11 CCI vs the placebo distribution:
- adj. R² = 0.8244 → **93rd** percentile (placebo median 0.761)
- logLik = 532.20 → **93rd** percentile (median 509.68)
- BIC = −954.75 → **93rd** percentile (median −909.72; lower is better)
- |λ| = 0.4483 → **98th** percentile (median 0.240)
- Joint Wald F on the 5-term CCI block = 6.88 → **94th** percentile (median 2.32)

**Permitted claim:**

> "Carried through the full multiplicative Spec-11 construction, the institutionally-timed credit-conditions index ranks at the 93rd percentile of a 198-draw random-knot placebo on model fit, at the 94th percentile on the joint significance of the credit-conditions block (Wald F = 6.9 vs a placebo median of 2.3), and at the 98th percentile on the estimated adjustment speed. The institutional knot placement is therefore doing genuine identifying work at the deployed stage — not merely at the additive spline-selection stage validated earlier."

**Binding limits:** this is **distributional (percentile) evidence that the CCI series as a whole is informative.** It is **not** evidence that any individual credit-interaction channel is identified — those fail under (4). Do not slide from "the CCI block ranks at the 94th percentile" to "the housing×CCI (or rate×CCI) channel is significant." Item B4 / Referee-1 finding 4 is now **closed** (the placebo tests the multiplicative Spec-11 object, not just the additive stage).

---

## (4) Inference — nested-bootstrap CIs are now the HEADLINE intervals

**Source CSV:** `australia_nested_bootstrap_ci.csv` (B = 199; CCI-knot **and** ecm_lag/PI-construction uncertainty carried inside each draw; delta-method CIs demoted to an appendix).

**Coefficient-by-coefficient, nested 95% CI (structural γ = −β/λ unless noted):**

| Coefficient | Point | Nested 95% CI | Excludes 0? | Verdict |
|---|---|---|---|---|
| **λ (adjustment speed)** | −0.448 | **[−0.273, −0.077]** | **YES** | **Survives — sign and non-zero speed identified** |
| `nla_y` (γ) | 0.060 | [−0.174, 0.317] | no | Not identified (sign only) |
| `ilfa_y` (γ) | 0.035 | [−0.086, 0.164] | no | Not identified (sign only) |
| `ln_yp_over_y` (γ) | 1.024 | [−0.124, 1.601] | no | Not identified (sign only) |
| `yp_x_cci` (γ) | −1.138 | [−103.3, 24.5] | no | Not identified (magnitude meaningless) |
| `ha_x_cci` (γ) | 0.006 | [−3.66, 2.36] | no | Not identified (magnitude meaningless) |
| `hp_x_1_minus_cci` (γ) | 0.062 | [−0.074, 0.376] | no | Not identified (sign only) |

**Binding decisions:**
- **Only λ survives.** Its honest CI [−0.273, −0.077] excludes zero — the ECM adjustment speed's sign and non-zero magnitude are identified. Note the nested interval is centred well inside the delta-method interval ([−0.695, −0.202]) and the nested median |λ| ≈ 0.17 is attenuated relative to the 0.45 point estimate (a mechanical consequence of letting `ecm_lag` become a bootstrap-generated regressor); report λ's **sign and significance** as the robust finding, and flag the level attenuation.
- **All six structural γ channels are undistinguishable from zero** once knot-selection and PI/ecm_lag construction uncertainty are honestly carried. Every one must be walked back to **"sign only, magnitude unidentified."**
- **Every "reject Williams' calibration" point comparison built on these γ must be retracted.** Williams' calibrated value lies **inside** the nested CI for `nla_y`, `ilfa_y`, `ln_yp_over_y`, `yp_x_cci`, and `ha_x_cci` (`williams_in_nested_ci = TRUE`); the paper **cannot** reject Williams on any of these. (For `hp_x_1_minus_cci` Williams' −0.13 sits just outside the CI, but the channel itself is indistinguishable from zero, so no "reject" claim is available there either.)
- The delta-method intervals are anti-conservative by 5–6× for the wealth/PI γ and by 66–189× for the two ratio-heavy CCI-interaction γ (`australia_nested_bootstrap_ci.csv`); they may appear only as an **appendix** comparison, never as the headline.

---

## (5) Permanent income — real-time is the inferential basis (plan B3)

**Source CSVs:** `australia_pi_realtime_robustness.csv` (Spec 6 window, n = 86), `australia_joint_pi_robustness_spec11.csv`.

**The sign flip (headline numbers):**
- Full-sample PI measure (`Italy_fullsample`): ln(yp/y) coefficient = **+0.325** (t = **1.50**) — positive but **insignificant**.
- Real-time PI measure (`Italy_realtime`): ln(yp/y) coefficient = **−0.145** (t = **−2.23**) — **negative and significant**.
- AR real-time variant (`AR_realtime`): yp coefficient = **−0.158** (t = **−1.68**) — negative, marginally significant; also λ attenuates to −0.095 (t = −1.68).

**Binding framing (per plan B3):** the **real-time (causal) measure is the inferential basis.** The full-sample measure is **descriptive only** and must be labelled as such.

**Permitted sentence:**

> "Under the real-time, causally-dated permanent-income measure the ln(yp/y) coefficient is negative and significant (−0.145, t = −2.23; AR variant −0.158, t = −1.68). The full-sample measure yields a positive but insignificant coefficient (+0.325, t = 1.50) and is reported as descriptive only. The sign of the permanent-income channel is therefore not robust to the information set, and no positive-sign structural claim may be made on it."

**Supporting point (may be stated):** treating permanent income as jointly determined (SUR) barely moves the Spec 11 estimates — ln(yp/y) 0.4591 → 0.4529 (−1.4%), yp×cci −0.510 → −0.498 (`australia_joint_pi_robustness_spec11.csv`) — so simultaneity is **not** what drives the full-sample result; the fragility is measurement/information-set, not endogeneity.

---

## (6) What the paper can now honestly claim — the manuscript's thesis (5 sentences)

The paper establishes that a single aggregate consumption equation with credit-conditioned housing collateral effects fits Australian data well and supports a genuine long-run (cointegrating) relationship for both the preferred and the LIVES-faithful headline specification (bounds F = 5.6 and 9.6; `australia_bounds_test.csv`), carried by the joint wealth/housing/credit/permanent-income vector rather than by any imposed unit-income restriction, which is itself nonstationary (`australia_cy_stationarity.csv`). The move to the LIVES functional form, not the larger sample, is what sharpens the estimated speed of adjustment (λ: −0.239 → −0.542 from form alone on the same window; `australia_spec11_ablation.csv`), and the institutionally-timed credit-conditions index carries real identifying content at the deployed stage (93rd–98th placebo percentiles; `australia_cci_placebo_spec11_summary.csv`). But once knot-selection and permanent-income-construction uncertainty are honestly propagated, **only the sign and non-zero magnitude of the error-correction speed survive** (nested 95% CI [−0.273, −0.077]); every one of the six structural wealth, permanent-income and credit-interaction elasticities is statistically indistinguishable from zero and its point magnitude is unidentified (`australia_nested_bootstrap_ci.csv`). Consequently the paper can make **no** claim to reject Williams' calibration on any individual channel — his values lie inside the honest intervals — and the permanent-income channel even reverses sign under the causal real-time measure (−0.145, t = −2.23; `australia_pi_realtime_robustness.csv`). The honest, near-unrejectable thesis is therefore methodological: **a single credit-conditioned aggregate equation can identify that Australian consumption error-corrects to a credit-sensitive long-run relation, but it cannot identify the magnitudes of the individual LIVES channels — the data support the form and the adjustment mechanism, not the structural coefficients.**
