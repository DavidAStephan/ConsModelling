# Deep audit — July 2026 (multi-agent)

**Date:** 2026-07-16 · **Method:** 15 finder agents (8 code / 2 mechanical / 5 econometrics) over the full pipeline, every critical/major claim independently adversarially verified by a separate verifier agent instructed to refute it. 31 agents, ~2.5M tokens. Prior June-2026 review (NS-120…NS-135) taken as baseline; only new defects or fix-regressions reported.

## Verdict

The June-2026 P0 fixes are, with one propagation failure, genuinely implemented and correct: real-time PI is causal, the MacKinnon EG critical values are right and honestly framed, the Spec-8 tables reconcile, the deployed-protocol placebo (84th percentile) recomputes exactly, and nearly every number in the abstract/§1 now traces to a committed CSV cell. All 70 unit tests pass and the committed outputs are fresh (June 12 rebuild).

However, the audit found **two new critical sign errors** — one of which inverts every number in the paper's lead §10.1 decomposition, which is additionally **non-reproducible from source** — plus a stale companion paper contradicting the committed CSVs, an internally-inconsistent IV robustness check for the headline spec, and one headline overclaim the paper's own appendix tables contradict.

---

## Critical (confirmed, high confidence)

### C1. §10.1 long-run decomposition is sign-inverted
`plot_longrun_decomposition()` computes contributions as `cf/λ` instead of the canonical `−cf/λ` used everywhere else (`build_results_table` :2325, `gamma_inference.R`). With λ<0 throughout (all 14 committed λs negative), **every driver's contribution to log(c/y) is sign-flipped** in both committed decomposition CSVs, and §10.1 quotes the flipped numbers verbatim as its policy narrative.
- Code: `australia_estimation.R:3807` (stale comment at :3796 preserves the old positive-λ convention).
- Independent numeric confirmation: sum-of-contributions correlates **−0.81 / −0.95** with the actual demeaned series in the Spec 3 / Spec 11 CSVs; residual variance exceeds signal variance 3–4×.
- Found independently by two auditors (code + econometrics); both verifications confirmed.

### C2. Sign error in the calibrated-offset fixed-point solver corrupts Spec 10 and Spec 12
`dlcons_adj <- dlcons - lambda_guess * offset_calib` (`australia_estimation.R:1398`, `:1541`) should be `+` under the codebase's own convention (β_OLS = −λ·γ, λ<0; confirmed from file header :12-19, :2325, and the committed CSV). The error plausibly **manufactures the paper's "calibration collapse" negative-control narrative** (λ collapsing toward zero / flipping sign) rather than that being an empirical result. Spec 10/12 must be re-run with the corrected sign before the negative-control story can stand.

---

## Major (confirmed, high confidence)

### M1. The §10.1 lead decomposition artifacts are non-reproducible from source
`australia_longrun_contributions_spec11.csv/.png` have **no generating code path**: `plot_longrun_decomposition()` writes hard-coded unsuffixed filenames and is called exactly once — on `preferred_spec` = Spec 3 (`:4457`), not Spec 11. The `file_suffix="_spec11"` mechanism exists only in `run_italy_style_robustness`. (Confirmed by two independent auditors.) Fix together with C1: add a suffix argument, call for Spec 11, regenerate.

### M2. Companion paper entirely stale — NS-125 never propagated
`LIVES/docs/companion_paper_draft.md` (last commit 2026-06-04) predates the June-12 rebuild that regenerated every LIVES output. Its headline Phase-B result still asserts **χ²(6)=2.24 (p=0.90)** at λ̂≈−0.197; the committed `williams_calibration_wald.csv` gives **χ²(6)=7.55 (p=0.27)** at λ̂=−0.2386 (also χ²(4)=1.83 vs quoted 1.07, ha_y χ²=1.54 vs 0.05). The main paper is correctly resynced; the companion contradicts it throughout abstract, §7.1–§7.3.

### M3. Spec 11 IV check violates its own endogeneity rule (`yp_x_cci` instruments itself)
The IV block's comment (:3088-3092) defines endogenous as "anything that contains CURRENT lincome", but the code (:3093) hard-codes only `ln_y_over_c`/`ecm_lag`/`ln_yp_over_y`. `yp_x_cci = (ln_yp_over_y − ȳ)·cci` mechanically contains current income yet lands in the exogenous/instrument block — an endogenous regressor used as its own instrument, while Wu–Hausman rejects exogeneity and Sargan is borderline. The §8.1 IV robustness claim for the headline spec is invalid as coded. (Found independently by two auditors.)

### M4. Income-measure robustness batteries substitute income only in the ECM term
The §8.7 (scaled-income) and §8.8 (NPY) batteries swap the alternate income series via `gsub` on `ln_y_over_c`/`ecm_lag` only (:3337-3356, :3417-3428), leaving `ln_yp_over_y`, `yp_x_cci`, `nla_y` and every other income-denominated ratio on the original ydi basis — while the paper narrates the coefficient shifts as reflecting the new income concept. (The code comment at :3354 admits the skip; the paper does not.)

### M5. §7.3.1's Spec-11 rejections of Williams' calibrations rest on admittedly anti-conservative CIs
The delta-method and residual-bootstrap CIs in `gamma_inference.R` hold the design matrix fixed (header lines 28-31 admit it), so no committed inference carries the generated-regressor / knot-pre-test uncertainty of CCI and PI. Converting Spec 6's imprecision-driven non-rejections into **active rejections** of Williams using these too-narrow intervals is an overclaim by the paper's own stated standard.

### M6. Abstract/§1.3 COVID-robustness overclaim
The abstract claims the wealth structure "remains [significant] under every COVID treatment" and §1.3 claims 5% significance "in the COVID-controlled variants" — but the pre-COVID variant (one of the three headline variants) has `nla_y` t=1.81 and `ilfa_y` t=1.74, only 10%-significant, exactly as the paper's own Tables B.1/B.4 flag with single asterisks. Prose contradicts the paper's own tables.

---

## Confirmed minor / notable

- **NS-124 residue:** OOS CCI interactions attached once with full-sample de-meaning/knot fit (`oos_forecast.R:523`, `sample_end=NA`), never rebuilt per window; guard is a `warning` not the specified hard `stop()` (:101). Disclosed in WP Appendix C.4, hence minor — but the "real-time" OOS label overstates.
- **ψ-bound refinement (updates a known-open issue):** ψ≈1.02 is a genuine OLS estimate, not an algebra error — but it is computed on a PI regressor rescaled by an ad-hoc GFC "learning ogive" absent from the stated formula, and its 95% CI **contains 0.95**: the breach is a point-estimate artifact, not significant evidence.
- **Placebo scope gap:** the featured 84th-percentile placebo tests the *additive* detrending spline (λ=−0.246), not the headline Spec 11 interaction structure (λ=−0.448). The credit-channel identification the paper leads with is placebo-untested.
- **"CCI flat pre-2007" is mechanical:** `smoothed_step` is identically zero before the first surviving knot; flatness is knot-support artefact, not evidence of stable pre-2007 credit conditions.
- **Residual unit error in `ln_hp_over_y`:** the 643201 house-price input is ABS $'000, never rescaled — hpi understated ×1000 throughout. Currently inert (intercepts + explicit de-meaning), but it sits inside the exact variable whose fix headlined the June review.
- **`williams_comparison.R` ψ-at-peak row conflates concepts:** Williams' ψ at CCI=1 (0.95) is compared against Spec 6's base + *post-2008 break dummy* sum (0.496), publishing a meaningless 82%/119% gap in the committed CSV (`ours_source` note discloses the construction; the paper does not quote the gap). Verified directly.
- **Normalization mismatch:** `cci_method_comparison.R`/`cci_alternatives.R` reconstruct `cci_williams` under a different normalization than the deployed pipeline — committed comparison charts have wrong amplitude/shape and a false "peak-normalised to ±1" caption (correlations quoted in text are scale-invariant, unaffected).
- **Latent wrong-CV bug:** `fit_long_run_spec()`/`fit_dols_spec()` (`model_helpers.R:338`) test cointegration against univariate DF CVs — the exact bug `eg_mackinnon_cv()` fixed. Currently dead code; disarm before reuse.
- Spec 9 (Kalman CCI) interactions never attached to global `model_data` → silently exempt from the cointegration screen (:1890). `EXPECTED_SIGNS` has no CCI-interaction entries, so the wrong-sign flag never fires on the LIVES mechanism terms (:2242). Preferred spec selected twice by two functions with different fallbacks (:4425). D02 zero-growth splice contaminates Spec 6b's `d2_log_creditd02_lag2` at 2019Q4/2020Q1. NS-125 Wald treats λ̂ as a fixed constant (discards its sampling variance). Test coverage: 8/42 `model_helpers` functions tested; core fitting/PI/CV/CCI-orientation functions untested. `review/action_plan.md` still records superseded λ/χ² values. Abstract quotes ψ-breach magnitude and PI OLS/t ranges slightly narrower than committed numbers. Stale window description ("1988Q3–2024Q4") inside `williams_comparison.R` vs actual 2003Q3+ complete-cases window. `LIVES/joint_cci_identification.R:250` unguarded NA-propagation path; the 4-eq joint CCI degenerates to a single surviving knot.

## Refuted by verification (do not act on)

- **`prime_age_share` OOS leakage** — spline is full-sample, but quantified impact on the committed series is negligible; not material to Spec 7's OOS ranking.
- **"Missing `master_data.csv`"** — exists at `Australia/data_raw/master_data.csv`; the auditor resolved the path from repo root instead of the Australia project root.
- **SSM/signal-extraction test gaps at claimed severity** — the five functions are dead code (never called anywhere); the 8-function coverage gap is real but is a minor risk, not critical.

## Verified clean (regressions checked, none found)

NS-120 real-time PI (both constructors traced leak-free, boundary conditions correct) · NS-121 MacKinnon CVs (values, n_vars=k+1 keying, CSV match) · NS-122/123 Spec-8 λ=−0.458 and knot tables reconcile · NS-124 column-drop fix (Spec 8 vs 9 OOS RMSEs now differ) · NS-125 in code and main paper · NS-130 placebo protocol identity + 84th/80th percentiles recomputed · M3 $bn→$m · ln_hp_over_y real/real construction · CSV round-trip · HAC, Chow battery, spec selector, Spec 11 plumbing · LIVES sign conventions and SUR samples · ρ̂=−0.013 consistent everywhere · outputs fresh · 70/70 tests pass.

## Recommended sequence

**P0 (change headline artifacts):** fix C1 sign + add `file_suffix` to `plot_longrun_decomposition`, regenerate both decompositions (§10.1 rewrite); fix C2 offset sign, re-run Spec 10/12 and re-assess the "calibration collapse" narrative; fix M3 endog set (`yp_x_cci`, and audit `ha_x_cci` etc. under the same rule), re-run §8.1; correct the M6 abstract/§1.3 sentence to match Tables B.1/B.4.
**P1:** regenerate the companion paper from current CSVs (M2); full-substitution income batteries or honest disclosure (M4); soften §7.3.1 rejections per M5; run the deployed-protocol placebo on the Spec 11 interaction structure.
**P2:** the minors above, led by the $'000 hpi rescale, the Spec 9 plumbing, and the hard `stop()` guard.

*Caveat: each critical/major finding was confirmed by one adversarial verifier (two criticals by two independent finders + verifiers); minors are finder-reported, spot-checked only where noted.*
