# Plan of action — Australia LIVES consumption model

**Audit verdict (independently confirmed):** an honest, dataset-rich, near-RDP-grade study whose headline "puzzle resolved / matches Williams" narrative rests on (a) a look-ahead-biased permanent-income regressor and (b) a string of paper-vs-CSV number mismatches. **Major revision required; not yet MARTIN-ready.** Every critical claim below was checked against the committed code and CSVs and is marked CONFIRMED with its evidence.

This plan **extends the existing structure** in `Australia/docs/next_steps_plan_2026.md` (Tiers A–D) and the `next_steps.md` NS-### backlog rather than inventing a new scheme. The existing Tier A is marked "done / editorial only" — that judgement is now **wrong**: the audit shows the empirical content is *not* settled (look-ahead PI, wrong cointegration CVs, OOS bug) and the editorial numbers are *not* reconciled. P0/P1 below re-open Tier A and insert new NS-IDs (NS-120…NS-135) continuing the existing numbering.

---

## P0 — Correctness & integrity (could CHANGE the headline; do first)

These are confirmed defects that either change a headline number or invalidate inference. Nothing else should ship until they are resolved.

> **Status update (2026-06-04, branch `p0-correctness-fixes`).** NS-121, NS-122, NS-123, NS-125 are **DONE** (code fixed, pipeline + Wald test re-run, papers/guide reconciled, tests green, headline λ=−0.180 still reproduces). **Two results changed materially:**
> - **NS-121 (cointegration):** under correct Engle–Granger MacKinnon critical values, **no** specification clears the cointegration screen (Spec 6 ADF −3.22 vs CV −5.47; all `pass_coint` now FALSE). The single-equation long run is *not* formally established — WP §6.2/§6.3 updated.
> - **NS-125 (Wald):** the sign-convention bug was driving the *entire* rejection. Corrected, Williams' Table 1 calibrations are **NOT rejected** — χ²(6) = 2.24 (p = 0.90), was 29.10 (p < 0.001). The companion paper's Phase B thesis is reversed (now *consistency*, dissolving the M6 contradiction). Companion §1/§7/§8, `code_guide.md`, `phase_a_progress.md` updated.
> - **NS-122/123:** Spec 8 λ corrected to −0.445 (8 sites); §8.4 interaction table re-derived (`ha_y×CCI` is right-signed +0.0016, not the stated −0.0020); §5.1.1 knot table regenerated to the live 3 survivors; §5.3.1 reconciled (the "6" is the single-pass joint-analysis count, distinct from the iterated 3).
>
> **NS-120 (look-ahead PI) — DONE (author chose: keep full-sample LP, reframe as a measurement).** Added a causal `real_time=TRUE` expanding-window mode to `construct_permanent_income_italy()` and a diagnostic ([pi_realtime_diagnostic.R](../Australia/R/pi_realtime_diagnostic.R) → [australia_pi_realtime_robustness.csv](../Australia/outputs/australia_pi_realtime_robustness.csv)). **Finding:** the PI sign-flip is a look-ahead artefact — real-time PI coef = −0.105 (vs full-sample +0.244), and only ~half the λ gain survives (real-time λ = −0.118 vs full-sample −0.197 vs AR −0.051); Spec 6 loses no observations. **Per the author's decision**, the canonical headline keeps the full-sample Italy measure (λ=−0.180 unchanged) but the WP now frames it explicitly as a non-causal *measurement*, with a prominent look-ahead disclosure (§4.3) and the real-time variant as an operational robustness column (§7.4, §8.9). Abstract, §1.2, §1.3, §2.5, §9, §11.4 reframed; README limitation added. The real-time variant is the MARTIN-operational version (P3/NS-135).

### NS-120 — Make permanent income leak-free in the headline (resolves C1) · RDP + MARTIN
**Issue.** `PI_METHOD <- "italy"` (`australia_estimation.R:45`) drives the entire "puzzle resolved" story, and it is non-causal. `construct_permanent_income_italy` builds its target from *realised future income* (`fut <- inc[(t+1):(t+k)]`, lines 305–308), fits ONE full-sample OLS (`train_mask` spans all t, lines 347–359), then writes in-sample fitted values for every t including the ~40 tail quarters (lines 366–377). **CONFIRMED.** `australia_pi_method_comparison.csv`: leak-free AR `ln_yp_over_y` = **−0.0031** (insignificant) vs Italy **+0.240**; AR λ = **−0.083** vs Italy λ = **−0.193**. So the λ near-doubling and the PI sign-flip are method artefacts, undisclosed.
**Steps.** (1) Re-implement Italy LP as expanding-window: refit the LP using only data through each t, predict t in real time. (2) Re-run the headline; report in a new table how much of the λ gain and the PI flip survive in real time. (3) If they do not survive, demote Italy LP to a robustness column and carry the leak-free AR result as the headline; either way add an explicit "look-ahead vs real-time PI" subsection. (4) Fix the internal contradiction: the abstract/§1 (lines 137, 196, 2354) call the AR PI "significantly negative −0.20 / flips from −0.20 to +0.30" but the paper's own §9 table (line 1755) and the CSV give −0.003 insignificant.
**Effort.** 2–3 days. **Depends on.** none. **Gates** NS-126, NS-132, all MARTIN work.

### NS-121 — Replace Dickey–Fuller with Engle–Granger MacKinnon critical values (resolves C2) · RDP + MARTIN
**Issue.** `model_helpers.R:233` runs `urca::ur.df(x, type="drift")` (tau 5% ≈ −2.88) and `australia_cointegration.csv` marks Spec 4/5/6 `coint_adf_pass=TRUE` at ADF ≈ −3.23/−3.22 against `coint_adf_5pct_cv = −2.88`. **CONFIRMED.** For an Engle–Granger residual test with this many regressors the MacKinnon CV is ≈ −4.3 to −4.5, so −3.23 does NOT reject no-cointegration; the long-run framing is not actually established by the screen as coded. (Spec 1/2 at −0.56 fail even on DF.)
**Steps.** (1) Add a MacKinnon-CV lookup keyed on regressor count (or use `urca::ca.po` / `egcm`); replace the hard-coded −2.88/−2.89. (2) Re-run, re-commit `australia_cointegration.csv`, and state honestly which (if any) candidate cointegrates on its operational sample. (3) Reconcile §6.2/§7 framing with the new screen result.
**Effort.** 1 day. **Depends on.** none. **Gates** MARTIN long-run imposition.

### NS-122 — Reconcile all Spec 8 λ references and the §5.5/§8.4 interaction table to the committed CSV (resolves C3, C4) · RDP
**Issue (C3).** λ = **−0.377** is stated in 8 places (abstract l.38; §1.3 l.190; §5.1.1 l.1239; §5.5 l.1427; §7.3 l.1739; §8.4 l.1832; §9 l.2161; §11.1 l.2368) but `australia_full_results.csv` and `australia_lambda_robustness.csv` both give **−0.444919757** (t=−3.30). −0.377 appears in NO CSV. **CONFIRMED.**
**Issue (C4).** §8.4 table (l.1824–1825): `ha_y×CCI` stated −0.0020 (t=−0.29, "wrong sign"); CSV gives **+0.00159 (t=+0.32)** — sign-flipped and actually right-signed-but-insignificant. `hp_x_1_minus_cci` stated +0.0046 (t=+0.40); CSV **+0.00765 (t=+1.00)**. The §5.5 "wrong-signed housing-MPC interaction" interpretive claim is contradicted by the author's own output. **CONFIRMED.**
**Steps.** (1) Regenerate every Spec 8 λ and the dependent γ_HA/γ_NLA profile from the CSV (−0.445). (2) Re-derive the entire §5.5/§8.4 interaction table from `australia_full_results.csv`; correct the "wrong-signed" framing to "right-signed but insignificant." (3) Build a small `build_wp_tables.R`-style script (NS-001 was marked done but is stale) so numbers are pulled from CSVs, not hand-typed. Note: the §6.3 |λ|∈(0.02,0.30) screen-failure logic (`australia_estimation.R:2161`) is unaffected — Spec 8 fails at either value.
**Effort.** 1 day. **Depends on.** none (but re-do after NS-120 if Spec 8 is re-estimated). **Serves** reproducibility.

### NS-123 — Regenerate §5.1.1 CCI knot table from the live survivor CSV and reconcile §5.1/§5.3.1 (resolves C5) · RDP
**Issue.** §5.1.1 (l.1215–1222) tabulates SIX surviving knots {1992Q1, 2007Q3, 2009Q1, 2019Q1, 2020Q2, 2021Q4} with coefficients matching neither the CSV nor each other (it even lists 2021Q4 as surviving with a "−" prior but a +0.005 coef). `australia_williams_cci_knots.csv` has `survived=TRUE` for exactly **three**: sdmma_2009_01 (+0.0118), sdmma_2019_01 (−0.0338), sdmma_2020_04 (+0.0047). **CONFIRMED.** The `cci_williams` series the paper "uses throughout" is defined by this set, so the identification device is currently non-reproducible from the committed output.
**Steps.** (1) Regenerate §5.1.1 directly from the CSV (three knots, with the real coefficients and sign-violator/aliased annotations). (2) Reconcile the §5.1 four-knot table, §5.3.1's count, and §8.19 to one consistent narrative. (3) State the single reduction algorithm (iterated drop-on-violation) used to build the deployed series and confirm the placebo benchmark tests the SAME reduction.
**Effort.** half a day. **Depends on.** none. **Serves** reproducibility.

### NS-124 — Fix the OOS validator's silent interaction-column drop (resolves C6) · RDP + MARTIN
**Issue.** `oos_forecast.R:93` does `rhs_terms <- intersect(rhs_terms, names(train))`. The CCI interaction columns (`r_x_cci`, `hp_x_1_minus_cci`, `yp_x_cci`, `ha_x_cci`, Kalman `_k` variants) are created only on local md8/md9 copies inside `run_all_specifications`, never attached to the `model_data` passed to OOS, so they are silently dropped and Spec 8 collapses to the base. **CONFIRMED in output:** `australia_oos_rmse.csv` shows Spec8 and Spec9 with byte-identical RMSE at every horizon (e.g. 0.0324235979532093 at h=1). Any OOS claim about the CCI specs is therefore void.
**Steps.** (1) Recompute the de-meaned interaction columns inside `forecast_at_window` (PI is already recomputed per window at line 460 in the rebuild branch — extend that to interactions). (2) Replace the silent `intersect` with a hard `stop()` when a declared regressor is missing. (3) Re-run OOS; re-commit `australia_oos_rmse.csv`; re-write any §8 OOS sentence that treated Spec 8/9 as distinct.
**Effort.** 1 day. **Depends on.** NS-120 (real-time PI in the same loop). **Serves** RDP OOS + MARTIN real-time check.

### NS-125 — Fix the companion Wald sign-convention bug and re-run the joint test (resolves C7) · companion
**Issue.** `williams_calibration_test.R:106` computes `implied_ols = gamma_target * lambda_hat` with SIGNED λ̂ (≈ −0.197). For `ha_y` this yields target 0.0488 × (−0.197) = **−0.00963**, but the actual Spec-6 OLS estimate is **+0.00822** and the pipeline's structural convention is γ = OLS/|λ| (positive target). `williams_calibration_wald.csv` confirms `ha_y implied_ols = −0.0096…`, χ²=7.18, reject; the negative-target `ln_hp_over_y` is also flipped to +0.0257. The joint χ²(6)=29.1 (p=1e-4) is inflated by these sign errors. **CONFIRMED.**
**Steps.** (1) Use `implied_ols = gamma_target * abs(lambda_hat)`, handling the genuinely negative structural sign of `ln_hp_over_y` consistently. (2) Re-run; re-commit `williams_calibration_wald.csv`; report the corrected per-coefficient and joint χ². (3) Update the companion-paper "Williams calibrations formally rejected" claim to the corrected magnitude/composition (likely still some rejection, but not 29.1).
**Effort.** half a day. **Depends on.** none. **Serves** companion.

---

## P1 — Must-do before submission

### NS-126 — Generated-regressor inference: bootstrap / Pagan–Murphy-Topel SEs (resolves M1) · RDP + MARTIN
**Issue.** PI and the CCI spline `cci_williams` are *generated* regressors (PI a fitted forecast, the spline a full-sample pre-tested selection) but inference uses only the second-stage Newey–West vcov; the structural γ carries no SE at all. **CONFIRMED:** a grep for `pagan|murphy.topel|bootstrap|generated.regressor` across `Australia/R/*.R` returns nothing, and `australia_full_results.csv` has no SE column on `structural_param`. NW SEs understate the true uncertainty.
**Steps.** (1) Wrap the two-stage estimator (PI construction → ECM fit) in a block/wild bootstrap; report bootstrap SEs/CIs on λ, the wealth γ, and the PI coefficient. (2) Add delta-method or bootstrap SEs on the structural γ = OLS/|λ|. (3) State which coefficients remain significant once the generated-regressor uncertainty is carried — this directly affects the "matches Williams" claim, since Spec 6 λ = −0.180 is already only t=−1.76 (p=0.084, NOT significant at 5%; CONFIRMED in `australia_full_results.csv`).
**Effort.** 3–4 days. **Depends on.** NS-120 (PI must be real-time before its uncertainty is bootstrapped). **Gates** MARTIN coefficient import.

### NS-127 — Tighten the "matches Williams / external validation" framing to what insignificant, non-overlapping estimates support · RDP
**Issue.** The "matches Williams almost exactly" claim is the cancellation of a ~37% OLS deficit against a ~37% |λ| deficit, on a 2003Q3–2024Q4 effective window (cci_ratio binding, n=86 — CONFIRMED at `australia_full_results.csv` Spec6 n=86 and disclosed at WP l.551–554) with near-zero overlap with Williams' 1978–2008, every long-run level coefficient individually insignificant, and `eq_y` wrong-signed. The companion's own Wald test rejects Williams' joint calibration but that is absent from the WP "where we agree" narrative.
**Steps.** (1) Re-phrase §9 from "external validation / matches almost exactly" to "structurally comparable point estimates, individually insignificant, on a non-overlapping sample." (2) Surface the corrected companion Wald rejection (NS-125) in the WP §9. (3) State the effective identification window (2003Q3+ / n=86) wherever the Williams comparison or a policy claim is made.
**Effort.** 1–2 days. **Depends on.** NS-120, NS-125, NS-126. **Serves** RDP.

### NS-128 — Make the Spec-6-vs-Spec-2 "preferred" disclosure consistent throughout · RDP
**Issue.** `australia_spec_selection.csv` sets `is_preferred=TRUE` for **Spec2**, not Spec 6 — CONFIRMED. The paper *does* disclose this in §6.3 (l.1524–1531, "selector returns Spec 2") — a genuine strength — but ≥6 other passages (l.25, 182, 550, 1434, 1570) call Spec 6 "the preferred specification" without qualification. A referee will read the unqualified usage as overclaiming.
**Steps.** Standardise on "narrative-preferred Spec 6 (selector-best is Spec 2)" at every first mention per section; add a one-line footnote in the abstract/§1 pointing to §6.3 for the selector divergence.
**Effort.** half a day. **Depends on.** none. **Serves** RDP.

### NS-129 — Refresh stale diagnostics labels (§7.2, BICs, AR labels) · RDP
**Issue.** §7.2 (l.1631–1632) states Spec 6 AR(1) p=0.30 and AR(4) p=0.20; `australia_full_diagnostics.csv` gives **0.261** and **0.1445** (RESET 0.0001 matches). Several Spec 8/10 BIC and AR labels are similarly stale (the critique flags these; spot-checks confirm drift). **CONFIRMED (minor but reproducibility-relevant).**
**Steps.** Auto-pull all §7.2/§8 diagnostic numbers from `australia_full_diagnostics.csv` via the NS-122 table-builder; do a full paper-vs-CSV numeric reconciliation pass and log it.
**Effort.** 1 day. **Depends on.** NS-122 table-builder. **Serves** RDP reproducibility.

### NS-130 — Disclose the placebo/deployed CCI reduction mismatch and the §5.2 protocol mis-statement · RDP
**Issue.** The maximal-GETS placebo uses single-pass reduction (7 survivors) while deployed `cci_williams` uses iterated reduction (3 survivors), so the percentile benchmark tests a *different* CCI than the paper deploys; and §5.2 mischaracterises the literal-4-knot placebo as using "the same drop-on-violation reduction protocol" when the code enters all four knots unconditionally. (Follows from NS-123; flagged by the referee.)
**Steps.** Re-run the placebo under the *deployed* iterated reduction (or state clearly the benchmark is for the maximal set and report both); correct the §5.2 protocol sentence.
**Effort.** half a day. **Depends on.** NS-123. **Serves** RDP.

---

## P2 — Strengthen / extend

### NS-131 — Re-build the back-extension wealth proxies before any Spec 6b headline use · RDP + MARTIN
**Issue.** `australia_data_download.R:1331–1346`: `eq_y_proxy` is held *constant* at the 1988Q3 value pre-1988 (zero variance) and `super_y_proxy` is a *deterministic linear ramp*. **CONFIRMED.** Spec 6b's "only significant λ" (l.190; `australia_cointegration.csv` ADF −3.77) therefore partly rests on regressors with no genuine pre-1988 variation; the 2019Q2→Q3 D02 credit splice has a no-overlap zero-growth join (`australia_data_download.R:1001`). 
**Steps.** Replace the zero-variance/deterministic proxies with genuine series (ASX All-Ords back-cast for equities; a real super accumulation series); fix the credit splice; correct SEs for measurement/generated-regressor error. Then re-assess whether Spec 6b's significance survives.
**Effort.** 3–5 days. **Depends on.** NS-121, NS-126. **Serves** RDP back-extension claim + MARTIN.

### NS-132 — Re-run the real-time OOS with per-window PI and reconstructed interactions · RDP + MARTIN
**Issue.** Tie-off of NS-120 + NS-124: produce a genuinely real-time OOS (PI recomputed each window, CCI interactions rebuilt inside the window). Note the honest negative result already holds and is a strength — `australia_oos_rmse.csv` confirms Spec 6 (0.0322/0.0332/0.0416 at h=1/4/8) loses to RW-drift (0.0309/0.0309/0.0328) at every horizon. **CONFIRMED.**
**Steps.** Re-run; report real-time RMSEs vs RW/AR(1); position the contribution as long-run structural interpretation, not forecast improvement.
**Effort.** 1 day (after NS-120/124). **Depends on.** NS-120, NS-124. **Serves** RDP + MARTIN.

### NS-133 — Companion paper: finish HEW equation, ζ_i normalisation, de-meaned interactions, write-up (was Tier B / B1–B5) · companion
**Issue.** The companion draft exists (`LIVES/docs/companion_paper_draft.md`, 616 lines) but its headline Wald result is bugged (NS-125) and the SUR ρ̂ it relies on is mis-quoted in the WP (abstract/§5.4/§8.18 say ≈0.0007; `lives_sur_2eq_resid_corr.csv` gives **−0.0045**, CONFIRMED). The ρ̂≈0 result is a genuine, citable finding (kills the efficiency case for joint estimation) — preserve it but quote it correctly.
**Steps.** (1) Fix NS-125 first. (2) Correct the ρ̂ value in BOTH the WP (5 occurrences: l.56, 501, 1380, 2074, 2414) and the companion. (3) Complete B1 (HEW eq), B2 (ζ_i normalisation), B3 (de-meaned CCI interactions — already note this may flip `ha_x_cci`). (4) Write up. Recommend keeping this a SEPARATE companion paper (per existing decision) so the headline RDP ships uncontaminated.
**Effort.** 2–4 weeks. **Depends on.** NS-125; benefits from NS-120/123. **Serves** companion.

### NS-134 — Single aggregate wealth coefficient with a CI (decision support for RDP and MARTIN) · RDP + MARTIN
**Issue.** The four wealth components (nla_y, eq_y, super_y, ha_y) are individually insignificant and partly offsetting; `eq_y` is wrong-signed (CONFIRMED in `australia_full_results.csv`). For both referee clarity and MARTIN reconciliation (MARTIN runs a single net-wealth elasticity ≈0.17), present one aggregate wealth coefficient with a bootstrap CI alongside the disaggregated view.
**Effort.** 1–2 days. **Depends on.** NS-126. **Serves** RDP + MARTIN.

---

## P3 — MARTIN-integration track

### NS-135 — MARTIN nesting: balanced-growth long run + calibrated thin channels · MARTIN
**Issue.** MARTIN's consumption block is a cointegrating relation rc = β1·hdy + (1−β1)·hnw + β2·RCR (+ shift), with income+wealth RESTRICTED to sum to one (balanced growth), a calibrated real-rate semi-elasticity (~0.05%/100bp) and net-wealth elasticity ~0.17. The estimated Spec 6 does not nest this, its real-rate coefficient is ≈0/insignificant (CONFIRMED, `real_rate` t small in CSV), and its long-run is not validly screened.
**Steps.** (1) Re-specify the long run in MARTIN's form: net wealth (or the disaggregated ratios) inside the cointegrating vector; test and optionally impose income+wealth = 1 so the estimate NESTS MARTIN. (2) Benchmark the interest-rate semi-elasticity to MARTIN's 0.05%/100bp rather than imposing the insignificant estimate; reconcile the net-wealth elasticity to ~0.17 with a CI. (3) Resolve the CCI-for-MARTIN decision explicitly: drop the placebo-failing spline (matching MARTIN) or carry only a short-run credit-growth control; never propose the spline CCI for import. (4) Only impose coefficients that are stable, identified, and carry valid (NS-126) inference.
**Effort.** 1–2 weeks. **Depends on.** NS-120 (real-time PI — gating), NS-121 (valid coint screen), NS-126 (valid inference), NS-131 (proxies), NS-132 (real-time OOS). **Serves** MARTIN.

---

## P4 — Optional / defer

- **NS-031 / Tier C — full FIML with shared ϖ.** Months of custom-likelihood code; the only path empirically shown to close the wealth-coefficient gap, but paper-output ratio is low. Defer until the companion (NS-133) lands. Effort: weeks–months.
- **NS-013/14 / B4 — Quarto rendering pipeline.** Submission-time formatting; do after numbers are frozen (post-P1). Effort: 2–3 days.
- **NS-024/043 — sectoral-bonds bucket / data_raw cleanup.** Housekeeping. Effort: 1–2 days.
- **NS-114 — hand-coded SoMP survey CCI.** 2–3 weeks; only if it becomes a headline data contribution.
- **NS-107 — APRA observable CCI post-2008.** A genuine external validator for the 2007/2019 knots; worthwhile but not blocking.

---

## Recommended sequence

### 1–2 week horizon (correctness + must-do edits)
1. **NS-120** (real-time PI) — gating; everything downstream depends on the verdict. 2–3 d.
2. In parallel (independent of NS-120): **NS-121** (Engle–Granger CVs, 1 d), **NS-122** (Spec 8 λ + §8.4 table reconcile, 1 d), **NS-123** (3-knot §5.1.1, 0.5 d), **NS-125** (companion Wald sign, 0.5 d).
3. **NS-124** (OOS interaction-drop) once NS-120's real-time PI lands. 1 d.
4. **NS-128/129/130** editorial reconciliation (preferred-spec language, stale diagnostics, placebo protocol). ~2 d total.
Output: a paper whose numbers reproduce from the CSVs and whose headline is honestly framed.

### 1–2 month horizon (strengthen + companion)
5. **NS-126** (generated-regressor bootstrap inference) — then **NS-127** (re-frame "matches Williams") and **NS-134** (aggregate wealth + CI). ~1.5 weeks.
6. **NS-131** (rebuild back-extension proxies) + **NS-132** (real-time OOS tie-off). ~1 week.
7. **NS-133** (companion paper: fix ρ̂ quote, B1/B2/B3, write-up). 2–4 weeks, in parallel with submission prep.
8. **NS-013/14** Quarto pipeline once numbers are frozen.

### If budget / appetite exists
9. **NS-135** (MARTIN nesting: balanced-growth long run, calibrated thin channels, CCI decision) — only after NS-120/121/126/131/132. 1–2 weeks.
10. **Tier C / NS-031** full FIML with shared ϖ — the gap-closing research question, months of work; pursue only if the wealth-coefficient gap is *the* question.

### Preserve (strengths confirmed; do not regress)
- The intellectual honesty (placebo failures, ρ̂≈0 SUR, OOS loss to RW, selector divergence) — all CONFIRMED in the CSVs; keep foregrounding.
- The leak-free AR PI forecaster (`construct_permanent_income`, expanding-window `train <- dat[seq_len(i),]`) — the correct MARTIN-appropriate design; promote it.
- The 1976Q3 back-extension as a clean falsifiable design (subject to NS-131 proxy fixes).
- NW HAC + Andrews(1991), explicit seeds, the NLA Wald restriction test, and the cached-RDS path that reproduces λ=−0.180 exactly.
