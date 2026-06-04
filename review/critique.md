## Lead-reviewer consolidated critique — Australia LIVES consumption model

**Project:** `/Users/davidstephan/Documents/consmodelling` · **Targets:** RBA Research Discussion Paper (RDP) + MARTIN consumption block
**Verdict:** Major revision required before submission; **not ready** for MARTIN integration.

---

## 1. Executive summary

This is an ambitious and, in many respects, exemplary piece of applied central-bank econometrics: a contemporary single-equation Muellbauer–Williams "LIVES" error-correction consumption model for Australia, a genuinely novel back-extension of the master dataset to 1976Q3, and an unusually candid stress-test of the credit-conditions index (CCI) via placebo battery, cross-equation joint-survival, and SUR. The intellectual honesty is the standout strength — the paper foregrounds its own CCI placebo failures, the near-zero SUR residual correlation that demolishes the efficiency case for joint estimation, the out-of-sample loss to a random walk, and the fact that the automated selector prefers a different specification than the headline. That conduct is RDP-grade and a referee will respect it.

However, the draft is **not yet submittable** for two reasons that are independent of the honesty. First, the **single most important risk**: the headline permanent-income result (the "puzzle resolved" — λ roughly doubling and the PI coefficient flipping positive to "match Williams") is **driven by a look-ahead-biased regressor**. The canonical permanent-income series (`PI_METHOD <- "italy"`) builds its target from *realised future income*, fits one full-sample OLS, and uses in-sample fitted values for every quarter (`australia_estimation.R:304-379`). The leak-free rolling AR(8) forecaster, by contrast, gives an insignificant PI coefficient (−0.003, not the abstract's "significantly negative −0.20") and a smaller λ; the within-AR sensitivity grid holds λ ≈ −0.083 across all 18 cells. The headline is thus an artefact of method choice, not a robust feature, and this is not disclosed.

Second, **numerous headline numbers contradict the committed outputs the pipeline actually produces**. Spec 8 λ is stated as −0.377 in seven places but every CSV gives −0.445; the §8.4 Spec-8 interaction table disagrees with the CSV on all four terms (the ha_y×CCI term is even sign-flipped); the SUR ρ̂ is quoted as ≈0.0007 when the committed file gives −0.0045; §5.1.1 tabulates six surviving CCI knots with specific coefficients when the live `australia_williams_cci_knots.csv` survives only three; and several diagnostics (Spec 8/10 BIC, Spec 6 AR(1)/AR(4) labels) are stale. These are reproducibility failures of the kind a referee can catch by re-running, and they undermine trust in the numbers that *are* correct.

Two further structural issues compound this. The **cointegration screen uses ordinary Dickey–Fuller critical values (−2.88) where Engle–Granger residual-test (MacKinnon) values (≈ −4.3 to −4.5) are required** (`model_helpers.R:233`), so the central long-run claim is not actually established by the screen as coded; on correct critical values Spec 4/6's ADF ≈ −3.23 does *not* reject no-cointegration. And the **whole apparatus of generated regressors (permanent income, the CCI spline) is treated as observed**, with Newey–West SEs that understate the true uncertainty. The single most important strength to preserve is the dataset and the honest negative-result framing; the single most important risk is that the headline "matches Williams / puzzle resolved" narrative rests on a look-ahead artefact plus a string of paper-vs-CSV mismatches. **MARTIN-readiness verdict: not ready** — the equation as specified cannot be computed at forecast time (look-ahead PI), its long-run is not screened with valid critical values, its coefficients carry no valid inference, and it does not nest MARTIN's balanced-growth form.

---

## 2. What is genuinely strong (preserve these)

- **Intellectual honesty throughout.** The paper foregrounds rather than buries its weaknesses: the CCI placebo failures (§5.2, 19th/34th percentiles), the ρ̂ ≈ 0 SUR result that kills the efficiency case for joint estimation (§5.4/§8.18), the OOS forecasts losing to a random walk at h=4/h=8 ("we record this honestly rather than overstating"), the individually insignificant wealth coefficients, and the selector/narrative divergence. This is the draft's best feature. **Verified:** `australia_oos_rmse.csv` confirms Spec6 RMSE (0.0322/0.0332/0.0416 at h=1/4/8) is worse than Benchmark_RW_drift (0.0309/0.0309/0.0328) at every horizon.
- **The back-extension to 1976Q3 is a real, citable contribution.** The Spec 4/6b test on n=190 is a clean falsifiable design isolating whether the gap with Williams is a sample-length artefact (answer: no). Prior absence of back-extended Australian data made this impossible.
- **Leak-free AR permanent income.** `construct_permanent_income()` (`australia_estimation.R:114+`) is a genuine expanding-window forecaster (`train <- dat[seq_len(i), ]`), with no future leakage — the correct, MARTIN-appropriate design. The discounting (δ=0.95, δ_q=δ^(1/4), k=40, normalised weights) faithfully matches De Bonis et al. / Chauvin–Muellbauer.
- **House-price chain-linking is correct in the canonical RDS.** The 2011Q3 bridge boundary shows dlog = −1.78% (no step). **Verified directly:** RDS gives 386.72 → 379.90 (−1.78%), clean.
- **CCI machinery is faithfully implemented.** The SDMMA smoothed step rises 0→1 over eight quarters with the correct symmetric S-shape; the Hendry/Krolzig drop-on-violation reduction records surviving/dropped/aliased knots to CSV for transparency. The placebo battery itself is rare and admirable in this literature.
- **Sound econometric infrastructure:** Newey–West HAC with Andrews(1991) bandwidth throughout, graceful collinear-term dropping, a proper NLA `γ_LA + γ_LOANS = 0` Wald test (`australia_nla_restriction_test.csv`), explicit seeds on all randomised tests, and a fast cached-RDS re-estimation path that reproduces λ=−0.180 exactly.
- **Strong MARTIN positioning** (§2.3, §10.3): framing the paper as a freely-estimated benchmark against which MARTIN's calibrated elasticities can be evaluated is exactly the institutional value proposition an RBA reader wants.
- **Candid data documentation** (`data.md`): the `pop_millions` mis-naming, RBA-vs-ABS mortgage-rate fallback, and CSV-vs-RDS Chow divergence are all disclosed; hard `stopifnot` assertions guard against silent ABS vintage/unit changes.

---

## 3. Critical issues (must-fix before submission)

### C1. Headline permanent-income result is driven by a look-ahead-biased regressor (undisclosed)
- **Why it matters:** The "Australian permanent-income puzzle resolved," the λ near-doubling, and the PI sign-flip — all headline claims — depend on `PI_METHOD <- "italy"` (`australia_estimation.R:45`), which is non-causal. The leak-free AR method gives the opposite (insignificant) result.
- **Evidence (verified):** `construct_permanent_income_italy` builds the target from realised future income (`fut <- inc[(t+1):(t+k)]; y_p_target[t] <- sum(weights*fut)`, lines 304-308), fits ONE full-sample OLS over a training mask spanning all t (lines 347-359), then uses in-sample fitted values for ALL t including the ~40 tail quarters (lines 367-377), and forms `ln_yp_over_y = y_p_hat - lincome` (line 379). `australia_pi_method_comparison.csv`: AR `ln_yp_over_y` = −0.0031, Italy = +0.2404; AR λ = −0.0829, Italy λ = −0.1928. The within-AR sensitivity grid holds λ ∈ [−0.082, −0.083] across all cells (`australia_permanent_income_sensitivity.csv`); the only AR-grid cell reaching λ ≈ −0.20 with positive PI is the (also non-causal) two-sided HP filter.
- **Referee/MARTIN risk:** Showstopper. The equation cannot be evaluated at MARTIN forecast time because future income is unknown. A referee will read this as the central result being a smoothing artefact.
- **Recommendation:** Re-estimate the headline with a recursive/expanding-window Italy projection (refit using only data through each t). Report how much of the λ gain and PI flip survives in real time. If it does not survive, demote Italy LP to a robustness column and carry the AR (leak-free) result as the headline.
- **Status: confirmed (critical).**

### C2. Cointegration screen uses Dickey–Fuller, not Engle–Granger, critical values
- **Why it matters:** The long-run cointegrating relationship underpinning the ECM framing is "established" against the wrong null distribution; with correct critical values none of the preferred long-run relations clearly cointegrate.
- **Evidence (verified):** `model_helpers.R:233` calls `urca::ur.df(x, type="drift")`, whose tau critical value is ≈ −2.88 at 5%. `australia_cointegration.csv` codes `coint_adf_pass=TRUE` for Spec 4/5/6 at ADF ≈ −3.23/−3.22 against `coint_adf_5pct_cv = −2.88`. For an Engle–Granger residual-based test with multiple regressors the MacKinnon CV is ≈ −4.3 to −4.5, so −3.23 does NOT reject no-cointegration; the aggregate Spec 1/2 family at ADF −0.56 fails decisively even on DF values.
- **Referee/MARTIN risk:** The paper's structural framing rests on a long-run equilibrium the screen does not actually support; MARTIN cannot impose a cointegrating vector that is not established.
- **Recommendation:** Replace with Engle–Granger MacKinnon critical values appropriate to the regressor count; re-run and report honestly which (if any) candidate cointegrates on the operational sample.
- **Status: confirmed (critical).**

### C3. Spec 8 λ stated as −0.377 everywhere; committed outputs give −0.445
- **Why it matters:** Spec 8's λ = −0.377 "exceeding Williams in magnitude" is used as a headline selling point and feeds the dependent γ profile.
- **Evidence (verified):** Stated −0.377 at abstract (l.38), §1.3 (l.190), §5.1.1 (l.1239), §5.5 (l.1427), §7.3 (l.1739), §8.4 (l.1832), §9 (l.2161), §11.1 (l.2368). But `australia_full_results.csv` Spec8 `ecm_lag` = **−0.444919757** (t=−3.30) and `australia_lambda_robustness.csv` Spec8 full = −0.444919757. The value −0.377 appears in NO CSV (the closest, −0.373/−0.375, are Spec 7/7b).
- **Referee/MARTIN risk:** Non-reproducibility; a referee re-running the pipeline gets a different headline number.
- **Recommendation:** Regenerate all Spec 8 λ references (and the dependent γ_HA/γ_NLA) from the committed CSV (−0.445), or re-run and re-commit if −0.377 was intended. Note: the §6.3 screen-failure logic is unaffected (Spec 8 fails the upper-bound |λ| screen at either value).
- **Status: confirmed (critical).**

### C4. §5.5 / §8.4 Spec-8 CCI-interaction table contradicts the CSV on all four terms (ha_y×CCI is sign-flipped)
- **Why it matters:** §5.5 reports the housing-MPC×CCI interaction as "wrong-signed" (γ_HA_cci = −0.003, t=−0.65); the committed estimate is the opposite sign (right-signed under Williams' prior) and the whole "total m.p.c. at CCI peak" decomposition built on it is wrong.
- **Evidence (verified):** `australia_full_results.csv` Spec8: `ha_x_cci` = **+0.00159** (t=+0.32, p=0.75); `hp_x_1_minus_cci` = +0.00765 (t=+1.00); `r_x_cci` = +0.00192 (t=+1.85); `yp_x_cci` = −0.6113 (t=−2.12). WP §8.4 table states ha_y×CCI = −0.0020 (t=−0.29), hp composite t=+0.40, yp×CCI t=−1.66 — none match; the ha×CCI sign is reversed.
- **Referee/MARTIN risk:** A central interpretive claim ("wrong-signed housing-MPC interaction") is contradicted by the author's own output.
- **Recommendation:** Re-derive every Spec 8 number in §5.5 and §8.4 from the committed CSV; correct the "wrong-signed" framing (the term is right-signed but insignificant) and the de-meaned hp×(1−CCI) t-stat (+1.00, not +0.40).
- **Status: confirmed (critical).**

### C5. §5.1.1 reports six surviving CCI knots; the live pipeline survives only three (and the knot sets are mutually inconsistent across the paper)
- **Why it matters:** The `cci_williams` series the paper says it "uses throughout the rest of the paper" is defined by the surviving-knot set, which is mis-stated.
- **Evidence (verified):** `australia_williams_cci_knots.csv` has `survived=TRUE` for exactly three knots: sdmma_2009_01 (+0.0118), sdmma_2019_01 (−0.0338), sdmma_2020_04 (+0.0047). WP §5.1.1 (l.1214-1226) tabulates SIX {1992Q1 −0.020, 2007Q3 −0.007, 2009Q1 +0.006, 2019Q1 −0.027, 2020Q2 +0.077, 2021Q4 +0.005} — neither the knots nor the coefficients match the CSV, and §5.1.1's set internally contradicts the §5.1 four-knot table and §5.3.1's count.
- **Referee/MARTIN risk:** The identification device is not reproducible from the committed output; a referee cannot regenerate §5.1.1.
- **Recommendation:** Regenerate §5.1.1 directly from `australia_williams_cci_knots.csv` (three knots), reconcile §5.1/§5.3.1/§8.19, and state one reduction algorithm (see M5).
- **Status: confirmed (critical; if anything understated — §5.1.1 also contradicts its own §5.1 table).**

### C6. OOS validator silently drops all CCI interaction regressors — Spec 8 and Spec 9 collapse to identical (byte-identical) forecasts
- **Why it matters:** The out-of-sample comparison that purports to evaluate the CCI-interaction specs against benchmarks is not actually evaluating them.
- **Evidence (verified):** `oos_forecast.R:93` does `rhs_terms <- intersect(rhs_terms, names(train))`. The interaction columns (`r_x_cci`, `hp_x_1_minus_cci`, `yp_x_cci`, `ha_x_cci`, and the Kalman `_k` variants) are created only on local md8/md9 copies inside `run_all_specifications`, not as columns of `model_data` passed to OOS, so they are silently dropped and both specs collapse to the base. **Confirmed in output:** `australia_oos_rmse.csv` shows Spec8 and Spec9 produce identical RMSE to the last digit at every horizon (e.g. 0.0324235979532093 at h=1). (`cci_williams` itself IS attached at `australia_estimation.R:3687`/1813; the bug is specific to the interaction columns.)
- **Referee/MARTIN risk:** Any OOS claim about CCI specs is void; a real-time OOS is a MARTIN prerequisite.
- **Recommendation:** Recompute the de-meaned interaction columns inside `forecast_at_window` (or attach them to `model_data` before validation). Replace the silent `intersect` with a hard error when a spec's regressor is missing.
- **Status: confirmed (critical).**

### C7. Companion-paper "Williams calibrations formally rejected" is partly an artefact of a sign-convention bug in the Wald target
- **Why it matters:** The companion paper's headline Wald rejection (χ²(6)=29.1, p<0.001) uses restriction targets with the wrong sign for the wealth/PI coefficients.
- **Evidence (verified):** `williams_calibration_test.R:106` computes `implied_ols = gamma_target * lambda_hat` with SIGNED `lambda_hat` (≈ −0.197). For ha_y this sets the target to 0.0488 × (−0.197) = **−0.00963**, but the actual Spec-6 OLS estimate is **+0.00822** (and the headline pipeline defines structural γ = OLS/|λ|, i.e. it expects a positive target). `williams_calibration_wald.csv` confirms `ha_y implied_ols = −0.00963`, χ²=7.18, reject. The sign mismatch inflates each restriction's distance and hence the joint χ².
- **Referee/MARTIN risk:** The companion's central inferential result is overstated; this must be fixed before the joint test is reported.
- **Recommendation:** Apply `implied_ols = gamma_target * abs(lambda_hat)` uniformly to all six restrictions (handling the genuinely negative structural sign of `ln_hp_over_y` consistently), re-run, and report the corrected χ². Likely still some rejection, but the magnitude/composition will change.
- **Status: confirmed (critical, companion paper).**

---

## 4. Major issues

### M1. Generated regressors (permanent income, CCI spline, income-volatility residual) treated as observed; NW SEs understate true uncertainty
- **Evidence:** PI enters the ECM as a fitted value but inference uses only the second-stage NW vcov; no Pagan(1984)/Murphy–Topel/bootstrap correction (grep for these returns nothing). The CCI spline `cci_williams` is a pre-tested, full-sample-selected fixed regressor; `abs_income_resid` (`australia_estimation.R:658-676`) is a full-sample in-sample AR(8) residual. **Scope correction (verifier):** applies to the canonical AR PI generator (lines 114-263) and the Italy block, plus `abs_income_resid` — wider than originally located.
- **Risk:** Reported PI/wealth SEs are too small; any "significance" claim is conditional on the first stage being error-free. MARTIN needs valid CIs on every imported coefficient.
- **Recommendation:** Block-bootstrap the entire two-stage procedure (refit the PI projection and the ECM in each draw) or apply Murphy–Topel; at minimum caveat that inference is conditional on the generated regressors being measured without error.
- **Status: confirmed (major).**

### M2. Structural γ = −OLS/λ reported with no delta-method/bootstrap SE
- **Evidence (verified):** `australia_full_results.csv` has a `structural_param` column but **no corresponding SE column** — the γ profile is a deterministic ratio of two estimates, presented without inference. `williams_comparison.R:91` sets se=NA.
- **Risk:** The entire "γ matches Williams" comparison is point-estimate-only; the two inputs (ha_y t=1.52; λ t=−1.76) are individually insignificant, so the γ CI is wide.
- **Recommendation:** Compute delta-method SEs (or bootstrap) on each implied γ = OLS/|λ| and report CIs alongside the Williams comparison.
- **Status: confirmed (major).**

### M3. "Matches Williams almost exactly" / "external validation" rests on individually insignificant n=86 coefficients and the cancellation of two ~37% deficits
- **Evidence (verified):** Spec 6 γ_HA = 0.0088/0.180 = 0.0491 vs Williams 0.0488 — but ha_y t=1.52 (p=0.13), nla_y t=0.96, eq_y t=−0.13 (**wrong sign**), ln_hp_over_y t=−0.85, ln_yp_over_y t=0.76, and λ t=−1.76 (p=0.084, not sig at 5%). The "exact" match is a −36.7% OLS gap offsetting a −37.0% |λ| gap. **Verifier downgrade to major (from critical):** the underlying SEs/t-stats and their insignificance ARE disclosed in §7.1/§7.3 (the §7.3 table prints the −37% gaps explicitly and warns the match "prevents a simple statement"); the issue is that the abstract/§1.3 do not carry that caveat.
- **Risk:** Referee reads the abstract claim as overstated relative to the body.
- **Recommendation:** Align the abstract/§1.3 with §7.3's own caveat — report the implied γ with a CI and reframe as "broadly consistent in sign and order of magnitude, with the close numerical match arising from offsetting OLS and |λ| deficits."
- **Status: confirmed (major).**

### M4. Headline comparison window is ~2003Q3–2024Q4 (cci_ratio binding), not the stated "1988Q4–2024Q4"; near-zero overlap with Williams 1978–2008
- **Evidence (verified):** `cci_ratio` is non-NA only from 2002Q3; after the Δ² transform and lag-2 the first usable `d2_logcci_lag2` is ~2003Q3, binding Spec 6's long run to n=86. Yet the abstract, `williams_comparison.R:14`, and §7.3/§9 repeat "1988Q4–2024Q4." **Verifier correction:** effective start is 2003Q3 (not 2002Q3). The constraint IS disclosed at §7.1, but not propagated.
- **Risk:** The "external validation on data Williams did not see" claim is on a window with essentially no overlap with his 1978–2008 sample — but also barely overlaps the post-GFC period the abstract emphasises.
- **Recommendation:** State the effective 2003Q3–2024Q4 window wherever n=86 or the Williams comparison appears.
- **Status: confirmed (major).**

### M5. CCI placebo benchmark tests a different CCI than the paper deploys; §5.2 mischaracterises the reduction protocol
- **Evidence (verified):** The maximal-GETS placebo (`cci_placebo_maximal_gets_extended.R:107-151`) does a SINGLE reduction pass (canonical → 7 survivors per `..._maximal_extended_summary.csv`), while the deployed `cci_williams` uses iterated reduction (`australia_estimation.R:1760-1788`, max_iter=10 → 3 survivors). So the "64th percentile" canonical benchmark is a different CCI. Separately, §5.2 (l.1252-1254) says the literal-4-knot placebo uses "the same drop-on-violation reduction protocol," but `cci_placebo_test.R:92-94` explicitly enters all four knots unconditionally ("no sign-prior reduction... we want to test pure fit"). The placebo also differs in long-run specification (aggregate vs disaggregated wealth) and in single-pass-vs-iterated reduction.
- **Risk:** The placebo result — already a load-bearing honest finding — does not test the deployed CCI, weakening the (correct) conclusion that the spline is not a common factor.
- **Recommendation:** Make the placebo call the iterated `fit_consumption_with_williams_cci` (shared helper) on the same sample/spec as the deployed CCI for both canonical and random draws; correct §5.2 to describe the actual protocol.
- **Status: confirmed (major).**

### M6. Formal Wald rejection of Williams' calibration is absent from the headline WP's "where we agree" narrative
- **Evidence:** `williams_calibration_wald.csv`: JOINT_ALL_6 χ²=29.10 (p=1e-4, reject at 1%); JOINT_WEALTH_4 χ²=10.03 (p=0.040). The companion reports this; WP §9/§11.1 describe only agreement. (Note: this χ² is itself affected by C7's sign bug — fix C7 first, then report the corrected test.)
- **Risk:** The WP's agreement framing omits the author's own formal rejection.
- **Recommendation:** After fixing C7, add the corrected joint Wald result to §9 with the companion's honest reconciliation (implied-γ point estimate matches; the joint calibration is rejected because the OLS coefficients are individually too imprecise / offsetting).
- **Status: confirmed (major).**

### M7. BIC compared across specs with effective n from 64 to 190 — not comparable across samples
- **Evidence (verified):** `fit_ecm_spec` restricts each spec to its own complete-cases sample; `australia_full_diagnostics.csv`: Spec2 n=86 BIC=−500.8, Spec1 n=146 BIC=−919.2, Spec6b n=180/190 BIC ≈ −1116. `select_preferred_spec` ranks via `which.min(out$bic)` (`australia_estimation.R:2206/2218`). Because BIC = −2logL + k·ln(n) scales with n, cross-n ranking is invalid.
- **Risk:** The selector's preference ordering (and the §6.3 BIC tiebreak) is not well-defined across samples.
- **Recommendation:** Compare BIC only within common-sample groups, or re-estimate all specs on a common intersection sample for selection; add a guard refusing to BIC-rank specs with differing n.
- **Status: confirmed (major).**

### M8. AR permanent-income coefficient mis-stated as "significantly negative −0.20" in the abstract/intro
- **Evidence (verified):** Abstract (l.34-36): "a significantly negative long-run coefficient on log(y^p/y) under a rolling AR(8) forecaster." `australia_pi_method_comparison.csv`: AR `ln_yp_over_y` = −0.0031 (SE 0.00068 in the comparison spec — i.e. ≈ zero, not −0.20). The WP's own §7.4 (l.2186) says "approximately zero on Spec 1 (−0.003 under AR)." The "+0.30" flip target also has no support: the actual Italy value is +0.24 (also insignificant in the preferred spec, where ln_yp_over_y t=0.76).
- **Risk:** A factual misstatement of a headline coefficient and its significance.
- **Recommendation:** Correct abstract/§1.3 to "approximately zero (insignificant) under AR → +0.20–0.24 (also insignificant) under the full-sample Italy fit"; report t-stats. Fix one (spec, before, after) triple and propagate (the "+0.30" figure appears nowhere in the CSVs and should be removed).
- **Status: confirmed (major) — downgraded from critical by verifier because the body (§7.4/§9) reports the correct values.**

### M9. §7.4 AR-vs-Italy table labelled "Spec 1" but reports Spec 2 numbers
- **Evidence (verified):** §7.4 (l.1757-1759) says "reported on the Spec 1 long-run regression," giving Italy ecm_lag=−0.193, ln_yp_over_y=+0.240. These are Spec 2 values; Spec 1's actual Italy values (`australia_full_results.csv`) are −0.1772/+0.1703.
- **Recommendation:** Relabel to "Spec 2 (the preferred specification)" (also at §9 l.2186); the numbers themselves are correct Spec 2 values.
- **Status: confirmed (major).**

### M10. SUR residual correlation ρ̂ stated as ≈0.0007; committed value is −0.0045
- **Evidence (verified):** Abstract (l.56), §1.3, §2.6, §5.4, §8.18, §11.2 all cite ρ̂ ≈ 0.0007. `lives_sur_2eq_resid_corr.csv`: "SUR rho(eps_C, eps_H)" = **−0.0045**, "OLS" = −0.0038. The CSV was regenerated but the prose was not. (The conclusion — negligible correlation, no efficiency gain — is unchanged.)
- **Recommendation:** Replace 0.0007 with −0.0045 throughout the WP, companion, and `code_guide.md`.
- **Status: confirmed (major).**

### M11. NPY income-robustness claim ("shifts λ ~+18% toward Williams") is directionally wrong in its only supporting output
- **Evidence (verified):** WP §8.8 and `williams_comparison.R:252`. `australia_williams_income_robustness.csv` Spec2 ecm_lag: base=−0.1928, NPY=−0.0938 — |λ| FALLS from 0.193 to 0.094 (−51%), moving AWAY from Williams' −0.286.
- **Recommendation:** Correct §8.8 to state the NPY measure moves λ away from Williams, or produce a Spec 6 NPY refit that genuinely supports a "toward" shift.
- **Status: confirmed (major).**

### M12. Two mutually inconsistent "Williams Table 1" γ_NLA reference columns (0.159 vs 0.066) coexist
- **Evidence (verified):** Headline §7.3 uses γ_HA=0.0488, γ_NLA=0.159; the back-extension tables (l.1448, l.1712) and `next_steps_plan_2026.md:61` use a different "Williams Table 1" set {nla_y +0.066, eq_y +0.013, super_y +0.013, ha_y +0.052}. These cannot both be Williams' Table 1.
- **Recommendation:** Adopt one authoritative Williams source (Aust system paper Table 1 Col 1 / BIS chapter: γ_HA=0.0488, γ_IFA=0.022, γ_NLA=0.159) consistently; if the 0.066 set is a different (e.g. implied-OLS) quantity, relabel it.
- **Status: confirmed (major).**

### M13. Stale/contradictory hard-coded interpretive text in williams_comparison.R
- **Evidence (verified):** `williams_comparison.R:264-346` hard-codes "our λ is about 5× smaller (−0.052 vs −0.286)" and "γ_HA ... 0.282" — but the committed CSV/WP report λ=−0.180 (1.6×) and γ_HA=0.049.
- **Recommendation:** Replace hard-coded sentences with values computed from the live objects.
- **Status: confirmed (major).**

### M14. ha_y MPC labelled "at CCI=0" is actually Williams' CCI-PEAK value
- **Evidence (verified):** `williams_comparison.R:60` notes "Housing wealth MPC at CCI=0; falls to 0.0452 at peak." The source (Aust system paper p.12) says the opposite: 0.0488 is the value "at the peak of credit liberality, dropping to 0.0452 in 2008(2)," and at CCI=0 housing wealth "has no consumption impact: there is no classical housing wealth effect." So Spec 6 (no CCI interactions) is being compared against Williams' peak-CCI MPC while the label says CCI=0.
- **Recommendation:** Correct the note and the §7.3/§1.3/abstract framing to state 0.0488 is the CCI-peak value (Williams' CCI=0 housing MPC is ≈0).
- **Status: confirmed (major).**

### M15. networth_y_raw_proxy mixes $bn M3 with $m housing, making the M3 term numerically inert
- **Evidence (verified):** `australia_data_download.R:1088` builds `m3_household_proxy = m3_aggregate * wage_share/100`, and `m3_aggregate` is RBA D03 DMAM3N in **$ billion** (never passed through `rescale_abs`). At line 1237 it is added to `housing_wealth_proxy` (in $ million) before the 1988Q3 growth-rate splice (line 1247). The M3 term therefore contributes ~0.01% of the raw_proxy numerator — numerically inert — contradicting WP 3.12's framing of M3 as a meaningful component. (The final spliced series is not mis-scaled because the anchor cancels units; the defect is that the aggregate proxy is effectively housing-only.)
- **Risk:** A documented data component does not do what the paper says; a landmine for MARTIN if reused.
- **Recommendation:** Rescale `m3_household_proxy`/`m3_aggregate` to $ millions before forming the raw proxy, OR rebuild the aggregate proxy in dimensionless ratio form (like the disaggregated proxies).
- **Status: confirmed (major).**

### M16. Back-extension wealth proxies are generated regressors but Spec 6b/Spec 4-extended report uncorrected t-stats
- **Evidence (verified):** Spec 6b's "significant" λ=−0.229 (t=−4.17, vs Spec 6 t=−1.76) is fit on n=190 using four long-run regressors that are proxies anchored at a single 1988Q3 quarter and back-cast via M3, RBA credit, and hpi×pop. **Verifier correction:** the t-stats are Newey–West HAC (not plain OLS), so the precise issue is they are "not corrected for two-step generated-regressor variance" and anchor-sensitivity is untested.
- **Risk:** The only "significant" λ in the paper may be a proxy-construction artefact; this is the result the back-extension narrative leans on.
- **Recommendation:** Add an explicit caveat that extended-sample SEs are conditional on proxies measured without error and anchor-sensitivity is untested; ideally bootstrap over anchor/proxy construction.
- **Status: confirmed (major).**

### M17. Portable master_data.csv is stale (180 rows from 1980Q1) and still contains the +10.4% spurious 2011Q3 HPI step
- **Evidence (verified):** `master_data.csv` has 180 data rows; hpi col shows 2011Q2=342.24 → 2011Q3=379.90 (dlog = +10.44%) — exactly the bridge→current artefact §3.4.1 says was eliminated. The canonical RDS has 194 rows (1976Q3+) and a clean −1.78% at 2011Q3.
- **Risk:** Anyone reproducing from the portable CSV gets a different (corrupted) dataset; this is the artefact the paper specifically claims to have fixed.
- **Recommendation:** Regenerate `master_data.csv` from the current RDS (194×105), verify the 2011Q3 dlog matches, and update `data.md` row/col counts and start date.
- **Status: confirmed (major).**

### M18. Renv lockfile out of sync — cold restore would be incomplete
- **Evidence (verified):** `renv::status()` reports installed-and-used-but-unrecorded: **systemfit** (LIVES SUR), **car** (NLA Wald, `linearHypothesis`), **AER**, **forecast**, **lme4**, **quantreg**, plus transitive deps.
- **Risk:** A clean restore + run cannot reproduce the headline; CI/reproducibility fails.
- **Recommendation:** `renv::snapshot()`, add systemfit/AER/car/forecast to DESCRIPTION Imports, commit the lockfile, add a CI job doing clean restore + run-from-RDS.
- **Status: confirmed (major).**

### M19. Headline estimation/selection pipeline is structurally untested
- **Evidence (verified):** `tests/testthat.R` sources only `model_helpers.R`; `construct_permanent_income(_italy)`, `fit_ecm_spec`, `run_all_specifications`, `select_preferred_spec`, `fit_consumption_with_williams_cci`, and `forecast_at_window` (the latter in `oos_forecast.R:83`, not estimation.R) are never in test scope. The one PI helper under test (`compute_log_yp_over_y`) is dead code with a self-flagged argument bug.
- **Risk:** The leak-free property of PI, the BIC-comparability guard, and the OOS interaction handling have no regression tests — exactly the bugs found above.
- **Recommendation:** Source the helper portion of `australia_estimation.R` in the harness; add fixtures for the PI leak-free property, `fit_ecm_spec` invariants, and the OOS interaction-column presence.
- **Status: confirmed (major).**

### M20. Permanent-income method mislabelled "Jordà (2005) local projection"
- **Evidence (verified):** The forecaster is a single full-sample OLS of a pre-aggregated discounted MA of future log income on contemporaneous predictors — not a Jordà local projection (which regresses outcomes at horizon h on date-t info, one regression per h). It matches De Bonis et al. (2020) Appendix A.2 / Table A.3.
- **Recommendation:** Rename throughout (e.g. "Italy-style full-sample income-projection forecaster"), cite De Bonis et al. (2020) Appendix A.2 precisely, and remove the Jordà attribution.
- **Status: confirmed (major).**

### M21. Reference-fidelity errors that a careful referee will catch
- **LIVES acronym given two expansions; the §2.2 form is fabricated.** §1.1 (l.102): "Latent Interactive Variable Equation System" (correct, matches Duca & Muellbauer). §2.2 (l.302-303): "life-cycle Integration of Variable Expectations and Structure" (no source). **Fix:** delete the §2.2 form, use the source-verbatim expansion throughout. **(confirmed, major)**
- **Italian benchmark mis-cited as "De Bonis, Marino and Muellbauer (2024)."** The actual authors are De Bonis, Liberati, Muellbauer and Rondinelli (Banca d'Italia TD 1304, 2020). **Fix:** correct author list/year/venue in `.bib` and every in-text citation. (Drop the unsupported claim that a PDF title page was inspected — no Italian De Bonis PDF is in the repo; the source `References/Italy.pdf` is recoverable from git history.) **(confirmed, major)**
- **§7.3/§9 Spec 8 γ profile (γ_HA=0.028, γ_NLA=0.091) does not match the CSV.** `australia_full_results.csv` structural_param: ha_y=0.0218, nla_y=0.0979. **Fix:** recompute from the CSV after fixing λ. **(confirmed, major)**

---

## 5. Minor issues & polish

- **§6.3/§7.2 Spec 8 BIC=−930.6, adj-R²=0.798, DW=1.99 are stale** — CSV gives BIC=−948.5, adj-R²=0.821, DW=1.87; Spec 10 BIC −491.5 vs −493.7. (verified) Drive the tables from CSVs via Quarto chunks so they cannot drift. **(minor)**
- **§7.1/§7.2 AR(1)/AR(4) p-values and OK/rej labels disagree with the diagnostics CSV.** §7.1 quotes AR(1) p=0.30, AR(4) p=0.20; CSV gives 0.261/0.145. The §7.2 OK/rej column is inverted for several specs (Spec 6 shows "rej/rej" but both p>0.05 → should be OK/OK). (verified) Regenerate programmatically at a stated threshold. **(minor)**
- **Spec 6b sample size is 190 in the cointegration CSV but 180 in the diagnostics CSV** (verified — one routine drops 10 rows for a lagged regressor). Reconcile so all three artefacts use one n. **(minor)**
- **eq_y_proxy is zero-variance and super_y_proxy a deterministic linear ramp pre-1988**, yet both enter Spec 6b/4-extended as estimated wealth elasticities (verified: eq_y_proxy constant 0.5989 pre-1988; super_y_proxy constant first-difference ≈0.0124). Flag γ_EQ/γ_SUPER on the extended sample as identified almost entirely off the post-1988 segment. **(minor)**
- **RBA D02 total-credit splice imposes exactly zero QoQ growth at the 2019Q2→Q3 boundary** (no overlap; `australia_data_download.R:1006-1014`), contaminating `d2_log_creditd02_lag2` in Spec 6b. Estimate the boundary growth or dummy 2019Q3. **(minor)**
- **Spec 9 (Kalman CCI) interactions are not de-meaned while Spec 8 (Williams) are** — the `cci_method_comparison` presents them as "identical structure, only the CCI series differs," which is false. De-mean Spec 9 consistently. **(minor)**
- **`australia_cci_method_summary.md` coefficient table is corrupted** — every row repeats the intercept value (vector-recycling bug); the underlying CSV is correct. **(minor)**
- **Reduction drops ALL sign-violators simultaneously each iteration** (`australia_estimation.R:1763-1781`), departing from Williams'/Hendry–Krolzig "strongest-violator-first, one-at-a-time"; path-dependence is uncontrolled. Implement one-at-a-time reduction and report robustness across orderings. **(minor)**
- **Wald test refits Spec 6 with λ̂=−0.197 vs canonical −0.180** (a 9% drift in the denominator that maps OLS→γ); the companion footnotes it but the headline does not. Make the Wald test consume the cached Spec 6 fit. **(minor)**
- **Williams' single IFA calibration (0.022) is split into two arbitrary 50/50 Wald restrictions** (eq_y=0.011, super_y=0.011), inflating the joint df and testing a stronger hypothesis. Use one restriction eq_y+super_y = 0.022·|λ|. **(minor)**
- **House-price and mortgage ECM terms barely mean-revert** in the 4-eq SUR (λ_H=−0.037 t=−1.80; λ_M=+0.0015 wrong sign; λ_W=+0.067 wrong sign) — three of four auxiliary ECM terms fail; the paper notes HEW but not the mortgage equation. **(minor)**
- **PI helper functions** (`compute_log_yp_over_y`, etc., `model_helpers.R:1015-1207`) are dead code with a self-flagged argument bug — remove or quarantine. **(minor)**
- **Counterfactual 3 (CCI peak vs zero ≈ 0)** is mechanically zero by the de-meaning convention (`australia_counterfactuals_summary.csv` eos gap = −4.9e-18), not an economic finding. Relabel as a sanity check or drop. **(minor)**
- **Sample size quoted as n=194 (abstract/§3) but estimation/back-extension uses n=190**; the §3.14 tier table sums to 87. State "master n=194; largest estimable n=190" and annotate the tier table. **(minor)**
- **`cons_deflator_norm` normalised to 2015=100 in code but documented as 2022-23=100** in WP §3.1/data.md. One-line fix. **(nitpick)**
- **`pop_millions` actually holds thousands** (`australia_data_download.R:399`) — scale cancels in all current uses but is a MARTIN landmine; rename to `pop_thousands`. **(nitpick)**
- **Step-4/§F2 orchestration comments still say "Williams 4-knot spline"** though the basis is 15-knot maximal-GETS. Update comments. **(nitpick)**
- **Reference nitpicks (verified):** Williams' fourth canonical knot mis-dated 2007Q1 (source: 2007Q3); "Duca, Muellbauer and Tobin (2013)" adds Tobin as a co-author (actual: Duca and Muellbauer); Chauvin & Muellbauer (2018) mislabelled as a Banque de France WP (actual: *Économie et Statistique* 500-501-502). **(nitpick)**
- **NOTE — refuted finding dropped:** The claim that the methodology attributions (Jordà LP framing aside, plus Drehmann, γ_LA+γ_LOANS=0, SUR) are misattributed to the Italian paper was **refuted** — the actual Italian paper (recoverable as `References/Italy.pdf`) does use these devices. Only the "Jordà local projection" label itself is wrong (see M20).

---

## 6. MARTIN-readiness assessment (cross-cutting)

**Verdict: NOT READY.** MARTIN's consumption block (RDP 2019-07) is a long-run cointegrating relationship `rc = β1·hdy + (1−β1)·hnw + β2·RCR + β3·D2007Q1` with income+net-wealth coefficients **restricted to sum to one** (balanced growth), a calibrated real-rate semi-elasticity (~0.05%/100bp), a long-run net-wealth elasticity ~0.17, and no CCI mechanism. The preferred Spec 6 as estimated is incompatible on several gating points:

1. **Look-ahead PI is a showstopper (C1)** — the equation literally cannot be computed at forecast time. Replace with a recursive/expanding-window forecaster and re-estimate λ and the PI coefficient in real time.
2. **No established long-run (C2)** — fix the cointegration screen to Engle–Granger MacKinnon CVs; if nothing cointegrates on the operational sample, the long-run cannot be imposed as a MARTIN equilibrium.
3. **No valid inference (M1, M2)** — bootstrap/Pagan-correct the two-stage generated regressors; compute delta-method/bootstrap CIs on every structural γ and on λ. Only import coefficients that are stable and identified.
4. **Does not nest MARTIN's form** — re-specify the long run to put net wealth (or the wealth/income ratios) inside the cointegrating vector and test/impose the income+wealth=1 balanced-growth restriction.
5. **Thin channels should be calibrated, not estimated** — the interest-rate semi-elasticity is insignificant ≈0 (Spec 6 real_rate t=−0.17); benchmark it to MARTIN's 0.05%/100bp and present a single aggregate wealth coefficient with a CI rather than four offsetting insignificant components.
6. **CCI decision must be explicit** — either drop CCI (matching MARTIN) or carry only a short-run credit-growth control; do not propose the placebo-failing spline. State the effective 2003Q3+ / n=86 identification window wherever a MARTIN recommendation is made.
7. **Real-time OOS required** — fix the silent interaction-drop (C6), recompute PI per window, and report real-time RMSEs vs RW/AR(1). Position the contribution as structural interpretation, not forecast improvement.
8. **Rebuild back-extension proxies before any Spec 6b MARTIN use** (M15, M16, and the minor proxy issues).

---

## 7. Reproducibility status (smoke test)

The smoke test ran on R 4.5.3 (renv-activated). Results:

- **PASS** — All 27 project R files parse. The 24 `testthat` blocks pass. The fast re-estimation (`run_estimation_from_rds.R`) runs with no downloads and **reproduces the headline λ=−0.180 exactly** (Spec6 `ecm_lag` = −0.180107535147886, nw_se = 0.1025, t = −1.757, identical to WP §7.1) — a genuine strength; document this as the canonical headline-reproduction command.
- **FAIL** — `renv.lock` is incomplete (M18): systemfit, car, AER, forecast, lme4, quantreg + transitive deps are used but unrecorded; a cold restore would not reproduce the run.
- **PARTIAL** — WP Tables 6.3/7.2 BIC for Spec 8 (−930.6 vs CSV −948.5) and Spec 10 (−491.5 vs −493.7) are stale.
- **PARTIAL** — Spec 6 AR(1)/AR(4) diagnostic labels in Table 7.2 contradict regenerated p-values (0.261/0.145 → should be OK/OK, printed as rej/rej).
- **SKIP** — Cold rebuild with live ABS downloads not exercised.

Net: the *cached-RDS* path is reproducible and the headline λ is solid; the *cold-restore* path and several hand-transcribed table cells are not. The single biggest reproducibility win is to drive WP Tables 6.3/7.2/8.4 from the CSVs via inline Quarto chunks so they cannot drift, and to `renv::snapshot()`.

---

## 8. Numbers-consistency status (numbers audit)

Independently recomputed from the committed RDS/CSVs, the **modern-sample data statistics reconcile well** (NPY/GDI, FHB shares, burden correlation, net-worth proxy path, wage share all matched the WP). The **estimation-output-vs-prose layer does not.** Confirmed discrepancies, in descending severity:

- Spec 8 λ: paper −0.377 vs CSV **−0.445** (7 locations). [C3]
- §5.5/§8.4 Spec-8 interaction table: all four terms disagree; ha_y×CCI **sign-flipped** (paper −0.0020 vs CSV +0.00159). [C4]
- §5.1.1 CCI knots: paper six (with coefficients) vs CSV **three**. [C5]
- SUR ρ̂: paper ≈0.0007 vs CSV **−0.0045**. [M10]
- AR PI coefficient: paper "significantly negative −0.20" vs CSV **−0.003 (insignificant)**; "+0.30" flip target unsupported (actual +0.24). [M8]
- §7.4 table labelled Spec 1 but reports **Spec 2** numbers. [M9]
- §7.3/§9 Spec 8 γ profile (γ_HA=0.028, γ_NLA=0.091) vs CSV **0.022/0.098**. [M21]
- Spec 8/10 BIC, Spec 6 AR(1)/AR(4) labels: **stale/inverted**. [§7]
- Spec 6b n: **180 vs 190** across two CSVs. [§5]
- NPY robustness "λ +18% toward Williams": **directionally wrong** (actually −51%, away). [M11]
- Two incompatible "Williams Table 1" γ_NLA columns: **0.159 vs 0.066**. [M12]

The Wald χ²(6)=29.1 figure exists in `williams_calibration_wald.csv` but appears nowhere in `wp_draft.md` (it is a companion-paper claim) — and is itself affected by the C7 sign bug, so it should be recomputed before being cited anywhere.

**Bottom line:** the data layer is trustworthy; the estimation-results-in-prose layer needs a single disciplined pass that regenerates every reported coefficient, λ, BIC, diagnostic, and ρ̂ directly from the committed CSVs (ideally via Quarto inline chunks), after the look-ahead PI and cointegration-screen fixes change the underlying numbers.
