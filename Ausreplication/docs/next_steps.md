# Next steps — modelling backlog

A structured list of outstanding modelling work. Each item is written as
a self-contained prompt that an AI coding agent (Claude Code / similar)
could be asked to implement. Items are tagged with:

- **Effort**: hours / days / weeks
- **Depends on**: prerequisite items (and external data, where relevant)
- **Decision point**: items that require user judgement before an agent
  can sensibly act
- **Files**: principal files to touch
- **Success criterion**: how to know it's done

Item numbers are stable identifiers; new items are appended rather than
renumbered. Strike completed items in place rather than deleting them.

---

## Tier 1 — Quick wins (hours)

### NS-001 Auto-fill WP table placeholders from CSVs

The WP draft (`Ausreplication/docs/wp_draft.md`) has `[TABLE-FROM-DATA]`
placeholders in §7, §8 and §9 that point to specific output CSVs. Build
a small R helper that reads each CSV and emits a markdown table with
appropriate columns + caption + footnotes.

- **Effort:** half a day
- **Depends on:** nothing
- **Files:** new `Ausreplication/R/build_wp_tables.R`; references `Ausreplication/docs/wp_draft.md`
- **Success criterion:** running `Rscript Ausreplication/R/build_wp_tables.R` produces an `Ausreplication/outputs/wp_tables.md` file with one section per WP placeholder, ready to splice into the draft. Tables formatted to 3-4 significant figures; significant-at-1%/5% terms emphasised.

### NS-002 Verify the [VERIFY] citation tags in the lit review

The lit review (§2 of wp_draft.md) has ~9 inline `[VERIFY]` tags marking citations the agent wasn't 100% certain of. Resolve each: confirm year, journal/series, page numbers, exact title.

- **Effort:** 1-2 hours per tag
- **Depends on:** access to academic search (Google Scholar, RBA website)
- **Files:** `Ausreplication/docs/wp_draft.md` (replace [VERIFY] with confirmed citations); update References section
- **Success criterion:** zero remaining [VERIFY] tags; each citation has exact title, year, series/journal, volume/issue/page numbers if applicable, and a URL or DOI in a comment

### NS-003 Document houseprice_old.csv provenance

`Ausreplication/data_raw/houseprice_old.csv` (77 rows, 1986Q2-2003Q3) has no recorded source. Likely an old ABS Cat 6416.0 vintage but unconfirmed. Add provenance metadata.

- **Effort:** ~1 hour
- **Depends on:** access to ABS historical data services (or recall from whoever sourced the file originally)
- **Files:** `Ausreplication/docs/data.md` §4.1 (replace "UNDOCUMENTED IN-REPO" with full provenance); optionally add a sidecar `houseprice_old.metadata.txt`
- **Success criterion:** The data.md entry for this file lists: original ABS catalogue number, table number, vintage release date, retrieval URL, retrieval date, and the chosen base year if rescaled

### NS-004 Verify APRA macroprudential dates against actual policy announcements

The smooth-transition dummies `d_apra_2014` and `d_apra_2017` are centred on 2014Q4 and 2017Q2 respectively, with `half_width = 2.5` (quarters). These dates were chosen from press summaries. Cross-check against APRA's actual policy chronology.

- **Effort:** half a day
- **Depends on:** APRA Information Paper "Macroprudential Policy" historical timeline; media releases archive
- **Files:** `Ausreplication/R/australia_data_download.R` Section 5 (potentially update centre dates and/or `half_width`); `Ausreplication/docs/data.md` §8 (document the verification)
- **Success criterion:** Each macroprudential dummy's centre date matches APRA's announced policy date (not press-coverage date); half-width is justified either by the announced phase-in period or by a sensitivity test
- **Sensitivity:** While verifying, run a quick `half_width ∈ {1.5, 2.5, 4.5}` sensitivity on Spec 6's `ecm_lag` to confirm robustness

### NS-005 Regenerate decomposition chart under Italy LP PI method

The headline `australia_longrun_decomposition.png` is currently produced under the default `PI_METHOD = "ar"`. Generate a parallel chart under `PI_METHOD = "italy"` for comparison.

- **Effort:** ~1 hour (just flip the flag and rerun, save chart with a different filename)
- **Depends on:** nothing
- **Files:** `Ausreplication/R/australia_estimation.R` (modify the `plot_longrun_decomposition` call to optionally tag filename with PI method); `Ausreplication/outputs/australia_longrun_decomposition_italy.png` (new)
- **Success criterion:** Both charts exist; the Italy version shows the post-2008 PI step shift much more strongly (reflecting the +0.30 vs +0.20 sign-flip on `ln_yp_over_y`)

### NS-006 Add Spec 7b: cohort + RBA-measured mortgage payment burden

Currently Spec 7 uses the synthetic `mortgage_burden` (= debt × rate / income) which is biased ~30% high in level vs the RBA E13 measured payment burden (cor = 0.93 over 2009Q1+ overlap). Add a Spec 7b that uses `mortgage_payment_burden_rba` over the post-2009 sample.

- **Effort:** ~30 min to add the spec, half a day to write up the result
- **Depends on:** nothing (RBA E13 already wired in)
- **Files:** `Ausreplication/R/australia_estimation.R` `run_all_specifications()` (add Spec 7b after Spec 7); `Ausreplication/docs/wp_draft.md` §8 (add a paragraph)
- **Success criterion:** A new spec appears in `australia_full_results.csv` with `mortgage_payment_burden_rba` as a long-run regressor over the 2009Q1+ sub-sample. Spec 7b's `mortgage_burden`-equivalent coefficient is documented vs Spec 7's synthetic equivalent.

### NS-007 PI sensitivity on the discount rate

The default `delta = 0.95` quarterly (~5% per quarter discount) follows Williams. Italy and France use the same. Run a focused sensitivity on Spec 6 over `delta ∈ {0.85, 0.90, 0.93, 0.95, 0.97, 0.99}` with both PI methods, report `lambda` and `ln_yp_over_y` coefficients.

- **Effort:** half a day
- **Depends on:** nothing
- **Files:** new `Ausreplication/R/pi_discount_sensitivity.R`; `Ausreplication/outputs/australia_pi_discount_sensitivity.csv`
- **Success criterion:** A 12-row CSV (6 deltas × 2 methods) showing both coefficients. Result is referenced as a robustness footnote in the WP.

---

## Tier 2 — Medium WP work (1-3 days)

### NS-010 Draft the WP Introduction

The WP draft (`Ausreplication/docs/wp_draft.md`) §1 is currently bullet points. Write a proper 3-page introduction matching the lit review's voice. Should: (a) motivate the paper, (b) state the three contributions explicitly, (c) preview the headline result, (d) note the methodology pivot (Italy LP), (e) acknowledge the binding obstacle (RBA pre-1988 data) and how the paper is honest about it, (f) signpost the structure.

- **Effort:** 2 days writing + revision
- **Depends on:** §2 lit review (done); §10 decomposition (partially drafted)
- **Files:** `Ausreplication/docs/wp_draft.md` §1
- **Success criterion:** ~1,500-2,000 words (3 pages of an RBA RDP). Reads as substantive academic prose, not a bulleted summary. Numbers in the headline-result paragraph match the latest pipeline run.

### NS-011 Draft the WP Conclusion

§11 is currently a paragraph-length skeleton. Write a proper conclusion summarising the contribution, listing the limitations honestly, and pointing to the companion-paper extensions (multi-equation LIVES; sample back-extension once RBA delivers; counterfactual policy work).

- **Effort:** half a day
- **Depends on:** the rest of the WP being settled
- **Files:** `Ausreplication/docs/wp_draft.md` §11
- **Success criterion:** ~500-800 words. Includes a "limitations and future work" subsection with the three companion-paper extensions and an honest treatment of the partial-CCI-identification problem.

### NS-012 Implement the three counterfactuals for §10

§10.2 of the WP draft suggests three counterfactual exercises:
(i) no-APRA-2014/2017 tightening
(ii) no-JobKeeper
(iii) CCI=0 baseline vs CCI=peak (Williams' regime)

Build each as a function in `australia_estimation.R` that takes the preferred spec and zeros out (or shifts) the relevant regressor(s), producing a counterfactual fitted-log(c/y) path. Plot all three plus the baseline.

- **Effort:** 1-2 days for all three plus chart polishing
- **Depends on:** Spec 6 (or 8 for CCI counterfactual) is settled; PI method is settled
- **Files:** new section in `australia_estimation.R` (`run_counterfactuals()`); new outputs `australia_counterfactual_apra.csv`, `australia_counterfactual_jobkeeper.csv`, `australia_counterfactual_cci.csv`; new chart `australia_counterfactuals.png`
- **Success criterion:** Each counterfactual reports the implied path of log(c/y), the difference from baseline, and the 4-quarter cumulative consumption gap. Chart shows all three on one panel with annotations of the policy episode dates.

### NS-013 Build a Quarto/RMarkdown rendering pipeline for the WP

The current draft is plain markdown. For a polished central-bank submission you'll want LaTeX or PDF output with proper figure/table cross-referencing, citation management via BibTeX, and consistent typography. Quarto handles this.

- **Effort:** 2-3 days (one to set up; rest is iteration on style)
- **Depends on:** WP draft sufficiently stable that the structure won't change much
- **Files:** new `Ausreplication/docs/wp_draft.qmd` (Quarto version of wp_draft.md), `Ausreplication/docs/wp.bib` (BibTeX), `Ausreplication/docs/_quarto.yml` (rendering config); add `quarto` to README dev requirements
- **Success criterion:** `quarto render Ausreplication/docs/wp_draft.qmd` produces a PDF that looks like a central-bank WP (RDP / TPRP / Bank Italy temi). All citations resolve from the .bib. All cross-references are clickable. CI is updated to render the PDF on push.

### NS-014 Auto-generated WP figures section

Currently figures are produced separately by the pipeline; they're referenced in the WP but not auto-pulled. Build a system where each figure has a stable filename, a caption stored alongside, and a Quarto include that pulls them in.

- **Effort:** 1 day (depends on NS-013)
- **Depends on:** NS-013
- **Files:** `Ausreplication/docs/figures.qmd` (new); modify `australia_estimation.R` to write a captions sidecar JSON
- **Success criterion:** Each WP figure can be regenerated by a single Rscript invocation; the Quarto build pulls them in with their captions automatically; no figure paths are hard-coded in the WP draft

### NS-015 Add a "Williams-prior" calibrated specification (Spec 9)

Williams (2010) calibrates rather than estimates several parameters: γ_2 (illiquid wealth MPC) at 0.01, ψ_0=0.20, ψ_1=0.93 such that ψ at CCI peak = 0.95, ϖ=1.2 in (1−ϖ·CCI). Add a Spec 9 that imposes Williams' priors and reports the resulting fit.

- **Effort:** 1-2 days
- **Depends on:** Spec 8 framework (done); willingness to constrain coefficients in OLS (use `nlxb` or restricted OLS)
- **Files:** `Ausreplication/R/australia_estimation.R` (new helper for restricted estimation); new spec; `australia_williams_prior_spec.csv`
- **Success criterion:** Spec 9 reports OLS coefficients on free terms (lambda, the wealth coefs) given Williams' priors are imposed on the others. Compare implied lambda and adjusted R^2 against unrestricted Spec 6.

---

## Tier 3 — Methodological extensions (days–weeks)

### NS-020 Sample back-extension Phase 1 — public-data backbone

Build the publicly-reproducible pre-1988 backbone using Williams' recipes from WP492:

- LA pre-1988: M3 (RBA D03) × household factor income share (ABS 5204-06), chained at 1988Q2 onto the existing B20-derived deposits series
- HC pre-1988: D02 level at 2008Q2 back-cast via D01 growth rates to August 1976
- Demographics: ABS Cat 3201 annual age-share series → cubic-spline to quarterly back to 1971
- House prices pre-1986: REIA series spliced if available (otherwise note BIS Shrapnel as the next gap to fill)

Wealth aggregates `ha_y` and `ifa_y` will remain NA pre-1988 until RBA delivers the unpublished series, but everything else extends. This unlocks Spec 1-3 (aggregate net-worth) on the longer sample.

- **Effort:** 3-4 days
- **Depends on:** RBA D01, D02, D03 historical data (publicly available via RBA chartpacks); ABS 5204-06 historical (Time Series Service if needed); ABS 3201 historical
- **Files:** `Ausreplication/R/australia_data_download.R` (new sections for pre-1988 splicing); `Ausreplication/docs/data.md` (new §10 "Sample back-extension")
- **Success criterion:** `master` has non-NA values for `cons_real_pc`, `ydi_real_pc`, `mortgage_rate`, `unemp_rate`, `prime_age_share`, `lf_share`, `cci_ratio`-equivalent, `nla_y` (deposits-only, debt-netted where available), `debt_y`, `hpi`, `ln_hp_over_y` back to **1976Q3**. Pipeline runs end-to-end without assertion failures. Spec 1 fits on the longer sample with all coefficients within 1.5 SE of the 1988+ baseline.

### NS-021 Source BIS Shrapnel pre-1978 house prices via Treasury

Williams (2010) splices BIS Shrapnel data 1972Q3-1978Q2 before the REIA segment, sourced via Treasury. If accessible, this extends house prices back to 1972Q3. Without it, NS-020's house-price series stops at 1978Q3 (REIA start).

- **Effort:** 1-2 days of data sourcing, 1 day of integration
- **Depends on:** Treasury contact
- **Decision point:** whether to chase Treasury given diminishing marginal returns relative to NS-020 + RBA-supplied HA/FA
- **Files:** `Ausreplication/data_raw/bis_shrapnel_hpi.csv` (new); `Ausreplication/R/australia_data_download.R` House Price section (extend the splice chain)
- **Success criterion:** `hpi` is non-NA back to 1972Q3 in the back-extended master. The splice scale factors at the join quarters (1978Q2-1978Q3 BIS→REIA, 1986Q1-1986Q2 REIA→ABS-old, 2002Q4-2003Q1 ABS-old→ABS-new) are documented in data.md.

### NS-022 Replace synthetic with measured mortgage burden in Spec 7b only (E13 over post-2009 sample)

Already partially scoped under NS-006. Decide whether the full WP Spec 7 should switch to the RBA-measured payment burden (losing pre-2009 data) or keep the synthetic with a footnote.

- **Decision point:** This is a research-design call. My recommendation: keep Spec 7 with synthetic burden for sample reasons; Spec 7b is the explicit measurement-quality robustness column for §8.
- **Effort:** 1 day
- **Files:** as in NS-006
- **Success criterion:** WP §7-§8 has both versions, with the "level vs cycle" decomposition (RBA series captures level correctly; synthetic captures cycle correctly with cor=0.93) explicitly discussed

### NS-023 Pin ABS series IDs

Currently the data download uses regex pattern matching against ABS series names (e.g. `^FINAL CONSUMPTION EXPENDITURE.*Chain volume`). A future ABS rebase that renames a series will silently switch to a different vintage. Pin specific series IDs (e.g. `A2304402X`) where stable IDs exist.

- **Effort:** 1 day for an audit pass + verification
- **Depends on:** running the pipeline cold and inspecting which series get picked
- **Files:** `Ausreplication/R/australia_data_download.R` (replace regex `pick_abs()` calls with `pick_abs_by_id()`)
- **Success criterion:** Each ABS series is identified by its stable series_id rather than a regex pattern; an integration test confirms the loaded series matches the previous regex-picked series

### NS-024 Audit unused 5232.0 sectoral workbooks for bonds-as-separate-bucket

`data_raw/` contains 60+ ABS Cat 5232 workbooks beyond the 5232035 used in the model. Italy treats bonds as a semi-liquid bucket distinct from equities/super (Italy.pdf §3.2). Investigate whether one of the unused sectoral workbooks gives a clean Australian household bonds series; if so, add as a separate wealth bucket.

- **Effort:** 1-2 days
- **Depends on:** nothing
- **Files:** new section in `australia_data_download.R`; potential new `bonds_y` variable
- **Success criterion:** Either: (a) a `bonds_y` series is added with documented coverage, used in a "Spec 4-bonds" robustness column; or (b) a documented finding that the ABS does not separately report bonds in the Australian household balance sheet, with a note for the data appendix

### NS-025 Implement the income volatility predictor properly

The current `abs_income_resid` is the absolute value of the AR(8) residual on log income. Italy uses |ε̂| from an AR(2) on income (Italy.pdf p.22 short-run regressor). Verify the AR order matters; consider also a simpler GARCH(1,1) volatility proxy. This is the proxy for precautionary saving in §6.

- **Effort:** 1 day
- **Depends on:** nothing
- **Files:** `Ausreplication/R/australia_estimation.R` `compute_income_volatility()` (currently AR8); add an `AR_ORDER` config flag like `PI_METHOD`
- **Success criterion:** Spec 6 is refit with AR(2), AR(4), AR(8), and GARCH(1,1) volatility proxies; the `abs_income_resid` coefficient is reported across the four; sensitivity is logged in a new robustness CSV

### NS-026 Add a rolling-CCI-inclusive specification

The Williams 4-knot CCI is currently used only in Spec 8. Add a parallel rolling estimation (10-year windows) of Spec 6 with `cci_williams` as a control, to test whether the post-2007 CCI tightening is identifying any additional consumption-equation variation beyond what the COVID dummies capture.

- **Effort:** 1 day
- **Depends on:** Williams CCI being identified (works on current sample with 2 surviving knots; better with NS-020)
- **Files:** `Ausreplication/R/australia_estimation.R` `fit_rolling_window()` (extend); new chart
- **Success criterion:** Rolling-coefficient chart on `cci_williams` shows whether the loading is stable post-2007; if it isn't, the WP §10 policy implications discussion needs adjusting

---

## Tier 4 — Large rebuilds (weeks–months)

### NS-030 Sample back-extension Phase 2 — RBA unpublished HA + FA

Once the RBA delivers their unpublished pre-1988 housing wealth and total financial assets series (Williams 2010 cites these as supplied "on request"), splice them onto the existing 1988+ disaggregated wealth. Apply Bonci-Coletta-style break adjustments at the 1989 ABS sectoral reclassification.

- **Effort:** 1-2 weeks once data arrives
- **Depends on:** RBA delivery (in progress per user message)
- **Decision point:** Splicing methodology — Bonci-Coletta (Italy paper Appendix A.2) vs Williams' MSMEAS smoothed-step approach (Aust paper p.16-17). Likely mix.
- **Files:** `Ausreplication/data_raw/rba_pre1988_hh_wealth.csv` (new); extend `australia_data_download.R`
- **Success criterion:** Wealth ratios `ha_y, eq_y, super_y, nla_y, networth_y` extend back to 1977Q1. Spec 1, 4, 6 refit on extended sample. Williams 4-knot CCI 1979 knot identifies (passes sign prior). Comparison report in WP §9 updated with the back-extended results.

### NS-031 Multi-equation LIVES system

Estimate the full four-equation LIVES system (consumption, house prices, mortgage stock, home equity withdrawal) jointly by FIML, with the Williams CCI spline identified as a common factor across equations. This is what Williams (2010) and Muellbauer-Williams (2012) actually do; we currently estimate only the consumption block.

- **Effort:** Months. This is a major rebuild.
- **Depends on:** NS-030 (sample back-extension); also requires constructing house-price equation, mortgage-stock equation, HEW equation following Williams' Tables 2-4
- **Decision point:** Whether the central-bank WP should be the single-equation paper with this as a follow-up, or whether to wait and produce the full system. Recommendation: ship single-equation now, full system as companion.
- **Files:** new `Ausreplication/R/lives_system.R`; new sections in WP draft
- **Success criterion:** All four equations estimate jointly with FIML; the common-factor CCI spline is identified by the cross-equation restrictions; the consumption-equation coefficients change by less than 30% from single-equation OLS estimates on the same sample

### NS-032 Multi-country comparison harmonisation

Build a single comparison table that puts our Australia results alongside Italy (De Bonis et al. 2024), France (Chauvin-Muellbauer), UK (Aron-Muellbauer-Murphy 2012), US (Duca-Muellbauer-Murphy 2010), and the original Williams Australia. Standardise units, sign conventions, sample windows. This is Italy.pdf Table 4 plus Australia.

- **Effort:** 1 week
- **Depends on:** access to the original published coefficient tables for each country (we have them embedded in the relevant PDFs)
- **Files:** new `Ausreplication/R/cross_country_comparison.R`; `australia_cross_country_comparison.csv` (~20 rows × 6 columns)
- **Success criterion:** Side-by-side structural parameters (housing MPC, illiquid MPC, NLA MPC, ψ, λ) for 6 countries. Sample windows clearly labelled. Notes on calibration vs free estimation per country. Reference for the WP §9 international-comparison paragraph.

### NS-033 Out-of-sample forecast validation

Hold out the last 4 quarters of data, refit the preferred spec on 1988Q4-2023Q4, and forecast 2024Q1-2024Q4. Report RMSE and 95% prediction intervals. Repeat with a 4-quarter rolling horizon over 2015-2024 to test the model's forecasting properties throughout the recent macroprudential and pandemic periods.

- **Effort:** 2-3 days
- **Depends on:** preferred spec is settled
- **Files:** new `Ausreplication/R/forecast_validation.R`; new outputs `australia_oos_forecast.csv`, `australia_rolling_oos_forecast.png`
- **Success criterion:** Forecast RMSE on quarterly Δlog(c) is reported alongside a benchmark random-walk-with-drift forecast. Rolling-window RMSE is plotted; periods of larger error are documented and interpreted

---

## Tier 5 — Engineering / housekeeping

### NS-040 Add unit tests for compare_pi_methods

The new `compare_pi_methods()` function is non-trivial — fits two specs, joins, pivots. Add testthat tests covering: (a) the function runs end-to-end on a fabricated minimal model_data, (b) the wide-format output has the expected columns, (c) the two methods produce different coefficients (i.e. no silent identity collapse).

- **Effort:** 2 hours
- **Depends on:** nothing
- **Files:** new `tests/testthat/test-pi_methods.R`
- **Success criterion:** 4-5 test_that blocks; all pass

### NS-041 Profile and cache PI computations

The Italy LP method is fast (single regression). The AR method is slow because of the rolling-window forecast loop (`min_train` iterations × 40-step forecast each). Profile and consider caching: the PI series for a given `(model_data, k, delta, gfc)` configuration is deterministic.

- **Effort:** 1 day
- **Depends on:** nothing
- **Files:** `Ausreplication/R/australia_estimation.R` `construct_permanent_income()` (add caching)
- **Success criterion:** Pipeline runtime drops from current ~30s to <15s for run_estimation_from_rds.R

### NS-042 Add a regression test for the master-data round-trip

The CSV round-trip can drift bits at machine precision, which we documented. Add a CI-runnable test that confirms the round-trip diff is below 1e-9 for every numeric column, and that the spec selector picks the same preferred spec under both paths (currently it doesn't always — Chow-borderline cases flip).

- **Effort:** 1 day
- **Depends on:** nothing
- **Files:** new `tests/testthat/test-csv_roundtrip.R`
- **Success criterion:** Test runs in CI and either (a) confirms identical preferred-spec selection, or (b) flags the divergence as expected on Chow-borderline cases. Docs updated to make the CI behaviour visible.

### NS-043 Clean up unused 5232 workbooks

`Ausreplication/data_raw/` has ~60 ABS Cat 5232 workbooks that are not used. Either wire one in (NS-024) or delete. Same for the `5204055011do001-005` workbooks.

- **Effort:** 1 hour (decision); hours to delete
- **Decision point:** Are any of these expected to be useful for future work?
- **Files:** `Ausreplication/data_raw/` cleanup
- **Success criterion:** `data_raw/` contains only files used by the pipeline (or deliberately retained with documented future use)

### NS-044 Migrate to a Quarto book for the project

The project has accumulated three documentation files (data.md, project_status.md, wp_draft.md, next_steps.md), the main outputs (charts, CSVs), and the source code. A Quarto book would make this a single navigable artefact.

- **Effort:** 1 week
- **Depends on:** NS-013 (Quarto setup)
- **Decision point:** Whether the project is best served as: (a) WP + reproducibility kit, (b) Quarto book, (c) both
- **Files:** `_quarto.yml`, restructuring
- **Success criterion:** `quarto render` produces a navigable book covering README, data documentation, project status, WP draft, and an appendix with all output charts and tables

---

## Items requiring user judgement (no agent can decide)

### NS-100 Choose canonical PI_METHOD for the WP

`PI_METHOD = "ar"` is the current default; `"italy"` delivers the headline result. The WP §4.3 needs a clear methodology choice. My recommendation: **canonical = "italy"** with `"ar"` as the headline robustness column, justified by (a) the Italian methodology precedent, (b) the resolution of the Australian PI puzzle, (c) the closer match to Williams' λ, (d) the labour-force-share predictor's substantive role in capturing demographic effects. But it's a judgement call.

- **Decision needed**: Choose canonical. If "italy", the default `PI_METHOD <- "ar"` line in `australia_estimation.R` should be flipped, and downstream specs/charts/tables regenerated.

### NS-101 Multi-equation LIVES extension scope

Three options for the WP framing:
- **(A) Stay single-equation** — rebrand as "Australian counterpart to the consumption equation in the LIVES system". Lowest effort. Most defensible against critique that wealth/credit are endogenous.
- **(B) Two-equation system** — consumption + housing wealth, by SUR. ~2 weeks. Addresses the most pointed wealth-endogeneity critique.
- **(C) Full LIVES port** — months. Faithful to Williams 2010.

- **Decision needed**: Pick A, B, or C. NS-031 implements C; A is essentially "do nothing"; B would be a separate Tier-3 item to add.

### NS-102 Identify target journal / publication channel

Options:
- RBA Research Discussion Paper series (in-house, fastest)
- Treasury Tax Policy Research Paper (similar)
- SSRN preprint then journal (Economic Record? Australian Economic Review?)
- Submit to an international journal directly (Journal of Monetary Economics, Review of Income and Wealth, Economic Modelling)

- **Decision needed**: Affects the WP format, length, and tone. Currently drafted in central-bank-WP style, which is closest to RBA RDP / Treasury TPRP.

### NS-103 BIS Shrapnel sourcing

NS-021 needs Treasury contact. Decision: chase or skip?

### NS-104 Companion paper structure

Several large items (multi-equation LIVES, full sample back-extension, counterfactual policy simulator) could be separate companion papers. Decision: which go in the headline WP, which in companion(s)?

---

## Recently completed (for reference)

- **A.0** ✅ Williams (2009) §4.2.1 non-property income recipe (`npy_real_pc`)
- **A.1** ✅ Williams (2010/2012) structural-parameter comparison table
- **Italy LP PI helper** ✅ `construct_permanent_income_italy()` + `compare_pi_methods()` + `PI_METHOD` flag
- **Three user-supplied CSVs** ✅ RBA F6 mortgage rate, ABS 15+ population, ABS 5206020 income components — all wired in
- **RBA E13 burden ratios** ✅ `mortgage_interest_burden_rba`, `mortgage_payment_burden_rba`
- **NLA cross-equation restriction** ✅ Wald test, accepted in every spec/sample
- **Williams 4-knot CCI** ✅ Reduced-form spline, 2 of 4 knots survive on 1988+ sample
- **WP draft** ✅ §2 Lit Review, §3 Data, §4 Model, §5 Identification, §6 Specs, §7 Results, §8 Robustness, §9 Williams comparison fully written. §1 Intro, §10 Decomp counterfactuals, §11 Conclusion still skeletal.

---

**Last updated:** generated alongside wp_draft.md commit. Update timestamps as items move between tiers or are completed.
