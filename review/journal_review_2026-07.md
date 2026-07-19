# Pre-submission journal review — July 2026 (multi-agent)

**Date:** 2026-07-19 · **Scope:** publication readiness of `Australia/docs/wp_draft.md` for an academic journal. 7 reviewers (citation/bib mechanics, bibliographic web-verification, literature currency, presentation/journal fit, source fidelity vs the References PDFs, and two Opus referee reports), 13 factual fatal/major findings adversarially verified (all confirmed), fabricated-citation finding verified directly against the source PDF. Builds on, and does not re-litigate, the June/July 2026 code audits.

## Overall verdict

**Not submittable as-is; a strong paper is inside it.** The research content is credible and the honesty is a genuine asset, but the manuscript would be desk-rejected on mechanics alone (length, abstract, bibliography integrity, internal-project narration) before any referee reads the economics. Beyond mechanics, both referee reports converge on the same conclusion: the paper's **positive framing** ("Williams-consistency, form is decisive, structure transfers") is its weakest posture — each headline positive claim has a confound or an inference gap a referee will find — while its **negative/methodological content** ("what a single equation can and cannot identify about the LIVES credit mechanism") is near-unrejectable. Reframe, restructure, and fix the inference gaps below, and this is a credible submission to Economic Record or IJCB.

---

## A. Desk-rejection items (mechanical, cheap, do first)

1. **Fabricated citation (VERIFIED against source PDF).** "Duca, Muellbauer and Murphy (2013)" (lines 2417, 2420, 4534) does not exist: the Tobin-LIVES paper (`References/Duca_Muellbauer_Tobin LIVES DP July 2013r.pdf`, ECB WP 1581) is by **Duca and Muellbauer only**. The bib carries BOTH a correct `duca_muellbauer_2013` entry and a duplicate `duca_muellbauer_murphy_2013` with the phantom author and a fabricated venue ("European Commission, European Economy Discussion Paper 14"). It is argument-bearing (§5.3's multi-equation state-space identification claim). Fix: delete the phantom entry, repoint the three cites, and re-check the attributed claim against the real paper.
2. **Missing bib entries for cited works:** De Bonis, Liberati, Muellbauer & Rondinelli (2020) — cited 15+ times as the core methodological precedent, absent from the bib (an orphaned, unverifiable "De Bonis, Marino & Muellbauer (2024)" entry sits there instead — delete it, add the real 2020 Temi di discussione 1304); Ando & Modigliani (1963) — cited, no entry.
3. **Abstract is ~1,260 words** (target ≤250) and contains internal notation.
4. **Length ~48,500 words / 104pp** — 3–5× journal norms. Triage: keep a ~10–12k-word main paper; move §3.10–3.15 (data minutiae) and most of §8's twenty robustness subsections to an online appendix.
5. **Internal revision history narrated to the reader:** ~37 "earlier draft" references plus pipeline/repository/fix language ("the pipeline-isolation fix", "the deflator fix") must be removed wholesale, not softened.
6. **356 inline CSV filenames as evidentiary citations; no numbered figures.** Replace with Table N / Figure N apparatus.
7. **Spec-number prose.** Lead with three named specifications (faithful LIVES; conventional baseline; calibration-imposed); demote the other nine to the appendix.

## B. Referee 1 — econometric inference (all verified; each has a cheapest-credible-fix)

1. **The long-run apparatus rests on an unestablished equilibrium.** Engle–Granger fails for every spec, and the paper's fallback ("imposed unit-income vector → stationary c/y") is asserted, never tested. *Fix:* ARDL/bounds test (Pesaran-Shin-Smith) on the preferred specs + a stationarity test on the imposed equilibrium ratio; if bounds also fail, demote all long-run/structural language — this is the one genuine **fatal risk**.
2. **The novel "reject Williams" inferences use CIs the paper admits are anti-conservative** (two generated regressors + pre-tested spline held fixed). Admission without correction will not survive: a referee will demand a nested bootstrap (re-estimate PI + re-select knots inside each draw). Feasible with the existing pipeline; days not weeks.
3. **The real-time PI sign flip.** A headline structural channel reverses sign under the causal measure; the "measurement, not forecast" framing is referee-bait. *Fix:* make the real-time variant the inferential basis (or symmetric headline), keep the full-sample as descriptive.
4. **Placebo scope.** The 84th-percentile placebo validates the additive knot-selection stage, not the multiplicative Spec-11 deployment. *Fix:* run the deployed-protocol placebo ON the Spec-11 structure (script exists; cheap).
5. **"Structure transfers" vs wrong-signed channels.** Two of the largest long-run channels are wrong-signed vs theory; the thesis sentence must be qualified to the channels that actually transfer.

## C. Referee 2 — contribution & framing (all verified)

1. **"Form is decisive" is confounded:** Spec 11 (n=146) vs Spec 6 (n=86) differs in sample (+70%) AND CCI series, with no ablation. *Fix (the single most valuable new exercise):* estimate Spec 11 on Spec 6's exact sample/CCI and decompose the difference — form vs sample.
2. **The LIVES-distinctive credit channels are the ones that fail** (housing×CCI, affordability, rate interactions insignificant/wrong-signed/post-2007-identified), yet the paper claims to validate the LIVES form off the credit-invariant MPCs and PI. The abstract must say which channels come alive and which do not.
3. **"Calibrations don't transfer" leans on ψ≈1.0–1.1,** which the paper itself flags as inadmissible and sign-reversing in real time — weakest possible support for a headline.
4. **A load-bearing reconciliation depends on the (stale, unsubmitted) companion paper.** Either inline the needed result or cut the dependency.
5. **Recommended reframe:** from "Australian LIVES estimate consistent with Williams" to **"what a single aggregate equation can and cannot identify about credit-conditioned consumption"** — same content, near-unrejectable posture, and the honesty becomes the contribution. Title should change accordingly.

## D. Source fidelity (verified vs the References PDFs)

§4.1's canonical equation is **faithful** to Muellbauer–Williams Eq (7) (all six channels, ψ(CCI), ϖ=1.2, de-meaning), and every calibration value matches Table 1 col 1 exactly — no fatal fidelity error. Fix the labeling: (a) "Williams Table 1" is attributed to the 7-page BIS chapter, which contains no such table — cite the actual system paper; (b) the 0.0488 housing target is a derived peak-MPC mislabeled as a Table-1 coefficient (Table 1 has γ₁=0.0606); (c) LIVES acronym mis-expanded in §4.1; (d) a "binding cap" attributed to Williams that his system never reaches; (e) §5's description of Williams' 2009 CCI construction is imprecise.

## E. Literature additions a referee will demand

May, Nodari & Rees (2020, AER — the direct Australian comparator; its absence is the most likely referee flag) · De Bonis, Liberati, Muellbauer & Rondinelli (2023 sequel to the paper's core source) · Geiger, Muellbauer & Rupprecht (2016, German LIVES application, missing from §2.2's country list) · generated-regressor/pre-test inference cites (Pagan 1984; Murphy–Topel; pre-test literature) given the paper flags the problem itself. §2 is otherwise unusually well-integrated; its currency boundary is ~2019.

## F. Target journals

1. **Economic Record** — natural home (Australian empirical macro); needs the full length triage; the Australian-data contribution carries weight.
2. **International Journal of Central Banking** — fits the credit-conditions/policy-model angle (MARTIN relevance); slightly more tolerant of length; the reframed "what one equation identifies" version fits its methods-aware readership.
3. **Economic Modelling** — fallback; values the model-comparison exercise; fastest route but lower signal.

## Prioritized plan

- **Tier 1 (mechanical, ~2–4 days):** items A1–A7, D(a–e), E additions. No new estimation.
- **Tier 2 (substantive, ~1–2 weeks, decides the paper's ceiling):** B1 bounds/stationarity test; B2 nested bootstrap; C1 ablation (Spec 11 on Spec 6 sample); B4 Spec-11 placebo; B3 real-time-PI promotion; then the C5 reframe of title/abstract/§1.
- **Tier 3:** companion dependency (C4) — inline or regenerate the companion before citing it.
