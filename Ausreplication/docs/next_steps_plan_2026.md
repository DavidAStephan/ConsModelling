# Next-steps plan (2026 forward-looking)

This document is the forward-looking tier-based plan as of mid-May
2026. It is companion to (not a replacement for) the historical
backlog in [`next_steps.md`](next_steps.md), which retains the stable
NS-001 to NS-114 IDs that other documents reference.

The headline empirical work (back-extension to 1976Q3, three placebo
batteries, LIVES phases 1 and 3, items 1 and 2 cross-check, paper
revision) has now landed. What follows is what's needed to ship the
headline single-equation paper, build the multi-equation companion
paper, and (if appetite/budget exist) tackle the months-long FIML
extension.

---

## Tier A — Ship the headline single-equation paper (1–2 weeks)

The empirical content is complete. What's left is editorial.

| #  | Item | Effort | Notes |
|----|------|--------|-------|
| A1 | **Counterfactuals (NS-012)** | 1–2 days | The §10 chart that's currently missing. Three exercises: no-APRA-2014/2017, no-JobKeeper, CCI=peak-vs-zero. Decomposition machinery already in `plot_longrun_decomposition()`. Decide whether to run on Spec 6 (1988+) or Spec 1 back-extended (1976+) or both. |
| A2 | **Auto-fill table placeholders (NS-001)** | half day | The `[TABLE-FROM-DATA: ...]` markers in §7–§9. Build a small R helper that reads each CSV and emits a markdown table. |
| A3 | **Verify the 9 [VERIFY] citation tags (NS-002)** | 1–2 hours each | All in §2 lit review. Confirm year, journal, page numbers via Google Scholar / RBA archive. |
| A4 | **Polish abstract and tighten §1 / §11** | half day | Current versions are dense — fine for a working paper, but want one more editorial pass before submission. |
| A5 | **Choose target venue (NS-102)** | decision | RBA RDP, Treasury TPRP, *Economic Record*, or SSRN preprint → journal. Affects format and length. RBA RDP for in-house route, *Economic Record* for journal route, *Review of Income and Wealth* for international LIVES audience. |

---

## Tier B — Build the multi-equation companion paper (2–4 weeks)

This is where the scientific contribution moves beyond what
single-equation OLS can deliver. Phase 1 and phase 3 of the
[`LIVES/`](../../LIVES/) folder are already done; the gap is the
HEW equation, the cross-equation parameter restrictions Williams
imposes, and writing the work up as a stand-alone paper.

| #  | Item | Effort | Notes |
|----|------|--------|-------|
| B1 | **Build HEW equation (Williams Aust eq 13)** | 2–3 days | Williams' RBA HEW series is unpublished. Two options: (a) request from RBA; (b) construct proxy as `Δ(fin_loans_proxy) − dwelling_investment / income`. Williams pre-multiplies by `z = 1/(HA/y)` for heteroskedasticity — replicate. |
| B2 | **Cross-equation ζ_i normalisation** | 1 day | Pin ζ_h = 1 in the HP equation per Williams Aust §5.1; estimate ζ_c, ζ_m, ζ_w as relative scalings via custom optimisation or via a re-scaled common-factor `cci_williams`. Fixes the M-equation sign violation in [LIVES/docs/phase3_findings.md §2](../../LIVES/docs/phase3_findings.md). |
| B3 | **De-meaned CCI interactions in Spec 8** | half day | Williams Aust §5.1 explicitly demeans interacted variables; we don't. May flip the wrong-signed `ha_x_cci` (currently −0.0011, p=0.52) to its theoretically correct positive value. Item 1 from cross-check identified this. |
| B4 | **Quarto rendering pipeline (NS-013/14)** | 2–3 days | Convert `wp_draft.md` to `wp_draft.qmd` with proper cross-refs, BibTeX, figure auto-pulls. Companion paper would also use it. |
| B5 | **Write up companion paper** | 1–2 weeks | Stand-alone short paper documenting the LIVES phase 1 + phase 3 findings, the joint cross-equation CCI identification, and what items B1–B3 add. Outline implicit across [`LIVES/docs/phase1_findings.md`](../../LIVES/docs/phase1_findings.md), [`phase3_findings.md`](../../LIVES/docs/phase3_findings.md), [`cross_check_against_papers.md`](../../LIVES/docs/cross_check_against_papers.md), [`items_1_and_2_findings.md`](../../LIVES/docs/items_1_and_2_findings.md). |

---

## Tier C — True FIML with parameter restrictions (months)

This is the only path empirically demonstrated to close the
wealth-coefficient gap with Williams. The cross-check confirmed it's
the path Williams actually took. NS-031 in
[`next_steps.md`](next_steps.md) budgeted "1–2 months"; realistically
more given custom likelihood code.

| #  | Item | Effort | Notes |
|----|------|--------|-------|
| C1 | **Custom FIML likelihood with shared ϖ** | 2–4 weeks | Maximum likelihood over the 4-equation system with `ϖ` (the wealth × (1−ϖ·CCI) parameter) shared across equations. Likely needs `maxLik` or hand-coded gradient. Big code commitment. |
| C2 | **Iterated CCI estimation** | 1 week | Williams iterates: fit → identify surviving knots → rebuild CCI from coefficients → refit → ... until convergence. Our pipeline iterates once. |
| C3 | **Williams' Table 1 calibrations as testable restrictions** | 1 week | Test whether Williams' γ_HA = 0.0488, γ_NLA = 0.066, ψ_0 = 0.20, ϖ = 1.2 calibrations are *empirically* accepted on the Australian data under FIML. If yes → replication; if no → genuinely Australia-specific story. |

---

## Tier D — Optional / exploratory (defer until Tier B done)

| #  | Item | Effort | Notes |
|----|------|--------|-------|
| D1 | **Spec 6 back-extension via longer-history SR CCI** | half day | Replace `d2_logcci_lag2` with `Δ²log credit_total_d02` (1976Q3+ from RBA D02). Lets Spec 6 fit on n=190 instead of n=86. Worthwhile if reviewers ask. |
| D2 | **Pre-1978 labour force from ABS Cat 6204.0** | days | Archived ABS historical labour force compilation, 1966+. Would push the spine to 1966Q3 (~10 more quarters). Marginal value; only worth it if going to Tier C. |
| D3 | **HP equation specifics** | days | Williams' "frenzy" cubic, DSRISK, log-user-cost-of-capital, FHOS dummy. Captures non-linear HP dynamics. Important for the companion paper, not the headline. |
| D4 | **Quarto book conversion** | week | Combine WP + data.md + LIVES findings into a single navigable document. Useful as a reproducibility artefact rather than a paper. |
| D5 | **`house_price_history_long.csv` provenance note** | 1 hour | Short data-appendix note recording the specific TRYM vintage, retrieval URL, and modifications since release. |

---

## Recommended sequence

1. **Items A1–A5 now**, in parallel where possible. ~1–2 weeks. Gets the headline paper out.
2. **Items B3 (de-meaning) and B1 (HEW) next** — concrete enough to do without committing to full FIML. ~half-day to 3 days each.
3. **Item B2 (ζ_i normalisation)** — clean test of one specific Williams identification trick without the FIML build. 1 day.
4. **Item B5 (companion paper writeup)** — pulls Tier B work + LIVES phase 1+3 findings into a stand-alone paper. ~1–2 weeks.
5. **Tier C** — only with appetite/budget for months of custom-likelihood econometrics. The gap-closing question is genuinely interesting but the paper-output ratio drops.

---

## Decision points the author should weigh in on

- **Where does the headline paper go?** RBA RDP route shortens
  publication path but shrinks readership; *Economic Record* is the
  natural Australian journal home; *Review of Income and Wealth*
  attracts the international LIVES audience.
- **Companion paper or Tier C extension of headline?** Recommend
  **separate companion paper** — keeps the headline paper crisp on
  the single-equation findings (which include the empirical
  falsification, a real result), and lets the multi-equation work
  develop without holding up submission.
- **NS-031 (full FIML) priority?** Months of custom code. Worth the
  effort only if the Australian wealth-coefficient gap is *the*
  research question. If the question is "show the LIVES framework on
  contemporary Australian data", the headline paper is enough.

---

## Status of items in [next_steps.md](next_steps.md) (the historical backlog)

The May 2026 work has materially landed several items previously open
in the historical backlog:

- **NS-020** (Sample back-extension to ~1976Q3) ✅ DONE — phase 1
  public-data backbone is wired in (TRYM HPI, RBA D03 M3, RBA D02
  total credit, labour force historic CSV, disaggregated wealth
  proxies). Date spine pushed back from 1980Q1 to 1976Q3. Master is
  now n=194.
- **NS-031 partial** — LIVES folder has 3-equation system (consumption
  + house prices + mortgage stock), joint CCI identification, two
  documented findings papers (`phase1_findings.md`,
  `phase3_findings.md`). Full FIML is Tier C above.
- **NS-001** (auto-fill WP table placeholders) ⏳ still pending →
  Tier A item A2.
- **NS-002** (verify [VERIFY] citation tags) ⏳ still pending → Tier A
  item A3.
- **NS-010** (draft WP intro) ✅ DONE — full prose now in §1.
- **NS-011** (draft WP conclusion) ✅ DONE — full prose now in §11.
- **NS-012** (counterfactuals) ⏳ still pending → Tier A item A1.
- **NS-013/14** (Quarto pipeline) ⏳ still pending → Tier B item B4.
- **NS-021** (BIS Shrapnel pre-1978 HP) ✅ obsolete — TRYM splice
  supersedes.

For other NS- items (the cross-check item, sectional priors,
ha_x_cci, etc.), see [`back_extension_findings.md`](back_extension_findings.md),
[`LIVES/docs/items_1_and_2_findings.md`](../../LIVES/docs/items_1_and_2_findings.md),
and the May 2026 entries in
[`LIVES/docs/cross_check_against_papers.md`](../../LIVES/docs/cross_check_against_papers.md).

---

**Last updated:** 2026-05-09.
