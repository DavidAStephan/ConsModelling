# Multi-equation system estimation — plan

The single-equation working paper is now complete: it reproduces
Williams' (2010, 2012) structural γ profile to a useful
approximation on the preferred Spec 6, and explains the residual
divergence on simpler specifications as a consequence of the
single-equation framing rather than of sample length. The natural
next step is the multi-equation companion paper that closes the
remaining identification questions by estimating the LIVES system
itself.

This document sets out the plan to get there.

---

## 1. Where we are

**Already built in [`LIVES/`](..):**

- **Phase 1 — two-equation SUR.** Consumption + house prices on the
  back-extended 1976Q3+ sample (n = 189). Implementation:
  [`lives_sur_2eq.R`](../R/lives_sur_2eq.R). Residual correlation
  ρ̂(ε_C, ε_H) ≈ 0.0007 — joint estimation gives no efficiency gain.
  ([phase1_findings.md](phase1_findings.md).)
- **Phase 3 — three-equation system with joint sign-survival CCI.**
  Consumption + house prices + mortgage stock. Implementations:
  [`joint_cci_identification.R`](../R/joint_cci_identification.R),
  [`lives_sur_3eq.R`](../R/lives_sur_3eq.R). Two of six
  single-equation knot survivors pass joint sign tests across all
  three equations (1986 financial deregulation; 2017 APRA round II).
  The joint-identified CCI fixes the HP equation's sign violation
  but not the mortgage equation's.
  ([phase3_findings.md](phase3_findings.md).)
- **Cross-check follow-ups.** Items 1 (`ha_x_cci` interaction) and 2
  (sectional sign priors) implemented and tested.
  ([items_1_and_2_findings.md](items_1_and_2_findings.md).)
- **Methodology documentation.** [methodology.md](methodology.md);
  [cross_check_against_papers.md](cross_check_against_papers.md).

**What we are missing relative to Williams' four-equation FIML system:**

1. **The HEW equation** (Williams Aust eq 13). The four-equation
   LIVES system needs a home-equity-withdrawal block; we have only
   three equations.
2. **Cross-equation parameter restrictions** — specifically (i) the
   ζ_h = 1 normalisation in the HP equation with ζ_c, ζ_m, ζ_w
   estimated as relative scalings, and (ii) the shared ϖ in the
   wealth × (1 − ϖ·CCI) interaction across equations.
3. **Iterated CCI estimation.** Williams iterates fit → identify
   surviving knots → rebuild CCI from coefficients → refit until
   convergence. Our pipeline iterates once.
4. **FIML estimation itself.** Our phase 3 imposes sign restrictions
   via joint knot survival, not parameter equality. True FIML
   imposes the parameter equality the LIVES theory requires.

The first three are scoped, tractable extensions of what's already
built. The fourth is a meaningful code commitment but unblocked by
the previous three.

---

## 2. What "multi-equation system estimation" means here

The target is the Williams (2010) four-equation LIVES system,
estimated jointly with cross-equation parameter restrictions:

```
Δlog c_t   = θ_C  [ … + γ_HA·HA/y·(1 − ϖ·CCI) + ψ_t·log(y^p/y) + ζ_c·CCI + … ]  + ε_C,t
Δlog hp_t  = θ_H  [ … + α_R·r·(1 + κ·CCI) + ζ_h·CCI + … ]                        + ε_H,t
Δlog m_t   = θ_M  [ … + η·CCI + … ]                                              + ε_M,t
hew_t      = θ_W  [ … + (HLI factor)·CCI + … ] · 1/(HA/y)                        + ε_W,t
```

with **ζ_h = 1** as identification, ζ_c, ζ_m, ζ_w as relative
scalings, and **ϖ** restricted to the same value across the
consumption and HP equations.

The system identifies CCI as a **common factor** across the four
equations under sign constraints, which is the structural content
Williams' framework delivers and which our single-equation OLS
cannot match.

---

## 3. Phased plan

The plan is split into three phases. Phase A is unblocked work that
extends the existing three-equation SUR to a four-equation system
with structural identification. Phase B is the FIML build — the
months-of-work item. Phase C is the companion-paper writeup.

### Phase A — Complete the four-equation system under SUR (2–4 weeks)

The goal is to land a clean four-equation system that imposes
Williams' structural identification (ζ_h = 1, shared ϖ, iterated
CCI) under SUR rather than FIML, and to land the substantive
results that follow.

| # | Item | Effort | Notes |
|---|------|--------|-------|
| A1 | **HEW equation construction**             | 2–3 days | The fourth equation. RBA's HEW series is unpublished; either request it or construct the proxy `HEW = Δ(fin_loans_proxy) − dwelling_investment / income`. Williams pre-multiplies by `z = 1/(HA/y)` for heteroskedasticity — replicate. Sign priors per Williams (Aust eq 13). |
| A2 | **Cross-equation ζ normalisation**         | 1 day   | Pin ζ_h = 1 in the HP equation by construction; estimate ζ_c, ζ_m, ζ_w as relative scalings. Implementation: rescale `cci_williams_joint` so its loading in the HP equation is unity, then refit the other equations. This is the fix to the M-equation sign violation flagged in [phase3_findings.md §2](phase3_findings.md). |
| A3 | **Iterated CCI estimation**                | half day to 1 day | Wrap the current `joint_cci_identification.R` pipeline in a fixed-point loop: fit → identify surviving knots → rebuild CCI from coefficients → refit until the surviving-knot set is stable. Convergence criterion: same surviving knots across two consecutive iterations. |
| A4 | **De-meaned CCI interactions in Spec 8**   | half day | Williams Aust §5.1 demeans interacted variables before taking products; we don't. May flip the wrong-signed `ha_x_cci` (currently −0.0011, p = 0.52) to its theoretically correct positive value. Identified as a likely cause of the wrong sign in [items_1_and_2_findings.md §1.5](items_1_and_2_findings.md). |
| A5 | **HP-equation refinements** (optional)     | 1–2 days | Frenzy effect (cubic of lagged real-HP growth), DSRISK (4-quarter MA of negative-only Δ₄log p^h), FHOS dummy, inverse housing demand. From Williams Aust eq 11. Captures non-linear HP dynamics; defer unless reviewers ask. |
| A6 | **Four-equation SUR**                       | 1 day   | Combine A1–A4. Stack the four equations and estimate by SUR. Implementation extends [`lives_sur_3eq.R`](../R/lives_sur_3eq.R). |
| A7 | **Joint-survival re-run with HEW**         | 1 day   | Refit the maximal-GETS knot set against all four equations. Likely reduces the joint survivor set below the current two knots; may force a re-think of the candidate basis. |

**Phase A deliverables:**

- A working four-equation SUR with ζ_h = 1 normalisation, iterated
  CCI, and the four-equation joint-survival CCI.
- Updated coefficient table; structural γ profile under the
  four-equation system; cross-equation residual correlations.
- A `phase_a_findings.md` document summarising the empirical results
  and any remaining diagnostic gaps.

**Decision point at the end of Phase A:** is the four-equation
SUR enough for the companion paper, or do we need to commit to
Phase B (FIML)? This depends on whether ζ normalisation and
iterated CCI close the residual gap with Williams' Table 1 or
whether the gap is genuinely structural to the lack of parameter
equality.

### Phase B — Full FIML build (4–8 weeks)

If Phase A's structural identification through ζ normalisation and
iterated CCI is not sufficient to recover the Williams Table 1
profile, Phase B implements the FIML approach Williams actually
uses.

| # | Item | Effort | Notes |
|---|------|--------|-------|
| B1 | **Custom FIML likelihood**                 | 2–4 weeks | Maximum likelihood over the four-equation system with `ϖ` (the wealth × (1 − ϖ·CCI) parameter) shared across the consumption and HP equations and ζ_c, ζ_m, ζ_w estimated as relative scalings. Likely needs `maxLik` or hand-coded gradient. Significant code commitment. |
| B2 | **Test Williams' Table 1 calibrations as restrictions** | 1 week | Test whether Williams' γ_HA = 0.0488, γ_NLA = 0.066 (Italian-convention NLA), ψ_0 = 0.20, ϖ = 1.2 calibrations are *empirically* accepted on Australian data under FIML. If accepted, the paper has a clean replication; if not, an Australia-specific story. |
| B3 | **Joint Kalman state-space CCI** (optional) | 3–5 days | Tobin Lives (Duca and Muellbauer 2013) extracts the latent factor as a Kalman-filtered stochastic trend in a joint state-space model. Our Kalman CCI uses housing-loan-flow as the single-equation anchor; the multi-equation version is the right benchmark. |

**Phase B deliverables:**

- A FIML estimate of the four-equation LIVES system on Australian
  data.
- A formal test of Williams' Table 1 calibrations.
- The "true" wealth γ profile, recoverable in principle from the
  structural restrictions.

### Phase C — Companion paper writeup (1–2 weeks)

| # | Item | Effort | Notes |
|---|------|--------|-------|
| C1 | **Companion paper draft**                    | 1 week  | Stand-alone short paper documenting the Phase 1 SUR, Phase 3 sign-survival, Phase A four-equation system, and Phase B FIML (if completed). Mirror the structure of the headline single-equation paper. |
| C2 | **Cross-references with the headline paper** | half day | The two papers should reference each other cleanly. The headline paper already references "a separate companion directory"; the companion paper should reference the headline by name once it has one. |
| C3 | **Reproducibility kit and venue submission** | half day | Quarto rendering (see Tier B item B4 in [`../../Australia/docs/next_steps_plan_2026.md`](../../Australia/docs/next_steps_plan_2026.md) is shared infrastructure), bib file, repo URL. |

---

## 4. Recommended sequence

1. **Items A4 (de-meaned CCI interactions) and A3 (iterated CCI)
   first**, in either order. Both are short and unblock judgment
   on whether the existing three-equation system already produces
   the structural identification we want, before committing to A1
   (HEW). Specifically: if A4 flips `ha_x_cci` to its theoretically
   correct positive value, the wealth-coefficient story changes
   materially.

2. **Item A2 (ζ normalisation) and A1 (HEW)** next, in that order.
   A2 fixes the M-equation sign violation that was the headline
   gap in [phase3_findings.md](phase3_findings.md); A1 completes
   the four-equation system. A7 (joint survival across all four
   equations) follows mechanically.

3. **Decide on Phase B vs companion paper.** Two reasonable paths:
   - *Ship a Phase A companion paper now*, presenting the four-
     equation SUR with structural identification (ζ normalisation,
     iterated CCI, joint-survival) as the contribution. Defer FIML
     to a follow-up. Lower effort, lower risk, faster turnaround.
   - *Build Phase B before writing the paper.* Higher effort,
     higher payoff if Williams' calibrations test out cleanly.

4. **Phase C** writeup runs in parallel with the Phase A → B
   transition once the empirical content is stable.

---

## 5. Decision points the author should weigh in on

- **HEW data sourcing.** Request the RBA's unpublished HEW series
  (cleanest but blocking on RBA response), or construct the proxy
  `Δ(fin_loans_proxy) − dwelling_investment / income` from existing
  series (fastest but needs cross-check)? The proxy is the
  pragmatic default; the RBA request runs in parallel.

- **Phase B commitment.** Months of custom-likelihood econometrics
  is worth the effort if (i) the residual wealth-coefficient gap
  from Phase A is large enough to be a paper finding in itself, or
  (ii) the audience for the companion paper is a journal that
  expects FIML rather than SUR. The Phase A SUR may be a publishable
  contribution on its own if the structural identification
  (ζ normalisation, iterated CCI, joint survival) demonstrably
  improves on the single-equation results.

- **Venue.** ✅ **Decided: RBA Research Discussion Paper** (May 2026)
  — matching the headline paper's target venue. The companion paper
  is the natural follow-on under the same imprint; submitting both
  to the same RDP series simplifies cross-referencing and keeps the
  multi-equation diagnosis in the same audience as the headline
  single-equation finding.

- **Paper scope.** Two reasonable framings: (i) "Williams' LIVES
  system applied to contemporary Australian data" — a replication
  + extension paper; (ii) "Single-equation OLS vs system FIML on
  the same data" — a methodology paper that uses the LIVES system
  as the vehicle for a methodological comparison. The first is the
  natural sequel to the headline paper; the second has wider
  potential readership.

---

## 6. Risks and dependencies

- **HEW data availability.** If the RBA series is not obtainable
  and the proxy construction is unsatisfactory, the four-equation
  system regresses to three equations and the paper's contribution
  narrows.
- **FIML convergence.** Custom-likelihood code on a system this
  size is non-trivial to get to converge cleanly. The first
  passes may require starting-value engineering and constraint
  re-formulation.
- **The Williams calibrations may not test out.** Phase B's
  hypothesis test is intentionally exposed to falsification. If
  Williams' Table 1 calibrations are rejected on Australian data,
  the paper still has a finding — just a different one.
- **Joint-survival knot set may shrink to zero or one.** Adding the
  HEW equation to the joint sign-survival pipeline (item A7) may
  eliminate both currently surviving knots (1986, 2017). In that
  case the iterated CCI of A3 has to work harder, and the candidate
  set may need expansion.

---

## 7. Companion paper structure (sketch)

A working draft outline for the companion paper, to be developed in
parallel with the empirical work.

1. **Introduction.** Motivation: the headline single-equation
   paper reproduces the Williams γ profile to a useful approximation
   but cannot impose the cross-equation parameter restrictions that
   Williams' system FIML delivers. This paper builds the system.

2. **The LIVES framework.** Brief restatement of the four-equation
   model, the role of CCI as a common factor, and the cross-equation
   parameter restrictions (ζ normalisation, shared ϖ).

3. **Data.** Reused from the headline paper (back-extended to
   1976Q3). New construction: the HEW series (sourced or proxied).

4. **Sequential build.** Phase 1 SUR (consumption + HP); Phase 3
   sign-survival across three equations; Phase A four-equation
   system with ζ normalisation, iterated CCI, de-meaned
   interactions; Phase B FIML (if completed).

5. **Results.** Wealth γ profile under each phase; the
   evolution of the CCI loadings across equations; the
   joint-survival knot set; the test of Williams' Table 1
   calibrations.

6. **Discussion.** What the multi-equation system buys over the
   single-equation framework, and what (if anything) is still
   unresolved.

7. **Conclusion.**

---

## 8. Status of items in the historical backlog

This plan resolves several items that were Tier B and Tier C in
[`../../Australia/docs/next_steps_plan_2026.md`](../../Australia/docs/next_steps_plan_2026.md):

- **B1 (HEW equation)** → Phase A item A1.
- **B2 (cross-equation ζ normalisation)** → Phase A item A2.
- **B3 (de-meaned CCI interactions)** → Phase A item A4.
- **B5 (companion paper writeup)** → Phase C.
- **C1 (custom FIML)** → Phase B item B1.
- **C2 (iterated CCI)** → Phase A item A3.
- **C3 (Williams' Table 1 calibrations as testable restrictions)**
  → Phase B item B2.

Items D1–D5 in the same document (Spec 6 back-extension via
longer-history SR CCI; pre-1978 labour force; HP equation
specifics; Quarto book conversion; TRYM provenance note) remain in
the deferred/exploratory tier.

---

**Last updated:** 2026-05-20.
