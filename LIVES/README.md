# LIVES — multi-equation extension

Williams (2010) estimates the Australian LIVES system as **four
simultaneous equations** by FIML, with a common credit-conditions
spline and cross-equation parameter restrictions. The single-equation
work in [`Australia/`](../Australia/) replicates the
consumption block only.

The placebo and back-extension findings landed in May 2026 produced
two falsification results that point to single-equation OLS as a
binding limit:

1. **Williams' canonical 4-knot CCI** sits at the 19th/10th percentile
   of random-knot draws on the back-extended 1976Q3+ sample (worse than
   the 49th/22nd on the original 1988+ sample). Maximal-GETS rescues
   identification only weakly (64th/36th percentile).
2. **Wealth coefficients on the extended sample** do *not* close the
   gap with Williams' published Table 1 — `ha_y, eq_y, super_y`
   coefficients get smaller, not larger; `nla_y` collapses to ~zero.

Both results are consistent with a multi-equation framework being
required for the structural identification Williams claimed. This
folder builds that framework, in phases.

See [`Australia/docs/back_extension_findings.md`](../Australia/docs/back_extension_findings.md)
for the empirical motivation.

---

## Phased plan

| Phase | Scope | Effort | Status |
|-------|-------|--------|--------|
| **1** | Two-equation SUR: consumption + house prices | ~2-3 weeks | starting |
| **2** | Add mortgage-stock equation (3-equation system) | ~1-2 weeks | future |
| **3** | Add HEW (home equity withdrawal) equation; full LIVES FIML | ~1-2 months | future |

**Why phased.** Each step adds one equation, lets us check that the
existing equation's coefficients don't drift wildly, and keeps the
econometrics tractable. Phase 1 alone resolves the most-pointed
endogeneity critique (housing wealth being endogenous to the
consumption decision) and produces a paper-quality
common-factor CCI identification.

---

## Phase 1 — two-equation SUR (consumption + house prices)

**System.**

```
Δlog c_t  = θ_C · LR_C(wealth, ydp, hp/y, real_rate, CCI, ecm_C) + Σ β_C · SR_C + ε_C,t
Δlog hp_t = θ_H · LR_H(income, real_rate, mortgage_credit, demog, CCI, ecm_H) + Σ β_H · SR_H + ε_H,t
```

**Estimator.** Zellner SUR via `systemfit::systemfit(..., method="SUR")`.
Stack the two equations as a system, allow `cov(ε_C, ε_H) ≠ 0`, and
estimate jointly. SUR delivers efficiency gains over equation-by-equation
OLS when residuals are correlated *and* the regressor sets differ.

**Identification.** The same `cci_williams` spline appears in both
equations as a long-run regressor. Cross-equation restrictions on its
loading are *not* imposed in phase 1 (defer to phase 3 FIML); phase 1
is purely an efficiency-gain exercise. The residual correlation
between consumption and house-price equations is the substantive
quantity to report.

**What this delivers.**
- A single-source coefficient table with both equations' long-run
  parameters, jointly estimated.
- The cross-equation residual correlation `cov(ε_C, ε_H)` — a structural
  test of whether the two equations share an unobserved shock (which
  cci_williams is meant to capture).
- Standard errors that account for the cross-equation correlation
  (matters for any joint hypothesis test).
- A house-price equation that can be referenced in WP §10
  counterfactuals (eg. "no-APRA" affects house prices, which then feeds
  back into consumption via housing wealth).

---

## Phase 2 — add mortgage-stock equation

Williams (2010) Table 3 is a mortgage-stock dynamics equation.
Variables include real mortgage stock, real disposable income, real
house prices (from phase 1), CCI, mortgage interest rate, demographics.
Adds one more equation to the SUR; `mortgage_stock_real` becomes
endogenous to the system.

**Effort.** 1-2 weeks once phase 1 is settled. The additional
estimation machinery is incremental.

**What this delivers.** Endogeneity of housing credit is now in the
system. The CCI loading can be tested for cross-equation consistency
(it should have the same sign in mortgage-stock as in consumption).

---

## Phase 3 — full LIVES (4-equation FIML)

Add the HEW (home equity withdrawal) equation (Williams Table 4),
estimate by FIML rather than SUR, and impose cross-equation parameter
restrictions on the CCI spline (the *same* CCI loading appears in all
four equations with sign restrictions). This is the version Williams
actually estimated and Muellbauer-Williams (2012) extends.

**Effort.** 1-2 months. FIML estimation is materially harder than SUR
and may require custom likelihood code; cross-equation restrictions
add several rounds of model-selection iteration.

**Decision point** (NS-101): is phase 3 part of the headline WP, or a
companion paper? Recommendation per current `next_steps.md`: ship the
single-equation paper now, the 2-equation extension as the immediate
companion, and full LIVES as a follow-up.

---

## Folder layout

```
LIVES/
├── README.md                     this file
├── docs/
│   └── methodology.md            equations, identification, estimator details
├── R/
│   ├── lives_data_prep.R         loads extended master_data, builds shared regressors
│   ├── house_price_equation.R    standalone Williams-style ECM for log(real hp)
│   ├── lives_sur_2eq.R           phase-1 joint SUR (consumption + house prices)
│   ├── lives_sur_3eq.R           phase-2 (planned, future)
│   └── lives_fiml_4eq.R          phase-3 (planned, future)
└── outputs/
    └── (CSVs, plots, comparison tables)
```

## Reproducibility

LIVES depends on the master dataset built by
[`Australia/R/australia_data_download.R`](../Australia/R/australia_data_download.R).
Run the data download first; LIVES scripts then load the saved RDS.

```
# From project root:
Rscript Australia/R/australia_data_download.R
Rscript LIVES/R/lives_data_prep.R          # prepares LIVES-specific frame
Rscript LIVES/R/house_price_equation.R     # standalone HP ECM
Rscript LIVES/R/lives_sur_2eq.R            # phase 1 joint estimation
```

## What's NOT in scope here

- Re-implementing the consumption equation from scratch — phase 1 reuses
  the existing `fit_ecm_spec()` + Spec 6 template from
  `Australia/R/australia_estimation.R`.
- Williams' specific functional forms beyond the broad ECM structure —
  we follow Williams' Tables 2-4 in spirit (real-prices regressed on
  real-income/credit/rates with ECM), with deviations documented.
- Sample back-extension is treated as orthogonal: phase 1 estimates on
  the longest sample where all equations have data (probably 1988Q3+
  initially, then can extend to 1976Q3+ once mortgage-stock proxy is built).
