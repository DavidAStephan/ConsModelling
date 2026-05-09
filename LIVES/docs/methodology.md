# LIVES — methodology

## 1. The two equations of phase 1

### 1.1 Consumption equation

Same Spec 6 template as the single-equation paper, with the canonical
Italy-LP permanent-income forecaster:

```
Δlog c_t = θ_C [ γ_NLA · nla_y + γ_EQ · eq_y + γ_SUPER · super_y
                + γ_HA · ha_y · (1 − ϖ · cci_t)        # housing × CCI interaction
                + γ_HP · log(hp/y)
                + γ_R · real_rate
                + ψ_0 · log(yp/y) · (1 − ψ_1 · cci_t)  # PI × CCI interaction
                + ψ_post · log(yp/y) · step2008
                + λ · ecm_lag_C ]
              + Σ β_C · SR_terms + Σ d_C · dummies + ε_C,t
```

with `ecm_lag_C = log(c_{t-1}) − log(y_t)`. This is the ECM convention
where `λ < 0` means restoring force.

### 1.2 House price equation

Following Williams (2010) Table 2 in spirit, an ECM in real
log house price:

```
Δlog hp_t = φ_H [ α_INC · log(real_yd_pc)
                + α_CRED · log(mortgage_credit / income)
                + α_R · real_rate · (1 + κ · cci_t)    # CCI scales rate sensitivity
                + α_DEMOG · prime_age_share
                + ζ · cci_t                              # direct CCI effect on HP
                + λ_H · ecm_lag_H ]
              + Σ β_H · SR_H_terms + Σ d_H · HP_dummies + ε_H,t
```

with `ecm_lag_H = log(hp_{t-1} / yd_{t-1})` — the long-run house-price-to-income
relationship. λ_H is the speed at which house prices revert to a long-run
equilibrium ratio with income.

**Sign priors:**
- α_INC > 0 (richer households can sustain higher house prices)
- α_CRED > 0 (more available credit pushes prices up)
- α_R < 0 (higher rates depress house prices, especially when CCI is
  loose so households are more rate-sensitive — κ > 0)
- α_DEMOG > 0 (more prime-age population pushes prices up via housing
  demand)
- ζ > 0 (looser credit conditions directly support higher prices)
- λ_H < 0 (mean reversion to the income-anchored long-run)

### 1.3 Why these forms

Williams' (2010) Table 2 specification is a Muellbauer-style ECM with
real-income and credit-availability long-run drivers, plus interactions
with the credit-conditions index. The exact Williams coefficients
aren't directly reproducible without his estimation tables, so we
follow the structural form rather than the literal coefficient list.

The deliberate parallel structure (CCI interactions in both
consumption and house prices) is what makes the SUR / FIML
identification work: the *same* `cci_williams` spline enters both
equations, so the data informs its loading via residual covariance.

---

## 2. Estimation

### 2.1 Phase 1 — Zellner SUR

Stack the two equations as a 2-equation system. Estimate via
`systemfit::systemfit(eqns, method = "SUR")`. SUR is GLS with the
covariance matrix estimated from equation-by-equation OLS residuals
(2-step procedure).

**Why SUR not OLS.** Equation-by-equation OLS gives consistent but
inefficient estimates if `cov(ε_C, ε_H) ≠ 0`. The unobserved CCI shock
plausibly affects both equations simultaneously (looser credit boosts
both consumption and house prices), so we expect non-zero residual
correlation.

**Why SUR not FIML.** SUR doesn't impose cross-equation parameter
restrictions; FIML does. For phase 1, we want efficiency gains without
the model-selection complexity of choosing which restrictions to
impose. Phase 3 will add the restrictions.

### 2.2 What the SUR delivers vs single-equation OLS

- **Standard errors** account for cross-equation correlation. Joint
  hypothesis tests (e.g. testing whether the housing-channel coefficient
  in consumption equals minus the housing-supply coefficient in HP
  equation) become well-defined.
- **Coefficient point estimates** can change modestly if the residual
  correlation is non-zero. The SUR estimator weights observations by
  the inverse residual covariance.
- **The residual correlation `ρ̂(ε_C, ε_H)` itself** is a substantive
  finding. If it's near zero, the two equations are essentially
  independent (no efficiency gain from SUR; weak case for the LIVES
  framework). If it's strongly positive (or negative), the
  cross-equation linkage Williams claims is empirically present.

### 2.3 Sample window

The house-price equation's regressors `mortgage_credit / income` is
ABS 5232035 housing loans, which starts 1988Q3+. Until a back-cast
proxy is built, phase 1 fits on 1988Q3+ only.

A future enhancement (phase 1.5 or phase 2): use the
`credit_total_d02 × constant_household_share` back-cast (already in
master as `fin_loans_proxy`) so the HP equation can extend to 1976Q3+.
That makes the phase-1 sample length match the back-extended Spec 1.

---

## 3. CCI variant

Phase 1 uses the maximal-GETS canonical `cci_williams` series already
in the master (built by `fit_consumption_with_williams_cci()` in the
single-equation pipeline, using 15 candidate knots reduced via
sign-prior survival). This keeps the phase-1 results comparable to
the single-equation Spec 8.

---

## 4. Phases 2 and 3 (deferred)

**Phase 2** adds a mortgage-stock equation:

```
Δlog M_t = φ_M [ α_HP · log(hp/y)        # households leverage rising HP
              + α_INC · log(real_yd_pc)
              + α_R · real_rate
              + α_DEMOG · prime_age_share
              + η · cci_t                  # CCI directly increases borrowing
              + λ_M · ecm_lag_M ]
            + ε_M,t
```

with `ecm_lag_M = log(M_{t-1} / yd_{t-1})` — long-run mortgage-stock-to-income.

**Phase 3** adds an HEW equation and switches to FIML. Williams (2010)
imposes that the CCI loading appears with the **same sign and
magnitude** across all four equations (with the housing-equity-extraction
equation having the largest absolute loading). FIML estimates the
common CCI parameters jointly.

These are out of scope for the initial LIVES build but the data
infrastructure (mortgage stock proxy, HEW measure) should be
identifiable from existing master variables once they're spec'd.
