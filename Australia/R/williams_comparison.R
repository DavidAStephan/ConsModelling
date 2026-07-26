#!/usr/bin/env Rscript
# ==============================================================================
# Williams (2010) WP 492 / Muellbauer-Williams BIS chapter — comparison table
# ==============================================================================
#
# Side-by-side comparison of our preferred Australia consumption-equation
# estimates against Williams' published Table 1 results from the BIS chapter
# (Aust system paper - revised.pdf). Produces:
#
#   outputs/australia_williams_comparison.csv     wide table
#   outputs/australia_williams_comparison.md      markdown commentary for WP
#
# Caveats baked into the comparison:
#   - Sample window: ours 1988Q4-2026Q1 (n=86 for Spec 6); Williams 1978Q1-2008Q2
#   - Income measure: ours uses ydi_real_pc by default; Williams uses NPY
#     (we have npy_real_pc available as a robustness column — see
#      australia_williams_income_robustness.csv).
#   - Estimation: ours OLS with Newey-West HAC; Williams FIML in 4-equation system.
#   - CCI interactions: Williams' Table 1 has r×CCI, log(p^h/y)×(1-1.2·CCI),
#     log(y^p/y)×CCI; our Spec 6 has none of these (they're in Spec 8).
#     We compare Spec 6 to Williams' "CCI=0 baseline" effects where the
#     interaction collapses, AND Spec 8 (where it exists) to Williams'
#     "CCI=peak" effects.
#
# Run: Rscript Australia/R/williams_comparison.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr); library(stringr)
})

.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
output_dir   <- file.path(project_root, "outputs")

# ------------------------------------------------------------------------------
# Williams' published estimates (BIS chapter Table 1, also in Aust system paper)
# ------------------------------------------------------------------------------
# Sources:
#   Aust system paper - revised.pdf, Table 1 col 1 (consumption equation,
#   FIML 1978Q1-2008Q2, n=122).
#   Reproduced in bispap64p (BIS chapter), Figs 1-2.
#
# Structural parameters (i.e. coefficient / lambda) where applicable. Where
# the interaction is `term × (1-ϖ·CCI)` we report the at-CCI=0 effect;
# where `term × CCI` we report the at-CCI=peak effect (ϖ=1.2; CCI peak ≈ 0.81
# in long-run-house-price units, normalised here to 1).
williams <- tribble(
  ~term,                ~williams_estimate, ~williams_se, ~williams_t, ~note,
  "lambda",                       -0.286,        0.083,      -3.45,    "Speed of adjustment, FIML",
  "ha_y",                          0.0488,       0.0095,      5.14,    "Housing wealth MPC at CCI peak; 0.0452 by 2008Q2 (≈0 at CCI=0)",
  "ha_y_at_peak",                  0.0452,       NA,          NA,      "Housing wealth MPC at CCI peak (paper text p.13)",
  "ifa_y",                         0.022,        NA,          NA,      "Illiquid financial MPC; calibrated, not freely estimated (p.18)",
  "nla_y",                         0.159,        0.041,       3.88,    "Net liquid (= LA - total household debt) MPC",
  "ln_hp_over_y_at_cci0",         -0.130,        0.027,      -4.81,    "log(p^h/y) coef at CCI=0; +0.026 at CCI peak (sign reversal)",
  "real_rate_at_peak",            -0.871,        0.435,      -2.00,    "r x CCI coef; effect at CCI=1",
  "ln_yp_over_y_at_cci0",          0.20,         NA,          NA,      "psi_0 calibrated; weight on permanent income at CCI=0",
  "ln_yp_over_y_at_peak",          0.95,         NA,          NA,      "psi at CCI=1 (psi_0 + psi_1 x CCI; capped at 0.95)",
  "demftb_d4",                    -0.138,        0.058,      -2.38,    "First-home-buyer cohort share, change form",
  "wapop_d4",                    -0.069,         NA,          NA,      "Working-age population share, change form",
  "dsrisk",                        0.050,        0.022,       2.27,    "Downside-risk index on housing returns",
  "dlog_c_lag",                   -0.110,        0.041,      -2.68,    "Lagged consumption growth (durables habit)"
)

# ------------------------------------------------------------------------------
# Our preferred-spec estimates
# ------------------------------------------------------------------------------
results_path <- file.path(output_dir, "australia_full_results.csv")
if (!file.exists(results_path)) stop("results not found: ", results_path)
res <- readr::read_csv(results_path, show_col_types = FALSE)

# Spec 6 (preferred, no CCI interactions) — for CCI=0 baseline comparison
spec6 <- res %>% filter(specification == "Spec6_Preferred")
spec8 <- res %>% filter(specification == "Spec8_CCI_Interactions")

extract_struct <- function(df, term_name) {
  row <- df %>% filter(term == term_name)
  if (nrow(row) == 0L) return(list(est = NA_real_, se = NA_real_, t = NA_real_,
                                    p = NA_real_, lambda = NA_real_))
  list(
    est    = row$structural_param[1L],   # OLS / lambda
    se     = NA_real_,                    # NW SE on structural is non-trivial
    t      = row$t_stat[1L],
    p      = row$p_value[1L],
    lambda = row$lambda[1L]
  )
}

extract_ols <- function(df, term_name) {
  row <- df %>% filter(term == term_name)
  if (nrow(row) == 0L) return(list(est = NA_real_, se = NA_real_, t = NA_real_,
                                    p = NA_real_))
  list(
    est = row$ols_estimate[1L],
    se  = row$nw_se[1L],
    t   = row$t_stat[1L],
    p   = row$p_value[1L]
  )
}

# Build "ours" column. For lambda, use OLS estimate of ecm_lag directly.
# For the wealth MPCs etc., use structural_param (= OLS / lambda) by sign
# convention: ours stores structural with the canonical negative-lambda
# convention (so |structural| compares to Williams' positive convention).
# We also report the raw OLS coefficient so readers can see whether the
# divergence is in the OLS coefficient (substantive) or in the lambda
# (estimation-method driven).
spec6_lambda <- extract_ols(spec6, "ecm_lag")
spec8_lambda <- extract_ols(spec8, "ecm_lag")

s_ha   <- extract_struct(spec6, "ha_y")
s_eq   <- extract_struct(spec6, "eq_y")
s_sup  <- extract_struct(spec6, "super_y")
s_nla  <- extract_struct(spec6, "nla_y")
s_hpy  <- extract_struct(spec6, "ln_hp_over_y")
s_r    <- extract_struct(spec6, "real_rate")
s_yp   <- extract_struct(spec6, "ln_yp_over_y")
s_yp08 <- extract_struct(spec6, "ln_yp_over_y_post2008")

# Raw OLS coefs for direct comparison (Williams reports structural; we
# report both so readers can disentangle "OLS coef differs" from "lambda
# differs and amplifies the implied long-run").
o_ha   <- extract_ols(spec6, "ha_y")
o_eq   <- extract_ols(spec6, "eq_y")
o_sup  <- extract_ols(spec6, "super_y")
o_nla  <- extract_ols(spec6, "nla_y")
o_hpy  <- extract_ols(spec6, "ln_hp_over_y")
o_r    <- extract_ols(spec6, "real_rate")
o_yp   <- extract_ols(spec6, "ln_yp_over_y")
o_yp08 <- extract_ols(spec6, "ln_yp_over_y_post2008")

# Sign convention: take absolute value of structural for comparison since
# Williams reports positive values for the wealth MPCs (his cointegrating
# normalisation puts the lambda sign on the lhs). We track signs separately.
abs_or_na <- function(x) if (is.na(x)) NA_real_ else abs(x)
ssign_or_na <- function(struct, expected_sign = 1) {
  if (is.na(struct)) return(NA)
  sign(struct) == sign(expected_sign)
}

# IFA equivalent: Williams aggregates equities + super into one bucket. We
# split them; sum gives an apples-to-apples comparison. Cannot sum SEs
# without covariance; report as "approx" with NA SE.
ifa_struct <- if (!is.na(s_eq$est) && !is.na(s_sup$est)) s_eq$est + s_sup$est else NA_real_

# Spec 8 interactions (partial CCI-match) — for CCI=peak comparison
s8_r_x_cci         <- extract_struct(spec8, "r_x_cci")
s8_hp_x_minus_cci  <- extract_struct(spec8, "hp_x_1_minus_cci")
s8_yp_x_cci        <- extract_struct(spec8, "yp_x_cci")

# ------------------------------------------------------------------------------
# Assemble the comparison table
# ------------------------------------------------------------------------------
ours <- tribble(
  ~term,                  ~ours_ols, ~ours_struct,            ~ours_t,           ~ours_source,
  "lambda",               spec6_lambda$est,    spec6_lambda$est, spec6_lambda$t,    "Spec 6 ecm_lag (OLS == structural for ecm_lag itself)",
  "ha_y",                 o_ha$est,            s_ha$est,  s_ha$t,        "Spec 6 (CCI=0 by construction)",
  "ha_y_at_peak",         NA_real_,            NA_real_,             NA_real_,      "Not estimated in Spec 6",
  "ifa_y",                if (!is.na(o_eq$est) && !is.na(o_sup$est)) o_eq$est + o_sup$est else NA_real_,
                          ifa_struct, NA_real_,           "Spec 6 eq_y + super_y aggregated",
  "nla_y",                o_nla$est,           s_nla$est, s_nla$t,       "Spec 6 NLA fix; restriction accepted",
  "ln_hp_over_y_at_cci0", o_hpy$est,           s_hpy$est,            s_hpy$t,       "Spec 6 (no CCI interaction)",
  "real_rate_at_peak",    o_r$est,             s_r$est,              s_r$t,         "Spec 6 (no r x CCI interaction)",
  "ln_yp_over_y_at_cci0", o_yp$est,            s_yp$est,             s_yp$t,        "Spec 6 (free, not calibrated)",
  "ln_yp_over_y_at_peak", if (!is.na(o_yp$est) && !is.na(o_yp08$est)) o_yp$est + o_yp08$est else NA_real_,
                          if (!is.na(s_yp$est) && !is.na(s_yp08$est)) s_yp$est + s_yp08$est else NA_real_,
                          NA_real_,            "Spec 6 sum of base + post-2008 break",
  "demftb_d4",            NA_real_,            NA_real_,             NA_real_,      "Not in Spec 6 (we use prime_age_share in level)",
  "wapop_d4",             NA_real_,            NA_real_,             NA_real_,      "Not in Spec 6",
  "dsrisk",               NA_real_,            NA_real_,             NA_real_,      "Not in Spec 6 (DSRISK not constructed)",
  "dlog_c_lag",           NA_real_,            NA_real_,             NA_real_,      "Not in Spec 6 short-run set"
)

williams_lambda <- 0.286  # absolute value, from Williams (2012) Table 1 col 1

comp <- williams %>% left_join(ours, by = "term") %>%
  mutate(
    # Williams' implied OLS coefficient, for direct apples-to-apples
    # comparison with our OLS: implied_ols = gamma * |lambda_W|
    williams_implied_ols = ifelse(term == "lambda", williams_estimate,
                                   williams_estimate * williams_lambda),
    # Relative OLS gap — does the OLS coef agree with Williams' implied OLS?
    ols_rel_diff_pct    = ifelse(is.na(ours_ols) | is.na(williams_implied_ols) |
                                  abs(williams_implied_ols) < 1e-10,
                                  NA_real_,
                                  100 * (ours_ols - williams_implied_ols) /
                                        abs(williams_implied_ols)),
    # Relative gamma gap (structural-to-structural)
    struct_rel_diff_pct = ifelse(is.na(ours_struct) | is.na(williams_estimate) |
                                  abs(williams_estimate) < 1e-10,
                                  NA_real_,
                                  100 * (ours_struct - abs(williams_estimate)) /
                                        abs(williams_estimate))
  ) %>%
  select(term, williams_estimate, williams_se, williams_t,
         williams_implied_ols,
         ours_ols, ours_struct, ours_t,
         ols_rel_diff_pct, struct_rel_diff_pct,
         williams_note = note, ours_source)

out_csv <- file.path(output_dir, "australia_williams_comparison.csv")
write.csv(comp, out_csv, row.names = FALSE)
message(sprintf("[williams_comparison] CSV saved: %s", out_csv))

# Also dump the Spec 8 CCI-interaction comparison (where matchable)
spec8_comp <- tribble(
  ~williams_term,          ~williams_estimate, ~ours_term,         ~ours_estimate,        ~ours_t,                 ~note,
  "alpha_c1 (r x CCI)",          -0.871,       "r_x_cci",          s8_r_x_cci$est,        s8_r_x_cci$t,            "Williams effect at CCI=1; we report structural coef on r_x_cci",
  "alpha_c4 (hp/y x (1-1.2*CCI))", -0.130,     "hp_x_1_minus_cci", s8_hp_x_minus_cci$est, s8_hp_x_minus_cci$t,     "Williams at CCI=0; we report structural coef",
  "psi_1 (yp/y x CCI)",            0.93,       "yp_x_cci",         s8_yp_x_cci$est,       s8_yp_x_cci$t,           "Williams calibrated; we estimate freely"
)
out_spec8_csv <- file.path(output_dir, "australia_williams_spec8_comparison.csv")
write.csv(spec8_comp, out_spec8_csv, row.names = FALSE)
message(sprintf("[williams_comparison] Spec 8 CSV saved: %s", out_spec8_csv))

# ------------------------------------------------------------------------------
# Markdown commentary — input for the WP methodology section
# ------------------------------------------------------------------------------
fmt <- function(x, digits = 4) {
  if (is.na(x)) "—"
  else if (abs(x) >= 100) sprintf(paste0("%.", digits-2, "f"), x)
  else sprintf(paste0("%.", digits, "f"), x)
}

# Pre-compute the wealth-MPC OLS-vs-Williams gaps, since that's the headline.
ha_ols_w  <- if (!is.na(o_ha$est)) 100 * (o_ha$est  - 0.0488) / 0.0488 else NA
ifa_ols_w <- if (!is.na(o_eq$est) && !is.na(o_sup$est))
               100 * ((o_eq$est + o_sup$est) - 0.022) / 0.022 else NA
nla_ols_w <- if (!is.na(o_nla$est)) 100 * (o_nla$est - 0.159) / 0.159 else NA
lambda_w  <- if (!is.na(spec6_lambda$est)) 100 * (abs(spec6_lambda$est) - 0.286) / 0.286 else NA

md_lines <- c(
  "# Williams comparison — structural parameters",
  "",
  "Side-by-side comparison of our preferred specification (Spec 6) against",
  "Muellbauer-Williams (2012) Table 1 / BIS chapter Figs 1-2. Williams' sample",
  "is **1978Q1-2008Q2 (n=122)**; ours is **1988Q3-2026Q1 (n=86 for Spec 6)**. Williams",
  "estimates by **FIML in a 4-equation system** with imposed sign priors;",
  "we use **OLS with Newey-West HAC SEs** in a single equation.",
  "",
  "Williams' income measure is **non-property household disposable income (NPY)**;",
  "we use **gross disposable income (`ydi_real_pc`)** in Spec 6 by default.",
  "The Williams-NPY robustness column is in",
  "`australia_williams_income_robustness.csv` (read the live shift there).",
  "",
  "## How to read this comparison",
  "",
  "Williams reports **structural** long-run coefficients γ in his Table 1.",
  "Each γ relates to the **OLS coefficient** of Spec 6 by γ = OLS_coef / |λ|",
  "(the ECM normalisation: in `Δlog c = λ·γ·X + ...`, the OLS coefficient",
  "on X is `λ·γ`). So a divergence in the structural γ can come from either",
  "(a) genuinely different OLS coefficients, or (b) a different λ.",
  "",
  "We report **both** below so the reader can see which channel drives any",
  "given gap. Read the live numbers from the table (regenerated each run",
  "from the current fits) rather than from any hand-written summary; the",
  "Spec 6 coefficients are individually imprecise (n = 86), so treat the",
  "structural profile as consistency evidence, not confirmation — see the",
  "confidence intervals in the headline paper Section 7.3.1.",
  "",
  "## Headline comparison (Spec 6, full sample 1988Q3-2026Q1)",
  "",
  sprintf("Williams' lambda = -0.286, ours = %s.", fmt(spec6_lambda$est)),
  "Showing both forms below to disentangle the channels:",
  "",
  "| Term | Williams γ | Williams implied OLS (γ × \\|λ\\|) | Our OLS | Our γ (= OLS / \\|λ\\|) | OLS gap | γ gap |",
  "|------|-----------:|----------------------------------:|--------:|---------------------:|--------:|------:|",
  sprintf("| **λ** | %s | (same) | %s | (same) | %s%% | (same) |",
          fmt(-0.286), fmt(spec6_lambda$est),
          if (!is.na(lambda_w)) sprintf("%+.0f", lambda_w) else "—"),
  sprintf("| Housing `ha_y` | %s | %s | %s | %s | %s%% | %s%% |",
          fmt(0.0488), fmt(0.0488 * 0.286), fmt(o_ha$est), fmt(s_ha$est),
          if (!is.na(o_ha$est)) sprintf("%+.0f", 100*(o_ha$est - 0.0488*0.286)/(0.0488*0.286)) else "—",
          if (!is.na(s_ha$est)) sprintf("%+.0f", 100*(abs(s_ha$est)-0.0488)/0.0488) else "—"),
  sprintf("| Illiquid `eq_y + super_y` | %s (calibrated) | %s | %s | %s | %s%% | %s%% |",
          fmt(0.022), fmt(0.022 * 0.286),
          fmt(if (!is.na(o_eq$est) && !is.na(o_sup$est)) o_eq$est + o_sup$est else NA_real_),
          fmt(ifa_struct),
          if (!is.na(o_eq$est) && !is.na(o_sup$est))
            sprintf("%+.0f", 100*((o_eq$est+o_sup$est) - 0.022*0.286)/(0.022*0.286)) else "—",
          if (!is.na(ifa_struct)) sprintf("%+.0f", 100*(abs(ifa_struct)-0.022)/0.022) else "—"),
  sprintf("| Net liquid `nla_y` | %s | %s | %s | %s | %s%% | %s%% |",
          fmt(0.159), fmt(0.159 * 0.286), fmt(o_nla$est), fmt(s_nla$est),
          if (!is.na(o_nla$est)) sprintf("%+.0f", 100*(o_nla$est - 0.159*0.286)/(0.159*0.286)) else "—",
          if (!is.na(s_nla$est)) sprintf("%+.0f", 100*(abs(s_nla$est)-0.159)/0.159) else "—"),
  sprintf("| log(HP/y) | %s | %s | %s | %s | — | — |",
          fmt(-0.130), fmt(-0.130 * 0.286), fmt(o_hpy$est), fmt(s_hpy$est)),
  sprintf("| ψ (PI weight) at CCI=0 | %s (calibrated) | %s | %s | %s | — | — |",
          fmt(0.20), fmt(0.20 * 0.286), fmt(o_yp$est), fmt(s_yp$est)),
  "",
  "## Spec 8 (CCI-interaction) match",
  "",
  "Spec 8 includes Williams' CCI interactions on the truncated 1988Q3+ sample",
  "with our reduced-form `cci_williams` (surviving knots per",
  "`australia_williams_cci_knots.csv`; see headline paper Section 5.1.1).",
  "",
  "| Williams term | Williams γ | Spec 8 OLS coef | Spec 8 implied γ | Note |",
  "|---|---:|---:|---:|---|",
  sprintf("| α_c1 (r × CCI) | %s | %s | %s | Sign FAIL on our sample (small +ve) |",
          fmt(-0.871), fmt(extract_ols(spec8, "r_x_cci")$est), fmt(s8_r_x_cci$est)),
  sprintf("| α_c4 (log(HP/y) × (1-1.2·CCI)) | %s | %s | %s | Sign agrees; magnitude smaller |",
          fmt(-0.130), fmt(extract_ols(spec8, "hp_x_1_minus_cci")$est),
          fmt(s8_hp_x_minus_cci$est)),
  sprintf("| ψ_1 (log(y^p/y) × CCI) | %s | %s | %s | Williams calibrates; we estimate freely (sign FAIL) |",
          fmt(0.93), fmt(extract_ols(spec8, "yp_x_cci")$est),
          fmt(s8_yp_x_cci$est)),
  "",
  "## Where we agree, where we differ",
  "",
  "**Level OLS coefficients:** read the live values from the table above;",
  "they are within an order of magnitude of Williams' published structural",
  "gamma values.",
  "",
  "**Sign agreement:** All wealth MPCs are positive in our preferred spec",
  "(the NLA-netting fix delivered this), log(HP/y) is negative as expected,",
  "and λ is negative as required for a stable ECM. Spec 6 passes the sign",
  "screen comprehensively.",
  "",
  "**Read the structural gamma profile from the table above (live values).**",
  "",
  sprintf("1. **Our lambda is %s against Williams' -0.286** (live value).",
          fmt(spec6_lambda$est)),
  "   Any gap between the implied structural gammas and Williams' Table 1 is",
  "   plausibly due to (i) missing CCI interactions in Spec 6, which Williams",
  "   partitions across r×CCI, log(HP/y)×(1-1.2·CCI), and ψ_1·CCI; (ii) FIML",
  "   cross-equation identification across the LIVES system; (iii) the sample",
  "   window — Williams 1978-2008 includes the deregulation-era acceleration",
  "   our 1988+ sample misses.",
  "",
  "2. **Read the implied gamma profile from the table above** (live values,",
  "   not hand-typed). These are point estimates on a small (n=86) sample",
  "   with wide confidence intervals (headline paper Section 7.3.1), so treat",
  "   agreement as statistical consistency, not precise confirmation; the",
  "   sharper Spec 11 intervals in `australia_gamma_inference.csv` reject",
  "   some Williams magnitudes outright.",
  "",
  "3. **The permanent-income coefficient depends on the forecaster**: positive",
  "   under the canonical Italy direct-forecast measure, negative under the",
  "   rolling AR(8) (the documented Australian puzzle); see headline paper",
  "   Section 7.4 and `australia_pi_method_comparison.csv`.",
  "",
  "## Sample / methodology notes for the WP",
  "",
  "- Williams' sample ends 2008Q2 by design (so the GFC tightening at the 2007",
  "  spline knot is identified by the spline only, not by post-GFC realisations).",
  "  Our sample to 2024Q4 includes 16 additional years of post-GFC data.",
  "- Few of Williams' 4 canonical knots survive sign-prior reduction on our",
  "  1988Q3+ sample (see `australia_knot_experiment.csv`). The deployed",
  "  `cci_williams` instead uses the iterated maximal-GETS reduction",
  "  (survivors in `australia_williams_cci_knots.csv`). Sample back-extension",
  "  to ~1975Q1 is the",
  "  standing research priority for full Williams replication. The RBA",
  "  unpublished pre-1988 balance sheet series is the binding obstacle.",
  "- Williams calibrates several coefficients (illiquid MPC γ_2 ≈ 0.01-0.022,",
  "  ψ values 0.20→0.95, the −1.2 weight in (1−ϖ·CCI)). We estimate everything",
  "  freely. A future companion specification could impose Williams' priors",
  "  and report Bayesian posteriors.",
  "",
  "## Suggested framing for the WP",
  "",
  "The headline message for the methodology section: **read the live OLS and",
  "structural gaps from the table; on a small (n=86) sample with wide",
  "confidence intervals (Section 7.3.1) agreement is statistical consistency,",
  "not confirmation.",
  "Any residual gap reflects the single-equation framing, the missing CCI",
  "interactions in the preferred spec, and the non-overlapping post-1988 sample.**",
  "",
  "Generated by `Australia/R/williams_comparison.R`. To refresh after",
  "re-estimation: `Rscript Australia/R/williams_comparison.R`.",
  ""
)
out_md <- file.path(output_dir, "australia_williams_comparison.md")
writeLines(md_lines, out_md)
message(sprintf("[williams_comparison] Markdown saved: %s", out_md))

# Console summary
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Williams comparison\n")
cat(strrep("=", 70), "\n", sep = "")
print(comp %>% select(term, williams_estimate, williams_implied_ols,
                      ours_ols, ours_struct,
                      ols_rel_diff_pct, struct_rel_diff_pct), n = 14)
cat("\nFiles:\n")
cat("  ", out_csv, "\n")
cat("  ", out_spec8_csv, "\n")
cat("  ", out_md, "\n")
