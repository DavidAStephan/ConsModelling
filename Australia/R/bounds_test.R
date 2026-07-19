# ==============================================================================
# bounds_test.R  (plan item B1, review/journal_review_2026-07.md, Referee 1 #1)
#
# Engle-Granger residual cointegration fails for every spec
# (run_cointegration_battery() in australia_estimation.R). This script runs
# the PSS fallback: does the data support ANY long-run relation, freely
# estimated, without needing to know a priori whether the long-run regressors
# are I(0) or I(1)?
#
#  (1) A Pesaran-Shin-Smith (2001, PSS) ARDL bounds test on the three named
#      specs (Spec6_Preferred, Spec11_LIVES_Headline, Spec12_LIVES_Calibrated).
#      Exact regressor sets verified against run_all_specifications() in
#      australia_estimation.R AND against the committed
#      outputs/australia_full_results.csv coefficient table.
#  (2) ADF (drift) + KPSS (level) stationarity tests on the imposed
#      equilibrium ratio ecm_lag = ln(cons_{t-1}) - ln(income_t) ("ln(c/y)"),
#      over the full sample and each spec's own estimation window.
#
# METHOD (bounds test)
# ---------------------
# For each spec, take its exact long-run variable set, replace the imposed
# combination `ecm_lag = lag(lcons,1) - lincome` with its two UNRESTRICTED
# components (lcons and lincome, each as a separate LAGGED level -- this is
# the generalisation the test is FOR: whether the paper's forced unit-income,
# mixed-timing combination is the only supportable relation, or whether a
# freely-estimated one is), and estimate the unrestricted conditional ECM
# (UECM), PSS (2001) eq. (8) (their Case III: unrestricted intercept, no
# trend -- this ECM design has never carried a trend term):
#
#   dlcons_t = c + phi*lcons_{t-1} + rho*lincome_{t-1}
#              + sum_j beta_j * X_{j,t-1}         (spec's other lr_vars, lagged)
#              + psi*dlcons_{t-1}                  (one AR lag, LHS dynamics)
#              + [spec's own sr_vars]               (short-run dynamics, as estimated)
#              + [spec's own dummy_vars]             (structural-break controls)
#              + e_t
#
#   F-test:  H0: phi = 0 AND all beta_j = 0 (joint)  -- PSS Table CI(iii)
#   t-test:  H0: phi = 0                               -- PSS Table CII(iii)
#   k = number of long-run "forcing" variables EXCLUDING the dependent
#       variable's own lag (lincome + the spec's other lr_vars).
#
# Critical value bounds hardcoded below from Pesaran, Shin & Smith (2001,
# "Bounds Testing Approaches to the Analysis of Level Relationships",
# Journal of Applied Econometrics 16(3): 289-326), read directly off the
# original tables (not a secondary source):
#   Table CI(iii)  (F-statistic, Case III: unrestricted intercept, no trend), p.300
#   Table CII(iii) (t-statistic, Case III: unrestricted intercept, no trend), p.303
# both at the asymptotic 5% significance column, k = 1..10 (T=1000,
# 40,000-replication stochastic simulation per the paper's Table CI/CII notes).
#
# Verdict logic follows PSS's own recommended sequential procedure (Section
# 4, p.304: test the F-stat first; if it rejects, confirm with the t-stat):
#   - "not_cointegrated" if EITHER stat falls on the I(0) side of its lower bound
#   - "cointegrated"     if BOTH stats clear the I(1) side of their upper bound
#   - "inconclusive"     otherwise
#
# CAVEATS (recorded, not hidden):
#  - PSS's derivation assumes {x_t} is "long-run forcing" for y_t (their
#    Assumption 3: no feedback from the LEVEL of y_t into the x-block). This
#    script tests the single conditional equation only, as is standard
#    applied practice; Assumption 3 is not separately verified.
#  - PSS's tabulated bounds are asymptotic (simulated at T=1000). Spec6's
#    estimation window is n ~ 85-86; small-sample bounds run a bit wider
#    than the asymptotic ones tabulated here (see Narayan 2005 for small-T
#    critical values, not reproduced in this script) -- read the Spec6 row
#    as indicative, not exact.
#  - The UECM is estimated by OLS with classical (non-HAC) standard errors,
#    matching the F/t statistics' derivation in PSS; this differs from the
#    paper's headline ECM tables, which report Newey-West SEs.
#  - Spec12_LIVES_Calibrated's long-run vector is PARTLY calibrated: fixed,
#    non-estimated weights (gamma_ifa=0.022, psi_0=0.20, psi_1=0.93) enter
#    via an offset subtracted from dlcons inside an iteratively-solved fixed
#    point (fit_lives_calibrated_spec()), not as freely estimated
#    coefficients -- the identical reason run_cointegration_battery()
#    already skips Specs 10/12 for the static Engle-Granger test. A PSS UECM
#    requires ALL long-run coefficients to be free, so the procedure is not
#    well-defined for Spec 12's actual estimating equation. This script
#    marks Spec 12's primary row "infeasible" (with the reason above) and
#    additionally reports a DIAGNOSTIC-ONLY bounds test on Spec 12's two
#    FREELY estimated long-run regressors (ha_x_cci, nla_y) + income,
#    clearly flagged as partial (it omits the calibrated ilfa/permanent-
#    income channels entirely, so it under-states Spec 12's true long run).
#
# Run:  Rscript Australia/R/bounds_test.R      (from the project root)
# Out:  Australia/outputs/australia_bounds_test.csv
#       Australia/outputs/australia_cy_stationarity.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
})
options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS <- "Australia"

# ------------------------------------------------------------------------------
# Load helper functions + the estimation functions (fit_ecm_spec,
# fit_lives_calibrated_spec, add_model_variables, ...) WITHOUT running
# australia_estimation.R's MAIN EXECUTION block -- same pipeline-isolation
# pattern as gamma_inference.R / refit_spec46_extended.R, so this script
# cannot silently re-trigger data downloads or overwrite unrelated outputs.
# ------------------------------------------------------------------------------
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src      <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
he       <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")), envir = he)

# ------------------------------------------------------------------------------
# Build model_data exactly as gamma_inference.R does, then attach the
# deployed (committed) Williams CCI series from the pipeline artefact so
# Spec 11 / Spec 12's interaction terms are available without refitting
# Stage-1 CCI.
# ------------------------------------------------------------------------------
master <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")) %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome) %>%
  arrange(date)
master <- he$add_model_variables(master)
master <- he$compute_income_volatility(master)
master <- he$construct_permanent_income_italy(master)
master <- master %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)

cci_path <- file.path(PROJ_AUS, "outputs", "australia_cci_williams_series.csv")
if (!file.exists(cci_path))
  stop("[bounds_test] ", cci_path, " not found -- run the pipeline (Step 4a-iii) first.")
cci_series <- read.csv(cci_path) %>%
  mutate(date = as.Date(date)) %>%
  select(date, cci_williams)
master <- master %>%
  left_join(cci_series, by = "date") %>%
  mutate(ilfa_y = eq_y + super_y)

mask <- !is.na(master$cci_williams) &
        master$date >= as.Date("1980-01-01") &
        master$date <= as.Date("2024-10-01")
ha_m <- mean(master$ha_y[mask],         na.rm = TRUE)
hp_m <- mean(master$ln_hp_over_y[mask], na.rm = TRUE)
r_m  <- mean(master$real_rate[mask],    na.rm = TRUE)
yp_m <- mean(master$ln_yp_over_y[mask], na.rm = TRUE)
master <- master %>%
  mutate(
    r_x_cci          = (real_rate    - r_m)  * cci_williams,
    hp_x_1_minus_cci = (ln_hp_over_y - hp_m) * (1 - 1.2 * cci_williams),
    yp_x_cci         = (ln_yp_over_y - yp_m) * cci_williams,
    ha_x_cci         = (ha_y         - ha_m) * cci_williams
  )

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

cat(sprintf("[bounds_test] master: %d rows, %d cols; cci_williams non-NA: %d\n",
            nrow(master), ncol(master), sum(!is.na(master$cci_williams))))

# ==============================================================================
# PART 1: PSS (2001) bounds test
# ==============================================================================

# Table CI(iii): F-statistic bounds, Case III (unrestricted intercept, no
# trend), asymptotic 5% critical values, PSS (2001) p.300.
PSS_F_I0_5PCT <- c(`1` = 4.94, `2` = 3.79, `3` = 3.23, `4` = 2.86, `5` = 2.62,
                    `6` = 2.45, `7` = 2.32, `8` = 2.22, `9` = 2.14, `10` = 2.06)
PSS_F_I1_5PCT <- c(`1` = 5.73, `2` = 4.85, `3` = 4.35, `4` = 4.01, `5` = 3.79,
                    `6` = 3.61, `7` = 3.50, `8` = 3.39, `9` = 3.30, `10` = 3.24)

# Table CII(iii): t-statistic bounds, Case III (unrestricted intercept, no
# trend), asymptotic 5% critical values, PSS (2001) p.303. The I(0) bound
# equals the DF critical value (-2.86) for all k (Table CII note); only the
# I(1) bound widens with k.
PSS_T_I0_5PCT <- c(`1` = -2.86, `2` = -2.86, `3` = -2.86, `4` = -2.86, `5` = -2.86,
                    `6` = -2.86, `7` = -2.86, `8` = -2.86, `9` = -2.86, `10` = -2.86)
PSS_T_I1_5PCT <- c(`1` = -3.22, `2` = -3.53, `3` = -3.78, `4` = -3.99, `5` = -4.19,
                    `6` = -4.38, `7` = -4.57, `8` = -4.72, `9` = -4.88, `10` = -5.03)

build_uecm_data <- function(data, other_level_vars, sr_vars, dummy_vars,
                            sample_start = as.Date("1980-01-01"),
                            sample_end   = as.Date("2024-10-01")) {
  d <- data %>% arrange(date) %>%
    mutate(lcons_lag1 = lag(lcons, 1L), lincome_lag1 = lag(lincome, 1L),
           dlcons_lag1 = lag(dlcons, 1L))
  for (v in other_level_vars) d[[paste0(v, "_lag1")]] <- lag(d[[v]], 1L)
  level_terms <- c("lcons_lag1", "lincome_lag1", paste0(other_level_vars, "_lag1"))
  sr_terms    <- c("dlcons_lag1", sr_vars)
  req <- c("dlcons", level_terms, sr_terms)
  est_data <- d %>%
    filter(date >= sample_start, date <= sample_end) %>%
    filter(complete.cases(across(all_of(req))))
  list(data = est_data, level_terms = level_terms, sr_terms = sr_terms)
}

# Estimate the UECM and run the PSS F- and t-bounds tests for one spec.
run_bounds_test <- function(spec_label, data, other_level_vars, sr_vars, dummy_vars,
                            sample_start = as.Date("1980-01-01"),
                            sample_end   = as.Date("2024-10-01"),
                            note_prefix  = "") {
  other_ok <- other_level_vars[vapply(other_level_vars, function(v)
    v %in% names(data) && !all(is.na(data[[v]])), logical(1))]
  if (length(other_ok) < length(other_level_vars))
    message(sprintf("  [%s] dropping unavailable long-run vars: %s", spec_label,
                    paste(setdiff(other_level_vars, other_ok), collapse = ", ")))
  sr_ok <- sr_vars[vapply(sr_vars, function(v)
    v %in% names(data) && !all(is.na(data[[v]])), logical(1))]

  bd <- build_uecm_data(data, other_ok, sr_ok, dummy_vars, sample_start, sample_end)
  est_data <- bd$data
  n <- nrow(est_data)
  k <- 1L + length(other_ok)   # lincome_lag1 + the spec's other long-run vars

  base_row <- list(spec = spec_label, k = k, n_obs = n,
                   sample_from = NA_character_, sample_to = NA_character_,
                   F_stat = NA_real_, I0_bound_5pct = NA_real_, I1_bound_5pct = NA_real_,
                   t_stat = NA_real_, t_bounds = NA_character_,
                   t_I0_bound_5pct = NA_real_, t_I1_bound_5pct = NA_real_,
                   F_verdict = NA_character_, t_verdict = NA_character_,
                   verdict = "infeasible", note = "")

  if (n < 30L) {
    base_row$note <- paste0(note_prefix, sprintf("only %d complete UECM observations (<30)", n))
    return(base_row)
  }

  dummy_ok <- dummy_vars[vapply(dummy_vars, function(v)
    v %in% names(est_data) && var(est_data[[v]], na.rm = TRUE) > 0, logical(1))]

  all_rhs   <- c(bd$level_terms, bd$sr_terms, dummy_ok)
  fmla_full <- reformulate(all_rhs, response = "dlcons")
  fit_full  <- lm(fmla_full, data = est_data)

  aliased <- setdiff(names(which(is.na(coef(fit_full)))), "(Intercept)")
  if (any(bd$level_terms %in% aliased)) {
    base_row$n_obs <- n
    base_row$sample_from <- format(min(est_data$date))
    base_row$sample_to   <- format(max(est_data$date))
    base_row$note <- paste0(note_prefix, "collinear (aliased) long-run level term(s): ",
                            paste(intersect(bd$level_terms, aliased), collapse = ", "))
    return(base_row)
  }
  if (length(aliased) > 0L) {
    message(sprintf("  [%s] dropping collinear SR/dummy terms: %s", spec_label,
                    paste(aliased, collapse = ", ")))
    all_rhs   <- setdiff(all_rhs, aliased)
    fmla_full <- reformulate(all_rhs, response = "dlcons")
    fit_full  <- lm(fmla_full, data = est_data)
  }

  # F-test: joint significance of ALL lagged long-run LEVEL terms.
  rhs_restricted  <- setdiff(all_rhs, bd$level_terms)
  fmla_restricted <- if (length(rhs_restricted) > 0L)
    reformulate(rhs_restricted, response = "dlcons") else dlcons ~ 1
  fit_restricted <- lm(fmla_restricted, data = est_data)
  a <- anova(fit_restricted, fit_full)
  F_stat <- a$F[2]

  cf <- summary(fit_full)$coefficients
  t_stat <- if ("lcons_lag1" %in% rownames(cf)) cf["lcons_lag1", "t value"] else NA_real_

  if (k > 10L) {
    message(sprintf("[%s] k=%d exceeds PSS Table CI/CII (max tabulated k=10); using k=10 bound (conservative).",
                    spec_label, k))
  }
  kc   <- as.character(min(max(k, 1L), 10L))
  I0_F <- unname(PSS_F_I0_5PCT[kc]); I1_F <- unname(PSS_F_I1_5PCT[kc])
  t_I0 <- unname(PSS_T_I0_5PCT[kc]); t_I1 <- unname(PSS_T_I1_5PCT[kc])

  F_verdict <- if (is.na(F_stat)) "infeasible" else
    if (F_stat > I1_F) "cointegrated" else if (F_stat < I0_F) "not_cointegrated" else "inconclusive"
  t_verdict <- if (is.na(t_stat)) "infeasible" else
    if (t_stat < t_I1) "cointegrated" else if (t_stat > t_I0) "not_cointegrated" else "inconclusive"

  verdict <- if (F_verdict == "not_cointegrated" || t_verdict == "not_cointegrated") "not_cointegrated"
             else if (F_verdict == "cointegrated" && t_verdict == "cointegrated") "cointegrated"
             else "inconclusive"

  list(spec = spec_label, k = k, n_obs = n,
       sample_from = format(min(est_data$date)), sample_to = format(max(est_data$date)),
       F_stat = F_stat, I0_bound_5pct = I0_F, I1_bound_5pct = I1_F,
       t_stat = t_stat, t_bounds = sprintf("I0=%.2f / I1=%.2f", t_I0, t_I1),
       t_I0_bound_5pct = t_I0, t_I1_bound_5pct = t_I1,
       F_verdict = F_verdict, t_verdict = t_verdict,
       verdict = verdict,
       note = paste0(note_prefix, sprintf("n=%d, k=%d, F(%d,%d)=%.3f, t(lcons_lag1)=%.3f",
                                          n, k, k + 1L, fit_full$df.residual, F_stat, t_stat)))
}

# ---- Spec 6 / Spec 11: exact regressor sets from run_all_specifications() ----
# (verified against outputs/australia_full_results.csv "term" column).
spec_defs <- list(
  Spec6_Preferred = list(
    other_level_vars = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                         "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008"),
    sr_vars    = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")
  ),
  Spec11_LIVES_Headline = list(
    other_level_vars = c("nla_y", "ilfa_y", "ha_x_cci", "hp_x_1_minus_cci",
                         "r_x_cci", "cci_williams", "ln_yp_over_y", "yp_x_cci"),
    sr_vars    = c("dd4_income", "d2_log_unemp", "abs_income_resid")
  )
)

results_list <- list()
for (nm in names(spec_defs)) {
  sd <- spec_defs[[nm]]
  cat(sprintf("\n--- Bounds test: %s ---\n", nm))
  res <- run_bounds_test(nm, master, sd$other_level_vars, sd$sr_vars, base_dummies)
  print(as.data.frame(res[c("spec", "k", "n_obs", "F_stat", "I0_bound_5pct", "I1_bound_5pct",
                            "t_stat", "t_bounds", "verdict")]))
  results_list[[nm]] <- res
}

# ---- Spec 12: infeasible in strict PSS form (calibrated offset); see header ----
spec12_note <- paste0(
  "Spec 12's long-run vector is PARTLY calibrated: gamma_ifa=0.022 * ilfa_y_internal + ",
  "psi_0=0.20 * ln_yp_over_y + psi_1=0.93 * (ln_yp_over_y*cci) enter via a FIXED, ",
  "non-estimated offset subtracted from dlcons inside an iteratively-solved fixed point ",
  "(fit_lives_calibrated_spec()), not as freely estimated coefficients -- the identical ",
  "reason run_cointegration_battery() already skips Specs 10/12 for the static ",
  "Engle-Granger test ('calibrated-offset long run; static EG regression not applicable'). ",
  "A PSS UECM requires ALL long-run coefficients to be freely estimated, so the bounds ",
  "procedure is not well-defined for Spec 12's actual estimating equation. See the ",
  "'_freeOnly_diagnostic' row below for a PARTIAL test on the two freely estimated ",
  "long-run regressors only (excludes the calibrated ilfa/permanent-income channels, ",
  "so it understates Spec 12's true long run)."
)
spec12_primary <- list(spec = "Spec12_LIVES_Calibrated", k = NA_integer_, n_obs = NA_integer_,
                       sample_from = NA_character_, sample_to = NA_character_,
                       F_stat = NA_real_, I0_bound_5pct = NA_real_, I1_bound_5pct = NA_real_,
                       t_stat = NA_real_, t_bounds = NA_character_,
                       t_I0_bound_5pct = NA_real_, t_I1_bound_5pct = NA_real_,
                       F_verdict = NA_character_, t_verdict = NA_character_,
                       verdict = "infeasible", note = spec12_note)
cat("\n--- Bounds test: Spec12_LIVES_Calibrated ---\n")
cat("INFEASIBLE: ", spec12_note, "\n")

cat("\n--- Bounds test: Spec12_LIVES_Calibrated_freeOnly_diagnostic ---\n")
spec12_diag <- run_bounds_test(
  "Spec12_LIVES_Calibrated_freeOnly_diagnostic", master,
  other_level_vars = c("ha_x_cci", "nla_y"),
  sr_vars    = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
  dummy_vars = base_dummies,
  note_prefix = "DIAGNOSTIC ONLY -- free regressors (ha_x_cci, nla_y) + income; excludes Spec 12's calibrated ilfa_y/ln_yp_over_y/yp_x_cci offset, so this does NOT test Spec 12's actual imposed long run. ")
print(as.data.frame(spec12_diag[c("spec", "k", "n_obs", "F_stat", "I0_bound_5pct", "I1_bound_5pct",
                                  "t_stat", "t_bounds", "verdict")]))

results_list[["Spec12_LIVES_Calibrated"]] <- spec12_primary
results_list[["Spec12_LIVES_Calibrated_freeOnly_diagnostic"]] <- spec12_diag

bounds_tbl <- bind_rows(lapply(results_list, function(r) as_tibble(r)))
out1 <- file.path(PROJ_AUS, "outputs", "australia_bounds_test.csv")
write_csv(bounds_tbl, out1)
cat(sprintf("\nSaved: %s\n", out1))

# ==============================================================================
# PART 2: Stationarity of the imposed equilibrium ratio
#         ecm_lag = ln(cons_{t-1}) - ln(income_t)  ("ln(c/y)")
# ==============================================================================
# ADF (drift, i.e. constant/no trend -- urca::ur.df type="drift") and KPSS
# (level, i.e. constant/no trend -- urca::ur.kpss type="mu") are a matched
# pair: ADF's null is a unit root, KPSS's null is stationarity, both against
# the SAME deterministic (constant-only) alternative, so agreement between
# the two is much stronger evidence than either test alone.

run_adf_kpss <- function(label, x, min_n = 20L) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < min_n) {
    return(tibble(window = label, n_obs = n,
                  adf_stat = NA_real_, adf_5pct_cv = NA_real_, adf_verdict = NA_character_,
                  kpss_stat = NA_real_, kpss_5pct_cv = NA_real_, kpss_verdict = NA_character_,
                  note = sprintf("fewer than %d observations", min_n)))
  }
  adf <- run_adf_drift(x, lags = 4L)   # model_helpers.R; urca::ur.df(type="drift")
  adf_verdict <- if (is.na(adf$adf_stat) || is.na(adf$adf_5pct)) NA_character_ else
    if (adf$adf_stat < adf$adf_5pct) "stationary" else "nonstationary"

  kpss_fit <- tryCatch(urca::ur.kpss(x, type = "mu", lags = "short"),
                       error = function(e) NULL)
  if (is.null(kpss_fit)) {
    kpss_stat <- NA_real_; kpss_cv <- NA_real_; kpss_verdict <- NA_character_
  } else {
    kpss_stat    <- unname(kpss_fit@teststat)
    kpss_cv      <- unname(kpss_fit@cval[1L, "5pct"])
    kpss_verdict <- if (kpss_stat > kpss_cv) "nonstationary" else "stationary"
  }

  tibble(window = label, n_obs = n,
         adf_stat = adf$adf_stat, adf_5pct_cv = adf$adf_5pct, adf_verdict = adf_verdict,
         kpss_stat = kpss_stat, kpss_5pct_cv = kpss_cv, kpss_verdict = kpss_verdict,
         note = "")
}

# Each spec's ACTUAL estimation window: re-fit the real (non-UECM) spec so
# the window matches what the paper's committed coefficient table used, not
# the UECM's window (which loses one extra quarter to the added lag terms).
spec6_fit <- he$fit_ecm_spec(
  master, "Spec6_Preferred",
  lr_vars = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y", "real_rate",
             "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag"),
  sr_vars = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid"),
  dummy_vars = base_dummies)
spec11_fit <- he$fit_ecm_spec(
  master, "Spec11_LIVES_Headline",
  lr_vars = c("nla_y", "ilfa_y", "ha_x_cci", "hp_x_1_minus_cci", "r_x_cci",
             "cci_williams", "ln_yp_over_y", "yp_x_cci", "ecm_lag"),
  sr_vars = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
  dummy_vars = base_dummies)
spec12_fit <- he$fit_lives_calibrated_spec(master, dummy_vars = base_dummies)

cat(sprintf("\n[sanity check] est_data n: Spec6=%d  Spec11=%d  Spec12=%d\n",
            nrow(spec6_fit$est_data), nrow(spec11_fit$est_data), nrow(spec12_fit$est_data)))

windows <- list(
  full                    = range(master$date[!is.na(master$ecm_lag)]),
  Spec6_Preferred         = range(spec6_fit$est_data$date),
  Spec11_LIVES_Headline   = range(spec11_fit$est_data$date),
  Spec12_LIVES_Calibrated = range(spec12_fit$est_data$date)
)

cy_rows <- lapply(names(windows), function(w) {
  rng <- windows[[w]]
  x <- master$ecm_lag[master$date >= rng[1] & master$date <= rng[2]]
  out <- run_adf_kpss(w, x)
  out$sample_from <- format(rng[1]); out$sample_to <- format(rng[2])
  out
})
cy_tbl <- bind_rows(cy_rows) %>%
  select(window, sample_from, sample_to, n_obs, adf_stat, adf_5pct_cv, adf_verdict,
         kpss_stat, kpss_5pct_cv, kpss_verdict, note)

cat("\n=== ln(c/y) [ecm_lag] stationarity: ADF (drift) + KPSS (level) ===\n")
print(as.data.frame(cy_tbl))

out2 <- file.path(PROJ_AUS, "outputs", "australia_cy_stationarity.csv")
write_csv(cy_tbl, out2)
cat(sprintf("\nSaved: %s\n", out2))

cat("\n[bounds_test] Done.\n")
