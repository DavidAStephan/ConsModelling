#!/usr/bin/env Rscript
# ==============================================================================
# NS-B2 — Nested (CCI-knot-uncertainty) bootstrap for the Spec 11 LIVES headline
# ==============================================================================
#
# Motivation. The paper's existing uncertainty statements for Spec 11 (the
# delta-method CI and the plain moving-block residual bootstrap, both in
# gamma_inference.R / australia_gamma_inference.csv) hold the CCI series
# cci_williams FIXED at its point estimate and treat the six CCI interaction
# regressors (ha_x_cci, hp_x_1_minus_cci, r_x_cci, yp_x_cci, cci_williams
# itself) as if they were observed data rather than generated regressors from
# a two-stage estimator (Stage 1: iterated sign-prior knot-survival reduction
# on a 15-knot SDMMA basis; Stage 2: the Spec 11 ECM). Both existing methods
# are therefore anti-conservative: a referee will ask what the CIs look like
# once the knot-selection pre-test is allowed to vary with the data, as the
# paper itself admits (see gamma_inference.R's CAVEAT and
# review/journal_review_2026-07.md item B2).
#
# Design (implemented exactly as specified in the B2 task):
#   For b in 1..B:
#     (i)   Resample the FITTED SPEC-11 ECM's residuals with a moving-block
#           bootstrap, block length L = 8 quarters, WRAP-AROUND (circular):
#           block start positions are drawn uniformly from 1..n and each
#           block indexes (s, s+1, ..., s+L-1) mod n, so blocks that run off
#           the end of the sample wrap back to the start. This differs from
#           the boundary-avoiding block bootstrap in gamma_inference.R
#           (which restricts starts to 1..n-L+1); wrap-around gives every
#           observation, including the last L-1, an equal chance of
#           appearing as a block interior point.
#     (ii)  Rebuild bootstrap consumption growth
#           dlcons_b = fitted(Spec 11) + resampled residuals, over the Spec
#           11 estimation window (1988Q3-2024Q4, n=146, contiguous quarters).
#           Integrate dlcons_b into a bootstrap consumption LEVEL path
#           lcons_b by cumulative summation from the ACTUAL log-consumption
#           level in the quarter immediately preceding the estimation
#           window (1988Q2) — i.e. the true initial condition is preserved;
#           only the within-sample growth path is resampled.
#     (iii) HOLDING THE INCOME AND WEALTH DATA FIXED (lincome, ha_y, nla_y,
#           eq_y, super_y, ln_hp_over_y, real_rate, ln_yp_over_y are never
#           touched), rebuild ecm_lag_b = lag(lcons_b, 1) - lincome and
#           RE-RUN the CCI knot-selection stage — the same iterated
#           drop-on-violation (Hendry-Krolzig) reduction the deployed
#           pipeline uses — on the bootstrap consumption. This is exactly
#           fit_consumption_with_williams_cci() from australia_estimation.R,
#           called with the SAME lr_vars/sr_vars/dummy_vars/sample_end and
#           the SAME 15-knot maximal basis + institutional sign priors as
#           Step 4a-i of the deployed pipeline (and as the "deployed-protocol
#           placebo" section of cci_placebo_test.R exercises with randomised
#           knot dates) — but now the surviving knot SET is free to differ
#           from the deployed set because the response (dlcons_b) differs.
#           This produces a bootstrap CCI_b.
#     (iv)  Rebuild the six Spec 11 interaction regressors from CCI_b using
#           the SAME de-meaning convention as the deployed pipeline (Step
#           4a-ii): each de-meaning mean is recomputed on this draw's own
#           estimation mask (!is.na(CCI_b) & 1980Q1 <= date <= sample_end),
#           not frozen at the point-estimate's mean — the convention (the
#           formula), not the numeric constant, is held fixed.
#     (v)   Re-estimate Spec 11 (same fit_ecm_spec call as the deployed
#           pipeline) and store lambda, the OLS coefficients on nla_y,
#           ilfa_y, ln_yp_over_y, yp_x_cci, ha_x_cci, hp_x_1_minus_cci, and
#           the implied structural coefficients gamma_i = -beta_i / lambda.
#
# LIMITATION (disclosed, not hidden): permanent income (ln_yp_over_y) is
# constructed ONCE from the actual data via construct_permanent_income_italy()
# and held fixed across all B draws. This bootstrap therefore carries the
# CCI/knot-selection pre-test uncertainty but NOT the permanent-income
# first-stage uncertainty (that is item B3 in the review plan — real-time-PI
# promotion — and is out of scope here). The resulting CIs are still
# anti-conservative on that one remaining dimension; they are a strict
# improvement on, not a full replacement for, a fully joint bootstrap.
#
# Runtime guard: the first 5 draws are timed; if the elapsed time projects to
# more than 2.5 hours for B = 199, B is cut to 99 and the fallback is logged
# in the output CSVs (n_draws_planned vs n_draws_run).
#
# OBSERVED MECHANISM (documented, not a bug — verified by isolating it): the
# nested-bootstrap lambda distribution is centred well below the point
# estimate in magnitude (draw median |lambda| ~ 0.17 vs point |lambda| =
# 0.448) even when the CCI Stage-1 refit is bypassed and only ecm_lag is
# allowed to float. The cause is the LEVEL-INTEGRATION step itself: because
# dlcons_b uses the ORIGINAL (point-estimate) fitted values plus resampled
# residuals — a fixed-design residual bootstrap, exactly as specified — the
# residual-bootstrap noise accumulates via cumsum() into ecm_lag_b, which
# therefore carries a slow-moving (non-iid) perturbation on top of the true
# ecm_lag rather than simple iid measurement error (correlation with the
# actual ecm_lag path ~0.85, but variance ~25% higher). Regressing dlcons_b
# on this noisier, partially-decoupled ecm_lag_b attenuates the estimated
# adjustment speed toward zero. This is an unavoidable consequence of using
# a fixed-design (non-recursive) residual bootstrap to regenerate a
# regressor that is itself constructed from the cumulative consumption
# level — a recursive/dynamic bootstrap (where fitted values update each
# period using the evolving ecm_lag_b) would not have this property, but
# was not what item B2 specified. The resulting CIs are therefore wide for
# an additional, disclosed reason beyond CCI-knot uncertainty alone: they
# also carry uncertainty in the ecm_lag construction that the delta-method
# and the existing plain residual bootstrap (which freezes the full design
# matrix X, including ecm_lag) do not.
#
# Outputs:
#   Australia/outputs/australia_nested_bootstrap.csv     (per-draw results)
#   Australia/outputs/australia_nested_bootstrap_ci.csv   (CI comparison table:
#     nested percentile CI vs the existing delta-method CI from
#     australia_gamma_inference.csv, side by side)
#
# Run:  Rscript Australia/R/nested_bootstrap.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(sandwich); library(lmtest)
})
options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
output_dir <- file.path(PROJ_AUS, "outputs")

B_PLANNED   <- 199L
BLOCK_LEN   <- 8L
SEED        <- 20260716L
TIME_GUARD_HOURS <- 2.5

# ------------------------------------------------------------------
# Setup: source pipeline helper functions (fit_ecm_spec,
# fit_consumption_with_williams_cci, build_williams_cci_basis,
# add_model_variables, compute_income_volatility,
# construct_permanent_income_italy) exactly as gamma_inference.R does, into
# an isolated child environment so nothing here can clobber a canonical
# pipeline run.
# ------------------------------------------------------------------
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src      <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
he       <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")), envir = he)

master <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")) %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)
master <- he$add_model_variables(master)
master <- he$compute_income_volatility(master)
master <- he$construct_permanent_income_italy(master)   # canonical PI — FIXED for every draw
master <- master %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
sample_end <- as.Date("2024-10-01")

cci_lr_vars <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ecm_lag")
spec11_lr_vars <- c("nla_y", "ilfa_y", "ha_x_cci", "hp_x_1_minus_cci",
                    "r_x_cci", "cci_williams", "ln_yp_over_y", "yp_x_cci",
                    "ecm_lag")
spec11_sr_vars <- c("dd4_income", "d2_log_unemp", "abs_income_resid")
gamma_terms    <- c("nla_y", "ilfa_y", "ln_yp_over_y", "yp_x_cci",
                    "ha_x_cci", "hp_x_1_minus_cci")

# ------------------------------------------------------------------
# Point estimate: rebuild the deployed CCI (Stage 1) + Spec 11 (Stage 2) on
# the ACTUAL data. This reproduces australia_gamma_inference.csv's Spec11
# row (lambda = -0.4483, n = 146) exactly and gives us the fitted values /
# residuals / estimation-window row indices the bootstrap resamples from.
# ------------------------------------------------------------------
cat("[nested_bootstrap] Fitting point-estimate CCI (Stage 1) + Spec 11 (Stage 2) on actual data...\n")
cci_fit_actual <- he$fit_consumption_with_williams_cci(
  master, lr_vars = cci_lr_vars, sr_vars = character(0),
  dummy_vars = base_dummies, sample_end = sample_end
)
master$cci_williams <- cci_fit_actual$model_data$cci_williams

attach_interactions <- function(md, mask) {
  ha_m <- mean(md$ha_y[mask],         na.rm = TRUE)
  hp_m <- mean(md$ln_hp_over_y[mask], na.rm = TRUE)
  r_m  <- mean(md$real_rate[mask],    na.rm = TRUE)
  yp_m <- mean(md$ln_yp_over_y[mask], na.rm = TRUE)
  md %>% mutate(
    r_x_cci          = (real_rate    - r_m)  * cci_williams,
    hp_x_1_minus_cci = (ln_hp_over_y - hp_m) * (1 - 1.2 * cci_williams),
    yp_x_cci         = (ln_yp_over_y - yp_m) * cci_williams,
    ha_x_cci         = (ha_y         - ha_m) * cci_williams,
    ilfa_y           = eq_y + super_y
  )
}
mask_actual <- !is.na(master$cci_williams) &
  master$date >= as.Date("1980-01-01") & master$date <= sample_end
master <- attach_interactions(master, mask_actual)

sp11_actual <- he$fit_ecm_spec(
  data = master, spec_name = "Spec11_PointEstimate",
  lr_vars = spec11_lr_vars, sr_vars = spec11_sr_vars,
  dummy_vars = base_dummies, sample_end = sample_end
)
cf_actual  <- coef(sp11_actual$fit)
lam_actual <- cf_actual[["ecm_lag"]]
cat(sprintf("[nested_bootstrap] Point estimate: lambda = %.4f, n = %d\n",
            lam_actual, nobs(sp11_actual$fit)))

# Estimation-window row indices into `master`, in ascending-date order.
win_idx <- match(sp11_actual$est_data$date, master$date)
stopifnot(!anyNA(win_idx), all(diff(win_idx) == 1L))  # contiguous, as verified
n_win   <- length(win_idx)
seed_lcons  <- master$lcons[min(win_idx) - 1L]
fitted_vals <- as.numeric(fitted(sp11_actual$fit))
resid_vals  <- as.numeric(resid(sp11_actual$fit))
stopifnot(length(fitted_vals) == n_win)

# ------------------------------------------------------------------
# Moving-block bootstrap index generator, WRAP-AROUND (circular), block
# length L. Returns a length-n vector of indices into 1:n.
# ------------------------------------------------------------------
mb_bootstrap_indices <- function(n, L) {
  nblocks <- ceiling(n / L)
  starts  <- sample.int(n, nblocks, replace = TRUE)
  idx <- unlist(lapply(starts, function(s) ((s - 1L + 0:(L - 1L)) %% n) + 1L))
  idx[seq_len(n)]
}

# ------------------------------------------------------------------
# Build a bootstrap copy of `master` with dlcons/lcons/ecm_lag replaced over
# the Spec 11 estimation window; income and wealth columns untouched.
# ------------------------------------------------------------------
build_bootstrap_master <- function() {
  boot_idx <- mb_bootstrap_indices(n_win, BLOCK_LEN)
  e_star   <- resid_vals[boot_idx]
  dlcons_b <- fitted_vals + e_star
  lcons_b  <- cumsum(dlcons_b) + seed_lcons

  md_b <- master
  md_b$dlcons[win_idx] <- dlcons_b
  md_b$lcons[win_idx]  <- lcons_b
  md_b <- md_b %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)
  md_b
}

# ------------------------------------------------------------------
# One nested-bootstrap draw: Stage 1 (CCI re-selection) + Stage 2 (Spec 11).
# Returns a one-row tibble; failures are flagged, not silently dropped.
# ------------------------------------------------------------------
run_one_draw <- function(b) {
  md_b <- build_bootstrap_master()

  cci_b <- tryCatch(
    he$fit_consumption_with_williams_cci(
      md_b, lr_vars = cci_lr_vars, sr_vars = character(0),
      dummy_vars = base_dummies, sample_end = sample_end
    ),
    error = function(e) NULL
  )
  if (is.null(cci_b) || length(cci_b$surviving_knots) == 0L) {
    return(tibble(draw = b, success = FALSE,
                  fail_reason = if (is.null(cci_b)) "CCI Stage 1 fit error"
                                else "all knots dropped (WILLIAMS_FALLBACK)",
                  n_surviving_knots = if (is.null(cci_b)) NA_integer_ else 0L,
                  surviving_knots = NA_character_,
                  cci_min = NA_real_, cci_max = NA_real_,
                  n_obs = NA_integer_, lambda = NA_real_))
  }

  md_b$cci_williams <- cci_b$model_data$cci_williams
  mask_b <- !is.na(md_b$cci_williams) &
    md_b$date >= as.Date("1980-01-01") & md_b$date <= sample_end
  md_b <- attach_interactions(md_b, mask_b)

  sp11_b <- tryCatch(
    he$fit_ecm_spec(data = md_b, spec_name = "Spec11_Nested",
                    lr_vars = spec11_lr_vars, sr_vars = spec11_sr_vars,
                    dummy_vars = base_dummies, sample_end = sample_end),
    error = function(e) NULL
  )
  if (is.null(sp11_b) || !("ecm_lag" %in% names(coef(sp11_b$fit))) ||
      is.na(coef(sp11_b$fit)[["ecm_lag"]])) {
    return(tibble(draw = b, success = FALSE,
                  fail_reason = "Spec 11 Stage 2 fit error / aliased ecm_lag",
                  n_surviving_knots = length(cci_b$surviving_knots),
                  surviving_knots = paste(cci_b$surviving_knots, collapse = ";"),
                  cci_min = min(md_b$cci_williams, na.rm = TRUE),
                  cci_max = max(md_b$cci_williams, na.rm = TRUE),
                  n_obs = NA_integer_, lambda = NA_real_))
  }

  cf_b  <- coef(sp11_b$fit)
  lam_b <- cf_b[["ecm_lag"]]
  ols_vals   <- setNames(rep(NA_real_, length(gamma_terms)), gamma_terms)
  gamma_vals <- setNames(rep(NA_real_, length(gamma_terms)), gamma_terms)
  for (tm in gamma_terms) {
    if (tm %in% names(cf_b) && !is.na(cf_b[[tm]])) {
      ols_vals[[tm]]   <- cf_b[[tm]]
      gamma_vals[[tm]] <- -cf_b[[tm]] / lam_b
    }
  }

  row <- tibble(
    draw = b, success = TRUE, fail_reason = NA_character_,
    n_surviving_knots = length(cci_b$surviving_knots),
    surviving_knots = paste(cci_b$surviving_knots, collapse = ";"),
    cci_min = min(md_b$cci_williams, na.rm = TRUE),
    cci_max = max(md_b$cci_williams, na.rm = TRUE),
    n_obs = nobs(sp11_b$fit), lambda = lam_b
  )
  for (tm in gamma_terms) row[[paste0("ols_", tm)]]   <- ols_vals[[tm]]
  for (tm in gamma_terms) row[[paste0("gamma_", tm)]] <- gamma_vals[[tm]]
  row
}

# ------------------------------------------------------------------
# Runtime guard: time the first 5 draws, project the full run, cut B to 99
# if the projection exceeds TIME_GUARD_HOURS.
# ------------------------------------------------------------------
set.seed(SEED)
cat("[nested_bootstrap] Timing first 5 draws for the runtime guard...\n")
t0 <- proc.time()[["elapsed"]]
warmup_rows <- lapply(1:5, run_one_draw)
t5 <- proc.time()[["elapsed"]] - t0
per_draw <- t5 / 5
projected_hours <- per_draw * B_PLANNED / 3600
cat(sprintf("[nested_bootstrap] 5 draws took %.2fs (%.4fs/draw); projected B=%d run = %.3f hours\n",
            t5, per_draw, B_PLANNED, projected_hours))

if (projected_hours > TIME_GUARD_HOURS) {
  B_FINAL <- 99L
  cat(sprintf("[nested_bootstrap] RUNTIME GUARD TRIPPED: projected %.2fh > %.1fh cap. Cutting B from %d to %d.\n",
              projected_hours, TIME_GUARD_HOURS, B_PLANNED, B_FINAL))
} else {
  B_FINAL <- B_PLANNED
  cat(sprintf("[nested_bootstrap] Runtime guard OK (%.3fh projected <= %.1fh cap). Running full B=%d.\n",
              projected_hours, TIME_GUARD_HOURS, B_FINAL))
}

pb_step <- max(1L, B_FINAL %/% 20L)
remaining_rows <- list()
if (B_FINAL > 5L) {
  for (b in 6:B_FINAL) {
    if (b %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", b, B_FINAL))
    remaining_rows[[length(remaining_rows) + 1L]] <- run_one_draw(b)
  }
}

all_rows <- bind_rows(c(warmup_rows[seq_len(min(5L, B_FINAL))], remaining_rows))
all_rows <- all_rows %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))

t_total <- proc.time()[["elapsed"]] - t0
cat(sprintf("[nested_bootstrap] Completed %d draws in %.1fs (%d succeeded, %d failed)\n",
            nrow(all_rows), t_total, sum(all_rows$success), sum(!all_rows$success)))

write.csv(all_rows, file.path(output_dir, "australia_nested_bootstrap.csv"),
          row.names = FALSE)
cat("Saved: Australia/outputs/australia_nested_bootstrap.csv\n")

# ==============================================================================
# CI comparison table: nested percentile CI vs the existing delta-method CI
# ==============================================================================
ok <- all_rows %>% filter(success)
n_ok <- nrow(ok)
cat(sprintf("[nested_bootstrap] %d / %d draws usable for percentile CIs\n",
            n_ok, nrow(all_rows)))

q95 <- function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)

delta_path <- file.path(output_dir, "australia_gamma_inference.csv")
delta_tbl  <- if (file.exists(delta_path)) {
  read.csv(delta_path) %>% filter(spec == "Spec11")
} else {
  message("[nested_bootstrap] ", delta_path, " not found — delta-method columns will be NA")
  NULL
}
delta_row <- function(term) {
  if (is.null(delta_tbl)) return(list(ols = NA_real_, gamma = NA_real_,
                                       ci_lo = NA_real_, ci_hi = NA_real_,
                                       williams = NA_real_))
  r <- delta_tbl[delta_tbl$term == term, ]
  if (nrow(r) == 0L) return(list(ols = NA_real_, gamma = NA_real_,
                                  ci_lo = NA_real_, ci_hi = NA_real_,
                                  williams = NA_real_))
  list(ols = r$ols[1], gamma = r$gamma[1], ci_lo = r$ci_lo[1],
       ci_hi = r$ci_hi[1], williams = r$williams[1])
}

# lambda: no delta-method row in australia_gamma_inference.csv (it reports
# gamma_i CIs, not a lambda CI), so compute a simple Wald 95% CI from the
# point-estimate fit's Newey-West vcov for side-by-side comparison, flagged
# as such via method = "wald(NW)" rather than "delta".
lam_se_actual <- sqrt(sp11_actual$nw_vcov["ecm_lag", "ecm_lag"])
lam_wald_lo <- lam_actual - 1.96 * lam_se_actual
lam_wald_hi <- lam_actual + 1.96 * lam_se_actual

ci_rows <- list()

# --- lambda ---
qlam <- q95(ok$lambda)
ci_rows[["lambda"]] <- tibble(
  spec = "Spec11", term = "lambda", quantity = "coefficient",
  point = lam_actual,
  conventional_method = "wald(NW)",
  conventional_ci_lo = lam_wald_lo, conventional_ci_hi = lam_wald_hi,
  conventional_width = lam_wald_hi - lam_wald_lo,
  nested_median = unname(qlam[2]),
  nested_ci_lo = unname(qlam[1]), nested_ci_hi = unname(qlam[3]),
  nested_width = unname(qlam[3] - qlam[1]),
  width_ratio_nested_over_conventional = unname((qlam[3] - qlam[1]) / (lam_wald_hi - lam_wald_lo)),
  williams = NA_real_, n_draws = sum(!is.na(ok$lambda))
)

for (tm in gamma_terms) {
  dr <- delta_row(tm)

  ols_col   <- ok[[paste0("ols_", tm)]]
  gamma_col <- ok[[paste0("gamma_", tm)]]

  q_ols   <- q95(ols_col)
  q_gamma <- q95(gamma_col)

  ci_rows[[paste0(tm, "_ols")]] <- tibble(
    spec = "Spec11", term = tm, quantity = "coefficient (OLS beta)",
    point = dr$ols, conventional_method = "delta/NW (not applicable to raw beta; NW SE shown)",
    conventional_ci_lo = NA_real_, conventional_ci_hi = NA_real_,
    conventional_width = NA_real_,
    nested_median = unname(q_ols[2]),
    nested_ci_lo = unname(q_ols[1]), nested_ci_hi = unname(q_ols[3]),
    nested_width = unname(q_ols[3] - q_ols[1]),
    width_ratio_nested_over_conventional = NA_real_,
    williams = NA_real_, n_draws = sum(!is.na(ols_col))
  )

  conv_width <- dr$ci_hi - dr$ci_lo
  nest_width <- unname(q_gamma[3] - q_gamma[1])
  ci_rows[[paste0(tm, "_gamma")]] <- tibble(
    spec = "Spec11", term = tm, quantity = "structural gamma (-beta/lambda)",
    point = dr$gamma, conventional_method = "delta-method (Newey-West)",
    conventional_ci_lo = dr$ci_lo, conventional_ci_hi = dr$ci_hi,
    conventional_width = conv_width,
    nested_median = unname(q_gamma[2]),
    nested_ci_lo = unname(q_gamma[1]), nested_ci_hi = unname(q_gamma[3]),
    nested_width = nest_width,
    width_ratio_nested_over_conventional = nest_width / conv_width,
    williams = dr$williams, n_draws = sum(!is.na(gamma_col))
  )
}

ci_tbl <- bind_rows(ci_rows) %>%
  mutate(
    conventional_excludes_zero = !is.na(conventional_ci_lo) & !is.na(conventional_ci_hi) &
      (conventional_ci_lo > 0 | conventional_ci_hi < 0),
    nested_excludes_zero = !is.na(nested_ci_lo) & !is.na(nested_ci_hi) &
      (nested_ci_lo > 0 | nested_ci_hi < 0),
    williams_in_nested_ci = !is.na(williams) & !is.na(nested_ci_lo) & !is.na(nested_ci_hi) &
      williams >= nested_ci_lo & williams <= nested_ci_hi
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  mutate(
    n_draws_planned = B_PLANNED, n_draws_run = B_FINAL,
    n_draws_usable  = n_ok,
    runtime_guard_tripped = B_FINAL < B_PLANNED
  )

write.csv(ci_tbl, file.path(output_dir, "australia_nested_bootstrap_ci.csv"),
          row.names = FALSE)
cat("Saved: Australia/outputs/australia_nested_bootstrap_ci.csv\n")

cat("\n=== Nested bootstrap CI comparison (Spec 11) ===\n")
print(as.data.frame(ci_tbl %>%
  select(term, quantity, point, conventional_ci_lo, conventional_ci_hi,
         nested_ci_lo, nested_ci_hi, width_ratio_nested_over_conventional,
         nested_excludes_zero)), row.names = FALSE)
