#!/usr/bin/env Rscript
# ==============================================================================
# Deployed-PROTOCOL placebo test on the Spec 11 (LIVES headline) structure
# (journal_review_2026-07.md, item B4 / Referee-1 finding 4)
# ==============================================================================
#
# Motivation. cci_placebo_test.R's "DEPLOYED-PROTOCOL PLACEBO" section
# validates the ADDITIVE 15-knot SDMMA spline that feeds Spec 8/11 (the
# knot-selection stage): it re-fits the Spec-4-style long run with random
# knot sets under the same iterated sign-prior reduction and positions
# Williams' institutional knots in that distribution. It stops there — it
# never carries the resulting placebo cci_williams series into the
# MULTIPLICATIVE Spec-11 interaction structure that is actually the paper's
# headline object. This script closes that gap.
#
# Protocol per placebo draw:
#   1. Generate a random 15-knot set under the SAME support/window and
#      duplicate-year-month rule as the existing deployed-protocol placebo
#      (uniform in 1979-01-01..2021-12-01), paired with the deployed
#      institutional sign priors (same knot ORDER -> same priors, exactly
#      as cci_placebo_test.R's make_basis_fn()).
#   2. Run the SAME iterated drop-on-violation reduction
#      (fit_consumption_with_williams_cci(), Spec-4-style long run) to
#      obtain a placebo cci_williams series.
#   3. Build the FULL Spec 11 regressor set with that placebo CCI, using
#      exactly the de-meaning convention at australia_estimation.R's Spec 8/11
#      construction (~lines 1795-1870 / the [4a-ii] global-attach block
#      ~line 4057-4078): ha_x_cci, hp_x_1_minus_cci, r_x_cci de-meaned
#      interactions, cci_williams autonomous-intercept level, yp_x_cci
#      de-meaned interaction, ilfa_y = eq_y + super_y.
#   4. Estimate Spec 11 on that placebo regressor set and record adj R^2,
#      log-likelihood, BIC, lambda (ecm_lag coefficient), and the joint
#      Newey-West Wald/F test that the five CCI-dependent terms
#      (ha_x_cci, hp_x_1_minus_cci, r_x_cci, cci_williams, yp_x_cci) are
#      jointly zero.
#
# The REAL cci_williams (institutional 15-knot basis, same reduction) is put
# through the identical Spec-11 construction and positioned in the resulting
# 200-draw placebo distribution on each metric.
#
# Seed convention: reuses cci_placebo_test.R's deployed-protocol placebo seed
# (20260611L) and knot-generation loop verbatim, so this script's random
# knot draws are IDENTICAL, quarter-for-quarter, to the additive-stage
# placebo's draws -- the two placebo tests differ only in which downstream
# specification (Spec-4-style additive vs Spec-11 multiplicative) is fit on
# top of the same random institutional terrain.
#
# Output:
#   outputs/australia_cci_placebo_spec11.csv          (per-draw rows)
#   outputs/australia_cci_placebo_spec11_summary.csv   (percentiles)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(tidyr)
  library(zoo); library(sandwich); library(lmtest); library(stringr)
  library(lubridate)
})

N_DRAWS_TARGET <- 200L
RUNTIME_BUDGET_MIN <- 90

.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
output_dir   <- file.path(project_root, "outputs")

source(file.path(project_root, "R", "model_helpers.R"))

# ------------------------------------------------------------------------
# model_data: use pipeline state if this script is sourced from the
# orchestrator (australia_estimation.R MAIN); otherwise rebuild in
# isolation from the RDS. Identical to cci_placebo_test.R's block so the
# canonical permanent-income method and variable construction match exactly
# (NS review fix: never rebuild-and-clobber the caller's model_data).
# ------------------------------------------------------------------------
if (exists("model_data") && exists("fit_ecm_spec") &&
    exists("construct_permanent_income_italy")) {
  message("[cci_placebo_spec11] using pipeline model_data (canonical PI method)")
  model_data <- model_data  # local working copy in this script's environment
} else {
  master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))

  model_data <- master %>%
    rename(
      dlcons   = d_ln_cons_pc,
      lincome  = ln_ydi_real_pc,
      lcons    = ln_cons_real_pc
    ) %>%
    mutate(ecm_lag = lag(lcons, 1L) - lincome)

  src <- readLines(file.path(project_root, "R", "australia_estimation.R"))
  guard_start <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
  guard_end   <- guard_start + 6L  # block is 7 lines including closing brace
  src_safe <- src[-(guard_start:guard_end)]
  main_start <- grep("\\[Step 1\\]", src_safe)[1L]
  eval(parse(text = paste(src_safe[1:(main_start - 1L)], collapse = "\n")),
       envir = environment())

  model_data <- add_model_variables(model_data)
  model_data <- compute_income_volatility(model_data)
  model_data <- if (identical(PI_METHOD, "italy")) {
    construct_permanent_income_italy(model_data)
  } else {
    construct_permanent_income(model_data)
  }
  model_data <- model_data %>%
    mutate(ecm_lag = lag(lcons, 1L) - lincome)
}

if (!exists("fit_consumption_with_williams_cci")) {
  stop("[cci_placebo_spec11] fit_consumption_with_williams_cci not available; ",
       "cannot run (needs australia_estimation.R sourced up to its helper ",
       "definitions).")
}

SAMPLE_END <- as.Date("2024-10-01")

# Spec-4-style template used for the additive knot-survival reduction stage
# (identical to cci_placebo_test.R's spec_template / deployed protocol).
spec_template <- list(
  lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

# Spec 11 sr_vars / dummy_vars, matching australia_estimation.R exactly
# (Spec8/Spec11 block, ~line 1817-1870).
SPEC11_SR_VARS    <- c("dd4_income", "d2_log_unemp", "abs_income_resid")
SPEC11_DUMMY_VARS <- spec_template$dummy_vars  # same base_dummies

# ------------------------------------------------------------------------
# run_deployed_protocol(): the additive-stage reduction, exactly as in
# cci_placebo_test.R (same lr_vars/sr_vars/dummy_vars/sample_end), but
# returning the FULL fit_consumption_with_williams_cci() result (not just
# summary stats) so the caller can pull model_data$cci_williams forward
# into the Spec-11 construction.
# ------------------------------------------------------------------------
run_deployed_protocol <- function(basis_fn) {
  md <- model_data
  md <- md[, !grepl("^sdmma_", names(md))]
  tryCatch(
    fit_consumption_with_williams_cci(
      md,
      lr_vars    = spec_template$lr_vars,
      sr_vars    = spec_template$sr_vars,
      dummy_vars = spec_template$dummy_vars,
      sample_end = SAMPLE_END,
      basis_fn   = basis_fn
    ),
    error = function(e) NULL
  )
}

make_basis_fn <- function(knot_dates, priors) {
  force(knot_dates); force(priors)
  function(d) {
    basis <- vapply(knot_dates, function(k) smoothed_step(d, k),
                    numeric(length(d)))
    colnames(basis) <- paste0("sdmma_",
                              gsub("-", "_", substr(knot_dates, 1, 7)))
    attr(basis, "sign_priors") <- priors
    basis
  }
}

# ------------------------------------------------------------------------
# fit_spec11_with_cci(): builds the FULL Spec 11 (LIVES headline)
# regressor set given a cci series, reproducing australia_estimation.R's
# Spec 8/11 de-meaning convention exactly (lines ~1795-1870):
#   - means (ha, hp, r, yp) computed on the mask date>=1980-01-01,
#     date<=sample_end, cci non-NA;
#   - de-meaned interactions r_x_cci, hp_x_1_minus_cci (varpi=1.2),
#     yp_x_cci, ha_x_cci;
#   - ilfa_y = eq_y + super_y;
#   - lr_vars: nla_y, ilfa_y, ha_x_cci, hp_x_1_minus_cci, r_x_cci,
#     cci_williams (autonomous intercept level), ln_yp_over_y, yp_x_cci,
#     ecm_lag;
#   - sr_vars/dummy_vars: Spec 11's own (dd4_income, d2_log_unemp,
#     abs_income_resid / base_dummies).
# ------------------------------------------------------------------------
CCI_TERMS <- c("ha_x_cci", "hp_x_1_minus_cci", "r_x_cci",
               "cci_williams", "yp_x_cci")

fit_spec11_with_cci <- function(base_model_data, cci_values,
                                 sample_end = SAMPLE_END) {
  md <- base_model_data
  md$cci_williams <- cci_values
  if (!"ilfa_y" %in% names(md)) md$ilfa_y <- md$eq_y + md$super_y

  mask <- !is.na(md$cci_williams) &
          md$date >= as.Date("1980-01-01") &
          md$date <= sample_end
  if (sum(mask, na.rm = TRUE) < 30L) return(NULL)

  ha_mean <- mean(md$ha_y[mask],         na.rm = TRUE)
  hp_mean <- mean(md$ln_hp_over_y[mask], na.rm = TRUE)
  r_mean  <- mean(md$real_rate[mask],    na.rm = TRUE)
  yp_mean <- mean(md$ln_yp_over_y[mask], na.rm = TRUE)

  md <- md %>%
    mutate(
      r_x_cci          = (real_rate    - r_mean)  * cci_williams,
      hp_x_1_minus_cci = (ln_hp_over_y - hp_mean) * (1 - 1.2 * cci_williams),
      yp_x_cci         = (ln_yp_over_y - yp_mean) * cci_williams,
      ha_x_cci         = (ha_y         - ha_mean) * cci_williams
    )

  spec <- tryCatch(
    fit_ecm_spec(
      data       = md,
      spec_name  = "Spec11_placebo",
      lr_vars    = c("nla_y", "ilfa_y",
                     "ha_x_cci", "hp_x_1_minus_cci", "r_x_cci",
                     "cci_williams",
                     "ln_yp_over_y", "yp_x_cci",
                     "ecm_lag"),
      sr_vars    = SPEC11_SR_VARS,
      dummy_vars = SPEC11_DUMMY_VARS,
      sample_end = sample_end
    ),
    error = function(e) NULL
  )
  spec
}

# Joint Newey-West Wald/F test that a set of terms are jointly zero, using
# the spec's own HAC (Newey-West) vcov -- base R/sandwich only, no car.
joint_wald_test <- function(spec, terms) {
  fit <- spec$fit
  cf  <- coef(fit)
  present <- terms[terms %in% names(cf) & !is.na(cf[terms])]
  if (length(present) == 0L) {
    return(list(F = NA_real_, df1 = 0L, df2 = NA_integer_,
                p = NA_real_, n_terms = 0L))
  }
  b <- cf[present]
  V <- spec$nw_vcov[present, present, drop = FALSE]
  Vinv <- tryCatch(solve(V), error = function(e) NULL)
  if (is.null(Vinv)) {
    return(list(F = NA_real_, df1 = length(present), df2 = NA_integer_,
                p = NA_real_, n_terms = length(present)))
  }
  q    <- length(present)
  W    <- as.numeric(t(b) %*% Vinv %*% b)
  df2  <- df.residual(fit)
  Fstat <- W / q
  pval  <- pf(Fstat, q, df2, lower.tail = FALSE)
  list(F = Fstat, df1 = q, df2 = df2, p = pval, n_terms = q)
}

spec11_metrics <- function(spec, n_survivors_additive = NA_integer_) {
  if (is.null(spec)) {
    return(tibble::tibble(
      adj_r2 = NA_real_, logLik = NA_real_, BIC = NA_real_,
      lambda = NA_real_, n_obs = NA_integer_,
      wald_F = NA_real_, wald_df1 = NA_integer_, wald_df2 = NA_integer_,
      wald_p = NA_real_, n_cci_terms = NA_integer_,
      n_survivors_additive = n_survivors_additive
    ))
  }
  cf <- coef(spec$fit)
  w  <- joint_wald_test(spec, CCI_TERMS)
  tibble::tibble(
    adj_r2 = summary(spec$fit)$adj.r.squared,
    logLik = as.numeric(logLik(spec$fit)),
    BIC    = stats::BIC(spec$fit),
    lambda = if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_,
    n_obs  = nobs(spec$fit),
    wald_F = w$F, wald_df1 = w$df1, wald_df2 = w$df2, wald_p = w$p,
    n_cci_terms = w$n_terms,
    n_survivors_additive = n_survivors_additive
  )
}

# ==============================================================================
# 1. REAL cci_williams -> Spec 11 benchmark (institutional 15-knot basis,
#    same iterated reduction as the deployed pipeline)
# ==============================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
cat("Fitting REAL (institutional) cci_williams -> Spec 11 benchmark\n")
cat(strrep("=", 70), "\n", sep = "")

real_basis  <- build_williams_cci_basis(model_data$date)
real_priors <- attr(real_basis, "sign_priors")
n_knots     <- ncol(real_basis)

real_reduction <- run_deployed_protocol(build_williams_cci_basis)
if (is.null(real_reduction)) {
  stop("[cci_placebo_spec11] real (institutional) reduction failed; aborting.")
}
real_cci    <- real_reduction$model_data$cci_williams
real_spec11 <- fit_spec11_with_cci(model_data, real_cci)
if (is.null(real_spec11)) {
  stop("[cci_placebo_spec11] real Spec 11 fit failed; aborting.")
}
real_metrics <- spec11_metrics(real_spec11,
                               length(real_reduction$surviving_knots))
cat(sprintf("  REAL: adj R^2=%.4f  logLik=%.2f  BIC=%.2f  lambda=%.4f  n_obs=%d\n",
            real_metrics$adj_r2, real_metrics$logLik, real_metrics$BIC,
            real_metrics$lambda, real_metrics$n_obs))
cat(sprintf("  REAL: CCI-block Wald F(%d,%d)=%.3f  p=%.4g  (%d/%d survivors)\n",
            real_metrics$wald_df1, real_metrics$wald_df2, real_metrics$wald_F,
            real_metrics$wald_p, length(real_reduction$surviving_knots),
            n_knots))

# ==============================================================================
# 2. Placebo draws: random 15-knot sets -> reduction -> Spec 11
# ==============================================================================
dp_window_start <- as.numeric(as.Date("1979-01-01"))
dp_window_end   <- as.numeric(as.Date("2021-12-01"))

draw_one <- function(seed_state_unused = NULL) {
  repeat {
    rd <- sort(as.Date(round(runif(n_knots, dp_window_start, dp_window_end))))
    nm <- substr(as.character(rd), 1, 7)
    if (!anyDuplicated(nm)) break
  }
  reduction <- run_deployed_protocol(make_basis_fn(as.character(rd), real_priors))
  if (is.null(reduction)) {
    return(list(row = tibble::tibble(
      knots = paste(format(rd), collapse = ", "),
      adj_r2 = NA_real_, logLik = NA_real_, BIC = NA_real_,
      lambda = NA_real_, n_obs = NA_integer_,
      wald_F = NA_real_, wald_df1 = NA_integer_, wald_df2 = NA_integer_,
      wald_p = NA_real_, n_cci_terms = NA_integer_,
      n_survivors_additive = NA_integer_
    )))
  }
  placebo_cci <- reduction$model_data$cci_williams
  s11 <- fit_spec11_with_cci(model_data, placebo_cci)
  m <- spec11_metrics(s11, length(reduction$surviving_knots))
  list(row = tibble::tibble(knots = paste(format(rd), collapse = ", ")) %>%
              bind_cols(m))
}

set.seed(20260611L)  # same seed as cci_placebo_test.R's deployed-protocol
                      # placebo -> identical knot draws, additive stage only
                      # replayed here so both placebos share random terrain

cat(sprintf("\nTiming first 5 draws for the runtime guard...\n"))
t5 <- system.time({
  first5 <- lapply(1:5, function(i) draw_one())
})
elapsed5_min <- unname(t5["elapsed"]) / 60
per_draw_min <- elapsed5_min / 5
projected_200_min <- per_draw_min * N_DRAWS_TARGET
cat(sprintf("  5 draws took %.2f min (%.3f min/draw); projected %d-draw total = %.1f min\n",
            elapsed5_min, per_draw_min, N_DRAWS_TARGET, projected_200_min))

if (projected_200_min > RUNTIME_BUDGET_MIN) {
  N_DRAWS <- 100L
  runtime_note <- sprintf(
    "Reduced from 200 to 100 draws: projected 200-draw runtime %.1f min > %d min budget (5-draw timing: %.2f min).",
    projected_200_min, RUNTIME_BUDGET_MIN, elapsed5_min)
  message("[cci_placebo_spec11] ", runtime_note)
} else {
  N_DRAWS <- N_DRAWS_TARGET
  runtime_note <- sprintf(
    "Full 200 draws run: projected 200-draw runtime %.1f min <= %d min budget (5-draw timing: %.2f min).",
    projected_200_min, RUNTIME_BUDGET_MIN, elapsed5_min)
}

placebo_rows <- vector("list", N_DRAWS)
for (i in 1:5) placebo_rows[[i]] <- first5[[i]]$row

pb_step <- max(1L, N_DRAWS %/% 20L)
if (N_DRAWS > 5L) {
  cat(sprintf("\nRunning remaining %d placebo draws (Spec-11 deployed protocol)...\n",
              N_DRAWS - 5L))
  for (i in 6:N_DRAWS) {
    if (i %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", i, N_DRAWS))
    placebo_rows[[i]] <- draw_one()$row
  }
}

placebo_tbl <- bind_rows(placebo_rows) %>%
  mutate(draw = row_number(), .before = 1L)

write.csv(placebo_tbl, file.path(output_dir, "australia_cci_placebo_spec11.csv"),
          row.names = FALSE)
cat(sprintf("\nSaved: %s\n",
            file.path(output_dir, "australia_cci_placebo_spec11.csv")))

# ==============================================================================
# 3. Percentile of REAL Spec 11 in the placebo distribution, per metric
# ==============================================================================
pct_higher_better <- function(placebo_vec, real_val) {
  x <- placebo_vec[is.finite(placebo_vec)]
  if (length(x) == 0L || !is.finite(real_val)) return(NA_real_)
  mean(x < real_val)
}
pct_lower_better <- function(placebo_vec, real_val) {
  x <- placebo_vec[is.finite(placebo_vec)]
  if (length(x) == 0L || !is.finite(real_val)) return(NA_real_)
  mean(x > real_val)
}

pct_adj_r2 <- pct_higher_better(placebo_tbl$adj_r2, real_metrics$adj_r2)
pct_logLik <- pct_higher_better(placebo_tbl$logLik, real_metrics$logLik)
pct_BIC    <- pct_lower_better(placebo_tbl$BIC,    real_metrics$BIC)
pct_lambda <- pct_higher_better(abs(placebo_tbl$lambda), abs(real_metrics$lambda))
pct_waldF  <- pct_higher_better(placebo_tbl$wald_F, real_metrics$wald_F)

finite_n <- function(v) sum(is.finite(v))

summary_tbl <- tibble::tibble(
  metric = c("adj_r2", "logLik", "BIC", "abs_lambda", "wald_F_cci_block"),
  real_value = c(real_metrics$adj_r2, real_metrics$logLik, real_metrics$BIC,
                 abs(real_metrics$lambda), real_metrics$wald_F),
  placebo_n_finite = c(finite_n(placebo_tbl$adj_r2), finite_n(placebo_tbl$logLik),
                       finite_n(placebo_tbl$BIC), finite_n(placebo_tbl$lambda),
                       finite_n(placebo_tbl$wald_F)),
  placebo_mean = c(mean(placebo_tbl$adj_r2, na.rm = TRUE),
                   mean(placebo_tbl$logLik, na.rm = TRUE),
                   mean(placebo_tbl$BIC, na.rm = TRUE),
                   mean(abs(placebo_tbl$lambda), na.rm = TRUE),
                   mean(placebo_tbl$wald_F, na.rm = TRUE)),
  placebo_median = c(median(placebo_tbl$adj_r2, na.rm = TRUE),
                     median(placebo_tbl$logLik, na.rm = TRUE),
                     median(placebo_tbl$BIC, na.rm = TRUE),
                     median(abs(placebo_tbl$lambda), na.rm = TRUE),
                     median(placebo_tbl$wald_F, na.rm = TRUE)),
  placebo_p10 = c(quantile(placebo_tbl$adj_r2, 0.10, na.rm = TRUE),
                  quantile(placebo_tbl$logLik, 0.10, na.rm = TRUE),
                  quantile(placebo_tbl$BIC, 0.10, na.rm = TRUE),
                  quantile(abs(placebo_tbl$lambda), 0.10, na.rm = TRUE),
                  quantile(placebo_tbl$wald_F, 0.10, na.rm = TRUE)),
  placebo_p90 = c(quantile(placebo_tbl$adj_r2, 0.90, na.rm = TRUE),
                  quantile(placebo_tbl$logLik, 0.90, na.rm = TRUE),
                  quantile(placebo_tbl$BIC, 0.90, na.rm = TRUE),
                  quantile(abs(placebo_tbl$lambda), 0.90, na.rm = TRUE),
                  quantile(placebo_tbl$wald_F, 0.90, na.rm = TRUE)),
  real_percentile = c(pct_adj_r2, pct_logLik, pct_BIC, pct_lambda, pct_waldF),
  higher_is_better = c(TRUE, TRUE, FALSE, TRUE, TRUE)
)

meta_tbl <- tibble::tibble(
  metric = c("n_draws", "n_draws_target", "runtime_note",
             "real_n_survivors_additive", "real_n_knots_candidate",
             "real_wald_p", "real_lambda", "real_n_obs", "seed"),
  value = c(as.character(N_DRAWS), as.character(N_DRAWS_TARGET), runtime_note,
            as.character(length(real_reduction$surviving_knots)),
            as.character(n_knots),
            sprintf("%.4g", real_metrics$wald_p),
            sprintf("%.4f", real_metrics$lambda),
            as.character(real_metrics$n_obs),
            "20260611")
)

write.csv(summary_tbl,
          file.path(output_dir, "australia_cci_placebo_spec11_summary.csv"),
          row.names = FALSE)
write.csv(meta_tbl,
          file.path(output_dir, "australia_cci_placebo_spec11_meta.csv"),
          row.names = FALSE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("SPEC 11 DEPLOYED-PROTOCOL PLACEBO -- SUMMARY\n")
cat(strrep("=", 70), "\n", sep = "")
print(as.data.frame(summary_tbl), row.names = FALSE)
cat(sprintf("\nn_draws = %d (target %d). %s\n", N_DRAWS, N_DRAWS_TARGET, runtime_note))
cat(sprintf("Saved: %s\n", file.path(output_dir, "australia_cci_placebo_spec11.csv")))
cat(sprintf("Saved: %s\n", file.path(output_dir, "australia_cci_placebo_spec11_summary.csv")))
cat(sprintf("Saved: %s\n", file.path(output_dir, "australia_cci_placebo_spec11_meta.csv")))
