#!/usr/bin/env Rscript
# ==============================================================================
# Australia Household Consumption Model — Part 2: Estimation Framework
# ==============================================================================
#
# Adaptation of the Muellbauer ECM framework for Australia.
# Reference: Duca, Muellbauer et al. (Australia system paper)
#
# ASSUMES: model_data tibble already exists in the environment (sourced by
# australia_consumption_model.R after Part 1).
#
# Model (canonical Engle-Granger negative-restoration convention):
#   Δ ln c_t = λ·[ln(c_{t-1}/y_t) - α_0 - α_c·CCI_t - α_1·r_t
#                 - Σγ_i·(A_i,t-1/y_t) - φ·ln(y^p_t/y_t)] + short-run + ε_t
# where ecm_lag = ln(c_{t-1}) - ln(y_t) and λ < 0 for a stable, restoring ECM.
#
# Structural params = -estimated OLS coef / λ (so structural sign = OLS sign,
# because both numerator and denominator flip relative to the old positive
# convention).
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
  library(strucchange)
  library(broom)
  library(zoo)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Permanent-income construction method.
#   "ar"    rolling AR(8) + trend + post-2008 break + GFC-learning ogive
#           (the original method; default for backwards compatibility)
#   "italy" De Bonis et al. (2020) direct forecast — directly forecasts the
#           discounted weighted average of log income in one step.
#           Adds labour-force-share predictor (Italy.pdf §3.3 highlights
#           this as the predictor that captures the early-1990s slowdown).
# A `compare_pi_methods()` step writes both side-by-side regardless of
# which is selected as the canonical input to the spec loop.
PI_METHOD <- "italy"

# Resolve output directory relative to this script
.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
output_dir <- normalizePath(
  file.path(dirname(.this_file), "..", "outputs"),
  winslash = "/", mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Source model_helpers so Williams CCI functions are available regardless of
# the entry point (australia_consumption_model.R, run_estimation_from_rds.R).
.helpers_path <- file.path(dirname(.this_file), "model_helpers.R")
if (file.exists(.helpers_path) &&
    !exists("build_williams_cci_basis", inherits = TRUE)) {
  source(.helpers_path, local = FALSE)
}

if (!exists("model_data")) {
  stop(
    "[australia_estimation.R] 'model_data' not found in environment.\n",
    "  This script is Part 2 — run australia_consumption_model.R to source",
    " Part 1 first.\n"
  )
}

# Source model_helpers.R if `run_adf_drift` is not already defined (the
# data-download script normally sources it, but the fast-rerun path skips
# data download and so needs an explicit source here).
if (!exists("run_adf_drift", mode = "function")) {
  helpers_path <- file.path(dirname(.this_file), "model_helpers.R")
  if (file.exists(helpers_path)) source(helpers_path, local = TRUE)
}

cat(rep("=", 70), "\n", sep = "")
cat("Australia Consumption Model — Part 2: Estimation Framework\n")
cat(rep("=", 70), "\n\n", sep = "")

# Helper: format a Date as "YYYYQn"
format_yq <- function(d) {
  if (length(d) == 0L || is.na(d)) return(NA_character_)
  q <- as.integer(ceiling(as.integer(format(d, "%m")) / 3))
  sprintf("%sQ%d", format(d, "%Y"), q)
}


# ==============================================================================
# SECTION A: Permanent Income Construction
# ==============================================================================
# Rolling out-of-sample AR(8) + trend forecast in log income LEVELS, with
# discounted sum of deviations from current income.  Australia exhibits a
# negative α₂ (permanent income relative to current income does not raise c/y),
# likely reflecting precautionary saving and wealth-channel dominance rather
# than forward-looking Euler-equation smoothing as in Italy.
#
# For Australia, unemp_rate starts 1978 → used as optional predictor.
# GFC learning adjustment kept: Australia avoided recession but confidence
# fell sharply; agents plausibly updated PI downward after 2008.
# min_train = 40 (paper standard; feasible since income runs from 1959).
# ==============================================================================

construct_permanent_income <- function(model_data, k = 40L, delta = 0.95,
                                       gfc_ogive = TRUE) {

  delta_q      <- delta^(1 / 4)
  disc_weights <- delta_q^(seq_len(k) - 1L)
  disc_weights <- disc_weights / sum(disc_weights)

  n   <- nrow(model_data)
  dat <- model_data %>% arrange(date)

  early_enough <- function(col) {
    if (!col %in% names(dat)) return(FALSE)
    vals <- dat[[col]]
    if (all(is.na(vals))) return(FALSE)
    min(dat$date[!is.na(vals)], na.rm = TRUE) < as.Date("2000-01-01")
  }

  has_unemp  <- early_enough("unemp_rate")
  has_oil    <- early_enough("log_oil")
  has_reer   <- early_enough("log_reer")
  has_stocks <- early_enough("log_stocks")

  dat <- dat %>%
    mutate(
      t_index   = seq_len(n),
      step2008  = as.integer(date >= as.Date("2008-07-01")),
      trend_brk = t_index * step2008,
      inc_ma4   = (lag(lincome, 1L) + lag(lincome, 2L) +
                   lag(lincome, 3L) + lag(lincome, 4L)) / 4,
      inc_lag5  = lag(lincome, 5L),
      inc_lag6  = lag(lincome, 6L),
      inc_lag7  = lag(lincome, 7L),
      inc_lag8  = lag(lincome, 8L)
    )

  core_vars     <- c("t_index", "step2008", "trend_brk",
                     "inc_ma4", "inc_lag5", "inc_lag6", "inc_lag7", "inc_lag8")
  optional_vars <- character(0)
  if (has_unemp)  optional_vars <- c(optional_vars, "unemp_rate")
  if (has_oil)    optional_vars <- c(optional_vars, "log_oil")
  if (has_reer)   optional_vars <- c(optional_vars, "log_reer")
  if (has_stocks) optional_vars <- c(optional_vars, "log_stocks")

  if (length(optional_vars) == 0L) {
    message("[construct_permanent_income] Using AR(8) + trend + break only.")
  } else {
    message("[construct_permanent_income] Additional predictors: ",
            paste(optional_vars, collapse = ", "))
  }

  rhs_vars  <- c(core_vars, optional_vars)
  min_train <- 40L
  ln_yp_over_y <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (i <= min_train) next
    train    <- dat[seq_len(i), , drop = FALSE]
    req_cols <- c("lincome", rhs_vars)
    train_cc <- train[complete.cases(train[, req_cols, drop = FALSE]), ]
    if (nrow(train_cc) < min_train) next

    fit_inc <- tryCatch(
      lm(reformulate(rhs_vars, response = "lincome"), data = train_cc),
      error = function(e) NULL
    )
    if (is.null(fit_inc)) next

    current_row      <- dat[i, , drop = FALSE]
    lincome_history  <- dat$lincome[seq_len(i)]
    forecast_levels  <- rep(NA_real_, k)
    inc_path         <- lincome_history
    cf               <- coef(fit_inc)

    for (h in seq_len(k)) {
      n_path <- length(inc_path)
      l1 <- if (n_path >= 1L) inc_path[n_path]      else NA_real_
      l2 <- if (n_path >= 2L) inc_path[n_path - 1L] else NA_real_
      l3 <- if (n_path >= 3L) inc_path[n_path - 2L] else NA_real_
      l4 <- if (n_path >= 4L) inc_path[n_path - 3L] else NA_real_
      l5 <- if (n_path >= 5L) inc_path[n_path - 4L] else NA_real_
      l6 <- if (n_path >= 6L) inc_path[n_path - 5L] else NA_real_
      l7 <- if (n_path >= 7L) inc_path[n_path - 6L] else NA_real_
      l8 <- if (n_path >= 8L) inc_path[n_path - 7L] else NA_real_

      if (anyNA(c(l1, l2, l3, l4, l5, l6, l7, l8))) {
        forecast_levels[h] <- l1
        inc_path <- c(inc_path, l1)
        next
      }

      ma4_fc  <- (l1 + l2 + l3 + l4) / 4
      t_h     <- dat$t_index[i] + h
      step_h  <- as.integer(t_h >= which(dat$date >= as.Date("2008-07-01"))[1L])
      if (is.na(step_h)) step_h <- 1L
      trend_h <- t_h * step_h

      new_row <- data.frame(
        `(Intercept)` = 1, t_index = t_h, step2008 = step_h,
        trend_brk = trend_h, inc_ma4 = ma4_fc,
        inc_lag5 = l5, inc_lag6 = l6, inc_lag7 = l7, inc_lag8 = l8,
        check.names = FALSE
      )
      for (v in optional_vars)
        new_row[[v]] <- if (!is.na(current_row[[v]])) current_row[[v]] else 0

      terms_in_model <- names(cf)
      avail_terms    <- intersect(terms_in_model, names(new_row))
      ic       <- cf[["(Intercept)"]]
      pred_val <- if (!is.na(ic)) ic else 0
      for (trm in setdiff(avail_terms, "(Intercept)")) {
        if (!is.na(cf[[trm]]) && !is.na(new_row[[trm]]))
          pred_val <- pred_val + cf[[trm]] * new_row[[trm]]
      }
      forecast_levels[h] <- pred_val
      inc_path <- c(inc_path, pred_val)
    }

    if (!anyNA(forecast_levels)) {
      deviations      <- forecast_levels - dat$lincome[i]
      ln_yp_over_y[i] <- sum(disc_weights * deviations)
    }
  }

  dat$ln_yp_over_y <- ln_yp_over_y

  # GFC learning adjustment (same ogive as Italy). Toggleable via gfc_ogive so
  # the sensitivity grid can test a genuinely ogive-free variant (previously
  # the gfc=FALSE grid cells were a no-op).
  gfc_start  <- as.Date("2008-07-01")
  adj_end    <- as.Date("2012-04-01")
  n_adj_qtrs <- 15L

  dat <- dat %>%
    mutate(
      learning_weight = if (gfc_ogive) case_when(
        date < gfc_start ~ 1.0,
        date >= adj_end  ~ 0.5,
        TRUE ~ {
          q_since <- as.integer(
            round(as.numeric(difftime(date, gfc_start, units = "days")) / 91.25)
          )
          1.0 - 0.5 * (pmin(q_since, n_adj_qtrs) / n_adj_qtrs)
        }
      ) else 1.0,
      ln_yp_over_y          = ln_yp_over_y * learning_weight,
      ln_yp_over_y_post2008 = ln_yp_over_y * step2008
    ) %>%
    select(-t_index, -step2008, -trend_brk,
           -inc_ma4, -inc_lag5, -inc_lag6, -inc_lag7, -inc_lag8,
           -learning_weight)

  dat
}


# ==============================================================================
# SECTION A.italy: Italy-style local-projection permanent income
# ==============================================================================
#
# Direct (single-regression) forecast of the discounted weighted average of
# log income k quarters ahead — the De Bonis et al. (2020, Appendix A.2)
# method (related in spirit to, but distinct from, Jorda 2005 per-horizon
# local projections) — rather than a rolling AR(p) whose forecasts are
# aggregated. Predictors are selected to match Italy
# Appendix A.6 / Table A2 as closely as data availability permits:
#
#   - time trend, post-2008 split-trend (level shift + slope shift)
#   - 4Q-MA of log per-capita income
#   - log(lf_share) — labour-force-to-population ratio (Italy's headline
#     contribution: captures the demographic slowdown of trend income in
#     the early 1990s).
#   - unemployment rate level
#   - log oil, log REER, log stocks (if any are early-enough)
#   - short-run dynamics: Δ4 ln(y), Δ4 unemp_rate
#
# Optional 2008Q3 sigmoid learning-weight (matching AR method default).
#
# Returns model_data with `ln_yp_over_y` and `ln_yp_over_y_post2008`
# overwritten by the LP-derived values, in the same convention as
# `construct_permanent_income`.
# ==============================================================================

construct_permanent_income_italy <- function(model_data, k = 40L,
                                             delta = 0.95,
                                             gfc_ogive = TRUE,
                                             real_time = FALSE) {
  # real_time = FALSE (default): the published Italy convention — ONE
  #   full-sample LP fit, in-sample fitted values used as y^p for every t.
  #   This is non-causal (y^p_t embeds coefficients estimated from data after
  #   t) and is therefore NOT operational at MARTIN forecast time.
  # real_time = TRUE: expanding-window LP. At each t the LP is re-fit using
  #   only training observations whose full k-quarter target is realised by t
  #   (s <= t - k), then t is predicted from its own date-t predictors. This
  #   is causal/real-time and MARTIN-operational, at the cost of ~k+min_train
  #   quarters of warm-up before the first y^p is available.

  delta_q  <- delta^(1 / 4)
  weights  <- delta_q^(seq_len(k) - 1L)
  weights  <- weights / sum(weights)

  dat <- model_data %>% arrange(date)
  n   <- nrow(dat)

  # ---- Build the LP target: weighted average of log income over t+1 .. t+k
  inc <- dat$lincome
  y_p_target <- rep(NA_real_, n)
  for (t in seq_len(n - k)) {
    fut <- inc[(t + 1L):(t + k)]
    if (!anyNA(fut)) y_p_target[t] <- sum(weights * fut)
  }

  # ---- Assemble predictors
  early_enough <- function(col) {
    if (!col %in% names(dat)) return(FALSE)
    vals <- dat[[col]]
    if (all(is.na(vals))) return(FALSE)
    min(dat$date[!is.na(vals)], na.rm = TRUE) < as.Date("2000-01-01")
  }
  has_lf     <- early_enough("lf_share")
  has_unemp  <- early_enough("unemp_rate")
  has_oil    <- early_enough("log_oil")
  has_reer   <- early_enough("log_reer")
  has_stocks <- early_enough("log_stocks")

  dat <- dat %>%
    mutate(
      t_index   = seq_len(n),
      step2008  = as.integer(date >= as.Date("2008-07-01")),
      trend_brk = t_index * step2008,
      inc_ma4   = (lag(lincome, 1L) + lag(lincome, 2L) +
                   lag(lincome, 3L) + lag(lincome, 4L)) / 4,
      d4_lincome = lincome - lag(lincome, 4L)
    )
  if (has_lf)    dat$ln_lf_share <- log(pmax(dat$lf_share, 1e-9))
  if (has_unemp) dat$d4_unemp     <- dat$unemp_rate - lag(dat$unemp_rate, 4L)

  predictor_vars <- c("t_index", "step2008", "trend_brk",
                      "inc_ma4", "d4_lincome")
  if (has_lf)     predictor_vars <- c(predictor_vars, "ln_lf_share")
  if (has_unemp)  predictor_vars <- c(predictor_vars, "unemp_rate", "d4_unemp")
  if (has_oil)    predictor_vars <- c(predictor_vars, "log_oil")
  if (has_reer)   predictor_vars <- c(predictor_vars, "log_reer")
  if (has_stocks) predictor_vars <- c(predictor_vars, "log_stocks")

  message("[construct_permanent_income_italy] LP predictors: ",
          paste(predictor_vars, collapse = ", "))

  # ---- Fit the local projection on the training sample (t where target obs)
  train_mask <- !is.na(y_p_target) &
                complete.cases(dat[, predictor_vars, drop = FALSE])
  n_train <- sum(train_mask)
  if (n_train < 30L) {
    message("[construct_permanent_income_italy] insufficient training obs (",
            n_train, "); leaving ln_yp_over_y unchanged")
    return(model_data)
  }
  min_train <- 30L
  pred_df   <- dat[, predictor_vars, drop = FALSE]
  y_p_hat   <- rep(NA_real_, n)

  # Predict y_p_hat[i] from coefficient vector cf_i and the date-i predictors.
  # NA coefficients (predictors dropped for collinearity in the training
  # window, e.g. step2008 before 2008) contribute zero; a missing predictor
  # value at i yields NA.
  predict_row <- function(cf_i, i) {
    row_i <- as.list(pred_df[i, ])
    val   <- if ("(Intercept)" %in% names(cf_i)) cf_i[["(Intercept)"]] else 0
    for (v in predictor_vars) {
      xi <- row_i[[v]]
      if (is.na(xi)) return(NA_real_)
      bi <- cf_i[[v]]
      if (!is.na(bi)) val <- val + bi * xi
    }
    val
  }

  if (!real_time) {
    # --- Published Italy convention: ONE full-sample fit, fitted for all t.
    fit_df <- dat[train_mask, c(predictor_vars), drop = FALSE]
    fit_df$y_p_target <- y_p_target[train_mask]
    fmla <- reformulate(predictor_vars, response = "y_p_target")
    lp_fit <- lm(fmla, data = fit_df)
    message(sprintf(
      "[construct_permanent_income_italy] full-sample LP fit: n=%d, R^2=%.3f",
      nobs(lp_fit), summary(lp_fit)$r.squared
    ))
    cf <- coef(lp_fit)
    for (i in seq_len(n)) y_p_hat[i] <- predict_row(cf, i)
  } else {
    # --- Real-time expanding-window LP: at each i, train only on observations
    #     whose full k-horizon target is realised by i (s <= i - k).
    n_pred <- 0L
    for (i in seq_len(n)) {
      train_idx <- which(seq_len(n) <= (i - k) & train_mask)
      if (length(train_idx) < min_train) next
      if (anyNA(pred_df[i, ])) next
      fit_df <- dat[train_idx, predictor_vars, drop = FALSE]
      fit_df$y_p_target <- y_p_target[train_idx]
      lp_fit_i <- tryCatch(
        lm(reformulate(predictor_vars, response = "y_p_target"), data = fit_df),
        error = function(e) NULL
      )
      if (is.null(lp_fit_i)) next
      y_p_hat[i] <- predict_row(coef(lp_fit_i), i)
      if (!is.na(y_p_hat[i])) n_pred <- n_pred + 1L
    }
    message(sprintf(
      "[construct_permanent_income_italy] real-time LP: %d of %d quarters predicted (k=%d warm-up)",
      n_pred, n, k
    ))
  }

  ln_yp_over_y <- y_p_hat - dat$lincome

  # ---- Optional GFC ogive learning-weight (match AR method for comparability)
  if (gfc_ogive) {
    gfc_start  <- as.Date("2008-07-01")
    adj_end    <- as.Date("2012-04-01")
    n_adj_qtrs <- 15L
    learning_w <- ifelse(
      dat$date < gfc_start, 1.0,
      ifelse(dat$date >= adj_end, 0.5,
             1.0 - 0.5 * (pmin(
               as.integer(round(as.numeric(difftime(dat$date, gfc_start,
                                                    units = "days")) / 91.25)),
               n_adj_qtrs) / n_adj_qtrs))
    )
    ln_yp_over_y <- ln_yp_over_y * learning_w
  }

  dat$ln_yp_over_y          <- ln_yp_over_y
  dat$ln_yp_over_y_post2008 <- ln_yp_over_y * dat$step2008

  dat <- dat %>%
    select(-t_index, -step2008, -trend_brk, -inc_ma4, -d4_lincome) %>%
    select(-any_of(c("ln_lf_share", "d4_unemp")))

  dat
}


# ==============================================================================
# SECTION A.compare: PI method comparison (AR vs Italy LP)
# ==============================================================================
# Refits the preferred-spec template under both PI methods and writes a
# side-by-side coefficient table. Written to outputs unconditionally so
# the reader can compare regardless of which method was used for the
# canonical estimation.

compare_pi_methods <- function(model_data, preferred_spec_template,
                               output_dir) {
  base <- model_data %>%
    select(-any_of(c("ln_yp_over_y", "ln_yp_over_y_post2008")))

  refit_under <- function(method_name, pi_dat) {
    # ln_hp_over_y comes from the master dataset; do NOT re-derive it here
    # (a previous local re-derivation used a different, deflator-contaminated
    # formula, so this block was estimated on a different regressor).
    pi_dat <- pi_dat %>%
      mutate(ecm_lag = lag(lcons, 1L) - lincome)
    sp <- tryCatch(
      fit_ecm_spec(
        data       = pi_dat,
        spec_name  = paste0(preferred_spec_template$spec_name, "_", method_name),
        lr_vars    = preferred_spec_template$lr_vars,
        sr_vars    = preferred_spec_template$sr_vars,
        dummy_vars = preferred_spec_template$dummy_vars,
        sample_end = max(pi_dat$date)
      ),
      error = function(e) NULL
    )
    if (is.null(sp)) return(NULL)
    cf  <- coef(sp$fit)
    se  <- sqrt(diag(sp$nw_vcov))
    se  <- se[match(names(cf), names(se))]
    tibble::tibble(
      method   = method_name,
      term     = names(cf),
      estimate = unname(cf),
      se       = unname(se),
      n_obs    = nobs(sp$fit),
      r2_adj   = summary(sp$fit)$adj.r.squared
    )
  }

  ar_dat <- tryCatch(construct_permanent_income(base),
                     error = function(e) NULL)
  it_dat <- tryCatch(construct_permanent_income_italy(base),
                     error = function(e) NULL)

  rows <- list()
  if (!is.null(ar_dat))  rows$ar    <- refit_under("ar",    ar_dat)
  if (!is.null(it_dat))  rows$italy <- refit_under("italy", it_dat)
  long <- bind_rows(rows)
  if (nrow(long) == 0L) {
    message("[compare_pi_methods] both refits failed; skipping")
    return(invisible(NULL))
  }

  # Wide format: term, ar_estimate, italy_estimate, ar_se, italy_se
  wide <- long %>%
    pivot_wider(id_cols = term,
                names_from = method,
                values_from = c(estimate, se),
                names_glue = "{method}_{.value}")
  meta <- long %>%
    select(method, n_obs, r2_adj) %>% distinct() %>%
    pivot_wider(names_from = method, values_from = c(n_obs, r2_adj),
                names_glue = "{method}_{.value}")

  out_path <- file.path(output_dir, "australia_pi_method_comparison.csv")
  write.csv(wide, out_path, row.names = FALSE)
  message(sprintf("[compare_pi_methods] Saved: %s", out_path))

  meta_path <- file.path(output_dir, "australia_pi_method_meta.csv")
  write.csv(meta, meta_path, row.names = FALSE)

  cat("\n--- PI method comparison (preferred spec, full sample) ---\n")
  cat(sprintf("  %-30s %12s %12s\n", "term", "AR estimate", "Italy LP est"))
  cat(sprintf("  %s\n", strrep("-", 56)))
  for (i in seq_len(nrow(wide))) {
    cat(sprintf("  %-30s %12.6f %12.6f\n",
      wide$term[i],
      ifelse(is.na(wide$ar_estimate[i]),    0, wide$ar_estimate[i]),
      ifelse(is.na(wide$italy_estimate[i]), 0, wide$italy_estimate[i])))
  }
  invisible(wide)
}


# ==============================================================================
# SECTION A2: Permanent-income sensitivity grid
# ==============================================================================
# Refit the preferred spec across a grid of PI construction choices to expose
# fragility. Adds an HP-filter benchmark if mFilter is installed.
# ==============================================================================

run_pi_sensitivity <- function(model_data, preferred_spec_template,
                               output_dir) {
  # Save the existing PI-derived columns so we can restore between variants
  base_data <- model_data
  base_data$ln_yp_over_y          <- NULL
  base_data$ln_yp_over_y_post2008 <- NULL

  grid <- expand.grid(
    delta = c(0.90, 0.95, 0.97),
    k     = c(20L, 40L, 60L),
    gfc   = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  # ar_order is hard-coded to 8 in construct_permanent_income; expose by
  # toggling: 8 (default) and a simpler 4 via a one-off ad-hoc construction
  # (kept light: skip variants where construction would be redundant).

  rows <- list()
  for (i in seq_len(nrow(grid))) {
    cfg <- grid[i, ]
    pi_dat <- tryCatch(
      construct_permanent_income(base_data, k = cfg$k, delta = cfg$delta,
                                 gfc_ogive = cfg$gfc),
      error = function(e) NULL
    )
    if (is.null(pi_dat)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        variant = sprintf("delta=%.2f_k=%d_gfc=%s", cfg$delta, cfg$k, cfg$gfc),
        delta = cfg$delta, k = cfg$k, gfc_learning = cfg$gfc, ar_order = 8L,
        method = "rolling_ar8",
        lambda = NA_real_,
        struct_ln_yp_over_y = NA_real_,
        struct_main_wealth  = NA_real_
      )
      next
    }
    pi_dat <- pi_dat %>%
      mutate(ecm_lag = lag(lcons, 1L) - lincome)

    # Refit the preferred spec template
    sp <- tryCatch(
      fit_ecm_spec(
        data       = pi_dat,
        spec_name  = preferred_spec_template$spec_name,
        lr_vars    = preferred_spec_template$lr_vars,
        sr_vars    = preferred_spec_template$sr_vars,
        dummy_vars = preferred_spec_template$dummy_vars,
        sample_end = max(pi_dat$date)
      ),
      error = function(e) NULL
    )
    lam <- NA_real_; sa_yp <- NA_real_; sa_w <- NA_real_
    if (!is.null(sp)) {
      cf <- coef(sp$fit)
      lam <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
      if (!is.na(lam) && lam != 0) {
        if ("ln_yp_over_y" %in% names(cf))
          sa_yp <- -cf[["ln_yp_over_y"]] / lam
        wealth_terms <- intersect(c("ha_y", "ln_networth_y", "networth_y"),
                                  names(cf))
        if (length(wealth_terms) > 0L)
          sa_w <- -cf[[wealth_terms[1L]]] / lam
      }
    }
    rows[[length(rows) + 1L]] <- tibble::tibble(
      variant = sprintf("delta=%.2f_k=%d_gfc=%s", cfg$delta, cfg$k, cfg$gfc),
      delta = cfg$delta, k = cfg$k, gfc_learning = cfg$gfc, ar_order = 8L,
      method = "rolling_ar8",
      lambda = lam,
      struct_ln_yp_over_y = sa_yp,
      struct_main_wealth  = sa_w
    )
  }

  # HP-filter benchmark
  if (requireNamespace("mFilter", quietly = TRUE)) {
    hp_dat <- base_data
    li     <- hp_dat$lincome
    valid  <- which(!is.na(li))
    if (length(valid) > 20L) {
      li_v <- li[valid]
      hp_fit <- tryCatch(
        mFilter::hpfilter(li_v, freq = 1600, type = "lambda"),
        error = function(e) NULL
      )
      if (!is.null(hp_fit)) {
        trend <- as.numeric(hp_fit$trend)
        hp_dat$ln_yp_over_y <- NA_real_
        hp_dat$ln_yp_over_y[valid] <- trend - li_v
        hp_dat$ln_yp_over_y_post2008 <- 0
        hp_dat <- hp_dat %>%
          mutate(ecm_lag = lag(lcons, 1L) - lincome)
        sp <- tryCatch(
          fit_ecm_spec(
            data       = hp_dat,
            spec_name  = preferred_spec_template$spec_name,
            lr_vars    = preferred_spec_template$lr_vars,
            sr_vars    = preferred_spec_template$sr_vars,
            dummy_vars = preferred_spec_template$dummy_vars,
            sample_end = max(hp_dat$date)
          ),
          error = function(e) NULL
        )
        lam <- NA_real_; sa_yp <- NA_real_; sa_w <- NA_real_
        if (!is.null(sp)) {
          cf <- coef(sp$fit)
          lam <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
          if (!is.na(lam) && lam != 0) {
            if ("ln_yp_over_y" %in% names(cf))
              sa_yp <- -cf[["ln_yp_over_y"]] / lam
            wealth_terms <- intersect(c("ha_y", "ln_networth_y", "networth_y"),
                                      names(cf))
            if (length(wealth_terms) > 0L)
              sa_w <- -cf[[wealth_terms[1L]]] / lam
          }
        }
        rows[[length(rows) + 1L]] <- tibble::tibble(
          variant = "hp_filter_lambda1600",
          delta = NA_real_, k = NA_integer_, gfc_learning = NA,
          ar_order = NA_integer_, method = "hp_filter",
          lambda = lam,
          struct_ln_yp_over_y = sa_yp,
          struct_main_wealth  = sa_w
        )
      }
    }
  } else {
    message("[run_pi_sensitivity] mFilter not installed — skipping HP benchmark.")
  }

  out <- bind_rows(rows)
  out_path <- file.path(output_dir, "australia_permanent_income_sensitivity.csv")
  write.csv(out, out_path, row.names = FALSE)
  message(sprintf("[run_pi_sensitivity] Saved to %s", out_path))
  out
}


# ==============================================================================
# SECTION B: Income Volatility
# ==============================================================================

compute_income_volatility <- function(model_data) {
  dat <- model_data %>% arrange(date)
  n   <- nrow(dat)
  ar_dat <- dat %>% transmute(
    lincome,
    l1 = lag(lincome, 1L), l2 = lag(lincome, 2L),
    l3 = lag(lincome, 3L), l4 = lag(lincome, 4L),
    l5 = lag(lincome, 5L), l6 = lag(lincome, 6L),
    l7 = lag(lincome, 7L), l8 = lag(lincome, 8L)
  )
  ar_cc <- ar_dat[complete.cases(ar_dat), ]
  if (nrow(ar_cc) < 20L) { dat$abs_income_resid <- NA_real_; return(dat) }
  ar_fit      <- lm(lincome ~ l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8, data = ar_cc)
  resid_vals  <- rep(NA_real_, n)
  cc_idx      <- which(complete.cases(ar_dat))
  resid_vals[cc_idx] <- residuals(ar_fit)
  dat$abs_income_resid <- abs(resid_vals)
  dat
}


# ==============================================================================
# SECTION C: Model Diagnostics
# ==============================================================================

model_diagnostics <- function(fit, data, break_date = "2008-07-01") {
  s  <- summary(fit)
  n  <- nobs(fit)
  dw_val <- tryCatch(as.numeric(lmtest::dwtest(fit)$statistic),
                     error = function(e) NA_real_)
  het_pval <- tryCatch(lmtest::bptest(fit)$p.value,
                       error = function(e) NA_real_)
  ar1_pval <- tryCatch(lmtest::bgtest(fit, order = 1L)$p.value,
                       error = function(e) NA_real_)
  ar4_pval <- tryCatch(lmtest::bgtest(fit, order = 4L)$p.value,
                       error = function(e) NA_real_)
  # Chow stability test at break_date. strucchange::sctest fails (returns NA)
  # whenever a subsample design matrix is singular — e.g. COVID dummies that
  # are all-zero pre-2008 — which previously made 12 of 14 specs "fail" the
  # selector's stability screen by incomputability rather than by evidence.
  # Fallback: a manual Chow F-test on the coefficients estimable in BOTH
  # subsamples (columns with within-subsample variance), labelled as such.
  chow_method <- NA_character_
  bp_idx <- sum(data$date <= as.Date(break_date), na.rm = TRUE)
  chow_pval <- tryCatch({
    if (bp_idx < 5L || bp_idx > (n - 5L)) NA_real_
    else {
      chow_method <- "sctest"
      as.numeric(strucchange::sctest(fit, type = "Chow", point = bp_idx)$p.value)
    }
  }, error = function(e) NA_real_)
  if (is.na(chow_pval) && bp_idx >= 5L && bp_idx <= (n - 5L)) {
    chow_pval <- tryCatch({
      X <- model.matrix(fit)
      y <- model.response(model.frame(fit))
      pre  <- seq_len(bp_idx)
      post <- (bp_idx + 1L):n
      # keep columns with variation in BOTH subsamples (intercept always kept)
      keep <- vapply(seq_len(ncol(X)), function(j) {
        colnames(X)[j] == "(Intercept)" ||
          (stats::sd(X[pre, j]) > 0 && stats::sd(X[post, j]) > 0)
      }, logical(1L))
      Xk <- X[, keep, drop = FALSE]
      k  <- ncol(Xk)
      if (bp_idx <= k + 1L || (n - bp_idx) <= k + 1L) NA_real_
      else {
        ssr_pool <- sum(stats::lsfit(Xk, y, intercept = FALSE)$residuals^2)
        ssr_pre  <- sum(stats::lsfit(Xk[pre, , drop = FALSE],  y[pre],
                                     intercept = FALSE)$residuals^2)
        ssr_post <- sum(stats::lsfit(Xk[post, , drop = FALSE], y[post],
                                     intercept = FALSE)$residuals^2)
        f_stat <- ((ssr_pool - ssr_pre - ssr_post) / k) /
                  ((ssr_pre + ssr_post) / (n - 2L * k))
        chow_method <- "manual_common_coef"
        stats::pf(f_stat, k, n - 2L * k, lower.tail = FALSE)
      }
    }, error = function(e) NA_real_)
  }
  if (is.na(chow_pval)) chow_method <- NA_character_
  reset_pval <- tryCatch(
    lmtest::resettest(fit, power = 2L, type = "fitted")$p.value,
    error = function(e) NA_real_
  )

  # Re-run BP after dropping the four event-quarter rows; if the rejection
  # vanishes, label heteroskedasticity as event-driven rather than structural.
  het_pval_no_events <- NA_real_
  het_diagnosis      <- NA_character_
  event_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound")
  available <- intersect(event_dummies, names(model.frame(fit)))
  if (length(available) > 0L) {
    mf <- model.frame(fit)
    keep_idx <- which(rowSums(as.matrix(mf[, available, drop = FALSE]) != 0) == 0)
    if (length(keep_idx) > (length(coef(fit)) + 5L)) {
      mf_no_evt <- mf[keep_idx, , drop = FALSE]
      het_pval_no_events <- tryCatch({
        fit_no <- lm(formula(fit), data = mf_no_evt)
        lmtest::bptest(fit_no)$p.value
      }, error = function(e) NA_real_)
    }
  }
  if (!is.na(het_pval) && !is.na(het_pval_no_events)) {
    het_diagnosis <- if (het_pval < 0.05 && het_pval_no_events >= 0.05)
      "event_driven" else "structural"
  } else if (!is.na(het_pval)) {
    het_diagnosis <- if (het_pval < 0.05) "structural" else "ok"
  }

  tibble::tibble(
    n_obs = n, se_pct = s$sigma * 100, adj_r2 = s$adj.r.squared,
    dw = dw_val, lm_het_pval = het_pval,
    lm_het_pval_no_events = het_pval_no_events,
    het_diagnosis = het_diagnosis,
    ar1_pval = ar1_pval,
    ar4_pval = ar4_pval, chow_pval = chow_pval, chow_method = chow_method,
    reset_pval = reset_pval, schwarz = BIC(fit), loglik = as.numeric(logLik(fit))
  )
}


# ==============================================================================
# SECTION C2: WLS Variant (only used when het_diagnosis == "structural")
# ==============================================================================

fit_wls_variant <- function(spec) {
  if (!"abs_income_resid" %in% names(spec$est_data)) {
    message("[fit_wls_variant] abs_income_resid not in est_data; returning NULL.")
    return(NULL)
  }
  dat <- spec$est_data
  dat$.wls_weight <- 1 / (1 + dat$abs_income_resid)
  dat$.wls_weight[is.na(dat$.wls_weight)] <- 1
  # Build the formula with the data env as parent so `weights` is found.
  fmla_env <- spec$formula
  environment(fmla_env) <- environment()
  fit_wls <- tryCatch(
    do.call(stats::lm,
            list(formula = fmla_env, data = dat,
                 weights = quote(.wls_weight))),
    error = function(e) {
      message("[fit_wls_variant] lm error: ", e$message)
      NULL
    }
  )
  if (is.null(fit_wls)) return(NULL)

  cf_ols <- coef(spec$fit)
  cf_wls <- coef(fit_wls)
  terms  <- union(names(cf_ols), names(cf_wls))
  out <- tryCatch(
    tibble::tibble(
      term         = terms,
      ols_estimate = unname(cf_ols[terms]),
      wls_estimate = unname(cf_wls[terms]),
      diff         = unname(cf_wls[terms]) - unname(cf_ols[terms])
    ),
    error = function(e) {
      message("[fit_wls_variant] tibble error: ", e$message); NULL
    }
  )
  out
}


# ==============================================================================
# SECTION C3: Break Battery (supF, CUSUM, recursive coefficients)
# ==============================================================================

run_break_battery <- function(spec, output_dir) {
  fit      <- spec$fit
  est_data <- spec$est_data
  sp_name  <- spec$spec_name

  supF_stat <- NA_real_; supF_pval <- NA_real_
  bp_date   <- NA_character_
  fstats_obj <- tryCatch(strucchange::Fstats(formula(fit), data = est_data),
                         error = function(e) NULL)
  if (!is.null(fstats_obj)) {
    sct <- tryCatch(strucchange::sctest(fstats_obj, type = "supF"),
                    error = function(e) NULL)
    if (!is.null(sct)) {
      supF_stat <- as.numeric(sct$statistic)
      supF_pval <- as.numeric(sct$p.value)
    }
    bps <- tryCatch(strucchange::breakpoints(fstats_obj),
                    error = function(e) NULL)
    if (!is.null(bps) && !is.null(bps$breakpoints) &&
        length(bps$breakpoints) > 0L && !any(is.na(bps$breakpoints))) {
      bp_idx <- bps$breakpoints
      if (all(bp_idx <= nrow(est_data))) {
        bp_date <- paste(format(est_data$date[bp_idx], "%Y-%m"),
                         collapse = "; ")
      }
    }
  }

  cusum_pval <- NA_real_
  efp_obj <- tryCatch(
    strucchange::efp(formula(fit), data = est_data, type = "OLS-CUSUM"),
    error = function(e) NULL
  )
  if (!is.null(efp_obj)) {
    cs <- tryCatch(strucchange::sctest(efp_obj),
                   error = function(e) NULL)
    if (!is.null(cs)) cusum_pval <- as.numeric(cs$p.value)
  }

  # Recursive coefficient estimates over expanding windows starting at obs 30
  track_terms <- intersect(c("ecm_lag", "real_rate",
                             "ha_y", "ln_networth_y", "networth_y",
                             "eq_y", "super_y"),
                           names(coef(fit)))
  # Pick the "largest" wealth term: networth/log-networth if present, else ha_y
  wealth_pick <- if ("ln_networth_y" %in% track_terms) "ln_networth_y"
                 else if ("networth_y" %in% track_terms) "networth_y"
                 else if ("ha_y" %in% track_terms) "ha_y"
                 else NULL
  panel_terms <- unique(c("ecm_lag", "real_rate", wealth_pick))
  panel_terms <- intersect(panel_terms, names(coef(fit)))

  rec_rows <- list()
  start_n  <- 30L
  if (nrow(est_data) > start_n + 5L) {
    for (i in seq(start_n, nrow(est_data))) {
      sub_dat <- est_data[seq_len(i), , drop = FALSE]
      sub_fit <- tryCatch(lm(formula(fit), data = sub_dat),
                          error = function(e) NULL)
      if (is.null(sub_fit)) next
      cf <- coef(sub_fit)
      rec_rows[[as.character(i)]] <- tibble::tibble(
        window_end = est_data$date[i],
        n          = i,
        term       = panel_terms,
        estimate   = unname(cf[panel_terms])
      )
    }
  }
  rec_df <- bind_rows(rec_rows)

  if (length(panel_terms) > 0L && nrow(rec_df) > 0L) {
    p <- ggplot(rec_df, aes(window_end, estimate)) +
      geom_line(colour = "steelblue", linewidth = 0.7) +
      geom_hline(yintercept = 0, linetype = 2, colour = "red") +
      facet_wrap(~ term, scales = "free_y") +
      labs(title = paste("Recursive coefficient estimates —", sp_name),
           x = NULL, y = "Estimate") +
      theme_minimal(base_size = 11)
    png_path <- file.path(output_dir,
      sprintf("australia_recursive_coefficients_%s.png", tolower(sp_name)))
    tryCatch(
      ggsave(png_path, p, width = 9, height = 5, dpi = 150),
      error = function(e) message(sprintf(
        "[run_break_battery] Failed to save recursive plot: %s", e$message))
    )
  }

  tibble::tibble(
    specification   = sp_name,
    supF_stat       = supF_stat,
    supF_pval       = supF_pval,
    breakpoint_date = bp_date,
    cusum_pval      = cusum_pval
  )
}


# ==============================================================================
# SECTION D: Short-run and Dummy Variable Construction
# ==============================================================================

add_model_variables <- function(model_data) {
  dat <- model_data %>% arrange(date)

  # Logistic ogive transition: smooth 0->1 transition centred at t0 (in
  # quarter units after rescaling). half_width = 2.5 gives ~5-quarter ramp.
  # Defined locally so this function works on any model_data input,
  # including saved RDS where the function isn't part of the dataset.
  ogive <- function(t, t0, half_width = 2.5) {
    1 / (1 + exp(-(t - t0) / half_width))
  }

  # Backfill NLA cross-equation pieces if loaded from a pre-Task-1 RDS.
  if (!"nla_y_unrestricted" %in% names(dat) &&
      all(c("fin_deposits", "ydi_ann_nom") %in% names(dat))) {
    dat <- dat %>% mutate(nla_y_unrestricted = fin_deposits / ydi_ann_nom)
  }
  if (!"debt_y" %in% names(dat) &&
      all(c("fin_loans", "ydi_ann_nom") %in% names(dat))) {
    dat <- dat %>% mutate(debt_y = fin_loans / ydi_ann_nom)
  }

  # ---- Impulse dummies -------------------------------------------------------
  # GST: large one-off boost to nominal consumption July 2000 (Q3)
  # GFC: sharp drop Q3 2008; Q4 2008/Q1 2009 also affected
  # COVID: lockdown Q2 2020 (massive drop) + Q3 2020 rebound
  # The five Australian-narrative dummies (negative gearing, 1991 recession,
  # APRA macroprudential rounds, JobKeeper) are reconstructed here so they are
  # present even when model_data was loaded from a pre-Task-3 RDS snapshot.
  dat <- dat %>%
    mutate(
      d2000_gst     = as.integer(date == as.Date("2000-07-01")),
      d2008_gfc     = as.integer(date == as.Date("2008-07-01")),
      d2020_covid   = as.integer(date == as.Date("2020-04-01")),
      d2020_rebound = as.integer(date == as.Date("2020-07-01")),
      d_neg_gearing_8587 = as.integer(date >= as.Date("1985-07-01") &
                                       date <= as.Date("1987-07-01")),
      d_recession_1991   = as.integer(date == as.Date("1991-04-01")),
      d_apra_2014 = ogive(as.numeric(date - as.Date("2014-10-01")) / 91.31, 0),
      d_apra_2017 = ogive(as.numeric(date - as.Date("2017-04-01")) / 91.31, 0),
      d_jobkeeper_2020   = as.integer(date >= as.Date("2020-04-01") &
                                       date <= as.Date("2021-01-01"))
    )

  # ---- Log CCI (credit conditions proxy) ------------------------------------
  # Use log of housing loan flow as credit conditions indicator.
  # If cci_ratio already present use it directly; otherwise construct from
  # log(housing_loan_flow).
  if ("cci_ratio" %in% names(dat)) {
    dat <- dat %>% mutate(log_cci = cci_ratio)
  } else if ("housing_loan_flow" %in% names(dat)) {
    dat <- dat %>% mutate(log_cci = log(housing_loan_flow))
  } else {
    dat <- dat %>% mutate(log_cci = NA_real_)
  }

  # Δ² log CCI lagged 2
  dat <- dat %>%
    mutate(
      d2_logcci      = log_cci - 2 * lag(log_cci, 1L) + lag(log_cci, 2L),
      d2_logcci_lag2 = lag(d2_logcci, 2L)
    ) %>%
    select(-d2_logcci)

  # ---- Longer-history short-run CCI based on RBA D02 total credit --------
  # cci_ratio above is bounded at 2002Q3+ by ABS 5601.0 housing loan flow,
  # which truncates Spec 6 to n = 86 on the headline sample. RBA D02 total
  # credit (`credit_total_d02`) is available from 1976Q3 and is a defensible
  # alternative short-run credit-conditions proxy: its second difference of
  # logs captures the same "acceleration in credit availability" dynamic
  # without the housing-flow series start-date binding constraint. We
  # construct it in parallel so Spec 6 keeps its housing-flow CCI as the
  # canonical short-run regressor and Spec 6b swaps it for the longer-
  # history alternative.
  if ("credit_total_d02" %in% names(dat)) {
    dat <- dat %>%
      mutate(
        log_creditd02_safe       = log(pmax(credit_total_d02, 1e-9)),
        d2_log_creditd02         = log_creditd02_safe -
                                   2 * lag(log_creditd02_safe, 1L) +
                                   lag(log_creditd02_safe, 2L),
        d2_log_creditd02_lag2    = lag(d2_log_creditd02, 2L)
      ) %>%
      select(-log_creditd02_safe, -d2_log_creditd02)
  } else {
    dat <- dat %>% mutate(d2_log_creditd02_lag2 = NA_real_)
  }

  # ---- ΔΔ₄ income: diff(Δ₁ income, 4) -------------------------------------
  dat <- dat %>%
    mutate(
      dd4_income = (lincome - lag(lincome, 1L)) -
                   (lag(lincome, 4L) - lag(lincome, 5L))
    )

  # ---- Δ² log unemployment --------------------------------------------------
  dat <- dat %>%
    mutate(
      d2_log_unemp = c(NA_real_, NA_real_, diff(diff(log(unemp_rate))))
    )

  # ---- Lagged quarterly consumption growth (lag 4) ---------------------------
  # Captures habit formation / AR4 persistence. Analogous to the Δ² ln c^P
  # persistence term in Italy. Added to sr_vars of dynamic specs.
  dat <- dat %>%
    mutate(dlcons_lag4 = lag(dlcons, 4L))

  # ---- Pre-Superannuation Guarantee dummy ------------------------------------
  # SG became mandatory July 1992. super_y data before this is unreliable
  # (voluntary contributions only). Flag the 15 pre-SG quarters.
  dat <- dat %>%
    mutate(d_pre_sg = as.integer(date < as.Date("1992-07-01")))

  dat
}


# ==============================================================================
# SECTION E: fit_ecm_spec — fit one specification with NW SEs
# ==============================================================================

fit_ecm_spec <- function(data, spec_name, lr_vars, sr_vars = character(0),
                         dummy_vars = c("d2000_gst", "d2008_gfc",
                                        "d2020_covid", "d2020_rebound",
                                        "d_neg_gearing_8587", "d_recession_1991",
                                        "d_apra_2014", "d_apra_2017",
                                        "d_jobkeeper_2020"),
                         response     = "dlcons",
                         sample_start = as.Date("1980-01-01"),
                         sample_end   = as.Date("2024-10-01")) {

  # Drop sr_vars that are entirely NA in the data
  sr_vars_ok <- sr_vars[vapply(sr_vars, function(v) {
    v %in% names(data) && !all(is.na(data[[v]]))
  }, logical(1))]
  if (length(sr_vars_ok) < length(sr_vars))
    message(sprintf("  [%s] Dropping all-NA sr_vars: %s", spec_name,
                    paste(setdiff(sr_vars, sr_vars_ok), collapse = ", ")))

  all_rhs <- c(lr_vars, sr_vars_ok, dummy_vars)
  req     <- c(response, all_rhs)

  est_data <- data %>%
    filter(date >= sample_start, date <= sample_end) %>%
    filter(complete.cases(across(all_of(req))))

  if (nrow(est_data) < 30L)
    stop(sprintf("[fit_ecm_spec] Spec '%s': only %d complete observations.",
                 spec_name, nrow(est_data)))

  # Drop zero-variance dummies (pre-sample events)
  dummy_vars_ok <- dummy_vars[vapply(dummy_vars, function(v) {
    v %in% names(est_data) && var(est_data[[v]], na.rm = TRUE) > 0
  }, logical(1))]
  if (length(dummy_vars_ok) < length(dummy_vars))
    message(sprintf("  [%s] Dropping zero-variance dummies: %s", spec_name,
                    paste(setdiff(dummy_vars, dummy_vars_ok), collapse = ", ")))

  all_rhs_ok <- c(lr_vars, sr_vars_ok, dummy_vars_ok)
  fmla <- reformulate(all_rhs_ok, response = response)
  fit  <- lm(fmla, data = est_data)

  # Drop remaining collinear terms
  aliased_terms <- setdiff(names(which(is.na(coef(fit)))), "(Intercept)")
  if (length(aliased_terms) > 0L) {
    message(sprintf("  [%s] Dropping collinear terms: %s", spec_name,
                    paste(aliased_terms, collapse = ", ")))
    all_rhs_ok <- setdiff(all_rhs_ok, aliased_terms)
    fmla <- reformulate(all_rhs_ok, response = response)
    fit  <- lm(fmla, data = est_data)
  }

  # Newey-West HAC covariance
  bw      <- floor(4 * (nobs(fit) / 100)^(2 / 9))
  nw_vcov <- tryCatch(
    sandwich::NeweyWest(fit, lag = bw, prewhite = FALSE, adjust = TRUE),
    error = function(e) vcov(fit)
  )

  list(
    spec_name  = spec_name,
    lr_vars    = lr_vars,
    sr_vars    = sr_vars,
    dummy_vars = dummy_vars,
    formula    = fmla,
    fit        = fit,
    nw_vcov    = nw_vcov,
    est_data   = est_data
  )
}


# ==============================================================================
# SECTION F0: Cointegration Battery
# ==============================================================================
# For each of the six ECM specs, fit the static cointegrating regression of
# lcons on the long-run regressors (excluding ecm_lag) and test whether the
# residuals are I(0). Reports ADF (drift) plus optional Phillips-Ouliaris and
# Johansen (urca). Falls back to NA if urca is missing.
# ==============================================================================

run_cointegration_battery <- function(model_data, specs, output_dir,
                                       sample_end = as.Date("2024-10-01")) {
  has_urca <- requireNamespace("urca", quietly = TRUE)
  if (!has_urca) {
    message("[run_cointegration_battery] urca not available — ADF only.")
  }

  rows <- list()
  for (sp_name in names(specs)) {
    sp <- specs[[sp_name]]
    lr_no_ecm <- setdiff(sp$lr_vars, "ecm_lag")
    req <- c("lcons", lr_no_ecm)
    # Number of variables in the cointegrating regression (dependent + the k
    # long-run regressors). Drives the Engle–Granger MacKinnon critical value
    # below — a residual-based test, NOT a univariate Dickey–Fuller test.
    n_lr_vars <- length(lr_no_ecm) + 1L
    # Specs 10/12 impose part of the long run through a calibrated OFFSET;
    # their free lr_vars are not their long-run relation, so a static EG
    # regression on those columns would test the wrong object. Skip with an
    # explicit note rather than silently.
    if (sp_name %in% c("spec10", "spec12")) {
      rows[[sp_name]] <- tibble::tibble(
        specification = sp$spec_name,
        period = NA_character_, n_obs = NA_integer_, coint_n_vars = n_lr_vars,
        coint_adf_stat = NA_real_, coint_adf_5pct_cv = NA_real_,
        coint_adf_pass = NA, po_stat = NA_real_, po_pass = NA,
        johansen_r1_pass = NA,
        note = "calibrated-offset long run; static EG regression not applicable"
      )
      next
    }
    # Skip cointegration if any required regressor is missing from
    # model_data. (The Spec 8/11 interaction columns are now attached to the
    # global model_data at Step 4a-ii, so the headline spec IS screened; a
    # previous version skipped Spec 11 here as an implementation accident.)
    missing <- setdiff(req, names(model_data))
    if (length(missing) > 0L) {
      message(sprintf("[run_cointegration_battery] %s: skipping (missing %s)",
                      sp$spec_name, paste(missing, collapse = ", ")))
      rows[[sp_name]] <- tibble::tibble(
        specification = sp$spec_name,
        period = NA_character_, n_obs = NA_integer_, coint_n_vars = n_lr_vars,
        coint_adf_stat = NA_real_, coint_adf_5pct_cv = NA_real_,
        coint_adf_pass = NA, po_stat = NA_real_, po_pass = NA,
        johansen_r1_pass = NA,
        note = sprintf("skipped: missing %s", paste(missing, collapse = ", "))
      )
      next
    }

    coint_data <- model_data %>%
      filter(date <= sample_end) %>%
      filter(complete.cases(across(all_of(req))))

    n <- nrow(coint_data)
    if (n < 30L) {
      rows[[sp_name]] <- tibble::tibble(
        specification = sp$spec_name,
        period = format(range(coint_data$date), "%Y-%m"),
        n_obs = n, coint_n_vars = n_lr_vars,
        coint_adf_stat = NA_real_, coint_adf_5pct_cv = NA_real_,
        coint_adf_pass = NA, po_stat = NA_real_, po_pass = NA,
        johansen_r1_pass = NA,
        note = "skipped: fewer than 30 complete observations"
      )
      next
    }

    fmla   <- reformulate(lr_no_ecm, response = "lcons")
    static <- lm(fmla, data = coint_data)
    res    <- residuals(static)

    adf <- tryCatch(run_adf_drift(res, lags = 4L),
                    error = function(e) NULL)
    if (is.null(adf)) {
      adf_stat <- NA_real_; adf_cv <- NA_real_; adf_pass <- NA
    } else {
      adf_stat <- adf$adf_stat
      # Engle–Granger residual test: compare the τ statistic to the MacKinnon
      # critical value for n_lr_vars variables, NOT the univariate DF value
      # (adf$adf_5pct ≈ −2.86) returned by run_adf_drift.
      adf_cv   <- eg_mackinnon_cv(n_lr_vars, "5pct")
      adf_pass <- !is.na(adf_stat) && !is.na(adf_cv) && adf_stat < adf_cv
    }

    po_stat <- NA_real_; po_pass <- NA
    if (has_urca) {
      po_fit <- tryCatch(
        urca::ca.po(as.matrix(coint_data[, c("lcons", lr_no_ecm)]),
                    demean = "constant", type = "Pu"),
        error = function(e) NULL
      )
      if (!is.null(po_fit)) {
        po_stat <- as.numeric(po_fit@teststat)
        cvs     <- po_fit@cval
        cv5     <- if (!is.null(cvs) && "5pct" %in% colnames(cvs)) cvs[1L, "5pct"] else NA_real_
        po_pass <- !is.na(po_stat) && !is.na(cv5) && po_stat > cv5
      }
    }

    johansen_pass <- NA
    if (has_urca) {
      jvars <- c("lcons", "lincome",
                 if (sp_name %in% c("spec1", "spec2", "spec3")) "ln_networth_y" else "ha_y")
      jvars <- intersect(jvars, names(model_data))
      if (length(jvars) >= 3L) {
        jdat <- model_data %>%
          filter(date <= sample_end) %>%
          filter(complete.cases(across(all_of(jvars)))) %>%
          select(all_of(jvars))
        if (nrow(jdat) > 30L) {
          johansen_fit <- tryCatch(
            urca::ca.jo(as.matrix(jdat), type = "trace", ecdet = "const",
                        K = 2L, spec = "transitory"),
            error = function(e) NULL
          )
          if (!is.null(johansen_fit)) {
            tr <- johansen_fit@teststat
            cv <- johansen_fit@cval
            # H0: r=0; rejection at 5% → at least one cointegrating vector
            johansen_pass <- !is.null(tr) && !is.null(cv) &&
              length(tr) >= 1L && tr[length(tr)] > cv[length(tr), "5pct"]
          }
        }
      }
    }

    rows[[sp_name]] <- tibble::tibble(
      specification     = sp$spec_name,
      period            = sprintf("%s to %s",
                                  format(min(coint_data$date), "%Y-%m"),
                                  format(max(coint_data$date), "%Y-%m")),
      n_obs             = n,
      coint_n_vars      = n_lr_vars,
      coint_adf_stat    = adf_stat,
      coint_adf_5pct_cv = adf_cv,
      coint_adf_pass    = adf_pass,
      po_stat           = po_stat,
      po_pass           = po_pass,
      johansen_r1_pass  = johansen_pass,
      note              = ""
    )
  }

  out <- bind_rows(rows)
  out_path <- file.path(output_dir, "australia_cointegration.csv")
  write.csv(out, out_path, row.names = FALSE)
  message(sprintf("[run_cointegration_battery] Saved to %s", out_path))
  out
}


# ==============================================================================
# SECTION F: Six Estimation Specifications
# ==============================================================================
#
# Response:  dlcons = Δ ln(real per capita consumption)
# ECM term:  ecm_lag = ln(c_{t-1}) - ln(y_t)  (negative-restoration convention)
# Dummies:   GST 2000Q3, GFC 2008Q3, COVID 2020Q2, COVID rebound 2020Q3
#
# Wealth variables (Australian balance sheet decomposition):
#   ha_y    = housing assets / annual income
#   ilfa_y  = illiquid financial assets (equities + super) / annual income
#   nla_y   = net liquid assets (deposits - loans) / annual income
#   networth_y = total net worth / annual income
#   ln_hp_over_y = log(real house price / real income per capita)
#   ln_networth_y = log(networth_y)
# ==============================================================================

# ==============================================================================
# fit_williams_prior_spec — Spec 10 — Williams-prior calibrated estimation
# ==============================================================================
#
# Imposes Williams' (Aust system paper Table 1, BIS chapter) calibrated
# coefficients on three terms:
#   gamma_IFA  = 0.022   illiquid financial wealth MPC (eq + super)
#   psi(CCI)   = psi_0 + psi_1 * CCI  with psi_0 = 0.20, psi_1 = 0.93
#   varpi      = 1.2     log(HP/y) interaction multiplier
#
# In the OLS form Δlog c = sum_i β_i x_i + ε, the imposed restrictions
# become non-linear in OLS-coefficient space because β_i = λ * γ_i where
# γ_i are calibrated and λ is unknown. Solve via iterative fixed-point
# OLS: at each iteration, subtract λ * (calibrated regressor sum) from
# the dependent variable, OLS-regress the adjusted dependent on the
# remaining free regressors, read off the new λ from the ecm_lag
# coefficient, repeat until |λ - λ_old| < tol.
#
# Inputs:
#   model_data  the master tibble with cci_williams already attached
#   sample_end  as.Date("2024-10-01") by default
#   gamma_ifa, psi_0, psi_1, varpi  Williams' calibrated values
#   cci_col     "cci_williams" (default; the maximal-GETS canonical) or
#               other available CCI column
#
# Output:
#   Same structure as fit_ecm_spec(): list with $fit, $nw_vcov, $est_data,
#   etc., plus attributes recording the calibrated coefficients,
#   converged lambda, and iteration count.

fit_williams_prior_spec <- function(model_data,
                                     sample_end = as.Date("2024-10-01"),
                                     gamma_ifa = 0.022,
                                     psi_0     = 0.20,
                                     psi_1     = 0.93,
                                     varpi     = 1.2,
                                     cci_col   = "cci_williams",
                                     max_iter  = 50L,
                                     tol       = 1e-6,
                                     dummy_vars = NULL) {
  if (!cci_col %in% names(model_data) || all(is.na(model_data[[cci_col]]))) {
    return(NULL)
  }
  cci <- model_data[[cci_col]]

  d <- model_data %>% mutate(
    ifa_y_internal     = eq_y + super_y,
    hp_x_1_minus_cci_w = ln_hp_over_y * (1 - varpi * cci),
    r_x_cci_w          = real_rate    * cci,
    yp_x_cci_w         = ln_yp_over_y * cci
  )
  base_dummies <- if (!is.null(dummy_vars)) dummy_vars else
    c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
      "d_neg_gearing_8587", "d_recession_1991",
      "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
  free_lr <- c("ha_y", "nla_y", "hp_x_1_minus_cci_w", "r_x_cci_w", "ecm_lag")
  free_sr <- c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")

  # offset(t) = gamma_IFA * IFA + psi_0 * yp + psi_1 * yp*CCI
  offset_calib <- gamma_ifa * d$ifa_y_internal +
                  psi_0     * d$ln_yp_over_y +
                  psi_1     * d$yp_x_cci_w

  fit_at_lambda <- function(lambda_guess) {
    d_local <- d
    # Convention (header :12-19, build_results_table :2325): the structural
    # ECM is dlcons = lambda*(ecm_lag - offset_calib) + free terms + e, i.e.
    # dlcons = lambda*ecm_lag - lambda*offset_calib + free terms + e. Moving
    # the calibrated part to the LHS to isolate lambda*ecm_lag on the RHS
    # gives dlcons_adj = dlcons + lambda*offset_calib = lambda*ecm_lag + ...,
    # so the offset must be ADDED back, not subtracted.
    d_local$dlcons_adj <- d_local$dlcons + lambda_guess * offset_calib
    fit_ecm_spec(d_local, "Spec10_WilliamsPrior",
                 lr_vars    = free_lr,
                 sr_vars    = free_sr,
                 dummy_vars = base_dummies,
                 response   = "dlcons_adj",
                 sample_end = sample_end)
  }

  lambda_guess  <- -0.12  # initial value from Spec 8
  iters_used    <- 0L
  converged     <- FALSE
  diverged      <- FALSE
  for (i in seq_len(max_iter)) {
    spec <- fit_at_lambda(lambda_guess)
    cf <- coef(spec$fit)
    new_lambda <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
    if (is.na(new_lambda)) break
    if (abs(new_lambda) > 5 || !is.finite(new_lambda)) {
      diverged <- TRUE
      break
    }
    if (abs(new_lambda - lambda_guess) < tol) {
      iters_used <- i
      converged  <- TRUE
      break
    }
    lambda_guess <- new_lambda
    iters_used   <- i
  }
  if (diverged) {
    message(sprintf(
      "[fit_williams_prior_spec] diverged (|lambda| > 5 at iter %d); returning NULL",
      iters_used))
    return(NULL)
  }

  spec$spec_name              <- "Spec10_WilliamsPrior"
  attr(spec, "calibrated")    <- c(gamma_ifa = gamma_ifa,
                                    psi_0 = psi_0, psi_1 = psi_1,
                                    varpi = varpi)
  attr(spec, "iterations")    <- iters_used
  attr(spec, "converged")     <- converged
  attr(spec, "lambda_solved") <- lambda_guess
  attr(spec, "cci_used")      <- cci_col
  message(sprintf(
    "[fit_williams_prior_spec] %s in %d iterations: lambda = %.4f",
    if (converged) "converged" else "stopped (max_iter)",
    iters_used, lambda_guess))
  spec
}


# ==============================================================================
# fit_lives_calibrated_spec — Spec 12 — Calibrated LIVES headline
# ==============================================================================
# The faithful Muellbauer/Williams Eq 7 consumption equation with Williams'
# Table 1 (Aust system paper p.29) credit-channel coefficients IMPOSED, leaving
# free ONLY the parameters Australian post-1988 data can identify:
#   gamma_1   housing-collateral MPC  (loading on the de-meaned CCI*(HA/4y) term)
#   gamma_NLA net-liquid-asset MPC
#   lambda    speed of adjustment
#
# Motivation. A collinearity diagnostic on Spec 11 shows the six CCI-interaction
# regressors are 0.74-0.97 mutually correlated (each ~ proportional to CCI), so
# they cannot be jointly free-estimated off a single equation — which is exactly
# why Williams calibrates psi/varpi and estimates the system by FIML. Imposing
# his calibrations removes the unidentified free interactions; the companion
# paper's Wald test does not reject them (chi2(6) = 2.24, p = 0.90). With every
# CCI-scaled channel calibrated, the one credit channel left free (the housing-
# collateral term ha_x_cci) is identified against nla_y, a plain wealth ratio
# with low collinearity.
#
# This differs from Spec 10 (fit_williams_prior_spec) in three faithful-structure
# fixes: (i) housing enters ONLY through the credit-scaled interaction (free
# gamma_1), NOT a classical free ha_y level — Williams' housing MPC is zero at
# CCI=0; (ii) the autonomous-consumption intercept zeta_c*CCI is restored
# (calibrated, since it is 0.88-collinear with ha_x_cci); (iii) the real-rate and
# affordability interactions are calibrated rather than freely estimated (Spec 10
# leaves them free, reintroducing collinearity).
#
# Calibrations imposed (Williams Aust Table 1, col 1, p.29) — the scale-robust,
# theory-anchored subset:
#   gamma_IFA = 0.022   illiquid financial wealth MPC (eq + super, level ratio)
#   psi_0 = 0.20, psi_1 = 0.93   permanent-income gearing psi(CCI)=psi_0+psi_1*CCI
# These three (the exact set Spec 10 imposes) are robust to CCI scaling because
# they load on income/wealth RATIOS, not on the rate or price levels.
#
# DELIBERATELY OMITTED: Williams' real-rate (alpha_r=-0.8711), affordability
# (alpha_hp=-0.1298) and autonomous-consumption (zeta_c=0.1902) CCI channels are
# NOT imposed, for two reasons. (1) Scale: repo `real_rate` is in percent and
# `cci_williams` is peak-normalised to 1, whereas Williams' real rate and CCI
# (peak ~0.805) are on different scales, so his raw alpha_r makes the calibrated
# offset ~30x the size of Delta log c and the fixed-point iteration DIVERGES
# (confirmed empirically). (2) Identification: these three are 0.74-0.97 collinear
# with the free housing-collateral term ha_x_cci, so freely estimating them is the
# very collinearity this spec exists to avoid. Re-introducing them properly needs a
# scale-matched recalibration or the FIML route (next step). The free intercept
# absorbs the constant part of the omitted autonomous-consumption channel.
#
# Solved by the same iterative fixed-point OLS as Spec 10: subtract
# lambda * (calibrated long-run sum) from the dependent variable and re-read lambda
# from ecm_lag until convergence.
fit_lives_calibrated_spec <- function(model_data,
                                      sample_end = as.Date("2024-10-01"),
                                      gamma_ifa = 0.022,
                                      psi_0     = 0.20,
                                      psi_1     = 0.93,
                                      cci_col   = "cci_williams",
                                      max_iter  = 50L,
                                      tol       = 1e-6,
                                      dummy_vars = NULL) {
  if (!cci_col %in% names(model_data) || all(is.na(model_data[[cci_col]]))) {
    return(NULL)
  }
  cci <- model_data[[cci_col]]

  # De-meaning window for the free housing interaction: same as Spec 8.
  mask <- !is.na(cci) &
          model_data$date >= as.Date("1980-01-01") &
          model_data$date <= sample_end
  ha_mean <- mean(model_data$ha_y[mask], na.rm = TRUE)

  d <- model_data %>% mutate(
    ilfa_y_internal = eq_y + super_y,
    yp_x_cci_w      = ln_yp_over_y * cci,                 # offset (matches Spec 10)
    ha_x_cci        = (ha_y - ha_mean) * cci              # FREE: gamma_1 (de-meaned)
  )
  base_dummies <- if (!is.null(dummy_vars)) dummy_vars else
    c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
      "d_neg_gearing_8587", "d_recession_1991",
      "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
  free_lr <- c("ha_x_cci", "nla_y", "ecm_lag")
  free_sr <- c("dd4_income", "d2_log_unemp", "abs_income_resid")

  # Calibrated long-run target (structural, inside-bracket coefficients);
  # scale-robust subset only — see header.
  offset_calib <- gamma_ifa * d$ilfa_y_internal +
                  psi_0     * d$ln_yp_over_y +
                  psi_1     * d$yp_x_cci_w

  fit_at_lambda <- function(lambda_guess) {
    d_local <- d
    # Same sign convention as Spec 10 (see comment there): dlcons_adj must
    # ADD lambda*offset_calib, not subtract it, so that the fixed-point
    # regression of dlcons_adj on ecm_lag recovers lambda under
    # beta_OLS = -lambda*gamma (lambda < 0).
    d_local$dlcons_adj <- d_local$dlcons + lambda_guess * offset_calib
    fit_ecm_spec(d_local, "Spec12_LIVES_Calibrated",
                 lr_vars    = free_lr,
                 sr_vars    = free_sr,
                 dummy_vars = base_dummies,
                 response   = "dlcons_adj",
                 sample_end = sample_end)
  }

  lambda_guess <- -0.20  # near the Spec 11 / pre-COVID solution
  iters_used   <- 0L
  converged    <- FALSE
  diverged     <- FALSE
  for (i in seq_len(max_iter)) {
    spec <- fit_at_lambda(lambda_guess)
    cf <- coef(spec$fit)
    new_lambda <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
    if (is.na(new_lambda)) break
    if (abs(new_lambda) > 5 || !is.finite(new_lambda)) { diverged <- TRUE; break }
    if (abs(new_lambda - lambda_guess) < tol) {
      iters_used <- i; converged <- TRUE; break
    }
    lambda_guess <- new_lambda
    iters_used   <- i
  }
  if (diverged) {
    message(sprintf(
      "[fit_lives_calibrated_spec] diverged (|lambda| > 5 at iter %d); returning NULL",
      iters_used))
    return(NULL)
  }
  spec$spec_name              <- "Spec12_LIVES_Calibrated"
  attr(spec, "calibrated")    <- c(gamma_ifa = gamma_ifa, psi_0 = psi_0,
                                   psi_1 = psi_1)
  attr(spec, "iterations")    <- iters_used
  attr(spec, "converged")     <- converged
  attr(spec, "lambda_solved") <- lambda_guess
  attr(spec, "cci_used")      <- cci_col
  message(sprintf(
    "[fit_lives_calibrated_spec] %s in %d iterations: lambda = %.4f",
    if (converged) "converged" else "stopped (max_iter)",
    iters_used, lambda_guess))
  spec
}


run_all_specifications <- function(model_data, sample_end = as.Date("2024-10-01"),
                                   dummy_vars = NULL) {

  # dummy_vars override lets the COVID-rich variant run ALL fourteen specs
  # with quarterly 2020-21 dummies (a previous run_all_specifications_with_dummies
  # duplicate covered only Specs 1-6, so the WP's "sign-stable across all four
  # sample variants" claim had no committed evidence for Specs 7-12).
  base_dummies <- if (!is.null(dummy_vars)) dummy_vars else
    c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
      "d_neg_gearing_8587", "d_recession_1991",
      "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

  # ------------------------------------------------------------------
  # Spec 1: Log net worth (conventional baseline)
  # ------------------------------------------------------------------
  # ln_hp_over_y appears in EVERY long-run spec: Italy paper Section 2 p.7
  # treats it as a load-bearing baseline regressor capturing the affordability /
  # down-payment channel that partially offsets the housing wealth effect.
  spec1 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec1_LogNetWorth",
    lr_vars    = c("ln_networth_y", "ln_hp_over_y", "real_rate",
                   "ln_yp_over_y", "ecm_lag"),
    sr_vars    = character(0),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 2: Log net worth + credit conditions (Δ² log CCI lagged 2)
  # ------------------------------------------------------------------
  spec2 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec2_LogNetWorth_CCI",
    lr_vars    = c("ln_networth_y", "ln_hp_over_y", "real_rate",
                   "ln_yp_over_y", "ecm_lag"),
    sr_vars    = "d2_logcci_lag2",
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 3: Level net worth
  # ------------------------------------------------------------------
  spec3 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec3_LevelNetWorth",
    lr_vars    = c("networth_y", "ln_hp_over_y", "real_rate",
                   "ln_yp_over_y", "ecm_lag"),
    sr_vars    = character(0),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 4: Disaggregated wealth — NLA + equities + super + Housing
  # Super is split from equities: super is illiquid/mandatory, equities more liquid.
  # ------------------------------------------------------------------
  spec4 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec4_Disagg_NoCCI",
    lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ecm_lag"),
    sr_vars    = character(0),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 5: Full disaggregation + short-run dynamics
  # ------------------------------------------------------------------
  spec5 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec5_FullDisagg",
    lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ecm_lag"),
    sr_vars    = c("d2_logcci_lag2", "dd4_income",
                   "d2_log_unemp", "abs_income_resid"),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 6: Preferred — full disaggregation + GFC shift in φ
  # ------------------------------------------------------------------
  spec6 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec6_Preferred",
    lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                   "ecm_lag"),
    sr_vars    = c("d2_logcci_lag2", "dd4_income",
                   "d2_log_unemp", "abs_income_resid"),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 6b: Spec 6 with the back-extension-compatible short-run CCI.
  # Replaces d2_logcci_lag2 (bounded at 2002Q3+ by ABS 5601.0 housing loan
  # flow) with d2_log_creditd02_lag2 (Δ²log of RBA D02 total credit, back
  # to 1976Q3). Lets Spec 6 fit on n = 190 (1976Q3–2024Q4) instead of
  # n = 86 (2002Q3+). Comparison with Spec 6 is the back-extension
  # robustness column for the preferred specification.
  # ------------------------------------------------------------------
  if ("d2_log_creditd02_lag2" %in% names(model_data) &&
      any(!is.na(model_data$d2_log_creditd02_lag2))) {
    spec6b <- fit_ecm_spec(
      data       = model_data,
      spec_name  = "Spec6b_LongHistSRCCI",
      lr_vars    = c("nla_y_proxy", "eq_y_proxy", "super_y_proxy",
                     "ha_y_proxy", "ln_hp_over_y",
                     "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                     "ecm_lag"),
      sr_vars    = c("d2_log_creditd02_lag2", "dd4_income",
                     "d2_log_unemp", "abs_income_resid"),
      dummy_vars = base_dummies,
      sample_end = sample_end
    )
  } else {
    spec6b <- NULL
    message("Spec 6b skipped: d2_log_creditd02_lag2 not available")
  }

  # ------------------------------------------------------------------
  # Spec 7: Spec 6 + life-cycle (prime_age_share) and credit-access
  # (fhb_share) terms. mortgage_burden was originally also included but
  # was insignificant (p ~ 0.34) and correlated -0.42 with nla_y (both
  # proxy debt-service capacity); dropping it removes a collinearity
  # without losing economic content (the debt-service channel is already
  # captured in NLA = liquid - total debt).
  #
  # Substantive finding: prime_age_share has a large coefficient (~+5)
  # that absorbs wealth-effect variance in the post-1988 sample. As a
  # result nla_y can carry the wrong sign in Spec 7 even though it is
  # correctly positive in Spec 6. Demographics-vs-wealth collinearity
  # over the post-deregulation period is real and not a fixable bug.
  # ------------------------------------------------------------------
  spec7 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec7_CohortBurden",
    lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                   "prime_age_share", "fhb_share",
                   "ecm_lag"),
    sr_vars    = c("d2_logcci_lag2", "dd4_income",
                   "d2_log_unemp", "abs_income_resid"),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  # ------------------------------------------------------------------
  # Spec 7b: same as Spec 7 but uses the RBA E13 measured payment-burden
  # ratio (mortgage_payment_burden_rba) instead of the synthetic
  # mortgage_burden series. Sample is automatically truncated to 2009Q1+
  # because RBA E13 is unavailable before then. This is the
  # measurement-quality robustness column for the WP §8.
  # The synthetic mortgage_burden has correlation 0.93 with the RBA
  # measured series over the 2009+ overlap but is biased ~30% high in
  # level (synthetic uses total household debt × headline SVR; RBA uses
  # housing-only interest at effective rates).
  # ------------------------------------------------------------------
  if ("mortgage_payment_burden_rba" %in% names(model_data) &&
      any(!is.na(model_data$mortgage_payment_burden_rba))) {
    spec7b <- fit_ecm_spec(
      data       = model_data,
      spec_name  = "Spec7b_RBABurden",
      lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                     "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                     "prime_age_share", "fhb_share",
                     "mortgage_payment_burden_rba",
                     "ecm_lag"),
      sr_vars    = c("d2_logcci_lag2", "dd4_income",
                     "d2_log_unemp", "abs_income_resid"),
      dummy_vars = base_dummies,
      sample_end = sample_end
    )
  } else {
    spec7b <- NULL
    message("Spec 7b skipped: mortgage_payment_burden_rba not available")
  }

  # ------------------------------------------------------------------
  # Spec 8: Williams CCI + interaction terms (Aust paper eq 7).
  # Requires cci_williams in model_data (built by the iterative Williams
  # CCI fit step in MAIN execution). Returns NULL otherwise so downstream
  # filters can drop it gracefully.
  # ------------------------------------------------------------------
  if ("cci_williams" %in% names(model_data) &&
      any(!is.na(model_data$cci_williams))) {
    # Williams Aust §5.1: CCI interacts with DE-MEANED key variables.
    # Without de-meaning, each X·CCI interaction is collinear with an
    # implicit linear CCI level term that Spec 8 does not include
    # separately, biasing the interaction coefficient toward absorbing a
    # constant CCI shift (Aiken-West 1991). The means are computed on
    # the Spec 8 estimation window — the cases where cci_williams is
    # observed within the sample bounds — so the interactions have
    # mean zero at X = X̄ in-sample.
    spec8_mask <- !is.na(model_data$cci_williams) &
                  model_data$date >= as.Date("1980-01-01") &
                  model_data$date <= sample_end
    ha_mean <- mean(model_data$ha_y[spec8_mask],         na.rm = TRUE)
    hp_mean <- mean(model_data$ln_hp_over_y[spec8_mask], na.rm = TRUE)
    r_mean  <- mean(model_data$real_rate[spec8_mask],    na.rm = TRUE)
    yp_mean <- mean(model_data$ln_yp_over_y[spec8_mask], na.rm = TRUE)

    md8 <- model_data %>%
      mutate(
        r_x_cci          = (real_rate    - r_mean)  * cci_williams,
        hp_x_1_minus_cci = (ln_hp_over_y - hp_mean) * (1 - 1.2 * cci_williams),
        yp_x_cci         = (ln_yp_over_y - yp_mean) * cci_williams,
        # Time-varying housing-wealth m.p.c. (Williams Aust eq 7 γ_1t·HA;
        # Tobin Lives 2013 eq 5.2 (HLI_{t-1})·HA/y). Captures Williams'
        # central LIVES mechanism: housing wealth becomes more spendable
        # as collateral when credit conditions ease. With de-meaning the
        # interaction coefficient is interpretable as the change in the
        # housing-wealth m.p.c. as CCI moves by 1 unit, evaluated at
        # ha_y = mean(ha_y). Sign prior: γ_HA_cci > 0.
        ha_x_cci         = (ha_y        - ha_mean) * cci_williams
      )
    spec8 <- fit_ecm_spec(
      data       = md8,
      spec_name  = "Spec8_CCI_Interactions",
      lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y",
                     "ha_x_cci",
                     "hp_x_1_minus_cci",
                     "r_x_cci",
                     "ln_yp_over_y",
                     "yp_x_cci",
                     "ecm_lag"),
      sr_vars    = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
      dummy_vars = base_dummies,
      sample_end = sample_end
    )

    # ------------------------------------------------------------------
    # Spec 11: LIVES headline — faithful Muellbauer/Williams Eq 7 core.
    # The point of difference from Spec 8 (which is an over-parameterised
    # add-on) is that the *credit-conditions mechanism is the core long
    # run*, exactly as in Williams' Eq 7 (Aust system paper p.7):
    #   - Housing enters ONLY through the credit-scaled interaction
    #     gamma_1*CCI*(HA/4y); the plain `ha_y` LEVEL is dropped, because
    #     Williams' housing-wealth MPC is ZERO at CCI=0 (no classical
    #     housing wealth effect, p.12). Estimating a standalone `ha_y` —
    #     as every other spec does — tests a coefficient the theory says
    #     should be ~0, which is why it is insignificant everywhere.
    #   - The autonomous-consumption intercept zeta_c*CCI is RESTORED as a
    #     long-run level (`cci_williams`); Spec 8 omits it entirely.
    #   - Illiquid financial assets are combined (ilfa_y = eq_y + super_y)
    #     to match Williams' 3-fold HA/IFA/NLA split and remove the
    #     collinear, wrong-signed `eq_y`/`super_y` pair.
    #   - Permanent income is geared psi(CCI) = psi_0 + psi_1*CCI via the
    #     `ln_yp_over_y` + `yp_x_cci` pair (freely estimated here; the
    #     calibrated psi_0=0.20, psi_1=0.93 counterpart is Spec 10).
    # Interactions reuse md8's de-meaned construction. This is the
    # "Option A" decisive test of whether the LIVES mechanism is alive on
    # Australian data once the headline is specified as Williams' actual
    # equation rather than a generic constant-MPC wealth ECM.
    # ------------------------------------------------------------------
    md11 <- md8 %>% mutate(ilfa_y = eq_y + super_y)
    spec11 <- fit_ecm_spec(
      data       = md11,
      spec_name  = "Spec11_LIVES_Headline",
      lr_vars    = c("nla_y", "ilfa_y",
                     "ha_x_cci",           # gamma_1 * CCI * (HA/4y) — housing, interaction-only
                     "hp_x_1_minus_cci",   # alpha_4 * (1 - 1.2*CCI) * log(p^h/y)
                     "r_x_cci",            # alpha_1 * r * CCI
                     "cci_williams",       # zeta_c * CCI autonomous-consumption intercept
                     "ln_yp_over_y", "yp_x_cci",  # psi(CCI)*log(y^p/y) = psi_0*yp + psi_1*yp*CCI
                     "ecm_lag"),
      sr_vars    = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
      dummy_vars = base_dummies,
      sample_end = sample_end
    )
  } else {
    spec8   <- NULL
    spec11  <- NULL
    message("Spec 8 skipped: cci_williams not present in model_data ",
            "(USE_WILLIAMS_SDMMA_BASIS may be FALSE or Williams fit failed)")
  }

  # ------------------------------------------------------------------
  # Spec 9: Kalman state-space CCI + interaction terms.
  # Same long-run + short-run structure as Spec 8 but uses cci_kalman
  # (latent factor extracted by Kalman filter from multiple credit
  # indicators; see fit_kalman_cci() in model_helpers.R) instead of the
  # spline-based cci_williams. This is the methodologically-distinct
  # alternative requested in cci_exploration.md NS-105.
  # ------------------------------------------------------------------
  if ("cci_kalman" %in% names(model_data) &&
      any(!is.na(model_data$cci_kalman))) {
    # Mirror Spec 8's construction exactly — de-meaned interactions including
    # the housing-collateral channel — so the Spec 8 vs Spec 9 comparison
    # isolates the CCI *series* (spline vs Kalman). A previous version used
    # un-de-meaned interactions and omitted ha_x_cci_k, so the two specs
    # differed in structure as well as in CCI measure.
    spec9_mask <- !is.na(model_data$cci_kalman) &
                  model_data$date >= as.Date("1980-01-01") &
                  model_data$date <= sample_end
    ha_mean_k <- mean(model_data$ha_y[spec9_mask],         na.rm = TRUE)
    hp_mean_k <- mean(model_data$ln_hp_over_y[spec9_mask], na.rm = TRUE)
    r_mean_k  <- mean(model_data$real_rate[spec9_mask],    na.rm = TRUE)
    yp_mean_k <- mean(model_data$ln_yp_over_y[spec9_mask], na.rm = TRUE)
    md9 <- model_data %>%
      mutate(
        r_x_cci_k          = (real_rate    - r_mean_k)  * cci_kalman,
        hp_x_1_minus_cci_k = (ln_hp_over_y - hp_mean_k) * (1 - 1.2 * cci_kalman),
        yp_x_cci_k         = (ln_yp_over_y - yp_mean_k) * cci_kalman,
        ha_x_cci_k         = (ha_y         - ha_mean_k) * cci_kalman
      )
    spec9 <- fit_ecm_spec(
      data       = md9,
      spec_name  = "Spec9_KalmanCCI",
      lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y",
                     "ha_x_cci_k",
                     "hp_x_1_minus_cci_k",
                     "r_x_cci_k",
                     "ln_yp_over_y",
                     "yp_x_cci_k",
                     "ecm_lag"),
      sr_vars    = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
      dummy_vars = base_dummies,
      sample_end = sample_end
    )
  } else {
    spec9 <- NULL
    message("Spec 9 skipped: cci_kalman not present in model_data ",
            "(KFAS may not be installed or fit_kalman_cci failed)")
  }

  # ------------------------------------------------------------------
  # Spec 10: Williams-prior calibrated specification.
  # Imposes Williams' published calibrations on the illiquid wealth MPC
  # (gamma_2 = 0.022, BIS Table 1), the permanent-income weight
  # psi(CCI) = 0.20 + 0.93*CCI, and the housing-affordability
  # interaction multiplier varpi = 1.2. Estimates the remaining free
  # coefficients (lambda, gamma_HA, gamma_NLA, gamma_HP, alpha_r) by
  # iterative OLS that solves the fixed-point lambda condition.
  # ------------------------------------------------------------------
  if ("cci_williams" %in% names(model_data) &&
      any(!is.na(model_data$cci_williams))) {
    spec10 <- tryCatch(
      fit_williams_prior_spec(model_data, sample_end = sample_end,
                              dummy_vars = base_dummies),
      error = function(e) {
        message("Spec 10 (Williams-prior calibrated) failed: ",
                conditionMessage(e))
        NULL
      }
    )
  } else {
    spec10 <- NULL
    message("Spec 10 skipped: cci_williams not present in model_data")
  }

  # ------------------------------------------------------------------
  # Spec 12: Calibrated LIVES headline — faithful Eq 7 with Williams'
  # Table 1 credit-channel calibrations imposed, free only gamma_1
  # (housing-collateral MPC), gamma_NLA and lambda. The single-equation-
  # feasible response to the Spec 11 interaction-block collinearity.
  # ------------------------------------------------------------------
  if ("cci_williams" %in% names(model_data) &&
      any(!is.na(model_data$cci_williams))) {
    spec12 <- tryCatch(
      fit_lives_calibrated_spec(model_data, sample_end = sample_end,
                                dummy_vars = base_dummies),
      error = function(e) {
        message("Spec 12 (Calibrated LIVES headline) failed: ",
                conditionMessage(e))
        NULL
      }
    )
  } else {
    spec12 <- NULL
    message("Spec 12 skipped: cci_williams not present in model_data")
  }

  specs <- list(spec1 = spec1, spec2 = spec2, spec3 = spec3,
                spec4 = spec4, spec5 = spec5, spec6 = spec6,
                spec6b = spec6b,
                spec7 = spec7, spec7b = spec7b,
                spec8 = spec8, spec9 = spec9, spec10 = spec10,
                spec11 = spec11, spec12 = spec12)
  Filter(Negate(is.null), specs)
}


# ==============================================================================
# SECTION F1: COVID Robustness — Alternative Sample / Dummy Variants
# ==============================================================================
# Two extra variants on top of full and pre-COVID:
#   - covid_dropped: drop 2020Q1–2021Q4 entirely
#   - covid_rich:    individual quarterly dummies for 2020Q1–2021Q4
# ==============================================================================

run_specifications_covid_robust <- function(model_data,
                                            sample_end = as.Date("2024-10-01")) {
  drop_dat <- model_data %>%
    filter(!(date >= as.Date("2020-01-01") & date <= as.Date("2021-10-01")))
  specs_dropped <- run_all_specifications(drop_dat, sample_end = sample_end)

  rich_dat <- model_data
  rich_qtrs <- seq.Date(as.Date("2020-01-01"), as.Date("2021-10-01"),
                        by = "quarter")
  rich_dummy_names <- character(0)
  for (q in seq_along(rich_qtrs)) {
    nm <- sprintf("d_%s", format(rich_qtrs[q], "%Y_q%m"))
    rich_dat[[nm]] <- as.integer(rich_dat$date == rich_qtrs[q])
    rich_dummy_names <- c(rich_dummy_names, nm)
  }
  base_keep <- c("d2000_gst", "d2008_gfc")
  specs_rich <- run_all_specifications(
    rich_dat, sample_end = sample_end,
    dummy_vars = c(base_keep, rich_dummy_names)
  )

  list(covid_dropped = specs_dropped, covid_rich = specs_rich)
}


# ==============================================================================
# SECTION F2: Williams (2010) reduced-form CCI fit
# ==============================================================================
# Estimates the spline coefficients of the maximal SDMMA candidate basis
# (15 knots by default; Williams’ literal 4-knot set is a robustness variant)
# inside the Spec-4 disaggregated long-run consumption equation. Sign priors
# are enforced by general-to-specific drop-on-violation (Hendry/Krolzig 2005),
# matching the Aust paper Section 3 (p.4-5). Surviving knots are then
# combined into a normalised cci_williams series for use in Spec 8.
# ==============================================================================

fit_consumption_with_williams_cci <- function(model_data, lr_vars, sr_vars,
                                               dummy_vars,
                                               sample_end = as.Date("2024-10-01"),
                                               basis_fn   = build_williams_cci_basis,
                                               max_iter   = 10L) {
  # Iterated knot-survival per Williams (Aust paper §5.1). At each
  # iteration we re-fit the consumption ECM with the currently-surviving
  # candidate knot set, drop any knot whose sign violates its
  # institutional prior, and repeat. The loop terminates when the
  # surviving set is stable across two consecutive iterations (typical:
  # 1–3 iterations on the 1988+ sample).
  basis <- basis_fn(model_data$date)
  sign_priors <- attr(basis, "sign_priors")
  cci_terms   <- colnames(basis)
  for (j in seq_along(cci_terms)) {
    model_data[[cci_terms[j]]] <- basis[, j]
  }
  names(sign_priors) <- cci_terms

  full_lr        <- c(lr_vars, cci_terms)
  dropped_total  <- character(0)
  iter_log       <- list()

  spec <- fit_ecm_spec(model_data, "Williams_CCI", full_lr, sr_vars,
                       dummy_vars, sample_end = sample_end)

  for (iter in seq_len(max_iter)) {
    cf <- coef(spec$fit)
    candidates <- intersect(cci_terms, full_lr)
    new_violators <- character(0)
    for (nm in candidates) {
      if (nm %in% names(cf) && !is.na(cf[nm]) && cf[nm] != 0) {
        if (sign(cf[nm]) != sign_priors[[nm]]) {
          new_violators <- c(new_violators, nm)
        }
      }
    }
    iter_log[[iter]] <- list(
      iter       = iter,
      candidates = candidates,
      violators  = new_violators
    )
    if (length(new_violators) == 0L) break

    message(sprintf("Williams CCI iter %d: dropping sign-violators: %s",
                    iter, paste(new_violators, collapse = ", ")))
    dropped_total <- c(dropped_total, new_violators)
    full_lr       <- setdiff(full_lr, new_violators)

    if (length(intersect(cci_terms, full_lr)) == 0L) {
      break
    }
    spec <- fit_ecm_spec(model_data, "Williams_CCI", full_lr, sr_vars,
                         dummy_vars, sample_end = sample_end)
  }

  # A knot is "surviving" only if it (a) was not sign-violated and (b) was
  # not dropped by lm() as collinear (NA coefficient — typically the 1979
  # SDMMA, which is constant within the post-1988 estimation window).
  cf2 <- coef(spec$fit)
  candidate_surv <- intersect(cci_terms, full_lr)
  aliased_surv <- candidate_surv[
    !candidate_surv %in% names(cf2) | is.na(cf2[candidate_surv])
  ]
  surviving <- setdiff(candidate_surv, aliased_surv)

  cci_fitted <- rep(NA_real_, nrow(model_data))
  if (length(surviving) > 0L) {
    X <- as.matrix(model_data[, surviving, drop = FALSE])
    raw <- as.numeric(X %*% cf2[surviving])
    mx <- max(raw, na.rm = TRUE)
    if (is.finite(mx) && mx > 0) {
      cci_fitted <- raw / mx
    } else if (is.finite(mx) && mx < 0) {
      cci_fitted <- raw / abs(mx)
    } else {
      cci_fitted <- raw
    }
  }
  model_data$cci_williams <- cci_fitted

  list(spec = spec, model_data = model_data,
       surviving_knots = surviving,
       dropped_knots = dropped_total,
       aliased_knots = aliased_surv,
       sign_priors = sign_priors,
       knot_names = cci_terms,
       iter_log = iter_log,
       n_iter   = length(iter_log))
}

build_lambda_robustness_table <- function(specs_full, specs_precovid,
                                          specs_dropped, specs_rich,
                                          output_dir) {
  extract_lambda <- function(specs_list, variant_label) {
    rows <- list()
    for (sp_name in names(specs_list)) {
      sp <- specs_list[[sp_name]]
      cf <- coef(sp$fit)
      vc <- sp$nw_vcov
      lam <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
      lam_se <- if ("ecm_lag" %in% rownames(vc))
        sqrt(vc["ecm_lag", "ecm_lag"]) else NA_real_
      rows[[sp_name]] <- tibble::tibble(
        specification   = sp$spec_name,
        sample_variant  = variant_label,
        lambda          = lam,
        lambda_se       = lam_se
      )
    }
    bind_rows(rows)
  }

  lam_tbl <- bind_rows(
    extract_lambda(specs_full,     "full"),
    extract_lambda(specs_precovid, "precovid"),
    extract_lambda(specs_dropped,  "covid_dropped"),
    extract_lambda(specs_rich,     "covid_rich")
  ) %>%
    mutate(lambda_signed_correctly = !is.na(lambda) & lambda < 0)

  stable <- lam_tbl %>%
    group_by(specification) %>%
    summarise(
      n_neg = sum(!is.na(lambda) & lambda < 0),
      n_total = sum(!is.na(lambda)),
      .groups = "drop"
    ) %>%
    mutate(lambda_sign_stable_across_samples = n_neg >= 3L | (n_total - n_neg) >= 3L)

  out <- lam_tbl %>%
    left_join(stable %>% select(specification, lambda_sign_stable_across_samples),
              by = "specification")

  out_path <- file.path(output_dir, "australia_lambda_robustness.csv")
  write.csv(out, out_path, row.names = FALSE)
  message(sprintf("[build_lambda_robustness_table] Saved to %s", out_path))
  out
}


# ==============================================================================
# SECTION F2: Rolling-window Estimation
# ==============================================================================

fit_rolling_window <- function(spec_template, model_data, output_dir,
                               window_size = 60L) {
  req <- c("dlcons", spec_template$lr_vars, spec_template$sr_vars,
           spec_template$dummy_vars)
  req <- intersect(req, names(model_data))

  dat <- model_data %>%
    filter(complete.cases(across(all_of(req))))
  n <- nrow(dat)
  if (n < window_size + 4L) {
    message("[fit_rolling_window] Not enough rows (",
            n, ") for window ", window_size)
    return(invisible(NULL))
  }

  track_terms <- intersect(
    c("ecm_lag", "real_rate", "ha_y", "eq_y", "super_y", "nla_y",
      "ln_yp_over_y", "ln_networth_y", "networth_y"),
    union(spec_template$lr_vars, names(coef(spec_template$fit)))
  )

  rows <- list()
  for (start_i in seq_len(n - window_size + 1L)) {
    sub <- dat[start_i:(start_i + window_size - 1L), , drop = FALSE]
    fit <- tryCatch(lm(spec_template$formula, data = sub),
                    error = function(e) NULL)
    if (is.null(fit)) next
    bw <- floor(4L * (nobs(fit) / 100)^(2/9))
    vc <- tryCatch(
      sandwich::NeweyWest(fit, lag = bw, prewhite = FALSE, adjust = TRUE),
      error = function(e) vcov(fit)
    )
    cf <- coef(fit)
    se <- sqrt(diag(vc))
    win_end <- max(sub$date)
    for (trm in track_terms) {
      if (!trm %in% names(cf)) next
      est <- cf[[trm]]
      s   <- if (trm %in% names(se)) se[[trm]] else NA_real_
      rows[[length(rows) + 1L]] <- tibble::tibble(
        window_end = win_end, term = trm,
        estimate = est, se = s,
        ci_low = est - 1.96 * s, ci_high = est + 1.96 * s
      )
    }
  }
  out <- bind_rows(rows)
  out_path <- file.path(output_dir, "australia_rolling_coefs.csv")
  write.csv(out, out_path, row.names = FALSE)
  message(sprintf("[fit_rolling_window] Saved to %s", out_path))

  if (nrow(out) > 0L) {
    p <- ggplot(out, aes(window_end, estimate)) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
                  fill = "steelblue", alpha = 0.2) +
      geom_line(colour = "steelblue", linewidth = 0.7) +
      geom_hline(yintercept = 0, linetype = 2, colour = "red") +
      facet_wrap(~ term, scales = "free_y") +
      labs(title = sprintf("Rolling %d-quarter coefficient estimates", window_size),
           x = NULL, y = "Estimate") +
      theme_minimal(base_size = 11)
    png_path <- file.path(output_dir, "australia_rolling_coefs.png")
    tryCatch(
      ggsave(png_path, p, width = 9, height = 6, dpi = 150),
      error = function(e) message(sprintf(
        "[fit_rolling_window] Failed to save plot: %s", e$message))
    )
  }

  invisible(out)
}


# ==============================================================================
# SECTION G: Results Table
# ==============================================================================

# A-priori expected signs for each long-run regressor (canonical negative-λ
# convention). NA = no prior; "+/-" = ambiguous.
EXPECTED_SIGNS <- c(
  ha_y          = "+",
  eq_y          = "+",
  super_y       = "+",
  nla_y         = "+",
  ilfa_y        = "+",
  networth_y    = "+",
  ln_networth_y = "+",
  ln_hp_over_y  = "+/-",
  real_rate     = "-",
  ln_yp_over_y  = "+/-",
  ln_yp_over_y_post2008 = "+/-",
  ecm_lag       = "-"
)

expected_sign_lookup <- function(terms) {
  out <- unname(EXPECTED_SIGNS[terms])
  out[is.na(out)] <- NA_character_
  out
}

format_coef_label <- function(estimate, pval, sign_ok) {
  vapply(seq_along(estimate), function(i) {
    if (is.na(estimate[i])) return(NA_character_)
    star <- if (is.na(pval[i])) ""
            else if (pval[i] < 0.01) "***"
            else if (pval[i] < 0.05) "**"
            else if (pval[i] < 0.10) "*"
            else ""
    sign_str <- if (estimate[i] >= 0) "+" else "-"
    base <- sprintf("%s%.4f%s", sign_str, abs(estimate[i]), star)
    if (!is.na(sign_ok[i]) && !sign_ok[i]) paste0(base, " (wrong sign)") else base
  }, character(1))
}

build_results_table <- function(specs, output_dir = "outputs", period_label = "full") {

  all_rows <- list()

  for (sp_name in names(specs)) {
    sp   <- specs[[sp_name]]
    fit  <- sp$fit
    vcov <- sp$nw_vcov

    cf     <- coef(fit)
    se_raw <- sqrt(diag(vcov))
    se     <- rep(NA_real_, length(cf))
    names(se) <- names(cf)
    se[names(se_raw)] <- se_raw
    tstat  <- cf / se
    pval   <- 2 * pt(-abs(tstat), df = nobs(fit) - sum(!is.na(cf)))

    lambda <- cf["ecm_lag"]
    if (is.na(lambda) || abs(lambda) < 1e-10) {
      warning(sprintf("[build_results_table] λ near zero for %s", sp_name))
      lambda <- NA_real_
    }

    diag_row <- tryCatch(
      model_diagnostics(fit, sp$est_data, break_date = "2008-07-01"),
      error = function(e) tibble::tibble(
        n_obs = nobs(fit), se_pct = NA_real_, adj_r2 = NA_real_,
        dw = NA_real_, lm_het_pval = NA_real_,
        lm_het_pval_no_events = NA_real_, het_diagnosis = NA_character_,
        ar1_pval = NA_real_,
        ar4_pval = NA_real_, chow_pval = NA_real_, chow_method = NA_character_,
        reset_pval = NA_real_, schwarz = NA_real_, loglik = NA_real_
      )
    )

    coef_tbl <- tibble::tibble(
      specification    = sp$spec_name,
      term             = names(cf),
      ols_estimate     = unname(cf),
      nw_se            = unname(se),
      t_stat           = unname(tstat),
      p_value          = unname(pval),
      lambda           = lambda,
      # Negative convention: long-run params = -OLS / λ. Sign of structural
      # parameters is the same as the OLD positive-convention pipeline because
      # both numerator (regressor sign-flip) and denominator (λ sign-flip)
      # cancel for the ECM term, while non-ECM regressors keep their OLS coef
      # but pick up a leading minus from the new λ < 0 convention.
      structural_param = -unname(cf) / lambda
    ) %>%
      mutate(
        in_long_run  = term %in% sp$lr_vars,
        in_short_run = term %in% sp$sr_vars,
        is_dummy     = term %in% sp$dummy_vars,
        expected_sign = expected_sign_lookup(term),
        sign_ok      = case_when(
          expected_sign == "+" ~ sign(ols_estimate) > 0,
          expected_sign == "-" ~ sign(ols_estimate) < 0,
          TRUE ~ NA
        ),
        signif_5pct  = !is.na(p_value) & p_value < 0.05,
        signif_1pct  = !is.na(p_value) & p_value < 0.01,
        coef_label   = format_coef_label(ols_estimate, p_value, sign_ok)
      )

    all_rows[[sp_name]] <- list(coef_tbl = coef_tbl, diag = diag_row)
  }

  coef_combined <- bind_rows(lapply(all_rows, `[[`, "coef_tbl"))
  diag_combined <- bind_rows(lapply(all_rows, `[[`, "diag")) %>%
    mutate(specification = vapply(specs, function(s) s$spec_name, character(1L)))

  coef_combined  <- coef_combined  %>% mutate(period = period_label)
  diag_combined  <- diag_combined  %>% mutate(period = period_label)

  # Export
  coef_csv <- file.path(output_dir, sprintf("australia_%s_results.csv",     period_label))
  diag_csv <- file.path(output_dir, sprintf("australia_%s_diagnostics.csv", period_label))
  write.csv(coef_combined, coef_csv, row.names = FALSE)
  write.csv(diag_combined, diag_csv, row.names = FALSE)
  message(sprintf("[build_results_table] Exported coefficient table to %s", coef_csv))
  message(sprintf("[build_results_table] Exported diagnostics to %s",       diag_csv))

  # Console table
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("TABLE 1 — Australia Consumption Model: OLS Estimates  [%s]\n", period_label))
  cat(rep("=", 70), "\n\n", sep = "")

  for (sp_name in names(specs)) {
    sp      <- specs[[sp_name]]
    cf_rows <- coef_combined %>% filter(specification == sp$spec_name)
    lam     <- cf_rows$lambda[1]
    diag    <- diag_combined %>% filter(specification == sp$spec_name)

    cat(sprintf("  %-40s λ = %.4f\n", sp$spec_name, lam))
    cat(rep("-", 62), "\n", sep = "")
    cat(sprintf("  %-30s %8s  %8s  %8s  %6s\n",
                "Term", "OLS coef", "NW SE", "Struct", "p-val"))

    for (i in seq_len(nrow(cf_rows))) {
      r <- cf_rows[i, ]
      struct_str <- if (r$in_long_run && !is.na(r$structural_param))
        sprintf("%8.4f", r$structural_param) else "        "
      pval_str <- if (!is.na(r$p_value)) {
        if (r$p_value < 0.001) "<0.001" else sprintf("%.3f ", r$p_value)
      } else "   NA  "
      cat(sprintf("  %-30s %8.4f  %8.4f  %s  %s\n",
                  r$term, r$ols_estimate,
                  ifelse(is.na(r$nw_se), 0, r$nw_se),
                  struct_str, pval_str))
    }
    cat(sprintf(
      "  Diagnostics: N=%d  SE=%.2f%%  adjR²=%.3f  DW=%.2f\n",
      diag$n_obs, diag$se_pct, diag$adj_r2, diag$dw))
    cat(sprintf(
      "  AR(1) p=%.3f  AR(4) p=%.3f  Chow p=%s  RESET p=%.3f\n\n",
      diag$ar1_pval, diag$ar4_pval,
      ifelse(is.na(diag$chow_pval), "NA",
             sprintf("%.3f", diag$chow_pval)),
      diag$reset_pval))
  }

  invisible(list(coef_combined = coef_combined, diag_combined = diag_combined))
}


# ==============================================================================
# SECTION G2: Preferred Specification Selector
# ==============================================================================
# Scores each spec on four screens (sign, cointegration, λ, stability) plus a
# BIC tiebreaker. Returns one row per spec with pass/fail flags and a single
# `is_preferred` indicator. Designed to feed into downstream plots, the
# narrative summary, and the comparison table.
# ==============================================================================

select_preferred_spec <- function(coef_combined, diag_combined, coint_results,
                                  coef_combined_precovid = NULL,
                                  lambda_robust = NULL,
                                  output_dir = NULL) {

  full_coef <- coef_combined %>% filter(period == "full")
  pre_coef  <- if (!is.null(coef_combined_precovid)) {
    coef_combined_precovid %>% filter(period == "precovid")
  } else NULL
  full_diag <- diag_combined %>% filter(period == "full")

  spec_names <- unique(full_coef$specification)
  rows <- list()

  for (sp in spec_names) {
    cf  <- full_coef %>% filter(specification == sp)
    dg  <- full_diag %>% filter(specification == sp) %>% slice(1L)

    # 1) Sign screen — all signed long-run terms have sign_ok == TRUE
    signed_lr <- cf %>%
      filter(in_long_run, !is.na(expected_sign), expected_sign != "+/-",
             term != "ecm_lag")
    pass_signs <- if (nrow(signed_lr) == 0L) NA
                  else all(signed_lr$sign_ok, na.rm = FALSE)

    # 2) Cointegration screen — ADF rejects unit root
    pass_coint <- if (is.null(coint_results)) NA else {
      r <- coint_results %>% filter(specification == sp)
      if (nrow(r) == 0L) NA else r$coint_adf_pass[1L]
    }

    # 3) Lambda screen — sign matches expected (negative) AND |λ| in (0.02, 0.30)
    lam <- cf$lambda[1L]
    pass_lambda <- !is.na(lam) && lam < 0 && abs(lam) > 0.02 && abs(lam) < 0.30

    # 4) Stability screen — Chow not rejected at 1% AND λ sign-stable.
    # An incomputable Chow (NA even after the manual fallback) is treated as
    # NEUTRAL, not as a failure: a previous version required a non-NA p-value,
    # so 12 of 14 specs "failed" stability purely because sctest could not run
    # on subsamples with all-zero COVID dummies.
    chow_ok  <- is.na(dg$chow_pval) || dg$chow_pval > 0.01
    sign_stable <- if (!is.null(lambda_robust)) {
      r <- lambda_robust %>% filter(specification == sp)
      if (nrow(r) == 0L) NA else all(r$lambda_sign_stable_across_samples,
                                     na.rm = FALSE)
    } else if (!is.null(pre_coef)) {
      pre <- pre_coef %>% filter(specification == sp)
      lam_pre <- if (nrow(pre) > 0L) pre$lambda[1L] else NA_real_
      !is.na(lam) && !is.na(lam_pre) && sign(lam) == sign(lam_pre)
    } else NA
    pass_stability <- !is.na(chow_ok) && chow_ok &&
                      (is.na(sign_stable) || isTRUE(sign_stable))

    rows[[sp]] <- tibble::tibble(
      specification  = sp,
      pass_signs     = pass_signs,
      pass_coint     = pass_coint,
      pass_lambda    = pass_lambda,
      pass_stability = pass_stability,
      bic            = dg$schwarz
    )
  }

  out <- bind_rows(rows)

  # Identify preferred: passes all four (NA-tolerant) screens, lowest BIC
  passes_all <- function(r) {
    all(vapply(c("pass_signs", "pass_coint", "pass_lambda", "pass_stability"),
               function(k) isTRUE(r[[k]]) || is.na(r[[k]]),
               logical(1L)))
  }
  hard_pass_all <- function(r) {
    all(vapply(c("pass_signs", "pass_coint", "pass_lambda", "pass_stability"),
               function(k) isTRUE(r[[k]]),
               logical(1L)))
  }

  out$is_preferred <- FALSE
  hard_idx <- which(vapply(seq_len(nrow(out)),
                           function(i) hard_pass_all(out[i, ]),
                           logical(1L)))
  if (length(hard_idx) > 0L) {
    pick <- hard_idx[which.min(out$bic[hard_idx])]
    out$is_preferred[pick] <- TRUE
  } else {
    warning("[select_preferred_spec] No spec passes all four screens; ",
            "picking spec with the most passes (BIC tiebreak).")
    pass_count <- vapply(seq_len(nrow(out)), function(i) {
      sum(vapply(c("pass_signs", "pass_coint", "pass_lambda", "pass_stability"),
                 function(k) isTRUE(out[[k]][i]),
                 logical(1L)))
    }, integer(1L))
    max_pass <- max(pass_count)
    cand     <- which(pass_count == max_pass)
    pick     <- cand[which.min(out$bic[cand])]
    out$is_preferred[pick] <- TRUE
  }

  if (!is.null(output_dir)) {
    sel_path <- file.path(output_dir, "australia_spec_selection.csv")
    write.csv(out, sel_path, row.names = FALSE)
    message(sprintf("[select_preferred_spec] Saved to %s", sel_path))
  }

  out
}


# ==============================================================================
# SECTION G3: Wald test of NLA cross-equation restriction
# ==============================================================================
# Italy paper Section 2 eq 2.5 / Table 3 col 3 imposes gamma_LA + gamma_LOANS = 0
# (deposits and household debt enter with equal-and-opposite coefficients) and
# accepts it. This helper refits each disaggregated spec with the unrestricted
# decomposition (nla_y_unrestricted = deposits/y, debt_y = loans/y) and tests
# whether the restriction is statistically defensible in Australian data.

test_nla_restriction <- function(model_data,
                                 sample_label = "full",
                                 sample_end   = as.Date("2024-10-01")) {

  base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                    "d_neg_gearing_8587", "d_recession_1991",
                    "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

  # Spec families to test: 4 (no CCI), 5 (full disagg), 6 (preferred).
  spec_defs <- list(
    Spec4_Disagg_NoCCI = list(
      sr = character(0),
      lr_extra = c("eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ecm_lag")
    ),
    Spec5_FullDisagg = list(
      sr = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid"),
      lr_extra = c("eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ecm_lag")
    ),
    Spec6_Preferred = list(
      sr = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid"),
      lr_extra = c("eq_y", "super_y", "ha_y", "ln_hp_over_y",
                   "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                   "ecm_lag")
    )
  )

  rows <- list()
  for (sp in names(spec_defs)) {
    cfg <- spec_defs[[sp]]
    fit <- tryCatch(
      fit_ecm_spec(
        data       = model_data,
        spec_name  = paste0(sp, "_unrestricted"),
        lr_vars    = c("nla_y_unrestricted", "debt_y", cfg$lr_extra),
        sr_vars    = cfg$sr,
        dummy_vars = base_dummies,
        sample_end = sample_end
      ),
      error = function(e) {
        message(sprintf("[test_nla_restriction] %s failed: %s", sp, e$message))
        NULL
      }
    )
    if (is.null(fit)) next

    cf   <- coef(fit$fit)
    vcov <- fit$nw_vcov
    if (!all(c("nla_y_unrestricted", "debt_y") %in% names(cf))) {
      message(sprintf("[test_nla_restriction] %s: regressors dropped, skipping",
                      sp))
      next
    }
    b1 <- unname(cf["nla_y_unrestricted"])
    b2 <- unname(cf["debt_y"])
    v11 <- vcov["nla_y_unrestricted", "nla_y_unrestricted"]
    v22 <- vcov["debt_y", "debt_y"]
    v12 <- vcov["nla_y_unrestricted", "debt_y"]
    sum_bs <- b1 + b2
    se_sum <- sqrt(v11 + v22 + 2 * v12)

    have_car <- requireNamespace("car", quietly = TRUE)
    if (have_car) {
      lh <- tryCatch(
        car::linearHypothesis(fit$fit,
          "nla_y_unrestricted + debt_y = 0",
          vcov. = vcov, test = "Chisq"),
        error = function(e) NULL
      )
      if (!is.null(lh) && "Pr(>Chisq)" %in% names(lh)) {
        chi_stat <- lh$Chisq[2L]
        p_val    <- lh$`Pr(>Chisq)`[2L]
        t_stat   <- sign(sum_bs) * sqrt(chi_stat)
      } else {
        t_stat <- sum_bs / se_sum
        p_val  <- 2 * pt(-abs(t_stat),
                         df = nobs(fit$fit) - length(cf))
      }
    } else {
      t_stat <- sum_bs / se_sum
      p_val  <- 2 * pt(-abs(t_stat),
                       df = nobs(fit$fit) - length(cf))
    }

    rows[[sp]] <- tibble::tibble(
      specification         = sp,
      sample                = sample_label,
      b_nla                 = b1,
      b_debt                = b2,
      sum                   = sum_bs,
      se                    = se_sum,
      t_stat                = t_stat,
      p_value               = p_val,
      restriction_accepted  = isTRUE(p_val > 0.05)
    )
  }

  out <- if (length(rows)) bind_rows(rows) else
    tibble::tibble(specification = character(), sample = character(),
                   b_nla = numeric(), b_debt = numeric(),
                   sum = numeric(), se = numeric(),
                   t_stat = numeric(), p_value = numeric(),
                   restriction_accepted = logical())
  out
}


# ==============================================================================
# SECTION H2: Italy–Australia Comparison Table
# ==============================================================================

build_comparison_table <- function(aus_coef, output_dir, italy_dir) {
  # Read Italy structural parameters (long_run_coefficients.csv or ecm_coefficients.csv)
  italy_lr_path <- file.path(italy_dir, "long_run_coefficients.csv")
  italy_ec_path <- file.path(italy_dir, "ecm_coefficients.csv")
  italy_coef <- tryCatch({
    if (file.exists(italy_lr_path)) read.csv(italy_lr_path, stringsAsFactors = FALSE)
    else if (file.exists(italy_ec_path)) read.csv(italy_ec_path, stringsAsFactors = FALSE)
    else NULL
  }, error = function(e) NULL)

  if (is.null(italy_coef)) {
    message("[build_comparison_table] Italy coefficients not found — skipping comparison")
    return(invisible(NULL))
  }

  # Identify value column — accept "structural_param", "structural", "long_run", "estimate", "value"
  struct_col <- intersect(c("structural_param", "structural", "long_run", "estimate", "value"),
                          names(italy_coef))[1L]
  term_col   <- intersect(c("term", "variable", "covariate"), names(italy_coef))[1L]
  if (any(is.na(c(struct_col, term_col)))) {
    message("[build_comparison_table] Cannot identify columns in Italy output — skipping")
    return(invisible(NULL))
  }

  # Map Italy long-form term names to Australian short names where needed
  italy_term_map <- c(
    "housing_wealth_income_ratio"          = "ha_y",
    "net_liquid_assets_income_ratio"       = "nla_y",
    "illiquid_financial_income_ratio"      = "ilfa_y",
    "log_disposable_permanent_income_ratio"= "ln_yp_over_y",
    "real_mortgage_rate"                   = "real_rate",
    "credit_conditions_index"              = "cci"
  )

  key_terms <- c("ha_y", "nla_y", "ilfa_y", "eq_y", "super_y",
                 "bonds_y", "ln_hp_over_y", "real_rate", "ln_yp_over_y",
                 "ecm_lag")

  # Identify specification column — if absent assign a default
  spec_col <- intersect(c("specification", "spec", "model"), names(italy_coef))[1L]

  italy_tbl <- italy_coef %>%
    rename(term_raw = !!term_col, structural_param = !!struct_col) %>%
    mutate(term = if_else(term_raw %in% names(italy_term_map),
                          italy_term_map[term_raw], term_raw)) %>%
    filter(term %in% key_terms) %>%
    mutate(country = "Italy",
           specification = if (!is.na(spec_col)) .data[[spec_col]] else "Italy_Preferred") %>%
    select(country, specification, term, structural_param)

  aus_tbl <- aus_coef %>%
    filter(term %in% key_terms, in_long_run | term == "ecm_lag",
           period == "full") %>%
    mutate(country = "Australia") %>%
    select(country, specification, term, structural_param)

  comparison <- bind_rows(italy_tbl, aus_tbl) %>%
    arrange(term, country, specification)

  comp_path <- file.path(output_dir, "italy_australia_comparison.csv")
  write.csv(comparison, comp_path, row.names = FALSE)
  message(sprintf("[build_comparison_table] Saved to %s", comp_path))

  # Console summary — preferred spec only (last spec in each country)
  cat("\n--- Italy vs Australia: structural parameters (preferred spec) ---\n")
  preferred_aus   <- aus_tbl   %>% filter(grepl("Preferred|Spec6", specification)) %>%
    group_by(term) %>% slice_tail(n = 1) %>% ungroup()
  preferred_italy <- italy_tbl %>%
    group_by(term) %>% slice_tail(n = 1) %>% ungroup()
  comp_wide <- full_join(
    preferred_italy %>% rename(Italy = structural_param) %>% select(term, Italy),
    preferred_aus   %>% rename(Australia = structural_param) %>% select(term, Australia),
    by = "term"
  ) %>% arrange(term)
  for (i in seq_len(nrow(comp_wide))) {
    cat(sprintf("  %-20s  Italy: %8.4f   Australia: %8.4f\n",
      comp_wide$term[i],
      ifelse(is.na(comp_wide$Italy[i]),       NA, comp_wide$Italy[i]),
      ifelse(is.na(comp_wide$Australia[i]),   NA, comp_wide$Australia[i])))
  }

  # ------------------------------------------------------------------
  # Speed-of-adjustment (lambda) comparison table
  #
  # Italy: coefficient on ln_y_over_c in italy_table1_results.csv (positive
  #   convention; Italy script has not yet been switched to ecm_lag).
  # Australia: coefficient on ecm_lag in australia_*_results.csv (canonical
  #   Engle-Granger negative-restoration convention; λ < 0 = restoring force).
  #
  # Earlier code looked for ecm_coefficients.csv with term == "ecm_lag" in
  # italy_dir, which resolved to an unrelated old Australia ECM at the project
  # root with ecm_lag = -0.0099. That stale value was being reported as Italy
  # lambda. The fix reads italy_table1_results.csv directly.
  # ------------------------------------------------------------------
  italy_t1_candidates <- c(
    file.path(output_dir, "italy_table1_results.csv"),
    file.path(italy_dir,  "italy_table1_results.csv")
  )
  italy_t1_path <- italy_t1_candidates[file.exists(italy_t1_candidates)][1L]
  italy_lambda_full <- tryCatch({
    if (is.na(italy_t1_path)) NA_real_ else {
      ec   <- read.csv(italy_t1_path, stringsAsFactors = FALSE)
      pref <- ec[ec$term == "ln_y_over_c" &
                 grepl("Preferred", ec$specification), ]
      if (nrow(pref) == 0L) {
        # Fall back to last spec (which is the preferred in italy_estimation.R)
        all_lr <- ec[ec$term == "ln_y_over_c", ]
        if (nrow(all_lr) > 0L) tail(all_lr$ols_estimate, 1L) else NA_real_
      } else {
        pref$ols_estimate[1L]
      }
    }
  }, error = function(e) NA_real_)
  italy_lambda_source <- if (is.null(italy_t1_path) || is.na(italy_t1_path))
    "italy_table1_results.csv (NOT FOUND)" else italy_t1_path

  aus_lambda <- aus_coef %>%
    filter(term == "ecm_lag") %>%
    select(specification, period, lambda) %>%
    distinct()

  cat("\n--- Speed of adjustment (lambda) comparison ---\n")
  cat(sprintf("  Italy (ln_y_over_c, full, preferred spec):  %8.4f  [abs = %.4f per quarter]\n",
              italy_lambda_full, abs(italy_lambda_full)))
  cat(sprintf("  %-8s  %-35s  %8s  %8s\n", "Period", "Australia specification", "lambda", "|lambda|"))
  cat(sprintf("  %s\n", strrep("-", 65)))
  for (i in seq_len(nrow(aus_lambda))) {
    cat(sprintf("  %-8s  %-35s  %8.4f\n",
      aus_lambda$period[i], aus_lambda$specification[i],
      aus_lambda$lambda[i]))
  }

  # Both countries should now use the canonical negative convention.
  preferred_aus_lambda <- aus_lambda %>%
    filter(grepl("Preferred|Spec6", specification), period == "full") %>%
    pull(lambda)
  if (length(preferred_aus_lambda) > 0L && !is.na(italy_lambda_full) &&
      !is.na(preferred_aus_lambda[1L])) {
    if (sign(italy_lambda_full) != sign(preferred_aus_lambda[1L])) {
      message(sprintf(
        "[build_comparison_table] Italy λ (%.4f, positive convention) and Australia preferred λ (%.4f, negative convention) have different signs as expected for the differing conventions.",
        italy_lambda_full, preferred_aus_lambda[1L]))
    }
  }

  lambda_path <- file.path(output_dir, "italy_australia_lambda.csv")
  lambda_tbl <- bind_rows(
    tibble(country = "Italy",
           specification = "Italy_Preferred",
           period = "full",
           lambda = italy_lambda_full,
           lambda_source = italy_lambda_source,
           note = "OLS coefficient on ln(y/c) in italy_table1_results.csv (preferred spec, full sample)"),
    aus_lambda %>% mutate(country = "Australia",
                          lambda_source = "australia_*_results.csv (term == ecm_lag)",
                          note = "OLS coefficient on ecm_lag (negative-restoration convention)")
  ) %>%
    select(country, specification, period, lambda, lambda_source, note)
  write.csv(lambda_tbl, lambda_path, row.names = FALSE)
  message(sprintf("[build_comparison_table] Lambda table saved to %s", lambda_path))

  invisible(comparison)
}

# ==============================================================================
# SECTION H3: Narrative Model Summary (markdown)
# ==============================================================================

write_model_summary <- function(specs_full, specs_precovid,
                                results_full, results_precovid,
                                comparison, selection, coint_results,
                                lambda_robust = NULL,
                                break_results = NULL,
                                wls_results = NULL,
                                pi_sensitivity = NULL,
                                output_dir) {

  full_coef <- results_full$coef_combined
  full_diag <- results_full$diag_combined
  pre_coef  <- results_precovid$coef_combined

  preferred_row <- selection %>% filter(is_preferred)
  preferred_name <- if (nrow(preferred_row) > 0L)
    preferred_row$specification[1L] else NA_character_

  # Sample summaries
  full_dates <- range(specs_full$spec1$est_data$date)
  pre_dates  <- range(specs_precovid$spec1$est_data$date)
  full_n <- nobs(specs_full$spec1$fit)
  pre_n  <- nobs(specs_precovid$spec1$fit)

  pref_cf <- full_coef %>% filter(specification == preferred_name)
  pref_diag <- full_diag %>% filter(specification == preferred_name) %>% slice(1L)

  out <- character(0)
  out <- c(out, "# Australia Household Consumption Model — Summary", "")
  out <- c(out, "## Sample and data")
  out <- c(out, sprintf("- Full sample: %s–%s, n=%d",
                        format_yq(full_dates[1L]),
                        format_yq(full_dates[2L]), full_n))
  out <- c(out, sprintf("- Pre-COVID sample: %s–%s, n=%d",
                        format_yq(pre_dates[1L]),
                        format_yq(pre_dates[2L]), pre_n))
  out <- c(out, "- Data sources: ABS HFCE 5206008, ABS Household Income 5206020, ABS Balance Sheet 5232035, RBA f06hist mortgage rate, ABS lending 560101.", "")

  out <- c(out, "## Preferred specification")
  if (!is.na(preferred_name)) {
    pass_str <- preferred_row %>%
      transmute(s = sprintf("signs=%s, coint=%s, λ=%s, stability=%s",
                            pass_signs, pass_coint, pass_lambda, pass_stability)) %>%
      pull(s)
    out <- c(out, sprintf("**%s** (selected by automated screen). Reason: %s.",
                          preferred_name, pass_str), "")
    out <- c(out, sprintf("Long-run structural coefficients (preferred spec, full sample):"), "")
    out <- c(out, "| Term | Coef | t-stat | p | Expected sign | Sign OK |",
             "|------|------|--------|---|---------------|---------|")
    pref_lr <- pref_cf %>% filter(in_long_run)
    for (i in seq_len(nrow(pref_lr))) {
      r <- pref_lr[i, ]
      out <- c(out, sprintf("| %s | %.4f | %.2f | %.3f | %s | %s |",
                            r$term, r$ols_estimate, r$t_stat, r$p_value,
                            ifelse(is.na(r$expected_sign), "—", r$expected_sign),
                            ifelse(is.na(r$sign_ok), "—",
                                   ifelse(r$sign_ok, "yes", "no"))))
    }
    out <- c(out, "")
  } else {
    out <- c(out, "_No spec selected._", "")
  }

  out <- c(out, "## All specifications — diagnostics traffic light", "")
  out <- c(out, "| Spec | adj R² | DW | AR(1) | AR(4) | Het | Chow | RESET | BIC | Sign | Coint | λ | Stability |",
           "|------|--------|----|-------|-------|-----|------|-------|-----|------|-------|---|-----------|")
  spec_names <- unique(full_diag$specification)
  for (sp in spec_names) {
    d  <- full_diag %>% filter(specification == sp) %>% slice(1L)
    sl <- selection %>% filter(specification == sp)
    tick <- function(b) if (is.na(b)) "—" else if (isTRUE(b)) "Y" else "N"
    out <- c(out, sprintf("| %s | %.3f | %.2f | %s | %s | %s | %s | %s | %.1f | %s | %s | %s | %s |",
      sp, d$adj_r2, d$dw,
      tick(d$ar1_pval > 0.05), tick(d$ar4_pval > 0.05),
      tick(d$lm_het_pval > 0.05), tick(d$chow_pval > 0.05),
      tick(d$reset_pval > 0.05), d$schwarz,
      tick(sl$pass_signs[1L]), tick(sl$pass_coint[1L]),
      tick(sl$pass_lambda[1L]), tick(sl$pass_stability[1L])
    ))
  }
  out <- c(out, "")

  out <- c(out, "## Lambda comparison (full vs pre-COVID)", "")
  out <- c(out, "| Spec | Full sample λ | Pre-COVID λ | Sign-stable? |",
           "|------|---------------|-------------|--------------|")
  for (sp in spec_names) {
    fl <- (full_coef %>% filter(specification == sp))$lambda[1L]
    pl <- (pre_coef  %>% filter(specification == sp))$lambda[1L]
    stable <- if (!is.na(fl) && !is.na(pl)) sign(fl) == sign(pl) else NA
    out <- c(out, sprintf("| %s | %.4f | %.4f | %s |", sp,
                          ifelse(is.na(fl), NA_real_, fl),
                          ifelse(is.na(pl), NA_real_, pl),
                          ifelse(is.na(stable), "—", ifelse(stable, "yes", "no"))))
  }
  out <- c(out, "")

  out <- c(out, "## Italy vs Australia (preferred specs)", "")
  if (!is.null(comparison) && nrow(comparison) > 0L) {
    pa <- comparison %>% filter(country == "Australia",
                                grepl("Preferred|Spec6", specification))
    pi_aus <- pa %>% group_by(term) %>% slice_tail(n = 1) %>% ungroup() %>%
      select(term, Australia = structural_param)
    pit <- comparison %>% filter(country == "Italy") %>%
      group_by(term) %>% slice_tail(n = 1) %>% ungroup() %>%
      select(term, Italy = structural_param)
    cmp <- full_join(pit, pi_aus, by = "term") %>% arrange(term)
    out <- c(out, "| Term | Italy | Australia |", "|------|-------|-----------|")
    for (i in seq_len(nrow(cmp))) {
      r <- cmp[i, ]
      out <- c(out, sprintf("| %s | %s | %s |", r$term,
                            ifelse(is.na(r$Italy), "—", sprintf("%.4f", r$Italy)),
                            ifelse(is.na(r$Australia), "—", sprintf("%.4f", r$Australia))))
    }
  } else {
    out <- c(out, "_Italy comparison not available._")
  }
  out <- c(out, "")

  out <- c(out, "## Known issues")
  for (sp in spec_names) {
    sl <- selection %>% filter(specification == sp)
    failures <- character(0)
    if (isFALSE(sl$pass_signs[1L]))     failures <- c(failures, "sign")
    if (isFALSE(sl$pass_coint[1L]))     failures <- c(failures, "cointegration")
    if (isFALSE(sl$pass_lambda[1L]))    failures <- c(failures, "λ range/sign")
    if (isFALSE(sl$pass_stability[1L])) failures <- c(failures, "stability")
    if (length(failures) > 0L) {
      out <- c(out, sprintf("- %s fails: %s", sp,
                            paste(failures, collapse = ", ")))
    }
  }
  if (any(!is.na(full_diag$lm_het_pval) & full_diag$lm_het_pval < 0.05)) {
    out <- c(out, "- Heteroskedasticity rejected at 5% in some specs — see `lm_het_pval`, `lm_het_pval_no_events`, `het_diagnosis` columns of diagnostics CSV.")
  }
  out <- c(out, "- COVID handling: see `australia_lambda_robustness.csv` for sample sensitivity.")
  out <- c(out, "- `model_helpers.R::compute_log_yp_over_y` ignores its `discount`, `horizon`, `weights`, `denom` arguments and returns a raw level gap. Flagged for human review.")
  out <- c(out, "- Permanent income relies on three coincident GFC corrections (step2008, trend_brk, learning-weight ogive) plus spec 6's `ln_yp_over_y_post2008` interaction. See `australia_permanent_income_sensitivity.csv`.")
  out <- c(out, "")

  out <- c(out, "## Reproducibility")
  out <- c(out, "- Run: `Rscript Australia/R/australia_consumption_model.R`")
  out <- c(out, "- Fast re-estimation: `Rscript Australia/R/run_estimation_from_rds.R`")
  out <- c(out, "- Random seed: not used (OLS is deterministic).")
  out <- c(out, sprintf("- Date generated: %s", format(Sys.Date(), "%Y-%m-%d")))
  out <- c(out, "")

  md_path <- file.path(output_dir, "australia_model_summary.md")
  writeLines(out, md_path)
  message(sprintf("[write_model_summary] Saved to %s", md_path))
  invisible(md_path)
}


# ==============================================================================
# SECTION H: Plots
# ==============================================================================

plot_actual_vs_fitted <- function(spec, output_dir = "outputs") {
  fit      <- spec$fit
  est_data <- spec$est_data
  sp_name  <- spec$spec_name

  fitted_vals  <- fitted(fit)
  actual_vals  <- est_data$dlcons
  resid_vals   <- residuals(fit)
  dates        <- est_data$date

  p1 <- ggplot(tibble(date = dates, actual = actual_vals, fitted = fitted_vals)) +
    geom_line(aes(date, actual), colour = "steelblue", linewidth = 0.7) +
    geom_line(aes(date, fitted), colour = "firebrick",  linewidth = 0.7, linetype = 2) +
    labs(title = paste("Actual vs Fitted —", sp_name),
         subtitle = sprintf("N=%d, adj R²=%.3f",
                            nobs(fit), summary(fit)$adj.r.squared),
         x = NULL, y = "Δ ln consumption") +
    theme_minimal(base_size = 11)

  png_path <- file.path(output_dir,
    paste0("australia_", tolower(sp_name), "_actual_vs_fitted.png"))
  ggsave(png_path, p1, width = 9, height = 5, dpi = 150)
  message(sprintf("[plot_actual_vs_fitted] Saved to %s", png_path))

  p2 <- ggplot(tibble(date = dates, resid = resid_vals)) +
    geom_line(aes(date, resid), colour = "grey40", linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = 2, colour = "red") +
    labs(title = paste("Residuals —", sp_name), x = NULL, y = "Residual") +
    theme_minimal(base_size = 11)

  resid_path <- file.path(output_dir,
    paste0("australia_", tolower(sp_name), "_residuals.png"))
  ggsave(resid_path, p2, width = 9, height = 4, dpi = 150)
  message(sprintf("[plot_actual_vs_fitted] Residual plot saved to %s", resid_path))
}


# ==============================================================================
# SECTION I: Preferred-Spec Selector
# ==============================================================================
#
# Picks the spec used for downstream robustness/decomposition work.  Current
# convention: the script's "preferred" spec is Spec6_Preferred when present;
# fall back to the last spec in the list otherwise.
# ==============================================================================

pick_preferred_spec_object <- function(specs, preferred_name = NULL) {
  if (!is.null(preferred_name) && length(preferred_name) > 0L) {
    matches <- vapply(specs, function(sp) identical(sp$spec_name, preferred_name),
                      logical(1L))
    if (any(matches)) return(specs[[which(matches)[1L]]])
  }
  pref_keys <- intersect(c("spec6", "spec7"), names(specs))
  if (length(pref_keys) > 0L) return(specs[[tail(pref_keys, 1L)]])
  specs[[length(specs)]]
}


# ==============================================================================
# SECTION J: Italy-style Robustness Suite (Italy.pdf §4.2)
# ==============================================================================
#
# Implements the four robustness checks Italy uses to validate single-equation
# OLS:
#   (1) IV on current income (Hall 1978 endogeneity test)
#   (2) Joint PI + consumption FIML (SUR)
#   (3) Chow battery at 1995Q1, 2000Q1, 2008Q3, 2020Q1
#   (4) Scaled-income alternative (where available)
#   (5) Drehmann amortising-mortgage real rate (N=25y for Australia)
#
# Each block is wrapped in tryCatch — a failure in one does not break the rest.
# ==============================================================================

run_italy_style_robustness <- function(preferred_spec, model_data, output_dir,
                                       file_suffix = "") {

  # file_suffix lets the suite run for more than one specification without
  # overwriting (MAIN runs it for the selector-preferred spec and, with
  # suffix "_spec11", for the LIVES headline Spec 11).
  suffixed <- function(fname) {
    file.path(output_dir, sub("\\.csv$", paste0(file_suffix, ".csv"), fname))
  }

  spec_name <- preferred_spec$spec_name
  est_data  <- preferred_spec$est_data
  base_fit  <- preferred_spec$fit
  lr_vars   <- preferred_spec$lr_vars
  sr_vars   <- preferred_spec$sr_vars
  dummies   <- preferred_spec$dummy_vars
  fmla      <- preferred_spec$formula
  response  <- as.character(fmla[[2L]])
  rhs_terms <- attr(terms(fmla), "term.labels")

  message(sprintf("\n[run_italy_style_robustness] Preferred spec: %s", spec_name))
  message(sprintf("  N = %d observations, RHS terms = %d",
                  nobs(base_fit), length(rhs_terms)))

  # ----------------------------------------------------------------------
  # (1) OLS vs IV-on-current-income
  # ----------------------------------------------------------------------
  iv_path <- suffixed("australia_iv_robustness.csv")
  tryCatch({
    if (!requireNamespace("AER", quietly = TRUE))
      stop("AER package not installed — install.packages('AER')")

    iv_data <- model_data %>%
      arrange(date) %>%
      mutate(
        lincome_l1   = lag(lincome,      1L),
        lincome_l2   = lag(lincome,      2L),
        lincome_l4   = lag(lincome,      4L),
        unemp_l1     = lag(unemp_rate,   1L),
        unemp_l2     = lag(unemp_rate,   2L),
        mortgage_l1  = lag(mortgage_rate, 1L)
      )

    iv_data_aligned <- iv_data %>%
      filter(date >= min(est_data$date),
             date <= max(est_data$date))

    instruments <- c("lincome_l1", "lincome_l2", "lincome_l4",
                     "unemp_l1", "unemp_l2", "mortgage_l1")

    # Identify endogenous regressors: anything that contains CURRENT lincome.
    # That is the legacy ln_y_over_c, the canonical ecm_lag
    # (= lag(lcons,1) - lincome_t), and ln_yp_over_y (= y^p_hat - lincome_t).
    # A previous version omitted ecm_lag, so the Hall-endogeneity term itself
    # was treated as exogenous and only the PI gap was instrumented.
    #
    # The SAME rule also catches three Spec 8/11 CCI interaction terms that a
    # previous version left in the exogenous/instrument block:
    #   - yp_x_cci  = (ln_yp_over_y - mean)*cci_williams: mechanically
    #     contains ln_yp_over_y, hence current lincome.
    #   - ha_x_cci  = (ha_y - mean)*cci_williams: ha_y = housing_wealth /
    #     ydi_ann_nom (CURRENT-period income denominator).
    #   - hp_x_1_minus_cci = (ln_hp_over_y - mean)*(1 - 1.2*cci_williams):
    #     ln_hp_over_y = log(hpi*pop/ydi_ann_nom), same current-income ratio.
    # r_x_cci = (real_rate - mean)*cci_williams is NOT added: real_rate has no
    # income term, so it does not meet the stated rule. cci_williams itself
    # (bare CCI level, no ratio) likewise stays exogenous. nla_y/ilfa_y are
    # also current-income ratios by construction but are left as-is here:
    # they are not CCI interactions and were out of scope for this fix.
    endog <- intersect(c("ln_y_over_c", "ecm_lag", "ln_yp_over_y",
                        "yp_x_cci", "ha_x_cci", "hp_x_1_minus_cci"),
                       rhs_terms)
    if (length(endog) == 0L) endog <- intersect("ln_y_over_c", rhs_terms)
    exog  <- setdiff(rhs_terms, endog)

    iv_data_full <- iv_data_aligned %>%
      filter(complete.cases(across(all_of(c(response, rhs_terms, instruments)))))

    if (nrow(iv_data_full) < 30L)
      stop(sprintf("only %d complete obs for IV", nrow(iv_data_full)))

    fmla_iv <- as.formula(sprintf(
      "%s ~ %s | %s",
      response,
      paste(rhs_terms,                        collapse = " + "),
      paste(c(exog, instruments),             collapse = " + ")
    ))

    iv_fit <- AER::ivreg(fmla_iv, data = iv_data_full)

    # Re-fit OLS on the same sample so the comparison is apples-to-apples
    fmla_ols_same <- reformulate(rhs_terms, response = response)
    ols_fit_same  <- lm(fmla_ols_same, data = iv_data_full)

    cf_ols <- coef(ols_fit_same)
    cf_iv  <- coef(iv_fit)
    # HAC (Newey-West) SEs for both fits, matching the headline convention;
    # plain ivreg/lm vcov understates SEs under the serial correlation the
    # diagnostics document.
    nw_lag <- floor(4 * (nrow(iv_data_full) / 100)^(2 / 9))
    se_ols <- sqrt(diag(tryCatch(
      sandwich::NeweyWest(ols_fit_same, lag = nw_lag, prewhite = FALSE,
                          adjust = TRUE),
      error = function(e) vcov(ols_fit_same))))
    se_iv  <- sqrt(diag(tryCatch(
      sandwich::NeweyWest(iv_fit, lag = nw_lag, prewhite = FALSE,
                          adjust = TRUE),
      error = function(e) vcov(iv_fit))))

    iv_tbl <- tibble::tibble(
      specification    = spec_name,
      term             = names(cf_ols),
      ols_estimate     = unname(cf_ols),
      ols_se           = unname(se_ols[names(cf_ols)]),
      iv_estimate      = unname(cf_iv[names(cf_ols)]),
      iv_se            = unname(se_iv [names(cf_ols)]),
      delta_iv_minus_ols = iv_estimate - ols_estimate,
      pct_change       = 100 * (iv_estimate - ols_estimate) / ols_estimate,
      n_obs            = nrow(iv_data_full),
      n_instruments    = length(instruments),
      endogenous_terms = paste(endog, collapse = ", ")
    )
    write.csv(iv_tbl, iv_path, row.names = FALSE)
    message(sprintf("[run_italy_style_robustness] IV table saved: %s", iv_path))

    # First-stage strength + standard IV diagnostics (weak instruments,
    # Wu-Hausman, Sargan) — previously not computed anywhere.
    iv_diag <- tryCatch({
      sm <- summary(iv_fit, diagnostics = TRUE)
      dg <- as.data.frame(sm$diagnostics)
      dg$diagnostic <- rownames(sm$diagnostics)
      tibble::as_tibble(dg) %>%
        mutate(specification = spec_name) %>%
        select(specification, diagnostic, dplyr::everything())
    }, error = function(e) {
      tibble::tibble(specification = spec_name, diagnostic = "FAILED",
                     error = conditionMessage(e))
    })
    iv_diag_path <- suffixed("australia_iv_diagnostics.csv")
    write.csv(iv_diag, iv_diag_path, row.names = FALSE)
    message(sprintf("[run_italy_style_robustness] IV diagnostics saved: %s",
                    iv_diag_path))
  }, error = function(e) {
    message(sprintf("  [IV block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), iv_path, row.names = FALSE)
  })

  # ----------------------------------------------------------------------
  # (2) Joint PI + consumption FIML (SUR)
  # ----------------------------------------------------------------------
  joint_path <- suffixed("australia_joint_pi_robustness.csv")
  tryCatch({
    if (!requireNamespace("systemfit", quietly = TRUE))
      stop("systemfit package not installed — install.packages('systemfit')")

    # PI equation: regress lincome on its own AR(8) + trend + break + optional
    # extras (matching construct_permanent_income's RHS).  We use a smaller,
    # estimable subset that's stable on the same sample.
    pi_data <- model_data %>%
      arrange(date) %>%
      mutate(
        t_index = seq_len(n()),
        step2008 = as.integer(date >= as.Date("2008-07-01")),
        trend_brk = t_index * step2008,
        inc_l1 = lag(lincome, 1L),
        inc_l2 = lag(lincome, 2L),
        inc_l4 = lag(lincome, 4L),
        inc_l8 = lag(lincome, 8L)
      )

    pi_rhs <- c("t_index", "step2008", "trend_brk",
                "inc_l1", "inc_l2", "inc_l4", "inc_l8", "unemp_rate")
    pi_fmla <- reformulate(pi_rhs, response = "lincome")

    sur_data <- pi_data %>%
      filter(date >= min(est_data$date),
             date <= max(est_data$date)) %>%
      filter(complete.cases(across(all_of(c(response, rhs_terms, pi_rhs)))))

    if (nrow(sur_data) < 30L)
      stop(sprintf("only %d complete obs for SUR", nrow(sur_data)))

    cons_fmla <- reformulate(rhs_terms, response = response)

    # systemfit forbids blanks/underscores in equation labels
    sur_fit <- systemfit::systemfit(
      formula = list(consumption = cons_fmla, perminc = pi_fmla),
      method  = "SUR",
      data    = sur_data
    )

    # Re-fit consumption single-equation OLS on the SAME sample
    ols_same <- lm(cons_fmla, data = sur_data)

    cons_eq_idx <- 1L
    cf_sur <- sur_fit$eq[[cons_eq_idx]]$coefficients
    se_sur <- sqrt(diag(sur_fit$eq[[cons_eq_idx]]$coefCov))

    cf_ols <- coef(ols_same)
    se_ols <- sqrt(diag(vcov(ols_same)))

    joint_tbl <- tibble::tibble(
      specification    = spec_name,
      equation         = "consumption",
      term             = names(cf_ols),
      single_eq_ols    = unname(cf_ols),
      single_eq_se     = unname(se_ols),
      sur_joint        = unname(cf_sur[names(cf_ols)]),
      sur_joint_se     = unname(se_sur[names(cf_ols)]),
      delta_sur_minus_ols = sur_joint - single_eq_ols,
      pct_change       = 100 * (sur_joint - single_eq_ols) / single_eq_ols,
      n_obs            = nrow(sur_data)
    )
    write.csv(joint_tbl, joint_path, row.names = FALSE)
    message(sprintf("[run_italy_style_robustness] Joint PI+cons table saved: %s",
                    joint_path))
  }, error = function(e) {
    message(sprintf("  [Joint SUR block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), joint_path, row.names = FALSE)
  })

  # ----------------------------------------------------------------------
  # (3) Chow battery at multiple sample cuts
  # ----------------------------------------------------------------------
  chow_path <- suffixed("australia_chow_battery.csv")
  tryCatch({
    break_dates <- as.Date(c("1995-01-01", "2000-01-01",
                             "2008-07-01", "2020-01-01"))
    fit_dates <- est_data$date
    n_full    <- nobs(base_fit)
    chow_fmla <- formula(base_fit)
    chow_rows <- lapply(break_dates, function(bd) {
      bp_idx <- sum(fit_dates <= bd, na.rm = TRUE)
      if (bp_idx < 5L || bp_idx > (n_full - 5L)) {
        return(tibble::tibble(
          specification = spec_name, break_date = bd,
          chow_stat = NA_real_, chow_pval = NA_real_,
          parameters_stable_5pct = NA, n_pre = bp_idx,
          n_post = n_full - bp_idx,
          note = "break too close to sample edge"
        ))
      }
      # Use formula method: sctest(formula, type = "Chow", point = bp_idx)
      # supports the `point` argument; the fit method does not.
      sct <- tryCatch(
        strucchange::sctest(chow_fmla, type = "Chow",
                            point = bp_idx, data = est_data),
        error = function(e) NULL
      )
      if (is.null(sct)) {
        tibble::tibble(
          specification = spec_name, break_date = bd,
          chow_stat = NA_real_, chow_pval = NA_real_,
          parameters_stable_5pct = NA, n_pre = bp_idx,
          n_post = n_full - bp_idx,
          note = "sctest failed"
        )
      } else {
        tibble::tibble(
          specification = spec_name, break_date = bd,
          chow_stat = unname(as.numeric(sct$statistic)),
          chow_pval = as.numeric(sct$p.value),
          parameters_stable_5pct = as.numeric(sct$p.value) > 0.05,
          n_pre = bp_idx,
          n_post = n_full - bp_idx,
          note = ""
        )
      }
    })
    chow_tbl <- bind_rows(chow_rows)
    write.csv(chow_tbl, chow_path, row.names = FALSE)
    message(sprintf("[run_italy_style_robustness] Chow battery saved: %s",
                    chow_path))
  }, error = function(e) {
    message(sprintf("  [Chow block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), chow_path, row.names = FALSE)
  })

  # ----------------------------------------------------------------------
  # (4) Scaled-income alternative
  # ----------------------------------------------------------------------
  scaled_path <- suffixed("australia_scaled_income_robustness.csv")
  tryCatch({
    labour_col <- intersect(
      c("labour_transfer_income_real_pc",
        "labour_transfer_real_pc",
        "compensation_of_employees_real_pc",
        "wages_real_pc"),
      names(model_data)
    )
    if (length(labour_col) == 0L) {
      message("  [Scaled-income block] No labour-income series in model_data ",
              "(checked: labour_transfer_income_real_pc, ",
              "compensation_of_employees_real_pc, wages_real_pc).  Skipping ",
              "Italy-style scaled-income variant; documented in CSV.")
      write.csv(tibble::tibble(
        specification = spec_name, status = "SKIPPED",
        reason = paste0(
          "No clean labour+transfer income series available in current ",
          "Australia data pipeline (ABS 5206020 wages component not extracted ",
          "into model_data).  Italy uses average of labour+transfer and total ",
          "disposable income; only ydi_real_pc is currently available.  ",
          "Cannot construct scaled_income without fabrication.")
      ), scaled_path, row.names = FALSE)
    } else {
      lcol <- labour_col[1L]
      scaled_data <- model_data %>%
        arrange(date) %>%
        mutate(
          scaled_income_real_pc = 0.5 * ydi_real_pc + 0.5 * .data[[lcol]],
          lincome_scaled        = log(scaled_income_real_pc),
          # Provide both conventions so the substitution below works regardless
          # of whether the spec uses canonical ecm_lag or legacy ln_y_over_c.
          ln_y_over_c_scaled    = lincome_scaled - lag(lcons, 1L),
          ecm_lag_scaled        = lag(lcons, 1L) - lincome_scaled
        )

      # Replace WHICHEVER ECM term the spec uses with its scaled counterpart.
      rhs_terms_scaled <- rhs_terms
      rhs_terms_scaled <- gsub("\\bln_y_over_c\\b", "ln_y_over_c_scaled",
                               rhs_terms_scaled, perl = TRUE)
      rhs_terms_scaled <- gsub("\\becm_lag\\b", "ecm_lag_scaled",
                               rhs_terms_scaled, perl = TRUE)
      # Also swap any direct lincome references (some specs may reference it
      # via ln_yp_over_y constructions, but those are pre-built — skip).
      fmla_scaled <- reformulate(rhs_terms_scaled, response = response)

      scaled_data_cc <- scaled_data %>%
        filter(date >= min(est_data$date), date <= max(est_data$date)) %>%
        filter(complete.cases(across(all_of(c(response, rhs_terms_scaled)))))

      fit_scaled <- lm(fmla_scaled, data = scaled_data_cc)
      cf_scaled  <- coef(fit_scaled)
      se_scaled  <- sqrt(diag(vcov(fit_scaled)))
      cf_base    <- coef(base_fit)
      se_base    <- sqrt(diag(vcov(base_fit)))

      align_name_scaled <- function(x) {
        x <- gsub("ln_y_over_c_scaled", "ln_y_over_c", x)
        gsub("ecm_lag_scaled", "ecm_lag", x)
      }
      cf_scaled_named <- setNames(cf_scaled, align_name_scaled(names(cf_scaled)))
      se_scaled_named <- setNames(se_scaled, align_name_scaled(names(se_scaled)))

      nm <- union(names(cf_base), names(cf_scaled_named))

      scaled_tbl <- tibble::tibble(
        specification     = spec_name,
        term              = nm,
        base_estimate     = unname(cf_base       [match(nm, names(cf_base))]),
        base_se           = unname(se_base       [match(nm, names(se_base))]),
        scaled_estimate   = unname(cf_scaled_named[match(nm, names(cf_scaled_named))]),
        scaled_se         = unname(se_scaled_named[match(nm, names(se_scaled_named))]),
        labour_income_var = lcol,
        n_obs             = nrow(scaled_data_cc)
      )
      write.csv(scaled_tbl, scaled_path, row.names = FALSE)
      message(sprintf("[run_italy_style_robustness] Scaled-income table saved: %s",
                      scaled_path))
    }
  }, error = function(e) {
    message(sprintf("  [Scaled-income block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), scaled_path, row.names = FALSE)
  })

  # ----------------------------------------------------------------------
  # (4b) Williams (2009/2010) non-property income alternative
  # Replaces ydi_real_pc with npy_real_pc — the income measure used in
  # Williams's Australia LIVES estimation. NPY strips imputed property
  # income (and a corresponding share of property-related tax) from gross
  # disposable income. This is a different choice from Italy's scaled
  # income (which is a 50/50 average); Williams uses NPY directly.
  # ----------------------------------------------------------------------
  npy_path <- suffixed("australia_williams_income_robustness.csv")
  tryCatch({
    if (!"npy_real_pc" %in% names(model_data)) {
      message("  [Williams-income block] npy_real_pc not in model_data; ",
              "skipping (need ABS 5206020 components from household_income.csv)")
      write.csv(tibble::tibble(
        specification = spec_name, status = "SKIPPED",
        reason = "npy_real_pc not constructed in current data pipeline"
      ), npy_path, row.names = FALSE)
    } else {
      npy_data <- model_data %>%
        arrange(date) %>%
        mutate(
          lincome_npy        = log(pmax(npy_real_pc, 1e-9)),
          ln_y_over_c_npy    = lincome_npy - lag(lcons, 1L),
          ecm_lag_npy        = lag(lcons, 1L) - lincome_npy
        )
      rhs_terms_npy <- rhs_terms
      rhs_terms_npy <- gsub("\\bln_y_over_c\\b", "ln_y_over_c_npy",
                            rhs_terms_npy, perl = TRUE)
      rhs_terms_npy <- gsub("\\becm_lag\\b", "ecm_lag_npy",
                            rhs_terms_npy, perl = TRUE)
      fmla_npy <- reformulate(rhs_terms_npy, response = response)

      npy_data_cc <- npy_data %>%
        filter(date >= min(est_data$date), date <= max(est_data$date)) %>%
        filter(complete.cases(across(all_of(c(response, rhs_terms_npy)))))

      fit_npy <- lm(fmla_npy, data = npy_data_cc)
      cf_npy  <- coef(fit_npy)
      se_npy  <- sqrt(diag(vcov(fit_npy)))
      cf_base <- coef(base_fit)
      se_base <- sqrt(diag(vcov(base_fit)))

      align_npy <- function(x) {
        x <- gsub("ln_y_over_c_npy", "ln_y_over_c", x)
        gsub("ecm_lag_npy", "ecm_lag", x)
      }
      cf_npy_named <- setNames(cf_npy, align_npy(names(cf_npy)))
      se_npy_named <- setNames(se_npy, align_npy(names(se_npy)))

      nm <- union(names(cf_base), names(cf_npy_named))
      npy_tbl <- tibble::tibble(
        specification = spec_name,
        term          = nm,
        base_estimate = unname(cf_base       [match(nm, names(cf_base))]),
        base_se       = unname(se_base       [match(nm, names(se_base))]),
        npy_estimate  = unname(cf_npy_named  [match(nm, names(cf_npy_named))]),
        npy_se        = unname(se_npy_named  [match(nm, names(se_npy_named))]),
        income_var    = "npy_real_pc (Williams 2009 §4.2.1)",
        n_obs         = nrow(npy_data_cc)
      )
      write.csv(npy_tbl, npy_path, row.names = FALSE)
      message(sprintf("[run_italy_style_robustness] Williams-income table saved: %s",
                      npy_path))
    }
  }, error = function(e) {
    message(sprintf("  [Williams-income block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), npy_path, row.names = FALSE)
  })

  # ----------------------------------------------------------------------
  # (5) Drehmann amortising-mortgage-adjusted real rate
  # adjR = R / (1 - (1+R)^-N), with N = 100 quarters (25 years for Australia)
  # ----------------------------------------------------------------------
  drehmann_path <- suffixed("australia_drehmann_robustness.csv")
  tryCatch({
    if (!"mortgage_rate" %in% names(model_data) ||
        !"hicp_4q_ann"   %in% names(model_data))
      stop("mortgage_rate or hicp_4q_ann missing from model_data")

    # mortgage_rate is reported as % per annum.  Convert to per-quarter rate
    # for the amortisation formula (geometric: (1+R_q) = (1 + R_a/100)^(1/4)).
    drehmann_data <- model_data %>%
      arrange(date) %>%
      mutate(
        r_q                       = (1 + mortgage_rate / 100)^(1/4) - 1,
        mortgage_rate_drehmann_q  = ifelse(r_q == 0, 0,
                                           r_q / (1 - (1 + r_q)^(-100))),
        mortgage_rate_drehmann_a  = ((1 + mortgage_rate_drehmann_q)^4 - 1) * 100,
        real_rate_drehmann        = mortgage_rate_drehmann_a - hicp_4q_ann
      )

    if (!"real_rate" %in% rhs_terms) {
      stop("preferred spec has no real_rate term — cannot substitute Drehmann")
    }

    rhs_terms_dre <- gsub("\\breal_rate\\b", "real_rate_drehmann",
                          rhs_terms, perl = TRUE)
    fmla_dre <- reformulate(rhs_terms_dre, response = response)

    dre_data_cc <- drehmann_data %>%
      filter(date >= min(est_data$date), date <= max(est_data$date)) %>%
      filter(complete.cases(across(all_of(c(response, rhs_terms_dre)))))

    fit_dre <- lm(fmla_dre, data = dre_data_cc)
    cf_dre  <- coef(fit_dre)
    se_dre  <- sqrt(diag(vcov(fit_dre)))
    cf_base <- coef(base_fit)
    se_base <- sqrt(diag(vcov(base_fit)))

    align_name <- function(x) gsub("real_rate_drehmann", "real_rate", x)
    cf_dre_named <- setNames(cf_dre, align_name(names(cf_dre)))
    se_dre_named <- setNames(se_dre, align_name(names(se_dre)))

    nm <- union(names(cf_base), names(cf_dre_named))

    dre_tbl <- tibble::tibble(
      specification     = spec_name,
      term              = nm,
      base_estimate     = unname(cf_base    [match(nm, names(cf_base))]),
      base_se           = unname(se_base    [match(nm, names(se_base))]),
      drehmann_estimate = unname(cf_dre_named[match(nm, names(cf_dre_named))]),
      drehmann_se       = unname(se_dre_named[match(nm, names(se_dre_named))]),
      n_obs             = nrow(dre_data_cc),
      maturity_years    = 25,
      maturity_quarters = 100
    )
    write.csv(dre_tbl, drehmann_path, row.names = FALSE)
    message(sprintf("[run_italy_style_robustness] Drehmann table saved: %s",
                    drehmann_path))
  }, error = function(e) {
    message(sprintf("  [Drehmann block] FAILED: %s", conditionMessage(e)))
    write.csv(tibble::tibble(
      specification = spec_name, status = "FAILED",
      error = conditionMessage(e)
    ), drehmann_path, row.names = FALSE)
  })

  invisible(NULL)
}


# ==============================================================================
# SECTION K: Long-run Contributions Decomposition Plot
# ==============================================================================
#
# Headline policy chart.  For the preferred spec, decompose fitted log(c/y) into
# the de-meaned contribution of each long-run regressor:
#
#   contribution_t(j) = beta_j * (X_jt - mean(X_j))
#
# where beta_j = OLS coefficient on X_j divided by lambda (the structural param).
#
# Stacked area plot (geom_area, position = "stack") plus a thin line for the
# residual = actual log(c/y) - sum of contributions.  Saved as 16x9 PNG at
# 200 dpi for slide-quality.
# ==============================================================================

run_counterfactuals <- function(specs_full, output_dir) {
  # Three §10.2 NS-012 counterfactuals on the headline specifications:
  #   CF1: no-APRA  — zero out d_apra_2014, d_apra_2017 in Spec 6.
  #   CF2: no-COVID — zero out d_jobkeeper_2020, d2020_covid, d2020_rebound
  #                   in Spec 6.
  #   CF3: CCI peak vs zero — Spec 8 with the four CCI-interacted regressors
  #                   re-evaluated at CCI = 1 (Williams' peak) and CCI = 0
  #                   (no liberalisation), holding other regressors at their
  #                   observed values.
  # Outputs:
  #   australia_counterfactuals.csv          long-format date × scenario × value
  #   australia_counterfactual_paths.png     comparison chart

  modify_X_and_refit <- function(spec, modifications) {
    fit  <- spec$fit
    est  <- spec$est_data
    cf   <- coef(fit)
    X    <- model.matrix(formula(fit), est)
    X_cf <- X
    for (nm in names(modifications)) {
      if (nm %in% colnames(X_cf)) {
        X_cf[, nm] <- modifications[[nm]]
      }
    }
    fitted_cf       <- as.numeric(X_cf  %*% cf[colnames(X_cf)])
    fitted_baseline <- as.numeric(X     %*% cf[colnames(X)])
    list(date         = est$date,
         baseline_dlc = fitted_baseline,
         cf_dlc       = fitted_cf)
  }

  scenarios <- list()

  # ---- Counterfactual 1: no-APRA on Spec 6 -----------------------------
  spec6 <- specs_full$spec6
  if (!is.null(spec6)) {
    cf1 <- modify_X_and_refit(spec6,
              list(d_apra_2014 = 0, d_apra_2017 = 0))
    scenarios$no_apra <- tibble::tibble(
      date     = cf1$date,
      scenario = "no_apra_2014_2017",
      dlog_c_baseline = cf1$baseline_dlc,
      dlog_c_cf       = cf1$cf_dlc,
      delta_dlog_c    = cf1$cf_dlc - cf1$baseline_dlc
    ) %>% mutate(cum_delta_log_c = cumsum(delta_dlog_c))
  }

  # ---- Counterfactual 2: no-COVID-support on Spec 6 --------------------
  if (!is.null(spec6)) {
    cf2 <- modify_X_and_refit(spec6,
              list(d_jobkeeper_2020 = 0,
                   d2020_covid      = 0,
                   d2020_rebound    = 0))
    scenarios$no_covid_support <- tibble::tibble(
      date     = cf2$date,
      scenario = "no_covid_support",
      dlog_c_baseline = cf2$baseline_dlc,
      dlog_c_cf       = cf2$cf_dlc,
      delta_dlog_c    = cf2$cf_dlc - cf2$baseline_dlc
    ) %>% mutate(cum_delta_log_c = cumsum(delta_dlog_c))
  }

  # ---- Counterfactual 3: CCI peak vs zero on Spec 8 --------------------
  # Spec 8's CCI interactions are constructed on de-meaned levels. The
  # composite design therefore depends on the in-sample means of the
  # interacted variables; we recover those from the est_data and rebuild
  # the four interaction columns at the counterfactual CCI value.
  spec8 <- specs_full$spec8
  if (!is.null(spec8) && all(c("ha_x_cci", "hp_x_1_minus_cci",
                                "r_x_cci", "yp_x_cci")
                              %in% names(coef(spec8$fit)))) {
    est <- spec8$est_data
    ha_mean <- mean(est$ha_y,         na.rm = TRUE)
    hp_mean <- mean(est$ln_hp_over_y, na.rm = TRUE)
    r_mean  <- mean(est$real_rate,    na.rm = TRUE)
    yp_mean <- mean(est$ln_yp_over_y, na.rm = TRUE)

    make_cci_interactions <- function(cci_value) {
      list(
        ha_x_cci         = (est$ha_y         - ha_mean) * cci_value,
        r_x_cci          = (est$real_rate    - r_mean)  * cci_value,
        yp_x_cci         = (est$ln_yp_over_y - yp_mean) * cci_value,
        hp_x_1_minus_cci = (est$ln_hp_over_y - hp_mean) *
                           (1 - 1.2 * cci_value)
      )
    }

    cf3_peak <- modify_X_and_refit(spec8, make_cci_interactions(1))
    cf3_zero <- modify_X_and_refit(spec8, make_cci_interactions(0))

    scenarios$cci_peak <- tibble::tibble(
      date            = cf3_peak$date,
      scenario        = "cci_at_peak",
      dlog_c_baseline = cf3_peak$baseline_dlc,
      dlog_c_cf       = cf3_peak$cf_dlc,
      delta_dlog_c    = cf3_peak$cf_dlc - cf3_peak$baseline_dlc
    ) %>% mutate(cum_delta_log_c = cumsum(delta_dlog_c))

    scenarios$cci_zero <- tibble::tibble(
      date            = cf3_zero$date,
      scenario        = "cci_at_zero",
      dlog_c_baseline = cf3_zero$baseline_dlc,
      dlog_c_cf       = cf3_zero$cf_dlc,
      delta_dlog_c    = cf3_zero$cf_dlc - cf3_zero$baseline_dlc
    ) %>% mutate(cum_delta_log_c = cumsum(delta_dlog_c))
  }

  if (length(scenarios) == 0L) {
    message("[run_counterfactuals] no scenarios computable — skipping outputs")
    return(invisible(NULL))
  }

  scen_long <- bind_rows(scenarios)
  csv_path <- file.path(output_dir, "australia_counterfactuals.csv")
  write.csv(scen_long, csv_path, row.names = FALSE)
  message(sprintf("[run_counterfactuals] CSV saved: %s (%d rows)",
                  csv_path, nrow(scen_long)))

  # Headline summary: cumulative consumption impact at +4 and +8 quarters
  # post the relevant event quarter, and at end of sample.
  fmt_pct <- function(x) sprintf("%+.2f%%", 100 * x)

  pick_h <- function(df, event_date, h) {
    target <- as.Date(event_date) + months(3 * h)
    sub <- df %>% filter(date >= as.Date(event_date) & date <= target)
    if (nrow(sub) < h) return(NA_real_)
    sub$cum_delta_log_c[nrow(sub)] - sub$cum_delta_log_c[1]
  }

  summary_rows <- list()
  if (!is.null(scenarios$no_apra)) {
    df <- scenarios$no_apra
    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      scenario = "no_apra_2014_2017",
      event_date = "2014-12-01",
      h4_log_c_gap  = pick_h(df, "2014-12-01", 4),
      h8_log_c_gap  = pick_h(df, "2014-12-01", 8),
      eos_log_c_gap = df$cum_delta_log_c[nrow(df)] -
                      df$cum_delta_log_c[which(df$date >= as.Date("2014-12-01"))[1]]
    )
  }
  if (!is.null(scenarios$no_covid_support)) {
    df <- scenarios$no_covid_support
    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      scenario = "no_covid_support",
      event_date = "2020-03-01",
      h4_log_c_gap  = pick_h(df, "2020-03-01", 4),
      h8_log_c_gap  = pick_h(df, "2020-03-01", 8),
      eos_log_c_gap = df$cum_delta_log_c[nrow(df)] -
                      df$cum_delta_log_c[which(df$date >= as.Date("2020-03-01"))[1]]
    )
  }
  if (!is.null(scenarios$cci_peak) && !is.null(scenarios$cci_zero)) {
    df_peak <- scenarios$cci_peak
    df_zero <- scenarios$cci_zero
    summary_rows[[length(summary_rows) + 1L]] <- tibble::tibble(
      scenario = "cci_peak_vs_zero",
      event_date = "1988-12-01",
      h4_log_c_gap  = NA_real_,
      h8_log_c_gap  = NA_real_,
      eos_log_c_gap = (df_peak$cum_delta_log_c[nrow(df_peak)]) -
                      (df_zero$cum_delta_log_c[nrow(df_zero)])
    )
  }
  if (length(summary_rows) > 0L) {
    summary_tbl <- bind_rows(summary_rows)
    csv2_path <- file.path(output_dir, "australia_counterfactuals_summary.csv")
    write.csv(summary_tbl, csv2_path, row.names = FALSE)
    message(sprintf("[run_counterfactuals] summary CSV saved: %s", csv2_path))
  }

  # Chart: cumulative consumption deviation (log points) for each scenario
  scenarios_for_plot <- scen_long %>%
    filter(scenario %in% c("no_apra_2014_2017",
                            "no_covid_support",
                            "cci_at_peak",
                            "cci_at_zero")) %>%
    mutate(scenario_label = factor(
      scenario,
      levels = c("no_apra_2014_2017", "no_covid_support",
                  "cci_at_peak", "cci_at_zero"),
      labels = c("No 2014/17 APRA macroprudential",
                  "No COVID income support",
                  "CCI at Williams' peak (=1)",
                  "CCI at zero (no liberalisation)")
    ))

  p <- ggplot(scenarios_for_plot,
              aes(x = date, y = 100 * cum_delta_log_c,
                  colour = scenario_label)) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
    geom_line(linewidth = 0.75) +
    scale_colour_manual(
      name = NULL,
      values = c(
        "No 2014/17 APRA macroprudential"   = "#1f78b4",
        "No COVID income support"           = "#e31a1c",
        "CCI at Williams' peak (=1)"        = "#33a02c",
        "CCI at zero (no liberalisation)"   = "#fdae61"
      )) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(
      title    = "Australia: policy counterfactuals on log(c) — cumulative deviation from baseline",
      subtitle = "Counterfactuals 1–2 on Spec 6 (preferred); counterfactual 3 on Spec 8 (CCI interactions)",
      x = NULL,
      y = "Cumulative deviation from baseline (log points × 100)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30"))

  png_path <- file.path(output_dir, "australia_counterfactual_paths.png")
  ggsave(png_path, p, width = 16, height = 9, dpi = 200)
  message(sprintf("[run_counterfactuals] PNG saved: %s", png_path))

  invisible(scen_long)
}

plot_longrun_decomposition <- function(preferred_spec, output_dir,
                                       file_suffix = "") {

  spec_name <- preferred_spec$spec_name
  est_data  <- preferred_spec$est_data
  fit       <- preferred_spec$fit
  lr_vars   <- preferred_spec$lr_vars
  cf        <- coef(fit)

  ecm_term <- if ("ecm_lag" %in% names(cf)) "ecm_lag" else
              if ("ln_y_over_c" %in% names(cf)) "ln_y_over_c" else NA_character_
  if (is.na(ecm_term))
    stop("[plot_longrun_decomposition] no ECM term (ecm_lag or ln_y_over_c) in preferred spec")

  lambda <- cf[ecm_term]
  if (is.na(lambda) || abs(lambda) < 1e-10)
    stop("[plot_longrun_decomposition] lambda near zero — cannot rescale")

  # Long-run regressors EXCLUDING the ECM term (ln_y_over_c itself); contributions
  # are de-meaned and scaled by structural coef = -OLS / lambda (canonical
  # negative-lambda convention: build_results_table :2325, gamma_inference.R).
  decomp_vars <- setdiff(lr_vars, c("ln_y_over_c", "ecm_lag"))
  decomp_vars <- intersect(decomp_vars, names(est_data))
  decomp_vars <- decomp_vars[!vapply(est_data[, decomp_vars, drop = FALSE],
                                     function(x) all(is.na(x)), logical(1))]
  if (length(decomp_vars) == 0L)
    stop("[plot_longrun_decomposition] no usable long-run vars in preferred spec")

  # Compute contributions
  contrib_mat <- vapply(decomp_vars, function(v) {
    x        <- est_data[[v]]
    beta_str <- -unname(cf[v]) / unname(lambda)
    beta_str * (x - mean(x, na.rm = TRUE))
  }, numeric(nrow(est_data)))

  # log(c/y) actual: lcons - lincome (note ln_y_over_c = lincome - lag(lcons))
  if (all(c("lcons", "lincome") %in% names(est_data))) {
    log_c_over_y_actual <- est_data$lcons - est_data$lincome
  } else if ("ln_y_over_c" %in% names(est_data)) {
    # fallback: use -ln_y_over_c (lag-shifted by construction)
    log_c_over_y_actual <- -est_data$ln_y_over_c
  } else {
    stop("[plot_longrun_decomposition] no lcons/lincome in est_data")
  }
  log_c_over_y_actual_dem <- log_c_over_y_actual - mean(log_c_over_y_actual,
                                                        na.rm = TRUE)

  sum_contrib <- rowSums(contrib_mat)
  residual    <- log_c_over_y_actual_dem - sum_contrib

  # Long-format CSV
  long_df <- bind_rows(lapply(seq_along(decomp_vars), function(i) {
    tibble::tibble(
      date         = est_data$date,
      term         = decomp_vars[i],
      contribution = contrib_mat[, i]
    )
  }))
  long_df <- bind_rows(
    long_df,
    tibble::tibble(date = est_data$date, term = "actual_demeaned",
                   contribution = log_c_over_y_actual_dem),
    tibble::tibble(date = est_data$date, term = "residual",
                   contribution = residual)
  )

  csv_path <- file.path(output_dir,
                        sprintf("australia_longrun_contributions%s.csv",
                                file_suffix))
  write.csv(long_df, csv_path, row.names = FALSE)
  message(sprintf("[plot_longrun_decomposition] CSV saved: %s (%d rows)",
                  csv_path, nrow(long_df)))

  # Colour palette by economic role
  term_palette <- c(
    nla_y                 = "#1f78b4",  # blue (credit / liquid)
    eq_y                  = "#33a02c",  # green (financial wealth)
    super_y               = "#b2df8a",  # light green (long-horizon wealth)
    ha_y                  = "#006d2c",  # dark green (housing wealth, level)
    ln_hp_over_y          = "#fdae61",  # orange (housing affordability)
    ilfa_y                = "#a6cee3",  # pale blue (illiquid financial)
    networth_y            = "#33a02c",
    ln_networth_y         = "#33a02c",
    bonds_y               = "#0570b0",
    real_rate             = "#e31a1c",  # red (interest rate)
    ln_yp_over_y          = "#6a3d9a",  # purple (permanent income)
    ln_yp_over_y_post2008 = "#cab2d6",  # light purple (PI x post-2008)
    cohort_share          = "#8c510a",  # brown (demographics)
    burden_y              = "#bf812d"   # brown (debt burden)
  )

  # Stacked-area data: drop actual + residual, keep only term contributions
  area_df <- long_df %>%
    filter(!term %in% c("actual_demeaned", "residual")) %>%
    mutate(term = factor(term, levels = decomp_vars))

  resid_df <- long_df %>% filter(term == "residual")

  fmt_q <- function(d) {
    sprintf("%dQ%d", lubridate::year(d), lubridate::quarter(d))
  }
  sample_label <- sprintf("%s to %s",
                          fmt_q(min(est_data$date)),
                          fmt_q(max(est_data$date)))

  fill_cols <- term_palette[as.character(levels(area_df$term))]
  miss <- is.na(fill_cols)
  if (any(miss))
    fill_cols[miss] <- scales::hue_pal()(sum(miss))

  p <- ggplot() +
    geom_area(data = area_df,
              aes(x = date, y = contribution, fill = term),
              position = "stack", alpha = 0.9) +
    geom_line(data = resid_df,
              aes(x = date, y = contribution),
              colour = "grey20", linewidth = 0.4) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
    scale_fill_manual(values = fill_cols, name = "Long-run term") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(
      title    = sprintf("Australia: Long-run contributions to log(c/y) — preferred spec %s",
                         spec_name),
      subtitle = sprintf("Sample window: %s  (N = %d quarters)",
                         sample_label, nobs(fit)),
      caption  = paste("De-meaned contributions of each long-run term to log(c/y);",
                       "residual = actual − sum of contributions."),
      x = NULL,
      y = "De-meaned log(c/y)"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey30"),
          plot.caption  = element_text(colour = "grey40"))

  png_path <- file.path(output_dir,
                        sprintf("australia_longrun_decomposition%s.png",
                                file_suffix))
  ggsave(png_path, p, width = 16, height = 9, dpi = 200)
  message(sprintf("[plot_longrun_decomposition] PNG saved: %s", png_path))

  invisible(list(plot = p, data = long_df, decomp_vars = decomp_vars,
                 lambda = unname(lambda)))
}


# ==============================================================================
# MAIN EXECUTION
# ==============================================================================

cat("[Step 1] Constructing dummy and short-run variables...\n")
model_data <- add_model_variables(model_data)

cat("[Step 2] Computing income volatility (AR8 residuals)...\n")
model_data <- compute_income_volatility(model_data)

cat(sprintf("[Step 3] Constructing permanent income series (PI_METHOD = '%s')...\n",
            PI_METHOD))
model_data <- if (identical(PI_METHOD, "italy")) {
  construct_permanent_income_italy(model_data)
} else {
  construct_permanent_income(model_data)
}

# Add variables that depend on permanent income
model_data <- model_data %>%
  mutate(
    # Re-assert canonical negative-restoration ECM term post-PI construction.
    # ln_hp_over_y is taken from the master dataset (nominal hp / nominal
    # income pc = real/real); the previous overwrite here used a
    # deflator-contaminated formula and is deliberately removed.
    ecm_lag = lag(lcons, 1L) - lincome
  )

n_pi <- sum(!is.na(model_data$ln_yp_over_y))
message(sprintf("[construct_permanent_income] ln_yp_over_y: %d non-NA obs", n_pi))

cat("[Step 4] Estimating specifications across four sample variants...\n")
# Order-of-execution note (Williams CCI iteration):
#   (i)   Specs 1-6 are independent of cci_williams; Spec 8 needs it.
#   (ii)  Fit the maximal (15-knot) SDMMA spline inside the Spec-4 disaggregated
#         long-run on the FULL sample. Sign-prior reduction (Hendry/Krolzig
#         2005) drops knots whose OLS coefficient violates its institutional
#         sign prior; the surviving combination defines cci_williams.
#   (iii) Attach cci_williams to model_data so Spec 8 can use it in the spec
#         loop with (r_x_cci, hp_x_1_minus_cci, yp_x_cci).
WILLIAMS_FALLBACK <- FALSE
williams_fit_info <- NULL
if (any(grepl("^sdmma_", names(model_data)))) {
  cat("  [4a-i] Fitting Williams-style maximal (15-knot) CCI spline (Spec-4 long-run)\n")
  base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                    "d_neg_gearing_8587", "d_recession_1991",
                    "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
  williams_fit_info <- tryCatch(
    fit_consumption_with_williams_cci(
      model_data,
      lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                     "real_rate", "ln_yp_over_y", "ecm_lag"),
      sr_vars    = character(0),
      dummy_vars = base_dummies,
      sample_end = as.Date("2024-10-01")
    ),
    error = function(e) {
      message("[Williams CCI] fit failed: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(williams_fit_info)) {
    surv  <- williams_fit_info$surviving_knots
    drop  <- williams_fit_info$dropped_knots
    alias <- williams_fit_info$aliased_knots
    cat(sprintf("  [4a-i] Surviving knots: %s\n",
                if (length(surv) == 0L) "(none)" else paste(surv, collapse = ", ")))
    cat(sprintf("  [4a-i] Dropped (sign-violators): %s\n",
                if (length(drop) == 0L) "(none)" else paste(drop, collapse = ", ")))
    cat(sprintf("  [4a-i] Aliased (collinear / constant in sample): %s\n",
                if (length(alias) == 0L) "(none)" else paste(alias, collapse = ", ")))

    knot_csv <- tibble::tibble(
      knot         = williams_fit_info$knot_names,
      sign_prior   = williams_fit_info$sign_priors,
      survived     = williams_fit_info$knot_names %in% surv,
      sign_violator = williams_fit_info$knot_names %in% drop,
      aliased      = williams_fit_info$knot_names %in% alias,
      ols_estimate = sapply(williams_fit_info$knot_names, function(nm) {
        cf <- coef(williams_fit_info$spec$fit)
        if (nm %in% names(cf)) unname(cf[nm]) else NA_real_
      })
    )
    write.csv(knot_csv,
              file.path(output_dir, "australia_williams_cci_knots.csv"),
              row.names = FALSE)

    if (length(surv) == 0L) {
      WILLIAMS_FALLBACK <- TRUE
      message("[Williams CCI] All knots dropped by sign-prior reduction; ",
              "the reduced-form Williams CCI is not viable on the 1988Q4+ ",
              "sample. Consider Item 9 (sample back-extension) as a ",
              "prerequisite to recover the pre-1988 turning points.")
    } else {
      model_data$cci_williams <- williams_fit_info$model_data$cci_williams
      for (nm in williams_fit_info$knot_names) {
        if (!nm %in% names(model_data)) {
          model_data[[nm]] <- williams_fit_info$model_data[[nm]]
        }
      }
      cat(sprintf("  [4a-i] cci_williams: %d non-NA obs (%s -> %s)\n",
                  sum(!is.na(model_data$cci_williams)),
                  as.character(min(model_data$date[!is.na(model_data$cci_williams)])),
                  as.character(max(model_data$date[!is.na(model_data$cci_williams)]))))

      # [4a-ii] Attach the de-meaned CCI interaction columns (and the combined
      # illiquid-financial ratio) to the GLOBAL model_data, using exactly the
      # Spec-8 construction. This makes Spec 8/11's long-run regressors
      # visible to every downstream consumer — the cointegration battery
      # (which previously skipped Spec 11 as "missing regressors"), the OOS
      # validator, gamma inference, and the counterfactual engine.
      cci_attach_end  <- as.Date("2024-10-01")
      cci_attach_mask <- !is.na(model_data$cci_williams) &
                         model_data$date >= as.Date("1980-01-01") &
                         model_data$date <= cci_attach_end
      ha_mean_g <- mean(model_data$ha_y[cci_attach_mask],         na.rm = TRUE)
      hp_mean_g <- mean(model_data$ln_hp_over_y[cci_attach_mask], na.rm = TRUE)
      r_mean_g  <- mean(model_data$real_rate[cci_attach_mask],    na.rm = TRUE)
      yp_mean_g <- mean(model_data$ln_yp_over_y[cci_attach_mask], na.rm = TRUE)
      model_data <- model_data %>%
        mutate(
          r_x_cci          = (real_rate    - r_mean_g)  * cci_williams,
          hp_x_1_minus_cci = (ln_hp_over_y - hp_mean_g) * (1 - 1.2 * cci_williams),
          yp_x_cci         = (ln_yp_over_y - yp_mean_g) * cci_williams,
          ha_x_cci         = (ha_y         - ha_mean_g) * cci_williams,
          ilfa_y           = eq_y + super_y
        )

      # [4a-iii] Persist the deployed CCI series + interactions so the index
      # path (zero pre-2009, negative post-2019) and the interaction
      # correlation matrix are reproducible from a committed artefact —
      # previously cci_williams existed in no committed dataset and the WP's
      # pointer for the collinearity claim was unactionable.
      cci_series_csv <- model_data %>%
        select(date, cci_williams, ha_x_cci, hp_x_1_minus_cci,
               r_x_cci, yp_x_cci)
      write.csv(cci_series_csv,
                file.path(output_dir, "australia_cci_williams_series.csv"),
                row.names = FALSE)
      cci_int_cols <- c("cci_williams", "ha_x_cci", "hp_x_1_minus_cci",
                        "r_x_cci", "yp_x_cci")
      cci_cor <- cor(model_data[cci_attach_mask, cci_int_cols],
                     use = "pairwise.complete.obs")
      write.csv(round(cci_cor, 4),
                file.path(output_dir, "australia_cci_interaction_corr.csv"),
                row.names = TRUE)
      cat(sprintf("  [4a-iii] cci_williams series + interaction correlations saved (range %.2f to %.2f)\n",
                  min(model_data$cci_williams, na.rm = TRUE),
                  max(model_data$cci_williams, na.rm = TRUE)))
    }
  } else {
    WILLIAMS_FALLBACK <- TRUE
  }
} else {
  message("[Williams CCI] No SDMMA basis columns on model_data — skipping ",
          "(USE_WILLIAMS_SDMMA_BASIS may be FALSE)")
  WILLIAMS_FALLBACK <- TRUE
}

cat("  [4a] Full sample (1988Q4–2024Q4)\n")
specs_full     <- run_all_specifications(model_data, sample_end = as.Date("2024-10-01"))
cat("  [4b] Pre-COVID sample (1988Q4–2019Q4)\n")
specs_precovid <- run_all_specifications(model_data, sample_end = as.Date("2019-10-01"))
cat("  [4c] COVID-dropped + COVID-rich-dummies variants\n")
covid_specs <- run_specifications_covid_robust(model_data,
                                               sample_end = as.Date("2024-10-01"))

cat("[Step 5] Building results tables...\n")
results_full     <- build_results_table(specs_full,     output_dir, period_label = "full")
results_precovid <- build_results_table(specs_precovid, output_dir, period_label = "precovid")

cat("[Step 5b] Wald test of NLA cross-equation restriction (gamma_LA + gamma_LOANS = 0)...\n")
nla_test_full     <- test_nla_restriction(model_data,
                       sample_label = "full",
                       sample_end   = as.Date("2024-10-01"))
nla_test_precovid <- test_nla_restriction(model_data,
                       sample_label = "precovid",
                       sample_end   = as.Date("2019-10-01"))
nla_test <- bind_rows(nla_test_full, nla_test_precovid)
write.csv(nla_test,
          file.path(output_dir, "australia_nla_restriction_test.csv"),
          row.names = FALSE)
print(nla_test)

# ---- Spec 8 sign-prior verdict ---------------------------------------------
# Compares the long-run interaction coefficients (Spec 8) to their
# institutional sign priors (Aust paper eq 7); reports PASS/FAIL and p-value.
if (!is.null(specs_full$spec8)) {
  sp8 <- specs_full$spec8
  cf  <- coef(sp8$fit)
  se  <- sqrt(diag(sp8$nw_vcov))
  pv  <- 2 * pt(-abs(cf / se[match(names(cf), names(se))]),
                df = nobs(sp8$fit) - sum(!is.na(cf)))
  prior_tbl <- tibble::tribble(
    ~term,                ~prior,           ~prior_sign,
    "r_x_cci",            "negative",        -1,
    "hp_x_1_minus_cci",   "negative",        -1,
    "yp_x_cci",           "positive",         1,
    "ln_yp_over_y",       "small positive",   1
  )
  prior_tbl$ols_estimate <- vapply(prior_tbl$term,
    function(nm) if (nm %in% names(cf)) unname(cf[nm]) else NA_real_,
    numeric(1))
  prior_tbl$p_value <- vapply(prior_tbl$term,
    function(nm) if (nm %in% names(pv)) unname(pv[nm]) else NA_real_,
    numeric(1))
  prior_tbl$verdict <- ifelse(
    is.na(prior_tbl$ols_estimate), "NA",
    ifelse(sign(prior_tbl$ols_estimate) == prior_tbl$prior_sign, "PASS", "FAIL")
  )

  cat("\n--- Spec 8 sign-prior verdicts (full sample) ---\n")
  cat(sprintf("  %-22s %-15s %12s %10s  %s\n",
              "term", "prior", "ols_estimate", "p_value", "verdict"))
  for (i in seq_len(nrow(prior_tbl))) {
    r <- prior_tbl[i, ]
    cat(sprintf("  %-22s %-15s %12.6f %10.3f  %s\n",
                r$term, r$prior,
                ifelse(is.na(r$ols_estimate), 0, r$ols_estimate),
                ifelse(is.na(r$p_value), NA, r$p_value),
                r$verdict))
  }
  write.csv(prior_tbl,
            file.path(output_dir, "australia_spec8_sign_prior_verdicts.csv"),
            row.names = FALSE)
}

# Combined file with both periods
combined_coef <- bind_rows(results_full$coef_combined, results_precovid$coef_combined)
combined_diag <- bind_rows(results_full$diag_combined, results_precovid$diag_combined)
write.csv(combined_coef, file.path(output_dir, "australia_all_results.csv"),     row.names = FALSE)
write.csv(combined_diag, file.path(output_dir, "australia_all_diagnostics.csv"), row.names = FALSE)

cat("[Step 6] Running cointegration battery (ADF + PO + Johansen)...\n")
coint_results <- tryCatch(
  run_cointegration_battery(model_data, specs_full, output_dir,
                            sample_end = as.Date("2024-10-01")),
  error = function(e) {
    message("[run_cointegration_battery] Error: ", e$message); NULL
  }
)

cat("[Step 7] Consolidating COVID robustness lambda table...\n")
lambda_robust <- tryCatch(
  build_lambda_robustness_table(specs_full, specs_precovid,
                                covid_specs$covid_dropped, covid_specs$covid_rich,
                                output_dir),
  error = function(e) {
    message("[build_lambda_robustness_table] Error: ", e$message); NULL
  }
)

cat("[Step 7b] Spec 11 full coefficient vector across the four sample variants...\n")
# The headline's wealth-significance claim must be checkable against the
# COVID-robust variants, not just lambda: a review found the full-sample
# nla_y/ilfa_y significance does not survive dropping 2020Q1-2021Q4, while
# the paper only committed lambda per variant.
tryCatch({
  spec11_variant_rows <- list()
  spec11_sources <- list(
    full          = specs_full$spec11,
    precovid      = specs_precovid$spec11,
    covid_dropped = covid_specs$covid_dropped$spec11,
    covid_rich    = covid_specs$covid_rich$spec11
  )
  for (vn in names(spec11_sources)) {
    sp <- spec11_sources[[vn]]
    if (is.null(sp)) next
    cf <- coef(sp$fit)
    vc <- sp$nw_vcov
    se <- sqrt(diag(vc))[match(names(cf), colnames(vc))]
    lam <- if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_
    spec11_variant_rows[[vn]] <- tibble::tibble(
      sample_variant = vn,
      term           = names(cf),
      ols_estimate   = unname(cf),
      nw_se          = unname(se),
      t_stat         = unname(cf) / unname(se),
      lambda         = lam,
      structural_param = ifelse(names(cf) == "ecm_lag", -1,
                                unname(cf) / abs(lam)),
      n_obs          = nobs(sp$fit)
    )
  }
  spec11_variants <- bind_rows(spec11_variant_rows)
  write.csv(spec11_variants,
            file.path(output_dir, "australia_spec11_variants.csv"),
            row.names = FALSE)
  cat(sprintf("  Saved australia_spec11_variants.csv (%d variants)\n",
              length(spec11_variant_rows)))
}, error = function(e) message("[spec11_variants] Error: ", e$message))

cat("[Step 7c] Spec 11 ogive sensitivity (Italy PI with / without the GFC learning weight)...\n")
# The headline Italy-LP permanent-income series is multiplied by a learning
# ogive that halves it after 2012 (gfc_ogive = TRUE default) — previously
# undisclosed for the headline measure and untested. Refit Spec 11 with the
# raw (no-ogive) series; the interactions that involve ln_yp_over_y are
# rebuilt so the comparison is internally consistent.
tryCatch({
  if (!is.null(specs_full$spec11) && "cci_williams" %in% names(model_data)) {
    base_noog <- model_data %>%
      select(-any_of(c("ln_yp_over_y", "ln_yp_over_y_post2008")))
    noog <- construct_permanent_income_italy(base_noog, gfc_ogive = FALSE)
    noog <- noog %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)
    mask_no <- !is.na(noog$cci_williams) &
               noog$date >= as.Date("1980-01-01") &
               noog$date <= as.Date("2024-10-01")
    yp_mean_no <- mean(noog$ln_yp_over_y[mask_no], na.rm = TRUE)
    noog <- noog %>%
      mutate(yp_x_cci = (ln_yp_over_y - yp_mean_no) * cci_williams)
    sp11_noog <- fit_ecm_spec(
      data       = noog,
      spec_name  = "Spec11_LIVES_Headline_NoOgive",
      lr_vars    = specs_full$spec11$lr_vars,
      sr_vars    = specs_full$spec11$sr_vars,
      dummy_vars = specs_full$spec11$dummy_vars,
      sample_end = as.Date("2024-10-01")
    )
    grab <- function(sp, label) {
      cf <- coef(sp$fit); vc <- sp$nw_vcov
      se <- sqrt(diag(vc))[match(names(cf), colnames(vc))]
      lam <- cf[["ecm_lag"]]
      tibble::tibble(variant = label, term = names(cf),
                     ols_estimate = unname(cf), nw_se = unname(se),
                     t_stat = unname(cf) / unname(se), lambda = lam,
                     structural_param = ifelse(names(cf) == "ecm_lag", -1,
                                               unname(cf) / abs(lam)),
                     n_obs = nobs(sp$fit))
    }
    ogive_tbl <- bind_rows(grab(specs_full$spec11, "ogive (headline)"),
                           grab(sp11_noog,        "no ogive"))
    write.csv(ogive_tbl,
              file.path(output_dir, "australia_spec11_ogive_robustness.csv"),
              row.names = FALSE)
    lam_no <- ogive_tbl$lambda[ogive_tbl$variant == "no ogive"][1L]
    yp_no  <- ogive_tbl$ols_estimate[ogive_tbl$variant == "no ogive" &
                                     ogive_tbl$term == "ln_yp_over_y"]
    cat(sprintf("  No-ogive Spec 11: lambda = %.3f, ln_yp_over_y = %.3f\n",
                lam_no, yp_no))
  } else {
    message("[spec11_ogive] Spec 11 or cci_williams unavailable; skipped")
  }
}, error = function(e) message("[spec11_ogive] Error: ", e$message))

cat("[Step 8] Selecting preferred specification...\n")
selection <- select_preferred_spec(
  coef_combined         = results_full$coef_combined,
  diag_combined         = results_full$diag_combined,
  coint_results         = coint_results,
  coef_combined_precovid = results_precovid$coef_combined,
  lambda_robust         = lambda_robust,
  output_dir            = output_dir
)
preferred_name <- (selection %>% filter(is_preferred))$specification[1L]
preferred_idx <- which(vapply(specs_full,
                              function(s) s$spec_name == preferred_name,
                              logical(1L)))
if (length(preferred_idx) == 0L) {
  warning("[main] Preferred spec name not matched in specs_full; falling back to spec6.")
  preferred_idx <- which(names(specs_full) == "spec6")
}
preferred_spec <- specs_full[[preferred_idx[1L]]]
message(sprintf("[main] Preferred spec: %s", preferred_name))

cat("[Step 9] Running break battery on preferred spec...\n")
break_results <- tryCatch(
  run_break_battery(preferred_spec, output_dir),
  error = function(e) {
    message("[run_break_battery] Error: ", e$message); NULL
  }
)
if (!is.null(break_results)) {
  brk_path <- file.path(output_dir, "australia_breaks.csv")
  write.csv(break_results, brk_path, row.names = FALSE)
  message(sprintf("[main] Breaks saved to %s", brk_path))
}

cat("[Step 10] Conditional WLS robustness on preferred spec...\n")
pref_diag_row <- results_full$diag_combined %>%
  filter(specification == preferred_name) %>% slice(1L)
wls_results <- NULL
het_diag_val <- if (nrow(pref_diag_row) > 0L) pref_diag_row$het_diagnosis[1L] else NA_character_
message(sprintf("[main] Preferred spec het_diagnosis: %s",
                ifelse(is.na(het_diag_val), "NA", het_diag_val)))
if (!is.na(het_diag_val) && het_diag_val == "structural") {
  wls_results <- tryCatch(fit_wls_variant(preferred_spec),
                          error = function(e) {
                            message("[fit_wls_variant] Error: ", e$message); NULL
                          })
  if (!is.null(wls_results)) {
    wls_path <- file.path(output_dir, "australia_wls_robustness.csv")
    write.csv(wls_results, wls_path, row.names = FALSE)
    message(sprintf("[main] WLS robustness saved to %s", wls_path))
  } else {
    message("[main] WLS fit returned NULL.")
  }
} else {
  message("[main] Skipping WLS — het_diagnosis is not 'structural'.")
}

cat("[Step 11] Running permanent-income sensitivity grid...\n")
pi_sensitivity <- tryCatch(
  run_pi_sensitivity(model_data, preferred_spec, output_dir),
  error = function(e) {
    message("[run_pi_sensitivity] Error: ", e$message); NULL
  }
)

cat("[Step 11b] Comparing PI methods (AR vs Italy LP) on preferred spec...\n")
tryCatch(
  compare_pi_methods(model_data, preferred_spec, output_dir),
  error = function(e) {
    message("[compare_pi_methods] Error: ", e$message); NULL
  }
)

cat("[Step 12] Running rolling-window estimation on preferred spec...\n")
rolling_results <- tryCatch(
  fit_rolling_window(preferred_spec, model_data, output_dir, window_size = 60L),
  error = function(e) {
    message("[fit_rolling_window] Error: ", e$message); NULL
  }
)

cat("[Step 13] Building Italy–Australia comparison table...\n")
# italy_table1_results.csv (published Italy reference numbers from De Bonis
# et al. 2024) lives in Australia/outputs/. The Italy estimation
# pipeline itself was removed during the May 2026 repo cleanup; we keep
# only the published reference file as a benchmark for the cross-country
# comparison in build_comparison_table.
comparison <- build_comparison_table(aus_coef = combined_coef,
                                     output_dir = output_dir,
                                     italy_dir = output_dir)

cat("[Step 14] Writing narrative model summary...\n")
tryCatch(
  write_model_summary(specs_full, specs_precovid,
                      results_full, results_precovid,
                      comparison, selection, coint_results,
                      lambda_robust = lambda_robust,
                      break_results = break_results,
                      wls_results   = wls_results,
                      pi_sensitivity = pi_sensitivity,
                      output_dir    = output_dir),
  error = function(e) message("[write_model_summary] Error: ", e$message)
)

cat("[Step 15] Generating plots — preferred spec + spec 1...\n")
plot_actual_vs_fitted(preferred_spec, output_dir = output_dir)
plot_actual_vs_fitted(specs_full$spec1, output_dir = output_dir)

# Sub-scripts are sourced into an ISOLATED child environment. They can READ
# pipeline state (model_data, specs_full, ...) through the parent chain, but
# their own assignments stay local — a previous version sourced them with
# local = TRUE at top level, so knot_experiment.R / cci_placebo_test.R
# REASSIGNED the global model_data (rebuilt with the AR permanent-income
# constructor), silently contaminating every later step including the IV and
# SUR robustness blocks. Do not revert to local = TRUE.
source_isolated <- function(fname) {
  source(file.path(dirname(.this_file), fname),
         local = new.env(parent = globalenv()))
}

cat("[Step 16] Building Williams structural-parameter comparison table...\n")
tryCatch(
  source_isolated("williams_comparison.R"),
  error = function(e) message("[williams_comparison] Error: ", e$message)
)

cat("[Step 17] Knot-experiment: testing 7 CCI spline variants...\n")
tryCatch(
  source_isolated("knot_experiment.R"),
  error = function(e) message("[knot_experiment] Error: ", e$message)
)

cat("[Step 18] CCI method comparison (Williams maximal-GETS vs Kalman)...\n")
tryCatch(
  source_isolated("cci_method_comparison.R"),
  error = function(e) message("[cci_method_comparison] Error: ", e$message)
)

cat("[Step 19] CCI alternatives (PCA, credit-to-GDP gap, macropru intensity)...\n")
tryCatch(
  source_isolated("cci_alternatives.R"),
  error = function(e) message("[cci_alternatives] Error: ", e$message)
)

cat("[Step 20] Random-knot placebo tests (literal 4-knot + deployed protocol)...\n")
tryCatch(
  source_isolated("cci_placebo_test.R"),
  error = function(e) message("[cci_placebo_test] Error: ", e$message)
)

cat("[Step 21] CCI fit-improvement decomposition (detrending vs identification)...\n")
tryCatch(
  source_isolated("cci_fit_decomposition.R"),
  error = function(e) message("[cci_fit_decomposition] Error: ", e$message)
)

cat("[Step 22] Out-of-sample forecast validation (NS-033)...\n")
tryCatch(
  source_isolated("oos_forecast.R"),
  error = function(e) message("[oos_forecast] Error: ", e$message)
)

cat("[Step 8] Selecting preferred spec for downstream robustness work...\n")
preferred_name_val <- if (exists("selection") && "is_preferred" %in% names(selection)) {
  sel_row <- selection[isTRUE(selection$is_preferred) | selection$is_preferred, ]
  if (nrow(sel_row) > 0L) sel_row$specification[1L] else NULL
} else NULL
preferred_spec <- pick_preferred_spec_object(specs_full, preferred_name_val)
cat(sprintf("  Preferred spec: %s\n", preferred_spec$spec_name))

cat("[Step 9] Italy-style robustness suite (4 blocks)...\n")
tryCatch(
  run_italy_style_robustness(preferred_spec, model_data, output_dir),
  error = function(e)
    message(sprintf("  [Step 9] aborted: %s", conditionMessage(e)))
)

cat("[Step 9b] Italy-style robustness suite for the LIVES headline (Spec 11)...\n")
# The suite previously ran only for the selector-preferred spec, so the IV /
# SUR / Chow / income-measure robustness conclusions were never demonstrated
# on the paper's actual headline equation.
tryCatch({
  if (!is.null(specs_full$spec11)) {
    run_italy_style_robustness(specs_full$spec11, model_data, output_dir,
                               file_suffix = "_spec11")
  } else {
    message("  [Step 9b] skipped: Spec 11 not available")
  }
}, error = function(e)
    message(sprintf("  [Step 9b] aborted: %s", conditionMessage(e)))
)

cat("[Step 10] Long-run decomposition plot (headline output)...\n")
tryCatch(
  plot_longrun_decomposition(preferred_spec, output_dir),
  error = function(e)
    message(sprintf("  [Step 10] aborted: %s", conditionMessage(e)))
)

cat("[Step 10b] Long-run decomposition plot for the LIVES headline (Spec 11)...\n")
# Mirrors Step 9b: the §10.1 decomposition narrative is about the LIVES
# headline mechanism, so it must be regenerated on Spec 11, not just on the
# selector-preferred spec (previously Spec 3/6). Skipped gracefully if Spec 11
# is absent from this run (e.g. USE_WILLIAMS_SDMMA_BASIS = FALSE).
tryCatch({
  if (!is.null(specs_full$spec11)) {
    plot_longrun_decomposition(specs_full$spec11, output_dir,
                               file_suffix = "_spec11")
  } else {
    message("  [Step 10b] skipped: Spec 11 not available")
  }
}, error = function(e)
    message(sprintf("  [Step 10b] aborted: %s", conditionMessage(e)))
)

cat("[Step 11] Policy counterfactuals (NS-012)...\n")
tryCatch(
  run_counterfactuals(specs_full = specs_full,
                      output_dir = output_dir),
  error = function(e)
    message(sprintf("  [Step 11] aborted: %s", conditionMessage(e)))
)

cat(rep("=", 70), "\n", sep = "")
cat("Estimation complete. Outputs written to:", output_dir, "\n")
cat(rep("=", 70), "\n", sep = "")
