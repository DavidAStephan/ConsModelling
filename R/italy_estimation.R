#!/usr/bin/env Rscript

# ==============================================================================
# Italy Household Consumption Model — Part 2: Estimation Framework
# ==============================================================================
#
# Replication of: De Bonis, Liberati, Muellbauer, Rondinelli (2025)
# "Why net worth is the wrong concept for explaining consumption: evidence
# from Italy"
#
# ASSUMES: model_data tibble already exists in the environment with all
# variables listed in the preamble. This is Part 2; Part 1 handles data
# download and preparation.
#
# Model:
#   Δ ln c_t = λ[α_0 + α_c·CCI_t + α_1·r_t + Σγ_i·(A_i,t-1/y_t)
#              + φ·ln(y^p_t/y_t) + ln(y_t/c_{t-1})] + short-run + ε_t
#
# Since ln(y/c)_{t-1} enters the bracket multiplied by λ, OLS gives:
#   Δ ln c_t = a + b_r·r_t + Σc_i·(A_i/y)_t + b_p·ln(y^p/y)_t
#              + λ·ln(y/c)_{t-1} + short-run + ε_t
# where structural params = estimated coef / λ
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

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir   <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# SECTION A: Permanent Income Construction
# ==============================================================================
#
# construct_permanent_income()
#
# Constructs ln(y^p_t / y_t) using a rolling out-of-sample income forecasting
# model, following the method described in the paper (Table A2).
#
# The forecasting equation regresses log income on:
#   - Time trend and a structural break at 2008Q3 (slope + step shift)
#   - A 4-quarter moving average of log income (lags 1-4)
#   - Additional macro predictors (unemployment, log oil, REER, stock index)
#     when available in model_data; otherwise falls back to AR(8)
#
# Permanent income is the discounted sum of k=40 quarterly forecasts, using
# geometrically-declining quarterly discount weights with annual factor delta.
#
# After 2008Q3 a "learning adjustment" is applied (Table A3 of paper):
#   agents initially over-weight recent bad news, with a weight that declines
#   via an ogive from 1.0 at 2008Q3 toward 0.5 at 2012Q2 and is fixed
#   thereafter.  The adjustment multiplies the pre-2008 ln(y^p/y) series by
#   that weight before 2012Q2, and by 0.5 afterwards.
#
# Arguments:
#   model_data  — tibble with at minimum: date, lincome
#   k           — forecast horizon in quarters (default 40 = 10 years)
#   delta       — annual discount factor (default 0.95)
#
# Returns model_data with ln_yp_over_y and ln_yp_over_y_post2008 added /
# overwritten.
# ==============================================================================

construct_permanent_income <- function(model_data, k = 40, delta = 0.95) {

  # Quarterly discount factor and weights
  delta_q <- delta^(1 / 4)
  disc_weights <- delta_q^(seq_len(k) - 1L)
  disc_weights <- disc_weights / sum(disc_weights)   # normalise to sum to 1

  n   <- nrow(model_data)
  dat <- model_data %>% arrange(date)

  # ------------------------------------------------------------------
  # Identify which optional predictors are present in the data
  # ------------------------------------------------------------------
  # Only use optional predictors if they cover enough of the early sample.
  # A predictor starting after 2000 would eliminate all training observations
  # (since complete.cases requires ALL regressors to be non-NA), so we drop
  # any optional variable whose first non-NA date is after 2000-01-01.
  early_enough <- function(col) {
    if (!col %in% names(dat)) return(FALSE)
    vals <- dat[[col]]
    if (all(is.na(vals))) return(FALSE)
    first_date <- min(dat$date[!is.na(vals)], na.rm = TRUE)
    first_date < as.Date("2000-01-01")
  }
  has_unemp  <- early_enough("unemp_rate")
  has_oil    <- early_enough("log_oil")
  has_reer   <- early_enough("log_reer")
  has_stocks <- early_enough("log_stocks")
  has_emprat <- early_enough("emp_rate")

  # ------------------------------------------------------------------
  # Build the feature matrix for the forecasting equation.
  # Core regressors (always present):
  #   trend, I(t >= 2008Q3)*trend, I(t >= 2008Q3) [step],
  #   MA4 of log income (mean of lags 1:4), AR lags 5-8
  # Optional regressors added when available.
  # ------------------------------------------------------------------

  dat <- dat %>%
    mutate(
      t_index    = seq_len(n),
      step2008   = as.integer(date >= as.Date("2008-07-01")),
      trend_brk  = t_index * step2008,
      # 4q MA of log income (average of lags 1-4)
      inc_ma4    = (lag(lincome, 1) + lag(lincome, 2) +
                    lag(lincome, 3) + lag(lincome, 4)) / 4,
      inc_lag5   = lag(lincome, 5),
      inc_lag6   = lag(lincome, 6),
      inc_lag7   = lag(lincome, 7),
      inc_lag8   = lag(lincome, 8)
    )

  core_vars <- c("t_index", "step2008", "trend_brk",
                 "inc_ma4", "inc_lag5", "inc_lag6", "inc_lag7", "inc_lag8")

  optional_vars <- character(0)
  if (has_unemp)  optional_vars <- c(optional_vars, "unemp_rate")
  if (has_oil)    optional_vars <- c(optional_vars, "log_oil")
  if (has_reer)   optional_vars <- c(optional_vars, "log_reer")
  if (has_stocks) optional_vars <- c(optional_vars, "log_stocks")
  if (has_emprat) optional_vars <- c(optional_vars, "emp_rate")

  if (length(optional_vars) == 0L) {
    message(
      "[construct_permanent_income] Optional macro predictors not found in ",
      "model_data. Using AR(8) + trend + break specification for income ",
      "forecasting (Table A2 fallback)."
    )
  }

  rhs_vars <- c(core_vars, optional_vars)

  # ------------------------------------------------------------------
  # Rolling out-of-sample estimation (information available at t)
  # Minimum training window: 24 observations (~6 years, 1.5 business cycles)
  # The paper uses 40; we reduce to 24 because Italian Eurostat quarterly
  # income only starts 1999Q1, limiting available training data.
  # With income from 1995Q1 (annual back-extension) and 24-obs window,
  # first PI forecast is ~2002Q4 rather than 2006Q4, giving N~68 vs ~53.
  min_train <- 24L
  ln_yp_over_y <- rep(NA_real_, n)

  for (i in seq_len(n)) {

    if (i <= min_train) next

    train <- dat[seq_len(i), , drop = FALSE]

    # Drop rows where any regressor or the LHS is NA
    req_cols <- c("lincome", rhs_vars)
    train_cc <- train[complete.cases(train[, req_cols, drop = FALSE]), ]

    if (nrow(train_cc) < min_train) next

    fit_inc <- tryCatch(
      lm(reformulate(rhs_vars, response = "lincome"), data = train_cc),
      error = function(e) NULL
    )
    if (is.null(fit_inc)) next

    # ----------------------------------------------------------------
    # Forecast k steps ahead from period i.
    # We use the model coefficients with all time-varying predictors
    # held at their period-i values (steady-state / no-change forecast).
    # ----------------------------------------------------------------
    current_row <- dat[i, , drop = FALSE]

    # Build a forecast state: carry forward the current row's predictors
    # and advance the lagged income values iteratively
    lincome_history <- dat$lincome[seq_len(i)]

    forecast_levels <- rep(NA_real_, k)
    inc_path <- lincome_history   # grows as we extend

    cf <- coef(fit_inc)

    for (h in seq_len(k)) {

      n_path <- length(inc_path)

      # Lagged income values needed for this forecast step
      l1 <- if (n_path >= 1L) inc_path[n_path]       else NA_real_
      l2 <- if (n_path >= 2L) inc_path[n_path - 1L]  else NA_real_
      l3 <- if (n_path >= 3L) inc_path[n_path - 2L]  else NA_real_
      l4 <- if (n_path >= 4L) inc_path[n_path - 3L]  else NA_real_
      l5 <- if (n_path >= 5L) inc_path[n_path - 4L]  else NA_real_
      l6 <- if (n_path >= 6L) inc_path[n_path - 5L]  else NA_real_
      l7 <- if (n_path >= 7L) inc_path[n_path - 6L]  else NA_real_
      l8 <- if (n_path >= 8L) inc_path[n_path - 7L]  else NA_real_

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
        `(Intercept)` = 1,
        t_index    = t_h,
        step2008   = step_h,
        trend_brk  = trend_h,
        inc_ma4    = ma4_fc,
        inc_lag5   = l5,
        inc_lag6   = l6,
        inc_lag7   = l7,
        inc_lag8   = l8,
        check.names = FALSE
      )

      # Append optional vars at period-i values (held constant)
      for (v in optional_vars) {
        new_row[[v]] <- if (!is.na(current_row[[v]])) current_row[[v]] else 0
      }

      # Predict: only use terms present in the fitted model
      # Skip aliased (NA) coefficients — treat them as zero
      terms_in_model <- names(cf)
      avail_terms <- intersect(terms_in_model, names(new_row))
      ic <- cf[["(Intercept)"]]
      pred_val <- if (!is.na(ic)) ic else 0
      for (trm in setdiff(avail_terms, "(Intercept)")) {
        if (!is.na(cf[[trm]]) && !is.na(new_row[[trm]])) {
          pred_val <- pred_val + cf[[trm]] * new_row[[trm]]
        }
      }

      forecast_levels[h] <- pred_val
      inc_path <- c(inc_path, pred_val)
    }

    # ----------------------------------------------------------------
    # Discounted sum of forecasted log levels minus current log level
    # ln(y^p_t / y_t) = Σ w_h * [ln_y_hat(t+h) - ln_y(t)]
    # ----------------------------------------------------------------
    if (!anyNA(forecast_levels)) {
      deviations <- forecast_levels - dat$lincome[i]
      ln_yp_over_y[i] <- sum(disc_weights * deviations)
    }
  }

  dat$ln_yp_over_y <- ln_yp_over_y

  # ------------------------------------------------------------------
  # Learning adjustment after GFC (Table A3 logic):
  #   Pre-GFC (before 2008Q3):  weight = 1.0  (no adjustment)
  #   2008Q3 to 2012Q1:         ogive declining from 1.0 to 0.5
  #                             over 15 quarters
  #   2012Q2 onward:            weight = 0.5 (permanently lower)
  #
  # The adjusted series = ln_yp_over_y * learning_weight
  # The post-2008 term for the regression captures the *additional*
  # (negative) effect: ln_yp_over_y_post2008 = ln_yp_over_y * step2008
  # ------------------------------------------------------------------

  gfc_start   <- as.Date("2008-07-01")   # 2008Q3
  adj_start   <- as.Date("2008-07-01")
  adj_end     <- as.Date("2012-04-01")   # 2012Q2 — stabilisation point
  n_adj_qtrs  <- 15L                     # quarters of declining weight

  dat <- dat %>%
    mutate(
      # Learning weight: 1 before GFC, declining ogive during 2008Q3-2012Q1,
      # fixed at 0.5 from 2012Q2
      learning_weight = case_when(
        date < gfc_start  ~ 1.0,
        date >= adj_end   ~ 0.5,
        TRUE ~ {
          q_since <- as.integer(
            round(as.numeric(difftime(date, gfc_start, units = "days")) / 91.25)
          )
          # Ogive: smoothly declines from 1 to 0.5 over n_adj_qtrs quarters
          1.0 - 0.5 * (pmin(q_since, n_adj_qtrs) / n_adj_qtrs)
        }
      ),
      ln_yp_over_y = ln_yp_over_y * learning_weight,
      # post-2008 interaction for Spec 6 (captures GFC regime shift in φ)
      ln_yp_over_y_post2008 = ln_yp_over_y * step2008
    ) %>%
    select(-t_index, -step2008, -trend_brk,
           -inc_ma4, -inc_lag5, -inc_lag6, -inc_lag7, -inc_lag8,
           -learning_weight)

  dat
}


# ==============================================================================
# SECTION B: Income Volatility
# ==============================================================================
#
# compute_income_volatility()
#
# Fits an AR(8) on log income and returns the absolute residuals as a proxy
# for income uncertainty / precautionary savings motive.
#
# Arguments: model_data tibble with lincome column
# Returns:   model_data with abs_income_resid added / overwritten
# ==============================================================================

compute_income_volatility <- function(model_data) {

  dat <- model_data %>% arrange(date)
  n   <- nrow(dat)

  # Build AR(8) design matrix
  ar_dat <- dat %>%
    transmute(
      lincome,
      l1 = lag(lincome, 1), l2 = lag(lincome, 2),
      l3 = lag(lincome, 3), l4 = lag(lincome, 4),
      l5 = lag(lincome, 5), l6 = lag(lincome, 6),
      l7 = lag(lincome, 7), l8 = lag(lincome, 8)
    )

  ar_cc <- ar_dat[complete.cases(ar_dat), ]

  if (nrow(ar_cc) < 20L) {
    warning("[compute_income_volatility] Fewer than 20 complete observations ",
            "for AR(8). Returning NA for abs_income_resid.")
    dat$abs_income_resid <- NA_real_
    return(dat)
  }

  ar_fit <- lm(
    lincome ~ l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8,
    data = ar_cc
  )

  # Residuals aligned back to the full date vector
  resid_vals <- rep(NA_real_, n)
  cc_idx <- which(complete.cases(ar_dat))
  resid_vals[cc_idx] <- residuals(ar_fit)

  dat$abs_income_resid <- abs(resid_vals)
  dat
}


# ==============================================================================
# SECTION C: Diagnostics
# ==============================================================================
#
# model_diagnostics()
#
# Computes a standard battery of post-estimation diagnostics for a fitted lm
# object, returning a single-row tibble.
#
# Arguments:
#   fit        — an lm object
#   data       — the data used to fit the model (needed for Chow test)
#   break_date — character "YYYY-MM-DD" for Chow structural break test
#
# Returns a tibble with:
#   n_obs, se_pct, adj_r2, dw, lm_het_pval, ar1_pval, ar4_pval,
#   chow_pval, reset_pval, schwarz, loglik
# ==============================================================================

model_diagnostics <- function(fit, data, break_date = "2002-01-01") {

  s <- summary(fit)
  n <- nobs(fit)

  # --- Standard error as percentage ---
  se_pct <- s$sigma * 100

  # --- Adjusted R² ---
  adj_r2 <- s$adj.r.squared

  # --- Durbin-Watson ---
  dw_val <- tryCatch(
    as.numeric(lmtest::dwtest(fit)$statistic),
    error = function(e) NA_real_
  )

  # --- Breusch-Pagan heteroskedasticity ---
  het_pval <- tryCatch(
    lmtest::bptest(fit)$p.value,
    error = function(e) NA_real_
  )

  # --- Godfrey LM serial correlation tests ---
  ar1_pval <- tryCatch(
    lmtest::bgtest(fit, order = 1L)$p.value,
    error = function(e) NA_real_
  )
  ar4_pval <- tryCatch(
    lmtest::bgtest(fit, order = 4L)$p.value,
    error = function(e) NA_real_
  )

  # --- Chow structural break test ---
  # Uses strucchange::sctest (Fstats approach) at the nominated break date.
  chow_pval <- tryCatch({
    # Identify the break point as an integer row number in the fitted data
    fit_dates <- data$date[as.integer(rownames(model.frame(fit)))]
    break_dt  <- as.Date(break_date)
    bp_idx    <- sum(fit_dates <= break_dt, na.rm = TRUE)
    if (bp_idx < 5L || bp_idx > (n - 5L)) {
      NA_real_
    } else {
      sc <- strucchange::sctest(fit, type = "Chow", point = bp_idx)
      as.numeric(sc$p.value)
    }
  }, error = function(e) NA_real_)

  # --- RESET test (power = 2) ---
  reset_pval <- tryCatch(
    lmtest::resettest(fit, power = 2L, type = "fitted")$p.value,
    error = function(e) NA_real_
  )

  # --- Information criteria ---
  bic_val    <- BIC(fit)
  loglik_val <- as.numeric(logLik(fit))

  tibble::tibble(
    n_obs        = n,
    se_pct       = se_pct,
    adj_r2       = adj_r2,
    dw           = dw_val,
    lm_het_pval  = het_pval,
    ar1_pval     = ar1_pval,
    ar4_pval     = ar4_pval,
    chow_pval    = chow_pval,
    reset_pval   = reset_pval,
    schwarz      = bic_val,
    loglik       = loglik_val
  )
}


# ==============================================================================
# DUMMY VARIABLE CONSTRUCTION
# ==============================================================================
#
# Adds all required dummy and short-run variables to model_data.
# Assumes model_data already has: date, log_cci (or cci), log_public_cons,
# lincome, unemp_rate columns.
# ==============================================================================

add_model_variables <- function(model_data) {

  dat <- model_data %>% arrange(date)

  # ------------------------------------------------------------------
  # Impulse dummies
  # ------------------------------------------------------------------
  dat <- dat %>%
    mutate(
      d1983Q1 = as.integer(date == as.Date("1983-01-01")),
      d1993Q1 = as.integer(date == as.Date("1993-01-01")),
      d2013Q1 = as.integer(date == as.Date("2013-01-01"))
    )

  # ------------------------------------------------------------------
  # Pre-1990 Q3 seasonal dummy
  # Q3 = July quarter start = months 7
  # ------------------------------------------------------------------
  dat <- dat %>%
    mutate(
      d1990Q3_pre = as.integer(
        lubridate::month(date) == 7L & date < as.Date("1990-01-01")
      )
    )

  # ------------------------------------------------------------------
  # ERM crisis dummy (2-quarter MA of raw_erm)
  # raw_erm: +1 in 1992Q3 and 1992Q4, -1 in 1993Q1 and 1993Q2
  # ------------------------------------------------------------------
  dat <- dat %>%
    mutate(
      raw_erm = case_when(
        date %in% as.Date(c("1992-07-01", "1992-10-01")) ~  1,
        date %in% as.Date(c("1993-01-01", "1993-04-01")) ~ -1,
        TRUE ~ 0
      ),
      d1992_erm = 0.5 * (raw_erm + lag(raw_erm, 1))
    ) %>%
    select(-raw_erm)

  # ------------------------------------------------------------------
  # Fornero pension reform dummy (ogive over 2012Q2 – 2013Q4)
  # Weights: quarter k of 7 gets weight k/8 (so max = 7/8 at 2013Q4)
  # Stays at 7/8 after 2013Q4
  # ------------------------------------------------------------------
  fornero_dates <- as.Date(c(
    "2012-04-01", "2012-07-01", "2012-10-01",
    "2013-01-01", "2013-04-01", "2013-07-01", "2013-10-01"
  ))
  fornero_weights <- seq_len(7L) / 8

  dat <- dat %>%
    mutate(
      d2012_fornero = case_when(
        date < fornero_dates[1L]  ~ 0,
        date > fornero_dates[7L]  ~ fornero_weights[7L],
        TRUE ~ {
          idx <- match(date, fornero_dates)
          ifelse(is.na(idx), 0, fornero_weights[idx])
        }
      )
    )

  # ------------------------------------------------------------------
  # Step dummy 2008Q3 (GFC)
  # ------------------------------------------------------------------
  dat <- dat %>%
    mutate(step2008Q3 = as.integer(date >= as.Date("2008-07-01")))

  # ------------------------------------------------------------------
  # Short-run variables
  # ------------------------------------------------------------------

  # Ensure log_cci is present (derive from cci if needed)
  if (!"log_cci" %in% names(dat) && "cci" %in% names(dat)) {
    dat <- dat %>% mutate(log_cci = log(cci))
  }

  # d2_logcci_lag2: Δ² log CCI lagged 2 periods
  #   Δ² log_cci_t = log_cci_t - 2*log_cci_{t-1} + log_cci_{t-2}
  #   then lag the whole thing by 2
  dat <- dat %>%
    mutate(
      d2_logcci      = log_cci - 2 * lag(log_cci, 1) + lag(log_cci, 2),
      d2_logcci_lag2 = lag(d2_logcci, 2)
    ) %>%
    select(-d2_logcci)

  # d2_public_cons: Δ² log public consumption
  dat <- dat %>%
    mutate(
      d2_public_cons = c(NA_real_, NA_real_,
                         diff(diff(log_public_cons)))
    )

  # dd4_income: ΔΔ₄ log income = diff(lincome,1) - diff(lag(lincome,4),1)
  #   = (lincome - lag(lincome,1)) - (lag(lincome,4) - lag(lincome,5))
  dat <- dat %>%
    mutate(
      dd4_income = (lincome - lag(lincome, 1)) -
                   (lag(lincome, 4) - lag(lincome, 5))
    )

  # d2_log_unemp: Δ² log unemployment rate
  dat <- dat %>%
    mutate(
      d2_log_unemp = c(NA_real_, NA_real_,
                       diff(diff(log(unemp_rate))))
    )

  dat
}


# ==============================================================================
# HELPER: fit one ECM specification with NW/HC robust standard errors
# ==============================================================================

fit_ecm_spec <- function(data, spec_name, lr_vars, sr_vars = character(0),
                         dummy_vars = c("d1992_erm", "d2012_fornero",
                                        "d1990Q3_pre"),
                         response = "dlcons",
                         sample_start = as.Date("1980-01-01"),
                         sample_end   = as.Date("2019-10-01")) {

  # Drop any sr_vars that are entirely NA (e.g., missing data series)
  sr_vars_ok <- sr_vars[vapply(sr_vars, function(v) {
    v %in% names(data) && !all(is.na(data[[v]]))
  }, logical(1))]
  if (length(sr_vars_ok) < length(sr_vars)) {
    dropped <- setdiff(sr_vars, sr_vars_ok)
    message(sprintf("  [%s] Dropping all-NA sr_vars: %s",
                    spec_name, paste(dropped, collapse=", ")))
  }

  all_rhs <- c(lr_vars, sr_vars_ok, dummy_vars)
  req     <- c(response, all_rhs)

  est_data <- data %>%
    filter(date >= sample_start, date <= sample_end) %>%
    filter(complete.cases(across(all_of(req))))

  if (nrow(est_data) < 30L) {
    stop(sprintf("[fit_ecm_spec] Spec '%s': only %d complete observations.",
                 spec_name, nrow(est_data)))
  }

  # Drop dummy_vars that have zero variance in est_data (e.g. pre-sample
  # dummies that are all-zero over the estimation window).  Aliased
  # dummies cause lm() to return NA coefficients, which in turn breaks
  # lmtest::bgtest / dwtest silently.
  dummy_vars_ok <- dummy_vars[vapply(dummy_vars, function(v) {
    v %in% names(est_data) && var(est_data[[v]], na.rm = TRUE) > 0
  }, logical(1))]
  if (length(dummy_vars_ok) < length(dummy_vars)) {
    dropped_d <- setdiff(dummy_vars, dummy_vars_ok)
    message(sprintf("  [%s] Dropping zero-variance dummies (outside sample): %s",
                    spec_name, paste(dropped_d, collapse = ", ")))
  }

  all_rhs_ok <- c(lr_vars, sr_vars_ok, dummy_vars_ok)
  fmla <- reformulate(all_rhs_ok, response = response)
  fit  <- lm(fmla, data = est_data)

  # Drop any remaining perfectly collinear terms (e.g. ln_yp_over_y_post2008
  # equals ln_yp_over_y when sample is entirely post-2008).
  aliased_terms <- names(which(is.na(coef(fit))))
  aliased_terms <- setdiff(aliased_terms, "(Intercept)")
  if (length(aliased_terms) > 0L) {
    message(sprintf("  [%s] Dropping collinear terms (aliased in sample): %s",
                    spec_name, paste(aliased_terms, collapse = ", ")))
    all_rhs_ok <- setdiff(all_rhs_ok, aliased_terms)
    fmla <- reformulate(all_rhs_ok, response = response)
    fit  <- lm(fmla, data = est_data)
  }

  # Newey-West HAC covariance (Andrews 1991 bandwidth)
  bw     <- floor(4 * (nobs(fit) / 100)^(2 / 9))
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
# SECTION D: Six Estimation Specifications
# ==============================================================================
#
# All specifications:
#   - Response:   dlcons (Δ ln real per capita consumption)
#   - Sample:     1980Q1 – 2019Q4
#   - Always include in long-run bracket:  real_rate, ln_y_over_c
#   - Always include dummies:
#       d1992_erm, d2012_fornero, d1990Q3_pre (seasonal correction)
#
# The structural long-run coefficients are recovered by dividing OLS
# estimates by λ (the coefficient on ln_y_over_c).
# ==============================================================================

run_all_specifications <- function(model_data) {

  # Shared dummy set (always present)
  base_dummies <- c("d1992_erm", "d2012_fornero", "d1990Q3_pre")

  # ------------------------------------------------------------------
  # Spec 1 — Conventional log net worth (Column 1 in paper)
  #   Long-run: ln_networth_y, real_rate, ln_yp_over_y, ln_y_over_c
  #   Short-run: none
  # ------------------------------------------------------------------
  spec1 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec1_LogNetWorth",
    lr_vars   = c("ln_networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
    sr_vars   = character(0),
    dummy_vars = base_dummies
  )

  # ------------------------------------------------------------------
  # Spec 2 — Conventional + CCI short-run (Column 2)
  #   Same long-run as Spec 1
  #   Short-run adds: d2_logcci_lag2
  # ------------------------------------------------------------------
  spec2 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec2_LogNetWorth_CCI",
    lr_vars   = c("ln_networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
    sr_vars   = "d2_logcci_lag2",
    dummy_vars = base_dummies
  )

  # ------------------------------------------------------------------
  # Spec 3 — Conventional net worth in levels (Column 3)
  #   Long-run: networth_y (level), real_rate, ln_yp_over_y, ln_y_over_c
  #   Short-run: none
  # ------------------------------------------------------------------
  spec3 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec3_LevelNetWorth",
    lr_vars   = c("networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
    sr_vars   = character(0),
    dummy_vars = base_dummies
  )

  # ------------------------------------------------------------------
  # Spec 4 — NFA + housing assets (Column 4)
  #   Long-run: nfa_y, ha_y, ln_hp_over_y, real_rate, ln_yp_over_y,
  #             ln_y_over_c
  #   Short-run: d2_logcci_lag2
  # ------------------------------------------------------------------
  spec4 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec4_NFA_Housing",
    lr_vars   = c("nfa_y", "ha_y", "ln_hp_over_y",
                  "real_rate", "ln_yp_over_y", "ln_y_over_c"),
    sr_vars   = "d2_logcci_lag2",
    dummy_vars = base_dummies
  )

  # ------------------------------------------------------------------
  # Spec 5 — Full disaggregation, no GFC shift (Column 5)
  #   Long-run: nla_y, bonds_y, ilfa_y, ha_y, ln_hp_over_y,
  #             real_rate, ln_yp_over_y, ln_y_over_c
  #   Short-run: d2_logcci_lag2, d2_public_cons, dd4_income,
  #              d2_log_unemp, abs_income_resid
  #   Extra impulse dummies added to base set
  # ------------------------------------------------------------------
  spec5_dummies <- c(base_dummies, "d1983Q1", "d1993Q1", "d2013Q1")

  spec5 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec5_FullDisagg",
    lr_vars   = c("nla_y", "bonds_y", "ilfa_y", "ha_y", "ln_hp_over_y",
                  "real_rate", "ln_yp_over_y", "ln_y_over_c"),
    sr_vars   = c("d2_logcci_lag2", "d2_public_cons", "dd4_income",
                  "d2_log_unemp",   "abs_income_resid"),
    dummy_vars = spec5_dummies
  )

  # ------------------------------------------------------------------
  # Spec 6 — Preferred: full disaggregation + GFC shift in φ (Column 6)
  #   Same as Spec 5 but adds ln_yp_over_y_post2008 to long-run
  # ------------------------------------------------------------------
  spec6 <- fit_ecm_spec(
    data      = model_data,
    spec_name = "Spec6_Preferred",
    lr_vars   = c("nla_y", "bonds_y", "ilfa_y", "ha_y", "ln_hp_over_y",
                  "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                  "ln_y_over_c"),
    sr_vars   = c("d2_logcci_lag2", "d2_public_cons", "dd4_income",
                  "d2_log_unemp",   "abs_income_resid"),
    dummy_vars = spec5_dummies
  )

  list(spec1 = spec1, spec2 = spec2, spec3 = spec3,
       spec4 = spec4, spec5 = spec5, spec6 = spec6)
}


# ==============================================================================
# SECTION E: Results Table (Table 1 equivalent)
# ==============================================================================
#
# build_results_table()
#
# For each specification:
#   1. Extracts OLS coefficients and NW standard errors
#   2. Recovers structural long-run parameters (coef / λ)
#   3. Appends diagnostic statistics
#   4. Returns a tidy long-format tibble and exports CSV
# ==============================================================================

build_results_table <- function(specs, output_dir = "outputs") {

  all_rows <- list()

  spec_names <- names(specs)

  for (sp_name in spec_names) {

    sp   <- specs[[sp_name]]
    fit  <- sp$fit
    vcov <- sp$nw_vcov

    cf      <- coef(fit)
    # Align SE with coefficient vector (vcov excludes aliased/NA terms)
    se_raw  <- sqrt(diag(vcov))
    se      <- rep(NA_real_, length(cf))
    names(se) <- names(cf)
    se[names(se_raw)] <- se_raw
    tstat   <- cf / se
    pval    <- 2 * pt(-abs(tstat), df = nobs(fit) - sum(!is.na(cf)))

    # Speed of adjustment (λ) = coefficient on ln_y_over_c
    lambda <- cf["ln_y_over_c"]
    if (is.na(lambda) || abs(lambda) < 1e-10) {
      warning(sprintf("[build_results_table] λ near zero for %s — ",
                      sp_name), "structural params will be NA.")
      lambda <- NA_real_
    }

    # Diagnostics (Chow break at 2008Q3 — GFC onset, natural structural break)
    diag_row <- tryCatch(
      model_diagnostics(fit, sp$est_data, break_date = "2008-07-01"),
      error = function(e) {
        tibble::tibble(
          n_obs = nobs(fit), se_pct = NA_real_, adj_r2 = NA_real_,
          dw = NA_real_, lm_het_pval = NA_real_, ar1_pval = NA_real_,
          ar4_pval = NA_real_, chow_pval = NA_real_,
          reset_pval = NA_real_, schwarz = NA_real_, loglik = NA_real_
        )
      }
    )

    # Build tidy coefficient frame
    coef_tbl <- tibble::tibble(
      specification   = sp$spec_name,
      term            = names(cf),
      ols_estimate    = unname(cf),
      nw_se           = unname(se),
      t_stat          = unname(tstat),
      p_value         = unname(pval),
      lambda          = lambda,
      # Structural long-run param = OLS coef / λ
      # (only meaningful for long-run regressors; applied to all here —
      #  caller should filter to lr_vars only for interpretation)
      structural_param = unname(cf) / lambda
    ) %>%
      mutate(
        in_long_run = term %in% sp$lr_vars,
        in_short_run = term %in% sp$sr_vars,
        is_dummy     = term %in% sp$dummy_vars
      )

    # Append diagnostic stats as extra columns
    diag_wide <- diag_row %>%
      mutate(specification = sp$spec_name)

    all_rows[[sp_name]] <- list(coef_tbl = coef_tbl, diag = diag_wide)
  }

  # ------------------------------------------------------------------
  # Long-format coefficient table for all specs
  # ------------------------------------------------------------------
  coef_combined <- bind_rows(lapply(all_rows, `[[`, "coef_tbl"))

  # ------------------------------------------------------------------
  # Wide-format summary (one row per term, one column per spec) —
  # mimics Table 1 layout in the paper
  # ------------------------------------------------------------------
  wide_ols <- coef_combined %>%
    select(term, specification, ols_estimate) %>%
    tidyr::pivot_wider(names_from = specification,
                       values_from = ols_estimate,
                       names_prefix = "ols_")

  wide_se <- coef_combined %>%
    select(term, specification, nw_se) %>%
    tidyr::pivot_wider(names_from = specification,
                       values_from = nw_se,
                       names_prefix = "se_")

  wide_struct <- coef_combined %>%
    filter(in_long_run) %>%
    select(term, specification, structural_param) %>%
    tidyr::pivot_wider(names_from = specification,
                       values_from = structural_param,
                       names_prefix = "lr_")

  # Diagnostics wide
  diag_combined <- bind_rows(lapply(all_rows, `[[`, "diag"))

  # ------------------------------------------------------------------
  # Export
  # ------------------------------------------------------------------
  csv_path <- file.path(output_dir, "italy_table1_results.csv")

  # Export: long-format coefficients (most informative for downstream use)
  readr_available <- requireNamespace("readr", quietly = TRUE)
  if (readr_available) {
    readr::write_csv(coef_combined, csv_path)
  } else {
    write.csv(coef_combined, csv_path, row.names = FALSE)
  }
  message(sprintf("[build_results_table] Exported coefficient table to %s",
                  csv_path))

  diag_csv <- file.path(output_dir, "italy_diagnostics.csv")
  if (readr_available) {
    readr::write_csv(diag_combined, diag_csv)
  } else {
    write.csv(diag_combined, diag_csv, row.names = FALSE)
  }
  message(sprintf("[build_results_table] Exported diagnostics to %s",
                  diag_csv))

  # ------------------------------------------------------------------
  # Pretty-print summary to console
  # ------------------------------------------------------------------
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("TABLE 1 — Italy Consumption Model: OLS Estimates\n")
  cat(strrep("=", 70), "\n\n")

  spec_list <- unique(coef_combined$specification)

  for (sp_nm in spec_list) {

    sub <- coef_combined %>% filter(specification == sp_nm)
    lam <- sub$lambda[1L]

    cat(sprintf("  %-40s  λ = %.4f\n", sp_nm, ifelse(is.na(lam), 0, lam)))
    cat(sprintf("  %-40s\n", strrep("-", 60)))
    cat(sprintf("  %-30s  %8s  %8s  %8s  %8s\n",
                "Term", "OLS coef", "NW SE", "Struct", "p-val"))

    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      struct_str <- if (r$in_long_run && !is.na(r$structural_param)) {
        sprintf("%8.4f", r$structural_param)
      } else {
        "        "
      }
      cat(sprintf("  %-30s  %8.4f  %8.4f  %s  %s\n",
                  r$term,
                  r$ols_estimate,
                  r$nw_se,
                  struct_str,
                  format_pval(r$p_value)))
    }

    dg <- diag_combined %>% filter(specification == sp_nm)
    if (nrow(dg) > 0L) {
      cat(sprintf(
        "  Diagnostics: N=%d  SE=%.2f%%  adjR²=%.3f  DW=%.2f\n",
        dg$n_obs, dg$se_pct, dg$adj_r2, dg$dw
      ))
      cat(sprintf(
        "  AR(1) p=%.3f  AR(4) p=%.3f  Chow p=%.3f  RESET p=%.3f\n",
        dg$ar1_pval, dg$ar4_pval,
        ifelse(is.na(dg$chow_pval), NA_real_, dg$chow_pval),
        ifelse(is.na(dg$reset_pval), NA_real_, dg$reset_pval)
      ))
    }
    cat("\n")
  }

  invisible(list(
    coef_table     = coef_combined,
    diagnostics    = diag_combined,
    wide_ols       = wide_ols,
    wide_se        = wide_se,
    wide_structural = wide_struct
  ))
}


# Small helper for p-value formatting
format_pval <- function(p) {
  if (is.na(p)) return("    NA  ")
  if (p < 0.001) return(" <0.001 ")
  sprintf("%7.3f ", p)
}


# ==============================================================================
# SECTION F: Plots
# ==============================================================================

# ------------------------------------------------------------------------------
# F1: Actual vs Fitted — Preferred Model (Spec 6)
# ------------------------------------------------------------------------------

plot_actual_vs_fitted <- function(spec6_result, output_dir = "outputs") {

  fit      <- spec6_result$fit
  est_data <- spec6_result$est_data

  plot_dat <- tibble::tibble(
    date    = est_data$date,
    actual  = fitted(fit) + residuals(fit),   # = actual dlcons
    fitted  = fitted(fit),
    resid   = residuals(fit)
  )

  p <- ggplot(plot_dat, aes(x = date)) +
    geom_line(aes(y = actual * 100, colour = "Actual"),
              linewidth = 0.7) +
    geom_line(aes(y = fitted * 100, colour = "Fitted"),
              linewidth = 0.7, linetype = "dashed") +
    scale_colour_manual(
      values = c("Actual" = "#1f4e79", "Fitted" = "#c00000"),
      name   = NULL
    ) +
    labs(
      title    = "Preferred Model (Spec 6): Actual vs Fitted",
      subtitle = "Quarterly change in log real per capita consumption (%)",
      x        = NULL,
      y        = "Δ ln c  ×  100",
      caption  = paste0(
        "Note: OLS with Newey-West standard errors. ",
        "Sample: 1980Q1 – 2019Q4."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  out_path <- file.path(output_dir, "italy_spec6_actual_vs_fitted.png")
  ggsave(out_path, p, width = 9, height = 5, dpi = 150)
  message(sprintf("[plot_actual_vs_fitted] Saved to %s", out_path))

  # Residual plot
  p_resid <- ggplot(plot_dat, aes(x = date, y = resid * 100)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4) +
    geom_line(colour = "#1f4e79", linewidth = 0.6) +
    labs(
      title    = "Preferred Model (Spec 6): Residuals",
      subtitle = "OLS residuals from consumption ECM",
      x        = NULL,
      y        = "Residual  ×  100"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  resid_path <- file.path(output_dir, "italy_spec6_residuals.png")
  ggsave(resid_path, p_resid, width = 9, height = 4, dpi = 150)
  message(sprintf("[plot_actual_vs_fitted] Residual plot saved to %s",
                  resid_path))

  invisible(p)
}


# ------------------------------------------------------------------------------
# F2: Long-run structural coefficients comparison across specifications
# ------------------------------------------------------------------------------

plot_lr_coefficients <- function(results_table, output_dir = "outputs") {

  # Filter to long-run regressors only, exclude intercept and ln_y_over_c
  # (λ is reported separately)
  lr_coefs <- results_table$coef_table %>%
    filter(in_long_run, term != "(Intercept)", term != "ln_y_over_c") %>%
    filter(!is.na(structural_param)) %>%
    mutate(
      # Clean labels
      term_label = dplyr::recode(term,
        "ln_networth_y"        = "Log net worth/y",
        "networth_y"           = "Net worth/y",
        "nfa_y"                = "Net fin assets/y",
        "nla_y"                = "Net liquid assets/y",
        "bonds_y"              = "Bonds/y",
        "ilfa_y"               = "Illiq fin assets/y",
        "ha_y"                 = "Housing assets/y",
        "ln_hp_over_y"         = "Log house price/y",
        "real_rate"            = "Real interest rate",
        "ln_yp_over_y"         = "ln(y^p/y)",
        "ln_yp_over_y_post2008"= "ln(y^p/y) × post-2008"
      ),
      spec_short = dplyr::recode(specification,
        "Spec1_LogNetWorth"     = "Col.1",
        "Spec2_LogNetWorth_CCI" = "Col.2",
        "Spec3_LevelNetWorth"   = "Col.3",
        "Spec4_NFA_Housing"     = "Col.4",
        "Spec5_FullDisagg"      = "Col.5",
        "Spec6_Preferred"       = "Col.6"
      )
    )

  if (nrow(lr_coefs) == 0L) {
    message("[plot_lr_coefficients] No long-run structural parameters to plot.")
    return(invisible(NULL))
  }

  p <- ggplot(
    lr_coefs,
    aes(x = spec_short, y = structural_param, fill = spec_short)
  ) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
    facet_wrap(~ term_label, scales = "free_y", ncol = 3L) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    labs(
      title    = "Long-run structural parameters across specifications",
      subtitle = "Coefficient / λ  (speed-of-adjustment)",
      x        = "Specification",
      y        = "Structural parameter",
      caption  = paste0(
        "OLS estimates, Newey-West standard errors. ",
        "Sample: 1980Q1 – 2019Q4."
      )
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.text       = element_text(face = "bold", size = 8),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  out_path <- file.path(output_dir, "italy_lr_coefficients_comparison.png")
  ggsave(out_path, p, width = 11, height = 8, dpi = 150)
  message(sprintf("[plot_lr_coefficients] Saved to %s", out_path))

  invisible(p)
}


# ==============================================================================
# MAIN EXECUTION BLOCK
# ==============================================================================
#
# Assumes `model_data` tibble is already in the environment (created by Part 1).
# If running this script standalone for testing, a minimal stub is created.
# ==============================================================================

if (!exists("model_data")) {
  stop(paste0(
    "[italy_estimation.R] 'model_data' not found in environment.\n",
    "  This script is Part 2 — run the data preparation script (Part 1) first\n",
    "  to create and populate the model_data tibble."
  ))
}

message("\n", strrep("=", 70))
message("Italy Consumption Model — Part 2: Estimation Framework")
message(strrep("=", 70))

# --- Step 1: Add dummy and short-run variables ---
message("\n[Step 1] Constructing dummy and short-run variables...")
model_data <- add_model_variables(model_data)

# --- Step 2: Income volatility proxy ---
message("[Step 2] Computing income volatility (AR8 residuals)...")
if (!"abs_income_resid" %in% names(model_data) ||
    all(is.na(model_data$abs_income_resid))) {
  model_data <- compute_income_volatility(model_data)
}

# --- Step 3: Permanent income ---
message("[Step 3] Constructing permanent income series...")
if (!"ln_yp_over_y" %in% names(model_data) ||
    all(is.na(model_data$ln_yp_over_y))) {
  model_data <- construct_permanent_income(model_data, k = 40, delta = 0.95)
} else {
  message("  [Note] ln_yp_over_y already present — skipping construction.")
  # Still ensure post-2008 interaction exists
  if (!"ln_yp_over_y_post2008" %in% names(model_data)) {
    model_data <- model_data %>%
      mutate(ln_yp_over_y_post2008 =
               ln_yp_over_y * as.integer(date >= as.Date("2008-07-01")))
  }
}

# --- Step 4: Estimate all six specifications ---
message("[Step 4] Estimating six consumption specifications...")
specs <- run_all_specifications(model_data)

# --- Step 5: Build and export results table ---
message("[Step 5] Building results table...")
results <- build_results_table(specs, output_dir = output_dir)

# --- Step 6: Plots ---
message("[Step 6] Generating plots...")
plot_actual_vs_fitted(specs$spec6, output_dir = output_dir)
plot_lr_coefficients(results, output_dir = output_dir)

message("\n", strrep("=", 70))
message("Estimation complete. Outputs written to: ", output_dir)
message(strrep("=", 70), "\n")
