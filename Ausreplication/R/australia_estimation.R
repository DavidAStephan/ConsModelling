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
# Model:
#   Δ ln c_t = λ[α_0 + α_c·CCI_t + α_1·r_t + Σγ_i·(A_i,t-1/y_t)
#              + φ·ln(y^p_t/y_t) + ln(y_t/c_{t-1})] + short-run + ε_t
#
# Structural params = estimated OLS coef / λ
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

if (!exists("model_data")) {
  stop(
    "[australia_estimation.R] 'model_data' not found in environment.\n",
    "  This script is Part 2 — run australia_consumption_model.R to source",
    " Part 1 first.\n"
  )
}

cat(rep("=", 70), "\n", sep = "")
cat("Australia Consumption Model — Part 2: Estimation Framework\n")
cat(rep("=", 70), "\n\n", sep = "")


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

construct_permanent_income <- function(model_data, k = 40L, delta = 0.95) {

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

  # GFC learning adjustment (same ogive as Italy)
  gfc_start  <- as.Date("2008-07-01")
  adj_end    <- as.Date("2012-04-01")
  n_adj_qtrs <- 15L

  dat <- dat %>%
    mutate(
      learning_weight = case_when(
        date < gfc_start ~ 1.0,
        date >= adj_end  ~ 0.5,
        TRUE ~ {
          q_since <- as.integer(
            round(as.numeric(difftime(date, gfc_start, units = "days")) / 91.25)
          )
          1.0 - 0.5 * (pmin(q_since, n_adj_qtrs) / n_adj_qtrs)
        }
      ),
      ln_yp_over_y          = ln_yp_over_y * learning_weight,
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
  chow_pval <- tryCatch({
    fit_dates <- data$date[as.integer(rownames(model.frame(fit)))]
    bp_idx    <- sum(fit_dates <= as.Date(break_date), na.rm = TRUE)
    if (bp_idx < 5L || bp_idx > (n - 5L)) NA_real_
    else as.numeric(strucchange::sctest(fit, type = "Chow", point = bp_idx)$p.value)
  }, error = function(e) NA_real_)
  reset_pval <- tryCatch(
    lmtest::resettest(fit, power = 2L, type = "fitted")$p.value,
    error = function(e) NA_real_
  )
  tibble::tibble(
    n_obs = n, se_pct = s$sigma * 100, adj_r2 = s$adj.r.squared,
    dw = dw_val, lm_het_pval = het_pval, ar1_pval = ar1_pval,
    ar4_pval = ar4_pval, chow_pval = chow_pval,
    reset_pval = reset_pval, schwarz = BIC(fit), loglik = as.numeric(logLik(fit))
  )
}


# ==============================================================================
# SECTION D: Short-run and Dummy Variable Construction
# ==============================================================================

add_model_variables <- function(model_data) {
  dat <- model_data %>% arrange(date)

  # ---- Impulse dummies -------------------------------------------------------
  # GST: large one-off boost to nominal consumption July 2000 (Q3)
  # GFC: sharp drop Q3 2008; Q4 2008/Q1 2009 also affected
  # COVID: lockdown Q2 2020 (massive drop) + Q3 2020 rebound
  dat <- dat %>%
    mutate(
      d2000_gst     = as.integer(date == as.Date("2000-07-01")),
      d2008_gfc     = as.integer(date == as.Date("2008-07-01")),
      d2020_covid   = as.integer(date == as.Date("2020-04-01")),
      d2020_rebound = as.integer(date == as.Date("2020-07-01"))
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
                                        "d2020_covid", "d2020_rebound"),
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
# SECTION F: Six Estimation Specifications
# ==============================================================================
#
# Response:  dlcons = Δ ln(real per capita consumption)
# ECM term:  ln_y_over_c = ln(income/lagged consumption)
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

run_all_specifications <- function(model_data, sample_end = as.Date("2024-10-01")) {

  base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound")

  # ------------------------------------------------------------------
  # Spec 1: Log net worth (conventional baseline)
  # ------------------------------------------------------------------
  spec1 <- fit_ecm_spec(
    data       = model_data,
    spec_name  = "Spec1_LogNetWorth",
    lr_vars    = c("ln_networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
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
    lr_vars    = c("ln_networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
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
    lr_vars    = c("networth_y", "real_rate", "ln_yp_over_y", "ln_y_over_c"),
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
                   "real_rate", "ln_yp_over_y", "ln_y_over_c"),
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
                   "real_rate", "ln_yp_over_y", "ln_y_over_c"),
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
                   "ln_y_over_c"),
    sr_vars    = c("d2_logcci_lag2", "dd4_income",
                   "d2_log_unemp", "abs_income_resid"),
    dummy_vars = base_dummies,
    sample_end = sample_end
  )

  list(spec1 = spec1, spec2 = spec2, spec3 = spec3,
       spec4 = spec4, spec5 = spec5, spec6 = spec6)
}


# ==============================================================================
# SECTION G: Results Table
# ==============================================================================

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

    lambda <- cf["ln_y_over_c"]
    if (is.na(lambda) || abs(lambda) < 1e-10) {
      warning(sprintf("[build_results_table] λ near zero for %s", sp_name))
      lambda <- NA_real_
    }

    diag_row <- tryCatch(
      model_diagnostics(fit, sp$est_data, break_date = "2008-07-01"),
      error = function(e) tibble::tibble(
        n_obs = nobs(fit), se_pct = NA_real_, adj_r2 = NA_real_,
        dw = NA_real_, lm_het_pval = NA_real_, ar1_pval = NA_real_,
        ar4_pval = NA_real_, chow_pval = NA_real_,
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
      structural_param = unname(cf) / lambda
    ) %>%
      mutate(
        in_long_run  = term %in% sp$lr_vars,
        in_short_run = term %in% sp$sr_vars,
        is_dummy     = term %in% sp$dummy_vars
      )

    all_rows[[sp_name]] <- list(coef_tbl = coef_tbl, diag = diag_row)
  }

  coef_combined <- bind_rows(lapply(all_rows, `[[`, "coef_tbl"))
  diag_combined <- bind_rows(lapply(all_rows, `[[`, "diag")) %>%
    mutate(specification = names(specs))

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
                 "ln_y_over_c")

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
    filter(term %in% key_terms, in_long_run | term == "ln_y_over_c",
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
  # Italy: ecm_lag coefficient from ecm_coefficients.csv (negative = restoring)
  # Australia: coefficient on ln_y_over_c (positive convention; abs value comparable)
  # ------------------------------------------------------------------
  italy_ecm_path <- file.path(italy_dir, "ecm_coefficients.csv")
  italy_lambda <- tryCatch({
    ec <- read.csv(italy_ecm_path, stringsAsFactors = FALSE)
    row <- ec[ec$term == "ecm_lag", ]
    if (nrow(row) > 0L) row$estimate[1L] else NA_real_
  }, error = function(e) NA_real_)

  aus_lambda <- aus_coef %>%
    filter(term == "ln_y_over_c") %>%
    select(specification, period, lambda) %>%
    distinct()

  cat("\n--- Speed of adjustment (lambda) comparison ---\n")
  cat(sprintf("  Italy (ecm_lag, full sample):  %8.4f  [abs = %.4f per quarter]\n",
              italy_lambda, abs(italy_lambda)))
  cat(sprintf("  %-8s  %-35s  %8s  %8s\n", "Period", "Australia specification", "lambda", "|lambda|"))
  cat(sprintf("  %s\n", strrep("-", 65)))
  for (i in seq_len(nrow(aus_lambda))) {
    cat(sprintf("  %-8s  %-35s  %8.4f  %8.4f\n",
      aus_lambda$period[i], aus_lambda$specification[i],
      aus_lambda$lambda[i],  abs(aus_lambda$lambda[i])))
  }

  lambda_path <- file.path(output_dir, "italy_australia_lambda.csv")
  lambda_tbl <- bind_rows(
    tibble(country = "Italy", specification = "Italy_Preferred (full)",
           period = "full", lambda = italy_lambda),
    aus_lambda %>% mutate(country = "Australia")
  )
  write.csv(lambda_tbl, lambda_path, row.names = FALSE)
  message(sprintf("[build_comparison_table] Lambda table saved to %s", lambda_path))

  invisible(comparison)
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
# MAIN EXECUTION
# ==============================================================================

cat("[Step 1] Constructing dummy and short-run variables...\n")
model_data <- add_model_variables(model_data)

cat("[Step 2] Computing income volatility (AR8 residuals)...\n")
model_data <- compute_income_volatility(model_data)

cat("[Step 3] Constructing permanent income series...\n")
model_data <- construct_permanent_income(model_data)

# Add variables that depend on permanent income
model_data <- model_data %>%
  mutate(
    # Ensure ln_y_over_c uses lagged consumption
    ln_y_over_c = lincome - lag(lcons, 1L),
    # ln(HP/y): log real house price relative to real income per capita
    ln_hp_over_y = log(hpi / exp(lincome) * (cons_deflator_norm / 100))
  )

n_pi <- sum(!is.na(model_data$ln_yp_over_y))
message(sprintf("[construct_permanent_income] ln_yp_over_y: %d non-NA obs", n_pi))

cat("[Step 4] Estimating specifications — two sample periods...\n")
cat("  [4a] Full sample (1988Q4–2024Q4)\n")
specs_full     <- run_all_specifications(model_data, sample_end = as.Date("2024-10-01"))
cat("  [4b] Pre-COVID sample (1988Q4–2019Q4)\n")
specs_precovid <- run_all_specifications(model_data, sample_end = as.Date("2019-10-01"))

cat("[Step 5] Building results tables...\n")
results_full     <- build_results_table(specs_full,     output_dir, period_label = "full")
results_precovid <- build_results_table(specs_precovid, output_dir, period_label = "precovid")

# Combined file with both periods
combined_coef <- bind_rows(results_full$coef_combined, results_precovid$coef_combined)
combined_diag <- bind_rows(results_full$diag_combined, results_precovid$diag_combined)
write.csv(combined_coef, file.path(output_dir, "australia_all_results.csv"),     row.names = FALSE)
write.csv(combined_diag, file.path(output_dir, "australia_all_diagnostics.csv"), row.names = FALSE)

cat("[Step 6] Building Italy–Australia comparison table...\n")
italy_dir <- normalizePath(file.path(output_dir, "../../outputs"), winslash = "/")
build_comparison_table(aus_coef = combined_coef, output_dir = output_dir,
                       italy_dir = italy_dir)

cat("[Step 7] Generating plots...\n")
plot_actual_vs_fitted(specs_full$spec6, output_dir = output_dir)
plot_actual_vs_fitted(specs_full$spec1, output_dir = output_dir)

cat(rep("=", 70), "\n", sep = "")
cat("Estimation complete. Outputs written to:", output_dir, "\n")
cat(rep("=", 70), "\n", sep = "")
