# ==============================================================================
# LIVES — phase 1 data preparation
#
# Loads the master dataset built by Australia/R/australia_data_download.R
# and adds the variables needed by the house-price equation (and any other
# equations introduced in later phases).
#
# Output:
#   LIVES/outputs/lives_model_data.rds  — prepped tibble keyed on quarterly date
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

master_rds <- file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")
stopifnot(file.exists(master_rds))
master <- readRDS(master_rds)
cat(sprintf("Loaded master: %d rows, %d cols (1976Q3+ extended)\n",
            nrow(master), ncol(master)))

# ----------------------------------------------------------------------------
# Apply the same renames the Australia estimation pipeline uses, then
# re-source its helpers (without main exec) to get add_model_variables(),
# compute_income_volatility(), construct_permanent_income_italy(), and the
# Williams CCI machinery. This keeps the consumption block bit-for-bit
# identical to the single-equation paper.
# ----------------------------------------------------------------------------
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)

model_data <- master %>%
  rename(dlcons  = d_ln_cons_pc,
         lincome = ln_ydi_real_pc,
         lcons   = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)

add_model_variables              <- helpers_env$add_model_variables
compute_income_volatility        <- helpers_env$compute_income_volatility
construct_permanent_income_italy <- helpers_env$construct_permanent_income_italy
fit_ecm_spec                     <- helpers_env$fit_ecm_spec
fit_consumption_with_williams_cci <- helpers_env$fit_consumption_with_williams_cci

model_data <- add_model_variables(model_data)
model_data <- compute_income_volatility(model_data)
model_data <- construct_permanent_income_italy(model_data)
model_data <- model_data %>%
  mutate(ecm_lag      = lag(lcons, 1L) - lincome,
         ln_hp_over_y = log(hpi / exp(lincome) * (cons_deflator_norm / 100)))

# ----------------------------------------------------------------------------
# Williams maximal-GETS CCI: fit consumption block first to get cci_williams
# as a side product. Phase 1 SUR will treat cci_williams as a *fixed*
# regressor in both equations (i.e. we don't iterate to joint convergence;
# that's deferred to phase 3 FIML).
# ----------------------------------------------------------------------------
base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
spec4_lr <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
              "real_rate", "ln_yp_over_y", "ecm_lag")

cat("\nFitting consumption Spec-4-with-Williams-CCI to extract cci_williams...\n")
williams_fit <- fit_consumption_with_williams_cci(
  model_data,
  lr_vars    = spec4_lr,
  sr_vars    = character(0),
  dummy_vars = base_dummies,
  sample_end = max(model_data$date)
)
model_data <- williams_fit$model_data    # has cci_williams attached
cat(sprintf("  cci_williams: %d non-NA obs, range [%.3f, %.3f], surviving knots = %d\n",
            sum(!is.na(model_data$cci_williams)),
            min(model_data$cci_williams, na.rm = TRUE),
            max(model_data$cci_williams, na.rm = TRUE),
            length(williams_fit$surviving_knots)))
cat("  Surviving knots:", paste(williams_fit$surviving_knots, collapse = ", "), "\n")

# ----------------------------------------------------------------------------
# House-price equation regressors
# ----------------------------------------------------------------------------
# log_hp_real      = log(hpi / cons_deflator) — real house price, indexed
# dlog_hp_real     = quarterly first-difference of log_hp_real (LHS)
# log_hp_over_yd   = log(real hp / real disposable income per capita) —
#                    long-run-equilibrium ratio (used in HP ECM term)
# ecm_lag_H        = log(hp_{t-1} / y_t) — restoring force in HP equation
# log_credit_y     = log(fin_loans_proxy / ydi_ann_nom) — credit-availability
#                    proxy for HP equation (uses the back-extended fin_loans
#                    series so HP equation can fit on the longer sample)
# real_rate_x_cci  = real_rate × cci_williams interaction
#
# The single-equation pipeline already builds ln_hp_over_y as
# log(hpi / (ydi_ann_nom / pop_millions / (cons_deflator_norm/100))) which
# is real house price relative to real income per capita. We reuse it as
# the long-run anchor ln_hp_over_yd and define the ECM lag from it.
# ----------------------------------------------------------------------------
model_data <- model_data %>%
  mutate(
    # Real house-price level (an index). cons_deflator_norm is 100-based
    # (=100 in 2022-23). hpi is itself an index; scale doesn't matter for
    # log-differences, only for the ECM term (which divides by income).
    log_hp_real      = log(hpi / (cons_deflator_norm / 100)),
    dlog_hp_real     = log_hp_real - lag(log_hp_real, 1L),

    # Long-run anchor: ln(hp / yd_real_pc) — same as ln_hp_over_y already
    # in master (kept aliased for clarity in the HP equation context).
    ln_hp_over_yd    = ln_hp_over_y,
    # Original "income-anchored" ECM: lag(log(hp/yd)). Doesn't work as a
    # restoring force on the Australian sample because hp/yd has trended
    # up ~4x from 1976-2024 (no mean reversion to a constant ratio).
    ecm_lag_H_ratio  = lag(ln_hp_over_yd, 1L),
    # Used by the HP equation: lag(log_hp_real) lets income/credit enter
    # as separate LR regressors and the data identify the long-run
    # cointegrating combination implicitly via the lagged level. Standard
    # Engle-Granger ECM convention. Sign prior on this regressor: < 0.
    ecm_lag_H        = lag(log_hp_real, 1L),

    # Credit availability for HP equation: log(real-mortgage-stock-to-income).
    # Use fin_loans_proxy where available so HP equation can fit on the
    # back-extended sample; falls back to fin_loans (= NA pre-1988) if the
    # proxy hasn't been built. The ratio normalises by income to remove
    # nominal scale.
    log_credit_y = if ("fin_loans_proxy" %in% names(.)) {
      log(pmax(fin_loans_proxy / ydi_ann_nom, 1e-6))
    } else {
      log(pmax(fin_loans / ydi_ann_nom, 1e-6))
    },

    # CCI interactions for the HP equation
    real_rate_x_cci = real_rate * cci_williams,
    cci_williams_lvl = cci_williams,    # direct CCI effect

    # Δ short-run regressors (for HP equation SR dynamics, parallel to
    # consumption equation's d2_logcci_lag2 / dd4_income / etc.)
    d_log_credit_y    = log_credit_y - lag(log_credit_y, 1L),
    d_real_rate       = real_rate - lag(real_rate, 1L),
    d4_log_hp_real    = log_hp_real - lag(log_hp_real, 4L),    # year-on-year HP growth

    # ----- Mortgage-stock equation regressors (Williams Table 3 spirit)
    # Real mortgage stock ≈ real household liabilities. Use fin_loans_proxy
    # (extended back to 1976Q3 via credit_total_d02 growth-rate splice) so
    # the mortgage-stock equation can fit on the longer sample. Caveat:
    # fin_loans is total household liabilities, not housing-specific; on
    # the modern sample housing dominates (~85%) so it's close enough.
    log_M_real        = log(if ("fin_loans_proxy" %in% names(.))
                              fin_loans_proxy / (cons_deflator_norm / 100)
                            else fin_loans / (cons_deflator_norm / 100)),
    dlog_M_real       = log_M_real - lag(log_M_real, 1L),
    log_M_over_y      = log_M_real - lincome,
    # Use lag(log_M_real) and let income enter as a separate LR regressor;
    # this is the same Engle-Granger pattern as the HP equation. The data
    # identifies whatever long-run combination of (income, hp/y, rate, etc.)
    # is cointegrating with M_real. Sign prior on this regressor: < 0.
    ecm_lag_M         = lag(log_M_real, 1L)
  )

# ----------------------------------------------------------------------------
# Quick coverage report
# ----------------------------------------------------------------------------
cat("\nLIVES key variable coverage:\n")
for (v in c("dlcons", "lcons", "lincome", "ecm_lag",
            "ha_y", "nla_y", "eq_y", "super_y",
            "ha_y_proxy", "nla_y_proxy", "eq_y_proxy", "super_y_proxy",
            "cci_williams",
            "log_hp_real", "dlog_hp_real", "ln_hp_over_yd", "ecm_lag_H",
            "log_credit_y", "real_rate", "real_rate_x_cci",
            "prime_age_share")) {
  if (v %in% names(model_data)) {
    nn <- sum(!is.na(model_data[[v]]))
    if (nn > 0) {
      cat(sprintf("  %-22s nn=%d, %s..%s\n", v, nn,
        format(min(model_data$date[!is.na(model_data[[v]])])),
        format(max(model_data$date[!is.na(model_data[[v]])]))))
    } else cat(sprintf("  %-22s ALL NA\n", v))
  } else cat(sprintf("  %-22s NOT FOUND\n", v))
}

# Save
out_path <- file.path(PROJ_LIVES, "outputs", "lives_model_data.rds")
saveRDS(model_data, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
