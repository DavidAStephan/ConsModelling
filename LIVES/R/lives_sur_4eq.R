# ==============================================================================
# LIVES — Phase A item A6: 4-equation SUR (consumption + HP + M + HEW)
#
# Combines the Phase A items A1 (HEW equation construction), A2 (ζ_h = 1
# normalisation via HP-weighted CCI), A3 (iterated CCI estimation, inherited
# from the upstream Williams CCI fit), and A4 (de-meaned CCI interactions in
# the consumption block — inherited from the headline Spec 8 build).
#
# Four estimation regimes:
#   (a) 4-eq SUR with cci_williams (consumption-fitted, pre-joint baseline)
#   (b) 4-eq SUR with cci_williams_joint   (4-eq joint sign survival, cons-weighted)
#   (c) 4-eq SUR with cci_williams_joint_h (4-eq joint sign survival, HP-weighted — ζ_h = 1)
#   (d) 4-eq SUR with cci_williams_joint_m (4-eq joint sign survival, M-weighted)
#
# Output:
#   LIVES/outputs/lives_sur_4eq_coefs.csv      — coefficients across regimes
#   LIVES/outputs/lives_sur_4eq_residcorr.csv  — residual correlations
#   LIVES/outputs/lives_phase_a_summary.csv    — headline diagnostics
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(systemfit); library(tidyr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))

# Variables required to exist on the model frame
required <- c("cci_williams", "cci_williams_joint",
              "cci_williams_joint_h", "cci_williams_joint_m",
              "hew_proxy", "ecm_lag_W", "ha_y_proxy")
missing <- setdiff(required, names(dat))
if (length(missing) > 0L) {
  stop("lives_model_data.rds missing required columns: ",
       paste(missing, collapse = ", "),
       "\n(Re-run lives_data_prep.R and joint_cci_identification.R first.)")
}

dat$ln_networth_y_extended <- dat$ln_networth_y_proxy

# ----------------------------------------------------------------------------
# Build the four equations as a function of which CCI series to use.
# ----------------------------------------------------------------------------
build_eqns <- function(cci_var) {
  cci_term <- if (is.na(cci_var)) character(0) else cci_var
  list(
    cons = reformulate(c("ln_networth_y_extended", "ln_hp_over_y", "real_rate",
                         "ln_yp_over_y", cci_term, "ecm_lag",
                         "d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                         "d_neg_gearing_8587", "d_recession_1991",
                         "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020"),
                       response = "dlcons"),
    hp   = reformulate(c("lincome", "log_credit_y", "real_rate", "prime_age_share",
                         cci_term, "ecm_lag_H",
                         "d_log_credit_y", "d_real_rate",
                         "d2008_gfc", "d2020_covid", "d2020_rebound",
                         "d_neg_gearing_8587", "d_recession_1991",
                         "d_apra_2014", "d_apra_2017"),
                       response = "dlog_hp_real"),
    m    = reformulate(c("lincome", "ln_hp_over_yd", "real_rate", "prime_age_share",
                         cci_term, "ecm_lag_M",
                         "dlog_hp_real", "d_real_rate",
                         "d2008_gfc", "d2020_covid", "d2020_rebound",
                         "d_neg_gearing_8587", "d_recession_1991",
                         "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020"),
                       response = "dlog_M_real"),
    # HEW equation (Williams Aust eq 13 spirit): housing-equity withdrawal as
    # a function of income, housing wealth, real rate, demographics, CCI, and
    # an ECM term in lagged HEW. Sign priors:
    #   lincome > 0, ha_y_proxy > 0, real_rate < 0, prime_age_share > 0,
    #   cci > 0 (looser credit → more withdrawal), ecm_lag_W < 0 (mean reversion).
    w    = reformulate(c("lincome", "ha_y_proxy", "real_rate", "prime_age_share",
                         cci_term, "ecm_lag_W",
                         "dlog_hp_real", "d_real_rate",
                         "d2008_gfc", "d2020_covid", "d2020_rebound",
                         "d_recession_1991",
                         "d_apra_2014", "d_apra_2017"),
                       response = "hew_proxy")
  )
}

# ----------------------------------------------------------------------------
# Common-sample window across all four equations.
# ----------------------------------------------------------------------------
common_vars <- c("dlcons", "dlog_hp_real", "dlog_M_real", "hew_proxy",
                 "ln_networth_y_extended", "ln_hp_over_y", "real_rate",
                 "ln_yp_over_y", "ecm_lag",
                 "lincome", "log_credit_y", "prime_age_share", "ecm_lag_H",
                 "ecm_lag_M", "ecm_lag_W", "ln_hp_over_yd",
                 "d_log_credit_y", "d_real_rate", "ha_y_proxy",
                 "cci_williams", "cci_williams_joint",
                 "cci_williams_joint_h", "cci_williams_joint_m",
                 "d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

common <- dat %>%
  filter(date >= as.Date("1976-07-01")) %>%
  filter(complete.cases(across(all_of(common_vars))))

cat(sprintf("Common 4-eq sample: %d obs, %s to %s\n",
            nrow(common), format(min(common$date)), format(max(common$date))))

# ----------------------------------------------------------------------------
# Fit each regime
# ----------------------------------------------------------------------------
regimes <- list(
  a = list(label = "a_cons_only_cci",       cci = "cci_williams"),
  b = list(label = "b_joint_cci_cons_wt",   cci = "cci_williams_joint"),
  c = list(label = "c_joint_cci_HP_wt",     cci = "cci_williams_joint_h"),
  d = list(label = "d_joint_cci_M_wt",      cci = "cci_williams_joint_m")
)

fit_sur_regime <- function(spec_list, data, label) {
  cat(sprintf("\n--- Regime %s ---\n", label))
  fit <- systemfit::systemfit(spec_list, method = "SUR", data = data)
  fit
}

fits <- lapply(regimes, function(r) {
  eqns <- build_eqns(r$cci)
  fit_sur_regime(eqns, common, r$label)
})

# ----------------------------------------------------------------------------
# Pull coefficient tables
# ----------------------------------------------------------------------------
pull_sur_eq <- function(sur_obj, eq_index, eq_name, regime_label) {
  sm <- summary(sur_obj)
  cm <- sm$eq[[eq_index]]$coefficients
  tibble(
    regime    = regime_label,
    equation  = eq_name,
    term      = rownames(cm),
    estimate  = cm[, "Estimate"],
    std_error = cm[, "Std. Error"],
    t_stat    = cm[, "t value"],
    p_value   = cm[, "Pr(>|t|)"]
  )
}

eq_names <- c("consumption", "house_price", "mortgage", "hew")

coefs_all <- bind_rows(lapply(seq_along(regimes), function(k) {
  r <- regimes[[k]]
  bind_rows(lapply(seq_along(eq_names), function(j) {
    pull_sur_eq(fits[[k]], j, eq_names[j], r$label)
  }))
}))

write_csv(coefs_all,
          file.path(PROJ_LIVES, "outputs", "lives_sur_4eq_coefs.csv"))
cat(sprintf("\nSaved: LIVES/outputs/lives_sur_4eq_coefs.csv  (%d rows)\n",
            nrow(coefs_all)))

# ----------------------------------------------------------------------------
# Residual correlation matrix per regime
# ----------------------------------------------------------------------------
resid_corr <- function(sur_obj, label) {
  cmat <- cov2cor(summary(sur_obj)$residCov)
  tibble(
    regime    = label,
    pair      = c("C_H", "C_M", "C_W", "H_M", "H_W", "M_W"),
    rho       = c(cmat[1, 2], cmat[1, 3], cmat[1, 4],
                  cmat[2, 3], cmat[2, 4], cmat[3, 4])
  )
}
resid_tbl <- bind_rows(lapply(seq_along(regimes), function(k) {
  resid_corr(fits[[k]], regimes[[k]]$label)
}))
write_csv(resid_tbl,
          file.path(PROJ_LIVES, "outputs", "lives_sur_4eq_residcorr.csv"))
cat(sprintf("Saved: LIVES/outputs/lives_sur_4eq_residcorr.csv\n"))

cat("\n--- Residual correlations ---\n")
for (k in seq_along(regimes)) {
  cat(sprintf("\nRegime %s:\n", regimes[[k]]$label))
  print(resid_corr(fits[[k]], regimes[[k]]$label) %>% select(pair, rho),
        n = Inf, digits = 4)
}

# ----------------------------------------------------------------------------
# Headline diagnostics: CCI loading sign across equations, λ across equations
# ----------------------------------------------------------------------------
diag_rows <- list()
for (k in seq_along(regimes)) {
  r <- regimes[[k]]
  cci_col <- r$cci
  cf <- coefs_all %>% filter(regime == r$label)
  for (eq in eq_names) {
    cci_row <- cf %>% filter(equation == eq, term == cci_col)
    lambda_row <- cf %>% filter(equation == eq,
                                str_detect(term, "ecm_lag(_H|_M|_W)?$"))
    diag_rows[[length(diag_rows) + 1L]] <- tibble(
      regime          = r$label,
      equation        = eq,
      cci_coef        = if (nrow(cci_row)) cci_row$estimate else NA_real_,
      cci_t           = if (nrow(cci_row)) cci_row$t_stat else NA_real_,
      cci_sign_prior  = c(consumption = +1, house_price = +1,
                          mortgage = +1, hew = +1)[eq],
      cci_sign_ok     = if (nrow(cci_row)) sign(cci_row$estimate) == 1 else NA,
      lambda          = if (nrow(lambda_row)) lambda_row$estimate else NA_real_
    )
  }
}
diag_tbl <- bind_rows(diag_rows)
write_csv(diag_tbl,
          file.path(PROJ_LIVES, "outputs", "lives_phase_a_summary.csv"))
cat(sprintf("\nSaved: LIVES/outputs/lives_phase_a_summary.csv\n"))

cat("\n--- CCI sign and λ across equations (4-eq SUR) ---\n")
print(diag_tbl, n = Inf, digits = 4)

# ----------------------------------------------------------------------------
# Highlight the A2 test: under HP-weighted CCI, is the M sign violation fixed?
# ----------------------------------------------------------------------------
cat("\n--- A2 ζ_h = 1 normalisation test ---\n")
for (k in c("a", "b", "c", "d")) {
  r <- regimes[[k]]
  cci_col <- r$cci
  m_cci <- coefs_all %>% filter(regime == r$label, equation == "mortgage",
                                 term == cci_col)
  w_cci <- coefs_all %>% filter(regime == r$label, equation == "hew",
                                 term == cci_col)
  cat(sprintf("  Regime %s: M-eq CCI = %+.4f (t=%+.2f), HEW-eq CCI = %+.4f (t=%+.2f)\n",
              r$label,
              ifelse(nrow(m_cci),  m_cci$estimate, NA),
              ifelse(nrow(m_cci),  m_cci$t_stat,   NA),
              ifelse(nrow(w_cci),  w_cci$estimate, NA),
              ifelse(nrow(w_cci),  w_cci$t_stat,   NA)))
}

cat("\nDone.\n")
