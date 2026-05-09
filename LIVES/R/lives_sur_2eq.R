# ==============================================================================
# LIVES — phase 1: two-equation SUR (consumption + house prices)
#
# Joint Zellner SUR estimation of:
#   (C) Spec-1-aggregate consumption equation (using ln_networth_y_proxy)
#   (H) Williams Table-2-style house price ECM
#
# Both equations share the cci_williams regressor (constructed upstream
# from the consumption-equation Williams maximal-GETS reduction). SUR
# does NOT impose cross-equation parameter restrictions; it allows
# cov(eps_C, eps_H) ≠ 0 and re-weights observations accordingly. The
# residual correlation rho_hat is the headline structural quantity.
#
# Output:
#   LIVES/outputs/lives_sur_2eq_coefs.csv     — coefficients from each equation
#   LIVES/outputs/lives_sur_2eq_compare.csv   — SUR vs single-equation OLS
#   LIVES/outputs/lives_sur_2eq_resid_corr.csv — residual covariance + corr
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(systemfit)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Ausreplication"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))
cat(sprintf("Loaded LIVES model data: %d rows\n", nrow(dat)))

# ----------------------------------------------------------------------------
# Equation specifications
# ----------------------------------------------------------------------------
# Use Spec 1 (aggregate networth proxy) for consumption — same as the
# back-extension placebo + refit work, so we're directly comparable.
# Use the corrected HP ECM (lag(log_hp_real) anchor).
#
# Both equations include the same cci_williams_lvl term so the SUR can
# tell us whether the loading sign on cci is consistent across equations
# (Williams' prior: same-sign with positive consumption effect AND positive
# house-price effect). The standalone OLS gave NEGATIVE on HP — the SUR
# may or may not change that.
# ----------------------------------------------------------------------------

# Alias the aggregate-proxy as the canonical "ln_networth_y" so the
# template matches single-equation Spec 1 verbatim.
dat <- dat %>%
  mutate(ln_networth_y_extended = ln_networth_y_proxy)

# C-equation: Spec 1 with aggregate proxy
c_lr     <- c("ln_networth_y_extended", "ln_hp_over_y", "real_rate",
              "ln_yp_over_y", "ecm_lag")
c_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
               "d_neg_gearing_8587", "d_recession_1991",
               "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

# H-equation: Williams Table 2 spirit
h_lr      <- c("lincome", "log_credit_y", "real_rate", "prime_age_share",
               "cci_williams_lvl", "ecm_lag_H")
h_sr      <- c("d_log_credit_y", "d_real_rate")
h_dummies <- c("d2008_gfc", "d2020_covid", "d2020_rebound",
               "d_neg_gearing_8587", "d_recession_1991",
               "d_apra_2014", "d_apra_2017")

# Build formulas
c_rhs <- c(c_lr, c_dummies)
h_rhs <- c(h_lr, h_sr, h_dummies)

# Drop zero-variance dummies first based on full sample
trim_zero_var <- function(d, vars) {
  vars[vapply(vars, function(v) {
    v %in% names(d) && var(d[[v]], na.rm = TRUE) > 0
  }, logical(1))]
}

# ----------------------------------------------------------------------------
# Common-sample restriction: SUR requires the same observations in every
# equation. Take complete cases across BOTH equations' RHS plus both LHS
# plus dates inside the back-extended sample.
# ----------------------------------------------------------------------------
all_vars <- unique(c("dlcons", "dlog_hp_real", c_lr, c_dummies,
                     h_lr, h_sr, h_dummies))
common <- dat %>%
  filter(date >= as.Date("1976-07-01")) %>%
  filter(complete.cases(across(all_of(all_vars))))

# Drop zero-var dummies on the common sample (different from the per-equation
# trim because SUR estimates on common sample only)
c_dummies <- trim_zero_var(common, c_dummies)
h_dummies <- trim_zero_var(common, h_dummies)
c_rhs <- c(c_lr, c_dummies)
h_rhs <- c(h_lr, h_sr, h_dummies)

cat(sprintf("\nCommon SUR sample: %d obs, %s to %s\n",
            nrow(common), format(min(common$date)), format(max(common$date))))
cat(sprintf("  C-equation regressors (%d): %s\n",
            length(c_rhs), paste(c_rhs, collapse = ", ")))
cat(sprintf("  H-equation regressors (%d): %s\n",
            length(h_rhs), paste(h_rhs, collapse = ", ")))

c_formula <- reformulate(c_rhs, response = "dlcons")
h_formula <- reformulate(h_rhs, response = "dlog_hp_real")

# ----------------------------------------------------------------------------
# Step 1: equation-by-equation OLS (baseline for comparison)
# ----------------------------------------------------------------------------
cat("\n--- Step 1: equation-by-equation OLS ---\n")
fit_c_ols <- lm(c_formula, data = common)
fit_h_ols <- lm(h_formula, data = common)

cat(sprintf("  C-equation OLS: n=%d, R^2=%.4f, adj R^2=%.4f, RMSE=%.5f\n",
            nobs(fit_c_ols),
            summary(fit_c_ols)$r.squared,
            summary(fit_c_ols)$adj.r.squared,
            sqrt(mean(residuals(fit_c_ols)^2))))
cat(sprintf("  H-equation OLS: n=%d, R^2=%.4f, adj R^2=%.4f, RMSE=%.5f\n",
            nobs(fit_h_ols),
            summary(fit_h_ols)$r.squared,
            summary(fit_h_ols)$adj.r.squared,
            sqrt(mean(residuals(fit_h_ols)^2))))

# Residual correlation under OLS
res_c <- residuals(fit_c_ols)
res_h <- residuals(fit_h_ols)
rho_ols <- cor(res_c, res_h)
cat(sprintf("  cov(eps_C, eps_H) under OLS = %.6f, rho = %.4f\n",
            cov(res_c, res_h), rho_ols))

# ----------------------------------------------------------------------------
# Step 2: SUR
# ----------------------------------------------------------------------------
cat("\n--- Step 2: SUR (Zellner) ---\n")
eqns <- list(cons = c_formula, hp = h_formula)
fit_sur <- systemfit(eqns, method = "SUR", data = common)
sm_sur <- summary(fit_sur)
print(sm_sur, equations = FALSE)

# Pull SUR coefficients per equation
sur_coefs <- function(eq_name) {
  cf_tbl <- sm_sur$eq[[which(names(eqns) == eq_name)]]
  cmat <- cf_tbl$coefficients
  tibble(
    term      = rownames(cmat),
    estimate  = cmat[, "Estimate"],
    std_error = cmat[, "Std. Error"],
    t_stat    = cmat[, "t value"],
    p_value   = cmat[, "Pr(>|t|)"]
  )
}
sur_c <- sur_coefs("cons")
sur_h <- sur_coefs("hp")

# Long-run structural conversion for each equation's λ
struct_c <- {
  lam <- sur_c$estimate[sur_c$term == "ecm_lag"]
  sur_c %>%
    mutate(lr_struct = if_else(term %in% c_lr & term != "ecm_lag",
                                -estimate / lam, NA_real_))
}
struct_h <- {
  lam <- sur_h$estimate[sur_h$term == "ecm_lag_H"]
  sur_h %>%
    mutate(lr_struct = if_else(term %in% h_lr & term != "ecm_lag_H",
                                -estimate / lam, NA_real_))
}

# Save coefficient tables
write_csv(bind_rows(struct_c %>% mutate(equation = "consumption", .before = 1),
                    struct_h %>% mutate(equation = "house_price", .before = 1)),
          file.path(PROJ_LIVES, "outputs", "lives_sur_2eq_coefs.csv"))

# ----------------------------------------------------------------------------
# Step 3: side-by-side OLS vs SUR comparison
# ----------------------------------------------------------------------------
cat("\n--- Step 3: OLS vs SUR coefficient comparison ---\n")

ols_coefs <- function(fit) {
  cf <- summary(fit)$coefficients
  tibble(term = rownames(cf), ols_estimate = cf[, "Estimate"],
         ols_se = cf[, "Std. Error"])
}

cmp_c <- left_join(
  sur_c %>% rename(sur_estimate = estimate, sur_se = std_error) %>%
    select(term, sur_estimate, sur_se),
  ols_coefs(fit_c_ols), by = "term"
) %>%
  mutate(equation = "consumption",
         pct_change_est = 100 * (sur_estimate - ols_estimate) / ols_estimate,
         se_ratio       = sur_se / ols_se)

cmp_h <- left_join(
  sur_h %>% rename(sur_estimate = estimate, sur_se = std_error) %>%
    select(term, sur_estimate, sur_se),
  ols_coefs(fit_h_ols), by = "term"
) %>%
  mutate(equation = "house_price",
         pct_change_est = 100 * (sur_estimate - ols_estimate) / ols_estimate,
         se_ratio       = sur_se / ols_se)

cmp <- bind_rows(cmp_c, cmp_h) %>%
  select(equation, term, ols_estimate, sur_estimate, pct_change_est,
         ols_se, sur_se, se_ratio)
print(cmp, n = Inf, digits = 4)
write_csv(cmp, file.path(PROJ_LIVES, "outputs", "lives_sur_2eq_compare.csv"))

# ----------------------------------------------------------------------------
# Step 4: residual correlation under SUR (the headline structural quantity)
# ----------------------------------------------------------------------------
cat("\n--- Step 4: residual covariance + correlation ---\n")
sigma_sur <- sm_sur$residCov
rho_sur <- sigma_sur[1, 2] / sqrt(sigma_sur[1, 1] * sigma_sur[2, 2])
cat(sprintf("  SUR residual covariance matrix:\n"))
print(sigma_sur, digits = 6)
cat(sprintf("\n  rho(eps_C, eps_H) under OLS = %.4f\n", rho_ols))
cat(sprintf("  rho(eps_C, eps_H) under SUR = %.4f\n", rho_sur))
cat(sprintf("  (these are essentially equal by construction; SUR uses the OLS\n"))
cat(sprintf("   residuals to estimate Sigma in a 2-step procedure)\n"))

# Residual-covariance interpretation
verdict <- if (abs(rho_sur) > 0.5) {
  "STRONG cross-equation linkage — supports LIVES framework"
} else if (abs(rho_sur) > 0.25) {
  "MODERATE cross-equation linkage — common shock present"
} else if (abs(rho_sur) > 0.1) {
  "WEAK cross-equation linkage — modest efficiency gain from SUR"
} else {
  "NEGLIGIBLE cross-equation linkage — single-equation OLS approximately efficient"
}
cat(sprintf("\n  >>> Residual-correlation verdict: %s\n", verdict))

# Save residual-correlation summary
write_csv(tibble(
  metric = c("OLS rho(eps_C, eps_H)",
             "SUR rho(eps_C, eps_H)",
             "SUR sigma_CC",
             "SUR sigma_HH",
             "SUR sigma_CH",
             "Verdict"),
  value  = c(sprintf("%.4f", rho_ols),
             sprintf("%.4f", rho_sur),
             sprintf("%.6f", sigma_sur[1, 1]),
             sprintf("%.6f", sigma_sur[2, 2]),
             sprintf("%.6f", sigma_sur[1, 2]),
             verdict)
), file.path(PROJ_LIVES, "outputs", "lives_sur_2eq_resid_corr.csv"))

cat(sprintf("\nSaved: LIVES/outputs/lives_sur_2eq_coefs.csv\n"))
cat(sprintf("Saved: LIVES/outputs/lives_sur_2eq_compare.csv\n"))
cat(sprintf("Saved: LIVES/outputs/lives_sur_2eq_resid_corr.csv\n"))
