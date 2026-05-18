# ==============================================================================
# LIVES — phase 3: 3-equation SUR with JOINT-identified CCI
#
# 3-equation system (consumption + house prices + mortgage stock) using
# `cci_williams_joint` — the CCI built by requiring sign-prior survival
# across ALL THREE equations (see joint_cci_identification.R).
#
# This is the phase-3 deliverable: the substantive test of whether
# cross-equation CCI identification (a) closes the Williams wealth-
# coefficient gap and (b) eliminates the cci sign violations that
# the phase-1 SUR exhibited.
#
# Three estimation regimes compared:
#   (a) Single-equation OLS, consumption-fitted CCI   (status quo)
#   (b) Single-equation OLS, joint-identified CCI     (cross-eq sign-survived)
#   (c) 3-equation SUR, joint-identified CCI          (joint estimation)
#
# Output:
#   LIVES/outputs/lives_sur_3eq_coefs.csv
#   LIVES/outputs/lives_phase3_comparison.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(systemfit)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))
stopifnot("cci_williams_joint" %in% names(dat))
dat$ln_networth_y_extended <- dat$ln_networth_y_proxy

# ----------------------------------------------------------------------------
# Define equations parametrised by which CCI to use
# ----------------------------------------------------------------------------
build_eqns <- function(cci_var) {
  # cci_var in {"cci_williams", "cci_williams_joint", NA = no CCI}
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
                       response = "dlog_M_real")
  )
}

# Determine the common-sample window across all 3 equations
all_lhs_rhs <- c("dlcons", "dlog_hp_real", "dlog_M_real",
                 "ln_networth_y_extended", "ln_hp_over_y", "real_rate",
                 "ln_yp_over_y", "ecm_lag",
                 "lincome", "log_credit_y", "prime_age_share", "ecm_lag_H",
                 "ecm_lag_M", "ln_hp_over_yd",
                 "d_log_credit_y", "d_real_rate",
                 "cci_williams", "cci_williams_joint",
                 "d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
common <- dat %>%
  filter(date >= as.Date("1976-07-01")) %>%
  filter(complete.cases(across(all_of(all_lhs_rhs))))

cat(sprintf("Common 3-eq sample: %d obs, %s to %s\n",
            nrow(common), format(min(common$date)), format(max(common$date))))

# ----------------------------------------------------------------------------
# Regime (a): single-equation OLS, consumption-fitted CCI
# ----------------------------------------------------------------------------
cat("\n--- Regime (a): single-eq OLS, consumption-fitted cci_williams ---\n")
eq_a <- build_eqns("cci_williams")
fit_a_c <- lm(eq_a$cons, data = common)
fit_a_h <- lm(eq_a$hp,   data = common)
fit_a_m <- lm(eq_a$m,    data = common)

# ----------------------------------------------------------------------------
# Regime (b): single-equation OLS, joint-identified CCI
# ----------------------------------------------------------------------------
cat("--- Regime (b): single-eq OLS, joint-identified cci_williams_joint ---\n")
eq_b <- build_eqns("cci_williams_joint")
fit_b_c <- lm(eq_b$cons, data = common)
fit_b_h <- lm(eq_b$hp,   data = common)
fit_b_m <- lm(eq_b$m,    data = common)

# ----------------------------------------------------------------------------
# Regime (c): 3-equation SUR with joint-identified CCI
# ----------------------------------------------------------------------------
cat("--- Regime (c): 3-eq SUR, joint-identified cci_williams_joint ---\n")
fit_c_sur <- systemfit(eq_b, method = "SUR", data = common)
sm_sur <- summary(fit_c_sur)

# ----------------------------------------------------------------------------
# Helper: pull (estimate, se, t) for the LR vars of an equation from a fit
# ----------------------------------------------------------------------------
pull_lm <- function(fit, terms) {
  cf <- summary(fit)$coefficients
  m <- cf[intersect(terms, rownames(cf)), c("Estimate", "Std. Error"), drop = FALSE]
  tibble(term = rownames(m),
         estimate = m[, "Estimate"],
         std_error = m[, "Std. Error"])
}
pull_sur <- function(sur_obj, eq_index, terms) {
  cmat <- sur_obj$eq[[eq_index]]$coefficients
  keep <- intersect(terms, rownames(cmat))
  tibble(term = keep,
         estimate = cmat[keep, "Estimate"],
         std_error = cmat[keep, "Std. Error"])
}

c_terms <- c("ln_networth_y_extended", "ln_hp_over_y", "real_rate",
             "ln_yp_over_y", "cci_williams", "cci_williams_joint", "ecm_lag")
h_terms <- c("lincome", "log_credit_y", "real_rate", "prime_age_share",
             "cci_williams", "cci_williams_joint", "ecm_lag_H")
m_terms <- c("lincome", "ln_hp_over_yd", "real_rate", "prime_age_share",
             "cci_williams", "cci_williams_joint", "ecm_lag_M")

# Long-run structural conversion
lr_struct <- function(tbl, terms_x_lambda) {
  # terms_x_lambda: a vector that EXCLUDES the ecm_lag term itself
  lam_term <- tbl$term[grepl("^ecm_lag", tbl$term)][1]
  if (is.na(lam_term)) return(tbl)
  lambda <- tbl$estimate[tbl$term == lam_term]
  tbl %>%
    mutate(lr_struct = if_else(term %in% terms_x_lambda,
                                -estimate / lambda, NA_real_),
           lambda    = lambda)
}

# Build comparison table (consumption equation across regimes)
tbl_c_a <- pull_lm(fit_a_c, c_terms) %>%
  mutate(regime = "a_cons_only_cci", .before = 1) %>%
  lr_struct(setdiff(c_terms, "ecm_lag"))
tbl_c_b <- pull_lm(fit_b_c, c_terms) %>%
  mutate(regime = "b_joint_cci_OLS", .before = 1) %>%
  lr_struct(setdiff(c_terms, "ecm_lag"))
tbl_c_c <- pull_sur(sm_sur, 1L, c_terms) %>%
  mutate(regime = "c_joint_cci_SUR", .before = 1) %>%
  lr_struct(setdiff(c_terms, "ecm_lag"))

tbl_h_a <- pull_lm(fit_a_h, h_terms) %>%
  mutate(regime = "a_cons_only_cci", .before = 1) %>%
  lr_struct(setdiff(h_terms, "ecm_lag_H"))
tbl_h_b <- pull_lm(fit_b_h, h_terms) %>%
  mutate(regime = "b_joint_cci_OLS", .before = 1) %>%
  lr_struct(setdiff(h_terms, "ecm_lag_H"))
tbl_h_c <- pull_sur(sm_sur, 2L, h_terms) %>%
  mutate(regime = "c_joint_cci_SUR", .before = 1) %>%
  lr_struct(setdiff(h_terms, "ecm_lag_H"))

tbl_m_a <- pull_lm(fit_a_m, m_terms) %>%
  mutate(regime = "a_cons_only_cci", .before = 1) %>%
  lr_struct(setdiff(m_terms, "ecm_lag_M"))
tbl_m_b <- pull_lm(fit_b_m, m_terms) %>%
  mutate(regime = "b_joint_cci_OLS", .before = 1) %>%
  lr_struct(setdiff(m_terms, "ecm_lag_M"))
tbl_m_c <- pull_sur(sm_sur, 3L, m_terms) %>%
  mutate(regime = "c_joint_cci_SUR", .before = 1) %>%
  lr_struct(setdiff(m_terms, "ecm_lag_M"))

all_coefs <- bind_rows(
  tbl_c_a %>% mutate(equation = "consumption", .before = 1),
  tbl_c_b %>% mutate(equation = "consumption", .before = 1),
  tbl_c_c %>% mutate(equation = "consumption", .before = 1),
  tbl_h_a %>% mutate(equation = "house_price", .before = 1),
  tbl_h_b %>% mutate(equation = "house_price", .before = 1),
  tbl_h_c %>% mutate(equation = "house_price", .before = 1),
  tbl_m_a %>% mutate(equation = "mortgage",    .before = 1),
  tbl_m_b %>% mutate(equation = "mortgage",    .before = 1),
  tbl_m_c %>% mutate(equation = "mortgage",    .before = 1)
)
write_csv(all_coefs,
          file.path(PROJ_LIVES, "outputs", "lives_sur_3eq_coefs.csv"))

# ----------------------------------------------------------------------------
# Pretty-print the consumption equation comparison (the headline test:
# does joint CCI identification change wealth coefs?)
# ----------------------------------------------------------------------------
print_compare <- function(eq_name, tbl_a, tbl_b, tbl_c) {
  cat(sprintf("\n--- %s equation: regime comparison ---\n", eq_name))
  combined <- bind_rows(
    tbl_a %>% select(regime, term, estimate, lr_struct, lambda),
    tbl_b %>% select(regime, term, estimate, lr_struct, lambda),
    tbl_c %>% select(regime, term, estimate, lr_struct, lambda)
  )
  # Wide format: term × regime
  wide <- combined %>%
    select(term, regime, estimate, lr_struct) %>%
    tidyr::pivot_wider(names_from = regime,
                       values_from = c(estimate, lr_struct))
  print(wide, n = Inf, digits = 4)
  lambdas <- combined %>%
    distinct(regime, lambda) %>% filter(!is.na(lambda))
  cat("  Lambdas: ")
  cat(paste(sprintf("%s=%.4f", lambdas$regime, lambdas$lambda), collapse = ", "),
      "\n")
}

print_compare("CONSUMPTION", tbl_c_a, tbl_c_b, tbl_c_c)
print_compare("HOUSE PRICE", tbl_h_a, tbl_h_b, tbl_h_c)
print_compare("MORTGAGE",    tbl_m_a, tbl_m_b, tbl_m_c)

# Residual correlations
res_a <- residuals(fit_b_c)  # joint CCI single-eq for cons
sigma_sur <- sm_sur$residCov
cat(sprintf("\n--- 3-eq SUR residual covariance (joint-CCI regime c) ---\n"))
print(sigma_sur, digits = 6)
cor_sur <- cov2cor(sigma_sur)
cat(sprintf("\n--- 3-eq SUR residual correlation ---\n"))
print(cor_sur, digits = 4)

# Headline summary CSV
cci_c_b <- tbl_c_b$estimate[tbl_c_b$term == "cci_williams_joint"]
cci_h_b <- tbl_h_b$estimate[tbl_h_b$term == "cci_williams_joint"]
cci_m_b <- tbl_m_b$estimate[tbl_m_b$term == "cci_williams_joint"]
cci_c_a <- tbl_c_a$estimate[tbl_c_a$term == "cci_williams"]
cci_h_a <- tbl_h_a$estimate[tbl_h_a$term == "cci_williams"]
cci_m_a <- tbl_m_a$estimate[tbl_m_a$term == "cci_williams"]

summary_tbl <- tibble(
  metric = c("Consumption  λ (regime a)", "Consumption  λ (regime b)", "Consumption  λ (regime c)",
             "Consumption  ha_y / netw LR (regime a)",
             "Consumption  ha_y / netw LR (regime b)",
             "Consumption  ha_y / netw LR (regime c)",
             "Consumption  cci coefficient sign (regime a)",
             "Consumption  cci coefficient sign (regime b)",
             "Consumption  cci coefficient sign (regime c)",
             "House price  cci coefficient sign (regime a)",
             "House price  cci coefficient sign (regime b)",
             "House price  cci coefficient sign (regime c)",
             "Mortgage     cci coefficient sign (regime a)",
             "Mortgage     cci coefficient sign (regime b)",
             "Mortgage     cci coefficient sign (regime c)",
             "SUR rho(eps_C, eps_H)",
             "SUR rho(eps_C, eps_M)",
             "SUR rho(eps_H, eps_M)"),
  value  = c(sprintf("%.4f", tbl_c_a$lambda[1]),
             sprintf("%.4f", tbl_c_b$lambda[1]),
             sprintf("%.4f", tbl_c_c$lambda[1]),
             sprintf("%.4f", tbl_c_a$lr_struct[tbl_c_a$term == "ln_networth_y_extended"]),
             sprintf("%.4f", tbl_c_b$lr_struct[tbl_c_b$term == "ln_networth_y_extended"]),
             sprintf("%.4f", tbl_c_c$lr_struct[tbl_c_c$term == "ln_networth_y_extended"]),
             sprintf("%.5f (%s)", cci_c_a, ifelse(cci_c_a > 0, "+", "-")),
             sprintf("%.5f (%s)", cci_c_b, ifelse(cci_c_b > 0, "+", "-")),
             sprintf("%.5f (%s)", tbl_c_c$estimate[tbl_c_c$term == "cci_williams_joint"],
                     ifelse(tbl_c_c$estimate[tbl_c_c$term == "cci_williams_joint"] > 0, "+", "-")),
             sprintf("%.5f (%s)", cci_h_a, ifelse(cci_h_a > 0, "+", "-")),
             sprintf("%.5f (%s)", cci_h_b, ifelse(cci_h_b > 0, "+", "-")),
             sprintf("%.5f (%s)", tbl_h_c$estimate[tbl_h_c$term == "cci_williams_joint"],
                     ifelse(tbl_h_c$estimate[tbl_h_c$term == "cci_williams_joint"] > 0, "+", "-")),
             sprintf("%.5f (%s)", cci_m_a, ifelse(cci_m_a > 0, "+", "-")),
             sprintf("%.5f (%s)", cci_m_b, ifelse(cci_m_b > 0, "+", "-")),
             sprintf("%.5f (%s)", tbl_m_c$estimate[tbl_m_c$term == "cci_williams_joint"],
                     ifelse(tbl_m_c$estimate[tbl_m_c$term == "cci_williams_joint"] > 0, "+", "-")),
             sprintf("%.4f", cor_sur[1, 2]),
             sprintf("%.4f", cor_sur[1, 3]),
             sprintf("%.4f", cor_sur[2, 3]))
)
write_csv(summary_tbl,
          file.path(PROJ_LIVES, "outputs", "lives_phase3_comparison.csv"))

cat(sprintf("\nSaved: LIVES/outputs/lives_sur_3eq_coefs.csv\n"))
cat(sprintf("Saved: LIVES/outputs/lives_phase3_comparison.csv\n"))
