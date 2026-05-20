# ==============================================================================
# Phase B item B2 — formal test of Williams' Table 1 calibrations as
# parameter restrictions on the consumption equation.
#
# Williams (2010) calibrates a set of long-run structural γ:
#   γ_HA  ∈ [0.0452, 0.0488]   (peak vs end-of-sample housing-wealth m.p.c.)
#   γ_IFA = 0.022              (illiquid financial, calibrated not estimated)
#   γ_NLA = 0.159              (net liquid assets — Williams)
#   ψ_0   = 0.20               (permanent-income weight at CCI = 0)
#   ϖ     = 1.2                (wealth × (1 − ϖ·CCI) interaction)
#   λ     = −0.286             (speed of adjustment)
#
# Are these jointly accepted on the contemporary Australian data?
#
# We refit Spec 6 (the preferred disaggregated specification) and conduct a
# Wald test using car::linearHypothesis with the Newey–West covariance.
# Each γ_i restriction is OLS_coef_i + γ_i × λ = 0  (since OLS = λ × γ).
# We test individually, in groups, and jointly.
#
# Output:
#   LIVES/outputs/williams_calibration_wald.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(car); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

# ----------------------------------------------------------------------------
# Re-create Spec 6 on the canonical Italy-LP master dataset by sourcing the
# helpers and running fit_ecm_spec directly.
# ----------------------------------------------------------------------------
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)

src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)
fit_ecm_spec <- helpers_env$fit_ecm_spec

master <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds"))

# Re-derive the Spec 6 LHS / RHS exactly as the canonical pipeline does
master <- master %>%
  rename(dlcons  = d_ln_cons_pc,
         lincome = ln_ydi_real_pc,
         lcons   = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

# Add the post-2008 PI break and the SR variables required by Spec 6
master <- helpers_env$add_model_variables(master)
master <- helpers_env$compute_income_volatility(master)
master <- helpers_env$construct_permanent_income_italy(master)
master <- master %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

spec6 <- fit_ecm_spec(
  data       = master,
  spec_name  = "Spec6_PreferredForWaldTest",
  lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008",
                 "ecm_lag"),
  sr_vars    = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp",
                 "abs_income_resid"),
  dummy_vars = base_dummies,
  sample_end = as.Date("2024-10-01")
)

fit <- spec6$fit
vcv <- spec6$nw_vcov

cf <- coef(fit)
lambda_hat <- cf[["ecm_lag"]]
cat(sprintf("Spec 6 λ̂ = %.4f\n", lambda_hat))

# ----------------------------------------------------------------------------
# Williams Table 1 calibrations.
# Restriction format for linearHypothesis: each row is one linear restriction
# on the OLS coefficient vector, encoded as
#     OLS_coef_i  ==  γ_target_i × λ_hat
# We treat λ as fixed at its estimated value to make the restriction linear
# in the remaining OLS coefficients. (A joint restriction including λ would
# be non-linear in OLS_coef_i / λ, requiring a delta-method Wald.)
# ----------------------------------------------------------------------------
williams <- tibble::tribble(
  ~term,            ~gamma_target, ~description,
  "ha_y",                  0.0488, "Housing wealth γ at CCI peak (Williams)",
  "eq_y",                  0.0110, "Illiquid financial — equity share (Williams 0.022/2)",
  "super_y",               0.0110, "Illiquid financial — super share (Williams 0.022/2)",
  "nla_y",                 0.1590, "Net liquid assets γ (Williams)",
  "ln_hp_over_y",         -0.1300, "log(p^h/y) at CCI=0 (Williams)",
  "ln_yp_over_y",          0.2000, "ψ_0 (Williams calibrated)"
) %>%
  mutate(implied_ols  = gamma_target * lambda_hat,
         restriction  = sprintf("%s = %.6f", term, implied_ols))

# Per-coefficient Wald tests
per_coef <- list()
for (i in seq_len(nrow(williams))) {
  tt <- williams$term[i]
  hyp <- williams$restriction[i]
  res <- car::linearHypothesis(fit, hypothesis.matrix = hyp,
                                vcov. = vcv, test = "Chisq")
  per_coef[[i]] <- tibble(
    term         = tt,
    gamma_target = williams$gamma_target[i],
    implied_ols  = williams$implied_ols[i],
    chi2         = res$Chisq[2],
    df           = res$Df[2],
    p_value      = res$`Pr(>Chisq)`[2]
  )
}
per_coef_tbl <- bind_rows(per_coef) %>%
  mutate(reject_5pct = p_value < 0.05,
         reject_1pct = p_value < 0.01)

# Joint Wald: all six restrictions simultaneously
joint_hyp <- williams$restriction
joint_res <- car::linearHypothesis(fit, hypothesis.matrix = joint_hyp,
                                    vcov. = vcv, test = "Chisq")
joint_row <- tibble(
  term         = "JOINT_ALL_6",
  gamma_target = NA_real_,
  implied_ols  = NA_real_,
  chi2         = joint_res$Chisq[2],
  df           = joint_res$Df[2],
  p_value      = joint_res$`Pr(>Chisq)`[2],
  reject_5pct  = joint_res$`Pr(>Chisq)`[2] < 0.05,
  reject_1pct  = joint_res$`Pr(>Chisq)`[2] < 0.01
)

# Wealth-only joint Wald (the 4 wealth coefficient restrictions together)
wealth_hyp <- williams %>% filter(term %in% c("ha_y", "eq_y", "super_y", "nla_y")) %>%
  pull(restriction)
wealth_res <- car::linearHypothesis(fit, hypothesis.matrix = wealth_hyp,
                                     vcov. = vcv, test = "Chisq")
wealth_row <- tibble(
  term         = "JOINT_WEALTH_4",
  gamma_target = NA_real_,
  implied_ols  = NA_real_,
  chi2         = wealth_res$Chisq[2],
  df           = wealth_res$Df[2],
  p_value      = wealth_res$`Pr(>Chisq)`[2],
  reject_5pct  = wealth_res$`Pr(>Chisq)`[2] < 0.05,
  reject_1pct  = wealth_res$`Pr(>Chisq)`[2] < 0.01
)

out_tbl <- bind_rows(per_coef_tbl, wealth_row, joint_row)
out_tbl <- out_tbl %>%
  mutate(across(c(chi2, p_value), ~ round(.x, 4)))

write_csv(out_tbl,
          file.path(PROJ_LIVES, "outputs", "williams_calibration_wald.csv"))
cat("\n--- Williams Table 1 calibration Wald tests ---\n")
print(out_tbl, n = Inf, digits = 4)
cat(sprintf("\nSaved: LIVES/outputs/williams_calibration_wald.csv\n"))

# ----------------------------------------------------------------------------
# Interpretation hint
# ----------------------------------------------------------------------------
cat("\n--- Interpretation ---\n")
cat(sprintf("Joint test of all 6 Williams calibrations: χ²(6) = %.2f, p = %.4f\n",
            joint_row$chi2, joint_row$p_value))
cat(sprintf("Joint test of 4 wealth calibrations only:  χ²(4) = %.2f, p = %.4f\n",
            wealth_row$chi2, wealth_row$p_value))
cat(sprintf("Per-coefficient rejections at 5%%: %s\n",
            paste(per_coef_tbl$term[per_coef_tbl$reject_5pct], collapse = ", ")))
