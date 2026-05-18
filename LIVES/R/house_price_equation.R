# ==============================================================================
# LIVES — standalone house-price ECM (Williams Table 2 in spirit)
#
# Williams (2010) Table 2 specifies an ECM in real log house prices with
# real-income, credit-availability, real-rate, demographic, and CCI
# regressors plus an income-anchored long-run ECM term. This script fits
# that single equation by OLS as a *sanity check* before the 2-equation
# SUR. We expect:
#   - α_inc > 0 (richer households can sustain higher prices)
#   - α_credit > 0 (credit availability supports prices)
#   - α_realrate < 0 (higher rates depress prices)
#   - α_prime_age > 0 (prime-age population pushes demand)
#   - α_cci > 0 (looser conditions support prices)
#   - λ_H < 0 (mean reversion to income-anchored long run)
#
# Output:
#   LIVES/outputs/hp_equation_standalone.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))
cat(sprintf("Loaded LIVES model data: %d rows\n", nrow(dat)))

# Source the fit_ecm_spec helper from the Australia estimation script
src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)
fit_ecm_spec <- helpers_env$fit_ecm_spec

# ----------------------------------------------------------------------------
# House-price equation specification
# ----------------------------------------------------------------------------
# LHS:    Δlog hp_real
# LR:     lincome                     (real disposable income per cap)
#         log_credit_y                 (real mortgage stock / income)
#         real_rate                    (mortgage rate − annual CPI inflation)
#         prime_age_share              (demographic demand)
#         cci_williams_lvl             (direct credit-conditions effect)
#         ecm_lag_H                    (lag of ln(hp/yd) — restoring force)
# SR:     d_log_credit_y               (Δ credit-to-income, current quarter)
#         d_real_rate                  (Δ real rate, current quarter)
# Dummies: house-relevant subset of base_dummies (drop d2000_gst — GST is a
#         consumption shock, not directly a house-price one).
# ----------------------------------------------------------------------------
hp_lr_vars <- c("lincome", "log_credit_y", "real_rate", "prime_age_share",
                "cci_williams_lvl", "ecm_lag_H")
hp_sr_vars <- c("d_log_credit_y", "d_real_rate")
hp_dummies <- c("d2008_gfc", "d2020_covid", "d2020_rebound",
                "d_neg_gearing_8587", "d_recession_1991",
                "d_apra_2014", "d_apra_2017")

cat("\nHouse-price ECM (single-equation OLS, full extended sample):\n")
cat("  LHS:     dlog_hp_real\n")
cat(sprintf("  LR:      %s\n", paste(hp_lr_vars, collapse = ", ")))
cat(sprintf("  SR:      %s\n", paste(hp_sr_vars, collapse = ", ")))
cat(sprintf("  Dummies: %s\n", paste(hp_dummies, collapse = ", ")))

fit_hp <- fit_ecm_spec(
  data         = dat,
  spec_name    = "HP_Williams_T2",
  response     = "dlog_hp_real",
  lr_vars      = hp_lr_vars,
  sr_vars      = hp_sr_vars,
  dummy_vars   = hp_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(dat$date)
)

cat(sprintf("\nFit: n=%d, %s to %s, R^2=%.4f, adj R^2=%.4f, RMSE=%.5f\n",
            nrow(fit_hp$est_data),
            format(min(fit_hp$est_data$date)),
            format(max(fit_hp$est_data$date)),
            summary(fit_hp$fit)$r.squared,
            summary(fit_hp$fit)$adj.r.squared,
            sqrt(mean(residuals(fit_hp$fit)^2))))

# Long-run structural coefficients: -coef / λ_H
cf <- summary(fit_hp$fit)$coefficients
lambda_H <- cf["ecm_lag_H", "Estimate"]
cat(sprintf("\n--- Long-run structural coefficients (LR coef = -coef/lambda_H) ---\n"))
cat(sprintf("  lambda_H (ecm_lag_H) = %.4f  (sign prior: < 0)\n", lambda_H))

lr_terms <- setdiff(hp_lr_vars, "ecm_lag_H")
sign_priors <- c(lincome = ">0", log_credit_y = ">0", real_rate = "<0",
                 prime_age_share = ">0", cci_williams_lvl = ">0")

lr_struct <- tibble(
  term = lr_terms,
  estimate    = cf[lr_terms, "Estimate"],
  std_error   = cf[lr_terms, "Std. Error"],
  t_stat      = cf[lr_terms, "t value"],
  p_value     = cf[lr_terms, "Pr(>|t|)"],
  lr_struct   = -cf[lr_terms, "Estimate"] / lambda_H,
  sign_prior  = sign_priors[lr_terms],
  prior_match = (sign(-cf[lr_terms, "Estimate"] / lambda_H) ==
                  ifelse(sign_priors[lr_terms] == ">0", 1, -1))
)
print(lr_struct, n = Inf, digits = 4)

# Short-run + dummies
cat("\n--- Short-run + dummies ---\n")
sr_terms <- intersect(c(hp_sr_vars, hp_dummies, "(Intercept)"),
                      rownames(cf))
print(tibble(
  term = sr_terms,
  estimate = cf[sr_terms, "Estimate"],
  std_error = cf[sr_terms, "Std. Error"],
  t_stat   = cf[sr_terms, "t value"],
  p_value  = cf[sr_terms, "Pr(>|t|)"]
), n = Inf, digits = 4)

# Sign-prior assessment
cat("\n--- Sign-prior assessment ---\n")
n_match <- sum(lr_struct$prior_match, na.rm = TRUE)
n_total <- length(lr_terms)
cat(sprintf("  %d of %d long-run coefficients match sign priors\n",
            n_match, n_total))
violators <- lr_struct$term[!lr_struct$prior_match]
if (length(violators) > 0L) {
  cat("  Sign-violators (will need attention in SUR):", paste(violators, collapse = ", "), "\n")
} else {
  cat("  All long-run coefficients match priors — clean specification.\n")
}

# Save
out <- bind_rows(
  lr_struct %>% mutate(block = "long_run", .before = 1),
  tibble(block = "short_run",
         term  = sr_terms,
         estimate = cf[sr_terms, "Estimate"],
         std_error = cf[sr_terms, "Std. Error"],
         t_stat = cf[sr_terms, "t value"],
         p_value = cf[sr_terms, "Pr(>|t|)"],
         lr_struct = NA_real_,
         sign_prior = NA_character_,
         prior_match = NA),
  tibble(block = "lambda",
         term  = "ecm_lag_H",
         estimate = lambda_H,
         std_error = cf["ecm_lag_H", "Std. Error"],
         t_stat = cf["ecm_lag_H", "t value"],
         p_value = cf["ecm_lag_H", "Pr(>|t|)"],
         lr_struct = NA_real_,
         sign_prior = "<0",
         prior_match = lambda_H < 0)
)
out_path <- file.path(PROJ_LIVES, "outputs", "hp_equation_standalone.csv")
write_csv(out, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
