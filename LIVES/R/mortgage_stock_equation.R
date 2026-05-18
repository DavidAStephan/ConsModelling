# ==============================================================================
# LIVES — standalone mortgage-stock ECM (Williams Table 3 in spirit)
#
# Δlog(real mortgage stock) on:
#   LR: real income, log(hp/y), real_rate, prime_age_share, cci_williams,
#       lag(log_M_real)  [ECM term]
#   SR: Δlog(hp_real), Δreal_rate
# Sign priors:
#   α_inc > 0 (income supports debt capacity)
#   α_HP > 0 (rising HP increases mortgage demand via leverage)
#   α_R < 0 (rates suppress new borrowing)
#   α_DEMOG > 0 (more prime-age population = more borrowing)
#   η > 0 (looser CCI → more borrowing)
#   λ_M < 0 (mean reversion to long-run M-to-income ratio)
#
# Output:
#   LIVES/outputs/mortgage_stock_equation_standalone.csv
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

src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)
fit_ecm_spec <- helpers_env$fit_ecm_spec

m_lr_vars <- c("lincome", "ln_hp_over_yd", "real_rate", "prime_age_share",
               "cci_williams_lvl", "ecm_lag_M")
m_sr_vars <- c("dlog_hp_real", "d_real_rate")
m_dummies <- c("d2008_gfc", "d2020_covid", "d2020_rebound",
               "d_neg_gearing_8587", "d_recession_1991",
               "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

cat("\nMortgage-stock ECM (single-equation OLS, full extended sample):\n")
cat("  LHS:     dlog_M_real\n")
cat(sprintf("  LR:      %s\n", paste(m_lr_vars, collapse = ", ")))
cat(sprintf("  SR:      %s\n", paste(m_sr_vars, collapse = ", ")))

fit_m <- fit_ecm_spec(
  data         = dat,
  spec_name    = "M_Williams_T3",
  response     = "dlog_M_real",
  lr_vars      = m_lr_vars,
  sr_vars      = m_sr_vars,
  dummy_vars   = m_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(dat$date)
)

cat(sprintf("\nFit: n=%d, %s to %s, R^2=%.4f, adj R^2=%.4f, RMSE=%.5f\n",
            nrow(fit_m$est_data),
            format(min(fit_m$est_data$date)),
            format(max(fit_m$est_data$date)),
            summary(fit_m$fit)$r.squared,
            summary(fit_m$fit)$adj.r.squared,
            sqrt(mean(residuals(fit_m$fit)^2))))

cf <- summary(fit_m$fit)$coefficients
lambda_M <- cf["ecm_lag_M", "Estimate"]
cat(sprintf("\n--- Long-run structural coefs (LR coef = -coef/lambda_M) ---\n"))
cat(sprintf("  lambda_M = %.4f  (sign prior: < 0)\n", lambda_M))

lr_terms <- setdiff(m_lr_vars, "ecm_lag_M")
sign_priors <- c(lincome = ">0", ln_hp_over_yd = ">0", real_rate = "<0",
                 prime_age_share = ">0", cci_williams_lvl = ">0")

lr_struct <- tibble(
  term = lr_terms,
  estimate    = cf[lr_terms, "Estimate"],
  std_error   = cf[lr_terms, "Std. Error"],
  t_stat      = cf[lr_terms, "t value"],
  p_value     = cf[lr_terms, "Pr(>|t|)"],
  lr_struct   = -cf[lr_terms, "Estimate"] / lambda_M,
  sign_prior  = sign_priors[lr_terms],
  prior_match = (sign(-cf[lr_terms, "Estimate"] / lambda_M) ==
                  ifelse(sign_priors[lr_terms] == ">0", 1, -1))
)
print(lr_struct, n = Inf, digits = 4)

cat("\n--- Short-run + dummies ---\n")
sr_terms <- intersect(c(m_sr_vars, m_dummies, "(Intercept)"), rownames(cf))
print(tibble(
  term = sr_terms,
  estimate = cf[sr_terms, "Estimate"],
  std_error = cf[sr_terms, "Std. Error"],
  t_stat   = cf[sr_terms, "t value"],
  p_value  = cf[sr_terms, "Pr(>|t|)"]
), n = Inf, digits = 4)

n_match <- sum(lr_struct$prior_match, na.rm = TRUE)
cat(sprintf("\nSign-prior match: %d of %d\n", n_match, length(lr_terms)))
violators <- lr_struct$term[!lr_struct$prior_match]
if (length(violators) > 0L) {
  cat("Violators (will need attention):", paste(violators, collapse = ", "), "\n")
}

write_csv(
  bind_rows(
    lr_struct %>% mutate(block = "long_run", .before = 1),
    tibble(block = "lambda", term = "ecm_lag_M",
           estimate = lambda_M, std_error = cf["ecm_lag_M", "Std. Error"],
           t_stat = cf["ecm_lag_M", "t value"], p_value = cf["ecm_lag_M", "Pr(>|t|)"],
           lr_struct = NA_real_, sign_prior = "<0", prior_match = lambda_M < 0)
  ),
  file.path(PROJ_LIVES, "outputs", "mortgage_stock_equation_standalone.csv")
)
cat(sprintf("\nSaved: LIVES/outputs/mortgage_stock_equation_standalone.csv\n"))
