# ==============================================================================
# refit_spec46_extended.R
#
# Refit Spec 4 (disaggregated, no CCI, no SR) and Spec 6 (preferred — disagg
# + post-2008 PI shift + SR dynamics) on the back-extended 1976Q3+ sample
# using the disaggregated wealth proxies (ha_y_proxy, nla_y_proxy,
# eq_y_proxy, super_y_proxy) and compare to the 1988Q3+ baseline.
#
# Output:
#   outputs/spec46_extended_comparison.csv  — coefficient comparison table
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ <- "Australia"
master <- readRDS(file.path(PROJ, "outputs", "australia_model_dataset.rds"))
source(file.path(PROJ, "R", "model_helpers.R"), local = TRUE)

# Build model_data and source helpers (no main exec)
model_data <- master %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

src <- readLines(file.path(PROJ, "R", "australia_estimation.R"))
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

model_data <- add_model_variables(model_data)
model_data <- compute_income_volatility(model_data)
model_data <- construct_permanent_income_italy(model_data)
model_data <- model_data %>%
  mutate(ecm_lag      = lag(lcons, 1L) - lincome)

cat(sprintf("\nmodel_data: %d rows\n", nrow(model_data)))
for (v in c("nla_y", "eq_y", "super_y", "ha_y",
            "nla_y_proxy", "eq_y_proxy", "super_y_proxy", "ha_y_proxy")) {
  cat(sprintf("  %-16s nn=%d, first=%s\n", v, sum(!is.na(model_data[[v]])),
    format(min(model_data$date[!is.na(model_data[[v]])]))))
}

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

# ---- Spec 4 (disaggregated, no CCI, no SR) ----
spec4_lr <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
              "real_rate", "ln_yp_over_y", "ecm_lag")

cat("\n", strrep("=", 60), "\n", sep = "")
cat("SPEC 4 (disaggregated, no CCI)\n")
cat(strrep("=", 60), "\n", sep = "")

fit_spec4_base <- fit_ecm_spec(
  data         = model_data,
  spec_name    = "Spec4_baseline",
  lr_vars      = spec4_lr,
  sr_vars      = character(0),
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(model_data$date)
)

ext4_dat <- model_data %>%
  mutate(nla_y    = nla_y_proxy,
         eq_y     = eq_y_proxy,
         super_y  = super_y_proxy,
         ha_y     = ha_y_proxy)

fit_spec4_ext <- fit_ecm_spec(
  data         = ext4_dat,
  spec_name    = "Spec4_extended",
  lr_vars      = spec4_lr,
  sr_vars      = character(0),
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(ext4_dat$date)
)

# ---- Spec 6 (preferred — disagg + PI post-2008 shift + SR) ----
# SR vars and ln_yp_over_y_post2008 may bound the sample.
spec6_lr <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
              "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag")
spec6_sr <- c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")

cat("\n", strrep("=", 60), "\n", sep = "")
cat("SPEC 6 (preferred — disagg + post-2008 PI shift + SR)\n")
cat(strrep("=", 60), "\n", sep = "")

fit_spec6_base <- fit_ecm_spec(
  data         = model_data,
  spec_name    = "Spec6_baseline",
  lr_vars      = spec6_lr,
  sr_vars      = spec6_sr,
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(model_data$date)
)

fit_spec6_ext <- fit_ecm_spec(
  data         = ext4_dat,    # same alias of disagg proxies
  spec_name    = "Spec6_extended",
  lr_vars      = spec6_lr,
  sr_vars      = spec6_sr,
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(ext4_dat$date)
)

# ---- Long-run structural coefficients ----
lr_struct <- function(fit, lr_terms) {
  cf <- summary(fit$fit)$coefficients
  if (!"ecm_lag" %in% rownames(cf)) {
    return(setNames(rep(NA_real_, length(lr_terms) + 1L),
                    c("lambda", lr_terms)))
  }
  lambda <- cf["ecm_lag", "Estimate"]
  out <- c(lambda = lambda)
  for (nm in lr_terms) {
    out[nm] <- if (nm %in% rownames(cf)) -cf[nm, "Estimate"] / lambda
               else NA_real_
  }
  out
}

spec4_terms <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y")
spec6_terms <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008")

lr4_b <- lr_struct(fit_spec4_base, spec4_terms)
lr4_e <- lr_struct(fit_spec4_ext,  spec4_terms)
lr6_b <- lr_struct(fit_spec6_base, spec6_terms)
lr6_e <- lr_struct(fit_spec6_ext,  spec6_terms)

print_compare <- function(label, b, e, terms, fit_b, fit_e) {
  cat(sprintf("\n--- %s long-run structural coefs ---\n", label))
  tbl <- tibble(
    term = c("lambda", terms),
    baseline = c(b["lambda"], b[terms]),
    extended = c(e["lambda"], e[terms])
  ) %>%
    mutate(pct_change = ifelse(abs(baseline) > 1e-9,
                               100 * (extended - baseline) / baseline,
                               NA_real_))
  print(tbl, n = Inf, digits = 4)

  cat(sprintf("  baseline n=%d  R^2=%.4f  adj R^2=%.4f  RMSE=%.5f\n",
              nrow(fit_b$est_data),
              summary(fit_b$fit)$r.squared,
              summary(fit_b$fit)$adj.r.squared,
              sqrt(mean(residuals(fit_b$fit)^2))))
  cat(sprintf("  extended n=%d  R^2=%.4f  adj R^2=%.4f  RMSE=%.5f\n",
              nrow(fit_e$est_data),
              summary(fit_e$fit)$r.squared,
              summary(fit_e$fit)$adj.r.squared,
              sqrt(mean(residuals(fit_e$fit)^2))))
  invisible(tbl)
}

tbl4 <- print_compare("SPEC 4", lr4_b, lr4_e, spec4_terms,
                       fit_spec4_base, fit_spec4_ext)
tbl6 <- print_compare("SPEC 6", lr6_b, lr6_e, spec6_terms,
                       fit_spec6_base, fit_spec6_ext)

combined <- bind_rows(
  tbl4 %>% mutate(spec = "Spec4_NoCCI", .before = 1),
  tbl6 %>% mutate(spec = "Spec6_Preferred", .before = 1)
)
out_path <- file.path(PROJ, "outputs", "spec46_extended_comparison.csv")
write_csv(combined, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
