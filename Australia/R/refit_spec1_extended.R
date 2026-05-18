# ==============================================================================
# refit_spec1_extended.R
#
# Refit Spec 1 (aggregate log-networth ECM) on the back-extended 1976Q3+
# sample using `ln_networth_y_proxy` (M3-allocated + housing-back-cast,
# growth-rate-spliced onto the official networth_y at 1988Q3) and compare
# coefficients to the 1988Q3+ baseline that uses the official series.
# Permanent income is constructed via construct_permanent_income_italy().
#
# Output:
#   outputs/spec1_extended_comparison.csv  — coefficient comparison table
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ <- "Australia"

master <- readRDS(file.path(PROJ, "outputs", "australia_model_dataset.rds"))
source(file.path(PROJ, "R", "model_helpers.R"), local = TRUE)

# Build model_data the same way the orchestrator does
model_data <- master %>%
  rename(dlcons  = d_ln_cons_pc,
         lincome = ln_ydi_real_pc,
         lcons   = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

# Source the estimation script body in a sandboxed env to grab the helper
# functions without triggering the heavy main-execution block that follows
# the function definitions.
estim_src <- readLines(file.path(PROJ, "R", "australia_estimation.R"))
main_exec_at <- grep("^# MAIN EXECUTION", estim_src)[1]
stopifnot(!is.na(main_exec_at))
helpers_only <- estim_src[seq_len(main_exec_at - 1L)]
helpers_env <- new.env(parent = globalenv())
helpers_env$model_data <- model_data    # satisfy the script's guard check
eval(parse(text = paste(helpers_only, collapse = "\n")), envir = helpers_env)

construct_permanent_income_italy <- helpers_env$construct_permanent_income_italy
add_model_variables              <- helpers_env$add_model_variables
compute_income_volatility        <- helpers_env$compute_income_volatility
fit_ecm_spec                     <- helpers_env$fit_ecm_spec

model_data <- add_model_variables(model_data)
model_data <- compute_income_volatility(model_data)
model_data <- construct_permanent_income_italy(model_data)
model_data <- model_data %>%
  mutate(ecm_lag      = lag(lcons, 1L) - lincome,
         ln_hp_over_y = log(hpi / exp(lincome) * (cons_deflator_norm / 100)))

cat(sprintf("\nmodel_data: %d rows, %d cols\n", nrow(model_data), ncol(model_data)))
cat(sprintf("  ln_networth_y nn=%d, first=%s\n",
  sum(!is.na(model_data$ln_networth_y)),
  format(min(model_data$date[!is.na(model_data$ln_networth_y)]))))
cat(sprintf("  ln_networth_y_proxy nn=%d, first=%s\n",
  sum(!is.na(model_data$ln_networth_y_proxy)),
  format(min(model_data$date[!is.na(model_data$ln_networth_y_proxy)]))))
cat(sprintf("  ln_yp_over_y nn=%d, first=%s\n",
  sum(!is.na(model_data$ln_yp_over_y)),
  format(min(model_data$date[!is.na(model_data$ln_yp_over_y)]))))

cat("\n=========================================================\n")
cat("  Spec 1 refit comparison: 1988Q3+ vs 1976Q3+\n")
cat("=========================================================\n")

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
lr_vars_v <- c("ln_networth_y", "ln_hp_over_y", "real_rate",
               "ln_yp_over_y", "ecm_lag")

fit_baseline <- fit_ecm_spec(
  data         = model_data,
  spec_name    = "Spec1_baseline",
  lr_vars      = lr_vars_v,
  sr_vars      = character(0),
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(model_data$date)
)

extended_dat <- model_data
extended_dat$ln_networth_y_official <- extended_dat$ln_networth_y
extended_dat$ln_networth_y          <- extended_dat$ln_networth_y_proxy

fit_extended <- fit_ecm_spec(
  data         = extended_dat,
  spec_name    = "Spec1_extended",
  lr_vars      = lr_vars_v,
  sr_vars      = character(0),
  dummy_vars   = base_dummies,
  sample_start = as.Date("1976-07-01"),
  sample_end   = max(extended_dat$date)
)

e_base <- fit_baseline$est_data
e_ext  <- fit_extended$est_data
cat(sprintf("\nBaseline:  n=%d, %s to %s\n",
  nrow(e_base), format(min(e_base$date)), format(max(e_base$date))))
cat(sprintf("Extended:  n=%d, %s to %s\n",
  nrow(e_ext), format(min(e_ext$date)), format(max(e_ext$date))))

lr_struct <- function(fit) {
  cf <- summary(fit$fit)$coefficients
  if (!"ecm_lag" %in% rownames(cf))
    return(setNames(rep(NA_real_, 5L),
                    c("lambda", "ln_networth_y", "ln_hp_over_y",
                      "real_rate", "ln_yp_over_y")))
  lambda <- cf["ecm_lag", "Estimate"]
  out <- c(lambda = lambda)
  for (nm in c("ln_networth_y", "ln_hp_over_y", "real_rate", "ln_yp_over_y")) {
    out[nm] <- if (nm %in% rownames(cf)) -cf[nm, "Estimate"] / lambda
               else NA_real_
  }
  out
}

lr_base <- lr_struct(fit_baseline)
lr_ext  <- lr_struct(fit_extended)

comparison <- tibble(
  term = c("lambda (ecm_lag)", "ln_networth_y (LR)", "ln_hp_over_y (LR)",
           "real_rate (LR)", "ln_yp_over_y (LR)"),
  baseline_1988 = c(lr_base["lambda"], lr_base["ln_networth_y"],
                    lr_base["ln_hp_over_y"], lr_base["real_rate"],
                    lr_base["ln_yp_over_y"]),
  extended_1976 = c(lr_ext["lambda"],  lr_ext["ln_networth_y"],
                    lr_ext["ln_hp_over_y"],  lr_ext["real_rate"],
                    lr_ext["ln_yp_over_y"])
) %>%
  mutate(pct_change = ifelse(abs(baseline_1988) > 1e-9,
                             100 * (extended_1976 - baseline_1988) / baseline_1988,
                             NA_real_))

cat("\n--- Long-run structural coefficients ---\n")
print(comparison, n = Inf, digits = 4)

cat("\n--- Goodness of fit ---\n")
cat(sprintf("  baseline  n=%d, R^2 = %.4f, adj R^2 = %.4f, RMSE = %.5f\n",
            nrow(e_base),
            summary(fit_baseline$fit)$r.squared,
            summary(fit_baseline$fit)$adj.r.squared,
            sqrt(mean(residuals(fit_baseline$fit)^2))))
cat(sprintf("  extended  n=%d, R^2 = %.4f, adj R^2 = %.4f, RMSE = %.5f\n",
            nrow(e_ext),
            summary(fit_extended$fit)$r.squared,
            summary(fit_extended$fit)$adj.r.squared,
            sqrt(mean(residuals(fit_extended$fit)^2))))

out_path <- file.path(PROJ, "outputs", "spec1_extended_comparison.csv")
write_csv(comparison, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
