# ==============================================================================
# NS-120 diagnostic — real-time vs full-sample permanent income.
#
# The canonical headline uses construct_permanent_income_italy() in its
# FULL-SAMPLE form: one OLS fit over the whole sample, in-sample fitted
# values used as y^p for every quarter. That is non-causal — y^p_t embeds
# coefficients estimated from data after t — and so is not operational at
# MARTIN forecast time. This script refits the Spec 6 template under three
# permanent-income measures and prints λ and the PI coefficients side by
# side so the look-ahead's contribution to the headline can be quantified:
#
#   1. AR_realtime      — rolling AR(8) forecaster (construct_permanent_income),
#                         already expanding-window / leak-free.
#   2. Italy_fullsample — published Italy LP (real_time = FALSE). Non-causal.
#   3. Italy_realtime   — expanding-window Italy LP (real_time = TRUE). Causal.
#
# Run:  Rscript Australia/R/pi_realtime_diagnostic.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(sandwich); library(lmtest)
})
options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS <- "Australia"

# --- Source helpers exactly as williams_calibration_test.R does -------------
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src      <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
he       <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = he)

# --- Build the Spec 6 base data ---------------------------------------------
master0 <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")) %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)
master0 <- he$add_model_variables(master0)
master0 <- he$compute_income_volatility(master0)

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
lr_vars <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
             "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag")
sr_vars <- c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")

fit_under <- function(label, pidat) {
  pidat <- pidat %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)
  sp <- tryCatch(
    he$fit_ecm_spec(data = pidat, spec_name = paste0("Spec6_", label),
                    lr_vars = lr_vars, sr_vars = sr_vars,
                    dummy_vars = base_dummies,
                    sample_end = as.Date("2024-10-01")),
    error = function(e) { message("fit failed for ", label, ": ", e$message); NULL })
  if (is.null(sp)) return(NULL)
  cf <- coef(sp$fit); se <- sqrt(diag(sp$nw_vcov)); se <- se[match(names(cf), names(se))]
  g  <- function(t) { i <- match(t, names(cf)); if (is.na(i)) c(NA, NA) else c(cf[i], cf[i] / se[i]) }
  tibble(
    method     = label,
    n          = nobs(sp$fit),
    lambda     = g("ecm_lag")[1],            lambda_t  = g("ecm_lag")[2],
    yp         = g("ln_yp_over_y")[1],       yp_t      = g("ln_yp_over_y")[2],
    yp_post08  = g("ln_yp_over_y_post2008")[1], yp_post08_t = g("ln_yp_over_y_post2008")[2],
    ha_y       = g("ha_y")[1],               ha_y_t    = g("ha_y")[2]
  )
}

ar  <- he$construct_permanent_income(master0)
itf <- he$construct_permanent_income_italy(master0, real_time = FALSE)
itr <- he$construct_permanent_income_italy(master0, real_time = TRUE)

res <- bind_rows(
  fit_under("AR_realtime",      ar),
  fit_under("Italy_fullsample", itf),
  fit_under("Italy_realtime",   itr)
)

cat("\n=== NS-120: Spec 6 under three permanent-income measures ===\n")
print(as.data.frame(res), digits = 3, row.names = FALSE)

out_path <- file.path(PROJ_AUS, "outputs", "australia_pi_realtime_robustness.csv")
write.csv(res, out_path, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", out_path))

# First/last quarter for which each PI measure is available (warm-up cost)
cov <- function(d, label) {
  ok <- !is.na(d$ln_yp_over_y)
  cat(sprintf("%-18s y^p available: %s .. %s (%d quarters)\n",
              label, format(min(d$date[ok])), format(max(d$date[ok])), sum(ok)))
}
cat("\n=== Permanent-income coverage (warm-up cost of the real-time fit) ===\n")
cov(ar,  "AR_realtime"); cov(itf, "Italy_fullsample"); cov(itr, "Italy_realtime")
