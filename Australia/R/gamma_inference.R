# ==============================================================================
# NS-126 / NS-134 — inference on the implied structural coefficients.
#
# The headline reports structural gamma_i = beta_i / |lambda| as point ratios
# with NO standard error, and compares them to Williams' Table 1. This script
# attaches proper uncertainty:
#
#  (1) Delta-method SE and 95% CI on each implied gamma_i = beta_i/|lambda|,
#      using the Newey-West vcov of (beta, lambda) (so the correlation between
#      the numerator and the speed of adjustment is carried).
#  (2) An aggregate wealth coefficient gamma_W = sum(wealth betas)/|lambda|
#      with a delta-method CI (NS-134) — the single number MARTIN's net-wealth
#      elasticity maps onto.
#  (3) A moving-block residual bootstrap (block length 8, B = 1000) as a
#      distribution-free robustness on lambda and the gamma profile.
#  (4) A flag for whether Williams' Table 1 value lies inside each 95% CI.
#
# CAVEAT (generated regressors): both the delta method and the residual
# bootstrap hold the right-hand side fixed, so they propagate sampling
# uncertainty in the ECM coefficients but NOT first-stage uncertainty in the
# generated permanent-income and CCI regressors. The real-time PI sensitivity
# (australia_pi_realtime_robustness.csv) gives a partial read on the former.
#
# Run:  Rscript Australia/R/gamma_inference.R
# Out:  Australia/outputs/australia_gamma_inference.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(sandwich); library(lmtest)
})
options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS <- "Australia"

source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src      <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
he       <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")), envir = he)

master <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")) %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)
master <- he$add_model_variables(master)
master <- he$compute_income_volatility(master)
master <- he$construct_permanent_income_italy(master)   # canonical full-sample measure
master <- master %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
lr_vars <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
             "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag")
sr_vars <- c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")

sp <- he$fit_ecm_spec(data = master, spec_name = "Spec6_GammaInference",
                      lr_vars = lr_vars, sr_vars = sr_vars,
                      dummy_vars = base_dummies, sample_end = as.Date("2024-10-01"))
fit <- sp$fit
V   <- sp$nw_vcov
b   <- coef(fit)
lam <- b[["ecm_lag"]]
cat(sprintf("Spec 6 lambda = %.4f (n=%d)\n", lam, nobs(fit)))

# Williams Table 1 reference (structural gamma)
WILL <- c(ha_y = 0.0488, nla_y = 0.1590, eq_y = 0.0110, super_y = 0.0110,
          ln_hp_over_y = -0.1300, ln_yp_over_y = 0.2000)

# ---- (1) Delta-method gamma_i = -beta_i / lambda  (lambda < 0 => /|lambda|) ----
gamma_terms <- c("ha_y", "nla_y", "eq_y", "super_y", "ln_hp_over_y", "ln_yp_over_y")
delta_gamma <- function(term) {
  bi <- b[[term]]
  idx <- c(term, "ecm_lag")
  Vsub <- V[idx, idx]
  # gamma = -bi/lam ; grad = (d/dbi, d/dlam) = (-1/lam, bi/lam^2)
  g <- c(-1 / lam, bi / lam^2)
  se <- sqrt(as.numeric(t(g) %*% Vsub %*% g))
  gam <- -bi / lam
  tibble(term = term, ols = bi, gamma = gam, gamma_se = se,
         ci_lo = gam - 1.96 * se, ci_hi = gam + 1.96 * se,
         williams = unname(WILL[term]))
}
gtab <- bind_rows(lapply(gamma_terms, delta_gamma))

# ---- (2) Aggregate wealth coefficient gamma_W = sum(wealth betas)/|lambda| ----
wealth <- c("ha_y", "nla_y", "eq_y", "super_y")
S   <- sum(b[wealth])
idx <- c(wealth, "ecm_lag")
Vsub <- V[idx, idx]
grad <- c(rep(-1 / lam, length(wealth)), S / lam^2)
seW  <- sqrt(as.numeric(t(grad) %*% Vsub %*% grad))
gamW <- -S / lam
agg <- tibble(term = "WEALTH_AGGREGATE", ols = S, gamma = gamW, gamma_se = seW,
              ci_lo = gamW - 1.96 * seW, ci_hi = gamW + 1.96 * seW,
              williams = sum(WILL[wealth]))

# ---- (3) Moving-block residual bootstrap (L=8, B=1000) ------------------------
X  <- model.matrix(fit)
yh <- as.numeric(fitted(fit))
e  <- as.numeric(resid(fit))
n  <- length(e); L <- 8L; B <- 1000L
nb <- ceiling(n / L)
boot_lam <- numeric(B)
boot_g   <- matrix(NA_real_, B, length(gamma_terms),
                   dimnames = list(NULL, gamma_terms))
# deterministic block starts per replication (no RNG: vary by index) so the
# result is reproducible without Math.random-style state.
starts_grid <- ((seq_len(nb * B) * 37L) %% (n - L + 1L)) + 1L
for (bnum in seq_len(B)) {
  st <- starts_grid[((bnum - 1L) * nb + 1L):(bnum * nb)]
  idxb <- as.integer(unlist(lapply(st, function(s) s:(s + L - 1L))))[seq_len(n)]
  ystar <- yh + e[idxb]
  cf <- tryCatch(qr.coef(qr(X), ystar), error = function(z) NULL)
  if (is.null(cf) || is.na(cf[["ecm_lag"]])) next
  lb <- cf[["ecm_lag"]]
  boot_lam[bnum] <- lb
  for (tm in gamma_terms) boot_g[bnum, tm] <- -cf[[tm]] / lb
}
boot_lam <- boot_lam[boot_lam != 0]
q <- function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
boot_rows <- bind_rows(lapply(gamma_terms, function(tm) {
  qq <- q(boot_g[, tm])
  tibble(term = tm, boot_gamma_med = qq[2], boot_ci_lo = qq[1], boot_ci_hi = qq[3])
}))
lam_q <- q(boot_lam)

# ---- Assemble + write --------------------------------------------------------
out <- gtab %>% left_join(boot_rows, by = "term") %>% bind_rows(agg)
out$williams_in_ci <- out$williams >= out$ci_lo & out$williams <= out$ci_hi
out <- out %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
write.csv(out, file.path(PROJ_AUS, "outputs", "australia_gamma_inference.csv"),
          row.names = FALSE)

cat("\n=== Implied structural gamma with delta-method 95% CIs ===\n")
print(as.data.frame(out), row.names = FALSE)
cat(sprintf("\nlambda bootstrap median [95%% CI]: %.3f [%.3f, %.3f]\n",
            lam_q[2], lam_q[1], lam_q[3]))
cat(sprintf("Williams Table 1 value inside our 95%% CI: %s\n",
            paste(sprintf("%s=%s", out$term, ifelse(out$williams_in_ci, "yes", "NO")),
                  collapse = ", ")))
cat("Saved: Australia/outputs/australia_gamma_inference.csv\n")
