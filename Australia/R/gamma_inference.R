# ==============================================================================
# NS-126 / NS-134 — inference on the implied structural coefficients.
#
# The headline reports structural gamma_i = beta_i / |lambda| as point ratios
# with NO standard error, and compares them to Williams' Table 1. This script
# attaches proper uncertainty, for BOTH the conventional baseline (Spec 6) and
# the LIVES headline (Spec 11) — a previous version covered Spec 6 only, while
# the paper deployed its conclusion against the Spec 11 headline.
#
#  (1) Delta-method SE and 95% CI on each implied gamma_i = beta_i/|lambda|,
#      using the Newey-West vcov of (beta, lambda) (so the correlation between
#      the numerator and the speed of adjustment is carried).
#  (2) An aggregate wealth coefficient gamma_W = sum(wealth betas)/|lambda|
#      with a delta-method CI (NS-134) — the single number MARTIN's net-wealth
#      elasticity maps onto.
#  (3) A moving-block residual bootstrap (block length 8, B = 1000) as a
#      distribution-free robustness on lambda and the gamma profile. Block
#      starts are RANDOM with a fixed seed — a previous version generated them
#      from a deterministic arithmetic cycle, which yielded only 79 unique
#      "replications" and was not a bootstrap.
#  (4) A flag for whether Williams' Table 1 value lies inside each 95% CI.
#      For Spec 11 the housing reference is Williams' PEAK housing MPC
#      (0.0488): the deployed CCI is peak-normalised to 1, so the structural
#      coefficient on CCI*(HA/4y) is the MPC at the credit-loose peak. The
#      rate loading alpha_r and the autonomous-consumption loading zeta_c are
#      NOT scale-comparable across the two CCI normalisations and carry NA.
#
# CAVEAT (generated regressors): both the delta method and the residual
# bootstrap hold the right-hand side fixed, so they propagate sampling
# uncertainty in the ECM coefficients but NOT first-stage uncertainty in the
# generated permanent-income and CCI regressors.
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

# Attach the deployed CCI series (and rebuild the de-meaned interactions /
# combined illiquid ratio) from the committed pipeline artefact so the Spec 11
# block runs without refitting the Stage-1 CCI.
cci_path <- file.path(PROJ_AUS, "outputs", "australia_cci_williams_series.csv")
have_cci <- file.exists(cci_path)
if (have_cci) {
  cci_series <- read.csv(cci_path) %>%
    mutate(date = as.Date(date)) %>%
    select(date, cci_williams)
  master <- master %>%
    left_join(cci_series, by = "date") %>%
    mutate(ilfa_y = eq_y + super_y)
  mask <- !is.na(master$cci_williams) &
          master$date >= as.Date("1980-01-01") &
          master$date <= as.Date("2026-01-01")
  ha_m <- mean(master$ha_y[mask],         na.rm = TRUE)
  hp_m <- mean(master$ln_hp_over_y[mask], na.rm = TRUE)
  r_m  <- mean(master$real_rate[mask],    na.rm = TRUE)
  yp_m <- mean(master$ln_yp_over_y[mask], na.rm = TRUE)
  master <- master %>%
    mutate(
      r_x_cci          = (real_rate    - r_m)  * cci_williams,
      hp_x_1_minus_cci = (ln_hp_over_y - hp_m) * (1 - 1.2 * cci_williams),
      yp_x_cci         = (ln_yp_over_y - yp_m) * cci_williams,
      ha_x_cci         = (ha_y         - ha_m) * cci_williams
    )
} else {
  message("[gamma_inference] ", cci_path, " not found — Spec 11 block skipped. ",
          "Run the pipeline (Step 4a-iii) first.")
}

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

spec_defs <- list(
  Spec6 = list(
    label   = "Spec6_GammaInference",
    lr_vars = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag"),
    sr_vars = c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid"),
    gamma_terms = c("ha_y", "nla_y", "eq_y", "super_y", "ln_hp_over_y",
                    "ln_yp_over_y"),
    wealth_terms = c("ha_y", "nla_y", "eq_y", "super_y"),
    williams = c(ha_y = 0.0488, nla_y = 0.1590, eq_y = 0.0110, super_y = 0.0110,
                 ln_hp_over_y = -0.1300, ln_yp_over_y = 0.2000)
  ),
  Spec11 = list(
    label   = "Spec11_GammaInference",
    lr_vars = c("nla_y", "ilfa_y", "ha_x_cci", "hp_x_1_minus_cci", "r_x_cci",
                "cci_williams", "ln_yp_over_y", "yp_x_cci", "ecm_lag"),
    sr_vars = c("dd4_income", "d2_log_unemp", "abs_income_resid"),
    gamma_terms = c("ha_x_cci", "nla_y", "ilfa_y", "hp_x_1_minus_cci",
                    "ln_yp_over_y", "yp_x_cci"),
    wealth_terms = c("ha_x_cci", "nla_y", "ilfa_y"),
    # ha_x_cci vs Williams' PEAK housing MPC (CCI peak-normalised to 1 here);
    # ln_yp_over_y vs psi_0 = 0.20; yp_x_cci vs psi_1 = 0.93;
    # hp_x_1_minus_cci vs alpha_4 = -0.130. r_x_cci / zeta_c omitted: not
    # scale-comparable across CCI normalisations.
    williams = c(ha_x_cci = 0.0488, nla_y = 0.1590, ilfa_y = 0.0220,
                 hp_x_1_minus_cci = -0.1300, ln_yp_over_y = 0.2000,
                 yp_x_cci = 0.9300)
  )
)
if (!have_cci) spec_defs$Spec11 <- NULL

set.seed(20260611L)
all_out <- list()

for (sd_name in names(spec_defs)) {
  sd <- spec_defs[[sd_name]]
  sp <- tryCatch(
    he$fit_ecm_spec(data = master, spec_name = sd$label,
                    lr_vars = sd$lr_vars, sr_vars = sd$sr_vars,
                    dummy_vars = base_dummies,
                    sample_end = as.Date("2026-01-01")),
    error = function(e) { message("[", sd_name, "] fit failed: ",
                                  conditionMessage(e)); NULL })
  if (is.null(sp)) next
  fit <- sp$fit
  V   <- sp$nw_vcov
  b   <- coef(fit)
  lam <- b[["ecm_lag"]]
  cat(sprintf("%s lambda = %.4f (n=%d)\n", sd_name, lam, nobs(fit)))

  gamma_terms <- intersect(sd$gamma_terms, names(b))
  WILL <- sd$williams

  # ---- (1) Delta-method gamma_i = -beta_i / lambda ----
  delta_gamma <- function(term) {
    bi <- b[[term]]
    idx <- c(term, "ecm_lag")
    Vsub <- V[idx, idx]
    g <- c(-1 / lam, bi / lam^2)
    se <- sqrt(as.numeric(t(g) %*% Vsub %*% g))
    gam <- -bi / lam
    tibble(term = term, ols = bi, gamma = gam, gamma_se = se,
           ci_lo = gam - 1.96 * se, ci_hi = gam + 1.96 * se,
           williams = unname(WILL[term]))
  }
  gtab <- bind_rows(lapply(gamma_terms, delta_gamma))

  # ---- (2) Aggregate wealth coefficient ----
  wealth <- intersect(sd$wealth_terms, names(b))
  S   <- sum(b[wealth])
  idx <- c(wealth, "ecm_lag")
  Vsub <- V[idx, idx]
  grad <- c(rep(-1 / lam, length(wealth)), S / lam^2)
  seW  <- sqrt(as.numeric(t(grad) %*% Vsub %*% grad))
  gamW <- -S / lam
  will_w <- sum(WILL[intersect(wealth, names(WILL))], na.rm = TRUE)
  agg <- tibble(term = "WEALTH_AGGREGATE", ols = S, gamma = gamW, gamma_se = seW,
                ci_lo = gamW - 1.96 * seW, ci_hi = gamW + 1.96 * seW,
                williams = will_w)

  # ---- (3) Moving-block residual bootstrap (L=8, B=1000, seeded RNG) ----
  X  <- model.matrix(fit)
  yh <- as.numeric(fitted(fit))
  e  <- as.numeric(resid(fit))
  n  <- length(e); L <- 8L; B <- 1000L
  nb <- ceiling(n / L)
  boot_lam <- numeric(B)
  boot_g   <- matrix(NA_real_, B, length(gamma_terms),
                     dimnames = list(NULL, gamma_terms))
  for (bnum in seq_len(B)) {
    st <- sample.int(n - L + 1L, nb, replace = TRUE)
    idxb <- as.integer(unlist(lapply(st, function(s) s:(s + L - 1L))))[seq_len(n)]
    ystar <- yh + e[idxb]
    cf <- tryCatch(qr.coef(qr(X), ystar), error = function(z) NULL)
    if (is.null(cf) || is.na(cf[["ecm_lag"]])) next
    lb <- cf[["ecm_lag"]]
    boot_lam[bnum] <- lb
    for (tm in gamma_terms) boot_g[bnum, tm] <- -cf[[tm]] / lb
  }
  boot_lam_f <- boot_lam[boot_lam != 0]
  q <- function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)
  boot_rows <- bind_rows(lapply(gamma_terms, function(tm) {
    qq <- q(boot_g[, tm])
    tibble(term = tm, boot_gamma_med = qq[2], boot_ci_lo = qq[1],
           boot_ci_hi = qq[3])
  }))
  lam_q <- q(boot_lam_f)

  out <- gtab %>% left_join(boot_rows, by = "term") %>% bind_rows(agg)
  out$williams_in_ci <- !is.na(out$williams) &
    out$williams >= out$ci_lo & out$williams <= out$ci_hi
  out <- out %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    mutate(spec = sd_name, lambda = round(lam, 4), n_obs = nobs(fit)) %>%
    select(spec, dplyr::everything())
  all_out[[sd_name]] <- out

  cat(sprintf("\n=== %s: implied structural gamma with delta-method 95%% CIs ===\n",
              sd_name))
  print(as.data.frame(out), row.names = FALSE)
  cat(sprintf("\n%s lambda bootstrap median [95%% CI]: %.3f [%.3f, %.3f]\n",
              sd_name, lam_q[2], lam_q[1], lam_q[3]))
}

final <- bind_rows(all_out)
write.csv(final, file.path(PROJ_AUS, "outputs", "australia_gamma_inference.csv"),
          row.names = FALSE)
cat("Saved: Australia/outputs/australia_gamma_inference.csv\n")
