# ==============================================================================
# Item 2: sectional sign-prior CCI vs maximal-GETS canonical
#
# Williams (Aust paper §5.1) imposes sign priors over PERIODS:
#   1982-1990  : non-negative (financial deregulation)
#   1990-1993  : non-positive (banking sector distress)
#   1993-2007  : non-negative (new entrants, securitisation)
#   2007+      : non-positive (GFC tightening; extended to APRA 2014/17, COVID, etc.)
#
# Our maximal-GETS canonical uses 15 candidate knots with knot-by-knot point
# sign priors. This script:
#   1. Builds cci_williams_sectional via the new
#      build_williams_cci_basis_sectional() helper (8 knots, one per period)
#   2. Fits Spec 4-style consumption equation with each CCI variant
#   3. Reports surviving knots, lambda, R^2, and fitted CCI level
#   4. Re-runs the random-knot placebo using the sectional protocol to see
#      whether the sectional CCI moves into the upper tail of random draws
#
# Output:
#   LIVES/outputs/sectional_cci_comparison.csv
#   LIVES/outputs/sectional_placebo_summary.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260508L)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))
cat(sprintf("Loaded LIVES model data: %d rows\n", nrow(dat)))

source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)

fit_ecm_spec <- helpers_env$fit_ecm_spec
fit_consumption_with_williams_cci <- helpers_env$fit_consumption_with_williams_cci

# ----------------------------------------------------------------------------
# Drop existing sdmma_* columns and attach the sectional basis
# ----------------------------------------------------------------------------
build_and_fit <- function(model_data, basis_fn, label) {
  md <- model_data
  if (any(grepl("^sdmma_", names(md)))) md <- md[, !grepl("^sdmma_", names(md))]
  basis <- basis_fn(md$date)
  for (j in seq_len(ncol(basis))) md[[colnames(basis)[j]]] <- basis[, j]

  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("  candidate knots (%d): %s\n", ncol(basis),
              paste(attr(basis, "knots"), collapse = ", ")))
  cat(sprintf("  sign priors:          %s\n",
              paste(attr(basis, "sign_priors"), collapse = ", ")))

  base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                    "d_neg_gearing_8587", "d_recession_1991",
                    "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
  spec4_lr <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                "real_rate", "ln_yp_over_y", "ecm_lag")

  # Pass basis_fn through: fit_consumption_with_williams_cci rebuilds the
  # basis internally, and a previous version let it default to the MAXIMAL
  # basis — so the "sectional" comparison fit was actually the maximal fit
  # (byte-identical coefficient blocks in sectional_cci_comparison.csv).
  fit <- fit_consumption_with_williams_cci(
    md, lr_vars = spec4_lr, sr_vars = character(0),
    dummy_vars = base_dummies,
    sample_end = max(md$date),
    basis_fn   = basis_fn
  )
  cat(sprintf("  surviving knots: %s\n",
              paste(fit$surviving_knots, collapse = ", ")))
  cat(sprintf("  dropped knots:   %s\n",
              if (length(fit$dropped_knots) > 0) paste(fit$dropped_knots, collapse = ", ") else "(none)"))
  cf <- coef(fit$spec$fit)
  list(
    fit       = fit,
    md        = fit$model_data,
    lambda    = if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_,
    adj_r2    = summary(fit$spec$fit)$adj.r.squared,
    rmse      = sqrt(mean(residuals(fit$spec$fit)^2)),
    n         = nobs(fit$spec$fit),
    surviving = fit$surviving_knots,
    cci       = fit$model_data$cci_williams
  )
}

# ----------------------------------------------------------------------------
# Build / fit both CCI variants
# ----------------------------------------------------------------------------
res_max <- build_and_fit(dat, build_williams_cci_basis,            "Maximal-GETS (15 candidate knots)")
res_sec <- build_and_fit(dat, build_williams_cci_basis_sectional,  "Sectional (Williams period priors)")

cat(sprintf("\n--- Comparison ---\n"))
cat(sprintf("                      Maximal-GETS    Sectional\n"))
cat(sprintf("  surviving knots     %-14d  %d\n",
            length(res_max$surviving), length(res_sec$surviving)))
cat(sprintf("  lambda              %-14.4f  %.4f\n", res_max$lambda, res_sec$lambda))
cat(sprintf("  adj R^2             %-14.4f  %.4f\n", res_max$adj_r2, res_sec$adj_r2))
cat(sprintf("  RMSE                %-14.5f  %.5f\n", res_max$rmse,  res_sec$rmse))
cat(sprintf("  n                   %-14d  %d\n",     res_max$n,     res_sec$n))

# Correlation between the two CCI series
common_idx <- !is.na(res_max$cci) & !is.na(res_sec$cci)
rho_cci <- cor(res_max$cci[common_idx], res_sec$cci[common_idx])
cat(sprintf("  cor(maximal, sectional) = %.4f\n", rho_cci))

# Save side-by-side coefficient table
ext_cf <- function(fit_obj, label) {
  cf <- summary(fit_obj$spec$fit)$coefficients
  tibble(
    cci_variant = label,
    term        = rownames(cf),
    estimate    = cf[, "Estimate"],
    std_error   = cf[, "Std. Error"],
    t_stat      = cf[, "t value"],
    p_value     = cf[, "Pr(>|t|)"]
  )
}
coef_tbl <- bind_rows(
  ext_cf(res_max$fit, "maximal_gets"),
  ext_cf(res_sec$fit, "sectional")
)
write_csv(coef_tbl, file.path(PROJ_LIVES, "outputs", "sectional_cci_comparison.csv"))

# ----------------------------------------------------------------------------
# Placebo with sectional protocol: 200 random period-set draws.
# Each draw: pick 8 random period start dates uniformly in 1979-2021
# (matching the sectional cardinality), with random ±1 sign priors
# (matching the protocol). Apply Hendry-Krolzig sign-prior reduction.
# ----------------------------------------------------------------------------
N_DRAWS  <- 200L
N_KNOTS  <- length(attr(build_williams_cci_basis_sectional(dat$date), "knots"))
WINDOW_LO <- as.Date("1979-01-01")
WINDOW_HI <- as.Date("2021-12-01")

# Spec 1 + extended-sample setup for placebo (matching the original placebo's
# spec template, so percentiles are comparable)
dat$ln_networth_y_extended <- dat$ln_networth_y_proxy

spec_template <- list(
  lr_vars    = c("ln_networth_y_extended", "ln_hp_over_y", "real_rate",
                 "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

fit_with_basis <- function(knots, sign_priors) {
  md <- dat
  if (any(grepl("^sdmma_", names(md)))) md <- md[, !grepl("^sdmma_", names(md))]
  basis <- vapply(knots, function(k) smoothed_step(md$date, k),
                  numeric(nrow(md)))
  cci_terms <- paste0("sdmma_", gsub("-", "_", substr(knots, 1, 7)))
  cci_terms <- make.unique(cci_terms)
  colnames(basis) <- cci_terms
  for (j in seq_along(cci_terms)) md[[cci_terms[j]]] <- basis[, j]
  full_lr <- c(spec_template$lr_vars, cci_terms)
  spec1 <- tryCatch(
    fit_ecm_spec(md, "Sectional_Pl", full_lr,
                 spec_template$sr_vars, spec_template$dummy_vars,
                 sample_start = as.Date("1976-07-01"),
                 sample_end   = as.Date("2024-10-01")),
    error = function(e) NULL
  )
  if (is.null(spec1)) return(list(adj_r2 = NA_real_, lambda = NA_real_,
                                   n_obs = NA_integer_, n_surviving = NA_integer_))
  cf <- coef(spec1$fit)
  violators <- character(0)
  for (j in seq_along(cci_terms)) {
    nm <- cci_terms[j]
    if (nm %in% names(cf) && !is.na(cf[nm]) && cf[nm] != 0) {
      if (sign(cf[nm]) != sign_priors[j]) violators <- c(violators, nm)
    }
  }
  spec2 <- spec1
  if (length(violators) > 0L) {
    full_lr2 <- setdiff(full_lr, violators)
    if (length(intersect(cci_terms, full_lr2)) > 0L) {
      spec2 <- tryCatch(
        fit_ecm_spec(md, "Sectional_Pl", full_lr2,
                     spec_template$sr_vars, spec_template$dummy_vars,
                     sample_start = as.Date("1976-07-01"),
                     sample_end   = as.Date("2024-10-01")),
        error = function(e) spec1
      )
    }
  }
  cf2 <- coef(spec2$fit)
  candidate_surv <- intersect(cci_terms, names(cf2))
  surviving <- candidate_surv[!is.na(cf2[candidate_surv])]
  list(
    adj_r2      = summary(spec2$fit)$adj.r.squared,
    lambda      = if ("ecm_lag" %in% names(cf2)) cf2[["ecm_lag"]] else NA_real_,
    n_obs       = nobs(spec2$fit),
    n_surviving = length(surviving),
    n_violators = length(violators)
  )
}

# Canonical sectional benchmark (Williams' periods)
sec_basis <- build_williams_cci_basis_sectional(dat$date)
canonical_sec <- fit_with_basis(attr(sec_basis, "knots"),
                                 attr(sec_basis, "sign_priors"))
cat(sprintf("\n--- Sectional canonical (Williams' periods) on Spec-1-extended ---\n"))
cat(sprintf("  adj R^2 = %.4f, lambda = %.4f, n=%d, surviving = %d\n",
            canonical_sec$adj_r2, canonical_sec$lambda,
            canonical_sec$n_obs, canonical_sec$n_surviving))

# 200 random sectional draws
cat(sprintf("\n--- Running %d random sectional draws (%d knots each, random ±1 priors) ---\n",
            N_DRAWS, N_KNOTS))
window_lo_n <- as.numeric(WINDOW_LO)
window_hi_n <- as.numeric(WINDOW_HI)
placebo_rows <- list()
pb_step <- max(1L, N_DRAWS %/% 10L)
for (i in seq_len(N_DRAWS)) {
  if (i %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", i, N_DRAWS))
  random_dates  <- sort(as.Date(round(runif(N_KNOTS, window_lo_n, window_hi_n))))
  random_priors <- sample(c(-1L, 1L), N_KNOTS, replace = TRUE)
  res <- fit_with_basis(as.character(random_dates), random_priors)
  placebo_rows[[i]] <- tibble::tibble(
    draw      = i,
    adj_r2    = res$adj_r2,
    lambda    = res$lambda,
    n_obs     = res$n_obs,
    surviving = res$n_surviving
  )
}
placebo_tbl <- bind_rows(placebo_rows)

# Verdict
finite_r2 <- placebo_tbl$adj_r2[is.finite(placebo_tbl$adj_r2)]
finite_la <- placebo_tbl$lambda[is.finite(placebo_tbl$lambda)]
w_r2_pct <- mean(finite_r2 < canonical_sec$adj_r2, na.rm = TRUE)
w_la_pct <- mean(abs(finite_la) < abs(canonical_sec$lambda), na.rm = TRUE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("SECTIONAL PLACEBO VERDICT (extended sample, Spec 1 aggregate proxy)\n")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf("\nWilliams' sectional canonical (8 period knots):\n"))
cat(sprintf("  adjusted R^2 = %.4f\n", canonical_sec$adj_r2))
cat(sprintf("  lambda       = %.4f\n", canonical_sec$lambda))
cat(sprintf("\nPlacebo distribution (%d draws):\n", length(finite_r2)))
cat(sprintf("  adj R^2  mean=%.4f  median=%.4f  90th pct=%.4f\n",
            mean(finite_r2), median(finite_r2), quantile(finite_r2, 0.9)))
cat(sprintf("  |lambda| mean=%.4f  median=%.4f  90th pct=%.4f\n",
            mean(abs(finite_la)), median(abs(finite_la)),
            quantile(abs(finite_la), 0.9)))

cat(sprintf("\nWilliams sectional percentile rank:\n"))
cat(sprintf("  adj R^2:  %.0f%% (vs maximal-GETS at 64%%, vs literal 4-knot at 19%%)\n",
            100 * w_r2_pct))
cat(sprintf("  |lambda|: %.0f%% (vs maximal-GETS at 36%%, vs literal 4-knot at 10%%)\n",
            100 * w_la_pct))

verdict <- if (w_r2_pct > 0.9 && w_la_pct > 0.9) {
  "STRONG SUPPORT — sectional priors clearly outperform random"
} else if (w_r2_pct > 0.75 || w_la_pct > 0.75) {
  "MODERATE SUPPORT — sectional priors beat most random combinations"
} else if (w_r2_pct > 0.5) {
  "WEAK SUPPORT — sectional above median but not far"
} else {
  "DETRENDING CRITIQUE PERSISTS — sectional below random median"
}
cat(sprintf("\n>>> VERDICT: %s\n\n", verdict))

# Save summary
write_csv(tibble(
  metric = c("Sectional canonical adj R^2",
             "Sectional canonical |lambda|",
             "Sectional adj R^2 percentile rank",
             "Sectional |lambda| percentile rank",
             "Comparator note",
             "Verdict"),
  value = c(sprintf("%.4f", canonical_sec$adj_r2),
            sprintf("%.4f", canonical_sec$lambda),
            sprintf("%.0fth", 100 * w_r2_pct),
            sprintf("%.0fth", 100 * w_la_pct),
            "see australia_williams_knot_placebo*_summary.csv",
            verdict)
), file.path(PROJ_LIVES, "outputs", "sectional_placebo_summary.csv"))

cat(sprintf("Saved: LIVES/outputs/sectional_cci_comparison.csv\n"))
cat(sprintf("Saved: LIVES/outputs/sectional_placebo_summary.csv\n"))
