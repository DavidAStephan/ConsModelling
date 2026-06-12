#!/usr/bin/env Rscript
# ==============================================================================
# Williams CCI placebo test (NS-106)
# ==============================================================================
#
# Question: are Williams' canonical 4 knot dates (1979/1992/1998/2007)
# institutionally meaningful, or could a randomly-chosen set of 4 knot
# dates produce equally good consumption-equation fit?
#
# Procedure:
#   1. Generate N_DRAWS random 4-knot sets uniformly distributed in
#      [1979, 2007] (Williams' candidate window).
#   2. For each random set, build the SDMMA basis and refit the same
#      Spec-4-style consumption equation as the canonical Williams setup.
#   3. Record adjusted R² and λ for each placebo fit.
#   4. Position Williams' actual canonical 4-knot result in the empirical
#      distribution.
#
# Verdict:
#   - If Williams' canonical 4-knot R² and |λ| are in the upper tail
#     (top 10%) of the placebo distribution → institutional knot placement
#     is doing real identification work; the spline is structural.
#   - If they sit near the median → the user's 'detrending' critique is
#     vindicated; the spline is largely capturing whatever flexible
#     curve a 4-knot SDMMA can fit.
#
# Output:
#   outputs/australia_williams_knot_placebo.csv
#   outputs/australia_williams_knot_placebo.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(tidyr)
  library(ggplot2); library(zoo); library(sandwich); library(lmtest)
  library(broom); library(stringr); library(lubridate); library(strucchange)
})

set.seed(20260507L)
N_DRAWS <- 200L  # number of placebo draws

.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
output_dir   <- file.path(project_root, "outputs")

source(file.path(project_root, "R", "model_helpers.R"))

# Use pipeline state when sourced from australia_estimation.R MAIN (the
# orchestrator sources this script into an isolated child environment, so the
# canonical model_data — including the canonical permanent-income method and
# the deployed cci_williams — is readable here and assignments stay local).
# Rebuild from the RDS only when running standalone. A previous version
# rebuilt unconditionally with the AR permanent-income constructor AND
# assigned into the caller's environment, clobbering the canonical model_data
# for every later pipeline step.
if (exists("model_data") && exists("fit_ecm_spec") &&
    exists("construct_permanent_income_italy")) {
  message("[cci_placebo_test] using pipeline model_data (canonical PI method)")
  model_data <- model_data  # local working copy in this script's environment
} else {
  master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))

  # Build model_data the same way the orchestrator does, then run the
  # pre-Step-1 estimation-script content to get add_model_variables() etc.
  model_data <- master %>%
    rename(
      dlcons   = d_ln_cons_pc,
      lincome  = ln_ydi_real_pc,
      lcons    = ln_cons_real_pc
    ) %>%
    mutate(ecm_lag = lag(lcons, 1L) - lincome)

  src <- readLines(file.path(project_root, "R", "australia_estimation.R"))
  guard_start <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
  guard_end   <- guard_start + 6L  # block is 7 lines including closing brace
  src_safe <- src[-(guard_start:guard_end)]
  main_start <- grep("\\[Step 1\\]", src_safe)[1L]
  eval(parse(text = paste(src_safe[1:(main_start - 1L)], collapse = "\n")),
       envir = environment())

  model_data <- add_model_variables(model_data)
  model_data <- compute_income_volatility(model_data)
  model_data <- if (identical(PI_METHOD, "italy")) {
    construct_permanent_income_italy(model_data)
  } else {
    construct_permanent_income(model_data)
  }
  model_data <- model_data %>%
    mutate(ecm_lag = lag(lcons, 1L) - lincome)
}

# Spec-4-style template (no SR dynamics, base dummies)
spec_template <- list(
  lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

# Refit-with-knot-set: returns adj R^2 and lambda; no sign-prior reduction
# (we want to test pure fit, not survival). All 4 knots enter the long-run
# unconditionally as additional regressors.
fit_with_knot_set <- function(knot_dates) {
  md <- model_data
  if (any(grepl("^sdmma_", names(md)))) {
    md <- md[, !grepl("^sdmma_", names(md))]
  }
  basis <- vapply(knot_dates, function(k) smoothed_step(md$date, k),
                  numeric(nrow(md)))
  cci_terms <- paste0("sdmma_", gsub("-", "_", substr(knot_dates, 1, 7)))
  colnames(basis) <- cci_terms
  for (j in seq_along(cci_terms)) md[[cci_terms[j]]] <- basis[, j]
  full_lr <- c(spec_template$lr_vars, cci_terms)
  spec <- tryCatch(
    fit_ecm_spec(md, "Placebo", full_lr,
                 spec_template$sr_vars, spec_template$dummy_vars,
                 sample_end = as.Date("2024-10-01")),
    error = function(e) NULL
  )
  if (is.null(spec)) return(list(adj_r2 = NA_real_, lambda = NA_real_,
                                  n_obs = NA_integer_, n_aliased = NA_integer_))
  cf <- coef(spec$fit)
  n_aliased <- sum(is.na(cf[cci_terms]))
  list(
    adj_r2     = summary(spec$fit)$adj.r.squared,
    lambda     = if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_,
    n_obs      = nobs(spec$fit),
    n_aliased  = n_aliased
  )
}

# 1. Williams canonical 4-knot — the actual benchmark
canonical_knots <- c("1979-01-01", "1992-01-01", "1998-01-01", "2007-01-01")
cat(sprintf("\nFitting Williams canonical 4-knot benchmark...\n"))
canonical_fit <- fit_with_knot_set(canonical_knots)
cat(sprintf("  Williams canonical: adj R^2 = %.4f, lambda = %.4f, %d aliased of 4\n",
            canonical_fit$adj_r2, canonical_fit$lambda, canonical_fit$n_aliased))

# 2. 200 random 4-knot sets uniformly in [1979, 2007]
window_start <- as.numeric(as.Date("1979-01-01"))
window_end   <- as.numeric(as.Date("2007-12-01"))
cat(sprintf("\nRunning %d placebo draws (uniform random knots in 1979-2007)...\n",
            N_DRAWS))

placebo_rows <- list()
pb_step <- max(1L, N_DRAWS %/% 20L)
for (i in seq_len(N_DRAWS)) {
  if (i %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", i, N_DRAWS))
  random_dates <- sort(as.Date(round(runif(4, window_start, window_end))))
  res <- fit_with_knot_set(as.character(random_dates))
  placebo_rows[[i]] <- tibble::tibble(
    draw      = i,
    knots     = paste(format(random_dates), collapse = ", "),
    adj_r2    = res$adj_r2,
    lambda    = res$lambda,
    n_obs     = res$n_obs,
    n_aliased = res$n_aliased
  )
}
placebo_tbl <- bind_rows(placebo_rows)
write.csv(placebo_tbl, file.path(output_dir, "australia_williams_knot_placebo.csv"),
          row.names = FALSE)

# Position Williams in the distribution
finite_r2 <- placebo_tbl$adj_r2[is.finite(placebo_tbl$adj_r2)]
finite_la <- placebo_tbl$lambda[is.finite(placebo_tbl$lambda)]

w_r2_pct  <- mean(finite_r2 < canonical_fit$adj_r2, na.rm = TRUE)
w_la_pct  <- mean(abs(finite_la) < abs(canonical_fit$lambda), na.rm = TRUE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("PLACEBO TEST VERDICT\n")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf("\nWilliams canonical 4-knot benchmark:\n"))
cat(sprintf("  adjusted R^2 = %.4f\n", canonical_fit$adj_r2))
cat(sprintf("  lambda       = %.4f\n", canonical_fit$lambda))
cat(sprintf("\nPlacebo distribution from %d random 4-knot draws (uniform in 1979-2007):\n",
            N_DRAWS))
cat(sprintf("  adj R^2  mean=%.4f  median=%.4f  90th pct=%.4f  max=%.4f\n",
            mean(finite_r2), median(finite_r2),
            quantile(finite_r2, 0.9), max(finite_r2)))
cat(sprintf("  |lambda| mean=%.4f  median=%.4f  90th pct=%.4f  max=%.4f\n",
            mean(abs(finite_la)), median(abs(finite_la)),
            quantile(abs(finite_la), 0.9), max(abs(finite_la))))

cat(sprintf("\nWilliams percentile rank in placebo distribution:\n"))
cat(sprintf("  adj R^2:  %.0f%% (Williams beats %.0f%% of random draws)\n",
            100 * w_r2_pct, 100 * w_r2_pct))
cat(sprintf("  |lambda|: %.0f%% (Williams' |lambda| larger than %.0f%% of random)\n",
            100 * w_la_pct, 100 * w_la_pct))

verdict <- if (w_r2_pct > 0.9 && w_la_pct > 0.9) {
  "STRONG SUPPORT for institutional knot placement"
} else if (w_r2_pct > 0.75 || w_la_pct > 0.75) {
  "MODERATE SUPPORT — Williams beats most random draws"
} else if (w_r2_pct > 0.5) {
  "WEAK SUPPORT — Williams above median but not far"
} else {
  "DETRENDING CRITIQUE VINDICATED — Williams below median"
}
cat(sprintf("\n  VERDICT: %s\n\n", verdict))

# Plot: histogram of placebo R^2 and lambda with Williams marked
p1_data <- tibble(adj_r2 = finite_r2)
p1 <- ggplot(p1_data, aes(adj_r2)) +
  geom_histogram(bins = 30L, fill = "lightgrey", colour = "black") +
  geom_vline(xintercept = canonical_fit$adj_r2,
             colour = "firebrick", linewidth = 1.2) +
  annotate("text", x = canonical_fit$adj_r2,
           y = max(table(cut(p1_data$adj_r2, 30L))) * 0.9,
           label = sprintf("Williams canonical\n(%.0fth percentile)",
                            100 * w_r2_pct),
           hjust = if (w_r2_pct > 0.5) 1.05 else -0.05,
           colour = "firebrick", fontface = "bold") +
  labs(title = "Placebo distribution: adjusted R²",
       subtitle = sprintf("%d random 4-knot draws uniform in 1979-2007",
                           N_DRAWS),
       x = "adjusted R²", y = "count") +
  theme_minimal(base_size = 11)

p2_data <- tibble(lambda_abs = abs(finite_la))
p2 <- ggplot(p2_data, aes(lambda_abs)) +
  geom_histogram(bins = 30L, fill = "lightgrey", colour = "black") +
  geom_vline(xintercept = abs(canonical_fit$lambda),
             colour = "firebrick", linewidth = 1.2) +
  annotate("text", x = abs(canonical_fit$lambda),
           y = max(table(cut(p2_data$lambda_abs, 30L))) * 0.9,
           label = sprintf("Williams canonical\n(%.0fth percentile)",
                            100 * w_la_pct),
           hjust = if (w_la_pct > 0.5) 1.05 else -0.05,
           colour = "firebrick", fontface = "bold") +
  labs(title = "Placebo distribution: |λ|",
       subtitle = sprintf("Verdict: %s", verdict),
       x = "|λ|", y = "count") +
  theme_minimal(base_size = 11)

if (requireNamespace("patchwork", quietly = TRUE)) {
  combined <- patchwork::wrap_plots(p1, p2, ncol = 2L)
} else {
  combined <- p1
}

out_png <- file.path(output_dir, "australia_williams_knot_placebo.png")
ggsave(out_png, combined, width = 12, height = 5, dpi = 200)
cat(sprintf("Saved: %s\n", out_png))

# Verdict summary CSV
verdict_tbl <- tibble(
  metric = c("Williams adj R^2", "Williams |lambda|",
             "Williams adj R^2 percentile rank",
             "Williams |lambda| percentile rank",
             "n_placebo_draws", "verdict"),
  value = c(
    sprintf("%.4f", canonical_fit$adj_r2),
    sprintf("%.4f", abs(canonical_fit$lambda)),
    sprintf("%.0f%%", 100 * w_r2_pct),
    sprintf("%.0f%%", 100 * w_la_pct),
    as.character(N_DRAWS),
    verdict
  )
)
write.csv(verdict_tbl,
          file.path(output_dir, "australia_williams_knot_placebo_verdict.csv"),
          row.names = FALSE)
cat(sprintf("Saved: %s\n",
            file.path(output_dir, "australia_williams_knot_placebo_verdict.csv")))

# ==============================================================================
# DEPLOYED-PROTOCOL placebo (review item: the runs above test the literal
# Williams 4-knot entered unconditionally — a DIFFERENT object from the
# deployed cci_williams, which is the 15-knot maximal basis under ITERATED
# sign-prior reduction inside the Spec-4 long run on the 1988+ sample. This
# section placebo-tests exactly the deployed construction: each draw replaces
# the 15 institutional knot dates with random dates (same sign-prior pattern)
# and runs the same iterated fit_consumption_with_williams_cci() reduction.)
# ==============================================================================

if (exists("fit_consumption_with_williams_cci")) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("DEPLOYED-PROTOCOL PLACEBO (iterated reduction, 15-knot basis)\n")
  cat(strrep("=", 70), "\n", sep = "")

  set.seed(20260611L)

  deployed_basis  <- build_williams_cci_basis(model_data$date)
  deployed_priors <- attr(deployed_basis, "sign_priors")
  n_knots         <- ncol(deployed_basis)

  make_basis_fn <- function(knot_dates, priors) {
    force(knot_dates); force(priors)
    function(d) {
      basis <- vapply(knot_dates, function(k) smoothed_step(d, k),
                      numeric(length(d)))
      colnames(basis) <- paste0("sdmma_",
                                gsub("-", "_", substr(knot_dates, 1, 7)))
      attr(basis, "sign_priors") <- priors
      basis
    }
  }

  run_deployed_protocol <- function(basis_fn) {
    md <- model_data
    md <- md[, !grepl("^sdmma_", names(md))]
    res <- tryCatch(
      fit_consumption_with_williams_cci(
        md,
        lr_vars    = spec_template$lr_vars,
        sr_vars    = spec_template$sr_vars,
        dummy_vars = spec_template$dummy_vars,
        sample_end = as.Date("2024-10-01"),
        basis_fn   = basis_fn
      ),
      error = function(e) NULL
    )
    if (is.null(res)) {
      return(list(adj_r2 = NA_real_, lambda = NA_real_,
                  n_obs = NA_integer_, n_survivors = NA_integer_))
    }
    cf <- coef(res$spec$fit)
    list(
      adj_r2      = summary(res$spec$fit)$adj.r.squared,
      lambda      = if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_,
      n_obs       = nobs(res$spec$fit),
      n_survivors = length(res$surviving_knots)
    )
  }

  cat("\nFitting deployed (institutional 15-knot, iterated reduction) benchmark...\n")
  deployed_fit <- run_deployed_protocol(build_williams_cci_basis)
  cat(sprintf("  Deployed: adj R^2 = %.4f, lambda = %.4f, %d knots survive\n",
              deployed_fit$adj_r2, deployed_fit$lambda,
              deployed_fit$n_survivors))

  dp_window_start <- as.numeric(as.Date("1979-01-01"))
  dp_window_end   <- as.numeric(as.Date("2021-12-01"))
  cat(sprintf("\nRunning %d deployed-protocol placebo draws...\n", N_DRAWS))

  dp_rows <- list()
  for (i in seq_len(N_DRAWS)) {
    if (i %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", i, N_DRAWS))
    repeat {
      rd <- sort(as.Date(round(runif(n_knots, dp_window_start, dp_window_end))))
      nm <- substr(as.character(rd), 1, 7)
      if (!anyDuplicated(nm)) break  # avoid duplicate sdmma_YYYY_MM columns
    }
    res <- run_deployed_protocol(make_basis_fn(as.character(rd),
                                               deployed_priors))
    dp_rows[[i]] <- tibble::tibble(
      draw        = i,
      knots       = paste(format(rd), collapse = ", "),
      adj_r2      = res$adj_r2,
      lambda      = res$lambda,
      n_obs       = res$n_obs,
      n_survivors = res$n_survivors
    )
  }
  dp_tbl <- bind_rows(dp_rows)
  write.csv(dp_tbl,
            file.path(output_dir, "australia_williams_knot_placebo_deployed.csv"),
            row.names = FALSE)

  dp_r2 <- dp_tbl$adj_r2[is.finite(dp_tbl$adj_r2)]
  dp_la <- dp_tbl$lambda[is.finite(dp_tbl$lambda)]
  dp_r2_pct <- mean(dp_r2 < deployed_fit$adj_r2, na.rm = TRUE)
  dp_la_pct <- mean(abs(dp_la) < abs(deployed_fit$lambda), na.rm = TRUE)

  dp_verdict <- if (dp_r2_pct > 0.9 && dp_la_pct > 0.9) {
    "STRONG SUPPORT for institutional knot placement"
  } else if (dp_r2_pct > 0.75 || dp_la_pct > 0.75) {
    "MODERATE SUPPORT — deployed CCI beats most random draws"
  } else if (dp_r2_pct > 0.5) {
    "WEAK SUPPORT — deployed CCI above median but not far"
  } else {
    "DETRENDING CRITIQUE VINDICATED — deployed CCI below median"
  }

  cat(sprintf("\nDeployed-protocol percentile rank: adj R^2 %.0f%%, |lambda| %.0f%%\n",
              100 * dp_r2_pct, 100 * dp_la_pct))
  cat(sprintf("  median survivors in placebo draws: %.0f (deployed: %d)\n",
              median(dp_tbl$n_survivors, na.rm = TRUE),
              deployed_fit$n_survivors))
  cat(sprintf("  VERDICT: %s\n", dp_verdict))

  dp_verdict_tbl <- tibble(
    metric = c("Deployed adj R^2", "Deployed |lambda|",
               "Deployed adj R^2 percentile rank",
               "Deployed |lambda| percentile rank",
               "Deployed n_survivors",
               "Placebo median n_survivors",
               "n_placebo_draws", "verdict"),
    value = c(
      sprintf("%.4f", deployed_fit$adj_r2),
      sprintf("%.4f", abs(deployed_fit$lambda)),
      sprintf("%.0f%%", 100 * dp_r2_pct),
      sprintf("%.0f%%", 100 * dp_la_pct),
      as.character(deployed_fit$n_survivors),
      sprintf("%.0f", median(dp_tbl$n_survivors, na.rm = TRUE)),
      as.character(N_DRAWS),
      dp_verdict
    )
  )
  write.csv(dp_verdict_tbl,
            file.path(output_dir,
                      "australia_williams_knot_placebo_deployed_verdict.csv"),
            row.names = FALSE)
  cat(sprintf("Saved: %s\n",
              file.path(output_dir,
                        "australia_williams_knot_placebo_deployed_verdict.csv")))
} else {
  message("[cci_placebo_test] fit_consumption_with_williams_cci not available; ",
          "skipping deployed-protocol placebo")
}
