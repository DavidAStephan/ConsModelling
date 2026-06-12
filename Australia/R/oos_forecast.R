#!/usr/bin/env Rscript
# ==============================================================================
# Out-of-sample forecast validation (NS-033)
# ==============================================================================
#
# A flexible rolling-window OOS validator for consumption-equation specs.
# Designed to work with any spec produced by run_all_specifications() or by
# fit_ecm_spec() directly, plus arbitrary future specs.
#
# Conditional vs unconditional forecasts:
#   - CONDITIONAL (default; what this script does): given the actual realised
#     macro path (income, wealth, rates, prices, ...), how well does the
#     consumption equation predict consumption growth h periods ahead? This
#     is the standard policy-counterfactual question -- 'if the macro state
#     evolves as observed, what does the model say about consumption?'
#   - UNCONDITIONAL (not implemented): forecast all RHS variables jointly
#     (would need a VAR or set of auxiliary equations). Pure unconditional
#     forecast is meaningful only in a multi-equation system; in a single-
#     equation framework conditional is the right concept.
#
# Forecast iteration for h-step-ahead:
#   - For h=1: use actual RHS at t+1, predict dlcons_{t+1}.
#   - For h>1: feed the predicted log c_{t+1}, ..., log c_{t+h-1} back
#     through ecm_lag at each step. All other RHS vars at their actual
#     observed values.
#
# Benchmarks:
#   - 'rw_drift': random walk with drift; forecasts dlcons_{t+h} = mean
#     of dlcons over training sample.
#   - 'ar1':      AR(1) on dlcons; forecasts iterated.
#
# Adding a new spec to the validator:
#   pass an additional element to specs_list in run_oos_validation() with
#   the same shape as fit_ecm_spec output: a list with $spec_name,
#   $lr_vars, $sr_vars, $dummy_vars, $est_data, and $fit. Spec 10 (Williams-
#   prior calibrated) is a known exception and is handled in the loop with
#   a special branch.
#
# Outputs:
#   outputs/australia_oos_forecasts.csv           long-format forecast paths
#   outputs/australia_oos_rmse.csv                RMSE summary spec x horizon
#   outputs/australia_oos_forecast_paths.png      preferred spec actual+forecast
#   outputs/australia_oos_rolling_rmse.png        rolling RMSE chart
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(readr)
  library(ggplot2); library(zoo); library(sandwich); library(lmtest)
  library(broom); library(stringr); library(lubridate); library(strucchange)
})

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

# ==============================================================================
# Core helper: forecast one spec at a single window end
# ==============================================================================
#
# Inputs:
#   spec_template  list with $spec_name, $lr_vars, $sr_vars, $dummy_vars
#                  (the est_data and fit fields are NOT needed; we refit
#                  on each window's training subsample)
#   model_data     full master tibble (with columns dlcons, lcons,
#                  lincome, ecm_lag, and all RHS vars)
#   window_end     as.Date — last quarter included in the training sample
#   horizons       integer vector, e.g. c(1, 4, 8)
#
# Returns: tibble (window_end, horizon, target_date, actual_dlcons,
#                  forecast_dlcons, forecast_se, lcons_actual,
#                  lcons_forecast)
forecast_at_window <- function(spec_template, model_data, window_end,
                                horizons = c(1L, 4L, 8L)) {
  if (!"date" %in% names(model_data)) stop("model_data must have a 'date' column")
  d <- model_data %>% arrange(date)
  train <- d %>% filter(date <= window_end)
  if (nrow(train) < 30L) {
    return(tibble::tibble())
  }
  rhs_terms <- c(spec_template$lr_vars, spec_template$sr_vars,
                 spec_template$dummy_vars)
  # Surface (do not silently drop) any declared regressor missing from
  # model_data — the failure mode that previously collapsed Spec 8/9 to
  # their base form when the CCI interaction columns were not attached.
  missing_rhs <- setdiff(rhs_terms, names(train))
  if (length(missing_rhs) > 0L)
    warning(sprintf(
      "[forecast_at_window] %s: declared regressors absent from model_data and dropped: %s",
      spec_template$spec_name, paste(missing_rhs, collapse = ", ")))
  rhs_terms <- intersect(rhs_terms, names(train))
  req <- c("dlcons", rhs_terms)
  cc <- complete.cases(train[, req, drop = FALSE])
  train_cc <- train[cc, , drop = FALSE]
  if (nrow(train_cc) < 30L) return(tibble::tibble())

  # Refit
  fmla <- reformulate(rhs_terms, response = "dlcons")
  fit  <- tryCatch(lm(fmla, data = train_cc), error = function(e) NULL)
  if (is.null(fit)) return(tibble::tibble())

  # Drop terms lm aliased and refit if needed
  aliased <- names(which(is.na(coef(fit))))
  aliased <- setdiff(aliased, "(Intercept)")
  if (length(aliased) > 0L) {
    rhs_terms <- setdiff(rhs_terms, aliased)
    fmla <- reformulate(rhs_terms, response = "dlcons")
    fit  <- lm(fmla, data = train_cc)
  }

  resid_se <- summary(fit)$sigma  # residual SE for prediction-interval halfwidth

  out_rows <- list()
  max_h <- max(horizons)
  # Iteratively predict dlcons one step at a time, feeding back into ecm_lag
  # for h>1. All other RHS vars come from actuals (conditional forecast).
  predicted_dlcons <- numeric(max_h)
  predicted_lcons  <- numeric(max_h)
  predicted_lcons_running <- train$lcons[which(train$date == window_end)[1L]]
  if (length(predicted_lcons_running) == 0L || is.na(predicted_lcons_running))
    return(tibble::tibble())

  # Future quarters 1..max_h after window_end
  future_dates <- d$date[d$date > window_end][seq_len(max_h)]
  future_dates <- future_dates[!is.na(future_dates)]
  if (length(future_dates) < 1L) return(tibble::tibble())

  for (h in seq_along(future_dates)) {
    target_date <- future_dates[h]
    target_row <- d[d$date == target_date, , drop = FALSE]
    if (nrow(target_row) != 1L) next

    # For multi-step iteration, ecm_lag at t+h needs lag(lcons, 1) - lincome
    # which requires lcons at t+h-1 (predicted) and lincome at t+h (actual).
    if (h == 1L) {
      ecm_lag_t <- predicted_lcons_running - target_row$lincome
    } else {
      ecm_lag_t <- predicted_lcons[h - 1L] - target_row$lincome
    }

    # Build the prediction row using actual RHS vars except for the iterated
    # ecm_lag (which uses predicted lcons history).
    pred_row <- target_row
    if ("ecm_lag" %in% rhs_terms) pred_row$ecm_lag <- ecm_lag_t

    # Predict dlcons at t+h
    pred_complete <- all(rhs_terms %in% names(pred_row)) &&
                     !any(is.na(pred_row[, rhs_terms, drop = FALSE]))
    if (!pred_complete) {
      # Skip this horizon if any RHS var is missing
      next
    }
    fc_dlcons <- as.numeric(predict(fit, newdata = pred_row))

    predicted_dlcons[h] <- fc_dlcons
    predicted_lcons[h]  <- (if (h == 1L) predicted_lcons_running
                             else predicted_lcons[h - 1L]) + fc_dlcons

    if (h %in% horizons) {
      out_rows[[length(out_rows) + 1L]] <- tibble::tibble(
        window_end       = window_end,
        horizon          = h,
        target_date      = target_date,
        actual_dlcons    = target_row$dlcons,
        forecast_dlcons  = fc_dlcons,
        forecast_se      = resid_se,
        actual_lcons     = target_row$lcons,
        forecast_lcons   = predicted_lcons[h],
        spec_name        = spec_template$spec_name
      )
    }
  }
  bind_rows(out_rows)
}


# ==============================================================================
# Rolling-window orchestrator for one spec
# ==============================================================================

rolling_forecast <- function(spec_template, model_data,
                              rolling_start = as.Date("2015-01-01"),
                              rolling_end   = NA,
                              horizons      = c(1L, 4L, 8L),
                              window_step   = 1L,
                              verbose       = FALSE) {
  d <- model_data %>% arrange(date)
  if (is.na(rolling_end)) rolling_end <- max(d$date) - max(horizons) * 91L
  windows <- d$date[d$date >= rolling_start & d$date <= rolling_end]
  windows <- windows[seq(1L, length(windows), by = window_step)]
  if (length(windows) == 0L) return(tibble::tibble())

  if (verbose) {
    message(sprintf("[rolling_forecast %s] %d windows from %s to %s",
                    spec_template$spec_name, length(windows),
                    format(min(windows)), format(max(windows))))
  }

  rows <- list()
  for (i in seq_along(windows)) {
    w <- windows[i]
    fc <- forecast_at_window(spec_template, model_data, w, horizons = horizons)
    if (nrow(fc) > 0L) rows[[length(rows) + 1L]] <- fc
  }
  bind_rows(rows)
}


# ==============================================================================
# Attach the Spec 8 / Spec 9 CCI interaction columns to model_data.
#
# These columns are built only on local md8/md9 copies inside
# run_all_specifications, so the OOS harness never sees them and the CCI
# specs silently collapse to their base form. Replicate the md8/md9
# construction here (matching australia_estimation.R) so the OOS refit
# includes them. NOTE: cci_williams and the de-mean constants are
# full-sample objects, so the OOS for the CCI specs is conditional on a
# full-sample-constructed CCI — NOT real-time for the credit channel. The
# permanent-income input is real-time ONLY in the standalone path (which
# rebuilds with the expanding-window AR constructor); when sourced from the
# pipeline, model_data carries the canonical FULL-SAMPLE Italy LP measure,
# so the committed OOS embeds a non-causal PI as well. Both caveats must be
# disclosed in WP §8.13.
# ==============================================================================
attach_cci_oos_interactions <- function(md, sample_end = NA) {
  if (is.na(sample_end)) sample_end <- max(md$date, na.rm = TRUE)
  if ("cci_williams" %in% names(md) && any(!is.na(md$cci_williams))) {
    m <- !is.na(md$cci_williams) &
         md$date >= as.Date("1980-01-01") & md$date <= sample_end
    ha_mean <- mean(md$ha_y[m],         na.rm = TRUE)
    hp_mean <- mean(md$ln_hp_over_y[m], na.rm = TRUE)
    r_mean  <- mean(md$real_rate[m],    na.rm = TRUE)
    yp_mean <- mean(md$ln_yp_over_y[m], na.rm = TRUE)
    md <- md %>% mutate(
      r_x_cci          = (real_rate    - r_mean)  * cci_williams,
      hp_x_1_minus_cci = (ln_hp_over_y - hp_mean) * (1 - 1.2 * cci_williams),
      yp_x_cci         = (ln_yp_over_y - yp_mean) * cci_williams,
      ha_x_cci         = (ha_y         - ha_mean) * cci_williams
    )
  }
  if ("cci_kalman" %in% names(md) && any(!is.na(md$cci_kalman))) {
    md <- md %>% mutate(
      r_x_cci_k          = real_rate    * cci_kalman,
      hp_x_1_minus_cci_k = ln_hp_over_y * (1 - 1.2 * cci_kalman),
      yp_x_cci_k         = ln_yp_over_y * cci_kalman
    )
  }
  md
}


# ==============================================================================
# Naive benchmark forecasters
# ==============================================================================

# Random walk with drift: forecast dlcons_{t+h} = mean of dlcons over training
benchmark_rw_drift <- function(model_data, rolling_start, rolling_end,
                                horizons) {
  d <- model_data %>% arrange(date)
  windows <- d$date[d$date >= rolling_start & d$date <= rolling_end]
  rows <- list()
  for (w_idx in seq_along(windows)) {
    w <- windows[w_idx]  # preserves Date class
    train <- d %>% filter(date <= w & !is.na(dlcons))
    if (nrow(train) < 12L) next
    drift <- mean(train$dlcons)
    se    <- sd(train$dlcons)
    last_lcons <- tail(train$lcons[!is.na(train$lcons)], 1L)
    if (length(last_lcons) == 0L) next
    future_dates <- d$date[d$date > w][seq_len(max(horizons))]
    future_dates <- future_dates[!is.na(future_dates)]
    pred_lcons <- last_lcons
    for (h in seq_along(future_dates)) {
      target <- d[d$date == future_dates[h], ]
      if (nrow(target) != 1L) next
      pred_lcons <- pred_lcons + drift
      if (h %in% horizons) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          window_end       = w,
          horizon          = h,
          target_date      = target$date,
          actual_dlcons    = target$dlcons,
          forecast_dlcons  = drift,
          forecast_se      = se,
          actual_lcons     = target$lcons,
          forecast_lcons   = pred_lcons,
          spec_name        = "Benchmark_RW_drift"
        )
      }
    }
  }
  bind_rows(rows)
}

# AR(1) on dlcons
benchmark_ar1 <- function(model_data, rolling_start, rolling_end, horizons) {
  d <- model_data %>% arrange(date) %>%
    mutate(dlcons_lag1 = lag(dlcons, 1L))
  windows <- d$date[d$date >= rolling_start & d$date <= rolling_end]
  rows <- list()
  for (w_idx in seq_along(windows)) {
    w <- windows[w_idx]  # preserves Date class
    train <- d %>% filter(date <= w) %>%
      filter(!is.na(dlcons) & !is.na(dlcons_lag1))
    if (nrow(train) < 20L) next
    fit <- lm(dlcons ~ dlcons_lag1, data = train)
    cf  <- coef(fit)
    a   <- cf[["(Intercept)"]]
    b   <- cf[["dlcons_lag1"]]
    se  <- summary(fit)$sigma
    last_dlcons <- train$dlcons[nrow(train)]
    last_lcons  <- tail(train$lcons[!is.na(train$lcons)], 1L)
    if (length(last_lcons) == 0L) next
    future_dates <- d$date[d$date > w][seq_len(max(horizons))]
    future_dates <- future_dates[!is.na(future_dates)]
    pred_dlcons <- last_dlcons
    pred_lcons  <- last_lcons
    for (h in seq_along(future_dates)) {
      pred_dlcons <- a + b * pred_dlcons
      pred_lcons  <- pred_lcons + pred_dlcons
      if (h %in% horizons) {
        target <- d[d$date == future_dates[h], ]
        if (nrow(target) != 1L) next
        rows[[length(rows) + 1L]] <- tibble::tibble(
          window_end       = w,
          horizon          = h,
          target_date      = target$date,
          actual_dlcons    = target$dlcons,
          forecast_dlcons  = pred_dlcons,
          forecast_se      = se,
          actual_lcons     = target$lcons,
          forecast_lcons   = pred_lcons,
          spec_name        = "Benchmark_AR1"
        )
      }
    }
  }
  bind_rows(rows)
}


# ==============================================================================
# RMSE summary
# ==============================================================================

forecast_rmse_summary <- function(forecasts_long) {
  if (nrow(forecasts_long) == 0L) return(tibble::tibble())
  forecasts_long %>%
    filter(!is.na(forecast_dlcons), !is.na(actual_dlcons)) %>%
    mutate(error = actual_dlcons - forecast_dlcons) %>%
    group_by(spec_name, horizon) %>%
    summarise(
      n        = n(),
      mae      = mean(abs(error)),
      rmse     = sqrt(mean(error^2)),
      bias     = mean(error),
      mae_pct  = 100 * mean(abs(error) / (abs(actual_dlcons) + 1e-9)),
      .groups  = "drop"
    ) %>%
    arrange(horizon, rmse)
}


# ==============================================================================
# Top-level orchestrator
# ==============================================================================

run_oos_validation <- function(specs_list, model_data,
                                rolling_start = as.Date("2015-01-01"),
                                rolling_end   = NA,
                                horizons      = c(1L, 4L, 8L),
                                output_dir    = NULL) {
  if (is.na(rolling_end))
    rolling_end <- max(model_data$date, na.rm = TRUE) - 365L  # leave 4 quarters
  cat(sprintf("\n=== OOS validation: rolling %s -> %s, horizons %s ===\n",
              format(rolling_start), format(rolling_end),
              paste(horizons, collapse = ", ")))

  forecasts <- list()
  for (sp in specs_list) {
    if (is.null(sp)) next
    fc <- rolling_forecast(sp, model_data,
                            rolling_start = rolling_start,
                            rolling_end   = rolling_end,
                            horizons      = horizons,
                            verbose       = TRUE)
    if (nrow(fc) > 0L) forecasts[[sp$spec_name]] <- fc
  }

  cat("\n[run_oos_validation] running RW drift benchmark...\n")
  forecasts$Benchmark_RW_drift <- benchmark_rw_drift(model_data,
                                                       rolling_start,
                                                       rolling_end,
                                                       horizons)
  cat("[run_oos_validation] running AR(1) benchmark...\n")
  forecasts$Benchmark_AR1 <- benchmark_ar1(model_data, rolling_start,
                                            rolling_end, horizons)

  forecasts_long <- bind_rows(forecasts)
  if (nrow(forecasts_long) == 0L) {
    cat("[run_oos_validation] no forecasts generated\n")
    return(invisible(NULL))
  }

  rmse_tbl <- forecast_rmse_summary(forecasts_long)
  cat("\n--- RMSE summary (sorted by RMSE within horizon) ---\n")
  print(rmse_tbl, n = nrow(rmse_tbl))

  if (!is.null(output_dir)) {
    forecasts_path <- file.path(output_dir, "australia_oos_forecasts.csv")
    rmse_path      <- file.path(output_dir, "australia_oos_rmse.csv")
    write.csv(forecasts_long, forecasts_path, row.names = FALSE)
    write.csv(rmse_tbl,       rmse_path,      row.names = FALSE)
    cat(sprintf("\nSaved: %s\nSaved: %s\n", forecasts_path, rmse_path))

    # Plot 1: actual vs forecast over the OOS window for the first non-
    # benchmark spec (the "preferred" by convention is first in specs_list).
    primary_spec <- specs_list[[1L]]$spec_name
    primary_long <- forecasts_long %>%
      filter(spec_name == primary_spec, horizon == 1L)
    if (nrow(primary_long) > 0L) {
      p1 <- ggplot(primary_long,
                   aes(x = target_date)) +
        geom_line(aes(y = actual_dlcons), colour = "black", linewidth = 0.8) +
        geom_line(aes(y = forecast_dlcons), colour = "firebrick",
                  linewidth = 0.7, linetype = 2) +
        geom_ribbon(aes(ymin = forecast_dlcons - 1.96 * forecast_se,
                        ymax = forecast_dlcons + 1.96 * forecast_se),
                    fill = "firebrick", alpha = 0.15) +
        labs(title = sprintf("OOS 1-quarter-ahead forecast — %s",
                              primary_spec),
             subtitle = "Actual (black solid) vs forecast (red dashed) ± 1.96 SE",
             x = NULL, y = "Δ log(c)") +
        theme_minimal(base_size = 11)
      out_png <- file.path(output_dir, "australia_oos_forecast_paths.png")
      ggsave(out_png, p1, width = 10, height = 5, dpi = 200)
      cat(sprintf("Saved: %s\n", out_png))
    }

    # Plot 2: rolling 4Q RMSE comparison across specs at horizon=1
    rolling_rmse <- forecasts_long %>%
      filter(horizon == 1L, !is.na(forecast_dlcons), !is.na(actual_dlcons)) %>%
      arrange(spec_name, target_date) %>%
      group_by(spec_name) %>%
      mutate(error_sq = (actual_dlcons - forecast_dlcons)^2,
             rolling_rmse = sqrt(zoo::rollmean(error_sq, k = 8L,
                                                fill = NA, align = "right"))) %>%
      ungroup() %>%
      filter(!is.na(rolling_rmse))
    if (nrow(rolling_rmse) > 0L) {
      p2 <- ggplot(rolling_rmse,
                   aes(target_date, rolling_rmse, colour = spec_name)) +
        geom_line(linewidth = 0.6) +
        labs(title = "Rolling 8-quarter RMSE: 1-quarter-ahead forecasts",
             subtitle = "Lower is better",
             x = NULL, y = "Rolling RMSE", colour = NULL) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom")
      out_png2 <- file.path(output_dir, "australia_oos_rolling_rmse.png")
      ggsave(out_png2, p2, width = 11, height = 5, dpi = 200)
      cat(sprintf("Saved: %s\n", out_png2))
    }
  }

  invisible(list(forecasts = forecasts_long, rmse = rmse_tbl))
}


# ==============================================================================
# Pipeline integration: run on the live specs_full from estimation
# ==============================================================================

if (interactive()) {
  message("oos_forecast.R loaded (interactive mode)")
} else {
  # When sourced from australia_estimation.R as Step 22, the variables
  # `specs_full` and `model_data` exist in the parent environment. Use them.
  specs_full_local <- if (exists("specs_full")) specs_full else NULL
  model_data_local <- if (exists("model_data")) model_data else NULL

  if (is.null(specs_full_local) || is.null(model_data_local)) {
    # Standalone invocation: rebuild from RDS
    message("[oos_forecast] rebuilding specs from cached RDS for standalone run")
    master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))
    model_data_local <- master %>%
      rename(dlcons = d_ln_cons_pc,
             lincome = ln_ydi_real_pc,
             lcons   = ln_cons_real_pc) %>%
      mutate(ecm_lag = lag(lcons, 1L) - lincome)

    src <- readLines(file.path(project_root, "R", "australia_estimation.R"))
    guard_start <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
    guard_end   <- guard_start + 6L
    src_safe <- src[-(guard_start:guard_end)]
    main_start <- grep("\\[Step 1\\]", src_safe)[1L]
    eval(parse(text = paste(src_safe[1:(main_start - 1L)], collapse = "\n")),
         envir = environment())

    model_data_local <- add_model_variables(model_data_local)
    model_data_local <- compute_income_volatility(model_data_local)
    model_data_local <- construct_permanent_income(model_data_local)
    model_data_local <- model_data_local %>%
      mutate(ecm_lag = lag(lcons, 1L) - lincome)
    specs_full_local <- run_all_specifications(model_data_local)
  }

  # Validate the headline specs: the LIVES headline Spec 11, the conventional
  # baseline Spec 6, then 4 / 7 / 8 / 9. Spec 7b (sample-restricted) and
  # Spec 10/12 (calibrated offsets) excluded — they need special handling.
  # Attach the CCI interaction columns (and the combined illiquid-financial
  # ratio Spec 11 uses) so the CCI specs are actually evaluated (otherwise
  # they collapse to base — see attach_cci_oos_interactions).
  model_data_local <- attach_cci_oos_interactions(model_data_local)
  if (!"ilfa_y" %in% names(model_data_local) &&
      all(c("eq_y", "super_y") %in% names(model_data_local))) {
    model_data_local$ilfa_y <- model_data_local$eq_y + model_data_local$super_y
  }

  validate_keys <- intersect(c("spec11", "spec6", "spec4", "spec7", "spec8",
                               "spec9"),
                              names(specs_full_local))
  specs_for_oos <- specs_full_local[validate_keys]
  cat(sprintf("[oos_forecast] validating %d specs: %s\n",
              length(specs_for_oos),
              paste(sapply(specs_for_oos, `[[`, "spec_name"),
                    collapse = ", ")))

  run_oos_validation(specs_for_oos, model_data_local,
                      rolling_start = as.Date("2015-01-01"),
                      output_dir    = output_dir)
}
