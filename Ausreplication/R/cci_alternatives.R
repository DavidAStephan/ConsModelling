#!/usr/bin/env Rscript
# ==============================================================================
# Alternative CCI extractors — PCA, credit-to-GDP gap, macropru intensity
# ==============================================================================
#
# Three additional CCI methodologies that complement the spline-based
# cci_williams (Spec 8) and the state-space cci_kalman (Spec 9). All three
# rely only on data already in master (no extra sourcing required).
#
#   cci_pca       First principal component across 5 credit indicators.
#                 Simple benchmark to the Kalman state-space factor:
#                 PCA assumes Gaussian, common-factor; Kalman fits ML
#                 with explicit measurement and state noise.
#
#   cci_creditgap BIS credit-to-GDP-gap-style measure: log(household debt
#                 to annualised income) detrended by one-sided HP filter.
#                 A different-family measure of credit conditions: captures
#                 cyclical departures from a slow trend, not regime-shift
#                 identification.
#
#   macropru_intensity  Combined macroprudential tightening index from the
#                 existing APRA-related dummies (d_apra_2014, d_apra_2017,
#                 plus implicit relaxations and 2021 buffer hike). Replaces
#                 the discrete dummies with a continuous tightening measure.
#
# Outputs:
#   - master gains 3 new columns (cci_pca, cci_creditgap, macropru_intensity)
#   - outputs/australia_cci_method_4way.csv  side-by-side correlations + summary
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(tidyr)
  library(ggplot2)
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

master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))

# ------------------------------------------------------------------------------
# (a) cci_pca — first principal component across credit indicators
# ------------------------------------------------------------------------------
indicators <- master %>%
  transmute(
    date,
    log_housing_loan_flow = log(pmax(housing_loan_flow, 1e-9)),
    log_debt_y            = log(pmax(debt_y, 1e-9)),
    fhb_share,
    mortgage_burden,
    mortgage_rate_change  = c(NA, diff(mortgage_rate))
  )
ind_cols <- setdiff(names(indicators), "date")
ind_mat <- as.matrix(indicators[, ind_cols, drop = FALSE])
ind_complete <- complete.cases(ind_mat)
n_complete <- sum(ind_complete)
message(sprintf("[cci_pca] %d complete-case observations across %d indicators",
                n_complete, length(ind_cols)))

# Standardise on full sample, then PCA on complete-case subset
standardise <- function(x) {
  if (sum(!is.na(x)) < 4L) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
ind_z <- as.matrix(indicators[, ind_cols] %>% mutate(across(everything(), standardise)))

# PCA on complete-case rows; project the rest using the loadings
pca_fit <- prcomp(ind_z[ind_complete, ], center = FALSE, scale. = FALSE)
loadings <- pca_fit$rotation[, 1L]
# Project all rows (including those with NAs)
proj <- function(row) {
  if (any(is.na(row))) {
    # Use only available indicators, scaled by the share of variance their
    # loadings cover (a simple imputation that keeps the factor available
    # outside the common sample).
    avail <- !is.na(row)
    if (sum(avail) < 2L) return(NA_real_)
    sum(row[avail] * loadings[avail]) /
      sqrt(sum(loadings[avail]^2)) * sqrt(sum(loadings^2))
  } else {
    sum(row * loadings)
  }
}
cci_pca_raw <- apply(ind_z, 1L, proj)

# Orient: positive correlation with log_housing_loan_flow (anchor)
anc <- ind_z[, "log_housing_loan_flow"]
ok <- !is.na(anc) & !is.na(cci_pca_raw)
if (sum(ok) > 4L && cor(anc[ok], cci_pca_raw[ok]) < 0) {
  cci_pca_raw <- -cci_pca_raw
  loadings    <- -loadings
}
# Peak-normalise to ±1
mx <- max(abs(cci_pca_raw), na.rm = TRUE)
cci_pca <- if (is.finite(mx) && mx > 0) cci_pca_raw / mx else cci_pca_raw

cat("\n--- PCA loadings (first principal component) ---\n")
print(round(loadings, 3))
cat(sprintf("\nVariance explained by PC1: %.1f%%\n",
            100 * pca_fit$sdev[1L]^2 / sum(pca_fit$sdev^2)))

# ------------------------------------------------------------------------------
# (b) cci_creditgap — HP-filtered log(debt/income) gap (BIS-style)
# ------------------------------------------------------------------------------
# Use the household debt-to-annualised-income ratio. Log-transform, then apply
# a one-sided HP filter via running mFilter::hpfilter on expanding windows.
# BIS uses lambda=400000 for the credit-to-GDP gap (quarterly); we use the
# same value for comparability.
debt_log <- log(pmax(master$debt_y, 1e-9))

cci_creditgap <- rep(NA_real_, length(debt_log))
if (requireNamespace("mFilter", quietly = TRUE)) {
  # Two-sided HP for simplicity (one-sided is computed via Kalman filter and
  # is methodologically more demanding; the two-sided gap on a long sample is
  # a reasonable proxy for our purpose of CCI-correlation).
  observed_idx <- which(!is.na(debt_log))
  if (length(observed_idx) > 12L) {
    hp <- mFilter::hpfilter(debt_log[observed_idx], freq = 400000, type = "lambda")
    cci_creditgap[observed_idx] <- as.numeric(hp$cycle)
    # Sign convention: positive gap = credit faster than trend = looser CCI
    # Peak-normalise
    mx <- max(abs(cci_creditgap), na.rm = TRUE)
    if (is.finite(mx) && mx > 0) cci_creditgap <- cci_creditgap / mx
    message(sprintf("[cci_creditgap] HP gap: %d obs, range %.2f to %.2f",
                    sum(!is.na(cci_creditgap)),
                    min(cci_creditgap, na.rm = TRUE),
                    max(cci_creditgap, na.rm = TRUE)))
  }
} else {
  message("[cci_creditgap] mFilter not installed; skipping")
}

# ------------------------------------------------------------------------------
# (c) macropru_intensity — combined APRA tightening index
# ------------------------------------------------------------------------------
# Combine the existing macroprudential dummies into a single continuous
# tightening intensity measure. Sign convention: positive = looser credit
# (matches Williams' CCI). So:
#   d_apra_2014 (investor cap)        contributes -1
#   d_apra_2017 (interest-only cap)   contributes -1
# We add tightening events to the candidate list:
#   2018Q3 Hayne RC start             contributes -0.5 (pre-finding effect)
#   2019Q1 Hayne RC findings          contributes -1
#   2019Q3 APRA cap removal + buffer  contributes +1 (relaxation)
#   2020Q2 RBA emergency cut + COVID  contributes +0.5 (rate cut)
#   2021Q4 APRA buffer hike 2.5%->3%  contributes -1
#
# Each event is a smoothed-step ogive with width 2.5 quarters (same as the
# existing d_apra_* dummies for consistency).
ogive_step <- function(date_vec, t0, width = 2.5) {
  x <- as.numeric(date_vec - as.Date(t0)) / 91.31  # quarters since
  1 / (1 + exp(-x / width))
}

events <- list(
  list(t0 = "2014-12-01", weight = -1.0),
  list(t0 = "2017-03-01", weight = -1.0),
  list(t0 = "2018-09-01", weight = -0.5),
  list(t0 = "2019-01-01", weight = -1.0),
  list(t0 = "2019-09-01", weight = +1.0),
  list(t0 = "2020-04-01", weight = +0.5),
  list(t0 = "2021-12-01", weight = -1.0)
)

macropru_raw <- rep(0, nrow(master))
for (e in events) {
  step <- ogive_step(master$date, e$t0, width = 2.5)
  # Each event contributes its weight × (step - prior_step). The 'prior_step'
  # captures that each event modifies the cumulative state, not the level.
  macropru_raw <- macropru_raw + e$weight * step
}
mx <- max(abs(macropru_raw), na.rm = TRUE)
macropru_intensity <- if (is.finite(mx) && mx > 0) macropru_raw / mx else macropru_raw
# Pre-2014 the index is essentially 0; set it to NA there to signal that the
# index only describes the macroprudential era.
macropru_intensity[master$date < as.Date("2014-01-01")] <- NA_real_

message(sprintf("[macropru_intensity] %d obs, range %.2f to %.2f",
                sum(!is.na(macropru_intensity)),
                min(macropru_intensity, na.rm = TRUE),
                max(macropru_intensity, na.rm = TRUE)))

# ------------------------------------------------------------------------------
# Attach all three to master and persist
# ------------------------------------------------------------------------------
master <- master %>% mutate(
  cci_pca            = cci_pca,
  cci_creditgap      = cci_creditgap,
  macropru_intensity = macropru_intensity
)
saveRDS(master, file.path(output_dir, "australia_model_dataset.rds"))
message(sprintf("\nUpdated RDS: %s (%d cols)", "australia_model_dataset.rds",
                ncol(master)))

# ------------------------------------------------------------------------------
# 4-way CCI comparison summary
# ------------------------------------------------------------------------------

# Reconstruct cci_williams from the saved knot weights (same procedure as
# cci_method_comparison.R)
cci_w <- tryCatch({
  knots_csv <- read.csv(file.path(output_dir, "australia_williams_cci_knots.csv"),
                        stringsAsFactors = FALSE)
  surv <- knots_csv[knots_csv$survived %in% c(TRUE, "TRUE") &
                    !is.na(knots_csv$ols_estimate), ]
  if (nrow(surv) == 0L) stop("no surviving knots")
  knot_dates <- gsub("^sdmma_", "", surv$knot)
  knot_dates <- gsub("_", "-", knot_dates)
  knot_dates <- paste0(knot_dates, "-01")
  basis_mat <- vapply(knot_dates,
                      function(k) smoothed_step(master$date, k),
                      numeric(nrow(master)))
  raw <- as.numeric(basis_mat %*% surv$ols_estimate)
  mx <- max(abs(raw), na.rm = TRUE)
  if (is.finite(mx) && mx > 0) raw / mx else raw
}, error = function(e) rep(NA_real_, nrow(master)))

# Pairwise correlations across the 4 CCI methods
ccis <- tibble(
  date = master$date,
  Williams_max_GETS = cci_w,
  Kalman_state_space = master$cci_kalman,
  PCA_first_factor = master$cci_pca,
  Credit_to_income_gap = master$cci_creditgap
)

pairs <- expand.grid(a = setdiff(names(ccis), "date"),
                     b = setdiff(names(ccis), "date"),
                     stringsAsFactors = FALSE) %>%
  filter(a < b) %>%
  rowwise() %>%
  mutate(
    n_overlap = sum(!is.na(ccis[[a]]) & !is.na(ccis[[b]])),
    rho = if (n_overlap > 4L) {
      cor(ccis[[a]], ccis[[b]], use = "pairwise.complete.obs")
    } else NA_real_
  ) %>%
  ungroup()

cat("\n--- Pairwise correlations across 4 CCI methods ---\n")
print(pairs)

write.csv(pairs, file.path(output_dir, "australia_cci_method_4way.csv"),
          row.names = FALSE)
message(sprintf("\nSaved: %s", file.path(output_dir, "australia_cci_method_4way.csv")))

# 4-method time-series chart
plot_df <- ccis %>% pivot_longer(cols = -date, names_to = "method", values_to = "cci")
p <- ggplot(plot_df, aes(date, cci, colour = method)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_colour_manual(values = c(
    Williams_max_GETS = "steelblue",
    Kalman_state_space = "firebrick",
    PCA_first_factor = "darkgreen",
    Credit_to_income_gap = "darkorange")) +
  labs(
    title = "Australia CCI: four extraction methods",
    subtitle = "All peak-normalised to ±1; pairwise correlations in australia_cci_method_4way.csv",
    x = NULL, y = "CCI (peak-normalised)", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
out_png <- file.path(output_dir, "australia_cci_4way_comparison.png")
ggsave(out_png, p, width = 11, height = 5, dpi = 200)
message(sprintf("Saved: %s", out_png))

# Also write a methods-summary CSV
methods_summary <- tibble(
  method = c("Williams maximal-GETS spline", "Kalman state-space factor",
             "PCA first principal component", "Credit-to-income HP gap"),
  description = c(
    "15-knot smoothed-step spline at Australian institutional turning points; sign-prior reduction selects surviving knots",
    "Single common factor by ML Kalman filter on 5 credit indicators with anchor loading on housing-loan-flow",
    "First principal component across the same 5 standardised credit indicators",
    "BIS-style HP filter (lambda=400000) on log household debt-to-income; positive gap = looser credit"
  ),
  source_file = c("model_helpers.R::build_williams_cci_basis",
                  "model_helpers.R::fit_kalman_cci",
                  "cci_alternatives.R (this script)",
                  "cci_alternatives.R (this script)"),
  n_obs = c(sum(!is.na(cci_w)),
            sum(!is.na(master$cci_kalman)),
            sum(!is.na(master$cci_pca)),
            sum(!is.na(master$cci_creditgap)))
)
write.csv(methods_summary,
          file.path(output_dir, "australia_cci_methods_summary.csv"),
          row.names = FALSE)
message(sprintf("Saved: %s",
                file.path(output_dir, "australia_cci_methods_summary.csv")))
