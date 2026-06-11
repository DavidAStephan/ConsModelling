# ==============================================================================
# Williams CCI placebo test — EXTENDED SAMPLE (NS-020 back-extension)
# ==============================================================================
#
# Same logic as cci_placebo_test.R but on the 1976Q3+ back-extended sample
# using the aggregate-networth-proxy specification (Spec 1 with
# ln_networth_y_proxy as the wealth regressor) instead of the disaggregated
# Spec-4 setup. This lets us answer the question that the original 1988+
# placebo couldn't:
#
#   When the data covers the 1979 deregulation episode, do Williams'
#   canonical 4-knot dates (1979/1992/1998/2007) actually identify, or
#   do they still sit at the placebo median?
#
# Output:
#   outputs/australia_williams_knot_placebo_extended.csv
#   outputs/australia_williams_knot_placebo_extended.png
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260508L)
N_DRAWS <- 200L

PROJ <- "Australia"
output_dir <- file.path(PROJ, "outputs")

master <- readRDS(file.path(PROJ, "outputs", "australia_model_dataset.rds"))
source(file.path(PROJ, "R", "model_helpers.R"), local = TRUE)

# Build model_data and source helpers (without main exec)
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
  mutate(ecm_lag = lag(lcons, 1L) - lincome,
         # Alias the proxy as ln_networth_y for the spec template
         ln_networth_y_official = ln_networth_y,
         ln_networth_y          = ln_networth_y_proxy)

cat(sprintf("model_data: %d rows, ln_networth_y(=proxy) nn=%d, first=%s\n",
  nrow(model_data),
  sum(!is.na(model_data$ln_networth_y)),
  format(min(model_data$date[!is.na(model_data$ln_networth_y)]))))

# Spec-1-aggregate template (uses proxy as ln_networth_y on 1976Q3+ sample)
spec_template <- list(
  lr_vars    = c("ln_networth_y", "ln_hp_over_y", "real_rate",
                 "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

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
    fit_ecm_spec(md, "Placebo_Ext", full_lr,
                 spec_template$sr_vars, spec_template$dummy_vars,
                 sample_start = as.Date("1976-07-01"),
                 sample_end   = as.Date("2024-10-01")),
    error = function(e) NULL
  )
  if (is.null(spec)) return(list(adj_r2 = NA_real_, lambda = NA_real_,
                                  n_obs = NA_integer_, n_aliased = NA_integer_))
  cf <- coef(spec$fit)
  list(
    adj_r2    = summary(spec$fit)$adj.r.squared,
    lambda    = if ("ecm_lag" %in% names(cf)) cf[["ecm_lag"]] else NA_real_,
    n_obs     = nobs(spec$fit),
    n_aliased = sum(is.na(cf[cci_terms]))
  )
}

# Williams canonical 4-knot benchmark
canonical_knots <- c("1979-01-01", "1992-01-01", "1998-01-01", "2007-01-01")
cat(sprintf("\nFitting Williams canonical 4-knot benchmark (extended sample)...\n"))
canonical_fit <- fit_with_knot_set(canonical_knots)
cat(sprintf("  adj R^2 = %.4f, lambda = %.4f, n=%d, %d aliased of 4\n",
            canonical_fit$adj_r2, canonical_fit$lambda,
            canonical_fit$n_obs, canonical_fit$n_aliased))

# 200 random 4-knot draws uniformly in [1979, 2007] (same window as the
# 1988+ test, so percentile ranks are directly comparable)
window_start <- as.numeric(as.Date("1979-01-01"))
window_end   <- as.numeric(as.Date("2007-12-01"))
cat(sprintf("\nRunning %d placebo draws (uniform random knots in 1979-2007)...\n",
            N_DRAWS))

placebo_rows <- list()
pb_step <- max(1L, N_DRAWS %/% 10L)
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
write_csv(placebo_tbl, file.path(output_dir, "australia_williams_knot_placebo_extended.csv"))

# Verdict
finite_r2 <- placebo_tbl$adj_r2[is.finite(placebo_tbl$adj_r2)]
finite_la <- placebo_tbl$lambda[is.finite(placebo_tbl$lambda)]

w_r2_pct <- mean(finite_r2 < canonical_fit$adj_r2, na.rm = TRUE)
w_la_pct <- mean(abs(finite_la) < abs(canonical_fit$lambda), na.rm = TRUE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("PLACEBO TEST VERDICT — EXTENDED SAMPLE (1976Q3+, Spec 1 aggregate proxy)\n")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf("\nWilliams canonical 4-knot benchmark:\n"))
cat(sprintf("  adjusted R^2 = %.4f\n", canonical_fit$adj_r2))
cat(sprintf("  lambda       = %.4f\n", canonical_fit$lambda))
cat(sprintf("\nPlacebo distribution (%d draws):\n", length(finite_r2)))
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
  "DETRENDING CRITIQUE PERSISTS — Williams below median"
}
cat(sprintf("\n>>> VERDICT: %s\n\n", verdict))

# Plot
p1 <- ggplot(placebo_tbl, aes(x = adj_r2)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = canonical_fit$adj_r2, color = "red", size = 1) +
  annotate("text", x = canonical_fit$adj_r2, y = Inf,
           label = sprintf("Williams canonical\n(%.0fth pct)", 100 * w_r2_pct),
           vjust = 2, hjust = 1.05, color = "red") +
  labs(title = "Placebo: adj R^2 distribution (extended 1976Q3+ sample)",
       subtitle = sprintf("%d random 4-knot draws vs Williams canonical (Spec 1 aggregate proxy)",
                          length(finite_r2)),
       x = "Adjusted R^2", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(placebo_tbl, aes(x = abs(lambda))) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = abs(canonical_fit$lambda), color = "red", size = 1) +
  annotate("text", x = abs(canonical_fit$lambda), y = Inf,
           label = sprintf("Williams canonical\n(%.0fth pct)", 100 * w_la_pct),
           vjust = 2, hjust = 1.05, color = "red") +
  labs(title = "Placebo: |lambda| distribution (extended sample)",
       subtitle = sprintf("%d random 4-knot draws vs Williams canonical",
                          length(finite_la)),
       x = "|lambda|", y = "Frequency") +
  theme_minimal()

ggsave(file.path(output_dir, "australia_williams_knot_placebo_extended_r2.png"),
       p1, width = 10, height = 5, dpi = 120)
ggsave(file.path(output_dir, "australia_williams_knot_placebo_extended_lambda.png"),
       p2, width = 10, height = 5, dpi = 120)

# Summary CSV
summary_tbl <- tibble(
  metric = c("Williams adj R^2", "Williams |lambda|",
             "Williams adj R^2 percentile rank",
             "Williams |lambda| percentile rank",
             "Placebo n",
             "Placebo adj R^2 median",
             "Placebo |lambda| median",
             "Verdict"),
  value = c(sprintf("%.4f", canonical_fit$adj_r2),
            sprintf("%.4f", canonical_fit$lambda),
            sprintf("%.0fth", 100 * w_r2_pct),
            sprintf("%.0fth", 100 * w_la_pct),
            as.character(length(finite_r2)),
            sprintf("%.4f", median(finite_r2)),
            sprintf("%.4f", median(abs(finite_la))),
            verdict)
)
write_csv(summary_tbl,
          file.path(output_dir, "australia_williams_knot_placebo_extended_summary.csv"))

cat(sprintf("Saved: %s\n", file.path(output_dir, "australia_williams_knot_placebo_extended.csv")))
cat(sprintf("Saved: %s\n", file.path(output_dir, "australia_williams_knot_placebo_extended_summary.csv")))
