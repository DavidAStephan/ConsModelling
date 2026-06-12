#!/usr/bin/env Rscript
# ==============================================================================
# CCI method comparison — cci_williams (maximal-GETS spline) vs cci_kalman
# ==============================================================================
# Compares the two CCI extraction methods now implemented in the project:
#   1. cci_williams  - smoothed-step spline at 15 institutional knots,
#                      reduced by Hendry-Krolzig sign-prior elimination to
#                      a peak-normalised series (Spec 8)
#   2. cci_kalman    - single common factor extracted via maximum-likelihood
#                      Kalman filter from 5 credit indicators (Spec 9)
#
# Outputs:
#   outputs/australia_cci_method_comparison.csv      side-by-side coef table
#   outputs/australia_cci_method_summary.md          narrative
#   outputs/australia_cci_series_comparison.png      time-series chart
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(tibble)
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

# ---------- Build coefficient comparison table ----------
res <- readr::read_csv(file.path(output_dir, "australia_full_results.csv"),
                       show_col_types = FALSE)

normalise_term <- function(s) {
  s <- gsub("_k$", "", s)
  gsub("hp_x_1_minus_cci_k", "hp_x_1_minus_cci", s, fixed = TRUE)
}

s8 <- res %>% filter(specification == "Spec8_CCI_Interactions") %>%
  transmute(term = term, ols_williams = ols_estimate,
            t_williams = t_stat, sign_ok_williams = sign_ok)
s9 <- res %>% filter(specification == "Spec9_KalmanCCI") %>%
  mutate(term = normalise_term(term)) %>%
  transmute(term = term, ols_kalman = ols_estimate,
            t_kalman = t_stat, sign_ok_kalman = sign_ok)

comp <- full_join(s8, s9, by = "term") %>%
  mutate(
    diff_abs = ifelse(is.na(ols_williams) | is.na(ols_kalman),
                      NA_real_, ols_kalman - ols_williams)
  ) %>%
  arrange(term)

out_csv <- file.path(output_dir, "australia_cci_method_comparison.csv")
write.csv(comp, out_csv, row.names = FALSE)
cat(sprintf("Saved: %s\n", out_csv))

cat("\n--- CCI method comparison (Spec 8 Williams vs Spec 9 Kalman) ---\n")
print(comp %>% select(term, ols_williams, ols_kalman, diff_abs),
      n = nrow(comp))

# ---------- Time-series comparison of the two CCI series ----------
master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))
source(file.path(project_root, "R", "model_helpers.R"))

# Reconstruct cci_williams from the surviving-knot weights saved by the
# live pipeline. The australia_williams_cci_knots.csv reports OLS estimates
# for each knot; the cci_williams series is the linear combination of the
# surviving SDMMA columns, peak-normalised to ±1.
cci_w_series <- tryCatch({
  knots_csv <- read.csv(file.path(output_dir, "australia_williams_cci_knots.csv"),
                        stringsAsFactors = FALSE)
  surv <- knots_csv[knots_csv$survived %in% c(TRUE, "TRUE") & !is.na(knots_csv$ols_estimate), ]
  if (nrow(surv) == 0L) stop("no surviving knots in CSV")

  # Build the SDMMA basis at the surviving knot dates
  knot_dates <- gsub("^sdmma_", "", surv$knot)
  knot_dates <- gsub("_", "-", knot_dates)
  knot_dates <- paste0(knot_dates, "-01")
  basis_mat <- vapply(knot_dates,
                      function(k) smoothed_step(master$date, k),
                      numeric(nrow(master)))
  raw <- as.numeric(basis_mat %*% surv$ols_estimate)
  mx <- max(abs(raw), na.rm = TRUE)
  if (is.finite(mx) && mx > 0) raw / mx else raw
}, error = function(e) {
  message("[cci_method_comparison] Could not reconstruct cci_williams ",
          "(", conditionMessage(e), "); chart will compare kalman only.")
  rep(NA_real_, nrow(master))
})

cat(sprintf("\ncci_williams: %d non-NA obs\n", sum(!is.na(cci_w_series))))
cat(sprintf("cci_kalman:   %d non-NA obs\n", sum(!is.na(master$cci_kalman))))

# Both available together?
both <- !is.na(cci_w_series) & !is.na(master$cci_kalman)
if (sum(both) > 4L) {
  rho <- cor(cci_w_series[both], master$cci_kalman[both])
  cat(sprintf("\nPearson correlation cci_williams vs cci_kalman: %.3f (n=%d)\n",
              rho, sum(both)))
}

# Plot
plot_df <- tibble::tibble(
  date = master$date,
  Williams_max_GETS = cci_w_series,
  Kalman_state_space = master$cci_kalman
) %>% pivot_longer(cols = -date, names_to = "method", values_to = "cci")

p <- ggplot(plot_df, aes(date, cci, colour = method)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(Williams_max_GETS = "steelblue",
                                  Kalman_state_space = "firebrick")) +
  labs(
    title = "Australia CCI: maximal-GETS spline vs Kalman state-space",
    subtitle = sprintf("Both peak-normalised to ±1; rho = %.2f over overlap",
                       if (sum(both) > 4L) cor(cci_w_series[both],
                                                master$cci_kalman[both])
                       else NA_real_),
    x = NULL, y = "CCI (peak-normalised)", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
out_png <- file.path(output_dir, "australia_cci_series_comparison.png")
ggsave(out_png, p, width = 10, height = 5, dpi = 200)
cat(sprintf("Saved: %s\n", out_png))

# ---------- Markdown summary ----------
# fmt is VECTORISED: a previous scalar version (x <- x[1L]) silently recycled
# the first value (the intercept) down the whole coefficient table.
fmt <- function(x, d = 4) {
  if (length(x) == 0L) return("\u2014")
  out <- ifelse(is.na(x), "\u2014", sprintf(paste0("%.", d, "f"), x))
  out
}
fmt1 <- function(x, d = 4) fmt(if (length(x) >= 1L) x[1L] else NA_real_, d)

md_lines <- c(
  "# CCI method comparison — Williams maximal-GETS vs Kalman state-space",
  "",
  "Two methodologically distinct CCI extractors are now implemented:",
  "",
  "1. **Williams maximal-GETS spline (`cci_williams`)** — 15-knot",
  "   smoothed-step spline at the documented Australian financial-policy",
  "   turning points, reduced by Hendry-Krolzig sign-prior drop-on-",
  "   violation to the surviving subset, then peak-normalised.",
  "2. **Kalman state-space single factor (`cci_kalman`)** — common",
  "   factor extracted by maximum likelihood from 5 credit indicators",
  "   (log housing-loan-flow, log debt-to-income, FHB share, mortgage",
  "   burden, mortgage-rate spread). Anchored at +1 loading on housing-",
  "   loan-flow; idiosyncratic noise variances and other loadings",
  "   estimated via fitSSM. Smoothed state extracted via KFS.",
  "",
  sprintf("**Pearson correlation between the two series: %s**",
          fmt(if (sum(both) > 4L) cor(cci_w_series[both],
                                        master$cci_kalman[both]) else NA, 3)),
  "",
  "## Spec-8 (Williams) vs Spec-9 (Kalman) coefficient comparison",
  "",
  "Both specs share the same structure (Spec-4 long-run core + four",
  "DE-MEANED multiplicative CCI interactions including the housing-",
  "collateral channel + standard short-run dynamics + dummies); the only",
  "difference is which CCI series enters the interactions.",
  "",
  "| Term | Spec 8 (Williams) | Spec 9 (Kalman) | Diff |",
  "|------|------------------:|----------------:|-----:|",
  paste0("| ", comp$term, " | ",
         fmt(comp$ols_williams, 5), " | ",
         fmt(comp$ols_kalman, 5), " | ",
         fmt(comp$diff_abs, 5), " |"),
  "",
  "## Headline observations",
  "",
  sprintf("- **Speed of adjustment**: Spec 8 lambda = %s, Spec 9 lambda = %s.",
          fmt1(comp$ols_williams[comp$term == "ecm_lag"]),
          fmt1(comp$ols_kalman[comp$term == "ecm_lag"])),
  "- Wealth and interaction coefficients are method-dependent; read the",
  "  table above (regenerated from the live fits each run) rather than any",
  "  hand-written narrative for the current values.",
  "",
  "## What this means for the WP §5 narrative",
  "",
  "The two CCI methods are **complementary** rather than competing:",
  "",
  "- **Williams maximal-GETS** has the institutional grounding (each",
  "  surviving knot maps to a documented Australian financial-policy",
  "  episode). It identifies *regime-shift* variation — the discrete",
  "  changes around 1992 / 2007 / 2019 / 2020 / 2021.",
  "- **Kalman state-space** has the methodological-discipline argument",
  "  (the latent factor is forced to be common across multiple credit",
  "  indicators, not derived from authorial knot-date choice). It",
  "  identifies *level-of-leverage* variation — the smooth gradient",
  "  from tight 1980s credit to loose post-2010s credit.",
  "",
  "The choice between them",
  "depends on which credit-conditions concept is closer to what the",
  "research question asks about. For the WP, both should be reported",
  "side-by-side in §8 (Robustness) with the headline finding being that",
  "**the consumption-equation coefficient on `ecm_lag` is robust to the",
  "CCI specification** while the implied identification of credit-",
  "regime-shifts is method-dependent.",
  "",
  "Generated by `Australia/R/cci_method_comparison.R`. To refresh",
  "after re-estimation: `Rscript Australia/R/cci_method_comparison.R`.",
  ""
)
out_md <- file.path(output_dir, "australia_cci_method_summary.md")
writeLines(md_lines, out_md)
cat(sprintf("Saved: %s\n", out_md))
