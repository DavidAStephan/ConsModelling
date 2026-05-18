#!/usr/bin/env Rscript
# ==============================================================================
# CCI fit-improvement decomposition (NS-108)
# ==============================================================================
#
# Question: when we add CCI to the consumption equation, does it
# (a) cause the wealth and rate coefficients to materially change
#     (genuine identification work — CCI is recovering omitted-variable
#      structural information), or
# (b) just raise R² by absorbing residual variance without shifting the
#     other coefficients (the user's 'detrending' critique)?
#
# Procedure:
#   1. Refit Spec 6 (preferred, no CCI) on the standard sample.
#   2. Refit Spec 8 (Williams CCI interactions) on the same sample.
#   3. For each long-run regressor, compute:
#        - Spec 6 coefficient
#        - Spec 8 coefficient
#        - Absolute and percentage shift
#   4. Compute the R² gain.
#
# Verdict (rule of thumb):
#   - Wealth-coef shifts < 5% → CCI is mostly detrending
#   - Wealth-coef shifts 5-30% → mixed (some identification)
#   - Wealth-coef shifts > 30% → CCI is doing genuine identification
#
# Output:
#   outputs/australia_cci_fit_decomposition.csv
#   outputs/australia_cci_fit_decomposition.md
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(tidyr)
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

# Read existing results — Spec 6 (no CCI) and Spec 8 (with CCI) are already
# fit in the live pipeline.
res  <- readr::read_csv(file.path(output_dir, "australia_full_results.csv"),
                        show_col_types = FALSE)
diag <- readr::read_csv(file.path(output_dir, "australia_full_diagnostics.csv"),
                        show_col_types = FALSE)

s6 <- res %>% filter(specification == "Spec6_Preferred")
s8 <- res %>% filter(specification == "Spec8_CCI_Interactions")
s9 <- res %>% filter(specification == "Spec9_KalmanCCI")

# Strip the CCI-interaction columns from Spec 8 / Spec 9 (they don't have
# Spec 6 counterparts); for the comparison we focus on the "shared" terms
# that exist in both with-CCI and without-CCI specs.
shared_terms <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                  "real_rate", "ln_yp_over_y", "ecm_lag")

build_decomp <- function(s_no_cci, s_with_cci, label) {
  df <- shared_terms %>%
    purrr::map_dfr(function(term_name) {
      r6 <- s_no_cci  %>% filter(term == term_name)
      r8 <- s_with_cci %>% filter(term == term_name)
      tibble::tibble(
        comparison      = label,
        term            = term_name,
        coef_no_cci     = if (nrow(r6) > 0L) r6$ols_estimate[1L] else NA_real_,
        coef_with_cci   = if (nrow(r8) > 0L) r8$ols_estimate[1L] else NA_real_,
        t_no_cci        = if (nrow(r6) > 0L) r6$t_stat[1L] else NA_real_,
        t_with_cci     = if (nrow(r8) > 0L) r8$t_stat[1L] else NA_real_
      )
    }) %>%
    mutate(
      diff_abs    = coef_with_cci - coef_no_cci,
      diff_pct    = ifelse(is.na(coef_no_cci) | abs(coef_no_cci) < 1e-10,
                            NA_real_,
                            100 * diff_abs / abs(coef_no_cci)),
      verdict     = case_when(
        is.na(diff_pct) ~ NA_character_,
        abs(diff_pct) < 5  ~ "DETRENDING (shift < 5%)",
        abs(diff_pct) < 30 ~ "MIXED (shift 5-30%)",
        TRUE               ~ "IDENTIFICATION (shift > 30%)"
      )
    )
  df
}

if (!requireNamespace("purrr", quietly = TRUE))
  install.packages("purrr", repos = "https://cloud.r-project.org/", quiet = TRUE)

decomp_8 <- build_decomp(s6, s8, "Spec6 vs Spec8 (Williams maximal-GETS CCI)")
decomp_9 <- build_decomp(s6, s9, "Spec6 vs Spec9 (Kalman state-space CCI)")
decomp <- bind_rows(decomp_8, decomp_9)

# R² gain per CCI method
r2_6 <- diag %>% filter(specification == "Spec6_Preferred") %>% pull(adj_r2)
r2_8 <- diag %>% filter(specification == "Spec8_CCI_Interactions") %>% pull(adj_r2)
r2_9 <- diag %>% filter(specification == "Spec9_KalmanCCI") %>% pull(adj_r2)
r2_gain_8 <- if (length(r2_6) > 0 && length(r2_8) > 0) r2_8 - r2_6 else NA_real_
r2_gain_9 <- if (length(r2_6) > 0 && length(r2_9) > 0) r2_9 - r2_6 else NA_real_

write.csv(decomp, file.path(output_dir, "australia_cci_fit_decomposition.csv"),
          row.names = FALSE)

cat("\n--- CCI fit decomposition (Spec 6 no CCI vs Specs 8/9 with CCI) ---\n")
print(decomp %>% select(comparison, term, coef_no_cci, coef_with_cci,
                        diff_pct, verdict),
      n = nrow(decomp))

cat(sprintf("\nadj R^2 — Spec 6 (no CCI):           %.4f\n", r2_6))
cat(sprintf("adj R^2 — Spec 8 (Williams CCI):     %.4f  (gain %+.4f)\n",
            r2_8, r2_gain_8))
cat(sprintf("adj R^2 — Spec 9 (Kalman CCI):       %.4f  (gain %+.4f)\n",
            r2_9, r2_gain_9))

# Headline verdict — average shift on the wealth coefficients only
wealth_terms <- c("nla_y", "eq_y", "super_y", "ha_y")
mean_shift_8 <- decomp_8 %>%
  filter(term %in% wealth_terms) %>%
  pull(diff_pct) %>% abs() %>% mean(na.rm = TRUE)
mean_shift_9 <- decomp_9 %>%
  filter(term %in% wealth_terms) %>%
  pull(diff_pct) %>% abs() %>% mean(na.rm = TRUE)

verdict_8 <- if (mean_shift_8 < 5) "DETRENDING" else if (mean_shift_8 < 30) "MIXED" else "IDENTIFICATION"
verdict_9 <- if (mean_shift_9 < 5) "DETRENDING" else if (mean_shift_9 < 30) "MIXED" else "IDENTIFICATION"

cat(sprintf("\nMean |%%-shift| on wealth coefs (nla/eq/super/ha):\n"))
cat(sprintf("  Spec 8 (Williams CCI): %.1f%% — %s\n", mean_shift_8, verdict_8))
cat(sprintf("  Spec 9 (Kalman CCI):   %.1f%% — %s\n", mean_shift_9, verdict_9))

# Markdown
md <- c(
  "# CCI fit-improvement decomposition",
  "",
  "Tests whether adding CCI to the consumption equation does identification",
  "work or just absorbs residual variance (the 'detrending' critique).",
  "",
  "## Procedure",
  "",
  "Compare wealth and rate coefficients in:",
  "- **Spec 6** (preferred, no CCI long-run interactions)",
  "- **Spec 8** (Williams maximal-GETS CCI interactions)",
  "- **Spec 9** (Kalman state-space CCI interactions)",
  "",
  "## Headline result",
  "",
  sprintf("- **adj R² Spec 6**: %.4f", r2_6),
  sprintf("- **adj R² Spec 8** (Williams CCI): %.4f (gain %+.4f)", r2_8, r2_gain_8),
  sprintf("- **adj R² Spec 9** (Kalman CCI):   %.4f (gain %+.4f)", r2_9, r2_gain_9),
  "",
  sprintf("- Mean |%%-shift| on wealth coefs from Spec 6 to Spec 8: **%.1f%%** — %s",
          mean_shift_8, verdict_8),
  sprintf("- Mean |%%-shift| on wealth coefs from Spec 6 to Spec 9: **%.1f%%** — %s",
          mean_shift_9, verdict_9),
  "",
  "## Verdict rule of thumb",
  "",
  "- < 5%  shift → CCI is mostly **DETRENDING** (residual absorption)",
  "- 5-30% shift → **MIXED** (some identification; some detrending)",
  "- > 30% shift → CCI is doing genuine **IDENTIFICATION**",
  "",
  "## Term-by-term decomposition",
  "",
  "### Spec 6 vs Spec 8 (Williams maximal-GETS)",
  "",
  "| Term | Coef (no CCI) | Coef (with CCI) | %-shift | Verdict |",
  "|------|--------------:|----------------:|--------:|---------|",
  paste0("| ", decomp_8$term, " | ",
         sprintf("%.4f", decomp_8$coef_no_cci), " | ",
         sprintf("%.4f", decomp_8$coef_with_cci), " | ",
         sprintf("%+.1f", decomp_8$diff_pct), "% | ",
         decomp_8$verdict, " |"),
  "",
  "### Spec 6 vs Spec 9 (Kalman state-space)",
  "",
  "| Term | Coef (no CCI) | Coef (with CCI) | %-shift | Verdict |",
  "|------|--------------:|----------------:|--------:|---------|",
  paste0("| ", decomp_9$term, " | ",
         sprintf("%.4f", decomp_9$coef_no_cci), " | ",
         sprintf("%.4f", decomp_9$coef_with_cci), " | ",
         sprintf("%+.1f", decomp_9$diff_pct), "% | ",
         decomp_9$verdict, " |"),
  "",
  "## Interpretation",
  "",
  paste0("[",
         "Williams CCI verdict: ", verdict_8, " ",
         "(", sprintf("%.1f", mean_shift_8), "%); ",
         "Kalman CCI verdict: ", verdict_9, " ",
         "(", sprintf("%.1f", mean_shift_9), "%)",
         "]"),
  "",
  "Generated by `Australia/R/cci_fit_decomposition.R`.",
  ""
)
writeLines(md, file.path(output_dir, "australia_cci_fit_decomposition.md"))
cat(sprintf("\nSaved: %s\n",
            file.path(output_dir, "australia_cci_fit_decomposition.md")))
