#!/usr/bin/env Rscript
# ==============================================================================
# Italy–Australia Consumption Model: Full Side-by-Side Comparison Table
# ==============================================================================
#
# Reads:
#   Ausreplication/data_raw/italy_results.csv   — Italy Table 1 (long format)
#   Ausreplication/outputs/australia_all_results.csv
#   Ausreplication/outputs/australia_all_diagnostics.csv
#
# Outputs:
#   Ausreplication/outputs/italy_australia_full_comparison.csv
#
# Table structure: one row per variable, columns interleaved as
#   Italy_S1_coef, Italy_S1_t, ..., Italy_S6_coef, Italy_S6_t,
#   Aus_S1_coef,   Aus_S1_t,   ..., Aus_S6_coef,   Aus_S6_t
#
# Sign convention note:
#   Italy reports structural parameters in the long-run equation for ln(c/y).
#   Australia reports structural_param = OLS_coef / lambda, where OLS_coef is
#   from the equation for ln(y/c). This means signs on long-run terms are
#   directly comparable (both represent the coefficient in the ln(c/y) equation).
#   However, ln_yp_over_y may differ in construction — see notes below.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# --- Paths ---
# Resolve root relative to this script's location, with fallback for Rscript invocation
root_dir <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), winslash = "/"),
  error = function(e) normalizePath("Ausreplication", winslash = "/")
)
italy_path    <- file.path(root_dir, "data_raw", "italy_results.csv")
aus_coef_path <- file.path(root_dir, "outputs",   "australia_all_results.csv")
aus_diag_path <- file.path(root_dir, "outputs",   "australia_all_diagnostics.csv")
output_path   <- file.path(root_dir, "outputs",   "italy_australia_full_comparison.csv")

if (!file.exists(italy_path))    stop("Italy results not found: ", italy_path)
if (!file.exists(aus_coef_path)) stop("Australia results not found: ", aus_coef_path)
if (!file.exists(aus_diag_path)) stop("Australia diagnostics not found: ", aus_diag_path)

# ==============================================================================
# 1. ITALY TABLE
# ==============================================================================

italy_raw <- read.csv(italy_path, stringsAsFactors = FALSE)

# --- 1a. Parameters (long-run and short-run) ---
italy_params_wide <- italy_raw %>%
  filter(section %in% c("long_run", "short_run"), stat %in% c("coef", "t")) %>%
  pivot_wider(
    names_from  = c(column, stat),
    values_from = value,
    names_glue  = "Italy_S{column}_{stat}"
  ) %>%
  select(variable, symbol, section,
         matches("^Italy_S[1-6]_coef$"), matches("^Italy_S[1-6]_t$")) %>%
  # Interleave coef/t columns by spec number
  select(variable, symbol, section,
         as.vector(rbind(
           paste0("Italy_S", 1:6, "_coef"),
           paste0("Italy_S", 1:6, "_t")
         )))

# --- 1b. Diagnostics ---
italy_diag_wide <- italy_raw %>%
  filter(section == "diagnostics", stat == "value") %>%
  pivot_wider(
    names_from  = column,
    values_from = value,
    names_glue  = "Italy_S{column}_coef"
  ) %>%
  select(variable, symbol, section, paste0("Italy_S", 1:6, "_coef")) %>%
  mutate(across(paste0("Italy_S", 1:6, "_coef"), ~ round(.x, 4)))

# ==============================================================================
# 2. AUSTRALIA TABLE
# ==============================================================================

# Spec name → column number
spec_num_map <- c(
  Spec1_LogNetWorth     = 1L,
  Spec2_LogNetWorth_CCI = 2L,
  Spec3_LevelNetWorth   = 3L,
  Spec4_Disagg_NoCCI    = 4L,
  Spec5_FullDisagg      = 5L,
  Spec6_Preferred       = 6L
)

aus_all <- read.csv(aus_coef_path, stringsAsFactors = FALSE) %>%
  filter(period == "full") %>%
  mutate(spec_num = spec_num_map[specification])

# --- 2a. Variable mapping: Australia term → Italy row keys ---
#
# Each entry: aus_term = c(symbol = "...", variable = "...", section = "...")
# For long-run terms, value = structural_param (= OLS / lambda)
# For lambda, value = lambda (the raw OLS coef on ln_y_over_c)
# For short-run terms, value = ols_estimate

row_defs <- list(
  # Long-run
  list(aus_term = "ln_y_over_c",          symbol = "lambda",           section = "long_run",  variable = "Speed of adjustment",           use = "lambda"),
  list(aus_term = "(Intercept)",           symbol = "alpha0",           section = "long_run",  variable = "Constant",                       use = "structural"),
  list(aus_term = "real_rate",             symbol = "alpha1",           section = "long_run",  variable = "Adjusted r_t",                   use = "structural"),
  list(aus_term = "ln_yp_over_y",          symbol = "alpha2",           section = "long_run",  variable = "ln(y_p/y)",                      use = "structural"),
  list(aus_term = "ln_yp_over_y_post2008", symbol = "alpha2_post2008",  section = "long_run",  variable = "ln(y_p/y) x post-2008",          use = "structural"),
  list(aus_term = "ln_networth_y",         symbol = "gamma",            section = "long_run",  variable = "ln(A_{t-1}/y_t)",                use = "structural"),
  list(aus_term = "networth_y",            symbol = "gamma_netw",       section = "long_run",  variable = "A_{t-1}/y_t",                    use = "structural"),
  list(aus_term = "nla_y",                 symbol = "gamma_1_nla",      section = "long_run",  variable = "NLA_{t-1}/y_t",                  use = "structural"),
  list(aus_term = "eq_y",                  symbol = "gamma_eq",         section = "long_run",  variable = "Equities/income [Australia]",     use = "structural"),
  list(aus_term = "super_y",               symbol = "gamma_super",      section = "long_run",  variable = "Superannuation/income [Australia]", use = "structural"),
  list(aus_term = "ha_y",                  symbol = "gamma2",           section = "long_run",  variable = "HA_{t-1}/y_t",                   use = "structural"),
  list(aus_term = "ln_hp_over_y",          symbol = "gamma3",           section = "long_run",  variable = "ln HP_{t-1}/y_{t-1}",            use = "structural"),
  # Short-run
  list(aus_term = "d2_logcci_lag2",        symbol = "beta0",            section = "short_run", variable = "Δ2 ln CC_{t-2}",                 use = "ols"),
  list(aus_term = "dd4_income",            symbol = "beta2",            section = "short_run", variable = "ΔΔ4 ln y_t",                     use = "ols"),
  list(aus_term = "d2_log_unemp",          symbol = "beta3",            section = "short_run", variable = "Δ2 ln ur_t",                     use = "ols"),
  list(aus_term = "abs_income_resid",      symbol = "beta4",            section = "short_run", variable = "|ê_{t-1}|",                      use = "ols")
)

# Build one row per definition
aus_rows <- lapply(row_defs, function(rd) {
  sub <- aus_all %>% filter(term == rd$aus_term)
  if (nrow(sub) == 0L) return(NULL)

  sub <- sub %>%
    mutate(
      coef_val = case_when(
        rd$use == "lambda"     ~ lambda,
        rd$use == "structural" ~ structural_param,
        rd$use == "ols"        ~ ols_estimate
      ),
      t_val = t_stat
    ) %>%
    select(spec_num, coef_val, t_val)

  # Pivot wide
  coef_wide <- sub %>%
    select(spec_num, coef_val) %>%
    pivot_wider(names_from = spec_num, values_from = coef_val,
                names_glue = "Aus_S{spec_num}_coef")
  t_wide <- sub %>%
    select(spec_num, t_val) %>%
    pivot_wider(names_from = spec_num, values_from = t_val,
                names_glue = "Aus_S{spec_num}_t")

  bind_cols(
    tibble(variable = rd$variable, symbol = rd$symbol, section = rd$section),
    coef_wide,
    t_wide
  )
})

aus_params_wide <- bind_rows(aus_rows) %>%
  select(variable, symbol, section,
         as.vector(rbind(
           paste0("Aus_S", 1:6, "_coef"),
           paste0("Aus_S", 1:6, "_t")
         )))

# --- 2b. Australia Diagnostics ---
# Diagnostics CSV uses abbreviated spec names (spec1..spec6), not full names
spec_num_map_diag <- c(spec1 = 1L, spec2 = 2L, spec3 = 3L,
                       spec4 = 4L, spec5 = 5L, spec6 = 6L)
aus_diag_raw <- read.csv(aus_diag_path, stringsAsFactors = FALSE) %>%
  filter(period == "full") %>%
  mutate(spec_num = spec_num_map_diag[specification]) %>%
  filter(!is.na(spec_num))

diag_rows <- list(
  list(label = "Standard error x100", col = "se_pct"),
  list(label = "Adjusted R2",         col = "adj_r2"),
  list(label = "LM Het p-value",      col = "lm_het_pval"),
  list(label = "Durbin-Watson",       col = "dw"),
  list(label = "AR1/MA1 p-value",     col = "ar1_pval"),
  list(label = "AR4/MA4 p-value",     col = "ar4_pval"),
  list(label = "Chow p-value",        col = "chow_pval"),
  list(label = "RESET2 p-value",      col = "reset_pval"),
  list(label = "Schwarz Criterion",   col = "schwarz"),
  list(label = "Log likelihood",      col = "loglik")
)

aus_diag_wide <- lapply(diag_rows, function(dr) {
  sub <- aus_diag_raw %>%
    select(spec_num, value = !!dr$col) %>%
    distinct(spec_num, .keep_all = TRUE) %>%
    pivot_wider(names_from = spec_num, values_from = value,
                names_glue = "Aus_S{spec_num}_coef")
  bind_cols(tibble(variable = dr$label, symbol = "", section = "diagnostics"), sub)
}) %>%
  bind_rows()

# ==============================================================================
# 3. COMBINE ITALY + AUSTRALIA
# ==============================================================================

# Section order
sec_order <- c("long_run", "short_run", "diagnostics")

combined_params <- full_join(
  italy_params_wide,
  aus_params_wide,
  by = c("variable", "symbol", "section")
)

combined_diag <- full_join(
  italy_diag_wide,
  aus_diag_wide,
  by = c("variable", "symbol", "section")
)

final_table <- bind_rows(combined_params, combined_diag) %>%
  mutate(section = factor(section, levels = sec_order)) %>%
  arrange(section, match(symbol, c(
    # long-run order
    "lambda", "alpha0", "alpha1", "alpha2", "alpha2_post2008",
    "gamma", "gamma_netw",
    "gamma_1_nfa", "gamma_1_nla", "gamma_1_smlb", "gamma_1_ilapen",
    "gamma_eq", "gamma_super",
    "gamma2", "gamma3",
    # short-run order
    "beta0", "beta1", "beta2", "beta3", "beta4",
    "beta5", "beta6", "beta7"
  ))) %>%
  mutate(section = as.character(section))

# Round numeric columns
num_cols <- names(final_table)[sapply(final_table, is.numeric)]
final_table <- final_table %>%
  mutate(across(all_of(num_cols), ~ round(.x, 4)))

# ==============================================================================
# 4. PRINT CONSOLE SUMMARY
# ==============================================================================

fmt_val <- function(x) {
  if (is.na(x)) return(sprintf("%8s", "—"))
  sprintf("%8.3f", x)
}

cat("\n")
cat("==========================================================================\n")
cat("Italy vs Australia Consumption Model — Full Comparison\n")
cat("==========================================================================\n")

sections <- list(
  list(key = "long_run",  label = "LONG-RUN STRUCTURAL PARAMETERS"),
  list(key = "short_run", label = "SHORT-RUN PARAMETERS"),
  list(key = "diagnostics", label = "DIAGNOSTICS")
)

for (sec in sections) {
  rows <- final_table %>% filter(section == sec$key)
  if (nrow(rows) == 0L) next
  cat(sprintf("\n%s\n", sec$label))
  cat(strrep("-", 78), "\n")

  if (sec$key != "diagnostics") {
    # Header: Italy S1..S6, Australia S1..S6
    cat(sprintf("  %-34s  ", ""))
    cat("       Italy                                     Australia\n")
    cat(sprintf("  %-34s  ", "Variable"))
    for (s in 1:6) cat(sprintf("   S%d     ", s))
    cat("\n")
    cat(strrep("-", 78), "\n")

    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      cat(sprintf("  %-34s  [Italy] ", r$variable))
      for (s in 1:6) {
        cv <- r[[paste0("Italy_S", s, "_coef")]]
        tv <- r[[paste0("Italy_S", s, "_t")]]
        if (is.na(cv)) cat("    —     ") else cat(sprintf("% 5.3f", cv), "  ")
      }
      cat("\n")
      cat(sprintf("  %-34s  [Aus]   ", ""))
      for (s in 1:6) {
        cv <- r[[paste0("Aus_S", s, "_coef")]]
        if (is.na(cv)) cat("    —     ") else cat(sprintf("% 5.3f", cv), "  ")
      }
      cat("\n")
      # t-stats (combined row)
      it_ts <- sapply(1:6, function(s) r[[paste0("Italy_S", s, "_t")]])
      au_ts <- sapply(1:6, function(s) r[[paste0("Aus_S",   s, "_t")]])
      if (any(!is.na(it_ts)) || any(!is.na(au_ts))) {
        cat(sprintf("  %-34s  [t-It] ", ""))
        for (s in 1:6) {
          tv <- it_ts[s]
          if (is.na(tv)) cat("   (—)    ") else cat(sprintf("(% 4.2f)  ", tv))
        }
        cat("\n")
        cat(sprintf("  %-34s  [t-Au] ", ""))
        for (s in 1:6) {
          tv <- au_ts[s]
          if (is.na(tv)) cat("   (—)    ") else cat(sprintf("(% 4.2f)  ", tv))
        }
        cat("\n")
      }
    }
  } else {
    # Diagnostics: single value per spec
    cat(sprintf("  %-30s  %s\n", "Statistic",
                paste(sprintf("   S%d   ", 1:6), collapse = "")))
    cat(strrep("-", 78), "\n")
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      cat(sprintf("  %-30s  ", r$variable))
      for (s in 1:6) {
        it_v <- r[[paste0("Italy_S", s, "_coef")]]
        au_v <- r[[paste0("Aus_S",   s, "_coef")]]
        it_str <- if (is.na(it_v)) "   —   " else sprintf("% 6.3f", it_v)
        au_str <- if (is.na(au_v)) "   —   " else sprintf("% 6.3f", au_v)
        cat(sprintf("[I]%s [A]%s  ", it_str, au_str))
      }
      cat("\n")
    }
  }
}

cat("\n")
cat("Notes:\n")
cat("  Structural parameters = OLS coefficient / lambda (speed of adjustment).\n")
cat("  Italy specs 1-6 match Australia Spec1-Spec6 by construction type.\n")
cat("  eq_y and super_y (Australia) together correspond to ILFA in Italy;\n")
cat("  no separate bonds series exists in Australia.\n")
cat("  Italy NLA_{t-1}/y_t ≈ Australia nla_y = (deposits - loans) / income.\n")
cat("  Italy NFA_{t-1}/y_t (spec 4) is broader (all fin assets net of debt).\n")
cat("  [I] = Italy value, [A] = Australia value.\n")

# ==============================================================================
# 5. SAVE
# ==============================================================================

# Flatten any list columns (shouldn't remain after fixes, but guard against it)
final_table <- final_table %>%
  mutate(across(where(is.list), ~ sapply(.x, function(v) if (length(v) == 1L) v[[1L]] else NA_real_)))

write.csv(final_table, output_path, row.names = FALSE, na = "")
message("\nSaved: ", output_path)
