# ==============================================================================
# Maximal-GETS placebo on the EXTENDED sample
# ==============================================================================
#
# Companion to cci_placebo_extended.R. The earlier test asked whether the
# literal 4-knot Williams CCI (1979/1992/1998/2007) sits in the upper tail
# of random-4-knot draws on the back-extended sample. Verdict was "no" —
# Williams falls at the 19th/10th percentile, a clearer failure than the
# 1988+ result.
#
# This test asks the parallel question for the maximal-GETS protocol:
#
#   The canonical CCI now uses 15 candidate knot dates + 15 institutional
#   sign priors, with sign-violators dropped and the surviving combination
#   defining cci_williams. Does this protocol give Williams' SPECIFIC
#   choice of knots and priors an advantage over random combinations
#   following the same protocol?
#
# Null: 15 random knots (uniform in 1979–2021, the maximal-GETS window)
# plus 15 random sign priors (each ±1 with 50/50 probability), applying
# the same drop-on-violation reduction.
#
# If the canonical sits in the upper tail (>75th pct) on either metric,
# the maximal-GETS protocol is doing genuine identification work that
# random combinations can't match. If the canonical sits at the median
# (or worse), the protocol is mostly extracting flexibility from the
# candidate set rather than identifying institutional events.
#
# Output:
#   outputs/australia_williams_knot_placebo_maximal_extended.csv
#   outputs/australia_williams_knot_placebo_maximal_extended_summary.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
  library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260508L)
N_DRAWS  <- 200L
N_KNOTS  <- 15L
WINDOW_LO <- as.Date("1979-01-01")
WINDOW_HI <- as.Date("2021-12-01")

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
         ln_hp_over_y = log(hpi / exp(lincome) * (cons_deflator_norm / 100)),
         ln_networth_y_official = ln_networth_y,
         ln_networth_y          = ln_networth_y_proxy)

# Spec 1 aggregate (extended-sample compatible)
spec_template <- list(
  lr_vars    = c("ln_networth_y", "ln_hp_over_y", "real_rate",
                 "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

# --------------------------------------------------------------------------
# Maximal-GETS protocol: fit with all knots, drop sign-violators, refit.
# Returns the fitted spec's adj R^2, lambda, and surviving knot count.
# --------------------------------------------------------------------------
fit_maximal_gets <- function(knot_dates, sign_priors) {
  md <- model_data
  if (any(grepl("^sdmma_", names(md)))) {
    md <- md[, !grepl("^sdmma_", names(md))]
  }
  basis <- vapply(knot_dates, function(k) smoothed_step(md$date, k),
                  numeric(nrow(md)))
  cci_terms <- paste0("sdmma_", gsub("-", "_", substr(knot_dates, 1, 7)))
  # Ensure unique column names (random draws can collide on month-precision)
  cci_terms <- make.unique(cci_terms)
  colnames(basis) <- cci_terms
  for (j in seq_along(cci_terms)) md[[cci_terms[j]]] <- basis[, j]

  # First fit: all candidate knots
  full_lr <- c(spec_template$lr_vars, cci_terms)
  spec1 <- tryCatch(
    fit_ecm_spec(md, "MaximalGETS_Pl", full_lr,
                 spec_template$sr_vars, spec_template$dummy_vars,
                 sample_start = as.Date("1976-07-01"),
                 sample_end   = as.Date("2024-10-01")),
    error = function(e) NULL
  )
  if (is.null(spec1)) return(list(adj_r2 = NA_real_, lambda = NA_real_,
                                   n_obs = NA_integer_,
                                   n_surviving = NA_integer_,
                                   n_aliased = NA_integer_,
                                   n_violators = NA_integer_))

  # Identify sign-violators (per Hendry-Krolzig)
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
        fit_ecm_spec(md, "MaximalGETS_Pl", full_lr2,
                     spec_template$sr_vars, spec_template$dummy_vars,
                     sample_start = as.Date("1976-07-01"),
                     sample_end   = as.Date("2024-10-01")),
        error = function(e) spec1
      )
    } else {
      spec2 <- spec1
    }
  }

  cf2 <- coef(spec2$fit)
  candidate_surv <- intersect(cci_terms, names(cf2))
  surv_mask <- !is.na(cf2[candidate_surv])
  surviving <- candidate_surv[surv_mask]
  aliased   <- candidate_surv[!surv_mask]

  list(
    adj_r2     = summary(spec2$fit)$adj.r.squared,
    lambda     = if ("ecm_lag" %in% names(cf2)) cf2[["ecm_lag"]] else NA_real_,
    n_obs      = nobs(spec2$fit),
    n_surviving = length(surviving),
    n_aliased  = length(aliased),
    n_violators = length(violators)
  )
}

# --------------------------------------------------------------------------
# Canonical maximal-GETS benchmark (Williams' specific institutional choice)
# --------------------------------------------------------------------------
canonical_basis <- build_williams_cci_basis(model_data$date)
canonical_knots   <- attr(canonical_basis, "knots")
canonical_priors  <- attr(canonical_basis, "sign_priors")

cat(sprintf("\nCanonical maximal-GETS: %d knots in [%s, %s]\n",
            length(canonical_knots), min(canonical_knots), max(canonical_knots)))
cat("Knots:", paste(canonical_knots, collapse = ", "), "\n")
cat("Priors:", paste(canonical_priors, collapse = ","), "\n")

cat("\nFitting canonical maximal-GETS benchmark on extended sample...\n")
canonical_fit <- fit_maximal_gets(canonical_knots, canonical_priors)
cat(sprintf("  adj R^2 = %.4f, lambda = %.4f, n=%d\n",
            canonical_fit$adj_r2, canonical_fit$lambda, canonical_fit$n_obs))
cat(sprintf("  surviving knots = %d, aliased = %d, sign-violators = %d\n",
            canonical_fit$n_surviving, canonical_fit$n_aliased,
            canonical_fit$n_violators))

# --------------------------------------------------------------------------
# Placebo: N_DRAWS draws of (15 random knots + 15 random ±1 priors)
# --------------------------------------------------------------------------
window_lo_n <- as.numeric(WINDOW_LO)
window_hi_n <- as.numeric(WINDOW_HI)
cat(sprintf("\nRunning %d placebo draws (%d knots each, uniform in %s..%s, random ±1 priors)...\n",
            N_DRAWS, N_KNOTS, format(WINDOW_LO), format(WINDOW_HI)))

placebo_rows <- list()
pb_step <- max(1L, N_DRAWS %/% 10L)
for (i in seq_len(N_DRAWS)) {
  if (i %% pb_step == 0L) cat(sprintf("  draw %d / %d\n", i, N_DRAWS))
  random_dates  <- sort(as.Date(round(runif(N_KNOTS, window_lo_n, window_hi_n))))
  random_priors <- sample(c(-1L, 1L), N_KNOTS, replace = TRUE)
  res <- fit_maximal_gets(as.character(random_dates), random_priors)
  placebo_rows[[i]] <- tibble::tibble(
    draw        = i,
    knots       = paste(format(random_dates), collapse = ", "),
    priors      = paste(random_priors, collapse = ","),
    adj_r2      = res$adj_r2,
    lambda      = res$lambda,
    n_obs       = res$n_obs,
    n_surviving = res$n_surviving,
    n_aliased   = res$n_aliased,
    n_violators = res$n_violators
  )
}
placebo_tbl <- bind_rows(placebo_rows)
write_csv(placebo_tbl,
          file.path(output_dir, "australia_williams_knot_placebo_maximal_extended.csv"))

# Verdict
finite_r2 <- placebo_tbl$adj_r2[is.finite(placebo_tbl$adj_r2)]
finite_la <- placebo_tbl$lambda[is.finite(placebo_tbl$lambda)]

w_r2_pct <- mean(finite_r2 < canonical_fit$adj_r2, na.rm = TRUE)
w_la_pct <- mean(abs(finite_la) < abs(canonical_fit$lambda), na.rm = TRUE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("MAXIMAL-GETS PLACEBO VERDICT — EXTENDED SAMPLE\n")
cat(strrep("=", 70), "\n", sep = "")
cat(sprintf("\nCanonical maximal-GETS (Williams' 15 institutional knots/priors):\n"))
cat(sprintf("  adjusted R^2 = %.4f\n", canonical_fit$adj_r2))
cat(sprintf("  lambda       = %.4f\n", canonical_fit$lambda))
cat(sprintf("  surviving    = %d of %d candidate knots\n",
            canonical_fit$n_surviving, length(canonical_knots)))
cat(sprintf("\nPlacebo distribution (%d draws of %d random knots + %d random priors):\n",
            length(finite_r2), N_KNOTS, N_KNOTS))
cat(sprintf("  adj R^2  mean=%.4f  median=%.4f  90th pct=%.4f  max=%.4f\n",
            mean(finite_r2), median(finite_r2),
            quantile(finite_r2, 0.9), max(finite_r2)))
cat(sprintf("  |lambda| mean=%.4f  median=%.4f  90th pct=%.4f  max=%.4f\n",
            mean(abs(finite_la)), median(abs(finite_la)),
            quantile(abs(finite_la), 0.9), max(abs(finite_la))))
cat(sprintf("  surviving knots median = %.0f, mean = %.1f\n",
            median(placebo_tbl$n_surviving, na.rm = TRUE),
            mean(placebo_tbl$n_surviving, na.rm = TRUE)))

cat(sprintf("\nCanonical percentile rank in placebo distribution:\n"))
cat(sprintf("  adj R^2:  %.0f%% (canonical beats %.0f%% of random)\n",
            100 * w_r2_pct, 100 * w_r2_pct))
cat(sprintf("  |lambda|: %.0f%% (canonical |lambda| larger than %.0f%% of random)\n",
            100 * w_la_pct, 100 * w_la_pct))

verdict <- if (w_r2_pct > 0.9 && w_la_pct > 0.9) {
  "STRONG SUPPORT — institutional knots/priors clearly outperform random"
} else if (w_r2_pct > 0.75 || w_la_pct > 0.75) {
  "MODERATE SUPPORT — institutional choice beats most random combinations"
} else if (w_r2_pct > 0.5) {
  "WEAK SUPPORT — institutional choice above median but not far"
} else {
  "DETRENDING CRITIQUE PERSISTS — institutional choice below random median"
}
cat(sprintf("\n>>> VERDICT: %s\n\n", verdict))

# Plots
p1 <- ggplot(placebo_tbl, aes(x = adj_r2)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = canonical_fit$adj_r2, color = "red", linewidth = 1) +
  annotate("text", x = canonical_fit$adj_r2, y = Inf,
           label = sprintf("canonical maximal-GETS\n(%.0fth pct)", 100 * w_r2_pct),
           vjust = 2, hjust = 1.05, color = "red") +
  labs(title = "Maximal-GETS placebo: adj R² distribution (extended sample)",
       subtitle = sprintf("%d draws of %d random knots + %d random ±1 priors, drop-on-violation",
                          length(finite_r2), N_KNOTS, N_KNOTS),
       x = "Adjusted R²", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(placebo_tbl, aes(x = abs(lambda))) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = abs(canonical_fit$lambda), color = "red", linewidth = 1) +
  annotate("text", x = abs(canonical_fit$lambda), y = Inf,
           label = sprintf("canonical maximal-GETS\n(%.0fth pct)", 100 * w_la_pct),
           vjust = 2, hjust = 1.05, color = "red") +
  labs(title = "Maximal-GETS placebo: |lambda| distribution (extended sample)",
       subtitle = sprintf("%d draws", length(finite_la)),
       x = "|lambda|", y = "Frequency") +
  theme_minimal()

ggsave(file.path(output_dir, "australia_williams_knot_placebo_maximal_extended_r2.png"),
       p1, width = 10, height = 5, dpi = 120)
ggsave(file.path(output_dir, "australia_williams_knot_placebo_maximal_extended_lambda.png"),
       p2, width = 10, height = 5, dpi = 120)

# Summary CSV
summary_tbl <- tibble(
  metric = c("Canonical maximal-GETS adj R^2",
             "Canonical maximal-GETS |lambda|",
             "Canonical surviving knots",
             "Canonical adj R^2 percentile rank",
             "Canonical |lambda| percentile rank",
             "Placebo n",
             "Placebo adj R^2 median",
             "Placebo |lambda| median",
             "Placebo surviving knots median",
             "Verdict"),
  value = c(sprintf("%.4f", canonical_fit$adj_r2),
            sprintf("%.4f", canonical_fit$lambda),
            as.character(canonical_fit$n_surviving),
            sprintf("%.0fth", 100 * w_r2_pct),
            sprintf("%.0fth", 100 * w_la_pct),
            as.character(length(finite_r2)),
            sprintf("%.4f", median(finite_r2)),
            sprintf("%.4f", median(abs(finite_la))),
            sprintf("%.0f", median(placebo_tbl$n_surviving, na.rm = TRUE)),
            verdict)
)
write_csv(summary_tbl,
          file.path(output_dir, "australia_williams_knot_placebo_maximal_extended_summary.csv"))

cat(sprintf("Saved: %s\n",
            file.path(output_dir, "australia_williams_knot_placebo_maximal_extended.csv")))
cat(sprintf("Saved: %s\n",
            file.path(output_dir, "australia_williams_knot_placebo_maximal_extended_summary.csv")))
