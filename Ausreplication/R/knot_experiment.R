#!/usr/bin/env Rscript
# ==============================================================================
# Knot-experiment: test different Williams-style CCI spline variants
# ==============================================================================
#
# Question: Williams (2010) uses 4 knots at 1979/1992/1998/2007. Why 4?
# Australian institutional history has at least 16 plausible candidate
# turning points. This script compares 7 variants: the published 3- and
# 4-knot specs, several extended specs that include macroprudential
# episodes, an "Australia within-sample" spec that drops pre-1988
# knots, and a maximal-candidate spec that lets Hendry-Krolzig
# sign-prior reduction prune what isn't supported.
#
# For each variant, we report:
#   - which knots survive sign-prior reduction
#   - which are dropped as sign-violators
#   - which are aliased (constant within the estimation window)
#   - the consumption-equation lambda and adjusted R^2
#   - the implied CCI peak normalised to 1
#
# Run from the project root after a successful estimation pipeline run:
#   Rscript Ausreplication/R/knot_experiment.R
#
# Output:
#   outputs/australia_knot_experiment.csv     wide variant comparison
#   outputs/australia_knot_experiment.md      narrative summary
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(zoo); library(sandwich)
  library(lmtest); library(broom); library(tibble); library(stringr)
  library(lubridate); library(strucchange); library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Resolve paths
.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")
output_dir   <- file.path(project_root, "outputs")
data_raw_dir <- file.path(project_root, "data_raw")

source(file.path(project_root, "R", "model_helpers.R"))

# Load the master from RDS, build model_data the same way the
# orchestrator does.
master <- readRDS(file.path(output_dir, "australia_model_dataset.rds"))
model_data <- master %>%
  rename(
    dlcons   = d_ln_cons_pc,
    lincome  = ln_ydi_real_pc,
    lcons    = ln_cons_real_pc
  ) %>%
  mutate(
    ecm_lag = lag(lcons, 1L) - lincome
  )

# Source enough of australia_estimation.R to get add_model_variables(),
# compute_income_volatility(), construct_permanent_income(),
# fit_ecm_spec(), fit_consumption_with_williams_cci()
src <- readLines(file.path(project_root, "R", "australia_estimation.R"))
main_start <- grep("^cat..\\[Step 1", src)[1L]
eval(parse(text = paste(src[1:(main_start - 1L)], collapse = "\n")))

# Build derived columns in the same order as the live pipeline
model_data <- add_model_variables(model_data)
model_data <- compute_income_volatility(model_data)
model_data <- construct_permanent_income(model_data)
model_data <- model_data %>%
  mutate(
    ecm_lag      = lag(lcons, 1L) - lincome,
    ln_hp_over_y = log(hpi / exp(lincome) * (cons_deflator_norm / 100))
  )

# ==============================================================================
# CANDIDATE KNOT VARIANTS
# ==============================================================================
#
# Each variant is a named list of (date, sign_prior). Sign priors:
#   +1 = credit easing / liberalisation
#   -1 = credit tightening / retrenchment
#
# Sources for the candidate dates: my institutional knowledge of Australian
# financial regulation, cross-checked against the Aust system paper §3
# narrative (pp. 19-20), Battellino-McMillan (1989), Edey-Gray (1996),
# Bayoumi (1993), and APRA's published macroprudential timeline.

variants <- list(
  # V1: Williams (2009) original 3-knot
  williams_2009 = list(
    knots = c("1979-01-01", "1992-01-01", "1998-01-01"),
    signs = c(1, -1, 1),
    note  = "Williams (2009) Table 3 cols A/B; pre-GFC sample only"
  ),
  # V2: Williams (2010) / Muellbauer-Williams (2012) — current default
  williams_2010 = list(
    knots = c("1979-01-01", "1992-01-01", "1998-01-01", "2007-01-01"),
    signs = c(1, -1, 1, -1),
    note  = "Aust system paper Table 5; published 4-knot spec"
  ),
  # V3: 4-knot + 2014/2017 macroprudential
  six_with_macropru = list(
    knots = c("1979-01-01", "1992-01-01", "1998-01-01",
              "2007-01-01", "2014-12-01", "2017-03-01"),
    signs = c(1, -1, 1, -1, -1, -1),
    note  = "Williams 2010 + APRA investor cap (2014Q4) + IO cap (2017Q1)"
  ),
  # V4: 8-knot extended Australian institutional history
  eight_full_australia = list(
    knots = c("1979-01-01", "1986-01-01", "1992-01-01", "1998-01-01",
              "2007-01-01", "2014-12-01", "2017-03-01", "2019-09-01"),
    signs = c(1, 1, -1, 1, -1, -1, -1, 1),
    note  = paste("Extended Australian timeline: + 1986 housing-finance",
                  "deregulation + 2019Q3 APRA cap removal/buffer reduction")
  ),
  # V5: Within-sample only — drop knots that can't identify on 1988+
  australia_within_sample = list(
    knots = c("1992-01-01", "1998-01-01", "2007-01-01",
              "2014-12-01", "2017-03-01"),
    signs = c(-1, 1, -1, -1, -1),
    note  = "Drop pre-1988 knots that are aliased on our sample"
  ),
  # V6: Within-sample + banking-distress and policy-relaxation knots
  australia_within_sample_plus = list(
    knots = c("1990-09-01", "1992-01-01", "1998-01-01", "2007-01-01",
              "2014-12-01", "2017-03-01", "2019-09-01"),
    signs = c(-1, -1, 1, -1, -1, -1, 1),
    note  = paste("Within-sample + 1990Q3 (state bank distress) +",
                  "2019Q3 (APRA cap removal)")
  ),
  # V7: Maximal candidate set — let GETS sign-prior reduction prune
  maximal_gets = list(
    knots = c("1979-01-01", "1986-01-01", "1990-09-01", "1992-01-01",
              "1993-01-01", "1998-09-01", "2007-09-01", "2008-12-01",
              "2009-01-01", "2014-12-01", "2017-03-01", "2019-01-01",
              "2019-09-01", "2020-04-01", "2021-12-01"),
    signs = c(1, 1, -1, -1, -1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1),
    note  = paste("Maximal 15-knot Australian candidate set (Campbell '79,",
                  "housing dereg '86, banking distress '90/'92/'93,",
                  "Wallis/APRA '98, GFC '07, govt guarantee '08, FHB boost '09,",
                  "macropru '14/'17, Hayne RC '19, APRA relax '19Q3,",
                  "COVID '20, buffer hike '21). Let drop-on-violation prune.")
  )
)

# ==============================================================================
# EXPERIMENT RUNNER
# ==============================================================================

# Spec template — Spec 4 disaggregated long-run, no SR dynamics, base dummies
spec_template <- list(
  lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                 "d_neg_gearing_8587", "d_recession_1991",
                 "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
)

run_variant <- function(variant_name, variant) {
  cat(sprintf("\n=== Running variant: %s (%d candidate knots) ===\n",
              variant_name, length(variant$knots)))
  result <- tryCatch(
    fit_consumption_with_williams_cci(
      model_data = model_data %>% mutate(  # ensure no leftover sdmma_ cols
        across(starts_with("sdmma_"), ~ NULL)
      ),
      lr_vars    = spec_template$lr_vars,
      sr_vars    = spec_template$sr_vars,
      dummy_vars = spec_template$dummy_vars,
      sample_end = as.Date("2024-10-01")
    ),
    error = function(e) {
      message(sprintf("  ERROR: %s", conditionMessage(e)))
      return(NULL)
    }
  )
  if (is.null(result)) return(NULL)
  result
}

# fit_consumption_with_williams_cci uses a fixed knot set internally. We
# need to override this for the experiment by temporarily building the
# basis ourselves and running the same drop-on-violation logic. Define
# a copy of the function that accepts custom knots/signs.

fit_with_knots <- function(model_data, knots, signs,
                           lr_vars, sr_vars, dummy_vars,
                           sample_end = as.Date("2024-10-01")) {

  # Drop any leftover sdmma_ columns from previous runs
  if (any(grepl("^sdmma_", names(model_data)))) {
    model_data <- model_data[, !grepl("^sdmma_", names(model_data))]
  }

  basis <- vapply(knots, function(k) smoothed_step(model_data$date, k),
                  numeric(nrow(model_data)))
  cci_terms <- paste0("sdmma_", gsub("-", "_", substr(knots, 1, 7)))
  colnames(basis) <- cci_terms
  for (j in seq_along(cci_terms)) {
    model_data[[cci_terms[j]]] <- basis[, j]
  }

  full_lr <- c(lr_vars, cci_terms)
  spec <- fit_ecm_spec(model_data, "Knot_Test", full_lr, sr_vars,
                       dummy_vars, sample_end = sample_end)

  cf <- coef(spec$fit)
  violators <- character(0)
  aliased   <- character(0)
  surviving <- character(0)
  knot_estimates <- setNames(rep(NA_real_, length(cci_terms)), cci_terms)

  for (j in seq_along(cci_terms)) {
    nm <- cci_terms[j]
    if (!nm %in% names(cf) || is.na(cf[nm])) {
      aliased <- c(aliased, nm)
    } else {
      knot_estimates[nm] <- cf[nm]
      if (cf[nm] != 0 && sign(cf[nm]) != signs[j]) {
        violators <- c(violators, nm)
      } else {
        surviving <- c(surviving, nm)
      }
    }
  }

  # Refit with sign-violators dropped
  if (length(violators) > 0L) {
    full_lr_reduced <- setdiff(full_lr, violators)
    if (length(intersect(cci_terms, full_lr_reduced)) > 0L) {
      spec <- fit_ecm_spec(model_data, "Knot_Test", full_lr_reduced,
                           sr_vars, dummy_vars, sample_end = sample_end)
    }
  }

  cf2 <- coef(spec$fit)
  # Final survivors: not aliased, not violator, not collinear-dropped
  candidate_surv <- setdiff(intersect(cci_terms, names(cf2)),
                            c(violators, aliased))
  candidate_surv <- candidate_surv[!is.na(cf2[candidate_surv])]

  # Build cci series from final survivors
  cci_fitted <- rep(NA_real_, nrow(model_data))
  if (length(candidate_surv) > 0L) {
    X <- as.matrix(model_data[, candidate_surv, drop = FALSE])
    raw <- as.numeric(X %*% cf2[candidate_surv])
    mx <- max(abs(raw), na.rm = TRUE)
    if (is.finite(mx) && mx > 0) cci_fitted <- raw / mx else cci_fitted <- raw
  }

  list(
    spec        = spec,
    knots       = knots,
    signs       = signs,
    cci_terms   = cci_terms,
    surviving   = candidate_surv,
    violators   = violators,
    aliased     = aliased,
    estimates   = knot_estimates,
    n_obs       = nobs(spec$fit),
    adj_r2      = summary(spec$fit)$adj.r.squared,
    lambda      = if ("ecm_lag" %in% names(cf2)) cf2[["ecm_lag"]] else NA_real_,
    cci_fitted  = cci_fitted
  )
}

# Run all variants and collect results
results <- list()
for (vn in names(variants)) {
  v <- variants[[vn]]
  results[[vn]] <- tryCatch(
    fit_with_knots(model_data,
                   knots      = v$knots,
                   signs      = v$signs,
                   lr_vars    = spec_template$lr_vars,
                   sr_vars    = spec_template$sr_vars,
                   dummy_vars = spec_template$dummy_vars),
    error = function(e) {
      message(sprintf("[%s] FAILED: %s", vn, conditionMessage(e)))
      NULL
    }
  )
}

# ==============================================================================
# RESULTS TABLE
# ==============================================================================

cat(rep("=", 78), "\n", sep = "")
cat("KNOT EXPERIMENT — comparison across 7 variants\n")
cat(rep("=", 78), "\n", sep = "")

variant_summary <- list()
for (vn in names(results)) {
  r <- results[[vn]]
  if (is.null(r)) {
    variant_summary[[vn]] <- tibble::tibble(
      variant = vn, n_candidates = NA_integer_, n_aliased = NA_integer_,
      n_violators = NA_integer_, n_surviving = NA_integer_,
      n_obs = NA_integer_, lambda = NA_real_, adj_r2 = NA_real_,
      surviving = NA_character_, dropped = NA_character_, aliased = NA_character_
    )
    next
  }
  short_knot <- function(s) gsub("sdmma_", "", s)
  variant_summary[[vn]] <- tibble::tibble(
    variant       = vn,
    n_candidates  = length(r$knots),
    n_aliased     = length(r$aliased),
    n_violators   = length(r$violators),
    n_surviving   = length(r$surviving),
    n_obs         = r$n_obs,
    lambda        = r$lambda,
    adj_r2        = r$adj_r2,
    surviving     = paste(short_knot(r$surviving), collapse = ", "),
    dropped       = paste(short_knot(r$violators), collapse = ", "),
    aliased       = paste(short_knot(r$aliased), collapse = ", ")
  )
}
summary_tbl <- bind_rows(variant_summary)
print(summary_tbl, n = nrow(summary_tbl), width = Inf)

# Write CSV
out_csv <- file.path(output_dir, "australia_knot_experiment.csv")
write.csv(summary_tbl, out_csv, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", out_csv))

# Knot-by-knot estimates table (which knots get which coefficient under
# which variant — useful for spotting which knots are robustly supported)
all_knots_seen <- unique(unlist(lapply(results, function(r) {
  if (is.null(r)) return(character(0)) else r$cci_terms
})))
knot_estimates_long <- list()
for (vn in names(results)) {
  r <- results[[vn]]
  if (is.null(r)) next
  for (k in r$cci_terms) {
    knot_estimates_long[[length(knot_estimates_long) + 1L]] <- tibble::tibble(
      variant     = vn,
      knot        = gsub("sdmma_", "", k),
      sign_prior  = r$signs[match(k, r$cci_terms)],
      estimate    = r$estimates[k],
      verdict     = if (k %in% r$aliased) "aliased"
                    else if (k %in% r$violators) "sign-violator (DROPPED)"
                    else "surviving"
    )
  }
}
estimates_long <- bind_rows(knot_estimates_long)

out_csv2 <- file.path(output_dir, "australia_knot_experiment_estimates.csv")
write.csv(estimates_long, out_csv2, row.names = FALSE)
cat(sprintf("Saved: %s\n", out_csv2))

# ==============================================================================
# NARRATIVE
# ==============================================================================

# Identify which knots are robustly supported across multiple variants
knot_robustness <- estimates_long %>%
  filter(verdict == "surviving") %>%
  group_by(knot) %>%
  summarise(
    n_variants_surviving = n(),
    mean_estimate        = mean(estimate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_variants_surviving))

cat("\n--- Knot survival across variants ---\n")
print(knot_robustness, n = nrow(knot_robustness))

# Markdown summary
md_lines <- c(
  "# Knot experiment — comparison of CCI spline variants",
  "",
  "Tests 7 variants of the Williams-style smoothed-step CCI spline against",
  "the same disaggregated consumption equation (Spec 4 long-run + base",
  "dummies) on the 1988Q4-2024Q4 sample. Each variant proposes a different",
  "candidate knot set; the Hendry-Krolzig drop-on-violation reduction prunes",
  "knots that violate their institutional sign prior or are collinear/aliased.",
  "",
  "Reported per variant: number of candidate knots, knots aliased",
  "(constant within sample), knots dropped as sign-violators, knots",
  "surviving, the resulting consumption-equation lambda, adjusted R^2.",
  "",
  "## Summary table",
  "",
  "| Variant | Cand. | Aliased | Violators | Surviving | λ | adj R² |",
  "|---|---:|---:|---:|---:|---:|---:|",
  paste0("| ", summary_tbl$variant, " | ",
         summary_tbl$n_candidates, " | ",
         summary_tbl$n_aliased, " | ",
         summary_tbl$n_violators, " | ",
         summary_tbl$n_surviving, " | ",
         sprintf("%.4f", summary_tbl$lambda), " | ",
         sprintf("%.3f", summary_tbl$adj_r2), " |"),
  "",
  "## Robustness — which knots survive across multiple variants",
  "",
  "| Knot | Variants where it survives | Mean OLS coefficient |",
  "|---|---:|---:|",
  paste0("| ", knot_robustness$knot, " | ",
         knot_robustness$n_variants_surviving, " | ",
         sprintf("%.6f", knot_robustness$mean_estimate), " |"),
  "",
  "## Per-variant knot detail",
  "",
  paste(unlist(lapply(names(results), function(vn) {
    r <- results[[vn]]
    if (is.null(r)) return(c("", paste0("### ", vn), "FAILED.", ""))
    short_knot <- function(s) gsub("sdmma_", "", s)
    note <- variants[[vn]]$note
    c(
      "",
      paste0("### ", vn),
      paste0("**Description:** ", note),
      paste0("- **Candidate knots:** ", length(r$knots), " (",
             paste(format(as.Date(r$knots), "%Y-Q"), collapse = ", "), ")"),
      paste0("- **Aliased (constant within window):** ",
             if (length(r$aliased) == 0L) "none"
             else paste(short_knot(r$aliased), collapse = ", ")),
      paste0("- **Sign-violators (dropped):** ",
             if (length(r$violators) == 0L) "none"
             else paste(short_knot(r$violators), collapse = ", ")),
      paste0("- **Surviving:** ",
             if (length(r$surviving) == 0L) "**none — variant fails**"
             else paste(short_knot(r$surviving), collapse = ", ")),
      paste0("- **λ (consumption equation):** ", sprintf("%.4f", r$lambda)),
      paste0("- **Adjusted R²:** ", sprintf("%.3f", r$adj_r2)),
      ""
    )
  })), collapse = "\n"),
  "",
  "## Interpretation",
  "",
  "[TO FLESH OUT — depends on actual results]",
  "",
  "Generated by `Ausreplication/R/knot_experiment.R`. To refresh, re-run.",
  ""
)

out_md <- file.path(output_dir, "australia_knot_experiment.md")
writeLines(md_lines, out_md)
cat(sprintf("Saved: %s\n", out_md))

cat(rep("=", 78), "\n", sep = "")
cat("Done.\n")
cat(rep("=", 78), "\n", sep = "")
