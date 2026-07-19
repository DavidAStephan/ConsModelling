# ==============================================================================
# spec11_ablation.R
#
# Plan item C1 (journal_review_2026-07.md, Referee 2 finding 1): the paper's
# "form is decisive" claim compares Spec 11 (n=146, cci_williams,
# 1988Q3-2024Q4) against Spec 6 (n=86, cci_ratio-bound, 2002Q3-2024Q4). These
# two specs differ in BOTH functional form (generic wealth-ECM vs the LIVES
# Eq-7 credit-interaction core) AND estimation sample/CCI series. This script
# isolates the two by fitting two additional cells:
#
#   Cell A: Spec 11's regressor set, estimated on Spec 6's exact window
#           (2002Q3-2024Q4, n=86) -- "form only" test.
#   Cell B: Spec 6's regressor set, estimated on Spec 11's window
#           (1988Q3-2024Q4, n=146) with the cci_ratio-based short-run credit
#           term replaced by an analogous cci_williams-based term --
#           "sample/CCI-series only" test.
#
# Reproduction check (must pass before any new cell is estimated): replicate
# the committed baseline Spec 6 (lambda = -0.2386, Australia/outputs/
# australia_full_results.csv line 88) and Spec 11 (lambda = -0.4483, same
# file line 234) using the exact pipeline conventions from
# australia_estimation.R (fit_ecm_spec(), Newey-West HAC, the Williams CCI
# spline fit, the Spec 8/11 de-meaned interaction construction).
#
# Output: Australia/outputs/australia_spec11_ablation.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ <- "Australia"

# ------------------------------------------------------------------------
# 0. Load cached master RDS, attach the maximal SDMMA basis exactly as
#    run_estimation_from_rds.R does, and source the pipeline's own
#    functions (not reimplementations) via a "functions-only" eval that
#    strips the `model_data` existence guard and stops before MAIN
#    EXECUTION -- the same pattern used by refit_spec46_extended.R /
#    refit_spec1_extended.R for isolated re-estimation.
# ------------------------------------------------------------------------
master <- readRDS(file.path(PROJ, "outputs", "australia_model_dataset.rds"))
source(file.path(PROJ, "R", "model_helpers.R"), local = TRUE)

USE_WILLIAMS_SDMMA_BASIS <- TRUE
if (USE_WILLIAMS_SDMMA_BASIS && !any(grepl("^sdmma_", names(master)))) {
  williams_basis <- build_williams_cci_basis(master$date)
  for (j in seq_len(ncol(williams_basis))) {
    master[[colnames(williams_basis)[j]]] <- williams_basis[, j]
  }
  cat("Attached maximal (15-knot) SDMMA CCI basis to master\n")
}

model_data <- master %>%
  rename(dlcons = d_ln_cons_pc, lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  mutate(ecm_lag = lag(lcons, 1L) - lincome)

src      <- readLines(file.path(PROJ, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)

add_model_variables               <- helpers_env$add_model_variables
compute_income_volatility         <- helpers_env$compute_income_volatility
construct_permanent_income_italy  <- helpers_env$construct_permanent_income_italy
fit_ecm_spec                      <- helpers_env$fit_ecm_spec
fit_consumption_with_williams_cci <- helpers_env$fit_consumption_with_williams_cci

# Replicate MAIN EXECUTION Steps 1-3 (PI_METHOD = "italy", set at the top of
# australia_estimation.R and captured by helpers_env).
model_data <- add_model_variables(model_data)
model_data <- compute_income_volatility(model_data)
model_data <- construct_permanent_income_italy(model_data)
model_data <- model_data %>% mutate(ecm_lag = lag(lcons, 1L) - lincome)

cat(sprintf("model_data: %d rows, %d complete for core vars\n", nrow(model_data),
            sum(complete.cases(model_data %>% select(dlcons, ecm_lag, real_rate, lincome)))))

base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")
sample_end <- as.Date("2024-10-01")

# ------------------------------------------------------------------------
# Step 4a-i: fit the Williams CCI spline exactly as MAIN EXECUTION does,
# to attach cci_williams to model_data.
# ------------------------------------------------------------------------
stopifnot(any(grepl("^sdmma_", names(model_data))))
cat("[4a-i] Fitting Williams-style maximal (15-knot) CCI spline...\n")
williams_fit_info <- fit_consumption_with_williams_cci(
  model_data,
  lr_vars    = c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
                 "real_rate", "ln_yp_over_y", "ecm_lag"),
  sr_vars    = character(0),
  dummy_vars = base_dummies,
  sample_end = sample_end
)
stopifnot(!is.null(williams_fit_info), length(williams_fit_info$surviving_knots) > 0L)
model_data$cci_williams <- williams_fit_info$model_data$cci_williams
for (nm in williams_fit_info$knot_names) {
  if (!nm %in% names(model_data)) model_data[[nm]] <- williams_fit_info$model_data[[nm]]
}
cat(sprintf("[4a-i] cci_williams: %d non-NA obs (%s -> %s); surviving knots: %s\n",
            sum(!is.na(model_data$cci_williams)),
            as.character(min(model_data$date[!is.na(model_data$cci_williams)])),
            as.character(max(model_data$date[!is.na(model_data$cci_williams)])),
            paste(williams_fit_info$surviving_knots, collapse = ", ")))

# ------------------------------------------------------------------------
# Step 4a-ii: attach the de-meaned Spec 8/11 interaction columns, exactly
# reproducing australia_estimation.R's construction (means computed on the
# Spec 8 estimation window: cci_williams non-NA, 1980-01-01 <= date <=
# sample_end).
# ------------------------------------------------------------------------
spec8_mask <- !is.na(model_data$cci_williams) &
              model_data$date >= as.Date("1980-01-01") &
              model_data$date <= sample_end
ha_mean <- mean(model_data$ha_y[spec8_mask],         na.rm = TRUE)
hp_mean <- mean(model_data$ln_hp_over_y[spec8_mask], na.rm = TRUE)
r_mean  <- mean(model_data$real_rate[spec8_mask],    na.rm = TRUE)
yp_mean <- mean(model_data$ln_yp_over_y[spec8_mask], na.rm = TRUE)

md8 <- model_data %>%
  mutate(
    r_x_cci          = (real_rate    - r_mean)  * cci_williams,
    hp_x_1_minus_cci = (ln_hp_over_y - hp_mean) * (1 - 1.2 * cci_williams),
    yp_x_cci         = (ln_yp_over_y - yp_mean) * cci_williams,
    ha_x_cci         = (ha_y        - ha_mean) * cci_williams
  )
md11 <- md8 %>% mutate(ilfa_y = eq_y + super_y)

# ------------------------------------------------------------------------
# Regressor sets, exactly as australia_estimation.R defines Spec 6 / Spec 11
# ------------------------------------------------------------------------
spec6_lr <- c("nla_y", "eq_y", "super_y", "ha_y", "ln_hp_over_y",
              "real_rate", "ln_yp_over_y", "ln_yp_over_y_post2008", "ecm_lag")
spec6_sr <- c("d2_logcci_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")

spec11_lr <- c("nla_y", "ilfa_y", "ha_x_cci", "hp_x_1_minus_cci", "r_x_cci",
               "cci_williams", "ln_yp_over_y", "yp_x_cci", "ecm_lag")
spec11_sr <- c("dd4_income", "d2_log_unemp", "abs_income_resid")

# ==========================================================================
# STAGE 1: reproduce the committed baselines (STOP if either fails)
# ==========================================================================
cat("\n", strrep("=", 70), "\nSTAGE 1: baseline reproduction check\n", strrep("=", 70), "\n", sep = "")

base_spec6 <- fit_ecm_spec(
  data = md8, spec_name = "Spec6_Preferred_repro",
  lr_vars = spec6_lr, sr_vars = spec6_sr, dummy_vars = base_dummies,
  sample_end = sample_end
)
base_spec11 <- fit_ecm_spec(
  data = md11, spec_name = "Spec11_LIVES_Headline_repro",
  lr_vars = spec11_lr, sr_vars = spec11_sr, dummy_vars = base_dummies,
  sample_end = sample_end
)

lambda6  <- unname(coef(base_spec6$fit)["ecm_lag"])
lambda11 <- unname(coef(base_spec11$fit)["ecm_lag"])
n6  <- nrow(base_spec6$est_data)
n11 <- nrow(base_spec11$est_data)

cat(sprintf("Reproduced Spec 6:  lambda = %.4f (target -0.2386), n = %d (target 86)\n",
            lambda6, n6))
cat(sprintf("Reproduced Spec 11: lambda = %.4f (target -0.4483), n = %d (target 146)\n",
            lambda11, n11))

TOL <- 5e-4
ok6  <- !is.na(lambda6)  && abs(lambda6  - (-0.238632699010184)) < TOL && n6  == 86L
ok11 <- !is.na(lambda11) && abs(lambda11 - (-0.44826755184568))  < TOL && n11 == 146L

if (!ok6 || !ok11) {
  stop(sprintf(
    "[spec11_ablation] Baseline reproduction FAILED (Spec6 ok=%s, Spec11 ok=%s). ",
    ok6, ok11),
    "Stopping per task instructions -- do not proceed to the ablation cells.")
}
cat("Baseline reproduction PASSED for both Spec 6 and Spec 11. Proceeding.\n")

# ==========================================================================
# STAGE 2: Cell A -- Spec 11's regressor set on Spec 6's exact window
# ==========================================================================
cat("\n", strrep("=", 70), "\nSTAGE 2: Cell A (form=Spec11, sample=Spec6)\n", strrep("=", 70), "\n", sep = "")

spec6_dates    <- sort(base_spec6$est_data$date)
spec6_start    <- min(spec6_dates)
spec6_end      <- max(spec6_dates)
spec6_n_quarters_expected <- as.integer(round(as.numeric(
  difftime(spec6_end, spec6_start, units = "days")) / 91.31)) + 1L
cat(sprintf("Spec 6 window: %s -> %s (n = %d; contiguous-quarters check: %s)\n",
            spec6_start, spec6_end, length(spec6_dates),
            if (length(spec6_dates) == spec6_n_quarters_expected) "contiguous" else "HAS GAPS"))

cellA <- fit_ecm_spec(
  data = md11, spec_name = "CellA_Spec11form_Spec6sample",
  lr_vars = spec11_lr, sr_vars = spec11_sr, dummy_vars = base_dummies,
  sample_start = spec6_start, sample_end = spec6_end
)
cat(sprintf("Cell A: lambda = %.4f, n = %d (Spec 6 window forced)\n",
            unname(coef(cellA$fit)["ecm_lag"]), nrow(cellA$est_data)))

same_dates_A <- setequal(cellA$est_data$date, base_spec6$est_data$date)
cat(sprintf("Cell A uses exactly Spec 6's %d dates: %s\n", n6, same_dates_A))

# ==========================================================================
# STAGE 3: Cell B -- Spec 6's regressor set on Spec 11's window, with the
# cci_ratio-bound short-run credit term (d2_logcci_lag2, bound to 2002Q3+ by
# ABS 5601.0 housing-loan flow) replaced by an analogous transformation of
# cci_williams (available across the full 1988Q3+ window). cci_williams is
# a normalised spline index (max-scaled combination of sign-surviving SDMMA
# step functions), not a log ratio like cci_ratio = log(housing_loan_flow /
# ydi_ann_8qma); it is not uniformly signed/positive, so log(cci_williams)
# is not well-defined. The literal "Delta^2 log CCI lag 2" transform cannot
# be replicated. The closest form-preserving analogue -- second difference
# (not log-difference) of cci_williams, lagged 2, matching the "acceleration
# in credit conditions" economic content of the original term -- is used
# instead, and flagged as such in the output.
# ==========================================================================
cat("\n", strrep("=", 70), "\nSTAGE 3: Cell B (form=Spec6, sample=Spec11, CCI series swapped)\n", strrep("=", 70), "\n", sep = "")

cci_w_range <- range(model_data$cci_williams, na.rm = TRUE)
cat(sprintf("cci_williams range: [%.4f, %.4f] -- %s for log()\n",
            cci_w_range[1], cci_w_range[2],
            if (cci_w_range[1] > 0) "valid" else "INVALID (non-positive values present)"))

feasible_B <- cci_w_range[1] > 0

if (feasible_B) {
  # (unreachable given cci_williams is a max-scaled spline that spans
  #  negative-to-positive; kept for completeness in case the surviving
  #  knot combination is ever uniformly positive)
  md6b <- md8 %>%
    mutate(
      log_cci_w      = log(cci_williams),
      d2_logcciw     = log_cci_w - 2 * lag(log_cci_w, 1L) + lag(log_cci_w, 2L),
      d2_logcciw_lag2 = lag(d2_logcciw, 2L)
    )
  cellB_sr <- c("d2_logcciw_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")
} else {
  md6b <- md8 %>%
    mutate(
      d2_ccw      = cci_williams - 2 * lag(cci_williams, 1L) + lag(cci_williams, 2L),
      d2_ccw_lag2 = lag(d2_ccw, 2L)
    )
  cellB_sr <- c("d2_ccw_lag2", "dd4_income", "d2_log_unemp", "abs_income_resid")
}

spec11_start <- min(base_spec11$est_data$date)
spec11_end   <- max(base_spec11$est_data$date)

cellB <- tryCatch(
  fit_ecm_spec(
    data = md6b, spec_name = "CellB_Spec6form_Spec11sample_cciW",
    lr_vars = spec6_lr, sr_vars = cellB_sr, dummy_vars = base_dummies,
    sample_start = spec11_start, sample_end = spec11_end
  ),
  error = function(e) { message("[Cell B] fit failed: ", conditionMessage(e)); NULL }
)

cellB_note <- if (feasible_B) {
  "feasible: cci_williams uniformly positive; used Delta^2 log(cci_williams) lag2 (exact form match)"
} else {
  paste0("PARTIAL: cci_williams is a max-scaled spline index spanning negative ",
         "values (range shown above), so log(cci_ratio)-style Delta^2-log ",
         "transform is undefined for it. Substituted the second difference ",
         "(not log-difference) of cci_williams, lagged 2 quarters, as the ",
         "closest available 'acceleration in credit conditions' analogue. ",
         "This cell is form-approximate, not form-identical, on the SR credit ",
         "term; the LR block (nla_y, eq_y, super_y, ha_y, ln_hp_over_y, ",
         "real_rate, ln_yp_over_y, ln_yp_over_y_post2008) is unchanged from ",
         "Spec 6.")
}
cat(cellB_note, "\n")

if (!is.null(cellB)) {
  cat(sprintf("Cell B: lambda = %.4f, n = %d\n",
              unname(coef(cellB$fit)["ecm_lag"]), nrow(cellB$est_data)))
} else {
  cat("Cell B: fit failed; see message above.\n")
}

# ==========================================================================
# Assemble output table
# ==========================================================================
extract_row <- function(fit_obj, spec_form, sample_label, key_lr_name) {
  if (is.null(fit_obj)) {
    return(tibble(
      spec_form = spec_form, sample = sample_label, n = NA_integer_,
      lambda = NA_real_, lambda_t = NA_real_,
      nla_y = NA_real_, nla_y_t = NA_real_,
      ilfa_y = NA_real_, ilfa_y_t = NA_real_,
      housing_term = NA_character_, housing_coef = NA_real_, housing_t = NA_real_,
      ln_yp_over_y = NA_real_, ln_yp_over_y_t = NA_real_,
      note = "fit failed"
    ))
  }
  cf   <- summary(fit_obj$fit)$coefficients
  nw   <- lmtest::coeftest(fit_obj$fit, vcov. = fit_obj$nw_vcov)
  gt   <- function(nm) if (nm %in% rownames(nw)) unname(nw[nm, "Estimate"]) else NA_real_
  gtt  <- function(nm) if (nm %in% rownames(nw)) unname(nw[nm, "t value"]) else NA_real_

  has_ilfa <- "ilfa_y" %in% rownames(nw)
  # Housing term: ha_y (Spec6-style level) OR ha_x_cci (Spec11-style
  # credit-scaled interaction) OR both financial-wealth aggregates for eq_y+super_y
  housing_term_name <- if ("ha_x_cci" %in% rownames(nw)) "ha_x_cci" else "ha_y"

  tibble(
    spec_form       = spec_form,
    sample          = sample_label,
    n               = nrow(fit_obj$est_data),
    lambda          = gt("ecm_lag"),
    lambda_t        = gtt("ecm_lag"),
    nla_y           = gt("nla_y"),
    nla_y_t         = gtt("nla_y"),
    ilfa_y          = if (has_ilfa) gt("ilfa_y") else gt("eq_y") + gt("super_y"),
    ilfa_y_t        = if (has_ilfa) gtt("ilfa_y") else NA_real_,
    housing_term    = housing_term_name,
    housing_coef    = gt(housing_term_name),
    housing_t       = gtt(housing_term_name),
    ln_yp_over_y    = gt("ln_yp_over_y"),
    ln_yp_over_y_t  = gtt("ln_yp_over_y"),
    note            = ""
  )
}

out <- bind_rows(
  extract_row(base_spec6,  "Spec6_form (baseline)",  "Spec6_sample (2002Q3-2024Q4, n=86)",  "ha_y")     %>%
    mutate(note = sprintf("baseline reproduction; committed lambda=-0.2386, n=86 -- MATCH")),
  extract_row(base_spec11, "Spec11_form (baseline)", "Spec11_sample (1988Q3-2024Q4, n=146)", "ha_x_cci") %>%
    mutate(note = sprintf("baseline reproduction; committed lambda=-0.4483, n=146 -- MATCH")),
  extract_row(cellA, "Spec11_form", "Spec6_sample (2002Q3-2024Q4, n=86)", "ha_x_cci") %>%
    mutate(note = "Cell A: Spec 11 regressors on Spec 6's exact estimation window (form-only test)"),
  extract_row(cellB, "Spec6_form", "Spec11_sample (1988Q3-2024Q4, n=146)", "ha_y") %>%
    mutate(note = cellB_note)
)

out_path <- file.path(PROJ, "outputs", "australia_spec11_ablation.csv")
write_csv(out, out_path)
cat(sprintf("\nSaved: %s\n", out_path))
print(out, n = Inf, width = Inf)
