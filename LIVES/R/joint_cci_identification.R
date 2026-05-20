# ==============================================================================
# LIVES — phase 3 JOINT cci_williams identification
#
# The single-equation pipeline's `cci_williams` is built by Hendry-Krolzig
# sign-prior reduction over the *consumption* equation only. The phase-1
# SUR found that this consumption-fitted CCI sign-violates in the HP
# equation (negative loading, t = −4.68 vs prior > 0). That's a structural
# failure: the CCI is not behaving as a true common factor.
#
# This script implements the phase 3 fix: refit all 3 equations
# (consumption, house price, mortgage stock) with the full 15-knot
# Williams candidate basis, and drop knots that violate their sign prior
# in ANY equation. The surviving combination defines `cci_williams_joint`
# — a CCI that's required to behave as a common factor across the system.
#
# This implements Williams' cross-equation sign restrictions through
# coordinated knot survival rather than custom likelihood code. SUR/FIML
# would impose stronger restrictions (parameter equality across equations);
# joint sign survival is the tractable approximation.
#
# Output:
#   LIVES/outputs/lives_joint_cci_survival.csv
#   LIVES/outputs/lives_model_data.rds  (updated with cci_williams_joint)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(lubridate)
  library(sandwich); library(lmtest); library(zoo); library(stringr)
})

options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS   <- "Australia"
PROJ_LIVES <- "LIVES"

dat <- readRDS(file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))
cat(sprintf("Loaded LIVES model data: %d rows\n", nrow(dat)))

source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)
src <- readLines(file.path(PROJ_AUS, "R", "australia_estimation.R"))
guard_at <- grep("if \\(!exists\\(.model_data.\\)\\)", src)[1L]
src_safe <- src[-(guard_at:(guard_at + 6L))]
main_at  <- grep("^# MAIN EXECUTION", src_safe)[1L]
helpers_env <- new.env(parent = globalenv())
eval(parse(text = paste(src_safe[seq_len(main_at - 1L)], collapse = "\n")),
     envir = helpers_env)
fit_ecm_spec <- helpers_env$fit_ecm_spec

# ----------------------------------------------------------------------------
# Build the 15-knot Williams CCI basis as candidates
# ----------------------------------------------------------------------------
basis <- build_williams_cci_basis(dat$date)
knot_dates  <- attr(basis, "knots")
sign_priors <- attr(basis, "sign_priors")
cci_terms   <- colnames(basis)
n_knots <- length(cci_terms)
cat(sprintf("\n%d candidate knots over [%s, %s]\n",
            n_knots, min(knot_dates), max(knot_dates)))

# Attach the basis as columns of the model data
for (j in seq_len(n_knots)) {
  dat[[cci_terms[j]]] <- basis[, j]
}

# ----------------------------------------------------------------------------
# Equation specifications (without cci_williams_lvl; we use the raw 15-knot
# basis in the LR for joint identification)
# ----------------------------------------------------------------------------
base_dummies <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                  "d_neg_gearing_8587", "d_recession_1991",
                  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020")

# Use ln_networth_y_proxy aliased to ln_networth_y for consumption Spec 1
dat$ln_networth_y_extended <- dat$ln_networth_y_proxy

c_spec <- list(
  response = "dlcons",
  lr_base  = c("ln_networth_y_extended", "ln_hp_over_y", "real_rate",
               "ln_yp_over_y", "ecm_lag"),
  sr_vars  = character(0),
  dummies  = base_dummies
)
h_spec <- list(
  response = "dlog_hp_real",
  lr_base  = c("lincome", "log_credit_y", "real_rate", "prime_age_share",
               "ecm_lag_H"),
  sr_vars  = c("d_log_credit_y", "d_real_rate"),
  dummies  = setdiff(base_dummies, c("d2000_gst", "d_jobkeeper_2020"))
)
m_spec <- list(
  response = "dlog_M_real",
  lr_base  = c("lincome", "ln_hp_over_yd", "real_rate", "prime_age_share",
               "ecm_lag_M"),
  sr_vars  = c("dlog_hp_real", "d_real_rate"),
  dummies  = base_dummies
)
# HEW equation (Williams Aust eq 13). Sign priors per Williams' framework:
# CCI loose → higher HEW (households extract more equity when credit eases).
# Income positive (richer households extract more). Housing wealth positive
# (more collateral to extract from). Real rate negative (lower rates make
# extraction cheaper).
w_spec <- list(
  response = "hew_proxy",
  lr_base  = c("lincome", "ha_y_proxy", "real_rate", "prime_age_share",
               "ecm_lag_W"),
  sr_vars  = c("dlog_hp_real", "d_real_rate"),
  dummies  = setdiff(base_dummies, c("d2000_gst", "d_jobkeeper_2020"))
)

# ----------------------------------------------------------------------------
# Fit each equation with full 15-knot basis; record sign of each knot's
# coefficient
# ----------------------------------------------------------------------------
fit_equation_with_basis <- function(spec, label) {
  full_lr <- c(spec$lr_base, cci_terms)
  fit <- tryCatch(
    fit_ecm_spec(
      data         = dat,
      spec_name    = label,
      response     = spec$response,
      lr_vars      = full_lr,
      sr_vars      = spec$sr_vars,
      dummy_vars   = spec$dummies,
      sample_start = as.Date("1976-07-01"),
      sample_end   = max(dat$date)
    ),
    error = function(e) {message("  ", label, " fit error: ", e$message); NULL}
  )
  if (is.null(fit)) return(NULL)
  cf <- coef(fit$fit)
  list(
    fit = fit,
    cci_signs = vapply(cci_terms, function(k) {
      v <- cf[k]
      if (is.na(v) || v == 0) NA_integer_ else as.integer(sign(v))
    }, integer(1)),
    cci_estimates = vapply(cci_terms, function(k) {
      v <- cf[k]; if (is.na(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
  )
}

cat("\n--- Fitting 4 equations with full 15-knot CCI basis ---\n")
fit_c <- fit_equation_with_basis(c_spec, "Cons_15knot")
fit_h <- fit_equation_with_basis(h_spec, "HP_15knot")
fit_m <- fit_equation_with_basis(m_spec, "M_15knot")
fit_w <- fit_equation_with_basis(w_spec, "HEW_15knot")

# ----------------------------------------------------------------------------
# JOINT SURVIVAL: a knot survives iff its sign matches prior in ALL
# equations where it could be estimated (non-NA coefficient)
# ----------------------------------------------------------------------------
survival_tbl <- tibble(
  knot              = cci_terms,
  knot_date         = knot_dates,
  sign_prior        = sign_priors,
  c_sign            = fit_c$cci_signs,
  h_sign            = fit_h$cci_signs,
  m_sign            = fit_m$cci_signs,
  w_sign            = if (!is.null(fit_w)) fit_w$cci_signs else NA_integer_,
  c_estimate        = fit_c$cci_estimates,
  h_estimate        = fit_h$cci_estimates,
  m_estimate        = fit_m$cci_estimates,
  w_estimate        = if (!is.null(fit_w)) fit_w$cci_estimates else NA_real_
) %>%
  mutate(
    c_pass  = is.na(c_sign) | c_sign == sign_prior,
    h_pass  = is.na(h_sign) | h_sign == sign_prior,
    m_pass  = is.na(m_sign) | m_sign == sign_prior,
    w_pass  = is.na(w_sign) | w_sign == sign_prior,
    # Don't count NA (aliased / collinear) as a failure — it just means the
    # knot couldn't be estimated. Survival requires non-NA in at least one
    # equation AND no failures where it WAS estimated.
    n_estimated   = (!is.na(c_sign)) + (!is.na(h_sign)) +
                    (!is.na(m_sign)) + (!is.na(w_sign)),
    # Three-equation survival (legacy diagnostic; was the phase 3 deliverable)
    joint_pass_3eq = c_pass & h_pass & m_pass &
                    ((!is.na(c_sign)) + (!is.na(h_sign)) + (!is.na(m_sign)) > 0L),
    # Four-equation survival (new — A7 in the multi-equation plan)
    joint_pass    = c_pass & h_pass & m_pass & w_pass & (n_estimated > 0L)
  )

cat("\n--- Joint CCI knot survival table ---\n")
print(survival_tbl, n = Inf, digits = 3)

# Compare: consumption-only survival (the existing pipeline)
cat("\n--- Consumption-only survival (the existing cci_williams) ---\n")
survival_c_only <- survival_tbl %>%
  filter(!is.na(c_sign) & c_pass) %>%
  pull(knot)
cat(sprintf("  %d knots survive consumption-only test: %s\n",
            length(survival_c_only),
            if (length(survival_c_only) > 0) paste(survival_c_only, collapse = ", ") else "(none)"))

survivors_3eq    <- survival_tbl %>% filter(joint_pass_3eq) %>% pull(knot)
joint_survivors  <- survival_tbl %>% filter(joint_pass)     %>% pull(knot)

cat(sprintf("\n--- 3-equation joint survival (C ∩ H ∩ M) ---\n"))
cat(sprintf("  %d knots survive 3-eq test: %s\n",
            length(survivors_3eq),
            if (length(survivors_3eq) > 0) paste(survivors_3eq, collapse = ", ") else "(none)"))

cat(sprintf("\n--- 4-equation joint survival (C ∩ H ∩ M ∩ W) — Phase A item A7 ---\n"))
cat(sprintf("  %d knots survive 4-eq test: %s\n",
            length(joint_survivors),
            if (length(joint_survivors) > 0) paste(joint_survivors, collapse = ", ") else "(none)"))

# Knots that consumption alone would survive but joint test rejects
cons_kept_joint_dropped <- setdiff(survival_c_only, joint_survivors)
joint_kept_cons_dropped <- setdiff(joint_survivors, survival_c_only)
cat(sprintf("\n  Knots kept by consumption-only but dropped by 4-eq joint: %s\n",
            if (length(cons_kept_joint_dropped) > 0) paste(cons_kept_joint_dropped, collapse = ", ") else "(none)"))
cat(sprintf("  Knots kept by 4-eq joint but dropped by consumption-only: %s\n",
            if (length(joint_kept_cons_dropped) > 0) paste(joint_kept_cons_dropped, collapse = ", ") else "(none)"))

# ----------------------------------------------------------------------------
# Construct cci_williams_joint variants from joint-surviving knots.
#
# Williams (Aust paper §5.1) normalises ζ_h = 1 in the house-price equation
# — i.e. the CCI is the linear combination whose loading in the HP equation
# is unity. This is the cross-equation identification scheme that defines
# the relative scaling ζ_c, ζ_m, ζ_w in the other equations.
#
# We provide three variants:
#   cci_williams_joint     consumption-equation-weighted (legacy)
#   cci_williams_joint_h   HP-equation-weighted (Williams' ζ_h = 1)
#   cci_williams_joint_m   mortgage-equation-weighted
#
# All three are peak-normalised to unity. The HP-weighted variant is the
# cleanest implementation of Williams' normalisation scheme; the others
# are diagnostic alternatives.
# ----------------------------------------------------------------------------
if (length(joint_survivors) == 0L) {
  cat("\n  WARNING: no knots survive joint test. Falling back to consumption-only.\n")
  joint_survivors <- survival_c_only
}

peak_normalise <- function(x) {
  mx <- max(x, na.rm = TRUE)
  if (is.finite(mx) && mx > 0) {
    x / mx
  } else if (is.finite(mx) && mx < 0) {
    x / abs(mx)
  } else {
    x
  }
}

X_surv <- as.matrix(dat[, joint_survivors, drop = FALSE])
cci_williams_joint   <- peak_normalise(as.numeric(X_surv %*%
                                                     fit_c$cci_estimates[joint_survivors]))
cci_williams_joint_h <- peak_normalise(as.numeric(X_surv %*%
                                                     fit_h$cci_estimates[joint_survivors]))
cci_williams_joint_m <- peak_normalise(as.numeric(X_surv %*%
                                                     fit_m$cci_estimates[joint_survivors]))

# Compare correlations with the existing consumption-fitted cci_williams
rho_joint_vs_cons <- cor(cci_williams_joint, dat$cci_williams,
                          use = "complete.obs")
rho_h_vs_c        <- cor(cci_williams_joint_h, cci_williams_joint,
                          use = "complete.obs")
rho_m_vs_c        <- cor(cci_williams_joint_m, cci_williams_joint,
                          use = "complete.obs")
cat(sprintf("\n  cor(cci_williams_joint, cci_williams_cons-only)   = %.4f\n",
            rho_joint_vs_cons))
cat(sprintf("  cor(cci_williams_joint_h, cci_williams_joint)    = %.4f\n",
            rho_h_vs_c))
cat(sprintf("  cor(cci_williams_joint_m, cci_williams_joint)    = %.4f\n",
            rho_m_vs_c))

# Save updated model data
dat$cci_williams_joint   <- cci_williams_joint
dat$cci_williams_joint_h <- cci_williams_joint_h
dat$cci_williams_joint_m <- cci_williams_joint_m
saveRDS(dat, file.path(PROJ_LIVES, "outputs", "lives_model_data.rds"))

# Save survival table
write_csv(survival_tbl,
          file.path(PROJ_LIVES, "outputs", "lives_joint_cci_survival.csv"))

cat(sprintf("\nSaved: LIVES/outputs/lives_joint_cci_survival.csv\n"))
cat(sprintf("Saved: LIVES/outputs/lives_model_data.rds  (with cci_williams_joint)\n"))
