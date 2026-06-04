# ==============================================================================
# NS-135 — does the Australian long run nest MARTIN's consumption block?
#
# MARTIN (Ballantyne, Cassidy, Maddock & Sutton 2019, RDP 2019-07) models the
# long run of real consumption as homogeneous of degree one in real income and
# real net wealth — i.e. log c = w·log y + (1−w)·log NW + (rate term) — with a
# calibrated net-wealth elasticity (1−w) ≈ 0.17 and a small real-rate
# semi-elasticity. This script tests whether that structure is *accepted* on
# the contemporary Australian data.
#
# Construction. lcons and lincome are already log real per-capita series. Real
# per-capita net worth satisfies
#     log(NW_real_pc) = log(networth_y) + lincome + log(4)
# (networth_y is NW over ANNUALISED income; deflator and population cancel), so
# regressing  lcons ~ lincome + log(networth_y) + real_rate  has:
#   - coefficient on log(networth_y)  = the net-wealth elasticity (1−w),
#   - coefficient on lincome          = w + (1−w) = the sum of the income and
#                                        wealth weights, which MARTIN's
#                                        homogeneity restriction sets to 1.
# So the homogeneity test is simply H0: coef(lincome) = 1.
#
# Run:  Rscript Australia/R/martin_nesting.R
# Out:  Australia/outputs/australia_martin_nesting.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(sandwich); library(lmtest); library(car)
})
options(stringsAsFactors = FALSE, scipen = 999)

PROJ_AUS <- "Australia"
source(file.path(PROJ_AUS, "R", "model_helpers.R"), local = TRUE)

master <- readRDS(file.path(PROJ_AUS, "outputs", "australia_model_dataset.rds")) %>%
  rename(lincome = ln_ydi_real_pc, lcons = ln_cons_real_pc) %>%
  filter(!is.na(lcons), !is.na(lincome), !is.na(networth_y), !is.na(real_rate),
         networth_y > 0) %>%
  mutate(log_nw_y = log(networth_y))

cat(sprintf("Sample: %s .. %s (n=%d)\n",
            format(min(master$date)), format(max(master$date)), nrow(master)))

# ---- Unrestricted static long run -------------------------------------------
fit  <- lm(lcons ~ lincome + log_nw_y + real_rate, data = master)
V    <- NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
ct   <- lmtest::coeftest(fit, vcov. = V)
b    <- coef(fit)

# net-wealth elasticity = coef on log_nw_y ; income weight w = 1 − that
nw_elas <- b[["log_nw_y"]]
income_sum <- b[["lincome"]]

# ---- MARTIN homogeneity test: coef(lincome) = 1 -----------------------------
homog <- car::linearHypothesis(fit, "lincome = 1", vcov. = V, test = "Chisq")

# ---- Restricted (homogeneity-imposed) fit: lcons − lincome ~ log_nw_y + rate -
fit_r <- lm(I(lcons - lincome) ~ log_nw_y + real_rate, data = master)
Vr    <- NeweyWest(fit_r, prewhite = FALSE, adjust = TRUE)
ctr   <- lmtest::coeftest(fit_r, vcov. = Vr)

# ---- Cointegration of the unrestricted residual (Engle–Granger MacKinnon) ----
res    <- residuals(fit)
adf    <- run_adf_drift(res, lags = 4L)
n_vars <- 4L  # lcons + 3 regressors
cv5    <- eg_mackinnon_cv(n_vars, "5pct")

out <- tibble(
  quantity = c("coef_lincome (income+wealth sum)",
               "coef_log_nw_y (net-wealth elasticity)",
               "coef_real_rate",
               "homogeneity chi2 (coef_lincome=1)",
               "homogeneity p_value",
               "restricted net-wealth elasticity",
               "restricted real_rate",
               "EG ADF stat on residual",
               "EG 5% CV (n_vars=4)",
               "MARTIN calibrated net-wealth elasticity (ref)"),
  value = c(income_sum, nw_elas, b[["real_rate"]],
            homog$Chisq[2], homog$`Pr(>Chisq)`[2],
            coef(fit_r)[["log_nw_y"]], coef(fit_r)[["real_rate"]],
            adf$adf_stat, cv5, 0.17)
) %>% mutate(value = round(value, 4))

write.csv(out, file.path(PROJ_AUS, "outputs", "australia_martin_nesting.csv"),
          row.names = FALSE)

cat("\n=== Unrestricted long run (NW HAC SEs) ===\n"); print(ct)
cat(sprintf("\nNet-wealth elasticity = %.3f (MARTIN calibrates ~0.17)\n", nw_elas))
cat(sprintf("Income+wealth weight sum = %.3f (MARTIN homogeneity sets = 1)\n", income_sum))
cat(sprintf("Homogeneity test coef(lincome)=1:  chi2(1) = %.2f, p = %.3f -> %s\n",
            homog$Chisq[2], homog$`Pr(>Chisq)`[2],
            ifelse(homog$`Pr(>Chisq)`[2] < 0.05, "REJECTED", "not rejected")))
cat(sprintf("EG cointegration: ADF %.2f vs 5%% CV %.2f -> %s\n",
            adf$adf_stat, cv5, ifelse(adf$adf_stat < cv5, "cointegrated", "NOT cointegrated")))
cat("\n=== Restricted (homogeneity imposed) ===\n"); print(ctr)
cat("\nSaved: Australia/outputs/australia_martin_nesting.csv\n")
