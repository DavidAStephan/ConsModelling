#!/usr/bin/env Rscript
# ==============================================================================
# Italy Household Consumption Model — Master Script
# ==============================================================================
#
# Replication of: De Bonis, Liberati, Muellbauer, Rondinelli (2025)
# "Why net worth is the wrong concept for explaining consumption:
#  evidence from Italy"
#
# This master script:
#   1. Sources Part 1 (data download & construction → produces `master`)
#   2. Builds `model_data` from `master` with paper-consistent variable names
#   3. Sources Part 2 (estimation framework → produces results, tables, plots)
#
# Data sources:
#   - Eurostat REST API  (national accounts, unemployment, HPI, HICP)
#   - ECB SDW REST API   (financial sector accounts, MIR interest rates, BLS)
#   - BIS residential property prices (long-run house price back-fill)
#
# Data gaps vs. paper:
#   - Financial balance sheets 1980-1994: back-extrapolated (paper uses
#     Bonci & Coletta 2008; obtain from Banca d'Italia for exact replication)
#   - Housing wealth: approximated via HPI × calibrated stock level (paper
#     uses ISTAT from 2001 + Cannari et al. 2017 back to 1980)
#   - Mortgage interest rate pre-2003: extrapolated (paper uses BdI ABI series)
#   - CCI: log-credit/GDP proxy used (paper uses BoI granted credit lines)
#   - Public consumption: Eurostat government final consumption (proxy)
#
# Run from the project root:
#   Rscript R/italy_consumption_model.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(lmtest)
  library(sandwich)
  library(strucchange)
  library(broom)
  library(zoo)
  library(httr)
  library(jsonlite)
  library(readxl)
  library(stringr)
  library(urca)
})

options(stringsAsFactors = FALSE, scipen = 999)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
output_dir   <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# STEP 1: Run data download and construction
# ==============================================================================

message("\n", strrep("=", 70))
message("STEP 1: Downloading and constructing Italian data")
message(strrep("=", 70), "\n")

source(file.path(project_root, "R", "italy_data_download.R"), local = FALSE)
# After sourcing, `master` is in the global environment.


# ==============================================================================
# STEP 2: Download government consumption (for short-run control)
#         Eurostat namq_10_co3_p3, sector S13 (government)
# ==============================================================================

message("\n", strrep("=", 70))
message("STEP 2: Government consumption and housing wealth")
message(strrep("=", 70), "\n")

raw_dir_italy <- file.path(project_root, "data_raw_italy")

# Fetch helper (defined in italy_data_download.R but guard in case)
if (!exists("fetch_eurostat")) {
  source(file.path(project_root, "R", "italy_data_download.R"), local = FALSE)
}
if (!exists("cache_or_fetch")) {
  cache_or_fetch <- function(name, fn) {
    p <- file.path(raw_dir_italy, paste0(name, ".rds"))
    if (file.exists(p)) return(readRDS(p))
    result <- fn()
    if (!is.null(result)) saveRDS(result, p)
    result
  }
}

# 2a. Government final consumption (real, chain-linked volumes)
#     namq_10_gdp na_item=P31_S13 is the standard government consumption item.
#     Try each unit in turn; vector-valued unit filters cause URL recycling bugs.
raw_gov_cons <- cache_or_fetch("eurostat_gov_cons", function() {
  for (unit_code in c("CLV10_MEUR", "CLV_I10", "CP_MEUR")) {
    r <- tryCatch(
      fetch_eurostat("namq_10_gdp",
                     list(geo="IT", na_item="P31_S13", unit=unit_code, s_adj="NSA")),
      error = function(e) NULL
    )
    if (!is.null(r) && nrow(r) > 0) return(r)
  }
  NULL
}, raw_dir_italy)

gov_cons <- NULL
if (!is.null(raw_gov_cons) && nrow(raw_gov_cons) > 0) {
  gov_cons_tmp <- raw_gov_cons
  if ("s_adj" %in% names(gov_cons_tmp)) gov_cons_tmp <- gov_cons_tmp %>% filter(s_adj == "NSA")
  gov_cons <- gov_cons_tmp %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup() %>%
    select(date, gov_cons_real = value)
  message("  Government consumption: ", nrow(gov_cons), " obs, ",
          min(gov_cons$date), " to ", max(gov_cons$date))
}

# 2b. Non-financial assets (housing wealth) for Italian households
#     ECB QSA: non-financial assets, households + NPISH
#     Key: QSA.Q.N.IT.W0.S14_S15.S1.N.A.AN._Z._Z.EUR.V.N
#     (AN = non-financial assets, gross stock)
#
# Note: ECB QSA non-fin assets available from ~2001 at best.
# Full 1980-2019 housing wealth series requires Cannari et al. (2017) / ISTAT.
# We approximate using: ha = hpi_index × calibrated_baseline_stock

raw_nfin_assets <- cache_or_fetch("ecb_qsa_nfin_assets", function() {
  keys <- c(
    "QSA.Q.N.IT.W0.S14_S15.S1.N.A.AN._Z._Z.EUR.V.N",
    "QSA.Q.N.IT.W0.S1M.S1.N.A.AN._Z._Z.EUR.V.N"
  )
  for (k in keys) {
    r <- tryCatch(fetch_ecb_sdw(k), error = function(e) NULL)
    if (!is.null(r) && nrow(r) > 0) return(r)
  }
  NULL
}, raw_dir_italy)

message("  Non-financial assets (ECB QSA): ",
        if (!is.null(raw_nfin_assets)) nrow(raw_nfin_assets) else 0, " obs")


# ==============================================================================
# STEP 3: Construct model_data from master
#         Map to paper-consistent variable names and build derived variables
# ==============================================================================

message("\n", strrep("=", 70))
message("STEP 3: Building model_data")
message(strrep("=", 70), "\n")

# Quarterly date spine (should already exist from Part 1, but guard)
if (!exists("date_spine")) {
  date_spine <- seq(as.Date("1980-01-01"), as.Date("2019-10-01"), by = "quarter")
}

# 3.1 Add government consumption to master
master <- master %>%
  left_join(
    if (!is.null(gov_cons)) gov_cons else tibble(date = date_spine$date, gov_cons_real = NA_real_),
    by = "date"
  )

# 3.2 Construct per-capita financial asset components (real, per capita in EUR)
#     All financial stocks already in real million EUR after deflation in Part 1.
#     Per capita = / pop_millions   (millions EUR per person = EUR per person × 1e-6...
#     actually:  million EUR / million persons = EUR/person)

master <- master %>%
  mutate(
    # Alias columns to names expected downstream (download script uses different names)
    fin_debt_sec_real_pc  = fin_debtsec_real_pc,
    fin_mf_shares_real_pc = fin_mutfund_real_pc,
    fin_insurance_real_pc = fin_inspen_real_pc,
    # Per-capita government consumption
    gov_cons_real_pc      = ifelse(!is.na(pop_millions) & pop_millions > 0 &
                                     !is.na(gov_cons_real),
                                   gov_cons_real / pop_millions, NA_real_)
  )

# 3.3a Real house price index (deflated by consumption deflator)
master <- master %>%
  mutate(
    hpi_real = ifelse(!is.na(hpi) & !is.na(cons_deflator) & cons_deflator > 0,
                      hpi / (cons_deflator / 100), NA_real_)
  )

# 3.3 Construct housing wealth
#     Strategy:
#     (a) If ECB QSA non-financial assets available post-2001, use those (deflated)
#     (b) Pre-2001 and as fallback: approximate ha = hpi_real × baseline_stock
#         Calibrate baseline so mean(ha_y) ≈ 5.0 over available period.
#         This matches the paper's descriptive statistic (Table A5: mean ha/y = 5.03).

master <- master %>% arrange(date)

# Attempt to use ECB non-financial assets data
ha_from_ecb <- NULL
if (!is.null(raw_nfin_assets) && nrow(raw_nfin_assets) > 0) {
  defl_vec <- master$cons_deflator
  dates_vec <- master$date
  pop_vec   <- master$pop_millions

  ha_from_ecb <- raw_nfin_assets %>%
    rename(nfin_nom = value) %>%
    left_join(tibble(date = dates_vec, defl = defl_vec, pop = pop_vec), by = "date") %>%
    mutate(
      ha_real = ifelse(!is.na(defl) & defl != 0,
                       nfin_nom / (defl / 100), NA_real_),
      ha_real_pc = ifelse(!is.na(pop) & pop > 0, ha_real / pop, NA_real_)
    ) %>%
    select(date, ha_real_pc)
  message("  Housing wealth from ECB QSA: ", sum(!is.na(ha_from_ecb$ha_real_pc)), " non-NA obs")
}

# Merge ECB housing wealth
if (!is.null(ha_from_ecb) && sum(!is.na(ha_from_ecb$ha_real_pc)) > 20) {
  master <- master %>%
    left_join(ha_from_ecb, by = "date")
} else {
  master$ha_real_pc <- NA_real_
}

# Approximate housing wealth from house prices × calibrated stock
# The ratio ha_y = ha / (4 * ydi_real_pc) has historical mean ~5.03 for Italy.
# We use: ha_approx = hpi_real × K where K chosen so mean(ha_approx / (4*ydi_real_pc)) = 5.03

master <- master %>%
  mutate(
    # Use ECB data where available; fill gaps with HPI-based approximation
    ha_real_pc_ecb = ha_real_pc,

    # Target calibration: approximate housing stock in 2010 prices per capita
    # such that ratio to 4×annual income averages ~5.03
    # K = 5.03 × 4 × mean(ydi_real_pc) / mean(hpi_real)  [over 1995-2019 window]
    ha_real_pc = {
      # Build HPI-based approximation using a rolling calibration
      hpi_use <- ifelse(!is.na(hpi_real), hpi_real, hpi)
      # Overlap period where both ECB and HPI data exist
      has_ecb_data <- !is.na(ha_real_pc_ecb)
      if (sum(has_ecb_data) > 20) {
        # Calibrate K from overlap
        K <- mean(ha_real_pc_ecb[has_ecb_data] / hpi_use[has_ecb_data], na.rm = TRUE)
      } else {
        # Calibrate K so mean(ha_y) over data period ≈ 5.03
        # ha_y = ha_real_pc / (4 * ydi_real_pc)  →  K = 5.03 * 4 * mean(ydi/hpi)
        K <- 5.03 * 4 * mean(ydi_real_pc / hpi_use, na.rm = TRUE)
      }
      ha_approx <- K * hpi_use
      # Use ECB data where available; HPI approximation elsewhere
      ifelse(!is.na(ha_real_pc_ecb), ha_real_pc_ecb, ha_approx)
    }
  )

message("  Housing wealth (ha_real_pc): ",
        sum(!is.na(master$ha_real_pc)), " non-NA obs")

# 3.4 Construct annualised income for asset/income ratios
#     The paper uses "scaled income" ≈ a weighted average of labour+transfer
#     income and total disposable income.  We proxy with real disposable income.
#     ydi_real_pc is quarterly, so annualise: y_annual_pc = ydi_real_pc * 4

master <- master %>%
  mutate(
    y_annual_pc = ydi_real_pc * 4  # annualised income per capita (EUR, 2010 prices)
  )

# 3.5 Construct wealth/income ratios
#     In the paper these are ratios of real wealth stocks to real annual income,
#     measured in the same price base.

master <- master %>%
  mutate(
    # --- Net Liquid Assets / income ---
    # NLA = deposits + mutual fund shares + treasury bills - loans
    # Proxy: NLA ≈ deposits + mf_shares - loans  (T-bills approximated in bonds)
    nla_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                   (fin_deposits_real_pc + fin_mf_shares_real_pc - fin_loans_real_pc) /
                     y_annual_pc,
                   NA_real_),

    # --- Bonds (semi-liquid) / income ---
    # Bonds ≈ all debt securities (includes long-term gov bonds; some T-bills)
    bonds_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                     fin_debt_sec_real_pc / y_annual_pc,
                     NA_real_),

    # --- Illiquid Financial Assets / income ---
    # ILFA = quoted shares + pensions + life insurance reserves
    ilfa_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                    (fin_equity_real_pc + fin_insurance_real_pc) / y_annual_pc,
                    NA_real_),

    # --- Housing Assets / income ---
    ha_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                  ha_real_pc / y_annual_pc,
                  NA_real_),

    # --- Net Financial Assets / income (for spec 4) ---
    nfa_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                   (fin_total_asset_real_pc - fin_loans_real_pc) / y_annual_pc,
                   NA_real_),

    # --- Net Worth / income (conventional: all assets less debt) ---
    # Total = financial wealth + housing wealth
    networth_y = ifelse(!is.na(y_annual_pc) & y_annual_pc > 0,
                        (fin_total_asset_real_pc - fin_loans_real_pc + ha_real_pc) /
                          y_annual_pc,
                        NA_real_)
  )

# Guard against negative net worth (log undefined)
master <- master %>%
  mutate(
    ln_networth_y = ifelse(networth_y > 0, log(networth_y), NA_real_)
  )

message("  Wealth/income ratios constructed. Sample means:")
message("    nla_y:      ", round(mean(master$nla_y,      na.rm = TRUE), 3))
message("    bonds_y:    ", round(mean(master$bonds_y,    na.rm = TRUE), 3))
message("    ilfa_y:     ", round(mean(master$ilfa_y,     na.rm = TRUE), 3))
message("    ha_y:       ", round(mean(master$ha_y,       na.rm = TRUE), 3),
        "  (target ≈ 5.03)")
message("    nfa_y:      ", round(mean(master$nfa_y,      na.rm = TRUE), 3))
message("    networth_y: ", round(mean(master$networth_y, na.rm = TRUE), 3))

# 3.6 Log house price / income ratio
master <- master %>%
  mutate(
    # Real house price index per unit of real per-capita income
    # ln(HP/y): paper uses real HP index / real per-capita scaled income
    hp_over_y     = ifelse(!is.na(ydi_real_pc) & ydi_real_pc > 0,
                           hpi_real / ydi_real_pc,  NA_real_),
    ln_hp_over_y  = ifelse(hp_over_y > 0, log(hp_over_y), NA_real_)
  )

# 3.7 Real interest rate
#     r_t = nominal mortgage rate - 4-quarter annualised inflation
#     Paper uses BIS-adjusted nominal rate (12-year amortising loan)
#     Proxy: ECB MIR rate - 4-quarter log-change in consumption deflator

master <- master %>%
  arrange(date) %>%
  mutate(
    # 4-quarter log change in consumption deflator (annualised inflation)
    infl_4q = (log(cons_deflator) - log(lag(cons_deflator, 4L))),
    # Real rate = nominal - expected inflation (using lagged 4Q actual as proxy)
    real_rate = mortgage_rate / 100 - infl_4q   # mortgage_rate is in %, convert to fraction
  )

# Note on pre-2003 real rate: mort_rate is NA before ECB MIR data.
# Back-extrapolate using average spread of Italian 10-year bond yield over
# inflation, taken from BIS/OECD historical data.
# As a rough proxy, fill pre-2003 NAs with a constant 0.06 (6% real rate)
# typical of Italy's high-rate environment in the 1980s-1990s.
# Researchers should replace this with BdI/ABI mortgage rate data.

# Pre-2003 Italian mortgage rates: very high in the 1980s (>15%), declining
# through the 1990s.  Use ECB 10-year bond yield as fallback.
ecb_lt_rate <- cache_or_fetch("ecb_it_lt_rate", function() {
  # ECB AME long-term government bond yields for Italy
  tryCatch(fetch_ecb_sdw("IRS.Q.IT.L.L40.CI.0.EUR.N.Z"),
           error = function(e) {
             tryCatch(fetch_ecb_sdw("FM.Q.IT.EUR.BB.U2_10Y.LTGBY_RR.MFM.EUR.A"),
                      error = function(e2) NULL)
           })
}, raw_dir_italy)

if (!is.null(ecb_lt_rate) && nrow(ecb_lt_rate) > 0) {
  # ECB SDW returns TIME_PERIOD and OBS_VALUE columns; parse to date/value
  tc  <- intersect(c("TIME_PERIOD", "time_period"), names(ecb_lt_rate))[1]
  vc  <- intersect(c("OBS_VALUE",  "obs_value"),  names(ecb_lt_rate))[1]
  if (!is.na(tc) && !is.na(vc)) {
    lt_rate_q <- ecb_lt_rate %>%
      mutate(
        date      = parse_ecb_time(.data[[tc]]),
        lt_rate_raw = suppressWarnings(as.numeric(.data[[vc]]))
      ) %>%
      filter(!is.na(date), !is.na(lt_rate_raw)) %>%
      select(date, lt_rate_raw)
    master <- master %>%
      left_join(lt_rate_q, by = "date") %>%
      mutate(
        lt_real_rate = lt_rate_raw / 100 - infl_4q,
        real_rate    = ifelse(is.na(real_rate) & !is.na(lt_real_rate),
                              lt_real_rate, real_rate)
      )
  }
}

# Final fallback: simple historical approximation for pre-1999
master <- master %>%
  mutate(
    real_rate = ifelse(is.na(real_rate), {
      # Very rough: declining from ~0.08 in 1980 to ~0.04 in 1999
      yr <- year(date)
      ifelse(yr < 1999,
             0.08 - (yr - 1980) * (0.04 / 19),   # linear decline
             NA_real_)
    }, real_rate)
  )

message("  Real interest rate: ",
        sum(!is.na(master$real_rate)), " non-NA obs")

# 3.8 Credit conditions index
#     cci_ratio is log(credit/8q-MA GDP) from Part 1 — already in log space.
#     For the ECM short-run term Δ²log(CCI_{t-2}) we just need the log level.
master <- master %>%
  mutate(
    log_cci = cci_ratio,  # already log
    cci     = exp(coalesce(cci_ratio, 0))  # level for any direct use
  )

# 3.9 Public consumption (log per capita real)
master <- master %>%
  mutate(
    log_public_cons = ifelse(!is.na(gov_cons_real_pc) & gov_cons_real_pc > 0,
                             log(gov_cons_real_pc),
                             NA_real_)
  )

# 3.10 Assemble model_data with paper-consistent names
model_data <- master %>%
  select(
    date,
    # Core consumption and income
    lcons         = ln_cons_real_pc,
    lincome       = ln_ydi_real_pc,
    dlcons        = d_ln_cons_pc,
    # Unemployment
    unemp_rate,
    # Interest rate
    real_rate,
    # Credit conditions
    cci,
    log_cci,
    # House price
    ln_hp_over_y,
    # Asset/income ratios
    nla_y,
    bonds_y,
    ilfa_y,
    ha_y,
    nfa_y,
    networth_y,
    ln_networth_y,
    # Public consumption
    log_public_cons,
    # Extras passed through
    cons_deflator,
    hpi_real,
    ydi_real_pc,
    y_annual_pc,
    pop_millions
  ) %>%
  arrange(date) %>%
  # Construct ln(y/c_{t-1}) = lincome_t - lcons_{t-1}
  mutate(
    ln_y_over_c = lincome - lag(lcons, 1L)
  )

n_complete <- sum(complete.cases(model_data %>%
                                   select(dlcons, lcons, lincome, ln_y_over_c,
                                          unemp_rate, real_rate)))
message("  model_data rows: ", nrow(model_data),
        "  complete for core vars: ", n_complete)


# ==============================================================================
# STEP 4: Run estimation framework
# ==============================================================================

message("\n", strrep("=", 70))
message("STEP 4: Running estimation framework")
message(strrep("=", 70), "\n")

source(file.path(project_root, "R", "italy_estimation.R"), local = FALSE)
# The estimation script sources model_data, runs Steps 1-6, and writes outputs.


message("\n", strrep("=", 70))
message("Italy consumption model complete.")
message("Outputs written to: ", output_dir)
message(strrep("=", 70), "\n")
