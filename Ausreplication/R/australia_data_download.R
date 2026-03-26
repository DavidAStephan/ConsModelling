#!/usr/bin/env Rscript
# ==============================================================================
# Australia Household Consumption Model — Part 1: Data Download & Construction
# ==============================================================================
#
# Adaptation of the De Bonis / Muellbauer Italy ECM framework for Australia.
# Reference: Duca, Muellbauer et al. (Australia system paper)
#
# Data sources (all public):
#   - ABS 5206008  Household Final Consumption Expenditure (chain vol + current)
#   - ABS 5206020  Household Income Account (GDI, mortgage interest, wages)
#   - ABS 5232035  Household Balance Sheet (financial assets, liabilities,
#                  residential land & dwellings)
#   - ABS 641601   Residential Property Price Indexes (bridge to 2003)
#   - ABS 643201   Total Value of Dwellings (mean price, current)
#   - ABS 560101   Lending Indicators (housing credit flow, FHB share)
#   - ABS 6202001  Labour Force (unemployment rate, population 15+)
#   - ABS 3101059  Estimated Resident Population (for per-capita deflation)
#   - RBA f06hist  Standard Variable Mortgage Rate (historical)
#   - houseprice_old.csv  Pre-2003 house price back-fill
#
# Output:
#   outputs/australia_model_dataset.csv   — long-format coverage summary
#   outputs/australia_model_dataset.rds   — master tibble for Part 2
#
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(readabs)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(httr)
  library(jsonlite)
})

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------
.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    # Running via Rscript directly
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/",
                               mustWork = FALSE)
raw_dir    <- file.path(project_root, "data_raw")
output_dir <- file.path(project_root, "outputs")
cache_dir  <- file.path(project_root, ".cache")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir,  recursive = TRUE, showWarnings = FALSE)

source(file.path(project_root, "R", "model_helpers.R"), local = TRUE)

# ==============================================================================
# HELPERS
# ==============================================================================

# Cache helper — identical interface to italy_data_download.R
get_cached <- function(tag) {
  path <- file.path(cache_dir, paste0(tag, ".rds"))
  if (file.exists(path)) {
    message("  [cached] ", tag)
    return(readRDS(path))
  }
  NULL
}
save_cached <- function(obj, tag) {
  saveRDS(obj, file.path(cache_dir, paste0(tag, ".rds")))
  invisible(obj)
}

# Quarter start date from an Excel date serial (ABS workbooks)
# ABS quarterly dates use the FIRST DAY OF THE LAST MONTH of each calendar
# quarter (Sep 1, Dec 1, Mar 1, Jun 1).  Our spine uses the FIRST DAY OF THE
# FIRST MONTH (Jul 1, Oct 1, Jan 1, Apr 1).  Convert via floor_date().
abs_to_qstart <- function(dates) {
  lubridate::floor_date(as.Date(dates), unit = "quarter")
}

# Monthly → quarterly (mean of 3 monthly obs)
monthly_to_quarterly <- function(df, date_col = "date", value_col = "value") {
  df %>%
    mutate(
      yr  = year(.data[[date_col]]),
      qtr = quarter(.data[[date_col]])
    ) %>%
    group_by(yr, qtr) %>%
    summarise(
      date  = as.Date(sprintf("%04d-%02d-01", first(yr), (first(qtr) - 1L) * 3L + 1L)),
      value = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(date, value)
}

# Annual → quarterly via spline (for population)
annual_to_quarterly_spline <- function(ann_df, spine_dates,
                                       date_col = "date", val_col = "value") {
  ann_df <- ann_df %>% arrange(.data[[date_col]])
  x_ann  <- as.numeric(ann_df[[date_col]])
  y_ann  <- ann_df[[val_col]]
  x_q    <- as.numeric(spine_dates)
  vals   <- tryCatch(
    spline(x_ann, y_ann, xout = x_q)$y,
    error = function(e) approx(x_ann, y_ann, xout = x_q, rule = 2)$y
  )
  tibble(date = spine_dates, value = vals)
}

# Pick preferred series from a tidy ABS workbook tibble.
# Always converts dates to quarter-start convention via abs_to_qstart().
pick_abs <- function(raw, pattern,
                     types = c("Seasonally Adjusted", "Seasonally adjusted",
                               "Trend", "Original")) {
  candidates <- raw %>% filter(str_detect(series_name, pattern))
  if (nrow(candidates) == 0L) {
    warning("No ABS series matched: ", pattern)
    return(tibble(date = as.Date(character()), value = numeric()))
  }
  for (tp in types) {
    sub <- candidates %>% filter(series_type == tp)
    if (nrow(sub) > 0L) {
      best_id <- sub %>% count(series_id, sort = TRUE) %>% slice(1L) %>% pull(series_id)
      return(sub %>% filter(series_id == best_id) %>%
               mutate(date = abs_to_qstart(date)) %>%
               arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
               select(date, value))
    }
  }
  candidates %>%
    mutate(date = abs_to_qstart(date)) %>%
    arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
    select(date, value)
}

report_series <- function(v, name, dates) {
  non_na <- sum(!is.na(v))
  if (non_na == 0L) {
    message("  ", name, ": NO DATA")
    return(invisible(NULL))
  }
  idx   <- which(!is.na(v))
  d_from <- format(dates[min(idx)], "%Y-%m-%d")
  d_to   <- format(dates[max(idx)], "%Y-%m-%d")
  message(sprintf("  %s: %d non-NA obs, %s to %s", name, non_na, d_from, d_to))
}

# ==============================================================================
# SECTION 1: Date spine  (1980Q1 – 2024Q4)
# ==============================================================================

message("\nDate spine: quarterly 1980Q1 to 2024Q4")
spine_start <- as.Date("1980-01-01")
spine_end   <- as.Date("2024-10-01")
spine_dates <- seq(spine_start, spine_end, by = "quarter")
n_spine     <- length(spine_dates)
message("  ", n_spine, " quarters")

master <- tibble(date = spine_dates)

# ==============================================================================
# SECTION 2: Download / read raw ABS series
# ==============================================================================

message("\n--- Section 2: Loading ABS data ---")

# Helper: read workbook with caching
read_abs_cached <- function(path, tag) {
  cached <- get_cached(tag)
  if (!is.null(cached)) return(cached)
  message("  Reading ", basename(path))
  obj <- read_abs_ts_workbook(path)
  save_cached(obj, tag)
  obj
}

## 2.1  Real consumption (chain volume, SA)
message("2.1 Real household consumption (5206008)")
hfce_raw <- read_abs_cached(
  file.path(raw_dir, "5206008_Household_Final_Consumption_Expenditure.xlsx"),
  "abs_hfce"
)
cons_real_q <- pick_abs(hfce_raw,
  "^FINAL CONSUMPTION EXPENDITURE.*Chain volume",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>% rename(cons_real = value)
message(sprintf("  cons_real: %d obs, %s to %s",
  nrow(cons_real_q),
  format(min(cons_real_q$date), "%Y-%m-%d"),
  format(max(cons_real_q$date), "%Y-%m-%d")))

## 2.2  Nominal consumption (current prices, SA)
message("2.2 Nominal household consumption (5206008)")
cons_nom_q <- pick_abs(hfce_raw,
  "^FINAL CONSUMPTION EXPENDITURE.*Current prices",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>% rename(cons_nom = value)
message(sprintf("  cons_nom: %d obs", nrow(cons_nom_q)))

## 2.3  Gross disposable income (5206020)
message("2.3 Gross disposable income (5206020)")
inc_raw <- read_abs_cached(
  file.path(raw_dir, "5206020_Household_Income.xlsx"),
  "abs_hh_income"
)

# Try two possible series IDs that ABS uses for GDI across vintages
gdi_q <- tryCatch({
  # Prefer nominal GDI (current prices, SA)
  cands <- inc_raw %>% filter(str_detect(series_name, regex(
    "gross disposable income", ignore_case = TRUE)))
  if (nrow(cands) == 0L) stop("not found")
  best_id <- cands %>%
    filter(series_type %in% c("Seasonally Adjusted", "Seasonally adjusted")) %>%
    count(series_id, sort = TRUE) %>% slice(1L) %>% pull(series_id)
  cands %>% filter(series_id == best_id) %>%
    mutate(date = abs_to_qstart(date)) %>%
    arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
    select(date, value) %>% rename(ydi_nom = value)
}, error = function(e) {
  message("  GDI not found by name, trying fallback series_id A2302939L")
  inc_raw %>% filter(series_id == "A2302939L") %>%
    mutate(date = abs_to_qstart(date)) %>%
    arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
    select(date, value) %>% rename(ydi_nom = value)
})
message(sprintf("  ydi_nom: %d obs, %s to %s",
  nrow(gdi_q),
  format(min(gdi_q$date), "%Y-%m-%d"),
  format(max(gdi_q$date), "%Y-%m-%d")))

## 2.4  Mortgage interest paid (5206020) — for real rate construction
message("2.4 Mortgage interest paid (5206020)")
mort_int_q <- pick_abs(inc_raw,
  regex("property income payable.*interest.*dwelling|mortgage interest",
        ignore_case = TRUE),
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend", "Original")
) %>% rename(mort_int_paid = value)
if (nrow(mort_int_q) == 0L)
  message("  mortgage interest paid: not found — will use RBA rate series")

## 2.5  Population (3101059 — sum single-year age cohorts, annual → spline)
message("2.5 Population (3101059)")
pop_raw <- read_abs_cached(
  file.path(raw_dir, "3101059.xlsx"),
  "abs_pop"
)
# 3101059 contains one series per single-year age cohort.
# Sum all cohorts to get total ERP, then spline annual obs to quarterly.
pop_ann <- pop_raw %>%
  filter(str_detect(series_name,
    "^Estimated Resident Population ; Persons ; ")) %>%
  mutate(date = abs_to_qstart(date)) %>%
  group_by(date) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(date) %>%
  filter(value > 0)

if (nrow(pop_ann) >= 10L) {
  pop_q <- annual_to_quarterly_spline(pop_ann, spine_dates) %>%
    rename(pop_thousands = value) %>%
    mutate(pop_millions = pop_thousands / 1000)
  message(sprintf("  pop: %d quarterly obs via spline (from %d source obs)",
                  nrow(pop_q), nrow(pop_ann)))
} else {
  message("  pop: age-cohort series not found; falling back to working-age pop")
  pop_q <- pop_raw %>%
    filter(str_detect(series_name,
      regex("civilian population aged 15|working.age|persons.15", ignore_case = TRUE))) %>%
    filter(series_type %in% c("Original", "Seasonally Adjusted")) %>%
    mutate(date = abs_to_qstart(date)) %>%
    group_by(date) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    rename(pop_thousands = value) %>%
    mutate(pop_millions = pop_thousands / 1000)
  message(sprintf("  pop: %d obs (working-age fallback)", nrow(pop_q)))
}

## 2.6  Unemployment rate (6202001)
message("2.6 Unemployment rate (6202001)")
labour_raw <- read_abs_cached(
  file.path(raw_dir, "6202001.xlsx"),
  "abs_labour"
)
unemp_m <- pick_abs(labour_raw,
  "^Unemployment rate.*Persons",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend")
) %>% rename(unemp_rate = value)
unemp_q <- monthly_to_quarterly(unemp_m, value_col = "unemp_rate") %>%
  rename(unemp_rate = value)
message(sprintf("  unemp_rate: %d obs, %s to %s",
  nrow(unemp_q),
  format(min(unemp_q$date), "%Y-%m-%d"),
  format(max(unemp_q$date), "%Y-%m-%d")))

## 2.7  CPI / consumption deflator
message("2.7 Consumption deflator (from 5206008 nominal / real)")
deflator_q <- cons_real_q %>%
  inner_join(cons_nom_q, by = "date") %>%
  mutate(cons_deflator = 100 * cons_nom / cons_real) %>%
  select(date, cons_deflator)
message(sprintf("  cons_deflator: %d obs", nrow(deflator_q)))

## 2.8  Household balance sheet (5232035)
message("2.8 Household balance sheet (5232035)")
hh_bs_raw <- read_abs_cached(
  file.path(raw_dir, "5232035.xlsx"),
  "abs_hh_bs"
)

# Helper: rescale ABS $ millions / billions to common unit ($ millions)
rescale_abs <- function(df, unit_col = "unit") {
  if (!"unit" %in% names(df)) return(df %>% select(date, value))
  df %>%
    mutate(value = case_when(
      str_detect(unit, regex("billion",  ignore_case = TRUE)) ~ value * 1000,
      str_detect(unit, regex("million",  ignore_case = TRUE)) ~ value,
      str_detect(unit, regex("thousand", ignore_case = TRUE)) ~ value / 1000,
      TRUE ~ value
    )) %>%
    select(date, value)
}

pick_abs_bs <- function(pattern) {
  cands <- hh_bs_raw %>% filter(str_detect(series_name, regex(pattern, ignore_case = TRUE)))
  if (nrow(cands) == 0L) {
    warning("Balance sheet series not found: ", pattern)
    return(tibble(date = as.Date(character()), value = NA_real_))
  }
  best_id <- cands %>%
    filter(series_type %in% c("Original", "Seasonally Adjusted", "Trend")) %>%
    count(series_id, sort = TRUE) %>% slice(1L) %>% pull(series_id)
  if (length(best_id) == 0L) best_id <- cands %>% count(series_id, sort = TRUE) %>%
    slice(1L) %>% pull(series_id)
  raw_df <- cands %>% filter(series_id == best_id) %>%
    mutate(date = abs_to_qstart(date)) %>%
    arrange(date) %>% distinct(date, .keep_all = TRUE)
  rescale_abs(raw_df)
}

# Financial assets
fin_deposits_q   <- pick_abs_bs("currency and deposits") %>% rename(fin_deposits = value)
fin_equities_q   <- pick_abs_bs("shares and other equity") %>% rename(fin_equities = value)
fin_super_q      <- pick_abs_bs("superannuation") %>% rename(fin_super = value)
fin_other_fin_q  <- pick_abs_bs("other financial assets|other accounts receivable") %>%
  rename(fin_other = value)
# Liabilities
fin_loans_q      <- pick_abs_bs("liabilities.*loans|loans and placements") %>%
  rename(fin_loans = value)
# Non-financial: residential land & dwellings
housing_wealth_q <- pick_abs_bs("residential land and dwellings") %>%
  rename(housing_wealth = value)
# Total net worth: ABS series A83722648X from 5232035 (same workbook already loaded)
message("2.8b Household net worth (5232035 – A83722648X)")
networth_abs_raw <- hh_bs_raw %>%
  filter(series_id == "A83722648X") %>%
  mutate(date = abs_to_qstart(date)) %>%
  arrange(date) %>% distinct(date, .keep_all = TRUE)
if (nrow(networth_abs_raw) > 0L) {
  networth_abs_q <- rescale_abs(networth_abs_raw) %>% rename(closing_net_worth = value)
  message(sprintf("  closing net worth (A83722648X): %d obs, %s to %s",
    nrow(networth_abs_q),
    format(min(networth_abs_q$date), "%Y-%m-%d"),
    format(max(networth_abs_q$date), "%Y-%m-%d")))
} else {
  networth_abs_q <- tibble(date = as.Date(character()), closing_net_worth = numeric())
  message("  A83722648X not found in 5232035 — net worth will be computed from components")
}

for (nm in c("fin_deposits", "fin_equities", "fin_super", "fin_loans",
             "housing_wealth")) {
  obj <- get(paste0(nm, "_q"))
  if (nrow(obj) > 0L)
    message(sprintf("  %s: %d obs, %s to %s", nm, nrow(obj),
      format(min(obj$date), "%Y-%m-%d"), format(max(obj$date), "%Y-%m-%d")))
  else
    message("  ", nm, ": NOT FOUND")
}

## 2.9  Mortgage interest rate
message("2.9 Mortgage interest rate")
# Primary: RBA series FILRHLBVS (standard variable owner-occupier rate) via readrba.
# Fallback: implicit rate from ABS mortgage interest paid / lagged debt stock.
mort_rate_q <- NULL

if (requireNamespace("readrba", quietly = TRUE)) {
  tryCatch({
    message("  Fetching FILRHLBVS via readrba...")
    rba_series <- readrba::read_rba_seriesid("FILRHLBVS")
    if (!is.null(rba_series) && nrow(rba_series) > 0L) {
      mort_rate_m <- rba_series %>%
        select(date, value) %>%
        filter(!is.na(value), date >= as.Date("1970-01-01")) %>%
        arrange(date)
      mort_rate_q <- monthly_to_quarterly(mort_rate_m) %>%
        rename(mortgage_rate = value)
      message(sprintf("  mortgage_rate (RBA FILRHLBVS): %d obs, %s to %s",
        nrow(mort_rate_q),
        format(min(mort_rate_q$date), "%Y-%m-%d"),
        format(max(mort_rate_q$date), "%Y-%m-%d")))
    } else {
      message("  FILRHLBVS returned no data from readrba")
    }
  }, error = function(e) message("  readrba fetch failed: ", e$message))
} else {
  message("  readrba not installed — install with install.packages('readrba')")
}

# Fallback: implicit rate from ABS mortgage interest / lagged debt stock
if (is.null(mort_rate_q) || nrow(mort_rate_q) == 0L) {
  message("  Using implicit ABS rate (mort_int / lagged debt)")
  if (nrow(mort_int_q) > 0L && nrow(fin_loans_q) > 0L) {
    mort_rate_q <- mort_int_q %>%
      inner_join(fin_loans_q, by = "date") %>%
      arrange(date) %>%
      # Both mort_int_paid (5206020) and fin_loans (5232035, after rescale_abs) are in $ million.
      mutate(mortgage_rate = 400 * mort_int_paid / lag(fin_loans, 1L)) %>%
      filter(!is.na(mortgage_rate)) %>%
      select(date, mortgage_rate)
    message(sprintf("  mortgage_rate (implicit fallback): %d obs", nrow(mort_rate_q)))
  } else {
    mort_rate_q <- tibble(date = as.Date(character()), mortgage_rate = numeric())
    message("  mortgage_rate: insufficient data for implicit rate")
  }
}

## 2.9b RBA Cash Rate (for CCI back-extension to 1980s)
message("2.9b RBA Cash Rate")
cash_rate_q <- NULL

if (requireNamespace("readrba", quietly = TRUE)) {
  # Try daily target cash rate (FIRMMCRTD), fall back to overnight rate (FOOIRATCR)
  for (sid in c("FIRMMCRTD", "FOOIRATCR")) {
    tryCatch({
      message(sprintf("  Fetching %s via readrba...", sid))
      rba_cash <- readrba::read_rba_seriesid(sid)
      if (!is.null(rba_cash) && nrow(rba_cash) > 0L) {
        cash_rate_q <- rba_cash %>%
          select(date, value) %>%
          filter(!is.na(value), date >= as.Date("1970-01-01")) %>%
          arrange(date) %>%
          mutate(qdate = lubridate::floor_date(date, "quarter")) %>%
          group_by(date = qdate) %>%
          summarise(cash_rate = mean(value, na.rm = TRUE), .groups = "drop") %>%
          arrange(date)
        message(sprintf("  cash_rate (%s): %d quarterly obs, %s to %s",
          sid, nrow(cash_rate_q),
          format(min(cash_rate_q$date), "%Y-%m-%d"),
          format(max(cash_rate_q$date), "%Y-%m-%d")))
        break
      }
    }, error = function(e) message(sprintf("  %s fetch failed: %s", sid, e$message)))
  }
} else {
  message("  readrba not installed — CCI will use housing loan flow only (from 2002)")
}

## 2.10  House price index
message("2.10 House price index")
# Current: ABS 643201 mean dwelling price (preferred — longer and more
# comprehensive than 641601 which only goes to 2003)
tvd_path <- file.path(raw_dir, "643201.xlsx")
hpi_current_q <- tibble(date = as.Date(character()), value = numeric())
if (file.exists(tvd_path)) {
  tryCatch({
    tvd_raw <- read_abs_cached(tvd_path, "abs_tvd")
    hpi_current_q <- pick_abs(tvd_raw,
      "Mean price of residential dwellings.*Australia",
      types = c("Original", "Seasonally Adjusted"))
    message(sprintf("  hpi (643201 mean price): %d obs, %s to %s",
      nrow(hpi_current_q),
      format(min(hpi_current_q$date), "%Y-%m-%d"),
      format(max(hpi_current_q$date), "%Y-%m-%d")))
  }, error = function(e) message("  643201 parse error: ", e$message))
}

# Bridge: ABS 641601 RPPI (capital cities, back to 2003)
bridge_path <- file.path(raw_dir, "641601.xlsx")
hpi_bridge_q <- tibble(date = as.Date(character()), value = numeric())
if (file.exists(bridge_path)) {
  tryCatch({
    bridge_raw <- read_abs_cached(bridge_path, "abs_rppi")
    hpi_bridge_q <- pick_abs(bridge_raw,
      "Residential Property Price Index.*eight capital cities",
      types = c("Original", "Seasonally Adjusted"))
    message(sprintf("  hpi (641601 RPPI bridge): %d obs", nrow(hpi_bridge_q)))
  }, error = function(e) message("  641601 parse error: ", e$message))
}

# Legacy: houseprice_old.csv (pre-2003, from model_helpers splice)
legacy_path <- file.path(raw_dir, "houseprice_old.csv")
hpi_legacy_q <- tibble(date = as.Date(character()), value = numeric())
if (file.exists(legacy_path)) {
  tryCatch({
    leg <- read.csv(legacy_path, stringsAsFactors = FALSE)
    # Dates are in "Jun-1986" format (month-abbrev + year)
    date_col <- names(leg)[1]
    val_col  <- names(leg)[ncol(leg)]
    raw_dates <- leg[[date_col]]
    parsed_dates <- tryCatch(
      as.Date(paste0("01-", raw_dates), format = "%d-%b-%Y"),
      error = function(e) as.Date(rep(NA, length(raw_dates)))
    )
    # abs_to_qstart converts "01 Jun 1986" → "1986-04-01" (Q2 start)
    hpi_legacy_q <- tibble(
      date  = abs_to_qstart(parsed_dates),
      value = as.numeric(leg[[val_col]])
    ) %>% filter(!is.na(date), !is.na(value)) %>% arrange(date)
    message(sprintf("  hpi (legacy): %d obs, %s to %s",
      nrow(hpi_legacy_q),
      format(min(hpi_legacy_q$date), "%Y-%m-%d"),
      format(max(hpi_legacy_q$date), "%Y-%m-%d")))
  }, error = function(e) message("  houseprice_old.csv parse error: ", e$message))
}

# Splice: chain link base onto overlay (both must have a `value` column)
splice_hpi <- function(base, overlay) {
  # Normalise to `value` column in both
  norm <- function(df) {
    non_date <- setdiff(names(df), "date")
    df %>% rename(value = !!non_date[1])
  }
  base    <- norm(base)
  overlay <- norm(overlay)
  overlap <- inner_join(base, overlay, by = "date", suffix = c("_base", "_ov")) %>%
    filter(!is.na(value_base), !is.na(value_ov))
  if (nrow(overlap) < 4L) return(overlay)
  scale  <- mean(overlap$value_ov / overlap$value_base, na.rm = TRUE)
  before <- base %>%
    filter(date < min(overlay$date[!is.na(overlay$value)])) %>%
    mutate(value = value * scale)
  bind_rows(before, overlay) %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
}

# Build spliced series: legacy → bridge → current (all as `hpi` column)
hpi_spliced <- hpi_current_q %>% rename(hpi = value)
if (nrow(hpi_bridge_q) > 0L && nrow(hpi_current_q) > 0L) {
  hpi_spliced <- splice_hpi(hpi_bridge_q, hpi_current_q) %>% rename(hpi = value)
}
if (nrow(hpi_legacy_q) > 0L && nrow(hpi_spliced) > 0L) {
  hpi_spliced <- splice_hpi(hpi_legacy_q, hpi_spliced) %>% rename(hpi = value)
}
message(sprintf("  hpi (spliced): %d non-NA obs, %s to %s",
  sum(!is.na(hpi_spliced$hpi)),
  format(min(hpi_spliced$date[!is.na(hpi_spliced$hpi)]), "%Y-%m-%d"),
  format(max(hpi_spliced$date[!is.na(hpi_spliced$hpi)]), "%Y-%m-%d")))

## 2.11  Housing credit flow & FHB share (560101 — credit conditions proxy)
message("2.11 Housing credit flow (560101)")
credit_raw <- read_abs_cached(
  file.path(raw_dir, "560101.xlsx"),
  "abs_credit"
)
housing_loan_flow_m <- pick_abs(credit_raw,
  "Households.*Housing Finance.*Total dwellings.*New loan commitments.*Value",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Original")
) %>% rename(housing_loan_flow = value)
housing_loan_flow_q <- monthly_to_quarterly(housing_loan_flow_m,
                                             value_col = "housing_loan_flow") %>%
  rename(housing_loan_flow = value)

fhb_m <- pick_abs(credit_raw,
  "First Home Buyers.*New loan commitments.*Number",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Original")
) %>% rename(fhb_loans = value)
non_fhb_m <- pick_abs(credit_raw,
  "Non-First Home Buyers.*New loan commitments.*Number",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Original")
) %>% rename(non_fhb_loans = value)

fhb_q <- monthly_to_quarterly(fhb_m, value_col = "fhb_loans") %>%
  rename(fhb_loans = value)
non_fhb_q <- monthly_to_quarterly(non_fhb_m, value_col = "non_fhb_loans") %>%
  rename(non_fhb_loans = value)

message(sprintf("  housing_loan_flow: %d obs", nrow(housing_loan_flow_q)))

# ==============================================================================
# SECTION 3: Assemble master dataset
# ==============================================================================

message("\n--- Section 3: Assembling master dataset ---")

# Join all series to spine
master <- master %>%
  left_join(cons_real_q,       by = "date") %>%   # $m, chain vol
  left_join(cons_nom_q,        by = "date") %>%   # $m, current
  left_join(deflator_q,        by = "date") %>%   # index, 2023=100
  left_join(gdi_q,             by = "date") %>%   # $m, current, quarterly
  left_join(unemp_q,           by = "date") %>%   # %
  left_join(pop_q %>% select(date, pop_millions), by = "date") %>%
  left_join(fin_deposits_q,    by = "date") %>%   # $m
  left_join(fin_equities_q,    by = "date") %>%   # $m
  left_join(fin_super_q,       by = "date") %>%   # $m
  left_join(fin_loans_q,       by = "date") %>%   # $m
  left_join(housing_wealth_q,  by = "date") %>%   # $m
  left_join(networth_abs_q,    by = "date") %>%   # $m (ABS A83722648X, if found)
  left_join(mort_rate_q,       by = "date") %>%   # % p.a.
  left_join(hpi_spliced,       by = "date") %>%   # index
  left_join(housing_loan_flow_q %>% select(date, housing_loan_flow), by = "date") %>%
  left_join(fhb_q     %>% select(date, fhb_loans),     by = "date") %>%
  left_join(non_fhb_q %>% select(date, non_fhb_loans), by = "date")

# Deflate to real (2015 price basis)
# Normalise deflator to 2015 = 100
base_yr  <- 2015L
base_def <- master %>%
  filter(year(date) == base_yr) %>%
  summarise(m = mean(cons_deflator, na.rm = TRUE)) %>%
  pull(m)
if (is.na(base_def) || base_def == 0) base_def <- 100

master <- master %>%
  mutate(
    cons_deflator_norm = cons_deflator / base_def * 100,
    # Real per capita variables (2015 $m per million people → $ per person)
    cons_real_pc      = cons_real      / pop_millions,           # chain vol, per person
    ydi_real_pc       = (ydi_nom / (cons_deflator_norm / 100)) / pop_millions,
    housing_wealth_r  = housing_wealth / (cons_deflator_norm / 100),
    fin_deposits_r    = fin_deposits   / (cons_deflator_norm / 100),
    fin_equities_r    = fin_equities   / (cons_deflator_norm / 100),
    fin_super_r       = fin_super      / (cons_deflator_norm / 100),
    fin_loans_r       = fin_loans      / (cons_deflator_norm / 100)
  )

# Annualised income (quarterly flow × 4) for ratio construction
master <- master %>%
  mutate(
    ydi_ann_nom = ydi_nom * 4,                    # annual nominal GDI, $m
    ydi_ann_r   = (ydi_ann_nom / (cons_deflator_norm / 100)) / pop_millions
  )

# Wealth-to-income ratios (as in paper — ratios to annual income)
# Both 5232035 (after rescale_abs converts $ billion → $ million) and 5206020 income
# are in $ million, so the ratio is dimensionless. No extra scaling needed.
# Superannuation is split from equities: super is illiquid/mandatory; equities are more liquid.
master <- master %>%
  mutate(
    eq_y           = fin_equities                / ydi_ann_nom,   # equities
    super_y        = fin_super                   / ydi_ann_nom,   # superannuation
    ilfa_y         = eq_y + super_y,                              # combined (for Specs 1–3)
    nla_y          = (fin_deposits - fin_loans)  / ydi_ann_nom,   # net liquid assets
    ha_y           = housing_wealth              / ydi_ann_nom,   # housing assets
    debt_y         = fin_loans                   / ydi_ann_nom,
    # Use ABS official net worth (A83722648X) if available; otherwise sum components
    networth_y     = if_else(
      !is.na(closing_net_worth),
      closing_net_worth / ydi_ann_nom,
      (fin_deposits + fin_equities + fin_super + housing_wealth - fin_loans) / ydi_ann_nom
    )
  )

# Log versions
master <- master %>%
  mutate(
    ln_cons_real_pc = log(cons_real_pc),
    ln_ydi_real_pc  = log(ydi_real_pc),
    ln_hpi          = log(hpi),
    # log(y/c): ECM adjustment speed target
    ln_y_over_c     = ln_ydi_real_pc - ln_cons_real_pc,
    # log(HP / y): house price relative to income
    ln_hp_over_y    = log(hpi / (ydi_ann_nom / pop_millions / (cons_deflator_norm/100))),
    ln_networth_y   = log(pmax(networth_y, 1e-6)),
    # First differences
    d_ln_cons_pc    = c(NA_real_, diff(ln_cons_real_pc)),
    d_ln_ydi_pc     = c(NA_real_, diff(ln_ydi_real_pc))
  )

# Real interest rate: nominal mortgage rate minus 4-quarter inflation
master <- master %>%
  mutate(
    hicp_4q_ann  = 100 * (cons_deflator_norm / lag(cons_deflator_norm, 4L) - 1),
    real_rate    = mortgage_rate - hicp_4q_ann
  )

# CCI proxy: log(housing credit flow / 8q moving average GDP proxy)
# (GDP proxy = 4 × quarterly income)
master <- master %>%
  mutate(
    ydi_ann_8qma = zoo::rollmean(ydi_ann_nom, 8L, fill = NA, align = "right"),
    cci_ratio    = log(housing_loan_flow / ydi_ann_8qma)
  )

# Extend CCI backwards using mortgage-to-cash-rate spread
# The spread captures credit tightness pre-2002 when housing flow data is absent.
# Normalised in the 2002-2024 overlap so scale is consistent with cci_ratio.
if (!is.null(cash_rate_q) && nrow(cash_rate_q) > 0L) {
  master <- master %>%
    left_join(cash_rate_q, by = "date") %>%
    mutate(mort_spread = mortgage_rate - cash_rate)

  overlap <- master %>% filter(!is.na(cci_ratio), !is.na(mort_spread))
  n_pre   <- sum(is.na(master$cci_ratio) & !is.na(master$mort_spread))
  message(sprintf("  CCI overlap period: %d obs; pre-2002 to backfill: %d obs",
                  nrow(overlap), n_pre))

  if (nrow(overlap) >= 20L && n_pre > 0L) {
    s_mn <- mean(overlap$mort_spread, na.rm = TRUE)
    s_sd <- sd(overlap$mort_spread,   na.rm = TRUE)
    c_mn <- mean(overlap$cci_ratio,   na.rm = TRUE)
    c_sd <- sd(overlap$cci_ratio,     na.rm = TRUE)

    master <- master %>%
      mutate(
        cci_spread_norm = c_sd * ((mort_spread - s_mn) / s_sd) + c_mn,
        cci_ratio = ifelse(!is.na(cci_ratio), cci_ratio, cci_spread_norm)
      ) %>%
      select(-cci_spread_norm, -mort_spread, -cash_rate)

    message(sprintf("  CCI extended: %d total obs (housing-flow 2002+, spread proxy pre-2002)",
                    sum(!is.na(master$cci_ratio))))
  } else {
    master <- master %>% select(-mort_spread, -cash_rate)
    if (n_pre == 0L)
      message("  CCI: no pre-2002 gap to fill — keeping housing-flow series only")
    else
      message("  CCI extension: insufficient overlap — keeping housing-flow only")
  }
}

# FHB share
master <- master %>%
  mutate(
    fhb_share = fhb_loans / (fhb_loans + non_fhb_loans)
  )

# Print coverage
message("\n--- Coverage summary ---")
for (v in c("cons_real_pc", "ydi_real_pc", "unemp_rate", "mortgage_rate",
            "real_rate", "hpi", "ha_y", "eq_y", "super_y", "nla_y", "networth_y",
            "cci_ratio", "fhb_share")) {
  report_series(master[[v]], v, master$date)
}

complete_core <- sum(complete.cases(master %>%
  select(d_ln_cons_pc, ln_y_over_c, real_rate, ln_ydi_real_pc)))
message(sprintf("\n  master rows: %d   complete for core vars: %d",
                nrow(master), complete_core))

# ==============================================================================
# SECTION 4: (No back-extrapolation — using actual ABS data only from 1988Q3)
# ==============================================================================
# Balance sheet data (5232035) is available from 1988Q3. Wealth-to-income ratios
# are NA before that date. The estimation sample for disaggregated specs (4–6)
# therefore starts in 1988Q4 at the earliest (after lagging).
message("\n--- Section 4: No back-extrapolation — actual data only from 1988Q3 ---")

# Sanity-check: print wealth ratio ranges to confirm unit scaling is correct.
# ha_y should be ~3-8 (housing wealth ~3-8x annual income) for Australia.
message("\n--- Wealth ratio sanity check (2024 Q4 values) ---")
last_row <- master %>% filter(!is.na(ha_y)) %>% tail(1)
message(sprintf("  housing_wealth (raw, last obs): %.1f", tail(master$housing_wealth[!is.na(master$housing_wealth)], 1)))
message(sprintf("  ydi_ann_nom    (raw, last obs): %.1f", tail(master$ydi_ann_nom[!is.na(master$ydi_ann_nom)], 1)))
message(sprintf("  ha_y    range: %.2f – %.2f  (mean %.2f)", min(master$ha_y, na.rm=TRUE), max(master$ha_y, na.rm=TRUE), mean(master$ha_y, na.rm=TRUE)))
message(sprintf("  nla_y   range: %.2f – %.2f  (mean %.2f)", min(master$nla_y, na.rm=TRUE), max(master$nla_y, na.rm=TRUE), mean(master$nla_y, na.rm=TRUE)))
message(sprintf("  eq_y    range: %.2f – %.2f  (mean %.2f)", min(master$eq_y, na.rm=TRUE), max(master$eq_y, na.rm=TRUE), mean(master$eq_y, na.rm=TRUE)))
message(sprintf("  super_y range: %.2f – %.2f  (mean %.2f)", min(master$super_y, na.rm=TRUE), max(master$super_y, na.rm=TRUE), mean(master$super_y, na.rm=TRUE)))
message(sprintf("  networth_y range: %.2f – %.2f  (mean %.2f)", min(master$networth_y, na.rm=TRUE), max(master$networth_y, na.rm=TRUE), mean(master$networth_y, na.rm=TRUE)))
if (!is.null(master$mortgage_rate)) {
  mr <- master$mortgage_rate[!is.na(master$mortgage_rate)]
  message(sprintf("  mortgage_rate range: %.2f – %.2f %%  (mean %.2f %%)", min(mr), max(mr), mean(mr)))
}

# ==============================================================================
# SECTION 5: Dummy variables
# ==============================================================================

message("\n--- Section 5: Dummy variables ---")

master <- master %>%
  mutate(
    # GST introduction Q3 2000 — large one-off boost to nominal consumption
    d2000_gst    = as.integer(date == as.Date("2000-07-01")),
    # Global Financial Crisis (sharp consumption drop Q3-Q4 2008)
    d2008_gfc    = as.integer(date == as.Date("2008-07-01")),
    # COVID lockdown Q2 2020 (consumption collapse)
    d2020_covid  = as.integer(date == as.Date("2020-04-01")),
    # COVID rebound Q3 2020 (partial mechanical reversal)
    d2020_rebound = as.integer(date == as.Date("2020-07-01"))
  )

# ==============================================================================
# SECTION 6: Save outputs
# ==============================================================================

message("\n--- Section 6: Saving outputs ---")

# Coverage table
coverage <- tibble(
  variable = names(master)[names(master) != "date"],
  n_obs    = sapply(names(master)[names(master) != "date"],
                    function(v) sum(!is.na(master[[v]]))),
  date_from = sapply(names(master)[names(master) != "date"], function(v) {
    idx <- which(!is.na(master[[v]]))
    if (length(idx) == 0L) return(NA_character_)
    format(master$date[min(idx)], "%Y-%m-%d")
  }),
  date_to = sapply(names(master)[names(master) != "date"], function(v) {
    idx <- which(!is.na(master[[v]]))
    if (length(idx) == 0L) return(NA_character_)
    format(master$date[max(idx)], "%Y-%m-%d")
  })
)

write.csv(coverage, file.path(output_dir, "australia_model_dataset.csv"),
          row.names = FALSE)
saveRDS(master,     file.path(output_dir, "australia_model_dataset.rds"))
message("  Saved: ", file.path(output_dir, "australia_model_dataset.csv"))
message("  Saved: ", file.path(output_dir, "australia_model_dataset.rds"))

message("\nPart 1 complete.")
