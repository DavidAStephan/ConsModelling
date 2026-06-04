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
#   - RBA d02hist  Lending and Credit Aggregates (total credit 1976Q3+)
#   - RBA d03hist  Monetary Aggregates (M3 1959Q3+)
#   - houseprice_old.csv  Pre-2003 house price back-fill
#   - house_price_history_long.csv  Treasury TRYM model historical
#                  nominal national house price index (1959Q3+)
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

# Two independent credit-conditions switches (decoupled 2026-06, NS-131).
# Previously a single overloaded `USE_INSTITUTIONAL_CCI` flag gated both of the
# following, which made the canonical configuration (basis ON, overlay OFF)
# impossible to reproduce from a cold rebuild.
#
# USE_WILLIAMS_SDMMA_BASIS — attach the 15-knot Williams SDMMA candidate basis
#   to `master` so the iterated fit_consumption_with_williams_cci() can build
#   `cci_williams` and Specs 8/9/10 estimate. CANONICAL = TRUE.
USE_WILLIAMS_SDMMA_BASIS <- TRUE
#
# USE_INSTITUTIONAL_CCI_OVERLAY — backfill `cci_ratio` pre-2002 with the
#   Muellbauer-style regime+indicator blend (construct_institutional_cci). This
#   changes the Spec 2/5/6 short-run CCI term and shifts Spec 6 λ (−0.180 with
#   the overlay OFF → −0.188 with it ON). CANONICAL = FALSE: the specs that use
#   cci_ratio start at the 2002Q3 housing-flow anchor.
USE_INSTITUTIONAL_CCI_OVERLAY <- FALSE

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

# Read an RBA D-table by series ID. Standard RBA workbook layout:
#   row 1  : table title
#   row 2  : column titles
#   row 4  : frequency
#   row 6  : units
#   row 11 : series IDs
#   row 12+: data, col 1 = Excel-serial date, col k = value for series k
read_rba_d_table <- function(path, series_id, sheet = "Data") {
  df <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE))
  ids <- as.character(df[11, ])
  col_idx <- which(ids == series_id)[1]
  if (is.na(col_idx)) stop("series_id '", series_id, "' not found in ", path)
  raw <- df[12:nrow(df), c(1, col_idx), drop = FALSE]
  tibble(
    date  = as.Date(suppressWarnings(as.numeric(unlist(raw[[1]]))),
                    origin = "1899-12-30"),
    value = suppressWarnings(as.numeric(unlist(raw[[2]])))
  ) %>% filter(!is.na(date), !is.na(value)) %>% arrange(date)
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
# SECTION 1: Date spine  (1976Q3 – 2024Q4)
# ==============================================================================
# Spine extended back to 1976Q3 (NS-020 phase 1 target). Variables sourced
# from public data that cover the full window:
#   - cons / income (ABS 5206)        : 1959Q3+
#   - mortgage rate (RBA F6 hist)     : 1959Q3+
#   - house price (TRYM splice)       : 1959Q3+
#   - M3 monetary aggregate (RBA D03) : 1959Q3+
#   - total credit (RBA D02 splice)   : 1976Q3+
# Variables that remain NA pre-1978Q1 (ABS 6202 binding constraint):
#   - unemployment rate, labour force, prime_age_share
# Variables that remain NA pre-1988Q3 (ABS 5232 sectoral accounts):
#   - disaggregated wealth (ha_y, eq_y, super_y, nla_y)
#   - networth_y, debt_y
# Specs that need wealth disaggregation are unaffected by the back-extension;
# Spec 1-3 (aggregate net-worth) are the natural beneficiaries once an
# aggregate net-worth proxy is built from M3 + housing wealth.

message("\nDate spine: quarterly 1976Q3 to 2024Q4")
spine_start <- as.Date("1976-07-01")
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

## 2.4b  Household income components (data_raw/household_income.csv)
##   - compensation_of_employees: nominal $m, quarterly
##   - prop_inc_rec:               property income receivable, $m
##   - social_assisatance_ben:     social assistance benefits in cash, $m  [note typo in source]
##   - prop_inc_pay:               property income payable, $m (= mortgage interest etc.)
## Used to construct labour+transfer income for the Italy-style scaled-income
## robustness column (Italy.pdf §3.1: scaled_income = mean of disposable
## income and labour-plus-transfer income, down-weighting mismeasured property
## income).
message("2.4b Household income components (CSV)")
hh_inc_csv <- file.path(raw_dir, "household_income.csv")
hh_inc_q <- tibble(date = as.Date(character()))
if (file.exists(hh_inc_csv)) {
  raw <- read.csv(hh_inc_csv, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM")
  raw <- raw[!is.na(raw[[1L]]) & nzchar(raw[[1L]]), , drop = FALSE]
  # File has been supplied in two date conventions: "Sep-1959" (4-digit
  # year) and "Sep-59" (2-digit year). Try 4-digit first; if results land
  # before 1900 (i.e. parsed "59" as year 59 AD), reparse with century
  # inference (50-99 -> 19xx, 00-49 -> 20xx).
  raw_dates <- as.Date(paste0("01-", raw[[1L]]), format = "%d-%b-%Y")
  needs_century_fix <- mean(is.na(raw_dates)) > 0.5 ||
    (any(!is.na(raw_dates)) && min(raw_dates, na.rm = TRUE) < as.Date("1900-01-01"))
  if (needs_century_fix) {
    parts <- strsplit(raw[[1L]], "-")
    fixed <- vapply(parts, function(p) {
      if (length(p) != 2L) return(NA_character_)
      yy <- suppressWarnings(as.integer(p[2L]))
      if (is.na(yy)) return(NA_character_)
      yyyy <- if (yy >= 50L) 1900L + yy else 2000L + yy
      sprintf("01-%s-%d", p[1L], yyyy)
    }, character(1))
    raw_dates <- as.Date(fixed, format = "%d-%b-%Y")
  }
  keep <- !is.na(raw_dates)
  raw  <- raw[keep, , drop = FALSE]
  raw_dates <- raw_dates[keep]
  num_or_na <- function(col) {
    if (is.null(col)) return(rep(NA_real_, nrow(raw)))
    suppressWarnings(as.numeric(col))
  }
  hh_inc_q <- tibble(
    date                         = abs_to_qstart(raw_dates),
    compensation_of_employees    = num_or_na(raw$compensation_of_employees),
    prop_inc_rec                 = num_or_na(raw$prop_inc_rec),
    social_assistance_benefits   = num_or_na(raw$social_assisatance_ben),
    prop_inc_pay                 = num_or_na(raw$prop_inc_pay),
    # Williams (2009) §4.2.1 inputs (added in 2026 update):
    gos_dwellings                = num_or_na(raw$gos_dwellings),
    gross_mixed_income           = num_or_na(raw$gross_mixed_income),
    income_tax_payable           = num_or_na(raw$income_tax_payable),
    total_income_rec             = num_or_na(raw$total_income_rec),
    total_income_pay             = num_or_na(raw$total_income_pay),
    wage_share                   = num_or_na(raw$wage_share_incoem)  # source typo retained
  ) %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE) %>%
    filter(date <= spine_end)
  message(sprintf("  hh_inc components (CSV): %d obs, %s to %s, %d non-NA cols",
    nrow(hh_inc_q),
    format(min(hh_inc_q$date), "%Y-%m-%d"),
    format(max(hh_inc_q$date), "%Y-%m-%d"),
    sum(sapply(hh_inc_q[-1], function(c) any(!is.na(c))))))
} else {
  message("  household_income.csv NOT FOUND — labour+transfer income unavailable, ",
          "Italy-style scaled-income robustness will be skipped")
}

## 2.5  Population (15+ civilian population — ABS A84423091W)
message("2.5 Population (15+ civilian, ABS A84423091W)")
# Source: data_raw/population_workingage.csv (user-supplied ABS series).
# This is "Civilian Population aged 15 years and over", monthly, in
# thousands. It IS the correct denominator for the per-capita series
# used in the model (the README says we scale by 15+ pop, and Muellbauer's
# convention is the same).
#
# We previously summed single-year age cohorts from 3101059, but the
# Persons-by-age series in current ABS vintages truncates at age 47,
# producing a ~65% underestimate of total population. The 3101059
# workbook is still loaded below for the prime-age share construction
# (which uses Male+Female cohorts and is therefore unaffected).
pop_raw <- read_abs_cached(
  file.path(raw_dir, "3101059.xlsx"),
  "abs_pop"
)

pop_csv <- file.path(raw_dir, "population_workingage.csv")
if (file.exists(pop_csv)) {
  raw <- read.csv(pop_csv, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM")
  raw <- raw[!is.na(raw[[1L]]) & nzchar(raw[[1L]]), , drop = FALSE]
  raw_dates <- as.Date(paste0("01-", raw[[1L]]), format = "%d-%b-%Y")
  raw_vals  <- suppressWarnings(as.numeric(raw[[2L]]))
  keep <- !is.na(raw_dates) & !is.na(raw_vals)
  pop_m <- tibble(date = raw_dates[keep],
                  value = raw_vals[keep]) %>%
    arrange(date) %>%
    filter(date <= spine_end)  # trim ABS forward projections
  pop_q <- monthly_to_quarterly(pop_m) %>%
    rename(pop_thousands = value) %>%
    mutate(pop_millions = pop_thousands / 1000)
  message(sprintf("  pop_15plus (CSV): %d quarterly obs, %s to %s, last=%.1fM",
    nrow(pop_q),
    format(min(pop_q$date), "%Y-%m-%d"),
    format(max(pop_q$date), "%Y-%m-%d"),
    tail(pop_q$pop_millions, 1)))
} else {
  # Legacy fallback: sum single-year cohorts (KNOWN BIASED LOW per above).
  message("  population_workingage.csv NOT FOUND — falling back to (biased) cohort sum")
  pop_ann <- pop_raw %>%
    filter(str_detect(series_name,
      "^Estimated Resident Population ; Persons ; ")) %>%
    mutate(date = abs_to_qstart(date)) %>%
    group_by(date) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(date) %>%
    filter(value > 0)
  pop_q <- annual_to_quarterly_spline(pop_ann, spine_dates) %>%
    rename(pop_thousands = value) %>%
    mutate(pop_millions = pop_thousands / 1000)
  message(sprintf("  pop (biased cohort fallback): %d quarterly obs", nrow(pop_q)))
}

## 2.5b Prime-age share (25-54). Muellbauer's life-cycle adjustment: a higher
## share of working-age households shifts the aggregate consumption-to-income
## relationship toward saving and house-purchase behaviour.
## ABS 3101059 has single-year cohorts under "Persons" only up to age 47 in
## current vintages, so we sum across Male + Female single-year cohorts to get
## consistent numerator and denominator across the full 0-100 age range.
prime_age_share_q <- tryCatch({
  pop_mf <- pop_raw %>%
    filter(str_detect(series_name,
      "^Estimated Resident Population ; (Male|Female) ; ")) %>%
    mutate(
      age = suppressWarnings(as.integer(
        str_match(series_name,
          "^Estimated Resident Population ; (?:Male|Female) ; ([0-9]+)")[, 2]
      )),
      date = abs_to_qstart(date)
    ) %>%
    filter(!is.na(age))

  if (nrow(pop_mf) == 0L) stop("no Male/Female age-cohort series found")

  total_ann <- pop_mf %>%
    group_by(date) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
  prime_ann <- pop_mf %>%
    filter(age >= 25, age <= 54) %>%
    group_by(date) %>%
    summarise(prime = sum(value, na.rm = TRUE), .groups = "drop")

  share_ann <- inner_join(total_ann, prime_ann, by = "date") %>%
    filter(total > 0) %>%
    mutate(value = prime / total) %>%
    select(date, value) %>%
    arrange(date)

  share_q <- annual_to_quarterly_spline(share_ann, spine_dates) %>%
    rename(prime_age_share = value)
  message(sprintf("  prime_age_share: %d quarterly obs via spline (from %d annual obs)",
                  nrow(share_q), nrow(share_ann)))
  share_q
}, error = function(e) {
  message("  prime_age_share: failed (", e$message, ") — leaving NA")
  tibble(date = spine_dates, prime_age_share = NA_real_)
})

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

# Labour force level (persons, thousands) — used by Italy-style PI as
# log(labour_force / population) predictor. Same workbook (6202001) as
# the unemployment rate.
lf_m <- pick_abs(labour_raw,
  "^Labour force.*Persons",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Trend")
) %>% rename(labour_force = value)
if (nrow(lf_m) == 0L) {
  message("  labour_force: not found in 6202001 — lf_share will be NA")
  lf_q <- tibble(date = as.Date(character()), labour_force = NA_real_)
} else {
  lf_q <- monthly_to_quarterly(lf_m, value_col = "labour_force") %>%
    rename(labour_force = value)
  message(sprintf("  labour_force: %d obs, %s to %s",
    nrow(lf_q),
    format(min(lf_q$date), "%Y-%m-%d"),
    format(max(lf_q$date), "%Y-%m-%d")))
}

## 2.6b  Historical labour force back-extension (1964Q3 – 2011Q2)
# User-supplied CSV pulling together pre-1978 series for population (total
# and 15-64), labour force, and unemployed (counts in '000). Sources are
# old ABS Labour Force / Year Book / Foster compilation tables (the
# specifics are documented in data.md §3.6). Used to growth-rate-splice
# the modern (1978+) labour force variables back through 1976Q3 and
# beyond, unblocking per-capita variables on the extended sample.
message("2.6b Historical labour force back-extension (labour_force_historic.csv)")
lfh_path <- file.path(raw_dir, "labour_force_historic.csv")
if (file.exists(lfh_path)) {
  tryCatch({
    lfh <- read.csv(lfh_path, stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8-BOM", check.names = FALSE)
    # First column has a blank header and holds Mon-YYYY dates; access by
    # index rather than by name so the empty header doesn't trip [[..]].
    raw_dates <- as.Date(paste0("01-", lfh[[1]]), format = "%d-%b-%Y")
    keep <- !is.na(raw_dates)
    lfh <- lfh[keep, , drop = FALSE]
    raw_dates <- raw_dates[keep]
    qstart_dates <- abs_to_qstart(raw_dates)
    pop_total_hist_q <- tibble(date = qstart_dates,
                               value = suppressWarnings(as.numeric(lfh$pop_total))) %>%
      filter(!is.na(date), !is.na(value)) %>% arrange(date) %>%
      distinct(date, .keep_all = TRUE)
    pop_1564_hist_q  <- tibble(date = qstart_dates,
                               value = suppressWarnings(as.numeric(lfh$pop_15_64))) %>%
      filter(!is.na(date), !is.na(value)) %>% arrange(date) %>%
      distinct(date, .keep_all = TRUE)
    lf_hist_q        <- tibble(date = qstart_dates,
                               value = suppressWarnings(as.numeric(lfh$labour_force))) %>%
      filter(!is.na(date), !is.na(value)) %>% arrange(date) %>%
      distinct(date, .keep_all = TRUE)
    unemp_lvl_hist_q <- tibble(date = qstart_dates,
                               value = suppressWarnings(as.numeric(lfh$unemployed))) %>%
      filter(!is.na(date), !is.na(value)) %>% arrange(date) %>%
      distinct(date, .keep_all = TRUE)
    # Historic unemployment RATE = unemployed / labour_force × 100
    unemp_rate_hist_q <- inner_join(unemp_lvl_hist_q %>% rename(u = value),
                                    lf_hist_q %>% rename(lf = value),
                                    by = "date") %>%
      transmute(date, value = 100 * u / lf)
    message(sprintf("  historic raw: %d quarters, %s to %s",
      nrow(unemp_rate_hist_q),
      format(min(unemp_rate_hist_q$date), "%Y-%m-%d"),
      format(max(unemp_rate_hist_q$date), "%Y-%m-%d")))

    # Growth-rate splice the historic data onto each modern series. We
    # reuse the splice_hpi helper (defined later in the file) by inlining
    # the same logic here, since splice_hpi appears later in this script.
    chain_back <- function(base, overlay_df, overlay_col) {
      ov <- overlay_df %>% transmute(date, value = .data[[overlay_col]])
      joined <- inner_join(base, ov, by = "date",
                           suffix = c("_base", "_ov")) %>%
        filter(!is.na(value_base), !is.na(value_ov)) %>% arrange(date)
      if (nrow(joined) < 1L) return(overlay_df)
      anchor <- joined[1, ]
      before <- base %>%
        filter(date < anchor$date) %>%
        mutate(value = anchor$value_ov * value / anchor$value_base)
      bind_rows(before %>% transmute(date, !!overlay_col := value),
                overlay_df) %>%
        arrange(date) %>% distinct(date, .keep_all = TRUE)
    }

    if (nrow(pop_1564_hist_q) > 0L && nrow(pop_q) > 0L) {
      pop_q <- chain_back(pop_1564_hist_q, pop_q, "pop_thousands") %>%
        mutate(pop_millions = pop_thousands / 1000)
      message(sprintf("  pop (back-extended): %d obs, now %s to %s",
        nrow(pop_q),
        format(min(pop_q$date), "%Y-%m-%d"),
        format(max(pop_q$date), "%Y-%m-%d")))
    }
    if (nrow(lf_hist_q) > 0L && nrow(lf_q) > 0L) {
      lf_q <- chain_back(lf_hist_q, lf_q, "labour_force")
      message(sprintf("  labour_force (back-extended): %d obs, now %s to %s",
        nrow(lf_q),
        format(min(lf_q$date), "%Y-%m-%d"),
        format(max(lf_q$date), "%Y-%m-%d")))
    }
    if (nrow(unemp_rate_hist_q) > 0L && nrow(unemp_q) > 0L) {
      # Unemployment RATE is a ratio (not a level), so scale-shift splicing
      # would distort it. Use simple replacement: keep modern where modern
      # exists; backfill with historic before modern starts.
      modern_start <- min(unemp_q$date)
      hist_pre <- unemp_rate_hist_q %>%
        filter(date < modern_start) %>%
        rename(unemp_rate = value)
      unemp_q <- bind_rows(hist_pre, unemp_q) %>%
        arrange(date) %>% distinct(date, .keep_all = TRUE)
      message(sprintf("  unemp_rate (back-extended): %d obs, now %s to %s",
        nrow(unemp_q),
        format(min(unemp_q$date), "%Y-%m-%d"),
        format(max(unemp_q$date), "%Y-%m-%d")))
    }
    # Expose pop_total as a separate master column (no modern equivalent
    # in 6202; pop_total_hist ends 2011Q2, so it's a partial column).
    pop_total_q <- pop_total_hist_q %>%
      transmute(date, pop_total_thousands = value)
    message(sprintf("  pop_total (raw historic): %d obs", nrow(pop_total_q)))
  }, error = function(e) {
    message("  labour_force_historic.csv parse error: ", e$message)
    pop_total_q <<- tibble(date = as.Date(character()), pop_total_thousands = numeric())
  })
} else {
  message("  labour_force_historic.csv NOT FOUND — pre-1978 labour force will remain NA")
  pop_total_q <- tibble(date = as.Date(character()), pop_total_thousands = numeric())
}

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
# Source order:
#   1. data_raw/rba_filrhlbvs.csv  — user-supplied RBA F6 historical CSV.
#      This is the canonical source. Eliminates network dependence and
#      avoids the readrba fetch failing silently.
#   2. readrba::read_rba_seriesid("FILRHLBVS") — live fallback.
#   3. ABS-implicit (mort_int_paid / lagged debt stock) — last resort;
#      systematically biased low (peaks ~7% vs RBA's ~17% in 1989).
mort_rate_q <- NULL

rba_csv <- file.path(raw_dir, "rba_filrhlbvs.csv")
if (file.exists(rba_csv)) {
  tryCatch({
    raw <- read.csv(rba_csv, stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8-BOM")
    raw <- raw[!is.na(raw[[1L]]) & nzchar(raw[[1L]]), , drop = FALSE]
    raw_dates <- as.Date(paste0("01-", raw[[1L]]), format = "%d-%b-%Y")
    raw_vals  <- suppressWarnings(as.numeric(raw[[2L]]))
    keep <- !is.na(raw_dates) & !is.na(raw_vals)
    mort_rate_m <- tibble(date = raw_dates[keep],
                          value = raw_vals[keep]) %>% arrange(date)
    mort_rate_q <- monthly_to_quarterly(mort_rate_m) %>%
      rename(mortgage_rate = value)
    message(sprintf("  mortgage_rate (RBA F6 CSV): %d obs, %s to %s, peak %.2f%%",
      nrow(mort_rate_q),
      format(min(mort_rate_q$date), "%Y-%m-%d"),
      format(max(mort_rate_q$date), "%Y-%m-%d"),
      max(mort_rate_q$mortgage_rate, na.rm = TRUE)))
  }, error = function(e) message("  rba_filrhlbvs.csv parse error: ", e$message))
}

if (is.null(mort_rate_q) || nrow(mort_rate_q) == 0L) {
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
        message(sprintf("  mortgage_rate (RBA FILRHLBVS via readrba): %d obs, %s to %s",
          nrow(mort_rate_q),
          format(min(mort_rate_q$date), "%Y-%m-%d"),
          format(max(mort_rate_q$date), "%Y-%m-%d")))
      } else {
        message("  FILRHLBVS returned no data from readrba")
      }
    }, error = function(e) message("  readrba fetch failed: ", e$message))
  } else {
    message("  readrba not installed AND rba_filrhlbvs.csv not found")
  }
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

# Long history: house_price_history_long.csv
# Source: Treasury TRYM (Treasury Macroeconomic Model) historical database.
# Nominal, national, quarterly, 1959Q3 onwards. Provides the deepest layer
# of the splice and supersedes the houseprice_old.csv legacy fill where
# they overlap (1986Q2-2003Q3).
trym_path <- file.path(raw_dir, "house_price_history_long.csv")
hpi_trym_q <- tibble(date = as.Date(character()), value = numeric())
if (file.exists(trym_path)) {
  tryCatch({
    trym <- read.csv(trym_path, stringsAsFactors = FALSE)
    # Columns: first column is m/d/Y date string, value column is `ph_long`.
    date_col <- names(trym)[1]
    val_col  <- if ("ph_long" %in% names(trym)) "ph_long" else names(trym)[ncol(trym)]
    parsed_dates <- as.Date(trym[[date_col]], format = "%m/%d/%Y")
    hpi_trym_q <- tibble(
      date  = abs_to_qstart(parsed_dates),
      value = as.numeric(trym[[val_col]])
    ) %>% filter(!is.na(date), !is.na(value)) %>% arrange(date)
    message(sprintf("  hpi (TRYM long): %d obs, %s to %s",
      nrow(hpi_trym_q),
      format(min(hpi_trym_q$date), "%Y-%m-%d"),
      format(max(hpi_trym_q$date), "%Y-%m-%d")))
  }, error = function(e) message("  house_price_history_long.csv parse error: ", e$message))
}

# Splice: chain-link the BASE series onto the OVERLAY using growth rates.
# Standard ABS chain-linking convention. Anchors the level at the first
# overlapping quarter where both series have non-NA values, then back-casts
# the base series via its own QoQ growth rates:
#
#   chained[t] = overlay[t_anchor] * (base[t] / base[t_anchor])  for t < t_anchor
#   chained[t] = overlay[t]                                       for t >= t_anchor
#
# This preserves the BASE series' growth rates exactly while pinning the
# level to the overlay at the join. No averaging over noisy overlap windows;
# no level discontinuity at the join (by construction).
splice_hpi <- function(base, overlay) {
  # Normalise to `value` column in both
  norm <- function(df) {
    non_date <- setdiff(names(df), "date")
    df %>% rename(value = !!non_date[1])
  }
  base    <- norm(base)
  overlay <- norm(overlay)
  joined <- inner_join(base, overlay, by = "date", suffix = c("_base", "_ov")) %>%
    filter(!is.na(value_base), !is.na(value_ov)) %>%
    arrange(date)
  if (nrow(joined) < 1L) return(overlay)
  anchor_date <- joined$date[1]
  anchor_base <- joined$value_base[1]
  anchor_ov   <- joined$value_ov[1]
  before <- base %>%
    filter(date < anchor_date) %>%
    mutate(value = anchor_ov * value / anchor_base)
  bind_rows(before, overlay) %>% arrange(date) %>% distinct(date, .keep_all = TRUE)
}

# Build spliced series: TRYM → legacy → bridge → current (all as `hpi` column)
hpi_spliced <- hpi_current_q %>% rename(hpi = value)
if (nrow(hpi_bridge_q) > 0L && nrow(hpi_current_q) > 0L) {
  hpi_spliced <- splice_hpi(hpi_bridge_q, hpi_current_q) %>% rename(hpi = value)
}
if (nrow(hpi_legacy_q) > 0L && nrow(hpi_spliced) > 0L) {
  hpi_spliced <- splice_hpi(hpi_legacy_q, hpi_spliced) %>% rename(hpi = value)
}
if (nrow(hpi_trym_q) > 0L && nrow(hpi_spliced) > 0L) {
  hpi_spliced <- splice_hpi(hpi_trym_q, hpi_spliced) %>% rename(hpi = value)
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

# Anchor "First Home Buyers" with a leading "; " so it doesn't also match
# "; Non-First Home Buyers ;" — pick_abs selects by frequency and would
# otherwise resolve both regexes to the same (Non-FHB) series, leaving
# fhb_share constant at 0.5.
fhb_m <- pick_abs(credit_raw,
  "; First Home Buyers ;.*New loan commitments.*Number",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Original")
) %>% rename(fhb_loans = value)
non_fhb_m <- pick_abs(credit_raw,
  "; Non-First Home Buyers ;.*New loan commitments.*Number",
  types = c("Seasonally Adjusted", "Seasonally adjusted", "Original")
) %>% rename(non_fhb_loans = value)

fhb_q <- monthly_to_quarterly(fhb_m, value_col = "fhb_loans") %>%
  rename(fhb_loans = value)
non_fhb_q <- monthly_to_quarterly(non_fhb_m, value_col = "non_fhb_loans") %>%
  rename(non_fhb_loans = value)

message(sprintf("  housing_loan_flow: %d obs", nrow(housing_loan_flow_q)))

## 2.12  RBA D-tables: M3 monetary aggregate + total credit (back-extension)
# Williams (2010) used these for the pre-1988 splice of liquid assets and
# household credit. Both go back substantially further than the ABS 5232
# household balance-sheet series (1988+) and unlock the back-extension of
# nla_y / debt_y proxies under NS-020 phase 1.
message("2.12 RBA D-tables (M3 + total credit, back-extension)")

m3_aggregate_q   <- tibble(date = as.Date(character()), m3_aggregate = numeric())
credit_total_d02_q <- tibble(date = as.Date(character()), credit_total_d02 = numeric())

# M3 (DMAM3N — original, $bn). Full coverage 1959-07 to current.
d03_path <- file.path(raw_dir, "d03hist.xlsx")
if (file.exists(d03_path)) {
  tryCatch({
    m3_m <- read_rba_d_table(d03_path, "DMAM3N")
    m3_aggregate_q <- monthly_to_quarterly(m3_m, value_col = "value") %>%
      rename(m3_aggregate = value)
    message(sprintf("  m3_aggregate (D03 DMAM3N): %d obs, %s to %s",
      nrow(m3_aggregate_q),
      format(min(m3_aggregate_q$date), "%Y-%m-%d"),
      format(max(m3_aggregate_q$date), "%Y-%m-%d")))
  }, error = function(e) message("  D03 M3 parse error: ", e$message))
}

# Total credit: DLCACN (1976-09 to 2019-06) growth-rate spliced onto
# DLCACSFN (2019-07+, the post-2019 RBA reform successor). No quarterly
# overlap, so the splice applies the level continuity at the boundary.
d02_path <- file.path(raw_dir, "d02hist.xlsx")
if (file.exists(d02_path)) {
  tryCatch({
    credit_pre_m  <- read_rba_d_table(d02_path, "DLCACN")     # pre-2019 reform
    credit_post_m <- read_rba_d_table(d02_path, "DLCACSFN")   # post-2019 reform
    credit_pre_q  <- monthly_to_quarterly(credit_pre_m,  value_col = "value")
    credit_post_q <- monthly_to_quarterly(credit_post_m, value_col = "value")
    # Splice: post-reform is the modern overlay; pre-reform is the legacy base.
    # No overlap quarter (DLCACN ends 2019Q2; DLCACSFN starts 2019Q3), so
    # the helper anchors at the first joined date — which falls back to the
    # overlay's first date if no overlap. To preserve continuity at the
    # boundary, we do an explicit growth-rate chain via the last pre-reform
    # value and the first post-reform value.
    last_pre  <- tail(credit_pre_q, 1)
    first_post <- head(credit_post_q, 1)
    # Scale pre-reform values to align with post-reform series at the boundary,
    # using a growth-rate convention: pre_rescaled[t] = first_post * pre[t]/last_pre
    pre_rescaled <- credit_pre_q %>%
      mutate(value = first_post$value * value / last_pre$value)
    credit_total_d02_q <- bind_rows(pre_rescaled, credit_post_q) %>%
      arrange(date) %>% distinct(date, .keep_all = TRUE) %>%
      rename(credit_total_d02 = value)
    message(sprintf("  credit_total_d02 (D02 DLCACN→DLCACSFN): %d obs, %s to %s",
      nrow(credit_total_d02_q),
      format(min(credit_total_d02_q$date), "%Y-%m-%d"),
      format(max(credit_total_d02_q$date), "%Y-%m-%d")))
  }, error = function(e) message("  D02 total-credit parse error: ", e$message))
}

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
  left_join(hh_inc_q,          by = "date") %>%   # $m, components from CSV
  left_join(unemp_q,           by = "date") %>%   # %
  left_join(lf_q,              by = "date") %>%   # persons (thousands)
  left_join(pop_q %>% select(date, pop_millions), by = "date") %>%
  left_join(pop_total_q,                          by = "date") %>%   # total resident pop ('000), historic-only 1964-2011
  left_join(prime_age_share_q,                    by = "date") %>%   # prime working-age share (25-54)
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
  left_join(non_fhb_q %>% select(date, non_fhb_loans), by = "date") %>%
  left_join(m3_aggregate_q,    by = "date") %>%   # $bn (RBA D03 DMAM3N, 1959Q3+)
  left_join(credit_total_d02_q, by = "date")      # $bn (RBA D02, spliced 1976Q3+)

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

# Italy-style "scaled income" inputs: labour+transfer income (CSV) deflated
# and per-capita. Italy.pdf §3.1 averages this with full disposable income to
# down-weight property income (which is mismeasured in national accounts —
# imputed financial returns rather than realised cash flow).
# M3 household-allocated proxy: M3 × wage_share/100. The wage share of
# domestic factor income (labour income / GDP) is a defensible proxy for
# the household share of M3 — Williams (2010) used the household factor
# income share for this allocation. Coverage matches m3_aggregate × wage_share
# (1976Q3+ on the current spine). Used downstream as a longer-sample
# alternative to the B20 deposit-derived liquid-asset series.
if ("m3_aggregate" %in% names(master) && "wage_share" %in% names(master)) {
  master <- master %>%
    mutate(m3_household_proxy = m3_aggregate * wage_share / 100)
  message(sprintf("  m3_household_proxy: %d non-NA obs, range %.0f - %.0f $bn",
    sum(!is.na(master$m3_household_proxy)),
    min(master$m3_household_proxy, na.rm = TRUE),
    max(master$m3_household_proxy, na.rm = TRUE)))
}

if ("compensation_of_employees" %in% names(master) &&
    "social_assistance_benefits" %in% names(master)) {
  master <- master %>%
    mutate(
      labour_transfer_nom        = compensation_of_employees +
                                    social_assistance_benefits,
      labour_transfer_real_pc    = (labour_transfer_nom /
                                     (cons_deflator_norm / 100)) / pop_millions,
      ln_labour_transfer_real_pc = log(pmax(labour_transfer_real_pc, 1e-9)),
      # Scaled income: 50/50 average of disposable and labour+transfer.
      # Used by run_italy_style_robustness as the alternative income measure.
      scaled_income_real_pc      = 0.5 * ydi_real_pc + 0.5 * labour_transfer_real_pc,
      ln_scaled_income_real_pc   = log(pmax(scaled_income_real_pc, 1e-9))
    )
  message(sprintf("  labour+transfer real per-capita: %d obs, range %.0f - %.0f",
    sum(!is.na(master$labour_transfer_real_pc)),
    min(master$labour_transfer_real_pc, na.rm = TRUE),
    max(master$labour_transfer_real_pc, na.rm = TRUE)))
}

# Williams (2009) §4.2.1 — Non-property household disposable income (NPY).
# Recipe (verbatim from p.10):
#   npy_rec = total_income_rec − GOS_dwellings − prop_inc_rec
#   property_tax_share = (GOS_dwellings + prop_inc_rec) / total_income_rec
#   npy_pay = total_income_pay − interest_on_dwellings (= prop_inc_pay)
#             − property_tax_share × income_tax_payable
#   NPY     = npy_rec − npy_pay
# Then npy_real_pc = NPY / cons_deflator_norm * 100 / pop_millions and
#       ln_npy_real_pc = log(npy_real_pc).
# This is the income measure that appears as `y` in Williams (2010) WP 492's
# consumption equation. We add it as a parallel series to ydi_real_pc; the
# two coexist so a robustness column can compare Spec 6 estimates under
# each. Williams' paper uses property tax payable rather than total income
# tax — we follow his proxy (the receivable share applied to total tax).
williams_required <- c("gos_dwellings", "prop_inc_rec", "prop_inc_pay",
                       "income_tax_payable", "total_income_rec", "total_income_pay")
if (all(williams_required %in% names(master)) &&
    all(sapply(williams_required, function(v) any(!is.na(master[[v]]))))) {
  master <- master %>%
    mutate(
      property_tax_share = (gos_dwellings + prop_inc_rec) / total_income_rec,
      npy_rec_nom        = total_income_rec - gos_dwellings - prop_inc_rec,
      npy_pay_nom        = total_income_pay - prop_inc_pay -
                           property_tax_share * income_tax_payable,
      npy_nom            = npy_rec_nom - npy_pay_nom,
      npy_real_pc        = (npy_nom / (cons_deflator_norm / 100)) / pop_millions,
      ln_npy_real_pc     = log(pmax(npy_real_pc, 1e-9))
    )
  npy_obs <- master$npy_real_pc[!is.na(master$npy_real_pc)]
  message(sprintf("  Williams NPY (non-property income) real pc: %d obs, range %.0f - %.0f, mean %.0f",
    length(npy_obs), min(npy_obs), max(npy_obs), mean(npy_obs)))
  # Sanity: NPY/disposable ratio should be roughly 0.85-1.05 in modern
  # quarters (property income receivable is a meaningful share of GDI).
  ratio_modern <- master %>%
    filter(date >= as.Date("2010-01-01")) %>%
    mutate(r = npy_nom / ydi_nom) %>% pull(r)
  if (length(ratio_modern) > 0L) {
    message(sprintf("  NPY / GDI ratio (post-2010): %.3f - %.3f (mean %.3f)",
      min(ratio_modern, na.rm=TRUE), max(ratio_modern, na.rm=TRUE),
      mean(ratio_modern, na.rm=TRUE)))
  }
} else {
  missing <- setdiff(williams_required, names(master))
  if (length(missing) == 0L) missing <- "all-NA columns"
  message("  Williams NPY: not constructed — missing inputs: ",
          paste(missing, collapse = ", "))
}

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
    # Net liquid assets = liquid financial assets MINUS total household debt.
    # Italy paper Section 2 eq 2.5 / Table 3 col 3 and Australia paper eq 7
    # both define NLA this way and impose the cross-equation restriction
    # gamma_LA + gamma_LOANS = 0. fin_loans here is ABS 5232035 total household
    # liabilities (housing + consumer credit + other), the broadest available
    # household debt measure on the official balance sheet.
    nla_y                = (fin_deposits - fin_loans) / ydi_ann_nom,
    # Unrestricted components retained so the gamma_LA = -gamma_LOANS
    # restriction can be tested (see test_nla_restriction in the estimation script).
    nla_y_unrestricted   = fin_deposits             / ydi_ann_nom,
    debt_y               = fin_loans                / ydi_ann_nom,
    ha_y                 = housing_wealth           / ydi_ann_nom, # housing assets
    # Use ABS official net worth (A83722648X) if available; otherwise sum components
    networth_y     = if_else(
      !is.na(closing_net_worth),
      closing_net_worth / ydi_ann_nom,
      (fin_deposits + fin_equities + fin_super + housing_wealth - fin_loans) / ydi_ann_nom
    )
  )

# --------------------------------------------------------------------------
# Back-extension proxies for ln_networth_y (NS-020 phase 1)
# --------------------------------------------------------------------------
# The official `networth_y` is bounded at 1988Q3+ by ABS 5232 sectoral
# accounts. To enable Spec 1-3 fits on the longer (1976Q3+) sample, we
# build a proxy that:
#   (i)  uses M3-allocated-to-households (`m3_household_proxy`) plus a
#        hpi×pop back-cast of housing_wealth as the raw wealth aggregate
#   (ii) growth-rate-splices that raw aggregate ratio onto the official
#        `networth_y` at 1988Q3 (so the proxy equals the official series
#        from 1988Q3 onwards and back-casts smoothly through 1976Q3)
#
# Caveats explicitly noted: the back-cast omits equities and super
# (quantitatively small in 1976-1988 — Australian super pre-SGC was a
# negligible household asset class) and omits debt (mortgage debt was a
# much smaller share of household balance sheets in the 1970s). Use this
# proxy ONLY for back-extension exercises; never as a substitute for
# `networth_y` on the modern sample where the official series exists.
master <- master %>%
  mutate(
    # Housing wealth back-cast: anchor at first available official obs and
    # back-cast via hpi × pop_millions growth (i.e. assume dwellings stock
    # per capita is roughly constant across 1976-1988).
    housing_wealth_proxy = {
      anchor_idx <- which(!is.na(housing_wealth))[1L]
      if (length(anchor_idx) == 0L) {
        rep(NA_real_, n())
      } else {
        anchor_hw  <- housing_wealth[anchor_idx]
        anchor_hpi <- hpi[anchor_idx]
        anchor_pop <- pop_millions[anchor_idx]
        backcast   <- anchor_hw * (hpi / anchor_hpi) * (pop_millions / anchor_pop)
        if_else(is.na(housing_wealth), backcast, housing_wealth)
      }
    }
  ) %>%
  mutate(
    # Raw proxy ratio: (M3-allocated + housing_wealth_proxy) / ydi_ann_nom.
    # m3_household_proxy is in $ BILLION (RBA D03 DMAM3N, never rescaled),
    # whereas housing_wealth_proxy and ydi_ann_nom are in $ MILLION — so the
    # M3 term must be ×1000 before the sum, otherwise it contributes ~0.01%
    # of the numerator instead of its intended ~12% and the aggregate proxy
    # collapses to a housing-only back-cast (NS-131 / M15).
    networth_y_raw_proxy = (m3_household_proxy * 1000 + housing_wealth_proxy) /
                           ydi_ann_nom
  )
# Growth-rate splice raw_proxy onto official networth_y at 1988Q3.
{
  anchor_date <- as.Date("1988-07-01")
  off_at_anchor <- master$networth_y[master$date == anchor_date]
  raw_at_anchor <- master$networth_y_raw_proxy[master$date == anchor_date]
  if (length(off_at_anchor) == 1L && length(raw_at_anchor) == 1L &&
      is.finite(off_at_anchor) && is.finite(raw_at_anchor) && raw_at_anchor != 0) {
    scale <- off_at_anchor / raw_at_anchor
    master <- master %>%
      mutate(
        networth_y_proxy = if_else(
          date >= anchor_date & !is.na(networth_y),
          networth_y,
          networth_y_raw_proxy * scale
        ),
        ln_networth_y_proxy = log(pmax(networth_y_proxy, 1e-6))
      )
    message(sprintf("  networth_y_proxy: anchor %s, scale=%.4f, %d non-NA obs",
      format(anchor_date), scale, sum(!is.na(master$networth_y_proxy))))
  } else {
    master <- master %>%
      mutate(
        networth_y_proxy    = NA_real_,
        ln_networth_y_proxy = NA_real_
      )
    message("  networth_y_proxy: anchor unavailable, leaving NA")
  }
}

# --------------------------------------------------------------------------
# Disaggregated wealth proxies for back-extension (NS-020 phase 1, items 1-4)
# --------------------------------------------------------------------------
# For each official disaggregated wealth ratio (ha_y, nla_y, eq_y, super_y),
# build a proxy that:
#   - equals the official series for t >= 1988Q3 (the official series start)
#   - back-casts via the most relevant available aggregate for t < 1988Q3
#
# Methodology:
#   ha_y_proxy    = housing_wealth_proxy / ydi_ann_nom
#                   (housing back-cast already built via hpi×pop)
#   fin_deposits_proxy = anchor × m3_household_proxy[t] / m3_household_proxy[anchor]
#   fin_loans_proxy    = anchor × credit_total_d02[t]   / credit_total_d02[anchor]
#   nla_y_proxy   = (fin_deposits_proxy − fin_loans_proxy) / ydi_ann_nom
#   eq_y_proxy    = official; pre-1988 held constant at the 1988Q3 value
#                   (Option B — Australian household equity holdings were
#                    a small wealth share in the late 1970s/early 80s; the
#                    constant assumption introduces little level error and
#                    is straightforward to upgrade to ASX-All-Ords back-cast)
#   super_y_proxy = official; pre-1988 linear ramp from 0.1 × anchor at
#                   1976Q3 to anchor at 1988Q3 (Australian super pre-SGC
#                   1992 was a minor wealth class; matches Williams 2010
#                   Table A.1 ballpark)
#
# All four equal the official series at 1988Q3 by construction (boundary
# continuity). Caveats noted in data.md §3.4b.
{
  anchor_date <- as.Date("1988-07-01")
  ai_log <- master$date == anchor_date
  fin_dep_anc   <- master$fin_deposits[ai_log]
  fin_loans_anc <- master$fin_loans[ai_log]
  m3_anc        <- master$m3_household_proxy[ai_log]
  credit_anc    <- master$credit_total_d02[ai_log]
  eq_y_anc      <- master$eq_y[ai_log]
  super_y_anc   <- master$super_y[ai_log]

  if (length(fin_dep_anc) == 1L && is.finite(fin_dep_anc) &&
      length(m3_anc)      == 1L && is.finite(m3_anc) && m3_anc != 0 &&
      length(credit_anc)  == 1L && is.finite(credit_anc) && credit_anc != 0) {

    earliest_date <- as.Date("1976-07-01")
    span_days     <- as.numeric(anchor_date - earliest_date)

    master <- master %>%
      mutate(
        # --- 1. Housing assets ratio (uses housing_wealth_proxy already built)
        ha_y_proxy = housing_wealth_proxy / ydi_ann_nom,

        # --- 2. Liquid components: deposits and loans back-cast via M3 and D02
        fin_deposits_proxy = if_else(
          date >= anchor_date & !is.na(fin_deposits),
          fin_deposits,
          fin_dep_anc * m3_household_proxy / m3_anc
        ),
        fin_loans_proxy = if_else(
          date >= anchor_date & !is.na(fin_loans),
          fin_loans,
          fin_loans_anc * credit_total_d02 / credit_anc
        ),
        nla_y_proxy = (fin_deposits_proxy - fin_loans_proxy) / ydi_ann_nom,

        # --- 3. Equities ratio: hold constant at 1988Q3 value pre-1988
        eq_y_proxy = if_else(
          date >= anchor_date & !is.na(eq_y),
          eq_y,
          eq_y_anc
        ),

        # --- 4. Super ratio: linear ramp from 0.1×anchor at 1976Q3 to anchor at 1988Q3
        super_y_proxy = if_else(
          date >= anchor_date & !is.na(super_y),
          super_y,
          {
            t_norm <- pmin(pmax(as.numeric(date - earliest_date) / span_days,
                                0), 1)
            super_y_anc * (0.1 + 0.9 * t_norm)
          }
        )
      )

    # Sum-of-disaggregated cross-check: should match networth_y_proxy at 1988Q3
    master <- master %>%
      mutate(
        networth_y_disagg_proxy = ha_y_proxy + nla_y_proxy +
                                   eq_y_proxy + super_y_proxy,
        ln_networth_y_disagg_proxy = log(pmax(networth_y_disagg_proxy, 1e-6))
      )

    message(sprintf(
      "  Disagg proxies built: ha=%d nla=%d eq=%d super=%d non-NA obs",
      sum(!is.na(master$ha_y_proxy)),
      sum(!is.na(master$nla_y_proxy)),
      sum(!is.na(master$eq_y_proxy)),
      sum(!is.na(master$super_y_proxy))))
    message(sprintf(
      "  Coherence at 1988Q3 (boundary): networth_y=%.3f, agg_proxy=%.3f, disagg_sum=%.3f",
      master$networth_y[ai_log],
      master$networth_y_proxy[ai_log],
      master$networth_y_disagg_proxy[ai_log]))
    message(sprintf(
      "  Coherence at 1976Q3 (deepest): agg_proxy=%.3f, disagg_sum=%.3f, gap=%.1f%%",
      master$networth_y_proxy[master$date == earliest_date],
      master$networth_y_disagg_proxy[master$date == earliest_date],
      100 * (master$networth_y_disagg_proxy[master$date == earliest_date] -
             master$networth_y_proxy[master$date == earliest_date]) /
            master$networth_y_proxy[master$date == earliest_date]))
  } else {
    master <- master %>%
      mutate(
        ha_y_proxy = NA_real_, nla_y_proxy = NA_real_,
        eq_y_proxy = NA_real_, super_y_proxy = NA_real_,
        fin_deposits_proxy = NA_real_, fin_loans_proxy = NA_real_,
        networth_y_disagg_proxy = NA_real_,
        ln_networth_y_disagg_proxy = NA_real_
      )
    message("  Disagg proxies: anchor data unavailable, leaving NA")
  }
}

# Range guard: nla_y is total deposits minus total household debt and is
# expected to be NEGATIVE in modern Australia (debt stock exceeds liquid
# assets). Allow a wide ratio band [-2, +2] before flagging a unit error.
{
  rng <- range(master$nla_y, na.rm = TRUE)
  if (!all(is.finite(rng)) || rng[1L] < -2 || rng[2L] > 2) {
    warning(sprintf("nla_y outside plausible band [-2, +2]: %.3f to %.3f",
                    rng[1L], rng[2L]))
  }
}

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

# Labour-force participation share. Both labour_force and the pop_millions
# column are reported here in thousands of persons (the column name predates
# the unit clarification), so lf_share is the dimensionless ratio. Used by
# the Italy-style PI predictor when that path is enabled in estimation.
master <- master %>%
  mutate(lf_share = labour_force / pop_millions)
lf_obs <- master$lf_share[!is.na(master$lf_share)]
if (length(lf_obs) > 0L) {
  message(sprintf(
    "  lf_share: %d obs, range %.3f – %.3f (mean %.3f)",
    length(lf_obs), min(lf_obs), max(lf_obs), mean(lf_obs)
  ))
}

# ------------------------------------------------------------------------------
# Credit Conditions Index (CCI)
# ------------------------------------------------------------------------------
# Three options were considered for constructing CCI back through the 1980s
# and 1990s when ABS housing-finance flow data is unavailable:
#
#   A) Muellbauer-style institutional CCI: regime-step indicators for major
#      Australian credit-policy episodes (1983 deregulation, 1992 recession,
#      1998 Wallis reforms, 2007 GFC tightening) blended with standardised
#      housing-finance, leverage, and rate-headwind indicators. See
#      construct_institutional_cci() and build_credit_regime_basis() in
#      model_helpers.R.
#   B) Drop the pre-2002 backfill: cci_ratio is NA before 2002Q3 and the
#      specs that use it (Spec 2 and Spec 5) get a truncated effective
#      sample. Honest about what we know.
#   C) State-space (Kalman) latent factor over multiple credit indicators.
#
# Default is Option B because it is the most defensible without further
# domain calibration: the previous spread-normalisation backfill (mortgage
# rate minus cash rate) conflates funding costs and risk premia with credit
# availability and is theoretically suspect for an LIVES adaptation.
# Option A is wired in but disabled behind USE_INSTITUTIONAL_CCI_OVERLAY (see
# top of file); flip that flag to overlay the institutional CCI back to 1980Q1.
# ------------------------------------------------------------------------------

# CCI proxy: log(housing credit flow / 8q moving average GDP proxy)
# (GDP proxy = 4 × quarterly income). Only available 2002Q3+.
master <- master %>%
  mutate(
    ydi_ann_8qma = zoo::rollmean(ydi_ann_nom, 8L, fill = NA, align = "right"),
    cci_ratio    = log(housing_loan_flow / ydi_ann_8qma)
  )

message(sprintf("  cci_ratio (housing-flow): %d non-NA obs starting %s",
                sum(!is.na(master$cci_ratio)),
                format(min(master$date[!is.na(master$cci_ratio)]), "%Y-%m-%d")))
message("  CCI specs (Spec 2 / Spec 5) effective sample now starts 2002Q3 ",
        "after lagging — pre-2002 spread backfill dropped (Option B default).")

# Optional Option-A overlay is applied after fhb_share is built (further down).

# FHB share. Available 2002Q3+ from ABS 560101. Now consumed by Spec 7 (cohort +
# burden) in australia_estimation.R as a credit-access proxy for younger buyers.
master <- master %>%
  mutate(
    fhb_share = fhb_loans / (fhb_loans + non_fhb_loans)
  )

# Mortgage cash-flow burden. Standard Muellbauer construction:
# (debt stock x interest rate) / annual disposable income.
# Available 1988Q3+ (limited by fin_loans from balance sheet).
master <- master %>%
  mutate(
    mortgage_burden = (fin_loans * mortgage_rate / 100) / ydi_ann_nom
  )

## RBA E13 — measured housing-loan-payment burden (preferred over synthetic)
## Source: data_raw/e13-data.csv, table E13 published by RBA jointly with
## APRA. Two ratios of interest:
##   LPHTICRI  Interest charged on total housing loans / disposable income
##   LPHTSPRI  Scheduled repayments on total housing loans / disposable income
##             (this is the closer analogue of the Muellbauer cash-flow
##              burden — covers both interest AND principal)
## Both quarterly, SA, in PER CENT (we divide by 100 for fraction-of-income).
## Coverage: 2009Q1 onwards only (~16 years), so this CANNOT replace the
## synthetic mortgage_burden which we need back to 1988Q3. Both series are
## kept; the synthetic is used in Spec 7, the measured is available for
## post-2009 robustness or replacement when sample length permits.
e13_path <- file.path(raw_dir, "e13-data.csv")
if (file.exists(e13_path)) {
  tryCatch({
    # Header is 10 lines of metadata before the data; the row "Series ID"
    # is the column-name row, followed by date,value rows. Read whole file
    # and locate the Series ID line dynamically.
    raw_lines <- readLines(e13_path, encoding = "UTF-8")
    # Strip a leading BOM if present on the first line
    raw_lines[1L] <- sub("^\\xef\\xbb\\xbf", "", raw_lines[1L], useBytes = TRUE)
    sid_row <- grep("^Series ID,", raw_lines)
    if (length(sid_row) == 0L)
      stop("Series ID header row not found in e13-data.csv")
    e13 <- read.csv(text = paste(raw_lines[sid_row:length(raw_lines)],
                                  collapse = "\n"),
                    stringsAsFactors = FALSE, check.names = FALSE)
    # First column header is "Series ID" but holds dates in the data rows.
    names(e13)[1L] <- "date_str"
    e13 <- e13[!is.na(e13$date_str) & nzchar(e13$date_str), , drop = FALSE]
    e13_dates <- as.Date(e13$date_str, format = "%d-%b-%Y")
    keep <- !is.na(e13_dates)
    e13 <- e13[keep, , drop = FALSE]
    e13_dates <- e13_dates[keep]
    e13_q <- tibble(
      date                          = abs_to_qstart(e13_dates),
      mortgage_interest_burden_rba  = suppressWarnings(as.numeric(e13$LPHTICRI)) / 100,
      mortgage_payment_burden_rba   = suppressWarnings(as.numeric(e13$LPHTSPRI)) / 100
    ) %>%
      arrange(date) %>%
      distinct(date, .keep_all = TRUE) %>%
      filter(date <= spine_end)
    master <- master %>% left_join(e13_q, by = "date")
    message(sprintf("  RBA E13 burden ratios: %d obs, %s to %s",
      nrow(e13_q),
      format(min(e13_q$date), "%Y-%m-%d"),
      format(max(e13_q$date), "%Y-%m-%d")))
    cat(sprintf("  LPHTICRI (interest burden): mean %.3f, range %.3f - %.3f\n",
      mean(e13_q$mortgage_interest_burden_rba, na.rm=TRUE),
      min(e13_q$mortgage_interest_burden_rba, na.rm=TRUE),
      max(e13_q$mortgage_interest_burden_rba, na.rm=TRUE)))
    cat(sprintf("  LPHTSPRI (payment burden):  mean %.3f, range %.3f - %.3f\n",
      mean(e13_q$mortgage_payment_burden_rba, na.rm=TRUE),
      min(e13_q$mortgage_payment_burden_rba, na.rm=TRUE),
      max(e13_q$mortgage_payment_burden_rba, na.rm=TRUE)))
  }, error = function(e) message("  e13-data.csv parse error: ", e$message))
} else {
  message("  e13-data.csv NOT FOUND — RBA-measured burden ratios unavailable")
}

# (1) Institutional CCI overlay — backfill cci_ratio pre-2002 from a
#     regime+indicator blend (preserving the post-2002 housing-flow anchor).
#     CANONICAL = OFF (shifts Spec 6 λ; see flag comment at top of file).
if (USE_INSTITUTIONAL_CCI_OVERLAY) {
  basis <- build_credit_regime_basis(master$date)
  cci_input <- master %>%
    transmute(
      date,
      housing_loan_flow,
      house_price_index      = hpi,
      debt_income_ratio      = debt_y,
      first_home_buyer_share = fhb_share,
      real_mortgage_rate     = real_rate
    )
  inst_cci <- construct_institutional_cci(cci_input, basis)
  master <- master %>%
    left_join(inst_cci %>% select(date, cci_institutional_raw), by = "date") %>%
    mutate(
      cci_ratio = ifelse(is.na(cci_ratio), cci_institutional_raw, cci_ratio)
    ) %>%
    select(-cci_institutional_raw)
  message("  Using institutional CCI (Muellbauer-style regime+indicator blend) ",
          "to backfill pre-2002 cci_ratio.")
}

# (2) Williams SDMMA candidate basis — attached so the estimation step can fit
#     Specs 8/9/10 (CCI interactions). CANONICAL = ON. Independent of (1).
if (USE_WILLIAMS_SDMMA_BASIS) {
  williams_basis <- build_williams_cci_basis(master$date)
  for (j in seq_len(ncol(williams_basis))) {
    master[[colnames(williams_basis)[j]]] <- williams_basis[, j]
  }
  message("  Maximal-GETS CCI basis added to master (15 SDMMA candidate ",
          "knots; cci_williams fitted series will be computed in ",
          "estimation step).")
}

# Kalman state-space CCI (always built when KFAS is available; provides a
# methodologically-distinct alternative to the spline-based cci_williams).
# Indicators: log(housing_loan_flow), log(debt_y), fhb_share, mortgage_burden,
# Δ(mortgage_rate). Each is z-standardised inside fit_kalman_cci() on its own
# non-NA window. Single common factor identified by anchoring loading on
# log(housing_loan_flow) at +1. KFAS handles missing observations (e.g. flow
# data pre-2002) by extending uncertainty during unobserved periods rather
# than dropping rows.
if (requireNamespace("KFAS", quietly = TRUE)) {
  k_in <- master %>%
    transmute(
      date,
      log_housing_loan_flow = log(pmax(housing_loan_flow, 1e-9)),
      log_debt_y            = log(pmax(debt_y, 1e-9)),
      fhb_share,
      mortgage_burden,
      mortgage_rate
    )
  cci_k <- tryCatch(
    fit_kalman_cci(k_in,
                    indicator_cols = c("log_housing_loan_flow",
                                        "log_debt_y",
                                        "fhb_share",
                                        "mortgage_burden",
                                        "mortgage_rate_spread"),
                    anchor_col = "log_housing_loan_flow",
                    verbose = TRUE),
    error = function(e) {
      message("  Kalman CCI: estimation failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(cci_k)) {
    master <- master %>% left_join(cci_k, by = "date")
    message(sprintf("  Kalman CCI (cci_kalman): %d non-NA obs, range %.2f - %.2f",
      sum(!is.na(master$cci_kalman)),
      min(master$cci_kalman, na.rm = TRUE),
      max(master$cci_kalman, na.rm = TRUE)))
  }
} else {
  message("  KFAS not available; Kalman CCI skipped")
}

# Print coverage
message("\n--- Coverage summary ---")
for (v in c("cons_real_pc", "ydi_real_pc", "unemp_rate", "mortgage_rate",
            "real_rate", "hpi", "ha_y", "eq_y", "super_y",
            "nla_y", "nla_y_unrestricted", "debt_y", "networth_y",
            "cci_ratio", "fhb_share", "prime_age_share", "mortgage_burden",
            "labour_force", "lf_share",
            "labour_transfer_real_pc", "scaled_income_real_pc",
            "npy_real_pc", "property_tax_share",
            "mortgage_interest_burden_rba", "mortgage_payment_burden_rba",
            "cci_kalman")) {
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

# Hard data-construction assertions. Failures here mean ABS changed a unit,
# vintage, or series structure and downstream estimation results would be
# silently wrong. Better to halt here than to publish spurious coefficients.
stopifnot(
  "master has wrong row count (expected 194 quarters 1976Q3-2024Q4)" =
    nrow(master) == 194,
  "ha_y out of plausible range (expected ~1-10)" =
    min(master$ha_y, na.rm = TRUE) > 1 &&
    max(master$ha_y, na.rm = TRUE) < 10,
  "networth_y out of plausible range (expected ~2-12)" =
    min(master$networth_y, na.rm = TRUE) > 2 &&
    max(master$networth_y, na.rm = TRUE) < 12,
  "mortgage_rate out of plausible range (expected ~1-25%)" =
    min(master$mortgage_rate, na.rm = TRUE) > 1 &&
    max(master$mortgage_rate, na.rm = TRUE) < 25,
  "Insufficient complete cases for core ECM variables" =
    sum(complete.cases(master[, c("d_ln_cons_pc", "ln_y_over_c",
                                   "real_rate", "ln_ydi_real_pc",
                                   "ha_y")])) >= 130,
  "Quarterly date spacing is irregular" =
    all(as.numeric(diff(master$date)) %in% 89:92)
)

if ("prime_age_share" %in% names(master)) {
  stopifnot(
    "prime_age_share out of plausible range (expected 0.35-0.50)" =
      min(master$prime_age_share, na.rm = TRUE) > 0.35 &&
      max(master$prime_age_share, na.rm = TRUE) < 0.50
  )
}
if ("mortgage_burden" %in% names(master)) {
  stopifnot(
    "mortgage_burden out of plausible range (expected 0.02-0.20)" =
      min(master$mortgage_burden, na.rm = TRUE) > 0.02 &&
      max(master$mortgage_burden, na.rm = TRUE) < 0.20
  )
}

# ==============================================================================
# SECTION 5: Dummy variables
# ==============================================================================

message("\n--- Section 5: Dummy variables ---")

# Logistic ogive transition: smooth 0->1 transition centred at t0 (in
# quarter units after rescaling). half_width = 2.5 gives ~5-quarter ramp.
ogive <- function(t, t0, half_width = 2.5) {
  1 / (1 + exp(-(t - t0) / half_width))
}

master <- master %>%
  mutate(
    # GST introduction Q3 2000 — large one-off boost to nominal consumption
    d2000_gst    = as.integer(date == as.Date("2000-07-01")),
    # Global Financial Crisis (sharp consumption drop Q3-Q4 2008)
    d2008_gfc    = as.integer(date == as.Date("2008-07-01")),
    # COVID lockdown Q2 2020 (consumption collapse)
    d2020_covid  = as.integer(date == as.Date("2020-04-01")),
    # COVID rebound Q3 2020 (partial mechanical reversal)
    d2020_rebound = as.integer(date == as.Date("2020-07-01")),
    # Negative-gearing restriction 1985Q3 to 1987Q3 (9 quarters). Australia
    # paper finds coef ~ -0.029 in the house-price equation for this episode.
    d_neg_gearing_8587 = as.integer(date >= as.Date("1985-07-01") &
                                    date <= as.Date("1987-07-01")),
    # "Recession we had to have" — sharpest single-quarter pre-COVID drop.
    d_recession_1991   = as.integer(date == as.Date("1991-04-01")),
    # APRA macroprudential rounds: investor-loan caps (late 2014) and IO-loan
    # caps (early 2017). Logistic transition (~5 quarters) so the dummy moves
    # from 0 well before the announcement to 1 well after.
    d_apra_2014 = ogive(as.numeric(date - as.Date("2014-10-01")) / 91.31, 0),
    d_apra_2017 = ogive(as.numeric(date - as.Date("2017-04-01")) / 91.31, 0),
    # JobKeeper income support, 2020Q2-2021Q1 (4 quarters). Distinct from the
    # COVID consumption-shock impulse dummies above.
    d_jobkeeper_2020   = as.integer(date >= as.Date("2020-04-01") &
                                    date <= as.Date("2021-01-01"))
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
