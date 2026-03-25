#!/usr/bin/env Rscript
# =============================================================================
# Italy consumption function: replication of
#   De Bonis, Liberati, Muellbauer, Rondinelli (2025)
#   "Why net worth is the wrong concept for explaining consumption:
#    evidence from Italy"
# Data: 1980Q1-2019Q4 (quarterly)
#
# PART 1 — Data download and construction
#
# Sources:
#   - Eurostat REST API (national accounts, HICP, unemployment, house prices)
#   - ECB Statistical Data Warehouse REST API (MIR mortgage rates)
#   - BIS residential property prices (house price fallback)
#   - Back-extrapolation for pre-1999 financial balance sheet data following
#     the methodology described in Bonci & Coletta (2008)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(httr)
  library(jsonlite)
  library(readxl)
  library(stringr)
  library(zoo)
  library(tibble)
})

options(stringsAsFactors = FALSE, scipen = 999)

# =============================================================================
# SECTION 1: Setup — paths and date spine
# =============================================================================

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
raw_dir      <- file.path(project_root, "data_raw_italy")
output_dir   <- file.path(project_root, "outputs")

dir.create(raw_dir,    recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

START_DATE <- as.Date("1980-01-01")   # 1980Q1
END_DATE   <- as.Date("2019-10-01")   # 2019Q4

# Quarterly date spine: first day of each quarter
date_spine <- tibble(
  date = seq(START_DATE, END_DATE, by = "quarter")
)
message("Date spine: ", nrow(date_spine), " quarters from 1980Q1 to 2019Q4")

# =============================================================================
# SECTION 1b: Helper functions
# =============================================================================

# Parse Eurostat time strings to Date.
# Handles: "YYYY-Qq" / "YYYYQq" (quarterly), "YYYY-MM" (monthly), "YYYYMmm" (monthly), "YYYY" (annual)
parse_eurostat_time <- function(x) {
  x        <- as.character(x)
  out      <- rep(as.Date(NA), length(x))

  # Quarterly: contains "Q" (case-sensitive) e.g. "2020-Q1" or "2020Q1"
  is_q <- grepl("Q", x, fixed = TRUE)
  if (any(is_q)) {
    xq <- sub("^(\\d{4})-?Q([1-4])$", "\\1-Q\\2", x[is_q])
    yr <- suppressWarnings(as.integer(sub("^(\\d{4})-Q([1-4])$", "\\1", xq)))
    qn <- suppressWarnings(as.integer(sub("^(\\d{4})-Q([1-4])$", "\\2", xq)))
    ok <- !is.na(yr) & !is.na(qn)
    mo <- (qn[ok] - 1L) * 3L + 1L
    out[which(is_q)[ok]] <- as.Date(paste0(yr[ok], "-", sprintf("%02d", mo), "-01"))
  }

  # Monthly "YYYYMmm": e.g. "2020M01" or "1996M10"
  is_m_code <- grepl("^\\d{4}M\\d{2}$", x) & !is_q
  if (any(is_m_code)) {
    yr <- as.integer(substr(x[is_m_code], 1, 4))
    mo <- as.integer(substr(x[is_m_code], 6, 7))
    out[is_m_code] <- as.Date(paste0(yr, "-", sprintf("%02d", mo), "-01"))
  }

  # Monthly "YYYY-MM": e.g. "2020-01"
  is_m_dash <- grepl("^\\d{4}-\\d{2}$", x) & !is_q & !is_m_code
  if (any(is_m_dash)) {
    out[is_m_dash] <- as.Date(paste0(x[is_m_dash], "-01"))
  }

  # Annual "YYYY": exactly 4 digits
  is_a <- grepl("^\\d{4}$", x) & !is_q & !is_m_code & !is_m_dash
  if (any(is_a)) {
    out[is_a] <- as.Date(paste0(x[is_a], "-01-01"))
  }

  out
}

# Parse ECB time strings: "YYYY-Qq" (quarterly) or "YYYY-MM" (monthly)
parse_ecb_time <- function(x) {
  x <- as.character(x)
  is_q <- grepl("Q", x)
  result <- as.Date(NA)
  if (any(is_q)) {
    yr <- as.integer(sub("^(\\d{4})-Q(\\d)$", "\\1", x[is_q]))
    qn <- as.integer(sub("^(\\d{4})-Q(\\d)$", "\\2", x[is_q]))
    mo <- (qn - 1L) * 3L + 1L
    result[is_q] <- as.Date(paste0(yr, "-", sprintf("%02d", mo), "-01"))
  }
  if (any(!is_q)) {
    result[!is_q] <- as.Date(paste0(x[!is_q], "-01"))
  }
  result
}

# Fetch data from Eurostat JSON API with vectorised stride decoding
fetch_eurostat <- function(dataset_id, filters, timeout_sec = 120) {
  base_url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
    dataset_id
  )
  filter_str <- paste(
    paste0(names(filters), "=", unlist(filters)),
    collapse = "&"
  )
  url <- paste0(base_url, "?format=JSON&lang=en&", filter_str)
  message("  GET ", url)

  resp <- tryCatch(
    GET(url, timeout(timeout_sec)),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("  HTTP error or no response")
    return(NULL)
  }
  body <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                   error = function(e) { message("  JSON parse error: ", e$message); NULL })
  if (is.null(body) || is.null(body$value) || length(body$value) == 0) {
    message("  No values returned")
    return(NULL)
  }

  # Vectorised stride decoding
  dims       <- body$id
  dim_sizes  <- unlist(body$size)
  n_dims     <- length(dim_sizes)

  dim_labels <- lapply(dims, function(d) {
    idx <- body$dimension[[d]]$category$index
    lbl <- names(idx)
    pos <- unlist(idx) + 1L
    v   <- character(max(pos))
    v[pos] <- lbl
    v
  })
  names(dim_labels) <- dims

  val_pos    <- as.integer(names(body$value))
  val_values <- unlist(body$value)

  strides <- vapply(seq_len(n_dims), function(d) {
    if (d < n_dims) prod(dim_sizes[(d + 1):n_dims]) else 1L
  }, numeric(1))

  df <- data.frame(value = val_values, stringsAsFactors = FALSE)
  for (d in seq_len(n_dims)) {
    idx_0       <- (val_pos %/% strides[d]) %% dim_sizes[d]
    df[[dims[d]]] <- dim_labels[[d]][idx_0 + 1L]
  }

  df
}

# Fetch data from ECB SDW CSV API
# series_key is split on the FIRST dot: everything before is the flow,
# everything after is the key.  E.g. "MIR.M.IT..." → flow=MIR, key=M.IT...
fetch_ecb_sdw <- function(series_key, timeout_sec = 120) {
  dot_pos  <- regexpr("\\.", series_key)[[1]]
  flow     <- substr(series_key, 1, dot_pos - 1)
  key_part <- substr(series_key, dot_pos + 1, nchar(series_key))
  url <- paste0(
    "https://data-api.ecb.europa.eu/service/data/",
    flow, "/", key_part,
    "?format=csvdata"
  )
  message("  GET ", url)
  resp <- tryCatch(
    GET(url, timeout(timeout_sec)),
    error = function(e) { message("  ERROR: ", e$message); NULL }
  )
  if (is.null(resp) || http_error(resp)) {
    message("  HTTP error or no response")
    return(NULL)
  }
  txt <- content(resp, "text", encoding = "UTF-8")
  df  <- tryCatch(read.csv(text = txt, stringsAsFactors = FALSE),
                  error = function(e) { message("  CSV parse error: ", e$message); NULL })
  df
}

# ==============================================================================
# OECD QNA helper: fetch Italian quarterly national accounts via CSV endpoint
# The SDMX-JSON endpoint returns 8MB regardless of filter (unmanageable).
# The RestSDMX CSV endpoint at stats.oecd.org returns a compact, filtered CSV.
# subjects: OECD subject codes, e.g. c("P3S14_S15", "B6GS14_S15")
# measure:  "LNBQRSA" (real chain-linked, SA) or "CARSA" (nominal, SA)
# ==============================================================================
fetch_oecd_qna <- function(subjects,
                           measure     = "LNBQRSA",
                           start_time  = "1987-Q1",
                           end_time    = "2020-Q4",
                           timeout_sec = 90) {
  # Build OECD RestSDMX key: LOCATION.SUBJECT.MEASURE.FREQUENCY
  subj_str <- paste(subjects, collapse = "+")
  key <- sprintf("ITA.%s.%s.Q", subj_str, measure)

  # Try RestSDMX CSV endpoint (compact, filtered output)
  url_csv <- sprintf(
    "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/%s/OECD?startTime=%s&endTime=%s&format=csv",
    key, start_time, end_time
  )
  message("  OECD CSV GET ", url_csv)

  resp <- tryCatch(
    GET(url_csv, timeout(timeout_sec)),
    error = function(e) { message("  OECD connect error: ", e$message); NULL }
  )
  if (!is.null(resp) && status_code(resp) == 200) {
    txt <- content(resp, "text", encoding = "UTF-8")
    if (nchar(txt) > 100) {
      df <- tryCatch(
        read.csv(text = txt, stringsAsFactors = FALSE),
        error = function(e) { message("  OECD CSV parse error: ", e$message); NULL }
      )
      if (!is.null(df) && nrow(df) > 0) {
        message("  OECD CSV: ", nrow(df), " rows, cols: ",
                paste(names(df)[1:min(8, ncol(df))], collapse = ", "))
        return(df)
      }
    }
    message("  OECD CSV: empty or too small (", nchar(txt), " chars)")
  } else {
    message("  OECD CSV status: ", if (!is.null(resp)) status_code(resp) else "NULL")
  }

  # Fallback: try the SDMX-JSON endpoint but only fetch 2 years to keep size small
  url_json <- sprintf(
    "https://stats.oecd.org/SDMX-JSON/data/QNA/ITA.%s.%s.Q/all?startTime=%s&endTime=%s",
    subj_str, measure, start_time, end_time
  )
  message("  OECD JSON fallback: ", url_json)
  resp2 <- tryCatch(
    GET(url_json, timeout(timeout_sec)),
    error = function(e) { message("  OECD JSON connect error: ", e$message); NULL }
  )
  if (is.null(resp2) || status_code(resp2) != 200) {
    message("  OECD JSON status: ", if (!is.null(resp2)) status_code(resp2) else "NULL")
    return(NULL)
  }
  txt2 <- content(resp2, "text", encoding = "UTF-8")
  message("  OECD JSON response size: ", nchar(txt2), " chars")
  if (nchar(txt2) > 3000000) {
    message("  OECD JSON too large (>3MB) — skipping to avoid OOM crash")
    return(NULL)
  }
  js <- tryCatch(
    fromJSON(txt2, simplifyVector = FALSE),
    error = function(e) { message("  OECD JSON parse error: ", e$message); NULL }
  )
  if (is.null(js)) return(NULL)

  # Navigate new JSON structure: js$data$dataSets / js$data$structures
  ds     <- tryCatch(js$data$dataSets[[1]],   error = function(e) NULL)
  struct <- tryCatch(js$data$structures[[1]],  error = function(e) NULL)
  if (is.null(ds) || is.null(struct)) {
    message("  OECD JSON: unexpected structure")
    return(NULL)
  }

  dims_ser <- struct$dimensions$series
  dim_ids  <- sapply(dims_ser, function(d) d$id)
  subj_idx <- which(dim_ids == "SUBJECT")
  if (length(subj_idx) == 0) {
    message("  OECD JSON: SUBJECT dim not found. dims: ", paste(dim_ids, collapse = ", "))
    return(NULL)
  }
  subj_codes <- sapply(dims_ser[[subj_idx]]$values, function(x) x$id)

  dim_obs   <- struct$dimensions$observation
  time_vals <- sapply(dim_obs[[1]]$values, function(x) x$id)

  series_list <- ds$series
  if (is.null(series_list) || length(series_list) == 0) return(NULL)

  out_rows <- list()
  for (sk in names(series_list)) {
    parts     <- as.integer(strsplit(sk, ":")[[1]])
    subj_code <- tryCatch(subj_codes[parts[subj_idx] + 1L], error = function(e) NA_character_)
    if (is.na(subj_code)) next
    obs_data <- series_list[[sk]]$observations
    if (length(obs_data) == 0) next
    obs_idx  <- as.integer(names(obs_data)) + 1L
    obs_vals <- sapply(obs_data, function(x) {
      if (length(x) >= 1L && !is.null(x[[1L]])) as.numeric(x[[1L]]) else NA_real_
    })
    valid <- obs_idx >= 1L & obs_idx <= length(time_vals)
    if (!any(valid)) next
    out_rows[[sk]] <- tibble(
      time    = time_vals[obs_idx[valid]],
      value   = obs_vals[valid],
      subject = subj_code
    )
  }
  if (length(out_rows) == 0) return(NULL)
  bind_rows(out_rows)
}

# Parse OECD quarter string "YYYY-QN" to first day of quarter
parse_oecd_quarter <- function(qt_str) {
  parts <- strsplit(as.character(qt_str), "-Q")
  dates <- sapply(parts, function(p) {
    if (length(p) != 2L) return(NA_integer_)
    yr <- suppressWarnings(as.integer(p[1L]))
    q  <- suppressWarnings(as.integer(p[2L]))
    if (is.na(yr) || is.na(q) || q < 1L || q > 4L) return(NA_integer_)
    as.integer(as.Date(sprintf("%04d-%02d-01", yr, (q - 1L) * 3L + 1L)))
  })
  as.Date(dates, origin = "1970-01-01")
}

# Average monthly data to quarterly (group by year + quarter of date column)
monthly_to_quarterly <- function(df, date_col = "date", value_col = "value") {
  df %>%
    mutate(
      yr  = year(.data[[date_col]]),
      qtr = quarter(.data[[date_col]])
    ) %>%
    group_by(yr, qtr) %>%
    summarise(value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(date = as.Date(paste0(yr, "-", sprintf("%02d", (qtr - 1L) * 3L + 1L), "-01"))) %>%
    select(date, value)
}

# Cache-or-fetch: save raw RDS, re-fetch if cached result is empty
cache_or_fetch <- function(name, fetch_fn, raw_dir) {
  cache_file <- file.path(raw_dir, paste0(name, ".rds"))
  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (!is.null(cached) && nrow(cached) > 0) {
      message("  [cached] ", name)
      return(cached)
    } else {
      message("  [cache empty/null — re-fetching] ", name)
      file.remove(cache_file)
    }
  }
  result <- fetch_fn()
  if (!is.null(result) && nrow(result) > 0) {
    saveRDS(result, cache_file)
    message("  [saved] ", name)
  }
  result
}

# Report helper: print obs count and date range after each download
report_download <- function(name, df, date_col = "date", value_col = "value") {
  if (is.null(df) || nrow(df) == 0) {
    message("  ", name, ": FAILED or empty")
    return(invisible(NULL))
  }
  vals <- df[[value_col]]
  n_obs <- sum(!is.na(vals))
  dates <- df[[date_col]]
  message(sprintf("  %s: %d non-NA obs, %s to %s",
                  name, n_obs, min(dates, na.rm=TRUE), max(dates, na.rm=TRUE)))
}

# =============================================================================
# SECTION 2: Downloads
# =============================================================================

message("\n--- Section 2: Downloading data ---\n")

# ---------------------------------------------------------------------------
# 2.1 Real household consumption (chain-linked, NSA)
# ---------------------------------------------------------------------------
message("2.1 Real consumption (namq_10_gdp, P31_S14_S15, CLV10_MEUR)")
raw_cons_real <- cache_or_fetch("cons_real", function() {
  fetch_eurostat("namq_10_gdp", list(
    na_item = "P31_S14_S15",
    unit    = "CLV10_MEUR",
    geo     = "IT"
  ))
}, raw_dir)

if (!is.null(raw_cons_real)) {
  # The dataset returns s_adj dimension with multiple levels; keep NSA
  if ("s_adj" %in% names(raw_cons_real)) {
    raw_cons_real <- raw_cons_real %>% filter(s_adj == "NSA")
  }
  raw_cons_real <- raw_cons_real %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, cons_real = value)
  report_download("cons_real", raw_cons_real, value_col = "cons_real")
}

# ---------------------------------------------------------------------------
# 2.2 Nominal household consumption (for deflator construction)
# ---------------------------------------------------------------------------
message("2.2 Nominal consumption (namq_10_gdp, P31_S14_S15, CP_MEUR)")
raw_cons_nom <- cache_or_fetch("cons_nom", function() {
  fetch_eurostat("namq_10_gdp", list(
    na_item = "P31_S14_S15",
    unit    = "CP_MEUR",
    geo     = "IT"
  ))
}, raw_dir)

if (!is.null(raw_cons_nom)) {
  if ("s_adj" %in% names(raw_cons_nom)) {
    raw_cons_nom <- raw_cons_nom %>% filter(s_adj == "NSA")
  }
  raw_cons_nom <- raw_cons_nom %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, cons_nom = value)
  report_download("cons_nom", raw_cons_nom, value_col = "cons_nom")
}

# ---------------------------------------------------------------------------
# 2.3 Nominal gross disposable income (households + NPISH, NSA)
# ---------------------------------------------------------------------------
message("2.3 Gross disposable income (nasq_10_nf_tr, B6G, S14_S15, CP_MEUR, NSA)")
raw_ydi <- cache_or_fetch("ydi_nom", function() {
  fetch_eurostat("nasq_10_nf_tr", list(
    na_item = "B6G",
    sector  = "S14_S15",
    unit    = "CP_MEUR",
    s_adj   = "NSA",
    geo     = "IT"
  ))
}, raw_dir)

if (!is.null(raw_ydi)) {
  # Dataset has "direct" dimension (PAID/RECEIVED); keep PAID
  if ("direct" %in% names(raw_ydi)) {
    raw_ydi <- raw_ydi %>% filter(direct == "PAID")
  }
  raw_ydi <- raw_ydi %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, ydi_nom = value)
  report_download("ydi_nom", raw_ydi, value_col = "ydi_nom")
}

# ---------------------------------------------------------------------------
# 2.4 Nominal GDP (for credit conditions denominator)
# ---------------------------------------------------------------------------
message("2.4 Nominal GDP (namq_10_gdp, B1GQ, CP_MEUR, NSA)")
raw_gdp <- cache_or_fetch("gdp_nom", function() {
  fetch_eurostat("namq_10_gdp", list(
    na_item = "B1GQ",
    unit    = "CP_MEUR",
    s_adj   = "NSA",
    geo     = "IT"
  ))
}, raw_dir)

if (!is.null(raw_gdp)) {
  raw_gdp <- raw_gdp %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, gdp_nom = value)
  report_download("gdp_nom", raw_gdp, value_col = "gdp_nom")
}

# ---------------------------------------------------------------------------
# 2.5 Population (annual; interpolate to quarterly)
# ---------------------------------------------------------------------------
message("2.5 Population (demo_pjan, sex=T, age=TOTAL)")
raw_pop_annual <- cache_or_fetch("pop_annual", function() {
  fetch_eurostat("demo_pjan", list(
    sex = "T",
    age = "TOTAL",
    geo = "IT"
  ))
}, raw_dir)

raw_pop <- NULL
if (!is.null(raw_pop_annual)) {
  pop_ann <- raw_pop_annual %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date) %>%
    select(date, pop = value)

  # Cubic spline interpolation from annual to quarterly
  all_q <- date_spine$date
  spline_fit <- spline(
    x    = as.numeric(pop_ann$date),
    y    = pop_ann$pop,
    xout = as.numeric(all_q)
  )
  raw_pop <- tibble(
    date         = all_q,
    pop_millions = spline_fit$y / 1e6
  )
  message(sprintf("  pop: %d quarterly obs (spline from %d annual obs)",
                  nrow(raw_pop), nrow(pop_ann)))
}

# ---------------------------------------------------------------------------
# 2.6 Unemployment rate (seasonally adjusted, % of active population)
#     NOTE: do NOT include age= filter — returns empty; filter TOTAL after download
# ---------------------------------------------------------------------------
message("2.6 Unemployment rate (une_rt_q, sex=T, s_adj=SA, unit=PC_ACT)")
raw_unemp <- cache_or_fetch("unemp", function() {
  fetch_eurostat("une_rt_q", list(
    sex    = "T",
    s_adj  = "SA",
    unit   = "PC_ACT",
    geo    = "IT"
  ))
}, raw_dir)

if (!is.null(raw_unemp)) {
  # Filter to the broadest available age group.
  # Eurostat une_rt_q age codes: "TOTAL" (if present), or "Y15-74", "Y_GE15", etc.
  if ("age" %in% names(raw_unemp)) {
    age_vals <- unique(raw_unemp$age)
    preferred <- c("TOTAL", "Y_GE15", "Y15-74", "Y15-64")
    pick_age  <- intersect(preferred, age_vals)
    if (length(pick_age) > 0) {
      raw_unemp <- raw_unemp %>% filter(age == pick_age[1])
      message("  Unemployment age group used: ", pick_age[1])
    } else {
      # Fall back to the age group with most observations
      most_common_age <- raw_unemp %>% count(age, sort = TRUE) %>% slice(1) %>% pull(age)
      raw_unemp <- raw_unemp %>% filter(age == most_common_age)
      message("  Unemployment age group used (fallback): ", most_common_age)
    }
  }
  raw_unemp <- raw_unemp %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, unemp_rate = value)
  report_download("unemp_rate", raw_unemp, value_col = "unemp_rate")
}

# ---------------------------------------------------------------------------
# 2.7 HICP (monthly; average to quarterly deflator fallback)
# ---------------------------------------------------------------------------
message("2.7 HICP (prc_hicp_midx, coicop=CP00, unit=I15, monthly)")
raw_hicp_monthly <- cache_or_fetch("hicp_monthly", function() {
  fetch_eurostat("prc_hicp_midx", list(
    coicop = "CP00",
    unit   = "I15",
    geo    = "IT"
  ))
}, raw_dir)

raw_hicp <- NULL
if (!is.null(raw_hicp_monthly)) {
  hicp_m <- raw_hicp_monthly %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, value)
  # Average monthly HICP to quarterly
  raw_hicp <- monthly_to_quarterly(hicp_m) %>%
    rename(hicp = value)
  report_download("hicp", raw_hicp, value_col = "hicp")
}

# ---------------------------------------------------------------------------
# 2.8 Financial balance sheet (nasq_10_f_bs) — households + NPISH
#     Download each instrument separately; filter finpos after download
# ---------------------------------------------------------------------------

fetch_fin_bs <- function(na_item_code) {
  fetch_eurostat("nasq_10_f_bs", list(
    sector  = "S14_S15",
    unit    = "MIO_NAC",
    na_item = na_item_code,
    geo     = "IT"
  ))
}

process_fin_bs <- function(df, finpos_keep) {
  if (is.null(df)) return(NULL)
  if ("finpos" %in% names(df)) {
    # Eurostat nasq_10_f_bs uses "ASS" (not "ASSET") for financial assets
    keep_vals <- if (finpos_keep == "ASSET") c("ASSET", "ASS") else finpos_keep
    df <- df %>% filter(finpos %in% keep_vals)
  }
  df %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, value)
}

message("2.8a Deposits F2 (ASSET)")
raw_f2 <- cache_or_fetch("fin_f2", function() fetch_fin_bs("F2"), raw_dir)
fin_deposits <- process_fin_bs(raw_f2, "ASSET")
report_download("fin_deposits (F2 ASSET)", fin_deposits)

message("2.8b Debt securities F3 (ASSET)")
raw_f3 <- cache_or_fetch("fin_f3", function() fetch_fin_bs("F3"), raw_dir)
fin_debtsec <- process_fin_bs(raw_f3, "ASSET")
report_download("fin_debtsec (F3 ASSET)", fin_debtsec)

message("2.8c Loans F4 (LIAB)")
raw_f4 <- cache_or_fetch("fin_f4", function() fetch_fin_bs("F4"), raw_dir)
fin_loans <- process_fin_bs(raw_f4, "LIAB")
report_download("fin_loans (F4 LIAB)", fin_loans)

message("2.8d Equity/shares F5 (ASSET)")
raw_f5 <- cache_or_fetch("fin_f5", function() fetch_fin_bs("F5"), raw_dir)
fin_equity <- process_fin_bs(raw_f5, "ASSET")
report_download("fin_equity (F5 ASSET)", fin_equity)

message("2.8e Mutual fund shares F52 (ASSET)")
raw_f52 <- cache_or_fetch("fin_f52", function() fetch_fin_bs("F52"), raw_dir)
fin_mutfund <- process_fin_bs(raw_f52, "ASSET")
report_download("fin_mutfund (F52 ASSET)", fin_mutfund)

message("2.8f Insurance/pensions F6 (ASSET)")
raw_f6 <- cache_or_fetch("fin_f6", function() fetch_fin_bs("F6"), raw_dir)
fin_inspen <- process_fin_bs(raw_f6, "ASSET")
report_download("fin_inspen (F6 ASSET)", fin_inspen)

message("2.8g Total financial F (ASSET and LIAB)")
raw_f_total <- cache_or_fetch("fin_f_total", function() fetch_fin_bs("F"), raw_dir)
fin_total_asset <- NULL
fin_total_liab  <- NULL
if (!is.null(raw_f_total) && "finpos" %in% names(raw_f_total)) {
  fin_total_asset <- raw_f_total %>%
    filter(finpos %in% c("ASSET", "ASS")) %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, value)
  fin_total_liab <- raw_f_total %>%
    filter(finpos == "LIAB") %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, value)
  report_download("fin_total_asset (F ASSET)", fin_total_asset)
  report_download("fin_total_liab (F LIAB)", fin_total_liab)
}

# ---------------------------------------------------------------------------
# 2.9 ECB MIR mortgage rate (monthly new business, house purchase)
#     Series: MIR.M.IT.B.A2C.AM.R.A.2250.EUR.N
#     Split on first dot: flow=MIR, key=M.IT.B.A2C.AM.R.A.2250.EUR.N
# ---------------------------------------------------------------------------
message("2.9 ECB MIR mortgage rate")
raw_mir_monthly <- cache_or_fetch("mir_monthly", function() {
  fetch_ecb_sdw("MIR.M.IT.B.A2C.AM.R.A.2250.EUR.N")
}, raw_dir)

raw_mir <- NULL
if (!is.null(raw_mir_monthly)) {
  # ECB csvdata returns TIME_PERIOD and OBS_VALUE columns
  time_col  <- intersect(c("TIME_PERIOD", "time_period"), names(raw_mir_monthly))[1]
  value_col <- intersect(c("OBS_VALUE", "obs_value"), names(raw_mir_monthly))[1]
  if (!is.na(time_col) && !is.na(value_col)) {
    mir_m <- raw_mir_monthly %>%
      mutate(
        date  = parse_ecb_time(.data[[time_col]]),
        value = suppressWarnings(as.numeric(.data[[value_col]]))
      ) %>%
      filter(!is.na(date), !is.na(value)) %>%
      select(date, value)
    raw_mir <- monthly_to_quarterly(mir_m) %>%
      rename(mortgage_rate = value)
    report_download("mortgage_rate", raw_mir, value_col = "mortgage_rate")
  }
}

# ---------------------------------------------------------------------------
# 2.10 House price index (quarterly, Eurostat)
#      prc_hpi_q — NO purchase_type filter; filter after download
# ---------------------------------------------------------------------------
message("2.10 House price index (prc_hpi_q)")
raw_hpi <- cache_or_fetch("hpi", function() {
  fetch_eurostat("prc_hpi_q", list(geo = "IT"))
}, raw_dir)

hpi_eurostat <- NULL
if (!is.null(raw_hpi)) {
  hpi_df <- raw_hpi
  # Keep I15_Q unit and TOTAL purchase type (or DW as fallback)
  if ("unit" %in% names(hpi_df)) {
    hpi_df <- hpi_df %>% filter(unit == "I15_Q")
  }
  # Column may be named "purchase_type" or "purchase" depending on API version
  pt_col <- intersect(c("purchase_type", "purchase"), names(hpi_df))[1]
  if (!is.na(pt_col)) {
    pt_vals <- unique(hpi_df[[pt_col]])
    if ("TOTAL" %in% pt_vals) {
      hpi_df <- hpi_df %>% filter(.data[[pt_col]] == "TOTAL")
    } else if ("DW" %in% pt_vals) {
      hpi_df <- hpi_df %>% filter(.data[[pt_col]] == "DW")
    } else {
      # Keep first purchase type found
      hpi_df <- hpi_df %>% filter(.data[[pt_col]] == pt_vals[1])
    }
  }
  hpi_eurostat <- hpi_df %>%
    mutate(date = parse_eurostat_time(time)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    select(date, hpi = value)
  report_download("hpi_eurostat", hpi_eurostat, value_col = "hpi")
}

# ---------------------------------------------------------------------------
# 2.11 BIS house price index (long-run extension, xlsx download)
# ---------------------------------------------------------------------------
message("2.11 BIS house price index (long-run)")
bis_url    <- "https://www.bis.org/statistics/pp/pp_detailed.xlsx"
bis_file   <- file.path(raw_dir, "bis_hpi.xlsx")
hpi_bis    <- NULL

if (!file.exists(bis_file)) {
  dl <- tryCatch(
    download.file(bis_url, destfile = bis_file, mode = "wb", quiet = TRUE),
    error = function(e) { message("  BIS download failed: ", e$message); -1L }
  )
  if (dl != 0) file.remove(bis_file)
}

if (file.exists(bis_file)) {
  tryCatch({
    sheets <- excel_sheets(bis_file)
    q_sheet <- sheets[grepl("Quarterly", sheets, ignore.case = TRUE)][1]
    if (!is.na(q_sheet)) {
      # Read all as text; row 1 = series names (col headers), rows 2-4 = metadata
      df_raw <- suppressMessages(
        read_excel(bis_file, sheet = q_sheet, col_types = "text")
      )
      # Find Italy "all dwellings, whole country" (exclude Rome/Milan sub-regions)
      it_idx <- which(grepl("Italy.*all dwellings.*whole country",
                            names(df_raw), ignore.case = TRUE))
      it_idx <- it_idx[!grepl("Rome|Milan", names(df_raw)[it_idx])][1]
      if (!is.na(it_idx)) {
        # Skip 3 metadata rows (units, country, series code)
        data_rows <- df_raw[4:nrow(df_raw), ]
        hpi_bis <- data_rows %>%
          select(date_raw = 1, hpi_raw = all_of(it_idx)) %>%
          mutate(
            date_num = suppressWarnings(as.numeric(date_raw)),
            # Convert Excel serial date, snap end-of-quarter to start-of-quarter
            date_eod = as.Date(date_num, origin = "1899-12-30"),
            date     = lubridate::floor_date(date_eod, "quarter"),
            hpi_bis  = suppressWarnings(as.numeric(hpi_raw))
          ) %>%
          filter(!is.na(date), !is.na(hpi_bis)) %>%
          select(date, hpi_bis)
        message(sprintf("  BIS HPI Italy: %d obs, %s to %s (base 2015=100)",
                        nrow(hpi_bis), min(hpi_bis$date), max(hpi_bis$date)))
      } else {
        message("  BIS: Italy column not found in Quarterly Series")
        hpi_bis <- NULL
      }
    } else {
      message("  BIS: no Quarterly Series sheet. Available: ",
              paste(sheets, collapse = ", "))
      hpi_bis <- NULL
    }
  }, error = function(e) {
    message("  BIS parse failed: ", e$message)
    hpi_bis <- NULL
  })
}

# ---------------------------------------------------------------------------
# 2.12 OECD QNA quarterly data (back-extension for consumption and income)
# ---------------------------------------------------------------------------
# Italy household consumption (P3S14_S15) and gross disposable income
# (B6GS14_S15) in real chain-linked volumes, seasonally adjusted.
# Measure LNBQRSA = linked chained volume, SA, annual benchmark.
# These extend Eurostat coverage back to ~1990Q1.
# ---------------------------------------------------------------------------
message("2.12 OECD QNA Italy: household consumption + disposable income")
oecd_qna_raw <- NULL
tryCatch({
  # Try primary measure (real chain-linked, SA)
  oecd_qna_raw <- fetch_oecd_qna(
    subjects   = c("P3S14_S15", "B6GS14_S15"),
    measure    = "LNBQRSA",
    start_time = "1987-Q1",
    end_time   = "2020-Q4"
  )
  # Fallback: try without S15 (households only, not NPISH)
  if (is.null(oecd_qna_raw) || nrow(oecd_qna_raw) == 0) {
    message("  OECD: trying fallback subjects P3S14 / B6GS14")
    oecd_qna_raw <- fetch_oecd_qna(
      subjects   = c("P3S14", "B6GS14"),
      measure    = "LNBQRSA",
      start_time = "1987-Q1",
      end_time   = "2020-Q4"
    )
  }
  if (!is.null(oecd_qna_raw) && nrow(oecd_qna_raw) > 0) {
    oecd_qna_raw <- oecd_qna_raw %>%
      mutate(date = parse_oecd_quarter(time)) %>%
      filter(!is.na(date), !is.na(value))
    n_cons <- sum(oecd_qna_raw$subject %in% c("P3S14_S15", "P3S14") &
                  !is.na(oecd_qna_raw$value))
    n_inc  <- sum(oecd_qna_raw$subject %in% c("B6GS14_S15", "B6GS14") &
                  !is.na(oecd_qna_raw$value))
    message(sprintf("  OECD QNA: %d consumption obs, %d income obs", n_cons, n_inc))
    if (n_cons > 0) {
      cons_dates <- oecd_qna_raw %>%
        filter(subject %in% c("P3S14_S15", "P3S14")) %>%
        pull(date)
      message(sprintf("  OECD cons range: %s to %s",
                      min(cons_dates), max(cons_dates)))
    }
    if (n_inc > 0) {
      inc_dates <- oecd_qna_raw %>%
        filter(subject %in% c("B6GS14_S15", "B6GS14")) %>%
        pull(date)
      message(sprintf("  OECD income range: %s to %s",
                      min(inc_dates), max(inc_dates)))
    }
  } else {
    message("  OECD QNA: no data retrieved")
    oecd_qna_raw <- NULL
  }
}, error = function(e) {
  message("  OECD QNA download failed: ", e$message)
  oecd_qna_raw <<- NULL
})

# =============================================================================
# SECTION 3: Variable construction — assemble master dataset
# =============================================================================

message("\n--- Section 3: Assembling master dataset ---\n")

# Start from date spine and left-join all series
master <- date_spine

if (!is.null(raw_cons_real) && nrow(raw_cons_real) > 0)
  master <- master %>% left_join(raw_cons_real, by = "date")

if (!is.null(raw_cons_nom) && nrow(raw_cons_nom) > 0)
  master <- master %>% left_join(raw_cons_nom, by = "date")

if (!is.null(raw_ydi) && nrow(raw_ydi) > 0)
  master <- master %>% left_join(raw_ydi, by = "date")

if (!is.null(raw_gdp) && nrow(raw_gdp) > 0)
  master <- master %>% left_join(raw_gdp, by = "date")

if (!is.null(raw_pop) && nrow(raw_pop) > 0)
  master <- master %>% left_join(raw_pop, by = "date")

if (!is.null(raw_unemp) && nrow(raw_unemp) > 0)
  master <- master %>% left_join(raw_unemp, by = "date")

if (!is.null(raw_hicp) && nrow(raw_hicp) > 0)
  master <- master %>% left_join(raw_hicp, by = "date")

if (!is.null(raw_mir) && nrow(raw_mir) > 0)
  master <- master %>% left_join(raw_mir, by = "date")

if (!is.null(hpi_eurostat) && nrow(hpi_eurostat) > 0)
  master <- master %>% left_join(hpi_eurostat, by = "date")

# Extend HPI backwards using BIS data (both series are base 2015=100)
if (exists("hpi_bis") && !is.null(hpi_bis) && nrow(hpi_bis) > 0) {
  master <- master %>%
    left_join(hpi_bis, by = "date") %>%
    mutate(hpi = ifelse(!is.na(hpi), hpi, hpi_bis)) %>%
    select(-hpi_bis)
  message(sprintf("  HPI after BIS extension: %d non-NA obs", sum(!is.na(master$hpi))))
}

# Join financial balance sheet series
join_fin <- function(master, fin_df, col_name) {
  if (!is.null(fin_df) && nrow(fin_df) > 0) {
    fin_df <- fin_df %>% rename(!!col_name := value)
    master <- master %>% left_join(fin_df, by = "date")
  }
  master
}

master <- master %>%
  join_fin(fin_deposits, "fin_deposits") %>%
  join_fin(fin_debtsec,  "fin_debtsec")  %>%
  join_fin(fin_loans,    "fin_loans")    %>%
  join_fin(fin_equity,   "fin_equity")   %>%
  join_fin(fin_mutfund,  "fin_mutfund")  %>%
  join_fin(fin_inspen,   "fin_inspen")   %>%
  join_fin(fin_total_asset, "fin_total_asset") %>%
  join_fin(fin_total_liab,  "fin_total_liab")

# ---------------------------------------------------------------------------
# 3.1 Consumption deflator
# ---------------------------------------------------------------------------
# Primary: ratio of nominal to real consumption (normalised, 2010 avg = 100)
if (all(c("cons_nom", "cons_real") %in% names(master))) {
  master <- master %>%
    mutate(cons_deflator_raw = ifelse(
      !is.na(cons_nom) & !is.na(cons_real) & cons_real != 0,
      (cons_nom / cons_real) * 100,
      NA_real_
    ))
  # Normalise so 2010 average = 100
  avg_2010 <- master %>%
    filter(year(date) == 2010) %>%
    pull(cons_deflator_raw) %>%
    mean(na.rm = TRUE)
  master <- master %>%
    mutate(cons_deflator = cons_deflator_raw / avg_2010 * 100) %>%
    select(-cons_deflator_raw)
  message("  Consumption deflator constructed (normalised 2010=100, mean=",
          round(avg_2010, 2), ")")
} else if ("hicp" %in% names(master)) {
  # Fallback: use HICP as deflator
  avg_2010_hicp <- master %>%
    filter(year(date) == 2010) %>%
    pull(hicp) %>%
    mean(na.rm = TRUE)
  master <- master %>%
    mutate(cons_deflator = hicp / avg_2010_hicp * 100)
  message("  Consumption deflator: HICP fallback (normalised 2010=100)")
}

# ---------------------------------------------------------------------------
# 3.2 Nominal disposable income → real
# ---------------------------------------------------------------------------
if (all(c("ydi_nom", "cons_deflator") %in% names(master))) {
  master <- master %>%
    mutate(ydi_real = ydi_nom / (cons_deflator / 100))
}

# ---------------------------------------------------------------------------
# 3.3 Per-capita variables (divide by pop_millions → units are EUR per head × 1M)
# ---------------------------------------------------------------------------
if ("pop_millions" %in% names(master)) {
  if ("cons_real" %in% names(master))
    master <- master %>% mutate(cons_real_pc = cons_real / pop_millions)
  if ("ydi_real" %in% names(master))
    master <- master %>% mutate(ydi_real_pc = ydi_real / pop_millions)
}

# ---------------------------------------------------------------------------
# 3.2b Annual household income back-extension (pre-1999 quarterly approximation)
# ---------------------------------------------------------------------------
# Annual Eurostat data: nasa_10_nf_tr goes back to 1949 for Italy;
# nama_10_gdp (nominal + real) goes back to 1975 → GDP deflator from 1975.
# Strategy: deflate annual income by GDP deflator, divide by annual pop,
# spline-interpolate to quarterly, scale to match quarterly series overlap.
if ("ydi_real_pc" %in% names(master)) {
  tryCatch({
    # Fetch annual income
    ann_ydi_raw <- cache_or_fetch("ann_ydi_nom", function() {
      fetch_eurostat("nasa_10_nf_tr",
                     list(na_item = "B6G", sector = "S14_S15",
                          unit = "CP_MEUR", geo = "IT"))
    }, raw_dir)

    # Fetch annual nominal and real GDP for deflator
    ann_gdp_n_raw <- cache_or_fetch("ann_gdp_nom", function() {
      fetch_eurostat("nama_10_gdp",
                     list(na_item = "B1GQ", unit = "CP_MEUR", geo = "IT"))
    }, raw_dir)

    ann_gdp_r_raw <- cache_or_fetch("ann_gdp_real", function() {
      fetch_eurostat("nama_10_gdp",
                     list(na_item = "B1GQ", unit = "CLV10_MEUR", geo = "IT"))
    }, raw_dir)

    parse_ann_series <- function(df, valcol) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      out <- df %>%
        mutate(date = parse_eurostat_time(time)) %>%
        filter(!is.na(date), !is.na(value)) %>%
        # Annual data: keep only rows with January dates (whole years)
        filter(month(date) == 1) %>%
        arrange(date) %>%
        select(date, !!valcol := value)
      if (nrow(out) == 0) return(NULL)
      out
    }

    a_ydi  <- parse_ann_series(ann_ydi_raw,   "a_ydi_nom")
    a_gn   <- parse_ann_series(ann_gdp_n_raw, "a_gdp_nom")
    a_gr   <- parse_ann_series(ann_gdp_r_raw, "a_gdp_real")

    if (!is.null(a_ydi) && !is.null(a_gn) && !is.null(a_gr)) {
      # Annual population: mean of quarterly pop for each year
      pop_ann <- master %>%
        filter(!is.na(pop_millions)) %>%
        mutate(year_start = as.Date(paste0(year(date), "-01-01"))) %>%
        group_by(date = year_start) %>%
        summarise(pop_ann = mean(pop_millions, na.rm = TRUE), .groups = "drop")

      ann_merged <- a_ydi %>%
        inner_join(a_gn,   by = "date") %>%
        inner_join(a_gr,   by = "date") %>%
        inner_join(pop_ann, by = "date") %>%
        mutate(
          gdp_defl_ratio = a_gdp_nom / a_gdp_real,   # GDP deflator (ratio)
          ydi_real_ann   = a_ydi_nom / gdp_defl_ratio / pop_ann
        ) %>%
        filter(!is.na(ydi_real_ann), year(date) >= 1975, year(date) <= 2019)

      if (nrow(ann_merged) >= 5) {
        message(sprintf("  Annual income: %d obs, %s to %s",
                        nrow(ann_merged), min(ann_merged$date), max(ann_merged$date)))

        # Spline interpolate from annual (mid-year anchors) to quarterly dates
        # Use July 1 of each year as midpoint anchor for the annual value
        ann_years  <- year(ann_merged$date)
        ann_vals   <- ann_merged$ydi_real_ann
        # Mid-year (July 1) as numeric date anchor
        ann_x <- as.numeric(as.Date(paste0(ann_years, "-07-01")))

        # Quarterly dates within annual range
        q_min <- as.Date(paste0(min(ann_years), "-01-01"))
        q_max <- as.Date(paste0(max(ann_years), "-10-01"))
        q_all <- master$date[master$date >= q_min & master$date <= q_max]

        spl <- spline(ann_x, ann_vals, xout = as.numeric(q_all), method = "natural")
        q_annual_ext <- tibble(date = q_all, ydi_real_ann_q = pmax(spl$y, 0.01))

        # Scale to match quarterly series in 1999Q1–2010Q4 overlap
        overlap <- master %>%
          select(date, ydi_real_pc) %>%
          filter(!is.na(ydi_real_pc),
                 date >= as.Date("1999-01-01"), date <= as.Date("2010-10-01")) %>%
          inner_join(q_annual_ext, by = "date")

        if (nrow(overlap) >= 4) {
          scale_fac <- mean(overlap$ydi_real_pc, na.rm = TRUE) /
                       mean(overlap$ydi_real_ann_q, na.rm = TRUE)
          q_annual_ext <- q_annual_ext %>%
            mutate(ydi_real_ann_q = ydi_real_ann_q * scale_fac)

          # Fill NA ydi_real_pc values (pre-1999) from back-extended series
          master <- master %>%
            left_join(q_annual_ext, by = "date") %>%
            mutate(
              ydi_real_pc = ifelse(!is.na(ydi_real_pc), ydi_real_pc, ydi_real_ann_q)
            ) %>%
            select(-ydi_real_ann_q)

          n_ext <- sum(!is.na(master$ydi_real_pc[master$date < as.Date("1999-01-01")]))
          message(sprintf("  Annual income back-extension: %d pre-1999 obs added (scale=%.3f)",
                          n_ext, scale_fac))
        }
      }
    }
  }, error = function(e) {
    message("  Annual income back-extension failed: ", e$message)
  })
}

# ---------------------------------------------------------------------------
# 3.2c OECD quarterly back-extension for consumption and income
# ---------------------------------------------------------------------------
# Uses OECD QNA real chain-linked series to push coverage further back
# than the Eurostat quarterly series. Scaled to match in overlap period.
# Consumption: fills cons_real_pc for quarters before 1996Q1
# Income:      fills ydi_real_pc for quarters before earliest available
# ---------------------------------------------------------------------------
if (!is.null(oecd_qna_raw) && nrow(oecd_qna_raw) > 0 &&
    "pop_millions" %in% names(master)) {

  # Identify which subject code was returned (with or without S15)
  cons_subj <- if (any(oecd_qna_raw$subject == "P3S14_S15")) "P3S14_S15" else "P3S14"
  inc_subj  <- if (any(oecd_qna_raw$subject == "B6GS14_S15")) "B6GS14_S15" else "B6GS14"

  # ---- Consumption back-extension ----
  if ("cons_real_pc" %in% names(master)) {
    oecd_cons <- oecd_qna_raw %>%
      filter(subject == cons_subj) %>%
      select(date, oecd_cons_val = value) %>%
      filter(!is.na(oecd_cons_val))

    if (nrow(oecd_cons) > 0) {
      # Scale OECD to match Eurostat per-capita in 1996Q1-2010Q4 overlap
      overlap_c <- master %>%
        select(date, cons_real_pc) %>%
        filter(!is.na(cons_real_pc),
               date >= as.Date("1996-01-01"), date <= as.Date("2010-10-01")) %>%
        inner_join(oecd_cons, by = "date") %>%
        filter(!is.na(oecd_cons_val))

      if (nrow(overlap_c) >= 8) {
        scale_c <- mean(overlap_c$cons_real_pc / overlap_c$oecd_cons_val, na.rm = TRUE)
        oecd_cons_scaled <- oecd_cons %>%
          mutate(cons_oecd_pc = oecd_cons_val * scale_c) %>%
          select(date, cons_oecd_pc)

        master <- master %>%
          left_join(oecd_cons_scaled, by = "date") %>%
          mutate(cons_real_pc = ifelse(!is.na(cons_real_pc), cons_real_pc, cons_oecd_pc)) %>%
          select(-cons_oecd_pc)

        n_c_added <- sum(!is.na(master$cons_real_pc[
          master$date < as.Date("1996-01-01")]))
        message(sprintf(
          "  OECD cons back-extension: %d pre-1996 obs added (scale=%.4f)",
          n_c_added, scale_c))
      } else {
        message(sprintf("  OECD cons: only %d overlap rows — skipping", nrow(overlap_c)))
      }
    }
  }

  # ---- Income back-extension ----
  if ("ydi_real_pc" %in% names(master)) {
    oecd_inc <- oecd_qna_raw %>%
      filter(subject == inc_subj) %>%
      select(date, oecd_inc_val = value) %>%
      filter(!is.na(oecd_inc_val))

    if (nrow(oecd_inc) > 0) {
      # Use existing ydi_real_pc to calibrate scale
      first_q_date <- min(master$date[!is.na(master$ydi_real_pc)], na.rm = TRUE)
      overlap_y <- master %>%
        select(date, ydi_real_pc) %>%
        filter(!is.na(ydi_real_pc),
               date <= as.Date("2010-10-01")) %>%
        inner_join(oecd_inc, by = "date") %>%
        filter(!is.na(oecd_inc_val))

      if (nrow(overlap_y) >= 8) {
        scale_y <- mean(overlap_y$ydi_real_pc / overlap_y$oecd_inc_val, na.rm = TRUE)
        oecd_inc_scaled <- oecd_inc %>%
          mutate(ydi_oecd_pc = oecd_inc_val * scale_y) %>%
          select(date, ydi_oecd_pc)

        master <- master %>%
          left_join(oecd_inc_scaled, by = "date") %>%
          mutate(ydi_real_pc = ifelse(!is.na(ydi_real_pc), ydi_real_pc, ydi_oecd_pc)) %>%
          select(-ydi_oecd_pc)

        n_y_added <- sum(!is.na(master$ydi_real_pc[master$date < first_q_date]))
        message(sprintf(
          "  OECD income back-extension: %d pre-%s obs added (scale=%.4f)",
          n_y_added, format(first_q_date, "%Y-%m"), scale_y))
      } else {
        message(sprintf("  OECD income: only %d overlap rows — skipping", nrow(overlap_y)))
      }
    }
  }
}

# ---------------------------------------------------------------------------
# 3.4 Deflate financial stocks and construct per-capita real values
# ---------------------------------------------------------------------------
fin_cols <- c("fin_deposits", "fin_debtsec", "fin_loans", "fin_equity",
              "fin_mutfund", "fin_inspen", "fin_total_asset", "fin_total_liab")

if ("cons_deflator" %in% names(master)) {
  for (col in fin_cols) {
    if (col %in% names(master)) {
      real_col <- paste0(col, "_real")
      master[[real_col]] <- master[[col]] / (master$cons_deflator / 100)
      if ("pop_millions" %in% names(master)) {
        pc_col <- paste0(col, "_real_pc")
        master[[pc_col]] <- master[[real_col]] / master$pop_millions
      }
    }
  }
}

# Net financial wealth (assets minus liabilities, real per capita)
if (all(c("fin_total_asset_real", "fin_total_liab_real") %in% names(master))) {
  master <- master %>%
    mutate(
      fin_net_real    = fin_total_asset_real - fin_total_liab_real,
      fin_net_real_pc = fin_net_real / pop_millions
    )
}

# =============================================================================
# SECTION 4: Pre-1999 back-extrapolation for financial balance sheet series
# =============================================================================
# NOTE: This is a rough placeholder based on early observed growth rates.
#       For a proper replication, replace with Bonci & Coletta (2008) data
#       which provides Italian household balance sheet series back to 1980.
# =============================================================================

message("\n--- Section 4: Back-extrapolating financial series pre-1999 ---\n")

back_extrapolate <- function(series, dates, n_growth_quarters = 20) {
  # Use the first n_growth_quarters available non-NA observations to estimate
  # the average quarterly growth rate, then extrapolate backwards.
  first_obs_idx <- which(!is.na(series))[1]
  if (is.null(first_obs_idx) || is.na(first_obs_idx)) return(series)

  # Indices before first available observation
  pre_idx <- seq_len(first_obs_idx - 1L)
  if (length(pre_idx) == 0) return(series)

  # Growth rate from first n_growth_quarters
  growth_window <- series[first_obs_idx:(first_obs_idx + n_growth_quarters - 1L)]
  growth_window <- growth_window[!is.na(growth_window)]
  if (length(growth_window) < 2) return(series)

  avg_q_growth <- mean(diff(log(growth_window[growth_window > 0])), na.rm = TRUE)

  # Back-extrapolate: series[t-1] = series[t] / exp(avg_q_growth)
  result <- series
  for (i in rev(pre_idx)) {
    result[i] <- result[i + 1L] / exp(avg_q_growth)
  }
  result
}

# Apply back-extrapolation to real per-capita financial series
fin_real_pc_cols <- paste0(fin_cols[fin_cols %in% names(master)], "_real_pc")
fin_real_pc_cols <- fin_real_pc_cols[fin_real_pc_cols %in% names(master)]

for (col in fin_real_pc_cols) {
  n_before <- sum(!is.na(master[[col]]))
  master[[col]] <- back_extrapolate(master[[col]], master$date)
  n_after <- sum(!is.na(master[[col]]))
  message(sprintf("  %s: %d → %d non-NA obs after back-extrapolation", col, n_before, n_after))
}

# Also back-extrapolate net financial wealth if available
for (col in c("fin_net_real_pc", "fin_net_real")) {
  if (col %in% names(master)) {
    master[[col]] <- back_extrapolate(master[[col]], master$date)
  }
}

# =============================================================================
# SECTION 5: Log transforms and saving
# =============================================================================

message("\n--- Section 5: Log transforms and saving ---\n")

if ("cons_real_pc" %in% names(master))
  master <- master %>% mutate(ln_cons_real_pc = log(cons_real_pc))

if ("ydi_real_pc" %in% names(master))
  master <- master %>% mutate(ln_ydi_real_pc = log(ydi_real_pc))

if ("ln_cons_real_pc" %in% names(master))
  master <- master %>% mutate(d_ln_cons_pc = c(NA_real_, diff(ln_cons_real_pc)))

if ("ln_ydi_real_pc" %in% names(master))
  master <- master %>% mutate(d_ln_ydi_pc = c(NA_real_, diff(ln_ydi_real_pc)))

if ("hpi" %in% names(master))
  master <- master %>% mutate(ln_hpi = log(hpi))

if ("fin_loans" %in% names(master))
  master <- master %>% mutate(ln_fin_loans = log(fin_loans))

# Credit conditions index (CCI) proxy:
# log(fin_loans / 8-quarter centred MA of nominal GDP)
if (all(c("fin_loans", "gdp_nom") %in% names(master))) {
  master <- master %>%
    mutate(
      gdp_nom_8qma = as.numeric(rollmean(gdp_nom, k = 8, fill = NA, align = "center")),
      cci_ratio    = log(fin_loans / gdp_nom_8qma)
    )
  message("  CCI ratio (log credit/8qMA GDP) constructed")
}

# Save to outputs
csv_path <- file.path(output_dir, "italy_model_dataset.csv")
rds_path <- file.path(output_dir, "italy_model_dataset.rds")

write.csv(master, csv_path, row.names = FALSE)
saveRDS(master,   rds_path)

message("  Saved: ", csv_path)
message("  Saved: ", rds_path)

# =============================================================================
# Data coverage table
# =============================================================================

message("\n--- Data coverage summary ---\n")

coverage <- master %>%
  summarise(across(
    where(is.numeric),
    list(
      n_obs     = ~sum(!is.na(.)),
      date_from = ~{
        idx <- which(!is.na(.))
        if (length(idx) == 0) NA_character_ else as.character(min(master$date[idx]))
      },
      date_to   = ~{
        idx <- which(!is.na(.))
        if (length(idx) == 0) NA_character_ else as.character(max(master$date[idx]))
      }
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  tidyr::pivot_longer(
    everything(),
    names_to  = c("variable", ".value"),
    names_sep = "__"
  )

print(coverage, n = Inf)

message("\nPart 1 complete.")
