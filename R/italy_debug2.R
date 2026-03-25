#!/usr/bin/env Rscript
# Debug round 2: test corrected API calls

suppressPackageStartupMessages({ library(httr); library(jsonlite); library(dplyr) })

quick_get <- function(url) {
  cat("GET", url, "\n")
  r <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
  if (is.null(r)) { cat("  -> request error\n"); return(NULL) }
  cat("  -> status", status_code(r), "\n")
  if (status_code(r) == 200) {
    txt <- rawToChar(content(r, "raw"))
    cat("  -> response size:", nchar(txt), "chars\n")
    cat("  -> first 300 chars:", substr(txt, 1, 300), "\n")
  }
  r
}

# ---- Fix 1: ECB SDW correct URL format: flow/key not flow.key ----
cat("\n=== ECB SDW FIX: Use flow/key format ===\n")
# Correct: split at first dot → flow=QSA, key=Q.N.IT...
full_key <- "QSA.Q.N.IT.W0.S14_S15.S1.N.A.F2.T._Z.EUR.V.N"
flow <- sub("^([^.]+)\\..*", "\\1", full_key)
key  <- sub("^[^.]+\\.", "", full_key)
ecb_url <- paste0("https://data-api.ecb.europa.eu/service/data/", flow, "/", key, "?format=csvdata")
r_ecb <- quick_get(ecb_url)

# ---- Fix 2: Eurostat consumption — correct table name ----
cat("\n=== Eurostat consumption alternatives ===\n")
# Try nasq_10_nf_tr (quarterly sector non-financial accounts)
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=P3&sector=S14_S15&unit=CLV10_MEUR")

# Try namq_10_gdp with P31_S14_S15 (household consumption item)
cat("\n--- Try namq_10_gdp P31_S14_S15 ---\n")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON&lang=en&geo=IT&na_item=P31_S14_S15&unit=CLV10_MEUR")

# ---- Fix 3: Eurostat unemployment without age filter ----
cat("\n=== Unemployment without age filter ===\n")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/une_rt_q?format=JSON&lang=en&geo=IT&sex=T&s_adj=SA&unit=PC_ACT")

# ---- Fix 4: Eurostat income (B6G in namq_10_gdp) ----
cat("\n=== Income: namq_10_gdp B6G S14_S15 ===\n")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CLV10_MEUR")

# ---- Fix 5: HICP ----
cat("\n=== HICP ===\n")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hicp_midx?format=JSON&lang=en&geo=IT&coicop=CP00&unit=I15")
