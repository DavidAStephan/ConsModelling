#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(httr); library(jsonlite); library(dplyr) })

quick_get <- function(url, label="") {
  cat("\n[", label, "]\n", url, "\n", sep="")
  r <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
  if (is.null(r)) { cat("  -> error\n"); return(NULL) }
  cat("  -> status", status_code(r))
  if (status_code(r) == 200) {
    b <- tryCatch(fromJSON(rawToChar(content(r,"raw")), simplifyVector=FALSE), error=function(e) NULL)
    if (!is.null(b)) {
      cat("  N values:", length(b$value), "  dims:", paste(b$id, collapse=","), "  sizes:", paste(unlist(b$size), collapse=","), "\n")
    } else cat("  (non-JSON)\n")
  } else cat("\n")
  r
}

# 1. Income via nasq_10_nf_tr (try CP_MEUR and CP_MNAC)
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CP_MEUR", "Income CP_MEUR")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CP_MNAC", "Income CP_MNAC")

# 2. GDP via namq_10_gdp
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON&lang=en&geo=IT&na_item=B1GQ&unit=CP_MEUR", "GDP CP_MEUR")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/namq_10_gdp?format=JSON&lang=en&geo=IT&na_item=B1GQ&unit=CLV10_MEUR", "GDP CLV10")

# 3. Eurostat quarterly financial accounts (nasq_10_f_bs)
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&finpos=LIAB&na_item=F4&unit=MIO_NAC", "QFA loans")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&finpos=ASSET&na_item=F2&unit=MIO_NAC", "QFA deposits")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&finpos=ASSET&na_item=F5&unit=MIO_NAC", "QFA equity")
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&finpos=ASSET&na_item=F6&unit=MIO_NAC", "QFA insurance")

# 4. Population via demo_pjan (annual, interpolated)
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/demo_pjan?format=JSON&lang=en&geo=IT&sex=T&age=TOTAL", "Population")

# 5. House price index via prc_hpi_q
quick_get("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hpi_q?format=JSON&lang=en&geo=IT&purchase_type=TOTAL", "HPI Eurostat")

# 6. ECB MIR mortgage rate (monthly)
quick_get("https://data-api.ecb.europa.eu/service/data/MIR/M.IT.B.A2C.AM.R.A.2250.EUR.N?format=csvdata", "ECB MIR A2C monthly")
quick_get("https://data-api.ecb.europa.eu/service/data/MIR/M.IT.B.A2.AM.R.A.2250.EUR.N?format=csvdata", "ECB MIR A2 monthly")
