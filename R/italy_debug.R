#!/usr/bin/env Rscript
# Quick diagnostic: test fetch functions and print first few rows

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tibble)
})

# ---- Inline the fixed fetch_eurostat ----

parse_eurostat_time <- function(x) {
  x <- as.character(x)
  m1 <- regmatches(x, regexpr("^(\\d{4})-Q([1-4])$", x, perl = TRUE))
  m2 <- regmatches(x, regexpr("^(\\d{4})Q([1-4])$",  x, perl = TRUE))
  combined <- ifelse(nchar(m1) > 0, m1, ifelse(nchar(m2) > 0, m2, NA_character_))
  as.Date(ifelse(
    !is.na(combined),
    paste0(substr(combined, 1, 4), "-",
           sprintf("%02d", (as.integer(substr(combined, nchar(combined), nchar(combined))) - 1L) * 3L + 1L),
           "-01"),
    NA_character_
  ))
}

fetch_eurostat <- function(dataset_id, filters = list(), timeout_sec = 120) {
  filter_str <- paste(
    mapply(function(key, vals) paste0(key, "=", paste(vals, collapse = "+")),
           names(filters), filters),
    collapse = "&"
  )
  url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
                dataset_id, "?format=JSON&lang=en")
  if (nchar(filter_str) > 0) url <- paste0(url, "&", filter_str)

  cat("Fetching:", url, "\n")

  resp <- tryCatch(
    GET(url, timeout(timeout_sec), add_headers(Accept = "application/json")),
    error = function(e) { cat("Request error:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(resp)) return(NULL)
  cat("Status code:", status_code(resp), "\n")
  if (status_code(resp) != 200) return(NULL)

  body <- tryCatch(
    fromJSON(rawToChar(content(resp, "raw")), simplifyVector = FALSE),
    error = function(e) { cat("JSON parse error:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(body)) return(NULL)

  cat("Dimensions:", paste(body$id, collapse = ", "), "\n")
  cat("Sizes:", paste(unlist(body$size), collapse = ", "), "\n")
  cat("N values:", length(body$value), "\n")

  dims      <- body$id
  dim_sizes <- unlist(body$size)
  n_dims    <- length(dim_sizes)

  dim_labels <- lapply(dims, function(d) {
    idx <- body$dimension[[d]]$category$index
    lbl <- names(idx)
    pos <- unlist(idx)
    v   <- character(length(lbl))
    v[pos + 1L] <- lbl
    v
  })
  names(dim_labels) <- dims

  val_pos    <- as.integer(names(body$value))
  val_values <- unlist(body$value)

  if (length(val_pos) == 0) { cat("No values in response.\n"); return(NULL) }

  # Vectorised decoding
  strides <- vapply(seq_len(n_dims), function(d) {
    if (d < n_dims) prod(dim_sizes[(d + 1L):n_dims]) else 1L
  }, numeric(1L))

  df <- data.frame(value = val_values, stringsAsFactors = FALSE)
  for (d in seq_len(n_dims)) {
    idx_0based  <- (val_pos %/% strides[d]) %% dim_sizes[d]
    df[[dims[d]]] <- dim_labels[[d]][idx_0based + 1L]
  }
  df <- as_tibble(df)

  cat("Parsed columns:", paste(names(df), collapse = ", "), "\n")
  cat("First time values:", paste(head(df$time, 5), collapse = ", "), "\n")

  if ("time" %in% names(df)) {
    df <- df %>%
      mutate(date = parse_eurostat_time(time)) %>%
      filter(!is.na(date))
  }

  cat("Rows after date parse:", nrow(df), "\n")
  df
}

# ---- Test 1: Consumption ----
cat("\n=== TEST 1: Consumption (namq_10_co3_p3) ===\n")
r1 <- fetch_eurostat("namq_10_co3_p3",
                     filters = list(geo = "IT", na_item = "P3",
                                    sector = "S14_S15", unit = "CLV10_MEUR"))
if (!is.null(r1) && nrow(r1) > 0) {
  cat("SUCCESS:", nrow(r1), "rows\n")
  print(head(r1))
} else {
  cat("FAILED or 0 rows — trying without sector filter\n")
  r1b <- fetch_eurostat("namq_10_co3_p3",
                        filters = list(geo = "IT", na_item = "P3", unit = "CLV10_MEUR"))
  if (!is.null(r1b)) cat("Without sector:", nrow(r1b), "rows\n")
}

# ---- Test 2: Unemployment ----
cat("\n=== TEST 2: Unemployment (une_rt_q) ===\n")
r2 <- fetch_eurostat("une_rt_q",
                     filters = list(geo = "IT", sex = "T", age = "TOTAL",
                                    s_adj = "SA", unit = "PC_ACT"))
if (!is.null(r2) && nrow(r2) > 0) {
  cat("SUCCESS:", nrow(r2), "rows\n")
  print(head(r2 %>% select(date, value)))
} else {
  cat("FAILED\n")
}

# ---- Test 3: ECB QSA deposits ----
cat("\n=== TEST 3: ECB QSA deposits ===\n")
url3 <- "https://data-api.ecb.europa.eu/service/data/QSA.Q.N.IT.W0.S14_S15.S1.N.A.F2.T._Z.EUR.V.N?format=csvdata"
cat("Fetching:", url3, "\n")
resp3 <- tryCatch(GET(url3, timeout(60)), error = function(e) NULL)
if (!is.null(resp3)) {
  cat("Status:", status_code(resp3), "\n")
  if (status_code(resp3) == 200) {
    txt <- rawToChar(content(resp3, "raw"))
    cat("First 500 chars:\n", substr(txt, 1, 500), "\n")
  }
}
