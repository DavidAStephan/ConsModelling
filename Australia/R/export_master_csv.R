#!/usr/bin/env Rscript
# ==============================================================================
# Export master dataset as a portable wide-format CSV.
# ==============================================================================
#
# Reads outputs/australia_model_dataset.rds (or runs the full data download
# pipeline if no RDS is present), applies the same variable backfills that
# the estimation script normally reconstructs at runtime, then writes a
# human-readable wide-format CSV to data_raw/master_data.csv.
#
# This CSV can be edited by hand, version-controlled, and used as the single
# source of truth for re-runs that should bypass ABS/RBA downloads. Pair
# with R/load_master_from_csv.R to round-trip.
#
# Run from the project root:
#   Rscript Australia/R/export_master_csv.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(zoo); library(lubridate)
  library(readr); library(tibble); library(stringr)
})

.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath(".", winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")

rds_path <- file.path(project_root, "outputs", "australia_model_dataset.rds")
csv_path <- file.path(project_root, "data_raw", "master_data.csv")

if (!file.exists(rds_path)) {
  stop("RDS not found at ", rds_path,
       "\nRun Rscript Australia/R/australia_consumption_model.R first ",
       "to download data and build the RDS, OR provide the RDS by some other ",
       "means (e.g. download from a colleague).")
}

master <- readRDS(rds_path)
cat(sprintf("Loaded RDS: %d rows x %d cols (%s -> %s)\n",
            nrow(master), ncol(master),
            format(min(master$date), "%Y-%m-%d"),
            format(max(master$date), "%Y-%m-%d")))

# ------------------------------------------------------------------------------
# Backfill variables that estimation.R::add_model_variables() reconstructs at
# runtime. This way the exported CSV is self-contained and the loader doesn't
# need to know about reconstruction logic.
# ------------------------------------------------------------------------------

ogive <- function(x) 1 / (1 + exp(-x))

if (!"nla_y_unrestricted" %in% names(master) &&
    all(c("fin_deposits", "ydi_ann_nom") %in% names(master))) {
  master$nla_y_unrestricted <- master$fin_deposits / master$ydi_ann_nom
  cat("  added nla_y_unrestricted (deposits / annualised income)\n")
}

if (!"d_neg_gearing_8587" %in% names(master)) {
  master$d_neg_gearing_8587 <- as.integer(master$date >= as.Date("1985-07-01") &
                                          master$date <= as.Date("1987-07-01"))
  cat("  added d_neg_gearing_8587 (1985Q3 to 1987Q3)\n")
}
if (!"d_recession_1991" %in% names(master)) {
  master$d_recession_1991 <- as.integer(master$date == as.Date("1991-04-01"))
  cat("  added d_recession_1991 (1991Q2)\n")
}
if (!"d_apra_2014" %in% names(master)) {
  master$d_apra_2014 <- ogive(
    as.numeric(master$date - as.Date("2014-10-01")) / 91.31 / 2.5)
  cat("  added d_apra_2014 (smooth transition centred 2014Q4)\n")
}
if (!"d_apra_2017" %in% names(master)) {
  master$d_apra_2017 <- ogive(
    as.numeric(master$date - as.Date("2017-04-01")) / 91.31 / 2.5)
  cat("  added d_apra_2017 (smooth transition centred 2017Q2)\n")
}
if (!"d_jobkeeper_2020" %in% names(master)) {
  master$d_jobkeeper_2020 <- as.integer(master$date >= as.Date("2020-04-01") &
                                        master$date <= as.Date("2021-01-01"))
  cat("  added d_jobkeeper_2020 (2020Q2 to 2021Q1)\n")
}

# Reorder columns: date first, then groups by topic for readability.
preferred_order <- c(
  "date",
  # Aggregate consumption / income
  "cons_real", "cons_nom", "cons_deflator", "cons_deflator_norm",
  "ydi_nom", "ydi_ann_nom", "ydi_ann_r", "ydi_ann_8qma",
  # Per-capita
  "pop_millions", "prime_age_share", "cons_real_pc", "ydi_real_pc",
  "ln_cons_real_pc", "ln_ydi_real_pc", "d_ln_cons_pc", "d_ln_ydi_pc",
  # Labour
  "unemp_rate",
  # Balance sheet (raw and deflated)
  "fin_deposits", "fin_equities", "fin_super", "fin_loans", "housing_wealth",
  "closing_net_worth",
  "fin_deposits_r", "fin_equities_r", "fin_super_r", "fin_loans_r",
  "housing_wealth_r",
  # Wealth/income ratios
  "eq_y", "super_y", "ilfa_y", "nla_y", "nla_y_unrestricted", "ha_y",
  "debt_y", "networth_y", "ln_networth_y",
  # House prices
  "hpi", "ln_hpi", "ln_hp_over_y",
  # Interest rates
  "mortgage_rate", "hicp_4q_ann", "real_rate", "mortgage_burden",
  # Credit
  "housing_loan_flow", "fhb_loans", "non_fhb_loans", "cci_ratio", "fhb_share",
  # ECM target
  "ln_y_over_c",
  # Dummies
  "d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
  "d_neg_gearing_8587", "d_recession_1991",
  "d_apra_2014", "d_apra_2017", "d_jobkeeper_2020"
)
present <- intersect(preferred_order, names(master))
extras  <- setdiff(names(master), preferred_order)
master <- master[, c(present, extras)]
if (length(extras) > 0L) {
  cat(sprintf("Extra columns appended at end: %s\n",
              paste(extras, collapse = ", ")))
}

cat(sprintf("Writing %d rows x %d cols to %s\n",
            nrow(master), ncol(master), csv_path))
# Full double precision (17 significant digits guarantees binary round-trip).
# readr::write_csv truncates to ~15 digits and loses bits that downstream
# numerical tests (e.g. strucchange Chow at boundary cases) are sensitive
# to. Use base R write.table with %.17g formatting.
write_csv_high_precision <- function(df, path) {
  fmt_col <- function(x) {
    if (inherits(x, "Date")) format(x, "%Y-%m-%d")
    else if (is.integer(x)) as.character(x)
    else if (is.numeric(x)) ifelse(is.na(x), "", formatC(x, digits = 17, format = "g"))
    else as.character(x)
  }
  out <- as.data.frame(lapply(df, fmt_col), stringsAsFactors = FALSE)
  write.table(out, path, sep = ",", row.names = FALSE, quote = FALSE,
              na = "", fileEncoding = "UTF-8")
}
write_csv_high_precision(master, csv_path)
cat("Done.\n")
cat(sprintf("File size: %.1f KB\n",
            file.size(csv_path) / 1024))
