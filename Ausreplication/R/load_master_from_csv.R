#!/usr/bin/env Rscript
# ==============================================================================
# Load master dataset from a portable wide-format CSV.
# ==============================================================================
#
# Reads data_raw/master_data.csv (produced by export_master_csv.R) and
# reconstructs the master tibble exactly as the data-download script would.
# Then runs the estimation pipeline.
#
# This bypasses ALL ABS / RBA downloads — useful when:
#   - You don't have internet access for the RBA fetch
#   - The ABS workbooks have been updated and you want to freeze the prior
#     vintage's results
#   - You want to edit individual cells of the master dataset by hand
#     (e.g. patch a known data error) and run with the corrected values
#
# Run from the project root:
#   Rscript Ausreplication/R/load_master_from_csv.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(zoo); library(lubridate)
  library(readr); library(tibble); library(stringr)
  library(sandwich); library(lmtest); library(strucchange)
  library(broom); library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

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

csv_path <- file.path(project_root, "data_raw", "master_data.csv")

if (!file.exists(csv_path)) {
  stop("CSV not found at ", csv_path,
       "\nRun Rscript Ausreplication/R/export_master_csv.R first to ",
       "produce it from the cached RDS, or have someone share the CSV.")
}

cat(sprintf("Loading master from %s\n", csv_path))
cat("[note] CSV round-trip is at machine precision (~1e-10 max abs diff per\n")
cat("       column). Substantive results (BICs, coefficient signs, magnitudes)\n")
cat("       are essentially identical to the RDS path. Edge-case Chow-stability\n")
cat("       selector flags can flip on borderline specs because\n")
cat("       strucchange::sctest is bit-sensitive near critical values. For\n")
cat("       bit-identical reproduction use the RDS at outputs/australia_model_dataset.rds.\n\n")
master <- readr::read_csv(csv_path, col_types = cols(
  date = col_date(format = "%Y-%m-%d"),
  .default = col_double()
), progress = FALSE)
master <- as_tibble(master)

# Coerce the dummy columns from numeric to integer (write_csv stores them
# as numeric; estimation code expects integers for the 0/1 dummies).
dummy_cols <- c("d2000_gst", "d2008_gfc", "d2020_covid", "d2020_rebound",
                "d_neg_gearing_8587", "d_recession_1991",
                "d_jobkeeper_2020")
for (cn in intersect(dummy_cols, names(master))) {
  master[[cn]] <- as.integer(master[[cn]])
}

cat(sprintf("Loaded: %d rows x %d cols (%s -> %s)\n",
            nrow(master), ncol(master),
            format(min(master$date), "%Y-%m-%d"),
            format(max(master$date), "%Y-%m-%d")))

# Build model_data with the same renaming the orchestrator does.
model_data <- master %>%
  rename(
    dlcons   = d_ln_cons_pc,
    lincome  = ln_ydi_real_pc,
    lcons    = ln_cons_real_pc
  ) %>%
  mutate(
    ecm_lag = lag(lcons, 1L) - lincome
  )

complete_core <- sum(complete.cases(model_data %>%
  select(dlcons, ecm_lag, real_rate, lincome)))
message(sprintf("  model_data rows: %d   complete for core vars: %d",
                nrow(model_data), complete_core))

cat(rep("=", 70), "\n", sep = "")
cat("Running estimation framework from CSV-loaded master\n")
cat(rep("=", 70), "\n", sep = "")

source(file.path(script_dir, "australia_estimation.R"), local = TRUE)

cat(rep("=", 70), "\n", sep = "")
cat("Done.\n")
cat(rep("=", 70), "\n", sep = "")
