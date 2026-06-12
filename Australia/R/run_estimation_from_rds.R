#!/usr/bin/env Rscript
# Quick re-estimation using saved RDS (skips data download)
# Run from project root: Rscript Australia/R/run_estimation_from_rds.R

suppressPackageStartupMessages({
  library(readxl); library(readabs); library(dplyr); library(tidyr)
  library(stringr); library(lubridate); library(zoo); library(httr)
  library(jsonlite); library(sandwich); library(lmtest)
  library(strucchange); library(tibble); library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Resolve script location for output_dir
.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    else normalizePath("Australia/R/run_estimation_from_rds.R",
                       winslash = "/", mustWork = FALSE)
  }
)
script_dir   <- dirname(.this_file)
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/")

cat(rep("=", 70), "\n", sep = "")
cat("Loading saved model dataset (skipping data download)\n")
cat(rep("=", 70), "\n", sep = "")

rds_path <- file.path(project_root, "outputs", "australia_model_dataset.rds")
if (!file.exists(rds_path)) stop("RDS not found: ", rds_path)
master <- readRDS(rds_path)
cat(sprintf("Loaded: %d rows, %d cols\n", nrow(master), ncol(master)))

# Source helpers and (re-)attach the maximal (15-knot) SDMMA CCI basis if not already present
source(file.path(script_dir, "model_helpers.R"), local = TRUE)
USE_WILLIAMS_SDMMA_BASIS <- TRUE
if (USE_WILLIAMS_SDMMA_BASIS &&
    !any(grepl("^sdmma_", names(master)))) {
  williams_basis <- build_williams_cci_basis(master$date)
  for (j in seq_len(ncol(williams_basis))) {
    master[[colnames(williams_basis)[j]]] <- williams_basis[, j]
  }
  cat("Attached maximal (15-knot) SDMMA CCI basis to master (",
      paste(colnames(williams_basis), collapse = ", "), ")\n", sep = "")
}

# Build model_data exactly as australia_consumption_model.R does
model_data <- master %>%
  rename(
    dlcons  = d_ln_cons_pc,
    lincome = ln_ydi_real_pc,
    lcons   = ln_cons_real_pc
  ) %>%
  mutate(
    # Canonical Engle-Granger ECM lag: λ < 0 = restoring force.
    ecm_lag = lag(lcons, 1L) - lincome
  )

complete_core <- sum(complete.cases(model_data %>%
  select(dlcons, ecm_lag, real_rate, lincome)))
message(sprintf("  model_data rows: %d   complete for core vars: %d",
                nrow(model_data), complete_core))

cat(rep("=", 70), "\n", sep = "")
cat("Running estimation framework\n")
cat(rep("=", 70), "\n", sep = "")

source(file.path(script_dir, "australia_estimation.R"), local = TRUE)

cat(rep("=", 70), "\n", sep = "")
cat("Done.\n")
cat(rep("=", 70), "\n", sep = "")
