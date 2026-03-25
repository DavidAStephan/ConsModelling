#!/usr/bin/env Rscript
# ==============================================================================
# Australia Household Consumption Model — Master Script
# ==============================================================================
#
# Adaptation of the Muellbauer ECM framework for Australia.
# Reference: Duca, Muellbauer et al. (Australia system paper)
#
# This master script:
#   1. Sources Part 1 (data download & construction → produces `master`)
#   2. Builds `model_data` from `master` with paper-consistent variable names
#   3. Sources Part 2 (estimation framework → produces results, tables, plots)
#
# Run from the project root:
#   Rscript Ausreplication/R/australia_consumption_model.R
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
  library(sandwich)
  library(lmtest)
  library(strucchange)
  library(tibble)
  library(ggplot2)
})

options(stringsAsFactors = FALSE, scipen = 999)

# Resolve script location
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
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/",
                               mustWork = FALSE)

cat(rep("=", 70), "\n", sep = "")
cat("STEP 1: Downloading and constructing Australian data\n")
cat(rep("=", 70), "\n", sep = "")

source(file.path(script_dir, "australia_data_download.R"), local = TRUE)
# After sourcing Part 1, `master` tibble is available in this environment

cat("\n", rep("=", 70), "\n", sep = "")
cat("STEP 2: Building model_data\n")
cat(rep("=", 70), "\n", sep = "")

# Rename columns to match Italy estimation script conventions.
# The estimation script (italy_estimation.R, adapted below) expects:
#   dlcons      = Δ ln(real per capita consumption)
#   ln_y_over_c = ln(y/c_{t-1}) — ECM target
#   real_rate   = real mortgage rate
#   ln_yp_over_y = ln(y^p / y)  — constructed by estimation script
#   ilfa_y, nla_y, ha_y, networth_y = wealth/income ratios
#   unemp_rate  = unemployment rate (%)
#   cci_ratio   = credit conditions proxy

model_data <- master %>%
  rename(
    dlcons   = d_ln_cons_pc,
    lincome  = ln_ydi_real_pc,
    lcons    = ln_cons_real_pc
  ) %>%
  mutate(
    # Ensure ln_y_over_c is present (income relative to lagged consumption)
    ln_y_over_c = lincome - lag(lcons, 1L)
  )

complete_core <- sum(complete.cases(model_data %>%
  select(dlcons, ln_y_over_c, real_rate, lincome)))
message(sprintf("  model_data rows: %d   complete for core vars: %d",
                nrow(model_data), complete_core))

cat("\n", rep("=", 70), "\n", sep = "")
cat("STEP 3: Running estimation framework\n")
cat(rep("=", 70), "\n", sep = "")

source(file.path(script_dir, "australia_estimation.R"), local = TRUE)

cat("\n", rep("=", 70), "\n", sep = "")
cat("Australia consumption model complete.\n")
cat("Outputs written to:", file.path(project_root, "outputs"), "\n")
cat(rep("=", 70), "\n", sep = "")
