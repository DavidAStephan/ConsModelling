#!/usr/bin/env Rscript
# Quick re-estimation using saved RDS (skips data download)
# Run from project root: Rscript Ausreplication/R/run_estimation_from_rds.R

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
    else normalizePath("Ausreplication/R/run_estimation_from_rds.R",
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

# Build model_data exactly as australia_consumption_model.R does
model_data <- master %>%
  rename(
    dlcons  = d_ln_cons_pc,
    lincome = ln_ydi_real_pc,
    lcons   = ln_cons_real_pc
  ) %>%
  mutate(
    ln_y_over_c = lincome - lag(lcons, 1L)
  )

complete_core <- sum(complete.cases(model_data %>%
  select(dlcons, ln_y_over_c, real_rate, lincome)))
message(sprintf("  model_data rows: %d   complete for core vars: %d",
                nrow(model_data), complete_core))

cat(rep("=", 70), "\n", sep = "")
cat("Running estimation framework\n")
cat(rep("=", 70), "\n", sep = "")

source(file.path(script_dir, "australia_estimation.R"), local = TRUE)

cat(rep("=", 70), "\n", sep = "")
cat("Done.\n")
cat(rep("=", 70), "\n", sep = "")
