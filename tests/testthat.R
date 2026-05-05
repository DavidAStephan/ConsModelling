# Top-level test driver.
#
# Loads the helpers under test and runs every test_that() block under
# tests/testthat/. The helpers are sourced (not library()-loaded) because
# model_helpers.R is a flat script of free functions, not a packaged R
# namespace.

library(testthat)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(zoo)
  library(readxl)
  library(urca)
  library(KFAS)
  library(sandwich)
})

# Resolve the project root so this driver works whether invoked from the
# project root (the documented case) or from elsewhere.
.this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE),
  error = function(e) {
    args <- commandArgs(trailingOnly = FALSE)
    m    <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
    if (length(m) > 0L) {
      normalizePath(m[1L], winslash = "/", mustWork = FALSE)
    } else {
      normalizePath("tests/testthat.R", winslash = "/", mustWork = FALSE)
    }
  }
)
project_root <- normalizePath(file.path(dirname(.this_file), ".."), winslash = "/")
helpers_path <- file.path(project_root, "Ausreplication", "R", "model_helpers.R")
test_dir_path <- file.path(project_root, "tests", "testthat")

source(helpers_path, local = FALSE)

result <- test_dir(test_dir_path, reporter = "summary", stop_on_failure = FALSE)

# Exit non-zero if any tests failed, so CI surfaces the failure.
df <- as.data.frame(result)
if (any(df$failed > 0) || any(df$error)) {
  quit(status = 1L)
}
