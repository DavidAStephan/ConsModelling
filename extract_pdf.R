#!/usr/bin/env Rscript
# Quick PDF text extraction using pdftools
tryCatch({
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    install.packages("pdftools", repos = "https://cloud.r-project.org")
  }
  library(pdftools)
  txt <- pdf_text("References/Italy.pdf")
  cat("TOTAL_PAGES:", length(txt), "\n")
  for (i in seq_along(txt)) {
    cat("\n===PAGE", i, "===\n")
    cat(txt[[i]])
  }
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})
