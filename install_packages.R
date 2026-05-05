#!/usr/bin/env Rscript
# Restore the locked package versions for this project.
#
# This script defers entirely to renv. The exact package set (direct and
# transitive) and pinned versions live in renv.lock at the project root;
# renv::restore() reads that lockfile and installs anything missing into
# the project-local library under renv/library/.
#
# Direct (top-level) dependencies are also enumerated in DESCRIPTION as a
# human-readable manifest.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

Sys.setenv(RENV_CONFIG_INSTALL_VERBOSE = "FALSE")
renv::restore(prompt = FALSE)
