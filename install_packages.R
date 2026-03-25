# Install missing R packages required for ConsModelling project
# Missing packages identified: strucchange, patchwork

missing_packages <- c("strucchange", "patchwork")

install.packages(missing_packages, repos = "https://cran.rstudio.com/")

cat("Installation complete.\n")
