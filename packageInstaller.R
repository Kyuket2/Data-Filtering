# List of packages
required_packages <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "DT",
  "scales",
  "data.table",
  "gridExtra",
  "plotly",
  "glmnet"
)

# Install missing packages
installed <- installed.packages()
for (pkg in required_packages) {
  if (!(pkg %in% rownames(installed))) {
    cat(" Installing:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(" Already installed:", pkg, "\n")
  }
}

cat("\n All required packages are installed and ready to use.\n")