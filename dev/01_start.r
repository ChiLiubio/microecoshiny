# 01_start.R - Project initialization
# Run this script to set up the microecoshiny project

# Install golem if not available
if (!requireNamespace("golem", quietly = TRUE)) {
  install.packages("golem")
}

# Install key dependencies
pkgs <- c(
  "shiny", "bs4Dash", "microeco", "file2meco",
  "ggplot2", "plotly", "DT", "dplyr", "tidyr",
  "shinycssloaders", "shinyWidgets", "shinyAce",
  "fresh", "readxl", "writexl", "cowplot"
)

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing: ", pkg)
    install.packages(pkg)
  }
}

# Install suggested packages for full functionality
suggested <- c("ape", "Biostrings", "vegan", "shinytest2")
for (pkg in suggested) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing suggested: ", pkg)
    if (pkg == "Biostrings") {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }
      BiocManager::install(pkg)
    } else {
      install.packages(pkg)
    }
  }
}

message("\nmicroecoshiny project setup complete!")
message("Run dev/run_dev.R to start the application.")
