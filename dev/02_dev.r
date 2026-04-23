# 02_dev.R - Development helpers

library(golem)

# Use development options
options(
  shiny.port = 8080,
  shiny.autoreload = TRUE,
  shiny.launch.browser = TRUE,
  golem.app.prod = FALSE
)

# Set test data path environment variable
Sys.setenv(
  microecoshiny_TEST_DATA = "E:/3_R_packages/AI/microeco_protocol_v1/Input/1.Amplicon/QIIME2_converted"
)

# Helper: Check for missing dependencies
check_dependencies <- function() {
  pkgs <- c("shiny", "bs4Dash", "microeco", "file2meco",
            "ggplot2", "plotly", "DT", "dplyr", "tidyr",
            "shinycssloaders", "shinyWidgets", "shinyAce",
            "fresh", "readxl", "writexl", "cowplot")

  missing <- character(0)
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }

  if (length(missing) > 0) {
    message("Missing packages: ", paste(missing, collapse = ", "))
    message("Run dev/01_start.R to install them.")
  } else {
    message("All dependencies are available.")
  }

  invisible(missing)
}

# Helper: Document all modules
document_modules <- function() {
  devtools::document()
  message("Documentation updated.")
}

# Helper: Run tests
run_tests <- function() {
  devtools::test()
}

# Helper: Build package
build_pkg <- function() {
  devtools::build()
}

# Helper: Check package
check_pkg <- function() {
  devtools::check()
}
