#!/usr/bin/env Rscript
# run_dev.R - Start the microecoshiny application in development mode
# Usage: Rscript dev/run_dev.R

# Set development options
options(
  shiny.port = 8080,
  shiny.host = "127.0.0.1",
  shiny.autoreload = TRUE,
  shiny.launch.browser = TRUE,
  shiny.minified = FALSE,
  golem.app.prod = FALSE
)

# Set test data path
Sys.setenv(
  microecoshiny_TEST_DATA = "E:/3_R_packages/AI/microeco_protocol_v1/Input/1.Amplicon/QIIME2_converted"
)

# Print startup message
cat("\n")
cat("===========================================\n")
cat("  microecoshiny v0.1.0                    \n")
cat("  Microbiome Data Analysis Tool            \n")
cat("  Starting in development mode...          \n")
cat("  URL: http://127.0.0.1:8080              \n")
cat("===========================================\n")
cat("\n")

# Load required packages
library(shiny)
library(golem)
library(bs4Dash)
library(microeco)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(shinyAce)
library(fresh)
library(shinycssloaders)

# Source all R files
source("R/app_config.r")
source("R/app_ui.r")
source("R/app_server.r")
source("R/fct_i18n.r")
source("R/fct_utils.r")
source("R/mod_abund.r")
source("R/mod_alpha.r")
source("R/mod_beta.r")
source("R/mod_codes.r")
source("R/mod_composition.r")
source("R/mod_core.r")
source("R/mod_diff.r")
source("R/mod_env.r")
source("R/mod_export.r")
source("R/mod_func.r")
source("R/mod_workspace.r")
source("R/mod_import.r")
source("R/mod_ml.r")
source("R/mod_multiomics.r")
source("R/mod_network.r")
source("R/mod_norm.r")
source("R/mod_nullmodel.r")
source("R/mod_obj_browser.r")
source("R/mod_preprocess.r")
source("R/run_app.r")

# Run the application
run_app()
