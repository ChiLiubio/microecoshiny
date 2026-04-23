# 03_deploy.R - Deployment scripts

# === Deploy to ShinyApps.io ===
deploy_shinyapps <- function() {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
  }

  rsconnect::setAccountInfo(
    name = Sys.getenv("SHINYAPPS_ACCOUNT"),
    token = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET")
  )

  rsconnect::deployApp(
    appDir = here::here(),
    appTitle = "microecoshiny",
    forceUpdate = TRUE
  )
}

# === Deploy to Docker ===
create_dockerfile <- function() {
  dockerfile <- "
FROM rocker/shiny:4.3.0

# System dependencies
RUN apt-get update && apt-get install -y \\
    libcurl4-openssl-dev \\
    libssl-dev \\
    libxml2-dev \\
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e \\"install.packages(c('golem', 'bs4Dash', 'microeco', 'file2meco', \\
    'ggplot2', 'plotly', 'DT', 'dplyr', 'tidyr', 'shinycssloaders', \\
    'shinyWidgets', 'shinyAce', 'fresh', 'readxl', 'writexl', 'cowplot'))\\"

# Copy app
COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server/

# Install package
RUN R -e \\"devtools::install('.')\\"

# Expose port
EXPOSE 3838

# Run
CMD [\\"R\\", \\"-e\\", \\"options(shiny.port = 3838, shiny.host = '0.0.0.0'); microecoshiny::run_app()\\"]
"

  writeLines(dockerfile, "Dockerfile")
  message("Dockerfile created. Build with: docker build -t microecoshiny .")
}

# === Deploy as static HTML (for simple sharing) ===
# Note: Full Shiny app cannot be static, but plot outputs can be exported

# === Deploy to Posit Connect ===
deploy_posit <- function() {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
  }

  rsconnect::setAccountInfo(
    name = Sys.getenv("CONNECT_SERVER"),
    token = Sys.getenv("CONNECT_API_KEY")
  )

  rsconnect::deployApp(
    appDir = here::here(),
    appTitle = "microecoshiny",
    forceUpdate = TRUE
  )
}
