# microecoshiny

<!-- badges -->
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R Version](https://img.shields.io/badge/R%20%3E%3D%204.1.0-brightgreen)](https://www.r-project.org)

## Overview

**microecoshiny** is an interactive web application for comprehensive microbiome data analysis, built on top of the [`microeco`](https://github.com/ChiLiubio/microeco) R package. It provides a user-friendly web interface for the complete workflow from data import to advanced analysis, targeting microbiome research (16S rRNA, ITS, metagenomics, etc.).

## Key Features

- **Interactive Web Interface** - No R coding required, point-and-click analysis
- **Bilingual Support** - Seamless switching between English and Chinese
- **Real-time Object Browser** - Right panel shows all microtable objects in memory
- **Draggable Resizable Layout** - IDE-style layout with adjustable panel widths
- **Auto-generated R Code** - All operations produce reproducible R code
- **Multiple Data Format Support** - QIIME2, BIOM, TSV, RData, RDS formats

## Main Analysis Modules

### Data Management
| Module | Description |
|--------|-------------|
| Import | Auto-detect, QIIME2, RData/RDS, TSV file import |
| Preprocess | Filtering, merging, subsetting, taxonomy cleaning |
| Normalization | 10 methods (rarefy, CLR, CSS, TMM, etc.) |
| Export | Save data and plots in various formats |

### Basic Analysis
| Module | Description |
|--------|-------------|
| Composition | Bar, heatmap, pie, box, line, donut plots with facets |
| Abundance | Community abundance calculation and visualization |
| Alpha Diversity | 11 metrics with 7 chart types |
| Beta Diversity | 6 ordination methods with statistical tests |
| Core Microbiome | Core species detection and Venn diagrams |

### Advanced Analysis
| Module | Description |
|--------|-------------|
| Differential Abundance | 22 methods (LEfSe, DESeq2, edgeR, ANCOM-BC2, etc.) |
| Environmental Factors | Correlation, RDA, CCA, Mantel test |
| Network Analysis | COR, SpiecEasi, gcoca with role analysis |
| Null Model | NTI, NRI, betaNTI, RCbray calculations |
| Functional Prediction | FAPROTAX, FUNGuild, NJC19 methods |
| Machine Learning | 6-step workflow with multiple algorithms |

### Multi-omics
| Module | Description |
|--------|-------------|
| Metagenome & Metabolomics | Cross-omics integration and correlation analysis |

## Technical Stack

| Component | Technology |
|-----------|------------|
| Web Framework | Shiny + Golem |
| UI Framework | bs4Dash (Bootstrap 4) |
| Data Core | microeco (>= 2.1.0) |
| Plotting | ggplot2 + plotly |
| Tables | DT |
| Code Editor | shinyAce |
| UI Components | shinyWidgets |
| Drag & Drop | shinyjqui |

## Installation

### Requirements

- R (>= 4.1.0)
- Required R packages: shiny, microeco, bs4Dash

### Install from Source

```r
# Install dependencies
install.packages(c("devtools", "remotes"))

# Install microecoshiny online
devtools::install_github("ChiLiubio/microecoshiny")

# Or install microecoshiny from local file
devtools::install_local("path/to/microecoshiny")
```


```r
# Launch the app
microecoshiny::run_app()
```



### Quick Start

1. Launch the app with `microecoshiny::run_app()`
2. Load data via "Import" tab (use built-in example data for testing)
3. Navigate through analysis modules in the left sidebar
4. View and manage data objects in the right-side Object Browser
5. Download generated R code from the "Generate Code" tab

