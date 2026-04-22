# =============================================================================
# 00_packages.R  –  Load required packages
# =============================================================================
# All packages are loaded here so dependencies are visible in one place.
# To install any missing packages run:
#   install.packages(c(
#     "tidyverse", "readxl", "openxlsx", "wbstats",
#     "doParallel", "foreach", "ggrepel", "ggforce", "ggh4x", "scico"
#   ))
#
# Minimum versions: dplyr >= 1.1.0 (for reframe()), tidyr >= 1.0.0
# =============================================================================

library(tidyverse)    # ggplot2, dplyr, tidyr, readr, purrr, stringr, forcats
library(readxl)       # read Excel files (replaces xlsx / reshape2)
library(openxlsx)     # write multi-sheet Excel files
library(wbstats)      # World Bank API
library(doParallel)   # parallel back-end for foreach
library(foreach)      # parallel iteration
library(ggrepel)      # non-overlapping text labels in ggplot2
library(ggforce)      # additional ggplot2 geoms / faceting
library(ggh4x)        # extended faceting helpers (facet_grid2, independent scales)
library(scico)        # perceptually uniform scientific colour palettes
library(legendry)     # additional ggplot2 functions
