# =============================================================================
# 00_setup.R  –  Pre-flight checks and package loading
# =============================================================================
# 1. Verifies that all required input files and credentials are present before
#    any data loading or computation begins. Stops with a clear message if
#    anything is missing.
# 2. Loads all required packages so dependencies are visible in one place.
#
# To install any missing packages run:
#   install.packages(c("tidyverse", "readxl", "openxlsx", "httr2", "wbstats", "doParallel", "foreach", "ggrepel", "ggforce", "ggh4x", "scico", "legendry"))
# =============================================================================

# ─── Global Carbon Project file names ─────────────────────────────────────────
# Update these when a new GCB release is published (e.g. "...2026v1.0.xlsx").
# Files must be placed in the data/ directory.
# Download from: https://globalcarbonbudget.org/datahub/

GCB_NATIONAL_FILE <- "data/National_Fossil_Carbon_Emissions_2025_v0.3.xlsx"
GCB_GLOBAL_FILE   <- "data/Global_Carbon_Budget_2025_v0.6.xlsx"

# ─── Pre-flight checks ────────────────────────────────────────────────────────

.problems <- character(0)

# 1. Global Carbon Project data files
if (!file.exists(GCB_NATIONAL_FILE))
  .problems <- c(.problems, paste(
    paste0("MISSING: ", GCB_NATIONAL_FILE),
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  data/. If a newer version is available, also update GCB_NATIONAL_FILE",
    "  at the top of R/00_setup.R.", sep = "\n"))

if (!file.exists(GCB_GLOBAL_FILE))
  .problems <- c(.problems, paste(
    paste0("MISSING: ", GCB_GLOBAL_FILE),
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  data/. If a newer version is available, also update GCB_GLOBAL_FILE",
    "  at the top of R/00_setup.R.", sep = "\n"))

# 2. UN Population data (only needed on first run; cached afterwards)
if (!file.exists("data/un_population.csv") &&
    nchar(trimws(Sys.getenv("UN_POP_TOKEN"))) == 0)
  .problems <- c(.problems, paste(
    "MISSING: UN Population API token (needed for first run only).",
    "  Register and generate a token at:",
    "  https://population.un.org/dataportalapi/index.html",
    "  Then run:  Sys.setenv(UN_POP_TOKEN = \"your_token_here\")",
    "  Or add UN_POP_TOKEN=your_token_here to your .Renviron file.",
    "  After the first successful run the data is cached to",
    "  data/un_population.csv and the token is no longer needed.", sep = "\n"))

# 3. SSP scenario data
if (!file.exists("data/ssp_data.csv"))
  .problems <- c(.problems, paste(
    "MISSING: data/ssp_data.csv",
    "  Run the following to download SSP scenario data:",
    "  python python/fetch_ssp_data.py",
    "  (requires a free IIASA account)", sep = "\n"))

if (length(.problems) > 0) {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("ACTION REQUIRED before this script can run:\n")
  message(paste(.problems, collapse = "\n\n"))
  message(paste(rep("=", 70), collapse = ""), "\n")
  stop("Resolve the issues above, then re-run main.R.", call. = FALSE)
}
rm(.problems)

# ─── Load packages ────────────────────────────────────────────────────────────

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
library(legendry)     # nested axis guide for supplementary figures
