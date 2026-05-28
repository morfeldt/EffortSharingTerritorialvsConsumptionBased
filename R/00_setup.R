# =============================================================================
# 00_setup.R  –  Pre-flight checks and package loading
# =============================================================================
# 1. Verifies that all required input files and credentials are present before
#    any data loading or computation begins. Stops with a clear message if
#    anything is missing.
# 2. Loads all required packages so dependencies are visible in one place.
#
# To install any missing packages run:
#   install.packages(c("tidyverse", "readxl", "openxlsx", "wbstats","doParallel", "foreach", "ggrepel", "ggforce", "ggh4x", "scico", "legendry", "ggalluvial"))
#
# Minimum versions: dplyr >= 1.1.0 (for reframe()), tidyr >= 1.0.0
#
# KNOWN WORKING VERSIONS (as of May 2026)
# ggplot2  3.5.2  –  ggplot2 >= 4.0.0 rewrote the guide system in a way that
#                    breaks legendry's nested axis. Stay on 3.5.2 until legendry
#                    is updated. To restore:
#                      remotes::install_version("ggplot2",  "3.5.2")
#                      remotes::install_version("legendry", "0.2.2")
#                      remotes::install_version("ggh4x",    "0.2.8")
#                      remotes::install_version("ggrepel",  "0.9.5")
#                      remotes::install_version("ggalluvial", "0.12.5")
# =============================================================================

# ─── Pre-flight checks ────────────────────────────────────────────────────────

.problems <- character(0)

# 1. Global Carbon Project data files
#    Download from: https://globalcarbonbudget.org/datahub/
#    Two files are needed — update the filenames in 02_load_data.R if a new
#    version has been published (e.g. "...2025v1.0.xlsx").
if (!file.exists("data/National_Fossil_Carbon_Emissions_2025_v0.3.xlsx"))
  .problems <- c(.problems, paste(
    "MISSING: National_Fossil_Carbon_Emissions_2025_v0.3.xlsx",
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  the project root. If a newer version is available, also update the",
    "  filename on line 48 of R/02_load_data.R.", sep = "\n"))

if (!file.exists("data/Global_Carbon_Budget_2025_v0.6.xlsx"))
  .problems <- c(.problems, paste(
    "MISSING: Global_Carbon_Budget_2025_v0.6.xlsx",
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  the project root. If a newer version is available, also update the",
    "  filename on line 69 of R/02_load_data.R.", sep = "\n"))

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
# ggalluvial not loaded: incompatible with ggplot2 3.5.2 (calls gg_par from 4.0.x)
# Sankey diagrams use ggforce::geom_parallel_sets instead.
