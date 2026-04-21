# =============================================================================
# Fair-Share Carbon Budgets under Dual Accounting
# =============================================================================
# Replication code for:
#   "National mitigation ambition under dual accounting systems"
#
# Entry point. Sources all modules in order.
# Run from the project root:
#   source("main.R")        # in R / RStudio
#   Rscript main.R          # from the terminal
#
# Before the first run, fetch SSP scenario data:
#   python python/fetch_ssp_data.py
#   (requires a free IIASA account; see README.md)
#
# UN Population Data Portal API token
# ------------------------------------
# 02_load_data.R fetches population projections from the UN Data Portal API,
# which requires a Bearer token. To obtain one, register and generate a token
# at: https://population.un.org/dataportalapi/index.html
#
# Supply the token by setting the environment variable UN_POP_TOKEN before
# sourcing this script, e.g.:
#   Sys.setenv(UN_POP_TOKEN = "your_token_here")
#   source("main.R")
#
# To avoid setting it every session, add the following line to your .Renviron
# file (edit it with usethis::edit_r_environ()):
#   UN_POP_TOKEN=your_token_here
#
# The token is never written to any file in this project.
# =============================================================================

# Clear workspace
rm(list = ls(all.names = TRUE))
gc()

# Source modules in order --------------------------------------------------
source("R/00_preflight.R")          # 1. Check required files and credentials
source("R/00_packages.R")           # 2. Load and (optionally) install packages
source("R/01_parameters.R")         # 3. Global parameters and plot labels
source("R/02_load_data.R")          # 4. Download / read raw data
source("R/03_prepare_data.R")       # 5. Clean, reshape, aggregate Rest-of-World
source("R/04_allocation_functions.R") # 6. Allocation-principle helper functions
source("R/05_calculate_budgets.R")  # 7. National carbon budget calculations (parallelised)
source("R/06_plot_paper.R")         # 8. Main-paper figures
source("R/07_plot_supplementary.R") # 9. Supplementary figures
source("R/08_export.R")             # 10. Write Excel output tables

message("Done. Output written to output/")
