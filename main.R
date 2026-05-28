# =============================================================================
# Net-Zero Targets under Dual Emissions Accounting and Global Effort Sharing
# =============================================================================
# Replication code for:
#   "Net-Zero Targets under Dual Emissions Accounting and Global Effort Sharing"
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
source("R/00_setup.R")              # 1. Pre-flight checks and package loading
source("R/01_parameters.R")         # 2. Global parameters and plot labels
source("R/02_load_data.R")          # 3. Download / read raw data
source("R/03_prepare_data.R")       # 4. Clean, reshape, aggregate Rest-of-World
source("R/04_allocation_functions.R") # 5. Allocation-principle helper functions
source("R/05_calculate_budgets.R")  # 6. National carbon budget calculations (parallelised)
source("R/06_plot_sample_countries.R") # 7. Sample-country figures
source("R/07_plot_all_countries.R") # 8. All-countries figures (incl. ExtendedDataFigure1)
source("R/08_export.R")             # 9. Write Excel output tables
source("R/09_text_data.R")          # 10. In-text data (sample countries + all countries)
source("R/10_sankey_transitions.R") # 11. Sankey diagram (Figure6)

message("Done. Output written to output/")
