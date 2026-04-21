# =============================================================================
# 01_parameters.R  –  Global parameters and plot labels
# =============================================================================

# -----------------------------------------------------------------------------
# Parallel computing
# -----------------------------------------------------------------------------
# Number of CPU cores used for the budget calculation (05_calculate_budgets.R).
# Set to 1 to disable parallelism (useful for debugging).
NoCores <- 4 #max(1L, detectCores() - 1L)

# -----------------------------------------------------------------------------
# Carbon budgets (starting from 2022)
# -----------------------------------------------------------------------------
# Source: IPCC AR6 WG1, Table SPM.2 (50th percentile for 1.5 °C, 67th for 2 °C)
# Adjusted for 2020–2021 actual emissions using GCP data (see 03_prepare_data.R).
# Units: GtCO2
CarbonBudget <- tibble(
  TempTarget  = c(1.5, 2),
  BudgetGtCO2 = c(500, 900)
)

# -----------------------------------------------------------------------------
# SSP scenario settings
# -----------------------------------------------------------------------------
# Scenario used for Capability and Contraction-and-Convergence allocation principles.
SSPScenario <- "SSP2"

# Model names for GDP and population projections from the SSP database.
# Set to NULL to auto-select the first model found in data/ssp_data.csv.
# After running python/fetch_ssp_data.py, check data/ssp_data.csv for available
# models and set these explicitly for reproducibility, e.g.:
#   SSPModelGDP        <- "OECD ENV-Growth 2023"
#   SSPModelPopulation <- "IIASA-WiC POP 2023"
SSPModelGDP        <- "OECD ENV-Growth 2025"
SSPModelPopulation <- "IIASA-WiC POP 2025"

# Years over which the analysis runs
YearStart    <- 1990L   # first year of historical period used in budgets
YearEnd      <- 2023L   # last year with observed emissions (GCP data)
YearBudget   <- 2024L   # first year of the forward-looking budget period
YearHorizon  <- 2070L   # end of the budget horizon

# -----------------------------------------------------------------------------
# ISO-2 codes for EU-27 member states (used to aggregate EU bloc)
# -----------------------------------------------------------------------------
EUStatesISO2 <- c(
  "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR", "ES", "FR",
  "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
  "PL", "PT", "RO", "SI", "SK", "FI", "SE"
)

# -----------------------------------------------------------------------------
# Plot labels
# -----------------------------------------------------------------------------
# Accounting framework axis labels (f = 0: territorial, f = 1: consumption-based)
LabelAccountingFramework <- c(
  "0" = "Territorial\naccounting",
  "1" = "Consumption-based\naccounting"
)

# Emission type labels (used in trend plots)
LabelEmissions <- c(
  "Consumption Emissions"  = "Consumption-based emissions",
  "Territorial Emissions"  = "Territorial emissions"
)

# Historic responsibility year labels
LabelHistoricResponsibility <- c(
  "0" = "No historic responsibility",
  setNames(
    paste0("Historic responsibility from ", seq(1990, 2021, 5)),
    as.character(seq(1990, 2021, 5))
  )
)
