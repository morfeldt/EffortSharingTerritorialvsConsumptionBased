# =============================================================================
# 01_parameters.R  –  Global parameters and plot labels
# =============================================================================

# -----------------------------------------------------------------------------
# Parallel computing
# -----------------------------------------------------------------------------
# Number of CPU cores used for the budget calculation (05_calculate_budgets.R).
# Set to 1 to disable parallel computing (useful for debugging).
NoCores <- 4 #max(1L, detectCores() - 1L)

# -----------------------------------------------------------------------------
# Carbon budgets (starting from 2024)
# -----------------------------------------------------------------------------
# Source: IPCC AR6 WG1, Table SPM.2 (50th percentile for 1.5 °C, 67th for 2 °C)
# Adjusted for 2020–2023 actual emissions using GCP data (see 03_prepare_data.R).
# Units: GtCO2
CarbonBudget <- tibble(
  TempTarget  = c(1.5, 2),
  BudgetGtCO2 = c(500, 1150)
)

# -----------------------------------------------------------------------------
# SSP scenario settings
# -----------------------------------------------------------------------------
# Scenario used for future population and GDP projections (all allocation principles).
SSPScenario <- "SSP2"

# Model names for GDP and population projections from the SSP database.
# Must exactly match a model name in data/ssp_data.csv — an error is raised
# at load time if the value is missing or not found in the data. After running
# python/fetch_ssp_data.py, check data/ssp_data.csv for available model names.
SSPModelGDP        <- "OECD ENV-Growth 2025"
SSPModelPopulation <- "IIASA-WiC POP 2025"

# GDP unit to select from the SSP database. The OECD ENV-Growth model provides
# GDP in multiple base years; pick one for consistency. Check data/ssp_data.csv
# for available units if you switch to a different GDP model.
SSPUnitGDP <- "billion USD_2017/yr"

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
# Weight for assigning responsibility axis labels (α = 0: territorial, α = 1: consumption-based)
LabelWeightResponsibility <- c(
  "0" = "Full weight on territorial emissions",
  "0.5" = "Equal weighting",
  "1" = "Full weight on consumption-based emissions"
)

LabelTempTarget <- c(
  "1.5" = "1.5°C with 50% probability",
  "2"   = "2°C with 67% probability"
)

# Emission type labels (used in trend plots)
LabelEmissions <- c(
  "Consumption Emissions"  = "Consumption-based emissions",
  "Territorial Emissions"  = "Territorial emissions"
)
