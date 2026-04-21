# Fair-Share Carbon Budgets under Dual Accounting

Replication code for:

> **"National mitigation ambition under dual accounting systems"**  
> *[Authors, Journal, Year — to be updated upon acceptance]*

---

## Overview

This repository contains the R code used to derive national carbon budgets
and implied net-zero years under a dual (territorial + consumption-based)
accounting framework, and to produce all figures in the paper and supplementary
material.

The key finding is that assessments of national mitigation ambition are
sensitive not only to the carbon budget allocation principle applied, but also
to the weight assigned to producing versus consuming countries in bearing
responsibility for emissions.

---

## Repository structure

```
main.R                        # Entry point — sources all modules in order
R/
  00_packages.R               # Package loading
  01_parameters.R             # Global parameters and plot labels
  02_load_data.R              # Download and read raw data
  03_prepare_data.R           # Cleaning, reshaping, Rest-of-world aggregation
  04_allocation_functions.R   # Allocation-principle helper functions
  05_calculate_budgets.R      # National carbon budget calculations (parallelised)
  06_plot_paper.R             # Main-paper figures
  07_plot_supplementary.R     # Supplementary figures
  08_export.R                 # Write Excel output tables
python/
  fetch_ssp_data.py           # Fetch SSP projections from IIASA (run once)
data/
  ssp_data.csv                # SSP output from fetch_ssp_data.py (not in repo)
output/
  Graphs/                     # PNG figures (created on first run)
  NationalCarbonBudgets.xlsx  # Full results table
  DataForSupplementary.xlsx   # Data underlying supplementary figures
```

---

## Quick start

### 1. Install R packages

```r
install.packages(c(
  "tidyverse", "readxl", "openxlsx", "wbstats",
  "doParallel", "foreach", "ggrepel", "ggforce", "ggh4x", "scico"
))
```

Minimum requirement: **dplyr ≥ 1.1.0** (released 2023-01).

### 2. Fetch SSP scenario data (once)

```bash
pip install pyam-iamc pandas
python python/fetch_ssp_data.py
```

This downloads GDP|PPP and Population projections from the
[IIASA SSP Scenario Database 3.2](https://data.ece.iiasa.ac.at) and saves
them to `data/ssp_data.csv`.  A free IIASA account is required; credentials
are cached after the first login.

After downloading, open `data/ssp_data.csv`, note the model names printed
by the script, and pin them in `R/01_parameters.R`:

```r
SSPModelGDP        <- "OECD ENV-Growth 2023"   # example — check your output
SSPModelPopulation <- "IIASA-WiC POP 2023"     # example — check your output
```

**If you cannot access the IIASA database**, place `iamc_db_GDP.xlsx` and
`iamc_db_POP.xlsx` (IAMC wide format, downloaded manually from the SSP portal)
in the project root.  `R/02_load_data.R` will detect the xlsx files and use
them instead.

### 3. Provide remaining input files

| File | Description | Source |
|------|-------------|--------|
| `CountryAssumptions.xlsx` | Country list, development classification, EU membership | Compiled by authors |
| `unpopulation_dataportal.csv` | UN Population Division data (1960–2100, medium variant, both sexes) | [UN Population Portal](https://population.un.org/dataportal/) |
| `National_Fossil_Carbon_Emissions_2024v1.0.xlsx` | GCP national emission data | [Global Carbon Project](https://globalcarbonbudgetdata.org/) |
| `Global_Carbon_Budget_2024_v1.0.xlsx` | GCP global carbon budget | [Global Carbon Project](https://globalcarbonbudgetdata.org/) |

Historical GDP data are downloaded automatically from the
[World Bank API](https://data.worldbank.org/) at run time.

### 4. Run the analysis

```r
# In R or RStudio
source("main.R")
```

```bash
# From the terminal
Rscript main.R
```

All figures are saved to `output/Graphs/` and tables to `output/`.

---

## Carbon budget allocation principles

Five allocation principles are implemented (see Supplementary Methods):

| Principle | Description |
|-----------|-------------|
| Equal Cumulative per Capita | Equal per-capita share of the cumulative budget from a historic base year |
| Annual Equal per Capita | Equal per-capita share of the annual global emission pathway |
| Grandfathering | Proportional to current (2021) emission share |
| Contraction and Convergence | Convergence to equal per capita by 2050 |
| Capability | Inverse weighting by GDP per unit of population² |

The `AccountingFramework` parameter (*f*) ranges from 0 (full territorial
responsibility) to 1 (full consumption-based responsibility), in steps of 0.05.
For the Equal Cumulative per Capita principle, historic base years from 1990 to
2021 (in 5-year steps) are used.

---

## Key parameters (`R/01_parameters.R`)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `NoCores` | `detectCores() - 1` | CPU cores for parallel budget calculation |
| `CarbonBudget` | 500 GtCO₂ (1.5 °C), 900 GtCO₂ (2 °C) | AR6 WG1 budgets (adjusted for 2020–2021) |
| `SSPScenario` | `"SSP2"` | SSP scenario used for Capability and C&C |
| `SSPModelGDP` | `NULL` (auto) | Model for GDP|PPP projections |
| `SSPModelPopulation` | `NULL` (auto) | Model for population projections |
| `YearEnd` | `2021` | Last year with observed GCP emissions |

---

## License

[To be specified upon publication]
